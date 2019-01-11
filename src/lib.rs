#![recursion_limit = "128"]

extern crate proc_macro;

use std::collections::HashMap;

use crate::proc_macro::{TokenStream, TokenTree};
use quote::quote;
use syn::parse::Parser;
use syn::{self, Attribute, Data, DataStruct, Field, Fields, FieldsNamed, LitStr, Type};

#[proc_macro_attribute]
pub fn serde_fallback(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut item_ast: syn::DeriveInput = syn::parse(item).unwrap();

    // Get all fields in the annotated struct
    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named: fields, .. }),
        ..
    }) = &mut item_ast.data
    {
        fields
    } else {
        return quote! { #item_ast }.into();
    };

    let struct_ident = item_ast.ident.to_string();

    // Functions which will be optionally appended to the struct
    let mut append_fns = HashMap::new();

    for field in fields.iter_mut() {
        let mut default_fn = None;
        let mut deser_fn = None;

        field.attrs.retain(|attr| {
            if let Some((default, deser)) = todo(attr) {
                if let Some(default) = default {
                    default_fn = Some(default);
                }
                if let Some(deser) = deser {
                    deser_fn = Some(deser);
                }
                false
            } else {
                true
            }
        });

        let gen = if let Some(deser_fn) = deser_fn {
            quote! { #[serde(deserialize_with=#deser_fn)] }
        } else {
            let ty = parse_type(field).unwrap_or_else(String::new);
            if ty == "Vec" {
                let method_name = struct_ident.clone() + "::default_deserialize_vec";
                append_fns
                    .entry(method_name.clone())
                    .or_insert(default_deserialize_vec(&struct_ident));
                quote! { #[serde(deserialize_with=#method_name)] }
            } else {
                let mut method_name = default_fn
                    .as_ref()
                    .map(|f| f.value())
                    .unwrap_or_else(|| String::from("Default::default"));
                append_fns
                    .entry(method_name.clone())
                    .or_insert(default_deserialize(&struct_ident, field, &method_name));
                method_name = format!(
                    "{}::default_deserialize_{}",
                    struct_ident,
                    method_name.replace(":", "_")
                );
                quote! { #[serde(deserialize_with=#method_name)] }
            }
        };
        let attr = Attribute::parse_outer.parse(gen.into()).unwrap();
        field.attrs.push(attr[0].clone());

        let gen = if let Some(default_fn) = default_fn {
            quote! { #[serde(default=#default_fn)] }
        } else {
            quote! { #[serde(default)] }
        };
        let parser = Attribute::parse_outer;
        let attr = parser.parse(gen.into()).unwrap();
        field.attrs.push(attr[0].clone());
    }

    let mut tokens: TokenStream = quote! { #item_ast }.into();
    for (_, default_fn) in append_fns {
        tokens.extend(default_fn);
    }
    tokens
}

fn parse_type(field: &Field) -> Option<String> {
    if let Type::Path(type_path) = &field.ty {
        type_path
            .path
            .segments
            .first()
            .map(|f| f.value().ident.to_string())
    } else {
        None
    }
}

// TODO: Beautify, comment, rename
fn todo(attr: &Attribute) -> Option<(Option<LitStr>, Option<LitStr>)> {
    let segment = attr.path.segments.first()?;
    if segment.value().ident != "serde_fallback" {
        return None;
    }

    let tts: TokenStream = attr.tts.clone().into();
    let mut named_lits = if let Some(TokenTree::Group(group)) = tts.into_iter().next() {
        let mut stream = group.stream().into_iter();
        parse_named_lits(&mut stream)
    } else {
        return Some((None, None));
    };

    let default_fn = named_lits.remove("default");
    let deser_fn = named_lits.remove("deserialize_with");

    Some((default_fn, deser_fn))
}

/// Parse a `TokenTree` stream into a number of named literals.
fn parse_named_lits<T>(stream: &mut T) -> HashMap<String, LitStr>
where
    T: Iterator<Item = TokenTree>,
{
    let mut map = HashMap::new();

    loop {
        if let Some((ident, lit)) = parse_named_lit(stream) {
            map.insert(ident, lit);
        } else {
            break;
        }

        if !match_punct(stream, ",") {
            break;
        }
    }

    map
}

/// Parse the next elements of a `TokenTree` stream as a named literal.
fn parse_named_lit<T>(stream: &mut T) -> Option<(String, LitStr)>
where
    T: Iterator<Item = TokenTree>,
{
    // Check identifier
    let ident = parse_ident(stream)?;

    // Only `=` is allowed as separator
    if !match_punct(stream, "=") {
        return None;
    }

    // Get actual literal assigned
    let lit = parse_lit(stream)?;

    Some((ident, lit))
}

/// Check if the next element of a `TokenTree` stream matches a specific punctuation.
fn match_punct<T>(stream: &mut T, expected: &str) -> bool
where
    T: Iterator<Item = TokenTree>,
{
    if let Some(TokenTree::Punct(punct)) = stream.next() {
        if punct.to_string() == expected {
            return true;
        }
    }
    false
}

/// Parse the next element of a `TokenTree` stream as an identifier.
fn parse_ident<T>(stream: &mut T) -> Option<String>
where
    T: Iterator<Item = TokenTree>,
{
    if let TokenTree::Ident(ident) = stream.next()? {
        Some(ident.to_string())
    } else {
        None
    }
}

/// Parse the next element of a `TokenTree` stream as a string literal.
fn parse_lit<T>(stream: &mut T) -> Option<LitStr>
where
    T: Iterator<Item = TokenTree>,
{
    syn::parse::<LitStr>(stream.next()?.into()).ok()
}

/// `TokenStream` for default deserializer.
fn default_deserialize(struct_ident: &str, field: &Field, default_fn: &str) -> TokenStream {
    let struct_ty: Type = syn::parse_str(struct_ident).unwrap();
    let field_ty = &field.ty;

    let fn_name = format!("default_deserialize_{}", default_fn.replace(":", "_"));
    let fn_name: syn::Expr = syn::parse_str(&fn_name).unwrap();

    let default_fn = format!("{}()", default_fn);
    let default_fn: syn::Expr = syn::parse_str(&default_fn).unwrap();

    (quote! {
        impl #struct_ty {
            fn #fn_name<'a, D>(
                deserializer: D,
            ) -> ::std::result::Result<#field_ty, D::Error>
            where
                D: de::Deserializer<'a>,
            {
                match #field_ty::deserialize(deserializer) {
                    Ok(value) => Ok(value),
                    Err(err) => {
                        // TODO
                        println!("TODO: Problem with config: {}; using default value", err);
                        Ok(#default_fn)
                    }
                }
            }
        }
    })
    .into()
}

/// `TokenStream` for default `Vec` deserializer.
fn default_deserialize_vec(struct_ident: &str) -> TokenStream {
    let ty: Type = syn::parse_str(struct_ident).unwrap();
    (quote! {
        impl #ty {
            fn default_deserialize_vec<'a, D, T>(
                deserializer: D,
            ) -> ::std::result::Result<Vec<T>, D::Error>
            where
                D: de::Deserializer<'a>,
                T: Deserialize<'a> + Default,
            {
                // Deserialize as generic vector
                let vec = match Vec::<serde_yaml::Value>::deserialize(deserializer) {
                    Ok(vec) => vec,
                    Err(err) => {
                        // TODO
                        println!("TODO: Problem with config: {}; using empty vector", err);
                        return Ok(Vec::new());
                    },
                };

                // Move to lossy vector
                let mut bindings: Vec<T> = Vec::new();
                for value in vec {
                    match T::deserialize(value) {
                        Ok(binding) => bindings.push(binding),
                        Err(err) => {
                            // TODO
                            println!("TODO: Problem with config: {}; skipping value", err);
                        },
                    }
                }

                Ok(bindings)
            }
        }
    })
    .into()
}
