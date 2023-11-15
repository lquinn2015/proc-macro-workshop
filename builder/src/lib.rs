use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, punctuated::Punctuated, Attribute, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{name}Builder");
    let bident = syn::Ident::new(&bname, ast.ident.span());

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!()
    };
    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_type("Option", ty).is_some() {
            quote! { #name: #ty}
        } else if builder_of(f).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty>}
        }
    });

    let builder_fields_empty = fields.iter().map(|f| {
        let name = &f.ident;
        let is_builder = builder_of(&f);
        if is_builder.is_some() {
            quote! { #name: std::vec::Vec::new() }
        } else {
            quote! { #name: std::option::Option::None }
        }
    });

    let build_func_decl = fields.iter().map(|f| {
        let name = &f.ident;
        if ty_inner_type("Option", &f.ty).is_some() || builder_of(&f).is_some() {
            quote! { #name: self.#name.clone() }
        } else {
            quote! { #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))? }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        let (arg_type, value) = if let Some(inner_ty) = ty_inner_type("Option", ty) {
            (inner_ty, quote! { std::option::Option::Some(#name) })
        } else if builder_of(&f).is_some() {
            (ty, quote! {#name})
        } else {
            (ty, quote! { std::option::Option::Some(#name) })
        };

        let set_method = quote! {
                pub fn #name(&mut self, #name: #arg_type) -> &mut Self {
                    self.#name = #value;
                    self
                }
        };

        match extend_methods(&f) {
            None => set_method,
            Some((true, extension)) => extension,
            Some((false, extension)) => {
                let expr = quote! {
                    #set_method
                    #extension
                };
                expr.into()
            }
        }
    });

    let expanded = quote! {

        pub struct #bident {
            #(#builder_fields,)*
        }

        impl #bident {
            #(#methods)*
            //#(#extend_methods)*


            fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#build_func_decl,)*
                })
            }
        }

        impl #name {
            fn builder() -> #bident  {
                #bident {
                    #(#builder_fields_empty,)*
                }
            }

        }
    };

    expanded.into()
}

fn builder_of(f: &syn::Field) -> Option<&Attribute> {
    for attr in &f.attrs {
        let path = attr.path();
        if path.is_ident("builder") {
            return Some(attr);
        }
    }
    None
}

fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
    Some((
        false,
        syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
    ))
}

fn extend_methods(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let g = builder_of(f)?;
    let nvs = g
        .parse_args_with(Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated)
        .ok()?;

    let meta = match &nvs[0] {
        syn::Meta::NameValue(nv) => {
            if !nv.path.is_ident("each") {
                return mk_err(&g.meta);
            }
            nv
        }
        _ => {
            return mk_err(&g.meta);
        }
    };

    let val = match meta.value.clone().into_token_stream().into_iter().next() {
        Some(TokenTree::Literal(l)) => l,
        _ => {
            return mk_err(&g);
        }
    };
    match syn::Lit::new(val) {
        syn::Lit::Str(s) => {
            let arg = syn::Ident::new(&s.value(), s.span());
            let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
            let method = quote! {
                pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                    self.#name.push(#arg);
                    self
                }
            };
            Some((&arg == name, method))
        }
        _a => mk_err(_a),
    }
}

fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        };
    }
    None
}
