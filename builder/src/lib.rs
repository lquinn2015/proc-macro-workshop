use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DeriveInput};

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
        } else {
            quote! { #name: std::option::Option<#ty>}
        }
    });

    let builder_fields_empty = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let build_func_decl = fields.iter().map(|f| {
        let name = &f.ident;
        if ty_inner_type("Option", &f.ty).is_some() {
            let expr = quote! {
                #name: self.#name.clone()
            };
            return expr;
        }
        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
        }
    });

    let extend_methods = fields.iter().filter_map(|f| {
        let name = f.ident.as_ref().unwrap();
        for attr in &f.attrs {
            let path = attr.path();
            let mut tts = attr.meta.clone().into_token_stream().into_iter();
            tts.next();
            eprintln!("Attribute found:  {:#?}", attr);
            eprintln!("Attribute tts: {:#?}", tts);
            if path.segments.len() == 1 && path.segments[0].ident == "builder" {
                if let Some(TokenTree::Group(g)) = tts.clone().into_iter().next() {
                    let mut tokens = g.stream().into_iter();
                    eprintln!("tokens: {:#?}", tokens);
                    match tokens.next().unwrap() {
                        TokenTree::Ident(ref i) => assert_eq!(i, "each"),
                        tt => panic!("Invalid token, expected 'each' found {}", tt),
                    }
                    match tokens.next().unwrap() {
                        TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
                        tt => panic!("Invalid token, expected 'each' found {}", tt),
                    }
                    let arg = match tokens.next().unwrap() {
                        TokenTree::Literal(l) => l,
                        tt => panic!("expected string found, {}", tt),
                    };
                    match syn::Lit::new(arg) {
                        syn::Lit::Str(s) => {
                            let arg = syn::Ident::new(&s.value(), s.span());
                            let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
                            return Some(quote! {
                                pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                                    self.#name.push(#arg);
                                }
                            });
                        }
                        _ => panic!("Not a valid string"),
                    }
                }
            }
        }
        None
    });

    let builder_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if let Some(inner_ty) = ty_inner_type("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let expanded = quote! {

        pub struct #bident {
            #(#builder_fields,)*
        }

        impl #bident {
            #(#builder_methods)*
            #(#extend_methods)*


            fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
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
