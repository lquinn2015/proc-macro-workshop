use proc_macro::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{parse_macro_input, visit_mut::VisitMut};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut out = input.clone();
    let ty = parse_macro_input!(input as syn::Item);
    assert!(args.is_empty());
    if let Err(e) = sorted_varients(ty) {
        out.extend(TokenStream::from(e.to_compile_error()));
    }
    out.into()
}

fn sorted_varients(input: syn::Item) -> Result<(), syn::Error> {
    if let syn::Item::Enum(e) = input {
        let mut names = Vec::new();
        for varient in e.variants.iter() {
            let name = varient.ident.to_string();
            if names.last().map(|last| &name < last).unwrap_or(false) {
                let next_lex_i = names.binary_search(&name).unwrap_err();
                return Err(syn::Error::new(
                    varient.ident.span(),
                    format!("{} should sort before {}", name, names[next_lex_i]),
                ));
            }
            names.push(name);
        }
        Ok(())
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected enum or match expression",
        ))
    }
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut f = parse_macro_input!(input as syn::ItemFn);
    assert!(args.is_empty());
    let mut lm = LexiographicMatching::default();
    lm.visit_item_fn_mut(&mut f);

    let mut ts = quote!( #f );
    ts.extend(lm.errors.into_iter().take(1).map(|e| e.to_compile_error()));
    ts.into()
}

#[derive(Default)]
struct LexiographicMatching {
    errors: Vec<syn::Error>,
}

impl syn::visit_mut::VisitMut for LexiographicMatching {
    fn visit_expr_match_mut(&mut self, m: &mut syn::ExprMatch) {
        if m.attrs.iter().any(|a| a.path().is_ident("sorted")) {
            // remove sorted
            m.attrs.retain(|a| !a.path().is_ident("sorted"));
            // check varients
            let mut names = Vec::new();
            let mut wild = None;
            for arm in m.arms.iter() {
                if let Some(ref w) = wild {
                    self.errors
                        .push(syn::Error::new_spanned(&w, format!("Wildcards go last")));
                    break;
                }
                let path = if let Some(path) = get_arm_path(&arm.pat) {
                    path
                } else if let syn::Pat::Wild(w) = &arm.pat {
                    wild = Some(w);
                    continue;
                } else {
                    self.errors.push(syn::Error::new_spanned(
                        &arm.pat,
                        format!("unsupported by #[sorted]"),
                    ));
                    continue;
                };

                let name = path_as_string(&path);

                if names.last().map(|last| &name < last).unwrap_or(false) {
                    let next_lex_i = names.binary_search(&name).unwrap_err();
                    self.errors.push(syn::Error::new(
                        path.span(),
                        format!("{} should sort before {}", name, names[next_lex_i]),
                    ));
                }
                names.push(name);
            }
        }
        // Keep going
        syn::visit_mut::visit_expr_match_mut(self, m)
    }
}

fn path_as_string(p: &syn::Path) -> String {
    p.segments
        .iter()
        .map(|s| format!("{}", quote!(#s)))
        .collect::<Vec<_>>()
        .join("::")
}

fn get_arm_path(arm: &syn::Pat) -> Option<syn::Path> {
    match *arm {
        syn::Pat::Ident(syn::PatIdent { ident: ref id, .. }) => Some(id.clone().into()),
        syn::Pat::Path(ref p) => Some(p.path.clone()),
        syn::Pat::Struct(ref s) => Some(s.path.clone()),
        syn::Pat::TupleStruct(ref s) => Some(s.path.clone()),
        _ => None,
    }
}
