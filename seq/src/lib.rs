use proc_macro::TokenStream;
use syn::parse::Parse;
use syn::{parse_macro_input, Token};

#[derive(Debug)]
struct SeqMacroInput {
    from: usize,
    to: usize,
    ident: syn::Ident,
    tt: proc_macro2::TokenStream,
}

impl Parse for SeqMacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = syn::Ident::parse(input)?;
        let _tin: Token![in] = input.parse()?;
        let from = syn::LitInt::parse(input)?;
        let _dots: Token![..] = input.parse()?;
        let to = syn::LitInt::parse(input)?;

        let content;
        let _braces = syn::braced!(content in input);

        let tt = proc_macro2::TokenStream::parse(&content)?;
        /*eprintln!(
            "{:?} from[{:?}]..to[{:?}] content: {:?}",
            ident, from, to, tt
        );*/

        let from = from.base10_parse::<usize>()?;
        let to = to.base10_parse::<usize>()?;

        Ok(SeqMacroInput {
            from,
            to,
            tt,
            ident,
        })
    }
}

impl Into<TokenStream> for SeqMacroInput {
    fn into(self) -> TokenStream {
        (self.from..self.to)
            .map(|iter: usize| self.expand(self.tt.clone(), iter))
            .collect::<proc_macro2::TokenStream>()
            .into()
    }
}

impl SeqMacroInput {
    fn expand2(&self, tt: proc_macro2::TokenTree, iter: usize) -> proc_macro2::TokenTree {
        match tt {
            proc_macro2::TokenTree::Group(g) => {
                let mut expanded =
                    proc_macro2::Group::new(g.delimiter(), self.expand(g.stream(), iter));
                expanded.set_span(g.span());
                proc_macro2::TokenTree::Group(expanded)
            }
            proc_macro2::TokenTree::Ident(ident) if ident == self.ident => {
                let mut lit = proc_macro2::Literal::usize_unsuffixed(iter);
                lit.set_span(ident.span());
                proc_macro2::TokenTree::Literal(lit)
            }
            proc_macro2::TokenTree::Ident(ident) => {
                // Search for ~ followed by self.ident at end of ident
                // or   ~ ident ~ _suffix
                let name = ident.to_string();
                let partsi: Vec<_> = name.split("~").collect();
                if let Some(i) = parts.iter().position(|s| s == &self.ident) {}
                let mut lit = proc_macro2::Literal::usize_unsuffixed(iter);
                lit.set_span(ident.span());
                proc_macro2::TokenTree::Literal(lit)
                else {
                    proc_macro2::TokenTree::Ident(ident)
                } 

            }
            tt => tt,
        }
    }

    fn expand(&self, stream: proc_macro2::TokenStream, i: usize) -> proc_macro2::TokenStream {
        stream.into_iter().map(|tt| self.expand2(tt, i)).collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    input.into()
}
