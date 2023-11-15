use proc_macro::TokenStream;
use syn::parse::Parse;
use syn::{parse_macro_input, Token};

#[derive(Debug)]
struct SeqMacroInput {}

impl Parse for SeqMacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let var = syn::Ident::parse(input)?;
        let _tin: Token![in] = input.parse()?;
        let rang: syn::ExprRange = input.parse()?;
        eprintln!("{:#?}", var);
        eprintln!("{:#?}", rang);
        let _tree: syn::ExprBlock = input.parse()?;

        Ok(SeqMacroInput {})
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as SeqMacroInput);
    eprintln!("{:#?}", ast);

    TokenStream::new()
}
