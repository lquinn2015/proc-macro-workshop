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
        let inclusive = input.peek(Token![..=]);
        if inclusive {
            let _idot: Token![..=] = input.parse()?;
        } else {
            let _dots: Token![..] = input.parse()?;
        }
        let to = syn::LitInt::parse(input)?;

        let content;
        let _braces = syn::braced!(content in input);
        let tt = proc_macro2::TokenStream::parse(&content)?;

        let from = from.base10_parse::<usize>()?;
        let mut to = to.base10_parse::<usize>()?;
        if inclusive {
            to += 1;
        }

        Ok(SeqMacroInput {
            from,
            to,
            tt,
            ident,
        })
    }
}

impl Into<proc_macro2::TokenStream> for SeqMacroInput {
    fn into(self) -> proc_macro2::TokenStream {
        self.expand(self.tt.clone())
    }
}

#[derive(Clone, Copy, Debug)]
enum Mode {
    ReplaceIdent(usize),
    ReplaceSeq,
}

impl SeqMacroInput {
    fn expand2(
        &self,
        tt: proc_macro2::TokenTree,
        rest: &mut proc_macro2::token_stream::IntoIter,
        mutated: &mut bool,
        mode: Mode,
    ) -> proc_macro2::TokenStream {
        let tt = match tt {
            proc_macro2::TokenTree::Group(g) => {
                let (expanded, g_mutated) = self.expand_pass(g.stream(), mode);
                let mut expanded = proc_macro2::Group::new(g.delimiter(), expanded);
                *mutated |= g_mutated;
                expanded.set_span(g.span());
                proc_macro2::TokenTree::Group(expanded)
            }
            proc_macro2::TokenTree::Ident(ident) if ident == self.ident => {
                if let Mode::ReplaceIdent(iter) = mode {
                    let mut lit = proc_macro2::Literal::usize_unsuffixed(iter);
                    lit.set_span(ident.span());
                    *mutated = true;
                    proc_macro2::TokenTree::Literal(lit)
                } else {
                    // No replace in top level pass
                    proc_macro2::TokenTree::Ident(ident)
                }
            }
            proc_macro2::TokenTree::Ident(mut ident) => {
                //  foo~N~bar
                //  foo~N
                let mut peek = rest.clone();
                match (mode, peek.next(), peek.next()) {
                    (
                        Mode::ReplaceIdent(iter),
                        Some(proc_macro2::TokenTree::Punct(ref punct)),
                        Some(proc_macro2::TokenTree::Ident(ref ident2)),
                    ) if punct.as_char() == '~' && ident2 == &self.ident => {
                        ident =
                            proc_macro2::Ident::new(&format!("{}{}", ident, iter), ident.span());
                        *rest = peek.clone();
                        *mutated = true;
                        match peek.next() {
                            Some(proc_macro2::TokenTree::Punct(ref punct))
                                if punct.as_char() == '~' =>
                            {
                                *rest = peek.clone();
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                proc_macro2::TokenTree::Ident(ident)
            }
            proc_macro2::TokenTree::Punct(p) if p.as_char() == '#' => {
                if let Mode::ReplaceSeq = mode {
                    // is this #(...)*
                    let mut peek = rest.clone();
                    match (peek.next(), peek.next()) {
                        (
                            Some(proc_macro2::TokenTree::Group(ref rep)),
                            Some(proc_macro2::TokenTree::Punct(ref star)),
                        ) if rep.delimiter() == proc_macro2::Delimiter::Parenthesis
                            && star.as_char() == '*' =>
                        {
                            // expand each group for each iter in range
                            *mutated = true;
                            *rest = peek;
                            return (self.from..self.to)
                                .map(|iter| {
                                    self.expand_pass(rep.stream(), Mode::ReplaceIdent(iter))
                                })
                                .map(|(ts, _)| ts)
                                .collect();
                        }
                        _ => {}
                    }
                }
                proc_macro2::TokenTree::Punct(p)
            }
            tt => tt,
        };
        std::iter::once(tt).collect()
    }

    fn expand_pass(
        &self,
        stream: proc_macro2::TokenStream,
        mode: Mode,
    ) -> (proc_macro2::TokenStream, bool) {
        let mut out = proc_macro2::TokenStream::new();
        let mut mutated = false;
        let mut tts = stream.into_iter();
        while let Some(tt) = tts.next() {
            let expanded = self.expand2(tt, &mut tts, &mut mutated, mode);
            out.extend(expanded);
        }
        (out, mutated)
    }

    /*
    Seq!(N in 0..10 {
       fn x#N() {}
       #(fn y#N(){})*
    })
    */

    fn expand(&self, stream: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        let (out, mutated) = self.expand_pass(stream.clone(), Mode::ReplaceSeq);
        if mutated {
            return out;
        }

        (self.from..self.to)
            .map(|iter| self.expand_pass(stream.clone(), Mode::ReplaceIdent(iter)))
            .map(|(ts, _)| ts)
            .collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    //eprintln!("{:#?}", input);
    let output: proc_macro2::TokenStream = input.into();
    output.into()
}
