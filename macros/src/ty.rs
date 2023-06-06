use super::*;

/// Expand the `#[ty]` macro.
pub fn ty(stream: TokenStream, item: &mut syn::Item) -> Result<TokenStream> {
    let ty = prepare(stream, item)?;
    Ok(create(&ty, item))
}

struct Type {
    ident: Ident,
    name: String,
    long: String,
    display: String,
    category: String,
    docs: String,
    constructor: Option<syn::Expr>,
    scope: Option<BlockWithReturn>,
}

fn prepare(stream: TokenStream, item: &mut syn::Item) -> Result<Type> {
    let (attrs, ident) = match item {
        syn::Item::Struct(item) => (&mut item.attrs, &item.ident),
        syn::Item::Type(item) => (&mut item.attrs, &item.ident),
        syn::Item::Enum(item) => (&mut item.attrs, &item.ident),
        _ => bail!(item, "invalid item"),
    };

    let docs = documentation(attrs);
    let mut lines = docs.split('\n').collect();
    let category = meta_line(&mut lines, "Category")?.into();
    let display: String = meta_line(&mut lines, "Display")?.into();
    let docs = lines.join("\n").trim().into();

    let str: syn::LitStr = syn::parse2(stream)?;
    let ty = Type {
        ident: ident.clone(),
        name: str.value(),
        long: display.to_lowercase(),
        display,
        category,
        docs,
        constructor: parse_attr(attrs, "constructor")?.flatten(),
        scope: parse_attr(attrs, "scope")?.flatten(),
    };

    Ok(ty)
}

fn create(ty: &Type, item: &syn::Item) -> TokenStream {
    let Type { ident, name, long, display, category, docs, .. } = ty;
    let scope = create_scope_builder(ty.scope.as_ref());
    let constructor = quote_option(&ty.constructor.as_ref().map(|ident| {
        quote! {
            ::typst::eval::Func::from(#ident)
        }
    }));
    quote! {
        #item

        impl ::typst::eval::TypeOf for #ident {
            fn ty() -> ::typst::eval::Type {
                static NATIVE: ::typst::eval::NativeType = ::typst::eval::NativeType {
                    info: ::typst::eval::Lazy::new(|| ::typst::eval::TypeInfo {
                        name: #name,
                        long: #long,
                        display: #display,
                        docs: #docs,
                        category: #category,
                        constructor: #constructor,
                        scope: #scope,
                    }),
                };
                (&NATIVE).into()
            }
        }
    }
}
