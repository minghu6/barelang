use proc_macro::TokenStream;

use quote::quote;


#[proc_macro]
pub fn hardcode_into_vmty(input: TokenStream) -> TokenStream {

    TokenStream::from(quote! {
        let mut ty_ts = match ty.to_string().as_str() {
            "i8" | "u8" => quote! {
                i8_t
            },
            "usize" => quote! {
                size_t
            },
            "i32" => quote! {
                i32_t
            },
            "void" => quote! {
                void_t
            },
            _ => panic!("Unsupported vm type name {}", ty.to_string()),
        };
        for _ in 0..ptrlv {
            ty_ts.extend(quote! {
                .ptr_type(AddressSpace::Generic)
            })
        }
    })

}