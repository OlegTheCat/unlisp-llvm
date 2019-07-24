extern crate proc_macro;
extern crate proc_macro2;

use proc_macro2::*;
use quote::quote;
use syn;

fn build_apply_body(
    f_arg_ident: &Ident,
    args_list_ident: &Ident,
    parsed_invoke: &syn::ItemFn,
) -> TokenStream {
    let invoke_name = &parsed_invoke.ident;
    let mut invoke_args = quote!( #f_arg_ident , );

    let mut apply_body = TokenStream::new();
    let mut rest_to_intern = quote!(#args_list_ident);

    for (i, _) in parsed_invoke.decl.inputs.iter().skip(1).enumerate() {
        let arg_ident = Ident::new(format!("arg_{}", i).as_str(), Span::call_site());

        apply_body = quote! {
            #(#apply_body)*
            let __rest = #rest_to_intern;
            let #arg_ident = __rest.first();
        };

        rest_to_intern = quote!(__rest.rest());
        invoke_args = quote!( #(#invoke_args)* #arg_ident ,);
    }

    quote! {
        {
            #(#apply_body)*
            #invoke_name(#(#invoke_args)*)
        }
    }
}

#[proc_macro_attribute]
pub fn trivial_apply(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let parsed_invoke = syn::parse_macro_input!(item as syn::ItemFn);

    if parsed_invoke.decl.variadic.is_some() {
        panic!("cannot derive trivial apply for variadic functions");
    }

    let abi = &parsed_invoke.abi;

    let apply_fn_name_str = format!(
        "{}_apply",
        parsed_invoke.ident.to_string().replace("_invoke", "")
    );
    let apply_fn_name = Ident::new(apply_fn_name_str.as_str(), Span::call_site());

    let f_arg_ident = Ident::new("f", Span::call_site());
    let args_list_ident = Ident::new("args", Span::call_site());

    let apply_body = build_apply_body(&f_arg_ident, &args_list_ident, &parsed_invoke);

    let result = quote! {
        #parsed_invoke

        unsafe #abi fn #apply_fn_name(#f_arg_ident: *const crate::defs::Function,
                                      #args_list_ident: crate::defs::List) -> crate::defs::Object
            #apply_body

    };

    result.into()
}

#[proc_macro_attribute]
pub fn runtime_fn(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let parsed_fn = syn::parse_macro_input!(item as syn::ItemFn);
    let fn_ident = &parsed_fn.ident;
    let args = &parsed_fn.decl.inputs;
    let ret_ty = &parsed_fn.decl.output;
    let abi = &parsed_fn.abi;
    let unsafety = &parsed_fn.unsafety;

    if parsed_fn.decl.variadic.is_some() {
        panic!("runtime fn cannot be variadic");
    }

    if !parsed_fn.decl.generics.params.is_empty() {
        panic!("runtime fn cannot be generic");
    }

    let used_ident = Ident::new(
        format!("___USED_{}", fn_ident.to_string().to_uppercase()).as_str(),
        Span::call_site(),
    );

    let result = quote! {
        #[inline(never)]
        #[no_mangle]
        #parsed_fn

        #[used]
        static #used_ident: #unsafety #abi fn(#args) #ret_ty  = #fn_ident;
    };

    result.into()
}
