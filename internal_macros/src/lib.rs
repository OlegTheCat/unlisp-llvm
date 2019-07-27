extern crate proc_macro;
extern crate proc_macro2;

use proc_macro2::*;
use quote::{quote, quote_spanned};
use syn;
use syn::spanned::Spanned;
use uuid::Uuid;

fn ident(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}

fn compile_error_spanned<T: quote::ToTokens>(tokens: T, message: &str) -> TokenStream {
    syn::Error::new_spanned(tokens, message).to_compile_error()
}

fn gensym(span: Span) -> Ident {
    Ident::new(
        &format!("__gensym_{}", Uuid::new_v4().to_simple()),
        span
    )
}

fn build_apply_body(
    f_arg_ident: &Ident,
    args_list_ident: &Ident,
    parsed_invoke: &syn::ItemFn,
) -> TokenStream {
    let invoke_name = &parsed_invoke.ident;
    let mut invoke_args = quote!( #f_arg_ident , );

    let mut apply_body = TokenStream::new();
    let mut rest_to_intern = quote!(#args_list_ident);

    for _ in parsed_invoke.decl.inputs.iter().skip(1) {
        let arg_ident = gensym(Span::call_site());

        let rest_ident = gensym(Span::call_site());

        apply_body = quote! {
            #(#apply_body)*
            let #rest_ident = #rest_to_intern;
            let #arg_ident = #rest_ident.first();
        };

        rest_to_intern = quote!(#rest_ident.rest());
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
    let invoke_ident = &parsed_invoke.ident;

    if let Some(vararg) = parsed_invoke.decl.variadic {
        return compile_error_spanned(vararg, "cannot derive trivial apply for variadic functions")
            .into();
    }

    let abi = &parsed_invoke.abi;

    let apply_fn_name_str = format!(
        "{}_apply",
        invoke_ident.to_string().replace("_invoke", "")
    );
    let apply_fn_name = Ident::new(&apply_fn_name_str, Span::call_site());

    let f_arg_ident = Ident::new("f", Span::call_site());
    let args_list_ident = Ident::new("args", Span::call_site());
    let apply_body = build_apply_body(&f_arg_ident, &args_list_ident, &parsed_invoke);
    let mod_ident = gensym(Span::call_site());

    let result = quote! {
        #parsed_invoke

        mod #mod_ident {
            use crate::defs::*;
            use super::#invoke_ident;

            pub unsafe #abi fn #apply_fn_name(#f_arg_ident: *const Function,
                                              #args_list_ident: List) -> Object
                #apply_body
        }
        use #mod_ident::#apply_fn_name;
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

    let used_ident = ident(&format!("___USED_{}", fn_ident.to_string().to_uppercase()));

    let result = quote! {
        #[inline(never)]
        #[no_mangle]
        #parsed_fn

        #[used]
        static #used_ident: #unsafety #abi fn(#args) #ret_ty  = #fn_ident;
    };

    result.into()
}

fn extract_item_fn(item: &syn::Item) -> Option<&syn::ItemFn> {
    match item {
        syn::Item::Fn(item_fn) => Some(item_fn),
        _ => None,
    }
}

fn is_unlisp_rt_fn(fn_item: &syn::ItemFn) -> bool {
    fn_item.ident.to_string().starts_with("unlisp_rt")
}

fn llvm_def_generation_error<T: quote::ToTokens>(tokens: T, message: &str) -> TokenStream {
    compile_error_spanned(tokens, &format!("llvm defs generation failed: {}", message))
}

fn type_path_to_simple_name(ty_path: &syn::TypePath) -> Result<String, &str> {
    if ty_path.qself.is_some() {
        return Err("types with qualified self types are not supported");
    }

    let path = &ty_path.path;

    if path.leading_colon.is_some() {
        return Err("types with leading colon are not supported");
    }

    let segments = &path.segments;
    if segments.len() > 1 || segments.is_empty() {
        return Err("types with exactly one segment are the only supported types");
    }

    if !segments[0].arguments.is_empty() {
        return Err("types with type arguments are not supported");
    }

    Ok(segments[0].ident.to_string())
}

fn is_rt_type(name: &str) -> bool {
    match name {
        "Object" | "Function" | "List" | "Symbol" => true,
        _ => false,
    }
}

fn build_llvm_type_construction(
    ctx_ident: &Ident,
    module_ident: &Ident,
    ty: &syn::Type,
) -> TokenStream {
    match ty {
        syn::Type::Path(ty_path) => {
            let ty_name = match type_path_to_simple_name(ty_path) {
                Ok(name) => name,
                Err(message) => return llvm_def_generation_error(ty, message),
            };

            match ty_name.as_str() {
                "i64" | "u64" => quote_spanned!(ty.span()=> #ctx_ident.i64_type()),
                "i32" | "u32" => quote_spanned!(ty.span()=> #ctx_ident.i32_type()),
                "c_char" => quote_spanned!(ty.span()=> #ctx_ident.i8_type()),
                "c_void" => quote_spanned!(ty.span()=> #ctx_ident.void_type()),
                "VaList" => quote_spanned!(ty.span()=> #module_ident
                                           .get_type("va_list")
                                           .unwrap()
                                           .as_struct_type()
                                           .ptr_type(AddressSpace::Generic)),
                "bool" => quote_spanned!(ty.span()=> #ctx_ident.bool_type()),
                name if is_rt_type(name) => {
                    let llvm_name = format!("unlisp_rt_{}", name.to_lowercase());
                    quote_spanned!(ty.span()=> #module_ident
                                   .get_type(#llvm_name)
                                   .unwrap()
                                   .as_struct_type()
                                   .clone())
                }
                _ => llvm_def_generation_error(ty, &format!("unsupported type: {}", quote!(#ty))),
            }
        }
        syn::Type::Ptr(ty_ptr) => {
            let base_ty = &ty_ptr.elem;
            let base_constr = build_llvm_type_construction(ctx_ident, module_ident, &base_ty);
            quote_spanned!(ty.span()=> #base_constr.ptr_type(AddressSpace::Generic))
        }
        syn::Type::Never(_) => quote_spanned!(ty.span()=> #ctx_ident.void_type()),
        _ => llvm_def_generation_error(ty, "unsupported type"),
    }
}

fn build_llvm_type_construction_for_ret_type(
    ctx_ident: &Ident,
    module_ident: &Ident,
    ret_ty: &syn::ReturnType,
) -> TokenStream {
    match ret_ty {
        syn::ReturnType::Type(_, ty) => build_llvm_type_construction(&ctx_ident, &module_ident, ty),
        syn::ReturnType::Default => quote_spanned!(ret_ty.span()=> #ctx_ident.void_type()),
    }
}

fn build_llvm_def_generator_for_fn(fn_item: &syn::ItemFn) -> (Ident, TokenStream) {
    let ctx_arg_ident = Ident::new("ctx", fn_item.ident.span());
    let module_arg_ident = Ident::new("module", fn_item.ident.span());

    let ret_ty_constr = build_llvm_type_construction_for_ret_type(
        &ctx_arg_ident,
        &module_arg_ident,
        &fn_item.decl.output,
    );
    let param_constrs: Vec<_> = fn_item
        .decl
        .inputs
        .iter()
        .map(|param| match param {
            syn::FnArg::Captured(captured) => {
                build_llvm_type_construction(&ctx_arg_ident, &module_arg_ident, &captured.ty)
            }
            _ => llvm_def_generation_error(param, "unsupported fn arg"),
        })
        .collect();

    let rt_fn_name = fn_item.ident.to_string();
    let generator_fn_ident = Ident::new(&format!("{}_gen_def", &rt_fn_name), fn_item.ident.span());

    let generator_fn = quote_spanned! {fn_item.ident.span()=>
        #[allow(unused)]
        pub fn #generator_fn_ident(#ctx_arg_ident: &Context, #module_arg_ident: &Module) {
            module.add_function(
                #rt_fn_name,
                #ret_ty_constr.fn_type(&[#(#param_constrs.into(),)*], false),
                None
            );
        }
    };

    (generator_fn_ident, generator_fn)
}

#[proc_macro_attribute]
pub fn gen_llvm_defs(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let parsed_mod = syn::parse_macro_input!(item as syn::ItemMod);
    let mod_ident = &parsed_mod.ident;
    let content = parsed_mod.content.as_ref().map(|(_, items)| items);

    if content.is_none() {
        return llvm_def_generation_error(mod_ident, "cannot gen llvm defs for bodyless module")
            .into();
    }

    let content = content.unwrap();

    let rt_fns = content
        .iter()
        .filter_map(extract_item_fn)
        .filter(|f| is_unlisp_rt_fn(*f));

    let generator_idents_and_fns = rt_fns.map(build_llvm_def_generator_for_fn);
    let generator_idents = generator_idents_and_fns.clone().map(|x| x.0);
    let generator_fns = generator_idents_and_fns.map(|x| x.1);

    let defs_mod_ident = Ident::new("rt_fns_llvm_defs", mod_ident.span());

    let result = quote_spanned! {mod_ident.span() =>
        mod #mod_ident {
            #(#content)*


            #[allow(unused_imports)]
            #[cfg(feature = "llvm_defs")]
            pub mod #defs_mod_ident {

                use inkwell::context::Context;
                use inkwell::module::Module;
                use inkwell::AddressSpace;

                #(#generator_fns)*

                pub fn gen_defs(ctx: &Context, module: &Module) {
                    #(#generator_idents(ctx, module);)*
                }
            }
        }
    };

    result.into()
}
