# unlisp-llvm

WIP LLVM-based compiler for a toy Lisp language. Current goal is to reach feature-parity with [interpreted version](https://github.com/OlegTheCat/unlisp).

Each expression is compiled to LLVM-IR, which is in turn is compiled to a machine code and then executed.

```
>>> (+ 1 2)
Expression compiled to LLVM IR:
; ModuleID = 'mod_1'
source_filename = "mod_1"

%unlisp_rt_symbol = type { i8*, %unlisp_rt_function* }
%unlisp_rt_function = type { i32, i8*, i8**, i64, i1, i8*, i8* }
%unlisp_rt_object = type { i32, i8* }

@"+" = global [2 x i8] c"+\00"

declare %unlisp_rt_symbol* @unlisp_rt_intern_sym(i8*)

declare %unlisp_rt_object @unlisp_rt_object_from_int(i64)

declare i64 @unlisp_rt_int_from_obj(%unlisp_rt_object)

declare %unlisp_rt_object @unlisp_rt_object_from_function(%unlisp_rt_function*)

declare %unlisp_rt_object @unlisp_rt_object_from_symbol(%unlisp_rt_symbol*)

declare i1 @unlisp_rt_object_is_nil(%unlisp_rt_object)

declare %unlisp_rt_object @unlisp_rt_nil_object()

declare i8* @malloc(i32)

declare void @raise_arity_error(i8*, i64, i64)

define %unlisp_rt_object @__repl_form__unlisp_2() {
entry:
  %intern = call %unlisp_rt_symbol* @unlisp_rt_intern_sym(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"+", i32 0, i32 0))
  %sym_fn_gep = getelementptr inbounds %unlisp_rt_symbol, %unlisp_rt_symbol* %intern, i32 0, i32 1
  %fun_ptr = load %unlisp_rt_function*, %unlisp_rt_function** %sym_fn_gep
  %arg_count_ptr = getelementptr inbounds %unlisp_rt_function, %unlisp_rt_function* %fun_ptr, i32 0, i32 3
  %arg_count = load i64, i64* %arg_count_ptr
  %arg_num_ok = icmp eq i64 2, %arg_count
  br i1 %arg_num_ok, label %entry1, label %entry3

entry1:                                           ; preds = %entry
  %invoke_gep = getelementptr inbounds %unlisp_rt_function, %unlisp_rt_function* %fun_ptr, i32 0, i32 5
  %invoke_ptr = load i8*, i8** %invoke_gep
  %fn_ptr_cast = bitcast i8* %invoke_ptr to %unlisp_rt_object (%unlisp_rt_function*, %unlisp_rt_object, %unlisp_rt_object)*
  %call = call %unlisp_rt_object @unlisp_rt_object_from_int(i64 1)
  %call2 = call %unlisp_rt_object @unlisp_rt_object_from_int(i64 2)
  %invoke_result = call %unlisp_rt_object %fn_ptr_cast(%unlisp_rt_function* %fun_ptr, %unlisp_rt_object %call, %unlisp_rt_object %call2)
  ret %unlisp_rt_object %invoke_result

entry3:                                           ; preds = %entry
  call void @raise_arity_error(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"+", i32 0, i32 0), i64 %arg_count, i64 2)
  unreachable
}


3
```

## Building

### Prerequisites

* `rustup`
* `cargo`
* `rlwrap`

### Build steps

1. Switch to the latest nightly Rust: `rustup update nightly && rustup default nightly`
1. Install LLVM-7
   * Ubuntu: `sudo apt install llvm-7-dev`
   * OS X: `brew install llvm@7 && export PATH="/usr/local/opt/llvm@7/bin:$PATH"`
1. Build & run the project:
   * Ubuntu: `RUSTFLAGS='-C link-args=-Wl,-export-dynamic' rlwrap cargo run`
   * OS X: `rlwrap cargo run`

## Features

### Literals

```
>>> 1
1
>>> nil
nil
>>> "foo"
"foo"
```

### Lisp special forms

```
>>> (let ((x 1) (y x)) (+ y x))
2
>>> (if (equal 2 (+ 1 1)) "foo" "bar")
"foo"
```

### Lists

```
>>> (cons 1 nil)
(1)
>>> (rest (list 1 2))
(2)
>>> (first (list 1 2))
1
```

### Varargs

```
>>> (set-fn (quote list) (lambda (& args) args))
nil
>>> (list 1 2 3)
(1 2 3)
```

### Apply

```
>>> (apply (symbol-function (quote +)) (quote (1 2)))
3
>>> (apply (symbol-function (quote +)) 1 2 (quote 3 4))
10
```


### Functions & closures

```
>>> (set-fn (quote foo) (lambda (x) (lambda (y) (+ x y))))
nil
>>> (set-fn (quote bar) (foo 1))
nil
>>> (bar 2)
3
```

### Error reporting

```
>>> x
compilation error: undefined symbol x

>>> (set-fn (quote x) (lambda (y)))
nil

>>> (x 1 2)
runtime error: wrong number of arguments (2) passed to x

>>> (+ 1 (quote x))
runtime error: cannot cast symbol to int

>>> (undefined-fn 1 2 3)
runtime error: undefined function undefined-fn

```
