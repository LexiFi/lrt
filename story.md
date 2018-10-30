# Announcing LexiFi Runtime Types

During the last three months I did an internship at LexiFi in Paris. The
central theme was to make LexiFi's system for runtime types accessible to the
OCaml community. I'm proud to present the outcome of my project with the
public release of lrt v0.1.

LexiFi has an extended compiler, that is able to produce runtime
representations of OCaml types. Since many years, LexiFi uses these
runtime types to implement generic type-driven operations. In fact, they
are a key element in many interesting higher level components. Among
others, there are an automatic GUI framework and a database layer that
automatically derives SQL schema from types and exposes a safe interface
for queries. The tight integration with the OCaml typechecker allows to
synthesize runtime representations with minimal (often zero) input from
the programmer and results in very convenient APIs.

On my first day at LexiFi, I encountered code similar to the following
snippet, which prints `Foo {bar = "Hello World"}` to the standard
output.

```ocaml
type nontriv_type =
  | Foo of {bar : string}

let () = debug (Foo {bar = "Hello World"})
```

How is this possible? The signature of `debug` provided me with some
hint on what's going on: Somehow the call to `debug` is automatically
expanded with a runtime type argument. The implementation of `debug`
destructs the provided type information in order to dynamically print
the given value.

```
val debug: t: 'a ttype -> 'a -> unit
```

Convenient, but unfortunately incompatible with the rest of the
ecosystem. In order to release the interesting bits of the
infrastructure that heavily depend runtime types, a compatibility
mechanism is required. And that's where I came into play.

During the first days, I quickly found out, that the compiler extension
derives a quasi-textual type representation (`stype`) from the types
inferred by the type checker. After the synthesis, the `stype`
is simply cast to an `'a ttype` with the correct `'a` filled in. This
last step guarantees some safety while passing type representations
around. E.g. for the above `debug` function, the type checker will stop
the user from providing a runtime type that does not fit the value.

I was told, that in principle a PPX syntax extension should be able to
do the same without relying on compiler modifications. I had no clue
about PPX, but it turned out that this is indeed the case. After digging
into the details of `stype` and learning how to do PPX, I came up with a
solution. Using the new PPX, the previous example becomes a bit more
verbose, but the burden of using runtime types on an ordinary compiler
is not too high.

```
type nontriv_type =
  | Foo of {bar : string}
  [@@deriving t]

let () = debug ~t:nontriv_type_t (Foo {bar = "Hello World"})
```

Of course, it took a while to get the details right. Mutual recursion,
float records, `[@@unboxed]`, abstract types, externally defined types,
the uncertainty around ppxlib, ppx_tools and ppx_deriving -- a lot of
things require special attention. At some point you have to make the
`stype` an `'a ttype`. `Obj.magic` is involved. You want to make it as
safe as possible and thus have to think a lot about how to lift syntax
to type checks.

Having available the basic infrastructure, I set out to include a few
low level LexiFi tools that make use of runtime types. This first
release contains a generic printer, a Quickcheck implementation, and
serializers using both JSON and a OCaml syntax compatible format. During
the porting of these tools, I stumbled upon the `xtype` -- another
runtime representation of types. In contrast to the `ttype`, it enables
safe inspection of type definitions. Instead of blindly copying it over
like I did with most parts of the previously tools, I decided to rewrite
it from scratch. The new representation allows to safely explore the
structure of types -- including sums, records and arrows -- through a
set of recursive GADTs. The module around `xtype` also handles the
destruction and construction of OCaml values based on runtime types.

During my third month at LexiFi, I tackled a another issue: when working
with runtime types, you often want to take action based on the structure
of types. E.g. an `'a option option` should be handled differently from
an ordinary `'a option`. The existing solution to such problems involved
sequences of equality checks. You first check the runtime type against
`option option`, and in case of inequality against `option`. On more
practical examples, you quickly get long cascades of expensive recursive
comparisons, potentially with a lot of redundant checks.

I peeked into the literature on term indexing and found that I might be
able to speed things up using discrimination trees. The biggest hurdle
here, was to map the `stype` to a simplified language that only contains
constants, applications and variables. Based on this abstraction, I was
able to provide a compact implementation of a trie, that supports
instantiation of type variables, variable normalization and
prioritization of specific types over more general ones. The resulting
index was then used to implement a pattern matching like mechanism for
runtime types. For example it is now possible to dynamically register
custom JSON converters given any runtime type:

```ocaml
module M : sig
  type t [@@deriving t]

  val of_string : string -> t
end = struct
  type t = { str : string; foo : bar } [@@deriving t]

  let of_string str = { str; foo = bar_of_string str }
  let string_conv = Json.conv ~t:[%t: string]
  let to_json t = string_conv.to_json t.str
  let of_json j = of_string (string_conv.of_json j)
  let () = Json.add ~t {to_json; of_json};
end

module N = struct
  let m =  M.of_string
    "I don't know much about M.t, but I can convert it to json!"

  let conv = Json.conv ~t:M.t

  (** Print "I don't know much about M.t, but I can convert it to json!" *)
  let speak () = print_endline (Json.encode (conv.to_json m))
end
```

In the end, I had some time left to wrap things up. I did some cleaning,
wrote documentation that hopefully enables others to use the tool, and
finally prepared the release to opam. If you got interested in LexiFi
runtime types, I recommend checking out the [documentation][main_doc]
and potentially also the code on [github][repo]. A good entry point
might be the implementation of `Json.conv`. It uses the
[xtype][xtype_doc] to derive generic converters for sum and record types
and the [matcher][matcher_doc] for dynamic registration of custom
converters.

[repo]: https://github.com/LexiFi/lrt
[main_doc]: https://lexifi.github.io/lrt/lrt/Lrt/index.html
[matcher_doc]: https://lexifi.github.io/lrt/lrt/Lrt/Matcher/index.html
[xtype_doc]: https://lexifi.github.io/lrt/lrt/Lrt/Xtype/index.html

But before getting too enthusiastic about using LexiFi runtime types in
your projects, you must be aware that this is a preliminary release.
LexiFi plans to adapt my changes and additions, but it's not clear to
which extent and how the integration will affect the APIs.

At last, I want to give a big thanks to LexiFi who provided me with this
great internship! During my time in Paris, I was not bored a single
second and the learning outcome is of course huge. Building the PPX
introduced me to meta programming and the OCaml AST. The `xtype` was my
first practical contact with GADTs, the memory representation of OCaml
values and the justified use of the Obj module. Finally, for the data
structure driving the pattern matching, I went all the way from
scientific literature to integration into an existing framework -- a
process that was never entirely covered at university. Thanks for
making all this possible!

