I need to nail down my use of this terminology if I'm ever going to write a compiler... ;-)

Need to cite these from somewhere.

- Explicit vs. implicit types: do I have to declare the type of a variable? In C and C++ (and Go?), yes. In Erlang,
  Python and Rust, no. Do these imply strong or static types? Implicit typing where I don't have to declare the type
  (F#) -- yes; Erlang, where variables (values) have types, but they can vary -- no (also Python).
- Weak vs. strong typing: will the compiler coerce types? In C, variously. In Javascript, far too eagerly. Loosely typed. Use =, ==, ===.
- Static vs. dynamic typing. Can a variable have a different type at runtime? With F#, no -- just because it's
  implicitly-typed doesn't mean the type can vary later. With Erlang, yeah; a variable can change type. Except it kinda
  can't, because Erlang doesn't have variables, it has values, and they're single-shot. Elixir also has single-shot
  variables/values, but you don't see that, because they shadow previous instances.
- PHP weirdness.

It's complicated, innit?
