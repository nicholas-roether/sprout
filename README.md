# Sprout

_**Warning**: This crate is still in the early stages of development, and is not guaranteed to be stable._
_Also, I built this for fun to use with my personal projects without having any idea about how_
_stuff like this usually works, so until I eventually look into this topic more and find time for_
_optimiziations, be warned that this implementation is likely not optimal._

`sprout` is a rust crate that parses text into ASTs (_Abstract Syntax Trees_), given definitions for tokens
and grammars built from those tokens.


## Usage

You can start by importing the sprout prelude. For most projects, this will be sufficient.

```rust
use sprout::prelude::*;
```

### Example

First, define an enum for your tokens. It should derive the necessary traits shown below.

```rust
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token {
   Number,
   Word,
   Space
}
```

Implement `std::fmt::Display` for your token enum. This will be used to generate human-readable error messages.

```rust
use std::fmt;

impl fmt::Display for Token {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
         Self::Number => write!(f, "number"),
         Self::Word => write!(f, "word"),
         Self::Space => write!(f, "space")
      }
   }
}
```

Now, you can use the `alphabet` macro to provide definitions for your tokens using a subset of regular expressions
syntax that includes parentheses (`()`), square brackets (`[]`), ranges (`[a-z]`), as well as the operators `*`, `+` and `?`.

```rust
use Token::*;

let alphabet = alphabet! {
   Number => "[0-9]+";
   Word => "[a-z]+";
   Space => " "
};
```

Next, you define your grammar, in terms of "procedures", or "procs". They are the abstract parts of your language
that will end up forming the nodes of your syntax tree.

First of all, you again create an enum:

```rust
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Proc {
   TwoOrThreeWords,
   WordOrNumber,
   Sequence
}
```

And, again, implement `std::fmt::Display`:

```rust
impl fmt::Display for Proc {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
         Self::TwoOrThreeWords => write!(f, "two or three words"),
         Self::WordOrNumber => write!(f, "word or number"),
         Self::Sequence => write!(f, "word/number sequence")
      }
   }
}
```

Finally, define your procedures using the `grammar` macro.

```rust
use Token::*;
use Proc::*;

let grammar = grammar! {
   #TwoOrThreeWords => Word, Space, Word, (Space, Word)?;
   #!WordOrNumber => [Word; Number];
   #?Sequence => ([#TwoOrThreeWords; #WordOrNumber]){Space}+,
};
```

As you can see in this example, within a grammar definition, names of procedures are always prefixed with a `#`.

The `#!` prefix on a procedure definition means that that procedure is _primitive_. _Primitive_ procedures are 
considered to be the **simple building blocks** of your language, which means that error messages will refer to 
them by their name rather than their composite parts. You should use `#!` on procedures that are so low-level that
their construction can be considered an implementation detail and is not relevant to the user.

The `#?` prefix on a procedure definition means that that procedure is _hidden_, meaning that they will never appear
in error messages by name. This is good to make sure your error messages don't get too abstract. You should use
`#?` on procedures that are so high-level and abstract that they are not relevant to localized parsing errors.

Names of tokens are usually left as-is, but you can also designate tokens as _signatures_ by prefixing them with an `@`. Signature
tokens give the current procedure priority over others, even if it only matches up to that point. This is handy if you want to define a
certain structure that forces the parser to interpret the text in a certain way, even if it leads to an error. It is good to use
signatures to try to capture the intuition of when a certain procedure is "obviously" meant, even if its structure is incorrect; this
helps produce more helpful error messages.

Sequences of tokens/procedures are comma-separated.

You can use the following special syntax inside procedure definitions for more complex patterns:

| Syntax            | Description                                         |
|-------------------|-----------------------------------------------------|
| `(<A>)*`          | Repeat `<A>` zero or more times                     |
| `(<A>)+`          | Repeat `<A>` one or more times                      |
| `(<A>){<B>}*`     | Repeat `<A>` zero or more times, delimited by `<B>` |
| `(<A>){<B>}+`     | Repeat `<A>` one or more times, delimited by `<B>`  |
| `(<A>)?`          | `<A>` is optional                                   |
| `[<A>; <B>; ...]` | Select one of `<A>`, `<B>`, etc.                    |


Now, finally, from your alphabet and grammar, you can construct a parser:

```rust
let parser = Parser::new(alphabet, grammar);
```

This parser will now simply spit out an AST (or a ParsingError) for any string you throw at it!
For example this input

```rust
let tree = parser.parse(Proc::Sequence, "abc ab 123 xyz 69".to_string());
```

would produce an AST like this:

```
Sequence["abc ab 123 xyz 69"](
   TwoOrThreeWords["abc ab"]
   WordOrNumber["123"]
   WordOrNumber["xyz"]
   WordOrNumber["69"]
)
```

Specifically, the output type, `AST`, is an alias for a `trees::Tree` of `ASTNode`s, where `ASTNode` has these fields:

| Name | Type                                          | Description                                           |
|------|-----------------------------------------------|-------------------------------------------------------|
| proc | `Proc` (or whatever you called it)            | The procedure that this node corresponds to           |
| text | `String`                                      | The text contained in the instance of the procedure   |
| pos  | `TextPosition` (has fields `line` and `char`) | The position of the start of the instance in the text |

For more information about the API of `trees::Tree` see [the trees documentation](https://docs.rs/crate/trees/latest)