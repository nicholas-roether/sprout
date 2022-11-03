# Sprout

_**Warning**: This crate is still in the early stages of development, and is not guaranteed to be stable._
_Also, I built this for fun to use with my personal projects without having any idea about how_
_stuff like this usually works, so until I eventually look into this topic more and find time for_
_optimiziations, be warned that this implementation is likely not optimal._

`sprout` is a rust crate that parses text into ASTs (_Abstract Syntax Trees_), given definitions for tokens
and grammars built from those tokens.


## Usage

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
         Self::Number => write!(f, "number")?,
         Self::Word => write!(f, "word")?,
         Self::Space => write!(f, "space")?
      }
      Ok(())
   }
}
```

Now, you can use the `alphabet` macro to provide definitions for your tokens using a subset of regular expressions
syntax that includes parentheses (`()`), square brackets (`[]`), ranges (`[a-z]`), as well as the operators `*`, `+` and `?`.

```rust
let alphabet = alphabet! {
   Token::Number => "[0-9]+";
   Token::Word => "[a-z]+";
   Token::Space => " "
}
```

Next, you define your grammar, in terms of "procedures", or "procs". They are the abstract parts of your language
that will end up forming the nodes of your syntax tree.

First of all, you again create an enum:

```rust
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
         Self::TwoOrThreeWords => write!(f, "two or three words")?,
         Self::WordOrNumber => write!(f, "word or number")?,
         Self::Sequence => write!(f, "word/number sequence")?
      }
      Ok(())
   }
}
```

Finally, define your procedures using the `grammar` macro in conjunction with `token`, `proc`, `repeat`, `choice` and `option`.

```rust
let grammar = grammar! {
   Proc::TwoOrThreeWords =>
      token!(Token::Word),
      token!(Token::Space),
      token!(Token::Word),
      option!(
         token!(Token::Space),
         token!(Token::Word)
      );
   Proc::WordOrNumber =>
      choice!(
         token!(Token::Word);
         token!(Token::Number)
      );
   Proc::Sequence =>
      repeat!(
         choice!(
            proc!(Proc::TwoOrThreeWords);
            proc!(Proc::WordOrNumber)
         )
      );
} 
```

As you can see, using `proc`, procedures can reference other procedures in the grammar, or even themselves.

Also: note _precedence_: The different paths in a `choice` will be checked in the order they appear, and including the content of an
`option` will always be preferred over exluding it.

Finally, from your alphabet and grammar, you can construct a parser:

```rust
let parser = Parser::new(alphabet, grammar);
```

This parser will now simply spit out an AST (or a ParsingError) for any string you throw at it!
For example this input

```rust
let tree = parser.parse("abc ab 123 xyz 69");
```

would produce an AST like this:

```
Sequence "abc ab 123 xyz 69"
   TwoOrThreeWords "abc ab"
   WordOrNumber "123"
   WordOrNumber "xyz"
   WordOrNumber "69"
```

Specifically, the output format is a `trees::Tree` of `ASTNode`s, where `ASTNode` has these fields:

| Name | Type                                          | Description                                           |
|------|-----------------------------------------------|-------------------------------------------------------|
| proc | `Proc` (or whatever you called it)            | The procedure that this node corresponds to           |
| text | `String`                                      | The text contained in the instance of the procedure   |
| pos  | `TextPosition` (has fields `line` and `char`) | The position of the start of the instance in the text |