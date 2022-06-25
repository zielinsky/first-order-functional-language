# first-order-functional-language
p ::= {define {d1 . . . dk} for e}\
d := [fun f (x1 . . . xl) = e]\
e ::=
  * x
  * {e1 ⊕ e2}
  * {ifz e0 then e1 else e2}
  * {let x be e1 in e2}
  * {f (e1 . . . el)}

⊕ ::= + | - | * | <=
