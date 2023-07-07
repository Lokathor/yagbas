# Yagbas Language Reference

## Grammar

Outline of how I think the language grammar works.

```
<Ident> ::= primitive

<NumberLiteral> ::= primitive

<ConstExpr> ::= <NumberLiteral> | <MacroInvoke>

<Place8> ::= a | b | c | d | e | h | l | [hl]

<Place16> ::= bc | de | hl | sp

<Data8> ::= <Place8> | <ConstExpr>

<ConstDecl> ::= const <Ident> = <ConstExpr> ;

<SectionDecl> ::= section <Ident> [ <0+ Location, comma separated> ] { <0+ BlockElement> }

<Label> ::= <Ident> | <NumberLiteral>

<MacroInvoke> ::= <Ident> ! ( <0+ TokenTrees> )

<AluOp> ::= add | adc | sub | sbc | and | xor | or | cp

<Condition> ::= cy | nc | z | nz | al

<JumpStatement> ::= jp <ConstExpr> | jp <Condition> , <ConstExpr> | jp hl

<DecStatement> ::= dec <Place8> | dec <Place16>

<AluStatement> ::= <AluOp> a , <Data8> | <AluOp> <Data8>

<LoadSpecialAddr> ::= bc | de | hl-- | hl++ | <ConstExpr>

<LoadStatement> ::= ld <Place8> , <Data8>
                  | ld <Place16> , <ConstExpr>
                  | ld [ <LoadSpecialAddr> ] , a
                  | ld a , [ <LoadSpecialAddr> ]
                  | ld [ <ConstExpr> ] , sp
                  | ld [ <ConstExpr> ] , a
                  | ld hl , sp + <ConstExpr>
                  | ld hl , sp
                  | ld sp , hl

<Statement> ::= <LoadStatement>
              | <AluStatement>
              | <JumpStatement>
              | <DecStatement>
```

the parser module hasn't been updated to include the below elements

```
<Location> ::= rom0

<BlockElement> ::= <Label> :
                 | <MacroInvoke> ;
                 | <Statement> ;
```