# Yagbas Language Reference

## Grammar

Outline of how I think the language grammar works.

```
<Ident> ::= primitive

<NumberLiteral> ::= primitive

<ConstExpr> ::= <NumberLiteral> | <MacroInvoke>

<Place8> ::= a | b | c | d | e | h | l | [hl]

<Data8> ::= <Place8> | <ConstExpr>

<ConstDecl> ::= const <Ident> = <ConstExpr> ;

<SectionDecl> ::= section <Ident> [ <0+ Location, comma separated> ] { <0+ BlockElement> }

<Label> ::= <Ident> | <NumberLiteral>

<MacroInvoke> ::= <Ident> ! ( <0+ TokenTrees> )
```

the parser module hasn't been updated to include the below elements

```
<Location> ::= rom0

<BlockElement> ::= <Label> :
                 | <MacroInvoke> ;
                 | <Statement> ;

<Statement> ::= <Load>
              | <AluStatement>
              | <Jump>
              | <Dec>

<Load> ::= ld <Place8> , <Data8>
         | ld <Place16> , <ConstExpr>
         | ld [ <LoadSpecialAddr> ] , a
         | ld a , [ <LoadSpecialAddr> ]
         | ld [ <ConstExpr> ] , sp
         | ld [ <ConstExpr> ] , a
         | ld hl , sp + <ConstExpr>
         | ld hl , sp
         | ld sp , hl

<Place16> ::= bc | de | hl | sp

<LoadSpecialAddr> ::= <ConstExpr> | bc | de | hl- | hl+

<AluStatement> ::= <AluOp> a , <Data8> | <AluOp> <Data8>

<AluOp> ::= add | adc | sub | sbc | and | xor | or | cp

<Jump> ::= jp <ConstExpr> | jp <Condition> , <ConstExpr> | jp hl

<Condition> ::= cy | nc | z | nz | al

<Dec> ::= dec <Place8> | dec <Place16>
```
