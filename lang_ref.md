# Yagbas Language Reference

## Grammar

Outline of how I think the language grammar works.

```
<ConstDecl> ::= const <Ident> = <ConstExpr> ;

<SectionDecl> ::= section <Ident> [ <0+ Location, comma separated> ] { <0+ BlockElement> }

<Ident> ::= primitive

<ConstExpr> ::= <NumberLiteral>

<NumberLiteral> ::= primitive

<Location> ::= rom0

<BlockElement> ::= <Label> :
                 | <MacroInvoke> ;
                 | <Statement> ;

<Label> ::= <Ident> | <NumberLiteral>

<MacroInvoke> ::= <Ident> ! ( <0+ TokenTrees> )

<Statement> ::= <Load>
              | <AluStatement>
              | <Jump>
              | <Dec>

<Load> ::= ld <Place8> , <Data8>
         | ld <Place16> , <ConstExpr>
         | ld <LoadSpecialPlace> , a
         | ld a , <LoadSpecialPlace>
         | ld [ <ConstExpr> ] , sp
         | ld [ <ConstExpr> ] , a
         | ld hl , sp + <ConstExpr>
         | ld hl , sp
         | ld sp , hl

<Place8> ::= a | b | c | d | e | h | l | [hl]

<Data8> ::= <Place8> | <ConstExpr>

<Place16> ::= bc | de | hl | sp

<LoadSpecialPlace> ::= [ <ConstExpr> ] | [bc] | [de] | [c] | [hl-] | [hl+]

<AluStatement> ::= <AluOp> a , <Data8> | <AluOp> <Data8>

<AluOp> ::= add | adc | sub | sbc | and | xor | or | cp

<Jump> ::= jp <ConstExpr> | jp <Condition> , <ConstExpr> | jp hl

<Condition> ::= cy | nc | z | nz | al

<Dec> ::= dec <Place8> | dec <Place16>
```
