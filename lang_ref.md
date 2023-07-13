# Yagbas Language Reference

## Grammar

```
<Module> ::= <Item...>

<Item> ::= <ConstDecl> | <SectionDecl>

<ConstDecl> ::= const <Ident> = <not(;) ...> ;

<SectionDecl> ::= section <Ident> [ <...> ] { <...> }

<BlockElem> ::= <MacroUse> ;
              | <InstrUse>
              | <PlaceUse>
              | <Label>
<MacroUse> ::= <Ident> ! ( <MacroArgs> )
<Label> ::= <NumLit> :
          | <Ident> :
<InstrUse> ::= <Instr> <not(;) ...> ;
<PlaceUse> ::= <Place> <not(;) ...> ;

<Place> ::= <Place8> | <Place16> | <PlaceIndirect> | <PlaceConst>
<Place8> ::= a | b | c | d | e | h | l | [hl]
<Place16> ::= bc | de | hl | sp
<PlaceIndirect> ::= [bc] | [de] | [hl++] | [hl--]
<PlaceConst> ::= [ <...> ]
```

