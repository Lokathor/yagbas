
# Abstract Grammar Thingy

```
item = const_item
     | fn_item
     | static_item

const_item = `const` ident = const_expression

ident = ?

const_expression = ?

fn_item = `fn` ident ( ) { statements }

statement = reg_statement
          | mem_statement
          | keyword_statement

reg_statement = register op expression

mem_statement = `[` mem_target_expr `]` op expression

keyword_statement = `if` test_expr { statements }
                  | `loop` { statements }
                  | `break`
                  | `continue`

static_item = `static` ident : type_signature = static_expression

type_signature = ?

static_expression = ?
```
