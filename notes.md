
# Abstract Grammar Thingy

```
item = const_item
     | fn_item
     | static_item

const_item = `const` ident = const_expression

ident = ?

const_expression = ?

fn_item = `fn` ident ( ) { [statement, 0 or more] }

statement = reg_statement
          | mem_statement
          | if_statement
          | loop_statement

reg_statement = ?

mem_statement = ?

if_statement = ?

loop_statement = ?

static_item = `static` ident : type_signature = static_expression

type_signature = ?

static_expression = ?
```
