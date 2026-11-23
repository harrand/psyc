# Cheatsheet

## String Literals

String literals in Sy are not nul-terminated by default. If you want a nul-terminating string, you must add the terminator yourself. For example:

```go

my_assert(my_zstrlen("foo") == 3); // false
my_assert(my_zstrlen("foo\0") == 3); // true
```

## Foo

Bar baz.
