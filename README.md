# Retrograde
A minimal Forth interpreter (with some cool additions)

## Distinct features
Retrograde, unlike standard implementations of Forth, allows for symbol literals to be pushed onto the stack. These literals use the `'<...>` notation. For example, `'abc` creates a symbol called "abc" and pushes it to the stack.
Retrograde allows for recursion without a need for a special token, such as `recurse` (as other Lisps require).

Some examples below:
```
[$] 1 2 3 sep 4 5 6 print*
654(1 2 3)
[$] sep 1 2 3 4 5 6 7 8 9
(| 1 2 3 4 5 6 7 8 9)
[$] drop*
()
[$]: sum one? if sumh else then ;
()
[$] : sumh empty? swap sep? rot or not if + sum then ;
()
[$] 1 2 3 4 5 sum
(15)
```
