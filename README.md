filter
======

Filter tool.

Build
-----

```
cabal build
```

Usage
-----

```
filter [--pos1 POSITION] [--delim1 DELIMITER] [--pos2 POSITION]
       [--delim2 DELIMITER] filepath1 filepath2
```

Example
-------

```
% cat input.tsv
a	123
b	456
c	789
% cat pattern.tsv
b
x
a
% filter input.tsv pattern.tsv
c	789
```

```
% cat input.tsv
a	123
b	456
c	789
% cat pattern.tsv
x	qwerty	123
y	asdfgh	987
z	zxcvbn	654
% filter --pos1 1 --pos2 2 input.tsv pattern.tsv
b	456
c	789
```
