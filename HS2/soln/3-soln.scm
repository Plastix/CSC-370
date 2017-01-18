;; CSC 370 hw2-3 soln, Winter 2017

Problem 3


1. Write an ambiguous BNF grammar for the language of all binary
   strings.

<amb-bin> ::= 0 | 1 | 0<amb-bin> | <amb-bin>1
                | 1<amb-bin> | <amb-bin>0    

2. Write an unambiguous BNF grammar for the language of all binary
   strings.  

<unamb-bin> ::= 0 | 1 | 0<unamb-bin> | 1<unamb-bin>

3. Write a BNF grammar for the language of binary strings that are
   palindromes.

<palin> ::= 0 | 1 | 11 | 00 | 0<palin>0 | 1<palin>1

4. Write a BNF grammar for the language of signed numbers represented
   in hexadecimal.

<signed-hex> ::= <unsigned-hex>
               | -<unsigned-hex>

<unsigned-hex> ::= <hex-digit>
                 | <hex-digit><unsigned-hex>

<hex-digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
              | 8 | 9 | A | B | C | D | E | F

5. Write a BNF grammar for Python function signatures assuming that
   you are given a production rule for identifiers <ID>.

<func> ::= def <ID> (<param-list>):

<param-list> ::= empty | <ID> | <ID>, <param-list-tail>

<param-list-tail> ::= <ID> | <ID>, <param-list-tail>



