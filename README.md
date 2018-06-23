
                               _____                    _____                    _____
                              /\    \                  /\    \                  /\    \
                             /::\    \                /::\____\                /::\    \
                            /::::\    \              /:::/    /               /::::\    \
                           /::::::\    \            /:::/    /               /::::::\    \
                          /:::/\:::\    \          /:::/    /               /:::/\:::\    \
                         /:::/__\:::\    \        /:::/    /               /:::/__\:::\    \
                        /::::\   \:::\    \      /:::/    /                \:::\   \:::\    \
                       /::::::\   \:::\    \    /:::/    /      _____    ___\:::\   \:::\    \
                      /:::/\:::\   \:::\ ___\  /:::/____/      /\    \  /\   \:::\   \:::\    \
                     /:::/__\:::\   \:::|    ||:::|    /      /::\____\/::\   \:::\   \:::\____\
                     \:::\   \:::\  /:::|____||:::|____\     /:::/    /\:::\   \:::\   \::/    /
                      \:::\   \:::\/:::/    /  \:::\    \   /:::/    /  \:::\   \:::\   \/____/
                       \:::\   \::::::/    /    \:::\    \ /:::/    /    \:::\   \:::\    \
                        \:::\   \::::/    /      \:::\    /:::/    /      \:::\   \:::\____\
                         \:::\  /:::/    /        \:::\__/:::/    /        \:::\  /:::/    /
                          \:::\/:::/    /          \::::::::/    /          \:::\/:::/    /
                           \::::::/    /            \::::::/    /            \::::::/    /
                            \::::/    /              \::::/    /              \::::/    /
                             \::/____/                \::/____/                \::/    /
                              ~~                       ~~                       \/____/

                                               A parser library
                                     Author:Nashe Mncube/TuringDisciple

### Introduction
BUS is a fully functional parser combinator library written in a Ocaml. It exists not to replace current parser libraries, but rather to act as a toy parser library for the curious and learning. Relatively small, bus uses advanced concepts in functional programming, which are easily accessible to a learner of functional languages who may not know, or may want to see to see these concepts in action.

### How does it work?
The best way to illustrate how this library works is via an example. Suppose that I define a simple language as such which describes a simple language consisting of expressions which can be added, and subtracted.

      <expr> := ("+"|"-") <num> ( ("+"|"-") <num> )*
      <num>  := ("0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9")+

For those not familiar with [Backus-Naur form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form), this is a way of representing context-free grammars. ***<expr>*** terms are mad of one ***<num>*** term or string of ***<num>*** terms separated by plus and minus terms. Each ***<num>*** term is simply a string of one or more digits.
So for example ***"1", "1+2", "42-32"*** are all strings within the grammar but strings such as ***100++, --5, 2-4+*** are not.

So what would a parser for this language look like? Well first we would define a data structure that corresponds to our grammars description. This may seem pointless as we could just parse the input on the fly, but it makes our lives easier as will be seen.

      type num  = Num of int
      type expr = Expr of   
