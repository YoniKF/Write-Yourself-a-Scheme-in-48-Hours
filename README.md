# Write-Yourself-a-Scheme-in-48-Hours
My solution for the [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) tutorial.

Compile using ```ghc -o scheme Main.hs```.

Example:

```
>scheme
>>> (define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))
(lambda ("n") ...)
>>> (fact 10)
3628800
>>> quit
```
