# Exercise 3

Open `./src/Main.purs` in your text editor to edit the exercise module.

To compile and run the exercise, from this directory run: `npm run validate`

You should see the following output when you've succeeded:

```
> @ validate /Users/jkachmar/src/lc2018/ex3
> pulp run

* Building project in /Users/jkachmar/src/lc2018/ex3
Compiling Main
           Src   Lib   All
Warnings   0     0     0  
Errors     0     0     0  
* Build successful.
"Valid: ThisIsAGoodPassword"


"[(InvalidPassword [EmptyField,(NoLowercase \"\"),(NoUppercase \"\"),(TooShort 0 6)])]"


"[(InvalidPassword [(NoUppercase \"badpassword\")])]"
```