Sams lessons

Starting up Haskell

What is a function?
What is a concrete type?
What is a polymorphic type?

Define the parts in this Haskell function definition:

> factorial :: (Num a, Eq a) => a -> a
> factorial 0 = 1
> factorial n = factorial (n-1) * n

Is Haskell compiled or interpreted?
Whats the difference?
How do you compile a Haskell program?
How do you run a Haskell program?
How do you interpret a Haskell program?
What does GHC stand for?
What is the difference between GHC and GHCi?

> fact n = if n == 0 then 1 else fact (n-1) * n

Define fibonacci in Haskell.
It is defined as Fib(0) = 0, Fib(1) = 1, and Fib(n) = Fib(n-1) + Fib(n-2)

-------------------

The heart of every Haskell application is recursion. 

Every loop and definition is made using recursion.

Define recursion.



> main :: IO ()
> main = print $ factorial 10