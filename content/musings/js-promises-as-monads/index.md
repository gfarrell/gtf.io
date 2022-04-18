---
title: "Promises as Monads"
date: 2022-04-18
lastmod: 2022-04-18
draft: false
abstract: >-
  Can we treat Javascript's Promise class as a Monad, and does that give us any
  useful features?
tags:
- programming
- javascript
- typescript
- haskell
- monads
- error-handling
toc: true

---

While `async/await` has become extremely popular in the javascript
and typescript communities (to which I'm just going to refer as
javascript or `js` henceforth), probably because it allows one to write
asynchronous code as though it were synchronous, which _feels_ much
nicer, I found myself irritated with the ergonomics of error handling
in javascript. I came back to writing js after a spell writing almost
exclusively in Haskell, and was struck how old `Promise`-based code I
had written had far more in common with the Haskell I was now writing
than the newer `async/await`-based code I had written before moving away
from js. So I set out to see if I was right -- can we treat `Promise`
like an instance of `Monad` (probably `ExceptT e IO a`), even if much
more loosely-typed (in typescript, for example, we can't specify the
type of the error with which a `Promise` might `reject()`).

## Monads and Monad Laws

Monads are defined {{footnote}}While there are formal definitions of
Monads from category theory, for the purposes of this exploration a
code-based definition will suffice.{{/footnote}} (in Haskell) through
two functions which apply to them: `bind` and `return`:

    class Monad m where
      return :: a -> m a
      (>>=) :: m a -> (a -> m b) -> m b

Broadly this means that you can use `return` to insert a value `a` into
a monadic context `m`, and you can chain together computations which
operate on that inner value using `bind` (or `>>=`).

There are three monad laws to which any instance of the `Monad`
typeclass must comply:

1. Left identity: `return a >>= f ≡ f a`
2. Right identity: `m >>= return ≡ m`
3. Associativity: `(m >>= g) >>= f ≡ m >>= (\x -> g x >>= f)`

One nice example of a monad in Haskell is the `Either` monad, which can
be used to represent a computation which can fail with some error type.
This is remarkably similar to the concept of a `Promise` which also
represents a computation (although, in this case, an asynchronous one)
which can fail.

The `Either` monad allows us to chain together computations with `>>=`
or `bind` without having to continually handle the error cases, because
`>>=` will only run the bound function on a `Right` value, not a `Left`
value. For example:

    -- Doubles a number and fails if the result is greater than 10
    f :: Integer -> Either String Integer
    f x = return . (* 2) >>= \y -> if y > 10 then Left "too big" else Right y

    -- Let's chain this together to demonstrate its utility
    result1 = f 8 >>= sendSecretCode
    -- In this case, `sendSecretCode` will never be called, because f 8 == Right "too big"
    result2 = f 4 >>= sendSecretCode
    -- In this case `sendSecretCode` will be called with the argument 4

This is a very contrived example (there are plenty of monad tutorials
out there to explore, and this is not the aim of this exploration), but
it shows that we can chain together computations without having to worry
about what happens if one fails, until the point at which we want to
handle the failures properly.

## Can we apply the monad laws to Promises?

The above `Either` case looks a lot like how we handle errors in
`Promise`-land:

    const f = (x: number) => new Promise((res, rej) => {
      const y = x * 2;
      if (y > 10) {
        rej("too big");
      } else {
        res(y);
      }
    });

    const result1 = await f(8).then(sendSecretCode);
    const result2 = await f(4).then(sendSecretCode);

To check if this intuition is correct, we will first create some
definitions so that our Promises can look a bit more like Haskell's
monads:

    const _return = <A extends any>(x: A): Promise<A> => Promise.resolve(x);
    const _right = _return;
    const _left = (e: any) => Promise.reject(e);
    const _bind = <A extends any, B extends any>(p: Promise<A>, f: ((x: A) => Promise<B>)) => p.then(f);
    const _withLeft = <E extends any, B extends any>(p: Promise<any>, f: ((e: E) => Promise<B>)) => p.catch(f);

This gives us constructors for the different `Either` cases `_left` and
`_right`, as well as our monad functions `_return` and `_bind`. We also
create `_withLeft` which allows us to handle the error cases.

To make this slightly more ergonomic to write, we will also create some
helper functions whose uses should become clear in the examples.

    async function assertEq(p1: Promise<any>, p2: Promise<any>) {
      const [a, b] = await Promise.all([p1, p2]);
      if(a !== b) {
        throw new Error(`Assertion Failure: ${a} /= ${b}`);
      }
    }
    async function seq(xs: (() => Promise<any>)[]) {
      return xs.reduce((c, x) => c.then(x), Promise.resolve());
    }
    const pipe =  (first: any, ...rest: ((x: any) => any)[]) => {
      return rest.reduce((x, f) => f(x), first);
    }
    const curry2 = <A extends any, B extends any, C extends any>
      (f: (x: A, y: B) => C) => (x: A) => (y: B) => f(x, y);
    const flip = <A extends any, B extends any, C extends any>
      (f: (x: A) => ((y: B) => C)) => (y: B) => (x: A) => f(x)(y);
    const _bind1 = flip(curry2(_bind));
    const _withLeft1 = flip(curry2(_withLeft));
    const mLog = (...args) => { console.log(...args); return _return(null); }

Let's check that we can actually apply the monad laws:

    // First, left identity
    //
    async function testFirstMonadLaw() {
      const f = (x: number) => _return(x + 1);
      const a = 5;
      console.log("Testing left identity (first monad law)");
      await assertEq( pipe(_return(a), _bind1(f)) // return a >>= f
                    , f(a));
    };

    // Second, right identity
    //
    async function testSecondMonadLaw() {
      const m = _return(42);
      console.log("Testing right identity (second monad law)");
      await assertEq( m
                    , pipe(m, _bind1(_return))); // m >>= return
    };

    // Third, associativity
    //
    async function testThirdMonadLaw() {
      const m = _return(2.712);
      const g = (x: number) => _return(x - 1);
      const f = (x: number) => _return(x * 2);
      console.log("Testing associativity (third monad law)");
      await assertEq( pipe(pipe(m, _bind1(g)), _bind1(f)) // (m >>= g) >>= f
                    , _bind(m, (x) => _bind(g(x), f)));   // m >>= \x -> g x >>= f
    }

    // Let's run them:
    seq([testFirstMonadLaw, testSecondMonadLaw, testThirdMonadLaw]);

These all pass just fine, so we can actually treat `Promise` as a monad as it
satisfies the monad laws and has the right properties.

## Is this useful?

While it is quite satisfying to demonstrate that the initial intuition
is correct, and that `Promise` can be treated as a monad, it would be
nice to also have a practical benefit to this realisation.

The main ergonomic boon highlighted above with respect to the `Either`
monad is that we can chain computations together without worrying about
handling the error state until the end. There are some limitations
to this in js, because the type system is not very strict (even with
typescript), and there is a mixture of error-handling systems (e.g.
mixing `throw` into a `Promise`-based system entails some extra handling
if you want to avoid "unhandled exception" notices).

We can demonstrate this with our new syntax:

    async function errorStatePropagation() {
      console.log("Demonstrating error state propagation");
      const f = (a: number) =>
        pipe( _return(a)
            , _bind1((x: number) => _return(x * 2))
            , _bind1((x: number) => x < 10 ? _right(x) : _left("too big"))
            , _bind1((x: number) => _return("Got " + x))
            , _withLeft1((e: string) => _return("Failed with " + e))
            );
      // This is the equivalent of:
      //   f a = either ((<>) "Failed with ") ((<>) "Got " . show) $
      //           return a
      //           >>= return . (* 2)
      //           >>= \x -> if x < 10 then Right x else Left "too big"
      await assertEq(f(3), _return("Got 6"));
      await assertEq(f(7), _return("Failed with too big"));
    }

This works, but getting the types right on `_bind1` and `_withLeft1` is
tricky, meaning that the typescript compiler might not be very helpful
for avoiding mistakes.

Whether we use this Haskell-esque syntax or the original `Promise`
syntax, however, I think that representing computations in this was
(monadically) is much more ergonomic for a programmer, rather than
relying on nested `try ... catch` clauses which are required for
`async/await`.

There is an executable script version of all the code in this exploration
[on Github](https://gist.github.com/gfarrell/d2504d12f85e80a8cc2933d587d320b3),
in case you want to play around with it yourself.
