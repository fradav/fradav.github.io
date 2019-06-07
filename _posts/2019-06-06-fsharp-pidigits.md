---
layout: post
title:  "Adventures on pidigits benchmark and F#"
date:   2019-06-06 05:14:39 +0200
categories: benchmark languages fsharp
comments: true
---


In my previous post regarding the pidigits benchmark comparison, I've been a little unfair towards F#, which I really like. Now it's time to amend.

## Background
It's all [there, on the benchmarksgame site](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/pidigits.html#pidigits).

> Adapt the step-by-step algorithm given on pages 4,6 & 7 of [pdf 156KB] [Unbounded Spigot Algorithms for the Digits of Pi](http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/spigot.pdf). (Not the deliberately obscure version given on page 2. Not the Rabinowitz-Wagon algorithm.) 

All right, it's based on this formula:

$$\frac{\pi}{4}=\sum_{i=0}^{\infty} \frac{(-1)^{i}}{2 i+1}$$

And then 
$$\pi=2+\frac{1}{3}\left(2+\frac{2}{5}\left(2+\frac{3}{7}\left(\cdots\left(2+\frac{i}{2 i+1}(\cdots)\right)\right)\right)\right)$$

And then, quoting the paper, we can see this as 
$$\pi=\left(2+\frac{1}{3} \times\right)\left(2+\frac{2}{5} \times\right)\left(2+\frac{3}{7} \times\right) \cdots\left(2+\frac{i}{2 i+1} \times\right) \cdots$$
which is no other than the composition of a dreadful _inﬁnite series of linear fractional transformations or Möbius transformations_ (you never guessed it.)

>  These are functions taking $x$ to $\frac{q x+r}{s x+t}$ for integers $q,r,s$ and $t$ with $q t-r s \neq 0$—that is, yielding a ratio of integer-coefﬁcient linear transformations of $x$. Such a transformation can be represented by the four coefﬁcients $q,r,s$ and $t$, and if they are arranged as a matrix $\left(\begin{array}{ll}{q} & {r} \\ {s} & {t}\end{array}\right)$ then function composition corresponds to matrix multiplication. 

## Code
Long story short, a couple of simplifications and factorizations later, we got this beautiful haskell code :

```haskell
-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
-- by Don Stewart, Einar Karttunen and Branimir Maksimovic
--

import System.Environment

data F = F !Integer !Integer !Integer !Integer

main = loop 10 0 . flip take (str (F 1 0 0 1) ns) . read . head =<< getArgs

ns = [ F k (4*k+2) 0 (2*k+1) | k <- [1..] ]

loop n s []     = putStrLn $ replicate n ' ' ++ "\t:" ++ show s
loop 0 s xs     = putStrLn ("\t:"++show s) >> loop 10 s xs
loop n s (x:xs) = putStr (show x)          >> loop (n-1) (s+1) xs

flr  x           (F q r s t) = (q*x + r) `div` (s*x + t)
comp (F q r s t) (F u v w x) = F (q*u+r*w) (q*v+r*x) (s*u+t*w) (s*v+t*x)

str z (x:xs) | y == flr 4 z = y : str (comp (F 10 (-10*y) 0 1) z) (x:xs)  
             | otherwise    =     str (comp z x) xs     where y = flr 3 z
```

## Some details
Bear in mind that, with this algorithm, somewhere at the fourth or the fifth decimal we already reached the numeric limit of an unsigned 64 bits integer in `z`. So the implementation should handle "no-limit" integers, known as _big integers_.

Some remarks on the code:

- `.` is composition in haskell.
- `flip` is just there to exchange arguments in the composition function for the `take` function (don't bother with it)

More important :
- the type `F` stores 4 `Integer` (they are big integers in haskell by default)
- `ns` is a infinite loop feeding the algorithm 
- `flr` is the digit extraction
- `comp` does the Möbius thing. 
- `str` is the main recursive loop where `z` is the state matrix and `x:xs` the pattern-matched input list. It checks if the extracted digit on z has the property of being the next "pi" digit. If so don't consume input, update the state for base-10, otherwise consume it and do the Möbius-thing next.
- `loop` does the printing, only printing digits by `n`-sized groups and printing the number of already computing so far.
- `main` puts everything in motion, gets the command-line (number of asked digits), launchs the recursive `str` loop with the initial state and prints it as it goes with the composed `loop` function.

## Ok, now let's do it in F# #

First, some utility functions. This is to measure the computation time


```fsharp
let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let oldout = System.Console.Out 
    System.Console.SetOut(System.IO.TextWriter.Null) // Redirect the output
    let returnValue = f()
    System.Console.SetOut(oldout)
    let sec = (float timer.ElapsedMilliseconds) / 1000.0
    sec
```

And this is to plot a bar chart of the computation times wrt the number of digits


```fsharp
// #load "XPlot.Plotly.Paket.fsx"
#load "XPlot.Plotly.fsx"

open XPlot.Plotly

let plot f range =
    range
    |> Seq.head
    |> (fun x -> duration (fun _ -> f x)) 
    |> ignore // Dry-run the first run to filter out the jit-compilation time on the next ones
    let data = 
        range
        |> Seq.map (fun x -> (x,duration (fun _ -> f x)))
    let c =
        Chart.Bar data
        |> Chart.WithXTitle "Time in seconds"
        |> Chart.WithYTitle "Number of digits"
    c
```


<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

<script type="text/javascript">
var require_save = require;
var requirejs_save = requirejs;
var define_save = define;
var MathJax_save = MathJax;
MathJax = require = requirejs = define = undefined;
/**
* plotly.js v1.47.4
* Copyright 2012-2019, Plotly, Inc.
* All rights reserved.
* Licensed under the MIT license
*/
require = require_save;
requirejs = requirejs_save;
define = define_save;
MathJax = MathJax_save;
function ifsharpMakeImage(gd, fmt) {
    return Plotly.toImage(gd, {format: fmt})
        .then(function(url) {
            var img = document.createElement('img');
            img.setAttribute('src', url);
            var div = document.createElement('div');
            div.appendChild(img);
            gd.parentNode.replaceChild(div, gd);
        });
}
function ifsharpMakePng(gd) {
    var fmt =
        (document.documentMode || /Edge/.test(navigator.userAgent)) ?
            'svg' : 'png';
    return ifsharpMakeImage(gd, fmt);
}
function ifsharpMakeSvg(gd) {
    return ifsharpMakeImage(gd, 'svg');
}
</script>



### First attempt, tail recursion woes


```fsharp
type F = F of bigint * bigint * bigint * bigint

let ns = 
    Seq.unfold (fun k -> Some(F (k, 4I * k + 2I, 0I, 2I*k + 1I),k+1I) ) 1I

let rec loop = function
    | n, s, l when Seq.isEmpty l  -> printfn "%s\t:%d" (String.replicate n " ") s
    | 0, s, xs                    -> printfn "\t:%d" s;loop (10,s,xs)
    | n, s, l                     -> printf "%d" (Seq.head l); loop ((n-1),(s+1),(Seq.tail l |> Seq.cache))

let flr x (F (q,r,s,t)) = (q*x + r) / (s*x + t)
let comp (F (q,r,s,t)) (F (u,v,w,x)) = F (q*u + r*w, q*v + r*x, s*u + t*w, s*v + t*x) 

let rec str z l = 
    let y = flr 3I z
    match z,l with
    | z, l when y = flr 4I z -> seq { yield (int y); yield! str (comp (F (10I,-10I*y,0I,1I)) z) l }
    | z, l                   -> str (comp z (Seq.head l)) (Seq.tail l |> Seq.cache)

let mainPi n =
    ns
    |> str (F(1I,0I,0I,1I))
    |> Seq.take n
    |> Seq.cache
    |> (fun x -> loop (10,0,x))
```


```fsharp
mainPi 27
```


    3141592653	:10
    5897932384	:20
    6264338   	:27



Pretty straightforward from the haskell implementation uh? Except that...
if we run 
```fsharp
mainPi 5000
```
we got a `StackOverflowException`.

And that's because the `Seq.tail |> Seq.cache` doesn't trigger the [tail-recursion optimization](https://en.wikipedia.org/wiki/Tail_call) so the cached `Seq.tail` are stored on the stack, over and over. And that's one of our first gotcha in F#: `Seq` are *NOT* lists as in usual functional programming way. Hence, we should ditch the `Seq.tail` thing. F#'s `List` is eager, so it doesn't lazy-evaluate in a nice functionnal "pipelining" way. We could define our `LazyList<T>` to do that, for a start.

### Lazyness (almost) everywhere

Let's define our `LazyList` type with the usual [car/cdr](https://en.wikipedia.org/wiki/CAR_and_CDR) semantics, the only difference with a "normal" list type is the "lazy" cdr. [Documentation for the lazy evaluation in F#](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lazy-expressions). And add some usual "list" functions for good measure.


```fsharp
type LazyList<'a> = 
   | Empty // Nil
   | Cons of 'a * Lazy<LazyList<'a>> with // Cons (Car, Cdr)
   // Generator
   static member unfold (f : 's -> ('a*'s) option) (init : 's) : LazyList<'a> = 
       match f init with
       | None -> Empty 
       | Some (a, s) -> Cons (a, lazy ( LazyList.unfold f s))
   // Selecting n first elements    
   static member take n (l : LazyList<'a>) : LazyList<'a> = 
       match l with
       | Empty -> Empty 
       | Cons (a, t) -> if n = 0 then Empty 
                        else Cons (a, lazy (LazyList.take (n-1) (t.Force())))
```

Now we convert all `Seq<T>` functions to `LasyList<T>`, and we could do a little more pattern matching along the way.


```fsharp
type F = F of bigint * bigint * bigint * bigint

let ns = 
    LazyList.unfold (fun k -> Some(F (k, 4I * k + 2I, 0I, 2I*k + 1I),k+1I) ) 1I

let rec loop = function
    | n, s, Empty       -> printfn "%s\t:%d" (String.replicate n " ") s
    | 0, s, xs          -> printfn "\t:%d" s;loop (10,s,xs)
    | n, s, Cons (x,xs) -> printf "%d" x;loop (n-1,s+1,xs.Force())

let flr x (F (q,r,s,t)) = (q*x + r) / (s*x + t)
let comp (F (q,r,s,t)) (F (u,v,w,x)) = F (q*u + r*w, q*v + r*x, s*u + t*w, s*v + t*x) 

let rec str z l = 
    let y = flr 3I z
    match z,l with
    | z, l when y = flr 4I z -> Cons(int y,lazy(str (comp (F (10I,-10I*y,0I,1I)) z) l))
    | z, Cons(x,xs)          -> str (comp z x) (xs.Force())
    | z, Empty -> str z l 

let mainPi n =
    ns
    |> str (F(1I,0I,0I,1I))
    |> LazyList.take n
    |> (fun x -> loop (10,0,x))
```


```fsharp
mainPi 27
```


    3141592653	:10
    5897932384	:20
    6264338   	:27



The only real changes from the first version with `Seq` is that we now enforce proper tail-recursion via the `Lazy<T>.Force()` call.


```fsharp
plot mainPi [1000..1000..10000]
```




<div id="cb7df023-8f5b-4f94-8b2f-07372980186e" style="width: 900px; height: 500px;"></div>

<script>
    var data = [{"type":"bar","x":[0.076,0.278,0.636,1.224,2.014,2.746,3.454,4.546,5.823,7.284],"y":[1000,2000,3000,4000,5000,6000,7000,8000,9000,10000],"orientation":"h"}];
    var layout = {"xaxis":{"title":"Time in seconds","_isSubplotObj":true},"yaxis":{"title":"Number of digits","_isSubplotObj":true}};
    Plotly.newPlot('cb7df023-8f5b-4f94-8b2f-07372980186e', data, layout);
</script>



And that's a very nice improvement over the `Seq.tail` version. We're still leagues away from the haskell performance, more on that later.

## No, we're not that Lazy

Now we're wondering if we could go a little more F#-idiomatic, without resorting to `LazyList`. First, let's take care of the printing with a nice [`Seq.scan`](https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/seq.scan%5b't,'state%5d-function-%5bfsharp%5d)
> Like `Seq.fold`, but computes on-demand and returns the sequence of intermediate and final results.

```fsharp
// Signature:
Seq.scan : ('State -> 'T -> 'State) -> 'State -> seq<'T> -> seq<'State>

// Usage:
Seq.scan folder state source
```


```fsharp
let unloop n =
    let fmt = Printf.StringFormat<string->int->string>("%-" + (string n) + "s\t:%d\n")
    Seq.map (fun y -> char (48 + y))
    >> Seq.chunkBySize n
    >> Seq.scan (fun (s,_) l -> let s = s + Array.length l
                                (s,sprintf fmt (System.String l) s)) (0,"")
    >> Seq.iter (snd >> System.Console.Write)
```

It first "transforms" digits as bytes, groups them by array of 10 and accumulate them with `Seq.scan` with the number of digits with the current output of ten digits nicely formatted as state and finally just write those outputs to the console
(The `StringFormat` trick is documented [there](https://fsharpforfunandprofit.com/posts/printf/#printf-gotchas))


```fsharp
seq [ 3; 4; 4; 1; 2; 4; 5 ] |> unloop 3
```


    344	:3
    124	:6
    5  	:7
    


Now what if instead of just generating the sequence of `k` consumed by the `comp` function, we move all the computation inside our [`Seq.unfold`](https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/seq.unfold%5b'state,'t%5d-function-%5bfsharp%5d) generator ? As for the unfold function state, we have just to store `k` and `z`
> Returns a sequence that contains the elements generated by the given computation.

```fsharp
// Signature:
Seq.unfold : ('State -> ('T * 'State) option) -> 'State -> seq<'T>

// Usage:
Seq.unfold generator state
```

All we have to do is to store in our state the remaining digits to compute, the `k` and the `z` accumulator. When we got a digit we "yield" a `Some int` otherwise a None. When `n` reachs 0 we stop. In the main call, we filter out all the `None` values.


```fsharp
let str (n,k,z) = // n : remaining digits to compute, k : to be consumed
    let y  = flr 3I z
    let yt = flr 4I z
    match n,y with
    | 0,_             -> None
    | _,y when y = yt -> Some(Some (int y),(n-1,k   ,comp (F(10I,(-10I*y),0I,1I)) z))
    | _,_             -> Some(None        ,(n  ,k+1I,comp z (F(k,(4I*k+2I),0I,(2I*k+1I)))))
    
let mainPi n =
    Seq.unfold str (n,1I,F(1I,0I,0I,1I))
    |> Seq.choose id
    |> unloop 10
```


```fsharp
mainPi 27
```


    3141592653	:10
    5897932384	:20
    6264338   	:27
    


Finally the complete snippet there


```fsharp
type F = F of bigint * bigint * bigint * bigint

let ns = 
    Seq.unfold (fun k -> Some(F (k, 4I * k + 2I, 0I, 2I*k + 1I),k+1I) ) 1I

let unloop n =
    Seq.map (fun y -> char (48 + y))
    >> Seq.chunkBySize n
    >> Seq.scan (fun (s,_) l -> let s = s + Array.length l
                                (s,sprintf "%-10s\t:%d\n" (System.String l) s)) (0,"")
    >> Seq.iter (snd >> System.Console.Write)

let flr x (F (q,r,s,t)) = (q*x + r) / (s*x + t)
let comp (F (q,r,s,t)) (F (u,v,w,x)) = F (q*u + r*w, q*v + r*x, s*u + t*w, s*v + t*x) 
    
let str (n,k,z) = // n : remaining digits to compute, k : to be consumed
    let y  = flr 3I z
    let yt = flr 4I z
    match n,y with
    | 0,_             -> None
    | _,y when y = yt -> Some(Some (int y),(n-1,k   ,comp (F(10I,(-10I*y),0I,1I)) z))
    | _,_             -> Some(None        ,(n  ,k+1I,comp z (F(k,(4I*k+2I),0I,(2I*k+1I)))))
    
let mainPi n =
    Seq.unfold str (n,1I,F(1I,0I,0I,1I))
    |> Seq.choose id
    |> unloop 10
```


```fsharp
plot mainPi [1000..1000..10000]
```




<div id="09273672-f32e-4235-888b-c3a7e6384780" style="width: 900px; height: 500px;"></div>

<script>
    var data = [{"type":"bar","x":[0.079,0.266,0.624,1.185,1.775,2.498,3.427,4.506,5.758,7.179],"y":[1000,2000,3000,4000,5000,6000,7000,8000,9000,10000],"orientation":"h"}];
    var layout = {"xaxis":{"title":"Time in seconds","_isSubplotObj":true},"yaxis":{"title":"Number of digits","_isSubplotObj":true}};
    Plotly.newPlot('09273672-f32e-4235-888b-c3a7e6384780', data, layout);
</script>



And that's about the best we could get at this without... a native bigint implementation. In actual .net core/.net framework, bigint is managed and NOT native. If we want the "native" bigint, we have to go a little hackish.

## Mutable/native land or "when all hell breaks loose"

Let's get the native library (should work on linux/mac/windows there), [doc](https://machinecognitis.github.io/Math.Gmp.Native/)


```fsharp
#load "Paket.fsx"
Paket.Package
    ["Math.Gmp.Native.NET"]
#load "Paket.Generated.Refs.fsx"
```

And now let's get to business.


```fsharp
open Math.Gmp.Native

// magic to get our mutable bigint
let inline init() =
    let mutable result = mpz_t()
    gmp_lib.mpz_init(result)
    result

// That's our z
let mutable q = init()
let mutable r = init()
let mutable s = init()
let mutable t = init()

// temporaries
let mutable u = init()
let mutable v = init()
let mutable w = init()

// initial value F(1,0,0,1)
let reset() =
    gmp_lib.mpz_set_si(q,1)
    gmp_lib.mpz_set_si(r,0)
    gmp_lib.mpz_set_si(s,0)
    gmp_lib.mpz_set_si(t,1)

// z <- comp x z
let inline compR bq br bs bt =
    gmp_lib.mpz_mul_si(u, r, bs)
    gmp_lib.mpz_mul_si(r, r, bq)
    gmp_lib.mpz_mul_si(v, t, br)
    gmp_lib.mpz_add(r, r, v)
    gmp_lib.mpz_mul_si(t, t, bt)
    gmp_lib.mpz_add(t, t, u)
    gmp_lib.mpz_mul_si(s, s, bt)
    gmp_lib.mpz_mul_si(u, q, bs)
    gmp_lib.mpz_add(s, s, u)
    gmp_lib.mpz_mul_si(q, q, bq)

// z <- comp z x
let inline compL bq br bs bt =
    gmp_lib.mpz_mul_si(r, r, bt)
    gmp_lib.mpz_mul_si(u, q, br)
    gmp_lib.mpz_add(r, r, u)
    gmp_lib.mpz_mul_si(u, t, bs)
    gmp_lib.mpz_mul_si(t, t, bt)
    gmp_lib.mpz_mul_si(v, s, br)
    gmp_lib.mpz_add(t, t, v)
    gmp_lib.mpz_mul_si(s, s, bq)
    gmp_lib.mpz_add(s, s, u)
    gmp_lib.mpz_mul_si(q, q, bq)

// flr j z 
let inline flr j =
    gmp_lib.mpz_mul_si(u, q, j)
    gmp_lib.mpz_add(u, u, r)
    gmp_lib.mpz_mul_si(v, s , j)
    gmp_lib.mpz_add(v, v, t)
    gmp_lib.mpz_tdiv_q(w, u, v)
    gmp_lib.mpz_get_si(w)
```

What's all this fuss is about? Now we store as mutable (native) variables the four bigint of our former `z` as a global state. In order to mutate this `z`-state with the result of the `comp` function, we have to provide two differents mutating functions, depending on where `z` is in the `comp` arguments list, on the right or on the left. The `u,v,w` are temporaries. The native functions `gmp_lib.mpz_[add,mul,tdiv,get]`are always storing the results in one of their arguments (generally, the first one, passed by reference), that's a pretty standard API for `C` libraries.

Now let's modify a bit our (un)folder function to account for mutation side effects for `z`.


```fsharp
let str (n,k) = // n : remaining digits to compute, k : to be consumed
    let y  = flr 3 
    let yt = flr 4
    match n,y with
    | 0,_             ->                           None
    | _,y when y = yt -> compR 10 (-10*y) 0 1     ;Some(Some (int y),(n-1,k))
    | _,_             -> compL k (4*k+2) 0 (2*k+1);Some(None        ,(n  ,k+1))
    
let mainPi n =
    reset()
    Seq.unfold str (n,1)
    |> Seq.choose id
    |> unloop 10
```


```fsharp
mainPi 27
```


    3141592653	:10
    5897932384	:20
    6264338   	:27
    



```fsharp
plot mainPi [1000..1000..10000]
```




<div id="4b545cac-7c0c-425f-a2ab-5ebf07c1f152" style="width: 900px; height: 500px;"></div>

<script>
    var data = [{"type":"bar","x":[0.008,0.036,0.087,0.144,0.227,0.33,0.46,0.607,0.784,0.971],"y":[1000,2000,3000,4000,5000,6000,7000,8000,9000,10000],"orientation":"h"}];
    var layout = {"xaxis":{"title":"Time in seconds","_isSubplotObj":true},"yaxis":{"title":"Number of digits","_isSubplotObj":true}};
    Plotly.newPlot('4b545cac-7c0c-425f-a2ab-5ebf07c1f152', data, layout);
</script>



Now, performance-wise, we're in the same ballpark than the top-tier languages, just something like 2x the pure C/C++ timings.

## Final touch

Let's get back to a sequence expression for the sake of comparison


```fsharp
let rec str k =
    seq {
        let y  = flr 3
        let yt = flr 4
        match y with
        | y when y = yt -> compR 10 (-10*y) 0 1     ;yield y; yield! str k
        | _             -> compL k (4*k+2) 0 (2*k+1);         yield! str (k+1)
    }
    
let mainPi n =
    reset()
    str 1
    |> Seq.take n
    |> unloop 10
```


```fsharp
mainPi 27
```


    3141592653	:10
    5897932384	:20
    6264338   	:27
    



```fsharp
plot mainPi [1000..1000..10000]
```




<div id="b8e7a676-9864-42e6-9b14-9ef63616123e" style="width: 900px; height: 500px;"></div>

<script>
    var data = [{"type":"bar","x":[0.009,0.034,0.078,0.144,0.225,0.354,0.477,0.608,0.784,0.969],"y":[1000,2000,3000,4000,5000,6000,7000,8000,9000,10000],"orientation":"h"}];
    var layout = {"xaxis":{"title":"Time in seconds","_isSubplotObj":true},"yaxis":{"title":"Number of digits","_isSubplotObj":true}};
    Plotly.newPlot('b8e7a676-9864-42e6-9b14-9ef63616123e', data, layout);
</script>



## Conclusion

The road from haskell to F#-idiomatic and F#-pragmatic (native collided) is a little bumpy one. We could import high-performance "native" code and still get a nice FP touch over it, thanks to the pragmatic approach of the language.
