---
layout: post
title:  "A taste of C++20 : range-v3 experiments, part I"
date:   2019-06-06 17:14:39 +0200
categories: functional c++ 
comments: true
---

Let's say we have a big matrix of $X\inℝ^{n\times{}m}$ of $n$ samples of dimension $m$, and $Y\inℝ^n$ an output.
We want to be able to iterate over Y through sorted indices of any $j$th column of $X$, $1\leq{}j\leq{}m$. And not storing any duplicate values of $Y$, or $X$ for that matter. We *may* store the sorted indices.

Let's code this with [range v3](https://github.com/ericniebler/range-v3)

First just set the include, the namespace and custom literal I can't live without.

```cpp
#include <vector>
#include <random>
#include <iostream>

#include <range/v3/all.hpp>
    
using namespace ranges;

constexpr std::size_t operator "" _z ( unsigned long long n )
    { return n; }
```

And now... the magic. First I want an indirecting lambda which is a closure on a given (lvalue, for that matter) range and which get the value at the specified index.

```cpp
CPP_template(class Rng)
    (requires Range<Rng>)
auto indirect_lambda(const Rng& v) {
    return [&v](const auto& i){ return v[i]; };
}
```

Notice how we enforced a range concept with the `CPP_template`. And we need an indirect comparison lambda, which does the same thing than our previous lambda, but "functored" with a comparison function. We're defaulting to the `less<>` function if unspecified.

```cpp
template<class Rng>
auto less_rng = std::less<range_value_type_t<Rng>>();

CPP_template(class Rng, class F = decltype(less_rng<Rng>) )
    (requires Range<Rng>)
auto indirect_comparison_lambda (const Rng& v, const F& f = less_rng<Rng>) {
    return [&v,&f](const auto& a, const auto& b) { return f(v[a],v[b]); };
}
```

Ok, nothing too fancy there.

```cpp
int main() {
    std::default_random_engine gen(123456);
    std::uniform_int_distribution dis(-10,10);
    const size_t n = 10;
    const auto& x = view::generate_n([&gen,&dis](){ return dis(gen); },n) | to_vector;
    const auto& y = view::generate_n([&gen,&dis](){ return dis(gen); },n) | to_vector;
    std::cout << "input vector x : " << std::endl;
    std::cout << (x | view::all) << std::endl;
    std::cout << "output vector y : " << std::endl;
    std::cout << (y | view::all) << std::endl;
```
Output:
```plain
input vector x : 
[-9,-10,0,5,-2,4,4,-4,-2,8]
output vector y : 
[-6,-10,-10,-6,-4,-3,-3,-8,8,3]
```

We generated (with a fixed seed for reproductibility purpose) two random vector of 10 signed integer. 

```cpp
    const auto& p = view::iota(0_z,n) 
        | to_vector 
        | action::sort(indirect_comparison_lambda(x));

    std::cout << "Sorted indices of x : " << std::endl ;
    std::cout << (p | view::all) << std::endl;
```
Output:
```plain
Sorted indices of x : 
[1,0,7,4,8,2,5,6,3,9]
```

`p` stores the sorted indices of l. And that's fine.

```cpp
    std::cout << "let's permute y in a lazy-view with the sorted indices of x : " << std::endl;
    const auto& ypermuted = p | view::transform(indirect_lambda(y));
    std::cout << ypermuted << std::endl;
```

Output:

```plain
let's permute y in a lazy-view with the sorted indices of x : 
[-10,-6,-8,-4,8,-10,-3,-3,-6,3]
```

Here comes the magic[^1]. If you paid attention you noticed we pretty much declared *everything* immutable. So when we got `ypermuted`, not only it's doesn't store anything in memory because it's an on-demand iterator based range, but it's doesn't modify any range either.

Now we want to do that in a more general way, and we'll write an ad hoc proxy range for accessing y, through whatever sorted indices of an input x. That's what we'll see in the part II.

[Code](https://github.com/fradav/fradav.github.io/blob/master/assets/cpp/rnagev3-part1.cpp)

[^1]: For the eventual FP-ninjas reading this, I'm aware it's pretty basic, but in C++ land, it's something that a few years ago, we wouldn't have dreamt of being able to do without resorting to a full fledged class writing. 