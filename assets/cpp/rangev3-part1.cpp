#include <vector>
#include <random>
#include <iostream>

#include <range/v3/all.hpp>
    
using namespace ranges;

constexpr std::size_t operator "" _z ( unsigned long long n )
    { return n; }

CPP_template(class Rng)
    (requires Range<Rng>)
auto indirect_lambda(const Rng& v) {
    return [&v](const auto& i){ return v[i]; };
}

template<class Rng>
auto less_rng = std::less<range_value_type_t<Rng>>();

CPP_template(class Rng, class F = decltype(less_rng<Rng>) )
    (requires Range<Rng>)
auto indirect_comparison_lambda (const Rng& v, const F& f = less_rng<Rng>) {
    return [&v,&f](const auto& a, const auto& b) { return f(v[a],v[b]); };
}

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

    const auto& p = view::iota(0_z,n) 
        | to_vector 
        | action::sort(indirect_comparison_lambda(x));

    std::cout << "Sorted indices of x : " << std::endl ;
    std::cout << (p | view::all) << std::endl;

    std::cout << "let's permute y in a lazy-view with the sorted indices of x : " << std::endl;
    const auto& ypermuted = p | view::transform(indirect_lambda(y));
    std::cout << ypermuted << std::endl;

    return 0;
}