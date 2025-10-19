// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "free.h"

// [[Rcpp::export]]
List lowlevel_diffn(const List &words, const NumericVector &coeffs,
                    const NumericVector &r
    ){
    const freealg F = prepare(words, coeffs);
    return retval(diffn(F, r));
}

// [[Rcpp::export]]
List lowlevel_simplify(const List &words, const NumericVector &coeffs){
    const freealg F = prepare(words,coeffs);
    return retval(F);
}

// [[Rcpp::export]]
List lowlevel_free_prod(
               const List &words1, const NumericVector &coeffs1,
               const List &words2, const NumericVector &coeffs2
              ){

    const freealg F1 = prepare(words1, coeffs1);
    const freealg F2 = prepare(words2, coeffs2);
    return retval(product(F1, F2));
}

// [[Rcpp::export]]
List lowlevel_free_sum(
              const List &words1, const NumericVector &coeffs1,
              const List &words2, const NumericVector &coeffs2
              ){
    const freealg F1 = prepare(words1, coeffs1);
    const freealg F2 = prepare(words2, coeffs2);
    return retval(sum(F1, F2));
}

// [[Rcpp::export]]
List lowlevel_free_power(
              const List &words, const NumericVector &coeffs,
              const NumericVector &n
              ){
    const freealg F = prepare(words, coeffs);
    return retval(power(F, n[0]));
}

