// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "free.h"

// [[Rcpp::export]]
List lowlevel_diffn(const List &words, const NumericVector &coeffs,
                    const NumericVector &r
    ){
    return retval(diffn(prepare(words,coeffs), r));
}

// [[Rcpp::export]]
List lowlevel_simplify(const List &words, const NumericVector &coeffs){
    return retval(prepare(words,coeffs));
}

// [[Rcpp::export]]
List lowlevel_free_prod(
               const List &words1, const NumericVector &coeffs1,
               const List &words2, const NumericVector &coeffs2
              ){

    return retval(
                  product(
                          prepare(words1,coeffs1),
                          prepare(words2,coeffs2)
                          )
                  );
}

// [[Rcpp::export]]
List lowlevel_free_sum(
              const List &words1, const NumericVector &coeffs1,
              const List &words2, const NumericVector &coeffs2
              ){

    return retval(
                  sum(
                      prepare(words1,coeffs1),
                      prepare(words2,coeffs2)
                      )
                  );
}

// [[Rcpp::export]]
List lowlevel_free_power(
              const List &words, const NumericVector &coeffs,
              const NumericVector &n
              ){
    return retval(power(prepare(words,coeffs), n[0]));
}

