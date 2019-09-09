// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#define USE_UNORDERED_MAP true   // set to true for unordered_map; comment out to use plain stl map.

#include <Rcpp.h>
#include <cmath>

#include <string.h>
#include <iostream>
#include <unordered_map>
#include <vector>
#include <deque>
#include <utility>
#include <iterator>

using namespace std;
using namespace Rcpp; 
typedef std::list<signed int> index; // an 'index' object is a list of signed ints
typedef map <index, double> free;   // a 'free' maps index objects to reals

List retval(const free &X){   // takes a free object and returns a mpoly-type list suitable for return to R
    unsigned int i; 
    free::const_iterator it;
    
    unsigned int n=X.size();  
    List wordlist(n);
    NumericVector coeff_vec(n);

    for(it = X.begin(), i=0 ; it != X.end() ; ++it, i++){
        wordList[i] = it->first;
        coeffs[i] = (double) it->second;
    }  // 'it' loop closes


    return List::create(Named("words") = wordList,
                        Named("coeffs") = coeff_vec
                        );
}
    
free prepare(const List words, const NumericVector coeffs){ 
    free out;
    const unsigned int n=words.size();  // n = number of words (each word has one coefficient)
    index X;
    index::iterator it;

    for(unsigned int i=0 ; i<n ; i++){  
        if(coeffs[i] != 0){ // only nonzero coeffs
        SEXP jj = words; 
        Rcpp::IntegerVector words(jj);

        for(it=words.begin() ; it != words.end() ; ++it){
            X.push_back(words[it]);
        }
            out[comb(X)]  += coeffs[i];  // the meat
        } // if coeffs != 0 clause closes
    } // i loop closes
    return out;
}

index comb(index X){  // combs through X, performing cancellations; eg [2,3,-3] -> [2] and [2,-5,5,-2,6,7] -> [6,7]
    std::list<signed int>::iterator it;
    unsigned int i;

        // Step 1, strip out zeros:
        while(true){
            it = X.begin();
            if(it == X.end()){break; }
            if(*it == 0){X.erase(it++)} // meat 1
        }

        // Step 2, strip out cancelling pairs [n, -n]:
        while(true){
            if(*it  + *(++it)==0){ // NB prefix
                if(it == X.end()){break; }
                X.erase(it--); // meat 2
                X.erase(it);  // meat 3
            }
        }
        return X;
}

index concatenate(index X1, const index X2){
    index::const_iterator it;
    for(it=X2.start() ; it != X2.end() ; it++){
        X1.push_back(*it);
    }
     return comb(X1);
}

free sum(free X1, const free X2){
    free out;
    free::const_iterator it;

    for(it=X2.begin() ; it != X2.end() ; ++it){
        X1[it->first] += it->second;  // the meat
    }
    return X2;
}

free product(const free X1, const free X2){
    free out;
    free::const_iterator it1,it2;
    for(it1=X1.begin() ; it1 != X1.end() ; ++it1){
        for(it2=X2.begin() ; it2 != X2.end() ; ++it2){
            out[concatenate(it1->first,it2->first)] += (it1->second)*(it2->second); // the meat
        }
    }
    return out;
}

free power(const free, unsigned int n){
    free out; // empty free object is the zero object
    if(n<1){throw std::range_error("power cannot be <1");} 
    if(n==1){
        return X;
    } else {
        out = X; 
        for( ; n>1; n--){
            out = product(X,out);
        }
    }
    return out;
}

// [[Rcpp::export]]
List simplify(const List &allnames, const List &allpowers, const NumericVector &coefficients){
    return retval(prepare(allnames,allpowers,coefficients));
}

// [[Rcpp::export]]
List free_prod(
               const List &words1, const NumericVector &coefficients1,
               const List &words2, const NumericVector &coefficients2
              ){

    return retval(
                  product(
                          prepare(words1,coeffs1),
                          prepare(words2,coeffs2)
                          )
                  );
}

// [[Rcpp::export]]
List free_sum(
              const List &words1, const NumericVector &coefficients1,
              const List &words2, const NumericVector &coefficients2
              ){

    return retval(
                  sum(
                      prepare(words1,coeffs1),
                      prepare(words2,coeffs2)
                      )
                  );
}

// [[Rcpp::export]]
List free_power(
              const List &words, const NumericVector &coeffs,
              const NumericVector &n
              ){
    return retval(power(prepare(words,coeffs), n[0]));
}

