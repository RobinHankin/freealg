/ -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

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
typedef std::list<signed int> flindex; // an 'flindex' object is a list of signed ints
typedef map <flindex, double> freealg;   // a 'freealg' maps flindex objects to reals

List retval(const freealg &X){   // takes a freealg object and returns a mpoly-type list suitable for return to R
    unsigned int i; 
    freealg::const_iterator it;
    
    unsigned int n=X.size();  
    List wordList(n);
    NumericVector coeffs(n);

    for(it = X.begin(), i=0 ; it != X.end() ; ++it, i++){
        wordList[i] = it->first;
        coeffs[i] = (double) it->second;
    }  // 'it' loop closes


    return List::create(Named("words") = wordList,
                        Named("coeffs") = coeffs
                        );
}
    
flindex comb(flindex X){  // combs through X, performing cancellations; eg [2,3,-3] -> [2] and [2,-5,5,-2,6,7] -> [6,7]
    std::list<signed int>::iterator it, prev,next;
    unsigned int i;
    it = X.begin();
    while(it != X.end()){
        if(*it == 0){
            it = X.erase(it);  // meat 1
        } else {
            it++;
        }
    }

    it = X.begin();        // Step 2, strip out cancelling pairs [n, -n]:
    while(it != X.end()){
        next = it+1;
        if(next != X.end()){
            if((*it + *next)==0){ 
                it = X.erase(it); // meat B
                X.erase(it); // meat A
                it = X.begin();
            }
        }
    }
    return X;
}

freealg prepare(const List words, const NumericVector coeffs){ 
    freealg out;
    const unsigned int n=words.size();  // n = number of words (each word has one coefficient)
    flindex X;
    flindex::iterator it;

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

flindex concatenate(flindex X1, const flindex X2){
    flindex::const_iterator it;
    for(it=X2.begin() ; it != X2.end() ; it++){
        X1.push_back(*it);
    }
     return comb(X1);
}

freealg sum(freealg X1, const freealg X2){
    freealg out;
    freealg::const_iterator it;

    for(it=X2.begin() ; it != X2.end() ; ++it){
        X1[it->first] += it->second;  // the meat
    }
    return X2;
}

freealg product(const freealg X1, const freealg X2){
    freealg out;
    freealg::const_iterator it1,it2;
    for(it1=X1.begin() ; it1 != X1.end() ; ++it1){
        for(it2=X2.begin() ; it2 != X2.end() ; ++it2){
            out[concatenate(it1->first,it2->first)] += (it1->second)*(it2->second); // the meat
        }
    }
    return out;
}

freealg power(const freealg, unsigned int n){
    freealg out; // empty freealg object is the zero object
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

