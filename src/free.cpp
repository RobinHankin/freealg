// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#define USE_UNORDERED_MAP true   // set to true for unordered_map; comment out to use plain stl map.

#include <Rcpp.h>


using namespace std;
using namespace Rcpp; 
typedef std::list<signed int> word; // a 'word' object is a list of signed ints
typedef map <word, double> freealg; // a 'freealg' maps word objects to reals

List retval(const freealg &X){   // takes a freealg object and returns a mpoly-type list suitable for return to R
    unsigned int i,j;
    const unsigned int n=X.size();   // n is the number of terms
    List indexList(n);
    NumericVector coeffs(n);
    word::const_iterator ic;
    freealg::const_iterator it;

    for(it = X.begin(), i=0 ; it != X.end() ; ++it, i++){

        coeffs[i] = (double) it->second;
        const word f = it->first;
        const unsigned int r = f.size();
        IntegerVector index(r);
        for(ic = f.begin(), j=0 ; ic != f.end() ; ++ic, ++j){
            index[j] = (signed int) *ic;
        }
        indexList[i] = index;
    }  // 'it' loop closes

    return List::create(
                        Named("indices") = indexList,
                        Named("coeffs") = coeffs
                        );
}
    
word comb(word X){  // combs through X, performing cancellations; eg [2,3,-3] -> [2] and [2,-5,5,-2,6,7] -> [6,7]
    word::iterator it;
    word::const_iterator current,next;
    it = X.begin();
    while(it != X.end()){
        if(*it == 0){
            it = X.erase(it);  // meat A (erases zero, increments 'it')
        } else {
            it++;  // increment anyway
        }
    }  // while loop closes

    it = X.begin();        // Step 2, strip out cancelling pairs [n, -n]:
    while(it != X.end()){
        current = it;
        ++it;
        next = it;
        if(it != X.end()){
            if(((*current) + (*next))==0){ 
                it = X.erase(current); // meat B
                it = X.erase(it);      // meat C
                it = X.begin();
            }
        }
    }
    return X;
}

freealg prepare(const List words, const NumericVector coeffs){ 
    freealg out;
    const unsigned int n=words.size();  // n = number of words (each word has one coefficient)

    for(unsigned int i=0 ; i<n ; i++){  
        if(coeffs[i] != 0){ // only nonzero coeffs
        SEXP jj = words[i]; 
        Rcpp::IntegerVector words(jj);
        word X;
        for(unsigned int j=0 ; j<words.size() ; ++j){

            X.push_back(words[j]);
        }
        out[comb(X)]  += coeffs[i];  // the meat
        } // if coeffs != 0 clause closes
    } // i loop closes
    return out;
}

word concatenate(word X1, const word X2){ 
    word::const_iterator it;
    for(it=X2.begin() ; it != X2.end() ; it++){
        X1.push_back(*it);
    }
     return comb(X1);
}

freealg sum(freealg X1, const freealg X2){ //X1 modified in place
    freealg out;
    freealg::const_iterator it;
    for(it=X2.begin() ; it != X2.end() ; ++it){
        X1[it->first] += it->second;  // the meat
    }
    return X1;
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

freealg power(const freealg X, unsigned int n){
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

freealg diff1(const freealg X, const unsigned int r){  // dX/dx_r
    freealg out; // empty freealg object is the zero object

    for(freealg::const_iterator it=X.begin() ; it != X.end() ; ++it){
        word w = it->first;  //cannot be const word w because we need w.begin()
        const double c = it->second;
        unsigned int i,j;
        word::iterator iw,iwc;

        for(iw = w.begin(), i=0 ; iw != w.end() ; ++i, ++iw){
            if( (*iw) == r){         // differential matches symbol, same sign
                word wcopy=w;
                word wrem;  // "wrem" = "w with one symbol removed"
                for(iwc = wcopy.begin() , j=0 ; iwc != wcopy.end() ; ++j, ++iwc){
                    if(i != j){wrem.push_back(*iwc);}
                }
                out[wrem] += c;     // The meat.
            } // if(same-sign match) closes...
            if( (*iw) == -r){    // ... so now search for an opposite sign match
                word wcopy=w;
                word wadd;  // "wadd" = "w with one symbol added"
                for(iwc = wcopy.begin() , j=0 ; iwc != wcopy.end() ; ++j, ++iwc){
                    if(i != j){
                        wadd.push_back(*iwc); //do it once
                    } else {
                        wadd.push_back(*iwc); // do it twice
                        wadd.push_back(*iwc);
                    }
                }
                out[wadd] -= c;     // The meat (negative sign)
            } // if(opposite-signe match) closes
        } // word w for() loop closes
    }  //freealg iteration ends;
    return out;
}

freealg diffn(freealg X, const NumericVector r){ // (d^len(r) X)/dr[1]...dr[len(r)]
    for(unsigned int i=0 ; i<r.size() ; ++i){
        X=diff1(X,(unsigned int) r[i]);
    }
    return X;
}

freealg multiply_pre_and_post(const freealg X, const NumericVector left, const NumericVector right){

    freealg out;
    unsigned int i;

    for(freealg::const_iterator it=X.begin() ; it != X.end() ; ++it){
        word w = it->first;  


        word wnew;
        for(i=left.size(); i>0 ; --i){
            w.push_front(left[i]);
        }

        for(i=0; i<right.size(); ++i){
            w.push_back(right[i]);
        }

        out[w] += it->second; // coefficient of w
    }
    return out;
}

freealg::iterator find_first_zero(freealg X){
    freealg::iterator it; // NB scope must extend out of for() loop
    for(it=X.begin() ; it != X.end() ; ++it){
        word w=it->first;
        for(word::const_iterator iw=w.begin() ; iw != w.end() ; ++iw){
            if(*iw == 0){
                return it;
            } // iw loop closes
        }
    }
    return it;
}

freealg change_r_for_zero(const freealg X, const int r){
    freealg Xout;
    freealg::iterator it;
    for(it=X.begin() ; it != X.end() ; ++it){
        word w = it->first;
        word wcopy = w;
        for(
            word::iterator iw  = w.begin(),
            word::iterator iwc = copy.begin() ;
            iw != w.end() ; ++iw, ++iwc){
            if( (*iw) == r) { // if we find an 'r'...
                *iwc = 0;    // ... set it to zero in wcopy
            }
        }
        Xout[wcopy] += it->second;
    } // Xz iteration closes; words (keys) of Xz now have 0 in place of r
    return Xout;
}

freealg subs(const freealg X, const freealg Y, const NumericVector r){
    freealg out,temp,Xz;
    freealg::const_iterator iz;
    unsigned int i;

    // We know the words of X have no no zeros, so first we substitute
    // r[0] for 0:
    Xz = change_r_for_zero(X,r[0]);
    // Now 'Xz' has zeros which should be substituted for Y


    while(find_first_zero(Xz) != Xz.end()){ // that is, while there is a zero...
        freealg::iterator p=find_first_zero(Xz);
        word w = p->first;
        for(word::const_iterator iw = w.begin(), i=0 ; iw != w.end() ; ++iw, ++i){
            if( (*iw) == 0) { // found a zero!
                Xz[w] = 0;  // get rid of the original word in Xz by setting the coeff=0...
                NumericVector left(i), right(w.size()-i-1);  // narrow scope
                for(int j=0, word::iterator jw=w.begin() ; j<i; ++j, ++jw){
                    left.push_back(*jw); // populate left...
                }
                ++jw;  //... skip the zero...
                for(int j=i+1 ; j<w.size(); ++j ,++jw){
                    right.push_back(*jw);//...and populate right
                }
                temp = multiply_pre_and_post(Y,left,right);
                for(freealg::iterator itemp=temp.begin() ; itemp !=temp.end() ; ++itemp){
                    Xz[itemp->first] += itemp->second;// Put the expansion back in Xz
                }
                break;  // that is, break out of the iw loop
            } // if(found_a_zero) closes
        }   // iw for loop closes
    } // main while loop closes.
    // if you are here, there are no zeros in the indices of Xz
    return Xz;
} //function subst() closes

// [[Rcpp::export]]
List lowlevel_subs(
                   const List &words1, const NumericVector &coeffs1,
                   const List &words2, const NumericVector &coeffs2,
                   const NumericVector &r
    ){
    return retval(subs(prepare(words1,coeffs1), prepare(words2,coeffs2),r));
}

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

