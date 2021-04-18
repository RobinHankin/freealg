// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#define USE_UNORDERED_MAP true   // set to true for unordered_map; comment out to use plain stl map.

#include <Rcpp.h>


using namespace std;
using namespace Rcpp; 
typedef std::list<signed int> word; // a 'word' object is a list of signed ints
typedef map <word, double> freealg; // a 'freealg' maps word objects to reals

List retval(const freealg &X){   // takes a freealg object and returns a mpoly-type list suitable for return to R
    int i,j;
    const int n=X.size();   // n is the number of terms
    List indexList(n);
    NumericVector coeffs(n);
    word::const_iterator ic;
    freealg::const_iterator it;

    for(it = X.begin(), i=0 ; it != X.end() ; ++it, i++){

        coeffs[i] = (double) it->second;
        const word f = it->first;
        const int r = f.size();
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
    
word comb(word w){  // combs through w, performing cancellations; eg [2,3,-3] -> [2] and [2,-5,5,-2,6,7] -> [6,7]
    word::iterator it;
    word::const_iterator current,next;
    it = w.begin();
    while(it != w.end()){
        if(*it == 0){
            it = w.erase(it);  // meat A (erases zero, increments 'it')
        } else {
            it++;  // increment anyway
        }
    }  // while loop closes

    it = w.begin();        // Step 2, strip out cancelling pairs [n, -n]:
    while(it != w.end()){
        current = it;
        ++it;
        next = it;
        if(it != w.end()){
            if(((*current) + (*next))==0){ 
                it = w.erase(current); // meat B
                it = w.erase(it);      // meat C
                it = w.begin();
            }
        }
    }
    return w;
}

freealg prepare(const List words, const NumericVector coeffs){ 
    freealg out;
    const int n=words.size();  // n = number of words (each word has one coefficient)

    for(int i=0 ; i<n ; i++){  
        if(coeffs[i] != 0){ // only nonzero coeffs
        SEXP jj = words[i]; 
        Rcpp::IntegerVector words(jj);
        word w;
        for(int j=0 ; j<words.size() ; ++j){

            w.push_back(words[j]);
        }
        const word cw = comb(w);
        out[cw] += coeffs[i];  // the meat
        if(out[cw] == 0){out.erase(cw);}
        } // if coeffs != 0 clause closes
    } // i loop closes
    return out;
}

word concatenate(word w1, const word w2){ 
    word::const_iterator it;
    for(it=w2.begin() ; it != w2.end() ; it++){
        w1.push_back(*it);
    }
     return comb(w1);
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

freealg power(const freealg X, int n){
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

freealg diff1(const freealg X, const int r){  // dX/dx_r
    freealg out; // empty freealg object is the zero object

    for(freealg::const_iterator it=X.begin() ; it != X.end() ; ++it){
        word w = it->first;  //cannot be const word w because we need w.begin()
        const double c = it->second;
        int i,j;
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
    for(int i=0 ; i<r.size() ; ++i){
        X=diff1(X,(int) r[i]);
    }
    return X;
}

freealg multiply_pre_and_post(const freealg Y, const word left, const word right){

    freealg out;

    for(freealg::const_iterator it=Y.begin() ; it != Y.end() ; ++it){
        const word w = it->first;  
        word wnew = w;
        const double coeff = it->second;

        for(auto ww=left.begin() ; ww != left.end() ; ++ww){
            wnew.push_front(*ww);
        }

        for(auto ww=right.begin() ; ww != right.end() ; ++ww){
            wnew.push_back(*ww);
        }

        out[wnew] += coeff; // coefficient of w
    }
    return out;
}

freealg::iterator find_first_zero(freealg &X){
    freealg::iterator it; // NB scope must extend out of for() loop
    for(it=X.begin() ; it != X.end() ; ++it){
        word w=it->first;
        for(word::const_iterator iw=w.begin() ; iw != w.end() ; ++iw){
            if(*iw == 0){
                return it; // 'it' points to a zero, if there is one...
            } // iw loop closes
        }
    }
    return it; //... and if there isn't, then it points to the end
}

freealg change_r_for_zero(const freealg &X, const int &r){
    freealg Xout;
    for(freealg::const_iterator it=X.begin() ; it != X.end() ; ++it){
        const word w = it->first;
        word wcopy = w;
        word::iterator iwc = wcopy.begin();
        for(
            word::const_iterator iw = w.begin(); iw != w.end() ; ++iw, ++iwc){
            if( (*iw) == r) { // if we find an 'r'...
                *iwc = 0;    // ... set it to zero in wcopy
            }
        }
        Xout[wcopy] += it->second;
    } // Xz iteration closes; words (keys) of Xz now have 0 in place of r
    return Xout;
}

freealg subs(const freealg X, const freealg Y, const NumericVector r){
    freealg out,Xz;
    freealg::const_iterator iz;

    // We know the words of X have no no zeros, so first we substitute
    // r[0] for 0:
    Xz = change_r_for_zero(X,r[0]);
    
    // Now 'Xz' has zeros; we substitute the zeros for Y

    while(find_first_zero(Xz) != Xz.end()){ // that is, while there is a zero...
        freealg::iterator p=find_first_zero(Xz);

        word w = p->first;
        const double coeff = p->second;
        int i=0;
        for(word::const_iterator iw = w.begin() ; iw != w.end() ; ++iw){// increment i at end of loop
            if( (*iw) == 0) { // found a zero!
                Xz.erase(w);  // get rid of the original word in Xz (NB not Xz[w]=0)
                word wleft, wright;  // NB i might be 0
                int j=0;
                word::iterator jw;  // scope of jw needs to extend after the for loop
                for(jw=w.begin() ; j<i ; ++j, ++jw){
                    wleft.push_front(*jw); // populate wleft...
                }

                ++jw;  //... skip the zero...
                
                //...and populate wright   
                for(; jw != w.end() ; ++jw){
                    wright.push_back(*jw);
                }

                freealg temp = multiply_pre_and_post(Y,wleft,wright);
                for(freealg::iterator itemp=temp.begin() ; itemp !=temp.end() ; ++itemp){
                    Xz[itemp->first] += (itemp->second)*coeff ; // Put the expansion back in Xz
                }
                break;  // that is, break out of the iw loop
            } // if(found_a_zero) closes
            ++i; //
        }   // iw for loop closes
    } // main "while(find_first_zero())" loop closes.
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

