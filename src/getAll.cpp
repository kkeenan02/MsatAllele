#include <Rcpp.h>
using namespace Rcpp;

// getAllele C++
// This function replicates the get.allele function from the MsatAllele package

// [[Rcpp::export]]
List getAll(NumericVector fragRef , double frag, double limit){
  int n = fragRef.size();
  LogicalVector all = fragRef.size();
  for(int i = 0; i < n; i++){
    if((fragRef[i] >= frag - limit) & (fragRef[i] <= frag + limit)){
      all[i] = TRUE;
    } else {
      all[i] = FALSE;
    }
  }
  NumericVector nw = fragRef[all];
  //return nw;
  int nwn = nw.size();
  int i = 0;
  for(int j = 0; j < nwn; j++){
    double dif = nw[j+1] - nw[j];
    if(dif >= (limit/2.0)){
      break;
    } else {
     i = i + 1; 
    }
  }
  return List::create(
    _["idx"] = i,
    _["range"] = nw
  );
}