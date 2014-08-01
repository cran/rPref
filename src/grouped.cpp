#include <Rcpp.h>
using namespace Rcpp;

#include "pref-classes.h"
#include "bnl.h"

// Grouped preference evaluation, based on Groups from dplyr
// We assume that the attribute "indices" of a grouped data frame stores the grouping information!


// --------------------------------------------------------------------------------------------------------------------------------




// Return preference selection for every group
// Grouping is NOT preserved, regrouping is done on the R level!
// Grouped T-K for topk > -1
// [[Rcpp::export]]
NumericVector grouped_pref_sel_impl(DataFrame data, DataFrame scores, List serial_pref, int topk) {
  
  List indices = data.attr("indices");
  int nind = indices.length();
  std::list<int> res;
  
  pref* p = CreatePreference(serial_pref, scores, 0);
  
  if (topk == -1) { // Non-grouped preference selection
  
    for (int i=0; i<nind; i++) {
      std::vector<int> group_indices = as< std::vector<int> >(indices[i]);
      std::list<int> tres = bnl_internal(group_indices, p);
      res.splice(res.end(), tres);
    }
    
  } else {
    
    for (int i=0; i<nind; i++) {
      std::vector<int> group_indices = as< std::vector<int> >(indices[i]);
      std::list<int> tres = bnl_topk_internal(group_indices, p, topk);
      res.splice(res.end(), tres);
    }
  }
  return(NumericVector(res.begin(), res.end()));
  
}


// --------------------------------------------------------------------------------------------------------------------------------

