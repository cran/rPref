
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

#include "scalagon.h" // Includes BNL, pref classes and Scalagon

// Scalagon/BNL Wrapper for parallel and non-parallel TOP-(LEVEL-)k SELECTION


// --------------------------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------------------------

// for non-grouping topk and grouping topk without levels
class Psel_worker_top : public Worker {
public:
   
  // input index vectors to read from
  std::vector< std::vector<int> > vs;
  
  // Preference
  ppref p;
  
  // output lists to write to
  std::vector< std::list<int> > results;
  
  // alpha value for Scalagon Scaling Factor 
  double alpha;
  
  // TOP-k settings
  topk_setting ts;
  
  // Sample indices
  std::vector< std::vector<int> > samples_ind;
  
  // initialize from Rcpp input and output matrixes (the RMatrix class
  // can be automatically converted to from the Rcpp matrix type)
  Psel_worker_top(std::vector< std::vector<int> >& vs, ppref p, int N, double alpha, 
    topk_setting& ts, std::vector< std::vector<int> >& samples_ind) : 
    vs(vs), p(p), results(N), alpha(alpha), ts(ts), samples_ind(samples_ind) { }
   
   // function call operator that work for the specified range (begin/end)
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t k = begin; k < end; k++) {
      scalagon scal_alg(true);
      scal_alg.sample_ind = samples_ind[k];
      // Levels make no sense in parallel runs! Take only the indices here (first member of flex_list)
      results[k] = scal_alg.run_topk(vs[k], p, ts, alpha, false).first; 
    }
  }
};


// --------------------------------------------------------------------------------------------------------------------------------

// for grouping topk with levels (parallel computation of final results WITH level)
class Psel_worker_top_level : public Worker {
public:
   
  // input index vectors to read from
  std::vector< std::vector<int> > vs;
  
  // Preference
  ppref p;
  
  // output lists to write to
  std::vector< pair_list > results;
  
  // alpha value for Scalagon Scaling Factor 
  double alpha;
  
  // TOP-k settings
  topk_setting ts;
  
  // Sample indices
  std::vector< std::vector<int> > samples_ind;
  
  // initialize from Rcpp input and output matrixes (the RMatrix class
  // can be automatically converted to from the Rcpp matrix type)
  Psel_worker_top_level(std::vector< std::vector<int> >& vs, ppref p, int N, double alpha,
                        topk_setting& ts, std::vector< std::vector<int> >& samples_ind) : 
    vs(vs), p(p), results(N), alpha(alpha), ts(ts), samples_ind(samples_ind) { }
   
   // function call operator that work for the specified range (begin/end)
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t k = begin; k < end; k++) {
      scalagon scal_alg(true);
      scal_alg.sample_ind = samples_ind[k];
      // Levels are true in this class!
      results[k] = scal_alg.run_topk(vs[k], p, ts, alpha, true).second; // second is pair_list
    }
  }
};


// --------------------------------------------------------------------------------------------------------------------------------


// Parallel NON-grouped preference TOP K selection
// ===============================================

// (NON-grouped!) subdivide dataset in N parts

// [[Rcpp::export]]
DataFrame pref_select_top_impl(DataFrame scores, List serial_pref, int N, double alpha, 
                               int top, int at_least, int toplevel, bool and_connected, bool show_levels) {
  
  NumericVector col1 = scores[0];  
  int ntuples = col1.size();
  
  if (ntuples == 0) return(DataFrame::create(Named(".indices") = NumericVector(),
                                             Named(".level")   = NumericVector()));
  
  topk_setting ts(top, at_least, toplevel, and_connected);
  
  // De-Serialize preference
  ppref p = CreatePreference(serial_pref, scores);  
  
  // Result list
  flex_list res;
  
  // Scalagon instance
  scalagon scal_alg;
  
  // Execute algorithm for non-parallel case
  if (N == 1) {
    
    // Create index vector
    std::vector<int> v(ntuples);
    for (int i = 0; i < ntuples; i++) v[i] = i;
    
    res = scal_alg.run_topk(v, p, ts, alpha, show_levels); // res is flex_list
  
  } else { // N > 1, parallel case
  
    // Tuples per partition
    int tuples_part = std::ceil(1.0 * ntuples / N);
    
    // Actual number of partitions (N_parts < N for very small numbers of ntuples like ntuples = 5)
    int N_parts = std::ceil(1.0 * ntuples / tuples_part);
  
    // Create N_parts index vectors (for parallelization)
    std::vector< std::vector<int> > vs(N_parts);
    std::vector< std::vector<int> > samples_ind(N_parts);
  
    int count = 0;
    for (int k = 0; k < N_parts; k++) {
      int local_n;
      if (k == N_parts-1) local_n = ntuples - count;
      else                local_n = tuples_part;
      
      samples_ind[k] = get_sample(local_n); // Sample indices for this partition
      vs[k] = std::vector<int>(local_n);
      for (int i = 0; i < local_n; i++) {
        vs[k][i] = count;
        count++;
      }
    }
    
    // Create worker and execute parallel
    Psel_worker_top worker(vs, p, N_parts, alpha, ts, samples_ind);
    parallelFor(0, N_parts, worker);
    
    std::list<int> list_merged;
    
    // Clue together
    for (int k=0; k<N_parts; k++) 
      list_merged.splice(list_merged.end(), worker.results[k]);
    
    // Merge and execute top k Scalagon/BNL again, potentially WITH LEVELS
    std::vector<int> vec_merged(list_merged.begin(), list_merged.end());
    res = scal_alg.run_topk(vec_merged, p, ts, alpha, show_levels); // res is flex_list
  }
  
  if (!show_levels) {
    // Return just indices (first member of flex_list)
    return(DataFrame::create(Named(".indices") = NumericVector(res.first.begin(), res.first.end())));
  } else {
    int nres = res.second.size();
    int count = 0;
    NumericVector ind(nres);
    NumericVector levels(nres);
    
		for (pair_list::iterator j = res.second.begin(); j != res.second.end(); ++j) {
      levels[count] = j->first;
      ind[count] = j->second;
      count++;
		}

    return(DataFrame::create(Named(".indices") = ind,
                             Named(".level")   = levels));
  }
  
}



           
// --------------------------------------------------------------------------------------------------------------------------------

// Parallel grouped preference TOP K selection
// ===========================================

// Grouped preference evaluation, based on Groups from dplyr
// Groups are given via indices list, calculated in dplyr

// [[Rcpp::export]]
DataFrame grouped_pref_sel_top_impl(List indices, DataFrame scores, List serial_pref, int N, double alpha, 
                                        int top, int at_least, int toplevel, bool and_connected, bool show_levels) {
  
  int nind = indices.length(); // Number of groups
  
  if (nind == 0) return(DataFrame::create(Named(".indices") = NumericVector(),
                                          Named(".level")   = NumericVector()));
  
  topk_setting ts(top, at_least, toplevel, and_connected);
  
  ppref p = CreatePreference(serial_pref, scores);
  
  scalagon scal_alg;
  
  // Compose indices for parallel case (for show_levels \in {FALSE, TRUE})
  std::vector< std::vector<int> > vs;
  std::vector< std::vector<int> > samples_ind;
  if (N > 1) { // vs is only used for the parallel case!
    vs          = std::vector< std::vector<int> >(nind);
    samples_ind = std::vector< std::vector<int> >(nind);
    for (int i = 0; i < nind; i++) {
      vs[i] = as< std::vector<int> >(indices[i]);
      samples_ind[i] = get_sample(vs[i].size()); // Sample indices for this partition
    }
  }
  
  DataFrame result_df;

  if (!show_levels) {
    
    // Do not show levels - Return just indices 
    // ----------------------------------------
    
    std::list<int> res;
    if (N > 1) { // parallel case - process groups in parallel
    
      // Create worker
      Psel_worker_top worker(vs, p, nind, alpha, ts, samples_ind); 
      
      // Execute parallel
      parallelFor(0, nind, worker);
      
      // Clue together
      for (int i=0; i<nind; i++) 
        res.splice(res.end(), worker.results[i]);
      
    } else { // non parallel case
    
      for (int i=0; i<nind; i++) {
        std::vector<int> group_indices = as< std::vector<int> >(indices[i]);
        std::list<int> tres = scal_alg.run_topk(group_indices, p, ts, alpha, false).first; // no levels
        res.splice(res.end(), tres);
      }
    }
    
    result_df = DataFrame::create(Named(".indices") = NumericVector(res.begin(), res.end()));
    
  } else {
    
    // Show levels - Return indices+levels
    // -----------------------------------
    
    pair_list res;
    if (N > 1) { // parallel case - process groups in parallel
    
      // Create worker for top-k WITH levels
      Psel_worker_top_level worker(vs, p, nind, alpha, ts, samples_ind); 
      
      // Execute parallel
      parallelFor(0, nind, worker);
      
      // Clue together
      for (int i = 0; i < nind; i++) 
        res.splice(res.end(), worker.results[i]);
      
    } else { // non parallel case
    
      for (int i=0; i<nind; i++) {
        std::vector<int> group_indices = as< std::vector<int> >(indices[i]);
        pair_list tres = scal_alg.run_topk(group_indices, p, ts, alpha, true).second; // no levels
        res.splice(res.end(), tres);
      }
    }
    
    int nres = res.size();
    int count = 0;
    NumericVector ind(nres);
    NumericVector levels(nres);
    
  	for (pair_list::iterator j = res.begin(); j != res.end(); ++j) {
      levels[count] = j->first;
      ind[count] = j->second;
      count++;
		}

    result_df = DataFrame::create(Named(".indices") = ind,
                                  Named(".level")   = levels);
  }
    
  
  // Return result
  return(result_df);
}


