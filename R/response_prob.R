#' @title Calculate COMPASS-derived overall responder probability
#'
#' @description Calculate the probability of responding to at least one
#' cytokine combination, assuming independence of the probability estimates
#' between cytokine combinations.
#'
#' @param c_obj object of class 'COMPASSResult'. The posterior probabilities
#' for individual cytokine combinations are obtained here.
#'
#' @details Calculates the probability of an individual responding to
#' at least one cytokine combination as (1-product((1-prob_i))),
#' where prob_i is the probability of responding to the i-th cytokine
#' combination and the product is taken over all cytokine combinations
#' except the all-negative cytokine combination.
#'
#' @return A dataframe with columns sampleid and prob.
#'
#' @examples
#' data('c_obj', package = 'compassutils')
#' response_prob(c_obj = c_obj)
#'
#' @export
response_prob <- function(c_obj){
  prob_mat <- c_obj$fit$mean_gamma
  sampleid_vec <- rownames(prob_mat)
  prob_tbl <- prob_mat %>%
    tibble::as_tibble() %>%
    dplyr::mutate(sampleid = sampleid_vec) %>%
    dplyr::select(sampleid, everything())
  cyt_vec <- stringr::str_split(
    colnames(prob_tbl)[2],
    "&"
  )[[1]]
  cyt_vec <- purrr::map_chr(cyt_vec, function(cyt){
    if(stringr::str_sub(cyt, 1, 1) != "!") return(cyt)
    stringr::str_sub(cyt, start = 2)
  })
  all_neg_cyt_combn <- paste0("!", cyt_vec,
                              collapse = "&")

  prob_tbl %>%
    tidyr::pivot_longer(
      -sampleid,
      names_to = 'cyt_combn',
      values_to = 'prob') %>%
    dplyr::filter(
      !stringr::str_detect(
        cyt_combn,
        all_neg_cyt_combn
        )
      ) %>%
    dplyr::group_by(sampleid) %>%
    dplyr::summarise(prob = 1 - prod(1 - prob),
                     .groups = 'drop')
}
