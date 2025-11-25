#' @title Calculate COMPASS-derived overall responder probability
#'
#' @description Calculate the probability of responding to at least one
#' cytokine combination, assuming independence of the probability estimates
#' between cytokine combinations.
#'
#' @param c_obj object of class 'COMPASSResult'. The posterior probabilities
#' for individual cytokine combinations are obtained here.
#' @param exc character vector. Specifies cytokine combination(s) to exclude.
#' If \code{NULL}, then none except the all-negative population are excluded.
#' Default is \code{NULL}.
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
#' data("c_obj", package = "UtilsCompassSV")
#' response_prob(c_obj = c_obj)
#' response_prob(
#'   c_obj = c_obj,
#'   exc = c(
#'     "IFNg&IL2&TNF&!IL17&!IL6&!IL22",
#'     "IFNg&!IL2&TNF&!IL17&!IL6&!IL22"
#'   )
#' )
#' @export
response_prob <- function(c_obj, exc = NULL) {
  prob_mat <- c_obj$fit$mean_gamma
  sampleid_vec <- rownames(prob_mat)
  prob_tbl <- prob_mat |>
    tibble::as_tibble() |>
    dplyr::mutate(sampleid = sampleid_vec) |>
    dplyr::select(sampleid, everything())
  cyt_vec <- stringr::str_split(
    colnames(prob_tbl)[2],
    "&"
  )[[1]]
  cyt_vec <- purrr::map_chr(cyt_vec, function(cyt) {
    if (stringr::str_sub(cyt, 1, 1) != "!") {
      return(cyt)
    }
    stringr::str_sub(cyt, start = 2)
  })
  all_neg_cyt_combn <- paste0("!", cyt_vec,
    collapse = "&"
  )
  n_cyt_combn_max <- 2^length(cyt_vec)

  prob_tbl <- prob_tbl[, -which(
    stringr::str_detect(
      colnames(prob_tbl),
      all_neg_cyt_combn
    )
  )]

  if (!ncol(prob_tbl) == n_cyt_combn_max) {
    stop(paste0(
      "removing all neg cyt combn (calculated as ",
      all_neg_cyt_combn,
      ") failed. Please notify package maintainer at rdxmig002@myuct.ac.za."
    ))
  }

  if (!is.null(exc)) {
    # check for matches
    purrr::walk(exc, function(x) {
      cn_vec <- colnames(prob_tbl)[-1]
      len_vec <- unique(stringr::str_length(cn_vec))
      if (!stringr::str_length(x) %in%
        len_vec) {
        stop(paste0(x, " is in exc but not in cyt combns of COMPASS object"),
          call. = FALSE
        )
      }
      match_ind <- purrr::map_lgl(cn_vec, function(cn) {
        stringr::str_detect(cn, x)
      }) |>
        any()
      if (!match_ind) {
        stop(paste0(x, " is in exc but not in cyt combns of COMPASS object"),
          call. = FALSE
        )
      }
    })

    for (x in exc) {
      prob_tbl <- prob_tbl[, -which(
        stringr::str_detect(
          colnames(prob_tbl),
          x
        ) &
          !stringr::str_detect(
            colnames(prob_tbl),
            paste0("!", x)
          )
      )]
    }

    if (ncol(prob_tbl) != (n_cyt_combn_max - length(exc))) {
      stop("Did not remove as many cytokine combinations from COMPASS object as in exc")
    }
  }

  prob_tbl |>
    tidyr::pivot_longer(
      -sampleid,
      names_to = "cyt_combn",
      values_to = "prob"
    ) |>
    dplyr::filter(
      !stringr::str_detect(
        cyt_combn,
        all_neg_cyt_combn
      )
    ) |>
    dplyr::group_by(sampleid) |>
    dplyr::summarise(
      prob = 1 - prod(1 - prob),
      .groups = "drop"
    )
}
