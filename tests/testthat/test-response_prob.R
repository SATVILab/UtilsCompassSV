test_that("response_prob works", {
  data('c_obj', package = 'compassutils')
  prob_tbl <- response_prob(c_obj = c_obj)
  prob_vec_54 <- c_obj$fit$mean_gamma[54,]
  prob_vec_54 <- prob_vec_54[-which(names(prob_vec_54) == "!IFNg&!IL2&!TNF&!IL17&!IL6&!IL22")]
  prob_54 <- 1 - prod(1-prob_vec_54)
  expect_true(
    identical(
      class(prob_tbl),
      c('tbl_df', 'tbl', 'data.frame')
      )
  )
  expect_equal(prob_54, prob_tbl[['prob']][[54]])
  expect_true(
    identical(
      colnames(prob_tbl),
      c("sampleid", "prob")
    )
  )

  # exclusions
  # --------------------

  # exclude triple-positive
  expect_identical(
    class(
      response_prob(
        c_obj = c_obj,
        exc = 'IFNg&IL2&TNF&!IL17&!IL6&!IL22'
      )
    ),
    c('tbl_df', 'tbl', 'data.frame')
  )

  # single exclusion
  prob_tbl <- response_prob(
    c_obj = c_obj,
    exc = 'IFNg&IL2&TNF&!IL17&!IL6&!IL22'
    )
  prob_vec_54 <- c_obj$fit$mean_gamma[54,]
  prob_vec_54 <- prob_vec_54[-which(names(prob_vec_54) %in% c(
    "!IFNg&!IL2&!TNF&!IL17&!IL6&!IL22",
    'IFNg&IL2&TNF&!IL17&!IL6&!IL22'
    ))]
  prob_54 <- 1 - prod(1-prob_vec_54)
  expect_equal(prob_54, prob_tbl[['prob']][[54]])

  # multiple exclusions
  prob_tbl <- response_prob(
    c_obj = c_obj,
    exc = c(
      'IFNg&IL2&TNF&!IL17&!IL6&!IL22',
      'IFNg&!IL2&TNF&!IL17&!IL6&!IL22'
    )
  )
  prob_vec_54 <- c_obj$fit$mean_gamma[54,]
  prob_vec_54 <- prob_vec_54[-which(names(prob_vec_54) %in% c(
    "!IFNg&!IL2&!TNF&!IL17&!IL6&!IL22",
    'IFNg&IL2&TNF&!IL17&!IL6&!IL22',
    'IFNg&!IL2&TNF&!IL17&!IL6&!IL22'
  ))]
  prob_54 <- 1 - prod(1-prob_vec_54)
  expect_equal(prob_54, prob_tbl[['prob']][[54]])

  # length-error
  expect_error(
    response_prob(
      c_obj = c_obj,
      exc = 'IFNg&IL2'
      )
  )
  # cyt_combn error
  expect_error(
    response_prob(
      c_obj = c_obj,
      exc = 'abcd&IL2&!TNF&!IL17&!IL6&!IL22'
      )
  )

})


