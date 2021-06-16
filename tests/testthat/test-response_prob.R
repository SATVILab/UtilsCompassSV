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
})


