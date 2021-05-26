test_that("convert_cyt_combn_format", {
  test_vec_compass <- c("IFNg&!IL2&TNF&IL6&!IL22",
                        "IFNg&IL2&TNF&IL6&IL22",
                        "!IFNg&!IL2&!TNF&!IL6&!IL22")
  convert_cyt_combn_format("IFNg&!IL2&TNF&IL6&!IL22", to = 'std')
  expect_identical(convert_cyt_combn_format(cyt_combn = test_vec_compass, to = 'std'),
                   c("IFNg+IL2-TNF+IL6+IL22-", "IFNg+IL2+TNF+IL6+IL22+", "IFNg-IL2-TNF-IL6-IL22-"))
  expect_error(convert_cyt_combn_format(cyt_combn = c(test_vec_compass[1],
                                                      "IFNg&!IL2&TNF&IL6"), to = 'std'))
  expect_error(convert_cyt_combn_format(cyt_combn = c("IFNg&!IL2&TNF&IL6&"), to = 'std'))
  expect_identical(convert_cyt_combn_format(cyt_combn = c("IFNg&!IL2&TNF&IL6&"), to = 'std', check = FALSE),
                   "IFNg+IL2-TNF+IL6++")
})
