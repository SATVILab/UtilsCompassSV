test_that("convert_cyt_combn_format", {
  test_vec_compass <- c("IFNg&!IL2&TNF&IL6&!IL22",
                        "IFNg&IL2&TNF&IL6&IL22",
                        "!IFNg&!IL2&!TNF&!IL6&!IL22")

  expect_identical(convert_cyt_combn_format(cyt_combn = test_vec_compass, to = 'std'),
                   c("IFNg+IL2-TNF+IL6+IL22-", "IFNg+IL2+TNF+IL6+IL22+", "IFNg-IL2-TNF-IL6-IL22-"))
  expect_error(convert_cyt_combn_format(cyt_combn = c(test_vec_compass[1],
                                                      "IFNg&!IL2&TNF&IL6"), to = 'std'))
  expect_error(convert_cyt_combn_format(cyt_combn = c("IFNg&!IL2&TNF&IL6&"), to = 'std'))
  expect_identical(convert_cyt_combn_format(cyt_combn = c("IFNg&!IL2&TNF&IL6&"), to = 'std', check = FALSE),
                   "IFNg+IL2-TNF+IL6++")

  lab_vec <- c("IFNg" = "cheese",
               "IL2" = "ham",
               "TNF" = "eggs",
               "IL6" = "bacon",
               "IL17" = "toast",
               "IL22" = "coffee")
  expect_identical(convert_cyt_combn_format(cyt_combn = test_vec_compass, to = 'std',
                                            lab = lab_vec),
                   c("cheese+ham-eggs+bacon+coffee-", "cheese+ham+eggs+bacon+coffee+", "cheese-ham-eggs-bacon-coffee-"))
  expect_false(
    identical(convert_cyt_combn_format(cyt_combn = test_vec_compass[1], to = 'std',
                                       lab = lab_vec),
              "cheese+ham-eggs+bacon+coffee+"
    )
  )

  test_vec_std <- c("IFNg+IL2-TNF+IL6+IL22-",
                    "IFNg+IL2+TNF+IL6+IL22+",
                    "IFNg-IL2-TNF-IL6-IL22-")

  expect_identical(convert_cyt_combn_format(cyt_combn = test_vec_std, to = 'compass'),
                   test_vec_compass)

  expect_identical(convert_cyt_combn_format(cyt_combn = test_vec_std, to = 'compass',
                                            lab = lab_vec),
                   c("cheese&!ham&eggs&bacon&!coffee", "cheese&ham&eggs&bacon&coffee", "!cheese&!ham&!eggs&!bacon&!coffee"))

})
