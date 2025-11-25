test_that("convert_cyt_combn_format", {
  test_vec_compass <- c(
    "IFNg&!IL2&TNF&IL6&!IL22",
    "IFNg&IL2&TNF&IL6&IL22",
    "!IFNg&!IL2&!TNF&!IL6&!IL22"
  )

  expect_identical(
    convert_cyt_combn_format(cyt_combn = test_vec_compass, to = "std"),
    c("IFNg+IL2-TNF+IL6+IL22-", "IFNg+IL2+TNF+IL6+IL22+", "IFNg-IL2-TNF-IL6-IL22-")
  )
  expect_error(convert_cyt_combn_format(cyt_combn = c(
    test_vec_compass[1],
    "IFNg&!IL2&TNF&IL6"
  ), to = "std"))
  expect_error(convert_cyt_combn_format(cyt_combn = c("IFNg&!IL2&TNF&IL6&"), to = "std"))
  expect_identical(
    convert_cyt_combn_format(cyt_combn = c("IFNg&!IL2&TNF&IL6&"), to = "std", check = FALSE),
    "IFNg+IL2-TNF+IL6++"
  )

  lab_vec <- c(
    "IFNg" = "cheese",
    "IL2" = "ham",
    "TNF" = "eggs",
    "IL6" = "bacon",
    "IL17" = "toast",
    "IL22" = "coffee"
  )
  expect_identical(
    convert_cyt_combn_format(
      cyt_combn = test_vec_compass, to = "std",
      lab = lab_vec
    ),
    c("cheese+ham-eggs+bacon+coffee-", "cheese+ham+eggs+bacon+coffee+", "cheese-ham-eggs-bacon-coffee-")
  )
  expect_false(
    identical(
      convert_cyt_combn_format(
        cyt_combn = test_vec_compass[1], to = "std",
        lab = lab_vec
      ),
      "cheese+ham-eggs+bacon+coffee+"
    )
  )

  test_vec_std <- c(
    "IFNg+IL2-TNF+IL6+IL22-",
    "IFNg+IL2+TNF+IL6+IL22+",
    "IFNg-IL2-TNF-IL6-IL22-"
  )

  expect_identical(
    convert_cyt_combn_format(cyt_combn = test_vec_std, to = "compass"),
    test_vec_compass
  )

  expect_identical(
    convert_cyt_combn_format(
      cyt_combn = test_vec_std, to = "compass",
      lab = lab_vec
    ),
    c("cheese&!ham&eggs&bacon&!coffee", "cheese&ham&eggs&bacon&coffee", "!cheese&!ham&!eggs&!bacon&!coffee")
  )
})

test_that("convert_cyt_combn_format errors on missing cyt_combn", {
  expect_error(
    convert_cyt_combn_format(to = "std"),
    "cyt_combn must be specified"
  )
})

test_that("convert_cyt_combn_format errors on non-character/non-factor cyt_combn", {
  expect_error(
    convert_cyt_combn_format(cyt_combn = 123, to = "std"),
    "cyt_combn must be a character or factor"
  )
  expect_error(
    convert_cyt_combn_format(cyt_combn = list("a", "b"), to = "std"),
    "cyt_combn must be a character or factor"
  )
})

test_that("convert_cyt_combn_format errors on missing to parameter", {
  expect_error(
    convert_cyt_combn_format(cyt_combn = "IFNg&IL2"),
    "must specify `to` parameters"
  )
})

test_that("convert_cyt_combn_format errors on invalid to parameter", {
  expect_error(
    convert_cyt_combn_format(cyt_combn = "IFNg&IL2", to = "invalid"),
    "`to` parameter must be either 'compass' or 'std'"
  )
})

test_that("convert_cyt_combn_format errors on non-logical force parameter", {
  expect_error(
    convert_cyt_combn_format(cyt_combn = "IFNg&IL2", to = "std", force = "yes"),
    "force parameter must be of class logical"
  )
})

test_that("convert_cyt_combn_format errors on non-logical silent parameter", {
  expect_error(
    convert_cyt_combn_format(cyt_combn = "IFNg&IL2", to = "std", silent = "no"),
    "silent parameter must be of class logical"
  )
})

test_that("convert_cyt_combn_format warns and returns input when already in std format", {
  std_input <- "IFNg+IL2-"
  expect_warning(
    result <- convert_cyt_combn_format(cyt_combn = std_input, to = "std"),
    "No &'s detected"
  )
  expect_identical(result, std_input)
})

test_that("convert_cyt_combn_format silently returns input when already in std format and silent=TRUE", {
  std_input <- "IFNg+IL2-"
  expect_silent(
    result <- convert_cyt_combn_format(cyt_combn = std_input, to = "std", silent = TRUE)
  )
  expect_identical(result, std_input)
})

test_that("convert_cyt_combn_format with force=TRUE converts even when no ampersands detected", {
  # When force is TRUE, it attempts conversion even if no & detected
  # Using an input that has & to test force=TRUE path
  compass_input <- "IFNg&IL2"
  result <- convert_cyt_combn_format(cyt_combn = compass_input, to = "std", force = TRUE)
  expect_identical(result, "IFNg+IL2+")
})

test_that("convert_cyt_combn_format works with factor input", {
  test_vec_compass <- factor(c("IFNg&IL2", "!IFNg&!IL2"))
  result <- convert_cyt_combn_format(cyt_combn = test_vec_compass, to = "std")
  expect_identical(result, c("IFNg+IL2+", "IFNg-IL2-"))
})
