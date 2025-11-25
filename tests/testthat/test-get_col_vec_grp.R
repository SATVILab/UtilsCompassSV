test_that(".get_col_vec_grp returns plot_prob_fill when it is a named character vector", {
  result <- UtilsCompassSV:::.get_col_vec_grp(
    plot_prob_fill = c("grp1" = "red", "grp2" = "blue"),
    .grp = c("grp1", "grp2")
  )
  expect_identical(result, c("grp1" = "red", "grp2" = "blue"))
})

test_that(".get_col_vec_grp errors on named non-character plot_prob_fill", {
  expect_error(
    UtilsCompassSV:::.get_col_vec_grp(
      plot_prob_fill = c("grp1" = 1, "grp2" = 2),
      .grp = c("grp1", "grp2")
    ),
    "plot_prob_fill must be of type character if not NULL"
  )
})

test_that(".get_col_vec_grp errors when .grp is NULL and plot_prob_fill is unnamed", {
  expect_error(
    UtilsCompassSV:::.get_col_vec_grp(
      plot_prob_fill = "red",
      .grp = NULL
    ),
    ".grp must be specified if plot_prob_fill not a named character vector"
  )
})

test_that(".get_col_vec_grp returns default colors when plot_prob_fill is NULL", {
  result <- UtilsCompassSV:::.get_col_vec_grp(
    plot_prob_fill = NULL,
    .grp = c("grp1", "grp2", "grp3")
  )
  expect_true(is.character(result))
  expect_equal(names(result), c("grp1", "grp2", "grp3"))
  expect_equal(length(result), 3)
})

test_that(".get_col_vec_grp handles single color for multiple groups", {
  result <- UtilsCompassSV:::.get_col_vec_grp(
    plot_prob_fill = "red",
    .grp = c("grp1", "grp2", "grp3")
  )
  expect_equal(result, c("grp1" = "red", "grp2" = "red", "grp3" = "red"))
})

test_that(".get_col_vec_grp sets names from .grp when same length as colors", {
  result <- UtilsCompassSV:::.get_col_vec_grp(
    plot_prob_fill = c("red", "blue", "green"),
    .grp = c("grp1", "grp2", "grp3")
  )
  expect_equal(names(result), c("grp1", "grp2", "grp3"))
  expect_equal(unname(result), c("red", "blue", "green"))
})

test_that(".get_col_vec_grp_null returns appropriate colors for groups", {
  result <- UtilsCompassSV:::.get_col_vec_grp_null(.grp = c("grp1", "grp2"))
  expect_true(is.character(result))
  expect_equal(names(result), c("grp1", "grp2"))
  expect_equal(length(result), 2)
})

test_that(".get_col_vec_grp_null handles single group", {
  result <- UtilsCompassSV:::.get_col_vec_grp_null(.grp = c("single_group"))
  expect_true(is.character(result))
  expect_equal(names(result), "single_group")
})

test_that(".get_col_vec_grp errors on unnamed non-character plot_prob_fill", {
  # Tests line 25 - unnamed non-character plot_prob_fill
  expect_error(
    UtilsCompassSV:::.get_col_vec_grp(
      plot_prob_fill = c(1, 2, 3),
      .grp = c("grp1", "grp2", "grp3")
    ),
    "plot_prob_fill must be of type character if not NULL"
  )
})
