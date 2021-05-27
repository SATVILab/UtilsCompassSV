test_that("plot_compass works", {
  data('c_obj_list', package = 'compassutils')
  plot_compass(c_obj_list, dir_save = tempdir())
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots.png")))
  invisible(suppressWarnings(file.remove(file.path(tempdir(), "compass_boxplots.png"))))

  plot_compass(c_obj_list, dir_save = tempdir(), facet = FALSE)
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots.png")))
  invisible(suppressWarnings(file.remove(file.path(tempdir(), "compass_boxplots.png"))))
})
