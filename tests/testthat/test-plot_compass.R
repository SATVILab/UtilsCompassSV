test_that("plot_compass works", {
  data('c_obj_list', package = 'compassutils')
  plot_compass(c_obj_list, dir_save = tempdir(), type = 'pp', return_plot_list = FALSE,
               shift_plot_pp_y = -0.07, shift_plot_grid_x = 0.034)
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots.png")))
  invisible(suppressWarnings(file.remove(file.path(tempdir(), "compass_boxplots.png"))))

  plot_compass(c_obj_list, dir_save = tempdir(), facet = FALSE, type = 'pp', return_plot_list = FALSE)
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots.png")))
  invisible(suppressWarnings(file.remove(file.path(tempdir(), "compass_boxplots.png"))))

  plot_compass(c_obj_list, dir_save = tempdir(), type = c('pp', 'scores'),
               return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
               shift_plot_pp_y = -0.05, shift_plot_grid_x = 0.052)
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots.png")))
  invisible(suppressWarnings(file.remove(file.path(tempdir(), "compass_boxplots.png"))))
})
