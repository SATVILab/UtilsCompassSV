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

  get_cyt_lab <- function(cyt){
    lapply(cyt, function(cyt_ind){
      switch(cyt_ind,
             "IFNg" = bquote('IFN'~gamma),
             cyt_ind)
    })
  }

  plot_compass(c_obj_list[1], dir_save = tempdir(), type = c('pp'),
               return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
               shift_plot_pp_y = -0.05, shift_plot_grid_x = 0.052, cyt_lab = get_cyt_lab)
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots.png")))

  get_cyt_lab <- function(cyt){
    lapply(cyt, function(cyt_ind){
      switch(cyt_ind,
             "IFNg" = bquote(paste(plain(paste("IFN")), gamma)),
             cyt_ind)
    })
  }


  #debugonce(compassutils:::.plot_compass_pp)

  plot_compass(c_obj_list[1], dir_save = tempdir(), type = c('pp'),
               return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
               shift_plot_pp_y = -0.05, shift_plot_grid_x = 0.052, cyt_lab = get_cyt_lab)
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots.png")))
  invisible(suppressWarnings(file.remove(file.path(tempdir(), "compass_boxplots.png"))))
})
