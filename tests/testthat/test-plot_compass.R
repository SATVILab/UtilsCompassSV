test_that("plot_compass works", {
  data("c_obj_list", package = "compassutils")
  fn_vec <- c(
    "compass_boxplots_grid.png",
    "compass_boxplots-EBV_CMV.png",
    "compass_boxplots-Live Mtb.png",
    "compass_boxplots-Non-secreted Mtb proteins.png",
    "compass_boxplots-Secreted Mtb proteins.png",
    "a.png", "b.png", "c.png", "d.png",
    "alternate.png"
  )
  rm_fn <- function(.fn_vec = fn_vec) {
    for (x in .fn_vec) {
      invisible(suppressWarnings(file.remove(file.path(tempdir(), x))))
    }
  }
  plot_compass(
    c_obj_list,
    dir_save = tempdir(),
    type = "pp",
    return_plot_list = FALSE,
    shift_plot_pp_y = -0.07,
    shift_plot_heatmap_x = 0.034
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  rm_fn()
  data("c_obj_list", package = "compassutils")

  # debugonce(plot_compass)
  plot_compass(
    c_obj_list,
    dir_save = tempdir(),
    type = "pp",
    return_plot_list = FALSE,
    shift_plot_pp_y = -0.07,
    shift_plot_heatmap_x = 0.034,
    save_ind = TRUE,
    save_grid = FALSE
  )
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots-EBV_CMV.png")))
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots-Live Mtb.png")))
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots-Non-secreted Mtb proteins.png")))
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots-Secreted Mtb proteins.png")))
  rm_fn()



  plot_compass(c_obj_list[1], dir_save = tempdir(), facet = FALSE, type = "pp", return_plot_list = FALSE)
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  rm_fn()

  plot_compass(c_obj_list[1],
    dir_save = tempdir(), type = c("pp", "scores"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.05
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  rm_fn()

  # boxplot widths
  # ------------------
  plot_compass(c_obj_list[1],
    dir_save = tempdir(), type = c("pp", "scores"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05,
    boxplot_width_pp = 0.1, boxplot_width_scores = 0.3, shift_plot_heatmap_x = 0.05
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  rm_fn()

  # scores lims
  # ------------------
  plot_compass(c_obj_list[1],
    dir_save = tempdir(), type = c("pp", "scores"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05,
    plot_scores_lims_y = c(0, 0.5)
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  rm_fn()

  # test naming
  # ---------------
  plot_compass(c_obj_list,
    dir_save = tempdir(), type = c("pp", "scores"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.052, file_grid = "alternate"
  )
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  expect_true(file.exists(file.path(tempdir(), "alternate.png")))
  rm_fn()
  plot_compass(c_obj_list,
    dir_save = tempdir(), type = c("pp", "scores"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.052, file_grid = "alternate",
    save_grid = FALSE, save_ind = TRUE, file_ind = c("a", "b", "c", "d")
  )
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots-EBV_CMV.png")))
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots-Live Mtb.png")))
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots-Non-secreted Mtb proteins.png")))
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots-Secreted Mtb proteins.png")))
  expect_true(file.exists(file.path(tempdir(), "a.png")))
  expect_true(file.exists(file.path(tempdir(), "b.png")))
  expect_true(file.exists(file.path(tempdir(), "c.png")))
  expect_true(file.exists(file.path(tempdir(), "d.png")))
  rm_fn()
  plot_compass(c_obj_list,
    dir_save = tempdir(), type = c("pp", "scores"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.052, file_grid = "alternate",
    save_grid = FALSE, save_ind = TRUE, file_ind = setNames(
      c("a", "b", "c", "d"),
      names(c_obj_list)[c(2, 1, 4, 3)]
    )
  )
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots-EBV_CMV.png")))
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots-Live Mtb.png")))
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots-Non-secreted Mtb proteins.png")))
  expect_true(!file.exists(file.path(tempdir(), "compass_boxplots-Secreted Mtb proteins.png")))
  expect_true(file.exists(file.path(tempdir(), "a.png")))
  expect_true(file.exists(file.path(tempdir(), "b.png")))
  expect_true(file.exists(file.path(tempdir(), "c.png")))
  expect_true(file.exists(file.path(tempdir(), "d.png")))
  rm_fn()

  get_cyt_lab <- function(cyt) {
    lapply(cyt, function(cyt_ind) {
      switch(cyt_ind,
        "IFNg" = bquote("IFN" ~ gamma),
        cyt_ind
      )
    })
  }

  plot_compass(c_obj_list[1],
    dir_save = tempdir(), type = c("pp"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05, cyt_lab = get_cyt_lab
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  rm_fn()

  get_cyt_lab <- function(cyt) {
    lapply(cyt, function(cyt_ind) {
      switch(cyt_ind,
        "IFNg" = bquote(paste(plain(paste("IFN")), gamma)),
        cyt_ind
      )
    })
  }


  # debugonce(compassutils:::.plot_compass_pp)

  plot_compass(c_obj_list[1],
    dir_save = tempdir(), type = c("pp"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.052, cyt_lab = get_cyt_lab
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  rm_fn()
})