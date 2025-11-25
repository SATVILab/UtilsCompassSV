# Helper function to clean up test files
cleanup_test_files <- function(files = c("compass_boxplots_grid.png")) {
  for (x in files) {
    invisible(suppressWarnings(file.remove(file.path(tempdir(), x))))
  }
}

test_that("plot_compass works", {
  data("c_obj_list", package = "UtilsCompassSV")
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
  data("c_obj_list", package = "UtilsCompassSV")

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


  # debugonce(UtilsCompassSV:::.plot_compass_pp)

  plot_compass(c_obj_list[1],
    dir_save = tempdir(), type = c("pp"),
    return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
    shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.052, cyt_lab = get_cyt_lab
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  rm_fn()
})

test_that("plot_compass errors on invalid type", {
  data("c_obj_list", package = "UtilsCompassSV")
  expect_error(
    plot_compass(
      c_obj_list,
      type = c("pp", "invalid_type"),
      return_plot_list = FALSE
    ),
    "type must be a character vector"
  )
})

test_that("plot_compass errors on invalid c_obj", {
  # Test with non-COMPASSResult object
  expect_error(
    plot_compass(
      c_obj = "not a COMPASS object",
      return_plot_list = FALSE
    ),
    "c_obj must either have class COMPASSResult"
  )
})

test_that("plot_compass errors on empty list", {
  expect_error(
    plot_compass(
      c_obj = list(),
      return_plot_list = FALSE
    ),
    "no data in c_obj"
  )
})

test_that("plot_compass errors on list with non-COMPASSResult elements", {
  data("c_obj_list", package = "UtilsCompassSV")
  mixed_list <- list(c_obj_list[[1]], "not a COMPASS object")
  expect_error(
    plot_compass(
      c_obj = mixed_list,
      return_plot_list = FALSE
    ),
    "not all elements in c_obj list have class COMPASSResult"
  )
})

test_that("plot_compass works with type as factor", {
  data("c_obj_list", package = "UtilsCompassSV")
  # Test with type as factor
  plot_compass(
    c_obj_list[1],
    dir_save = tempdir(),
    type = factor("pp"),
    return_plot_list = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  cleanup_test_files()
})

test_that("plot_compass auto-adds pp when not in type", {
  data("c_obj_list", package = "UtilsCompassSV")
  # Type "scores" alone should still work because pp is auto-added
  plot_compass(
    c_obj_list[1],
    dir_save = tempdir(),
    type = "scores",
    return_plot_list = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "compass_boxplots_grid.png")))
  cleanup_test_files()
})

test_that("plot_compass returns plot list when return_plot_list is TRUE", {
  data("c_obj_list", package = "UtilsCompassSV")
  result <- plot_compass(
    c_obj_list[1],
    dir_save = tempdir(),
    type = c("pp", "scores"),
    return_plot_list = TRUE
  )
  expect_true(is.list(result))
  expect_true("p_grid" %in% names(result))
  expect_true("p_probs" %in% names(result))
  expect_true("p_scores" %in% names(result))
  cleanup_test_files()
})

test_that("plot_compass assigns names to unnamed list", {
  data("c_obj_list", package = "UtilsCompassSV")
  # Create unnamed list
  unnamed_list <- unname(c_obj_list[1:2])
  result <- plot_compass(
    unnamed_list,
    dir_save = tempdir(),
    type = "pp",
    return_plot_list = TRUE
  )
  # Should have auto-generated names
  expect_true("grp1" %in% names(result$p_probs))
  expect_true("grp2" %in% names(result$p_probs))
  cleanup_test_files()
})
