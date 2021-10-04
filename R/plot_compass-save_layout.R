#' @title Create a layout based on post probs, cyt heat map and scores
#'
#' @param ind logical.
#' If \code{TRUE}, then sets of posterior probability
#' and PFS plots are saved individually.
#' @param p_list_pp
#' list of \code{ggplot2} objects.
#' Posterior probability plots and associated
#' cytokine-label grids, as returned by \code{compassutils:::.plot_compass_pp}.
#' @param p_list_scores
#' list of \code{ggplot2} objects.
#' PFS and FS response plots, as
#' returned by \code{compassutils:::.plot_compass_scores}.
#' @param height,width numeric. Height and width of saved plot(s).
#' @inheritParams plot_compass
#' @export
.save_layout <- function(ind, p_list_pp, p_list_scores = NULL,
                         prop_pp, shift_plot_heatmap_x,
                         shift_plot_scores_y,
                         shift_plot_pp_y,
                         height, width,
                         dir_save,
                         file,
                         save_format,
                         label,
                         n_col,
                         shift_label,
                         font_size_labels) {

  n_grp <- ifelse(ind, 1, length(p_list_pp$p_probs))
  scores_ind <- !is.null(p_list_scores)
  prop_pp[1] <- ifelse(!scores_ind, 1, prop_pp)
  n_col <- ifelse(is.null(n_col), min(2, n_grp), n_col)
  n_row <- ceiling(n_grp / n_col)
  col_ind_vec <- switch(
    as.character(ind),
    "TRUE" = rep(1, length(p_list_pp$p_probs)),
    "FALSE" = rep(seq_len(n_col), n_row)[seq_len(n_grp)]
  )
  row_ind_vec <- switch(
    as.character(ind),
    "TRUE" = rep(1, length(p_list_pp$p_probs)),
    "FALSE" = rep(seq_len(n_row), each = n_col)[seq_len(n_grp)]
  )

  height_region <- 1 / n_row
  height_pp_base <- height_region * prop_pp[2]
  height_pp <- height_pp_base + shift_plot_pp_y * height_pp_base
  height_grid <- height_region * (1 - prop_pp[2])
  width_region <- 1 / n_col
  width_pp <- width_region * prop_pp[1]
  width_scores <- width_region * (1 - prop_pp[1])
  shift_label_x <- width_region * shift_label[1]
  shift_label_y <- height_region * shift_label[2]

  if (!ind) p <- cowplot::ggdraw()

  if (ind) j <- 1 # allow looping when ind without changing grid location
  for (i in seq_along(p_list_pp$p_probs)) {
    if (!ind) j <- i

    if (ind) {
      p <- cowplot::ggdraw()
    }

    # overall location in grid
    row_ind <- row_ind_vec[j]
    col_ind <- col_ind_vec[j]
    x_left_most <- (col_ind - 1) * width_region
    y_bottom_most <- 1 - row_ind * height_region

    # pp
    y_pp <- y_bottom_most + height_grid
    x_pp <- x_left_most

    # grid
    y_grid <- y_bottom_most
    x_grid_base <- x_left_most
    x_grid <- x_grid_base + shift_plot_heatmap_x * width_pp
    width_grid <- width_pp - shift_plot_heatmap_x * width_pp

    p <- p +
      # add pp
      cowplot::draw_plot(
        plot = p_list_pp$p_probs[[i]],
        y = y_pp, height = height_pp,
        x = x_pp, width = width_pp
      ) +
      # add grid
      cowplot::draw_plot(
        plot = p_list_pp$p_grid,
        y = y_grid, height = height_grid,
        x = x_grid, width = width_grid
      )


    # scores
    if (scores_ind) {
      y_scores_base <- y_bottom_most + height_grid
      y_scores <- y_scores_base + height_pp * shift_plot_scores_y[1]
      height_scores <- height_pp + height_pp * shift_plot_scores_y[2]
      x_scores <- x_left_most + width_pp
      p <- p +
        cowplot::draw_plot(
          plot = p_list_scores[[names(p_list_pp$p_probs)[i]]] +
            theme(axis.title.x = element_blank()),
          y = y_scores, height = height_scores,
          x = x_scores, width = width_scores
        )
    }

    if (label) {
      p <- p +
        cowplot::draw_text(
          text = names(p_list_pp$p_probs)[i],
          fontface = "bold",
          x = x_left_most + shift_label_x,
          y = y_bottom_most + height_region + shift_label_y,
          vjust = 0, hjust = 0, size = font_size_labels
        )
    }

    if (ind) {
      if (!is.null(file)) {
        if (length(file) == length(p_list_pp$p_probs)) {
          if (is.null(names(file))) {
            fn_curr <- file[i]
          } else{
            fn_curr <- file[names(p_list_pp$p_probs)[i]]
            if (is.na(fn_curr)) fn_curr <- NULL
          }
        } else fn_curr <- NULL
      } else fn_curr <- NULL
      if (is.null(fn_curr)) {
        fn_curr <- paste0(
          "compass_boxplots-",
          switch(as.character(is.null(names(p_list_pp$p_probs))),
            "TRUE" = i,
            "FALSE" = names(p_list_pp$p_probs)[i] %>%
              stringr::str_replace_all("/", "_")
          )
        )
      }

      .save_plot(p = p, height = height, width = width, dir_save = dir_save,
                 file = fn_curr,
                 save_format = save_format,
                 n_row = 1, n_col = 1)
    }
  }

  # if saving individual plots, exit now
  if (ind) return(invisible(TRUE))

  .save_plot(p = p, height = height, width = width,
             dir_save = dir_save, file = file,
             save_format = save_format,
             n_row = n_col, n_col = n_col)

  invisible(TRUE)
}

#' @title Save an individual layout
#' @rdname save_plot
#'
#' @description Save an indivdual plot layout.
#'
#' @inheritParams plot_compass
#' @param p object of class \code{gg}. Plot to save.
#' @param n_row,n_col integer. Number of objects plotted per row and column.
#' Determines \code{width} and \code{height} if they are \code{NULL}.
#'
.save_plot <- function(p,
                       height,
                       width,
                       dir_save,
                       file,
                       save_format,
                       n_row,
                       n_col) {
  if (is.null(height)) {
    height <- 29 * 9 / 16 * 0.9 * n_row
  }
  if (is.null(width)) {
    width <- ifelse(n_col == 1, 29.4 / 2, 29.4)
  }
  if (!dir.exists(dir_save)) dir.create(dir_save, recursive = TRUE)
  cowplot::ggsave2(
    filename = file.path(
      dir_save,
      paste0(
        ifelse(is.null(file), "compass_boxplots_grid", file),
        ".", save_format
      )
    ),
    plot = p,
    units = "cm",
    width = width,
    height = height
  )
  invisible(TRUE)
}
