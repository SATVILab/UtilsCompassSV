# prep
# -------------------

#' @title Plot COMPASS PFS and FS scores
#'
#' @inheritParams plot_compass
#' @param c_obj a named list of COMPASSResult objects. Provides data to plot.
#' @param boxplot_width numeric. Width parameter for geom_boxplot.
#' @param font_size numeric. Base font size for plots.
#' @param line_width numeric. Width of lines in plots.
#'
#' @return A named list with names set to the names
#' of \code{c_obj}, where each element is the corresponding
#' boxplot of PFS and FS scores.
#' @keywords internal
.plot_compass_scores <- function(c_obj, plot_prob_fill, boxplot_width,
                                 plot_scores_lims_y,
                                 font_size,
                                 line_width = 0.5) {

  # prep
  score_tbl <- purrr::map_df(seq_along(c_obj), function(i) {
    x <- c_obj[[i]]
    pfs_vec <- COMPASS::PolyfunctionalityScore(x)
    fs_vec <- COMPASS::FunctionalityScore(x)
    tibble::tibble(
      .id = names(pfs_vec), .grp = names(c_obj)[i],
      PFS = pfs_vec
    ) %>%
      dplyr::mutate(FS = fs_vec[.id])
  }) %>%
    tidyr::pivot_longer(-c(.id, .grp),
      names_to = "score_type",
      values_to = "score"
    )

  if (is.null(plot_scores_lims_y)) {
    y_lim_vec <- c(0, max(score_tbl$score))
  } else {
    y_lim_vec <- plot_scores_lims_y
  }
  col_vec_grp <- .get_col_vec_grp(
    plot_prob_fill = plot_prob_fill,
    .grp = names(c_obj)
  )

  purrr::map(names(c_obj), function(.grp_curr) {
    ggplot(
      score_tbl %>%
        dplyr::filter(.grp == .grp_curr),
      aes(x = score_type, y = score, fill = .grp)
    ) +
      {
        box_args <- list(
          outlier.size = 0.25,
          outlier.colour = "gray50",
          linewidth = line_width
        )
        if (!is.null(boxplot_width)) box_args$width <- boxplot_width
        do.call(ggplot2::geom_boxplot, box_args)
      } +
      cowplot::theme_cowplot(font_size = font_size, line_size = line_width) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      cowplot::background_grid() +
      labs(x = "Score type", y = "Score") +
      scale_fill_manual(values = col_vec_grp) +
      coord_cartesian(ylim = y_lim_vec) +
      theme(legend.position = "none")
  }) %>%
    setNames(names(c_obj))
}


#' @title Plot COMPASS posterior probabilites
#'
#' @inheritParams plot_compass
#' @param c_obj a named list of COMPASSResult objects. Provides data to plot.
#' @param boxplot_width numeric. Width parameter for geom_boxplot.
#' @param font_size numeric. Base font size for plots.
#' @param line_width numeric. Width of lines in plots.
#'
#' @return A named list with names 'p_probs' and 'p_grid',
#' corresponding to the list of ggplot2 plots of posterior plots
#' and the heat map of the corresponding cytokine combination
#' labels, respectively.
#' @keywords internal
.plot_compass_pp <- function(c_obj, dir_save, prob_min, quant_min,
                             silent, cyt_order, plot_prob_fill, facet,
                             cyt_lab, boxplot_width, font_size, line_width,
                             tile_colour, tile_alpha, tile_fill) {
  pp_tbl <- purrr::map_df(seq_along(c_obj), function(i) {
    x <- c_obj[[i]]
    pp_mat <- x$fit$mean_gamma
    colnames(pp_mat) <- convert_cyt_combn_format(
      colnames(pp_mat),
      to = "std"
    )
    pp_tbl <- tibble::as_tibble(pp_mat)
    id_var <- x$data$individual_id
    pp_tbl[[".id"]] <- x$data$meta[[id_var]]
    pp_tbl <- pp_tbl[, ".id"] %>%
      dplyr::bind_cols(pp_tbl[, !(colnames(pp_tbl) == ".id")])
    pp_tbl %>%
      tidyr::pivot_longer(-.id,
        names_to = "combn",
        values_to = "prob"
      ) %>%
      dplyr::mutate(.grp = names(c_obj)[i])
  }) %>%
    dplyr::mutate(.grp = factor(.data$.grp, levels = names(c_obj)))


  # filter out low-response cytokine combinations
  # ----------------------
  combn_sel_vec <- pp_tbl %>%
    dplyr::filter(stringr::str_detect(combn, "[[+]]")) %>%
    dplyr::group_by(combn, .grp) %>%
    dplyr::filter(quantile(prob, prob_min) >= quant_min) %>%
    magrittr::extract2("combn") %>%
    unique()

  if (length(combn_sel_vec) == 0) {
    if (!silent) {
      warning(
        "No cytokine combinations met inclusion criteria. An unnamed list with a blank plot returned."
      )
    }
    return(switch(as.character(return_plot),
      "TRUE" = switch(as.character(init_list),
        "TRUE" = purrr::map(c_obj, function(x) ggplot()) %>%
          setNames(init_name_vec)
      ),
      "FALSE" = invisible(TRUE)
    ))
  }

  # calculate degree of each cytokine combination
  # ------------------------

  degree_lab_vec <- purrr::map_chr(combn_sel_vec, function(combn) {
    as.character(nrow(stringr::str_locate_all(combn, "[[+]]")[[1]]))
  }) %>%
    setNames(combn_sel_vec)

  pp_tbl %<>%
    dplyr::filter(combn %in% combn_sel_vec) %>%
    dplyr::mutate(degree = degree_lab_vec[combn])

  # order cyt combns within each degree by post probs
  # -------------------------
  combn_factor_levels_vec <- pp_tbl %>%
    dplyr::group_by(degree, combn, .grp) %>%
    dplyr::summarise(quant = quantile(prob, 0.75), .groups = "drop") %>%
    dplyr::group_by(degree, combn) %>%
    dplyr::filter(quant == max(quant)) %>%
    dplyr::slice(1) %>%
    dplyr::group_by(degree) %>%
    dplyr::arrange(quant, .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    magrittr::extract2("combn")

  pp_tbl %<>%
    dplyr::mutate(combn = factor(.data$combn, levels = combn_factor_levels_vec))

  # create grid tbl
  # --------------------------

  # get order of cytokines for grid
  if (is.null(cyt_order)) {
    cyt_order <- stringr::str_split(pp_tbl[["combn"]][1], "[[+-]]")[[1]]
    cyt_order <- cyt_order[-length(cyt_order)]
  } else {
    cyt_order_compass <- stringr::str_split(pp_tbl[["combn"]][1], "[[+-]]")[[1]]
    cyt_order_compass <- cyt_order_compass[-length(cyt_order_compass)]
    if (!identical(sort(intersect(cyt_order_compass, cyt_order)), sort(cyt_order))) {
      warning("cytokines in COMPASS and cyt_order differ, and so the cytokines in COMPASS are used.")
      cyt_order <- cyt_order_compass
    }
  }

  # create base table for grid
  grid_tbl <- pp_tbl %>%
    dplyr::select(combn, degree) %>%
    dplyr::group_by(combn, degree) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  for (cyt in cyt_order) {
    grid_tbl[[cyt]] <- as.character(stringr::str_detect(grid_tbl$combn, paste0(cyt, "[[+]]")))
  }

  grid_tbl %<>%
    tidyr::pivot_longer(-c(combn, degree),
      names_to = "cyt",
      values_to = "expressed"
    ) %>%
    dplyr::mutate(cyt = factor(.data$cyt,
      levels = cyt_order
    ))

  # set up colours
  # --------------------

  # grid
  if (!is.null(tile_fill)) {
    if (length(tile_fill) == 1L) {
      col_vec <- rep(tile_fill, length(cyt_order))
    } else if (length(tile_fill) < length(cyt_order)) {
      col_vec <- rep(tile_fill, length.out = length(cyt_order))
    } else {
      col_vec <- tile_fill[seq(1, length(cyt_order))]
    }
  } else {
    col_vec <- RColorBrewer::brewer.pal(
      n = length(cyt_order) + 2,
      name = "YlOrBr"
    )
    col_vec <- col_vec[-c(1, length(cyt_order))]
  }
  expr_degree_lab_vec <- setNames(col_vec, paste0("TRUE", 1:length(col_vec)))
  expr_degree_lab_vec <- c(expr_degree_lab_vec, setNames(
    rep("white", length(col_vec)),
    paste0("FALSE", 1:length(col_vec))
  ))

  col_vec_grp <- .get_col_vec_grp(
    plot_prob_fill = plot_prob_fill, .grp = names(c_obj)
  )

  # grid
  p_grid <- ggplot(
    grid_tbl %>%
      dplyr::mutate(expr_degree = paste0(expressed, degree)),
    aes(
      x = combn, y = cyt,
      fill = expr_degree
    )
  ) +
    cowplot::theme_cowplot(font_size = font_size, line_size = line_width) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    # geom_raster(col = 'black', linetype = 'solid') +
    scale_fill_manual(values = expr_degree_lab_vec) +
    theme(legend.position = "none") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  if (!is.null(tile_colour)) {
    p_grid <- p_grid + geom_tile(color = tile_colour, alpha = tile_alpha)
  } else {
    p_grid <- p_grid + geom_tile(alpha = tile_alpha)
  }

  if (!is.null(cyt_lab)) {
    p_grid_orig <- p_grid
    p_grid <- try(p_grid +
      scale_y_discrete(labels = cyt_lab))
    if (identical(class(p_grid), "try-error")) {
      warning("Using cyt_lab created an error. Creating cytokine grid plot without it.")
      p_grid <- p_grid_orig
    }
    rm(list = "p_grid_orig")
  }


  p_probs <- purrr::map(seq_along(c_obj), function(i) {
    grp_curr <- names(c_obj)[i]
    p <- ggplot(
      pp_tbl %>%
        dplyr::filter(.grp == grp_curr),
      aes(x = combn, y = prob, fill = .grp)
    ) +
      cowplot::theme_cowplot(font_size = font_size, line_size = line_width) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      cowplot::background_grid(major = "y") +
      {
        box_args <- list(
          outlier.size = 0.25,
          outlier.colour = "gray50",
          linewidth = line_width
        )
        if (!is.null(boxplot_width)) box_args$width <- boxplot_width
        do.call(ggplot2::geom_boxplot, box_args)
      } +
      scale_fill_manual(values = col_vec_grp) +
      lims(y = c(0, 1)) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "none"
      ) +
      labs(y = "Probability of a response") +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
      )
    if (facet) p <- p + facet_wrap(~.grp)
    p
  }) %>%
    setNames(names(c_obj))

  list("p_grid" = p_grid, "p_probs" = p_probs)
}
