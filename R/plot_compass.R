#' @title Plot COMPASS output
#'
#' @param c_obj object of class 'COMPASSResult', or a list of such objects. Provides
#' COMPASS data to plot.
#' @param dir_save character. Where to save the output. Default is working directory.
#' @param save logical. If \code{TRUE}, then plots are saved. Default is \code{TRUE}.
#' @param prob_min,quant_min [0,1]. Specify the minimum probability of a response for the minimum quantile
#' of samples that a cytokine combination must have to be included. For example,
#' if \code{prob_min == 0.5} and quant_min == {0.1}, then only cytokine combinations for which
#' at least 10% of observations had a probability of a response of 0.5 or greater will plotted.
#' Default is \code{0.8} and \code{0.25}.
#' @param cyt_order character. If not \code{NULL}, then the order in which cytokines are arranged
#' vertically along the annotation grid is given by this vector (with first element going at the bottom).
#' If \code{NULL}, then cytokines are ordered by their order in COMPASS output. Default is \code{NULL}.
#' @param silent logical. If \code{TRUE}, then any warnings that would have been otherwise given
#' are not. Default is \code{FALSE}.
#' @param shift_plot_grid_x [0,1]. Extent to shift prob_plot. Increasing it from 0 moves the start of
#' the probability plot further to the right. Maximum value is 1 (at which point the plot will
#' effectively be pushed off the plotting surface). Useful to increase to a value such as 0.01
#' if the cytokine names are long and push the labelling grid too far to the right. Tweak as required.
#' Default is \code{0}.
#' @param plot_prob_fill character. If a colour, then the boxplots are filled according
#' to the specified value. If a named vector of colours names correspond to names
#' of the bottom-most level of the \code{c_obj} lists, then boxplots are filled
#' accordingly.  Otherwise, boxplots are filled according to names of lower-most list but using
#' @param save_format 'pdf' or 'png'. Plot device to use. Default is 'png'.
#' @param facet logical vector. Whether to facet or save individual plots for each
#' group in \code{c_obj}. If \code{TRUE} only, then only a faceted version is saved.
#' If \code{FALSE} only, then plots are saved individually. If \code{c(TRUE, FALSE)},
#' then both the facted and individual plots are saved.
#' @param file character. Name of saved plot. If \code{NULL}, then
#' set to `compass_boxplots`.
#' @param type 'pp' and/or 'scores'. Specifies response type(s) to plot.
#' If 'pp' is included, then posterior probabilities of individual cytokine combinations are plotted.
#' If 'scores' is included, then the PFS and FS responses are included. Note that, at this stage,
#' 'pp' needs to be included, and so is added even if missing from \code{type}. Default is \code{c('pp', 'scores')}.
#' @param shift_plot_scores_y numeric vector of length two, restricted to [-1,1]. Specifies the amount to
#' squeeze the scores plot in. The first element controls the bottom position of the plot,
#' and the second the top. For both elements, a positive value means moving upwards. A value of zero corresponds no shift.
#' Typically the bottom element and top element should both be shifted down (and therefore have negative values).
#' Only applies if type includes both \code{'pp'} and \code{'scores'}.
#' Default is \code{c(0,0)}.
#' @param shift_plot_pp_y [0,1]. Specifies amount by which to shift the upper point of the probability plot upwards.
#' Positive values make it larger. Typically you want to shift this down, if anything, so use negative values.
#' Default is 0.
#' @param label logical. If \code{TRUE} and \code{c_obj} is a list, then the names of
#' elements in \code{c_obj} are used as labels for sub-figures.
#' If \code{c_obj} is not a list, then no label is printed, regardless of the
#' value of \code{label}. Default is \code{TRUE}.
#' @param height,width numeric. Height and width, respectively (of course), of the saved figure (if saved). If
#' \code{NULL}, then appropriate values are guessed at and used. Default is \code{NULL}.
#' @param shift_label numeric vector, values in \code{[0,1]}. Specifies amount by which to shift
#' the labels of the plot (if used). Default is \code{c(0.05, -0.04)}.
#' @param prop_pp numeric vector, values in \code{[0,1]}. Specifies coordinate-wise the proportion of plot region
#' for a single group in \code{c_obj} (if \code{c_obj} is a list; otherwise it's simply the proportion of the
#' plot) that is devoted (before applying \code{shift_plot_pp_y}) to the plot of posterior probabilities.
#' The first element is the proportion allocated to the probability plot along the x-axis, and (\code{1-prop_pp[1]}) is
#' then the space allocated to the scores plot. The first element is set equal to 0 if there is no scores plot.
#' The second element is the proportion allocate to the probabilty plot along the y-axis, and (\code{1-prop_pp[1]}) is
#' then the space allocated to the cytokine indicator grid.
#' @param return_plot_list logical. If \code{TRUE}, then a named list of the plots used to create the figure are returned. The
#' first element is \code{'p_grid'} for the cytokine grid plot. The second element is \code{'p_probs'}  for
#' a list of the posterior probability plots. The third element is named \code{'p_scores'}, and has sub-elements that
#' are the plots of the PFS and FS responses for each group. The \code{'p_scores'} element is only supplied if \code{'scores' %in% type}.

#'
#' @return A list, where each element is a \code{ggplot2} object.
#'
#' @export
#'
#' @examples
#' plot_compass(c_obj)
#'
#' @importFrom rlang !! ensym
#' @import ggplot2
plot_compass <- function(c_obj, dir_save = getwd(),
                         type = c('pp', 'scores'),
                         save = TRUE, save_format = 'png',
                         prob_min = 0.8, quant_min = 0.25,
                         silent = FALSE, cyt_order = NULL,
                         file = NULL,
                         plot_prob_fill = NULL,
                         shift_plot_grid_x = 0,
                         shift_plot_scores_y = c(0,0),
                         shift_plot_pp_y = 0,
                         shift_label = c(0.05, -0.04),
                         prop_pp = c(0.7, 0.7),
                         label = TRUE,
                         return_plot_list = TRUE,
                         facet = FALSE,
                         n_col = NULL,
                         height = NULL,
                         width = NULL){

  # prep
  # -------------------

  init_list <- is.list(c_obj)
  init_name_vec <- switch(as.character(init_list),
                          "TRUE" = names(c_obj),
                          "FALSE" = NULL)
  if(is.factor(type)) type <- as.character(type)
  if(!'pp' %in% type) type <- c(type, 'pp')

  if(!length(setdiff(type, c("pp", "scores"))) == 0){
    stop("type must be a character vector with 'pp' and/or 'scores' as elements")
  }

  # ensure c_obj is a list
  if(!is.list(c_obj) || identical(class(c_obj), "COMPASSResult")){
    stop("c_obj must either have class COMPASSResult or be a (possible named) list of such objects.")
  }

  if(identical(class(c_obj), "COMPASSResult")){
    label <- FALSE
    c_obj <- list(c_obj)
  }
  if(is.list(c_obj)){
    if(length(c_obj) == 0) stop("no data in c_obj")
    all_compass_obj <- purrr::map_lgl(c_obj, function(x) identical(class(x), "COMPASSResult")) %>%
      all()
    if(!all_compass_obj) stop("not all elements in c_obj list have class COMPASSResult")
  }
  if(is.null(names(c_obj))){
    names(c_obj) <- paste0("grp", seq_along(c_obj))
  }

  if('pp' %in% type){
    p_list_pp <- .plot_compass_pp(
      c_obj = c_obj, dir_save = dir_save, prob_min = prob_min,
      quant_min = quant_min, silent = silent, cyt_order = cyt_order,
      plot_prob_fill = plot_prob_fill, facet = facet
    )
  }
  scores_ind <- 'scores' %in% type
  if(scores_ind){
    p_list_scores <- .plot_compass_scores(
      c_obj = c_obj,
      plot_prob_fill = plot_prob_fill
      )
  }


  p <- cowplot::ggdraw()
  n_grp <- length(p_list_pp$p_probs)
  if(!scores_ind) prop_pp[1] <- 1
  if(is.null(n_col)) n_col <- min(2, n_grp)
  n_row <- ceiling(n_grp/n_col)
  height_region <- 1/n_row
  height_pp_base <- height_region * prop_pp[2]
  height_pp <- height_pp_base + shift_plot_pp_y * height_pp_base
  height_grid <- height_region * (1 - prop_pp[2])
  width_region <- 1/n_col
  width_pp <- width_region * prop_pp[1]
  width_scores <- width_region * (1 - prop_pp[1])
  shift_label_x <- width_region * shift_label[1]
  shift_label_y <- height_region * shift_label[2]
  for(i in seq_along(p_list_pp$p_probs)){

    # overall location in grid
    row_ind <- i %/% n_col + i %% n_col
    col_ind <- i - (row_ind - 1) * n_col
    x_left_most <- (col_ind - 1) * width_region
    y_bottom_most <- 1 - row_ind * height_region

    # pp
    y_pp <- y_bottom_most + height_grid
    x_pp <- x_left_most

    # grid
    y_grid <- y_bottom_most
    x_grid_base <- x_left_most
    x_grid <- x_grid_base + shift_plot_grid_x * width_pp
    width_grid <- width_pp - shift_plot_grid_x * width_pp

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
    if(scores_ind){
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

    if(label){
      p <- p +
        cowplot::draw_text(
          text = names(p_list_pp$p_probs)[i],
          fontface = "bold",
          x = x_left_most + shift_label_x,
          y = y_bottom_most + height_region + shift_label_y,
          vjust = 0, hjust = 0
        )
    }
  }

  if(save){
    if(is.null(height)){
      height <- 29 * 9/16 * 0.9 * n_row
    }
    if(is.null(width)){
      width <- ifelse(n_col == 1, 18, 29.4)
    }
    cowplot::ggsave2(filename = file.path(dir_save,
                                          paste0(
                                            ifelse(is.null(file), 'compass_boxplots', file), '.', save_format
                                          )
    ),
    plot = p,
    units = 'cm', width = width, height = height)
  }


  if(!return_plot_list) return(invisible(TRUE))

  p_list_pp %>%
    append(switch(as.character(scores_ind), "TRUE" = list('p_scores' = p_list_scores), "FALSE" = list()))

}

# prep
# -------------------

#' @title Plot COMPASS PFS and FS scores
#'
#' @inheritParams plot_compass
#' @param c_obj a named list of COMPASSResult objects. Provides data to plot.
#'
#' @return A named list with names set to the names
#' of \code{c_obj}, where each element is the corresponding
#' boxplot of PFS and FS scores.
.plot_compass_scores <- function(c_obj, plot_prob_fill){

  # prep
  score_tbl <- purrr::map_df(seq_along(c_obj), function(i){
    x <- c_obj[[i]]
    pfs_vec <- COMPASS::PolyfunctionalityScore(x)
    fs_vec <- COMPASS::FunctionalityScore(x)
    tibble::tibble(.id = names(pfs_vec), .grp = names(c_obj)[i],
                   PFS = pfs_vec) %>%
      dplyr::mutate(FS = fs_vec[.id])
  }) %>%
    tidyr::pivot_longer(-c(.id, .grp),
                        names_to = "score_type",
                        values_to = "score")

  y_lim_vec <- c(0, max(score_tbl$score))
  col_vec_grp <- .get_col_vec_grp(
    plot_prob_fill = plot_prob_fill,
    .grp = names(c_obj)
  )

  purrr::map(names(c_obj), function(.grp_curr){
    ggplot(score_tbl %>%
             dplyr::filter(.grp == .grp_curr),
           aes(x = score_type, y = score, fill = .grp)) +
      geom_boxplot(outlier.size = 0.25, outlier.colour = 'gray50') +
      cowplot::theme_cowplot() +
      cowplot::background_grid() +
      labs(x = "Score type", y = "Score") +
      scale_fill_manual(values = col_vec_grp) +
      lims(y = y_lim_vec) +
      theme(legend.position = 'none')
  }) %>%
    setNames(names(c_obj))
}



#' @title Plot COMPASS posterior probabilites
#'
#' @inheritParams plot_compass
#' @param c_obj a named list of COMPASSResult objects. Provides data to plot.
#'
#' @return A named list with names 'p_probs' and 'p_grid',
#' corresponding to the list of ggplot2 plots of posterior plots
#' and the heat map of the corresponding cytokine combination
#' labels, respectively.
.plot_compass_pp <- function(c_obj, dir_save, prob_min, quant_min,
                             silent, cyt_order, plot_prob_fill, facet){
  pp_tbl <- purrr::map_df(seq_along(c_obj), function(i){
    x <- c_obj[[i]]
    pp_mat <- x$fit$mean_gamma
    colnames(pp_mat) <- convert_cyt_combn_format(
      colnames(pp_mat),
      to = 'std'
    )
    pp_tbl <- tibble::as_tibble(pp_mat)
    id_var <- x$data$individual_id
    pp_tbl[['.id']] <- x$data$meta[[id_var]]
    pp_tbl <- pp_tbl[,'.id'] %>%
      dplyr::bind_cols(pp_tbl[,!(colnames(pp_tbl) == '.id')])
    pp_tbl %>%
      tidyr::pivot_longer(-.id,
                          names_to = 'combn',
                          values_to = 'prob') %>%
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

  if(length(combn_sel_vec) == 0){
    if(!silent){
      warning(
        "No cytokine combinations met inclusion criteria. An unnamed list with a blank plot returned."
      )
    }
    return(switch(as.character(return_plot),
                  "TRUE" = switch(as.character(init_list),
                                  "TRUE" = purrr::map(c_obj, function(x) ggplot()) %>%
                                    setNames(init_name_vec)),
                  "FALSE" = invisible(TRUE)))
  }

  # calculate degree of each cytokine combination
  # ------------------------

  degree_lab_vec <- purrr::map_chr(combn_sel_vec, function(combn){
    nrow(stringr::str_locate_all(combn, "[[+]]")[[1]])
  }) %>%
    setNames(combn_sel_vec)

  pp_tbl %<>%
    dplyr::filter(combn %in% combn_sel_vec) %>%
    dplyr::mutate(degree = degree_lab_vec[combn])

  # order cyt combns within each degree by post probs
  # -------------------------
  combn_factor_levels_vec <- pp_tbl %>%
    dplyr::group_by(degree, combn, .grp) %>%
    dplyr::summarise(quant = quantile(prob, 0.75), .groups = 'drop') %>%
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
  if(is.null(cyt_order)){
    cyt_order <- stringr::str_split(pp_tbl[['combn']][1], "[[+-]]")[[1]]
    cyt_order <- cyt_order[-length(cyt_order)]
  } else{
    cyt_order_compass <- stringr::str_split(pp_tbl[['combn']][1], "[[+-]]")[[1]]
    cyt_order_compass <- cyt_order_compass[-length(cyt_order_compass)]
    if(!identical(sort(intersect(cyt_order_compass, cyt_order)), sort(cyt_order))){
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

  for(cyt in cyt_order){
    grid_tbl[[cyt]] <- as.character(stringr::str_detect(grid_tbl$combn, paste0(cyt, "[[+]]")))
  }

  grid_tbl %<>%
    tidyr::pivot_longer(-c(combn, degree),
                        names_to = 'cyt',
                        values_to = 'expressed') %>%
    dplyr::mutate(cyt = factor(.data$cyt,
                               levels = cyt_order))

  # set up colours
  # --------------------

  # grid
  col_vec <- RColorBrewer::brewer.pal(n = length(cyt_order),
                                      name = "YlOrBr")
  col_vec <- col_vec[-(1:((length(col_vec) - max(as.numeric(grid_tbl$degree)))))]
  expr_degree_lab_vec <- setNames(col_vec, paste0("TRUE", 1:length(col_vec)))
  expr_degree_lab_vec <- c(expr_degree_lab_vec, setNames(rep('white', length(col_vec)),
                                                         paste0("FALSE", 1:length(col_vec))))

  col_vec_grp <- .get_col_vec_grp(plot_prob_fill = plot_prob_fill, .grp = names(c_obj))

  # grid
  p_grid <- ggplot(grid_tbl %>%
                     dplyr::mutate(expr_degree = paste0(expressed, degree)),
                   aes(x = combn, y = cyt,
                       fill = expr_degree)) +
    cowplot::theme_cowplot() +
    #geom_raster(col = 'black', linetype = 'solid') +
    geom_tile(col = 'black') +
    scale_fill_manual(values = expr_degree_lab_vec) +
    theme(legend.position = 'none') +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())


  p_probs <- purrr::map(seq_along(c_obj), function(i){
    grp_curr <- names(c_obj)[i]
    p <- ggplot(pp_tbl %>%
                  dplyr::filter(.grp == grp_curr),
                aes(x = combn, y = prob, fill = .grp)) +
      cowplot::theme_cowplot() +
      cowplot::background_grid(major = 'y') +
      geom_boxplot(outlier.size = 0.25, outlier.colour = 'gray50') +
      scale_fill_manual(values = col_vec_grp) +
      lims(y = c(0,1)) +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = 'none') +
      labs(y = "Probability of a response") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank())
    if(facet) p <- p + facet_wrap(~.grp)
    p
  }) %>%
    setNames(names(c_obj))

  list("p_grid" = p_grid, "p_probs" = p_probs)
}


