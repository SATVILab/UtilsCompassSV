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
#' @param shift_plot_grid [0,1]. Extent to shift prob_plot. Increasing it from 0 moves the start of
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
#'
#' @param return_plot logical. If \code{TRUE}, then plot output is returned.

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
                         save = TRUE, save_format = 'png',
                         prob_min = 0.8, quant_min = 0.25,
                         silent = FALSE, cyt_order = NULL,
                         file = NULL,
                         plot_prob_fill = NULL,
                         shift_plot_grid = 0,
                         return_plot = TRUE,
                         facet = TRUE){

  # prep
  # -------------------

  init_list <- is.list(c_obj)
  init_name_vec <- switch(as.character(init_list),
                          "TRUE" = names(c_obj),
                          "FALSE" = NULL)

  # ensure c_obj is a list
  if(!is.list(c_obj) || identical(class(c_obj), "COMPASSResult")){
    stop("c_obj must either have class COMPASSResult or be a (possible named) list of such objects.")
  }

  if(identical(class(c_obj), "COMPASSResult")) c_obj <- list(c_obj)
  if(is.list(c_obj)){
    all_compass_obj <- purrr::map_lgl(c_obj, function(x) identical(class(x), "COMPASSResult")) %>%
      all()
    if(!all_compass_obj) stop("not all elements in c_obj list have class COMPASSResult")
  }
  if(is.null(names(c_obj))){
    names(c_obj) <- paste0("grp", seq_along(c_obj))
  }

  pp_tbl <- purrr::map_df(seq_along(c_obj), function(i){
    x <- c_obj[[i]]
    pp_mat <- x$fit$mean_gamma
    colnames(pp_mat) <- convert_cyt_combn_format(
      colnames(pp_mat),
      to = 'std'
    )
    pp_tbl <- tibble::as_tibble(pp_mat)
    id_var <- x$data$individual_id
    pp_tbl[[id_var]] <- x$data$meta[[id_var]]
    pp_tbl <- pp_tbl[,id_var] %>%
      dplyr::bind_cols(pp_tbl[,!(colnames(pp_tbl) == id_var)])
    pp_tbl %>%
      tidyr::pivot_longer(-(!!ensym(id_var)),
                          names_to = 'combn',
                          values_to = 'prob') %>%
      dplyr::mutate(grp = names(c_obj)[i])
  }) %>%
    dplyr::mutate(grp = factor(.data$grp, levels = names(c_obj)))


  # filter out low-response cytokine combinations
  # ----------------------
  combn_sel_vec <- pp_tbl %>%
    dplyr::filter(stringr::str_detect(combn, "[[+]]")) %>%
    dplyr::group_by(combn, grp) %>%
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
    dplyr::group_by(degree, combn, grp) %>%
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

  # box plots
  if(is.null(plot_prob_fill)){
    col_vec_grp <- try(setNames(RColorBrewer::brewer.pal(n = length(c_obj) + 2,
                                            name = "GnBu")[-c(1, length(c_obj) + 2)],
                                names(c_obj)))
    if(class(col_vec_grp) == 'try-error'){
      col_vec_grp <- try(setNames(RColorBrewer::brewer.pal(n = length(c_obj) + 2,
                                                  name = "GnBu"),
                         names(c_obj)))
      if(class(col_vec_grp) == 'try-error'){
        col_vec_grp <- setNames(rep("#2b8cbe", length(c_obj)),
                                names(c_obj))
        }
      }
    } else if(length(plot_prob_fill) == 1){
      col_vec_grp <- setNames(rep(plot_prob_fill, length(c_obj)),
                            names(c_obj))
      } else if(is.null(names(plot_prob_fill)) || length(plot_prob_fill) != length(c_obj)){
        col_vec_grp <- try(setNames(plot_prob_fill,
                                    names(c_obj)))
        if(class(col_vec_grp) == 'try-error'){
          warning("plot_prob_fill not of length 1 or same length as c_obj. Therefore default colour scheme used.")
          col_vec_grp <- try(RColorBrewer::brewer.pal(n = length(c_obj) + 2,
                                                      name = "GnBu")[-c(1, length(c_obj) + 2)])
          if(class(col_vec_grp) == 'try-error'){
            col_vec_grp <- try(RColorBrewer::brewer.pal(n = length(c_obj) + 2,
                                                        name = "GnBu"))
            if(class(col_vec_grp) == 'try-error'){
              col_vec_grp <- setNames(rep("#2b8cbe", length(c_obj)),
                                      names(c_obj))
            }
          }
          }
        } else{
    col_vec_grp <- setNames(plot_prob_fill, names(plot_prob_fill))
    }


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
                  dplyr::filter(grp == grp_curr),
                aes(x = combn, y = prob, fill = grp)) +
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
    if(facet) p <- p + facet_wrap(~grp)
    p
  }) %>%
    setNames(names(c_obj))

  p <- cowplot::ggdraw()
  for(i in seq_along(p_probs)){
    width <- 1/length(p_probs)
    p <- p +
      cowplot::draw_plot(plot = p_probs[[i]], y = 0.3,
                         height = 0.7, x = width * (i-1), width = width) +
      cowplot::draw_plot(p_grid, y = 0, height = 0.3,
                         x = shift_plot_grid * width + width * (i-1),
                         width = width - shift_plot_grid * width)
    # cowplot::ggsave2(filename = file.path(dir_save, paste0(names(p_probs)[i], ".png")),
    #                  plot = p,
    #                  units = 'cm', width = 29.4, height = 29 * 9/16 * 0.9)
  }

  if(save){
    cowplot::ggsave2(filename = file.path(dir_save,
                                          paste0(
                                            ifelse(is.null(file), 'compass_boxplots', file), '.', save_format
                                            )
                                          ),
                     plot = p,
                     units = 'cm', width = 29.4, height = 29 * 9/16 * 0.9)
  }

  if(!return_plot) return(invisible(TRUE))

  return(p)

}
