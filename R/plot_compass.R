#' @title Plot COMPASS output
#'
#' @description  Plot COMPASS posterior probabilities as boxplots, and the PFS scores if desired.
#' A grid of figures is created whenever multiple COMPASS objects are plotted simultaneous, with
#' y-axis scales and cytokine combinations displayed fixed across plots.
#'
#'' @param c_obj object of class "COMPASSResult", or a list of such objects. Provides
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
#' @param shift_plot_heatmap_x [0,1]. Extent to shift prob_plot. Increasing it from 0 moves the start of
#' the probability plot further to the right. Maximum value is 1 (at which point the plot will
#' effectively be pushed off the plotting surface). Useful to increase to a value such as 0.01
#' if the cytokine names are long and push the labelling grid too far to the right. Tweak as required.
#' Default is \code{0}.
#' @param plot_prob_fill character. If a colour, then the boxplots are filled according
#' to the specified value. If a named vector of colours names correspond to names
#' of the bottom-most level of the \code{c_obj} lists, then boxplots are filled
#' accordingly.  Otherwise, boxplots are filled according to names of lower-most list but using
#' @param save_format "pdf" or "png". Plot device to use. Default is "png".
#' @param facet logical vector. Whether to facet or save individual plots for each
#' group in \code{c_obj}. If \code{TRUE} only, then only a faceted version is saved.
#' If \code{FALSE} only, then plots are saved individually. If \code{c(TRUE, FALSE)},
#' then both the facted and individual plots are saved.
#' @param type "pp" and/or "scores". Specifies response type(s) to plot.
#' If "pp" is included, then posterior probabilities of individual cytokine combinations are plotted.
#' If "scores" is included, then the PFS and FS responses are included. Note that, at this stage,
#' "pp" needs to be included, and so is added even if missing from \code{type}. Default is \code{c("pp", "scores")}.
#' @param shift_plot_scores_y numeric vector of length two, restricted to [-1,1]. Specifies the amount to
#' squeeze the scores plot in. The first element controls the bottom position of the plot,
#' and the second the top. For both elements, a positive value means moving upwards. A value of zero corresponds no shift.
#' Typically the bottom element and top element should both be shifted down (and therefore have negative values).
#' Only applies if type includes both \code{"pp"} and \code{"scores"}.
#' Default is \code{c(0,0)}.
#' @param shift_plot_pp_y [0,1]. Specifies amount by which to shift the upper point of the probability plot upwards.
#' Positive values make it larger. Typically you want to shift this down, if anything, so use negative values.
#' Default is 0.
#' @param boxplot_width_pp,boxplot_width_scores numeric. If not \code{NULL}, then supplied to \code{width} parameter of
#' \code{geom_boxplot} for the posterior probability and scores plots, respectively. Purpose is
#' to force the widths of the boxplots to be constant across elements in \code{c_obj}.
#' Default is \code{NULL}.
#' @param label logical. If \code{TRUE} and \code{c_obj} is a list, then the names of
#' elements in \code{c_obj} are used as labels for sub-figures.
#' If \code{c_obj} is not a list, then no label is printed, regardless of the
#' value of \code{label}. Default is \code{TRUE}.
#' @param height_grid,width_grid numeric. Height and width, respectively, in centimetres of the saved figure (if saved). If
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
#' first element is \code{"p_grid"} for the cytokine grid plot. The second element is \code{"p_probs"}  for
#' a list of the posterior probability plots. The third element is named \code{"p_scores"}, and has sub-elements that
#' are the plots of the PFS and FS responses for each group. The \code{"p_scores"} element is only supplied if \code{"scores"} is
#' in \code{"type"}.
#' @param cyt_lab function. A function that takes the cytokine names as input and returns the
#' value desired to be plotted along the y-axis of the grid plot. If not \code{NULL}, then
#' it is supplied when creating the grid plot to the \code{scale_y_continuous} function
#' via the \code{labels} parameter. For example, if we have two cytokines, \code{"IFNg"} and
#' \code{"TNF"}, but we want to display \code{"IFNg"} with the Greek gamma symbol, then we can
#' set \code{cyt_lab} equal to the following: \code{cyt_lab = function(cyt) purrr::map(cyt, function(cyt_ind){switch(cyt_ind, "IFNg" = bquote(paste(plain(paste("IFN")), gamma)), cyt_ind)})}.
#' This will change the label for \code{"IFNg"} but leave all the others as is.
#' @param save_ind logical. If \code{TRUE}, then plots for individual elements in \code{c_obj} are saved.
#' Default is \code{FALSE}.
#' @param save_grid logical. If \code{TRUE}, then a grid of all individual elements in \code{c_obj} are saved.
#' Default is \code{TRUE}.
#' @param height_ind,width_ind numeric. Height and width, respectively, in centimetres of the saved figure (if saved),
#' when saving individual elements in \code{c_obj}. If
#' \code{NULL}, then appropriate values are guessed at and used. Default is \code{NULL}.
#' @param file_grid,file_ind character (vector).
#' Names for grid plot and individual plots, respectively, to be saved as.
#' If \\code{is.null(file)}, then the grid plot is
#' simply named \code{compass_boxplots_grid}.
#' If \\code{is.null(file_ind)}, then each element
#' has its name taken from the corresponding list name in \code{c_obj}.
#' If \code{file_ind} is named, then the names of \code{c_obj}
#' are used to map onto elements of \code{file_ind}.
#' @param plot_scores_lims_y numeric vector of length 2.
#' If not \code{NULL}, specifies the y-axis range for the scores plots.
#' Default is \code{NULL}.
#' @return A list, where each element is a \code{ggplot2} object.
#'
#' @export
#'
#' @examples
#' library(UtilsCompassSV)
#' data("c_obj_list", package = "UtilsCompassSV")
#' plot_compass(
#'   c_obj = c_obj_list,
#'   type = c("pp", "scores"),
#'   return_plot_list = FALSE,
#'   shift_plot_scores = c(-0.05, 0.05),
#'   shift_plot_pp_y = -0.075,
#'   shift_plot_heatmap_x = 0.052
#' )
#' # The plot will then be saved to the working directory.
#'
#' # Can also use Greek symbols for cytokines:
#' get_cyt_lab <- function(cyt) {
#'   lapply(cyt, function(cyt_ind) {
#'     switch(cyt_ind,
#'       "IFNg" = bquote("IFN" ~ gamma),
#'       cyt_ind
#'     )
#'   })
#' }
#' plot_compass(c_obj_list[1],
#'   type = c("pp"),
#'   return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
#'   shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.052,
#'   cyt_lab = get_cyt_lab
#' )
#' @importFrom rlang !! ensym
#' @import ggplot2
plot_compass <- function(c_obj,
                         dir_save = getwd(),
                         type = c("pp", "scores"),
                         save = TRUE,
                         save_format = "png",
                         prob_min = 0.8,
                         quant_min = 0.25,
                         boxplot_width_scores = NULL,
                         boxplot_width_pp = NULL,
                         silent = FALSE,
                         cyt_order = NULL,
                         file_grid = NULL,
                         file_ind = NULL,
                         plot_prob_fill = NULL,
                         shift_plot_heatmap_x = 0,
                         shift_plot_scores_y = c(0, 0),
                         shift_plot_pp_y = 0,
                         shift_label = c(0.05, -0.04),
                         prop_pp = c(0.7, 0.7),
                         label = TRUE,
                         return_plot_list = TRUE,
                         facet = FALSE,
                         n_col = NULL,
                         cyt_lab = NULL,
                         save_grid = TRUE,
                         height_grid = NULL,
                         width_grid = NULL,
                         save_ind = FALSE,
                         font_size = 14,
                         line_width = 0.5,
                         height_ind = NULL,
                         width_ind = NULL,
                         plot_scores_lims_y = NULL,
                         font_size_labels = 14,
                         tile_colour = NULL) {

  # prep
  # -------------------

  init_list <- is.list(c_obj)
  init_name_vec <- switch(as.character(init_list),
    "TRUE" = names(c_obj),
    "FALSE" = NULL
  )
  if (is.factor(type)) type <- as.character(type)
  if (!"pp" %in% type) type <- c(type, "pp")

  if (!length(setdiff(type, c("pp", "scores"))) == 0) {
    stop("type must be a character vector with 'pp'
    and/or 'scores' as elements")
  }

  # ensure c_obj is a list
  if (!is.list(c_obj) || identical(class(c_obj), "COMPASSResult")) {
    stop("c_obj must either have class COMPASSResult
     or be a (possible named) list of such objects.")
  }

  if (identical(class(c_obj), "COMPASSResult")) {
    label <- FALSE
    c_obj <- list(c_obj)
  }
  if (is.list(c_obj)) {
    if (length(c_obj) == 0) stop("no data in c_obj")
    all_compass_obj <- purrr::map_lgl(c_obj, function(x) {
      identical(class(x), "COMPASSResult")
    }) %>%
      all()
    if (!all_compass_obj) {
      stop("not all elements in c_obj list have class COMPASSResult")
    }
  }
  if (is.null(names(c_obj))) {
    names(c_obj) <- paste0("grp", seq_along(c_obj))
  }

  if ("pp" %in% type) {
    p_list_pp <- .plot_compass_pp(
      c_obj = c_obj, dir_save = dir_save, prob_min = prob_min,
      quant_min = quant_min, silent = silent, cyt_order = cyt_order,
      boxplot_width = boxplot_width_pp,
      plot_prob_fill = plot_prob_fill, facet = facet,
      cyt_lab = cyt_lab, font_size = font_size, line_width = line_width,
      tile_colour = tile_colour
    )
  }

  scores_ind <- "scores" %in% type
  if (scores_ind) {
    p_list_scores <- .plot_compass_scores(
      c_obj = c_obj,
      boxplot_width = boxplot_width_scores,
      plot_prob_fill = plot_prob_fill,
      plot_scores_lims_y = plot_scores_lims_y,
      font_size = font_size,
      line_width = line_width
    )
  } else {
    p_list_scores <- NULL
  }

  # save individaul plots
  if (save_ind) {
    .save_layout(
      ind = TRUE, p_list_pp = p_list_pp,
      p_list_scores = p_list_scores,
      shift_plot_heatmap_x = shift_plot_heatmap_x,
      shift_plot_scores_y = shift_plot_scores_y,
      shift_plot_pp_y = shift_plot_pp_y,
      prop_pp = prop_pp,
      height = height_ind, width = width_ind,
      dir_save = dir_save,
      file = file_ind,
      save_format = save_format,
      label = label,
      n_col = 1,
      shift_label = shift_label,
      font_size_labels = font_size_labels
    )
  }

  if (save_grid) {
    .save_layout(
      ind = FALSE, p_list_pp = p_list_pp,
      p_list_scores = p_list_scores,
      shift_plot_heatmap_x = shift_plot_heatmap_x,
      shift_plot_scores_y = shift_plot_scores_y,
      shift_plot_pp_y = shift_plot_pp_y,
      prop_pp = prop_pp,
      height = height_grid, width = width_grid,
      dir_save = dir_save,
      file = file_grid,
      n_col = n_col,
      save_format = save_format,
      label = label,
      shift_label = shift_label,
      font_size_labels = font_size_labels
    )
  }


  if (!return_plot_list) {
    return(invisible(TRUE))
  }

  p_list_pp %>%
    append(
      switch(as.character(scores_ind),
        "TRUE" = list("p_scores" = p_list_scores),
        "FALSE" = list()
      )
    )
}
