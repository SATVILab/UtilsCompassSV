#' @title Get colours for groups for boxplots
#'
#' @inheritParams plot_compass
#' @param .grp character vector. Names of groups in COMPASS object.
#'
#' @return A named character vector, where names are names of groups
#' and elements are colours for the groups
#'
#' @examples
#' .get_col_vec_grp(plot_prob_fill = "red", .grp = c("grp1", "grp2"))
#' .get_col_vec_grp(plot_prob_fill = NULL, .grp = c("grp1", "grp2"))
#' .get_col_vec_grp(plot_prob_fill = c("grp1" = "red", "grp2" = "orange"))
.get_col_vec_grp <- function(plot_prob_fill = NULL, .grp = NULL){

  if(!is.null(names(plot_prob_fill))){
    if(!is.character(plot_prob_fill)){
      stop("plot_prob_fill must be of type character if not NULL")
    }
    return(plot_prob_fill)
  }

  if(is.null(.grp)) stop(".grp must be specified if plot_prob_fill not a named character vector")

  # plot_prob_fill not specified
  if(is.null(plot_prob_fill)){
    return(.get_col_vec_grp_null(.grp = .grp))
  }

  if(!is.character(plot_prob_fill)) stop("plot_prob_fill must be of type character if not NULL")

  if(length(plot_prob_fill) == 1){
    return(setNames(rep(plot_prob_fill, length(.grp)),
                    .grp))
  }

  col_vec_grp <- try(setNames(plot_prob_fill,
                              .grp))
  if(class(col_vec_grp) != 'try-error') return(col_vec_grp)

  warning("plot_prob_fill not of length 1 or same length as .grp Therefore default colour scheme used.")
  .get_col_vec_grp_null(.grp = .grp)
}

#' @title Get colour vector for groups if plot_prob_fill is NULL
.get_col_vec_grp_null <- function(.grp){
  col_vec_grp <- try(setNames(RColorBrewer::brewer.pal(n = length(.grp) + 2,
                                                       name = "GnBu")[-c(1, length(.grp) + 2)],
                              .grp))
  if(!class(col_vec_grp) == 'try-error') return(col_vec_grp)

  col_vec_grp <- try(setNames(RColorBrewer::brewer.pal(n = length(.grp) + 2,
                                                       name = "GnBu"),
                              .grp))
  if(class(col_vec_grp) != 'try-error') return(col_vec_grp)

  setNames(rep("#2b8cbe", length(.grp)), .grp)
}
