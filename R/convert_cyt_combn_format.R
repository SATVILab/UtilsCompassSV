#' @title Convert cytokine combination format to or from COMPASS format
#'
#' @description Convert cytokine combination format between COMPASS format and "standard " +/- format.
#' See \code{cyt_combn} parameter for details. NOTE: At present only converts from COMPASS to standard format.
#'
#' @param cyt_combn character vector. Cytokine combination, specified either in
#' "<cyt><+-><cyt><+-><cyt><+->..." (e.g. IFNg+IL2-TNF+) or
#' COMPASS "<!><cyt>&<!><cyt>&<!><cyt>..." (e.g. IFNg&!IL2&TNF) format.
#' @param to 'compass' or 'std'. Format to convert to. If \code{'compass'},
#' then output format is COMPASS format (specified above). If \code{'std'},
#' then output format is standard format (specified above).
#' @param force logical. If \code{TRUE}, then the code to convert to the specified format will be run.
#' If \code{FALSE}, then checks are made to ensure that conversion is not done if already in the
#' requested format. Default is \code{FALSE}.
#' @param silent logical. If \code{FALSE}, then warnings are printed if \code{force == FALSE} and
#' it is detected that \code{cyt_combn} already appears to be in the requested format. Default is \code{FALSE}.
#' @param check logical. If \code{TRUE}, then the converted format is checked that each element has
#' the same number of cytokines. Note that check is only performed if the conversion is attempted. Default is \code{TRUE}.
#' @param lab named character vector. Names are names for markers/channels as found in the cytokine combination,
#' and elements are corresponding names/channels. If \code{NULL}, then no labelling is done. Default is \code{NULL}.
#' @return character vector.
#'
#' @export
#'
#' @examples
#' convert_cyt_combn_format(c("IFNg&!IL2"), to = 'std')
#'
#' @importFrom magrittr %>% %<>%
convert_cyt_combn_format <- function(cyt_combn, to, force = FALSE, silent = FALSE,
                                     check = TRUE, lab = NULL){

  # prep
  # ------------------

  if(missing(cyt_combn)) stop("cyt_combn must be specified",
                              call. = FALSE)

  if(!is.character(cyt_combn) && !is.factor(cyt_combn)) stop("cyt_combn must be a character or factor.",
                                                             call. = FALSE)

  # checks
  if(missing(to)) stop("must specify `to` parameters",
                       call. = FALSE)
  if(!to %in% c("compass", "std")) stop("`to` parameter must be either 'compass' or 'std'",
                                        call. = FALSE)

  if(!is.logical(force)) stop("force parameter must be of class logical",
                              call. = FALSE)
  if(!is.logical(silent)) stop("silent parameter must be of class logical",
                               call. = FALSE)


  # convert
  # -------------------

  if(to == 'std'){

    # check for ampersands, if conversion not forced
    if(!force){
      if(!stringr::str_detect(cyt_combn[1], "&")){
        if(!silent){
          warning("No &'s detected in cyt_combn, and so no conversion to compass format done as force != TRUE")
        }
        return(cyt_combn)
      } else NULL
    } else NULL

    # convert
    out_vec <- purrr::map_chr(cyt_combn, function(x){
      purrr::map_chr(stringr::str_split(x, "&")[[1]], function(elem){
        if(stringr::str_detect(elem, "[[!]]")){
          cyt <- stringr::str_sub(elem, start = 2)
          if(!is.null(lab)) cyt <- lab[cyt]
          return(paste0(cyt, "-"))
        }
        if(!is.null(lab)) elem <- lab[elem]
        paste0(elem, "+")
      }) %>%
        unlist() %>%
        paste0(collapse = "")
    })

    # check
    if(check){
      n_cyt <- purrr::map_dbl(out_vec, function(x){
        cyt_vec <- stringr::str_split(x, "[[+-]]")[[1]]
        len <- length(cyt_vec)
        cyt_vec <- cyt_vec[-len]
        if(any(cyt_vec == "")){
          stop("At least one cytokine is a character of length zero after conversion.")
        }
        len-1
      }) %>%
        unique()
      if(length(n_cyt) != 1){
        stop(paste0("intended output has differing numbers of cytokines in for each cytokine combination."),
             call. = FALSE)
      }
    }
  } else if(to == 'compass'){
    # convert
    out_vec <- purrr::map_chr(cyt_combn, function(x){
      cyt_vec <- stringr::str_split(x, "[[+-]]")[[1]]
      cyt_vec <- cyt_vec[cyt_vec != ""]
      if(!is.null(lab)) cyt_vec <- lab[cyt_vec]
      level_vec <- stringr::str_split(x, "\\w")[[1]]
      level_vec <- level_vec[level_vec != ""]
      out <- cyt_vec[1]
      if(identical(level_vec[1], "-")) out <- paste0("!", out)
      cyt_vec <- cyt_vec[-1]
      level_vec <- level_vec[-1]
      for(i in seq_along(cyt_vec)){
        if(identical(level_vec[i], "+")){
          out <- paste0(out, "&", cyt_vec[i])
        } else{
          out <- paste0(out, "&!", cyt_vec[i])
        }
      }
      out
    })
  }

  out_vec

}
