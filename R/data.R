#' Example COMPASS result object
#'
#' A COMPASSResult object containing example data for testing and
#' demonstration purposes.
#'
#' @format A COMPASSResult object with the following structure:
#' \describe{
#'   \item{fit}{Fitted model results from COMPASS analysis}
#'   \item{data}{Input data used for COMPASS analysis}
#' }
#' @examples
#' data("c_obj", package = "UtilsCompassSV")
#' class(c_obj)
"c_obj"

#' Example list of COMPASS result objects
#'
#' A named list of COMPASSResult objects containing example data for
#' testing and demonstration purposes. Each element represents results
#' from different stimulation conditions.
#'
#' @format A list with 4 elements, each a COMPASSResult object:
#' \describe{
#'   \item{Live Mtb}{Results for live Mycobacterium tuberculosis stimulation}
#'   \item{Secreted Mtb proteins}{Results for secreted Mtb protein stimulation}
#'   \item{Non-secreted Mtb proteins}{Results for non-secreted Mtb protein stimulation}
#'   \item{EBV/CMV}{Results for EBV/CMV stimulation}
#' }
#' @examples
#' data("c_obj_list", package = "UtilsCompassSV")
#' names(c_obj_list)
"c_obj_list"
