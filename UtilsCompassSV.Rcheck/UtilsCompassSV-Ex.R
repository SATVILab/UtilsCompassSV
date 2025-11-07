pkgname <- "UtilsCompassSV"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "UtilsCompassSV-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('UtilsCompassSV')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("convert_cyt_combn_format")
### * convert_cyt_combn_format

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_cyt_combn_format
### Title: Convert cytokine combination format to or from COMPASS format
### Aliases: convert_cyt_combn_format

### ** Examples

convert_cyt_combn_format(c("IFNg&!IL2"), to = "std")
convert_cyt_combn_format(c("IFNg+IL2-"), to = "compass")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_cyt_combn_format", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dot-get_col_vec_grp")
### * dot-get_col_vec_grp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: .get_col_vec_grp
### Title: Get colours for groups for boxplots
### Aliases: .get_col_vec_grp

### ** Examples

.get_col_vec_grp(plot_prob_fill = "red", .grp = c("grp1", "grp2"))
.get_col_vec_grp(plot_prob_fill = NULL, .grp = c("grp1", "grp2"))
.get_col_vec_grp(plot_prob_fill = c("grp1" = "red", "grp2" = "orange"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dot-get_col_vec_grp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_compass")
### * plot_compass

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_compass
### Title: Plot COMPASS output
### Aliases: plot_compass

### ** Examples

library(UtilsCompassSV)
data("c_obj_list", package = "UtilsCompassSV")
plot_compass(
  c_obj = c_obj_list,
  type = c("pp", "scores"),
  return_plot_list = FALSE,
  shift_plot_scores = c(-0.05, 0.05),
  shift_plot_pp_y = -0.075,
  shift_plot_heatmap_x = 0.052
)
# The plot will then be saved to the working directory.

# Can also use Greek symbols for cytokines:
get_cyt_lab <- function(cyt) {
  lapply(cyt, function(cyt_ind) {
    switch(cyt_ind,
      "IFNg" = bquote("IFN" ~ gamma),
      cyt_ind
    )
  })
}
plot_compass(c_obj_list[1],
  type = c("pp"),
  return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
  shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.052,
  cyt_lab = get_cyt_lab
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_compass", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("response_prob")
### * response_prob

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: response_prob
### Title: Calculate COMPASS-derived overall responder probability
### Aliases: response_prob

### ** Examples

data("c_obj", package = "UtilsCompassSV")
response_prob(c_obj = c_obj)
response_prob(
  c_obj = c_obj,
  exc = c(
    "IFNg&IL2&TNF&!IL17&!IL6&!IL22",
    "IFNg&!IL2&TNF&!IL17&!IL6&!IL22"
  )
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("response_prob", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
