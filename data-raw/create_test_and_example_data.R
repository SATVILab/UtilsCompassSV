if(!require('cytofacs')){
  if(!require('devtools')) install.packages('devtools')
  devtools::install_github('SATVILab/cytofacs.git')
}

# individual output
c_obj <- cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$p1
usethis::use_data(c_obj, overwrite = TRUE)

# list
c_obj_list <- list("p1" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$p1,
                   "mtb" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$mtb,
                   "ebv" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$ebv,
                   "p4" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$p4) %>%
  setNames(c("Secreted Mtb proteins", "Live Mtb", "EBV/CMV", "Non-secreted Mtb proteins")) %>%
  magrittr::extract(c(2, 1, 4, 3))
usethis::use_data(c_obj_list, overwrite = TRUE)
