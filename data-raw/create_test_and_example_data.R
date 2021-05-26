if(!require('cytofacs')){
  if(!require('devtools')) install.packages('devtools')
  devtools::install_github('SATVILab/cytofacs.git')
}

# individual output
c_obj <- cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$p1

# list
c_obj_list <- list("cd4" = list("p1" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$p1,
                                "mtb" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$mtb),
                   "cd8" = list("p1" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$p1,
                                "ebv" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$ebv,
                                "p4" = cytofacs::cd4_th1_il17_il22_il6$compass$locb0.15_min_clust$p4))
