ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("dplyr", "tidyr", "ggplot2", "gridExtra", 
              "ranger", "hydroGOF",    #random forest and KGE
              "doParallel", "foreach", #do parallel
              "ggrepel", "ggpmisc"  #visualization
              )
ipak(packages)