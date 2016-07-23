# Check if devtools and shinysky are installed
pkgs <- c("devtools","shinysky")
pkgs_miss <- pkgs[!(pkgs %in% installed.packages())]
if(length(pkgs_miss)>0L){
  install.packages(pkgs_miss,
                   repos = "https://cloud.r-project.org/")
  # Shinysky outside CRAN
  devtools::install_github("AnalytixWare/ShinySky")


}
devtools::install_github("luismurao/leaflet")
# NicheToolbox dependencies

pkgs_ntb <- c('ggplot2',"leaflet",'plot3D','mixtools',
              'pvclust','fpc','snow',"sqldf",'shinysky',
              'psych','vcd','shinythemes',
              'KernSmooth','shinysky','nichetoolbox')

# Missing packages
pkgs_ntb_miss <- pkgs_ntb[!(pkgs_ntb %in% installed.packages())]
# Install missing packages
if(length(pkgs_ntb_miss)>0L)
  install.packages(pkgs_ntb_miss,
                   repos = "https://cloud.r-project.org/")
# Load packages
options(rgl.useNULL=TRUE)
sapply(pkgs_ntb,function(x) library(x,character.only = TRUE))
#rgl.init()




