# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shinydashboardPlus",
                "shinycssloaders",
                "shinythemes",
                "shinyWidgets",
                "knitr",
                "rgdal",
                "pander",
                "kableExtra",
                "leaflet",
                "leaflet.extras",
                "mapedit",
                "readr",
                "ncdf4",
                "ggplot2",
                "rasterVis",
                "sf",
                "raster",
                "sp",
                "knitr",
                "htmltools",
                "plyr",
                "dplyr")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))