# R script to run author supplied code, typically used to install additional R packages
# contains placeholders which are inserted by the compile script
# NOTE: this script is executed in the chroot context; check paths!

r <- getOption('repos')
r['CRAN'] <- 'https://mran.microsoft.com/snapshot/2021-01-30'
options(repos=r)

# ======================================================================

# packages go here
install.packages(c('dash', 'readr', 'here', 'ggthemes', 'remotes',
                   'tidyverse','plotly', 'lintr', 'spelling',
                   'checkmate', 'testthat', 'devtools', 'dplyr'))
remotes::install_github('facultyai/dash-bootstrap-components@r-release')