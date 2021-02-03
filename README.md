# Obesity Explorer

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/UBC-MDS/obesity-explorer-R/workflows/R-CMD-check/badge.svg)](https://github.com/UBC-MDS/obesity-explorer-R/actions)
[![Codecov test coverage](https://codecov.io/gh/UBC-MDS/obesity-explorer-R/branch/main/graph/badge.svg)](https://codecov.io/gh/UBC-MDS/obesity-explorer-R?branch=main)
<!-- badges: end -->

Authors: Dustin Burnham, Javairia Raza, Rafael Pilliard Hellwig, Tanmay Sharma

## Using the Obesity Dashboard

Obesity has been an increasing medical concern across the world in the 21st century. 
It is a medical precursor to diseases such as diabetes, heart diseases, high blood pressure and certain types of cancers. 
In spite of this, most people know very little about this disease, and are unaware of the factors that increase its relative risk. 
To increase awareness and deepen understanding, this [Obesity Dashboard](https://r-obesity-explorer.herokuapp.com/) allows users to explore obesity trends, probe associations with other variables and factors, and discover patterns related to this global epidemic.

[![App](doc/img/dashboard.gif)](https://r-obesity-explorer.herokuapp.com/)

## Contributing to the Dashboard

We love contributions! If you wish to help with this project or find a bug, please first review our [contributing guidelines](CONTRIBUTING.md).
Contributors are encouraged to clone the repository and install dependencies by running the following in an R session (the working directory must be at the project root):

```R
install.packages(devtools)
devtools::install_deps()
```

Once the requisite R packages are installed, the app can be run locally by executing the following at the terminal from the project root:

```bash
Rscript app.R
```

The URL path will be printed to screen, which can be copy-pasted into a browser. To exit the app, hit <kbd>Ctrl</kbd> + <kbd>C</kbd> in the terminal.

This app is written in R using the Dash framework, but the authors maintain a [parallel implementation](https://github.com/UBC-MDS/obesity-explorer) for Python. 
Details of the author's vision can be found in the [proposal document](doc/proposal.md).
