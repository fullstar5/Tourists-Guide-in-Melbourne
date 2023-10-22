# Tourists-Guide-in-Melbourne
The University of Melbourne Project

# R Shiny Application Guide

## Prerequisites

- Ensure [R](https://cran.r-project.org/mirrors.html) is installed on your machine.
- [RStudio](https://rstudio.com/products/rstudio/download/) or a similar R interface should be set up for running R scripts.
- Maintain an active internet connection, especially since the application uses APIs and Tableau Public.
- The file `tableau-in-shiny-v1.0.R`, provided by the instructor's team for embedding Tableau in R, should be in the same directory as `app.R`.

## Installing Required Libraries

If you haven't already installed the necessary libraries, execute the following command in R:
```R
install.packages(c("shiny", "shinydashboard", "bslib", "leaflet", "dplyr", "leaflet.extras", "httr", "jsonlite", "purrr", "sf", "htmltools"))
```



## Launching the Interface
Open RStudio or your preferred R environment.
Set your working directory to where the R Shiny application files (app.R) are located using the setwd() function.

Run the main app.R file with the following command:
```R
shiny::runApp('app.R')
```

Alternatively, you can click the 'Run App' button located at the top right of the R Studio interface.

## Feedback
If you encounter any issues or have queries, please refer to the code documentation/comments or contact our team members.

## Team Members' Contact:
- Hongyu Su: suhongyu30@gmail.com
- Ruotong Zhao: ruotzhao@student.unimelb.edu.au
- YiFei ZHANG: yifeizhang2@student.unimelb.edu.au
- Fan Pu: fppu@student.unimelb.edu.au

Thank you for your review!

---

IV Assignment3 - Group 37

10/22/2023
