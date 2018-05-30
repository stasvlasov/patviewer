## Creates OrbisR package

## --------------------------------------------------------------------------------
## Load or Install Packages
## --------------------------------------------------------------------------------
for(pkg in c('pbapply'
           , 'stringr'
           , 'data.table'
           , 'dplyr'
           , 'magrittr'
           , 'devtools'
           , 'roxygen2'))
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg, repos = 'http://cloud.r-project.org', quiet = TRUE)
        library(pkg, character.only = TRUE)
    }
## --------------------------------------------------------------------------------


## Making a package
## --------------------------------------------------------------------------------
setwd("~/mega/data/patview/patviewer")

## Updates package info
person("Stas", "Vlasov", 
       email = "s.vlasov@uvt.nl", 
       role  = c("aut", "cre")) %>% 
    {paste0("'",., "'")} %>%
    {options(devtools.desc.author = .)}



## Assume that it runs from "harmonizer" directory
list(Title  = "Tools for working with PatentsView.org database"
   , Date = "2018-05-29"
   , License = "MIT License"
   , Description = "Set of functions that help to prepare, to load into R session and to search PatentsView.org data"
   , References = "http://patentsview.org") %>% 
    {setup(rstudio = FALSE
         , description = .)}


## Update name spaces and documentation for functions
roxygenise()

document()



## Testing
## --------------------------------------------------------------------------------
install(".")

## install_github("stasvlasov/harmonizer")
library('patviewer')






