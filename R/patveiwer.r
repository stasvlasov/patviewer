## Tools for working with PatentsView.org
## ================================================================================



## --------------------------------------------------------------------------------
## Load or Install Packages (for testing)
## --------------------------------------------------------------------------------
## for(pkg in c('pbapply'
##            , "stringi"
##            , 'stringr'
##            , 'data.table'
##            , 'dplyr'
##            , 'magrittr'
##            , 'htmltab'
##            , 'XML'
##            ))
##     if(!require(pkg, character.only = TRUE)) {
##         install.packages(pkg, repos = 'http://cloud.r-project.org')
##         library(pkg, character.only = TRUE)
##     }
## --------------------------------------------------------------------------------






## ================================================================================
## Utilites functions
## ================================================================================
is.0 <- function(x) length(x) == 0



## ================================================================================
## Main functions
## ================================================================================

## --------------------------------------------------------------------------------
#' Downloads PatentsView.org files
#' 
#' @param url URL of the file
#' @param dest.file Safe as name. Default is the same as it is in the url.
#' @param dest.dir Where to save. Default if to the working directory patview-data-XXXXXXXX-zip, where XXXXXXXX is the verstion (i.e., date) of files automatically detected from the url (assumes certain form of url)
#' @return Saved file path.
#' @import magrittr stringr
#' @export
patview.download.file <- function(url
                                , dest.file = character(0)
                                , dest.dir = character(0)) {
    ## Assumes certain form of the link
    patview.data.version <- url %>% str_extract("\\d{8}")
    patview.data.name <- url %>% str_extract("[^/]+$")
    if(dest.file %>% is.0) {
        dest.file <- patview.data.name %>%
            str_replace_all("_", ".")
    }
    if(dest.dir %>% is.0) {
        dest.dir <- "patview-data-zip" %>% 
            file.path(getwd(), .)
    }
    if(!dir.exists(dest.dir)) dir.create(dest.dir)
    dest.file.path <- paste0("patview-"
                           , patview.data.version, "-"
                           , dest.file) %>% 
        file.path(dest.dir, .)
    if(dest.file.path %>% file.exists) {
        message("File '", dest.file, "' is already exist. Exiting.")
        return() %>% invisible
    } else {
        download.file(url
                    , destfile = dest.file.path
                    , method = "curl")
        message("File downloaded - ", dest.file)
        return(dest.file.path)
    }
}



## Tests
## "http://www.patentsview.org/download/"
## "http://www.patentsview.org/data/20171226/nber_category.tsv.zip" %>% 
##     patview.download.file



## --------------------------------------------------------------------------------
#' Downloads list of files from PatentsView.org that are available for download
#' 
#' @param patview.files.list.path A path to the html page with the table that contains the list of files to download. Will try to download it if it not founl. The default is in R working directury "patview.files.list.html"
#' @return Saved file path.
#' @export
patview.download.files.list <- function(patview.files.list.path = file.path(getwd(), "patview.files.list.html")) {
    if(!file.exists(patview.files.list.path)) {
        download.file("http://www.patentsview.org/download/"
                    , destfile = patview.files.list.path)
        message("File downloaded - ", patview.files.list.path)
        return(patview.files.list.path)
    } else message("File exists. Delete it if you what a new one or set new name")
}



## Tests
## patview.download.files.list()




## --------------------------------------------------------------------------------
#' Gets list of files from PatentsView.org that are available for download
#' 
#' @param patview.files.list.path A path to the html page with the table that contains the list of files to download. Will try to download it if it not founl. The default is in R working directury "patview.files.list.html"
#' @return Saved file path.
#' @import magrittr stringr htmltab dplyr XML
#' @export
patview.get.files.list <- function(patview.files.list.path = file.path(getwd(), "patview.files.list.html")) {
    if(!file.exists(patview.files.list.path)) patview.files.list.path %<>% patview.download.files.list
    if(file.exists(patview.files.list.path)) {
    cbind(patview.files.list.path %>%
          htmltab %>%
          set_names(names(.) %>% make.names %>% tolower)
        , url = patview.files.list.path %>% 
              htmltab(bodyFun = "getHTMLLinks") %>%
              set_names(names(.) %>%
                        make.names %>%
                        tolower) %>%
              extract2("table.name")
        , stringsAsFactors = FALSE) %>%
        transmute(name = str_extract(table.name, "^[a-zA-Z_]+")
                , url
                , size = str_replace(table.name, "^[a-zA-Z_]+", "")
                , description
                , rows = x..of.rows
                , origin) %>%
        return
    } else {
        message("Can't find the file with list of files and their urls.")
        return()
    }
}


## Tests
## patview.get.files.list()



## --------------------------------------------------------------------------------
#' Unzips (PatentsView.org bulk data) files.
#'
#' It also renames the files ("_" -> ".")
#' @param zipfile File name to unzip.
#' @param zipdir Default is in working directory "patview-data-zip"
#' @param exdir A path where to extract zip file. Default is in working directory "patview-data-tsv"
#' @return Unzipped file path.
#' @import magrittr stringr
#' @export
patview.unzip.file <- function(zipfile
                             , zipdir = file.path(getwd(), "patview-data-zip")
                             , exdir = file.path(getwd(), "patview-data-tsv")) {
    if(!dir.exists(exdir)) dir.create(exdir)
    if(str_detect(zipfile, exdir %>% list.files) %>% any) {
        message("Seems lile file ", zipfile, " is already extracted. Exiting.")
    } else {
        zipfile %>%
            file.path(zipdir,.) %>% 
            unzip(exdir = exdir) %T>% 
            file.rename(str_replace_all(.,"_", ".")) %>%
            str_replace_all("_", ".") %>% 
            return
    }
}



## Tests
## "patview-20171226-nber.category.tsv.zip" %>% patview.unzip.file




## Calculates number of lines a file has
get.file.nlines <- function(file.name, dir.path = getwd()) {
    file.name %>%
        file.path(dir.path, .) %>% 
        paste("grep -c $", .) %>%
        system(intern = TRUE) %>%
        as.numeric
}


## Tests
## "patview-data-tsv/nber.category.tsv" %>% get.file.nlines



## --------------------------------------------------------------------------------
#' Reads PatentsView .tsv files and saves them to .rds (also optional in batches)
#' 
#' @param file File name (.tsv is expected)
#' @param dir Directory where the file is. Default is in working directory "patview-data-tsv"
#' @param dir.rds Where to save. Default is in working directory "patview-data-rds"
#' @param batch.lines How many lines is to read in one batch (10^7 is recomended). The default is 0, meanning that reading will be done in one batch as a single file.
#' @param file.lines Length of the .tsv file. The default is 0. If is is not changed and batch.lines is specified then it will try to calculate it with grep.
#' @return Saved file(s) path.
#' @import magrittr stringr data.table
#' @export
patview.save.rds <- function(file
                           , dir = file.path(getwd(), "patview-data-tsv")
                           , dir.rds = file.path(getwd(), "patview-data-rds")
                           , batch.lines = 0
                           , file.lines = 0) {
    if(!dir.exists(dir.rds)) dir.create(dir.rds)
    if(batch.lines == 0) {
        file.rds.path <- file %>%
            str_replace("\\.tsv$", "") %>% 
            paste0(".rds") %>%
            file.path(dir.rds, .)
        if(file.rds.path %>% file.exists) {
            message("File ", file, " exists. Delete it if you want to replace.")
            return()
        }
        file.path(dir, file) %>%
            fread(stringsAsFactors = FALSE) %>%
            saveRDS(file = file.rds.path)
        return(file.rds.path)
    } else {
        if(file.lines == 0) file.lines <- get.file.nlines(file, dir)
        batch.file.format <- paste0("%0", nchar(file.lines), "d")
        ## Set start read rows for fread
        rows.skip <- seq(from = 0
                       , to = file.lines
                       , by = batch.lines)
        rows.read <- rows.skip[-1] %>%
            c(file.lines) %>%
            '-'(rows.skip)
        field.names <- 
            file.path(dir, file) %>%
            fread(nrows = 1
                , header = FALSE) %>%
            make.names
        sapply(1:length(rows.read), function(i) {
            ## extract batch
            message("* Reading lines from ", rows.skip[i])
            started <- Sys.time()
            file.rds.path <- file %>%
                str_replace("\\.tsv$", "") %>%
                paste0("-"
                     , sprintf(batch.file.format, rows.skip[i]), "-"  # add padding
                     , sprintf(batch.file.format, rows.skip[i] + rows.read[i] - 1)
                     , ".rds") %>%
                file.path(dir.rds, .)
            if(file.rds.path %>% file.exists) {
                message("File exists. Delete it if you want to replace.")
            } else {
                message("  - Started: ", date())
                file.path(dir, file) %>%
                    fread(nrows = rows.read[i]
                        , header = FALSE
                        , skip = rows.skip[i]
                        , showProgress = TRUE
                        , strip.white = FALSE
                        , quote = ""
                        , sep = "\t"
                        , stringsAsFactors = FALSE
                        , colClasses = rep("character", length(field.names))) %>%
                    set_names(field.names) %>% 
                    saveRDS(file.rds.path)
                message("  - Done! (in ", as.numeric(Sys.time() - started) %>% round, " minutes)")
            }
            return(file.rds.path)
        }) %>% return
    }
}




## Tests
## "nber.category.tsv" %>% patview.save.rds(batch.lines = 2)
## readRDS("patview-data-rds/nber.category-4-5.rds")




## --------------------------------------------------------------------------------
#' Filter tables of PatentsView.org bulk data
#'
#' @description
#' Similar to dplyr::filter but for tables of PatentsView.org bulk data saved in multiple .rds files
#' @param file.dir A path to directory with .rds files. Default is in working directory "patview-data-rds"
#' @param file.pattern A pattern for getting a file or a set of files (data batches)
#' @param ... A filtering conditions to fetch certain rows. (See dplyr::filter)
#' @return A data.table with a subset the data.
#' @import pbapply magrittr data.table dplyr
#' @export
patview.filter <- function(file.pattern, ...
                         , file.dir = file.path(getwd(), "patview-data-rds")) {
    file.dir %>%
        file.path(list.files(., pattern = file.pattern)) %>%
        pblapply(function(file.path)
            file.path %>%
            readRDS %>% 
            filter(...)) %>%
        rbindlist %>% 
        return
}
## --------------------------------------------------------------------------------



## Tests
## "nber.category.rds" %>% patview.filter(TRUE)
## "nber.category.rds" %>% patview.filter(id == 4)
## "nber.category" %>% patview.filter(str_detect(title, "m"))









