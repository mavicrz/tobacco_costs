# Download and Clean SIA -------------------------------------

# 0. Settings ---------------------------------------

rm(list=ls())
gc()

# Libraries

xfun::pkg_attach(c('tidyverse','purrr', 'glue', 'stringr', 'RCurl'), install=T)

# 1. Fucntion  -----------------------------------------------------------------

download_sia <- function(
    folder,
    state = NULL,
    time = NULL,
    file = NULL
) {
  
  url = 'ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados'
  
  file = ifelse(is.null(file) == T, glue('PA{state}{time}.dbc'), file)
  
  download.file(
    url = glue('{url}/', file),
    destfile = glue('{folder}/',file),
    method = 'curl'
  )
  
}

# 2. Donwload ------------------------------------------------------------------

# Write the path you want to save the files

folder <- ''

# Here you will know all files for Outpatient Production in SUS data

ftp_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/Dados"

files <- RCurl::getURL(ftp_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)

file_list <- tibble::tibble(file = stringr::str_split(files, '\\r\\n') %>% unlist()) %>% 
  dplyr::filter(stringr::str_starts(pattern = 'PA', file)) %>% 
  dplyr::mutate(state = stringr::str_sub(file, 3, 4),
                month = stringr::str_sub(file, 7, 8) %>%  as.numeric(),
                year = stringr::str_sub(file, 5, 6) %>% stringr::str_c(20,.) %>% as.numeric()) %>% 
  dplyr::filter(year %in% 2014:2023)

file_list %>% 
  purrr::map(.x = .$file, 
             .f = ~download_sia(folder = folder,
                                file = .x))
