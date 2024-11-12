# Clean SIA-------------------------------------

# 0. Settings ---------------------------------------

rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse','purrr', 'glue', 'renv'), install=T)

# 1. Function -----------------------------------------------------------

clean_sia <- function(state = NULL,time = NULL, file = NULL, folder) {
  
  temp <-  tempfile()
  
  url <-  'ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados'
  
  file <-  ifelse(is.null(file) == T, glue('PA{state}{time}.dbc'), file)
  
  download.file(url = glue('{url}/', file),
                method = 'curl',
                temp)
  
  temp_clean <- read.dbc::read.dbc(temp) %>% 
    tibble::as_tibble() %>% 
    dplyr::select(-PA_INCOUT, -PA_INCURG) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(pa_cidpri_t = pa_cidpri %>% 
                    stringr::str_remove_all('\\.|\\,| |\\;|\\-|\\_'),
                  pa_cidsec_t = pa_cidsec %>% 
                    stringr::str_remove_all('\\.|\\,| |\\;|\\-|\\_'),
                  pa_cidcas_t = pa_cidcas %>% 
                    stringr::str_remove_all('\\.|\\,| |\\;|\\-|\\_'),) %>% 
    dplyr::filter(stringr::str_detect(pattern = cid, pa_cidpri_t) |
                    stringr::str_detect(pattern = cid, pa_cidsec_t) |
                    stringr::str_detect(pattern = cid, pa_cidcas_t))
  
  haven::write_dta(data = temp_clean, path = glue('{folder}/',str_remove(file, '.dbc'),'.dta'))
}

# 2. Temporary download, clean and salve dta ----------------------------

# Regex list for IDC 

cid <- '^C67|^C53|^D06|^C19|^C20|^C21|^C15|^C64|^C32|^C92|^C22|^C34|^C14|^C25|^C16|I713|I714|^I70|I679|^I25|^A15|^A16|^J44|^J12|^J13|^J14|^J15|^J16|^J17|^J18|^E10|^E11|^E12|^E13|^E14|H251|H353|^S70|^S71|^S72|^S73|^S74|^S75|^S76|^S77|^S78|^S79|K053|K052|^M85|^K25|^K26|^K27|^M05|^M06|^M08'

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
              .f = ~clean_sia(file = .x, folder = folder))
