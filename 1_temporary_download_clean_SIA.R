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
  
  if(is.null(file) == T){file = glue('PA{state}{time}.dbc')}
  else(file = file)

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

# 1. Temporary download, clean and salve dta ----------------------------

# Regex list for IDC 

cid <- '^C67|^C53|^D06|^C19|^C20|^C21|^C15|^C64|^C32|^C92|^C22|^C34|^C14|^C25|^C16|I713|I714|^I70|I679|^I25|^A15|^A16|^J44|^J12|^J13|^J14|^J15|^J16|^J17|^J18|^E10|^E11|^E12|^E13|^E14|H251|H353|^S70|^S71|^S72|^S73|^S74|^S75|^S76|^S77|^S78|^S79|K053|K052|^M85|^K25|^K26|^K27|^M05|^M06|^M08'

# List states from Brazil, years and months (some combinations of month and year
# have more than one file (types a and b) because the population is too large, such as São Paulo - SP -  
# and Minas Gerais - MG - you will have to look for each exception in the FTP download site)

states_years <- expand_grid(
  year = 21:23,
  month = stringr::str_pad(string = 1:12, width = 2, side = 'left', pad = '0'),
  state = 'MG',  
  type = c('a','b')) %>% 
  dplyr::mutate(time = stringr::str_c(year,month,type)) %>% 
  dplyr::arrange(state)



states_years %>% 
  purrr::map2(.x = .$state,
              .y = .$time,
              .f = ~clean_sia(state = .x, time = .y, folder = 'projeto_tabaco/dados/limpos/SIA'))
