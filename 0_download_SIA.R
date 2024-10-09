# Download and Clean SIA -------------------------------------

# 0. Settings ---------------------------------------

rm(list=ls())
gc()

# Libraries

xfun::pkg_attach(c('tidyverse','purrr', 'glue', 'stringr'), install=T)

# 1. Download ------------------------------------------------------------------
download_sia <- function(
    folder,
    state,
    time
) {
  
  url = 'ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados'
  
  download.file(
    url = glue('{url}/PA{state}{time}.dbc'),
    destfile = glue('{folder}/PA{state}{time}.dbc'),
    method = 'curl'
  )
  
}

states_years <- expand_grid(
  year = 21:23,
  month = stringr::str_pad(string = 1:12, width = 2, side = 'left', pad = '0'),
  state = c('RJ'),
  var = c('a','b')
) %>% 
  dplyr::mutate(time = stringr::str_c(year,month,var))

states_years %>% 
  purrr::map2(.x = .$state, .y = .$time, 
              .f = ~download_sia(folder = 'projeto_tabaco/dados/brutos/SIA',
                                 state = .x, time = .y))
