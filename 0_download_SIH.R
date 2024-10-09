# Download and Clean SIH -------------------------------------
# In this code we download and clean internation data on 
# Brazilian health system


# Nota para Fabiana: neste código basta colocar algum caminho de pasta no campo "folder" na linha 48 do código
# Você deve escrever como 'C:/Users/Este é um exemplo de pasta'
# Com isso feito, é só rodar o código completo

# 0. Settings ---------------------------------------

rm(list=ls())
gc()

# Libraries
install.packages('xfun')

xfun::pkg_attach(c('tidyverse','purrr', 'glue', 'stringr'), install=T)

# 1. Download ------------------------------------------------------------------
download_sih <- function(
    folder,
    state,
    time
) {
  
  url = 'ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados'
  
  download.file(
    url = glue('{url}/RD{state}{time}.dbc'),
    destfile = glue('{folder}/RD{state}{time}.dbc'),
    method = 'curl'
  )
  
}

states_years <- dplyr::expand_grid(
  year = 14:22,
  month = stringr::str_pad(string = 1:12, width = 2, side = 'left', pad = '0'),
  state = c('AC','AL','AP','AM','BA','CE','DF','ES','GO','MA','MT','MS','MG','PA','PB','PR','PE','PI','RJ','RN','RO','RR','SC','SP','SE','TO')
) %>% 
  dplyr::mutate(time = stringr::str_c(year,month))

states_years %>% 
  tibble::rownames_to_column() %>% 
  dplyr::filter(rowname > 382) %>% 
  purrr::map2(.x = .$state, .y = .$time, 
              .f = ~download_sih(folder = 'projeto_tabaco/dados/brutos/SIH',
                                 state = .x, time = .y))
