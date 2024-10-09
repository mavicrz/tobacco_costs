# SIH---------------------------------------------------------------------------

# 0. Settings ------------------------------------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr','glue', 'beepr'), install=T)



sih_cost <- function(state){
  
  files <- list.files(path = 'projeto_tabaco/dados/limpos/SIH', pattern = glue::glue('RD{state}'), full.names = T)
  
  combined <- purrr::map(.x = files, 
                         .f = ~haven::read_dta(file = .x) %>%
                           dplyr::select(uf_zi, ano_cmpt, mes_cmpt,sexo, idade, val_tot, val_sh_fed, val_sp_fed, diag_princ, diag_secun, diag_princ_t, diag_secun_t) %>% 
                           dplyr::mutate(across(.cols = c('idade', 'val_sh_fed', 'val_sp_fed', 'val_tot'), ~as.numeric(.))) %>% 
                           dplyr::group_by(uf_zi, ano_cmpt, mes_cmpt, sexo, idade, diag_princ, diag_secun, diag_princ_t, diag_secun_t) %>% 
                           dplyr::summarize(across(.cols = c('val_sh_fed', 'val_sp_fed', 'val_tot'), ~sum(.,na.rm = T)), .groups = 'drop') %>% 
                           dplyr::filter(val_tot>0), .progress = T) %>% 
    purrr::reduce(bind_rows)
  
  haven::write_dta(combined, glue::glue('projeto_tabaco/dados/limpos/custos internação/RD{state}.dta'))
  
  #beepr::beep()
}
