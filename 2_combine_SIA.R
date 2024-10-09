# SIA-------------------------------------

# 0. Settings ---------------------------------------

rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse','glue', 'beepr', 'future', 'furrr'), install=T)

future::plan(strategy = future::multisession, workers = 4)

sia_cost <- function(state){
  
  files <- list.files(path = glue::glue('projeto_tabaco/dados/limpos/SIA/{state}'), full.names = T)
  
  combined <- furrr::future_map(.x = files, 
                                .f = ~haven::read_dta(file = .x) %>%
                                  dplyr::select('pa_ufmun','pa_cmp','pa_cidpri','pa_cidsec',
                                                'pa_cidcas','pa_cidpri_t','pa_cidsec_t',
                                                'pa_cidcas_t','pa_idade','pa_sexo',
                                                'pa_racacor','pa_valapr', 'pa_docorig') %>% 
                                  dplyr::mutate(across(.cols=everything(), ~haven::as_factor(.) %>% as.character(.)),
                                                across(.cols = c('pa_idade', 'pa_valapr'), ~as.numeric(.))) %>% 
                                  dplyr::group_by(pa_ufmun,pa_cmp,pa_cidpri,pa_cidsec,pa_cidcas,
                                                  pa_cidpri_t,pa_cidsec_t,pa_cidcas_t,pa_idade,
                                                  pa_sexo,pa_racacor, pa_docorig) %>% 
                                  dplyr::summarise(pa_valapr = sum(pa_valapr,na.rm = T), .groups = 'drop') %>% 
                                  dplyr::filter(pa_valapr>0), .progress = T) %>% 
    purrr::reduce(bind_rows)
  
  haven::write_dta(combined, glue::glue('projeto_tabaco/dados/limpos/custos ambulatoriais/PA{state}.dta'))
  
  beepr::beep()
  
}
