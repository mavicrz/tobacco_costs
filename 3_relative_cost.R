# Relative Costs -------------------------------------


# 0. Settings ---------------------------------------

rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse','purrr', 'haven'), install=T)


# 1. Relative Risks e PAR ----------------------------------------------------
rr_sg <- function(cid, sexo, idade){
  case_when(
    # Câncer de bexiga - Homens
    str_detect(pattern = '^C67', cid) == T & sexo == 'M' ~ 3.27,
    # Câncer de bexiga - Mulheres
    str_detect(pattern = '^C67', cid) == T & sexo == 'F' ~ 2.22,
    # Câncer de colo do útero - Mulheres
    str_detect(pattern = '^C53', cid) == T & sexo == 'F' ~ 1.59,
    # Câncer colorretal - Homens
    str_detect(pattern = '^C19|^C20|^C21', cid) == T & sexo == 'M' ~ 1.32,
    # Câncer colorretal - Mulheres
    str_detect(pattern = '^C19|^C20|^C21', cid) == T & sexo == 'F' ~ 1.17,
    # Câncer de esôfago - Homens
    str_detect(pattern = '^C15', cid) == T & sexo == 'M' ~ 6.76,
    # Câncer de esôfago - Mulheres
    str_detect(pattern = '^C15', cid) == T & sexo == 'F' ~ 7.75,
    # Câncer de rim - Homens
    str_detect(pattern = '^C64', cid) == T & sexo == 'M' ~ 2.72,
    # Câncer de rim - Mulheres
    str_detect(pattern = '^C64', cid) == T & sexo == 'F' ~ 1.29,
    # Câncer de laringe - Homens
    str_detect(pattern = '^C32', cid) == T & sexo == 'M' ~ 14.6,
    # Câncer de laringe - Mulheres
    str_detect(pattern = '^C32', cid) == T & sexo == 'F' ~ 13.02,
    # Leucemia mielóide aguda - Homens
    str_detect(pattern = '^C92', cid) == T & sexo == 'M' ~ 1.86,
    # Leucemia mielóide aguda- Mulheres
    str_detect(pattern = '^C92', cid) == T & sexo == 'F' ~ 1.13,
    # Câncer de fígado (carcinomahepatocelular) - Homens
    str_detect(pattern = '^C22', cid) == T & sexo == 'M' ~ 5.04,
    # Câncer de fígado (carcinomahepatocelular) - Mulheres
    str_detect(pattern = '^C22', cid) == T & sexo == 'F' ~ 1.7,
    # Câncer de pulmão - Homens - 35 a 54
    str_detect(pattern = '^C34', cid) == T & sexo == 'M' & idade %in% 35:54 ~ 14.33,
    # Câncer de pulmão - Homens - 55 a 64
    str_detect(pattern = '^C34', cid) == T & sexo == 'M' & idade %in% 55:64 ~ 19.03,
    # Câncer de pulmão - Homens - 65 a 74
    str_detect(pattern = '^C34', cid) == T & sexo == 'M' & idade %in% 65:74 ~ 28.29,
    # Câncer de pulmão - Homens - 75 +
    str_detect(pattern = '^C34', cid) == T & sexo == 'M' & idade > 74 ~ 22.51,
    # Câncer de pulmão - Mulheres - 35 a 54
    str_detect(pattern = '^C34', cid) == T & sexo == 'F' & idade %in% 35:54 ~ 13.3,
    # Câncer de pulmão - Mulheres - 55 a 64
    str_detect(pattern = '^C34', cid) == T & sexo == 'F' & idade %in% 55:64 ~ 18.95,
    # Câncer de pulmão - Mulheres - 65 a 74
    str_detect(pattern = '^C34', cid) == T & sexo == 'F' & idade %in% 65:74 ~ 23.65,
    # Câncer de pulmão - Mulheres - 75 +
    str_detect(pattern = '^C34', cid) == T & sexo == 'F' & idade > 74 ~ 23.08,
    # Câncer de cavidade oralefaringe - Homens
    str_detect(pattern = '^C14', cid) == T & sexo == 'M' ~ 10.89,
    # Câncer de cavidade oralefaringe - Mulheres
    str_detect(pattern = '^C14', cid) == T & sexo == 'F' ~ 5.08,
    # Câncer de pâncreas - Homens
    str_detect(pattern = '^C25', cid) == T & sexo == 'M' ~ 2.31,
    # Câncer de pâncreas - Mulheres
    str_detect(pattern = '^C25', cid) == T & sexo == 'F' ~ 2.25,
    # Câncer de estômago - Homens
    str_detect(pattern = '^C16', cid) == T & sexo == 'M' ~ 1.96,
    # Câncer de estômago - Mulheres
    str_detect(pattern = '^C16', cid) == T & sexo == 'F' ~ 1.36,
    # Aneurismada Aorta Abdominal - Homens
    str_detect(pattern = 'I713|I714', cid) == T & sexo == 'M' ~ 6.21,
    # Aneurismada Aorta Abdominal - Mulheres
    str_detect(pattern = 'I713|I714', cid) == T & sexo == 'F' ~ 7.07,
    # Aterosclerose/Doença Vascular Periférica - Homens
    str_detect(pattern = '^I70', cid) == T & sexo == 'M' ~ 2.44,
    # Aterosclerose/Doença Vascular Periférica - Mulheres
    str_detect(pattern = '^I70', cid) == T & sexo == 'F' ~ 1.83,
    # Doença Cerebrovascular - Homens - 35 a 54
    str_detect(pattern = 'I679', cid) == T & sexo == 'M' & idade %in% 35:54 ~ 3.27,
    # Doença Cerebrovascular - Homens - 55 a 64
    str_detect(pattern = 'I679', cid) == T & sexo == 'M' & idade %in% 55:64 ~ 3.27,
    # Doença Cerebrovascular - Homens - 65 a 74
    str_detect(pattern = 'I679', cid) == T & sexo == 'M' & idade %in% 65:74 ~ 2.17,
    # Doença Cerebrovascular - Homens - 75 +
    str_detect(pattern = 'I679', cid) == T & sexo == 'M' & idade > 74 ~ 1.48,
    # Doença Cerebrovascular - Mulheres - 35 a 54
    str_detect(pattern = 'I679', cid) == T & sexo == 'F' & idade %in% 35:54 ~ 4,
    # Doença Cerebrovascular - Mulheres - 55 a 64
    str_detect(pattern = 'I679', cid) == T & sexo == 'F' & idade %in% 55:64 ~ 4,
    # Doença Cerebrovascular - Mulheres - 65 a 74
    str_detect(pattern = 'I679', cid) == T & sexo == 'F' & idade %in% 65:74 ~ 2.27,
    # Doença Cerebrovascular - Mulheres - 75 +
    str_detect(pattern = 'I679', cid) == T & sexo == 'F' & idade > 74 ~ 1.7,
    # Doença Coronariana - Homens - 0 a 34
    #str_detect(pattern = '^I25', cid) == T & sexo == 'M' & idade <35 ~ ,
    # Doença Coronariana - Homens - 35 a 54
    str_detect(pattern = '^I25', cid) == T & sexo == 'M' & idade %in% 35:54 ~ 3.88,
    # Doença Coronariana - Homens - 55 a 64
    str_detect(pattern = '^I25', cid) == T & sexo == 'M' & idade %in% 55:64 ~ 2.99,
    # Doença Coronariana - Homens - 65 a 74
    str_detect(pattern = '^I25', cid) == T & sexo == 'M' & idade %in% 65:74 ~ 2.76,
    # Doença Coronariana - Homens - 75 +
    str_detect(pattern = '^I25', cid) == T & sexo == 'M' & idade > 74 ~ 1.98,
    # Doença Coronariana - Mulheres - 0 a 34
    #str_detect(pattern = '^I25', cid) == T & sexo == 'F' & idade <35 ~ ,
    # Doença Coronariana - Mulheres - 35 a 54
    str_detect(pattern = '^I25', cid) == T & sexo == 'F' & idade %in% 35:54 ~ 4.98,
    # Doença Coronariana - Mulheres - 55 a 64
    str_detect(pattern = '^I25', cid) == T & sexo == 'F' & idade %in% 55:64 ~ 3.25,
    # Doença Coronariana - Mulheres - 65 a 74
    str_detect(pattern = '^I25', cid) == T & sexo == 'F' & idade %in% 65:74 ~ 3.29,
    # Doença Coronariana - Mulheres - 75 +
    str_detect(pattern = '^I25', cid) == T & sexo == 'F' & idade > 74 ~ 2.25,
    # Tuberculose - Homens - 0 a 34
    #str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'M' & idade <35 ~ ,
    # Tuberculose - Homens - 35 a 54
    str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'M' & idade %in% 35:54 ~ 4.47,
    # Tuberculose - Homens - 55 a 64
    str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'M' & idade %in% 55:64 ~ 15.17,
    # Tuberculose - Homens - 65 a 74
    str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'M' & idade %in% 65:74 ~ 2.58,
    # Tuberculose - Homens - 75 +
    str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'M' & idade > 74 ~ 1.62,
    # Tuberculose - Mulheres - 0 a 34
    #str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'F' & idade <35 ~ ,
    # Tuberculose - Mulheres - 35 a 54
    str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'F' & idade %in% 35:54 ~ 6.43,
    # Tuberculose - Mulheres - 55 a 64
    str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'F' & idade %in% 55:64 ~ 9,
    # Tuberculose - Mulheres - 65 a 74
    str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'F' & idade %in% 65:74 ~ 1.75,
    # Tuberculose - Mulheres - 75 +
    str_detect(pattern = '^A15|^A16', cid) == T & sexo == 'F' & idade > 74 ~ 2.06,
    # Pneumonia - Homens - 0 a 34
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'M' & idade <35 ~ 1.75,
    # Pneumonia - Homens - 35 a 54
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'M' & idade %in% 35:54 ~ 4.47,
    # Pneumonia - Homens - 55 a 64
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'M' & idade %in% 55:64 ~ 15.17,
    # Pneumonia - Homens - 65 a 74
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'M' & idade %in% 65:74 ~ 2.58,
    # Pneumonia - Homens - 75 +
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'M' & idade > 74 ~ 1.62,
    # Pneumonia - Mulheres - 0 a 34
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'F' & idade <35 ~ 2.17,
    # Pneumonia - Mulheres - 35 a 54
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'F' & idade %in% 35:54 ~ 6.43,
    # Pneumonia - Mulheres - 55 a 64
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'F' & idade %in% 55:64 ~ 9,
    # Pneumonia - Mulheres - 65 a 74
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'F' & idade %in% 65:74 ~ 1.75,
    # Pneumonia - Mulheres - 75 +
    str_detect(pattern = '^J12|^J13|^J14|^J15|^J16|^J17|^J18', cid) == T & sexo == 'F' & idade > 74 ~ 2.06,
    # Diabetes - Homens - 0 a 34
    #str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'M' & idade <35 ~ ,
    # Diabetes - Homens - 35 a 54
    #str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'M' & idade %in% 35:54 ~ ,
    # Diabetes - Homens - 55 a 64
    #str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'M' & idade %in% 55:64 ~ ,
    # Diabetes - Homens - 65 a 74
    str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'M'  ~ 1.5,
    # Diabetes - Homens - 75 +
    #str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'M' & idade > 74 ~ 1,
    # Diabetes - Mulheres - 0 a 34
    #str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'F' & idade <35 ~ ,
    # Diabetes - Mulheres - 35 a 54
    #str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'F' & idade %in% 35:54 ~ ,
    # Diabetes - Mulheres - 55 a 64
    #str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'F' & idade %in% 55:64 ~ ,
    # Diabetes - Mulheres - 65 a 74
    str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'F' ~ 1.54,
    # Diabetes - Mulheres - 75 +
    #str_detect(pattern = '^E10|^E11|^E12|^E13|^E14', cid) == T & sexo == 'F' & idade > 74 ~ 1.1,
    # Doença Pulmonar Obstrutiva Crônica - Homens
    str_detect(pattern = '^J44', cid) == T & sexo == 'M' ~ 10.58,
    # Doença Pulmonar Obstrutiva Crônica - Mulheres
    str_detect(pattern = '^J44', cid) == T & sexo == 'F' ~ 13.08,
    # Catarata Nuclear - Homens
    str_detect(pattern = 'H251', cid) == T & sexo == 'M' ~ 2.24,
    # Catarata Nuclear - Mulheres
    str_detect(pattern = 'H251', cid) == T & sexo == 'F' ~ 2.24,
    # Degeneração Macular Neovascular relacionada à idade e atrófica - Homens
    str_detect(pattern = 'H353', cid) == T & sexo == 'M' ~ 3.21,
    # Degeneração Macular Neovascular relacionada à idade e atrófica - Mulheres
    str_detect(pattern = 'H353', cid) == T & sexo == 'F' ~ 2.2,
    # Fratura de quadril - Homens
    str_detect(pattern = '^S70|^S71|^S72|^S73|^S74|^S75|^S76|^S77|^S78|^S79', cid) == T & sexo == 'M' ~ 2.2,
    # Fratura de quadril - Mulheres
    str_detect(pattern = '^S70|^S71|^S72|^S73|^S74|^S75|^S76|^S77|^S78|^S79', cid) == T & sexo == 'F' ~ 1.8,
    # Periodontite - Homens
    str_detect(pattern = 'K053', cid) == T & sexo == 'M' ~ 12.87,
    # Periodontite - Mulheres
    str_detect(pattern = 'K053', cid) == T & sexo == 'F' ~ 12.87,
    # Periodontite - Homens
    str_detect(pattern = 'K052', cid) == T & sexo == 'M' ~ 5.03,
    # Periodontite - Mulheres
    str_detect(pattern = 'K053', cid) == T & sexo == 'F' ~ 5.03,
    # Baixa Densidade Óssea após menopausa - Mulheres
    str_detect(pattern = '^M85', cid) == T & sexo == 'F' ~ 1.5,
    # Úlcera Péptica (emportadoresdeHelicobacterpylori)
    str_detect(pattern = '^K25|^K26|^K27', cid) == T & sexo == 'F' ~ 1.37,
    # Artrite Reumatoide
    str_detect(pattern = '^M06', cid) == T ~ 1.83,
    T ~ NA
  )
}

par <- function(rr,p) {
  
  p*(rr-1)/(1+p*(rr-1))
  
}

doenca <- readxl::read_xlsx('projeto_tabaco/documentação/Relação doenças e CID.xlsx') %>% 
  dplyr::select(doenca = 1, cid_t) %>% 
  dplyr::mutate(cid = stringr::str_sub(cid_t,1,3))

# 2. IPCA ------------------------------------------------------------------
ipca <- readxl::read_xlsx('projeto_tabaco/dados/ipca.xlsx') %>% 
  dplyr::mutate(mes = rep(stringr::str_pad(string = 1:12, width = 2, side = 'left', pad = '0'), 10),
                ano = as.character(ano))

# 3. Custo relativo SIA --------------------------------------------------------

sia_files <- list.files(path = 'projeto_tabaco/dados/limpos/custos ambulatoriais',
                        pattern = glue::glue('^PA'), full.names = T)

sia_relative_cost <- function(file){
  haven::read_dta(file = file) %>% 
    dplyr::filter(pa_docorig != 'C') %>% 
    dplyr::mutate(ano = stringr::str_sub(pa_cmp, 1,4),
                  mes = stringr::str_sub(pa_cmp, 5,6)) %>% 
    dplyr::left_join(ipca, by = c('ano','mes')) %>% 
    dplyr::mutate(custos_ambulatoriais = pa_valapr*ipca) %>% 
    dplyr::select(cid_pri = pa_cidpri_t, 
                  cid_sec = pa_cidsec_t, 
                  cid_ass = pa_cidcas_t,
                  doc_origem = pa_docorig,
                  sexo = pa_sexo, 
                  idade = pa_idade, 
                  custos_ambulatoriais,
                  ano) %>% 
    dplyr::filter(idade >= 35) %>% 
    dplyr::mutate(
      rr_sg_cid_pri = rr_sg(cid = cid_pri, sexo = sexo, idade = idade),
      rr_sg_cid_sec = case_when(is.na(rr_sg_cid_pri) == T ~ rr_sg(cid_sec, sexo = sexo, idade = idade),
                                T~ NA),
      rr_sg_cid_ass = case_when(is.na(rr_sg_cid_sec) == T ~ rr_sg(cid_ass, sexo = sexo, idade = idade),
                                T~ NA),
      prevalencia = case_when(
        sexo == 'F' & idade %in% 18:34 ~ 0.06933474,
        sexo == 'F' & idade %in% 35:54 ~ 0.10824315,
        sexo == 'F' & idade %in% 55:64 ~ 0.14714933,
        sexo == 'F' & idade %in% 65:74~ 0.08534550,
        sexo == 'F' & idade > 74 ~ 0.04972168,
        sexo == 'M' & idade %in% 18:34 ~ 0.15818465,
        sexo == 'M' & idade %in% 35:54 ~ 0.15969495,
        sexo == 'M' & idade %in% 55:64 ~ 0.19306170,
        sexo == 'M' & idade %in% 65:74~ 0.13836659,
        sexo == 'M' & idade > 74 ~ 0.09711351),
      par_cid_pri = par(p = prevalencia, rr = rr_sg_cid_pri),
      par_cid_sec = par(p = prevalencia, rr = rr_sg_cid_sec),
      custos_ambulatoriais_par_cid_pri = par_cid_pri*custos_ambulatoriais,
      custos_ambulatoriais_par_cid_sec = par_cid_sec*custos_ambulatoriais)
}

sia <- purrr::map(.x = sia_files, .f = ~sia_relative_cost(.x), .progress = T) %>% 
  purrr::reduce(bind_rows)

sia_cid_pri <- sia %>%   
  dplyr::filter(is.na(rr_sg_cid_pri) == F) %>% 
  dplyr::group_by(cid_pri,ano) %>% 
  dplyr::summarise(across(.cols = c(custos_ambulatoriais_par_cid_pri, custos_ambulatoriais),
                          ~sum(., na.rm = T))) %>% 
  dplyr::rename(cid = cid_pri, custos_ambulatoriais_pri = custos_ambulatoriais) %>% 
  dplyr::filter(custos_ambulatoriais_par_cid_pri>0)

sia_cid_sec <- sia %>%   
  dplyr::filter(is.na(rr_sg_cid_sec) == F) %>% 
  dplyr::group_by(cid_sec,ano) %>% 
  dplyr::summarise(across(.cols = c(custos_ambulatoriais_par_cid_sec, custos_ambulatoriais),
                          ~sum(., na.rm = T))) %>% 
  dplyr::rename(cid = cid_sec, custos_ambulatoriais_sec = custos_ambulatoriais) %>% 
  dplyr::filter(custos_ambulatoriais_par_cid_sec>0)

# 4. Custo relativo SIH --------------------------------------------------------

sih_files <- list.files(path = 'projeto_tabaco/dados/limpos/custos internação',
                        pattern = glue::glue('^RD'), full.names = T)

sih_relative_cost <- function(file){
  haven::read_dta(file = file) %>% 
    dplyr::rename(ano = ano_cmpt,
                  mes = mes_cmpt) %>% 
    dplyr::left_join(ipca, by = c('ano','mes')) %>% 
    dplyr::mutate(custos_internacao = val_tot*ipca) %>% 
    dplyr::select(cid_pri = diag_princ_t, 
                  cid_sec = diag_secun_t,
                  cid_sec_1 = diagsec1_t,
                  sexo,
                  idade, 
                  custos_internacao,
                  ano) %>% 
    dplyr::filter(idade >= 35) %>% 
    dplyr::mutate(
      sexo = case_when(sexo == 1 ~ 'M', sexo == 3 ~ 'F'),
      rr_sg_cid_pri = rr_sg(cid = cid_pri, sexo = sexo, idade = idade),
      rr_sg_cid_sec = case_when(is.na(rr_sg_cid_pri) == T & ano == 2014 ~ rr_sg(cid_sec, sexo = sexo, idade = idade),
                                is.na(rr_sg_cid_pri) == T & ano > 2014 ~ rr_sg(cid_sec_1, sexo = sexo, idade = idade),
                                T~ NA),
      prevalencia = case_when(
        sexo == 'F' & idade %in% 18:34 ~ 0.06933474,
        sexo == 'F' & idade %in% 35:54 ~ 0.10824315,
        sexo == 'F' & idade %in% 55:64 ~ 0.14714933,
        sexo == 'F' & idade %in% 65:74~ 0.08534550,
        sexo == 'F' & idade > 74 ~ 0.04972168,
        sexo == 'M' & idade %in% 18:34 ~ 0.15818465,
        sexo == 'M' & idade %in% 35:54 ~ 0.15969495,
        sexo == 'M' & idade %in% 55:64 ~ 0.19306170,
        sexo == 'M' & idade %in% 65:74~ 0.13836659,
        sexo == 'M' & idade > 74 ~ 0.09711351),
      par_cid_pri = par(p = prevalencia, rr = rr_sg_cid_pri),
      par_cid_sec = par(p = prevalencia, rr = rr_sg_cid_sec),
      custos_internacao_par_cid_pri = par_cid_pri*custos_internacao,
      custos_internacao_par_cid_sec = par_cid_sec*custos_internacao)
}

sih <- purrr::map(.x = sih_files, .f = ~sih_relative_cost(.x), .progress = T) %>% 
  purrr::reduce(bind_rows)

sih_cid_pri <- sih %>%  
  dplyr::filter(is.na(rr_sg_cid_pri) == F) %>% 
  dplyr::group_by(cid_pri,ano) %>% 
  dplyr::summarise(across(.cols = c(custos_internacao_par_cid_pri, custos_internacao),
                          ~sum(., na.rm = T))) %>% 
  dplyr::rename(cid = cid_pri, custos_internacao_pri = custos_internacao) %>% 
  dplyr::filter(custos_internacao_par_cid_pri > 0)

sih_cid_sec <- sih %>%   
  dplyr::filter(is.na(rr_sg_cid_sec) == F) %>% 
  dplyr::mutate(cid_sec = case_when(ano > 2014 ~ cid_sec_1, T~ cid_sec)) %>% 
  dplyr::group_by(cid_sec,ano) %>% 
  dplyr::summarise(across(.cols = c(custos_internacao_par_cid_sec, custos_internacao),
                          ~sum(., na.rm = T))) %>% 
  dplyr::rename(cid = cid_sec, custos_internacao_sec = custos_internacao) %>% 
  dplyr::filter(custos_internacao_par_cid_sec>0)


# 5. Resultado Final -----------------------------------------------------------

tabela_cid <- sih_cid_pri %>% 
  dplyr::full_join(sih_cid_sec, by = c('cid','ano')) %>% 
  dplyr::full_join(sia_cid_pri, by = c('cid','ano')) %>% 
  dplyr::full_join(sia_cid_sec, by = c('cid','ano')) %>% 
  dplyr::group_by(cid, ano) %>% 
  dplyr::summarise(across(.cols = starts_with('custo'),
                          ~sum(., na.rm = T)))

writexl::write_xlsx(tabela_cid, 'projeto_tabaco/dados/custos_por_cid_ano_sem_bpac.xlsx')

tabela_resultados <- sih_cid_pri %>% 
  dplyr::full_join(sih_cid_sec, by = c('cid','ano')) %>% 
  dplyr::full_join(sia_cid_pri, by = c('cid','ano')) %>% 
  dplyr::full_join(sia_cid_sec, by = c('cid','ano')) %>% 
  dplyr::mutate(cid = stringr::str_sub(cid,1,3)) %>% 
  dplyr::inner_join(doenca, by = 'cid', multiple='all') %>% 
  dplyr::group_by(doenca, ano) %>% 
  dplyr::summarise(across(.cols = starts_with('custo'),
                          ~sum(., na.rm = T)), .groups = 'drop')

writexl::write_xlsx(tabela_resultados, 'projeto_tabaco/dados/custos_por_doenca_ano_sem_bpac.xlsx')

tabela_total <- sih_cid_pri %>% 
  dplyr::full_join(sih_cid_sec, by = c('cid','ano')) %>% 
  dplyr::full_join(sia_cid_pri, by = c('cid','ano')) %>% 
  dplyr::full_join(sia_cid_sec, by = c('cid','ano')) %>% 
  dplyr::mutate(cid = stringr::str_sub(cid,1,3)) %>% 
  dplyr::inner_join(doenca, by = 'cid', multiple='all') %>% 
  dplyr::group_by(doenca) %>% 
  dplyr::summarise(across(.cols = starts_with('custo'),
                          ~sum(., na.rm = T)), .groups = 'drop')

writexl::write_xlsx(tabela_total, 'projeto_tabaco/dados/custos_por_doenca_sem_bpac.xlsx')



tabela_media <- sih_cid_pri %>% 
  dplyr::full_join(sih_cid_sec, by = c('cid','ano')) %>% 
  dplyr::full_join(sia_cid_pri, by = c('cid','ano')) %>% 
  dplyr::full_join(sia_cid_sec, by = c('cid','ano')) %>% 
  dplyr::mutate(cid = stringr::str_sub(cid,1,3)) %>% 
  dplyr::inner_join(doenca, by = 'cid', multiple='all') %>% 
  dplyr::group_by(doenca, ano) %>% 
  dplyr::summarise(across(.cols = starts_with('custo'),
                          ~sum(., na.rm = T))) %>% 
  dplyr::group_by(doenca) %>% 
  dplyr::summarise(across(.cols = starts_with('custo'),
                          ~mean(., na.rm = T))) %>% 
  dplyr::select(doenca,
                custos_internacao_pri,custos_internacao_par_cid_pri,
                custos_ambulatoriais_pri,custos_ambulatoriais_par_cid_pri,
                custos_internacao_sec,custos_internacao_par_cid_sec,
                custos_ambulatoriais_sec,custos_ambulatoriais_par_cid_sec)

writexl::write_xlsx(tabela_media, 'projeto_tabaco/dados/custos_medio_por_doenca_sem_bpac.xlsx')

# 6. Gráficos -------------------------------------------------------------------

total %>%
  dplyr::group_by(ano) %>% 
  dplyr::summarise(across(.cols = starts_with('custo'),
                          ~sum(., na.rm = T))) %>% 
  tidyr::pivot_longer(
    cols = starts_with('custos'),
    names_to = c('fonte', 'tipo', 'diag'),
    names_pattern = 'custos_(.*)_(.*)_cid_(.*)',
    values_to = 'custos'
  ) %>% 
  dplyr::filter(diag == 'pri' & tipo == 'par') %>% 
  mutate(
    fonte = case_when(fonte == 'internacao_total' ~ 'Custo Hospitalar',
                      fonte == 'ambulatoriais_apac' ~ 'Custo Ambulatorial (APAC)',
                      fonte == 'ambulatoriais_bpac' ~  'Custo Ambulatorial (APAC, BPA-I, RAAS)'),
    #tipo = if_else(tipo == 'par', 'Custo do Tabaco (PAR)', 'Custo Total'),
    custos = custos / 1000000
  ) %>% 
  ggplot2::ggplot(mapping = aes(x = ano, y = custos, colour = fonte, group = fonte, label = round(custos,3))) +
  geom_line(size = 1) + geom_label(color = 'black', nudge_y = 12,fill='white')+ geom_point(color='black', size=1)+
  scale_y_continuous(n.breaks = 5)+
  #facet_wrap(facets = ~ fonte, scales = 'free_y', nrow = 2) +
  labs(x = '', y = 'Custos (em milhões de reais)',
       colour = '',
       title = '') +
  theme_bw() +
  theme(legend.position = 'bottom')+
  scale_color_manual(values = c("#E2C26A","#E2866A","#A88485"))

ggsave(filename = 'projeto_tabaco/plot/custos_totais_pri.png',
       plot = last_plot(), device = 'png', width = 28, height = 15, units = 'cm', bg = 'white')

total %>%
  dplyr::group_by(ano) %>% 
  dplyr::summarise(across(.cols = starts_with('custo'),
                          ~sum(., na.rm = T))) %>% 
  tidyr::pivot_longer(
    cols = starts_with('custos'),
    names_to = c('fonte', 'tipo', 'diag'),
    names_pattern = 'custos_(.*)_(.*)_cid_(.*)',
    values_to = 'custos'
  ) %>% 
  dplyr::group_by(fonte,tipo,ano) %>% 
  dplyr::summarise(custos = sum(custos,na.rm=T)) %>% 
  mutate(
    fonte = case_when(fonte == 'internacao_total' ~ 'Custo Hospitalar',
                      fonte == 'ambulatoriais_apac' ~ 'Custo Ambulatorial (APAC)',
                      fonte == 'ambulatoriais_bpac' ~  'Custo Ambulatorial (APAC, BPA-I, RAAS)'),
    tipo = if_else(tipo == 'par', 'Custo do Tabaco (PAR)', 'Custo Total'),
    custos = custos / 1000000000,
  ) %>% 
  ggplot2::ggplot(mapping = aes(x = ano, y = custos, colour = tipo, group = tipo, label = round(custos,2))) +
  geom_line(size = 1) + geom_label(color = 'black', nudge_y = 0.19,fill='white')+ geom_point(color='black', size=2)+
  scale_y_continuous(n.breaks = 10)+
  facet_wrap(facets = ~ fonte, scales = 'free_y', nrow = 3) +
  labs(x = '', y = 'Custos (em bilhões de reais)',
       colour = '',
       title = '') +
  theme_bw() +
  scale_color_manual(values = c("#E2C26A","#E2866A","#A88485"))+
  theme(legend.position = 'bottom')

total %>% 
  dplyr::left_join(doenca_classe, by = 'doenca') %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(classe) %>% 
  dplyr::summarise(ambulatorial_total_apac = sum(custos_ambulatoriais_apac_total_cid_pri + custos_ambulatoriais_apac_total_cid_sec),
                   ambulatorial_par_apac = sum(custos_ambulatoriais_apac_par_cid_pri + custos_ambulatoriais_apac_par_cid_sec),
                   ambulatorial_total_sem_bpac = sum(custos_ambulatoriais_bpac_total_cid_pri + custos_ambulatoriais_bpac_total_cid_sec),
                   ambulatorial_par_sem_bpac = sum(custos_ambulatoriais_bpac_par_cid_pri + custos_ambulatoriais_bpac_par_cid_sec),
                hospitalar_total = sum(custos_internacao_total_total_cid_pri + custos_internacao_total_total_cid_sec),
                hospitalar_par = sum(custos_internacao_total_par_cid_pri + custos_internacao_total_par_cid_sec)) %>% writexl::write_xlsx('projeto_tabaco/dados/custo_doenca_final.xlsx')
