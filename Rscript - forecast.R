##############################################################################
############       SCRIPT - APLICANDO AS FUNÇÕES DE FORECAST      ############ 
##############################################################################

##############################################################################	
# Instalar e carregar packages e funções 
##############################################################################	
#--------

# Limpar tudo	
rm(list=ls())	
cat("\014")	

# Limpar memória	
gc()	

# Install & load packages	
if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, tidyr, data.table, lubridate, funModeling, stargazer, openxlsx, 	
       readxl, imputeTS, plotly, autoTS,  zoo, arrow, Metrics, caret, purrr, hardhat) 	

# Impedir notação cientifica 	
options(scipen=999)	

# carregar funções	
source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/script - functions forcast new.R")

# Carregar listas de modelos caret
source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/lista de modelos caret.R")	

source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/script - functions forcast new.R")

source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/script-funcoes_projecoes_mensal.R")

source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/script - função para obter a importancia das variáveis VIP.R")

source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/script - função para obter as elasticidades.R")

#*****************************************************************************
### Importar dados de exemplo
#*****************************************************************************
read



#*****************************************************************************
### Iniciar pacote H2O	
#*****************************************************************************

# para utilizar pacotes do h2o
library(h2o)
h2o.init()

# Ver todos modelos suportados
modelos <- modelLookup()

# Filtrar lista de modelos
modelos <- modelos %>% filter(forReg == TRUE) %>% distinct(model)



#****************************************************
# Função para extrair a sazonalidade e usar como explicativas
#****************************************************

extract_sazonality <- function(df, colunas, fator_tend = 0, fator_saz = 1, fator_res = 0) {
  for (col in colunas) {
    col_saz <- paste0(col, "_saz")
    
    # Ajustar a coluna
    df[[col_saz]] <- df[[col]]
    df <- ajuste_pos_projecao_NA(df, col, col_saz, fator_tend, fator_saz, fator_res)
    
    # Preencher valores ausentes
    df <- df %>%
      group_by(Mes) %>%
      fill(all_of(col_saz), .direction = "downup") %>%
      ungroup()
  }
  return(df)
}

colunas <- c("ec_res", "ef_res", "ec_ind", "ef_ind", "ec_com", "ef_com", "ec_rur", "ef_rur",
             "ec_ipu", "ef_ipu", "ec_ppu", "ef_ppu", "ec_spu", "ef_spu", "ec_pro", "ef_pro",
             "ec_total", "ef_total", "ei_total")

df_B <- extract_sazonality(df_B, colunas)

#-------- 

##############################################################################	
# ### (Projeção) Classe - Residencial
##############################################################################
#-------

#*****************************************************************************
# Consumidores	
#*****************************************************************************
# Explicativas	
col_res_nc <- c("Trend", "indice_de_precos",  "populacao", "populacao_sqrt", "domicilios", "domicilios_sqrt", "renda_domiciliar", "massa_total", "pib_total", "tarifa_residencial", "D_choque_ecf_res_MA")

# Exemplo Chamada correta da função
nc_res_caret <- train_multi_models_reg_month_caret(df = df_B, 
                                                   lista_modelos = lista_modelos_1,
                                                   explicativas = col_res_nc,
                                                   var_target = "nc_res",
                                                   ultimo_mes_realizado = "2024-03-01",
                                                   data_inicio = NULL,
                                                   n_fora_modelo = 3,
                                                   n_folds = 10, 
                                                   n_meses_erros = 3)
  
# Definir modelos que vão para o ensemble
modelos_select <- c("nc_res_lm", "nc_res_gamSpline")

# Ajustes	
df_B$nc_res_proj <- rowMeans(nc_res_caret$df_projecoes[, c(modelos_select)])	
df_B <- ajuste_pos_projecao(df_B, "nc_res", "nc_res_proj", fator_tend = 1.003, fator_saz= 0, fator_res= 0)

# Obter a importancia das variáveis
nc_res_caret$var_importance_gamSpline <-  obter_var_importancia_vip(nc_res_caret$modelos_treinados, "gamSpline")
nc_res_caret$var_importance_lm <- obter_var_importancia_vip(nc_res_caret$modelos_treinados, "lm")

# verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("nc_res", "nc_res_proj", "nc_res_B_Tend"))
gc()   
  
#*****************************************************************************
# Consumida 	
#*****************************************************************************
# Covariadas	
col_res_ecf <- c("ec_res_saz", "Trend", "D_crises", "D_covid", "indice_de_precos", "dias_uteis",  "domicilios", "renda_domiciliar", "massa_total", "populacao", "pib_total", "Media_temperatura", "Min_temperatura", "Max_temperatura",  "tarifa_residencial", "Media_temperatura_sqrt", "Max_temperatura_sqrt",	"tarifa_residencial_sqrt")	
                 
# Rodar modelos caret
ec_res_caret <- treinar_avaliar_modelos_caret(df_B,     
                                             lista_modelos_1, 
                                              col_res_ecf,    
                                             "ec_res",    
                                             "2024-03-01",   
                                             "2024-01-01",   
                                             24) 

calcular_elasticidade_pdp(ec_res_caret$modelos_treinados, "rf", ".outcome")

# Definir modelos que vão para o ensemble
modelos_select <- c("ec_res_svmPoly_2", "ec_res_gaussprPoly_2")
# Ajustes	          
df_B$ec_res_proj <- rowMeans(ec_res_caret$df_projecoes[, c(modelos_select)])	
df_B <- ajuste_pos_projecao(df_B, "ec_res", "ec_res_proj", fator_tend = 1, fator_saz= 0.9, fator_res=0.9)

# Obter a importancia das variáveis
ec_res_caret$var_importance_svmPoly <-  obter_var_importancia_vip(ec_res_caret$modelos_treinados, "svmPoly_2")
ec_res_caret$var_importance_gaussprPoly <- obter_var_importancia_vip(ec_res_caret$modelos_treinados, "gaussprPoly_2")

# verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ec_res", "ec_res_proj", "ec_res_B_Tend"))
gc() 

#*****************************************************************************
# Faturada 	
#*****************************************************************************
# Rodar modelos caret
ef_res_caret <- treinar_avaliar_modelos_caret(df_B,     
                                     lista_modelos_1, 
                                     col_res_ecf,    
                                     "ef_res",    
                                     "2024-03-01",   
                                     "2024-01-01",   
                                     24) 

# Definir modelos que vão para o ensemble
modelos_select <- c("ef_res_svmPoly_2", "ef_res_gaussprPoly_2")
df_B$ef_res_proj <- rowMeans(ef_res_caret$df_projecoes[, c(modelos_select)])	
df_B <- ajuste_pos_projecao(df_B, "ef_res", "ef_res_proj", fator_tend = 1, fator_saz= 0.8, fator_res=0.9)

# Obter a importancia das variáveis
ef_res_caret$var_importance_svmPoly_2 <- obter_var_importancia_vip(ef_res_caret$modelos_treinados, "svmPoly_2")
ef_res_caret$var_importance_gaussprPoly_2 <- obter_var_importancia_vip(ef_res_caret$modelos_treinados, "gaussprPoly_2")

# verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ef_res", "ef_res_proj", "ec_res_B_Tend"))

# consumido e faturado
compare_multiple_columns_plot(df_B, "Data", c("ef_res", "ef_res_proj", "ec_res", "ec_res_proj"))

#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
# Residencial
df_B$diff_ecf_res <- df_B$ec_res_proj - df_B$ef_res_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_res_proj", "ef_res_proj"))
compare_multiple_columns_plot(df_B, "Data", c("ei_res_gd", "diff_ecf_res"))


#*****************************************************************************
# Injetada GD 	
#*****************************************************************************
# Explicativas	
col_ei_res_gd <- c("indice_de_precos", "indice_de_precos_sqrt", "tarifa_residencial", "Media_temperatura",	 
                   "Media_temperatura_sqrt","Max_temperatura", "Min_temperatura",  "diff_ecf_res")	

# Rodar modelos
ei_res_gd_caret <- treinar_avaliar_modelos_caret(df_B,            
                                                 lista_modelos_1, 
                                                 col_ei_res_gd,  
                                                 "ei_res_gd",     
                                                 "2024-03-01",  
                                                 "2023-10-01",    
                                                 24,
                                                 data_inicio="2018-06-01")

df_B$ei_res_gd_proj <- as.numeric(ei_res_gd_caret$df_test_com_previsoes$ei_res_gd_lm)
df_B <- ajuste_pos_projecao(df_B, "ei_res_gd", "ei_res_gd_proj", fator_tend = 1, fator_saz= 1, fator_res=1)

# Obter a importancia das variáveis
ei_res_gd_caret$var_importance_lm <- obter_var_importancia_vip(ei_res_gd_caret$modelos_treinados, "lm")

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_res_gd", "diff_ecf_res", "ei_res_gd_proj", "ei_res_gd_B_Tend"))

# ajuste
df_B <- ajustar_series_e_projecoes(df_B, "ei_res_gd", "ei_res_gd_proj", fator_tend = 1.2, fator_saz= 1, fator_res=1, option = "linear")

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_res_gd", "diff_ecf_res", "ei_res_gd_proj" , "ei_res_gd_B_Tend"))

# remover objetos e limpar memoria
rm(list=setdiff(ls(), objetos)) %>% gc() 
#--------

##############################################################################	
# ### (Projeção) Classe - Industrial
##############################################################################
#-------

#*****************************************************************************
# Consumidores	
#*****************************************************************************
# ajuste
df_B$D_choque_nc_ind_MA[is.na( df_B$D_choque_nc_ind_MA)] <- 1

# Covariadas	
col_ind_nc <- c("Mes", "D_crises", "D_covid",  "indice_de_precos", "populacao", "domicilios",  "massa_total", 
                "pib_total", "pib_industria", "pib_industria_sqrt", "tarifa_residencial", "tarifa_industrial", "tarifa_rural",
                "tarifa_industrial_sqrt",  "pim_extrativa", "pim_geral", "pim_energia", "renda_domiciliar")	
                	
# Rodar modelos
nc_ind_caret <- treinar_avaliar_modelos_caret(df_B,            
                                    lista_modelos_1, 
                                    col_ind_nc,      
                                    "nc_ind",        
                                    "2024-03-01",    
                                    "2024-03-01",    
                                    24)


# calcular	
df_B$nc_ind_proj <- rowMeans(nc_ind_caret$df_projecoes[, c("nc_ind_rf", "nc_ind_rf", "ei_res_gd_B_Tend")])	
                                          
# Ajustes	
df_B <- ajustar_series_e_projecoes(df_B, "nc_ind", "nc_ind_proj", fator_tend = 1, fator_saz= 0.3, fator_res= 0.5, start_date = "2023-06-01" )

# verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("nc_ind", "nc_ind_proj"))
gc()   

#*****************************************************************************
# Consumida
#*****************************************************************************
## Covariadas
col_ind_ecf <- c("ec_ind_saz", "D_crises", "D_covid", "nc_ind_proj", "indice_de_precos", "indice_de_precos_sqrt", "dias_uteis", "populacao", "massa_total", "pib_total", "renda_domiciliar", "pib_industria", "tarifa_industrial",  "tarifa_comercial",  "pim_geral", "pim_extrativa", "pim_energia", "Media_temperatura", "Min_temperatura", "Max_temperatura")

# Rodar modelos
ec_ind_caret <- treinar_avaliar_modelos_caret(df_B,            
                                               lista_modelos_1, 
                                               col_ind_ecf,      
                                               "ec_ind",        
                                               "2024-03-01",    
                                               "2024-03-01",    
                                               24)           

# Definir modelos que vão para o ensemble
models_select <- c("ec_ind_svmPoly", "ec_ind_leapBackward")
                       
# calcular	
df_B$ec_ind_proj <- rowMeans(ec_ind_caret$df_projecoes[, models_select])	

# Ajustes
df_B <- ajuste_pos_projecao(df_B, "ec_ind", "ec_ind_proj", fator_tend = 1.085, fator_saz = 1, fator_res = 1)

# Comparar
compare_multiple_columns_plot(df_B, "Data", c("ec_ind", "ec_ind_proj"))
gc()

#*****************************************************************************
# Faturada	
#*****************************************************************************
# Rodar modelos
ef_ind_caret <- Predict_caret_autots(df_B,            
                                     lista_modelos_1, 
                                     col_ind_ecf,      
                                     "ef_ind",        
                                     "2024-03-01",    
                                     "2024-03-01",    
                                     24)           

# Definir modelos que vão para o ensemble
models_select <- c("ef_ind_svmPoly", "ef_ind_leapBackward")

# calcular	
df_B$ef_ind_proj <- rowMeans(ef_ind_caret$df_projecoes[, models_select])	

# Ajustes
df_B <- ajuste_pos_projecao(df_B, "ef_ind", "ef_ind_proj", fator_tend = 1.08, fator_saz = 1, fator_res = 1)

# Comparar
compare_multiple_columns_plot(df_B, "Data", c("ef_ind", "ef_ind_proj", "ef_ind_B_Tend"))

# Comparar consumido e faturado
compare_multiple_columns_plot(df_B, "Data", c("ec_ind_proj", "ef_ind_proj", "ef_ind_B_Tend"))


#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
df_B$diff_ecf_ind <- df_B$ec_ind_proj - df_B$ef_ind_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_ind_proj", "ef_ind_proj"))
compare_multiple_columns_plot(df_B, "Data", c("ei_ind_gd", "diff_ecf_ind"))

# Ajuste
df_B <- ajuste_pos_projecao(df_B, "ei_ind_gd", "diff_ecf_ind", fator_tend = 1, fator_saz= 0, fator_res=0)


#*****************************************************************************
# Injetada GD Industrial	
#*****************************************************************************
# Explicativas	
col_ei_ind_gd <- c("indice_de_precos", "indice_de_precos_sqrt", "tarifa_industrial", "Media_temperatura",	 
                   "Media_temperatura_sqrt","Max_temperatura", "Min_temperatura",  "Media_pluviometria",  "diff_ecf_ind")	
                   
# Rodar modelos
ei_ind_gd_caret <- treinar_avaliar_modelos_caret(df_B,            
                                                 lista_modelos_1, 
                                                 col_ei_ind_gd,  
                                                 "ei_ind_gd",     
                                                 "2024-03-01",  
                                                 "2023-10-01",    
                                                 24,
                                                 data_inicio="2018-06-01")

df_B$ei_ind_gd_proj <- as.numeric(ei_ind_gd_caret$df_test_com_previsoes$ei_ind_gd_bayesglm)
df_B <- ajuste_pos_projecao(df_B, "ei_ind_gd", "ei_ind_gd_proj", fator_tend = 0.9, fator_saz= 1, fator_res=1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_ind_gd", "diff_ecf_ind",  "ei_ind_gd_proj"))

# Ajustar faturada
df_B$ef_ind_proj_2 <- df_B$ef_ind_proj - df_B$ei_ind_gd_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_ind", "ef_ind", "ec_ind_proj", "ef_ind_proj", "ef_ind_proj_2"))


#-------

##############################################################################	
# ### (Projeção) Classe - Comercial
##############################################################################
#-------

#*****************************************************************************
# Consumidores
#*****************************************************************************
# ajustes
df_B$D_choque_nc_com_MA[is.na(df_B$D_choque_nc_com_MA)] <-1 

## Covariadas
col_com_nc <- c("indice_de_precos", "dias_uteis", "domicilios", "massa_total", "pib_total", "pmc", "D_choque_nc_com_MA",
                "populacao", "D_crises", "tarifa_comercial", "Trend")

# Rodar modelos
nc_com_caret <- treinar_avaliar_modelos_caret(df_B,        
                                     lista_modelos_1,
                                     col_com_nc,   
                                     "nc_com",   
                                     "2024-03-01",  
                                     "2024-01-01",  
                                     24)

# Definir modelos que vão para o ensemble
models_select <- c("nc_com_Rborist", "nc_com_rf")
# calcular	
df_B$nc_com_proj <- rowMeans(nc_com_caret$df_projecoes[, c("nc_com_Rborist", "nc_com_rf")])	

df_B <- ajuste_pos_projecao(df_B, "nc_com", "nc_com_proj", fator_tend = 1.005, fator_saz= 1, fator_res= 1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("nc_com", "nc_com_proj"))

# Limpeza de memória
gc()
  
#*****************************************************************************
# Consumida
#*****************************************************************************
## Covariadas
col_com_ecf <- c("ec_com_saz", "indice_de_precos", "dias_uteis", "domicilios", "massa_total", "pib_total", "populacao", "D_crises", "D_covid", "Media_temperatura", "pmc", "tarifa_comercial",  "renda_domiciliar", "nc_com_proj")

# Rodar modelos
ec_com_caret <- Predict_caret_autots(df_B,            
                                     lista_modelos_1, 
                                     col_com_ecf,     
                                     "ec_com",        
                                     "2024-03-01",   
                                     "2024-03-01",    
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 48,
                                     limite = "mean")
  
# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_gaussprPoly", "_TS_sarima")
x_filtered <- select_models(ec_com_caret[["names_X_predict"]], modelos_selecionados)

# Aplicar ensemble stacking com Meta-modelo glmnet
ec_com_caret_ensemble <- treinar_avaliar_modelos_caret(ec_com_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       x_filtered,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "ec_com",       # Variável dependente
                                                       "2024-03-01",   # Último período realizado
                                                       "2024-03-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ec_com_proj <- as.numeric(ec_com_caret_ensemble$df_projecoes$ec_com_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ec_com", "ec_com_proj", fator_tend = 1.01, fator_saz = 1.3, fator_res = 0)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ec_com", "ec_com_proj", "ec_com_B_Tend"))

# Limpeza de memória
gc()

#*****************************************************************************
# Faturada
#*****************************************************************************

ef_com_caret <- Predict_caret_autots(df_B,            
                                     lista_modelos_1, 
                                     col_com_ecf,     
                                     "ef_com",        
                                     "2024-03-01",   
                                     "2024-03-01",    
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 48,
                                     limite = "mean")

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_gaussprPoly", "_TS_sarima")
x_filtered <- select_models(ef_com_caret[["names_X_predict"]], modelos_selecionados)

# Aplicar ensemble stacking com Meta-modelo glmnet
ef_com_caret_ensemble <- treinar_avaliar_modelos_caret(ef_com_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       x_filtered,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "ef_com",       # Variável dependente
                                                       "2024-03-01",   # Último período realizado
                                                       "2024-03-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ef_com_proj <- as.numeric(ef_com_caret_ensemble$df_projecoes$ef_com_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ef_com", "ef_com_proj", fator_tend = 1.01, fator_saz = 1.3, fator_res = 0)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ef_com", "ef_com_proj"))

# comparar consumido e faturado
compare_multiple_columns_plot(df_B, "Data", c("ef_com", "ef_com_proj", "ec_com", "ec_com_proj"))

# Limpeza de memória
gc()


#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
df_B$diff_ecf_com <- df_B$ec_com_proj - df_B$ef_com_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_com_proj", "ef_com_proj"))
compare_multiple_columns_plot(df_B, "Data", c("ei_com_gd", "diff_ecf_com"))


#*****************************************************************************
# Injetada GD Comercial	
#*****************************************************************************
# Explicativas	
col_ei_com_gd <- c("indice_de_precos", "tarifa_comercial", "Media_temperatura",	 "Max_temperatura", "Min_temperatura", "diff_ecf_com")	

# Rodar modelos
ei_com_gd_caret <- treinar_avaliar_modelos_caret(df_B,             # Dataframe
                                                 lista_modelos_1, # Lista de modelos principal
                                                 col_ei_com_gd,    # Lista de explicativas
                                                 "ei_com_gd",      # Variável dependente
                                                 "2024-01-01",     # Último período realizado
                                                 "2023-10-01",     # Período que os modelos não irão ver (meu guia)
                                                 24,
                                                 data_inicio="2018-06-01")  # Últimos x meses que o modelo vai usar como teste interno

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_glmnet") 

# Ajustes
df_B$ei_com_gd_proj <- as.numeric(ei_com_gd_caret$df_projecoes$ei_com_gd_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ei_com_gd", "ei_com_gd_proj", fator_tend = 1.12, fator_saz= 1, fator_res=1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_com_gd", "diff_ecf_com",  "ei_com_gd_proj"))

# remover objetos e limpar memoria
rm(list=setdiff(ls(), objetos)) %>% gc() 

#-------

##############################################################################	
# ### (Projeção) Classe - Rural
##############################################################################	
#-------

#*****************************************************************************
## Consumidores	
#*****************************************************************************
## Covariadas
col_rur_nc <- c("Data", "D_crises", "D_covid", "domicilios", "populacao", "tarifa_rural", "tarifa_rural_sqrt",
                "indice_de_precos", "pib_total", "producao_agropecuaria", "massa_total", "Aquecimento_global")
                
# Rodar modelo 
nc_rur_caret <- treinar_avaliar_modelos_caret(df_B,            # Dataframe
                                              lista_modelos_1, # Lista de modelos principal
                                              col_rur_nc,      # Lista de explicativas
                                              "nc_rur",        # Variável dependente
                                              "2024-03-01",    # Último período realizado
                                              "2024-03-01",    # Período que os modelos não irão ver (meu guia)
                                              24)              # Últimos x meses que o modelo vai usar como teste interno

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_rf", "_ranger", "_Rborist")
x_filtered <- select_models(nc_rur_caret[["names_X_predict"]], modelos_selecionados)

# Aplicar ensemble stacking com Meta-modelo glmnet
nc_rur_caret_ensemble <- treinar_avaliar_modelos_caret(nc_rur_caret[["df_projecoes"]], # Dataframe
                                                       glmnet ,         # Meta-modelo (glmnet)
                                                       x_filtered,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "nc_rur",       # Variável dependente
                                                       "2024-03-01",   # Último período realizado
                                                       "2024-03-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$nc_rur_proj <- as.numeric(nc_rur_caret_ensemble$df_projecoes$nc_rur_glmnet)
df_B <- ajuste_pos_projecao(df_B, "nc_rur", "nc_rur_proj", fator_tend = 0.995, fator_saz = 0.2, fator_res = 0.2)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("nc_rur", "nc_rur_proj", "nc_rur_B_Tend"))

# Limpeza de memória
gc()

#*****************************************************************************
# Consumida
#*****************************************************************************
## Covariadas
col_rur_ecf <- c("ec_rur_saz", "D_covid", "indice_de_precos", "dias_uteis", "domicilios", "domicilios_sqrt",  "populacao", "renda_domiciliar",  "massa_total", "tarifa_rural", "tarifa_rural_sqrt", "Media_temperatura", "Media_temperatura_sqrt","Max_temperatura", "producao_agropecuaria",  "pib_agropecuario", "pib_agropecuario_sqrt")
                 
# Rodar modelos
ec_rur_caret <- treinar_avaliar_modelos_caret(df_B,            
                                              lista_modelos_1, 
                                              col_rur_ecf,     
                                              "ec_rur",        
                                              "2024-03-01",   
                                              "2024-03-01",    
                                              48)

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_bridge", "_glmnet")

# Ajustes
df_B$ec_rur_proj <- as.numeric(ec_rur_caret$df_projecoes$ec_rur_glmnet + ec_rur_caret$df_projecoes$ec_rur_bridge)/2
df_B <- ajuste_pos_projecao(df_B, "ec_rur", "ec_rur_proj", fator_tend = 1.05, fator_saz = 1.3, fator_res = 1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ec_rur", "ec_rur_proj", "ec_rur_B_Tend"))

# Limpeza de memória
gc()

#*****************************************************************************
# Faturada 	
#*****************************************************************************
# Rodar modelos
ef_rur_caret <- treinar_avaliar_modelos_caret(df_B,            
                                              lista_modelos_1, 
                                              col_rur_ecf,     
                                              "ef_rur",        
                                              "2024-03-01",   
                                              "2024-03-01",    
                                              48)

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_bridge", "_glmnet")

# Ajustes
df_B$ef_rur_proj <- as.numeric(ef_rur_caret$df_projecoes$ef_rur_glmnet + ef_rur_caret$df_projecoes$ef_rur_bridge)/2
df_B <- ajuste_pos_projecao(df_B, "ef_rur", "ef_rur_proj", fator_tend = 1.03, fator_saz = 1.3, fator_res = 1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ec_rur", "ec_rur_proj", "ec_rur_B_Tend"))

# comparar consumida e faturada
compare_multiple_columns_plot(df_B, "Data", c("ec_rur", "ef_rur", "ec_rur_proj", "ef_rur_proj"))

# Limpeza de memória
gc()

#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
# Residencial
df_B$diff_ecf_rur <- df_B$ec_rur_proj - df_B$ef_rur_proj
compare_multiple_columns_plot(df_B, "Data", c("ei_rur_gd", "diff_ecf_rur"))


#*****************************************************************************
# Injetada GD Residencial	
#*****************************************************************************
# Explicativas	
col_ei_rur_gd <- c("indice_de_precos", "indice_de_precos_sqrt", "tarifa_rural",  
                   "Media_temperatura",	"Media_temperatura_sqrt",	 "Max_temperatura",
                   "Min_temperatura",  "Media_pluviometria",  "diff_ecf_rur")	

# Rodar modelos
ei_rur_gd_caret <- treinar_avaliar_modelos_caret(df_B,
                                                 lista_modelos_1, 
                                                 col_ei_rur_gd,   
                                                 "ei_rur_gd",    
                                                 "2024-03-01",     
                                                 "2024-03-01",    
                                                 24,
                                                 data_inicio="2018-06-01") 

# Ajustes
df_B$ei_rur_gd_proj <- as.numeric(ei_rur_gd_caret$df_projecoes$ei_rur_gd_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ei_rur_gd", "ei_rur_gd_proj", fator_tend = 1.15, fator_saz= 0.2, fator_res=0.2)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_rur_gd", "diff_ecf_rur",  "ei_rur_gd_proj", "ei_rur_gd_B_Tend"))

# Ajustar faturada
df_B$ef_rur_proj_2 <- df_B$ef_rur_proj - df_B$ei_rur_gd_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_rur", "ef_rur", "ec_rur_proj", "ef_rur_proj", "ef_rur_proj_2"))

#-------

##############################################################################	
# ### (Projeção) Classe - Setor Público	
##############################################################################	
#-------

#*****************************************************************************
# Consumidores	
#*****************************************************************************
## Covariadas	
col_nc_spu <- c("Mes_factor", "D_covid", "tarifa_residencial","tarifa_industrial", "tarifa_comercial", "tarifa_rural", 	
                "tarifa_poder_publico",  "tarifa_iluminacao_publica", "tarifa_servico_publico_sqrt", "tarifa_servico_publico",	
                "tarifa_consumo_proprio", "indice_de_precos", "pib_total")	
	
# Rodar modelos
nc_spu_caret <- treinar_avaliar_modelos_caret(df_B,            # Dataframe
                                              lista_modelos_1, # Lista de modelos principal
                                              col_nc_spu,      # Lista de explicativas
                                              "nc_spu",        # Variável dependente
                                              "2024-01-01",    # Último período realizado
                                              "2023-10-01",    # Período que os modelos não irão ver (meu guia)
                                              24)              # Últimos x meses que o modelo vai usar como teste interno

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_svmLinear", "_svmLinear2", "_lm", "_rlm", "_lmStepAIC", "_glm", "_glmStepAIC", "_bayesglm",
                          "_gaussprLinear", "_blasso", "_bridge", "_blassoAveraged")
x_filtered <- select_models(nc_spu_caret[["names_X_predict"]], modelos_selecionados)

# Aplicar ensemble stacking com Meta-modelo glmnet
nc_spu_caret_ensemble <- treinar_avaliar_modelos_caret(nc_spu_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       x_filtered,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "nc_spu",       # Variável dependente
                                                       "2024-01-01",   # Último período realizado
                                                       "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$nc_spu_proj <- as.numeric(nc_spu_caret_ensemble$df_projecoes$nc_spu_glmnet)
df_B <- ajuste_pos_projecao(df_B, "nc_spu", "nc_spu_proj", fator_tend = 1.007, fator_saz= 0.1, fator_res=0)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("nc_spu", "nc_spu_proj"))

# Limpeza de memória
gc()

#*****************************************************************************
# Consumida 	
#*****************************************************************************
## Covariadas	
col_ecf_spu <- c("Mes_factor", "D_crises", "D_covid", "indice_de_precos", "dias_uteis", "domicilios", "massa_total", "Estacao",
                 "pib_total", "populacao", "Media_temperatura", "Max_temperatura", "Min_temperatura", "tarifa_residencial",
                 "tarifa_poder_publico",  "tarifa_iluminacao_publica",  "tarifa_servico_publico", "tarifa_servico_publico_sqrt",
                 "renda_domiciliar", "Aquecimento_global", "nc_spu_proj")

# Rodar modelos
ec_spu_caret <- treinar_avaliar_modelos_caret(df_B,            # Dataframe
                                              lista_modelos_1, # Lista de modelos principal
                                              col_ecf_spu,      # Lista de explicativas
                                              "ec_spu",        # Variável dependente
                                              "2024-01-01",    # Último período realizado
                                              "2023-10-01",    # Período que os modelos não irão ver (meu guia)
                                              24)              # Últimos x meses que o modelo vai usar como teste interno

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_svmLinear", "_svmLinear2", "_svmPoly",  "_lm", "_leapBackward",
                          "_leapSeq", "_lmStepAIC", "_glmnet", "_glm", "_glm.nb", "_glmboost",
                          "_glmStepAIC", "_bayesglm", "_gaussprLinear", "_plsRglm", "_gaussprPoly", "_foba", 
                          "_lasso", "_ridge", "_bridge",  "_blassoAveraged", "_gamSpline", "_enet")   
x_filtered <- select_models(ec_spu_caret[["names_X_predict"]], modelos_selecionados)

# Aplicar ensemble stacking com Meta-modelo glmnet
ec_spu_caret_ensemble <- treinar_avaliar_modelos_caret(ec_spu_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       x_filtered,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "ec_spu",       # Variável dependente
                                                       "2024-01-01",   # Último período realizado
                                                       "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ec_spu_proj <- as.numeric(ec_spu_caret_ensemble$df_test_com_previsoes$ec_spu_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ec_spu", "ec_spu_proj", fator_tend = 1.01, fator_saz= 1.2, fator_res=0.5)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ec_spu", "ec_spu_proj"))

# Limpeza de memória
gc()

#*****************************************************************************
# Faturada 	
#*****************************************************************************
# Rodar modelos
ef_spu_caret <- treinar_avaliar_modelos_caret(df_B,            # Dataframe
                                              lista_modelos_1, # Lista de modelos principal
                                              col_ecf_spu,      # Lista de explicativas
                                              "ef_spu",        # Variável dependente
                                              "2024-01-01",    # Último período realizado
                                              "2023-10-01",    # Período que os modelos não irão ver (meu guia)
                                              24)              # Últimos x meses que o modelo vai usar como teste interno

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_svmLinear", "_svmLinear2", "_svmPoly",  "_lm", "_leapBackward",
                          "_leapSeq", "_lmStepAIC", "_glmnet", "_glm", "_glm.nb", "_glmboost",
                          "_glmStepAIC", "_bayesglm", "_gaussprLinear", "_plsRglm", "_gaussprPoly", "_foba", 
                          "_lasso", "_ridge", "_bridge",  "_blassoAveraged", "_gamSpline", "_enet")   
x_filtered <- select_models(ef_spu_caret[["names_X_predict"]], modelos_selecionados)

# Aplicar ensemble stacking com Meta-modelo glmnet
ef_spu_caret_ensemble <- treinar_avaliar_modelos_caret(ef_spu_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       x_filtered,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "ef_spu",       # Variável dependente
                                                       "2024-01-01",   # Último período realizado
                                                       "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ef_spu_proj <- as.numeric(ef_spu_caret_ensemble$df_test_com_previsoes$ef_spu_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ef_spu", "ef_spu_proj", fator_tend = 1, fator_saz= 1.15, fator_res=0.5)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ef_spu", "ef_spu_proj"))

# Comparar consumida e faturado
compare_multiple_columns_plot(df_B, "Data", c("ec_spu", "ec_spu_proj", "ef_spu_proj", "ef_spu"))

#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
# Residencial
df_B$diff_ecf_spu <- df_B$ec_spu_proj - df_B$ef_spu_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_spu_proj", "ef_spu_proj"))
compare_multiple_columns_plot(df_B, "Data", c("ei_spu_gd", "diff_ecf_spu"))


#*****************************************************************************
# Injetada GD 
#*****************************************************************************
# Explicativas	
col_ei_spu_gd <- c("indice_de_precos", "indice_de_precos_sqrt", "tarifa_servico_publico",  
                   "Media_temperatura",	"Media_temperatura_sqrt",	 "Max_temperatura",
                   "Min_temperatura",  "Media_pluviometria",  "diff_ecf_rur")	

# Rodar modelos
ei_spu_gd_caret <- treinar_avaliar_modelos_caret(df_B,
                                                 lista_modelos_1, 
                                                 col_ei_spu_gd,   
                                                 "ei_spu_gd",    
                                                 "2024-01-01",     
                                                 "2023-10-01",    
                                                 24,
                                                 data_inicio="2018-06-01") 

# Definir modelos que vão para o ensemble
ei_spu_gd_caret[["df_projecoes"]][["diff_ecf_spu"]] <- as.numeric(df_B$diff_ecf_spu)
models_select <- c("ei_spu_gd_rlm", "ei_spu_gd_plsRglm", "ei_spu_gd_foba",  "ei_spu_gd_ridge", "diff_ecf_spu") 

# Aplicar ensemble stacking com Meta-modelo glmnet
ei_spu_gd_caret_ensemble <- treinar_avaliar_modelos_caret(ei_spu_gd_caret[["df_projecoes"]], # Dataframe
                                                          glmnet,         # Meta-modelo (glmnet)
                                                          models_select,     # Lista de explicativas (valores projetados de cada modelo)
                                                          "ei_spu_gd",       # Variável dependente
                                                          "2024-01-01",   # Último período realizado
                                                          "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                          12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ei_spu_gd_proj <- as.numeric(ei_spu_gd_caret_ensemble$df_projecoes$ei_spu_gd_glmnet)
df_B <- ajustar_lag_lead(df_B, "2020-01-01", "ei_spu_gd_proj", 1, "lead")
df_B <- ajuste_pos_projecao(df_B, "ei_spu_gd", "ei_spu_gd_proj", fator_tend = 1, fator_saz= 2.4, fator_res=1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_spu_gd", "diff_ecf_spu",  "ei_spu_gd_proj"))

# remover objetos e limpar memoria
rm(list=setdiff(ls(), objetos)) %>% gc() 

#-------

##############################################################################	
# ### (Projeção) Classe - Iluminação Publica	
##############################################################################	
#-------

#*****************************************************************************
# Consumidores	
#*****************************************************************************
df_B$D_choque_nc_ipu_MA[is.na(df_B$D_choque_nc_ipu_MA)] <-1
  
## Covariadas	
col_nc_ipu <- c("Mes_factor", "D_covid", "D_crises", "tarifa_residencial","tarifa_industrial", "tarifa_comercial", "tarifa_rural", 	
                "tarifa_poder_publico",  "tarifa_iluminacao_publica", "tarifa_iluminacao_publica_sqrt", "tarifa_servico_publico",	
                "tarifa_consumo_proprio", "indice_de_precos", "pib_total", "D_choque_nc_ipu_MA", "pim_geral", "domicilios", 
                "populacao",  "massa_total")	

# Rodar modelos
nc_ipu_caret <- treinar_avaliar_modelos_caret(df_B,            
                                              lista_modelos_1, 
                                              col_nc_ipu,    
                                              "nc_ipu",        
                                              "2024-01-01",  
                                              "2023-10-01",  
                                              24)       

# Definir modelos que vão para o ensemble
modelos_selecionados <- c("_svmLinear", "_svmLinear2", "_svmLinear3", "_lm",  "_leapBackward", "_leapForward", "_leapSeq",
                          "_lmStepAIC", "_glmnet", "_glm", "_glm.nb", "_glmboost", "_glmStepAIC", "_bayesglm", "_foba",  
                          "_gamSpline") 
x_filtered <- select_models(nc_ipu_caret[["names_X_predict"]], modelos_selecionados)

# Aplicar ensemble stacking com Meta-modelo glmnet
nc_ipu_caret_ensemble <- treinar_avaliar_modelos_caret(nc_ipu_caret[["df_projecoes"]], 
                                                       glmnet,      
                                                       x_filtered,  
                                                       "nc_ipu",   
                                                       "2024-01-01",  
                                                       "2024-01-01",  
                                                       12)  

# Ajustes
df_B$nc_ipu_proj <- as.numeric(nc_ipu_caret_ensemble$df_test_com_previsoes$nc_ipu_glm)
df_B <- ajuste_pos_projecao(df_B, "nc_ipu", "nc_ipu_proj", fator_tend = 1.055, fator_saz= 0.8, fator_res=0)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("nc_ipu", "nc_ipu_proj"))

# Limpeza de memória
gc()

#*****************************************************************************
# Consumida 	
#*****************************************************************************
## Covariadas	
col_ecf_ipu <- c("Mes_factor", "D_crises", "D_covid", "indice_de_precos", "dias_uteis", "domicilios", "massa_total", "Estacao",
                 "pib_total", "populacao", "Media_temperatura", "Max_temperatura", "Min_temperatura", "tarifa_residencial",
                 "tarifa_poder_publico",  "tarifa_iluminacao_publica",  "tarifa_servico_publico", "tarifa_servico_publico_sqrt",
                 "renda_domiciliar", "Aquecimento_global", "nc_ipu_proj")

# Rodar modelos caret
ec_ipu_caret <- Predict_caret_autots(df_B,     
                                    lista_modelos_1, 
                                    col_ecf_ipu,    
                                    "ec_ipu",    
                                    "2024-01-01",   
                                    "2024-01-01",   
                                    24,
                                    data_inicio = "2005-01-01",
                                    n_test_ts = 42,
                                    limite = "mean")    

# Definir modelos que vão para o ensemble (manualmente)
select_models <- c("ec_ipu_TS_stlm", "ec_ipu_TS_tbats", "ec_ipu_TS_bats")   

# Aplicar ensemble stacking com Meta-modelo glmnet
ec_ipu_caret_ensemble <- treinar_avaliar_modelos_caret(ec_ipu_caret[["df_projecoes"]], 
                                                       glmnet,         
                                                       select_models,   
                                                       "ec_ipu",     
                                                       "2024-01-01",  
                                                       "2024-01-01",  
                                                       12)        

# Ajustes
df_B$ec_ipu_proj <- as.numeric(ec_ipu_caret_ensemble$df_test_com_previsoes$ec_ipu_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ec_ipu", "ec_ipu_proj", fator_tend = 1.01, fator_saz= 1, fator_res=01)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ec_ipu", "ec_ipu_proj"))

# Limpeza de memória
gc()

#*****************************************************************************
# Faturada 	
#*****************************************************************************
# Rodar modelos caret
ef_ipu_caret <- Predict_caret_autots(df_B,     
                                     lista_modelos_1, 
                                     col_ecf_ipu,    
                                     "ef_ipu",    
                                     "2024-01-01",   
                                     "2024-01-01",   
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 42,
                                     limite = "mean")    

# Definir modelos que vão para o ensemble (manualmente)
select_models <- c("ef_ipu_TS_stlm", "ef_ipu_TS_tbats", "ef_ipu_TS_bats")   

# Aplicar ensemble stacking com Meta-modelo glmnet
ef_ipu_caret_ensemble <- treinar_avaliar_modelos_caret(ef_ipu_caret[["df_projecoes"]], 
                                                       glmnet,         
                                                       select_models,   
                                                       "ef_ipu",     
                                                       "2024-01-01",  
                                                       "2024-01-01",  
                                                       12)        

# Ajustes
df_B$ef_ipu_proj <- as.numeric(ef_ipu_caret_ensemble$df_test_com_previsoes$ef_ipu_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ef_ipu", "ef_ipu_proj", fator_tend = 1.015, fator_saz= 1, fator_res=01)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ef_ipu", "ef_ipu_proj"))

# comsumido e faturado
compare_multiple_columns_plot(df_B, "Data", c("ef_ipu", "ef_ipu_proj", "ec_ipu", "ec_ipu_proj"))

#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
# Residencial
df_B$diff_ecf_ipu <- df_B$ec_ipu_proj - df_B$ef_ipu_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_ipu_proj", "ef_ipu_proj"))
compare_multiple_columns_plot(df_B, "Data", c("ei_ipu_gd", "diff_ecf_ipu"))

#*****************************************************************************
# Injetada GD 
#*****************************************************************************
df_B$ei_ipu_gd_proj <- 0 

#--------

##############################################################################	
# ### (Projeção) Classe - Poder Publico	
##############################################################################	
#--------

#*****************************************************************************
# Consumidores	
#*****************************************************************************
## Covariadas	
col_nc_ppu <- c("Mes_factor", "D_covid", "D_crises", "tarifa_residencial","tarifa_industrial", "tarifa_comercial", "tarifa_rural", 	
                "tarifa_poder_publico",  "tarifa_iluminacao_publica", "tarifa_poder_publico_sqrt", "tarifa_servico_publico",	
                "tarifa_consumo_proprio", "indice_de_precos", "pib_total", "pim_geral", "domicilios", "populacao",  "massa_total")
                	
# Rodar modelos
nc_ppu_caret <- treinar_avaliar_modelos_caret(df_B,       
                                              lista_modelos_1, 
                                              col_nc_ppu, 
                                              "nc_ppu",   
                                              "2024-01-01",  
                                              "2023-10-01", 
                                              24)   

# Definir modelos que vão para o ensemble
models_select <- c("nc_ppu_svmLinear", "nc_ppu_svmLinear2",  "nc_ppu_lm", "nc_ppu_leapBackward", 
                  "nc_ppu_leapForward", "nc_ppu_leapSeq","nc_ppu_lmStepAIC", "nc_ppu_glmnet", "nc_ppu_glm", "nc_ppu_glm.nb", 
                  "nc_ppu_glmboost", "nc_ppu_glmStepAIC", "nc_ppu_bayesglm", "nc_ppu_foba",  "nc_ppu_gamSpline") 
# Aplicar ensemble stacking com Meta-modelo glmnet
nc_ppu_caret_ensemble <- treinar_avaliar_modelos_caret(nc_ppu_caret[["df_projecoes"]],
                                                       glmnet,      
                                                       models_select,   
                                                       "nc_ppu",    
                                                       "2024-01-01", 
                                                       "2024-01-01",  
                                                       12)  

# Ajustes
df_B$nc_ppu_proj <- as.numeric(nc_ppu_caret_ensemble$df_test_com_previsoes$nc_ppu_glm)
df_B <- ajuste_pos_projecao(df_B, "nc_ppu", "nc_ppu_proj", fator_tend = 1.01, fator_saz= 0.8, fator_res=0)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("nc_ppu", "nc_ppu_proj"))

# Limpeza de memória
gc()

#*****************************************************************************
# Consumida 	
#*****************************************************************************
## Covariadas	
col_ecf_ppu <- c("Mes_factor", "D_crises", "D_covid", "indice_de_precos", "dias_uteis", "domicilios", "massa_total", "Estacao",
                 "pib_total", "populacao", "Media_temperatura", "Max_temperatura", "Min_temperatura", "tarifa_residencial",
                 "tarifa_poder_publico",  "tarifa_iluminacao_publica",  "tarifa_servico_publico", "tarifa_poder_publico_sqrt",
                 "renda_domiciliar", "Aquecimento_global", "nc_ppu_proj")

# Rodar modelos caret
ec_ppu_caret <- Predict_caret_autots(df_B,         
                                    lista_modelos_1, 
                                    col_ecf_ppu, 
                                    "ec_ppu",      
                                    "2024-01-01",   
                                    "2024-01-01",   
                                    24,
                                    data_inicio = "2005-01-01",
                                    n_test_ts = 12,
                                    limite = "sup")             

# modelos escolhidos
x_filtered <- c("ec_ppu_TS_prophet", "ec_ppu_lmStepAIC","ec_ppu_lm")   

# Aplicar ensemble stacking com Meta-modelo glmnet
ec_ppu_caret_ensemble <- treinar_avaliar_modelos_caret(ec_ppu_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       x_filtered,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "ec_ppu",       # Variável dependente
                                                       "2024-01-01",   # Último período realizado
                                                       "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ec_ppu_proj <- as.numeric(ec_ppu_caret_ensemble$df_test_com_previsoes$ec_ppu_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ec_ppu", "ec_ppu_proj", fator_tend = 1.05, fator_saz= 2, fator_res=1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ec_ppu", "ec_ppu_proj"))

# Limpeza de memória
gc()

#*****************************************************************************
# Faturada 	
#*****************************************************************************
# Rodar modelos caret
ef_ppu_caret <- Predict_caret_autots(df_B,         
                                     lista_modelos_1, 
                                     col_ecf_ppu, 
                                     "ef_ppu",      
                                     "2024-01-01",   
                                     "2024-01-01",   
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 12,
                                     limite = "sup")             

# modelos escolhidos
x_filtered <- c("ef_ppu_TS_prophet", "ef_ppu_lmStepAIC","ef_ppu_lm")   

# Aplicar ensemble stacking com Meta-modelo glmnet
ef_ppu_caret_ensemble <- treinar_avaliar_modelos_caret(ef_ppu_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       x_filtered,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "ef_ppu",       # Variável dependente
                                                       "2024-01-01",   # Último período realizado
                                                       "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ef_ppu_proj <- as.numeric(ef_ppu_caret_ensemble$df_test_com_previsoes$ef_ppu_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ef_ppu", "ef_ppu_proj", fator_tend = 1.05, fator_saz= 2, fator_res=1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ef_ppu", "ef_ppu_proj"))

# comparar consumida e faturada
compare_multiple_columns_plot(df_B, "Data", c("ef_ppu", "ef_ppu_proj","ec_ppu", "ec_ppu_proj"))

#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
# Residencial
df_B$diff_ecf_ppu <- df_B$ec_ppu_proj - df_B$ef_ppu_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_ppu_proj", "ef_ppu_proj"))
compare_multiple_columns_plot(df_B, "Data", c("ei_ppu_gd", "diff_ecf_ppu"))

#*****************************************************************************
# Injetada GD 
#*****************************************************************************
# Explicativas	
col_ei_ppu_gd <- c("indice_de_precos", "indice_de_precos_sqrt", "tarifa_poder_publico",  
                   "Media_temperatura",	"Media_temperatura_sqrt",	 "Max_temperatura",
                   "Min_temperatura",  "Media_pluviometria",  "diff_ecf_ppu")	

# Rodar modelos
ei_ppu_gd_caret <- treinar_avaliar_modelos_caret(df_B,
                                                 lista_modelos_1, 
                                                 col_ei_ppu_gd,   
                                                 "ei_ppu_gd",    
                                                 "2024-01-01",     
                                                 "2023-10-01",    
                                                 24,
                                                 data_inicio="2005-01-01") 

# Definir modelos que vão para o ensemble
models_select <- c("ei_ppu_gd_leapForward", "ei_ppu_gd_gamSpline", "ei_ppu_gd_gaussprPoly_2") 

# Aplicar ensemble stacking com Meta-modelo glmnet
ei_ppu_gd_caret_ensemble <- treinar_avaliar_modelos_caret(ei_ppu_gd_caret[["df_projecoes"]], 
                                                          glmnet,         
                                                          models_select,  
                                                          "ei_ppu_gd",    
                                                          "2024-01-01",   
                                                          "2024-01-01",   
                                                          12)            

# Ajustes
df_B$ei_ppu_gd_proj <- as.numeric(ei_ppu_gd_caret_ensemble$df_projecoes$ei_ppu_gd_glmnet)
df_B <- ajustar_lag_lead(df_B, "2020-01-01", "ei_ppu_gd_proj", 1, "lead")
df_B <- ajuste_pos_projecao(df_B, "ei_ppu_gd", "ei_ppu_gd_proj", fator_tend = 1, fator_saz= 1, fator_res=1)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_ppu_gd", "diff_ecf_ppu",  "ei_ppu_gd_proj"))

# remover objetos e limpar memoria
rm(list=setdiff(ls(), objetos)) %>% gc()

#--------

##############################################################################	
# ### (Projeção) Classe - Proprio
##############################################################################
#--------

#*****************************************************************************
# Consumidores	
#*****************************************************************************
## Covariadas	
col_nc_pro <- c("tarifa_residencial", "tarifa_industrial", "tarifa_comercial", "indice_de_precos", "pim_geral", "pib_total",  	
                "pmc", "domicilios", "domicilios_sqrt", "populacao", "massa_total",  "Mes_factor",  "D_crises", "D_covid")	
                
# Rodar modelos caret
nc_pro_caret <- Predict_caret_autots(df_B,     
                                     lista_modelos_1, 
                                     col_nc_pro,    
                                     "nc_pro",    
                                     "2024-01-01",   
                                     "2024-01-01",   
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 42,
                                     limite = "mean")    

# Definir modelos que vão para o ensemble (manualmente)
select_models <- c("nc_pro_TS_stlm", "nc_pro_BstLm", "nc_pro_rf", "nc_pro_gbm") 

# Aplicar ensemble stacking com Meta-modelo glmnet
nc_pro_caret_ensemble <- treinar_avaliar_modelos_caret(nc_pro_caret$df_test_com_previsoes,
                                                       glmnet,       
                                                       select_models,  
                                                       "nc_pro",     
                                                       "2024-01-01", 
                                                       "2024-01-01",  
                                                       12)   

# Ajustes
df_B$nc_pro_proj <- as.numeric(nc_pro_caret_ensemble$df_test_com_previsoes$nc_pro_glmnet)
df_B <- ajuste_pos_projecao(df_B, "nc_pro", "nc_pro_proj", fator_tend = 1, fator_saz= 1.5, fator_res=2)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("nc_pro", "nc_pro_proj"))

# Limpeza de memória
gc()


#*****************************************************************************
# Consumida 	
#*****************************************************************************
df_B$nc_pro_proj_sqrt <- df_B$nc_pro_proj * df_B$nc_pro_proj

## Covariadas	
col_ecf_pro <- c("D_crises", "D_covid", "indice_de_precos", "massa_total", "pib_total",  "Media_temperatura", "nc_pro_proj",
                 "nc_pro_proj_sqrt", "Max_temperatura", "tarifa_consumo_proprio", "tarifa_consumo_proprio_sqrt", "renda_domiciliar")
                 
# Rodar modelos caret
ec_pro_caret <- Predict_caret_autots(df_B,     
                                     lista_modelos_1, 
                                     col_ecf_pro,    
                                     "ec_pro",    
                                     "2024-01-01",   
                                     "2024-01-01",   
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 42,
                                     limite = "mean")       

# Definir modelos que vão para o ensemble
models_select <- c("ec_pro_TS_ets",  "ec_pro_blasso", "ec_pro_lasso", "ec_pro_ridge") 

# Aplicar ensemble stacking com Meta-modelo glmnet
ec_pro_caret_ensemble <- treinar_avaliar_modelos_caret(ec_pro_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       models_select,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "ec_pro",       # Variável dependente
                                                       "2024-01-01",   # Último período realizado
                                                       "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ec_pro_proj <- as.numeric(ec_pro_caret_ensemble$df_test_com_previsoes$ec_pro_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ec_pro", "ec_pro_proj", fator_tend = 1.03, fator_saz= 1.2, fator_res=2)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ec_pro", "ec_pro_proj"))

# Limpeza de memória
gc()

#*****************************************************************************
# Faturada 	
#*****************************************************************************
# Rodar modelos caret
ef_pro_caret <- Predict_caret_autots(df_B,     
                                     lista_modelos_1, 
                                     col_ecf_pro,    
                                     "ef_pro",    
                                     "2024-01-01",   
                                     "2024-01-01",   
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 42,
                                     limite = "mean")       

# Definir modelos que vão para o ensemble
models_select <- c("ef_pro_TS_ets",  "ef_pro_blasso", "ef_pro_lasso", "ef_pro_ridge") 

# Aplicar ensemble stacking com Meta-modelo glmnet
ef_pro_caret_ensemble <- treinar_avaliar_modelos_caret(ef_pro_caret[["df_projecoes"]], # Dataframe
                                                       glmnet,         # Meta-modelo (glmnet)
                                                       models_select,     # Lista de explicativas (valores projetados de cada modelo)
                                                       "ef_pro",       # Variável dependente
                                                       "2024-01-01",   # Último período realizado
                                                       "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                       12)             # Últimos x meses que o modelo vai usar como teste interno

# Ajustes
df_B$ef_pro_proj <- as.numeric(ef_pro_caret_ensemble$df_test_com_previsoes$ef_pro_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ef_pro", "ef_pro_proj", fator_tend = 1.03, fator_saz= 1.2, fator_res=2)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ef_pro", "ef_pro_proj"))

# Comparar consumido e faturado
compare_multiple_columns_plot(df_B, "Data", c("ef_pro", "ef_pro_proj", "ec_pro", "ec_pro_proj"))

#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
# Residencial
df_B$diff_ecf_pro <- df_B$ec_pro_proj - df_B$ef_pro_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_pro_proj", "ef_pro_proj"))
compare_multiple_columns_plot(df_B, "Data", c("ei_pro_gd", "diff_ecf_pro"))

#*****************************************************************************
# Injetada GD 
#*****************************************************************************
# Explicativas	
col_ei_pro_gd <- c("Trend_Aquecimento_global", "indice_de_precos", "Media_temperatura", "Max_temperatura", 	
                   "Min_temperatura", "Media_pluviometria", "massa_total", "domicilios")	

# Rodar modelos
ei_pro_gd_caret <- treinar_avaliar_modelos_caret(df_B,             # Dataframe
                                                 lista_modelos_GD, # Lista de modelos principal
                                                 col_ei_pro_gd,    # Lista de explicativas
                                                 "ei_pro_gd",      # Variável dependente
                                                 "2024-01-01",     # Último período realizado
                                                 "2023-10-01",     # Período que os modelos não irão ver (meu guia)
                                                 24)               # Últimos x meses que o modelo vai usar ppuo teste interno

# Definir modelos que vão para o ensemble
models_select <- c( "ei_pro_gd_svmPoly_2",  "ei_pro_gd_gaussprPoly_2")

#Aplicar ensemble stacking ppu Meta-modelo glmnet
ei_pro_gd_caret_ensemble <- treinar_avaliar_modelos_caret(ei_pro_gd_caret[["df_projecoes"]], # Dataframe
                                                          glmnet,         # Meta-modelo (glmnet)
                                                          models_select,     # Lista de explicativas (valores projetados de cada modelo)
                                                          "ei_pro_gd",       # Variável dependente
                                                          "2024-01-01",   # Último período realizado
                                                          "2024-01-01",   # Período que os modelos não irão ver (meu guia)
                                                          12)             # Últimos x meses que o modelo vai usar ppuo teste interno

# Ajustes
df_B$ei_pro_gd_proj <- as.numeric(ei_pro_gd_caret_ensemble$df_projecoes$ei_pro_gd_glmnet)
df_B <- ajuste_pos_projecao(df_B, "ei_pro_gd", "ei_pro_gd_proj", fator_tend = 0.4, fator_saz= 3, fator_res=0)

# Verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_pro_gd", "ei_pro_gd_proj"))

# remover objetos e limpar memoria
rm(list=setdiff(ls(), objetos)) %>% gc()

#--------

##############################################################################	
# ### (Somatório) todas as classes
##############################################################################	
#-------

#*****************************************************************************
# Consumidores
#*****************************************************************************
df_B <- as.data.frame(df_B)
# calcular nc total 	
df_B$nc_total_proj <- rowSums(df_B[, c("nc_res_proj", "nc_ind_proj", "nc_com_proj", "nc_rur_proj", "nc_ppu_proj", 
                                       "nc_spu_proj", "nc_ipu_proj", "nc_pro_proj")], na.rm = TRUE)	
                                               	
# Comparar 	
compare_multiple_columns_plot(df_B, "Data", c("nc_res_proj", "nc_ind_proj",	"nc_com_proj", "nc_rur_proj", "nc_ppu_proj", 
                                              "nc_spu_proj", "nc_ipu_proj", "nc_pro_proj", "nc_total_proj"))	

#*****************************************************************************
# Consumida 	
#*****************************************************************************
# calcular ec total 	
df_B$ec_total_proj <- rowSums(df_B[, c("ec_res_proj", "ec_ind_proj", "ec_com_proj", "ec_rur_proj", "ec_ppu_proj", 
                                       "ec_spu_proj", "ec_ipu_proj", "ec_pro_proj")], na.rm = TRUE)	

# Comparar 	
compare_multiple_columns_plot(df_B, "Data", c("ec_res_proj", "ec_ind_proj",	"ec_com_proj", "ec_rur_proj", "ec_ppu_proj", 
                                              "ec_spu_proj", "ec_ipu_proj", "ec_pro_proj", "ec_total_proj"))	

#*****************************************************************************
# Faturada 	
#*****************************************************************************
# calcular ec total 	
df_B$ef_total_proj <- rowSums(df_B[, c("ef_res_proj", "ef_ind_proj", "ef_com_proj", "ef_rur_proj", "ef_ppu_proj", 
                                       "ef_spu_proj", "ef_ipu_proj", "ef_pro_proj")], na.rm = TRUE)	

# Comparar 	
compare_multiple_columns_plot(df_B, "Data", c("ef_res_proj", "ef_ind_proj",	"ef_com_proj", "ef_rur_proj", "ef_ppu_proj", 
                                              "ef_spu_proj", "ef_ipu_proj", "ef_pro_proj", "ef_total_proj"))	

# remover objetos e limpar memoria
rm(list=setdiff(ls(), objetos)) %>% gc()

#-------

##############################################################################
# ### (Projeção) Energia Injetada total
##############################################################################
#-------
#*****************************************************************************
# Injetada total	
#*****************************************************************************
df_B$ei_total[df_B$Data == as.Date("2024-02-01")] <- 767993 
df_B$ei_total[df_B$Data == as.Date("2024-03-01")] <- 830565

# Explicativas	
col_ei_total <- c("Mes", "Trend", "D_covid", "D_crises", #"nc_total_proj", "ec_total_proj",
                  "tarifa_residencial", "tarifa_industrial", "tarifa_comercial", "tarifa_poder_publico", "tarifa_rural",
                  "tarifa_iluminacao_publica", "tarifa_servico_publico", "tarifa_consumo_proprio", "indice_de_precos",
                  "dias_uteis", "dias_totais", "pmc", "pim_geral", "pim_energia", "pim_extrativa", "populacao", "domicilios",
                  "renda_domiciliar",  "massa_total", "pib_total", "pib_servicos", "pib_industria", "pib_agropecuario",
                  "producao_agropecuaria", "Media_pluviometria", "Precipitacao_total", "Aquecimento_global",
                  "Media_temperatura", "Min_temperatura", "Max_temperatura", "ONI")

# Rodar modelos caret
ei_total_caret <- Predict_caret_autots(df_B,     
                                     lista_modelos_1, 
                                     col_ei_total,    
                                     "ei_total",    
                                     "2024-03-01",   
                                     "2024-03-01",   
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 24,
                                     limite = "mean")  

# Definir modelos que vão para o ensemble
models_select<- c("ei_total_TS_sarima", "ei_total_TS_prophet")

# Aplicar ensemble media
df_B$ei_total_proj <- rowMeans(ei_total_caret$df_projecoes[, c(models_select)], na.rm = TRUE)	

# Ajustes	
df_B <- ajuste_pos_projecao(df_B, "ei_total", "ei_total_proj", fator_tend = 0.997, fator_saz= 1, fator_res= 1)

# verificar se foi adicionado ao dataframe
compare_multiple_columns_plot(df_B, "Data", c("ei_total", "ei_total_proj"))

# remover objetos e limpar memoria
rm(list=setdiff(ls(), objetos)) %>% gc()

#-------

##############################################################################	
# ### (Projeção)  Número de consumidores GD	total
##############################################################################	
#-------

#*****************************************************************************
# Consumidores GD total
#*****************************************************************************
# Explicativas	
col_nc_total_gd <- c("Trend_Aquecimento_global", "nc_total_proj", "tarifa_residencial", "indice_de_precos", 
                     "massa_total",	"populacao", "domicilios")	

# Rodar modelos caret
nc_total_gd_caret <- Predict_caret_autots(df_B,     
                                     lista_modelos_1, 
                                     col_nc_total_gd,    
                                     "nc_total_gd",    
                                     "2024-01-01",   
                                     "2024-01-01",   
                                     24,
                                     data_inicio = "2005-01-01",
                                     n_test_ts = 42,
                                     limite = "mean") 

# Definir modelos que vão para o ensemble
models_select <- c("nc_total_gd_TS_ets", "nc_total_gd_TS_tbats",  "nc_total_gd_TS_bats", "nc_total_gd_TS_stlm") 

# calcular media ensemble 	
df_B$nc_total_gd_proj <- rowMeans(nc_total_gd_caret$df_projecoes[, models_select], na.rm = TRUE)	
                                               	
# Comparar 	
compare_multiple_columns_plot(df_B, "Data", c("nc_total_gd", "nc_total_gd_proj"))
compare_multiple_columns_plot(df_B, "Data", c("nc_total_proj", "nc_total_gd_proj"))

#-------

##############################################################################	
# ### (Somatório) Energia injetada GD total 
##############################################################################	
#-------

#*****************************************************************************
# Diferença consumida e faturada ≈ Injetada GD 		
#*****************************************************************************
df_B$diff_ecf_total <- df_B$ec_total_proj - df_B$ef_total_proj
compare_multiple_columns_plot(df_B, "Data", c("ec_total_proj", "ef_total_proj"))
compare_multiple_columns_plot(df_B, "Data", c("ei_total_gd", "diff_ecf_total"))

#*****************************************************************************
#Energia Injetada GD total
#*****************************************************************************

# calcular GD total 	
df_B$ei_total_gd_proj <- rowSums(df_B[, c("ei_res_gd_proj", "ei_ind_gd_proj", "ei_com_gd_proj", "ei_rur_gd_proj", "ei_ppu_gd_proj",	
                                          "ei_spu_gd_proj", "ei_ipu_gd_proj", "ei_pro_gd_proj")], na.rm = TRUE)	
	
# Comparar 	
compare_multiple_columns_plot(df_B, "Data", c("ei_res_gd_proj", "ei_ind_gd_proj", "ei_com_gd_proj", "ei_rur_gd_proj", "ei_ppu_gd_proj",	
                                              "ei_spu_gd_proj", "ei_ipu_gd_proj", "ei_pro_gd_proj", "ei_total_gd_proj"))	
	
# Comparar gd com convencional	
compare_multiple_columns_plot(df_B, "Data", c("ei_total_gd_proj", "ei_total_proj"))	
	
# Comparar gd com convencional	
compare_multiple_columns_plot(df_B, "Data", c("ei_total_gd_proj", "ei_total_gd", "diff_ecf_total"))	

#-------

##############################################################################	
### Unir projeções e salvar
##############################################################################	
#-------	
  
# selecionar colunas projetadas	
Col_originais <- c("ei_total", 
                   "nc_total", "ec_total", "ef_total", 
                   "nc_res",   "ec_res",   "ef_res", 
                   "nc_ind",   "ec_ind",   "ef_ind", 	
                   "nc_com",   "ec_com",   "ef_com", 
                   "nc_rur",   "ec_rur",   "ef_rur", 
                   "nc_spu",   "ec_spu",   "ef_spu", 
                   "nc_ipu",   "ec_ipu",   "ef_ipu", 
                   "nc_ppu",   "ec_ppu",   "ef_ppu", 
                   "nc_pro",   "ec_pro",   "ef_pro", 
                   "nc_total_gd", "ei_total_gd", "ei_res_gd",	"ei_ind_gd", "ei_com_gd", "ei_rur_gd",
                   "ei_ppu_gd", "ei_ipu_gd",  "ei_pro_gd", "ei_spu_gd", "ei_pro_gd")	
	
                   	
Col_projetadas <- paste0(Col_originais, "_proj")	
Col_originais <- c("Data", "Sigla_UF", Col_originais)	
Col_projetadas <- c("Data", "Sigla_UF", Col_projetadas)	
	
# Gerar dataframe das projeções	
df_B_proj <- df_B %>% 	
  filter(Data >= as.Date("2024-03-01")) %>%	
  dplyr::select(all_of(Col_projetadas)) %>% 	
  rename_with(~ gsub("_proj$", "", .x), .cols = everything())	

# # Gerar dataframe com o Realizado	
# df_B_realizado <- df_B %>% 	
#   filter(Data <= as.Date("2023-12-01")) %>%	
#   dplyr::select(all_of(Col_originais))	
# 	
# # Append	
# df_B_proj_ok <- rbind(df_B_realizado, df_B_proj)	
	
# Adicionar sufixo	
df_B_proj_ML <- df_B_proj %>%	
  rename_with(~ paste0(.x, "_ML"), .cols = everything()) %>%	
  rename(Data = Data_ML, Sigla_UF = Sigla_UF_ML) 

# salvar	
write_rds(df_B_proj_ML, "C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/ATIVIDADES/3 - Projecoes_mercado/Projecoes_series_eqt/MA/projecoes/df_previsoes_B_ML_MA_mar-2024.RDS")	

# 
# #********************************************************************
# # Verificação visual do realizado e projetado
# #********************************************************************
# # Gerar dataframe para plot
# df_plot <-left_join(df_B_proj_ML, df_B, by = join_by(Data, Sigla_UF)) 
# 
# # plotar (Residencial)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_res", "nc_res_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_res", "ec_res_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_res", "ef_res_ML"))
# 
# # plotar (Industrial)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_ind", "nc_ind_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_ind", "ec_ind_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_ind", "ef_ind_ML"))
# 
# # plotar (Comercial)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_com", "nc_com_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_com", "ec_com_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_com", "ef_com_ML"))
# 
# # plotar (Rural)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_rur", "nc_rur_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_rur", "ec_rur_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_rur", "ef_rur_ML"))
# 
# # plotar (Poder Publico)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_ppu", "nc_ppu_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_ppu", "ec_ppu_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_ppu", "ef_ppu_ML"))
# 
# # plotar (Serviço Publico)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_spu", "nc_spu_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_spu", "ec_spu_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_spu", "ef_spu_ML"))
# 
# # plotar (Iluminaçao Publica)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_ipu", "nc_ipu_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_ipu", "ec_ipu_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_ipu", "ef_ipu_ML"))
# 
# # plotar (Proprio)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_pro", "nc_pro_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_pro", "ec_pro_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_pro", "ef_pro_ML"))
# 
# # plotar (Total)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_total", "nc_total_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ec_total", "ec_total_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ef_total", "ef_total_ML"))
# 
# # plotar (GD)
# compare_multiple_columns_plot(df_plot, "Data", c("nc_total_gd", "nc_total_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_res_gd",   "ei_res_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_ind_gd",   "ei_ind_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_com_gd",   "ei_com_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_rur_gd",   "ei_rur_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_ppu_gd",   "ei_ppu_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_ipu_gd",   "ei_ipu_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_spu_gd",   "ei_spu_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_pro_gd",   "ei_pro_gd_ML"))
# compare_multiple_columns_plot(df_plot, "Data", c("ei_total_gd", "ei_total_gd_ML"))

#-------

##############################################################################
##           Ajustar consumida/faturada e gerar cenários 
##############################################################################
#--------

# Limpar tudo	
rm(list=ls())	
cat("\014")	

# Limpar memória	
gc()	

# Install & load packages	
if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, tidyr, data.table, lubridate, funModeling, stargazer, openxlsx, 	
       readxl, imputeTS, plotly, autoTS,  zoo, arrow, Metrics, caret, purrr, autokeras) 	
# Não adicionar package MASS da erro no select

# Impedir notação cientifica 	
options(scipen=999)	

# Sigla uf
UF <- "MA"

# carregar funções	
source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/script - functions forcast new.R")

# Carregar listas de modelos caret
source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/lista de modelos caret.R")	

# Importar dados	
load("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/DADOS/1_Dataframes_gerados/Dados_projecoes_pronto_jan_2024_com_Clima.RData")	

# Importar projeções 
df_B_P <- readRDS("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/ATIVIDADES/3 - Projecoes_mercado/Projecoes_series_eqt/MA/projecoes/df_previsoes_B_ML_MA_mar-2024.RDS")


#*****************************************************************************
#    Renomear as faturadas corrigidas
#*****************************************************************************

# Definir a função de simplificação
simplificar_colunas <- function(df) {
  df %>% 
    rename_with(.fn = ~ gsub("_ML$", "_B", .x), .cols = ends_with("_ML"))
}

# Aplicar a função aos dataframes
df_B_P <- simplificar_colunas(df_B_P)


#*****************************************************************************
#     Gerar cenários com base no PIB otimista e pessimista
#*****************************************************************************

# Selecionar pibs
PIB_O <- df_O %>% filter(Sigla_UF==UF) %>% select(Data, Sigla_UF, pib_total_O = pib_total)
PIB_B <- df_B %>% filter(Sigla_UF==UF) %>%  select(Data, Sigla_UF, pib_total_B = pib_total)
PIB_P <- df_P %>% filter(Sigla_UF==UF) %>%  select(Data, Sigla_UF, pib_total_P = pib_total)

# Join
PIB <- list(PIB_O, PIB_B, PIB_P) %>% reduce(left_join, by = c("Data", "Sigla_UF"))

# Calcular a taxa de crescimento (otimista) e decrescimento (pessimista) em relação ao cenário base
PIB <- PIB %>%
  mutate(
    # Calcular a diferença absoluta entre os cenários e o cenário base
    diff_pib_total_B.O = pib_total_O - pib_total_B,
    diff_pib_total_B.P = pib_total_P - pib_total_B,
    # Calcular o percentual de diferença em relação ao cenário base
    perc_diff_pib_total_B.O = 1 + (diff_pib_total_B.O / pib_total_B),
    perc_diff_pib_total_B.P = 1 + (diff_pib_total_B.P / pib_total_B),
    # Aplicar a substituição condicional
    perc_diff_pib_total_B.O = if_else(Data <= as.Date("2024-04-01"), 1, perc_diff_pib_total_B.O),
    perc_diff_pib_total_B.P = if_else(Data <= as.Date("2024-04-01"), 1, perc_diff_pib_total_B.P)
  ) %>% 
  select(Data, Sigla_UF, perc_diff_pib_total_B.O, perc_diff_pib_total_B.P)

# Realizar join
df_B_P_PIB <- left_join(df_B_P, PIB, by = join_by(Data, Sigla_UF))

# tirar o serilhado das series de diferença de cenários
df_B_P_PIB <- ajuste_pos_projecao(df_B_P_PIB, "perc_diff_pib_total_B.O", "perc_diff_pib_total_B.O", fator_tend = 1, fator_saz= 0, fator_res=0)
df_B_P_PIB <- ajuste_pos_projecao(df_B_P_PIB, "perc_diff_pib_total_B.P", "perc_diff_pib_total_B.P", fator_tend = 1, fator_saz= 0, fator_res=0)

# Função geradora de cenários
Gerar_cenarios <- function(df) {
  df <- df %>% 
    mutate(#################### Otimista ###################     ################### Pessimista ################### 
           ### Consumida
           ec_total_O = ec_total_B * perc_diff_pib_total_B.O,    ec_total_P = ec_total_B * perc_diff_pib_total_B.P,
           ec_res_O   = ec_res_B   * perc_diff_pib_total_B.O,    ec_res_P   = ec_res_B   * perc_diff_pib_total_B.P,
           ec_ind_O   = ec_ind_B   * perc_diff_pib_total_B.O,    ec_ind_P   = ec_ind_B   * perc_diff_pib_total_B.P,
           ec_com_O   = ec_com_B   * perc_diff_pib_total_B.O,    ec_com_P   = ec_com_B   * perc_diff_pib_total_B.P,
           ec_rur_O   = ec_rur_B   * perc_diff_pib_total_B.O,    ec_rur_P   = ec_rur_B   * perc_diff_pib_total_B.P,
           ec_spu_O   = ec_spu_B   * perc_diff_pib_total_B.O,    ec_spu_P   = ec_spu_B   * perc_diff_pib_total_B.P,
           ec_ipu_O   = ec_ipu_B   * perc_diff_pib_total_B.O,    ec_ipu_P   = ec_ipu_B   * perc_diff_pib_total_B.P,
           ec_ppu_O   = ec_ppu_B   * perc_diff_pib_total_B.O,    ec_ppu_P   = ec_ppu_B   * perc_diff_pib_total_B.P,
           ec_pro_O   = ec_pro_B   * perc_diff_pib_total_B.O,    ec_pro_P   = ec_pro_B   * perc_diff_pib_total_B.P,
           ### Faturada
           ef_total_O = ef_total_B * perc_diff_pib_total_B.O,    ef_total_P = ef_total_B * perc_diff_pib_total_B.P,
           ef_res_O   = ef_res_B   * perc_diff_pib_total_B.O,    ef_res_P   = ef_res_B   * perc_diff_pib_total_B.P,
           ef_ind_O   = ef_ind_B   * perc_diff_pib_total_B.O,    ef_ind_P   = ef_ind_B   * perc_diff_pib_total_B.P,
           ef_com_O   = ef_com_B   * perc_diff_pib_total_B.O,    ef_com_P   = ef_com_B   * perc_diff_pib_total_B.P,
           ef_rur_O   = ef_rur_B   * perc_diff_pib_total_B.O,    ef_rur_P   = ef_rur_B   * perc_diff_pib_total_B.P,
           ef_spu_O   = ef_spu_B   * perc_diff_pib_total_B.O,    ef_spu_P   = ef_spu_B   * perc_diff_pib_total_B.P,
           ef_ipu_O   = ef_ipu_B   * perc_diff_pib_total_B.O,    ef_ipu_P   = ef_ipu_B   * perc_diff_pib_total_B.P,
           ef_ppu_O   = ef_ppu_B   * perc_diff_pib_total_B.O,    ef_ppu_P   = ef_ppu_B   * perc_diff_pib_total_B.P,
           ef_pro_O   = ef_pro_B   * perc_diff_pib_total_B.O,    ef_pro_P   = ef_pro_B   * perc_diff_pib_total_B.P,
           ### Injetada total
           ei_total_O = ei_total_B * perc_diff_pib_total_B.O,    ei_total_P = ei_total_B * perc_diff_pib_total_B.P,
           ### consumidores
           nc_total_O = nc_total_B * perc_diff_pib_total_B.O,    nc_total_P  = nc_total_B * perc_diff_pib_total_B.P,
           nc_res_O   = nc_res_B   * perc_diff_pib_total_B.O,    nc_res_P    = nc_res_B   * perc_diff_pib_total_B.P,
           nc_ind_O   = nc_ind_B   * perc_diff_pib_total_B.O,    nc_ind_P    = nc_ind_B   * perc_diff_pib_total_B.P,
           nc_com_O   = nc_com_B   * perc_diff_pib_total_B.O,    nc_com_P    = nc_com_B   * perc_diff_pib_total_B.P,
           nc_rur_O   = nc_rur_B   * perc_diff_pib_total_B.O,    nc_rur_P    = nc_rur_B   * perc_diff_pib_total_B.P,
           nc_spu_O   = nc_spu_B   * perc_diff_pib_total_B.O,    nc_spu_P    = nc_spu_B   * perc_diff_pib_total_B.P,
           nc_ipu_O   = nc_ipu_B   * perc_diff_pib_total_B.O,    nc_ipu_P    = nc_ipu_B   * perc_diff_pib_total_B.P,
           nc_ppu_O   = nc_ppu_B   * perc_diff_pib_total_B.O,    nc_ppu_P    = nc_ppu_B   * perc_diff_pib_total_B.P,
           nc_pro_O   = nc_pro_B   * perc_diff_pib_total_B.O,    nc_pro_P    = nc_pro_B   * perc_diff_pib_total_B.P,
           # Geração distribuida GD (Logica contrária)
           nc_total_gd_O = nc_total_gd_B * perc_diff_pib_total_B.P,    nc_total_gd_P = nc_total_gd_B * perc_diff_pib_total_B.O,
           ei_total_gd_O = ei_total_gd_B * perc_diff_pib_total_B.P,    ei_total_gd_P = ei_total_gd_B * perc_diff_pib_total_B.O,
           ei_res_gd_O   = ei_res_gd_B   * perc_diff_pib_total_B.P,    ei_res_gd_P   = ei_res_gd_B   * perc_diff_pib_total_B.O,
           ei_ind_gd_O   = ei_ind_gd_B   * perc_diff_pib_total_B.P,    ei_ind_gd_P   = ei_ind_gd_B   * perc_diff_pib_total_B.O,
           ei_com_gd_O   = ei_com_gd_B   * perc_diff_pib_total_B.P,    ei_com_gd_P   = ei_com_gd_B   * perc_diff_pib_total_B.O,
           ei_rur_gd_O   = ei_rur_gd_B   * perc_diff_pib_total_B.P,    ei_rur_gd_P   = ei_rur_gd_B   * perc_diff_pib_total_B.O,
           ei_ppu_gd_O   = ei_ppu_gd_B   * perc_diff_pib_total_B.P,    ei_ppu_gd_P   = ei_ppu_gd_B   * perc_diff_pib_total_B.O, 
           ei_ipu_gd_O   = ei_ipu_gd_B   * perc_diff_pib_total_B.P,    ei_ipu_gd_P   = ei_ipu_gd_B   * perc_diff_pib_total_B.O,
           ei_spu_gd_O   = ei_spu_gd_B   * perc_diff_pib_total_B.P,    ei_spu_gd_P   = ei_spu_gd_B   * perc_diff_pib_total_B.O,
           ei_pro_gd_O   = ei_pro_gd_B   * perc_diff_pib_total_B.P,    ei_pro_gd_P   = ei_pro_gd_B   * perc_diff_pib_total_B.O,
    )
  return(df)
} 

# Gerar cenários
df_ok <- Gerar_cenarios(df_B_P_PIB)

#*****************************************************************************
# Plotar cenários 
#*****************************************************************************
# Consumidores
compare_multiple_columns_plot(df_ok, "Data", c("nc_total_P", "nc_total_O", "nc_total_B"))
compare_multiple_columns_plot(df_ok, "Data", c("nc_res_P",   "nc_res_O",   "nc_res_B"))
compare_multiple_columns_plot(df_ok, "Data", c("nc_ind_P",   "nc_ind_O",   "nc_ind_B"))
compare_multiple_columns_plot(df_ok, "Data", c("nc_com_P",   "nc_com_O",   "nc_com_B"))
compare_multiple_columns_plot(df_ok, "Data", c("nc_rur_P",   "nc_rur_O",   "nc_rur_B"))
compare_multiple_columns_plot(df_ok, "Data", c("nc_ppu_P",   "nc_ppu_O",   "nc_ppu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("nc_ipu_P",   "nc_ipu_O",   "nc_ipu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("nc_spu_P",   "nc_spu_O",   "nc_spu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("nc_pro_P",   "nc_pro_O",   "nc_pro_B"))

# Consumida
compare_multiple_columns_plot(df_ok, "Data", c("ec_total_P", "ec_total_O", "ec_total_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ec_res_P",   "ec_res_O",   "ec_res_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ec_ind_P",   "ec_ind_O",   "ec_ind_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ec_com_P",   "ec_com_O",   "ec_com_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ec_rur_P",   "ec_rur_O",   "ec_rur_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ec_ppu_P",   "ec_ppu_O",   "ec_ppu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ec_ipu_P",   "ec_ipu_O",   "ec_ipu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ec_spu_P",   "ec_spu_O",   "ec_spu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ec_pro_P",   "ec_pro_O",   "ec_pro_B"))

# Faturada
compare_multiple_columns_plot(df_ok, "Data", c("ef_total_P", "ef_total_O", "ef_total_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ef_res_P",   "ef_res_O",   "ef_res_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ef_ind_P",   "ef_ind_O",   "ef_ind_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ef_com_P",   "ef_com_O",   "ef_com_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ef_rur_P",   "ef_rur_O",   "ef_rur_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ef_ppu_P",   "ef_ppu_O",   "ef_ppu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ef_ipu_P",   "ef_ipu_O",   "ef_ipu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ef_spu_P",   "ef_spu_O",   "ef_spu_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ef_pro_P",   "ef_pro_O",   "ef_pro_B"))

# GD
compare_multiple_columns_plot(df_ok, "Data", c("nc_total_gd_P", "nc_total_gd_O", "nc_total_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_total_gd_P", "ei_total_gd_O", "ei_total_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_res_gd_P",   "ei_res_gd_O",   "ei_res_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_ind_gd_P",   "ei_ind_gd_O",   "ei_ind_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_com_gd_P",   "ei_com_gd_O",   "ei_com_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_rur_gd_P",   "ei_rur_gd_O",   "ei_rur_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_ppu_gd_P",   "ei_ppu_gd_O",   "ei_ppu_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_ipu_gd_P",   "ei_ipu_gd_O",   "ei_ipu_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_spu_gd_P",   "ei_spu_gd_O",   "ei_spu_gd_B"))
compare_multiple_columns_plot(df_ok, "Data", c("ei_pro_gd_P",   "ei_pro_gd_O",   "ei_pro_gd_B"))

#*****************************************************************************
# Ordenar colunas
#*****************************************************************************

ord_cols <- c("Data", 
              "nc_total_B", "nc_total_O", "nc_total_P", "nc_res_B", "nc_res_O", "nc_res_P", "nc_ind_B", "nc_ind_O", "nc_ind_P",
              "nc_com_B", "nc_com_O", "nc_com_P", "nc_rur_B", "nc_rur_O", "nc_rur_P", "nc_spu_B", "nc_spu_O", "nc_spu_P",
              "nc_ipu_B", "nc_ipu_O", "nc_ipu_P", "nc_ppu_B", "nc_ppu_O", "nc_ppu_P", "nc_pro_B", "nc_pro_O", "nc_pro_P",
              "ec_total_B", "ec_total_O", "ec_total_P",  "ec_res_B", "ec_res_O", "ec_res_P", "ec_ind_B", "ec_ind_O", "ec_ind_P",
              "ec_com_B", "ec_com_O", "ec_com_P", "ec_rur_B", "ec_rur_O", "ec_rur_P", "ec_spu_B", "ec_spu_O", "ec_spu_P",
              "ec_ipu_B", "ec_ipu_O", "ec_ipu_P", "ec_ppu_B", "ec_ppu_O", "ec_ppu_P", "ec_pro_B", "ec_pro_O", "ec_pro_P",
              "ef_total_B", "ef_total_O", "ef_total_P", "ef_res_B", "ef_res_O", "ef_res_P", "ef_ind_B", "ef_ind_O", "ef_ind_P",
              "ef_com_B", "ef_com_O", "ef_com_P", "ef_rur_B", "ef_rur_O", "ef_rur_P", "ef_spu_B", "ef_spu_O", "ef_spu_P",
              "ef_ipu_B", "ef_ipu_O", "ef_ipu_P", "ef_ppu_B", "ef_ppu_O", "ef_ppu_P", "ef_pro_B", "ef_pro_O", "ef_pro_P",
              "ei_total_B", "ei_total_O", "ei_total_P", "nc_total_gd_B", "nc_total_gd_O", "nc_total_gd_P",
              "ei_res_gd_B", "ei_res_gd_O", "ei_res_gd_P", "ei_ind_gd_B", "ei_ind_gd_O", "ei_ind_gd_P",
              "ei_com_gd_B", "ei_com_gd_O", "ei_com_gd_P", "ei_rur_gd_B", "ei_rur_gd_O", "ei_rur_gd_P",
              "ei_ppu_gd_B", "ei_ppu_gd_O", "ei_ppu_gd_P", "ei_ipu_gd_B", "ei_ipu_gd_O", "ei_ipu_gd_P",
              "ei_spu_gd_B", "ei_spu_gd_O", "ei_spu_gd_P", "ei_pro_gd_B",  "ei_pro_gd_O", "ei_pro_gd_P",
              "ei_total_gd_B", "ei_total_gd_O", "ei_total_gd_P" 
              )

df_ok <- df_ok %>% dplyr::select(all_of(ord_cols))


#*****************************************************************************
#  Salvar cenários
#*****************************************************************************

# salvar	
write_rds(df_ok, "C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/ATIVIDADES/3 - Projecoes_mercado/Projecoes_series_eqt/MA/projecoes/df_previsoes_B_ML_MA_mar-2024_ajustadas.RDS")	

#--------

##############################################################################
##                 Exportar projeções para excel padrão
##############################################################################
#--------

# Limpar tudo	
rm(list=ls())	
cat("\014")	

# Limpar memória	
gc()	

# Install & load packages	
if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, tidyr, data.table, lubridate, funModeling, stargazer, openxlsx, 	
       readxl, imputeTS, plotly, autoTS,  zoo, arrow, Metrics, caret, purrr, autokeras) 	
# Não adicionar package MASS da erro no select

# Impedir notação cientifica 	
options(scipen=999)	

# Sigla uf
UF <- "MA"

# carregar funções	
source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/script - functions forcast new.R")

# Carregar listas de modelos caret
source("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/FUNCOES_R/functions_forcast_silvio/lista de modelos caret.R")	

# Importar dados	
load("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/DADOS/1_Dataframes_gerados/Dados_projecoes_pronto_jan_2024_com_Clima.RData")	

# Importar projeções 
df <- readRDS("C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/ATIVIDADES/3 - Projecoes_mercado/Projecoes_series_eqt/MA/projecoes/df_previsoes_B_ML_MA_mar-2024_ajustadas.RDS")

#******************************************************************
# Função para gerar lista com dataframes que serão exportados
#******************************************************************
gerar_visoes_cenarios <- function(df) {
    
  # Gerar lista vazia
  list_proj <- list()
  #_______________________________________
  # Visão consumida (Cenários)
  #_______________________________________
  # Colunas
  col_CB <- c("Data", "ec_res_B", "ec_ind_B", "ec_com_B", "ec_rur_B", "ec_ppu_B", "ec_ipu_B", "ec_spu_B", "ec_pro_B", "ec_total_B")
  col_CO <- c("Data", "ec_res_O", "ec_ind_O", "ec_com_O", "ec_rur_O", "ec_ppu_O", "ec_ipu_O", "ec_spu_O", "ec_pro_O", "ec_total_O")
  col_CP <- c("Data", "ec_res_P", "ec_ind_P", "ec_com_P", "ec_rur_P", "ec_ppu_P", "ec_ipu_P", "ec_spu_P", "ec_pro_P", "ec_total_P")
  
  # Gerar dataframes 
  list_proj$df_visao_consumida_B <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_CB)) 
  list_proj$df_visao_consumida_O <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_CO)) 
  list_proj$df_visao_consumida_P <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_CP)) 
  
  #_______________________________________
  # Visão faturada (Cenários)
  #_______________________________________
  # Colunas
  col_FB <- c("Data", "ef_res_B", "ef_ind_B", "ef_com_B", "ef_rur_B", "ef_ppu_B", "ef_ipu_B", "ef_spu_B", "ef_pro_B", "ef_total_B")
  col_FO <- c("Data", "ef_res_O", "ef_ind_O", "ef_com_O", "ef_rur_O", "ef_ppu_O", "ef_ipu_O", "ef_spu_O", "ef_pro_O", "ef_total_O")
  col_FP <- c("Data", "ef_res_P", "ef_ind_P", "ef_com_P", "ef_rur_P", "ef_ppu_P", "ef_ipu_P", "ef_spu_P", "ef_pro_P", "ef_total_P")
  
  # Gerar dataframes 
  list_proj$df_visao_faturada_B <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_FB)) 
  list_proj$df_visao_faturada_O <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_FO)) 
  list_proj$df_visao_faturada_P <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_FP)) 
  
  #_______________________________________
  # Nº Consumidores (Cenários)
  #_______________________________________
  # Colunas
  col_NCB <- c("Data", "nc_res_B", "nc_ind_B", "nc_com_B", "nc_rur_B", "nc_ppu_B", "nc_ipu_B", "nc_spu_B", "nc_pro_B", "nc_total_B")
  col_NCO <- c("Data", "nc_res_O", "nc_ind_O", "nc_com_O", "nc_rur_O", "nc_ppu_O", "nc_ipu_O", "nc_spu_O", "nc_pro_O", "nc_total_O")
  col_NCP <- c("Data", "nc_res_P", "nc_ind_P", "nc_com_P", "nc_rur_P", "nc_ppu_P", "nc_ipu_P", "nc_spu_P", "nc_pro_P", "nc_total_P")
  
  # Gerar dataframes 
  list_proj$df_consumidores_B <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_NCB)) 
  list_proj$df_consumidores_O <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_NCO)) 
  list_proj$df_consumidores_P <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_NCP)) 
  
  #_______________________________________
  # Consumo Médio (Cenários)
  #_______________________________________
  list_proj$df_consumo_medio_B <- df %>% 
    dplyr::mutate(ec_res_medio_B = ec_res_B / nc_res_B,  
           ec_ind_medio_B = ec_ind_B / nc_ind_B,
           ec_com_medio_B = ec_com_B / nc_com_B, 
           ec_rur_medio_B = ec_rur_B / nc_rur_B,
           ec_ppu_medio_B = ec_ppu_B / nc_ppu_B, 
           ec_ipu_medio_B = ec_ipu_B / nc_ipu_B,
           ec_spu_medio_B = ec_spu_B / nc_spu_B,  
           ec_pro_medio_B = ec_pro_B / nc_pro_B,
           ec_total_medio_B  = ec_total_B / nc_total_B) %>% 
    dplyr::mutate(Ano = lubridate::year(Data))  %>% 
    dplyr::select("Ano", "Data", "ec_res_medio_B", "ec_ind_medio_B", "ec_com_medio_B", "ec_rur_medio_B",
           "ec_ppu_medio_B", "ec_ipu_medio_B", "ec_spu_medio_B", "ec_pro_medio_B", "ec_total_medio_B")
  
  list_proj$df_consumo_medio_O <- df %>% 
    dplyr::mutate(ec_res_medio_O = ec_res_O / nc_res_O,  
           ec_ind_medio_O = ec_ind_O / nc_ind_O,
           ec_com_medio_O = ec_com_O / nc_com_O, 
           ec_rur_medio_O = ec_rur_O / nc_rur_O,
           ec_ppu_medio_O = ec_ppu_O / nc_ppu_O, 
           ec_ipu_medio_O = ec_ipu_O / nc_ipu_O,
           ec_spu_medio_O = ec_spu_O / nc_spu_O,  
           ec_pro_medio_O = ec_pro_O / nc_pro_O,
           ec_total_medio_O  = ec_total_O / nc_total_O) %>% 
    dplyr::mutate(Ano = lubridate::year(Data))  %>% 
    dplyr::select("Ano", "Data", "ec_res_medio_O", "ec_ind_medio_O", "ec_com_medio_O", "ec_rur_medio_O",
           "ec_ppu_medio_O", "ec_ipu_medio_O", "ec_spu_medio_O", "ec_pro_medio_O", "ec_total_medio_O")
  
  list_proj$df_consumo_medio_P <- df %>% 
    dplyr::mutate(ec_res_medio_P = ec_res_P / nc_res_P,  
           ec_ind_medio_P = ec_ind_P / nc_ind_P,
           ec_com_medio_P = ec_com_P / nc_com_P, 
           ec_rur_medio_P = ec_rur_P / nc_rur_P,
           ec_ppu_medio_P = ec_ppu_P / nc_ppu_P, 
           ec_ipu_medio_P = ec_ipu_P / nc_ipu_P,
           ec_spu_medio_P = ec_spu_P / nc_spu_P,  
           ec_pro_medio_P = ec_pro_P / nc_pro_P,
           ec_total_medio_P  = ec_total_P / nc_total_P) %>% 
    dplyr::mutate(Ano = lubridate::year(Data))  %>% 
    dplyr::select("Ano", "Data", "ec_res_medio_P", "ec_ind_medio_P", "ec_com_medio_P", "ec_rur_medio_P",
           "ec_ppu_medio_P", "ec_ipu_medio_P", "ec_spu_medio_P", "ec_pro_medio_P", "ec_total_medio_P")
  
  #_______________________________________
  # Injetada (Cenários)
  #_______________________________________
  list_proj$df_injetada <- df %>%
    dplyr::mutate(Ano = lubridate::year(Data)) %>% 
    dplyr::select("Ano", "Data", "ei_total_B", "ei_total_P", "ei_total_O")
  
  #_______________________________________
  # Geração Distribuida (Cenários)
  #_______________________________________
  # Colunas
  col_GDB <- c("Data", "ei_res_gd_B", "ei_ind_gd_B", "ei_com_gd_B", "ei_rur_gd_B", "ei_ppu_gd_B", 
               "ei_ipu_gd_B", "ei_spu_gd_B", "ei_pro_gd_B", "ei_total_gd_B", "ei_total_gd_B", "nc_total_gd_B")
  
  col_GDO <- c("Data", "ei_res_gd_O", "ei_ind_gd_O", "ei_com_gd_O", "ei_rur_gd_O", "ei_ppu_gd_O", 
               "ei_ipu_gd_O", "ei_spu_gd_O", "ei_pro_gd_O", "ei_total_gd_O", "ei_total_gd_O", "nc_total_gd_O")
  
  col_GDP <- c("Data", "ei_res_gd_P", "ei_ind_gd_P", "ei_com_gd_P", "ei_rur_gd_P", "ei_ppu_gd_P", 
               "ei_ipu_gd_P", "ei_spu_gd_P", "ei_pro_gd_P", "ei_total_gd_P", "ei_total_gd_P", "nc_total_gd_P")
  
  # Gerar dataframes 
  list_proj$df_GD_B <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_GDB)) 
  list_proj$df_GD_O <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_GDO)) 
  list_proj$df_GD_P <- df %>% dplyr::mutate(Ano = lubridate::year(Data)) %>% dplyr::select(Ano, Data, all_of(col_GDP)) 
  
  # return
  return(list_proj)
}

# Gerar listas com dataframes prontos para exportar
list_df <- gerar_visoes_cenarios(df)

#******************************************************************
# Função para popular excel padrão
#******************************************************************
exportar_excel_padrao <- function(list, input_file, output_file){
  
  # Carregar a workbook existente
  wb <- loadWorkbook(input_file)
  
  #_______________________________________
  # Visão Consumida
  #_______________________________________
  
  # Visão consumida - Canário Base
  writeData(wb, sheet = "Visão Consumida (Base)", x = list$df_visao_consumida_B, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # Visão consumida - Canário Pessimista
  writeData(wb, sheet = "Visão Consumida (Pessimista)", x = list$df_visao_consumida_P, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # Visão consumida - Canário Otimista
  writeData(wb, sheet = "Visão Consumida (Otimista)", x = list$df_visao_consumida_O, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  #_______________________________________
  # Visão Faturada
  #_______________________________________
  
  # Visão Faturada - Canário Base
  writeData(wb, sheet = "Visão Faturada (Base)", x = list$df_visao_faturada_B, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # Visão Faturada - Canário Pessimista
  writeData(wb, sheet = "Visão Faturada (Pessimista)", x = list$df_visao_faturada_P, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # Visão Faturada - Canário Otimista
  writeData(wb, sheet = "Visão Faturada (Otimista)", x = list$df_visao_faturada_O, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  #_______________________________________
  # N° Consumidores
  #_______________________________________
  
  # N° Consumidores - Canário Base
  writeData(wb, sheet = "Consumidores (Base)", x = list$df_consumidores_B, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # N° Consumidores - Canário Pessimista
  writeData(wb, sheet = "Consumidores (Pessimista)", x = list$df_consumidores_P, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # N° Consumidores - Canário Otimista
  writeData(wb, sheet = "Consumidores (Otimista)", x = list$df_consumidores_O, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  #_______________________________________
  # Consumo Médio
  #_______________________________________
  
  # Consumo Médio - Canário Base
  writeData(wb, sheet = "Consumo Médio (Base)", x = list$df_consumo_medio_B, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # Consumo Médio - Canário Pessimista
  writeData(wb, sheet = "Consumo Médio (Pessimista)", x = list$df_consumo_medio_P, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # Consumo Médio - Canário Otimista
  writeData(wb, sheet = "Consumo Médio (Otimista)", x = list$df_consumo_medio_O, startCol = 25, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  #_______________________________________
  # Geração Distribuída (GD)
  #_______________________________________
  
  # Geração Distribuída - Canário Base
  writeData(wb, sheet = "GD (Base)", x = list$df_GD_B, startCol = 27, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # Geração Distribuída - Canário Pessimista
  writeData(wb, sheet = "GD (Pessimista)", x = list$df_GD_P, startCol = 27, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  # Geração Distribuída - Canário Otimista
  writeData(wb, sheet = "GD (Otimista)", x = list$df_GD_O, startCol = 27, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  #_______________________________________
  # Energia Injetada
  #_______________________________________
  
  # Consumo Médio - Canário Base
  writeData(wb, sheet = "Energia Injetada", x = list$df_injetada, startCol = 13, startRow = 235, colNames = FALSE)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
}

#******************************************************************
# Carregar excel padrão para popular
#******************************************************************
input_file <- "C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/ATIVIDADES/3 - Projecoes_mercado/Projecoes_series_eqt/MA/projecoes/EQTL_MA_Projeções_VAZIO.xlsx"
#******************************************************************
# Exportar Excel
#******************************************************************
output_file <- "C:/Users/u1011414/OneDrive - GRUPO EQUATORIAL ENERGIA/ATIVIDADES/3 - Projecoes_mercado/Projecoes_series_eqt/MA/projecoes/EQTL_Projeções_MA_ver_2.xlsx"
exportar_excel_padrao(list_df, input_file, output_file)

#--------
