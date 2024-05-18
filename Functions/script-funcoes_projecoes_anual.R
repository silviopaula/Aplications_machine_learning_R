train_evaluate_multiple_models_ML_annual <- function(df, 
                                                     lista_modelos, 
                                                     explicativas, 
                                                     var_target, 
                                                     ultimo_ano_realizado,
                                                     ano_inicio = NULL,
                                                     n_fora_modelo, 
                                                     n_test_caret) {
  # Início do contador de tempo
  start_time <- Sys.time()
  
  # Instalar e/ou carregar pacotes
  if (!require(pacman)) {
    install.packages("pacman")
  }
  pacman::p_load(tidyverse, caret, ggplot2, plotly, Metrics)
  
  # Definir uma semente global
  set.seed(2024)
  
  # Verifica se a variável dependente está no DataFrame
  verificar_variaveis <- function(df, variaveis, tipo) {
    if (!all(variaveis %in% names(df))) {
      variaveis_faltantes <- variaveis[!variaveis %in% names(df)]
      stop(paste("As seguintes variáveis", tipo, "não estão presentes no DataFrame:", paste(variaveis_faltantes, collapse = ", "), "."))
    }
  }
  
  # Verifica se a variável dependente está no DataFrame
  verificar_variaveis(df, var_target, "dependente")
  
  # Verifica se todas as variáveis explicativas estão no DataFrame
  verificar_variaveis(df, explicativas, "explicativas")
  
  # Ajustar ano_inicio se não definido
  if (is.null(ano_inicio)) {
    ano_inicio <- min(df$Ano)
  }
  
  # Ajustar ano_teste com base em n_fora_modelo
  ultimo_ano_realizado <- as.numeric(ultimo_ano_realizado)
  ano_teste <- ultimo_ano_realizado - n_fora_modelo
  
  # Função para filtrar e preparar os dados
  preparar_dados <- function(df, ano_inicio, ano_teste, explicativas, var_target) {
    df %>%
      dplyr::select(Ano, all_of(var_target), all_of(explicativas)) %>%
      dplyr::filter(Ano >= ano_inicio & Ano <= ano_teste) %>%
      drop_na()
  }
  
  df_train <- preparar_dados(df, ano_inicio, ano_teste, explicativas, var_target)
  
  df_test <- df %>%
    dplyr::select(Ano, all_of(var_target), all_of(explicativas)) %>%
    dplyr::mutate(across(where(is.numeric), ~if_else(is.na(.), 0, .)))
  
  # Cálculo de intervalos de anos
  calcular_intervalos <- function(ultimo_ano_realizado, df_train, n_test_caret) {
    ano_inicio_janela <- ultimo_ano_realizado - n_test_caret
    ano_inicio_df <- min(df_train$Ano)
    ano_final_df <- max(df_train$Ano)
    
    diferenca_anos <- ano_inicio_janela - ano_inicio_df
    diferenca_anos_final <- ano_final_df - ano_inicio_janela
    
    list(diferenca_anos, diferenca_anos_final)
  }
  
  intervalos <- calcular_intervalos(ultimo_ano_realizado, df_train, n_test_caret)
  diferenca_anos <- intervalos[[1]]
  diferenca_anos_final <- intervalos[[2]]
  
  # Treinamento dos modelos
  treinar_modelos <- function(formula, df_train, lista_modelos, train_control) {
    resultados_previsoes <- list()
    modelos_treinados <- list()
    mensagens_erro <- list()
    
    for (nome_modelo in names(lista_modelos)) {
      cat("Treinando modelo:", nome_modelo, "\n")
      tryCatch({
        suppressMessages({
          suppressWarnings({
            modelo_atual <- lista_modelos[[nome_modelo]]
            modelo_treinado <- train(
              formula, 
              data = df_train, 
              method = modelo_atual$method, 
              trControl = train_control, 
              preProcess = modelo_atual$preProcess, 
              tuneGrid = modelo_atual$tuneGrid
            )
            modelos_treinados[[nome_modelo]] <- modelo_treinado
            previsoes <- predict(modelo_treinado, newdata = df_test)
            resultados_previsoes[[nome_modelo]] <- previsoes
          })
        })
      }, error = function(e) {
        mensagens_erro[[nome_modelo]] <- e$message
      })
    }
    list(modelos_treinados, resultados_previsoes, mensagens_erro)
  }
  
  formula <- as.formula(paste(var_target, "~", paste(explicativas, collapse = " + ")))
  train_control <- trainControl(method = "timeslice", 
                                initialWindow = diferenca_anos, 
                                horizon = diferenca_anos_final, 
                                fixedWindow = TRUE, 
                                skip = 0)
  
  resultados <- treinar_modelos(formula, df_train, lista_modelos, train_control)
  modelos_treinados <- resultados[[1]]
  resultados_previsoes <- resultados[[2]]
  mensagens_erro <- resultados[[3]]
  
  # Adicionando previsões ao df_test
  for (nome_modelo in names(resultados_previsoes)) {
    nome_coluna_predicao <- paste0(var_target, "_", nome_modelo)
    df_test[[nome_coluna_predicao]] <- resultados_previsoes[[nome_modelo]]
  }
  
  # Cálculo de métricas
  calcular_metricas <- function(resultados_previsoes, df_test, var_target) {
    metricas_erro <- list()
    for (nome_modelo in names(resultados_previsoes)) {
      previsoes <- resultados_previsoes[[nome_modelo]]
      valores_reais <- df_test[[var_target]]
      rmse <- caret::RMSE(previsoes, valores_reais)
      mae <- Metrics::mae(previsoes, valores_reais)
      rsquared <- caret::R2(previsoes, valores_reais)
      mse <- Metrics::mse(previsoes, valores_reais)
      mape <- Metrics::mape(previsoes, valores_reais)
      metricas_erro[[nome_modelo]] <- c(RMSE = rmse, MAE = mae, R_Quadrado = rsquared, MSE = mse, MAPE = mape)
    }
    metricas_erro
  }
  
  metricas_erro <- calcular_metricas(resultados_previsoes, df_test, var_target)
  
  # Convertendo a lista de métricas em um dataframe
  df_metricas <- do.call(rbind, lapply(metricas_erro, function(x) t(data.frame(x))))
  rownames(df_metricas) <- names(metricas_erro)
  df_metricas <- as.data.frame(df_metricas)
  
  # Gerar dataframe com as previsões
  df_plot <- df_test %>%
    dplyr::mutate(!!var_target := if_else(Ano > ultimo_ano_realizado, NA_real_, !!sym(var_target)))
  
  df_projecoes <- df_plot %>% 
    dplyr::select(-all_of(explicativas))
  
  df_projecoes$Ano <- df$Ano
  
  names_ensemble_X <- setdiff(names(df_projecoes), c(var_target, "Ano"))
  
  # PLOTAR GRÁFICO 
  df_long <- df_projecoes %>%
    pivot_longer(cols = -Ano, names_to = "Model", values_to = "Value") %>%
    mutate(Model = gsub(paste0("^", var_target, "_"), "", Model)) %>% 
    arrange(Model)
  
  num_cores <- length(unique(df_long$Model))
  
  p_plotly <- plot_ly(data = df_long, x = ~Ano, y = ~Value, color = ~Model, colors = rainbow(num_cores), type = 'scatter', mode = 'lines') %>%
    layout(title = "Comparação dos modelos",
           xaxis = list(title = "Ano",
                        tickangle = 45,
                        tickformat = "%Y",
                        type = 'linear'),
           yaxis = list(title = "Projeções"),
           legend = list(title = list(text = 'Model')),
           hovermode = "x unified")
  
  print(p_plotly)
  
  # Gerar gráficos de métricas de erro com Plotly
  metricas_graficos <- list()
  metricas <- colnames(df_metricas)
  
  for (metrica in metricas) {
    df_metricas_long <- df_metricas %>%
      rownames_to_column(var = "Modelo") %>%
      dplyr::select(Modelo, all_of(metrica)) %>%
      pivot_longer(cols = -Modelo, names_to = "Métrica", values_to = "Valor") %>%
      arrange(Valor)
    
    p_error <- plot_ly(df_metricas_long, 
                       x = ~Valor, 
                       y = ~reorder(Modelo, Valor), 
                       type = 'bar', 
                       orientation = 'h') %>%
      layout(title = paste("Erro", metrica, "por Modelo"),
             xaxis = list(title = paste("Valor do", metrica)),
             yaxis = list(title = "Modelo"))
    
    metricas_graficos[[metrica]] <- p_error
  }
  
  # Fim do contador de tempo
  end_time <- Sys.time()
  tempo_execucao <- end_time - start_time
  cat("Tempo total de execução:", tempo_execucao, "\n")
  
  # Retorno final da função com adição dos plots e tempo de execução
  return(list(modelos_treinados = modelos_treinados, 
              df_projecoes = df_projecoes, 
              names_X_predict = names_ensemble_X,
              metricas = df_metricas, 
              df_test_com_previsoes = df_test, 
              plot_predict = p_plotly,
              plots_errors = metricas_graficos,
              tempo_execucao = tempo_execucao,
              mensagens_erro = mensagens_erro))
}


# Gerar dataframe de exemplo
set.seed(2024)
n_anos <- 20
anos <- seq(2000, 1999 + n_anos)
consumo_energia <- round(runif(n_anos, 1000, 5000), 2)  # Consumo de energia em MWh
temperatura_media <- round(runif(n_anos, 15, 25), 2)  # Temperatura média em °C
valor_tarifa <- round(runif(n_anos, 0.10, 0.50), 2)  # Valor da tarifa em R$/kWh
populacao <- round(runif(n_anos, 50000, 200000), 0)  # População
renda_media <- round(runif(n_anos, 2000, 5000), 2)  # Renda média em R$

# Criar o dataframe
df_exemplo <- data.frame(
  Ano = anos,
  consumo_energia = consumo_energia,
  temperatura_media = temperatura_media,
  valor_tarifa = valor_tarifa,
  populacao = populacao,
  renda_media = renda_media
)


# Lista de modelos a serem treinados
lista_modelos <- list(
  lm = list(method = "lm"),  # Regressão Linear
  rf = list(method = "rf", preProcess = c("center", "scale"), tuneGrid = data.frame(mtry = 2)),  # Random Forest
  gbm = list(method = "gbm", preProcess = c("center", "scale"), tuneGrid = expand.grid(interaction.depth = 1, n.trees = 50, shrinkage = 0.1, n.minobsinnode = 10)),  # Gradient Boosting
  ridge = list(method = "ridge"),  # Regressão Ridge
  lasso = list(method = "lasso"),  # Regressão Lasso
  enet = list(method = "enet"),  # Elastic Net
  svmLinear = list(method = "svmLinear", preProcess = c("center", "scale")),  # SVM Linear
  svmRadial = list(method = "svmRadial", preProcess = c("center", "scale"), tuneGrid = expand.grid(sigma = 0.01, C = 1)),  # SVM Radial
  knn = list(method = "knn", preProcess = c("center", "scale"), tuneGrid = data.frame(k = 5)),  # K-Nearest Neighbors
  nnet = list(method = "nnet", preProcess = c("center", "scale"), tuneGrid = expand.grid(size = 5, decay = 0.1)),  # Neural Network
  earth = list(method = "earth"),  # Multivariate Adaptive Regression Splines
  pcr = list(method = "pcr"),  # Principal Component Regression
  pls = list(method = "pls"),  # Partial Least Squares
  xgbTree = list(method = "xgbTree", tuneGrid = expand.grid(nrounds = 50, max_depth = 3, eta = 0.1, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 1)),  # XGBoost
  cubist = list(method = "cubist")  # Cubist
)

# Aplicar a função com o dataframe de exemplo
resultado <- train_evaluate_multiple_models_ML_annual(
  df = df_exemplo,
  lista_modelos = lista_modelos,
  explicativas = c("temperatura_media", "valor_tarifa", "populacao", "renda_media"),
  var_target = "consumo_energia",
  ultimo_ano_realizado = 2019,
  ano_inicio = 2000,
  n_fora_modelo = 1,
  n_test_caret = 3
)

# Exibir os resultados
print(resultado$df_projecoes)
print(resultado$metricas)
print(resultado$plot_predict)
print(resultado$plots_errors)







