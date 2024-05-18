## A função train_evaluate_multiple_models_ML que você compartilhou tem como objetivo treinar 
# uma série de modelos especificados, avaliá-los e retornar uma série de resultados, incluindo 
# métricas de erro e um gráfico interativo de plotly. A função parece ser bastante completa e bem 
# estruturada para realizar a tarefa de treinamento e avaliação de modelos de machine learning no R 
# com o pacote caret.

# Vamos comentar brevemente cada etapa do seu código:
  
# 1- Carregamento de Pacotes: Uso do pacote pacman para instalar e carregar os pacotes necessários. 
# Isso é uma prática eficiente para garantir que todos os pacotes estejam disponíveis.

# 2- Preparação dos Dados: A função separa os dados em conjuntos de treinamento e teste com base em 
# datas específicas e lida com valores NA. É uma abordagem prática para garantir que os modelos sejam 
# treinados e testados em condições apropriadas.

# 3- Definição e Treinamento de Modelos: A função itera através de uma lista de modelos especificada 
# pelo usuário, treina cada modelo e faz previsões no conjunto de teste. Isso oferece flexibilidade 
# para testar diferentes algoritmos e configurações de modelo.

# 4- Cálculo de Métricas de Erro: Para cada modelo treinado, calcula várias métricas de erro 
# (como RMSE, MAE, R^2, etc.) no conjunto de teste. Isso permite uma avaliação abrangente do desempenho de cada modelo.

# 5- Visualização dos Resultados: A função gera um gráfico interativo com o plotly, mostrando as previsões
# dos modelos em relação aos valores reais. Isso facilita a interpretação visual do desempenho dos modelos.

#---------------------------------------------------------------
# Função para rodar varios modelos de uma só vez com caret (com timeslice)
#---------------------------------------------------------------

train_multi_models_reg_month_caret_tl <- function(df, 
                                              lista_modelos, 
                                              explicativas, 
                                              var_target, 
                                              ultimo_mes_realizado,
                                              data_inicio = NULL,
                                              n_fora_modelo, 
                                              n_test_caret,
                                              n_meses_erros = 12) {
  # Início do contador de tempo
  start_time <- Sys.time()
  
  # Instalar e/ou carregar pacotes
  if (!require(pacman)) {
    install.packages("pacman")
  }
  pacman::p_load(tidyverse, caret, ggplot2, lubridate, plotly, Metrics)
  
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
  
  # Ajustar data_inicio se não definida
  if (is.null(data_inicio)) {
    data_inicio <- min(df$Data)
  }
  
  # Ajustar data_teste com base em n_fora_modelo
  ultimo_mes_realizado <- as.Date(ultimo_mes_realizado)
  data_teste <- ultimo_mes_realizado %m-% months(n_fora_modelo)
  
  # Função para filtrar e preparar os dados
  preparar_dados <- function(df, data_inicio, data_teste, explicativas, var_target) {
    df %>%
      dplyr::select(Data, all_of(var_target), all_of(explicativas)) %>%
      dplyr::filter(Data >= as.Date(data_inicio) & Data <= as.Date(data_teste)) %>%
      drop_na()
  }
  
  df_train <- preparar_dados(df, data_inicio, data_teste, explicativas, var_target)
  
  df_test <- df %>%
    dplyr::select(Data, all_of(var_target), all_of(explicativas)) %>%
    dplyr::mutate(across(where(is.numeric), ~if_else(is.na(.), 0, .)))
  
  # Cálculo de intervalos de datas
  calcular_intervalos <- function(ultimo_mes_realizado, df_train, n_test_caret) {
    data_inicio_janela <- ultimo_mes_realizado %m-% months(n_test_caret)
    data_inicio_df <- min(df_train$Data)
    data_final_df <- max(df_train$Data)
    
    diferenca_meses <- as.numeric(interval(start = data_inicio_df, end = data_inicio_janela) %/% months(1))
    diferenca_meses_final <- as.numeric(interval(start = data_inicio_janela, end = data_final_df) %/% months(1))
    
    list(diferenca_meses, diferenca_meses_final)
  }
  
  intervalos <- calcular_intervalos(ultimo_mes_realizado, df_train, n_test_caret)
  diferenca_meses <- intervalos[[1]]
  diferenca_meses_final <- intervalos[[2]]
  
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
                                initialWindow = diferenca_meses, 
                                horizon = diferenca_meses_final, 
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
  
  # Cálculo de métricas apenas nos últimos  meses realizados
  calcular_metricas_ultimos_meses <- function(resultados_previsoes, df_test, var_target, ultimo_mes_realizado) {
    # Filtrar os últimos meses realizados
    ultimo_mes_realizado <- as.Date(ultimo_mes_realizado)
    primeiro_mes_ultimos <- ultimo_mes_realizado %m-% months(n_meses_erros)
    df_test_filtrado <- df_test %>%
      filter(Data >= primeiro_mes_ultimos & Data <= ultimo_mes_realizado)
    
    # Calcular métricas
    metricas_erro <- list()
    for (nome_modelo in names(resultados_previsoes)) {
      previsoes <- resultados_previsoes[[nome_modelo]][df_test$Data >= primeiro_mes_ultimos & df_test$Data <= ultimo_mes_realizado]
      valores_reais <- df_test_filtrado[[var_target]]
      rmse <- caret::RMSE(previsoes, valores_reais)
      mae <- Metrics::mae(previsoes, valores_reais)
      rsquared <- caret::R2(previsoes, valores_reais)
      mse <- Metrics::mse(previsoes, valores_reais)
      mape <- Metrics::mape(previsoes, valores_reais)
      metricas_erro[[nome_modelo]] <- c(MAPE = mape, MAE = mae, RMSE = rmse, MSE = mse, R_Quadrado = rsquared)
    }
    metricas_erro
  }
  
  # Exemplo de uso na função principal
  metricas_erro <- calcular_metricas_ultimos_meses(resultados_previsoes, df_test, var_target, ultimo_mes_realizado)
  
  # Convertendo a lista de métricas em um dataframe
  df_metricas <- do.call(rbind, lapply(metricas_erro, function(x) t(data.frame(x))))
  rownames(df_metricas) <- names(metricas_erro)
  df_metricas <- as.data.frame(df_metricas)
  df_metricas <- df_metricas %>% arrange(MAPE)

  # Gerar dataframe com as previsões
  df_plot <- df_test %>%
    dplyr::mutate(!!var_target := if_else(Data > as.Date(ultimo_mes_realizado), NA_real_, !!sym(var_target)))
  
  df_projecoes <- df_plot %>% 
    dplyr::select(-all_of(explicativas))
  
  df_projecoes$Data <- df$Data
  
  names_ensemble_X <- setdiff(names(df_projecoes), c(var_target, "Data"))
  
  
  # Transformar o dataframe
  df_long <- df_projecoes %>%
    pivot_longer(cols = -Data, names_to = "Model", values_to = "Value") %>%
    mutate(Model = gsub(paste0("^", var_target, "_"), "", Model)) %>% 
    arrange(Model)
  
  
  num_cores <- length(unique(df_long$Model))
  
  # Selecionar paletas de cores e combiná-las
  base_palette1 <- RColorBrewer::brewer.pal(9, "Set1")
  base_palette2 <- RColorBrewer::brewer.pal(8, "Set2")
  base_palette3 <- RColorBrewer::brewer.pal(12, "Set3")
  base_palette_Paired <- RColorBrewer::brewer.pal(12, "Paired")
  base_palette_Accent <- RColorBrewer::brewer.pal(8, "Accent")
  
  # Combinar paletas
  combined_palette <- c(base_palette1, base_palette2, base_palette3, base_palette_Paired, base_palette_Accent)[1:num_cores]
  
  # Criar um vetor de cores correspondente aos modelos
  color_mapping <- setNames(combined_palette, unique(df_long$Model))
  
  # Ajustar a cor do var_target para preto e adicionar os traços ao gráfico
  p_plotly <- plot_ly() %>%
    add_trace(data = df_long %>% filter(Model != var_target), 
              x = ~Data, y = ~Value, 
              type = 'scatter', mode = 'lines',
              color = ~Model, 
              colors = color_mapping) %>%
    add_trace(data = df_long %>% filter(Model == var_target), 
              x = ~Data, y = ~Value, 
              type = 'scatter', mode = 'lines',
              name = var_target, line = list(color = 'black')) %>%
    layout(title = "Comparação dos modelos",
           xaxis = list(title = "Data",
                        tickangle = 45,
                        tickformat = "%b/%Y",
                        type = 'date'),
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
    
    # Definir a paleta de cores gradiente
    gradient_colors <- colorRampPalette(c("#008000", "#FF0000"))(100)
    
    # Adicionar uma escala de cores baseada nos valores
    df_metricas_long <- df_metricas_long %>%
      mutate(Color = gradient_colors[as.numeric(cut(Valor, breaks = 100))])
    
    p_error <- plot_ly(df_metricas_long, 
                       x = ~Valor, 
                       y = ~reorder(Modelo, Valor), 
                       type = 'bar', 
                       orientation = 'h',
                       marker = list(color = ~Color)) %>%
      layout(title = paste("Erro", metrica, "por Modelo"),
             xaxis = list(title = paste("Valor do", metrica)),
             yaxis = list(title = "Modelo"))
    
    metricas_graficos[[metrica]] <- p_error
  }
  
  # Fim do contador de tempo
  end_time <- Sys.time()
  tempo_execucao <- end_time - start_time
  
  # Calcular tempo de execução em minutos e segundos
  tempo_execucao_minutos <- as.numeric(difftime(end_time, start_time, units = "mins"))
  tempo_execucao_segundos <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Exibir informações de tempo
  cat("Data e hora de início:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Data e hora de término:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Tempo total de execução:", floor(tempo_execucao_minutos), "minutos e", round(tempo_execucao_segundos %% 60), "segundos.\n")
  
  print(df_metricas)
  
  # Retorno final da função com adição dos plots e tempo de execução
  return(list(modelos_treinados = modelos_treinados, 
              df_projecoes = df_projecoes, 
              names_X_predict = names_ensemble_X,
              metricas_erro = df_metricas, 
              df_test_com_previsoes = df_test, 
              plot_predict = p_plotly,
              plots_errors = metricas_graficos,
              tempo_execucao = tempo_execucao,
              mensagens_erro = mensagens_erro))
  
  
  # # Explicativas
  # col_res_nc <- c("Trend", "indice_de_precos",  "populacao", "populacao_sqrt", "domicilios")
  # 
  # # Exemplo Chamada correta da função
  # nc_res_caret <- train_evaluate_multiple_models_ML_mes(df = df_B, 
  #                                           lista_modelos = lista_modelos_1, 
  #                                           explicativas = col_res_nc, 
  #                                           var_target = "nc_res", 
  #                                           ultimo_mes_realizado = "2024-03-01", 
  #                                           data_inicio = NULL,
  #                                           n_fora_modelo = 6, 
  #                                           n_test_caret = 24)
}



#---------------------------------------------------------------
# Função para rodar varios modelos de uma só vez com caret (com crossvalidation)
#---------------------------------------------------------------


train_multi_models_reg_month_caret <- function(df, 
                                               lista_modelos, 
                                               explicativas, 
                                               var_target, 
                                               ultimo_mes_realizado,
                                               data_inicio = NULL,
                                               n_fora_modelo, 
                                               n_folds = 10, # Adiciona um argumento para o número de folds
                                               n_meses_erros = 12) {
  # Início do contador de tempo
  start_time <- Sys.time()
  
  # Instalar e/ou carregar pacotes
  if (!require(pacman)) {
    install.packages("pacman")
  }
  pacman::p_load(tidyverse, caret, ggplot2, lubridate, plotly, Metrics)
  
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
  
  # Ajustar data_inicio se não definida
  if (is.null(data_inicio)) {
    data_inicio <- min(df$Data)
  }
  
  # Ajustar data_teste com base em n_fora_modelo
  ultimo_mes_realizado <- as.Date(ultimo_mes_realizado)
  data_teste <- ultimo_mes_realizado %m-% months(n_fora_modelo)
  
  # Função para filtrar e preparar os dados
  preparar_dados <- function(df, data_inicio, data_teste, explicativas, var_target) {
    df %>%
      dplyr::select(Data, all_of(var_target), all_of(explicativas)) %>%
      dplyr::filter(Data >= as.Date(data_inicio) & Data <= as.Date(data_teste)) %>%
      drop_na()
  }
  
  df_train <- preparar_dados(df, data_inicio, data_teste, explicativas, var_target)
  
  df_test <- df %>%
    dplyr::select(Data, all_of(var_target), all_of(explicativas)) %>%
    dplyr::mutate(across(where(is.numeric), ~if_else(is.na(.), 0, .)))
  
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
  train_control <- trainControl(method = "cv", 
                                number = n_folds)
  
  resultados <- treinar_modelos(formula, df_train, lista_modelos, train_control)
  modelos_treinados <- resultados[[1]]
  resultados_previsoes <- resultados[[2]]
  mensagens_erro <- resultados[[3]]
  
  # Adicionando previsões ao df_test
  for (nome_modelo in names(resultados_previsoes)) {
    nome_coluna_predicao <- paste0(var_target, "_", nome_modelo)
    df_test[[nome_coluna_predicao]] <- resultados_previsoes[[nome_modelo]]
  }
  
  # Cálculo de métricas apenas nos últimos meses realizados
  calcular_metricas_ultimos_meses <- function(resultados_previsoes, df_test, var_target, ultimo_mes_realizado) {
    # Filtrar os últimos meses realizados
    ultimo_mes_realizado <- as.Date(ultimo_mes_realizado)
    primeiro_mes_ultimos <- ultimo_mes_realizado %m-% months(n_meses_erros)
    df_test_filtrado <- df_test %>%
      filter(Data >= primeiro_mes_ultimos & Data <= ultimo_mes_realizado)
    
    # Calcular métricas
    metricas_erro <- list()
    for (nome_modelo in names(resultados_previsoes)) {
      previsoes <- resultados_previsoes[[nome_modelo]][df_test$Data >= primeiro_mes_ultimos & df_test$Data <= ultimo_mes_realizado]
      valores_reais <- df_test_filtrado[[var_target]]
      rmse <- caret::RMSE(previsoes, valores_reais)
      mae <- Metrics::mae(previsoes, valores_reais)
      rsquared <- caret::R2(previsoes, valores_reais)
      mse <- Metrics::mse(previsoes, valores_reais)
      mape <- Metrics::mape(previsoes, valores_reais)
      metricas_erro[[nome_modelo]] <- c(MAPE = mape, MAE = mae, RMSE = rmse, MSE = mse, R_Quadrado = rsquared)
    }
    metricas_erro
  }
  
  # Exemplo de uso na função principal
  metricas_erro <- calcular_metricas_ultimos_meses(resultados_previsoes, df_test, var_target, ultimo_mes_realizado)
  
  # Convertendo a lista de métricas em um dataframe
  df_metricas <- do.call(rbind, lapply(metricas_erro, function(x) t(data.frame(x))))
  rownames(df_metricas) <- names(metricas_erro)
  df_metricas <- as.data.frame(df_metricas)
  df_metricas <- df_metricas %>% arrange(MAPE)
  
  # Gerar dataframe com as previsões
  df_plot <- df_test %>%
    dplyr::mutate(!!var_target := if_else(Data > as.Date(ultimo_mes_realizado), NA_real_, !!sym(var_target)))
  
  df_projecoes <- df_plot %>% 
    dplyr::select(-all_of(explicativas))
  
  df_projecoes$Data <- df$Data
  
  names_ensemble_X <- setdiff(names(df_projecoes), c(var_target, "Data"))
  
  # Transformar o dataframe
  df_long <- df_projecoes %>%
    pivot_longer(cols = -Data, names_to = "Model", values_to = "Value") %>%
    mutate(Model = gsub(paste0("^", var_target, "_"), "", Model)) %>% 
    arrange(Model)
  
  num_cores <- length(unique(df_long$Model))
  
  # Selecionar paletas de cores e combiná-las
  base_palette1 <- RColorBrewer::brewer.pal(9, "Set1")
  base_palette2 <- RColorBrewer::brewer.pal(8, "Set2")
  base_palette3 <- RColorBrewer::brewer.pal(12, "Set3")
  base_palette_Paired <- RColorBrewer::brewer.pal(12, "Paired")
  base_palette_Accent <- RColorBrewer::brewer.pal(8, "Accent")
  
  # Combinar paletas
  combined_palette <- c(base_palette1, base_palette2, base_palette3, base_palette_Paired, base_palette_Accent)[1:num_cores]
  
  # Criar um vetor de cores correspondente aos modelos
  color_mapping <- setNames(combined_palette, unique(df_long$Model))
  
  # Ajustar a cor do var_target para preto e adicionar os traços ao gráfico
  p_plotly <- plot_ly() %>%
    add_trace(data = df_long %>% filter(Model != var_target), 
              x = ~Data, y = ~Value, 
              type = 'scatter', mode = 'lines',
              color = ~Model, 
              colors = color_mapping) %>%
    add_trace(data = df_long %>% filter(Model == var_target), 
              x = ~Data, y = ~Value, 
              type = 'scatter', mode = 'lines',
              name = var_target, line = list(color = 'black')) %>%
    layout(title = "Comparação dos modelos",
           xaxis = list(title = "Data",
                        tickangle = 45,
                        tickformat = "%b/%Y",
                        type = 'date'),
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
    
    # Definir a paleta de cores gradiente
    gradient_colors <- colorRampPalette(c("#008000", "#FF0000"))(100)
    
    # Adicionar uma escala de cores baseada nos valores
    df_metricas_long <- df_metricas_long %>%
      mutate(Color = gradient_colors[as.numeric(cut(Valor, breaks = 100))])
    
    p_error <- plot_ly(df_metricas_long, 
                       x = ~Valor, 
                       y = ~reorder(Modelo, Valor), 
                       type = 'bar', 
                       orientation = 'h',
                       marker = list(color = ~Color)) %>%
      layout(title = paste("Erro", metrica, "por Modelo"),
             xaxis = list(title = paste("Valor do", metrica)),
             yaxis = list(title = "Modelo"))
    
    metricas_graficos[[metrica]] <- p_error
  }
  
  # Fim do contador de tempo
  end_time <- Sys.time()
  tempo_execucao <- end_time - start_time
  
  # Calcular tempo de execução em minutos e segundos
  tempo_execucao_minutos <- as.numeric(difftime(end_time, start_time, units = "mins"))
  tempo_execucao_segundos <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Exibir informações de tempo
  cat("Data e hora de início:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Data e hora de término:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Tempo total de execução:", floor(tempo_execucao_minutos), "minutos e", round(tempo_execucao_segundos %% 60), "segundos.\n")
  
  print(df_metricas)
  
  # Retorno final da função com adição dos plots e tempo de execução
  return(list(modelos_treinados = modelos_treinados, 
              df_projecoes = df_projecoes, 
              names_X_predict = names_ensemble_X,
              metricas_erro = df_metricas, 
              df_test_com_previsoes = df_test, 
              plot_predict = p_plotly,
              plots_errors = metricas_graficos,
              tempo_execucao = tempo_execucao,
              mensagens_erro = mensagens_erro))
}
