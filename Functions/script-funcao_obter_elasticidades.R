# Definir a função para calcular elasticidades aproximadas usando Partial Dependence
calcular_elasticidade_pdp <- function(lista_modelos, nome_modelo, variaveis_alvo) {
  
  # Carregar pacotes necessários
  if(!requireNamespace("pacman")) install.packages("pacman")
  pacman::p_load(randomForest, pdp, dplyr, ggplot2, colorspace)
  
  # Selecionar o modelo desejado da lista de modelos treinados
  model <- lista_modelos[[nome_modelo]]
  
  # Extrair dados de treinamento
  train_data <- as.data.frame(model$trainingData)
  
  # Ajustar nome da coluna de resposta, se necessário
  names(train_data)[1] <- "ec_res"
  
  # Função de predição
  pred_wrapper <- function(object, newdata) {
    predict(object, newdata)
  }
  
  # Inicializar uma lista para armazenar elasticidades
  elasticidades <- list()
  
  # Calcular Partial Dependence e elasticidade para cada variável
  for (var in variaveis_alvo) {
    pdp_result <- pdp::partial(model, pred.var = var, train = train_data, pred.fun = pred_wrapper)
    
    # Calcular a derivada numérica (diferença finita) das dependências parciais
    dp <- diff(pdp_result$yhat) / diff(pdp_result[[var]])
    
    # Calcular a elasticidade aproximada
    elasticidade <- (dp * pdp_result[[var]][-length(pdp_result[[var]])]) / pdp_result$yhat[-length(pdp_result$yhat)]
    
    # Armazenar a elasticidade média, lidando com possíveis NAs
    media_elasticidade <- mean(elasticidade, na.rm = TRUE)
    if (!is.na(media_elasticidade)) {
      elasticidades[[var]] <- media_elasticidade
    }
  }
  
  # Transformar a lista de elasticidades em um DataFrame
  elasticidades_df <- data.frame(
    Variavel = names(elasticidades),
    Elasticidade = unlist(elasticidades)
  )
  
  # Ordenar o DataFrame por elasticidade
  elasticidades_df <- elasticidades_df[order(-elasticidades_df$Elasticidade),]
  
  # Calcular as posições iniciais e finais das barras
  elasticidades_df$End <- cumsum(elasticidades_df$Elasticidade)
  elasticidades_df$Start <- c(0, head(elasticidades_df$End, n=-1))
  elasticidades_df$Index <- seq_along(elasticidades_df$Variavel)
  width <- 0.8
  
  # Gerar uma paleta customizada
  num_vars <- length(unique(elasticidades_df$Variavel))
  cores <- colorspace::qualitative_hcl(n = num_vars)
  
  # Criar o gráfico de cascata
  p <- ggplot(elasticidades_df, aes(fill = Variavel, label = round(Elasticidade, 2))) +
    geom_rect(aes(xmin = Index - width/2, xmax = Index + width/2, ymin = Start, ymax = End)) +
    scale_fill_manual(values = cores) +
    geom_text(aes(x = Index, y = (Start + End) / 2, label = round(Elasticidade, 2)), size = 3, color = "black", vjust = -0.5) +
    labs(title = paste("Gráfico de Cascata das Elasticidades -", nome_modelo),
         y = "Elasticidade Cumulativa", x = "") +
    theme_classic() +
    theme(legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, hjust = 1), 
          axis.ticks.x = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm")) +
    coord_cartesian(ylim = c(NA, 1.1 * max(elasticidades_df$End)), expand = FALSE) +
    scale_x_continuous(breaks = elasticidades_df$Index, labels = elasticidades_df$Variavel)
  
  print(p)
  
  # Retornar tanto o gráfico quanto o DataFrame
  return(list(grafico = p, dataframe_elasticidades = elasticidades_df))
}

# Exemplo de uso da função
# Supondo que ec_res_caret seja um objeto que contém os modelos treinados
# # e "rf" seja o nome do modelo Random Forest
# variaveis_alvo <- names(ec_res_caret$modelos_treinados$rf$trainingData)[-1]
# calcular_elasticidade_pdp(ec_res_caret$modelos_treinados, "rf", variaveis_alvo)
