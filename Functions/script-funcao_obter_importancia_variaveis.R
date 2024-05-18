# Definir a função para calcular a importância das variáveis usando vip
obter_var_importancia_vip <- function(lista_modelos, nome_modelo) {
  
  # Carregar pacotes necessários
  if(!requireNamespace("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, ggplot2, vip, dplyr, caret)
  
  # Selecionar o modelo desejado da lista de modelos treinados
  model <- lista_modelos[[nome_modelo]]
  
  # Extrair dados de treinamento
  train_data <- as.data.frame(model$trainingData)
  
  # Ajustar nome da coluna de resposta, se necessário
  names(train_data)[1] <- "ec_res"
  
  # Definir a função de predição
  pred_wrapper <- function(model, newdata) {
    predict(model, newdata)
  }
  
  # Calcular a importância das variáveis usando a função vip()
  vip_plot <- vip::vip(
    model,
    train = train_data,
    method = "permute",
    target = "ec_res",
    metric = "RMSE",
    nsim = 5,
    sample_frac = 0.5,
    pred_wrapper = pred_wrapper
  )
  
  # Extrair as importâncias das variáveis do objeto vip_plot
  importancias_df <- as.data.frame(vip::vi(model, method = "permute", 
                                           train = train_data, target = "ec_res", 
                                           metric = "RMSE", nsim = 5, 
                                           sample_frac = 0.5, 
                                           pred_wrapper = pred_wrapper))
  importancias_df <- importancias_df[order(-importancias_df$Importance),]
  names(importancias_df) <- c("Variavel", "Importancia")
  
  # Normalizar as importâncias para somar 100%
  soma_total <- sum(importancias_df$Importancia)
  importancias_df$Importancia <- importancias_df$Importancia / soma_total * 100
  
  # Calcular as posições iniciais e finais das barras
  importancias_df$End <- cumsum(importancias_df$Importancia)
  importancias_df$Start <- c(0, head(importancias_df$End, n=-1))
  importancias_df$Index <- seq_along(importancias_df$Variavel)
  width <- 0.8
  
  # Gerar uma paleta customizada
  num_vars <- length(unique(importancias_df$Variavel))
  cores <- colorspace::qualitative_hcl(n = num_vars)
  
  # Criar o gráfico de cascata
  p <- ggplot(importancias_df, aes(fill = Variavel, label = round(Importancia, 2))) +
    geom_rect(aes(xmin = Index - width/2, xmax = Index + width/2, ymin = Start, ymax = End)) +
    scale_fill_manual(values = cores) +
    geom_text(aes(x = Index, y = (Start + End) / 2, label = round(Importancia, 2)), size = 3, color = "black", vjust = -0.5) +
    labs(title = paste("Gráfico de Cascata das Importâncias das Variáveis -", nome_modelo),
         y = "Importância Cumulativa (%)", x = "") +
    theme_classic() +
    theme(legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, hjust = 1), 
          axis.ticks.x = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm")) +
    coord_cartesian(ylim = c(NA, 1.1 * max(importancias_df$End)), expand = FALSE) +
    scale_x_continuous(breaks = importancias_df$Index, labels = importancias_df$Variavel)
  
  print(p)
  
  # Filtrar dataframe das importancias
  importancias_df_2 <- importancias_df %>%
    dplyr::select(Variavel, Importancia) %>%
    mutate(Indice = row_number())
  
  rownames(importancias_df_2) <- importancias_df_2$Indice
  importancias_df_2$Indice <- NULL
  
  print(importancias_df_2)
  
  # Retornar tanto o gráfico quanto o DataFrame
  return(list(grafico = p, dataframe_importancias = importancias_df_2))
}
