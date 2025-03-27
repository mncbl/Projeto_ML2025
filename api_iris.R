# Carregar biblioteca
library(plumber)

# Carregar o modelo treinado com caret::train()
modelo <- readRDS("C:/Users/mateu/Downloads/modelo_regressao_iris.rds")

#* @apiTitle API de Classificação Binária da Espécie de Iris

#* Prediz a espécie da flor (setosa ou versicolor) com base nas medidas
#* @param Petal.Length Comprimento da pétala
#* @param Sepal.Length Comprimento da sépala
#* @param Sepal.Width Largura da sépala
#* @param Petal.Width Largura da pétala
#* @get /prever
prever <- function(Petal.Length, Sepal.Length, Sepal.Width, Petal.Width) {
  # Converter para numérico
  Petal.Length <- as.numeric(Petal.Length)
  Sepal.Length <- as.numeric(Sepal.Length)
  Sepal.Width  <- as.numeric(Sepal.Width)
  Petal.Width  <- as.numeric(Petal.Width)
  
  # Verificar entradas
  if (any(is.na(c(Petal.Length, Sepal.Length, Sepal.Width, Petal.Width)))) {
    return(list(error = "Todos os parâmetros devem ser números válidos."))
  }
  
  # Criar dataframe de entrada
  dados <- data.frame(
    Sepal.Length = Sepal.Length,
    Sepal.Width  = Sepal.Width,
    Petal.Length = Petal.Length,
    Petal.Width  = Petal.Width
  )
  
  # Prever classe diretamente
  classe <- predict(modelo, newdata = dados, type = "raw")
  
  # Retornar resultado
  return(list(
    especie_prevista = as.character(classe),
    dados_utilizados = dados
  ))
}
