library(jsonlite)

dados <- fromJSON("TFT_DataS10.json")


teste <- dados$units

nomes_das_listas <- sapply(teste, function(lista) lista[1])
print(nomes_das_listas)

names(dados$units)

dados$units$TFT10_Gnar
