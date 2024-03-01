library(jsonlite)
setwd("C:/Users/Gabriel/Documents/GitHub/Analise_TFT")
dados <- fromJSON("TFT_DataS10.json")


teste <- dados$units$TFT10_Gnar$ability

nomes_das_listas <- sapply(teste, function(lista) lista[1])
print(nomes_das_listas)

names(dados$units$tft10)
nomes_das_unidades <- names(dados$units)
nomes_das_unidades[1]
#webscraping para pegar as traits de cada personagem:

library(rvest)
library(stringr)

url <- "https://tactics.tools/info/units"
html <- read_html(url)

textosWS <- html %>%
  html_elements(xpath = '//*[@id="content-container"]/div/div/div/a/div') %>%
  html_text2()

Vetor <- str_split(textosWS,"\n")

max_length <- max(sapply(Vetor,length))

Vetor <- lapply(Vetor, function(x){
  length(x)<- max_length
  x
})

df <- do.call(rbind, Vetor)
colnames(df) <- c("Personagem","Custo","Trait1","Trait2","Trait3")

df<-as.data.frame(df)


# Mapear Traits e Tier
Traits <- lapply(df$Personagem, function(personagem) {
  traits <- c(df[df$Personagem == personagem, c("Trait1", "Trait2", "Trait3")])
  traits <- traits[!is.na(traits)]
  return(traits)
})

Tier <- df$Custo

for (i in seq_along(dados$units)) {
  personagem <- dados$units[[i]]$name
  if (personagem %in% df$Personagem) {
    dados$units[[i]]$Traits <- Traits[[match(personagem, df$Personagem)]]
    dados$units[[i]]$Tier <- Tier[match(personagem, df$Personagem)]
  }
}

write_json(dados, "TFT_DataS10.json")

# Webscaping das imagens :D

imageslinks <- html %>%
  html_elements(".border-primary-6 .object-cover")%>%
  html_attr("src")


image_names <- sapply(imageslinks, function(url) {
  # Divide a URL pelo caractere "/"
  parts <- strsplit(url, "/")[[1]]
  # Pega a última parte da URL (que contém o nome do arquivo) e divide pelo caractere "."
  filename_parts <- strsplit(tail(parts, 1), "\\.")[[1]]
  # Retorna o nome do arquivo sem a extensão
  paste0(filename_parts[1])
})

library(httr)
for (i in seq_along(imageslinks)) {
  GET(imageslinks[i], write_disk(paste0(image_names[i], ".jpg"),overwrite = T))
 }

image_names























