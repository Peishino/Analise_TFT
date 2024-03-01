library(shiny)
library(shinythemes)
library(jsonlite)
library(tidyverse)

getwd()
dados <- fromJSON("TFT_DataS10.json")


# Ordenação pela coluna Tier
ordenacao_tier <- order(sapply(dados$units, function(lista) lista$Tier))

nomes_das_unidades <- names(dados$units)[ordenacao_tier]
nomes_dos_labels <- sapply(dados$units, function(lista) lista$name)[ordenacao_tier]

# Função para obter a cor da borda com base no Tier
getBorderColor <- function(tier) {
  case_when(
    tier == 2 ~ "green",
    tier == 3 ~ "blue",
    tier == 4 ~ "purple",
    tier == 5 ~ "yellow",
    TRUE ~ "black"  # Cor padrão para outros Tiers
  )
}

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Seleção de Personagem TFT"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        selectInput("num_estrelas", "Número de Estrelas", c("1 Estrela", "2 Estrelas", "3 Estrelas")),
        lapply(seq_along(nomes_das_unidades), function(i) {
          actionButton(
            inputId = paste0("btn_", nomes_das_unidades[i]),
            label = nomes_dos_labels[i],
            style = sprintf("border-color: %s; background-image: url('%s.jpg'); background-size: cover; color: white; width: 110px; height:60px;", 
                            getBorderColor(dados$units[[nomes_das_unidades[i]]]$Tier),
                            nomes_das_unidades[i])
          )
        })
      )
    ),
    
    mainPanel(
      textOutput("info_personagem"),
      tableOutput("table_traits")
    )
  ),
  
  # Adicionar estilo CSS para as bordas dos botões
  tags$head(
    tags$style(
      HTML(
        paste0(
          lapply(seq_along(nomes_das_unidades), function(i) {
            sprintf("#btn_%s { border-width: 2px; border-radius: 5px; }", nomes_das_unidades[i])
          }),
          collapse = "\n"
        )
      )
    )
  )
)

server <- function(input, output) {
  selecao_personagens <- reactiveVal(list())  # Inicializa uma lista vazia para armazenar os personagens selecionados
  
  observe({
    lapply(nomes_das_unidades, function(personagem) {
      eventExpr <- reactive(input[[paste0("btn_", personagem)]])
      observeEvent(eventExpr(), {
        num_estrelas <- as.numeric(substr(input$num_estrelas, 1, 1))
        temp_selecao <- selecao_personagens()
        
        if (personagem %in% names(temp_selecao)) {
          temp_selecao[[personagem]] <- num_estrelas
        } else {
          temp_selecao[[personagem]] <- num_estrelas
        }
        
        selecao_personagens(temp_selecao)
      }, ignoreInit = TRUE)
    })
  })
  
  output$info_personagem <- renderText({
    if (length(selecao_personagens()) > 0) {
      hp_total <- sum(sapply(names(selecao_personagens()), function(personagem) {
        dados$units[[personagem]]$stats$hp * (1.8^(selecao_personagens()[[personagem]] - 1))
      }))
      
      paste("Você selecionou os personagens com estrelas:",
            paste(names(selecao_personagens()), ": ", unlist(selecao_personagens()), " estrelas", collapse = ", "),
            "HP Total:", hp_total)
    } else {
      "Selecione pelo menos um personagem."
    }
  })
  
  output$table_traits <- renderTable({
    if (length(selecao_personagens()) > 0) {
      traits <- sapply(names(selecao_personagens()), function(personagem) {
        dados$units[[personagem]]$Traits
      })
      contagem_traits <- table(unlist(traits))
      data.frame(Trait = names(contagem_traits), Contagem = as.numeric(contagem_traits))
    } else {
      data.frame(Trait = character(), Contagem = numeric())
    }
  })
}

shinyApp(ui, server)
