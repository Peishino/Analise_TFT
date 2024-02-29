library(shiny)
library(jsonlite)

dados <- fromJSON("TFT_DataS10.json")

nomes_das_unidades <- names(dados$units)
nomes_dos_labels <- sapply(teste, function(lista) lista[1])

ui <- fluidPage(
  titlePanel("Seleção de Personagem TFT"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        lapply(seq_along(nomes_das_unidades), function(i) {
          actionButton(inputId = nomes_das_unidades[i], label = nomes_dos_labels[i])
        }),
        actionButton(inputId = "calcular_hp", label = "Calcular HP Total")
      )
    ),
    
    mainPanel(
      textOutput("info_personagem")
    )
  )
)

server <- function(input, output) {
  selecao_personagens <- reactiveVal(character(0))  # Inicializa uma lista vazia para armazenar os personagens selecionados
  
  observe({
    lapply(nomes_das_unidades, function(personagem) {
      eventExpr <- reactive(input[[personagem]])
      observeEvent(eventExpr(), {
        if (personagem %in% selecao_personagens()) {
          selecao_personagens(selecao_personagens()[-which(selecao_personagens() == personagem)])
        } else {
          selecao_personagens(c(selecao_personagens(), personagem))
        }
      }, ignoreInit = TRUE)
    })
  })
  
  output$info_personagem <- renderText({
    if (length(selecao_personagens()) > 0) {
      hp_total <- sum(sapply(selecao_personagens(), function(personagem) dados$units[[personagem]]$stats$hp))
      paste("Você selecionou os personagens:", paste(selecao_personagens(), collapse = ", "), "HP Total:", hp_total)
    } else {
      "Selecione pelo menos um personagem."
    }
  })
}

shinyApp(ui, server)