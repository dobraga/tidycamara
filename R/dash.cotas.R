dash.cotas <- function(ano=NULL){
  if(!is.null(ano)){
    download.cotas(ano)
  }

  df <- read.cotas()

  header <- shinydashboard::dashboardHeader(
    title = "Dashboard de cotas"
  )

  sidebar <- shinydashboard::dashboardSidebar(
    shiny::dateRangeInput(
      "data",
      label = paste("Data"),
      start = min(df$dt_anomes), end = max(df$dt_anomes) - months(1),
      min = min(df$dt_anomes), max = max(df$dt_anomes),
      separator = " até ", format = "yyyy-mm",
      startview = 'year', language = "pt-BR"
    )
  )

  body <- shinydashboard::dashboardBody(
    shiny::fluidRow(
      shinydashboard::infoBoxOutput("soma", width = 6),
    ),
    shiny::fluidRow(
      shinydashboard::box(
        plotly::plotlyOutput("plot"), width = 12
      ),
    ),
    shiny::fluidRow(
      shinydashboard::box(
        plotly::plotlyOutput("plot_parlamentar"), width = 12
      ),
    )

  )

  ui <- shinydashboard::dashboardPage(header, sidebar, body)

  server <- function(input, output) {

    output$dataText  <- shiny::renderText({
      paste(
        length(input$data),
        input$data[1], input$data[2],
        paste(as.character(input$data), collapse = " to "),
        typeof(input$data)
      )
    })

    df_reac <- shiny::reactive({
      df %>%
        dplyr::filter(dt_anomes >= input$data[1] & dt_anomes <= input$data[2])
    })

    output$table <- shiny::renderTable({
      df_reac
    })

    output$plot <- plotly::renderPlotly({
      g <- df_reac() %>%
        dplyr::group_by(dt_anomes) %>%
        summarise(
          vlrLiquido = sum(vlrLiquido)
        ) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(
          ggplot2::aes(x=dt_anomes, y=vlrLiquido)
        ) + ggplot2::geom_line() +
        ggplot2::xlab("Data") +
        ggplot2::ylab("Valor Líquido") +
        ggplot2::scale_y_log10(labels = scales::label_number_si())

      plotly::ggplotly(g)
    })

    output$soma <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Gasto Total",
        paste("R$", formatC(sum(df_reac()$vlrLiquido), format="f", digits=2, big.mark=".", decimal.mark = ",")),
        color = "green", fill=T, icon = shiny::icon("money-bill-wave", lib = "font-awesome")
      )
    })

    output$plot_parlamentar <- plotly::renderPlotly({
      parlamentares <- df_reac() %>%
        dplyr::group_by(txNomeParlamentar) %>%
        dplyr::summarise(vlrFinal = sum(vlrFinal)) %>%
        dplyr::top_n(5, vlrFinal) %>% .$txNomeParlamentar

      g <- df_reac() %>%
        dplyr::filter(txNomeParlamentar %in% parlamentares) %>%
        dplyr::group_by(dt_anomes, txNomeParlamentar) %>%
        dplyr::summarise(vlrFinal = sum(vlrFinal)) %>%
        ggplot2::ggplot(
          ggplot2::aes(x=dt_anomes, y=vlrFinal, color = txNomeParlamentar)
        ) +
        ggplot2::geom_line() +
        ggplot2::xlab("Data") +
        ggplot2::ylab("Valor Líquido") +
        ggplot2::scale_y_log10(labels = scales::label_number_si())

      plotly::ggplotly(g)
    })

  }

  shiny::shinyApp(ui, server)
}
