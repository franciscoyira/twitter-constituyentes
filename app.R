library(shiny)
library(here)
library(dplyr)
library(readr)
library(stringr)
library(reactablefmtr)
library(plotly)
library(tweetrmd)
library(waiter)
library(aws.s3)

source(here("source", "func_status_badge.R"))
source(here("source", "func_update_rds_df.R"))

htmltools::tagList(
  tweetrmd:::html_dependency_twitter()
)

# Read the RDS files from AWS -----
# Retrieves the files metadata
rds_files_df <-
  aws.s3::get_bucket_df("fyac-final-data-twitter-constituyentes")

# Downloads files from S3 if the S3 version is more recent
# (or if there isn't a local version)
purrr::walk(rds_files_df$Key, update_rds_df)

# Reads the files from /data into memory
rnk_tweets <- read_rds(here("data", "rnk_tweets.rds"))
rnk_total_engagement <- read_rds(here("data", "rnk_total_engagement.rds"))
df_plot_coalitions <-
  read_rds(here("data", "df_plot_coalitions.rds"))

# Some post processing ----
max_engagement <- max(rnk_total_engagement$total_engagement)

rnk_data <- rnk_total_engagement[,c("screen_name",
                                    "total_engagement",
                                    "lista_grouped",
                                    "genero",
                                    "edad")]

df_last_week <-
  df_plot_coalitions %>%
  filter(week == max(week))

listas_levels <- df_plot_coalitions %>%
  pull(lista_grouped) %>%
  levels()

# Colour palette
pal <-  c(
  `La Lista del Pueblo` = "#720026",
  `Otros` = "#777777",
  `Independientes No Neutrales` = "#facc00",
  `Independiente (Pueblos Originarios)` = "#b47cc7",
  `Apruebo Dignidad` = "#d65f5f",
  `Lista del Apruebo` = "#ff9214",
  `Vamos por Chile` = "#306bac"
)

# The APP ----
ui <- fluidPage(
  autoWaiter(id = "p", fadeout = TRUE, color = "white",
             html =
               div(style = "color: gray", spin_loaders(id = 5, color = "gray"), h5("Cargando..."))
               ),
  titlePanel(div(style = "font-family: Bahnschrift Condensed;
    background-color: #47127D;
    color: white;
    text-align: center;
    border-radius: 5px;
    padding: 0.5px;", h1("Convención Constitucional en Twitter - Tracker semanal")),
             windowTitle = "Convención Constitucional en Twitter - Tracker semanal"),
  fluidRow(column(
    12, plotlyOutput(outputId = "p", height = "370px")
  )),
  # This part is for testing reactivity
  #fluidRow(verbatimTextOutput("filtring")),
  fluidRow(
    # I'll need to do something like this to add back the controls:
    # https://stackoverflow.com/questions/59541106/nested-fluidrow-layout-in-shiny
    column(6,
           fluidRow(
             column(6, radioButtons("gender", "Género",
                                         choiceValues = c(unique(rnk_data$genero), "All"),
                                         choiceNames = c("Mujer", "Hombre", "Todos"),
                                    selected = "All",
                                         inline = TRUE)),
             column(6, sliderInput("age", "Edad",
                                   value = c(min(rnk_data$edad), max(rnk_data$edad)),
                                   min = min(rnk_data$edad), max = max(rnk_data$edad))),
             style = "background-color:#f7f7f8;border-radius: 5px;margin-right: 10px;%;margin-bottom: 10px;margin-left: 1px;"

             ),
           fluidRow(column(12, h4("Constituyentes con mayor engagement (semana pasada)"),
                           style = "margin-left: 1px;")
                    ),
           fluidRow(reactableOutput(outputId = "t"))),
    column(6,
           fluidRow(column(12, h4("Tweets con mayor engagement (semana pasada)"),
                           style = "margin-left: 1px;")
           ),
           fluidRow(reactableOutput(outputId = "top_tweets")))),
  fluidRow(div(style = list("text-align: center;
    font-size: 9px;
    color: #C0C0C0;
    padding: 10px:"),
               "Creado por ", tags$a(href="https://www.franciscoyira.com", "Francisco Yirá"), "usando",
               tags$a(href="https://shiny.rstudio.com/", "R Shiny"),
                              "| Código bajo ", tags$a(href="https://github.com/franciscoyira/shiny_tracking_constituyentes/blob/main/LICENSE", "MIT License"), "© 2022",
               "| Repo en ", tags$a(href="https://github.com/franciscoyira/shiny_tracking_constituyentes", "GitHub")))

  # Also check this
  # https://shiny.rstudio.com/articles/layout-guide.html
)

server <- function(input, output, session) {
  # selected rows in Constituents table
  selected <- reactive(getReactableState("t", "selected"))

  # Data for the constituents ranking
  data <- reactiveVal(rnk_data)

    filter_rnk_const <- function(rnk_data) {
    if (is.null(event_data("plotly_restyle"))) {
      listas_to_show_chr <- listas_levels
    }
    else {
      event_restyle <- event_data("plotly_restyle")
      listas_to_show_raw <- event_restyle[[1]]$visible[1:7] %>%
        as.character()

      listas_to_show_bool <- listas_to_show_raw == "TRUE"
      listas_to_show_chr <- listas_levels[listas_to_show_bool]
    }

    filtered <- rnk_data %>%
      filter(lista_grouped %in% listas_to_show_chr) %>%
      filter(between(edad, input$age[1], input$age[2]))

    if (input$gender != "All") {
      filtered <- filtered %>%
        filter(genero == input$gender)
    }

    filtered
  }

    # Data for the tweets ranking
    data_tweets <- reactiveVal(
      rnk_tweets %>%
        transmute(
          Ranking = row_number(),
          Tweet = status_url,
          Likes = favorite_count,
          Retweets = retweet_count
        ) %>%
        slice_head(n = 12)
    )

    ## PLOTLY ENGAGEMENT COALITION ----
  output$p <- renderPlotly({
    plot_ly(
      df_plot_coalitions,
      x = ~ week,
      y = ~ total_engagement,
      color = ~ lista_grouped,
      text = ~ lista_grouped,
      type = 'scatter',
      mode = 'lines',
      colors = pal,
      legendgroup = ~ lista_grouped,
      hovertemplate = "Semana: %{x}<br>Engagement: %{y:,}<br><extra>%{text}</extra>"
    ) %>%
      add_trace(
        data = df_last_week,
        x = ~ week,
        y = ~ total_engagement,
        text = ~ lista_grouped,
        type = "scatter",
        mode = "markers",
        color = ~ lista_grouped,
        marker = list(opacity = 1),
        showlegend = FALSE,
        legendgroup = ~ lista_grouped
      ) %>%
      layout(
        title = list(
          text = 'Engagement semanal por coalición',
          font = list(size = 22),
          x = 0.01,
          y = 0.99
        ),
        xaxis = list(
          tickformat = "%d %B %Y",
          tick0 = "2021-05-31",
          dtick = 14 * 24 * 3600 * 1000,
          title = NA,
          range = c({
            dt = max(df_plot_coalitions$week)
            lubridate::month(dt) = lubridate::month(dt) - 3
            dt
          },
          max(df_plot_coalitions$week) + 3),
          rangeslider = list(visible = T,
                             thickness = 0.07),
          rangeselector = list(y = 0.91,
                               buttons = list(
                                 list(
                                   count = 3,
                                   label = "3 meses",
                                   step = "month",
                                   stepmode = "todate",
                                   active = TRUE
                                 ),
                                 list(
                                   count = 6,
                                   label = "6 meses",
                                   step = "month",
                                   stepmode = "todate"
                                 ),
                                 list(
                                   count = 1,
                                   label = "Este año",
                                   step = "year",
                                   stepmode = "todate"
                                 ),
                                 list(label = "Todo",
                                      step = "all")
                               ))
        ),
        yaxis = list(title = 'Engagement total (Likes + RTs)'),
        legend = list(title = list(text = '<b>Coaliciones</b>'),
                      itemclick="toggleothers",
                      itemdoubleclick=FALSE),
        font = list(family = "Bierstadt",
                    size = 12)
      ) %>%
      config(displaylogo = FALSE,
             locale = 'es') %>%
      event_register("plotly_restyle")
  })

  ## TABLE CONSTITUENTS ----
  output$t <- renderReactable({

    data() %>%
    reactable(
      defaultColDef = colDef(
        header = function(value) gsub(".", " ", value, fixed = TRUE),
        minWidth = 160,
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        screen_name = colDef(
          name = "Username de Twitter",
          minWidth = 220,
          maxWidth = 220,
          vAlign = "center",
          cell = function(value) {
            image <-
              img(
                src = paste0("/profile_pics/", sprintf("%s.jpg", value)),
                height = "30px",
                alt = value
              )

            tagList(div(
              style = list(
                display = "inline-flex",
                width = "30px",
                marginRight = "5px"
              ),
              image
            ),
            htmltools::tags$a(href = paste0("https://twitter.com/", value), value))
          }
        ),

        total_engagement = colDef(
          name = "Engagement total",
          format = colFormat(separators = TRUE),
          cell = data_bars(
            rnk_total_engagement,
            text_position = "above",
            bar_height = 11,
            number_fmt = scales::comma
          )
        ),
        lista_grouped = colDef(
          name = "Coalición",
          minWidth = 220,
          vAlign = "center",
          cell = function(value) {
            badge <- status_badge(lista = value)
            tagList(badge, value)
          }
        ),
        genero = colDef(show = FALSE),
        edad = colDef(show = FALSE)
      ),
      selection = "multiple",
      onClick = "select",
      bordered = TRUE,
      highlight = TRUE,
      showPageInfo = FALSE,
      fullWidth = FALSE,
      width = "97%",
      paginationType = "simple",
      defaultPageSize = 12,
      searchable = TRUE,
      style = list(fontFamily = 'Bierstadt'),
      language = reactableLang(
        searchPlaceholder = "Buscar constituyentes (username o coalición)",
        pagePrevious = "Anterior",
        pageNext = "Siguiente",
        pageNumbers = "{page} de {pages}",
        noData = "Sin resultados. Busca por username o coalición."
      ),
      theme = reactableTheme(style = list(
        "a" = list(
          color = "black",
          textDecoration = "none",
          borderBottom = "none",
          verticalAlign = "50%",
          fontSize = "18px",
          "&:hover, &:focus" = list(borderBottom = "1.5px solid black")
        )),
        searchInputStyle = list(
          alignSelf = "flex-start",
          marginLeft = "15px",
          marginBottom = "8px",
          width = "80%"),
        rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #393742")
      ))
  })

output$top_tweets <- renderReactable( {
  reactable(
    data_tweets(),
    defaultColDef = colDef(
      header = function(value)
        gsub(".", " ", value, fixed = TRUE),
      headerStyle = list(background = "#f7f7f8")
    ),
    columns = list(
      Ranking = colDef(name = "Rank",
                       maxWidth = 60,
                       align = "center",
                       vAlign = "center",
                       style = list(fontSize = 22)),
      Tweet = colDef(
        cell = function(Tweet) {
          tweet_embed(Tweet, omit_script = FALSE, plain = TRUE)
        },
        style = reactablefmtr::cell_style(font_size = 12),
        html = TRUE
      ),
      Likes = colDef(
        maxWidth = 100,
        vAlign = "center",
        cell = function(value) {
          tagList(scales::comma(value),
                  span(
                    shiny::icon("heart"),
                    role = "img",
                    style = list(
                      color = "#fe5979",
                      marginLeft = "5px",
                      marginRight = "5px"
                    )
                  ))
        }
      ),
      Retweets = colDef(
        maxWidth = 100,
        vAlign = "center",
        cell = function(value) {
          tagList(scales::comma(value),
                  span(
                    shiny::icon("retweet"),
                    role = "img",
                    style = list(
                      color = "#19cf86",
                      marginLeft = "5px",
                      marginRight = "5px"
                    )
                  ))
        }
      )),
    language = reactableLang(
      pagePrevious = "Anterior",
      pageNext = "Siguiente",
      pageNumbers = "{page} de {pages}"),
    bordered = FALSE,
    showPageInfo = FALSE,
    style = list(fontFamily = 'Bierstadt'),
    highlight = TRUE,
    defaultPageSize = 6,
    paginationType = "simple",
    theme = reactableTheme(style = list(
      "blockquote" = list(
        color = "black",
        fontSize = "12px")
      )
    ))

}

  )

  observeEvent(event_data("plotly_restyle"), {

    filtered <- filter_rnk_const(rnk_data)

    rnk_tweets_filtered <-
      rnk_tweets %>%
      inner_join(filtered %>%
                   mutate(screen_name_lower = str_to_lower(screen_name)),
                 by = "screen_name_lower") %>%
      transmute(
        Ranking = row_number(),
        Tweet = status_url,
        Likes = favorite_count,
        Retweets = retweet_count
      ) %>%
      slice_head(n = 12)
    data_tweets(rnk_tweets_filtered)

    data(filtered)


    })

  observeEvent(input$age, {

    filtered <- filter_rnk_const(rnk_data)

    data(filtered)

    rnk_tweets_filtered <-
      rnk_tweets %>%
      inner_join(filtered %>%
                   mutate(screen_name_lower = str_to_lower(screen_name)),
                 by = "screen_name_lower") %>%
      transmute(
        Ranking = row_number(),
        Tweet = status_url,
        Likes = favorite_count,
        Retweets = retweet_count
      ) %>%
      slice_head(n = 12)
    data_tweets(rnk_tweets_filtered)
  })

  observeEvent(input$gender, {

    filtered <- filter_rnk_const(rnk_data)

    data(filtered)

    rnk_tweets_filtered <-
      rnk_tweets %>%
      inner_join(filtered %>%
                   mutate(screen_name_lower = str_to_lower(screen_name)),
                 by = "screen_name_lower") %>%
      transmute(
        Ranking = row_number(),
        Tweet = status_url,
        Likes = favorite_count,
        Retweets = retweet_count
      ) %>%
      slice_head(n = 12)

    data_tweets(rnk_tweets_filtered)

  })

  # SELECTING CONSTITUENTS
  observe({
    rnk_tweets_filtered <-
      rnk_tweets %>%
      inner_join(data() %>%
                   {if (!is.null(selected())) slice(., selected()) else .} %>%
                   mutate(screen_name_lower = str_to_lower(screen_name)),
                 by = "screen_name_lower") %>%
      transmute(
        Ranking = row_number(),
        Tweet = status_url,
        Likes = favorite_count,
        Retweets = retweet_count
      ) %>%
      slice_head(n = 12)


    data_tweets(rnk_tweets_filtered)
  })

}

shinyApp(ui, server)
