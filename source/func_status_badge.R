# This function allows showing a square with the coallition colour on the
# bottom-left table (ranking of engagement per constituent) )
status_badge <- function(lista, width = "9px", height = width) {

  color <- switch(
    as.character(lista),
    `La Lista del Pueblo` = "#720026",
    `Otros` = "#777777",
    `Independientes No Neutrales` = "#facc00",
    `Independiente (Pueblos Originarios)` = "#b47cc7",
    `Apruebo Dignidad` = "#d65f5f",
    `Lista del Apruebo` = "#ff9214",
    `Vamos por Chile` = "#306bac"
  )
  span(
    shiny::icon("square"),
    role = "img",
    style = list(
      color = color,
      marginLeft = "5px",
      marginRight = "5px"
    ))
}
