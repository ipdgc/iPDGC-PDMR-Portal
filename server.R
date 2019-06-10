server <- function(input, output, session) {
  
  source("observe.reactives.R", local = T)
  source("figures.tables.R", local = T)
  source("download.R", local = T)

    output$errorNotification <- renderMenu({
    preerrorNotification()
  })
  
  show("startLogo")
  
  startAnim(session,
            id = "startLogo",
            type = "flipInX")
  
  delay(2000,
        {
          show("uiPage")
          hide("loadingPage")
          runjs(
            "dimension = document.getElementById('forestbox').offsetWidth;
            Shiny.onInputChange('dimension', dimension);"
          )
        }
  )
}