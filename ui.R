source("boxes.R", local = T)

source("dashboard.components.R", local = T)

ui <- tagList(tags$script(src = "forestplotsize.js"),
              withAnim(),
              div(
                id = "loadingPage",
                hidden(img(src = "IDPGC-logo-01pdmr.png", id = "startLogo"))
              ),
              hidden(
                div(id = "uiPage",
                    dashboardPage(
                      title = "iPDGC PD MR Portal",
                      header,
                      sidebar,
                      body
                    )
                )
              )
)