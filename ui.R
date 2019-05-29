ui <- fluidPage(tags$script(src = "forestplotsize.js"),
                dashboardPage(
                  header,
                  #dashboardHeader(disable = T),
                  sidebar,
                  body
                ),
                title = "iPDGC PD MR Portal")