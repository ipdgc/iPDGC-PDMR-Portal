snpWarning <- ifelse(isolate(presnplist[presnplist$Exposure == input$trait & presnplist$Outcome == input$outcome, `no. of SNPs`] < 10), T, F)
ncaseNAWarning <- ifelse(isolate(is.na(presnplist3[presnplist3$Exposure == input$trait, ncase])), T, F)
ncaseLowWarning <- ifelse(isolate(presnplist3[presnplist3$Exposure == input$trait, ncase] < 250), T, F)
ncaseZeroWarning <- ifelse(isolate(presnplist3[presnplist3$Exposure == input$trait, ncase] == 0), T, F)
none <- F
if (snpWarning) {
  if (!is.na(ncaseLowWarning)) {
    if (ncaseLowWarning){
      message <- "Warning: less than 250 participants in the exposure study and 10 instrumental variables in total. Advise caution when interpreting the results."
    } else {
      message <- "Warning: less than 10 instrumental variables detected. Advise caution when interpreting the results."
    }
  } else {
    message <- "Warning: less than 10 instrumental variables detected. Advise caution when interpreting the results."
  }
} else if (ncaseZeroWarning | ncaseNAWarning) {
  message <- "Warning: number of case-participants in the exposure study is missing. If the exposure is a continuous trait, please ignore. Otherwise, Advise caution when interpreting the results."
} else if (ncaseLowWarning) {
  message <- "Warning: less than 250 case-participants in the exposure study. Advise caution when interpreting the results."
} else {
  message <- ""
  none <- T
}

note <- notificationItemCustom(div(message, id = "warningNotification"),
                         icon = icon("exclamation-triangle"),
                         status = "warning")

if (none != T){dropdownMenu(note, type = "notifications")}
notificationItem("", icon = icon("check"))


# if (isolate(presnplist[presnplist$Exposure == input$trait & presnplist$Outcome == input$outcome, `no. of SNPs`] < 10)) {
#   if (isolate(is.na(presnplist3[presnplist3$Exposure == input$trait, ncase]))) {
#     notificationItem(span("Warning: less than 10 instrumental variables detected. Advise caution when interpreting the results.", id = "warningNotification"),
#                      icon = icon("exclamation-triangle"),
#                      status = "warning")
#   } else if (isolate(presnplist3[presnplist3$Exposure == input$trait, ncase] < 250)) {
#     notificationItem(span("Warning: less than 250 participants in the exposure study and 10 instrumental variables in total. Advise caution when interpreting the results.", id = "warningNotification"),
#                      icon = icon("exclamation-triangle"),
#                      status = "warning")
#   } else {
#     notificationItem(span("Warning: less than 10 instrumental variables detected. Advise caution when interpreting the results.", id = "warningNotification"),
#                      icon = icon("exclamation-triangle"),
#                      status = "warning")
#   }
# } else {
#   if (isolate(presnplist3[presnplist3$Exposure == input$trait, ncase] == 0)){ #case number = 0 likely means a continuous trait
#     notificationItem("",
#                      icon = icon("check"))
#   } else if (isolate(presnplist3[presnplist3$Exposure == input$trait, ncase] < 250)) {
#     notificationItem(span("Warning: less than 250 case-participants in the exposure study. Advise caution when interpreting the results.",
#                           id = "warningNotification"),
#                      icon = icon("exclamation-triangle"),status = "warning")
#   } else {
#     notificationItem("",
#                      icon = icon("check"))
#   }
# }