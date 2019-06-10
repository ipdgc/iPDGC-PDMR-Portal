'%ni%' <- Negate('%in%')

#======-menuItemCustom allows you to set an id for the generated tab link (for use in javascript, etc.).
#======Do not use unless absolutely necessary---it lacks some input verification functions

menuItemCustom <- function (text, ..., icon = NULL, badgeLabel = NULL, badgeColor = "green",
                            tabName = NULL, href = NULL, newtab = TRUE, selected = NULL,
                            expandedName = as.character(gsub("[[:space:]]", "", text)),
                            startExpanded = FALSE, idItem = NULL)
{
  subItems <- list(...)
  if (!is.null(href) + !is.null(tabName) + (length(subItems) >
                                            0) != 1) {
    stop("Must have either href, tabName, or sub-items (contained in ...).")
  }
  if (!is.null(badgeLabel) && length(subItems) != 0) {
    stop("Can't have both badge and subItems")
  }
  isTabItem <- FALSE
  target <- NULL
  if (!is.null(tabName)) {
    isTabItem <- TRUE
    href <- paste0("#shiny-tab-", tabName)
  }
  else if (is.null(href)) {
    href <- "#"
  }
  else {
    if (newtab)
      target <- "_blank"
  }
  if (!is.null(badgeLabel)) {
    badgeTag <- tags$small(class = paste0("badge pull-right bg-",
                                          badgeColor), badgeLabel)
  }
  else {
    badgeTag <- NULL
  }
  if (length(subItems) == 0) {
    return(tags$li(a(href = href, `data-toggle` = if (isTabItem) "tab",
                     `data-value` = if (!is.null(tabName)) tabName, `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
                     target = target, icon, span(text), badgeTag), id = idItem))
  }
  default <- if (startExpanded)
    expandedName
  else ""
  dataExpanded <- shiny::restoreInput(id = "sidebarItemExpanded",
                                      default) %OR% ""
  isExpanded <- nzchar(dataExpanded) && (dataExpanded == expandedName)
  tags$li(class = "treeview", id = idItem, a(href = href, icon, span(text),
                                             shiny::icon("angle-left", class = "pull-right")), do.call(tags$ul,
                                                                                                       c(class = paste0("treeview-menu", if (isExpanded) " menu-open" else ""),
                                                                                                         style = paste0("display: ", if (isExpanded) "block;" else "none;"),
                                                                                                         `data-expanded` = expandedName, subItems)))
}


# notification item for top right of the dashboard
notificationItemCustom <- function (text, icon = shiny::icon("warning"), status = "success") 
{
  icon <- tagAppendAttributes(icon, class = paste0("text-", 
                                                   status))
  tags$li(a(icon, text))
}
