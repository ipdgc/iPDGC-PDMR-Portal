#============BOXES============
#=================Forest plot box========================
forestplotbox <- box(title = "Forest Plot",
                     collapsible = T,
                     width = 12,
                     status = "warning",
                     solidheader = T,
                     height = NULL,
                     id = "forestbox",
                     # Main panel for displaying outputs
                     # First panel
                     div(
                       # Output: forest plot
                       plotOutput(outputId = "forestplot",
                                  inline = T),
                       div(id = 'plotkey',
                           "The",
                           tags$strong("black points"),
                           "represent the causal estimate (log-odds ratio) of each SNP on the risk of developing PD. Horizontal lines denote 95% confidence intervals. Colored lines on the bottom of the figure represent the causal estimate when combining all SNPs together, using the inverse variance weighted, weighted median and MR Egger methods.",
                           span("Red", style = "color:rgb(251,0,7); font-weight:bold"),
                           "represents risk estimates.",
                           span("Green", style = "color:rgb(11,84,1); font-weight:bold"),
                           "represents protective estimates.",
                           style = "margin-bottom:20px"),
                       # --ActionButton that shows download options (conditionalPanel)
                       downloadButton('qkDLforestplot', 'Quick-Download')
                     ))
#======
#=================Funnel box========================
funnelplotbox <- box(title = "Funnel Plot",
                     collapsible = T,
                     width = 12,
                     status = "warning",
                     solidheader = T,
                     height = NULL,
                     # Main panel for displaying outputs
                     # First panel
                     div(
                       # Output: funnel plot
                       plotOutput(outputId = "funnelplot",
                                  inline = T),
                       #paste(as.character(10*as.integer(ResTbl_reactive$`no. of SNPs`[1])), "px", sep = "")),
                       #input$PlHeight),
                       div(id = 'plotkey',
                           'Funnel plots are used to help determine the reliability of the MR analysis. Plot of an ideal MR analysis will produce a symmetric scatterplot where data points with higher instrumental strength (approximated by inverse of standard error) will coalesce around the beta of the MR result.',
                           div("Key:",
                               div("Inverse Variance Weighted", style = "color:blue"),
                               div("MR Egger", style = "color:#ff5e5e"),
                               div("Weighted Median", style = "color:#f2c71d"),
                               style = "font-weight:bold;margin-top:20px;margin-bottom:20px"
                           )
                       ),
                       # --ActionButton that shows download options (conditionalPanel)
                       downloadButton('qkDLfunnelplot', 'Quick-Download')
                     ))
#======
#=================Intro Jumbotron========================
introbox <- span(
  div(
    class = "jumbotron",
    h2(
      "iPDGC Parkinson's Disease Mendelian Randomization Portal",
      class = "jumbotrontext"
    ),
    p(
      span(
        h4(
          "Please select a Parkinson's Disease GWAS (outcome) and a trait (exposure), then click Analyze.",
          class = "jumbotrontext"
        )
      )
    ),
    div(
      span(
        actionButton("tabBut", "Dismiss"),
        id = "dismiss_button"
      ),
      id = "dismiss_button_container"
    )
  ),
  id = "jumbo_custom")
#======
#=================Table Boxes========================
MiniResTableBox <- box(
  title = "MR Results",
  collapsible = T,
  solidHeader = T,
  width = 12,
  status = "danger",
  tableOutput(outputId = 'minitable1.output'),
  div(
    id = 'plotkey',
    "p < 0.05 is",
    tags$strong(
      "bolded."
    )
  )
)

ResTableBox <- box(title = "MR Results",
                   collapsible = T,
                   solidHeader = T,
                   width = 12,
                   status = "warning",
                   tableOutput(outputId = 'table1.output')
)
HetTableBox <- box(title = "Heterogeneity estimates",
                   collapsible = T,
                   solidHeader = T,
                   width = 12,
                   status = "success",
                   tableOutput(outputId = 'table2.output')
)
PleTableBox <- box(title = "Horizontal pleiotropy estimates",
                   collapsible = T,
                   solidHeader = T,
                   width = 12,
                   status = "warning",
                   tableOutput(outputId = 'table3.output')
)
RevTableBox <- box(title = "Reverse causality",
                   collapsible = T,
                   solidHeader = T,
                   width = 12,
                   status = "success",
                   div(p("Only available for traits with public genome-wide summary statistics.")),
                   tableOutput(outputId = 'table5.output')
)
PubTableBox <- box(title = "Publication info",
                   collapsible = T,
                   solidHeader = T,
                   width = 12,
                   status = "warning",
                   tableOutput(outputId = 'table4.output')
)

qkDLtableBox <- box(title = "Quick Download",
                    collapsible = T,
                    solidHeader = T,
                    width = 12,
                    status = "danger",
                    downloadButton('qkDLtable', 'Quick-Download'))

KeyBox <- box(title = "Key",
              collapsible = T,
              solidHeader = T,
              width = 12,
              status = "info",
              div(tags$style(HTML(
                'p {
                font-size: 12px;
                margin-top: 0em;
                margin-bottom: 0em
                }'
              )),
              div(HTML('<p><b>id</b>: specific code attributed to each trait on <a href="http://www.mrbase.org/" target="_blank">MR base</a></p>
                         <p><b>se</b>: standard error</p>
                         <p><b>no. of SNPs</b>: number of SNPs</p>
                         <p><b>Q</b>: Cochran’s Q test estimates</p>
                         <p><b>Q_df</b>: Cochran’s Q test degree freedom.</p>'),
                  id = "tablekey")
              )
)
#======
#=================Forest Plot Download Box========================
FoPlotDLbox <-  box(
  title = "Download Forest Plot",
  status = "warning",
  # Slider for size of the downloaded plot
  sliderInput('FoDLplotw', 'Width of plot (inches)', min = 1, max = 50, step = 0.25, value = 8.5),
  sliderInput('FoDLploth', 'Height of plot (inches)', min = 1, max = 50, step = 0.25, value = 11),
  # Radio buttons for background color
  radioButtons('fobg', 'Background: ',
               c("White" = "white",
                 "Transparent" = "transparent")),
  checkboxInput("forestTitle", 'Add title ("Trait" vs "Outcome")'),
  # Download buttons
  downloadButton('FoPlotDLTIFF', 'Download in TIFF (uncompressed)'),
  downloadButton('FoPlotDLPNG', 'Download in PNG'),
  downloadButton('FoPlotDLPDF', 'Download in PDF'),
  downloadButton('FoPlotDLSVG', 'Download in SVG')
)
#======
##=================Funnel Plot Download Box========================
FuPlotDLbox <-  box(
  title = "Download Funnel Plot",
  status = "danger",
  # Slider for size of the downloaded plot
  sliderInput('FuDLplotw', 'Width of plot (inches)', min = 1, max = 50, step = 0.25, value = 8.5),
  sliderInput('FuDLploth', 'Height of plot (inches)', min = 1, max = 50, step = 0.25, value = 11),
  # Radio buttons for background color
  radioButtons('fubg', 'Background: ',
               c("White" = "white",
                 "Transparent" = "transparent")),
  checkboxInput("funnelTitle", 'Add title ("Trait" vs "Outcome")'),
  # Download buttons
  downloadButton('FuPlotDLTIFF', 'Download in TIFF (uncompressed)'),
  downloadButton('FuPlotDLPNG', 'Download in PNG'),
  downloadButton('FuPlotDLPDF', 'Download in PDF'),
  downloadButton('FuPlotDLSVG', 'Download in SVG')
)
#======
#=================Table Download Box========================
tableDLbox <- box(
  title = "Download tables",
  status = "warning",
  #--selecting which tables to download
  checkboxGroupInput('ResultDLselect', "Select tables to download: ",
                     c('MR results' = 'resDLcheck',
                       'Heterogeneity estimates' = 'hetDLcheck',
                       'Horizontal pleiotropy estimates' = 'pleDLcheck',
                       'Reverse causality' = 'revDLcheck',
                       'Publication info' = 'pubDLcheck'),
                     selected = c('resDLcheck',
                                  'hetDLcheck',
                                  'pleDLcheck',
                                  'revDLcheck',
                                  'pubDLcheck')),
  #--download buttons
  downloadButton('ResultDLCSV', 'Download in CSV'),
  downloadButton("ResultDLTab", "Download in tab-delimited text file"),
  downloadButton("ResultDLPDF", "Download in TeX")
)
#======