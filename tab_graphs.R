# Graphs ===========================================================================================

tab_graphs = tabPanel("Upload stats", fluid=TRUE,
                      
                      selectInput("select_plot", "Select panel", 
                                  choices = list("All panels", "PFBBr", "TMS", "Bile acids", "Indole")),
                      br(),
                      plotOutput("plot")
                      )