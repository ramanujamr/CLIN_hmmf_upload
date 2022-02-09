# PFBBr - Qual ===============================================================================================

tab_pfbbr_qual = tabPanel("PFBBr - Qual", fluid=TRUE,
         
                   ## Row 1/3 Filename --------------------------------------------------------------------------------
                   
                   fluidRow(column(12,
                                   h4("FILE NAME:"),
                                   h5("Ensure the file name is as per standard convention"),
                                   h5("removed_qcs_normalized_results_yyyymmdd_PFBBr_XXXX###_yyyymmdd"),
                                   h5("Example: removed_qcs_normalized_results_20201214_PFBBr_CLIN012_20211206")
                   )
                   ),
                   
                   hr(style = "border-top: 1px solid #000000;"),
                   
                   ## Row 2/3 Column names ----------------------------------------------------------------------------
                   
                   fluidRow(column(12,
                                   h4("COLUMN NAMES:"),
                                   h5("Ensure columns are exactly as per the standard list")
                   )
                   ),
                   
                   fluidRow(column(4, 
                                   DT::dataTableOutput("table_pfbbr_qual")
                   )
                   ), 
                   
                   hr(style = "border-top: 1px solid #000000;"),
                   
                   ## Row 3/3 Buttons ---------------------------------------------------------------------------------
                   
                   fluidRow(column(12,
                                   h4("UPLOAD:")
                   )
                   ),
                   
                   fluidRow(column(4,
                                   fileInput("inFile_pfbbr_qual", "Choose CSV File", multiple = F,
                                             accept = ".csv", placeholder = "No file selected"),
                                   tags$style(".progress-bar {background-color:green}"),
                                   textOutput("filename_pfbbr_qual")
                   ),
                   
                   column(4, actionButton("check_pfbbr_qual", "Check", width='200px'), textOutput("columns_pfbbr_qual")),
                   column(4, actionButton("upload_pfbbr_qual", "Upload to Postgress", width='200px'),  textOutput("upload_status_pfbbr_qual"))
                   ),
                   br(),
                   
                   fluidRow(column(4, offset = 8,
                                   actionButton("overwrite_pfbbr_qual", "Overwrite", width='200px'),
                                   tags$style(type="text/css", "#overwrite_pfbbr_qual {background-color:red;color: white}"))),
                   
                   br()
              
          )