# Indole - Qual ===========================================================================================

tab_indole_quant = tabPanel("Indole - Quant", fluid=TRUE,
                           
                           ## Row 1/3 Filename ---------------------------------------------------------------------------------
                           
                           fluidRow(column(12,
                                           h4("FILE NAME:"),
                                           h5("Ensure the file name is as per standard convention"),
                                           h5("removed_qcs_quant_results_yyyymmdd_Indole_XXXX###_yyyymmdd"),
                                           h5("Example: removed_qcs_quant_results_20201214_Indole_CLIN012_20211206")
                           )
                           ),
                           
                           hr(style = "border-top: 1px solid #000000;"),
                           
                           ## Row 2/3 Column names -----------------------------------------------------------------------------
                           
                           fluidRow(column(12,
                                           h4("COLUMN NAMES:"),
                                           h5("Ensure columns are exactly as per the standard list")
                           )
                           ),
                           
                           fluidRow(column(4, 
                                           DT::dataTableOutput("table_indole_quant")
                           )
                           ), 
                           
                           hr(style = "border-top: 1px solid #000000;"),
                           
                           ## Row 3/3 Buttons ----------------------------------------------------------------------------------
                           
                           fluidRow(column(12,
                                           h4("UPLOAD:")
                           )
                           ),
                           
                           fluidRow(column(4,
                                           fileInput("inFile_indole_quant", "Choose CSV File", multiple = F,
                                                     accept = ".csv", placeholder = "No file selected"),
                                           tags$style(".progress-bar {background-color:green}"),
                                           textOutput("filename_indole_quant")
                           ),
                           column(4, actionButton("check_indole_quant", "Check", width='200px'), textOutput("columns_indole_quant")),
                           column(4, actionButton("upload_indole_quant", "Upload to Postgress", width='200px'),  textOutput("upload_status_indole_quant"))
                           ),
                           br(),
                           fluidRow(column(4, offset = 8,
                                           actionButton("overwrite_indole_quant", "Overwrite", width='200px'),
                                           tags$style(type="text/css", "#overwrite_indole_quant {background-color:red;color: white}"))),
                           
                           br()
)