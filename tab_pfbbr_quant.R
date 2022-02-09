# PFBBr - Quant ===============================================================================================

tab_pfbbr_quant = tabPanel("PFBBr - Quant", fluid=TRUE,
         
         ### 1.2.1 Row 1/3 Filename --------------------------------------------------------------------------------
         
         fluidRow(column(12,
                         h4("FILE NAME:"),
                         h5("Ensure the file name is as per standard convention"),
                         h5("removed_qcs_quant_results_yyyymmdd_PFBBr_XXXX###_yyyymmdd"),
                         h5("Example: removed_qcs_quant_results_20201214_PFBBr_CLIN012_20211206")
         )
         ),
         
         hr(style = "border-top: 1px solid #000000;"),
         
         ### 1.2.2 Row 2/3 Column names ----------------------------------------------------------------------------
         
         fluidRow(column(12,
                         h4("COLUMN NAMES:"),
                         h5("Ensure columns are exactly as per the standard list")
         )
         ),
         
         fluidRow(column(4, 
                         DT::dataTableOutput("table_pfbbr_quant")
         )
         ),
         
         hr(style = "border-top: 1px solid #000000;"),
         
         ### 1.2.3 Row 3/3 Buttons ---------------------------------------------------------------------------------
         
         fluidRow(column(12,
                         h4("UPLOAD:")
         )
         ),
         
         fluidRow(column(4, 
                         fileInput("inFile_pfbbr_quant", "Choose CSV File", multiple = F,
                                   accept = ".csv", placeholder = "No file selected"),
                         tags$style(".progress-bar {background-color:green}"),
                         textOutput("filename_pfbbr_quant")
         ),
         column(4, actionButton("check_pfbbr_quant", "Check", width='200px'), textOutput("columns_pfbbr_quant")),
         column(4, actionButton("upload_pfbbr_quant", "Upload to Postgress", width='200px'),  textOutput("upload_status_pfbbr_quant"))
         ),
         br(),
         fluidRow(column(4, offset = 8,
                         actionButton("overwrite_pfbbr_quant", "Overwrite", width='200px'),
                         tags$style(type="text/css", "#overwrite_pfbbr_quant {background-color:red;color: white}"))),
         
         br()
)