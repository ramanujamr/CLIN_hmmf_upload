# TMS - Qual ===========================================================================================

tab_tms_qual = tabPanel("TMS - qual", fluid=TRUE,
                          
                          ## Row 1/3 Filename ---------------------------------------------------------------------------------
                          
                          fluidRow(column(12,
                                          h4("FILE NAME:"),
                                          h5("Ensure the file name is as per standard convention"),
                                          h5("removed_qcs_normalized_results_yyyymmdd_TMS_XXXX###_yyyymmdd"),
                                          h5("Example: removed_qcs_normalized_results_20201214_TMS_CLIN012_20211206")
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
                                          DT::dataTableOutput("table_tms_qual")
                          )
                          ), 
                          
                          hr(style = "border-top: 1px solid #000000;"),
                          
                          ## Row 3/3 Buttons ----------------------------------------------------------------------------------
                          
                          fluidRow(column(12,
                                          h4("UPLOAD:")
                          )
                          ),
                          
                          fluidRow(column(4,
                                          fileInput("inFile_tms_qual", "Choose CSV File", multiple = F,
                                                    accept = ".csv", placeholder = "No file selected"),
                                          textOutput("filename_tms_qual")
                          ),
                          column(4, actionButton("check_tms_qual", "Check", width='200px'), textOutput("columns_tms_qual")),
                          column(4, actionButton("upload_tms_qual", "Upload to Postgress", width='200px'),  textOutput("upload_status_tms_qual"))
                          ),
                          br()
)