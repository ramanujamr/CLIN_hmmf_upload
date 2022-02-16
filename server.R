
server <- function(input, output, session)
{
  
  ######################################################################################################################
  
  # 0. Initialization ####
  ## 0.1 Hide check and upload buttons =================================================================================

  shinyjs::hide("check_pfbbr_qual")
  shinyjs::hide("upload_pfbbr_qual")
  shinyjs::hide("overwrite_pfbbr_qual")
  
  shinyjs::hide("check_pfbbr_quant")
  shinyjs::hide("upload_pfbbr_quant")
  shinyjs::hide("overwrite_pfbbr_quant")
    
  shinyjs::hide("check_tms_qual")
  shinyjs::hide("upload_tms_qual")
  shinyjs::hide("overwrite_tms_qual")
  
  shinyjs::hide("check_bile_qual")
  shinyjs::hide("upload_bile_qual")
  shinyjs::hide("overwrite_bile_qual")
  
  shinyjs::hide("check_bile_quant")
  shinyjs::hide("upload_bile_quant")
  shinyjs::hide("overwrite_bile_quant")
  
  shinyjs::hide("check_indole_qual")
  shinyjs::hide("upload_indole_qual")
  shinyjs::hide("overwrite_indole_qual")
  
  shinyjs::hide("check_indole_quant")
  shinyjs::hide("upload_indole_quant")
  shinyjs::hide("overwrite_indole_quant")
  
  
  
  ## 0.2 Output permitted columns tables ===============================================================================
  
  output$table_pfbbr_qual = DT::renderDataTable(df_permitted_columns_pfbbr_qual, rownames = FALSE, 
                                                options = list(pageLength = 10))
  output$table_pfbbr_quant = DT::renderDataTable(df_permitted_columns_pfbbr_quant, rownames = FALSE, 
                                                 options = list(pageLength = 10))
  output$table_tms_qual = DT::renderDataTable(df_permitted_columns_tms_qual, rownames = FALSE, 
                                              options = list(pageLength = 10))
  output$table_bile_qual = DT::renderDataTable(df_permitted_columns_bile_qual, rownames = FALSE, 
                                               options = list(pageLength = 10))
  output$table_bile_quant = DT::renderDataTable(df_permitted_columns_bile_quant, rownames = FALSE, 
                                                options = list(pageLength = 10))
  output$table_indole_qual = DT::renderDataTable(df_permitted_columns_indole_qual, rownames = FALSE, 
                                                 options = list(pageLength = 10))
  output$table_indole_quant = DT::renderDataTable(df_permitted_columns_indole_quant, rownames = FALSE, 
                                                  options = list(pageLength = 10))
  
  
  ## 0.3 Upload stats plots ============================================================================================
  
  observeEvent(input$select_plot, 
               {
                 
                 df = read.csv("metabolomics_postgress_upload_log.csv")
                 df <- df %>% group_by(batch, panel) %>% arrange(desc(date), desc(time)) %>% dplyr::slice(1) %>% select(!starts_with("X"))
                 
                 df$sample_count <- as.numeric(df$sample_count)
                 df$submission_date = str_match(df$filename, pattern="(?!_results_)([0-9]{8})(?=_)")[, 2]
                 df$submission_date = str_replace(df$submission_date, pattern="(\\d{2})(\\d{2})(\\d{2})$", "\\1-\\2-\\3")
                 
                 df$batch = paste0(df$batch," ", df$submission_date)
                 
                 df = df %>% group_by(panel, batch) %>% summarise(sample_count = sum(sample_count))
                 
                 df = mutate(df, batch = toupper(batch))
                 
                 desired_order = c("PFBBr - qual", "PFBBr - quant", "TMS - qual", "Bile - qual", "Bile - quant" ,
                                   "Indole - qual", "Indole - quant")
                 
                 df_pfbbr = df[df$panel %in% c('PFBBr - qual','PFBBr - quant'),]
                 df_tms = df[df$panel %in% c('TMS - qual'),]
                 df_bile = df[df$panel %in% c('Bile - qual','Bile - quant'),]
                 df_indole = df[df$panel %in% c('Indole - qual','Indole - quant'),]
                 
                 
                 
                 
                 
                 if(input$select_plot %in% "All panels")
                 {
                   output$plot <- renderPlot({
                     
                     ggplot(df, aes(x=batch, y=sample_count, fill=factor(panel, levels=desired_order))) +
                       geom_col(position = 'stack', colour = 'black') +
                       ggtitle("Overall") +
                       xlab("Batch") +
                       ylab("# samples") +
                       theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1)) +
                       scale_fill_manual(name = "Panel",
                                           values = all_panel_colors)  
                   })
                 }
                 
                 
                 else if(input$select_plot %in% "PFBBr")
                 {
                   output$plot <- renderPlot({
                     
                     ggplot(df_pfbbr, aes(x=batch, y=sample_count, fill=panel)) +
                       geom_col(position = position_dodge(preserve = "single"), color='black') +
                       ggtitle("PFBBr") +
                       xlab("Batch") +
                       ylab("# samples") +
                       theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1)) +
                       scale_fill_manual(name = "Type", values = pfbbr_colors) +
                       coord_cartesian(ylim = c(0, 100)) 
                     
                   })
                 }
                 
                 
                 else if(input$select_plot %in% "TMS")
                 {
                   output$plot <- renderPlot({
                     
                     ggplot(df_tms, aes(x=batch, y=sample_count, fill=panel)) +
                       geom_col(position = position_dodge(preserve = "single"), color='black') +
                       ggtitle("TMS") +
                       xlab("Batch") +
                       ylab("# samples") +
                       theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1)) +
                       scale_fill_manual(name = "Type", values = tms_colors) + 
                       coord_cartesian(ylim = c(0, 100)) 
                     
                   })
                 }
                 
                 else if(input$select_plot %in% "Bile acids")
                 {
                   output$plot <- renderPlot({
                     
                     ggplot(df_bile, aes(x=batch, y=sample_count, fill=panel)) +
                       geom_col(position = position_dodge(preserve = "single"), color='black') +
                       ggtitle("Bile acids") +
                       xlab("Batch") +
                       ylab("# samples") +
                       theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1)) +
                       scale_fill_manual(name = "Type", values = bile_colors) + 
                       coord_cartesian(ylim = c(0, 100)) 
                     
                   })
                 }
                 
                 else if(input$select_plot %in% "Indole")
                 {
                   output$plot <- renderPlot({
                     
                     ggplot(df_indole, aes(x=batch, y=sample_count, fill=panel)) +
                       geom_col(position = position_dodge(preserve = "single"), color='black') +
                       ggtitle("Indole") +
                       xlab("Batch") +
                       ylab("# samples") +
                       theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1)) +
                       scale_fill_manual(name = "Type", values = indole_colors) + 
                       coord_cartesian(ylim = c(0, 100)) 
                     
                   })
                 }
                 
               }
  )
  

  
  
  ######################################################################################################################
  
  # 1. PFBBr - qual ####
  ## 1.1 Check file name ===============================================================================================
  
  check_file_name_pfbbr_qual <- reactive({ #'*PANEL SPECIFIC*
    
    file <- input$inFile_pfbbr_qual #'*PANEL SPECIFIC*
    ext <- tools::file_ext(file$datapath)

    # Validate if the file extension is csv
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    # Validate if the file name is proper
    name = file$name
    match = grepl(x = name,
                  pattern = "removed_qcs_normalized_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_PFBBr_[CE][LP][IH][ND][0-9]+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv|removed_qcs_normalized_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_PFBBr_EPCK015+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv", ignore.case = TRUE) #'*PANEL SPECIFIC*
    
    
    if (match)
    {
      # Check for duplicate file name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      
      if(any(df_log["filename"]==name))
      {
        shinyalert(title = "File name already exists", type = "error")
        shinyjs::hide("check_pfbbr_qual") #'*PANEL SPECIFIC*
        shinyjs::hide("upload_pfbbr_qual") #'*PANEL SPECIFIC*
        shinyjs::hide("overwrite_pfbbr_qual")
        return("File name already exists")
      }
      
      # If filename passes all the checks:
      else
      {
        shinyalert(title = "Filename looks good!", type = "success")
        shinyjs::show("check_pfbbr_qual") #'*PANEL SPECIFIC*
        return("File name OK")
      }
    }
    
    else 
    {
      
      shinyalert(title = "Invalid file name", type = "error")
      shinyjs::hide("check_pfbbr_qual") #'*PANEL SPECIFIC*
      shinyjs::hide("upload_pfbbr_qual") #'*PANEL SPECIFIC*
      shinyjs::hide("overwrite_pfbbr_qual")
      return("Invalid file name")
      
    }
  })
  
  output$filename_pfbbr_qual <- renderText(check_file_name_pfbbr_qual()) #'*PANEL SPECIFIC*
  
  
  ## 1.2 Check column names ============================================================================================
  
  observeEvent(input$check_pfbbr_qual, { #'*PANEL SPECIFIC*
    
    file = input$inFile_pfbbr_qual #'*PANEL SPECIFIC*
    req(file)
    df = read.csv(file$datapath)
    columns = colnames(df)
    columns = lapply(columns, tolower)
    
    if (all(columns %in% permitted_columns_pfbbr_qual) == FALSE) #'*PANEL SPECIFIC*
    {
      '%ni%' = Negate('%in%')
      incorrect_columns = columns[columns %ni% permitted_columns_pfbbr_qual] #'*PANEL SPECIFIC*
      incorrect_columns = gsub(x = incorrect_columns, pattern = "^x(?=[0-9])", replacement = "", perl = T)
      shinyalert("Following column names were not recognized:",incorrect_columns)
      output$columns_pfbbr_qual <- renderText("Invalid columns") #'*PANEL SPECIFIC*
    }
    
    else
    {
      shinyalert(title = "Column names look good!", type = "success")
      shinyjs::show("upload_pfbbr_qual") #'*PANEL SPECIFIC*
      output$columns_pfbbr_qual <- renderText("Columns OK") #'*PANEL SPECIFIC*
    }
    
  })
  
  
  ## 1.3 Upload to postgres ============================================================================================
  
  observeEvent(input$upload_pfbbr_qual, { #'*PANEL SPECIFIC*
    
    file = input$inFile_pfbbr_qual #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    # CHECK FOR DUPLICATE FILENAME
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    if(any(df_log["panel"]=="PFBBr - qual" & df_log["batch"]==batch) == TRUE) #'*PANEL SPECIFIC*
    {
      shinyalert(title = "File name already exists", type = "error")
      output$upload_status_pfbbr_qual <- renderText("File name already exists") #'*PANEL SPECIFIC*
      shinyjs::show("overwrite_pfbbr_qual")
    }
    
    else
    {
      
      # APPEND TO MASTER DATAFRAME
      
      con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                      user="dfi_admin", password="dfibugs")
      
      df_upload <- 
        df %>% 
        rename(metabolomicsID = sampleid) %>% 
        pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
        mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
               compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
               compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
               filename = name,
               date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
               batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
               type = "normalized") %>% #'*PANEL SPECIFIC*
        select(metabolomicsID, date_run, batch, type, filename, compound, value)
      
      
      dbWriteTable(con, "scfa_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_lab") #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_user") #'*PANEL SPECIFIC*
      
      
      # Update Log file
      
      name = file$name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      df_log[is.na(df_log)] = 0
      
      df_log1 <- data.frame(filename = name,
                            date = format(Sys.Date(),format = "%Y%m%d"),
                            time = format(Sys.time(),format = "%H:%M:%S"),
                            panel = "PFBBr - qual", #'*PANEL SPECIFIC*
                            batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                            sample_count = nrow(df)
      )
      df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
      
      write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
      
      shinyalert(title = "Upload successful", type = "success")
      output$upload_status_pfbbr_qual <- renderText("Upload successful") #'*PANEL SPECIFIC*
      
      dbDisconnect(con)
      
    }
    
  })
  
  
  observeEvent(input$overwrite_pfbbr_qual, {#'*PANEL SPECIFIC*
    
    file = input$inFile_pfbbr_qual #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch_current = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
  
    con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                    user="dfi_admin", password="dfibugs")
    
    postgres_table = tbl(con,"scfa_v3") %>% filter(!(batch == batch_current & type == "normalized")) %>% collect() 
    
    dbWriteTable(con, "scfa_v3", postgres_table, row.names = F, append = F, overwrite=T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_user") #'*PANEL SPECIFIC*
    
    
    df_upload <- 
      df %>% 
      rename(metabolomicsID = sampleid) %>% 
      pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
      mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
             compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
             compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
             filename = name,
             date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
             batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
             type = "normalized") %>% #'*PANEL SPECIFIC*
      select(metabolomicsID, date_run, batch, type, filename, compound, value)
    
    
    dbWriteTable(con, "scfa_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_user") #'*PANEL SPECIFIC*
    dbDisconnect(con)
    
    # UPDATE LOG FILE
    
    name = file$name
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    df_log1 <- data.frame(filename = name,
                          date = format(Sys.Date(),format = "%Y%m%d"),
                          time = format(Sys.time(),format = "%H:%M:%S"),
                          panel = "PFBBr - qual", #'*PANEL SPECIFIC*
                          batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                          sample_count = nrow(df)
    )
    df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
    
    write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)

    shinyalert(title = "Upload successful", type = "success")
    output$upload_status_pfbbr_qual <- renderText("Upload successful") #'*PANEL SPECIFIC*
    
  })
  
  
  
  ######################################################################################################################
  
  # 2. PFBBr - quant ####
  ## 2.1 Check file name ===============================================================================================
  
  check_file_name_pfbbr_quant <- reactive({ #'*PANEL SPECIFIC*
    
    file <- input$inFile_pfbbr_quant #'*PANEL SPECIFIC*
    ext <- tools::file_ext(file$datapath)
    
    # Validate if the file extension is csv
    validate(need(ext == "csv", "Please upload a csv file"))
    
    # Validate if the file name is proper
    name = file$name
    match = grepl(x = name,
                  pattern = "removed_qcs_quant_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_PFBBr_[CE][LP][IH][ND][0-9]+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv|removed_qcs_quant_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_PFBBr_EPCK015+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv", ignore.case = TRUE) #'*PANEL SPECIFIC*
    
    if (match == FALSE)
    {
      shinyalert(title = "Invalid file name", type = "error")
      shinyjs::hide("check_pfbbr_quant") #'*PANEL SPECIFIC*
      shinyjs::hide("upload_pfbbr_quant") #'*PANEL SPECIFIC*
      return("Invalid file name")
    }
    
    else 
    {
      # Check for duplicate file name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      
      if(any(df_log["filename"]==name) == TRUE)
      {
        shinyalert(title = "File name already exists", type = "error")
        shinyjs::hide("check_pfbbr_quant") #'*PANEL SPECIFIC*
        shinyjs::hide("upload_pfbbr_quant") #'*PANEL SPECIFIC*
        return("File name already exists")
      }
      
      # If filename passes all the checks:
      else
      {
        shinyalert(title = "Filename looks good!", type = "success")
        shinyjs::show("check_pfbbr_quant") #'*PANEL SPECIFIC*
        return("File name OK")
      }
      
    }
  })
  
  output$filename_pfbbr_quant <- renderText(check_file_name_pfbbr_quant()) #'*PANEL SPECIFIC*
  
  
  ## 2.2 Check column names ============================================================================================
  
  observeEvent(input$check_pfbbr_quant, { #'*PANEL SPECIFIC*
    
    file = input$inFile_pfbbr_quant #'*PANEL SPECIFIC*
    req(file)
    df = read.csv(file$datapath)
    columns = colnames(df)
    columns = lapply(columns, tolower)
    
    if (all(columns %in% permitted_columns_pfbbr_quant) == FALSE) #'*PANEL SPECIFIC*
    {
      '%ni%' = Negate('%in%')
      incorrect_columns = columns[columns %ni% permitted_columns_pfbbr_quant] #'*PANEL SPECIFIC*
      incorrect_columns = gsub(x = incorrect_columns, pattern = "^x(?=[0-9])", replacement = "", perl = T)
      shinyalert("Following column names were not recognized:",incorrect_columns)
      output$columns_pfbbr_quant <- renderText("Invalid columns") #'*PANEL SPECIFIC*
    }
    
    else
    {
      shinyalert(title = "Column names look good!", type = "success")
      shinyjs::show("upload_pfbbr_quant") #'*PANEL SPECIFIC*
      output$columns_pfbbr_quant <- renderText("Columns OK") #'*PANEL SPECIFIC*
    }
    
  })
  
  ## 2.3 Upload to postgres ============================================================================================
  
  observeEvent(input$upload_pfbbr_quant, { #'*PANEL SPECIFIC*
    
    file = input$inFile_pfbbr_quant #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    # CHECK FOR DUPLICATE FILENAME
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    if(any(df_log["panel"]=="PFBBr - quant" & df_log["batch"]==batch) == TRUE) #'*PANEL SPECIFIC*
    {
      shinyalert(title = "File name already exists", type = "error")
      output$upload_status_pfbbr_quant <- renderText("File name already exists") #'*PANEL SPECIFIC*
      shinyjs::show("overwrite_pfbbr_quant") #'*PANEL SPECIFIC*
    }
    
    
    # APPEND TO MASTER DATAFRAME
    else
    {
      con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                      user="dfi_admin", password="dfibugs")
      
      df_upload <- 
        df %>% 
        rename(metabolomicsID = sampleid) %>% 
        pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
        mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
               compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
               compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
               filename = name,
               date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
               batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
               type = "quant") %>% #'*PANEL SPECIFIC*
        select(metabolomicsID, date_run, batch, type, filename, compound, value)
      
      
      dbWriteTable(con, "scfa_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_lab") #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_user") #'*PANEL SPECIFIC*
      
      
      # UPDATE LOG FILE
      
      name = file$name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      df_log[is.na(df_log)] = 0
      
      df_log1 <- data.frame(filename = name,
                            date = format(Sys.Date(),format = "%Y%m%d"),
                            time = format(Sys.time(),format = "%H:%M:%S"),
                            panel = "PFBBr - quant", #'*PANEL SPECIFIC*
                            batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                            sample_count = nrow(df)
      )
      df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
      
      write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
      
      shinyalert(title = "Upload successful", type = "success")
      output$upload_status_pfbbr_quant <- renderText("Upload successful") #'*PANEL SPECIFIC*
      
      dbDisconnect(con)
      
      
    }
    
    
  })
  
  observeEvent(input$overwrite_pfbbr_quant, {#'*PANEL SPECIFIC*
    
    file = input$inFile_pfbbr_quant #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch_current = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                    user="dfi_admin", password="dfibugs")
    
    postgres_table = tbl(con,"scfa_v3") %>% filter(!(batch == batch_current & type == "quant")) %>% collect() #'*PANEL SPECIFIC*
    
    dbWriteTable(con, "scfa_v3", postgres_table, row.names = F, append = F, overwrite=T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_user") #'*PANEL SPECIFIC*
    
    
    df_upload <- 
      df %>% 
      rename(metabolomicsID = sampleid) %>% 
      pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
      mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
             compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
             compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
             filename = name,
             date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
             batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
             type = "quant") %>% #'*PANEL SPECIFIC*
      select(metabolomicsID, date_run, batch, type, filename, compound, value)
    
    
    dbWriteTable(con, "scfa_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON scfa_v3 TO dfi_user") #'*PANEL SPECIFIC*
    dbDisconnect(con)
    
    # UPDATE LOG FILE
    
    name = file$name
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    df_log1 <- data.frame(filename = name,
                          date = format(Sys.Date(),format = "%Y%m%d"),
                          time = format(Sys.time(),format = "%H:%M:%S"),
                          panel = "PFBBr - quant", #'*PANEL SPECIFIC*
                          batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                          sample_count = nrow(df)
    )
    df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
    
    write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
    
    shinyalert(title = "Upload successful", type = "success")
    output$upload_status_pfbbr_quant <- renderText("Upload successful") #'*PANEL SPECIFIC*
    
  })
  
  
  
  
  
  
  
  ######################################################################################################################
  
  # 3. TMS - qual ####
  ## 3.1 Check file name ===============================================================================================
  
  check_file_name_tms_qual <- reactive({ #'*PANEL SPECIFIC*
    
    file <- input$inFile_tms_qual #'*PANEL SPECIFIC*
    ext <- tools::file_ext(file$datapath)
    
    # Validate if the file extension is csv
    validate(need(ext == "csv", "Please upload a csv file"))
    
    # Validate if the file name is proper
    name = file$name
    match = grepl(x = name,
                  pattern = "removed_qcs_normalized_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_TMS_[CE][LP][IH][ND][0-9]+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv|removed_qcs_normalized_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_TMS_EPCK015+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv", ignore.case = TRUE) #'*PANEL SPECIFIC*
    
    if (match == FALSE)
    {
      shinyalert(title = "Invalid file name", type = "error")
      shinyjs::hide("check_tms_qual") #'*PANEL SPECIFIC*
      shinyjs::hide("upload_tms_qual") #'*PANEL SPECIFIC*
      shinyjs::hide("overwrite_tms_qual") #'*PANEL SPECIFIC*
      return("Invalid file name")
    }
    
    else 
    {
      # Check for duplicate file name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      
      if(any(df_log["filename"]==name) == TRUE)
      {
        shinyalert(title = "File name already exists", type = "error")
        shinyjs::hide("check_tms_qual") #'*PANEL SPECIFIC*
        shinyjs::hide("upload_tms_qual") #'*PANEL SPECIFIC*
        shinyjs::hide("overwrite_tms_qual") #'*PANEL SPECIFIC*
        return("File name already exists")
      }
      
      # If filename passes all the checks:
      else
      {
        shinyalert(title = "Filename looks good!", type = "success")
        shinyjs::show("check_tms_qual") #'*PANEL SPECIFIC*
        return("File name OK")
      }
      
    }
  })
  
  output$filename_tms_qual <- renderText(check_file_name_tms_qual()) #'*PANEL SPECIFIC*
  
  
  ## 3.2 Check column names ============================================================================================
  
  observeEvent(input$check_tms_qual, { #'*PANEL SPECIFIC*
    
    file = input$inFile_tms_qual #'*PANEL SPECIFIC*
    req(file)
    df = read.csv(file$datapath)
    columns = colnames(df)
    columns = lapply(columns, tolower)
    
    if (all(columns %in% permitted_columns_tms_qual) == FALSE) #'*PANEL SPECIFIC*
    {
      '%ni%' = Negate('%in%')
      incorrect_columns = columns[columns %ni% permitted_columns_tms_qual] #'*PANEL SPECIFIC*
      incorrect_columns = gsub(x = incorrect_columns, pattern = "^x(?=[0-9])", replacement = "", perl = T)
      shinyalert("Following column names were not recognized:",incorrect_columns)
      output$columns_tms_qual <- renderText("Invalid columns") #'*PANEL SPECIFIC*
    }
    
    else
    {
      shinyalert(title = "Column names look good!", type = "success")
      shinyjs::show("upload_tms_qual") #'*PANEL SPECIFIC*
      output$columns_tms_qual <- renderText("Columns OK") #'*PANEL SPECIFIC*
    }
    
  })
  
  
  ## 3.3 Upload to postgres ============================================================================================
  
  observeEvent(input$upload_tms_qual, { #'*PANEL SPECIFIC*
    
    file = input$inFile_tms_qual #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    # CHECK FOR DUPLICATE FILENAME
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    if(any(df_log["panel"]=="TMS - qual" & df_log["batch"]==batch) == TRUE) #'*PANEL SPECIFIC*
    {
      shinyalert(title = "File name already exists", type = "error")
      output$upload_status_tms_qual <- renderText("File name already exists") #'*PANEL SPECIFIC*
      shinyjs::show("overwrite_tms_qual") #'*PANEL SPECIFIC*
    }
      
    else
    {
      
      # APPEND TO MASTER DATAFRAME
      
      con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                      user="dfi_admin", password="dfibugs")
      
      df_upload <- 
        df %>% 
        rename(metabolomicsID = sampleid) %>% 
        pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
        mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
               compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
               compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
               filename = name,
               date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
               batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
               type = "normalized") %>% #'*PANEL SPECIFIC*
        select(metabolomicsID, date_run, batch, type, filename, compound, value)
      
      dbWriteTable(con, "tms_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON tms_v3 TO dfi_lab") #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON tms_v3 TO dfi_user") #'*PANEL SPECIFIC*
      
      
      # UPDATE LOG FILE
      
      name = file$name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      df_log[is.na(df_log)] = 0
      
      df_log1 <- data.frame(filename = name,
                            date = format(Sys.Date(),format = "%Y%m%d"),
                            time = format(Sys.time(),format = "%H:%M:%S"),
                            panel = "TMS - qual", #'*PANEL SPECIFIC*
                            batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                            sample_count = nrow(df)
      )
      df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
      
      write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
      
      shinyalert(title = "Upload successful", type = "success")
      output$upload_status_tms_qual <- renderText("Upload successful") #'*PANEL SPECIFIC*
      
      dbDisconnect(con)
      
    }
    
  })
  
  observeEvent(input$overwrite_tms_qual, {#'*PANEL SPECIFIC*
    
    file = input$inFile_tms_qual #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch_current = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                    user="dfi_admin", password="dfibugs")
    
    postgres_table = tbl(con,"tms_v3") %>% filter(!(batch == batch_current & type == "normalized")) %>% collect() #'*PANEL SPECIFIC*
    
    dbWriteTable(con, "tms_v3", postgres_table, row.names = F, append = F, overwrite=T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON tms_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON tms_v3 TO dfi_user") #'*PANEL SPECIFIC*
    
    
    df_upload <- 
      df %>% 
      rename(metabolomicsID = sampleid) %>% 
      pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
      mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
             compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
             compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
             filename = name,
             date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
             batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
             type = "normalized") %>% #'*PANEL SPECIFIC*
      select(metabolomicsID, date_run, batch, type, filename, compound, value)
    
    
    dbWriteTable(con, "tms_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON tms_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON tms_v3 TO dfi_user") #'*PANEL SPECIFIC*
    dbDisconnect(con)
    
    # UPDATE LOG FILE
    
    name = file$name
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    df_log1 <- data.frame(filename = name,
                          date = format(Sys.Date(),format = "%Y%m%d"),
                          time = format(Sys.time(),format = "%H:%M:%S"),
                          panel = "TMS - qual", #'*PANEL SPECIFIC*
                          batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                          sample_count = nrow(df)
    )
    df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
    
    write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
    
    shinyalert(title = "Upload successful", type = "success")
    output$upload_status_tms_qual <- renderText("Upload successful") #'*PANEL SPECIFIC*
    
  })
  
  
  
  
  
  
  ######################################################################################################################
  
  # 4. Bile acid - qual ####
  ## 4.1 Check file name ===============================================================================================
  
  check_file_name_bile_qual <- reactive({ #'*PANEL SPECIFIC*
    
    file <- input$inFile_bile_qual #'*PANEL SPECIFIC*
    ext <- tools::file_ext(file$datapath)
    
    # Validate if the file extension is csv
    validate(need(ext == "csv", "Please upload a csv file"))
    
    # Validate if the file name is proper
    name = file$name
    match = grepl(x = name,
                  pattern = "removed_qcs_normalized_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_BileAcid_[CE][LP][IH][ND][0-9]+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv|removed_qcs_normalized_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_BileAcid_EPCK015+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv", ignore.case = TRUE) #'*PANEL SPECIFIC*
    
    if (match == FALSE)
    {
      shinyalert(title = "Invalid file name", type = "error")
      shinyjs::hide("check_bile_qual") #'*PANEL SPECIFIC*
      shinyjs::hide("upload_bile_qual") #'*PANEL SPECIFIC*
      shinyjs::hide("overwrite_bile_qual") #'*PANEL SPECIFIC*
      return("Invalid file name")
    }
    
    else 
    {
      # Check for duplicate file name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      
      if(any(df_log["filename"]==name) == TRUE)
      {
        shinyalert(title = "File name already exists", type = "error")
        shinyjs::hide("check_bile_qual") #'*PANEL SPECIFIC*
        shinyjs::hide("upload_bile_qual") #'*PANEL SPECIFIC*
        shinyjs::hide("overwrite_bile_qual") #'*PANEL SPECIFIC*
        return("File name already exists")
      }
      
      # If filename passes all the checks:
      else
      {
        shinyalert(title = "Filename looks good!", type = "success")
        shinyjs::show("check_bile_qual") #'*PANEL SPECIFIC*
        return("File name OK")
      }
      
    }
  })
  
  output$filename_bile_qual <- renderText(check_file_name_bile_qual()) #'*PANEL SPECIFIC*
  
  
  ## 4.2 Check column names ============================================================================================
  
  observeEvent(input$check_bile_qual, { #'*PANEL SPECIFIC*
    
    file = input$inFile_bile_qual
    req(file)
    df = read.csv(file$datapath)
    columns = colnames(df)
    columns = lapply(columns, tolower)
    
    # Replace ambiguous compounds with a common name
    columns = str_replace(columns, "x6.oxolithocholic.acid", 
                          "x7.oxo.or.6.oxolithocholic.acid")
    
    columns = str_replace(columns, "x7.oxolithocholic.acid", 
                          "x7.oxo.or.6.oxolithocholic.acid")
    
    columns = str_replace(columns, "x6.oxolithocholic.acid.or.7.oxolithocholic.acid", 
                          "x7.oxo.or.6.oxolithocholic.acid")
    
    columns = str_replace(columns, "x7.oxo.or.6.oxo.lithocholic.acid", 
                          "x7.oxo.or.6.oxolithocholic.acid")
    
    columns = str_replace(columns, "tauro.alpha.muricholic.acid.or.tauro.beta.muricholic.acid", 
                          "tauro.alpha.or.tauro.beta.muricholic.acid")
    
  
    
    # If columns don't match, output invalid column names
    if (all(columns %in% permitted_columns_bile_qual) == FALSE) #'*PANEL SPECIFIC*
    {
      '%ni%' = Negate('%in%')
      incorrect_columns = columns[columns %ni% permitted_columns_bile_qual] #'*PANEL SPECIFIC*
      incorrect_columns = gsub(x = incorrect_columns, pattern = "^x(?=[0-9])", replacement = "", perl = T)
      shinyalert("Following column names were not recognized:",incorrect_columns)
      output$columns_bile_qual <- renderText("Invalid columns") #'*PANEL SPECIFIC*
    }
    
    # If columns match, display upload button
    else
    {
      shinyalert(title = "Column names look good!", type = "success")
      shinyjs::show("upload_bile_qual") #'*PANEL SPECIFIC*
      output$columns_bile_qual <- renderText("Columns OK") #'*PANEL SPECIFIC*
    }
    
  })
  
  
  
  ## 4.3 Upload to postgres ============================================================================================
  
  observeEvent(input$upload_bile_qual, { #'*PANEL SPECIFIC*
    
    file = input$inFile_bile_qual #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    # CHECK FOR DUPLICATE FILENAME
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    if(any(df_log["panel"]=="Bile - qual" & df_log["batch"]==batch) == TRUE) #'*PANEL SPECIFIC*
    {
      shinyalert(title = "File name already exists", type = "error")
      output$upload_status_bile_qual <- renderText("File name already exists") #'*PANEL SPECIFIC*
      shinyjs::show("overwrite_bile_qual") #'*PANEL SPECIFIC*
    }

    # APPEND TO MASTER DATAFRAME
    else
    {
      con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                      user="dfi_admin", password="dfibugs")
      
      df_upload <- 
        df %>% 
        rename(metabolomicsID = sampleid) %>% 
        pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
        mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
               compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
               compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
               filename = name,
               date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
               batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
               type = "normalized") %>% #'*PANEL SPECIFIC*
        select(metabolomicsID, date_run, batch, type, filename, compound, value)
      
      
      dbWriteTable(con, "bile_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_lab") #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_user") #'*PANEL SPECIFIC*
      
      
      # UPDATE LOG FILE
      
      name = file$name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      df_log[is.na(df_log)] = 0
      
      df_log1 <- data.frame(filename = name,
                            date = format(Sys.Date(),format = "%Y%m%d"),
                            time = format(Sys.time(),format = "%H:%M:%S"),
                            panel = "Bile - qual", #'*PANEL SPECIFIC*
                            batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                            sample_count = nrow(df)
      )
      df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
      
      write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
      
      shinyalert(title = "Upload successful", type = "success")
      output$upload_status_bile_qual <- renderText("Upload successful") #'*PANEL SPECIFIC*
      
      dbDisconnect(con)
  
      
    }

    
  })
  
  
  
  observeEvent(input$overwrite_bile_qual, {#'*PANEL SPECIFIC*
    
    file = input$inFile_bile_qual #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch_current = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                    user="dfi_admin", password="dfibugs")
    
    # Remove similar data
    
    postgres_table = tbl(con,"bile_v3") %>% filter(!(batch == batch_current & type == "normalized")) %>% collect() #'*PANEL SPECIFIC*
    
    dbWriteTable(con, "bile_v3", postgres_table, row.names = F, append = F, overwrite=T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_user") #'*PANEL SPECIFIC*
    
    
    df_upload <- 
      df %>% 
      rename(metabolomicsID = sampleid) %>% 
      pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
      mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
             compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
             compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
             filename = name,
             date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
             batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
             type = "normalized") %>% #'*PANEL SPECIFIC*
      select(metabolomicsID, date_run, batch, type, filename, compound, value)
    
    
    dbWriteTable(con, "bile_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_user") #'*PANEL SPECIFIC*
    dbDisconnect(con)
    
    # UPDATE LOG FILE
    
    name = file$name
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    df_log1 <- data.frame(filename = name,
                          date = format(Sys.Date(),format = "%Y%m%d"),
                          time = format(Sys.time(),format = "%H:%M:%S"),
                          panel = "Bile - qual", #'*PANEL SPECIFIC*
                          batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                          sample_count = nrow(df)
    )
    df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
    
    write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
    
    shinyalert(title = "Upload successful", type = "success")
    output$upload_status_bile_qual <- renderText("Upload successful") #'*PANEL SPECIFIC*
    
  })
  
  
  
  
  
  ######################################################################################################################
  
  # 5. Bile acid - quant ####
  ## 5.1 Check file name ===============================================================================================
  
  check_file_name_bile_quant <- reactive({ #'*PANEL SPECIFIC*
    
    file <- input$inFile_bile_quant #'*PANEL SPECIFIC*
    ext <- tools::file_ext(file$datapath)
    cat(file=stderr(),file$name)
    cat(file=stderr(),ext)
    
    # Validate if the file extension is csv
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    # Validate if the file name is proper
    name = file$name
    match = grepl(x = name,
                  pattern = "removed_qcs_quant_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_BileAcid_[CE][LP][IH][ND][0-9]+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv|removed_qcs_quant_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_BileAcid_EPCK015+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv", ignore.case = TRUE) #'*PANEL SPECIFIC*
    
    if (match == FALSE)
    {
      shinyalert(title = "Invalid file name", type = "error")
      shinyjs::hide("check_bile_quant") #'*PANEL SPECIFIC*
      shinyjs::hide("upload_bile_quant") #'*PANEL SPECIFIC*
      shinyjs::hide("overwrite_bile_quant") #'*PANEL SPECIFIC*
      return("Invalid file name")
    }
    
    else 
    {
      # Check for duplicate file name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      
      if(any(df_log["filename"]==name) == TRUE)
      {
        shinyalert(title = "File name already exists", type = "error")
        shinyjs::hide("check_bile_quant") #'*PANEL SPECIFIC*
        shinyjs::hide("upload_bile_quant") #'*PANEL SPECIFIC*
        shinyjs::hide("overwrite_bile_quant") #'*PANEL SPECIFIC*
        return("File name already exists")
      }
      
      # If filename passes all the checks:
      else
      {
        shinyalert(title = "Filename looks good!", type = "success")
        shinyjs::show("check_bile_quant") #'*PANEL SPECIFIC*
        return("File name OK")
      }
      
    }
  })
  
  output$filename_bile_quant <- renderText(check_file_name_bile_quant()) #'*PANEL SPECIFIC*
  
  
  ## 5.2 Check column names ============================================================================================
  
  
  observeEvent(input$check_bile_quant, { #'*PANEL SPECIFIC*
    
    file = input$inFile_bile_quant #'*PANEL SPECIFIC*
    req(file)
    df = read.csv(file$datapath)
    columns = colnames(df)
    columns = lapply(columns, tolower)
    
    if (all(columns %in% permitted_columns_bile_quant) == FALSE) #'*PANEL SPECIFIC*
    {
      '%ni%' = Negate('%in%')
      incorrect_columns = columns[columns %ni% permitted_columns_bile_quant] #'*PANEL SPECIFIC*
      incorrect_columns = gsub(x = incorrect_columns, pattern = "^x(?=[0-9])", replacement = "", perl = T)
      shinyalert("Following column names were not recognized:",incorrect_columns)
      output$columns_bile_quant <- renderText("Invalid columns") #'*PANEL SPECIFIC*
    }
    
    else
    {
      shinyalert(title = "Column names look good!", type = "success")
      shinyjs::show("upload_bile_quant") #'*PANEL SPECIFIC*
      output$columns_bile_quant <- renderText("Columns OK") #'*PANEL SPECIFIC*
    }
    
  })
  
  
  
  ## 5.3 Upload to postgres ============================================================================================
  

  
  observeEvent(input$upload_bile_quant, { #'*PANEL SPECIFIC*
    
    file = input$inFile_bile_quant #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    # CHECK FOR DUPLICATE FILENAME
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    if(any(df_log["panel"]=="Bile - quant" & df_log["batch"]==batch) == TRUE) #'*PANEL SPECIFIC*
    {
      shinyalert(title = "File name already exists", type = "error")
      output$upload_status_bile_quant <- renderText("File name already exists") #'*PANEL SPECIFIC*
      shinyjs::show("overwrite_bile_quant") #'*PANEL SPECIFIC*
    }
  
    else
    {
      
      # APPEND TO MASTER DATAFRAME
      
      con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                      user="dfi_admin", password="dfibugs")
      
      df_upload <- 
        df %>% 
        rename(metabolomicsID = sampleid) %>% 
        pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
        mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
               compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
               compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
               filename = name,
               date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
               batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
               type = "quant") %>% #'*PANEL SPECIFIC*
        select(metabolomicsID, date_run, batch, type, filename, compound, value)
      
      
      dbWriteTable(con, "bile_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_lab") #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_user") #'*PANEL SPECIFIC*
      
      
      # UPDATE LOG FILE
      
      name = file$name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      df_log[is.na(df_log)] = 0
      
      df_log1 <- data.frame(filename = name,
                            date = format(Sys.Date(),format = "%Y%m%d"),
                            time = format(Sys.time(),format = "%H:%M:%S"),
                            panel = "Bile - quant", #'*PANEL SPECIFIC*
                            batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                            sample_count = nrow(df)
      )
      df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
      
      write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
      
      shinyalert(title = "Upload successful", type = "success")
      output$upload_status_bile_quant <- renderText("Upload successful") #'*PANEL SPECIFIC*
      
      dbDisconnect(con)
      
    }
    
  })

  
  observeEvent(input$overwrite_bile_quant, {#'*PANEL SPECIFIC*
    
    file = input$inFile_bile_quant #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch_current = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                    user="dfi_admin", password="dfibugs")
    
    postgres_table = tbl(con,"bile_v3") %>% filter(!(batch == batch_current & type == "quant")) %>% collect() #'*PANEL SPECIFIC*
    
    dbWriteTable(con, "bile_v3", postgres_table, row.names = F, append = F, overwrite=T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_user") #'*PANEL SPECIFIC*
    
    
    df_upload <- 
      df %>% 
      rename(metabolomicsID = sampleid) %>% 
      pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
      mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
             compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
             compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
             filename = name,
             date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
             batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
             type = "quant") %>% #'*PANEL SPECIFIC*
      select(metabolomicsID, date_run, batch, type, filename, compound, value)
    
    
    dbWriteTable(con, "bile_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON bile_v3 TO dfi_user") #'*PANEL SPECIFIC*
    dbDisconnect(con)
    
    # UPDATE LOG FILE
    
    name = file$name
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    df_log1 <- data.frame(filename = name,
                          date = format(Sys.Date(),format = "%Y%m%d"),
                          time = format(Sys.time(),format = "%H:%M:%S"),
                          panel = "Bile - quant", #'*PANEL SPECIFIC*
                          batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                          sample_count = nrow(df)
    )
    df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
    
    write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
    
    shinyalert(title = "Upload successful", type = "success")
    output$upload_status_bile_quant <- renderText("Upload successful") #'*PANEL SPECIFIC*
    
  })
  
  
  
  ######################################################################################################################
  
  # 6. Indole - qual ####
  ## 6.1 Check file name ===============================================================================================
  
  check_file_name_indole_qual <- reactive({ #'*PANEL SPECIFIC*
    
    file <- input$inFile_indole_qual #'*PANEL SPECIFIC*
    ext <- tools::file_ext(file$datapath)
    
    # Validate if the file extension is csv
    validate(need(ext == "csv", "Please upload a csv file"))
    
    # Validate if the file name is proper
    name = file$name
    match = grepl(x = name,
                  pattern = "removed_qcs_normalized_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_Indole_[CE][LP][IH][ND][0-9]+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv|removed_qcs_normalized_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_Indole_EPCK015+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv", ignore.case = TRUE) #'*PANEL SPECIFIC*
    
    if (match == FALSE)
    {
      shinyalert(title = "Invalid file name", type = "error")
      shinyjs::hide("check_indole_qual") #'*PANEL SPECIFIC*
      shinyjs::hide("upload_indole_qual") #'*PANEL SPECIFIC*
      shinyjs::hide("overwrite_indole_qual") #'*PANEL SPECIFIC*
      return("Invalid file name")
    }
    
    else 
    {
      # Check for duplicate file name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      
      if(any(df_log["filename"]==name) == TRUE)
      {
        shinyalert(title = "File name already exists", type = "error")
        shinyjs::hide("check_indole_qual") #'*PANEL SPECIFIC*
        shinyjs::hide("upload_indole_qual") #'*PANEL SPECIFIC*
        shinyjs::hide("overwrite_indole_qual") #'*PANEL SPECIFIC*
        return("File name already exists")
      }
      
      # If filename passes all the checks:
      else
      {
        shinyalert(title = "Filename looks good!", type = "success")
        shinyjs::show("check_indole_qual") #'*PANEL SPECIFIC*
        return("File name OK")
      }
      
    }
  })
  
  output$filename_indole_qual <- renderText(check_file_name_indole_qual()) #'*PANEL SPECIFIC*
  
  
  ## 6.2 Check column names ============================================================================================
  
  observeEvent(input$check_indole_qual, { #'*PANEL SPECIFIC*
    
    file = input$inFile_indole_qual #'*PANEL SPECIFIC*
    req(file)
    df = read.csv(file$datapath)
    columns = colnames(df)
    columns = lapply(columns, tolower)
    
    # Replace ambiguous compounds with a common name
    columns = str_replace(columns, "x6.oxolithocholic.acid", 
                          "x7.oxo.or.6.oxolithocholic.acid")
    
    columns = str_replace(columns, "x7.oxolithocholic.acid", 
                          "x7.oxo.or.6.oxolithocholic.acid")
    
    columns = str_replace(columns, "x6.oxolithocholic.acid.or.7.oxolithocholic.acid", 
                          "x7.oxo.or.6.oxolithocholic.acid")
    
    columns = str_replace(columns, "x7.oxo.or.6.oxo.lithocholic.acid", 
                          "x7.oxo.or.6.oxolithocholic.acid")
    
    columns = str_replace(columns, "tauro.alpha.muricholic.acid.or.tauro.beta.muricholic.acid", 
                          "tauro.alpha.or.tauro.beta.muricholic.acid")
    
    
    
    # If columns don't match, output invalid column names
    if (all(columns %in% permitted_columns_indole_qual) == FALSE) #'*PANEL SPECIFIC*
    {
      '%ni%' = Negate('%in%')
      incorrect_columns = columns[columns %ni% permitted_columns_indole_qual] #'*PANEL SPECIFIC*
      incorrect_columns = gsub(x = incorrect_columns, pattern = "^x(?=[0-9])", replacement = "", perl = T)
      shinyalert("Following column names were not recognized:",incorrect_columns)
      output$columns_indole_qual <- renderText("Invalid columns") #'*PANEL SPECIFIC*
    }
    
    # If columns match, display upload button
    else
    {
      shinyalert(title = "Column names look good!", type = "success")
      shinyjs::show("upload_indole_qual") #'*PANEL SPECIFIC*
      output$columns_indole_qual <- renderText("Columns OK") #'*PANEL SPECIFIC*
    }
    
  })
  
  
  
  ## 6.3 Upload to postgres ============================================================================================
  
  observeEvent(input$upload_indole_qual, { #'*PANEL SPECIFIC*
    
    file = input$inFile_indole_qual #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    # CHECK FOR DUPLICATE FILENAME
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    if(any(df_log["panel"]=="Indole - qual" & df_log["batch"]==batch) == TRUE) #'*PANEL SPECIFIC*
    {
      shinyalert(title = "File name already exists", type = "error")
      output$upload_status_indole_qual <- renderText("File name already exists") #'*PANEL SPECIFIC*
      shinyjs::show("overwrite_indole_qual") #'*PANEL SPECIFIC*
    }
    
    # APPEND TO MASTER DATAFRAME
    else
    {
      con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                      user="dfi_admin", password="dfibugs")
      
      df_upload <- 
        df %>% 
        rename(metabolomicsID = sampleid) %>% 
        pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
        mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
               compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
               compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
               filename = name,
               date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
               batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
               type = "normalized") %>% #'*PANEL SPECIFIC*
        select(metabolomicsID, date_run, batch, type, filename, compound, value)
      
      
      dbWriteTable(con, "indole_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_lab") #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_user") #'*PANEL SPECIFIC*
      
      
      # UPDATE LOG FILE
      
      name = file$name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      df_log[is.na(df_log)] = 0
      
      df_log1 <- data.frame(filename = name,
                            date = format(Sys.Date(),format = "%Y%m%d"),
                            time = format(Sys.time(),format = "%H:%M:%S"),
                            panel = "Indole - qual", #'*PANEL SPECIFIC*
                            batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                            sample_count = nrow(df)
      )
      df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
      
      write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
      
      shinyalert(title = "Upload successful", type = "success")
      output$upload_status_indole_qual <- renderText("Upload successful") #'*PANEL SPECIFIC*
      
      
      dbDisconnect(con)
    }
    
    
  })
  
  
  observeEvent(input$overwrite_indole_qual, {#'*PANEL SPECIFIC*
    
    file = input$inFile_indole_qual #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch_current = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                    user="dfi_admin", password="dfibugs")
    
    postgres_table = tbl(con,"indole_v3") %>% filter(!(batch == batch_current & type == "normalized")) %>% collect() #'*PANEL SPECIFIC*
    
    dbWriteTable(con, "indole_v3", postgres_table, row.names = F, append = F, overwrite=T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_user") #'*PANEL SPECIFIC*
    
    
    df_upload <- 
      df %>% 
      rename(metabolomicsID = sampleid) %>% 
      pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
      mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
             compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
             compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
             filename = name,
             date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
             batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
             type = "normalized") %>% #'*PANEL SPECIFIC*
      select(metabolomicsID, date_run, batch, type, filename, compound, value)
    
    
    dbWriteTable(con, "indole_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_user") #'*PANEL SPECIFIC*
    dbDisconnect(con)
    
    # UPDATE LOG FILE
    
    name = file$name
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    df_log1 <- data.frame(filename = name,
                          date = format(Sys.Date(),format = "%Y%m%d"),
                          time = format(Sys.time(),format = "%H:%M:%S"),
                          panel = "Indole - qual", #'*PANEL SPECIFIC*
                          batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                          sample_count = nrow(df)
    )
    df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
    
    write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
    
    shinyalert(title = "Upload successful", type = "success")
    output$upload_status_indole_qual <- renderText("Upload successful") #'*PANEL SPECIFIC*
    
  })
  
  
  
  
  
  
  ######################################################################################################################
  
  # 7. Indole - quant ####
  ## 7.1 Check file name ===============================================================================================
  
  check_file_name_indole_quant <- reactive({ #'*PANEL SPECIFIC*
    
    file <- input$inFile_indole_quant #'*PANEL SPECIFIC*
    ext <- tools::file_ext(file$datapath)
    
    # Validate if the file extension is csv
    validate(need(ext == "csv", "Please upload a csv file"))
    
    # Validate if the file name is proper
    name = file$name
    match = grepl(x = name,
                  pattern = "removed_qcs_quant_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_Indole_[CE][LP][IH][ND][0-9]+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv|removed_qcs_quant_results_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]_Indole_EPCK015+_[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].csv", ignore.case = TRUE) #'*PANEL SPECIFIC*
    
    if (match == FALSE)
    {
      shinyalert(title = "Invalid file name", type = "error")
      shinyjs::hide("check_indole_quant") #'*PANEL SPECIFIC*
      shinyjs::hide("upload_indole_quant") #'*PANEL SPECIFIC*
      shinyjs::hide("overwrite_indole_quant") #'*PANEL SPECIFIC*
      return("Invalid file name")
    }
    
    else 
    {
      # Check for duplicate file name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      
      if(any(df_log["filename"]==name) == TRUE)
      {
        shinyalert(title = "File name already exists", type = "error")
        shinyjs::hide("check_indole_quant") #'*PANEL SPECIFIC*
        shinyjs::hide("upload_indole_quant") #'*PANEL SPECIFIC*
        shinyjs::hide("overwrite_indole_quant") #'*PANEL SPECIFIC*
        return("File name already exists")
      }
      
      # If filename passes all the checks:
      else
      {
        shinyalert(title = "Filename looks good!", type = "success")
        shinyjs::show("check_indole_quant") #'*PANEL SPECIFIC*
        return("File name OK")
      }
      
    }
  })
  
  output$filename_indole_quant <- renderText(check_file_name_indole_quant()) #'*PANEL SPECIFIC*
  
  
  ## 7.2 Check column names ============================================================================================
  
  
  observeEvent(input$check_indole_quant, { #'*PANEL SPECIFIC*
    
    file = input$inFile_indole_quant #'*PANEL SPECIFIC*
    req(file)
    df = read.csv(file$datapath)
    columns = colnames(df)
    columns = lapply(columns, tolower)
    
    if (all(columns %in% permitted_columns_indole_quant) == FALSE) #'*PANEL SPECIFIC*
    {
      '%ni%' = Negate('%in%')
      incorrect_columns = columns[columns %ni% permitted_columns_indole_quant] #'*PANEL SPECIFIC*
      incorrect_columns = gsub(x = incorrect_columns, pattern = "^x(?=[0-9])", replacement = "", perl = T)
      shinyalert("Following column names were not recognized:",incorrect_columns)
      output$columns_indole_quant <- renderText("Invalid columns") #'*PANEL SPECIFIC*
    }
    
    else
    {
      shinyalert(title = "Column names look good!", type = "success")
      shinyjs::show("upload_indole_quant") #'*PANEL SPECIFIC*
      output$columns_indole_quant <- renderText("Columns OK") #'*PANEL SPECIFIC*
    }
    
  })
  
  
  
  ## 7.3 Upload to postgres ============================================================================================
  
  
  
  observeEvent(input$upload_indole_quant, { #'*PANEL SPECIFIC*
    
    file = input$inFile_indole_quant #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    # CHECK FOR DUPLICATE FILENAME
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    if(any(df_log["panel"]=="Indole - quant" & df_log["batch"]==batch) == TRUE) #'*PANEL SPECIFIC*
    {
      shinyalert(title = "File name already exists", type = "error")
      output$upload_status_indole_quant <- renderText("File name already exists") #'*PANEL SPECIFIC*
      shinyjs::show("overwrite_indole_quant") #'*PANEL SPECIFIC*
    }
    
    else
    {
      
      # APPEND TO MASTER DATAFRAME
      
      con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                      user="dfi_admin", password="dfibugs")
      
      df_upload <- 
        df %>% 
        rename(metabolomicsID = sampleid) %>% 
        pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
        mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
               compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
               compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
               filename = name,
               date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
               batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
               type = "quant") %>% #'*PANEL SPECIFIC*
        select(metabolomicsID, date_run, batch, type, filename, compound, value)
      
      
      dbWriteTable(con, "indole_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_lab") #'*PANEL SPECIFIC*
      dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_user") #'*PANEL SPECIFIC*
      
      
      # UPDATE LOG FILE
      
      name = file$name
      df_log = read.csv("metabolomics_postgress_upload_log.csv")
      df_log[is.na(df_log)] = 0
      
      df_log1 <- data.frame(filename = name,
                            date = format(Sys.Date(),format = "%Y%m%d"),
                            time = format(Sys.time(),format = "%H:%M:%S"),
                            panel = "Indole - quant", #'*PANEL SPECIFIC*
                            batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                            sample_count = nrow(df)
      )
      df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
      
      write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
      
      shinyalert(title = "Upload successful", type = "success")
      output$upload_status_indole_quant <- renderText("Upload successful") #'*PANEL SPECIFIC*
      
      dbDisconnect(con)
      
    }
    
  })
  
  
  
  observeEvent(input$overwrite_indole_quant, {#'*PANEL SPECIFIC*
    
    file = input$inFile_indole_quant #'*PANEL SPECIFIC*
    df = read.csv(file$datapath)
    name = file$name
    batch_current = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name)
    
    con <-dbConnect(dbDriver("PostgreSQL"), host="128.135.41.32", dbname="clinical_db",
                    user="dfi_admin", password="dfibugs")
    
    postgres_table = tbl(con,"indole_v3") %>% filter(!(batch == batch_current & type == "quant")) %>% collect() #'*PANEL SPECIFIC*
    
    dbWriteTable(con, "indole_v3", postgres_table, row.names = F, append = F, overwrite=T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_user") #'*PANEL SPECIFIC*
    
    
    df_upload <- 
      df %>% 
      rename(metabolomicsID = sampleid) %>% 
      pivot_longer(-metabolomicsID, names_to = "compound", values_to = "value") %>% 
      mutate(compound = gsub(x = compound, pattern = "^x(?=[0-9])", replacement = "", perl = T, ignore.case = T),
             compound = gsub(x = compound, pattern = "\\.+", replacement = "-"),
             compound = gsub(x = compound, pattern = "-acid", replacement = " acid"),
             filename = name,
             date_run = str_extract(string = name, pattern = "([0-9]+)(?=_\\D)"),
             batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
             type = "quant") %>% #'*PANEL SPECIFIC*
      select(metabolomicsID, date_run, batch, type, filename, compound, value)
    
    
    dbWriteTable(con, "indole_v3", df_upload, row.names = F, append = T) #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_lab") #'*PANEL SPECIFIC*
    dbSendStatement(con, "GRANT SELECT ON indole_v3 TO dfi_user") #'*PANEL SPECIFIC*
    dbDisconnect(con)
    
    # UPDATE LOG FILE
    
    name = file$name
    df_log = read.csv("metabolomics_postgress_upload_log.csv")
    df_log[is.na(df_log)] = 0
    
    df_log1 <- data.frame(filename = name,
                          date = format(Sys.Date(),format = "%Y%m%d"),
                          time = format(Sys.time(),format = "%H:%M:%S"),
                          panel = "Indole - quant", #'*PANEL SPECIFIC*
                          batch = str_extract(pattern = "\\w{4}[0-9]{3}(?=_[0-9]+\\.)", string = name),
                          sample_count = nrow(df)
    )
    df_log <- df_log %>% mutate(date = as.character(date)) %>% bind_rows(df_log1)
    
    write.csv(x = df_log, file = "metabolomics_postgress_upload_log.csv", row.names = F)
    
    shinyalert(title = "Upload successful", type = "success")
    output$upload_status_indole_quant <- renderText("Upload successful") #'*PANEL SPECIFIC*
    
  })
  
  
  
  
  
  
  
  
}
  