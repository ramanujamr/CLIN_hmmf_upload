source('tab_pfbbr_qual.R')
source('tab_pfbbr_quant.R')
source('tab_tms_qual.R')
source('tab_bile_qual.R')
source('tab_bile_quant.R')
source('tab_indole_qual.R')
source('tab_indole_quant.R')
source('tab_graphs.R')


ui <- fluidPage(theme = shinytheme("journal"), useShinyjs(),
                
                titlePanel(h2("Upload metabolomics data to Postgres", align = "center")),
                
                tabsetPanel(
                  tab_pfbbr_qual,
                  tab_pfbbr_quant,
                  tab_tms_qual,
                  tab_bile_qual,
                  tab_bile_quant,
                  tab_indole_qual,
                  tab_indole_quant,
                  tab_graphs
                )
)