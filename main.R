library(pacman)
p_load(
  shiny,
  shinyalert,
  shinyjs,
  shinythemes,
  tidyr,
  tidyverse,
  dplyr,
  DT,
  stringr,
  ggplot2,
  RPostgreSQL,
  install = TRUE
)

# library(shiny)
# library(shinyalert)
# library(shinyjs)
# library(shinythemes)
# library(tidyr)
# library(tidyverse)
# library(dplyr)
# library(DT)
# library(stringr)
# library(ggplot2)
# library(RPostgreSQL)

# 0. Initializations ###################################################################################################

## 0.1 Permitted columns lists #########################################################################################

permitted_columns_pfbbr_qual = list("sampleid", "x2.methylbutyrate", "x3.aminoisobutyrate",	
                                    "x3.aminoisobutyrate2", "x4.methylvalerate", "x5.aminovalerate", "acetate",
                                    "alanine", "aspartate", "benzoate", "butyrate", "catechol", "crotonate",
                                    "cysteine", "desaminotyrosine",	"dopamine", "glutamate", "glycine", 
                                    "hexanoate", "histamine",	"hydrocinnamate",	"indole.3.acetate",	
                                    "indole.3.carboxaldehyde", "indole.3.propionate",	"isobutyrate", "isoleucine",
                                    "isovaleric.acid", "itaconate", "leucine", "lysine", "methionine", "p.cresol",
                                    "palmitate", "phenol", "phenylacetate", "phenylalanine", "proline", 
                                    "propionate",	"serine",	"succinate", "synephrine", "threonine",	"toluate",
                                    "trans.indole.3.acrylate", "tryptamine", "tryptophan", "tyramine",	
                                    "tyrosine",	"valerate",	"valine",	"vanillin")

permitted_columns_pfbbr_quant = list("sampleid", "acetate", "butyrate", "propionate", "succinate")


permitted_columns_tms_qual = list("sampleid", "x1.2.propanediol_o",	"x12.ketolithocholic.acid_ba",	"x13c.palmitate.329_itsd",
                                  "x2,3.butanediol_o",	"x2.aminobutyric.acid_o",	"x2.deoxy.ribose_c",
                                  "x2.hydroxy.3.methylbutyric.acid_o",	"x2.hydroxybutyric.acid_o",	"x2.hydroxyhexanoic.acid_o",
                                  "x2.hydroxyisocaproic.acid_o",	"x2.keto.gluconic.acid_c",	"x3,4.dihydroxyphenylacetic.acid_o",
                                  "x3.(3.hydroxyphenyl)propionic.acid_o",	"x3.aminoisobutyric.acid_o",
                                  "x3.epideoxycholic.acid_ba",	"x3.hydroxybutyric.acid_o",	"x3.ketolithocholic.acid_ba",
                                  "x3.oxodeoxycholic.acid_ba",	"x4.ethylphenol_o",	"x4.hydroxyphenylacetic.acid_o",	
                                  "x4.hydroxyphenyllactic.acid_o",	"x4.methylvaleric.acid_o",	"x5.aminovaleric.acid_o",
                                  "x5.hydroxylysine_o",	"x5.oxoproline_o",	"x7.ketolithocholic.acid_ba",	"alanine_aa",
                                  "alloisolithocholic.acid_ba",	"allose_c",	"alpha.ketobutyric.acid_o",	
                                  "alpha.ketoglutaric.acid._tca",	"alpha.muricholic.acid_ba",	"aminocaproic.acid_o",
                                  "arabinose_c",	"asparagine_aa",	"aspartic.acid_aa",	"benzoic.acid_o",
                                  "beta.muricholic/gamma.muricholic.acid_ba",	"cadaverine_o",	"caffeic.acid_o",	"caffeine_o",
                                  "catechol_o",	"cellobiose_c",	"chlorogenic.acid_o",	"cholesterol_o",
                                  "cholic/allocholic.acid_ba",	"cis.aconitic.acid_tca",	"cis.oleic.acid_o",	"citric.acid_tca",
                                  "coprostanol_o",	"coprostanone_o",	"creatinine/creatine_o",	"cysteine_aa",	"cystine_aa",	
                                  "d7.proline_itsd",	"decanoic.acid_o",	"deoxycarnitine_o",	"deoxycholic.acid_ba",	
                                  "desaminotyrosine_o",	"dihydrocaffeic.acid_o",	"dihydrocholesterol_o",	"dihydroferulic.acid_o",
                                  "dihydroresveratrol_o",	"dihydrosinapic.acid_o",	"dihydrotestosterone_o",	"dodecanoic.acid_o",
                                  "dopamine_o",	"dulcitol_c",	"elaidic.acid_o",	"erythronic.acid_c",	"ethanolamine_o",	
                                  "fructose_c",	"fucose_c",	"fumaric.acid_tca",	"gabapentin/gabapentin.lactam_o",	"galactose_c",
                                  "gamma.aminobutyric.acid_o",	"gluconic.acid_c",	"glucose_c",	"glutamic.acid_aa",
                                  "glutamine_aa",	"glutaric.acid_o",	"glycerol_c",	"glycine_aa",	"glycodeoxycholic.acid_ba",
                                  "glycolic.acid_o",	"hexanoic.acid_o",	"histamine_o",	"histidine_aa",	"hydrocinnamic.acid_o",
                                  "hyodeoxycholic.acid_ba",	"imidazoleacetic.acid_o",	"imidazolepropionic.acid_o",	"indole_o",
                                  "indole.3.acetamide_o",	"indole.3.acetic.acid_o",	"indole.3.acrylic.acid_o",	
                                  "indole.3.carboxaldehyde_o",	"indole.3.lactic.acid_o",	"indole.3.propionic.acid_o",
                                  "indole.3.pyruvic.acid_o",	"isocitric.acid_tca",	"isodeoxycholic.acid_ba",	"isoleucine_aa",
                                  "itaconic.acid_o",	"lactic.acid_o",	"lactose_c",	"lactulose_c",	"leucine_aa",
                                  "linoleic.acid_o",	"lithocholic.allolithocholic.isolithocholic.acid_ba",	"lysine_aa",
                                  "m.coumaric.acid_o",	"maleic.acid_o",	"malic.acid_tca",	"malonic.acid_o",	"maltitol_c",	
                                  "maltose_c",	"mannitol_c",	"mannose_c",	"melatonin_o",	"melibiose_c",	"meso.erythritol_c",
                                  "methionine_aa",	"methionine.sulfoxide_o",	"methylsuccinic.acid_o",	"myo.inositol_c",
                                  "myristic.acid_o",	"n.acetyl.glucosamine_c",	"niacin_o",	"o.phosphorylethanolamine_o",
                                  "octanoic.acid_o",	"octopamine_o",	"ornithine_o",	"oxalic.acid_o",	"oxaloacetic.acid_tca",	
                                  "p.coumaric.acid_o",	"p.cresol_o",	"p.toluic.acid_o",	"palmitic.acid_o",
                                  "pantothenic.acid_o",	"phenethylamine_o",	"phenylacetic.acid_o",	"phenylalanine_aa",
                                  "phenyllactic.acid_o",	"phenylpyruvic.acid_o",	"picolinic.acid_o",	"pinitol_c",	"proline_aa",
                                  "psicose_c",	"putrescine_o",	"pyruvic.acid_tca",	"quinic.acid_o",	"quinolinic.acid_o",	
                                  "raffinose_c",	"resveratrol_o",	"rhamnose_c",	"ribose_c",	"rosmarinic.acid_o",	"serine_aa",
                                  "shikimic.acid_o",	"sinapic.acid_o",	"sorbitol_c",	"sorbose_c",	"stearic.acid_o",	
                                  "succinic.acid_tca",	"sucrose_c",	"tartaric.acid_o",	"testosterone_o",	"threitol_c",
                                  "threonine_aa",	"trans.4.hydroxyproline_o",	"trans.cinnamic.acid_o",	"trans.ferulic.acid_o",
                                  "tryptamine_o",	"tryptophan_aa",	"tryptophol_o",	"tyramine_o",	"tyrosine_aa",	"tyrosol_o",
                                  "uracil_o",	"urea_o",	"urocanic.acid_o",	"ursodeoxycholic.acid_ba",	"valeric.acid_o",	
                                  "valine_aa",	"vanillin_o",	"xylose_c")

permitted_columns_bile_quant = list("sampleid", "x3.oxolithocholic.acid", "alloisolithocholic.acid", "cholic.acid", 
                                    "deoxycholic.acid", "glycocholic.acid", "isodeoxycholic.acid", "lithocholic.acid",
                                    "taurocholic.acid")

permitted_columns_bile_qual = list("sampleid", "cholic.acid",	"deoxycholic.acid",	"lithocholic.acid",
                                   "glycocholic.acid", "taurocholic.acid", "isodeoxycholic.acid",
                                   "alloisolithocholic.acid",	"x3.oxolithocholic.acid", 
                                   "tauro.alpha.or.tauro.beta.muricholic.acid",	"glycodehydrocholic.acid",
                                   "glycochenodeoxycholic.acid", "glycodeoxycholic.acid", 
                                   "glycoursodeoxycholic.acid",	"glycolithocholic.acid", 
                                   "glycohyodeoxycholic.acid",	"omega.muricholic.acid", "beta.muricholic.acid",
                                   "gamma.muricholic.acid",	"alpha.muricholic.acid", "allocholic.acid",	
                                   "chenodeoxycholic.acid",	"allolithocholic.acid",	"ursodeoxycholic.acid",	
                                   "hyodeoxycholic.acid",	"x3.deoxycholic.acid",	"isolithocholic.acid",	
                                   "x12.oxochenodeoxycholic.acid", "x7.oxodeoxycholic.acid",	"x3.oxocholic.acid",	
                                   "x3.oxo.or.3.oxochenodeoxycholic.acid", "x7.oxo.or.6.oxolithocholic.acid",
                                   "x12.oxolithocholic.acid",	"taurodeoxycholic.acid", "taurolithocholic.acid",	
                                   "taurochenodeoxycholic.acid", "tauroursodeoxycholic.acid",	
                                   "taurohyodeoxycholic.acid", "x7.12.dioxolithocholic.acid",	"ursocholic.acid")

permitted_columns_indole_qual = list("sampleid", "x3ohanthranilicacid", "x5hiaa", "x5ohtryptophan", "anthranilicacid",
                                     "biotin",	"dopamine",	"epinephrine", "folate", "glycine",	"indole",	"indole3acetamide",
                                     "indole3acetic",	"indole3acrylicacid",	"indole3carboxyaldehyde",	"indole3lacticacid",
                                     "indole3propionic",	"indolepyruvicacid",	"kynurenicacid",	"kynurenine",	"melatonin",
                                     "methylserotonin",	"nacetylserotonin",	"niacin",	"panthothenic",	"phenylalanine",
                                     "picolinicacid",	"plp",	"preq1",	"queuine",	"quinolinicacid",	"serotonin",
                                     "tryptamine",	"tryptophan",	"tryptophol",	"tyrosine")

permitted_columns_indole_quant = list("sampleid", "x5hiaa", "anthranilicacid",	"kynurenicacid", "kynurenine", "melatonin",
                                      "niacin", "phenylalanine", "serotonin", "tryptamine", "tryptophan", "tyrosine")


## 0.2 Remove leading x's and create dataframes for display table =====================================================
temp_list = gsub(permitted_columns_pfbbr_qual, pattern = "^x(?=[0-9])", replacement = "", perl = T)
df_permitted_columns_pfbbr_qual = data.frame(index = c(1:length(temp_list)), permitted.columns = unlist(temp_list))

temp_list = gsub(permitted_columns_pfbbr_quant, pattern = "^x(?=[0-9])", replacement = "", perl = T)
df_permitted_columns_pfbbr_quant = data.frame(index = c(1:length(temp_list)), permitted.columns = unlist(temp_list))

temp_list = gsub(permitted_columns_tms_qual, pattern = "^x(?=[0-9])", replacement = "", perl = T)
df_permitted_columns_tms_qual = data.frame(index = c(1:length(temp_list)), permitted.columns = unlist(temp_list))

temp_list = gsub(permitted_columns_bile_qual, pattern = "^x(?=[0-9])", replacement = "", perl = T)
df_permitted_columns_bile_qual = data.frame(index = c(1:length(temp_list)), permitted.columns = unlist(temp_list))

temp_list = gsub(permitted_columns_bile_quant, pattern = "^x(?=[0-9])", replacement = "", perl = T)
df_permitted_columns_bile_quant = data.frame(index = c(1:length(temp_list)), permitted.columns = unlist(temp_list))

temp_list = gsub(permitted_columns_indole_qual, pattern = "^x(?=[0-9])", replacement = "", perl = T)
df_permitted_columns_indole_qual = data.frame(index = c(1:length(temp_list)), permitted.columns = unlist(temp_list))

temp_list = gsub(permitted_columns_indole_quant, pattern = "^x(?=[0-9])", replacement = "", perl = T)
df_permitted_columns_indole_quant = data.frame(index = c(1:length(temp_list)), permitted.columns = unlist(temp_list))




# 1. Execution #########################################################################################################
source('ui.R', local=TRUE)
source('server.R', local=TRUE)
shinyApp(ui=ui, server=server)

#profvis::profvis(runApp('main.R'))
