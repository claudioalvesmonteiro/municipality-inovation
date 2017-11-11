#-----------------------------------------#
# BUREAUCRACY INOVATION - Data Analysis   #    
#-----------------------------------------#
# Recife - Pernambuco - Brasil            #
# May 2017 - November 2017                #    
#-----------------------------------------#
# Author: Claudio A. Monteiro             #
# claudiomonteirol.a@gmail.com            #
#-----------------------------------------#
# Any question contact the developer      #
# #UseFreeSoftware                        #
#-----------------------------------------#


# load packages
library(readr)
library(stargazer)
# load data
setwd("C:/Users/Monteiro-DataPC/Documents/Consulting/Analytique/Inovação Municipal (Carol)/Replication Documentation/Analysis Data")

#-------------------------------#
#          2008 Models          #
#-------------------------------#

# load data
data_inova_2008 <- read.csv("data_inova_2008.csv", stringsAsFactors = F)             

#----------------------#
# transform in factor  
data_inova_2008$inova5           <- factor(data_inova_2008$inova5)
data_inova_2008$metropolitano    <- factor(data_inova_2008$metropolitano, levels = c("0", "1"), labels = c("-non_metro", "-metro") )
data_inova_2008$cand_reelec_2008 <- factor(data_inova_2008$cand_reelec_2008)
data_inova_2008$cand_sex_2008    <- factor(data_inova_2008$cand_sex_2008, levels = c("0", "1"), labels = c("-masc", "-fem"))
data_inova_2008$cand_age_2008 <- as.numeric(data_inova_2008$cand_age_2008)
#---------------------#
# Models IQB PCR

model_pcr2008 <- function(x, Alinhamento){
  
  glm(inova5 ~  
        
        iqb_pcr_2008 + Alinhamento + 
        bur_consel_2008 + log_to_capital + log(pop_2008) + metropolitano + log_orcamento_2008 +
        cand_reelec_2008 + cand_esc_2008 + cand_age_2008, 
      data = x, family = "binomial")
}

modelpcr_estadual_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_estadual_2008)
modelpcr_federal_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_federal_2008)
modelpcr_ambos_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_ambos_2008)
modelpcr_n_alinhado_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$n_alinhado_2008)

# display results
stargazer(modelpcr_estadual_2008, modelpcr_federal_2008, modelpcr_ambos_2008, modelpcr_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# disaggregated
data_inova_2008$alinhamento_ambos_2008 <- factor(data_inova_2008$alinhamento_ambos_2008, levels = c("0", "1"), labels = c("-ausencia", "-presenga"))
data_inova_2008$alinhamento_estadual_2008  <- factor(data_inova_2008$alinhamento_estadual_2008,  levels = c("0", "1"), labels = c("-ausencia", "-presenga"))
data_inova_2008$alinhamento_federal_2008        <- factor(data_inova_2008$alinhamento_federal_2008,  levels = c("0", "1"), labels = c("-ausencia", "-presenga"))
data_inova_2008$n_alinhado_2008                 <- factor(data_inova_2008$n_alinhado_2008,  levels = c("0", "1"), labels = c("-ausencia", "-presenga"))

model_des2008 <- function(x, Alinhamento){
  
  glm(inova5 ~  
        
        bur_escol_2008 + bur_terc_2008 + bur_polit_2008 +  Alinhamento +
        bur_consel_2008 + log_to_capital + log(pop_2008) + metropolitano + log_orcamento_2008 +
        cand_reelec_2008 + cand_esc_2008 + cand_age_2008, 
      data = x, family = "binomial")
}

# execute models
model_estadual_2008 <- model_des2008(data_inova_2008, data_inova_2008$alinhamento_estadual_2008)
model_federal_2008 <- model_des2008(data_inova_2008, data_inova_2008$alinhamento_federal_2008)
model_ambos_2008 <- model_des2008(data_inova_2008, data_inova_2008$alinhamento_ambos_2008)
model_n_alinhado_2008 <- model_des2008(data_inova_2008, data_inova_2008$n_alinhado_2008)

# display results
stargazer(model_estadual_2008, model_federal_2008, model_ambos_2008, model_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# iqb pcr #
model_pcr2008 <- function(x, Alinhamento){
  
  glm(inova5 ~  
        
        Alinhamento +
        iqb_pcr_2008 + 
        bur_consel_2008 + log_to_capital + log(pop_2008) + metropolitano + log_orcamento_2008 +
        cand_reelec_2008 + cand_esc_2008 + cand_age_2008, 
      data = x, family = "binomial")
}

modelpcr_estadual_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_estadual_2008)
modelpcr_federal_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_federal_2008)
modelpcr_ambos_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_ambos_2008)
modelpcr_n_alinhado_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$n_alinhado_2008)

library(QuantPsyc)
lm.beta(modelpcr_n_alinhado_2008)

# display results
stargazer(modelpcr_estadual_2008, modelpcr_federal_2008, modelpcr_ambos_2008, modelpcr_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  


#----------#
# iqb pcr #
data_inova_2008$alinhamento_ambos_2008 <- as.numeric(data_inova_2008$alinhamento_ambos_2008, levels = c("0", "1"), labels = c("-ausencia", "-presenga"))
data_inova_2008$alinhamento_estadual_2008  <- factor(data_inova_2008$alinhamento_estadual_2008,  levels = c("0", "1"), labels = c("-ausencia", "-presenga"))
data_inova_2008$alinhamento_federal_2008        <- factor(data_inova_2008$alinhamento_federal_2008,  levels = c("0", "1"), labels = c("-ausencia", "-presenga"))
data_inova_2008$n_alinhado_2008                 <- factor(data_inova_2008$n_alinhado_2008,  levels = c("0", "1"), labels = c("-ausencia", "-presenga"))

model_pcr2008 <- function(x, Alinhamento){
  
  glm(inova5 ~  
        
        iqb_pcr_2008 + Alinhamento + iqb_pcr_2008*Alinhamento +
        bur_consel_2008 + log_to_capital + log(pop_2008) + metropolitano + log_orcamento_2008 +
        cand_reelec_2008 + cand_esc_2008 + cand_age_2008, 
      data = x, family = "binomial")
}

modelpcr_estadual_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_estadual_2008)
modelpcr_federal_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_federal_2008)
modelpcr_ambos_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$alinhamento_ambos_2008)
modelpcr_n_alinhado_2008 <- model_pcr2008(data_inova_2008, data_inova_2008$n_alinhado_2008)

# display results
stargazer(modelpcr_estadual_2008, modelpcr_federal_2008, modelpcr_ambos_2008, modelpcr_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# iqb factor #
model_fac2008 <- function(x, Alinhamento){
  
  glm(inova5 ~  
        
        iqb_factor_2008 + Alinhamento +
        bur_consel_2008 + log_to_capital + log(pop_2008) + metropolitano + log_orcamento_2008 +
        cand_reelec_2008 + cand_esc_2008 + cand_age_2008, 
      data = x, family = "binomial")
}

# execute models
modelfac_estadual_2008 <- model_fac2008(data_inova_2008, data_inova_2008$alinhamento_estadual_2008)
modelfac_federal_2008 <- model_fac2008(data_inova_2008, data_inova_2008$alinhamento_federal_2008)
modelfac_ambos_2008 <- model_fac2008(data_inova_2008, data_inova_2008$alinhamento_ambos_2008)
modelfac_n_alinhado_2008 <- model_fac2008(data_inova_2008, data_inova_2008$n_alinhado_2008)

# display results
stargazer(modelfac_estadual_2008, modelfac_federal_2008, modelfac_ambos_2008, modelfac_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  


#-----------------------------#
#         2012 MODELS         #
#-----------------------------#

# load data
setwd("C:/Users/Monteiro-DataPC/Documents/Consulting/Analytique/Inovação Municipal (Carol)/Replication Documentation/Analysis Data")
data_inova_2012 <- read.csv("data_inova_2012.csv", stringsAsFactors = F)             

#----------------------#
# transform in factor  

data_inova_2012$inova7               <- as.factor(data_inova_2012$inova7)
data_inova_2012$metropolitano        <- factor(data_inova_2012$metropolitano) 
data_inova_2012$cand_reelec_2012     <- factor(data_inova_2012$cand_reelec_2012)
data_inova_2012$alinhamento_ambos_2012    <- factor(data_inova_2012$alinhamento_ambos_2012, levels = c("0", "1"), labels = c("-ausencia", "-presença"))
data_inova_2012$alinhamento_estadual_2012 <- factor(data_inova_2012$alinhamento_estadual_2012,  levels = c("0", "1"), labels = c("-ausencia", "-presença"))
data_inova_2012$alinhamento_federal_2012  <- factor(data_inova_2012$alinhamento_federal_2012,  levels = c("0", "1"), labels = c("-ausencia", "-presença"))
data_inova_2012$n_alinhado_2012           <- factor(data_inova_2012$n_alinhado_2012,  levels = c("0", "1"), labels = c("-ausencia", "-presença"))
data_inova_2012$cand_age_2012    <- as.numeric(data_inova_2012$cand_age_2012)

#---------------------#
# MODELS

# disaggregated
model_des2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        bur_escol_2011 + bur_terc_2012  + bur_polit_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012
      , 
      data = x, family = "binomial")
}


modeldes_estadual_2012 <- model_des2012(data_inova_2012, data_inova_2012$alinhamento_estadual_2012)

modeldes_federal_2012 <- model_des2012(data_inova_2012, data_inova_2012$alinhamento_federal_2012)
modeldes_ambos_2012 <- model_des2012(data_inova_2012, data_inova_2012$alinhamento_ambos_2012)
modeldes_n_alinhado_2012 <- model_des2012(data_inova_2012, data_inova_2012$n_alinhado_2012)

# display results
stargazer(modeldes_estadual_2012, modeldes_federal_2012, modeldes_ambos_2012, modeldes_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  


# IQB Models
model_pcr2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        iqb_pcr_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}

# execute models
modelpcr_estadual_2012 <- model_pcr2012(data_inova_2012, data_inova_2012$alinhamento_estadual_2012)
modelpcr_federal_2012 <- model_pcr2012(data_inova_2012, data_inova_2012$alinhamento_federal_2012)
modelpcr_ambos_2012 <- model_pcr2012(data_inova_2012, data_inova_2012$alinhamento_ambos_2012)
modelpcr_n_alinhado_2012 <- model_pcr2012(data_inova_2012, data_inova_2012$n_alinhado_2012)

# display results
stargazer(modelpcr_estadual_2012, modelpcr_federal_2012, modelpcr_ambos_2012, modelpcr_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

lm.beta(modelpcr_estadual_2012)
lm.beta(modelpcr_federal_2012)
lm.beta(modelpcr_ambos_2012)
lm.beta(modelpcr_n_alinhado_2012)

# iqb factor model #

model_fac2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        iqb_factor_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}


modelfac_estadual_2012 <- model_fac2012(data_inova_2012, data_inova_2012$alinhamento_estadual_2012)
modelfac_federal_2012 <- model_fac2012(data_inova_2012, data_inova_2012$alinhamento_federal_2012)
modelfac_ambos_2012 <- model_fac2012(data_inova_2012, data_inova_2012$alinhamento_ambos_2012)
modelfac_n_alinhado_2012 <- model_fac2012(data_inova_2012, data_inova_2012$n_alinhado_2012)

# display results
stargazer(modelfac_estadual_2012, modelfac_federal_2012, modelfac_ambos_2012, modelfac_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  


#-------------------------------#
#     PT    2012 MODELS         #
#-------------------------------#

# load data
data_inova <- read.csv("data_inova_2012_pt.csv", stringsAsFactors = F)             

#----------------------#
# transform in factor  
data_inova$inova7 <- as.factor(data_inova$inova7)
data_inova$metropolitano <- factor(data_inova$metropolitano) 
data_inova$cand_reelec_2012 <- factor(data_inova$cand_reelec_2012)
data_inova$alinhamento_ambos_2012 <- factor(data_inova$alinhamento_ambos_2012, levels = c("0", "1"), labels = c("-ausencia", "-presença"))
data_inova$alinhamento_estadual_2012 <- factor(data_inova$alinhamento_estadual_2012,  levels = c("0", "1"), labels = c("-ausencia", "-presença"))
data_inova$alinhamento_federal_2012 <- factor(data_inova$alinhamento_federal_2012,  levels = c("0", "1"), labels = c("-ausencia", "-presença"))
data_inova$n_alinhado_2012 <- factor(data_inova$n_alinhado_2012,  levels = c("0", "1"), labels = c("-ausencia", "-presença"))
data_inova$partido_pt_2012 <- factor(data_inova$partido_pt_2012,  levels = c("0", "1"), labels = c("-ausencia", "-PT"))
data_inova$cand_age_2012 <- as.numeric(data_inova$cand_age_2012)
#---------------------#
# MODELS

# disaggregated
modelpt_des2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        bur_escol_2011 + bur_terc_2012 + bur_polit_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}

modelptdes_estadual_2012 <- modelpt_des2012(data_inova, data_inova$alinhamento_estadual_2012)
modelptdes_n_alinhado_2012 <- modelpt_des2012(data_inova, data_inova$n_alinhado_2012)

stargazer(modelptdes_estadual_2012, modelptdes_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Não Alinhado"))  


# IQB Models
modelpt_pcr2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        iqb_pcr_2012 + Alinhamento + (iqb_pcr_2012 * Alinhamento) +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}

modelpcrpt_estadual_2012 <- modelpt_pcr2012(data_inova, data_inova$alinhamento_estadual_2012)
modelpcrpt_n_alinhado_2012 <- modelpt_pcr2012(data_inova, data_inova$n_alinhado_2012)

stargazer(modelpcrpt_estadual_2012, modelpcrpt_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Não Alinhado"))  
lm.beta(modelpcrpt_estadual_2012)
lm.beta(modelpcrpt_n_alinhado_2012)

# iqb factor 2012
modelpt_fac2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        iqb_factor_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}

modelfacpt_estadual_2012 <- modelpt_fac2012(data_inova, data_inova$alinhamento_estadual_2012)
modelfacpt_n_alinhado_2012 <- modelpt_fac2012(data_inova, data_inova$n_alinhado_2012)

stargazer(modelfacpt_estadual_2012, modelfacpt_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Não Alinhado"))  

