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
library(QuantPsyc)

# set working directory
setwd("C:/Users/Monteiro-DataPC/Documents/Consulting/Analytique/Inovação Municipal (Carol)/Replication Documentation/Analysis Data")

#====================#
#    2008 s     #
#====================#

# load data
data_inova_2008 <- read.csv("data_inova_2008.csv", stringsAsfactors = F)             

# transform varibales  
data_inova_2008$inova5           <- factor(data_inova_2008$inova5)
data_inova_2008$metropolitano    <- factor(data_inova_2008$metropolitano, levels = c("0", "1"), labels = c("-non_metro", "-metro") )
data_inova_2008$cand_reelec_2008 <- factor(data_inova_2008$cand_reelec_2008)
data_inova_2008$cand_sex_2008    <- factor(data_inova_2008$cand_sex_2008, levels = c("0", "1"), labels = c("-masc", "-fem"))
data_inova_2008$cand_age_2008 <- as.numeric(data_inova_2008$cand_age_2008)

# S IQBM ACP
pcr2008 <- function(x, Alinhamento){
  glm(inova5 ~  
        iqb_pcr_2008 + 
        Alinhamento + 
        bur_consel_2008 + 
        log_to_capital + 
        log(pop_2008) + 
        metropolitano + 
        log_orcamento_2008 +
        cand_reelec_2008 + 
        cand_esc_2008 + 
        cand_age_2008, 
      data = x, family = "binomial")
}

# display results
stargazer(pcr_estadual_2008, pcr_federal_2008, pcr_ambos_2008, pcr_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# run models
pcr_estadual_2008 <- pcr2008(data_inova_2008, data_inova_2008$alinhamento_estadual_2008)
pcr_federal_2008 <- pcr2008(data_inova_2008, data_inova_2008$alinhamento_federal_2008)
pcr_ambos_2008 <- pcr2008(data_inova_2008, data_inova_2008$alinhamento_ambos_2008)
pcr_n_alinhado_2008 <- pcr2008(data_inova_2008, data_inova_2008$n_alinhado_2008)

# run standardized coefficients
lm.beta(pcr_estadual_2008)
lm.beta(pcr_federal_2008)
lm.beta(pcr_ambos_2008)
lm.beta(pcr_n_alinhado_2008)

#===== IQBM SEPARATED ======#
des2008 <- function(x, Alinhamento){
  glm(inova5 ~  
        bur_escol_2008 + 
        bur_terc_2008 + 
        bur_polit_2008 +  
        Alinhamento +
        bur_consel_2008 + 
        log_to_capital + 
        log(pop_2008) + 
        metropolitano + 
        log_orcamento_2008 +
        cand_reelec_2008 + 
        cand_esc_2008 + 
        cand_age_2008, 
      data = x, family = "binomial")
}

# execute models
estadual_2008 <- des2008(data_inova_2008, data_inova_2008$alinhamento_estadual_2008)
federal_2008 <- des2008(data_inova_2008, data_inova_2008$alinhamento_federal_2008)
ambos_2008 <- des2008(data_inova_2008, data_inova_2008$alinhamento_ambos_2008)
n_alinhado_2008 <- des2008(data_inova_2008, data_inova_2008$n_alinhado_2008)

# display results
stargazer(estadual_2008, federal_2008, ambos_2008, n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# run standardized coefficients
lm.beta(estadual_2008)
lm.beta(federal_2008)
lm.beta(ambos_2008)
lm.beta(n_alinhado_2008)

#===== MODELS IQBM VA 
VA2008 <- function(x, Alinhamento){
  glm(inova5 ~  
        iqb_factor_2008 + 
        Alinhamento +
        bur_consel_2008 + 
        log_to_capital + 
        log(pop_2008) + 
        metropolitano + 
        log_orcamento_2008 +
        cand_reelec_2008 + 
        cand_esc_2008 + 
        cand_age_2008, 
      data = x, family = "binomial")
}

# execute s
VA_estadual_2008 <- VA2008(data_inova_2008, data_inova_2008$alinhamento_estadual_2008)
VA_federal_2008 <- VA2008(data_inova_2008, data_inova_2008$alinhamento_federal_2008)
VA_ambos_2008 <- VA2008(data_inova_2008, data_inova_2008$alinhamento_ambos_2008)
VA_n_alinhado_2008 <- VA2008(data_inova_2008, data_inova_2008$n_alinhado_2008)

# display results
stargazer(VA_estadual_2008, VA_federal_2008, VA_ambos_2008, VA_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# run standardized coefficients
lm.beta(VA_estadual_2008)
lm.beta(VA_federal_2008)
lm.beta(VA_ambos_2008)
lm.beta(VA_n_alinhado_2008)

#=====================#
#    MODELS 2012      #
#=====================#

# load data
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

#===== IQBM SEPARATED ======#
des2012 <- function(x, Alinhamento){
  glm(inova7 ~  
        bur_escol_2011 + 
        bur_terc_2011  + 
        bur_polit_2012 + 
        Alinhamento +
        bur_consel_2012 + 
        log_to_capital + 
        log(pop_2012) + 
        metropolitano + 
        log_orcamento_2012 +
        cand_reelec_2012 + 
        cand_esc_2012 + 
        cand_age_2012, 
        data = x, family = "binomial")
}


des_estadual_2012 <- des2012(data_inova_2012, data_inova_2012$alinhamento_estadual_2012)
des_federal_2012 <- des2012(data_inova_2012, data_inova_2012$alinhamento_federal_2012)
des_ambos_2012 <- des2012(data_inova_2012, data_inova_2012$alinhamento_ambos_2012)
des_n_alinhado_2012 <- des2012(data_inova_2012, data_inova_2012$n_alinhado_2012)

# display results
stargazer(des_estadual_2012, des_federal_2012, des_ambos_2012, des_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# run standardized coefficients
lm.beta(des_estadual_2012)
lm.beta(des_federal_2012)
lm.beta(des_ambos_2012)
lm.beta(des_n_alinhado_2012)

# IQB s
pcr2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        iqb_pcr_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}

# execute s
pcr_estadual_2012 <- pcr2012(data_inova_2012, data_inova_2012$alinhamento_estadual_2012)
pcr_federal_2012 <- pcr2012(data_inova_2012, data_inova_2012$alinhamento_federal_2012)
pcr_ambos_2012 <- pcr2012(data_inova_2012, data_inova_2012$alinhamento_ambos_2012)
pcr_n_alinhado_2012 <- pcr2012(data_inova_2012, data_inova_2012$n_alinhado_2012)

# display results
stargazer(pcr_estadual_2012, pcr_federal_2012, pcr_ambos_2012, pcr_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

lm.beta(pcr_estadual_2012)
lm.beta(pcr_federal_2012)
lm.beta(pcr_ambos_2012)
lm.beta(pcr_n_alinhado_2012)

# iqb factor  #

VA2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        iqb_factor_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}


VA_estadual_2012 <- VA2012(data_inova_2012, data_inova_2012$alinhamento_estadual_2012)
VA_federal_2012 <- VA2012(data_inova_2012, data_inova_2012$alinhamento_federal_2012)
VA_ambos_2012 <- VA2012(data_inova_2012, data_inova_2012$alinhamento_ambos_2012)
VA_n_alinhado_2012 <- VA2012(data_inova_2012, data_inova_2012$n_alinhado_2012)

# display results
stargazer(VA_estadual_2012, VA_federal_2012, VA_ambos_2012, VA_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  


#-------------------------------#
#     PT    2012 S         #
#-------------------------------#

# load data
data_inova <- read.csv("data_inova_2012_pt.csv", stringsAsfactors = F)             

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
# S

# disaggregated
pt_des2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        bur_escol_2011 + bur_terc_2012 + bur_polit_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}

ptdes_estadual_2012 <- pt_des2012(data_inova, data_inova$alinhamento_estadual_2012)
ptdes_n_alinhado_2012 <- pt_des2012(data_inova, data_inova$n_alinhado_2012)

stargazer(ptdes_estadual_2012, ptdes_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Não Alinhado"))  


# IQB s
pt_pcr2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        iqb_pcr_2012 + Alinhamento + (iqb_pcr_2012 * Alinhamento) +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}

pcrpt_estadual_2012 <- pt_pcr2012(data_inova, data_inova$alinhamento_estadual_2012)
pcrpt_n_alinhado_2012 <- pt_pcr2012(data_inova, data_inova$n_alinhado_2012)

stargazer(pcrpt_estadual_2012, pcrpt_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Não Alinhado"))  
lm.beta(pcrpt_estadual_2012)
lm.beta(pcrpt_n_alinhado_2012)

# iqb factor 2012
pt_VA2012 <- function(x, Alinhamento){
  
  glm(inova7 ~  
        
        iqb_factor_2012 + Alinhamento +
        bur_consel_2012 + log_to_capital + log(pop_2012) + metropolitano + log_orcamento_2012 +
        cand_reelec_2012 + cand_esc_2012 + cand_age_2012, 
      data = x, family = "binomial")
}

VApt_estadual_2012 <- pt_VA2012(data_inova, data_inova$alinhamento_estadual_2012)
VApt_n_alinhado_2012 <- pt_VA2012(data_inova, data_inova$n_alinhado_2012)

stargazer(VApt_estadual_2012, VApt_n_alinhado_2012,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Não Alinhado"))  

