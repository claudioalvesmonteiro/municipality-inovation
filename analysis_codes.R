#===============================================#
# MODELLING MUNICIPALITY INNOVATION             #    
#===============================================#
# Recife - Pernambuco - Brazil                  #
# May 2017 - December 2017                      #    
#-----------------------------------------------#
# Ana Carolina                                  #
# Claudio A. Monteiro                           #
# Leticia Machado                               #
#-----------------------------------------------#
# Any question contact the developers           #
# #UseFreeSoftware                              #
#-----------------------------------------------#

# install.packages(c("readr", "stargazer", "QuantPsyc"), dependencies = T)

# load packages
library(readr) ; library(stargazer); library(QuantPsyc)

"/home/pacha/Documents/git_projects/municipality-inovation/Original Data"

#====================#
#    2008 MODELS     #
#====================#

# load data
data_inova_2008 <- read.csv("data_inova_2008.csv", stringsAsFactors  = F)             

# transform varibales  
data_inova_2008$inova5           <- factor(data_inova_2008$inova5, levels = c("0", "1"), labels = c("-dont_innovate", "-innovate"))
data_inova_2008$metropolitano    <- factor(data_inova_2008$metropolitano, levels = c("0", "1"), labels = c("-non_metro", "-metro") )
data_inova_2008$cand_reelec_2008 <- factor(data_inova_2008$cand_reelec_2008)
data_inova_2008$cand_sex_2008    <- factor(data_inova_2008$cand_sex_2008, levels = c("0", "1"), labels = c("-masc", "-fem"))
data_inova_2008$cand_age_2008    <- as.numeric(data_inova_2008$cand_age_2008)

# rename colnames
colnames(data_inova_2008) <- c( "idx", "codigo1", "codigo_ibge_1", "codigo_ibge_2", "IDHM", "Sigla_Estado", "municipio", "Estado",                  
                                "Distância_Até_Brasília_(log)", "Distância_até_Capital_log",  "Município_Metropolitano", "População",                
                                "Prêmio_Inovação_2007", "População2", "Conselho", "Reeleição",  
                                "cand_sex_2008", "Escolaridade_Prefeito", "Idade_Prefeito",  "Escolaridade_da_Burocracia",        
                                "Terceirização_da_Burocracia", "Especialização_da_Burocracia",  "Politização_da_Burocracia", 
                                "Alinhamento_Estadual","Alinhamento_Federal", "Alinhado_Ambos", "Não_Alinhado", "UF",                     
                                "Município", "municipio.y", "Orçamento",   "Orçamento_log",       
                                "IQB_ACP", "IQB_IA")

#===== IQBM ACP ======#
acp2008 <- function(x, Alinhamento){
  glm(Prêmio_Inovação_2007 ~  
        IQB_ACP +
        Alinhamento +
        Orçamento_log +
        Conselho +
        Distância_até_Capital_log +
    #    Município_Metropolitano +
         Reeleição +
        Idade_Prefeito +
        Escolaridade_Prefeito+
        log(População) +
        IDHM
   , 
      data = x, family = "binomial")
}


# run models
acp_estadual_2008 <- acp2008(data_inova_2008, data_inova_2008$Alinhamento_Estadual)
acp_federal_2008 <- acp2008(data_inova_2008, data_inova_2008$Alinhamento_Federal)
acp_ambos_2008 <- acp2008(data_inova_2008, data_inova_2008$Alinhado_Ambos)
acp_n_alinhado_2008 <- acp2008(data_inova_2008, data_inova_2008$Não_Alinhado)

# display results
stargazer(acp_estadual_2008, acp_federal_2008, acp_ambos_2008, acp_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps",  apply.coef = exp,  p.auto=FALSE,
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  


#===== IQBM SEPARATED ======#
des2008 <- function(x, Alinhamento){
  glm(Prêmio_Inovação_2007 ~  
        Escolaridade_da_Burocracia + 
        Terceirização_da_Burocracia + 
        Politização_da_Burocracia +  
        Alinhamento +
        Conselho +
        Orçamento_log +
        Distância_até_Capital_log +
        Município_Metropolitano +
        Reeleição +
        Idade_Prefeito +
        Escolaridade_Prefeito+
        log(População) +
        IDHM , 
      data = x, family = "binomial")
}

# execute models
estadual_2008 <- des2008(data_inova_2008, data_inova_2008$Alinhamento_Estadual)
federal_2008 <- des2008(data_inova_2008, data_inova_2008$Alinhamento_Federal)
ambos_2008 <- des2008(data_inova_2008, data_inova_2008$Alinhado_Ambos)
n_alinhado_2008 <- des2008(data_inova_2008, data_inova_2008$Não_Alinhado)

# display results
stargazer(estadual_2008, federal_2008, ambos_2008, n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", apply.coef = exp,  p.auto=FALSE,
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

#===== IQBM IA =====# 
IA_2007 <- function(x, Alinhamento){
  glm(Prêmio_Inovação_2007 ~  
        IQB_IA +
        Alinhamento +
        IDHM +
        Conselho +
        Orçamento_log +
        log(População) +
        Distância_até_Capital_log +
        Município_Metropolitano +
        Reeleição +
        Idade_Prefeito +
        Escolaridade_Prefeito, 
      data = x, family = "binomial")
}

# execute s
ia_estadual_2008 <- IA_2007(data_inova_2008, data_inova_2008$Alinhamento_Estadual)
ia_federal_2008 <- IA_2007(data_inova_2008, data_inova_2008$Alinhamento_Federal)
ia_ambos_2008 <- IA_2007(data_inova_2008, data_inova_2008$Alinhado_Ambos)
ia_n_alinhado_2008 <- IA_2007(data_inova_2008, data_inova_2008$Não_Alinhado)

# display results
stargazer(ia_estadual_2008, ia_federal_2008, ia_ambos_2008, ia_n_alinhado_2008,
          type = "text", title = "Results", style = "ajps", apply.coef = exp,  p.auto=FALSE,
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# run standardized coefficients
lm.beta(ia_estadual_2008)
lm.beta(ia_federal_2008)
lm.beta(ia_ambos_2008)
lm.beta(ia_n_alinhado_2008)

#=====================#
#    MODELS 2011      #
#=====================#

# load data
data_inova_2011 <- read.csv("data_inova_2012.csv", stringsAsFactors = F)

#----------------------#
# transform in factor  
data_inova_2011$inova7               <- as.factor(data_inova_2011$inova7)
data_inova_2011$metropolitano        <- factor(data_inova_2011$metropolitano) 
data_inova_2011$cand_reelec_2012     <- factor(data_inova_2011$cand_reelec_2012)
data_inova_2011$cand_age_2012    <- as.numeric(data_inova_2011$cand_age_2012)

# rename colnames
colnames(data_inova_2011) <- c( "idx", "codigo1", "codigo_ibge_1", "codigo_ibge_2", "IDHM", "Sigla_Estado", "municipio", "Estado",                  
                                "Distância_Até_Brasília_log", "Distância_até_Capital_log",  "Município_Metropolitano", "População",                
                                "Partido_PT","Prêmio_Inovação_2011", "População2", "Conselho", "Reeleição",  
                                "cand_sex_2011", "Escolaridade_Prefeito", "Idade_Prefeito",  "Escolaridade_da_Burocracia",        
                                "Terceirização_da_Burocracia", "Especialização_da_Burocracia",  "Politização_da_Burocracia", 
                                "Alinhamento_Estadual","Alinhamento_Federal", "Alinhado_Ambos", "Não_Alinhado", "UF",                     
                                "Município", "municipio.y", "Orçamento",   "Orçamento_log",       
                                "IQB_ACP", "IQB_IA")
#===== IQB PCA ======#
acp2011 <- function(x, Alinhamento){
  glm(Prêmio_Inovação_2011 ~  
        IQB_ACP +
        Alinhamento +
        IDHM +
        Conselho +
        Orçamento_log +
        log(População) +
        Distância_até_Capital_log +
        Município_Metropolitano +
        Reeleição +
        Idade_Prefeito +
        Escolaridade_Prefeito, 
      data = x, family = "binomial")
}

# execute s
acp_estadual_2011 <- acp2011(data_inova_2011, data_inova_2011$Alinhamento_Estadual)
acp_federal_2011 <- acp2011(data_inova_2011, data_inova_2011$Alinhamento_Federal)
acp_ambos_2011 <- acp2011(data_inova_2011, data_inova_2011$Alinhado_Ambos)
acp_n_alinhado_2011 <- acp2011(data_inova_2011, data_inova_2011$Não_Alinhado)

# display results
stargazer(acp_estadual_2011, acp_federal_2011, acp_ambos_2011, acp_n_alinhado_2011,
          type = "text", title = "Results", style = "ajps", apply.coef = exp,  p.auto=FALSE,
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# standardized coefficients
lm.beta(acp_estadual_2011)
lm.beta(acp_federal_2011)
lm.beta(acp_ambos_2011)
lm.beta(acp_n_alinhado_2011)

#===== IQBM SEPARATED ======#
des2011 <- function(x, Alinhamento){
  glm(  Prêmio_Inovação_2011 ~  
          Escolaridade_da_Burocracia + 
          Terceirização_da_Burocracia + 
          Politização_da_Burocracia +  
          Alinhamento +
          Conselho +
          Orçamento_log +
          Distância_até_Capital_log +
          Município_Metropolitano +
          Reeleição +
          Idade_Prefeito +
          Escolaridade_Prefeito +
          log(População) +
          IDHM, 
        data = x, family = "binomial")
}


des_estadual_2011 <- des2011(data_inova_2011, data_inova_2011$Alinhamento_Estadual)
des_federal_2011 <- des2011(data_inova_2011, data_inova_2011$Alinhamento_Federal)
des_ambos_2011 <- des2011(data_inova_2011, data_inova_2011$Alinhado_Ambos)
des_n_alinhado_2011 <- des2011(data_inova_2011, data_inova_2011$Não_Alinhado)

# display results
stargazer(des_estadual_2011, des_federal_2011, des_ambos_2011, des_n_alinhado_2011,
          type = "text", title = "Results", style = "ajps", apply.coef = exp,  p.auto=FALSE,
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# run standardized coefficients
lm.beta(des_estadual_2011)
lm.beta(des_federal_2011)
lm.beta(des_ambos_2011)
lm.beta(des_n_alinhado_2011)


# iqb factor  #

ia_2011 <- function(x, Alinhamento){
  glm(Prêmio_Inovação_2011 ~  
        IQB_IA +
        Alinhamento +
        IDHM +
        Conselho +
        Orçamento_log +
        log(População) +
        Distância_até_Capital_log +
        Município_Metropolitano +
        Reeleição +
        Idade_Prefeito +
        Escolaridade_Prefeito, 
      data = x, family = "binomial")
}

# execute models
ia_estadual_2011 <- ia_2011(data_inova_2011, data_inova_2011$Alinhamento_Estadual)
ia_federal_2011 <- ia_2011(data_inova_2011, data_inova_2011$Alinhamento_Federal)
ia_ambos_2011 <- ia_2011(data_inova_2011, data_inova_2011$Alinhado_Ambos)
ia_n_alinhado_2011 <- ia_2011(data_inova_2011, data_inova_2011$Não_Alinhado)

# display results
stargazer(ia_estadual_2011, ia_federal_2011, ia_ambos_2011, ia_n_alinhado_2011,
          type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Alinhamento Estadual", "Alinhamento Federal", "Alinhamento Ambos", "Não Alinhado"))  

# run standardized coefficients
lm.beta(ia_estadual_2011)
lm.beta(ia_federal_2011)
lm.beta(ia_ambos_2011)
lm.beta(ia_n_alinhado_2011)
© 2018 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
API
Training
Shop
Blog
About