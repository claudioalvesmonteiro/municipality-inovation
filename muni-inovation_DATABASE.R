#-----------------------------------------#
# BUREAUCRACY INOVATION - Data Building   #    
#-----------------------------------------#
# Recife - Pernambuco - Brasil            #
# November 2016 - September 2017          #    
#-----------------------------------------#
# Author: Claudio A. Monteiro             #
# claudiomonteirol.a@gmail.com            #
#-----------------------------------------#
# Any question contact the developer      #
# #UseFreeSoftware                        #
#-----------------------------------------#

# install packages
# install.packages(c("readr","stringr","stringi","readxl","dplyr","foreign","xlsx","corrplot","data.table", "psych"))

# load required packages
library(readr); library(stringr); library(stringi); library(readxl); library(dplyr)
library(foreign); library(xlsx); library(corrplot); library(data.table); library(psych)


# set database directory
setwd("/Users/mpcp/Documents/Claudio/untitled folder/municipality-innovation/Original Data")

#---------------------------------#
# Human Development Index (IDHM) 
#---------------------------------#

# load AtlasBrasil data
atlas_data <- AtlasBrasil_Consulta <- data.frame(read_delim("AtlasBrasil_Consulta.csv", 
                                                 ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                                 trim_ws = TRUE))

# remove 1st line 'Brasil'
atlas_data <- atlas_data[-1,]

# transform COD.IBGE to numeric and recode to code_muni
atlas_data$code_muni <- as.numeric(atlas_data$COD.IBGE)

# replace ',' for '.' and recode to IDHM
atlas_data$IDHM <- str_replace(atlas_data$IDHM..2010., pattern = ",", ".")

# transform to numeric
atlas_data$IDHM <- as.numeric(atlas_data$IDHM)

#--------------------------------------------#
# Distance to federal and state capital
#--------------------------------------------#

distance_data <- data.frame(read_delim("distanciaatebrasilia.csv", 
                            ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                            trim_ws = TRUE))
  

#---------------------------#
# Log to federal capital 

distance_data$log_to_brasilia <- log(distance_data$DISTANCIABRASILIA)

# replace -Inf for 0 and transform to numeric
distance_data$log_to_brasilia <- str_replace(distance_data$log_to_brasilia, pattern = "-Inf", "0")
distance_data$log_to_brasilia <- as.numeric(distance_data$log_to_brasilia)

#----------------------------#
# Log to state capital

distance_data$log_to_capital <- log(distance_data$DISTANCIACAPITAL)

# replace -Inf for 0 and transform to numeric
distance_data$log_to_capital <- str_replace(distance_data$log_to_capital, pattern = "-Inf", "0")
distance_data$log_to_capital <- as.numeric(distance_data$log_to_capital)

#-----------------#
# Merge data

data_inova <- merge(atlas_data[,5:6], distance_data[,c(1:4,12,13)], by = "code_muni")

#-------------------------------#
# 2008 and 2012 populations
#-------------------------------#

#------------#
# 2008   

pop_data_2008 <- read_excel("POP2008_DOU.xls")

# remove unwanted data
pop_data_2008 <- as.data.frame(pop_data_2008[-(1:4),-1])

# rename variables
colnames(pop_data_2008) <- c("cod1", "cod2", "municipios", "pop_2008")

# transform to numeric and round to 
pop_data_2008$cod1 <- as.numeric(pop_data_2008$cod1)
pop_data_2008$cod1 <- round(pop_data_2008$cod1, 1)

pop_data_2008$pop_2008 <- as.numeric(pop_data_2008$pop_2008)
pop_data_2008$pop_2008 <- round(pop_data_2008$pop_2008, 1)

# combine cod1 and cod2 into code_muni2
pop_data_2008$code_muni2<- with(pop_data_2008, paste0(cod1, cod2))

#------------#
# 2012   

pop_data_2012 <- data.frame(read_excel("estimativa_2012_DOU_28_08_2012_xls.xls"))

# remove unwanted data
pop_data_2012 <- as.data.frame(pop_data_2012[-c(1:2),-1])

# rename variables
colnames(pop_data_2012) <- c("cod1", "cod2", "municipios", "pop_2012")

# transform to numeric and round to 
pop_data_2012$cod1 <- as.numeric(pop_data_2012$cod1)

pop_data_2012$cod1 <- round(pop_data_2012$cod1, 1)

pop_data_2012$pop_2012 <- as.numeric(pop_data_2012$pop_2012)
pop_data_2012$pop_2012 <- round(pop_data_2012$pop_2012, 1)

# combine cod1 and cod2 into code_muni2
pop_data_2012$code_muni2<- with(pop_data_2012, paste0(cod1, cod2))

#--------------#
# Merge data

data_inova <- merge(pop_data_2008[4:5], data_inova, by = "code_muni2")
data_inova <- merge(pop_data_2012[4:5], data_inova, by = "code_muni2")

#------------------------------#
# Metropolitan 
#------------------------------#
data_metro <- read_excel("dados_metropolitano.xls")

# create and define metropolitan city
data_metro$metropolitano <- 0
data_metro$metropolitano[str_detect(data_metro$MesorregiãoGeográfica_Nome, pattern = "Metropolitana")] <- 1

# transform to numeric
data_metro$metropolitano <- as.numeric(data_metro$metropolitano)

# merge data
data_inova <- merge(data_inova, data_metro[,c(3, 8, 14)], by = "code_muni2")

#----------------------------#
# 2008 Bureaucracy Variables #
#----------------------------#

# load data
data_bur_total_2008 <- data.frame(read_delim("Burocratas_2008 (total).csv", 
                                  ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                  trim_ws = TRUE))
data_bur_esc_2008 <- data.frame(read_delim("Burocratas_2008 (escolaridade).csv", 
                                ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                trim_ws = TRUE))
  
#--------------------#
# manipulate data

# transform to numeric
data_bur_total_2008 <- data.frame(sapply(data_bur_total_2008, function(x) as.numeric(x)), stringsAsFactors = F)
data_bur_esc_2008 <-  data.frame(sapply(data_bur_esc_2008, function(x) as.numeric(x)), stringsAsFactors = F)

#---------------------------------------------------------#
# 1 - Knowledge of Permanent Bureaucracy
# ( Estatutarios + CLT (ensino superior e pos-graduacaoo) / total de funcion?rios)

data_bur_total_2008 <- data_bur_total_2008[!(data_bur_total_2008$estatutarios + data_bur_total_2008$clt)  == 0,]
data_bur_esc_2008 <- data_bur_esc_2008[!(data_bur_esc_2008$ESTATUTARIOS + data_bur_esc_2008$CLT)  == 0,]

# calculate variable
data_bur_esc_2008$bur_escol_2008 <- (data_bur_esc_2008$ESTATUTARIOS_ES + data_bur_esc_2008$ESTATUTARIOS_POS + 
                                       data_bur_esc_2008$CLT_ES + data_bur_esc_2008$CLT_POS) / data_bur_esc_2008$ADM_DIRETA

#--------------------------------------------------------#
# 2 - Privatization of bureaucracy
# Sem vinculo / total de funcion?rios.

# calculate variable
data_bur_esc_2008$bur_terc_2008 <-  (data_bur_esc_2008$SEMVINC_ES +  data_bur_esc_2008$SEMVINC_POS) /
  (data_bur_esc_2008$ESTATUTARIOS + data_bur_esc_2008$CLT)

#-------------------------------------------------------------#
# 3 - Specialization of Bureaucracy
# (N?mero de burocratas estatut?rios + CLT / total de burocratas)

# calculate variable
data_bur_total_2008$bur_espel_2008 <- (data_bur_total_2008$estatutarios + data_bur_total_2008$clt)  / 
  data_bur_total_2008$total_fun_direta

#-------------------------------------------------------------#
# 4 - Politization of Bureaucracy
# (Burocratas comissionados / total de burocratas)

# calculate variable
data_bur_total_2008$bur_polit_2008 <- data_bur_total_2008$comissionados / 
  (data_bur_total_2008$estatutarios + data_bur_total_2008$clt) 

#----------------#
# Merge data

data_bur_2008 <- merge(data_bur_esc_2008[,c(1, 32:33)], data_bur_total_2008[,c(1, 7:8)], by="code_muni")
data_inova <- merge(data_inova, data_bur_2008, by="code_muni")

#--------------------------------#
# VARIAVEIS BUROCRACIA 2012/2011 #
#--------------------------------#

# load data
data_bur_total_2011 <- data.frame(read_delim("burocratas_total_2011.csv", 
                                  ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                  trim_ws = TRUE))
  
data_bur_total_2012 <- data.frame(read_delim("funcionarios_2012.csv", 
                                  ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                  trim_ws = TRUE))
  
# remove rows without data
tail(data_bur_total_2011$CodIBGE, 60)
data_bur_total_2011 <- data_bur_total_2011[1:5565,]

# transform to numeric
data_bur_total_2011 <- data.frame(sapply(data_bur_total_2011, function(x) as.numeric(as.character(x))))
data_bur_total_2012 <- data.frame(sapply(data_bur_total_2012, function(x) as.numeric(as.character(x))))

#---------------------------------------------------------#
# 1 - Knowledge of Permanent Bureaucracy
# ( Estatut?rios + CLT (ensino superior e p?s-gradua??o) / total de funcion?rios)
data_bur_total_2012 <- data_bur_total_2012[!(data_bur_total_2012$estatutarios + data_bur_total_2012$celetistas)  == 0,]
data_bur_total_2011 <- data_bur_total_2011[!(data_bur_total_2011$Estatutarios +data_bur_total_2011$CLT)  == 0,]


# calculate variable
data_bur_total_2011$bur_escol_2011 <- (data_bur_total_2011$Estatutarios_ES + data_bur_total_2011$Estatutarios_PG + 
                                         data_bur_total_2011$CLT_S + data_bur_total_2011$CLT_PG) / data_bur_total_2011$Total

#--------------------------------------------------------#
# 2 - Privatization of bureaucracy
# Sem vinculo / total de funcion?rios

# calculate variable
data_bur_total_2011$bur_terc_2011 <- (data_bur_total_2011$SemVinculo_ES + data_bur_total_2011$SemVinculo_PG) / 
  (data_bur_total_2011$Estatutarios + data_bur_total_2011$CLT)

#-------------------------------------------------------------#
# 3 - Specialization of Bureaucracy
# (Numero de burocratas estatut?rios + CLT / total de burocratas)

# calculate variable
data_bur_total_2012$bur_espel_2012 <- (data_bur_total_2012$estatutarios + data_bur_total_2012$celetistas)  / data_bur_total_2012$total

#-------------------------------------------------------------#
# 4 - Politization of Bureaucracy
# (Burocratas comissionados / total de burocratas)

# calculate variable
data_bur_total_2012$bur_polit_2012 <- data_bur_total_2012$comissionados / (data_bur_total_2012$estatutarios + data_bur_total_2012$celetistas) 

#---------------#
# Merge data
data_bur_total_2011$code_muni <- data_bur_total_2011$CodIBGE
data_inova <- merge(data_inova, data_bur_total_2011[,c(32:34)], by="code_muni")

data_bur_total_2012$code_muni <- data_bur_total_2012$CodIbge
data_inova <- merge(data_inova, data_bur_total_2012[,c(8:10)], by="code_muni")

#-----------------------------#
# Management counsel     
#-----------------------------#

#--------------#
# 2008

# load data
consel_2008 <- data.frame(read_delim("CM_2008_recod.csv", 
                          ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                          trim_ws = TRUE))
  
# recode
consel_2008$bur_consel_2008 <- consel_2008$CM_dic_2008
consel_2008$code_muni <- consel_2008$CodMunicipio

# merge data
data_inova <- merge(data_inova, consel_2008[, 9:10], by = "code_muni")

#--------------#
# 2012

# load data
consel_2012 <-  data.frame(read_delim("CM_2012_rec.csv", 
                           ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                           trim_ws = TRUE))

# recode
consel_2012$bur_consel_2012 <- consel_2012$CM_dic_2012
consel_2012$code_muni <- consel_2012$CodMunicipio

# merge data
data_inova <- merge(data_inova, consel_2012[, 17:18], by = "code_muni")

#-----------------------------#
# Political Alignment         
#-----------------------------#

dados_eleicoes <- read_excel("dados_eleicoes_1.xlsx")

# criar variavel para padronizacao
dados_eleicoes$code_muni2 <- dados_eleicoes$COD_IBGE

# selecionar e manipular apenas 2008
dados_eleicoes_2008 <- subset(dados_eleicoes, dados_eleicoes$ANO == 2008)

# selecionar e manipular apenas 2012
dados_eleicoes_2012 <- subset(dados_eleicoes, dados_eleicoes$ANO == 2012)

#--------#
#  2008  

## criar variaveis partido estadual e federal ##
dados_eleicoes_2008 <- mutate(dados_eleicoes_2008, partido_gov_federal_2008 = "PT")

# criar variavel alinhamento federal (2008)
dados_eleicoes_2008 <- mutate(dados_eleicoes_2008, alinhamento_federal_2008 = ifelse(pt_cand_el_ant == partido_gov_federal_2008, 1, 0))

# criar variavel alinhamento estadual (2008)
dados_eleicoes_2008 <- mutate(dados_eleicoes_2008, alinhamento_estadual_2008 = ifelse(pt_cand_el_ant == pt_gov, 1, 0))

# manipular alinhamento #
dados_eleicoes_2008$alinhamento_politico_2008 <- dados_eleicoes_2008$alinhamento_estadual_2008 + dados_eleicoes_2008$alinhamento_federal_2008

# alinhamento em ambos
dados_eleicoes_2008 <- mutate(dados_eleicoes_2008, alinhamento_ambos_2008 = ifelse(alinhamento_politico_2008 == 2, 1, 0))

# nao alinhado
dados_eleicoes_2008 <- mutate(dados_eleicoes_2008, n_alinhado_2008 = ifelse(alinhamento_politico_2008 == 0, 1, 0))

#--------#
#  2012  #

## criar variaveis partido estadual e federal ##
dados_eleicoes_2012 <- mutate(dados_eleicoes_2012,  partido_gov_federal_2012 = "PT")

# criar variavel alinhamento federal (2012)
dados_eleicoes_2012 <- mutate(dados_eleicoes_2012, alinhamento_federal_2012 = ifelse(pt_cand_el_ant == partido_gov_federal_2012, 1, 0))

# criar variavel alinhamento estadual (2012)
dados_eleicoes_2012 <- mutate(dados_eleicoes_2012, alinhamento_estadual_2012 = ifelse(pt_cand_el_ant == pt_gov, 1, 0))

# manipular alinhamento (2012)
dados_eleicoes_2012$alinhamento_politico_2012 <- dados_eleicoes_2012$alinhamento_estadual_2012 + dados_eleicoes_2012$alinhamento_federal_2012

# alinhamento em ambos
dados_eleicoes_2012 <- mutate(dados_eleicoes_2012, alinhamento_ambos_2012 = ifelse(alinhamento_politico_2012 == 2, 1, 0))

# nao alinhado
dados_eleicoes_2012 <- mutate(dados_eleicoes_2012, n_alinhado_2012 = ifelse(alinhamento_politico_2012 == 0, 1, 0))

# partido do PT
dados_eleicoes_2012 <- mutate(dados_eleicoes_2012, partido_pt_2012 = ifelse(pt == "PT", 1, 0))

#------------#
# merge data #

data_inova <- merge(data_inova, dados_eleicoes_2008[,c(29,31:35)], by ="code_muni2")
data_inova <- merge(data_inova, dados_eleicoes_2012[,c(29,31:36)], by ="code_muni2")

#-------------------------#
# Candidate data
#-------------------------#

#--------------------#
# 2008

prefeitos2008 <- read_excel("prefeitos2008.xls")

colnames(prefeitos2008) <- c("code_muni", "cand_reelec_2008", "cand_sex_2008", 
                             "cand_age_2008", "escolaridade_2008")

# recode escolarity
prefeitos2008$cand_esc_2008 <- 0

cond <- c("Pós-graduação", "Ensino superior completo")   
prefeitos2008$cand_esc_2008[prefeitos2008$escolaridade_2008 %in% cond] <- 1

#--------------#
#  merge data

data_inova <- merge(prefeitos2008[,-5], data_inova, by = "code_muni")

#--------------------#
# 2012

prefeitos2012 <- read_excel("prefeitos2012.xls")

colnames(prefeitos2012) <- c("code_muni", "cand_reelec_2012", "cand_sex_2012", 
                             "cand_age_2012", "escolaridade_2012")

# recode escolarity
prefeitos2012$cand_esc_2012 <- 0

cond <- c("Pós-graduação", "Ensino superior completo")   
prefeitos2012$cand_esc_2012[prefeitos2012$escolaridade_2012 %in% cond] <- 1

#--------------#
#  merge data

data_inova <- merge(prefeitos2012[,-5], data_inova, by = "code_muni")

#--------------#
# INOVATION 5  #
#--------------#

# load data
inova5_manipul <-  data.frame(read_delim("inova5_manipul.csv", 
                              ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                              trim_ws = TRUE))

#----------------------------------#
# manipulate data_inova for merging

# remover acentos
data_inova$municipio <- stri_trans_general(data_inova$MUNICIPIO, "Latin-ASCII")
data_inova$municipio <- tolower(data_inova$municipio)

# create merge variable 
inova5_manipul$code_merge <- with(inova5_manipul, paste0(UF, municipio))
data_inova$code_merge <- with(data_inova, paste0(UF, municipio))

# remove duplicated
inova5_manipul <- inova5_manipul[!duplicated(inova5_manipul$code_merge),]

#-------------#
# merge data
data_inova_2008 <- merge(data_inova, inova5_manipul[,c(1,4:5)], by = "code_merge", all = T)

# remove na
data_inova_2008 <- data_inova_2008[!is.na(data_inova_2008$code_muni),]

# verificar e tranformar casos faltantes (inovadores/nao inovadores)
data_inova_2008$inova5 <- !is.na(data_inova_2008$inova5)
data_inova_2008$inova5 <- as.numeric(data_inova_2008$inova5)

data_inova_2008$inova5[data_inova_2008$habilitado == "Não"] <- 0

#-------------#
# INOVACAO 7  #
#-------------#

inova7 <- read_excel("inova7.xlsx")

# remover acentos
inova7$municipio <- stri_trans_general(inova7$municipio,"Latin-ASCII")
inova7$municipio <- tolower(inova7$municipio)

# merge var
inova7$code_merge <- with(inova7, paste0(UF, municipio))

# mergir bancos
inova7 <- inova7[!duplicated(inova7$code_merge),]
data_inova_2012 <- merge(data_inova, inova7[,c(1, 4:5)], by = "code_merge", all = T)

# treat duplicated
data_inova_2012 <- data_inova_2012[!is.na(data_inova_2012$code_muni),]

# verificar e tranformar casos faltantes (inovadores/nao inovadores)
data_inova_2012$inova7 <- !is.na(data_inova_2012$inova7)
data_inova_2012$inova7 <- as.numeric(data_inova_2012$inova7)

data_inova_2012$inova7[data_inova_2012$Habilitado == "Não"] <- 0

#----------------------------#
# Receita Orcamentaria       #
#----------------------------#
#* Variavel problematica com missing cases - salvar a parte

#----------------#
# 2008           #


# select data for 2008
data_inova_2008 <- data_inova_2008[,c("code_muni", "code_muni2", "code_merge", "IDHM", "UF", "municipio", "Nome_UF",
                                      "log_to_brasilia", "log_to_capital", "metropolitano", 
                                      
                                      "pop_2008",
                                      "inova5", "pop_2008", "bur_consel_2008",  "cand_reelec_2008", "cand_sex_2008", "cand_esc_2008", "cand_age_2008", 
                                      "bur_escol_2008","bur_terc_2008",  "bur_espel_2008","bur_polit_2008", "alinhamento_estadual_2008",
                                      "alinhamento_federal_2008", "alinhamento_ambos_2008", "n_alinhado_2008"
)]

# ler banco
RO2008 <- read_excel("RO2008.xlsx")

# tranformar em log
RO2008$log_orcamento_2008 <- log(as.numeric(RO2008$rec_orcamento_2008))

# merge var
RO2008$code_merge <- with(RO2008, paste0(UF, municipio))

# mergir bancos
data_inova_2008 <- merge(data_inova_2008, RO2008, by = "code_merge")

# remover duplicata e missing cases
data_inova_2008 <- data_inova_2008[!duplicated(data_inova_2008$code_muni),]
data_inova_2008 <- data_inova_2008[complete.cases(data_inova_2008),]

#---------------#  
# 2012          #

# select 2012 data
data_inova_2012 <- data_inova_2012[,c("code_muni", "code_muni2", "code_merge", "IDHM", "UF", "municipio", "Nome_UF",
                                      "log_to_brasilia", "log_to_capital", "metropolitano", 
                                      "pop_2012","partido_pt_2012",
                                      "inova7", "pop_2012", "bur_consel_2012",  "cand_reelec_2012", "cand_sex_2012", "cand_esc_2012", "cand_age_2012", 
                                      "bur_escol_2011","bur_terc_2011",  "bur_espel_2012","bur_polit_2012", "alinhamento_estadual_2012",
                                      "alinhamento_federal_2012", "alinhamento_ambos_2012", "n_alinhado_2012"
)]

# ler banco
RO2012 <- read_excel("RO2012.xlsx")

# tranformar em log
RO2012$log_orcamento_2012 <- log(as.numeric(RO2012$rec_orcamento_2012))

# merge var
RO2012$code_merge <- with(RO2012, paste0(UF, municipio))

# mergir bancos
data_inova_2012 <- merge(data_inova_2012, RO2012, by = "code_merge")

# remover duplicata e missing cases
data_inova_2012 <- data_inova_2012[!duplicated(data_inova_2012$code_muni),]
data_inova_2012 <- data_inova_2012[complete.cases(data_inova_2012),]

#===================================#
# QUALITY OF BUREAUCRACY INDEX      #
#===================================#

# select data
data_iqb_2008 <- data_inova_2008[,c("bur_escol_2008", "bur_terc_2008","bur_polit_2008")]
data_iqb_2012 <- data_inova_2012[,c("bur_escol_2011", "bur_terc_2011","bur_polit_2012")]

#---------------------------#
# testing adequacy of data  
#---------------------------#

#---- correlation table ----#
corrplot(cor(data_iqb_2008), method = "square", tl.col='black', tl.cex=.8, addCoef.col = "black") 
corrplot(cor(data_iqb_2012), method = "square", tl.col='black', tl.cex=.8, addCoef.col = "black")

#---- Bartlet's test ----#
bartlett.test(data_iqb_2008)
bartlett.test(data_iqb_2012)

#-------- KMO --------#
KMO(data_iqb_2008)
KMO(data_iqb_2012)

# function for range
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#---------------------------------------#
# Principal Components Analysis Method

#------------#
# 2008       #

# principal component analysis varimax
psych_pcr_2008 <- principal(data_iqb_2008, nfactors = 1, rotate = "varimax")

# display results
psych_pcr_2008
psych_pcr_2008$communality

pcr1 <- princomp(data_iqb_2008, cor=T)
pcr1
# component 1 to index
data_inova_2008$iqb_pcr_2008 <- 1 - range01(as.vector(psych_pcr_2008$scores))/1

#------------#
# 2012       

# principal component analysis varimax
psych_pcr_2012 <- principal(data_iqb_2012, nfactors = 1, rotate = "varimax")

# display results
psych_pcr_2012
psych_pcr_2012$communality

pcr2 <- princomp(data_iqb_2012, cor=T)

# component 1 to index
data_inova_2012$iqb_pcr_2012 <- 1 - range01(as.vector(psych_pcr_2012$scores))

#-----------#
# plot pcr

screeplot(pcr1, type = "lines")

#-----------------------#
# factor index method   
#-----------------------#

#==== 2008 ====#

# range
data_iqb_2008$bur_escol_2008 <- range01(data_iqb_2008$bur_escol_2008) 
data_iqb_2008$bur_polit_2008 <- range01(data_iqb_2008$bur_polit_2008) 
data_iqb_2008$bur_terc_2008 <- range01(data_iqb_2008$bur_terc_2008) 

# agregate
iqb_factor_2008 <- (data_iqb_2008$bur_escol_2008 + (1 - data_iqb_2008$bur_polit_2008) + 
                      (1 -data_iqb_2008$bur_terc_2008))

# range factor
data_inova_2008$iqb_factor_2008 <- range01(iqb_factor_2008)

#==== 2012 ====#

# range
data_iqb_2012$bur_escol_2011 <- range01(data_iqb_2012$bur_escol_2011) 
data_iqb_2012$bur_polit_2012 <- range01(data_iqb_2012$bur_polit_2012) 
data_iqb_2012$bur_terc_2011 <- range01(data_iqb_2012$bur_terc_2011) 

# agregate
iqb_factor_2012 <- (data_iqb_2012$bur_escol_2011 + ( 1 - data_iqb_2012$bur_polit_2012) + 
                      (1 - data_iqb_2012$bur_terc_2011)) 

# range factor
data_inova_2012$iqb_factor_2012 <- range01(iqb_factor_2012)

cor(data_inova_2012$iqb_factor_2012, data_inova_2012$iqb_pcr_2012)

#-----------------#
# Save data       #
#-----------------#
setwd("/Users/mpcp/Documents/Claudio/untitled folder/municipality-innovation/Analysis Data")

#-----------#
# 2008      #

write.csv(data_inova_2008, file = "data_inova_2008.csv")
#write.xlsx(data_inova_2008, file = "data_inova_2008.xls")

#-----------#
# 2012      #

write.csv(data_inova_2012, file = "data_inova_2012.csv")
#write.xlsx(data_inova_2012, file = "data_inova_2012.xls"
