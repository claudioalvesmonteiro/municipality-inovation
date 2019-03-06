#-----------------------------------------#
# BUREAUCRACY INOVATION - Graphs          #    
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
# install.packages(c("readr", "raster", "readxl", "ggthemes", "qdap", "readxl", "ggplot2", "directlabels", "ggrepel", "readr",
#                   "plyr", "rgdal", "ggmap", "maps", "mapdata", "maptools", "stringi", "DT", "xtable", "ggpubr", "dplyr"))

# carregar pacotes
library(readr); library(raster); library(readxl); library(ggthemes); library(qdap); library(ggplot2); library(directlabels)
library(ggrepel); library(readr); library(plyr); library(rgdal); library(ggmap); library(maps); library(mapdata)
library(maptools); library(stringi); library(DT); library(xtable); library(ggpubr); library(dplyr)


#====== mapping missing data ========#

# load shapefile and reanme citycode
shape_brasil <- shapefile("Original Data/Geodata/bra_cities_2010/municipios_2010.shp")
shape_brasil$code_muni2 <- shape_brasil$codigo_ibg

# load data
data_inova_2007 <- read.csv("/Users/mpcp/Documents/Claudio/untitled folder/municipality-innovation/Analysis Data/data_inova_2008.csv")
data_inova_2011 <- read.csv("/Users/mpcp/Documents/Claudio/untitled folder/municipality-innovation/Analysis Data/data_inova_2012.csv")

# merge and create miss variable
shape_miss <- merge(shape_brasil, data_inova_2007, by = "code_muni2", all = T)
shape_miss$miss <- 0
shape_miss$miss[is.na(shape_miss$IDHM)] <- 1

# tranformar shapefile em polygonsdataframe
miss_fortity <- fortify(shape_miss, region = "id")
miss_data <- join(miss_fortity, shape_miss@data, by = "id")

ggplot() +
  geom_polygon(data = miss_data, aes(x = long, y = lat, group = group,
                                    fill = miss_data$miss, colour= miss_data$miss))+
  coord_fixed() +
  theme_nothing()

#-------- by state

#=== 2007 ===#
missdata_est <- merge(data_inova_2007, shape_brasil@data, by = "code_muni2", all = T)
missdata_est$miss <- 0
missdata_est$miss[is.na(missdata_est$IDHM)] <- 1

# 
missdata_est07 <- data.frame(aggregate(missdata_est$miss, by=list(Category=missdata_est$uf), FUN=sum))
countcit_est07 <- data.frame(table(missdata_est$uf))
countcit_est07$Category <- countcit_est07$Var1

data_miss07 <- merge(missdata_est07, countcit_est07, by = "Category")
data_miss07$missing <- data_miss07$x / data_miss07$Freq
data_miss07$missing <- round(data_miss07$missing, 4)

data_miss07 <- data_miss07[order(data_miss07$missing),]
data_miss07$Category <- factor(data_miss07$Category, levels = data_miss07$Category)

miss07bar<- ggplot(data_miss07, aes(x = data_miss07$Category, y = data_miss07$missing))+
  geom_bar(stat = "identity", #aes(fill = data_miss07$iqb_pcr_2012), 
           fill = "#1c3c40") +
  xlab("") + ylab("ProporÃ§Ã£o de Casos Faltantes") +
  geom_label(aes(label = data_miss07$missing), size = 2.8) +
  theme(axis.text.y = element_text(colour = 'black', size = 13), 
        axis.title.y = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2),
        axis.text.x = element_text(colour = 'black', size = 13), 
        axis.title.x = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2))+
  ggtitle("2007") +
  guides(fill = F)+
  theme_arretado()+
  coord_flip()
miss07bar

#=== 2011 ===#
missdata_est2 <- merge(data_inova_2011, shape_brasil@data, by = "code_muni2", all = T)
missdata_est2$miss <- 0
missdata_est2$miss[is.na(missdata_est2$IDHM)] <- 1

# 
missdata_est11 <- data.frame(aggregate(missdata_est2$miss, by=list(Category=missdata_est2$uf), FUN=sum))
countcit_est11 <- data.frame(table(missdata_est2$uf))
countcit_est11$Category <- countcit_est11$Var1

data_miss11 <- merge(missdata_est11, countcit_est11, by = "Category")
data_miss11$missing <- data_miss11$x / data_miss11$Freq
data_miss11$missing <- round(data_miss11$missing, 4)

data_miss11 <- data_miss11[order(data_miss11$missing),]
data_miss11$Category <- factor(data_miss11$Category, levels = data_miss11$Category)

miss11bar<- ggplot(data_miss11, aes(x = data_miss11$Category, y = data_miss11$missing))+
  geom_bar(stat = "identity", #aes(fill = data_miss07$iqb_pcr_2012), 
           fill = "#1c3c40") +
  xlab("") + ylab("ProporÃ§Ã£o de Casos Faltantes") +
  geom_label(aes(label = data_miss11$missing), size = 2.8) +
  theme(axis.text.y = element_text(colour = 'black', size = 13), 
        axis.title.y = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2),
        axis.text.x = element_text(colour = 'black', size = 13), 
        axis.title.x = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2))+
  ggtitle("2011") +
  guides(fill = F)+
  theme_arretado()+
  coord_flip()
miss11bar

#@@@@@@@@@@@@@@@@

barmiss <- ggarrange(miss07bar, miss11bar)
ggsave("barmiss.png", barmiss, width = 9, height =8, units = "in")

#============


#======= mapping state ranking ======#
# carregar banco de dados e shapefile brasil
shape_brasil <- shapefile("Geodata/bra_cities_2010/estados_2010.shp")

data_inova_2008 <- read_delim("data_inova_2008.csv", ",", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                              trim_ws = TRUE)         

# manipular banco #
data_metro <- read_excel("C:/Users/Monteiro-DataPC/Documents/Consulting/Analytique/Inova??o Municipal (Carol)/Replication Documentation/Original Data/dados_metropolitano.xls")
data_t1 <- merge(data_inova_2008, data_metro, by = "code_muni2", all = T)

# do total de municipios , quantos inovaram, por estado
data_map_2008_1 <- data_t[, c("inova5", "code_muni", "code_muni2", "Nome_UF.y", "municipio.y")]

# criar variavel de municipios  e que inovaram
data_map_2008_1$incritos <- as.numeric(!is.na(data_map_2008_1$inova5))

data_map_2008_1$inovaram <- 0
data_map_2008_1$inovaram[data_map_2008_1$inova5 == 1] <- 1

#--- calcular inovacao por estado ---#
inova_map_2008_ins <- table(data_map_2008_1$Nome_UF.y, data_map_2008_1$incritos)
inova_map_2008_ino <- table(data_map_2008_1$Nome_UF.y, data_map_2008_1$inovaram)

inova_map_2008 <- data.frame(inova_map_2008_ins[,2],inova_map_2008_ino[,2] )
inova_map_2008$prop_inova <- inova_map_2008$inova_map_2008_ino...2. / inova_map_2008$inova_map_2008_ins...2.
inova_map_2008$estado <- rownames(inova_map_2008)

#--- merge final ---#

best_match= function(string_vector,string_replacement){
  library(purrr)
  library(stringi)
  
  s<-string_replacement %>% 
    purrr::map_int(~{
      .x %>% 
        RecordLinkage::levenshteinSim(string_vector) %>%
        match(max(.),.)
    })
  string_vector[s]<-string_replacement
  return(string_vector)
}

shape_brasil@data$estado <- best_match(shape_brasil@data$nome, inova_map_2008$estado)

shape_brasil <- merge(shape_brasil, inova_map_2008, by = "estado")

#-----------------------#
# Mapa Inovacao 2008    #
#-----------------------#

#--- arretado theme ---#
theme_arretado<- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black", size=11, hjust= .5, vjust=.5, face="plain"),
          axis.text.y = element_text(colour="black", size=11, angle= 0, hjust=1, vjust=0, face="plain"), 
          axis.title.x = element_text(colour="black", size=11, angle= 0, hjust=.5, vjust=0, face="bold"),
          axis.title.y = element_text(colour="black", size=11, angle= 90, hjust=0.5, vjust=0.6, face="bold"),
          title = element_text(colour="black", size=13, angle=0, hjust= .5, vjust=.5, face="bold"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}

# definir labels no mapa
shape_brasil <- shape_brasil[order(shape_brasil$prop_inova),]
shape_brasil$destaq <- 1
shape_brasil$destaq[22:26] <- ""

shape_brasil$destaq <- with(shape_brasil, paste0(shape_brasil$destaq, shape_brasil$estado))
shape_brasil$destaq_cod <- grepl(shape_brasil$destaq, pattern = "1")
shape_brasil$destaq[shape_brasil$destaq_cod == TRUE ] <- ""

# tranformar shapefile em polygonsdataframe
data_fortity <- fortify(shape_brasil, region = "estado")
Estado <- shape_brasil@data$estado

# extrair centroides dos poligonos
centroids.df <- as.data.frame(coordinates(shape_brasil))
names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column Estados

# This shapefile contained population data, let's plot it.
variavel <- shape_brasil@data$prop_inova
nomes_centroides <- shape_brasil$destaq

map_dataframe <- data.frame(Estado, variavel, centroids.df, nomes_centroides)

map1_2008 <- ggplot(data = map_dataframe, aes(map_id = Estado)) + 
  geom_map(aes(fill = map_dataframe$variavel), colour = grey(0.85),  map = data_fortity) +
  expand_limits(x = data_fortity$long, y = data_fortity$lat) +
  # scale_fill_gradient(colours=inferno(10, alpha = 1, begin = 1, end = 0))+
  scale_fill_gradient(name = "Propor??o" , low="#6dc066", high= "#021631")+
  geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 2.3, 
                   color = "black") + #add labels at centroids
  coord_fixed(1) +
  labs(title = "2007")+
  theme_nothing(legend = T)+
  theme(legend.key.size = unit(7, "mm"),
        legend.text = element_text(size = 9, hjust = 0, vjust = 3),
        legend.title = element_text(size = 9, hjust = 10, vjust = 3)
  )
map1_2008

#ggsave("mapa_ino1_2008.png", plot_2008, width = 8, height = 8, units = "in")

#===========================#
#========== 2012 ===========#

data_inova_2012 <- read_delim("data_inova_2012.csv", 
                              ",", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                              trim_ws = TRUE)         


# manipular banco #
data_t2 <- merge(data_inova_2012, data_metro, by = "code_muni2", all = T)

data_t$inova7 


# do total de municipios inscritos, quantos inovaram, por estado
data_map_2012_1 <- data_t[, c("inova7", "code_muni", "code_muni2", "Nome_UF.y", "municipio.y")]

# criar variavel de municipios inscritos e que inovaram
data_map_2012_1$incritos <- as.numeric(!is.na(data_map_2012_1$inova7))

data_map_2012_1$inovaram <- 0
data_map_2012_1$inovaram[data_map_2012_1$inova7 == 1] <- 1

#--- calcular inovacao por estado ---#
inova_map_2012_ins <- table(data_map_2012_1$Nome_UF.y, data_map_2012_1$incritos)
inova_map_2012_ino <- table(data_map_2012_1$Nome_UF.y, data_map_2012_1$inovaram)

inova_map_2012 <- data.frame(inova_map_2012_ins[,2],inova_map_2012_ino[,2] )
inova_map_2012$prop_inova <- inova_map_2012$inova_map_2012_ino...2. / inova_map_2012$inova_map_2012_ins...2.
inova_map_2012$estado <- rownames(inova_map_2012)

#--- merge final ---#

best_match= function(string_vector,string_replacement){
  library(purrr)
  library(stringi)
  
  s<-string_replacement %>% 
    purrr::map_int(~{
      .x %>% 
        RecordLinkage::levenshteinSim(string_vector) %>%
        match(max(.),.)
    })
  string_vector[s]<-string_replacement
  return(string_vector)
}

shape_brasil@data$estado <- best_match(shape_brasil@data$nome, inova_map_2012$estado)

shape_brasil <- merge(shape_brasil, inova_map_2012, by = "estado")

#-----------------------#
# Mapa Inovacao 2012    #
#-----------------------#

#--- arretado theme ---#
theme_arretado<- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=11,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=11,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}

# definir labels no mapa
shape_brasil <- shape_brasil[order(shape_brasil$prop_inova.y),]
shape_brasil$destaq <- 1
shape_brasil$destaq[22:26] <- ""

shape_brasil$destaq <- with(shape_brasil, paste0(shape_brasil$destaq, shape_brasil$estado))
shape_brasil$destaq_cod <- grepl(shape_brasil$destaq, pattern = "1")
shape_brasil$destaq[shape_brasil$destaq_cod == TRUE ] <- ""

# tranformar shapefile em polygonsdataframe
data_fortity <- fortify(shape_brasil, region = "estado")
Estado <- shape_brasil@data$estado

# extrair centroides dos poligonos
centroids.df <- as.data.frame(coordinates(shape_brasil))
names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column Estados

# This shapefile contained population data, let's plot it.
variavel <- shape_brasil@data$prop_inova.y
nomes_centroides <- shape_brasil$destaq

map_dataframe <- data.frame(Estado, variavel, centroids.df, nomes_centroides)

map1_2012 <- ggplot(data = map_dataframe, aes(map_id = Estado)) + 
  geom_map(aes(fill = map_dataframe$variavel), colour = grey(0.85),  map = data_fortity) +
  expand_limits(x = data_fortity$long, y = data_fortity$lat) +
  # scale_fill_gradient(colours=inferno(10, alpha = 1, begin = 1, end = 0))+
  scale_fill_gradient(name = "Propor??o" , low="#6dc066", high= "#021631")+
  geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 2.3, 
                   color = "black") + #add labels at centroids
  coord_fixed(1) +
  labs(title = "2011")+
  theme_nothing(legend = T)+
  theme(legend.key.size = unit(7, "mm"),
        legend.text = element_text(size = 9, hjust = 0, vjust = 3),
        legend.title = element_text(size = 9, hjust = 10, vjust = 3)
  )
map1_2012

#ggsave("mapa_ino1_2012.png", plot_2012, width = 8, height = 8, units = "in")

#---- arrange maps ----#

maps1 <- ggarrange(map1_2008, map1_2012, common.legend = T, legend = "bottom")
ggsave("maps.png", maps1, width = 5, height =4, units = "in")


#================================#
#===== barra inovacao total =====#

#---- 2008 ----#
barra_inova_2008 <- data.frame(table(data_inova_2008$inova5))
barra_inova_2008$Var1 <- factor(barra_inova_2008$Var1, levels = c('0', '1'), labels = c("N?o Inovou", "Inovou"))

barra_inova1 <- ggplot(barra_inova_2008, aes(x = barra_inova_2008$Var1, y = barra_inova_2008$Freq))+
  geom_bar(stat = "identity", aes(fill = barra_inova_2008$Var1)) +
  xlab("Inova??o") + ylab("N?mero de Munic?pios") +
  scale_fill_manual("Sexo", values = c("Inovou" = "#021631", "N?o Inovou" =  "lightgreen"))+
  geom_label(aes(y = 100,label = barra_inova_2008$Freq)) +
  theme(axis.text.y = element_text(colour = 'black', size = 13), 
        axis.title.y = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2),
        axis.text.x = element_text(colour = 'black', size = 13), 
        axis.title.x = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2))+
  ggtitle("2007") +
  guides(fill = F)+
  theme_arretado()

barra_inova1
#ggsave("barino.png", barra_inova1, width = 6, height =6, units = "in"

#---- 2012 ----#
barra_inova_2012 <- data.frame(table(data_inova_2012$inova7))
barra_inova_2012$Var1 <- factor(barra_inova_2012$Var1, levels = c('0', '1'), labels = c("N?o Inovou", "Inovou"))

barra_inova2 <- ggplot(barra_inova_2012, aes(x = barra_inova_2012$Var1, y = barra_inova_2012$Freq))+
  geom_bar(stat = "identity", aes(fill = barra_inova_2012$Var1)) +
  xlab("Inova??o") + ylab("N?mero de Munic?pios") +
  scale_fill_manual("Sexo", values = c("Inovou" = "#021631", "N?o Inovou" =  "lightgreen"))+
  geom_label(aes(y = 100,label = barra_inova_2012$Freq)) +
  theme(axis.text.y = element_text(colour = 'black', size = 13), 
        axis.title.y = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2),
        axis.text.x = element_text(colour = 'black', size = 13), 
        axis.title.x = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2))+
  ggtitle("2011") +
  guides(fill = F)+
  theme_arretado()

barra_inova2
#ggsave("barino_2012.png", barra_inova2, width = 6, height =6, units = "in")

# arrange and save
plotbar1 <- ggarrange(barra_inova1, barra_inova2)
ggsave("bar1.png", plotbar1, width = 6, height =3, units = "in")


#=================================#
#===== barra inovacao estado =====#

#---- 2008 ----#

inova_map_2008 <- inova_map_2008[complete.cases(inova_map_2008),]

# order 
inova_map_2008 <- inova_map_2008[order(inova_map_2008$prop_inova),]
inova_map_2008$estado <- factor(inova_map_2008$estado, levels = inova_map_2008$estado)

inova_map_2008$prop_inova <- round(inova_map_2008$prop_inova, 2)

barra_est1 <- ggplot(inova_map_2008, aes(x = inova_map_2008$estado, y = inova_map_2008$prop_inova))+
  geom_bar(stat = "identity", aes(fill = inova_map_2008$prop_inova), fill = "#1c3c40") +
  xlab("") + ylab("Propor??o") +
  geom_label(aes(label = inova_map_2008$prop_inova),  size = 3.5) +
  theme(axis.text.y = element_text(colour = 'black', size = 13), 
        axis.title.y = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2),
        axis.text.x = element_text(colour = 'black', size = 13), 
        axis.title.x = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2))+
  ggtitle("2007") +
  guides(fill = F)+
  theme_arretado()+
  coord_flip()
barra_est1


#---- 2012 ----#

inova_map_2012 <- inova_map_2012[complete.cases(inova_map_2012),]

# order 
inova_map_2012 <- inova_map_2012[order(inova_map_2012$prop_inova),]
inova_map_2012$estado <- factor(inova_map_2012$estado, levels = inova_map_2012$estado)

inova_map_2012$prop_inova <- round(inova_map_2012$prop_inova, 2)

barra_est2 <- ggplot(inova_map_2012, aes(x = inova_map_2012$estado, y = inova_map_2012$prop_inova))+
  geom_bar(stat = "identity", aes(fill = inova_map_2012$prop_inova), fill = "#1c3c40") +
  xlab("") + ylab("Propor??o") +
  geom_label(aes(label = inova_map_2012$prop_inova), size = 3.5) +
  theme(axis.text.y = element_text(colour = 'black', size = 13), 
        axis.title.y = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2),
        axis.text.x = element_text(colour = 'black', size = 13), 
        axis.title.x = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2))+
  ggtitle("2011") +
  guides(fill = F)+
  theme_arretado()+
  coord_flip()
barra_est2

# arrange and save
plotbar2 <- ggarrange(barra_est1, barra_est2)
ggsave("bar2.png", plotbar2, width = 8.2, height =9, units = "in")

#=======================#
#===== IQB GRAPHS ======#

#===== IQBM ACP 2007 =====#

# select variables
iqb_data07 <- data_t1[,c("Nome_UF.y", "iqb_pcr_2008", "iqb_factor_2008")]
iqb_data07 <- iqb_data07[complete.cases(iqb_data07),]

# manipulate
iqb_est07 <- aggregate(iqb_data07, by = list(iqb_data07$Nome_UF.y), mean)

iqb_est07 <- iqb_est07[order(-iqb_est07$iqb_pcr_2008),]
iqb_est07$Group.1 <- factor(iqb_est07$Group.1, levels = iqb_est07$Group.1)
iqb_est07$iqb_pcr_2008 <- round(iqb_est07$iqb_pcr_2008, 4)

#---- plot ----#
bar_iqb07 <- ggplot(iqb_est07, aes(x = iqb_est07$Group.1, y = iqb_est07$iqb_pcr_2008))+
  geom_bar(stat = "identity", aes(fill = iqb_est07$iqb_pcr_2008), fill = "#1c3c40") +
  xlab("") + ylab("IQBM") +
  geom_label(aes(label = iqb_est07$iqb_pcr_2008), size = 2.8) +
  theme(axis.text.y = element_text(colour = 'black', size = 13), 
        axis.title.y = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2),
        axis.text.x = element_text(colour = 'black', size = 13), 
        axis.title.x = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2))+
  ggtitle("2007") +
  guides(fill = F)+
  theme_arretado()+
  coord_flip()
bar_iqb07

#==== IQB ACP 2011 ====#
iqb_data11 <- data_t2[,c("Nome_UF.y", "iqb_pcr_2012", "iqb_factor_2012")]
iqb_data11 <- iqb_data11[complete.cases(iqb_data11),]

iqb_est11 <- aggregate(iqb_data11, by = list(iqb_data11$Nome_UF.y), mean)
iqb_est11 <- iqb_est11[order(-iqb_est11$iqb_pcr_2012),]

iqb_est11$Group.1 <- factor(iqb_est11$Group.1, levels = iqb_est11$Group.1)

iqb_est11$iqb_pcr_2012 <- round(iqb_est11$iqb_pcr_2012, 4)


bar_iqb11 <- ggplot(iqb_est11, aes(x = iqb_est11$Group.1, y = iqb_est11$iqb_pcr_2012))+
  geom_bar(stat = "identity", aes(fill = iqb_est11$iqb_pcr_2012), fill = "#1c3c40") +
  xlab("") + ylab("IQBM") +
  geom_label(aes(label = iqb_est11$iqb_pcr_2012), size = 2.8) +
  theme(axis.text.y = element_text(colour = 'black', size = 13), 
        axis.title.y = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2),
        axis.text.x = element_text(colour = 'black', size = 13), 
        axis.title.x = element_text(size = 13, 
                                    hjust = 0.5, vjust = 0.2))+
  ggtitle("2012") +
  guides(fill = F)+
  theme_arretado()+
  coord_flip()
bar_iqb11

bars_estiqb <- ggarrange(bar_iqb07, bar_iqb11)
ggsave("barsiqb.png", bars_estiqb, width = 8, height =6, units = "in")

getwd(
)

data_inova_2008$iqb_pcr_2008


#==================================================#
#            LOGISTIC REGRESSION PLOTS             #
#==================================================#

#-------------------------#
# Innovation X Bureaucracy 
#-------------------------#

# iqb pca 2007
p1 <- ggplot(data_inova_2008, aes(x = iqb_pcr_2008, y = inova5)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se = FALSE) +
  labs(title = "2007", x = "IQB ACP", y = "Inovação Municipal") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=18, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=12)) 


# iqb factor 2007
p2 <- ggplot(data_inova_2008, aes(x = iqb_factor_2008, y = inova5)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se = FALSE)+
labs(title = "2011", x = "IQB IA", y = "Inovação Municipal") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=18, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=12)) 


# iqb pca 2011
p3 <- ggplot(data_inova_2012, aes(x = iqb_pcr_2012, y = inova7)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se = FALSE)+
  labs(x = "IQB ACP", y = "Inovação Municipal") +
  theme(axis.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=12)) 

# iqb factor 2011
p4 <- ggplot(data_inova_2012, aes(x = iqb_factor_2012, y =inova7)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se = FALSE) +
  labs(x = "IQB IA", y = "Inovação Municipal") +
  theme(axis.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=12)) 

# arrange plots and save
library(ggpubr)
figure <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave("bur_inova_plot.png", width = 9, height = 6)


#-------------------------#
# Innovation X Money$ 
#-------------------------#

# iqb pca 2007
p11 <- ggplot(data_inova_2008, aes(x = log_orcamento_2008, y = inova5)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se = FALSE) +
  labs(title = "2007", x = "Orçamento Municipal (log)", y = "Inovação Municipal") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=18, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=12)) 

# iqb factor 2011
p22 <- ggplot(data_inova_2012, aes(x = log_orcamento_2012, y =inova7)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se = FALSE) +
  labs(title = "2011", x = "Orçamento Municipal (log)", y = "Inovação Municipal") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=18, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=12)) 

# arrange plots and save
library(ggpubr)
figure2 <- ggarrange(p11, p22)
figure2
ggsave("orc_inova_plot.png", width = 10, height = 4)

#--------------------#
# regression results #
#--------------------#

#---- coeffcients plot 1 ----#
coef.plot1 <- function(model, title){
  coefs <- as.data.frame(summary(model)$coefficients[-1,1:2])
  names(coefs)[2] <- "se" 
  coefs$vars <- rownames(coefs)
  
  coefplot <- ggplot(coefs, aes(vars, Estimate)) +
    geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
    geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se),lwd=.6, colour="black", width=0) +
    geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se),lwd=1, colour="black", width=0) +
    geom_point(size=1.3, pch=21, fill="black") + scale_x_discrete(name = "") + 
    scale_y_continuous(name = "Intervalo de Confiança") +
    theme_arretado()+
    coord_flip() +
    labs(title = title)
  return(coefplot)
}

#---- plot models 2007 ----#
cp1 <- plot_odds(acp_estadual_2008, "Alin. Estadual")
cp2 <- plot_odds(acp_federal_2008, "Alin. Federal")
cp3 <- plot_odds(acp_ambos_2008, "Alin. em Ambos")
cp4 <- plot_odds(acp_n_alinhado_2008, "Não Alinhado")

# arrange plots and save
figure3 <- ggarrange(cp1, cp2, cp3, cp4, nrow = 2, ncol = 2)
annotate_figure(figure3,
                top = text_grob("Resultado Modelos de Inovação Municipal 2007",face = "bold", size = 16))
ggsave("result_plot_IQBACP20071.png", width = 11, height = 8)


#---- plot models 2011 ----#
cp11 <- plot_odds(acp_estadual_2011, "Alin. Estadual")
cp22 <- plot_odds(acp_federal_2011, "Alin. Federal")
cp33 <- plot_odds(acp_ambos_2011, "Alin. em Ambos")
cp44 <- plot_odds(acp_n_alinhado_2011, "Não Alinhado")

# arrange plots and save
figure4 <- ggarrange(cp11, cp22, cp33, cp44, nrow = 2, ncol = 2)
annotate_figure(figure4,
                top = text_grob("Resultado Modelos de Inovação Municipal 2011",face = "bold", size = 16))
ggsave("result_plot_IQBACP2011.png", width = 11, height = 8)

#======================================#
# outlier boxplots    (BUILDING)       #

# 
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

data_inova_2008$outlier <- is_outlier(data_inova_2008$IQB_ACP)
sum(data_inova_2011$outlier)

data_inova_2011$outlier <- is_outlier(data_inova_2011$IQB_ACP)
data_inova_2011$outlier
data_inova_2008$outlier <- ifelse(data_inova_2008$outlier == TRUE, data_inova_2008$municipio, "")

ggplot(data = data_inova_2008, aes(x = "", y = data_inova_2008$IQB_ACP)) + 
  geom_boxplot() 

ggplot(data = data_inova_2008, aes(IQB_ACP)) +
  geom_histogram(binwidth = 0.01)


#================================#
#


plot_odds<-function(x, title = NULL){
  tmp<-data.frame(cbind(exp(coef(x)), exp(confint(x))))
  odds<-tmp[-1,]
  names(odds)<-c('OR', 'lower', 'upper')
  odds$vars<-row.names(odds)
  ticks<-c(1)
  
  ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    scale_y_log10(breaks=ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype=2) +
    coord_flip() +
    labs(title = title, x = 'Variables', y = 'OR') +
    theme_bw()
}








