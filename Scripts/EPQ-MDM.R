rm(list=ls())

source("funciones.R")
library(tidyverse)
library(ggthemes)
library(ggjoy)

script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- paste0(dirname(script.dir),"/")
bases.dir<-paste(dir,"Bases/",sep="")
resultados.dir <- paste0(dir,"Resultados/")
resultadosEPQ.dir <- paste0(resultados.dir,"EPQ/")

ind.t2.16 <- read_delim(paste0(bases.dir,"usu_individual_T216.txt"),delim = ";")
ind.t3.16 <- read_delim(paste0(bases.dir,"usu_individual_t316.txt"),delim = ";")
ind.t4.16 <- read_delim(paste0(bases.dir,"usu_individual_t416.txt"),delim = ";")
ind.t1.17 <- read_delim(paste0(bases.dir,"usu_individual_t117.txt"),delim = ";")


Variables <- c("CODUSU","ANO4","TRIMESTRE" ,"NRO_HOGAR","COMPONENTE","AGLOMERADO", "CH04", "CH06", "CH12",
               "H15","ESTADO","CAT_OCUP","PP02E","PP02I", "INTENSI","PP04A", "PP04B_COD",
               "PP07H", "PP10A","PP11L","PP11L1","PP11M", "PP11N", "PP11O",
               "P21","PP08J1","PONDERA", "PP04C", "NIVEL_ED", "PONDIIO")

Panel.t2.16 <- ind.t2.16[,c(paste(Variables))]
Panel.t3.16 <- ind.t3.16[,c(paste(Variables))]
Panel.t4.16 <- ind.t4.16[,c(paste(Variables))]
Panel.t1.17 <- ind.t1.17[,c(paste(Variables))]



Paneles <- bind_rows(Panel.t2.16,Panel.t3.16,Panel.t4.16,Panel.t1.17) %>%
    mutate  (PP04C = as.numeric(paste(PP04C)),
             NIVEL_ED = as.numeric(paste(NIVEL_ED)),
             Trimestre = paste(ANO4, TRIMESTRE, sep="_"),
             Id_Trimestre = match(Trimestre,unique(Trimestre))) %>% 
    group_by(CODUSU,NRO_HOGAR,COMPONENTE) %>% 
    mutate  (Cod_Desocup = paste0(lead(ESTADO),ESTADO,CAT_OCUP,lead(Id_Trimestre)-Id_Trimestre),
             En_Panel    = ifelse(lead(Id_Trimestre)-Id_Trimestre ==1,1,0),
             CH06_Diff   = abs(lead(CH06)-CH06)<=2,
             CH04_Diff   = lead(CH04)==CH04,
             Consistencia       =  ifelse((CH06_Diff & CH04_Diff) == FALSE,"inconsistente","consistente"),
             Tamano_estab = ifelse(PP04C<=6,yes = "1a10",no = 
                                     ifelse(PP04C>6 & PP04C<=10, yes = "11a200", no =
                                                       ifelse(PP04C>10 & PP04C<=12,
                                                              yes = "mas_200", no = NA ))),
             Nivel_ed = ifelse(NIVEL_ED %in% c(1:3,7),yes = "menor_Secundaria",no = 
                                          ifelse(NIVEL_ED %in% c(4:6), yes = "Secundario_o_mayor", no = NA )))

rm(list=c(ind.t2.16, ind.t3.16, ind.t4.16, ind.t1.17, 
          Panel.t2.16,Panel.t3.16, Panel.t4.16,Panel.t1.17))

Resumen_asalariados <- Paneles %>%
  filter(.,CAT_OCUP==3 & ESTADO==1) %>% 
  group_by(Trimestre) %>%
  summarise(
    Registros             =n(),
    Total                 = sum_na(PONDERA),
    Tasa_inf              = sum_na((PONDERA*(PP07H-1)))/
                            Total,
    Priv                  = sum_na(PONDERA[PP04A==2])/
                            sum_na(PONDERA[PP04A %in% c(1,2)]),
    Pub                   = sum_na(PONDERA[PP04A==1])/
                            sum_na(PONDERA[PP04A %in% c(1,2)]),
    tamano                = sum_na(PONDERA[!is.na(Tamano_estab)]),
    Hombres               = sum_na(PONDERA[CH04==1])/
                            Total,
    Mujeres               = sum_na(PONDERA[CH04==2])/
                            Total,
    '1:10'                 = sum_na(PONDERA[Tamano_estab =="1a10"])/
                            tamano,
    '11:200'               = sum_na(PONDERA[Tamano_estab =="11a200"])/
                            tamano,          
    '>200'                = sum_na(PONDERA[Tamano_estab =="mas_200"])/
                            tamano,
    Tamano_ed             = sum_na(PONDERA[!is.na(Nivel_ed)]),
    menor_Secundaria      = sum_na(PONDERA[Nivel_ed =="menor_Secundaria"])/
                             Tamano_ed,
    Secundario_o_mayor      = sum_na(PONDERA[Nivel_ed =="Secundario_o_mayor"])/
                            Tamano_ed,
    "Prec_1a10"            = sum_na(PONDERA[PP07H == 2 & Tamano_estab =="1a10"])/
      tamano,
    "NOPrec_1a10"          = sum_na(PONDERA[!(PP07H == 2 & Tamano_estab =="1a10") & !is.na(Tamano_estab)])/
      tamano,             
    Salario_Promedio      = weighted.mean(P21,PONDIIO),
    Salario_sin_SAC       = weighted.mean(P21-coalesce(PP08J1,0L),PONDIIO),
    Conjunto              = "Total")  %>% 
  ungroup(.) %>% 
  slice(-n())


####Nuevos Desocup Asal####
Resumen_ND_Asal <- Paneles %>%
  filter(.,Cod_Desocup==2131, Consistencia =="consistente") %>% 
  group_by(Trimestre) %>% 
  summarise(
    Registros             =n(),
    Total                 = sum_na(PONDERA),
    Tasa_inf              = sum_na((PONDERA*(PP07H-1)))/
                            Total,
    Priv                  = sum_na(PONDERA[PP04A==2])/
                            sum_na(PONDERA[PP04A %in% c(1,2)]),
    Pub                   = sum_na(PONDERA[PP04A==1])/
                            sum_na(PONDERA[PP04A %in% c(1,2)]),
    Hombres               = sum_na(PONDERA[CH04==1])/
                            Total,
    Mujeres               = sum_na(PONDERA[CH04==2])/
                            Total,
    tamano                = sum_na(PONDERA[!is.na(Tamano_estab)]),
    '1:10'                 = sum_na(PONDERA[Tamano_estab =="1a10"])/
                            tamano,
    '11:200'                = sum_na(PONDERA[Tamano_estab =="11a200"])/
                            tamano,
    '>200'                = sum_na(PONDERA[Tamano_estab =="mas_200"])/
                            tamano,
    Tamano_ed             = sum_na(PONDERA[!is.na(Nivel_ed)]),
    menor_Secundaria      = sum_na(PONDERA[Nivel_ed =="menor_Secundaria"])/
                            Tamano_ed,
    Secundario_o_mayor      = sum_na(PONDERA[Nivel_ed =="Secundario_o_mayor"])/
                            Tamano_ed,
    "Prec_1a10"            = sum_na(PONDERA[PP07H == 2 & Tamano_estab =="1a10"])/
                            tamano,
    "NOPrec_1a10"          = sum_na(PONDERA[!(PP07H == 2 & Tamano_estab =="1a10") & !is.na(Tamano_estab)])/
                            tamano,
    Salario_Promedio      = weighted.mean(P21,PONDIIO),
    Salario_sin_SAC       = weighted.mean(P21-coalesce(PP08J1,0L),PONDIIO),
    Conjunto              = "Nuevos desocupados")



#### Resumen Asalariados y Nuevos Desocupados Asalariados####
Asalariados <- bind_rows(Resumen_asalariados,Resumen_ND_Asal) 

############ Distribuciones entre categorias####
####Formales e Informales#### 
ggplot(Asalariados, aes(x = Conjunto, y= Tasa_inf, fill= Conjunto,
                        label = sprintf("%1.2f%%", 100*Tasa_inf))) +
  geom_col(alpha=1)+
  geom_text(nudge_y = 0.1)+
  labs(x="", y="", title="Tasa de informalidad", 
       subtitle = "Nuevos Desocupados y Total de Asalariados",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
   theme(legend.position="none",
         plot.title = element_text(size=12))+
   scale_y_continuous(limits = c(0,1) ,labels = scales::percent)+
  scale_fill_manual(values = c("black", "gray77"), 
                    labels=c("Nuevos Desocupados", "Total")) +
  facet_grid(.~Trimestre, labeller = "label_both")
  


ggsave(paste0(resultadosEPQ.dir,"Tasa informalidad.PNG"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Tasa informalidad_2.PNG"), scale = 2)

####Sector Publico y Privado####

  priv_pub <- Asalariados %>% 
  select(Trimestre, Pub,Priv,Conjunto) %>% 
  gather(., Sector, proporcion,2:3)


ggplot(priv_pub, aes(x = Conjunto, y= proporcion, fill= Sector)) +
  geom_col(alpha=1) +
  labs(x="", y="", title="Proporción de asalariados según sector", 
       subtitle = "Nuevos Desocupados y Total de Asalariados",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="bottom",
        axis.text.x = element_text(size=8))+
scale_fill_manual(values = c("gray95", "black"), 
                  labels=c("Privado","Público")) +
  scale_y_continuous(limits = c(0,1) ,labels = scales::percent)+
  facet_grid(.~Trimestre,  labeller = "label_both")  

ggsave(paste0(resultadosEPQ.dir,"Sector.PNG"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Sector_2.PNG"), scale = 2)


####Sexo####

Sexo <- Asalariados %>% 
  select(Trimestre, Hombres,Mujeres,Conjunto) %>% 
  gather(., Sexo, proporcion,2:3)


ggplot(Sexo, aes(x = Conjunto, y= proporcion, fill= Sexo)) +
  geom_col(alpha=1) +
  labs(x="", y="", title="Proporción de asalariados según género", 
       subtitle = "Nuevos Desocupados y Total de Asalariados",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c("gray95", "black"), 
                     labels=c("Hombres","Mujeres")) +
  scale_y_continuous(limits = c(0,1) ,labels = scales::percent)+
  facet_grid(.~Trimestre,  labeller = "label_both")  

ggsave(paste0(resultadosEPQ.dir,"Sexo.png"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Genero_2.png"), scale = 2)

####Tamaño de Establecimiento####
Tamano_establecimiento <-  Asalariados %>%  
  select(Trimestre,`1:10`, `11:200`,`>200`,Conjunto) %>%     
  gather(., Tamano_establecimiento,proporcion,2:4)

Tamano_establecimiento$Tamano_establecimiento <- factor(Tamano_establecimiento$Tamano_establecimiento,
                                                        levels = unique(Tamano_establecimiento$Tamano_establecimiento))

ggplot(Tamano_establecimiento, aes(x = Conjunto,
                                   y = proporcion, 
                                   fill =Tamano_establecimiento, 
                                   label = sprintf("%1.1f%%", 100*(proporcion)))) + 
  geom_col(position = "dodge",alpha=1) +
  geom_text(position = position_dodge(width=1),vjust = -1,hjust=0.3, size=2.4)+
  labs(x="", y="", title="Proporción de asalariados según tamaño de empresa", 
         subtitle = "Nuevos Desocupados y Total de Asalariados",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.title = element_text(size=8),
        legend.key.size =unit(.3,"cm"),
        legend.margin = margin(-1,-.1,3,-1,"cm"))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent)+
  scale_fill_manual(values = c("black", "gray80", "gray20", "gray60", "gray40"),
                    name= "Tamaño del \nestablecimiento") +
  facet_grid(.~Trimestre, labeller = "label_both")



ggsave(paste0(resultadosEPQ.dir,"Tamano empresa.png"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Tamano empresa_2.png"), scale = 2)

####Nivel Educativo####
Nivel_educativo <-  Asalariados %>%  
  select(Trimestre,menor_Secundaria, Secundario_o_mayor,
         Conjunto)%>% 
  gather(., Nivel_educativo,proporcion,2:3)

Nivel_educativo$Nivel_educativo <- factor(Nivel_educativo$Nivel_educativo,
                                              levels = unique(Nivel_educativo$Nivel_educativo))

ggplot(Nivel_educativo, aes(x = Conjunto, y = proporcion, fill =Nivel_educativo,
                            label = sprintf("%1.2f%%", 100*(proporcion)))) + 
  geom_col(position = "dodge",alpha=1) +
    geom_text(position = position_dodge(width=1),vjust = -1,hjust=0.5, size=2)+
  labs(x="", y="", title="Proporción de asalariados según nivel educativo",
         subtitle = "Nuevos Desocupados y Total de Asalariados",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("black", "gray80", "gray20", "gray60", "gray40"),
                               name= "Nivel Educativo") +
  facet_grid(.~Trimestre, labeller = "label_both")

ggsave(paste0(resultadosEPQ.dir,"Nivel Educativo.png"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Nivel Educativo_2.png"), scale = 2)

####Salario Promedio####
ggplot(Asalariados, aes(x = Conjunto, y= Salario_Promedio, fill= Conjunto))+
  geom_col(alpha=1)+
  labs(x="", y="", title="Salario Promedio mensual, en pesos corrientes", 
       subtitle = "Nuevos Desocupados y Total de asalariados",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="none",
        plot.title = element_text(size=12))+
  scale_fill_manual(values = c("black", "gray77"), 
                    labels=c("Nuevos Desocupados", "Total")) +
  facet_grid(.~Trimestre, labeller = "label_both")

ggsave(paste0(resultadosEPQ.dir,"Salario promedio.PNG"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Salario promedio_2.PNG"), scale = 2)


####Salario Kernel####
gdata <- Paneles %>% 
  filter(ESTADO == 1, CAT_OCUP ==3, Trimestre != "2017_1") %>% 
  mutate(Cod_Desocup = ifelse(Cod_Desocup == 2131, "Nuevos Desocupados", "Resto"),
         Cod_Desocup = as.factor(Cod_Desocup)) %>% 
  group_by(Trimestre, Cod_Desocup) %>% 
  mutate(P21_media = weighted.mean(P21,PONDIIO))

ggplot(gdata, aes(x= P21,y=Trimestre, weights= PONDIIO, fill = Cod_Desocup)) +
  geom_joy(alpha=0.6, scale=1)+
  labs(x="Ingreso ocupación principal", y="Trimestre", title="Distribución del Salario Nominal por trimestre", 
       subtitle = "Nuevos Desocupados y Total de asalariados",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,25000))+
  theme_tufte()+
  theme(legend.position="bottom",
        plot.title = element_text(size=12))+
  scale_fill_discrete(guide=guide_legend(title=NULL) )

ggsave(paste0(resultadosEPQ.dir,"Distribución del salario.PNG"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Distribución del salario_2.PNG"), scale = 2)

  


############ PROBABILIDADES por categoría##################
Resumen_Asal_en_Panel <- Paneles %>%
  filter(.,CAT_OCUP==3 & ESTADO==1,En_Panel==1, Consistencia =="consistente") %>% 
  group_by(Trimestre) %>%
  summarise(
    nd                  = sum_na(PONDERA[Cod_Desocup==2131]),
    Total          = nd/sum_na(PONDERA),
    Informal         = sum_na(PONDERA[Cod_Desocup==2131 & PP07H==2 ])/
                          sum_na(PONDERA[PP07H==2]),
   
    Formal           = sum_na(PONDERA[Cod_Desocup==2131 & PP07H==1 ])/
                          sum_na(PONDERA[PP07H==1]),

    Privado          = sum_na(PONDERA[Cod_Desocup==2131 & PP04A==2])/
                          sum_na(PONDERA[PP04A==2]),
    
    Público          = sum_na(PONDERA[Cod_Desocup==2131 & PP04A==1])/
                          sum_na(PONDERA[PP04A==1]),                        

    Hombres          = sum_na(PONDERA[Cod_Desocup==2131 & CH04==1])/
                          sum_na(PONDERA[CH04==1]),                        
    Mujeres          = sum_na(PONDERA[Cod_Desocup==2131 & CH04==2])/
                          sum_na(PONDERA[CH04==2]),                                                            
    "1:10"            = sum_na(PONDERA[Cod_Desocup==2131 & Tamano_estab =="1a10"])/
                          sum_na(PONDERA[Tamano_estab =="1a10"]),
    "11:200"          = sum_na(PONDERA[Cod_Desocup==2131 & Tamano_estab =="11a200"])/
                          sum_na(PONDERA[Tamano_estab =="11a200"]),
    ">200"           = sum_na(PONDERA[Cod_Desocup==2131 & Tamano_estab =="mas_200"])/
                          sum_na(PONDERA[Tamano_estab =="mas_200"]),
    menor_Secundaria = sum_na(PONDERA[Cod_Desocup==2131 & Nivel_ed =="menor_Secundaria"])/
                          sum_na(PONDERA[Nivel_ed =="menor_Secundaria"]),
    Secundario_o_mayor = sum_na(PONDERA[Cod_Desocup==2131 & Nivel_ed =="Secundario_o_mayor"])/
                          sum_na(PONDERA[Nivel_ed =="Secundario_o_mayor"]))
    
    


#########Graficos para probabilidades########
####Formales e Informales#### 

Informales_prop <- Resumen_Asal_en_Panel %>% 
  select(Trimestre, Formal,Total, Informal) %>% 
  gather(., Sector, proporcion,2:4)

Informales_prop$Sector <- factor(Informales_prop$Sector,
                                 levels = unique(Informales_prop$Sector))

ggplot(Informales_prop, aes(x = Sector, y= proporcion, fill= Sector,
                        label = sprintf("%1.2f%%", 100*proporcion))) +
  geom_col(alpha=1)+
  geom_text(nudge_y = 0.005)+
  labs(x="", y="",
       title = "Probabilidad de perder el empleo según trimestre y Precariedad",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="none",
        plot.title = element_text(size=12))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("gray77","black","gray40"), 
                    labels=c("ND Formales","ND Totales","ND Informales")) +
  facet_grid(.~Trimestre, labeller = "label_both")


ggsave(paste0(resultadosEPQ.dir,"Probabilidades_Informales.PNG"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Probabilidades_Informales2.PNG"), scale = 2)

####Sector Publico y Privado####

priv_pub_prop <- Resumen_Asal_en_Panel %>% 
  select(Trimestre, Público, Total, Privado) %>% 
  gather(., Sector, proporcion,2:4)

priv_pub_prop$Sector <- factor(priv_pub_prop$Sector, 
                               levels = unique(priv_pub_prop$Sector))

ggplot(priv_pub_prop, aes(x = Sector, y= proporcion, fill= Sector,
                            label = sprintf("%1.2f%%", 100*proporcion))) +
  geom_col(alpha=1)+
  geom_text(nudge_y = 0.005)+
  labs(x="", y="", 
       title = "Probabilidad de perder el empleo según trimestre y Sector",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="none",
        plot.title = element_text(size=12))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("gray77","black","gray40"), 
                    labels=c("Público","Totales","Privado")) +
  facet_grid(.~Trimestre,  labeller = "label_both")  

ggsave(paste0(resultadosEPQ.dir,"Probabilidades_Sector.PNG"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Probabilidades_Sector_2.PNG"), scale = 2)


####Sexo####

Sexo_prop <- Resumen_Asal_en_Panel %>% 
  select(Trimestre, Hombres, Total, Mujeres) %>% 
  gather(., Sexo, proporcion,2:4)


Sexo_prop$Sexo <- factor(Sexo_prop$Sexo,
                         levels=unique(Sexo_prop$Sexo))

ggplot(Sexo_prop, aes(x = Sexo, y= proporcion, fill= Sexo,
                      label = sprintf("%1.2f%%", 100*proporcion))) +
  geom_col(alpha=1) +
  geom_text(nudge_y = 0.005)+
    labs(x="", y="", 
         title = "Probabilidad de perder el empleo según trimestre y Género",
         caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="none",
        plot.title = element_text(size=12))+
  scale_fill_manual(values = c("gray77", "black","gray40"), 
                    labels=c("Hombres","Totales","Mujeres")) +
  facet_grid(.~Trimestre,  labeller = "label_both")  

ggsave(paste0(resultadosEPQ.dir,"Probabilidades_Sexo.png"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Probabilidades_Sexo_2.png"), scale = 2)

####Tamaño de Establecimiento####
Tamano_establecimiento_prop <-  Resumen_Asal_en_Panel %>%  
  select(Trimestre,Total,`1:10`, `11:200`,`>200`) %>%     
  gather(., Tamano_establecimiento,proporcion,2:5)

Tamano_establecimiento_prop$Tamano_establecimiento <- factor(Tamano_establecimiento_prop$Tamano_establecimiento,
                                                        levels = unique(Tamano_establecimiento_prop$Tamano_establecimiento))

ggplot(Tamano_establecimiento_prop, aes(x = Tamano_establecimiento,
                                   y = proporcion, 
                                   fill =Tamano_establecimiento, 
                                   label = sprintf("%1.2f%%", 100*(proporcion)))) + 
  geom_col(alpha=1)+
  geom_text(nudge_y = 0.001, size=2)+
  labs(x="", y="",
       title = "Probabilidad de perder el empleo según trimestre y \nTamaño de establecimiento",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="none",
        plot.title = element_text(size=12))+
  scale_fill_manual(values = c("black", "gray20", "gray80", "gray40"),
                    name= "Tamaño del \nestablecimiento") +
  facet_grid(.~Trimestre, labeller = "label_both")


ggsave(paste0(resultadosEPQ.dir,"Probabilidad_Tamano empresa.png"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Probabilidad_Tamano empresa_2.png"), scale = 2)

####Nivel Educativo####
Nivel_educativo_prop <-  Resumen_Asal_en_Panel %>%  
  select(Trimestre,menor_Secundaria,Total, Secundario_o_mayor) %>%     
  gather(., Nivel_educativo,proporcion,2:4)

Nivel_educativo_prop$Nivel_educativo <- factor(Nivel_educativo_prop$Nivel_educativo,
                                          levels = unique(Nivel_educativo_prop$Nivel_educativo))

ggplot(Nivel_educativo_prop, aes(x = Nivel_educativo,
                                        y = proporcion, 
                                        fill =Nivel_educativo, 
                                        label = sprintf("%1.2f%%", 100*(proporcion)))) + 
  geom_col(alpha=1)+
  geom_text(nudge_y = 0.005)+
  labs(x="", y="",
       title = "Probabilidad de perder el empleo según trimestre y \nNivel Educativo",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme_tufte()+
  theme(legend.position="none",
        plot.title = element_text(size=12))+
  scale_fill_manual(values = c("gray80","black", "gray40"),
                    name= "Nivel_educativo") +
  facet_grid(.~Trimestre, labeller = "label_both")

ggsave(paste0(resultadosEPQ.dir,"Probabilidad_Nivel Educativo.png"), scale = 1)
ggsave(paste0(resultadosEPQ.dir,"Probabilidad_Nivel Educativo_2.png"), scale = 2)

write.xlsx(as.data.frame(Asalariados),paste0(resultadosEPQ.dir,"Nuevos_Desocup.xlsx"),row.names = FALSE,sheetName = "Resumen_asalariados")
write.xlsx(as.data.frame(Resumen_Asal_en_Panel),paste0(resultadosEPQ.dir,"Nuevos_Desocup.xlsx"),row.names = FALSE,sheetName = "Probabilidades", append = TRUE)

