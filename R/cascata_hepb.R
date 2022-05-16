####################################################################
###########  cascata hepatite B - população geral do Brasil ########
####################################################################
###### script desenvolvido por Mikael Lemos ########################
###### versão 1.0 - 08.05.2022 ##### R v.4.1.2 #####################
####################################################################

######
### Carregando / instalando pacotes
######

#install.packages('plyr')
library('plyr')

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

# install.packages("tidyverse")
library("tidyverse")

# install.packages("lubridate")
library("lubridate")

#install.packages("Amelia")
library("Amelia")

#install.packages('microbenchmark')
library("microbenchmark")

#install.packages('ggplot2movies')
library("ggplot2movies")

#install.packages('profvis')
library("profvis")

#install.packages('Rcpp')
library("Rcpp")

#install.packages('compiler')
library("compiler")

#install.packages('memoise')
library("memoise")

#install.packages('DiagrammeR')
library("DiagrammeR")

#install.packages('rio')
library("rio")

#install.packages('readr')
library("readr")

#install.packages('data.table')
library("data.table")

#install.packages('feather')
library("feather")

#install.packages('WDI')
library("WDI")

#install.packages('eeptools')
library("eeptools")

#install.packages("waffle")
library("waffle")

#install.packages("ggthemes")
library("ggthemes")

#install.packages("waffle")
library("waffle")

#install.packages("plotly")
library("plotly")

library("stringr")

library("ggplot2")

library(RColorBrewer)

#install.packages("pryr")
library(pryr)
#install.packages("openxlsx")
library("openxlsx")

#install.packages("diyar")
library("diyar")

#install.packages('gtools')
library(gtools)

#install.packages('scales')
library(scales)

#install.packages("read.dbc")
library(read.dbc)

#####################
######## Infectados # 1a Barra 
####################        

# número estimado de infectados - pop_geral ; pop_criancas

n_estimado_infectados_hbv_pop_geral <- round(população_geral(número) * 0.004, digits = 0) 

n_estimado_infectados_hbv_pop_criancas <- round(população_criancas(número) * 0.001, digits = 0) 

n_estimado_infectados_hbv_brasil = n_estimado_infectados_hbv_pop_geral + n_estimado_infectados_hbv_pop_criancas

#####################
######## Diagnosticados - 2a Barra
####################

##############
#### GAL 
##############

# Filtro opcional - Exame: 
GAL_exames <- filter(GAL, Exame == "Hepatite B, HBsAg" | Exame == "Hepatite B, anti-HBc IgM" | Exame == "Hepatite B, HBV-DNA" )

# Filtro - Resultado: 

GAL_exames_reagente_detectavel <- filter(GAL_exames, Resultado == "Detectavel" | Resultado == "Acima do limite de quantificacao" | Resultado == "Reagente" )

## Salvando tabela intermediária

write.csv(GAL_exames_reagente_detectavel, "C:/../GAL_exames_reagente_detectavel.csv")

n_GAL_exames_reagente_detectavel

####################
######## Tratados - Iniciaram tratamento - 3a Barra
####################

##############
#### SIA/APAC
##############

# Filtro CID - AP_CIDPRI

SIA_CID_hepb <- filter(SIA_hepb, AP_CIDPRI == "B180" | AP_CIDPRI == "B181" )

## Salvando tabela intermediária

write.csv(SIA_CID_hepb, "C:/../SIA_CID_hepb.csv")

# Filtro Procedimento Principal - AP_PRIPAL

SIA_CID_hepb_ini_trat <- filter(SIA_CID_hepb, AP_PRIPAL == "0604460058" | AP_PRIPAL == "0604300018" | AP_PRIPAL == "0604300034" | AP_PRIPAL == "0604300042" | AP_PRIPAL == "0604390041" |AP_PRIPAL == "0604460023" |AP_PRIPAL == "0604460031" | AP_PRIPAL == "0604460040" | AP_PRIPAL == "0604460066")

## Salvando tabela intermediária

write.csv(SIA_CID_hepb_ini_trat, "C:/../SIA_CID_hepb_ini_trat.csv")

# Filtro: Remover duplicidades

SIA_CID_hepb_ini_trat_un <- distinct(SIA_CID_hepb_ini_trat, AP_CNSPCN , .keep_all = TRUE)

## Salvando tabela intermediária

write.csv(SIA_CID_hepb_ini_trat_un, "C:/../SIA_CID_hepb_ini_trat_un.csv")

##############
#### Siclom-HV (Para cacatas >= 2021)
##############

# Filtro Inicio de tratamento - st_inicio_trat_hbv

Siclom_hepb <- filter(Siclom, st_inicio_trat_hbv == "Sim" )

## Salvando tabela intermediária

write.csv(Siclom_hepb, "C:/../Siclom_hepb.csv")

# Filtro Procedimento Principal - Filtro Opcional - "tp_alfapeginterferona_2a_180mcg"; "tp_entecavir_0_5mg" ;"tp_tenofovir_300mg"

Siclom_hepb_ini_trat <- filter(SIA_CID_hepb, tp_alfapeginterferona_2a_180mcg == 1 | tp_entecavir_0_5mg == 1 | tp_tenofovir_300mg == 1)

## Salvando tabela intermediária

write.csv(Siclom_hepb_ini_trat, "C:/../Siclom_hepb_ini_trat.csv")

# Filtro: Remover duplicidades

Siclom_hepb_ini_trat_un <- distinct(Siclom_hepb_ini_trat, cd_pac_eleito , .keep_all = TRUE)

## Salvando tabela intermediária

write.csv(Siclom_hepb_ini_trat_un, "C:/../Siclom_hepb_ini_trat_un.csv")

## Para cacatas >= 2021

n_ini_trat = n_SIA_CID_hepb_ini_trat_un + n_Siclom_hepb_ini_trat_un

####################
######## Continuam tratamento - 4a Barra
####################

##############
#### GAL 
##############

# Filtro opcional - Exame: HBV-DNA
GAL_HBV_DNA <- filter(GAL, Exame == "Hepatite B, HBV-DNA" )

# Filtro - Resultado: Detectavel

GAL_HBV_DNA_detectavel <- filter(GAL_HBV_DNA, Resultado == "Detectavel" | Resultado == "Acima do limite de quantificacao" )

# Filtro - Periodo: 1 ano

GAL_HBV_DNA_detectavel$ano <- dmy(SICLOM_med_hepb$'Data da Coleta')

GAL_HBV_DNA_detectavel$ano <- year(SICLOM_med_hepb$'Data da Coleta')

GAL_HBV_DNA_detectavel_ano <- filter(SICLOM_med_hepb, ano == 2021(exemplo)) 

## Salvando tabela intermediária

write.csv(GAL_HBV_DNA_detectavel_ano, "C:/../GAL_HBV_DNA_detectavel_ano.csv")

##############
#### Siclom-HV (Para cacatas >= 2021)
##############

# Filtro Inicio de tratamento - st_inicio_trat_hbv

Siclom_hepb <- filter(Siclom, st_inicio_trat_hbv == "Sim" )

# Filtro HBV-DNA - tp_situacao_paciente_com_HBV_DNA

Siclom_hepb_hbv_dna <- filter(Siclom_hepb, tp_situacao_paciente_com_HBV_DNA == 1 )

# Filtro - Periodo: 1 ano

Siclom_hepb_hbv_dna$ano <- ymd_hms(Siclom_hepb_hbv_dna$dt_dispensa_sol)

Siclom_hepb_hbv_dna$ano <- year(Siclom_hepb_hbv_dna$dt_dispensa_sol)

Siclom_hepb_hbv_dna_ano <- filter(Siclom_hepb_hbv_dna, ano == 2021 (exemplo)) 

# Filtro: Remover duplicidades

Siclom_hepb_hbv_dna_ano_un <- distinct(Siclom_hepb_hbv_dna_ano, cd_pac_eleito , .keep_all = TRUE)

## Salvando tabela intermediária

write.csv(Siclom_hepb_hbv_dna_ano_un, "C:/../Siclom_hepb_hbv_dna_ano_un.csv")

n_continuam_trat = n_GAL_HBV_DNA_detectavel_ano + n_Siclom_hepb_hbv_dna_ano_un

####################
######## Supressão Viral - 5a Barra
####################

# A seguir um exemplo com os dados de 2019

## Preparando dataframes para comparação de indivíduos dos grupos "tratados"  (SIA/APAC)  X "Diagnosticados"   = "Supressão Viral"    

## Tratados - Iniciaram tratamento (SIA/APAC)

tratados_2019 <- select(SIA_CID_hepb_ini_trat_un, AP_NUIDADE, AP_SEXO, AP_RACACOR, AP_MUNPCN)

tratados_2019$AP_RACACOR[tratados_2019$AP_RACACOR == 1] <- "BRANCA"
tratados_2019$AP_RACACOR[tratados_2019$AP_RACACOR == 2] <- "PRETA"
tratados_2019$AP_RACACOR[tratados_2019$AP_RACACOR == 3] <- "PARDA"
tratados_2019$AP_RACACOR[tratados_2019$AP_RACACOR == 4] <- "AMARELA"
tratados_2019$AP_RACACOR[tratados_2019$AP_RACACOR == 5] <- "INDIGENA"
tratados_2019$AP_RACACOR[tratados_2019$AP_RACACOR == 99] <- "IGNORADO"

tratados_2019 <- select(tratados_2019, "Idade" = "AP_NUIDADE","Sexo" = "AP_SEXO", "Raça.Cor" = "AP_RACACOR",  "IBGE.Município.de.Residência" = "AP_MUNPCN")

tratados_2019$data_source <- "tratados_2019"

## Diagnosticados (GAL)

GAL_2019_HBV_DNA_supressao <- filter(GAL_2019_HBV_DNA, Resultado=="Nao Detectavel"| Resultado=="Abaixo do limite de quantificacao")

GAL_2019_HBV_DNA_supressao <- filter(GAL_2019_HBV_DNA_supressao, Valor < 2000)

diagnosticados_2019 <- select(GAL_2019_HBV_DNA_supressao, Idade, Sexo, Raça.Cor, IBGE.Município.de.Residência)

diagnosticados_2019$Sexo[diagnosticados_2019$Sexo == "MASCULINO"] <- "M"
diagnosticados_2019$Sexo[diagnosticados_2019$Sexo == "FEMININO"] <- "F"

is.na(diagnosticados_2019) <- diagnosticados_2019=='' 
diagnosticados_2019$Raça.Cor[is.na(diagnosticados_2019$Raça.Cor)] <- "IGNORADO"

diagnosticados_2019$data_source <- "diagnosticados_2019"

## Unindo diagnosticados e tratados

diagnosticados_tratados_2019 <- do.call("rbind", list(diagnosticados_2019, tratados_2019))

write.csv(diagnosticados_tratados_2019, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto4/resultados_cascata/diagnosticados_tratados_2019.csv")
write.xlsx(diagnosticados_tratados_2019, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto4/resultados_cascata/diagnosticados_tratados_2019.xlsx")

## Linkage - probabilistico 

p2019 <- links_wf_probabilistic(attribute = list(diagnosticados_tratados_2019$Idade, 
                                                 diagnosticados_tratados_2019$Sexo,
                                                 diagnosticados_tratados_2019$Raça.Cor,
                                                 diagnosticados_tratados_2019$IBGE.Município.de.Residência),
                                data_source = diagnosticados_tratados_2019$data_source,
                                probabilistic = TRUE,
                                m_probability = 0.95,
                                u_probability = NULL,
                                score_threshold = 1,
                                recursive = TRUE)


### Salvando tabelas

p2019_tbl <- as.data.frame(p2019$pid_weights)
p2019_tbl_2 <- as.data.frame(p2019$pid)

write.xlsx(p2019_tbl, "C:/.../p2019_tbl.xlsx")
write.xlsx(p2019_tbl_2, "C:/.../p2019_tbl_2.xlsx")

supressao_2019_un <- distinct(supressao_2019, sn_y , .keep_all = TRUE)

table(supresao_2019$record.match)

n_supressao_2019_un

############################
########## Plots = cascatas
###########################

##### Cascata 

# Ordem das barras da cascata
posicao <- c("Infectados", "Diagnosticados", "Iniciaram Tratamento", "Continuam Tratamento", "Supressão Viral")

# Criar tabela com dados para a cascata de cuidado

cascata <- matrix(c("n_estimado_infectados_hbv_brasil","Infectados", "n_GAL_exames_reagente_detectavel", "Diagnosticados", "n_ini_trat", "Iniciaram Tratamento","n_continuam_trat", "Continuam Tratamento", "supressao_un", "Supressão Viral"),ncol=2,byrow=TRUE)
colnames(cascata) <- c("Freq","tipo")
cascata <- as.data.frame(cascata)

cascata$Freq <- as.integer(cascata$Freq)

ggplot(data=cascata, aes(x=posicao, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue",  width=0.5) +
  geom_text(aes(label=Freq), vjust=-0.3, size=6)+
  theme_minimal()  + labs(x="", y = "Frequência") + ggtitle("Cascata de cuidado de pessoas com hepatite B") +theme(axis.text=element_text(size=18),
                                                                                                                                  axis.title=element_text(size=18)) + theme(plot.title = element_text(size = 20, face = "bold")) + scale_y_continuous(labels = comma) + scale_x_discrete(limits = posicao)
