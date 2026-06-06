#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# INTRODUÇÃO
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Neste script, vamos aprender como plotar mapas no R focando em variáveis
# econômicas brasileiras. Para avançarmos em conteúdos mais avançados, usaremos
# dados do programa Farmácia Popular do Brasil para estudar diferenças entre 
# farmácias credenciadas/não credenciadas ao programa. 
# Ao final, teremos aprendido a
#   1) plotar a distribuição destas variáveis em mapas detalhados (ao nível do 
#      setor censitário)
#   2) plotar endereços/estabelecimentos em mapas como pontos
#   3) verificar se um ponto está/não está contido em alguma parte do mapa
#   4) calcular distâncias entre pontos

install.packages('readxl')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('geobr')
install.packages('sf')
install.packages('units')
install.packages('reshape2')
install.packages('stringr')
install.packages('tidygeocoder')
install.packages('microdatasus')
install.packages("remotes")
install.packages("read.dbc", repos = "https://packagemanager.posit.co/cran/2024-07-05")
remotes::install_github("rfsaldanha/microdatasus")

library(readxl)
library(dplyr)
library(ggplot2)
library(geobr)
library(sf)
library(units)
library(reshape2)
library(stringr)
library(tidygeocoder)
library(microdatasus)

setwd('C:/Users/heito/Dropbox/ufabc/minicurso/material_mapas/')

# Contorno do Brasil
br = geobr::read_country()
ggplot()+
  geom_sf(data=br)

ggsave("figuras/map_br_contorno.png")
dev.off()

# Estados do Brasil
estados = geobr::read_state()
ggplot()+
  geom_sf(data=estados)

ggplot()+
  geom_sf(data=estados)+
  theme_void()

ggsave("figuras/map_br_estados.png")
dev.off()

# Municípios do Brasil
municipios = geobr::read_municipality(year = 2022)
sp = municipios %>% filter(code_state==35)
ggplot()+
  geom_sf(data=sp)+
  theme_void()

ggsave("figuras/map_sp_municipios.png")
dev.off()

# Note nos comandos acima que precisamos do ggplot(), e depois do geom_sf(), 
# para plotar mapas. Significa que a sintaxe e opções da maior parte dos
# comandos referentes a plotagem de mapas é similar àquela do pacote ggplot

# Vamos filtrar a região do ABC paulista

# 3513801 = Diadema
# 3529401 = Mauá
# 3543303 = Ribeirão Pires
# 3544103 = Rio Grande da Serra
# 3547809 = Santo André
# 3548708 = São Bernardo do Campo
# 3548807 = São Caetano do Sul
# 3550308 = São Paulo
abc_list = c(3513801, 3529401, 3543303, 3544103, 
             3547809, 3548708, 3548807, 3550308)
abc = municipios %>% filter(code_muni %in% abc_list)
scs = municipios %>% filter(code_muni==3548807)
ggplot()+
  geom_sf(data=abc, fill="white", color="black")+
  geom_sf(data=scs, fill="red", color="black")+
  theme_bw()

ggsave("figuras/map_abc_municipios.png")
dev.off()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ANÁLISE DAS COVARIADAS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Vamos focar no município de São Caetano do Sul (SP), que foi o município com
# maior IDH do Brasil de acordo com os dados dos Censos de 2010 e 2022.

# Ler o shapefile com os setores censitários de SCS/2010
## O comando read_sf() permite ler shapefiles, que são arquivos com coordenadas
## geográficas de algum lugar
sp_setores = read_sf('sp_setores_censitarios/35SEE250GC_SIR.shp', 
                     options = "ENCODING=WINDOWS-1252")
sp_setores = sp_setores %>% rename(code_muni = CD_GEOCODM)
sp_setores$code_muni = as.numeric(sp_setores$code_muni)
scs_setores = sp_setores %>% filter(code_muni==3548807)

ggplot()+
  geom_sf(data=scs_setores, fill='white')+
  theme_bw()

ggsave("figuras/map_scs_setoresCensitarios.png")
dev.off()

#-------------------------------------------------------------------------------
# DENSIDADE POPULACIONAL POR SETOR CENSITÁRIO
#-------------------------------------------------------------------------------

# Importar dados brutos do Censo 2010 sobre número de pessoas em cada setor
# censitário. Link para download está nos slides

pop = read.csv('SP_Exceto_Capital_20231030/Base informaçoes setores2010 universo SP_Exceto_Capital/CSV/pessoa13_sp2.csv')
pop = pop %>% rename(CD_GEOCODI = Cod_setor) 
pop$CD_GEOCODI = pop$CD_GEOCODI %>% format(scientific=FALSE)

pop = pop %>% rename(totres = V002) # Pessoas residentes em domicílios particulares permanentes
pop = pop %>% rename(totage00 = V022) # Pessoas com menos de 1 ano de idade
pop = pop %>% rename(totage01 = V035) # Pessoas de 1 ano de idade
pop = pop %>% rename(totage02 = V036) # Pessoas de 2 ano de idade
pop = pop %>% rename(totage03 = V037) # Pessoas de 3 ano de idade
pop = pop %>% rename(totage04 = V038) # ...
pop = pop %>% rename(totage05 = V039, totage06 = V040, totage07 = V041, 
                     totage08 = V042, totage09 = V043, totage10 = V044, 
                     totage11 = V045, totage12 = V046, totage13 = V047, 
                     totage14 = V048, totage15 = V049, totage16 = V050, 
                     totage17 = V051, totage18 = V052, totage19 = V053, 
                     totage20 = V054, totage21 = V055, totage22 = V056, 
                     totage23 = V057, totage24 = V058, totage25 = V059, 
                     totage26 = V060, totage27 = V061, totage28 = V062, 
                     totage29 = V063, totage30 = V064, totage31 = V065, 
                     totage32 = V066, totage33 = V067, totage34 = V068, 
                     totage35 = V069, totage36 = V070, totage37 = V071, 
                     totage38 = V072, totage39 = V073, totage40 = V074, 
                     totage41 = V075, totage42 = V076, totage43 = V077, 
                     totage44 = V078, totage45 = V079, totage46 = V080, 
                     totage47 = V081, totage48 = V082, totage49 = V083, 
                     totage50 = V084, totage51 = V085, totage52 = V086, 
                     totage53 = V087, totage54 = V088, totage55 = V089, 
                     totage56 = V090, totage57 = V091, totage58 = V092, 
                     totage59 = V093, totage60 = V094, totage61 = V095, 
                     totage62 = V096, totage63 = V097, totage64 = V098, 
                     totage65 = V099, totage66 = V100, totage67 = V101, 
                     totage68 = V102, totage69 = V103, totage70 = V104, 
                     totage71 = V105, totage72 = V106, totage73 = V107, 
                     totage74 = V108, totage75 = V109, totage76 = V110, 
                     totage77 = V111, totage78 = V112, totage79 = V113, 
                     totage80 = V114, totage81 = V115, totage82 = V116, 
                     totage83 = V117, totage84 = V118, totage85 = V119, 
                     totage86 = V120, totage87 = V121, totage88 = V122, 
                     totage89 = V123, totage90 = V124, totage91 = V125, 
                     totage92 = V126, totage93 = V127, totage94 = V128, 
                     totage95 = V129, totage96 = V130, totage97 = V131, 
                     totage98 = V132, totage99 = V133, totage100 = V134)

pop = pop %>% select(CD_GEOCODI, totres, totage00, totage01, totage02, 
                     totage03, totage04, totage05, totage06, totage07, 
                     totage08, totage09, totage10, totage11, totage12, 
                     totage13, totage14, totage15, totage16, totage17, 
                     totage18, totage19, totage20, totage21, totage22, 
                     totage23, totage24, totage25, totage26, totage27, 
                     totage28, totage29, totage30, totage31, totage32, 
                     totage33, totage34, totage35, totage36, totage37,
                     totage38, totage39, totage40, totage41, totage42, 
                     totage43, totage44, totage45, totage46, totage47, 
                     totage48, totage49, totage50, totage51, totage52, 
                     totage53, totage54, totage55, totage56, totage57, 
                     totage58, totage59, totage60, totage61, totage62, 
                     totage63, totage64, totage65, totage66, totage67, 
                     totage68, totage69, totage70, totage71, totage72, 
                     totage73, totage74, totage75, totage76, totage77,
                     totage78, totage79, totage80, totage81, totage82, 
                     totage83, totage84, totage85, totage86, totage87, 
                     totage88, totage89, totage90, totage91, totage92, 
                     totage93, totage94, totage95, totage96, totage97, 
                     totage98, totage99, totage100)

pop = pop %>% filter(!totres=="X") # remover linhas em que total de residentes é dado como X

scs_setores = scs_setores %>% left_join(pop) # merge do dataframe de SCS com o dataframe de população por setor censitário
scs_setores = scs_setores %>% replace(is.na(.), "0")
scs_setores$totres = as.numeric(scs_setores$totres)
scs_setores$areakm2 = st_area(scs_setores)/(10**3)
scs_setores$popkm2 = scs_setores$totres / scs_setores$areakm2 %>% as.numeric

# plotar a densidade da variável de "area em km2"
ggplot(scs_setores, aes(x=areakm2)) + 
  geom_density() + 
  geom_vline(aes(xintercept=mean(areakm2)), 
             color="red") +
  theme_bw()

ggsave("figuras/kds_areakm2scs.png")
dev.off()

# plotar a densidade da variável de "populacao por setor censitario"
ggplot(scs_setores, aes(x=totres)) + 
  geom_density() + 
  geom_vline(aes(xintercept=mean(totres)), 
             color="red") +
  theme_bw()

ggsave("figuras/kds_popresscs.png")
dev.off()

# plotar a densidade da variável de "densidade populacional por km2"
ggplot(scs_setores, aes(x=popkm2)) + 
  geom_density() + 
  geom_vline(aes(xintercept=mean(popkm2)), 
             color="red") +
  theme_bw()

ggsave("figuras/kds_popkm2scs.png")
dev.off()

# Usaremos os quintis desta variável para interpretar sua distribuição
# no território ao nível do setor censitário
quantile(scs_setores$popkm2)
quantile(scs_setores$popkm2, probs = seq(0, 1, 0.20))

ggplot() +
  geom_sf(data = scs_setores, aes(fill = popkm2)) +
  scale_fill_binned(type = "viridis", 
                    breaks = c(0, 7.888803, 12.389143, 15.558349, 20.215056), 
                    name = "Pop. KM²") +
  theme_bw()

ggsave("figuras/map_popdens_scs2010.png")
dev.off()

# Podemos usar outro esquema de cores, apesar de que o R não permite muita
# flexibilidade para este tipo de configuração

ggplot() +
  geom_sf(data = scs_setores, aes(fill = popkm2)) +
  scale_fill_steps(breaks = c(0, 7.888803, 12.389143, 15.558349, 20.215056), 
                   low = "cornsilk", high = "brown4") +
  theme_bw()

ggsave("figuras/map_popdens_scs2010_color1.png")
dev.off()

ggplot() +
  geom_sf(data = scs_setores, aes(fill = popkm2)) +
  scale_fill_stepsn(breaks = c(0, 7.888803, 12.389143, 15.558349, 20.215056), 
                    values = c(0, .2, .4, .6, .8),
                    colours = c("cornsilk", "green", "red",
                                "blue", "deeppink1") )+
  theme_bw()

ggsave("figuras/map_popdens_scs2010_color2.png")
dev.off()

#-------------------------------------------------------------------------------
# MÉDIA DE IDADE POR SETOR CENSITÁRIO
#-------------------------------------------------------------------------------

# Na mesma base de dados brutos do Censo 2010, encontramos dados sobre a idade
# dos moradores de cada setor censitário. Usaremos esta informação para 
# determinar a média de idade de cada setor censitário de SCS

# Remover colunas não utilizadas + geometry => passa a ser um dataframe comum
df1 <- scs_setores[, -c(1, 3:16, 118:119), drop = TRUE]
df1 <- melt(df1, id.vars = "CD_GEOCODI", 
            variable.name = "agegroup", value.name = "qty")

censustracts <- unique(df1$CD_GEOCODI)
agegroups <- unique(df1$agegroup)
ageavglist <- list()

# Para cada setor censitário, varremos a qtd de pessoas em cada grupo etário e,
# a partir disso, determinamos a média de idade naquele setor
for (ct in censustracts) {
  agesum <- 0
  for (agegroup in agegroups) {
    qty <- df1[df1$CD_GEOCODI==ct & df1$agegroup==agegroup, ]$qty
    
    if (grepl("100", agegroup, fixed = TRUE)) {
      age = as.numeric(str_sub(agegroup, -3, -1))
    } else {
      age = as.numeric(str_sub(agegroup, -2, -1))
    }
    
    if (qty>0) {
      for (x in 1:qty) {
        agesum = agesum + age
      }
    }
  }
  totres <- as.numeric(scs_setores[scs_setores$CD_GEOCODI==ct, ]$totres)
  if (totres==0) {
    ageavg = NA
  } else {
    ageavg = agesum / totres
  }
  len <- length(ageavglist)
  ageavglist[len+1] <- ageavg
}

# Adicionando a média de idade ao dataframe de SCS
scs_setores$avgage = unlist(ageavglist)

# plotar a densidade da variável de "idade por setor censitario"
agemean = mean(na.omit(scs_setores$avgage))
ggplot(scs_setores, aes(x=avgage)) + 
  geom_density() + 
  geom_vline(aes(xintercept=agemean), 
             color="red") +
  theme_bw()

ggsave("figuras/kds_avgagescs.png")
dev.off()

# Quintis de idade para plotagem do mapa
quantile(na.omit(scs_setores$avgage), probs = seq(0, 1, 0.20))

ggplot() +
  geom_sf(data = scs_setores, aes(fill = avgage)) +
  scale_fill_binned(type = "viridis", 
                    breaks = c(28.17391, 36.53398, 38.26969, 
                               40.12075, 41.74173), 
                    name = "Média de idade") +
  theme_bw()

ggsave("figuras/map_avgage_scs2010.png")
dev.off()

#-------------------------------------------------------------------------------
# MÉDIA DE RENDA PER CAPITA POR SETOR CENSITÁRIO
#-------------------------------------------------------------------------------

# Importar dados brutos do Censo 2010 sobre a renda de cada morador de cada
# setor censitário. Com essa informação, podemos calcular a média de renda per 
# capita em cada setor censitário. Link para download está nos slides
## Renda = qtd de salários mínimos

# Ler o shapefile com renda por setor censitário em 2010
renda_sp = read.csv('SP_Exceto_Capital_20231030/Base informaçoes setores2010 universo SP_Exceto_Capital/CSV/PessoaRenda_SP2.csv', 
                    sep = ';')
# Ajustar nome de variáveis
renda_sp = renda_sp %>% 
  rename(qtppinc_0.5 = V001, # qt de pessoas que ganham até 1/2 SM
         qtppinc_0.5_1 = V002, # qt de pessoas que ganham entre 1/2 e 1 SM
         qtppinc_1_2 = V003, # qt de pessoas que ganham entre 1 e 2 SM
         qtppinc_2_3 = V004, # qt de pessoas que ganham entre 2 e 3 SM
         qtppinc_3_5 = V005, # qt de pessoas que ganham entre 3 e 5 SM
         qtppinc_5_10 = V006, # qt de pessoas que ganham entre 5 e 10 SM
         qtppinc_10_15 = V007, # qt de pessoas que ganham entre 10 e 15 SM
         qtppinc_15_20 = V008, # qt de pessoas que ganham entre 15 e 20 SM
         qtppinc_21 = V009) # qt de pessoas que ganham 21 ou mais SM
renda_sp$cod_muni = substr(renda_sp$Cod_setor, 1, 6)
renda_scs <- renda_sp %>% 
  filter(cod_muni == 354880) %>%  # apenas SCS
  select(Cod_setor, qtppinc_0.5, qtppinc_0.5_1, qtppinc_1_2, qtppinc_2_3, 
         qtppinc_3_5, qtppinc_5_10, qtppinc_10_15, qtppinc_15_20, 
         qtppinc_21)

renda_scs <- melt(renda_scs, id.vars = "Cod_setor", 
                  variable.name = "incomegroup", value.name = "qty")

censustracts <- unique(renda_scs$Cod_setor)
incomegroups <- unique(renda_scs$incomegroup)
incavglist <- list()

# Para cada setor censitário, varremos a qtd de pessoas em cada faixa de renda
# e, a partir disso, determinamos a média de renda per capita naquele setor
for (ct in censustracts) {
  incsum = 0
  qtpp = 0
  
  for (incgroup in incomegroups) {
    qty <- as.numeric(renda_scs[renda_scs$Cod_setor==ct 
                                & renda_scs$incomegroup==incgroup, 
                                ]$qty)
    inc <- as.numeric(sapply(str_split(incgroup, "_"), tail, 1))
    
    if (qty>0) {
      incsum = incsum + (inc*qty)
      qtpp = qtpp + qty
    }
  }
  if (qtpp>0) {
    avginc = incsum / qtpp
  } else {
    avginc = NA
  }
  len <- length(incavglist)
  incavglist[len+1] <- avginc
}

# Criar dataframe apenas com código do setor e a renda média
df2 <- tibble(CD_GEOCODI = censustracts, avgincome = incavglist)
df2$CD_GEOCODI = df2$CD_GEOCODI %>% format(scientific=FALSE)
df2$avgincome = as.numeric(df2$avgincome)

# Merge do dataframe de SCS e o dataframe com a renda média
scs_setores = scs_setores %>% left_join(df2, by = "CD_GEOCODI")

# Plotar a densidade da média de renda
incomemean = mean(na.omit(scs_setores$avgincome))
ggplot(scs_setores, aes(x=avgincome)) + 
  geom_density() + 
  geom_vline(aes(xintercept=incomemean), 
             color="red") +
  theme_bw()

ggsave("figuras/kds_avgincomescs.png")
dev.off()

# Quintis de renda para plotagem do mapa
quantile(na.omit(scs_setores$avgincome), probs = seq(0, 1, 0.20))

ggplot() +
  geom_sf(data = scs_setores, aes(fill = avgincome)) +
  scale_fill_binned(type = "viridis", 
                    breaks = c(1.160920, 3.807278, 4.489761, 
                               5.323009, 6.940801), 
                    name="rpc média") +
  theme_bw()

ggsave("figuras/map_avgincome_scs2010.png")
dev.off()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# GEORREFERENCIAMENTO DE FARMÁCIAS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Georreferenciamento = geocoding = determinar (lat, lon) a partir de endereços.
# É possível usar o R para esta tarefa. Veja detalhes nos slides. Aqui, vamos
# ver como georreferenciar endereços de UBSs e farmácias em SCS. Detalhes sobre
# a origem destes dados estão nos slides

# Importar dados de endereços de UBSs em SCS
## Usando pacote microdatasus
df <- fetch_datasus(year_start = 2017, year_end = 2017, 
                    uf = "SP", month_start = 12, month_end = 12,
                    information_system = "CNES-ST")
df <- df[df$TP_UNID %in% c("02"), ] # apenas UBSs
df <- df[df$CODUFMUN %in% c("354880"), ] # apenas em SCS

# Abra o objeto "df"



# Importar dados de endereços de UBSs em SCS
## Usando dados prontos
ubs = read.csv('Enderecos/cnes_ubs_scs.csv', sep = ';')
ubs$munname = "SAO CAETANO DO SUL"
ubs$statename = "SP"

# Criar variável com endereço completo, para informar ao geocoder
## Note que os endereços de UBSs que temos acesso não são completos, pois não
## possuem o número do estabelecimento no logradouro. Aqui, poderíamos buscar
## essa informação na internet, pois são só 13 UBSs listadas. Mas tal tarefa
## se torna inviável em bases maiores. Mesmo assim, o geocoder busca as coords
## de endereços incompletos
ubs$addr = paste(paste(ubs$streetname, ",", sep=""), 
                 paste(ubs$munname, ",", sep=""), 
                 paste(ubs$district, ",", sep=""),
                 ubs$statename, str_pad(ubs$zipcode, 8, pad = "0"))

# Abra o objeto "ubs"

# Rodar o geocoder e jogar o output para o objeto "lat_lon"
lat_lon <- ubs %>% geocode(streetname, method = 'osm', 
                           lat = lat, long = lon, 
                           full_results = TRUE)

# Abra o objeto "lat_lon"

# Veja que alguns endereços não estão corretos (Santo André, Rio de Janeiro, 
# Manaus). Seria necessário ajustar manualmente a acentuação de alguns 
# endereços, incluir o número de cada UBS e talvez outros detalhes para que as 
# coords de (lat, lon) fossem precisas.

# Para ganharmos tempo, vamos usar os arquivos abaixo com as coords corretas.

# Importar o arquivo com as coordenadas corretas para UBSs
ubs = read.csv('Enderecos/cnes_ubs_scs_latlon.csv')

# Importar o arquivo com as coordenadas corretas para farmácias
pharms = read.csv('Enderecos/rais_pharmacies_scs_latlon_edit.csv', 
                  sep = ";")
pharms$store_id = pharms$store_id %>% format(scientific=FALSE)

# Abra o objeto "pharms"



# Para plotar no mapa as farmácias dentro e fora do Farmácia Popular, vamos
# criar dois dataframes distintos, um para cada tipo de farmácia
pharm_fp_in = pharms[pharms$atfpactive==1, ] %>% 
  select(store_id, latitude, longitude)
pharm_fp_out = pharms[pharms$atfpactive==0, ] %>% 
  select(store_id, latitude, longitude)

# Note que o nome de cada elemento está no atributo "colour" de "aes". Isso é 
# necessário para permitir que nossas configurações de forma, tamanho e cor dos
# pontos no mapa sejam efetivas. Note ainda que "Non-ATFP pharms" é o segundo
# item na legenda, mesmo tendo sido informado primeiro no comando. Isso porque
# o ggplot() ordena os elementos nomeados de forma alfabética, e por isso também
# informamos a cor de "Non-ATFP pharms" em segundo lugar ao final do comando
ggplot()+
  geom_sf(data=scs_setores, fill='white')+
  geom_point(data=pharm_fp_in, aes(x=longitude, y=latitude, 
                                   colour="Farmácias ATFP"), 
             shape = 16, size = 2) +
  geom_point(data=pharm_fp_out, aes(x=longitude, y=latitude, 
                                    colour="Farmácias não-ATFP"), 
             shape = 17, size = 2) +
  geom_point(data=ubs, aes(x=longitude, y=latitude, 
                           colour="UBSs"), 
             shape = 18, size = 2) +
  scale_colour_manual(values = c("palegreen3", "dodgerblue", 
                                 "orange"), name = "") +
  theme_void()

ggsave("figuras/map_ubspharms_scs2017.png")
dev.off()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# TESTE DE MÉDIA
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Queremos um teste de média que mostre se há diferenças sistemáticas entre
# farmácias credenciadas ao Farmácia Popular e farmácias não credenciadas

# Para começar, precisamos saber a qual setor censitário pertence cada farmácia,
# além de também precisarmos saber a qual distância cada farmácia está da UBS
# mais próxima

# Vamos recuperar o setor censitário de cada farmácia

# Converter o dataframe de farmácias em um dataframe geográfico, ou seja, com uma
# geometria (neste caso, um ponto no mapa) para analisarmos
pharms = st_as_sf(pharms, coords = c("longitude","latitude"), 
                  remove = FALSE, crs = 4326)

# Exemplo: verificando se um setor censitário "contém" uma farmácia
## Estamos convertendo tanto o polígono dos setores censitários quanto os pontos
## referentes às farmácias para o CRS 9311 porque este é um CRS "plano", que é o
## tipo de elemento geográfico que a função st_contains() espera receber

st_contains(st_transform(scs_setores[scs_setores$CD_GEOCODI=="354880705000001", ], 9311), 
            st_transform(pharms[pharms$store_id=="49", ], 9311))[[1]]

# Verificar qual setor censitário contém quais farmácias
censustracts <- unique(scs_setores$CD_GEOCODI)
farmacias <- unique(pharms$store_id)
location_list <- list()

# Para cada farmácia (lat, lon), verificamos se está contida na área (geometry) 
# de cada um dos setores censitários
## Dura ~2min 15s
for (farma in farmacias) {
  for (ct in censustracts) {
    x <- st_contains(st_transform(scs_setores[scs_setores$CD_GEOCODI==ct, 
                                              ], 9311), 
                     st_transform(pharms[pharms$store_id==farma, 
                                         ], 9311))[[1]]
    len <- length(x)
    
    if (len > 0) {
      if (x == 1) {
        len <- length(location_list)
        location_list[len+1] <- ct
      }
    }
  }
}
# Adicionando resultados ao dataframe das farmácias
pharms$CD_GEOCODI = location_list
pharms$CD_GEOCODI = as.numeric(pharms$CD_GEOCODI)
pharms$CD_GEOCODI = pharms$CD_GEOCODI %>% format(scientific=FALSE)

# Abra o objeto "pharms"


# Vamos determinar a distância de cada farmácia da UBS mais próxima

# Converter o dataframe de UBSs em um dataframe geográfico, ou seja, com uma
# geometria (neste caso, um ponto no mapa) para analisarmos
ubs = st_as_sf(ubs, coords = c("longitude","latitude"), 
               remove = FALSE, crs = 4326)

# Exemplo: como determinar a distância entre 2 pontos
## Estamos convertendo cada ponto para o CRS 9311 pois é um CRS "plano", que é
## o que esta função st_distance() espera receber. Resultado em metros

st_distance(st_transform(pharms[pharms$store_id==49, ], 9311),
            st_transform(ubs[ubs$cnes==2039389, ], 9311))

# Verificar a distância entre cada farmácia e a UBS mais próxima
ubslist <- unique(ubs$cnes)
farmacias <- unique(pharms$store_id)
distance_list <- list()

# Para cada farmácia, verificamos a distância dela para cada UBS e mantemos a 
# menor distância (em metros)
## Dura ~20s
for (farma in farmacias) {
  cur_x <- 9999999
  for (cnes in ubslist) {
    x <- st_distance(st_transform(ubs[ubs$cnes==cnes, 
                                      ], 9311), 
                     st_transform(pharms[pharms$store_id==farma, 
                                         ], 9311))
    if (as.numeric(x[[1, 1]]) < cur_x) {
      cur_x <- as.numeric(x[[1, 1]])
    }
  }
  len <- length(distance_list)
  distance_list[len+1] <- cur_x
}
# Adicionando resultados ao dataframe das farmácias
pharms$dist_ubs_m = distance_list
pharms$dist_ubs_m = as.numeric(pharms$dist_ubs_m)



# Criar dataframe apenas com código do setor censitário + covariadas do censo
df3 <- tibble(CD_GEOCODI = scs_setores$CD_GEOCODI, 
              avgincome = scs_setores$avgincome, 
              avgage = scs_setores$avgage, 
              popkm2 = scs_setores$popkm2)
df3$CD_GEOCODI = df3$CD_GEOCODI %>% format(scientific=FALSE) %>% as.numeric()
df3$avgincome = as.numeric(df3$avgincome)
df3$avgage = as.numeric(df3$avgage)
df3$popkm2 = as.numeric(df3$popkm2)

# Merge do dataframe das farmácias e do dataframe com as covariadas do censo
pharms$CD_GEOCODI = pharms$CD_GEOCODI %>% format(scientific=FALSE) %>% as.numeric()
pharms = pharms %>% left_join(df3, by = "CD_GEOCODI")

# Testando a diferença entre as médias de variáveis relevantes de farmácias
# credenciadas e não credenciadas ao ATFP

# RENDA PER CAPITA MÉDIA (AO NÍVEL DO SETOR CENSITÁRIO)
fp_in <- pharms[pharms$atfpactive==1, ]$avgincome
fp_out <- pharms[pharms$atfpactive==0, ]$avgincome
t.test(fp_in, fp_out)

mean(fp_in)
sd(fp_in)

mean(fp_out)
sd(fp_out)

# DISTÂNCIA MÉDIA PARA A UBS MAIS PRÓXIMA
fp_in <- pharms[pharms$atfpactive==1, ]$dist_ubs_m
fp_out <- pharms[pharms$atfpactive==0, ]$dist_ubs_m
t.test(fp_in, fp_out)

mean(fp_in)
sd(fp_in)

mean(fp_out)
sd(fp_out)

# MÉDIA DA DENSIDADE POPULACIONAL POR KM2 (AO NÍVEL DO SETOR CENSITÁRIO)
fp_in <- pharms[pharms$atfpactive==1, ]$popkm2
fp_out <- pharms[pharms$atfpactive==0, ]$popkm2
t.test(fp_in, fp_out)

mean(fp_in)
sd(fp_in)

mean(fp_out)
sd(fp_out)

# MÉDIA DA IDADE DOS RESIDENTES (AO NÍVEL DO SETOR CENSITÁRIO)
fp_in <- pharms[pharms$atfpactive==1, ]$avgage
fp_out <- pharms[pharms$atfpactive==0, ]$avgage
t.test(fp_in, fp_out)

mean(fp_in)
sd(fp_in)

mean(fp_out)
sd(fp_out)

# MÉDIA DE TRABALHADORES DE CADA TIPO DE FARMÁCIA
fp_in <- pharms[pharms$atfpactive==1, ]$cltwkrnum
fp_out <- pharms[pharms$atfpactive==0, ]$cltwkrnum
t.test(fp_in, fp_out)

mean(fp_in)
sd(fp_in)

mean(fp_out)
sd(fp_out)

# Testando média de trabalhadores sem duas empresas enormes
fp_in <- pharms[pharms$atfpactive==1 & !(pharms$store_id %in% c(3, 36)), ]$cltwkrnum
fp_out <- pharms[pharms$atfpactive==0, ]$cltwkrnum
t.test(fp_in, fp_out)

mean(fp_in)
sd(fp_in)

mean(fp_out)
sd(fp_out)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# APÊNDICE
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Detalhes do pacote geobr
br$geom

