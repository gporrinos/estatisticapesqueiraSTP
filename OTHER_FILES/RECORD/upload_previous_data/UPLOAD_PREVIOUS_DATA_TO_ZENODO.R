
library(estatisticapesqueiraSTP)



### OBTAIN REFERENCE DATA

rm(list = ls())
conf <- get_config_from_local()
token = "zNSDin2zDRF3Rop7Yg0Qp5KzZ4VwCD2r8zfHbqNsRHsf6u66Kfkx1yAbCTl9"
repository_name     = "test"
repository_location = conf$repository_location
forms               = conf$forms
databases           = conf$databases
variables           = conf$variables
config              = list(local_data_directory= "DADOS",
                           method = "GROUPED",
                           grouping_variable = "ano",
                           fileformat = "csv",
                           csv_separator = ",")
cleaning_function   = conf$cleaning_function
zenodo              = conf$zenodo
zenodo[["instance"]] = "sandbox"
cache               = conf$cache


language = "en"
update_server_data = TRUE
last_session = conf$last_session

if(!file.exists("inst/cache")) dir.create("inst/cache")
alldat <- append_and_update(
  local_data_directory = tempdir(),
  variables            = variables,
  databases            = databases,
  config               = config,
  last_session         = NULL,
  zenodo               = zenodo,
  token                = token,
  repository_name      = repository_name,
  cache                = cache,
  update_server_data   = FALSE,
  language             = language)
estatisticapesqueiraSTP::remove_files("inst/cache")


alldat$esforco$valid <- as.integer(alldat$esforco$valid)
alldat$embarcacoesativas$valid <- as.integer(alldat$embarcacoesativas$valid)
alldat$capturas$valid_peso <- as.integer(alldat$capturas$valid_peso)
alldat$capturas$valid_preco <- as.integer(alldat$capturas$valid_preco)
alldat$embarcacoes$valid_duracao_viagem <- as.integer(alldat$embarcacoes$valid_duracao_viagem)
alldat$embarcacoes$valid_combustivel <- as.integer(alldat$embarcacoes$valid_combustivel)












### OBTAIN REFERENCE DATA

conf <- get_config_from_local()
token = "tZvahbErRsKVzOg6cwqigmQ9Rclh2fj4dhoz8FxOYXngP0KxJPAoXG3v32YP"
repository_name     = conf$repository_name
repository_location = conf$repository_location
forms               = conf$forms
databases           = conf$databases
variables           = conf$variables
config              = conf$config
cleaning_function   = conf$cleaning_function
zenodo              = conf$zenodo
zenodo[["instance"]] = zenodo[["instance"]]
cache               = conf$cache


language = "en"
update_server_data = TRUE
last_session = conf$last_session







# version 5.0; 30 Mar 2026


################################################################################
####               PREPARE GLOBAL ENVIRONMENT AND DIRECTORIES               ####
################################################################################






### ------------------------ STEP 1: LOAD PACKAGES ------------------------- ###



suppressWarnings({
  suppressMessages({
    if(!require("jsonlite"))   install.packages("jsonlite", repos = "https://cloud.r-project.org")
    library(jsonlite)
  })})


suppressWarnings({
  suppressMessages({
    if(!require("httr")) install.packages("httr", repos='http://cran.us.r-project.org')
    library(httr)
  })})



suppressWarnings({
  suppressMessages({
    if(!require("dplyr")) install.packages("dplyr", repos='http://cran.us.r-project.org')
    library(dplyr)
  })})


suppressWarnings({
  suppressMessages({
    if(!require("readxl")) install.packages("readxl", repos='http://cran.us.r-project.org')
    library(readxl)
  })})






### ------------------ STEP 2: REMOVE PREVIOUS TEMPFILES ------------------- ###



tempfiles <- tempdir()









################################################################################
#                                 RECENSEAMENTO                                #
################################################################################



### READ "RECENSEAMENTO" FROM LOCAL FILE
dat <- read.csv(file.path("upload_previous_data", "number_of_active_vessels.csv"))
for(gear in colnames(dat)[-c(1,2)]){
  temp = data.frame(
    uuid = "recenseamento_2023",
    id = as.character(NA),
    ilha = as.character(NA),
    distrito   = gsub(".", " ", dat[,1], fixed = TRUE),
    comunidade = gsub(".", " ", dat[,2], fixed = TRUE),
    inquiridor = as.character(NA),
    data       = "2023-07-01",
    ano        = as.integer(2023),
    mes        = as.integer(7),
    unidade_pesca = gear,
    embarcacoes_ativas_mensal = dat[,which(colnames(dat) == gear)],
    deviceid  = as.character(NA),
    data_descarga = "230701_000000"
  )
  if(gear == colnames(dat)[3])
    embarcacoesativas = temp else
      embarcacoesativas = rbind(embarcacoesativas, temp)
}


### ADD MISSING VARIABLES
embarcacoesativas$unidade_pesca <- gsub(".", " ", embarcacoesativas$unidade_pesca, fixed = TRUE)
embarcacoesativas$ilha[which(embarcacoesativas$distrito == "pague")] <- "principe"
embarcacoesativas$ilha[which(!embarcacoesativas$distrito == "pague")] <- "saotome"
embarcacoesativas_RAW <- embarcacoesativas


embarcacoesativas$valid <-  1
embarcacoesativas$observacoes = ""



### TEST THAT COLNAMES AND CLASS IS THE SAME OF NEW DATA

  colnames(embarcacoesativas) == colnames(alldat$embarcacoesativas)
  unlist(lapply(c(1:ncol(embarcacoesativas)), function(i) class(embarcacoesativas[,i]))) ==
    unlist(lapply(c(1:ncol(alldat$embarcacoesativas)), function(i) class(alldat$embarcacoesativas[,i])))





################################################################################
#                            DOWNLOAD PREVIOUS DATA                            #
################################################################################




# Load credentials
email = "direcaodaspescas.stp@gmail.com"


# Load "googledrive" package
suppressWarnings({
  suppressMessages({
    if(!require("googledrive"))   install.packages("googledrive", repos = "https://cloud.r-project.org")
    library(googledrive)
  })})



### STEP 1: DOWNLOAD PREVIOUS DATA
suppressMessages({
  drive_auth(email = email)
  filesindrive <- as.data.frame(drive_ls(path = "ESTATISTICA_PESQUEIRA/dados"))
})

if(nrow(filesindrive) > 0 & "estatistica_pesqueira.rds" %in% filesindrive$name){
  suppressMessages({
    drive_download(file = filesindrive$id[filesindrive$name == "estatistica_pesqueira.rds"],
                   path = file.path(tempfiles,"estatistica_pesqueira.rds"),
                   overwrite = TRUE)
  })
  alldata = readRDS(file.path(tempfiles,"estatistica_pesqueira.rds"))
} else {
  alldata = list(RAW = list(embarcacoes = NULL, capturas          = NULL,
                            esforco     = NULL, embarcacoesativas = NULL),
                 DAT = list(embarcacoes = NULL, capturas          = NULL,
                            esforco     = NULL, embarcacoesativas = NULL))
}




### CREATE OBJECTS

for(dat in c("embarcacoes", "capturas","esforco", "embarcacoesativas")){
  for(type in c("RAW", "DAT")){
    temp <- alldata[names(alldata) == type][[1]]
    temp <- temp[names(temp) == dat]
    if(length(temp) > 0) {
      temp = temp[[1]]
      if(type == "RAW") {
        filepath = paste0(dat,"_RAW.rds")
        assign(paste0(dat,"_RAW"), temp)
      } else {
        filepath = paste0(dat,".rds")
        assign(paste0(dat), temp)
      }
      saveRDS(temp, file.path(tempfiles,filepath))
    }
  }
}








################################################################################
#                                 EMBARCACOES                                #
################################################################################



#============================ embarcacoes_RAW_RAW =============================#

### Rename "distrito" and "comunidade"
embarcacoes_RAW$hora_inicio_questionario <- as.character(NA)
embarcacoes_RAW$unidade_pesca <- embarcacoes_RAW$arte_de_pesca
embarcacoes_RAW$unidade_pesca[embarcacoes_RAW$arte_de_pesca == "FIO_E_ANZOL" & embarcacoes_RAW$propulsao == "motor"] <- "FIO E ANZOL MOTORIZADA"
embarcacoes_RAW$unidade_pesca[embarcacoes_RAW$arte_de_pesca == "FIO E ANZOL" & embarcacoes_RAW$propulsao == "motor"] <- "FIO E ANZOL MOTORIZADA"
embarcacoes_RAW$unidade_pesca[embarcacoes_RAW$arte_de_pesca == "FIO_E_ANZOL" & !embarcacoes_RAW$propulsao == "motor"] <- "FIO E ANZOL NAO MOTORIZADA"
embarcacoes_RAW$unidade_pesca[embarcacoes_RAW$arte_de_pesca == "FIO E ANZOL" & !embarcacoes_RAW$propulsao == "motor"] <- "FIO E ANZOL NAO MOTORIZADA"
embarcacoes_RAW$unidade_pesca[embarcacoes_RAW$embarcacao == "CARIOCO"] <- "FIO E ANZOL CARIOCO"
embarcacoes_RAW$unidade_pesca[embarcacoes_RAW$embarcacao == "TRAINERA"] <- "FIO E ANZOL TRAINERA"
embarcacoes_RAW$unidade_pesca[embarcacoes_RAW$embarcacao == "carioco"] <- "FIO E ANZOL CARIOCO"
embarcacoes_RAW$unidade_pesca[embarcacoes_RAW$embarcacao == "trainera"] <- "FIO E ANZOL TRAINERA"



embarcacoes_RAW$ano <- as.integer(substr(embarcacoes_RAW$data, 1,4))
embarcacoes_RAW$mes <- as.integer(substr(embarcacoes_RAW$data, 6,7))
embarcacoes_RAW$dia <- as.integer(substr(embarcacoes_RAW$data, 9,10))

embarcacoes_RAW$ano_saida <- as.integer(substr(embarcacoes_RAW$data_saida, 1,4))
embarcacoes_RAW$mes_saida <- as.integer(substr(embarcacoes_RAW$data_saida, 6,7))
embarcacoes_RAW$dia_saida <- as.integer(substr(embarcacoes_RAW$data_saida, 9,10))

embarcacoes_RAW$duracao_viagem <-
  as.numeric(
    as.difftime(
      as.POSIXct(paste(paste(add0(embarcacoes_RAW$ano),
                             add0(embarcacoes_RAW$mes),
                             add0(embarcacoes_RAW$dia),
                             sep = "-"),
                       embarcacoes_RAW$hora)) -
        as.POSIXct(paste(paste(add0(embarcacoes_RAW$ano_saida),
                               add0(embarcacoes_RAW$mes_saida),
                               add0(embarcacoes_RAW$dia_saida),
                               sep = "-"),
                         embarcacoes_RAW$hora_saida)),
      units = "hours"))/(60*60)


embarcacoes_RAW$hora       <- substr(embarcacoes_RAW$hora,1,5)
embarcacoes_RAW$hora_saida <- substr(embarcacoes_RAW$hora_saida,1,5)


embarcacoes_RAW$duracao_viagem_unidade = "hour"


### 2. CALCULATE TOTAL FUEL COST
for(x in c("gasolina", "petroleo", "gasoleo", "oleo_motor",
           "preco_gasolina", "preco_petroleo", "preco_gasoleo", "preco_oleo_motor"))
  embarcacoes_RAW[,which(colnames(embarcacoes_RAW) == x)] <-
  as.numeric(embarcacoes_RAW[,which(colnames(embarcacoes_RAW) == x)])

fuelcost    = function(fueltype) {
  if(nrow(embarcacoes_RAW) > 0){
    return(
      unlist(lapply(1:nrow(embarcacoes_RAW),function(i){
        price = embarcacoes_RAW[i,paste0("preco_",fueltype)]
        fuel  = embarcacoes_RAW[i,fueltype]
        if(fuel == 0) 0 else fuel*price
      }))
    ) } else {
      return(as.numeric(c()))
    }
}

embarcacoes_RAW$custo_combustivel = fuelcost("gasolina") + fuelcost("gasoleo") +
  fuelcost("petroleo") + fuelcost("oleo_motor")



embarcacoes_RAW$numero_pescadores <- as.integer(embarcacoes_RAW$numero_pescadores)


### Select only relevant variables
emb_cnames <- c('uuid',
                'id',
                'hora_inicio_questionario',
                'inquiridor',
                'ilha',
                'distrito',
                'comunidade',
                'distrito_desembarque',
                'comunidade_desembarque',
                'tipo_embarcacao',
                'embarcacao',
                'propulsao',
                'numero_pescadores',
                'arte_de_pesca',
                'unidade_pesca',
                'numero_registo',
                'data',
                'ano',
                'mes',
                'dia',
                'hora',
                'data_saida',
                'ano_saida',
                'mes_saida',
                'dia_saida',
                'hora_saida',
                'duracao_viagem',
                'duracao_viagem_unidade',
                'apanhou_peixe',
                'tipo_combustivel',
                'gasolina',
                'petroleo',
                'gasoleo',
                'oleo_motor',
                'preco_gasolina',
                'preco_petroleo',
                'preco_gasoleo',
                'preco_oleo_motor',
                'custo_combustivel',
                'problemas',
                'deviceid',
                'data_descarga')
embarcacoes_RAW = embarcacoes_RAW[emb_cnames]



### Change symbols
for(var in c("inquiridor","distrito", "comunidade",
             "distrito_desembarque", "comunidade_desembarque", "arte_de_pesca")){
  embarcacoes_RAW[,var] = gsub(".", " ", embarcacoes_RAW[,var], fixed = TRUE)
  embarcacoes_RAW[,var] = gsub("_", " ", embarcacoes_RAW[,var], fixed = TRUE)
}


### Test that number of variables and class is correct

rbind(alldat$embarcacoes_RAW, embarcacoes_RAW)
bind_rows(alldat$embarcacoes_RAW, embarcacoes_RAW)







#================================ embarcacoes =================================#




### STANDARDISE CLASS OF VARIABLES
questionnaire_information_path <- file.path("inst",
                                            "config",
                                            "forms.xlsx")
variables       <- read_excel(questionnaire_information_path, sheet = "vars")
positions = which(variables$dat == "embarcacoes")
varnames   = gsub("-", "_", variables$variablename[positions])
classnames = variables$class[positions]
for(i in 1:ncol(embarcacoes)){
  embarcacoes[,i] = as.character(embarcacoes[,i])
  cname = colnames(embarcacoes)[i]
  if(cname %in% varnames){
    as.class = get(paste0("as.", classnames[which(varnames == cname)]))
    embarcacoes[,i] = as.class(embarcacoes[,i])
  }
}




### SEPARATE YEAR, MONTH AND DAY
embarcacoes$ano  <- as.integer(substr(embarcacoes$data, 1,4))
embarcacoes$mes  <- as.integer(substr(embarcacoes$data, 6,7))
embarcacoes$dia  <- as.integer(substr(embarcacoes$data, 9,10))
embarcacoes$hora <- substr(embarcacoes$hora, 1,5)

embarcacoes$ano_saida  <- as.integer(substr(embarcacoes$data_saida, 1,4))
embarcacoes$mes_saida  <- as.integer(substr(embarcacoes$data_saida, 6,7))
embarcacoes$dia_saida  <- as.integer(substr(embarcacoes$data_saida, 9,10))
embarcacoes$hora_saida <- substr(embarcacoes$hora_saida, 1,5)




### ADD VARIABLES THAT DO NO EXIST IN PREVIOUS VERSION
embarcacoes$duracao_viagem_unidade = "hour"
embarcacoes$hora_inicio_questionario = NA

embarcacoes$valid[which(embarcacoes$valid == "VERIFICAR")] <-  TRUE
embarcacoes$valid <- as.integer(as.logical(embarcacoes$valid))
embarcacoes$valid_duracao_viagem <- embarcacoes$valid
embarcacoes$valid_combustivel    <-  1


### SELECT AND REORDER VARIABLES
emb_cnames <- c('uuid',
                'id',
                'hora_inicio_questionario',
                'inquiridor',
                'ilha',
                'distrito',
                'comunidade',
                'distrito_desembarque',
                'comunidade_desembarque',
                'tipo_embarcacao',
                'embarcacao',
                'propulsao',
                'numero_pescadores',
                'arte_de_pesca',
                "unidade_pesca",
                'numero_registo',
                'data',
                'ano',
                'mes',
                'dia',
                'hora',
                'data_saida',
                'ano_saida',
                'mes_saida',
                'dia_saida',
                'hora_saida',
                'duracao_viagem',
                'duracao_viagem_unidade',
                'apanhou_peixe',
                'tipo_combustivel',
                'gasolina',
                'petroleo',
                'gasoleo',
                'oleo_motor',
                'preco_gasolina',
                'preco_petroleo',
                'preco_gasoleo',
                'preco_oleo_motor',
                'custo_combustivel',
                'problemas',
                'deviceid',
                'data_descarga',
                'valid_duracao_viagem',
                'valid_combustivel',
                'observacoes')

embarcacoes <- embarcacoes[emb_cnames]



### REPLACE CHARACTERS
for(var in c("inquiridor","distrito", "comunidade",
             "distrito_desembarque", "comunidade_desembarque", "arte_de_pesca")){
  embarcacoes[,var] = gsub(".", " ", embarcacoes[,var], fixed = TRUE)
  embarcacoes[,var] = gsub("_", " ", embarcacoes[,var], fixed = TRUE)
}


### VERIFY THAT DATA FRAME HAS THE SAME NUMBER OF VARIABLES AND FORMAT
  rbind(alldat$embarcacoes,     embarcacoes)
  bind_rows(alldat$embarcacoes, embarcacoes)















nrow(embarcacoes)
delete_uuid <- embarcacoes$uuid[embarcacoes$ano == 2024]
embarcacoes <- embarcacoes[-which(embarcacoes$uuid == delete_uuid),]
nrow(embarcacoes)



nrow(embarcacoes_RAW)
embarcacoes_RAW <- embarcacoes_RAW[-which(embarcacoes_RAW$uuid == delete_uuid),]
nrow(embarcacoes_RAW)










################################################################################
#                                 CAPTURAS                                #
################################################################################


#============================== RENAME VARIABLES ==============================#

### Rename variable
for(datname in c("capturas", "capturas_RAW")){
  dat = get(datname)
  dat$peso <- as.numeric(dat$peso)
  dat$numero <- as.integer(dat$numero)
  dat$preco  <- as.integer(dat$preco)
  dat$nome_local   = dat$nomelocal
  if(!"preco_kg" %in% colnames(dat)) dat$preco_kg = as.numeric(NA)
  dat$preco_kg = as.numeric(dat$preco_kg)
  if(!"unidade_preco_kg" %in% colnames(dat)) dat$unidade_preco_kg = "STN/kg"
  if(!"valid" %in% colnames(dat)) dat$valid = TRUE
  dat$valid[dat$valid == "VERIFICAR"] <- TRUE
  dat$valid_peso <- as.integer(as.logical(dat$valid))
  dat$valid_preco <- 1
  if(!"observacoes" %in% colnames(dat)) dat$observacoes = ""
  dat$observacoes[is.na(dat$observacoes)] = ""
  dat$fotografia_link = dat$link

  dat$id <- unlist(
    lapply(
      dat$uuid,
      function(uuid) {
        pos = which(embarcacoes$uuid == uuid)
        if(length(pos) == 1)
          return(embarcacoes$id[pos]) else
            if(length(pos) == 0)
              return(NA) else
                if(length(pos) > 1)
                  return(embarcacoes$id[pos[1]])
      }
    )
  )

  dat$data_descarga <- unlist(
    lapply(
      dat$uuid,
      function(uuid) {
        pos = which(embarcacoes$uuid == uuid)
        if(length(pos) == 1)
          return(embarcacoes$data_descarga[pos]) else
            if(length(pos) == 0)
              return(NA) else
                if(length(pos) > 1)
                  return(embarcacoes$data_descarga[pos[1]])
      }
    )
  )



  dat$ano <- unlist(lapply(dat$uuid, function(uuid) {
    temp = embarcacoes$ano[which(embarcacoes$uuid == uuid)]
    if(length(temp) == 1) temp else NA
  }))



  assign(datname, dat)
}




#================================ capturas.old ================================#

capnames <- c("uuid",
              "id",
              "especie",
              "fao",
              "nome_local",
              "peso",
              "numero",
              "uso",
              "preco",
              "unidade_preco",
              "unidade_preco_kg",
              "preco_kg",
              "fotografia",
              "fotografia_link",
              "data_descarga")

capturas_RAW <- capturas_RAW[capnames]



### Standardise class of variables
questionnaire_information_path <- file.path("inst",
                                            "config",
                                            "forms.xlsx")
variables       <- read_excel(questionnaire_information_path, sheet = "vars")
positions = which(variables$dat == "capturas.old")



### CHECK WHETHER CLASS AND NUMBER OF VARIABLES IS THE SAME
  rbind(capturas_RAW,alldat$capturas_RAW)
  bind_rows(capturas_RAW,alldat$capturas_RAW)






#================================ capturas ================================#

capnames <-c(
  "uuid",
  "id",
  "especie",
  "fao",
  "nome_local",
  "peso",
  "numero",
  "uso",
  "preco",
  "unidade_preco",
  "preco_kg",
  "unidade_preco_kg",
  "fotografia",
  "fotografia_link",
  "data_descarga",
  "valid_peso",
  "valid_preco",
  "observacoes"
)

capturas <- capturas[capnames]



### Standardise class of variables
questionnaire_information_path <- file.path("inst",
                                            "config",
                                            "forms.xlsx")
variables       <- read_excel(questionnaire_information_path, sheet = "vars")
positions = which(variables$dat == "capturas")
varnames   = gsub("-", "_", variables$variablename[positions])
classnames = variables$class[positions]
for(i in 1:ncol(capturas)){
  capturas[,i] = as.character(capturas[,i])
  cname = colnames(capturas)[i]
  if(cname %in% varnames){
    as.class = get(paste0("as.", classnames[which(varnames == cname)]))
    capturas[,i] = as.class(capturas[,i])
  }
}


capturas$valid_peso  <- as.integer(capturas$valid_peso)
capturas$valid_preco <- as.integer(capturas$valid_preco)

### CHECK WHETHER CLASS AND NUMBER OF VARIABLES IS THE SAME
  rbind(capturas,alldat$capturas)
  bind_rows(capturas,alldat$capturas)

list(colnames(capturas),
     colnames(alldat$capturas))




nrow(capturas)
capturas <- capturas[-which(capturas$uuid == delete_uuid),]
nrow(capturas)



nrow(capturas_RAW)
capturas_RAW <- capturas_RAW[-which(capturas_RAW$uuid == delete_uuid),]
nrow(capturas_RAW)






################################################################################
#                                 ESFORCO                                #
################################################################################




### SEPARATE YEAR, MONTH AND DAY
esforco$ano <- as.integer(substr(esforco$data, 1,4))
esforco$mes <- as.integer(substr(esforco$data, 6,7))
esforco$dia <- as.integer(substr(esforco$data, 9,10))


esforco_RAW$ano <- as.integer(substr(esforco_RAW$data, 1,4))
esforco_RAW$mes <- as.integer(substr(esforco_RAW$data, 6,7))
esforco_RAW$dia <- as.integer(substr(esforco_RAW$data, 9,10))


esforco$distrito   = gsub("."," ", esforco$distrito, fixed = TRUE)
esforco$comunidade = gsub("."," ", esforco$comunidade, fixed = TRUE)
esforco$inquiridor = gsub("."," ", esforco$inquiridor, fixed = TRUE)


esforco_RAW$distrito   = gsub("."," ", esforco_RAW$distrito, fixed = TRUE)
esforco_RAW$comunidade = gsub("."," ", esforco_RAW$comunidade, fixed = TRUE)
esforco_RAW$inquiridor = gsub("."," ", esforco_RAW$inquiridor, fixed = TRUE)



esf_cnames <- c('uuid',
                'id',
                'ilha',
                'distrito',
                'comunidade',
                'inquiridor',
                'data',
                'ano',
                'mes',
                'dia',
                'pesca',
                'nao_pesca_razao',
                'unidade_pesca',
                'embarcacoes_ativas',
                'deviceid',
                'data_descarga')

esforco_RAW <- esforco_RAW[esf_cnames]
esforco     <- esforco[esf_cnames]

esforco_RAW$id <- as.character(esforco_RAW$id)
esforco$id     <- as.character(esforco$id)

esforco_RAW$embarcacoes_ativas <- as.integer(esforco_RAW$embarcacoes_ativas)
esforco$embarcacoes_ativas     <- as.integer(esforco$embarcacoes_ativas)

esforco$valid       <- 1
esforco$observacoes <- ""



rbind(esforco, alldat$esforco)
bind_rows(esforco, alldat$esforco)
rbind(esforco_RAW, alldat$esforco_RAW)
bind_rows(esforco_RAW, alldat$esforco_RAW)




datnames <- c("capturas", "embarcacoes", "embarcacoesativas", "esforco")
datnames = c(datnames, paste0(datnames, "_RAW"))
tryCatch(
  for(datname in datnames){
    rbind(get(datname),alldat[[datname]])
    bind_rows(get(datname),alldat[[datname]])
  }, error = function(e) {
    stop(datname)
  }
)








################################################################################
#                                    PICTURES                                  #
################################################################################



att_deposition <- CREATE_NEW_VERSION_OR_DEPOSITION(title       = paste(repository_name, "ATTACHMENTS"),
                                               description = zenodo[["description"]],
                                               creators    = zenodo[["creators"]],
                                               token       = token,
                                               instance    = zenodo[["instance"]],
                                               restricted  = FALSE)

positions <- which(!is.na(capturas$fotografia))
dat = data.frame(filename = NA, link = NA)[-1,]
for(i in positions){
  filepath <- file.path(tempfiles,capturas$fotografia[i])
  filename <- capturas$fotografia[i]
  tryCatch({

    drive_download(
      file = capturas$fotografia_link[i],
      path = filepath,
      overwrite = TRUE
    )

    link <- UPLOAD_TO_DEPOSITION(filepath = filepath,
                                 name     = filename,
                                 id = att_deposition$id,
                                 token = token,
                                 instance = zenodo[["instance"]])

    dat <- rbind(dat,
                 data.frame(filename = filename,
                            link = link)
    )

  }, error = function(e){cat("Failed")})
}


### SUBSTITUTE LINKS
for(i in 1:nrow(dat)){
  pos = which(capturas$fotografia == dat$filename[i])
  if(length(pos) == 1)
    capturas$fotografia_link[pos] <-
      dat$link[dat$filename == dat$filename[i]]
}


for(i in 1:nrow(dat)){
  pos = which(capturas_RAW$fotografia == dat$filename[i])
  if(length(pos) == 1)
    capturas_RAW$fotografia_link[pos] <-
      dat$link[dat$filename == dat$filename[i]]
}


### PUBLISH DEPOSITIONS
PUBLISH_DEPOSITION(id = att_deposition$id,
                   token = token,
                   instance = zenodo[["instance"]])








################################################################################
#                                 UPLOAD DATA                                #
################################################################################










# 2.1. Create new deposition version for collated data
cat("Criando nova versão ou repositório\n")
deposition <- CREATE_NEW_VERSION_OR_DEPOSITION(title       = repository_name,
                                               description = zenodo[["description"]],
                                               creators    = zenodo[["creators"]],
                                               instance    = zenodo[["instance"]],
                                               token       = token)


### STEP 1: DOWNLOAD PREVIOUS DATA
drive_auth(email = email)
filesindrive <- as.data.frame(drive_ls(path = "ESTATISTICA_PESQUEIRA/dados"))


drive_download(file = filesindrive$id[filesindrive$name == "estatistica_pesqueira.rds"],
               path = file.path(tempfiles,"estatistica_pesqueira.rds"),
               overwrite = TRUE)
UPLOAD_TO_DEPOSITION(filepath = file.path(tempfiles,"estatistica_pesqueira.rds"),
                     name     = "estatistica_pesqueira.rds",
                     id       = deposition$id,
                     token    = token,
                     instance = zenodo[["instance"]])


### STEP 1: DOWNLOAD PREVIOUS DATA
drive_auth(email = email)
filesindrive <- as.data.frame(drive_ls(path = "ESTATISTICA_PESQUEIRA/backup"))
for(i in 1:nrow(filesindrive)){
  filename = filesindrive$name[i]
  filepath = file.path(tempfiles,filename)
  drive_download(file = filesindrive$id[i],
                 path = filepath,
                 overwrite = TRUE)
  UPLOAD_TO_DEPOSITION(filepath = filepath,
                       name     = filename,
                       id       = deposition$id,
                       token    = token,
                       instance = zenodo[["instance"]])
}




### 4. PUBLISH COLLATED DATA DEPOSITION

cat("Publicando repositório de dados completos\n")
PUBLISH_DEPOSITION(id = deposition$id,
                   token = token,
                   instance = zenodo[["instance"]])








# 1. CREATE NEW VERSION OR DEPOSITION
deposition <- CREATE_NEW_VERSION_OR_DEPOSITION(title       = repository_name,
                                               description = zenodo[["description"]],
                                               creators    = zenodo[["creators"]],
                                               instance    = zenodo[["instance"]],
                                               token       = token)





# 2. DELETE FILES FROM PREVIOUS VERSIONS TO PREVENT LARGE REP. SIZE
# (links to files of previous versions still exist and are active)
for(x in deposition$files)
  DELETE(x$links$self,
         add_headers(Authorization = paste("Bearer", token)))










### UPDATE "all_data" TO SERVER, IF RELEVANT
now <- gsub(" ", "_", gsub("-", "", gsub(":", "", substr(Sys.time(), 3, 19) )))
dir.create(file.path(tempdir(), now))
filepath <- file.path(tempdir(), now, "data.sqlite")

# 2. READ DATA IN "data.sqlite" (creates file if it does not exist)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      filepath)

# Write collated data in "data.sqlite"
for (datname in datnames) {
  if(!is.null(get(datname))){
    DBI::dbWriteTable(conn = con,
                      name = datname,
                      value = get(datname),
                      overwrite = TRUE,
                      row.names = FALSE)
  }
}

# Disconnect file
DBI::dbDisconnect(con)




# Create new deposition
deposition <-
  CREATE_NEW_VERSION_OR_DEPOSITION(title       = repository_name,
                                   description = zenodo[["description"]],
                                   creators    = zenodo[["creators"]],
                                   token       = token,
                                   instance    = zenodo[["instance"]],
                                   language    = language)

# Delete previous files except "submissions.json"
for(x in deposition$files)
  if(!x$filename == "submissions.json")
    httr::DELETE(x$links$self,
                 httr::add_headers(Authorization = paste("Bearer", token)))


# Upload file "data.sqlite" to deposition
UPLOAD_TO_DEPOSITION(filepath = filepath,
                     name     = "data.sqlite",
                     id       = deposition$id,
                     token    = token,
                     instance = zenodo[["instance"]],
                     language = language)







### CREATE EMPTY DF TO APPEND FILEPATHS (DELETES UNUSED DIRECTORIES LATER)
all_filepaths <- data.frame(path = as.character(),
                            filepath = as.character(),
                            dat = as.character())

### WRITE NEW DATA, OBTAIN FILE PATHS OF NEW DATA FILES AND UPLOAD TO SERVER


extr_par <- function(par) lapply(databases, function(x) x[[par]])

for(i in 1:length(datnames)){
  # Get datname and dat
  datname = datnames[i]
  dat     = get(datname)


  # Is raw data?
  is_raw_data = substr(datname, nchar(datname)-3, nchar(datname)) == "_RAW"



  # Get parent name
  if(is_raw_data)
    abs_datname = substr(datname, 1, nchar(datname)-4) else
      abs_datname = datname

  versions <- lapply(extr_par("version"), function(x) if(is.na(x)) "" else x)
  ex_versions <- lapply(extr_par("exclude_version"), function(x) if(is.na(x)) "" else x)

  positions = which(
    extr_par("parent") == databases[[abs_datname]]$parent &
      extr_par("dat")   %in% datnames &
      extr_par("location") == "parent" &
      versions == versions[[abs_datname]] &
      ex_versions == ex_versions[[abs_datname]]
  )

  if(length(positions) == 0)
    parentname = NULL else
      parentname = lapply(databases, function(dat) dat$dat)[positions][[1]]

  if(!is.null(parentname) && is_raw_data)
    parentname = paste0(parentname, "_RAW")



  # Get parent
  if(is.null(parentname))
    parent = NULL else
      parent = get(parentname)





  # Create files of new data and obtain filepath of local file
  filepaths <- write_newdata(
    data                 = dat,
    name                 = datname,
    fileformat           = config[["fileformat"]],
    csv_separator        = config[["csv_separator"]],
    local_data_directory = file.path(tempdir(), now),
    method               = config[["method"]],
    nested               = databases[[abs_datname]]$location == "nested",
    grouping_variable    = config[["grouping_variable"]],
    parent               = parent,
    return.filepaths     = TRUE,
    language             = language)
  filepaths$dat <- datname
  all_filepaths <- rbind(all_filepaths, filepaths)


  # Update to server, if relevant
  for(filepath in filepaths$filepath){
    if(config[["method"]] == "GROUPED") {
      filename = paste(basename(dirname(filepath)),
                       basename(filepath),
                       sep = "_")
    } else {
      filename = basename(filepath)
    }
    UPLOAD_TO_DEPOSITION(filepath = filepath,
                         name     = filename,
                         id       = deposition$id,
                         token    = token,
                         instance = zenodo[["instance"]],
                         language = language
    )
  }
}

# Publish deposition
  deposition <- PUBLISH_DEPOSITION(id       = deposition$id,
                                   token    = token,
                                   instance = zenodo[["instance"]])
