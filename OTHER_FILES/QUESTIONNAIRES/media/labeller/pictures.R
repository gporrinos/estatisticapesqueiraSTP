


################################################################################
################################################################################
###------------------------ PART 1: DATA PREPARATION ------------------------###
################################################################################
################################################################################


# This part of the script calculates diversity indicators of each gear / trip (biomass, richness, 
# evenness, etc), adds covariates to databases, etc.







    #########################################################################
    ###########--------- WD, LOAD DATA, GENERAL FUNCTIONS --------###########
    #########################################################################




                #--------------------------------------------#
                ###------- Set WD and load database -------###
                #--------------------------------------------#



### PASSO 1: CARREGAR PACKAGES
suppressWarnings({
  suppressMessages({
    if(!require("ggplot2"))   install.packages("ggplot2", repos = "https://cloud.r-project.org")
    library(ggplot2)
  })})

suppressWarnings({
  suppressMessages({
    if(!require("readxl"))   install.packages("readxl", repos = "https://cloud.r-project.org")
    library(readxl)
  })})



suppressWarnings({
  suppressMessages({
    if(!require("jpeg"))   install.packages("jpeg", repos = "https://cloud.r-project.org")
    library(jpeg)
  })})
setwd(gsub("labeller/R-Portable","",getwd()))



## Pictures
pictures <- file.path(getwd(),"images_peixe")

## Load data
fish = as.data.frame(read_excel(paste0(getwd(),"/species.xlsx"),sheet = 1))
colnames(fish)[1] = "codigo_especie"

## Change special characters (just in case)
chr = data.frame(a = c("\xe3",  "\xea",   "\xe7",  "\xf4",   "\xe1",   "\xe9",  "\xfa"),
                 b = c("ã",     "ê",      "ç",     "ô",      "á",      "é",     "ú"))

for(i in 1:nrow(chr)){
     fish$nome_local_ST_fotografias <- gsub(chr$a[i],chr$b[i],fish$nome_local_ST_fotografias)
     fish$nome_local_PC_fotografias <- gsub(chr$a[i],chr$b[i],fish$nome_local_PC_fotografias)
             }


for(i in 1:nrow(fish)){
      if(paste0(fish$codigo_especie[i],".jpg") %in% list.files(pictures)) {
                  pic = jpeg::readJPEG(source = paste0(pictures,"/",fish$codigo_especie[i],".jpg"),native=TRUE)
                    } else {
                  pic = jpeg::readJPEG(source = paste0(pictures,"/espaco.jpg"),native=TRUE)
                    }
      textarea = 0.05
      sizefactor = 0.7
      p = ggplot(data.frame(x=c(0,1), y = c(0,1)), aes(x=x,y=y)) + theme_void() + 
                annotation_raster(pic, xmin = 0, xmax = 1, ymin = textarea, ymax = 1)
      for(j in 1:2){
         lab = c(fish$nome_local_ST_fotografias[i], fish$nome_local_PC_fotografias[i])[j]
         filename = c(fish$picture.ST[i],fish$picture.PC[i])[j]
         png(paste0(getwd(),"/labelled_fish/",filename,".png"),width = (567*sizefactor),height=(265*sizefactor)+((265*sizefactor)*textarea))
         print(p+geom_text(x=0.5,y=0.1,label=lab, size = (20*sizefactor)))
         dev.off()
         }
    }


png(paste0(getwd(),"/labelled_fish/espaco.png"),width = 567,height=265+(265*textarea))
    print(ggplot(data.frame(x=c(0,1), y = c(0,1)), aes(x=x,y=y)) + theme_void())
    dev.off()


warnings()






