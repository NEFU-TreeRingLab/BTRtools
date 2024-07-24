# usethis::create_package("D:/R_Github/BTRtools")

usethis::use_r("TestUITrend")

usethis::use_r("Sim_Cells")

# usethis::use_package("dplyr")
# usethis::use_package("mgcv")
# usethis::use_package("nls.multstart")
# usethis::use_package("rBTRdev")
# usethis::use_package("ggplot2")
# usethis::use_package("shiny")

devtools::document()

devtools::check()

devtools::unload()

devtools::load_all()



library(tidyverse)
library(shiny)



## Tab 1 ####
dt <- dtRW <- read.csv("Test\\Tdt\\FM.csv")
Nage = 'age'; Nrw = 'MRW' ; Nla = "MAXLA"
rwA = NULL; rwB =NULL; rwC = NULL; rwK = 5
laA = NULL; laB = NULL; laC = NULL; laK = 5

ResReg <- RegData(dt , Nage , Nrw, Nla,
                  rwA, rwB, rwC, rwK, laA, laB, laC, laK )

## Tab 2 ,3 ####

param <- openxlsx::read.xlsx("Test\\Tdt\\Parameters.xlsx")
# dry = 1; fgRLi= 0 ; vgRLi = 0;wgR =1

ObsA <- readxl:::read_excel("Test\\Tdt\\LS_FM_A2.xlsx")
# head(Obs)

ObsV <- ObsA |> dplyr::filter(path == 'V' )

ObsF <- ObsA |> dplyr::filter(path == 'F' )

# EwCell <- CellGrwothData(param,wgR = 1, dry = 1, fgRLi = 0,vgRLi = 0)
# LwCell <- CellGrwothData(param,wgR = 0.2, dry = 1, fgRLi = 2,vgRLi = 2)


ELwCell <- rbind(  CellGrwothData(param,wgR = 1, dry = 1, fgRLi = 0,vgRLi = 0 )|>
                     dplyr::mutate(EL = "Ew") ,
                   CellGrwothData(param,wgR = 0.2, dry = 1, fgRLi = 1,vgRLi = 1 )|>
                     dplyr::mutate(EL = "Lw")   )|>
  dplyr::select(Day,CA, CV,  DDOY,  EDOY,LWA , TDOY,WA,WT,type,EL)|>
  dplyr::distinct( .keep_all = TRUE, ignore = Day)

## Tab 4 sim gR &
