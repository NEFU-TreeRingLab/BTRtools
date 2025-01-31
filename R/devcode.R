## devCode

## Test

# usethis::create_package("D:/R_Github/BTRtools")

# usethis::use_r("TestUITrend")

# usethis::use_r("Sim_Cells")

# usethis::use_r("Sim_gRs")

# usethis::use_r("Sim_Cambial")

# usethis::use_r("Sim_BTR")

# usethis::use_package("dplyr")
# usethis::use_package("mgcv")
# usethis::use_package("nls.multstart")
# usethis::use_package("rBTRdev")
# usethis::use_package("ggplot2")
# usethis::use_package("shiny")
# usethis::use_package("data.table")
# usethis::use_package("ggpubr")
# usethis::use_package("openxlsx")
# usethis::use_package("tibble")
# usethis::use_package("tidyr")
# usethis::use_mit_license()
# setwd( "D:/R_Github/BTRtools") ## home
# setwd( "D:/ZhaoBQ/BTRtools")
# devtools::document()

# devtools::check()

# devtools::unload()

# devtools::load_all()

# devtools::unload()

# devtools::install()

# devtools::uninstall()

# library(tidyverse)



# library(shiny)


### cell growth
# input <- list()
# input$LiF <- 0.1
# input$LiV <- 0.1
# input$dry <- c( 0.8,0.9,1)
#
# dataInCell <- list()
# dataInCell$dtParam <- openxlsx::read.xlsx('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\2.LSBP_Parameters.xlsx')    # df ## Total Param
#
# dataInCell$CBParam <- dataInCell$dtParam[ dataInCell$dtParam$Module == 'InitialCambialCell'&
#                                             dataInCell$dtParam$Parameter %in% c('CV','CTD' ,'WT' ),
#                                           c( 'Parameter', 'ParamType',  'Module','Values' )  ]
# dataInCell$FParam <- dataInCell$dtParam[ dataInCell$dtParam$Note == 'Fiber' &
#                                            dataInCell$dtParam$Module %!in% c( 'GrowthRate' ) ,
#                                          c( 'Parameter', 'ParamType','Module', 'Values' )  ]
#
# dataInCell$VParam <- dataInCell$dtParam[ dataInCell$dtParam$Note == 'Vessel'&
#                                            dataInCell$dtParam$Module %!in% c( 'GrowthRate'  ) ,
#                                          c( 'Parameter', 'ParamType','Module', 'Values' )  ]
#
#
# df <- read.csv('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\5.Cells.csv')
# cls <- intersect(colnames(df),c( 'Year','LA','RadDistR','RRadDistR','CWTall'))
# dataInCell$dtFiber <- df <- df[ , cls   ] |> dplyr::mutate(type= "Obs")
#
# df <- read.csv('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\6.Vessel.csv')
# cls <- intersect(colnames(df),c( 'Year','LA','RadDistR','RRadDistR'))
# dataInCell$dtVessel <- df <- df[, cls   ] |> dplyr::mutate(type= "Obs")
#
# ###
#
# input <- list()
# input$LiF <- 0.1
# input$LiV <- 0.1
# input$dry <- c( 0.8,0.9,1)
# input$Syear <- 2010
# input$Eyear <- 2015
# input$Cores <- 6
# input$ShowY <- c(2010:2015)
#
# input$`T1&4` <- c( 2,15 )
# input$deltaH_A_Da <- 1
# input$deltaH_D <- 4
# input$deltaS_D <- 3
# input$`M1&2` <- c( 0.1,0.3)
# input$`M3&4` <- c( 0.5,0.7)
# input$`VPD1&2` <- c( 0,0.2 )
# input$`VPD3&4` <- c( 1,3)
#
# dataInBTR <- list(   )
# dataResBTR <- list(   )
#
# # dataInBTR$dtClim <- openxlsx::read.xlsx('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\1.LSClim1.xlsx')
# # dataInBTR$dtParam <- openxlsx::read.xlsx('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\2.LSBP_Parameters.xlsx')
# # dataInBTR$dtTrend <- openxlsx::read.xlsx('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\3.TrendAge.xlsx')
# # dataInBTR$dtRW <- read.csv('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\4.BP_Ring.csv')|> dplyr::mutate(type= "Obs")
#
# dataInBTR$dtClim <- openxlsx::read.xlsx('C:\\Users\\Dr. Zhao\\Desktop\\秦岭针阔叶\\HYClim.xlsx')
# dataInBTR$dtParam <- openxlsx::read.xlsx('C:\\Users\\Dr. Zhao\\Desktop\\秦岭针阔叶\\2_Parameter.xlsx')
# dataInBTR$dtTrend <- openxlsx::read.xlsx('C:\\Users\\Dr. Zhao\\Desktop\\秦岭针阔叶\\1_GAM_TrendAge.xlsx')
# dataInBTR$dtRW <- read.csv('C:\\Users\\Dr. Zhao\\Desktop\\秦岭针阔叶\\0_RingWidths.csv')|> dplyr::mutate(type= "Obs")

# df <- read.csv('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\5.Cells.csv')
# cls <- intersect(colnames(df),c( 'Year','LA','RadDistR','RRadDistR','CWTall'))
# dataInBTR$dtFiber <- df <- df[ , cls   ] |> dplyr::mutate(type= "Obs")
# df <- read.csv('C:\\Users\\Dr. Zhao\\Desktop\\TestData\\6.Vessel.csv')
# cls <- intersect(colnames(df),c( 'Year','LA','RadDistR','RRadDistR'))
# dataInBTR$dtVessel <- df <- df[, cls   ] |> dplyr::mutate(type= "Obs")


## Tab 1 ####
# dt <- dtRW <- read.csv('C:\\Users\\Dr. Zhao\\Desktop\\秦岭针阔叶\\0_RingWidths.csv')
# Nage = 'mean.age'; Nrw = 'MRW' ; Nla = "MRW"
#
# input <- list( xaxis = Nage , MRW = Nrw,MaxLA = Nla,rwA = NULL, rwB =NULL, rwC = NULL, rwK = 5,
# laA = NULL, laB = NULL, laC = NULL, laK = 5 )
# dataIn= list(dtRW = dtRW )
# ResReg <- RegData(dt , Nage , Nrw, Nla,Ncd,Nrcta ,
#                   rwA, rwB, rwC, rwK, laA, laB, laC, laK )
#
# ## Tab 2 ,3 ####
#
# {
#   ObsA <- readxl:::read_excel("E:/论文内容/202408/wood-anatomy-data-main/Data/Obsdata\\LS_BP_A2.xlsx") |>
#     dplyr::mutate(TID = stringr::str_sub(ID,1,4) , .before ="ID" )
#   ObsA$CWTall[ObsA$CWTall <= 0 ] <- NA
#   ObsFilter <- unique( ObsA[ ,c( 'TID','path','Year' ) ] )|>
#     dplyr::group_by(Year,path) |>
#     dplyr::summarise( n = dplyr::n() ) |>
#     dplyr::filter(n >= 3 )
#   ObsV <- ObsA |> dplyr::filter(path == 'V' , Year %in% ObsFilter$Year[ObsFilter$path == "V" ]   )
#   ObsF <- ObsA |> dplyr::filter(path == 'F', Year %in% ObsFilter$Year[ObsFilter$path == "F" ] ) |>
#     dplyr::mutate(RRadDistR = round(RRadDistR, 1)) |>
#     dplyr::group_by(TID, path,ID,Year,RRadDistR) |>
#     dplyr::summarise(across(  colnames(ObsV)[ c( -1:-5,-8 )]  , ~mean(., na.rm = TRUE) ) )
#   param <- openxlsx::read.xlsx('E:/论文内容/202408/wood-anatomy-data-main/Data/SimParameters/LSBPexp_Parameters.xlsx')
# }
#
# param <- openxlsx::read.xlsx("Test\\Tdt\\Parameters.xlsx")
# # dry = 1; fgRLi= 0 ; vgRLi = 0;wgR =1
#
# ObsA <- readxl:::read_excel("Test\\Tdt\\LS_FM_A2.xlsx")
# # head(Obs)
#
# ObsV <- ObsA |> dplyr::filter(path == 'V' )
#
# ObsF <- ObsA |> dplyr::filter(path == 'F' )
#
# # EwCell <- CellGrwothData(param,wgR = 1, dry = 1, fgRLi = 0,vgRLi = 0)
# # LwCell <- CellGrwothData(param,wgR = 0.2, dry = 1, fgRLi = 2,vgRLi = 2)
#
#
# ELwCell <- rbind(  CellGrwothData(param,wgR = 1, dry = 1, fgRLi = 0,vgRLi = 0 )|>
#                      dplyr::mutate(EL = "Ew") ,
#                    CellGrwothData(param,wgR = 0.2, dry = 1, fgRLi = 1,vgRLi = 1 )|>
#                      dplyr::mutate(EL = "Lw")   )|>
#   dplyr::select(Day,CA, CV,  DDOY,  EDOY,LWA , TDOY,WA,WT,type,EL)|>
#   dplyr::distinct( .keep_all = TRUE, ignore = Day)
#
# ## Tab 4 sim gRs ####
#
#
# Tage <- read.csv("C:\\Users\\Dr. Zhao\\Desktop\\TrendAge2.csv")
# param <- openxlsx::read.xlsx("Test\\Tdt\\Parameters.xlsx")
# clims <- openxlsx::read.xlsx("Test\\Tdt\\Clim_5.xlsx") |> dplyr::filter(site == "LS")
#
# # ttp <- rBTRdev::btr_parallel(
# #   clim = clims,
# #   parameters = param,
# #   age =  Tage,
# #   syear = 2000, eyear = 2020,Cores = 10,
# #   writeRes = F,intraannual = F, gTmethod = "Jonhson" , division = "limit",
# #   testLim = F,CZgR = c(1,0,0,1 ), Pbar = T , testMod = F,Dcase = "min", Named = NULL  )
#
#
# ObsA <- readxl:::read_excel("Test\\Tdt\\LS_FM_A2.xlsx")
# ObsA$CWTall[ObsA$CWTall <= 0 ] <- NA
# ObsFilter <- unique( ObsA[ ,c( 'TID','path','Year' ) ] )|>
#   dplyr::group_by(Year,path) |>
#   dplyr::summarise( n = dplyr::n() ) |>
#   dplyr::filter(n >= 3 )
#
#
# ObsV <- ObsA |> dplyr::filter(path == 'V' , Year %in% ObsFilter$Year[ObsFilter$path == "V" ]   )
# ObsF <- ObsA |> dplyr::filter(path == 'F', Year %in% ObsFilter$Year[ObsFilter$path == "F" ] ) |>
#   dplyr::mutate(RRadDistR = round(RRadDistR, 1)) |>
#   dplyr::group_by(TID, path,ID,Year,RRadDistR) |>
#   dplyr::summarise(across(colnames(ObsV)[c( -1:-5,-8 )] , ~mean(., na.rm = TRUE) ) )
#
#
#
#
#
# # Bdata <- list(
# #   param = data.frame(param),
# #   clims = data.frame(clims),
# #   Tage = data.frame(Tage)
# # )
# ##
# input <- param[ c(26:65),c(1,6)] |>
#   tibble::remove_rownames() |>
#   tibble::column_to_rownames("parameter") |>
#   t() |> as.data.frame()
#
# input$syear <- 2000
# input$eyear <- 2020
# input$Cores <- 12
#
# input$EwLiDoyF <- 120
# input$EwLiDoyV <- 186
# input$LwLiDoyF <- 190
# input$LwLiDoyV <- 236
# input$MaxLi.fiber <- 0.5
# input$MaxLi.vessel <- 1
#
#
# SimData <- list(
#   param = as.data.frame(param),
#   clims = as.data.frame(clims),
#   Tage = as.data.frame(Tage),
#   ObsF = as.data.frame(ObsF),
#   ObsV = as.data.frame(ObsV),
#   CLines = Obsline
# )
# ResData <- list()
