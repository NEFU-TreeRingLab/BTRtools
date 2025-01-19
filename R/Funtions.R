#########################
### Common Functions ###
#########################

#' inverse data
#'
`%!in%` <- Negate(`%in%`)

#' replace parameters data
#'
#' @importFrom data.table as.data.table
#' @import data.table
#'
#' @return Pdata.frame
#'
#' @param DataA DataA
#' @param DataB DataB
#' @param ons by on colume
#'
NewReplacesOld  <- function( DataNew , DataOld, ons = c( 'Parameter' )  ){

  DataNew <- data.table::as.data.table(DataNew |> dplyr::rename(NewValues = Values)  )
  DataOut <- data.table::as.data.table(DataOld)
  DataOut[DataNew, Values := NewValues, on = ons ]
  return( as.data.frame(DataOut))
}

##############
### Trends ###
##############

#' Calculate fuc
#'
#' @param method method
#' @param a As
#' @param b Bs
#' @param c Cs
#' @param age age
#'
#' @return res
#'
C3mod <- function(   method , a,b,c, age ){
  age <- as.numeric(age)
  ifelse( method == "exp",  Ress <-  a *exp( b *  age ) +c ,
          ifelse(  method == "log" ,  Ress <-  a* log(age, b) +c ,
                   Ress <-  a * age^b + c  ) )
  return(Ress )
}

#' Regressiong Exp
#' @param dt datas
#' @param colx X:Age
#' @param coly Y:MRW or maxLA
#' @param method methods
#' @param O_param Old parameters
#'
#' @return EXPmodel
#'
#' @importFrom nls.multstart nls_multstart
#'
simTrend <- function( Tdt , colx = 'age' ,coly , method , O_param   ){
  Tdt1 <- Tdt
  if (method == "exp") {
    ## 估计初始值
    # 判断增减函数
    if ( is.na( O_param$Bs  )   ) { ## 没有初始值时
      pCs <- min( Tdt[,coly ]  )
      Tdt1[ ,coly] <- Tdt1[ ,coly] - 0.9 *pCs
      Bs <- lm( as.formula( paste( "log(",  coly,') ~ ',colx   )   ) ,data = Tdt1    )|> coef()
      pBs <- c(  Bs[2] *0.5  , Bs[2] *1.5   )
      pAs <-  c(  exp(Bs[1]) *0.5  , exp(Bs[1]) *1.5   )   ## 1
    } else {
      pAs <- c( 0.7*O_param$As , 1.3*O_param$As   )
      pBs <- c( 0.7*O_param$Bs , 1.3*O_param$Bs   )
      pCs <- c( 0.7*O_param$Cs , 1.3*O_param$Cs   )
    }##
    f1 <- as.formula( paste( coly, "  ~ a* exp( b* ", colx ,") + c "   )   )
  } ## if exp
  if (method == "log") {
    if ( is.na( O_param$Bs  )   ) {
      ## 估计初始值
      # 判断增减函数
      Bs <- lm( as.formula( paste(  coly,' ~ log(',colx,")"   )   ) ,data = Tdt    )|> coef()
      pBs <- c( exp(1) *0.5 , exp(1) * 1.5 )
      pAs <- c(  Bs[2] * 0.5 ,Bs[2]  * 1.5 )     ## c(  0.1 , 1 ) ##
      pCs <- c(  Bs[1] * 0.5 , Bs[1] * 1.5 )
    } else {
      pAs <- c( 0.7*O_param$As , 1.3*O_param$As   )
      pBs <- c( 0.7*O_param$Bs , 1.3*O_param$Bs   )
      pCs <- c( 0.7*O_param$Cs , 1.3*O_param$Cs   )
    }## ##

    f1 <- as.formula( paste( coly, "~ a * log( ",colx ,",b)+ c  " ) )
  } ## if exp
  if (method == "pow") {
    if ( is.na( O_param$Bs  )   ) {
      ## 估计初始值
      # 判断增减函数
      pCs <- min( Tdt[,coly ]  )
      Bs <- lm( as.formula( paste( 'log(', coly,') ~  log(',colx ,")"  )   ) ,data = Tdt    )|> coef()
      pBs <- c(  Bs[2] * 0.5 ,  Bs[2] * 1.5   )
      pAs <- c(  exp(Bs[1])* 0.5 , exp(Bs[1])* 1.5 )

    } else {
      pAs <- c( 0.7*O_param$As , 1.3*O_param$As   )
      pBs <- c( 0.7*O_param$Bs , 1.3*O_param$Bs   )
      pCs <- c( 0.7*O_param$Cs , 1.3*O_param$Cs   )
    }## ##
    f1 <- as.formula( paste( coly, "  ~ a * ", colx ,"^b  + c "   )   )
  } ## if exp
  model2 <- nls.multstart::nls_multstart( f1  ,data = Tdt ,
                                          iter = 1000,control = nls.control(maxiter = 1000, minFactor = 1e-5, warnOnly = T),
                                          start_lower = c( a= min( pAs ), b= min(pBs) ,c =min(pCs)  ),
                                          start_upper = c( a= max( pAs ), b= max(pBs) ,c =max(pCs) ) ,
                                          lower = c( a = -abs( max( Tdt[coly]) ), b = -abs( max( Tdt[coly]) )  , c = 0 ),
                                          supp_errors = 'Y' )
  return( model2 )
}

#' Regression funtion for Trend line outputs GAM & EXP
#' @param RegData data
#' @param Mparam colname of age
#' @param Oparam colname of ring width
#'
#' @return Trend line data frame
#'
#' @importFrom dplyr mutate select
#' @importFrom mgcv gam
#' @importFrom rBTR mod.test
#'
## Regression & Plot Result
##

RegPlot <- function( RegData = dt , Mparam = dataIn$ManualParam , Oparam = dataIn$O_param ){
  Resdt <- logical()
  OColn <- colnames(RegData)
  for (i_col in OColn[ c( -1,-2) ]  ) {
    Mps <- Mparam[ i_col,  ]
    Ops <- Oparam[ i_col,  ]
    ## GAM model
    F1 <- as.formula( paste( i_col,"~ s(age,k = ",Ops$Ks, ")"   ) )
    RegData$ResGAM <- predict( mgcv::gam( F1 , data = RegData ), RegData   )|> as.numeric()
    Resdt <- rbind( rBTR::mod.test(NameRow = paste0( i_col,"_GAM"  ) , RegData[, i_col] , RegData$ResGAM ),    Resdt )
    RegData <- dplyr::rename(RegData, !!!setNames( 'ResGAM', paste0( 'G_', i_col )   ) )
    ## 3func
    if ( Ops$method != 'none' ) {
      Sim1 <- simTrend( Tdt = RegData,colx = "age", coly = i_col ,method = Ops$method , O_param = Ops  )
      if ( !is.null( Sim1)  ) {
        Ops$As <- coef( Sim1 )[1]
        Ops$Bs <- coef( Sim1 )[2]
        Ops$Cs <- coef( Sim1 )[3]
        RegData$Res2 <- C3mod( method =Ops$method ,a = Ops$As,b =  Ops$Bs, c= Ops$Cs, age = RegData$age    )
        Resdt <- rbind( rBTR::mod.test(NameRow = paste0( i_col,"_Fit"  ) , RegData[, i_col] , RegData$Res2 ),    Resdt )
        RegData <- dplyr::rename(RegData, !!!setNames( 'Res2'  ,paste0(  "F_" , i_col ))   )
      } ## null sim1
      if  ( all(!is.na(Mps))    ) {
        RegData$Res3 <- C3mod( method =Ops$method ,a = Mps$As,b =  Mps$Bs, c= Mps$Cs, age = RegData$age    )
        Resdt <- rbind( rBTR::mod.test(NameRow = paste0( i_col,"_Manual"  ) , RegData[, i_col] , RegData$Res3 ),    Resdt )
        RegData <- dplyr::rename(RegData, !!!setNames( 'Res3',paste0( "M_"  ,  i_col))   )

      }
    }

    ## sim func

    RegData <- RegData |> dplyr::rename( !!setNames( i_col , paste0('O_', i_col)  )   )
    ##
    Mparam[ i_col,  ] <- Mps
    Oparam[ i_col,  ] <- Ops

  }## for i_col

  ##  nors
  RegData_n <- RegData

  for (i in 3:ncol(RegData) ) {
    RegData_n[,i] <- rBTR::nor( RegData_n[,i],Zeros = T ) |> as.numeric()
  }

  Plotdata <- dplyr::bind_rows( dplyr::mutate(  RegData , tp = 'Origin', .after = "age" ) ,
                                dplyr::mutate(  RegData_n , tp = 'Normalization',.after = "age")
  ) |>
    tidyr::gather( key , val , c( -1:-3)   ) |>
    dplyr::filter( !(key %in% c(  'O_MaxLA' , 'O_RW' ) & tp == 'Normalization' ) ) |>
    tidyr::separate( key, c( "mod", 'fac'  )  )
  Plotdata$tp <- factor(Plotdata$tp , levels =c(  'Origin' ,'Normalization'  ),
                        labels =c(  'Origin' ,'Trend(Normalization)'  ) )
  Plotdata$fac <- factor(Plotdata$fac , levels =c(  'RW' ,'MaxLA'  )  )
  Plotdata$mod <- factor(Plotdata$mod , levels =c(  'O' ,'G' ,'F' , 'M' ) ,
                         labels = c('Origin', "GAM", "Fitting" ,"Manual"   )  )

  ## PLOTS
  P1 <- ggplot2::ggplot( Plotdata, ggplot2::aes(x = age , y = val ,color = mod)  )+
    ggplot2::theme_bw()+
    ggplot2::labs(y = NULL )+
    ggplot2::geom_line( linewidth = 1  )+
    ggplot2::geom_point( data =  Plotdata[ Plotdata$mod == "Origin",  ] , ggplot2::aes(x = age , y = val ,color = mod)  )+
    ggplot2::facet_wrap( fac~tp   ,scales = "free"    )

  ## model test




  ## Output
  out <- list('dts' =RegData , "DtTrend" = RegData_n , 'Mparam' = Mparam , 'Oparam' = Oparam ,"Plot" = P1, "modelTest" = Resdt )



  return(out)
}

###################
### Cell Growth ###
###################


#### CalCells  ####
#' CalCells
#'
#' @param CBparam CBparam
#' @param Cparam Cparam
#' @param gRLi gRLi
#' @param DroughtIndex DroughtIndex
#'
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr filter
#' @importFrom data.table rbindlist
#'
#' @return Ress
#'

CalculateCells <- function( CBparam = dataInCell$CBParam, Cparam = dataInCell$VParam ,
                            gRLi = 1 , DroughtIndex = c(  0.5,0.95) ){
  ## 干旱程度分组 一组一个细胞 ，分6组
  Dry <- round( c(0:9) *(DroughtIndex[2]-DroughtIndex[1])/9    +    DroughtIndex[1],3 )

  ## Cambial Cell
  Cambial <- matrix(0,ncol = 10, nrow = 1,
                    dimnames = list(c("1"),c('CA','CV','WA','LWA','WT','CRD','CTD','EDOY','TDOY','DDOY' )) ) |>
    as.data.frame()
  Cambial$CV  <- CBparam$Values[ CBparam$Parameter == 'CV' ]
  Cambial$WT  <- CBparam$Values[ CBparam$Parameter == 'WT' ]
  Cambial$CTD <- CBparam$Values[ CBparam$Parameter == 'CTD' ]
  Cambial$CRD <- Cambial$CV/(Cambial$CTD - 2*Cambial$WT   ) + 2*Cambial$WT
  Cambial$CA  <- Cambial$CTD*Cambial$CRD
  Cambial$WA  <- Cambial$CA-Cambial$CV

  Res <- list()
  ## 空表 daily cell
  daliycells <-
    matrix(0 , ncol = 10, nrow = 20, dimnames = list( as.character( 1:20 ),colnames(Cambial)) ) |>
    as.data.frame()
  daliycells[1:20,] <- Cambial

  daliycells$EL <- rep( c( "Earlywood","Latewood"),each = 10  )
  daliycells$Dry <- rep( Dry , time = 2 )
  ### Early cell
  Day = 1
  Res[['0']] <- daliycells
  while ( Day < 70 & any( daliycells$DDOY == 0  )     ) {
    daliycells <- DailCellGr( daliycells, Cparam,
                              gRLi  = rep( c( 0,gRLi),each = 10  ) ,
                              i_dry = rep( Dry , time = 2 )  ,
                              gREL  = rep( c( 0.7,0.15),each = 10  ) , DOY = Day   )
    Res[[as.character(Day)]] <- daliycells
    Day <- Day + 1
  } ## Early cell -


  Ress <- data.table::rbindlist(Res,idcol = "Day")

  Ress$Day <- as.numeric(Ress$Day)
  Ress <- Ress[Ress$DDOY >= Ress$Day | Ress$DDOY == 0, ]

  return( Ress )
}

#### daily Cell Growth ####
#' Daily cell growth
#'
#' @param cell cell
#' @param Cparam Cparam
#' @param gRLi gRLi
#' @param i_dry i_dry
#' @param gREL gREL
#' @param DOY description
#'
#' @importFrom tidyr spread
#'
#' @return Cells
#'
DailCellGr <- function( cell = dailycells , Cparam,gRLi , i_dry ,gREL = 0.65 , DOY = 1   ){

  if( Cparam$Module[1] != "VesselGrowth" ){

    FGP <- Cparam[ ,c('Parameter' , 'Values')] |> tidyr::spread( key = 'Parameter', value = 'Values')

    vc <- FGP$va_c.fiber * gREL * i_dry

    vw <- FGP$va_w.fiber * gREL * (1 + gRLi)

    vl <- FGP$va_l.fiber * gREL * (1 + gRLi)

  }else{

    FGP <- Cparam[ ,c('Parameter' , 'Values')] |> tidyr::spread( key = 'Parameter', value = 'Values')

    vc <- FGP$va_c.vessel * gREL* i_dry

    vw <- FGP$va_w.vessel * gREL  * (1 + gRLi)

    vl <- FGP$va_l.vessel * gREL  * (1 + gRLi)

  }


  Death <- rep(1,nrow(cell))

  cell$EDOY[cell$CA != 0 & cell$EDOY == 0 ] <- DOY

  for(i in 1:10){
    ### limit factor

    ## CAt max
    CPeri <- cell$WA/FGP$WTmin

    ifelse(Cparam$Module[1] != "VesselGrowth",
           CAtmax <- cell$CRD *(0.5*CPeri-cell$CRD) ,
           CAtmax <- (0.25*CPeri)^2 )

    limitWTa <- cell$WT/FGP$WTa
    limitWTa[ limitWTa>1 ] <- 1

    ## Death

    Death[cell$DDOY != 0 ] <- 0

    ## CA 细胞面积

    dCA_dt <- vc * cell$CA * ( 1 - cell$CA/FGP$CAmax ) * ( 1- limitWTa ) * Death

    dCA_dt[dCA_dt < 0 ] <- 0 ## error catching

    cell$TDOY[dCA_dt == 0 & cell$TDOY == 0 & cell$CA > 0 ] <- DOY

    dCA_dt[ cell$TDOY != 0 |  cell$DDOY != 0  ] <- 0 ## error catching

    ### WA 细胞壁面积

    dWA_dt <- vw * ( 1 - cell$WA/FGP$WAmax ) * ( 1 - 1/( 1 + (cell$CA-cell$WA)/FGP$mw )^FGP$sw  ) * Death

    dWA_dt[dWA_dt < 0 ] <- 0 ## error catching
    dWA_dt[cell$WT >= FGP$WTmax | cell$DDOY != 0 ] <- 0 ## error catching

    ### LWA 细胞壁木质化量
    dLWA_dt <- vl *  ( 1- 1/( 1 + (cell$CA - cell$WA )/FGP$ml )^FGP$sl  )*Death

    dLWA_dt[dLWA_dt > 0 &  dLWA_dt < 0.05 ] <- 0.05 ## error catching
    dLWA_dt[dLWA_dt < 0 ] <- 0 ## error catching
    dLWA_dt[cell$LWA >= cell$WA| cell$DDOY != 0 ] <- 0 ## error catching

    ###  summary
    ##
    dCW <- cell$CA - cell$WA
    ##

    cell$CA <- cell$CA +round(dCA_dt,3 )

    cell$CA[cell$CA >= CAtmax & cell$CA > 0   ] <- CAtmax[cell$CA >= CAtmax & cell$CA > 0] ## error catching


    cell$WA <- cell$WA + round(dWA_dt,3)

    cell$WA[cell$WA > FGP$WAmax & cell$CA > 0 ] <- FGP$WAmax ## error catching
    cell$WA[cell$WA > cell$CA & cell$CA > 0] <- cell$CA[cell$WA > cell$CA & cell$CA > 0] ## error catching

    cell$LWA <-  cell$LWA + round(dLWA_dt,3)

    cell$LWA[cell$LWA > cell$WA & cell$CA > 0] <- cell$WA[cell$LWA > cell$WA & cell$CA > 0]

    ## DDOY & TODY

    cell$DDOY[ cell$LWA >= cell$WA & cell$DDOY == 0  & cell$CA > 0  ] <- DOY

    cell$TDOY[ cell$WA >= FGP$WAmax & cell$TDOY == 0  & cell$CA > 0 ] <- DOY

    cell$TDOY[ cell$WT >= FGP$WTmax & cell$TDOY == 0  & cell$CA > 0 ] <- DOY

    cell$TDOY[ cell$DDOY != 0 & cell$TDOY == 0 ] <- DOY


    ## WT & CV & CRD 径向细胞大小

    cell$CTD[Cparam$Module[1] == "VesselGrowth"] <- floor(cell$CA ^ 0.5)
    cell$CRD <- cell$CA/cell$CTD##


    cell$WT <- ( 2*(cell$CTD + cell$CRD) - (4*(cell$CTD + cell$CRD)^2 - 16 *cell$WA)^0.5) / 8

    cell$CV <- cell$CA - cell$WA

    cell[ is.na(cell) ] <- 0

    if ( all(cell$DDOY[cell$CA != 0] != 0 ) ) {
      break
    }

  } ## for 1:10 end --------

  return(cell)


}

## 拆分出早材导管和晚材细胞 ###
#' early & late wood cell
#'
#' @param dt data
#'
#' @importFrom mgcv gam
#'
#' @return ELw cells

PartELcells <- function(dt = dataInCell$dtVessel ){

  k <- ifelse( nrow(dt) > 10, 8 , ifelse(nrow(dt) > 4 , 4 , 3  )   )

  gams <- mgcv::gam( LA ~ s(RRadDistR,k = k  )  , data = dt   )

  dts <- data.frame(  RRadDistR = 1:100 ,  LA = 0 )
  dts$LA <- predict(gams , dts)

  P1 <- which.min( abs(dts$LA - ((max(dts$LA) - min(dts$LA) )*0.667 +min(dts$LA))  )   )
  P2 <- which.min( abs(dts$LA - ((max(dts$LA) - min(dts$LA) )*0.35 +min(dts$LA))  )   )
  P3 <- which.min( abs(dts$LA - ((max(dts$LA) - min(dts$LA) )*0.1 +min(dts$LA))  )   )
  dt$EL <- "UU"
  dt$type <- 'Obs'
  dt$EL[dt$RRadDistR <= P1   ] <- "Earlywood"
  dt$EL[dt$RRadDistR >= P2 & dt$RRadDistR <= P3   ] <- "Latewood"

  if ( any(colnames(dt) %!in% 'CWTall'  ) ) {
    dt$CWTall <- NA
  }

  dt <- dt[dt$EL != 'UU',  c("LA"  , 'CWTall','EL','type' )  ] ##
  colnames(dt ) <- c( 'CV' , 'WT' , 'EL','type' )
  return( dt )
}


###############
### BTR sim ###
###############
#' Plot data for BTRres
#'
#' @param dtlist data list of shiny
#' @param Obsdtlist description
#' @param dtin data Input of shiny
#' @param years InputYears
#'
#' @return Plot data
#'
#'
BtrResRemake <- function( dtlist = dataResBTR$ResBTR ,dtin = dataInBTR, years = input$ShowY ,yC,yV   ){

  ## 模拟细胞数据整理
  pdRW <- dtlist$annaulRing[ ,c('Year','RingWidth', 'VesselDensity','VesselFraction'  )] |> ## 'MaxVesselLumenArea',
    dplyr::mutate(type = "Sim") |>
    dplyr::rename(MRW = RingWidth,  CD = VesselDensity,RCTA = VesselFraction )  ## MaxLA =MaxVesselLumenArea,

  pdF <- dtlist$xylem_trait[ ,c( 'Year', 'CV','WT', 'rrFiber','Raddist','DDOY' )] |>
    dplyr::mutate(type = "Sim") |>
    dplyr::rename(LA = CV, CWTall =WT, RRadDistR  = rrFiber ,RadDistR = Raddist,DOY = DDOY )

  pdV <- dtlist$xylem_trait[ !is.na(dtlist$xylem_trait$VCV),c( 'Year', 'VCV', 'rrVessel','Raddist','VDDOY' )] |>
    dplyr::mutate(type = "Sim") |>
    dplyr::rename(LA = VCV, RRadDistR = rrVessel  ,RadDistR = Raddist, DOY = VDDOY )

  ## 模拟年份与轮宽观测值年份交集
  Years <- intersect( pdRW$Year[pdRW$type == "Sim" ], unique(dtin$dtRW$Year)   )
  ## filter RWres
  pdRW <- pdRW[ pdRW$Year %in% Years,   ] |> dplyr::arrange(Year) |> dplyr::ungroup()

  Res <- list(  )

  ## 有观测值就计算结果
  if ( dtin$ObsInput[1] == 1 ) {
    ResRW <- dtin$dtRW[ dtin$dtRW$Year %in% Years, ] |> dplyr::arrange(Year)  |> dplyr::ungroup()
    Cols <- intersect( colnames(ResRW) ,  colnames(pdRW)   ) |> setdiff(  c( 'Year','type' )  )
    ModTestList <- list()
    for (icol in Cols) {
      ModTestList[[ icol ]] <- rBTR::mod.test( ResRW[[icol]]  ,pdRW[[icol]]   ) |> round(2) |> data.frame() ##
    }
    pdRW <-  pdRW |> dplyr::bind_rows( ResRW[,c('Year','type',Cols)]) |> tidyr::gather( key = Factor, value = val, c(-"Year", - "type"  )    )
    Res[['pdRW']] <- pdRW
    Res[['ModTestList']] <- ModTestList

  } ## if RW Obs

  ## Fiber
  ## 抽6年结果做展示

  ShowYears <-  ifelse( is.na(yC), intersect( Years, c(years[1]:years[2]) ), intersect( c(years[1]:years[2]), yC ) )
  ifelse( length(ShowYears) > 6, selected_Years <- sample(ShowYears, 6) |> sort() ,selected_Years <- ShowYears  )
  pdFiber <- pdF[ pdF$Year %in% selected_Years, ] |>
    tidyr::gather(key = Fac, value = val , c('LA','CWTall'))
  Res[['pdFiber']] <- pdFiber

  if (  dtin$ObsInput[2] == 1    ) {

    GamFiber <- dtin$dtFiber[dtin$dtFiber$Year%in% selected_Years,    ] |>
      GAMCells( celltype = "F"  ) |>
      tidyr::gather(key = Fac, value = val , c('LA','CWTall')) |>
      dplyr::mutate( type= "ObsLine" )

    TestFiber  <- rbind( rBTR::mod.test.cells( devOutputs = dtlist ,OBSdata = dtin$dtFiber ,
                                               Years = selected_Years,celltype = 'fiber' ,CellParam = 'CWTall'   ),
                         rBTR::mod.test.cells( devOutputs = dtlist , OBSdata = dtin$dtFiber ,
                                               Years = selected_Years,celltype = 'fiber' ,CellParam = 'LA'   )
    )[,c('Year','CellParam','COR','pvalue','MAPE')  ] |>
      dplyr::mutate( pvalue = dplyr::case_when(pvalue <= 0.05 ~ "*",pvalue > 0.05 ~ "" )  ,
                     ResC = paste0('r: ',round(  as.numeric(COR),3), pvalue,"\nMAPE: ",round( as.numeric(MAPE),1),"%"  )  )|>
      dplyr::rename(Fac = CellParam ) |> dplyr::filter( Year != 'all')##

    Res[['TestFiber']] <- TestFiber
    Res[['GamFiber']] <- GamFiber
  } ## If Fiber Obs

  ## Vessel
  ShowYears <-  ifelse( is.na(yV), intersect( Years, c(years[1]:years[2]) ), intersect( c(years[1]:years[2]), yV ) )
  pdVessel <- pdV[ pdV$Year %in% selected_Years  , ]
  Res[['pdVessel']] <- pdVessel
  if ( dtin$ObsInput[3] == 1    ) {

    GamVessel <- GAMCells( dt = dtin$dtVessel[dtin$dtVessel$Year %in% selected_Years,   ] ,celltype = "V"  ) |>
      dplyr::mutate(type = "ObsLine")

    TestVessel  <- rBTR::mod.test.cells( devOutputs = dtlist , OBSdata = dtin$dtFiber ,
                                         Years = selected_Years,celltype = 'fiber' ,CellParam = 'LA'   )[,c('Year','CellParam','COR','pvalue','MAPE')  ] |>
      dplyr::mutate( pvalue = dplyr::case_when(pvalue <= 0.05 ~ "*",pvalue > 0.05 ~ "" )  ,
                     ResC = paste0('r: ',round(  as.numeric(COR),3), pvalue,"\nMAPE: ",round( as.numeric(MAPE),1),"%"  )  )|>
      dplyr::rename(Fac = CellParam ) |> dplyr::filter( Year != 'all')##
    Res[['TestVessel']] <- TestVessel
    Res[['GamVessel']] <- GamVessel
  } ## If Vessel Obs

  ### RR & DOY
  dtGamDOY <- dtlist$IntraAnnualGrowth[,c('Year','DOY','RingWidth')] |>
    dplyr::left_join( dtlist$annaulRing[,c("Year",'RingWidth')] |> dplyr::rename(MRW = RingWidth)) |>
    dplyr::mutate( RRadDistR =RingWidth/MRW *100     ) |> dplyr::filter(Year %in% ShowYears   )
  GamDOY <- mgcv::gam( DOY  ~ s(RRadDistR , k = 4 ,by = Year  ) , data = dtGamDOY )

  inFiber <- dtin$dtFiber[ dtin$dtFiber$Year %in% selected_Years, ]
  inVessel <- dtin$dtVessel[ dtin$dtVessel$Year %in% selected_Years, ]
  if (!is.null(inFiber)) {
    inFiber$DOY <- predict(GamDOY , inFiber )
    Res[['inFiber']] <- inFiber
  }
  if (!is.null(inVessel)) {
    inVessel$DOY <- predict(GamDOY , inVessel )
    Res[['inVessel']] <- inVessel
  }

  return( Res )
}## BtrResRemake -

#' Sim cells line
#'
#' @param dt Fiber Result
#' @param celltype Cell type
#'
#' @importFrom data.table rbindlist
#' @importFrom mgcv gam
#'
#' @return Resdata
#'

GAMCells <- function( dt, celltype= "V"   ){
  DT <- data.frame( RRadDistR = 0:100    )
  Res <- list()


  if (celltype == "F") {
    for (i in unique( dt$Year) ) {

      if (nrow(dt[ dt$Year == i ,]) >= 5 ) { ks <- 5 } else { Ks <- 3 }
      if (nrow(dt[ dt$Year == i ,]) < 3 ) { break }

      GAM1 <- mgcv::gam( LA ~ s(RRadDistR , k = 5  ) ,data = dt[ dt$Year == i ,]    )
      DT$LA <- predict( GAM1, DT  )

      GAM2 <- mgcv::gam( CWTall ~ s(RRadDistR , k = ks  ) ,data = dt    )
      DT$CWTall <- predict( GAM2, DT  )
      Res[[ as.character(i) ]] <- DT
    }
  }else{
    for (i in unique( dt$Year) ) {
      if (nrow(dt[ dt$Year == i ,]) >= 5 ) { ks <- 5 } else { Ks <- 3 }
      if (nrow(dt[ dt$Year == i ,]) < 3 ) { break }

      GAM1 <- mgcv::gam( LA ~ s(RRadDistR , k = ks  ) ,data = dt[ dt$Year == i ,]    )
      DT$LA <- predict( GAM1, DT  )
      Res[[ as.character(i) ]] <- DT

    }

  }




  Resdt <- data.table::rbindlist(Res, idcol = 'Year')
  Resdt$Year <- as.numeric(Resdt$Year )
  return(Resdt)
} ## GAMCells-

#' ComputerGRs
#'
#'
#' @param GRParam paramters
#' @param dt climatedata
#'
#' @importFrom dplyr mutate select rowwise case_when summarise n group_by
#' @importFrom tidyr spread
#' @importFrom rBTR nor
#'
#' @return res
#'
GRsim <- function(GRParam , dt, dtRW, ys  ){

  ys <- intersect( c(ys[1]:ys[2]) , unique(dt$Year)) |> intersect( unique(dtRW$Year) )

  growthParam <-GRParam |> dplyr::select(c("Parameter","Values")) |>
    tidyr::spread(key = Parameter, value = Values)

  ### gT

  clim <- dplyr::mutate(dt[dt$Year %in% ys, ], gT = NA , gM = NA, gV = NA, L_i.fiber =NA , L_i.vessel = NA)

  Ta <- clim$TEM + 273.15
  R <- 8.314

  clim$gT <- ( Ta * exp( - growthParam$deltaH_A_Da / ( R * Ta) ) /
                 (1 + exp( growthParam$deltaS_D / R - growthParam$deltaH_D /(R*Ta) ) ) ) |>
    rBTR::nor( Zeros = T)
  clim$gT[clim$TEM <= growthParam$T1| clim$TEM >= growthParam$T4 ] <- 0



  ### gM

  clim$gM[ clim$soilM < growthParam$M1 & is.na(clim$gM) ] <- 0

  clim$gM[ clim$soilM > growthParam$M4 & is.na(clim$gM) ] <- 0

  clim$gM[ clim$soilM < growthParam$M2 & is.na(clim$gM) ] <-
    (clim$soilM[ clim$soilM < growthParam$M2 & is.na(clim$gM) ] - growthParam$M1) / (growthParam$M2-growthParam$M1)

  clim$gM[ clim$soilM > growthParam$M3 & is.na(clim$gM) ] <-
    (growthParam$M4 - clim$soilM[ clim$soilM > growthParam$M3 & is.na(clim$gM) ]) / (growthParam$M4-growthParam$M3)

  clim$gM[ clim$soilM <= growthParam$M3 & is.na(clim$gM) ] <- 1

  ### gT


  clim$gV[ clim$VPD < growthParam$VPD1 & is.na(clim$gV) ] <- 0

  clim$gV[ clim$VPD > growthParam$VPD4 & is.na(clim$gV) ] <- 0

  clim$gV[ clim$VPD < growthParam$VPD2 & is.na(clim$gV) ] <-
    (clim$VPD[ clim$VPD < growthParam$VPD2 & is.na(clim$gV) ] - growthParam$VPD1) / (growthParam$VPD2-growthParam$VPD1)

  clim$gV[ clim$VPD > growthParam$VPD3 & is.na(clim$gV) ] <-
    (growthParam$VPD4 - clim$VPD[ clim$VPD > growthParam$VPD3 & is.na(clim$gV) ]) / (growthParam$VPD4-growthParam$VPD3)

  clim$gV[ clim$VPD <= growthParam$VPD3 & is.na(clim$gV) ] <- 1

  rt <- max(clim$rootd)
  clim<- clim|> dplyr::rowwise() |> dplyr::mutate( Lim =  min( gT, gM, gV ),
                                                   LimFac = dplyr::case_when( Lim == gT ~ 'gT',
                                                                              Lim == gM ~ 'gM',
                                                                              Lim == gV ~ 'gV'     ),
                                                   Lim =  Lim* gE*rootd/rt  )

  clim2 <- clim[clim$Lim > 0, ] |> dplyr::group_by( Year,LimFac ) |>
    dplyr::summarise( gR = sum(Lim,na.rm = T) , Count = dplyr::n() , .groups = "drop"   )

  clim3 <- clim2 |> dplyr::group_by( Year ) |>
    dplyr::summarise( gRs = sum(gR,na.rm = T) , GrowthSeason = sum(Count)   ) |>
    dplyr::ungroup( ) |>
    dplyr::mutate(  degR = gRs/mean(gRs)    ) ##


  Res <- dplyr::left_join(clim3, dtRW  ) |> dplyr::select(Year,degR,deRW)

  return( list( Lims = clim2, LimY = Res   )  )
}

#' detrendRW
#'
#' @param dtTrend dtTrend
#' @param dtRW dtRW
#'
#' @importFrom rBTR nor
#' @importFrom dplyr left_join select mutate
#'
#' @return datas

detrendRW <- function( dtTrend , dtRW ){
  dtY <- intersect( unique(dtTrend$Year), unique(dtRW$Year)   )
  dtRes <- dtTrend[dtTrend$Year %in% dtY,  ] |> dplyr::left_join(dtRW) |> dplyr::select(Year, Tage,MRW )

  MMRW <- mean( dtRes$MRW )
  MMT <- mean(dtRes$Tage  )

  dtRes <- dtRes|> dplyr::mutate(deRW = MRW/( Tage * MMRW/MMT )   ) ## deRW = MRW - Tage * MMRW/MMT ,
  # ggplot( dtRes ) +
  #   geom_line(aes(x = Year, y = Tage *MMRW/MMT,color = "T"  ))+
  #   geom_line(aes(x = Year, y = MRW,color = "O"  ))
  # ggplot( dtRes ) +
  #   geom_line(aes(x = Year, y = MRW,color = "O"  ))
  # ggplot( dtRes ) +
  #   geom_line(aes(x = Year, y = deRW,color = "T"  ))+
  #   geom_line(aes(x = Year, y = deRW2,color = "O"  ))

  return(dtRes[  ,c( 'Year','deRW')])
}





