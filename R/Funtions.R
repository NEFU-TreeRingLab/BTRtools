#### func for Regression GAM & EXP ####
## Func EXP

#' 取反集
#'
`%!in%` <- Negate(`%in%`)

#' Compute_daylengthfactor
#'
#' @param phi Fix parameters
#'
#' @return day length and standardization day length

compute_daylength <-function( phi ){  ## daylength start


  #单位转换与常数设置
  lat  <-  phi * pi / 180# change to radians 角度改为弧度制
  I_0  <-  118.109#太阳常数MJ/s
  doy  <- 1:366
  theta_0  <-  2 * pi * doy / 366
  #地球轨道偏心率订正因子
  rho_2  <-  1.000110 + 0.034221 * cos(theta_0) + 0.001280 * sin(theta_0) +
    0.000719 * cos(2 * theta_0) + 0.000077 * sin(2 * theta_0)
  #赤纬/太阳偏角(180/pi)*
  delta  <-  (0.006918-0.399912 * cos(theta_0) + 0.070257 * sin(theta_0) -
                0.006758 * cos(2 * theta_0) + 0.000970 * sin(2 * theta_0)-
                0.002697 * cos(3 * theta_0) + 0.000148 * sin(3 * theta_0))
  #时角
  omega_0  <-  acos(-tan(lat) * tan(delta))
  #最大日照时数
  L <- (24 / pi * omega_0) / 12
  dLi <- c(0,diff(L))
  Li <- data.frame(DOY = doy , Li = Li, dLi = dLi)

  return(Li)

} ## daylength end -----------------

#' Regressiong Exp
#' @param dt datas
#' @param colx X:Age
#' @param coly Y:MRW or maxLA
#' @param SSalpha alpha
#' @param SSbeta beta
#' @param SSC C value
#'
#' @return EXPmodel
#'
#' @importFrom nls.multstart nls_multstart
#'
modelEXP <- function( dt,colx ,coly , SSalpha,SSbeta,SSC  ){
  as = c(SSalpha*0.5,  SSalpha*1.5  )
  bs = c( SSbeta * 0.5 , SSbeta * 1.5   )
  cs = c( SSC * 0.5 , SSC * 1.5  )

  f1 <- as.formula( paste( coly, "  ~ a* exp( b* ", colx ,") + c "   )   )
  model2 <- nls.multstart::nls_multstart( f1  ,
                           data = dt ,
                           iter = 500,
                           start_lower = c( a= min( as ), b= min(bs) ,c =min(cs)  ),
                           start_upper = c( a= max( as ), b= max(bs) ,c =max(cs) ) ,
                           lower = c( a = -abs( max( as ) ), b = -abs( max( bs ) ) , c = 0 ),
                           supp_errors = 'Y' )

  return( model2 )
}

#' Normalization
#'
#' @param x num vector
#'
#' @return normalization res
#'
nors <- function( x ){
  res <- x /max(x)
}

#' Regression funtion for Trend line outputs GAM & EXP
#' @param dt data
#' @param Nage colname of age
#' @param Nrw colname of ring width
#' @param Nla colname of La
#' @param Ncd colname of CD
#' @param Nrcta colname of RCTA
#' @param rwA rwA
#' @param rwB rwB
#' @param rwC rwC
#' @param rwK rwK
#' @param laA laA
#' @param laB laB
#' @param laC laC
#' @param laK laK
#'
#' @return Trend line data frame
#'
#' @importFrom dplyr mutate select
#' @importFrom mgcv gam
#' @importFrom rBTRdev mod.test
#'
## Regression Result
RegData <- function( dt , Nage , Nrw, Nla,Ncd, Nrcta,
                     rwA, rwB, rwC, rwK, laA, laB, laC, laK ){

   dtOri <- dt |>
    dplyr::select( all_of(c("Year", Nage, Nrw, Nla,Ncd, Nrcta ) ) ) |>
    na.omit()
  names(dtOri) <- c("Year", "age","MRW", "MaxLA", "CD", 'RCTA' )

  ifelse(is.na(rwA), rwA <- max(dtOri$MRW ),NA  )
  ifelse(is.na(rwC), rwC <- min(dtOri$MRW )*0.8,NA  )
  ifelse(is.na(rwB), rwB <- (rwA - rwC)/nrow(dtOri) ,NA  )

  ifelse(is.na(laA), laA <- max(dtOri$MaxLA ),NA  )
  ifelse(is.na(laC), laC <- min(dtOri$MaxLA )*0.8,NA  )
  ifelse(is.na(laB), laB <- (laA - laC)/nrow(dtOri) ,NA  )

  dtOri <- dtOri |>
    dplyr::mutate(pGamRw=0, pExpRw=0,pGamLa=0,pExpLa=0,
                  mExpRw = rwA*exp(rwB * age ) + rwC,
                  mExpLa = laA*exp(laB * age ) + laC )

  ExpResRW <- tryCatch(
    { modEXP = modelEXP(dt = dtOri,colx = "age",coly =  "MRW" , ## 运行modelEXP函数
               SSalpha = rwA, SSbeta = rwB, SSC = rwC )
      list(TN = "Y", modEXP = modEXP  )  ## 当函数正常运行时，输出 list, TN 赋值Y  和 modEXP赋值结果
    },error = function(e) { ## 当函数 error 时
      list(TN = "N", modEXP = modEXP  ) ## TN 赋值 N ， modExp 赋值结果“NULL”
    },waring = function(w){ ## 当函数 warning 时
      list(TN = "N", modEXP = modEXP  ) ## TN 赋值 N ， modExp 赋值结果“NULL”
    }   )## end trycatch

  if ( ExpResRW$TN == "Y" ) {
    modEXP <- ExpResRW$modEXP
    dtOri$pExpRw <- predict( modEXP , dtOri )
    alphaRw <- coef(modEXP)[1] |>round(5)
    betaRw <- coef(modEXP)[2] |>round(5)
    cRw <- coef(modEXP)[3] |>round(5)
  } else {
    alphaRw <- paste("error:", round(0.8*max(dtOri$MRW) ,5)  )
    betaRw <-  paste("error:", round((max(dtOri$MRW)-min(dtOri$MRW))/nrow(dtOri),5  )   )
    cRw <-  paste("error:", round(min(dtOri$MRW)*0.5,5  )   )
  }

  ExpResLA <- tryCatch(
    { modEXP = modelEXP(dt = dtOri,colx = "age",coly =  "MaxLA" ,
               SSalpha = rwA, SSbeta = rwB, SSC = rwC )
      list(TN = "Y", modEXP = modEXP  )
    },error = function(e) {
      list(TN = "N", modEXP = 0  )
    },waring = function(w){
      list(TN = "N", modEXP = 0  )
    }   )## end trycatch

  if ( ExpResLA$TN == "Y"  ) {
    modEXP <- ExpResLA$modEXP
    dtOri$pExpLa <- predict( modEXP , dtOri )
    alphaLa <- coef(modEXP)[1] |>round(5)
    betaLa <- coef(modEXP)[2] |>round(5)
    cLa <- coef(modEXP)[3] |>round(5)
  } else {
    alphaLa <- paste("error:", round(0.8*max(dtOri$MaxLA) ,5)  )
    betaLa <-  paste("error:", round((max(dtOri$MaxLA)-min(dtOri$MaxLA))/nrow(dtOri),5  )   )
    cLa <-  paste("error:", round(min(dtOri$MaxLA)*0.5,5  )   )
  }

  modGamRw <- mgcv::gam( MRW ~ s( age , k = rwK ) ,data = dtOri  )
  dtOri$pGamRw <- predict( modGamRw , dtOri )

  modGamLa <- mgcv::gam( MaxLA ~ s( age , k = laK ) ,data = dtOri  )
  dtOri$pGamLa<- predict( modGamLa , dtOri )

  dtNor <- dtOri |> dplyr::mutate( pGamRw = nors(pGamRw),  pExpRw = nors(pExpRw),mExpRw = nors(mExpRw),
                                   pGamLa = nors(pGamLa),  pExpLa = nors(pExpLa),mExpLa = nors(mExpLa) )
  param <- data.frame( Name = c( 'MRW' , 'MaxLA'),
                       alpha= c( alphaRw, alphaLa ) ,
                       beta = c( betaRw, betaLa ),
                       c = c( cRw, cLa))

  ModAs <- rbind( rBTRdev::mod.test(dtOri$MRW, dtOri$pGamRw ) ,
                  rBTRdev::mod.test(dtOri$MRW, dtOri$pExpRw ),
                  rBTRdev::mod.test(dtOri$MRW, dtOri$mExpRw ) ,

                  rBTRdev::mod.test(dtOri$MaxLA, dtOri$pGamLa ) ,
                  rBTRdev::mod.test(dtOri$MaxLA, dtOri$pExpLa ),
                  rBTRdev::mod.test(dtOri$MaxLA, dtOri$mExpLa )
  ) |> round(3)
  rownames(ModAs) <- c('RwGAM' , 'RwExp' ,'RwManual', 'LaGAM' , 'LaExp' , 'LaManual'  )

  out <- list(dtOri = dtOri, dtNor = dtNor,param = param , ModAs = ModAs)
  return(out)
}

#' For table 2,3 calculate cell growth
#'
#' @returns cell daily traits and new param
#'
#' @importFrom dplyr mutate select filter rename
#' @importFrom tidyr spread
#' @importFrom rBTRdev daily_grwoth
#' @importFrom data.table rbindlist
#'
#' @param param parameters
#' @param wgR wgR gR 0-1
#' @param dry dry factor 0-1
#' @param fgRli L_i for fiber gR
#' @param vgRLi L_i for vessel gR
#'

CellGrwothData <- function( param,wgR, dry, fgRLi,vgRLi ){

  fixparam.divi <- param[ param$modul == "division" & param$paramtype == "fixed",
                               c("parameter","values") ]   |>
    tidyr::spread( key = 'parameter', value = 'values')

  fixparam.growth.fiber <- param[param$modul == "growthC" & param$paramtype == "fixed" ,
                                      c("parameter","values")]  |>
    tidyr::spread( key = 'parameter', value = 'values')

  fixparam.growth.vessel <- param[param$modul == "growthV" & param$paramtype == "fixed",
                                       c("parameter","values") ] |>
    tidyr::spread( key = 'parameter', value = 'values')

  dynparam.growth.t <- matrix(0,nrow = 1, ncol = 19) |> data.frame()
  colnames(dynparam.growth.t) <-
    c( 'L_i.fiber','L_i.vessel','dCA_cz',	'SumCL',	'SumVL',	'SumV',
       'v_c.fiber',	'v_w.fiber',	'v_l.fiber',	'v_c.vessel',	'v_w.vessel',	'v_l.vessel',
       'deltaVN',	'Vcz',	'grwothSeason',	'age',	'T_age',	'czgR',	'egR' )

  clim.today <-  matrix(1,nrow = 1, ncol = 2) |> data.frame()
  colnames(clim.today) <- c('DOY', 'Year')

  dynparam.growth.t$v_c.fiber  <- fixparam.divi$va_c.fiber  * wgR * dry
  dynparam.growth.t$v_w.fiber  <- fixparam.divi$va_w.fiber  * wgR * (1+fgRLi)
  dynparam.growth.t$v_l.fiber  <- fixparam.divi$va_l.fiber * wgR * (1+fgRLi)

  dynparam.growth.t$v_c.vessel <- fixparam.divi$va_c.vessel * wgR * dry
  dynparam.growth.t$v_w.vessel <- fixparam.divi$va_w.vessel * wgR * (1+vgRLi)
  dynparam.growth.t$v_l.vessel <- fixparam.divi$va_l.vessel * wgR * (1+vgRLi)

  ## 设置各类初始值： 纤维细胞和导管初始值
  cells <- dplyr::filter(param ,  grepl("C0", modul ) ) |>
    dplyr::select( c("parameter","values") ) |>
    tidyr::spread( key = 'parameter', value = 'values') |>
    dplyr::select( "cell_L","Year", everything())
  cells[is.na(cells)] <- 0

  vessels <- dplyr::filter(param ,  grepl("V0", modul ) ) |>
    dplyr::select( c("parameter","values") ) |>
    tidyr::spread( key = 'parameter', value = 'values')|>
    dplyr::select( "cell_L","Year", everything())
  vessels[is.na(vessels)] <- 0
  ## 每年的空白生长表
  dailyCells <- list(
    dailyFiber = matrix(data = 0, ncol = ncol(cells), nrow = 1) |>
      as.data.frame() |>
      dplyr::rename( !!!setNames(  paste0("V", seq(1,12,1)), colnames(cells))  ) |>
      dplyr::mutate( Year = 2000 ,cell_L = 1 ),
    dailyVessels = matrix(data = 0, ncol = ncol(vessels), nrow = 1) |>
      as.data.frame() |>
      dplyr::rename( !!!setNames(  paste0("V", seq(1,14,1)), colnames(vessels))  ) |>
      dplyr::mutate( Year = 2000 ,cell_L = 1 )
  )

  dailyCells$dailyFiber[,-1:-2] <- cells[,-1:-2]
  dailyCells$dailyVessels[,-1:-2] <- vessels[,-1:-2]

  dfGrwoth <- list()
  dvGrwoth <- list()

  dfGrwoth[["0"]] <- dailyCells$dailyFiber
  dvGrwoth[["0"]] <- dailyCells$dailyVessels

  while( any(dailyCells$dailyFiber$DDOY == 0 , dailyCells$dailyVessels$DDOY == 0  )   ){

  dailyCells <-rBTRdev::daily_grwoth( newCell = 0,newVessel = 0,vesselsNum = 0,dailyCells =  dailyCells ,
                cells = cells,vessels = vessels ,
                clim.today = clim.today ,
                fixparam.growth.fiber = fixparam.growth.fiber,
                fixparam.growth.vessel = fixparam.growth.vessel,
                dynparam.growth.t = dynparam.growth.t  )

  dfGrwoth[[as.character(clim.today$DOY )]] <- dailyCells$dailyFiber
  dvGrwoth[[as.character(clim.today$DOY )]] <- dailyCells$dailyVessels
  clim.today$DOY <- clim.today$DOY+1
  }

  dfGrwoth <- data.table::rbindlist(dfGrwoth,use.names = T,fill = T,idcol = "Day")|>
    dplyr::mutate(Day = as.numeric(Day), type = "Fiber")
  dvGrwoth <- data.table::rbindlist(dvGrwoth,use.names = T,fill = T,idcol = "Day")|>
    dplyr::mutate(Day = as.numeric(Day), type = "Vessel")
out <- rbind( dfGrwoth,dvGrwoth, fill=TRUE)
return(out)

}


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

AupdatedB <- function( DataA , DataB, ons ){

  DataA <- data.table::as.data.table(DataA)
  DataB <- data.table::as.data.table(DataB)
  DataB[DataA, values := Nvalues, on = ons ]
  return( as.data.frame(DataB))
}

#' calculate gRs
#'
#' @return gRs
#'
#' @importFrom rBTRdev Compute_gR2
#' @importFrom dplyr left_join group_by arrange mutate case_when filter count summarise select
#' @importFrom tidyr spread gather
#'
#' @param clims climate data
#' @param param parameters
#'



ComputeGrs <- function(clims ,param){

  param$values <- as.numeric(param$values)
  growth_Param  <- param[param$modul == "gR",] ## 生长速率阈值参数

  microclim <- rBTRdev:::Compute_gR2(clims , growth_Param) |>
    dplyr::group_by(Year) |>
    dplyr::arrange(Year,DOY) |>
    dplyr::mutate( aT  = TEM - param$values[param$parameter == "T1"]  ,
                   aT = dplyr::case_when(
                   aT <0 ~ 0,
                   aT >= 0 ~ aT),
                   aaT = cumsum(aT),
                   wgR = gE * pmin(gT,gM,gV),
                   Mins = dplyr::case_when(
                     gT < gM &gT < gV ~ 'gTd',
                     gM < gT &gM < gV ~ 'gMd',
                     gV < gT &gV < gM ~ 'gVd')
                  ) |>
    dplyr::filter( wgR > 0  & aaT >= param$values[param$parameter == "AAT"]  )

    m1 <- microclim|>
          dplyr::group_by( Year ) |>
          dplyr::summarise(  gR = sum(wgR) , GSday = dplyr::n()  )

    m2 <- microclim |> dplyr::count(Mins) |> tidyr::spread( key  = Mins, value = n )

    m3 <- microclim|> dplyr::select(Year,DOY, wgR, Mins) |>
      tidyr::spread( key  = Mins, value = wgR ) |> dplyr::group_by(Year) |>
      dplyr::summarise( gTs = sum(gTd,na.rm = T) ,
                 gMs = sum(gMd,na.rm = T) ,
                 gVs = sum(gVd,na.rm = T)  )

    m4 <- microclim |> dplyr::group_by(DOY) |>
      dplyr::summarise( gT = mean(gT,na.rm = T) ,
                        gM = mean(gM,na.rm = T) ,
                        gV = mean(gV,na.rm = T)  ) |>
      tidyr::gather(gR, val,-1)

    m3 <- m1 |>
      dplyr::left_join(m2) |>
      dplyr::left_join(m3) |>
      dplyr::mutate( sgR = scale(gR)    ) ## gT = gTs / gR ,gM = gMs / gR,gV = gVs / gR




    return( list(DOYs = m4 , Years = m3   ) ) ## m3
}


#' calculate Div
#'
#' @return Div
#'
#' @importFrom rBTRdev Compute_gR2
#' @importFrom dplyr left_join group_by arrange mutate case_when filter count summarise select
#' @importFrom tidyr spread gather
#' @importFrom tibble remove_rownames column_to_rownames
#'
#' @param param parameters
#'

C_Div <- function( param ){

  params <- param |>
    dplyr::filter( modul %in% c( "division", "gR") ) |>
    dplyr::select(parameter,values) |>
    tibble::remove_rownames() |>
    tibble::column_to_rownames("parameter") |>
    t() |> as.data.frame( )

  rctaDivLimEW <- params$a2 *  seq(0,1,0.01) /params$maxRCTA
  rctaDivLimLW <- params$a2 *  seq(0,1,0.01) /(params$maxRCTA * params$RCTADivT)
  # rctaDivLim[ RCTA[ Today ]  == 99 ] <- waterDivLim ## ERROR CATCH mabey don't use it :20240803 修正后没有99

  EWdeltaD_T <- params$deltaD * ( 1 +  params$Div_alpha -
                                         exp( log(1/params$Div_alpha) * - rctaDivLimEW )  )

  LWdeltaD_T <- params$deltaD * ( 1 +  params$Div_alpha -
                                    exp( log(1/params$Div_alpha) * - rctaDivLimLW )  )

  ttRCTA <- data.frame(RCTAt = seq(0,1,0.01) , dD_EW = EWdeltaD_T,dD_LW = LWdeltaD_T  ) |>
    tidyr::gather(EL,dD,-1)


  ttczgR <- data.frame( gR = seq(0,1,0.05) ,
                        czgR = params$alpha_cz * exp( params$beta_cz * seq(0,1,0.05) ),
                        Vcz = params$va_cz * params$alpha_cz * exp( params$beta_cz * seq(0,1,0.05)) )

  return(list(ttRCTA = ttRCTA ,ttczgR=ttczgR ))
}

#' calculate Div
#'
#' @return Div
#'
#' @importFrom rBTRdev Compute_gR2
#' @importFrom dplyr left_join group_by arrange mutate case_when filter count summarise select
#' @importFrom tidyr spread gather
#' @importFrom tibble remove_rownames column_to_rownames
#'
#' @param param parameters
#'
#
# C_Li <- function( param, clims ){
#
#   gParam  <- param[param$modul == "gR",]
#   microclim <- clims |> dplyr::group_by(DOY) |>
#     dplyr::summarise( TEM = mean(TEM ,na.rm = T) ,
#                soilM = mean(soilM ,na.rm = T),
#                Ls = mean(Ls ,na.rm = T),
#                VPD = mean(VPD ,na.rm = T),
#                dL_i = mean(dL_i ,na.rm = T),
#                Ls = mean(Ls ,na.rm = T)) |>
#     rBTRdev:::Compute_gR2( gParam)
#
#
# }

#' calculate Litest
#'
#' @return Litest
#'
#' @importFrom dplyr left_join group_by arrange mutate case_when filter count summarise select
#' @importFrom tidyr spread gather
#' @importFrom tibble remove_rownames column_to_rownames
#' @importFrom mgcv gam
#'
#' @param microclim microclim
#' @param SimTrait SimTrait
#' @param ObsV ObsV
#' @param ObsF ObsF
#' @param InY InY
#'
Litest <- function ( microclim, SimTrait ,ObsV,ObsF,InY  ){
  SimClim <- microclim[  , c('Year', 'DOY','dL_i','L_i.fiber',"L_i.vessel"  )  ] |>
    dplyr::filter( Year == min( InY )  )
  SimTraitV <- SimTrait[, c('cell_L','Year', 'VCV','VDDOY','VEDOY', 'RRadDistR' )  ] |>
    dplyr::filter( !is.na(VCV) ) |> #dplyr::rename(RRadDistR = RRadDistRV) |>
    dplyr::mutate( DOY = ceiling( VEDOY +  (VDDOY-VEDOY)/2 ) )
  SimTraitF <- SimTrait[, c('cell_L','Year', 'CV','DDOY','EDOY','RRadDistR'  )  ] |>
    # dplyr::filter( CV!= 0 , Year %in% ResData$InY) |>
    dplyr::mutate( DOY = ceiling( EDOY +  (DDOY-EDOY)/2 ) )

  gamV <- mgcv::gam(DOY ~ s(RRadDistR), data = SimTraitV )
  ObsV <- ObsV[ ObsV$Year %in% InY ,c('Year','TID','LA','RRadDistR')]
  ObsV$DOY <- predict(gamV ,ObsV )

  gamF <- mgcv::gam(DOY ~  s(RRadDistR), data = SimTraitF )
  ObsF <- ObsF[ ObsF$Year %in% InY ,c('Year','TID','LA','RRadDistR')]
  ObsF$DOY <- predict(gamF ,ObsF )

  return( list( SimClim = SimClim ,  SimTraitV=SimTraitV ,SimTraitF = SimTraitF,
                ObsV = ObsV ,ObsF = ObsF ))

}


#' Regression Li parameters
#'
#' @return Li parameters
#'
#' @importFrom dplyr left_join group_by arrange mutate case_when filter count summarise select
#' @importFrom tidyr spread gather
#' @importFrom tibble remove_rownames column_to_rownames
#' @importFrom mgcv gam
#' @importFrom nls.multstart nls_multstart
#' @importFrom stats as.formula coef na.omit nls predict setNames
#' @importFrom utils head write.csv
#'
#' @param param parameters
#'
RegLi <- function(simclim , LineF , LineV , param ){

  params <- param |>
    dplyr::filter( modul %in% c( "gR"), paramtype == "fixed" ) |>
    dplyr::select(parameter,values) |>
    tibble::remove_rownames() |>
    tibble::column_to_rownames("parameter") |>
    t() |> as.data.frame( )

  ModL_i <- mgcv::gam(dL_i ~ s( DOY ),data =  simclim)

  LineF <-  dplyr::rename(LineF, DOYold = DOY, DOY = DOYnew,
                          OldL_i.fiber = L_i.fiber , L_i.fiber = NewL_i.fiber)
  LineV <-  dplyr::rename(LineV, DOYold = DOY, DOY = DOYnew,
                          OldL_i.vessel = L_i.vessel , L_i.vessel = NewL_i.vessel)

  LineF$dL_i <- predict(ModL_i , LineF )
  LineV$dL_i <- predict(ModL_i , LineV )

  # Pfiber <- nls.multstart::nls_multstart( L_i.fiber ~ a *exp( -exp(b - c * dL_i ))  ,
  #                                         data = LineF,
  #                                         iter = 500,
  #                                         start_lower = c( a= params$a.fiber *0.5, b= params$b.fiber *0.5 , c = params$c.fiber *0.5 ),
  #                                         start_upper = c( a= params$a.fiber *1.5, b= params$b.fiber *1.5 , c = params$c.fiber *1.5 ) ,
  #                                         # lower = c( a = -abs( max( as ) ), b = -abs( max( bs ) ) , c = 0 ),
  #                                         supp_errors = 'Y' )

  Pfiber <- tryCatch({
    nls( L_i.fiber ~ a *exp( -exp(b - c * dL_i ))  ,
                 data = LineF,
                 start =  list( a= params$a.fiber , b= params$b.fiber , c = params$c.fiber ) ) |>
      summary() |> coef()
  }, error = function(e){
    data.frame( param = c(params$a.fiber, params$b.fiber, params$c.fiber ) ,n = c('a','b','c')  )
  } , waring = function(w){
    data.frame( param = c(params$a.fiber, params$b.fiber, params$c.fiber ) ,n = c('a','b','c')  )
  } )

  Pvessel <- tryCatch(
    {nls( L_i.vessel ~ a *exp( -exp(b - c * dL_i ))  ,
                 data = LineV,
                 start =  list( a= params$a.vessel , b= params$b.vessel , c = params$c.vessel ) ) |>
        summary() |> coef()
    }, error = function(e){
      data.frame( param = c(params$a.fiber, params$b.fiber, params$c.fiber ) ,n = c('a','b','c')  )
    } , waring = function(w){
      data.frame( param = c(params$a.fiber, params$b.fiber, params$c.fiber ) ,n = c('a','b','c')  )
    } )

  # LineV$NewLi <- predict(Pvessel,LineV )

  NewP  <- data.frame( parameter = c( "a.fiber", "b.fiber", "c.fiber","a.vessel", "b.vessel", "c.vessel"),
                       Nvalues = c( Pfiber[1,1] ,Pfiber[2,1] ,Pfiber[3,1],
                                    Pvessel[1,1] ,Pvessel[2,1] ,Pvessel[3,1] )   )

  LineF$NewLi <- NewP[1,2] *exp( -exp(NewP[2,2] - NewP[3,2] * LineF$dL_i ) )
  LineV$NewLi <- NewP[4,2] *exp( -exp(NewP[5,2] - NewP[6,2] * LineF$dL_i ) )


  return( list(NewP = NewP ,LineF =LineF, LineV = LineV ) )
}

#' Cell Line simulate
#'
#' @return Cell traits line
#'
#' @export
#'
#' @importFrom dplyr left_join group_by arrange mutate case_when filter count summarise select
#' @importFrom tidyr spread gather separate
#' @importFrom tibble remove_rownames column_to_rownames
#' @importFrom mgcv gam
#' @importFrom stats as.formula coef na.omit nls predict setNames
#'
#' @param ObsV Obsdata vessel
#' @param ObsF Obsdata fiber
#'

CellLine <- function(ObsV,ObsF){

  cellline <- data.frame(RRadDistR=seq(0,100,1) )

  ObsFs <- ObsF |> dplyr::mutate(G2 = as.factor(paste(Year,TID,sep = "_") ) )
  ObsFs$Year  <- factor(ObsF$Year )

  ObsVs <- ObsV|> dplyr::mutate(G2 = as.factor(paste(Year,TID,sep = "_") ))
  ObsVs$Year  <- factor(ObsV$Year )

  ## total cell
  Mtfa <- mgcv::gam( LA ~ s(RRadDistR , k =4  )  ,  data = ObsF  )
  Mtfwt <- mgcv::gam( CWTall ~ s(RRadDistR , k =4  )  ,  data = ObsF  )
  Mtva <- mgcv::gam( LA ~ s(RRadDistR , k =4  )  ,  data = ObsV  )
  cellline$MtFa <- predict(Mtfa,cellline   )
  cellline$Mtfwt <- predict(Mtfwt,cellline   )
  cellline$Mtva <- predict(Mtva,cellline   )
  Mtfa2 <- mgcv::gam( LA ~ s(RRadDistR , k =4 ,by = Year  )  ,  data = ObsFs  )
  Mtfwt2 <- mgcv::gam( CWTall ~ s(RRadDistR , k =4  ,by = Year)  ,  data = ObsFs  )
  Mtva2 <- mgcv::gam( LA ~ s(RRadDistR , k =4  ,by = Year)  ,  data = ObsVs  )
  ## total & year
  Fys <- unique(ObsF$Year)
  Vys <- unique(ObsV$Year)
  FiberLine2 <-  data.frame(RRadDistR= rep(seq(0,100,1),length(Fys)) , Year= rep(Fys, each= 101   )   )
  FiberLine2$TYfa <- predict( Mtfa2, FiberLine2 )
  FiberLine2$TYfwt <- predict( Mtfwt2, FiberLine2 )

  VesselLine2 <-  data.frame(RRadDistR= rep(seq(0,100,1),length(Vys)) , Year= rep(Vys, each= 101   )   )
  VesselLine2$TYva <- predict( Mtva2, VesselLine2 )

  ## TID & year
  TIDF <- unique(ObsFs$G2)
  TIDV <- unique(ObsVs$G2)
  Mtfa3 <- mgcv::gam( LA ~ s(RRadDistR , k =3 ,by = G2 )  ,  data = ObsFs  )
  Mtfwt3 <- mgcv::gam( CWTall ~ s(RRadDistR , k =3 ,by =G2 )  ,  data = ObsFs  )
  Mtva3 <- mgcv::gam( LA ~ s(RRadDistR , k =3 ,by =G2)  ,  data = ObsVs  )
  FiberLine3 <-  data.frame(RRadDistR= rep(seq(0,100,1),length(TIDF)) , G2= rep(TIDF, each= 101   )   )
  VesselLine3 <-  data.frame(RRadDistR= rep(seq(0,100,1),length(TIDV)) , G2= rep(TIDV, each= 101   )   )
  FiberLine3$TYfa <- predict( Mtfa3, FiberLine3 )
  FiberLine3$TYfwt <- predict( Mtfwt3, FiberLine3 )
  VesselLine3$TYva <- predict( Mtva3, VesselLine3 )
  FiberLine3 <-tidyr::separate(FiberLine3, G2, c( "Year","TID"), sep = "_")
  VesselLine3 <-tidyr::separate(VesselLine3, G2, c( "Year","TID"), sep = "_")

  return( list( Cline = cellline, Fline2 = FiberLine2, Fline3 = FiberLine3,
                Vline2 = VesselLine2 , Vline3 = VesselLine3 ))

}



