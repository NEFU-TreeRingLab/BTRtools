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
RegData <- function( dt , Nage , Nrw, Nla,
                     rwA, rwB, rwC, rwK, laA, laB, laC, laK ){

   dtOri <- dt |>
    dplyr::select( all_of(c("Year", Nage, Nrw, Nla ) ) ) |>
    na.omit()
  names(dtOri) <- c("Year", "Age","MRW", "MaxLA")

  ifelse(is.na(rwA), rwA <- max(dtOri$MRW ),NA  )
  ifelse(is.na(rwC), rwC <- min(dtOri$MRW )*0.8,NA  )
  ifelse(is.na(rwB), rwB <- (rwA - rwC)/nrow(dtOri) ,NA  )

  ifelse(is.na(laA), laA <- max(dtOri$MaxLA ),NA  )
  ifelse(is.na(laC), laC <- min(dtOri$MaxLA )*0.8,NA  )
  ifelse(is.na(laB), laB <- (laA - laC)/nrow(dtOri) ,NA  )



  dtOri <- dtOri |>
    dplyr::mutate(pGamRw=0, pExpRw=0,pGamLa=0,pExpLa=0,
                  mExpRw = rwA*exp(rwB * Age ) + rwC,
                  mExpLa = laA*exp(laB * Age ) + laC )

  ExpResRW <- tryCatch(
    { modEXP = modelEXP(dt = dtOri,colx = "Age",coly =  "MRW" , ## 运行modelEXP函数
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
    { modEXP = modelEXP(dt = dtOri,colx = "Age",coly =  "MaxLA" ,
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

  modGamRw <- mgcv::gam( MRW ~ s( Age , k = rwK ) ,data = dtOri  )
  dtOri$pGamRw <- predict( modGamRw , dtOri )

  modGamLa <- mgcv::gam( MaxLA ~ s( Age , k = rwK ) ,data = dtOri  )
  dtOri$pGamLa<- predict( modGamLa , dtOri )

  dtNor <- dtOri |> dplyr::mutate( pGamRw = nors(pGamRw),  pExpRw = nors(pExpRw),
                            pGamLa = nors(pGamLa),  pExpLa = nors(pExpLa),
                            mExpRw = nors(mExpRw),  mExpLa = nors(mExpRw) )
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
       'deltaVN',	'Vcz',	'grwothSeason',	'Age',	'T_age',	'czgR',	'egR' )

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
#'
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


