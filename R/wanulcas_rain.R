
get_rain_pars <- function() {
  rain_par <- list(
    Rain_AnMemory = 2,
    Rain_AType = 1,
    Rain_BoundHeaLi = 25,
    Rain_CoefVar3 = 0.05,
    Rain_CoefVar4 = 0.05,
    Rain_GenSeed = 300,
    Rain_Gamma = 0.033621,
    Rain_Heavy = 42,
    Rain_HeavyP = .5,
    Rain_Peakines_Season1 = 1,
    Rain_Peakines_Season2 = 12,
    Rain_PondFlwRt = 10,
    Rain_PondStoreCp = 5,
    Rain_Probability = 0.5,
    Rain_Weibull_Param = 0.93,
    Rain_WettestMonth_Season1 = 1,
    Rain_WettestMonth_Season2 = 7,
    Rain_YearStart = 0,
    Rain_Shape_Max = 1.5,
    Rain_Shape_Min = -0.5,
    Rain_AnMemory = 2,
    Rain_IntensCoefVar = 0.3,
    Rain_IntensMean = 50,
    Rain_IntercDripRt = 10,
    Rain_IntMult = 3,
    Rain_I_Initial_Value = 1,
    Rain_Light = 9,
    Rain_Max_IntDripDur = 0.5,
    Rain_OffsetValue = -0.5,
    Rain_Pattern1_Max = 0.06,
    Rain_Pattern1_Min = -0.01,
    Rain_MonthlyMean_RainfallMax = 333,
    Rain_MonthlyMean_RainfallMin = 102,
    Rain_Cycle = 1,
    Rain_Multiplier = 1,
    Rain_UniorBimodial = 2
    
  )
 
  rain_month_par <- data.frame(
    Months = c(1:12),
    DoY = c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
    Rain_DaysIn_Order = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
    Rain_Numberof_WetDaypM = c(29, 27, 30, 26, 26, 22, 22, 16, 20, 23, 26, 29),
    Rain_CumDay1 = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
    Rain_CumDay2 = c(364, 333, 305, 274, 244, 213, 183, 152, 121, 91, 60, 30),
    Rain_RelWet_PersistencepM =
      c(1.01, 1, 1, 1.02, 1.07, 1.15, 1.19, 1.45, 1.27, 1.15, 1.08, 1.01),
    Rain_MonthlyMean_TotalRainfall =
      c(334, 297, 321, 306, 208, 153, 103, 119, 163, 239, 273, 315)
  )
  
  rain_graph <- list(Rain_MonthTot = data.frame(
    x = c(0, 33.2, 66.4, 99.5, 133, 166, 199, 232, 265, 299, 332, 365),
    y = c(240, 238, 204, 262, 188, 182, 195, 67, 148, 194, 290, 290)
  ),
  Rain_DayProp = data.frame(
    x = c(0, 30.4, 60.8, 91.3, 122, 152, 183, 213, 243, 274, 304, 335, 365),
    y = rep(0.32, 13)
  ))
  
  
  rain_month_par$WetD_ratio <- rain_month_par$Rain_Numberof_WetDaypM / rain_month_par$Rain_DaysIn_Order
  rain_month_par$Rain_MeanDailyon_WetDayspM <- ifelse(
    rain_month_par$Rain_Numberof_WetDaypM == 0,
    0,
    rain_month_par$Rain_MonthlyMean_TotalRainfall / rain_month_par$Rain_Numberof_WetDaypM
  )
  

  weather_df <- read.csv("data/weather.csv")
  
  rain_df <- read.csv("data/rainfall.csv")
  rain_df$date <- as.Date(rain_df$date, format = "%d-%b-%y")
  rain_df$DoY <- yday(rain_df$date)
  rain_df$time <- c(1:nrow(rain_df))
  
  return(list(
    rain_par = rain_par,
    rain_month_par = rain_month_par,
    rain_graph = rain_graph,
    rain_df = rain_df
  ))
}


get_rain <- function(time,
                     rain_par,
                     rain_month_par,
                     rain_graph,
                     rain_df,
                     Rain_Yesterday,
                     Ca_DOYStart = 1) {
  day <- time + Ca_DOYStart
  # Rain_DoY = IF (Rain_AType=1 AND Rain_Cycle?= 0) THEN (TIME+Ca_DOYStart-365*Rain_YearStart) ELSE
  # if (MOD(TIME+Ca_DOYStart-365*Rain_YearStart,365)) = 0 then 365 else (MOD(TIME+Ca_DOYStart-365*Rain_YearStart,366))
  if (rain_par$Rain_AType == 1 &&
      rain_par$Rain_Cycle == 0) {
    Rain_DoY <- day - 365 * rain_par$Rain_YearStart
  } else {
    if ((day - 365 * rain_par$Rain_YearStart %% 365) == 0) {
      Rain_DoY <- 365
    } else {
      Rain_DoY <- day - 365 * rain_par$Rain_YearStart %% 366
    }
  }
  
  # W_Irrigation_Data <- weather_df[weather_df$day == Rain_DoY, ]$irrigation
  
  # Rain = IF(INT(Rain_AType)=2)THEN (Rain_Type2*Rain_Multiplier)+W_Irrigation_Data else
  #   IF(INT(Rain_AType)=4)THEN((MAX(0,Rain_Today?*Rain_fTable))*Rain_Multiplier)+W_Irrigation_Data ELSE IF (INT(Rain_AType)=3) THEN ((Rain_fRandom*Rain_Today?)*Rain_Multiplier)+W_Irrigation_Data ELSE (RY152*Rain_Multiplier)+W_Irrigation_Data
  # Rain_IntercEvapCum(t) = Rain_IntercEvapCum(t - dt) + (Rain_InterceptEvapAvg) * dt
  if (rain_par$Rain_AType == 2) {
    Rain_Type2 <- get_simulated_rain(Rain_DoY, rain_par, rain_month_par, Rain_Yesterday)
    Rain <- (Rain_Type2$Rain * rain_par$Rain_Multiplier)
    Rain_Yesterday <- Rain_Type2$Rain_Yesterday
  } else if (rain_par$Rain_AType == 4) {
    Rain <- (
      get_monthly_avg_rain(Rain_DoY, rain_par, rain_graph) * rain_par$Rain_Multiplier
    )
  } else if (rain_par$Rain_AType == 3) {
    Rain <- (get_random_rain(Rain_DoY, rain_par, rain_graph) * rain_par$Rain_Multiplier)
  } else {
    Rain <- (get_daily_rain(time, rain_df) * rain_par$Rain_Multiplier)
  }
  return(list(Rain = Rain, Rain_Yesterday = Rain_Yesterday))
}

# Rain Type 1
get_daily_rain <- function(time, rain_df) {
  rain_df[rain_df$time == time, ]$rainfall
}

# Rain Type 2
get_simulated_rain <- function(Rain_DoY,
                               rain_par,
                               rain_month_par,
                               Rain_Yesterday) {
  m_df <- rain_month_par
  Rain_Random1 <- runif(1)
  Rain_Random2 <- runif(1)
  
  Rain_Pattern_MonthlyMean <- (rain_par$Rain_MonthlyMean_RainfallMin / rain_par$Rain_MonthlyMean_RainfallMax) *
    rain_par$Rain_Shape_Max
  Rain_PatternMonthlyMean_minusShapeMin <- Rain_Pattern_MonthlyMean - rain_par$Rain_Shape_Min
  
  Rain_CumWet_Season1 <- m_df[m_df$Months == rain_par$Rain_WettestMonth_Season1, ]$Rain_CumDay1
  Rain_CumWet_Season2 <- m_df[m_df$Months == rain_par$Rain_WettestMonth_Season2, ]$Rain_CumDay2
  # Rain_Pattern2 = IF
  # Rain_UniorBimodial?= 2
  # THEN
  # Rain_Probability*
  #   MAX(Rain_OffsetValue,
  #       (Rain_Shape_Max-Rain_Pattern1_Max+
  #          (1-Rain_PatternMonthlyMean_minusShapeMin)*
  #          (SIN(22/7*(((Rain_DoY-Rain_CumWet_Season1)/182.5)+182.5/365))^Rain_Peakines_Season1)-Rain_OffsetValue)+
  #         (1-Rain_Probability)*
  #         (Rain_Pattern_MonthlyMean-Rain_Pattern1_Min)*
  #         (1-Rain_PatternMonthlyMean_minusShapeMin)*
  #         MAX(Rain_OffsetValue,SIN(22/7*(((Rain_DoY+Rain_CumWet_Season2)/182.5)+182.5/365))^Rain_Peakines_Season2-Rain_OffsetValue))
  # ELSE
  # Rain_Probability*
  #   MAX(Rain_OffsetValue,
  #       (Rain_Shape_Max-Rain_Pattern1_Max+
  #          (1-Rain_PatternMonthlyMean_minusShapeMin)*
  #          (SIN(22/7*(((Rain_DoY-Rain_CumWet_Season1)/182.5)+182.5/365))^Rain_Peakines_Season1)-Rain_OffsetValue))
  rain_season_f <- function(Rain_CumWet_Season,
                            Rain_Peakines_Season) {
    sin(22 / 7 * (((Rain_DoY + Rain_CumWet_Season) / 182.5
    ) + 182.5 / 365))^Rain_Peakines_Season
  }
  
  rain_pattern_s1 <- rain_par$Rain_Shape_Max - rain_par$Rain_Pattern1_Max +
    (1 - Rain_PatternMonthlyMean_minusShapeMin) *
    rain_season_f(Rain_CumWet_Season1, rain_par$Rain_Peakines_Season1) -
    rain_par$Rain_OffsetValue
  
  Rain_Pattern2 <- NULL
  if (rain_par$Rain_UniorBimodial == 2) {
    Rain_Pattern2 <- max(
      rain_par$Rain_OffsetValue,
      rain_pattern_s1  +
        (1 - rain_par$Rain_Probability) *
        (Rain_Pattern_MonthlyMean - rain_par$Rain_Pattern1_Min) *
        (1 - Rain_PatternMonthlyMean_minusShapeMin) *
        max(
          rain_par$Rain_OffsetValue,
          rain_season_f(Rain_CumWet_Season2, rain_par$Rain_Peakines_Season2) - rain_par$Rain_OffsetValue
        )
    )
  } else {
    Rain_Pattern2 <- rain_par$Rain_Probability *
      max(rain_par$Rain_OffsetValue, rain_pattern_s1)
  }
  # Rain_Numberof_WetDaypD = if Rain_DoY <= 31 then Rain_Numberof_WetDaypM[january]else
  #   if Rain_DoY <= 59 then Rain_Numberof_WetDaypM[february] else
  
  imonth <- min(which(m_df$DoY >= Rain_DoY, arr.ind = TRUE))
  Rain_WetD_ratio <- m_df[m_df$Months == imonth, ]$WetD_ratio
  # Rain_Wet_Fraction = (Rain_Numberof_WetDaypD/Rain_DaysIn_Order)*(Rain_Pattern2/Rain_Shape_Max)
  Rain_Wet_Fraction <- Rain_WetD_ratio * (Rain_Pattern2 / rain_par$Rain_Shape_Max)
  Rain_RelWet_PersistencepD <- m_df[m_df$Months == imonth, ]$Rain_RelWet_PersistencepM
  Rain_MeanDailyon_WetDayspD <- m_df[m_df$Months == imonth, ]$Rain_MeanDailyon_WetDayspM
  # Rain_ProbabilityofDry = IF Rain_Wet_Fraction=1 then 0 else (Rain_Wet_Fraction*(1-Rain_RelWet_PersistencepD*Rain_Wet_Fraction)/(1-Rain_Wet_Fraction))
  Rain_ProbabilityofDry <- ifelse(
    Rain_Wet_Fraction == 1,
    0,
    Rain_Wet_Fraction * (1 - Rain_RelWet_PersistencepD * Rain_Wet_Fraction) /
      (1 - Rain_Wet_Fraction)
  )
  # Rain? = IF (Rain_Yesterday?=1 and Rain_Random1<Rain_Wet_Fraction*Rain_RelWet_PersistencepD) or (Rain_Yesterday?=0 and Rain_Random1<Rain_ProbabilityofDry) then 1 else 0
  is_Rain <- 0
  if ((Rain_Yesterday == 1 &&
       Rain_Random1 < Rain_Wet_Fraction * Rain_RelWet_PersistencepD) ||
      (Rain_Yesterday == 0 &&
       Rain_Random1 < Rain_ProbabilityofDry)) {
    is_Rain <- 1
  }
  Rain_YesterdayF <- -Rain_Yesterday + is_Rain
  Rain_Yesterday <- Rain_Yesterday + Rain_YesterdayF
  # Rain_Type2 = if
  # EXP(Rain_Gamma)*(-LOGN(1-Rain_Random2)^(1/Rain_Weibull_Param))+1 > 0
  # then
  # Rain?*(((Rain_MeanDailyon_WetDayspD-1)/EXP(Rain_Gamma))*(-LOGN(1-Rain_Random2)^(1/Rain_Weibull_Param))+1)
  # else
  #   0
  Rain_Type2 <- 0
  if (exp(rain_par$Rain_Gamma) * ((-log(1 - Rain_Random2))^(1 / rain_par$Rain_Weibull_Param)) +
      1 > 0) {
    Rain_Type2 <- is_Rain * (((Rain_MeanDailyon_WetDayspD - 1) / exp(rain_par$Rain_Gamma)
    ) * ((-log(1 - Rain_Random2))^(1 / rain_par$Rain_Weibull_Param)) + 1)
  }
  return(list(Rain = Rain_Type2, Rain_Yesterday = Rain_Yesterday))
}

# Rain Type 3
get_random_rain <- function(Rain_DoY, rain_par, rain_graph) {
  # Rain_Today? = IF(Time=int(time)AND(Random(0,1,(Rain_GenSeed+1))<Rain_DayProp))THEN(1) ELSE(0)
  Rain_DayProp <- get_graph_y(rain_graph$Rain_DayProp, Rain_DoY)
  Rain_Today <- ifelse(runif(1) < Rain_DayProp, 1, 0)
  # Rain_fRandom = IF(Random(0,1,(Rain_GenSeed+2))<Rain_HeavyP)THEN(MAX(Rain_BoundHeaLi,
  # NORMAL(Rain_Heavy ,Rain_CoefVar3*Rain_Heavy,(Rain_GenSeed+3))))ELSE (MIN(MAX(0.5,NORMAL(Rain_Light,5,Rain_GenSeed+3 )),Rain_BoundHeaLi))
  Rain_fRandom <- ifelse(
    runif(1) < rain_par$Rain_HeavyP,
    max(
      rain_par$Rain_BoundHeaLi,
      rnorm(
        1,
        rain_par$Rain_Heavy ,
        rain_par$Rain_CoefVar3 * rain_par$Rain_Heavy
      )
    ),
    min(max(0.5, rnorm(
      1, rain_par$Rain_Light, 5
    )), rain_par$Rain_BoundHeaLi)
  )
  return(Rain_fRandom * Rain_Today)
}

# Rain Type 4
get_monthly_avg_rain <- function(Rain_DoY, rain_par, rain_graph) {
  # Rain_Today? = IF(Time=int(time)AND(Random(0,1,(Rain_GenSeed+1))<Rain_DayProp))THEN(1) ELSE(0)
  Rain_DayProp <- get_graph_y(rain_graph$Rain_DayProp, Rain_DoY)
  Rain_Today <- ifelse(runif(1) < Rain_DayProp, 1, 0)
  
  Rain_MonthTot <- get_graph_y(rain_graph$Rain_MonthTot, Rain_DoY)
  # Rain_fTable = NORMAL(Rain_MonthTot/(30*Rain_DayProp),Rain_CoefVar4*Rain_MonthTot/(30*Rain_DayProp),Rain_GenSeed+3)
  Rain_fTable <- rnorm(
    1,
    Rain_MonthTot / (30 * Rain_DayProp),
    rain_par$Rain_CoefVar4 * Rain_MonthTot / (30 * Rain_DayProp)
  )
  return(max(0, Rain_Today * Rain_fTable))
}