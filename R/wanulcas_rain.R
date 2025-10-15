
get_rain_pars <- function() {
  RAIN_par <- list(
    RAIN_AnMemory = 2,
    RAIN_AType = 1,
    RAIN_BoundHeaLi = 25,
    RAIN_CoefVar3 = 0.05,
    RAIN_CoefVar4 = 0.05,
    RAIN_GenSeed = 300,
    RAIN_Gamma = 0.033621,
    RAIN_Heavy = 42,
    RAIN_HeavyP = .5,
    RAIN_Peakines_Season1 = 1,
    RAIN_Peakines_Season2 = 12,
    RAIN_PondFlwRt = 10,
    RAIN_PondStoreCp = 5,
    RAIN_Probability = 0.5,
    RAIN_Weibull_Param = 0.93,
    RAIN_WettestMonth_Season1 = 1,
    RAIN_WettestMonth_Season2 = 7,
    RAIN_YearStart = 0,
    RAIN_Shape_Max = 1.5,
    RAIN_Shape_Min = -0.5,
    RAIN_AnMemory = 2,
    RAIN_IntensCoefVar = 0.3,
    RAIN_IntensMean = 50,
    RAIN_IntercDripRt = 10,
    RAIN_IntMult = 3,
    RAIN_I_Initial_Value = 1,
    RAIN_Light = 9,
    RAIN_Max_IntDripDur = 0.5,
    RAIN_OffsetValue = -0.5,
    RAIN_Pattern1_Max = 0.06,
    RAIN_Pattern1_Min = -0.01,
    RAIN_MonthlyMean_RainfallMax = 333,
    RAIN_MonthlyMean_RainfallMin = 102,
    RAIN_Cycle = 1,
    RAIN_Multiplier = 1,
    RAIN_UniorBimodial = 2
    
  )
 
  RAIN_month_par <- data.frame(
    Months = c(1:12),
    DoY = c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
    RAIN_DaysIn_Order = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
    RAIN_Numberof_WetDaypM = c(29, 27, 30, 26, 26, 22, 22, 16, 20, 23, 26, 29),
    RAIN_CumDay1 = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
    RAIN_CumDay2 = c(364, 333, 305, 274, 244, 213, 183, 152, 121, 91, 60, 30),
    RAIN_RelWet_PersistencepM =
      c(1.01, 1, 1, 1.02, 1.07, 1.15, 1.19, 1.45, 1.27, 1.15, 1.08, 1.01),
    RAIN_MonthlyMean_TotalRainfall =
      c(334, 297, 321, 306, 208, 153, 103, 119, 163, 239, 273, 315)
  )
  
  RAIN_graph <- list(RAIN_MonthTot = data.frame(
    x = c(0, 33.2, 66.4, 99.5, 133, 166, 199, 232, 265, 299, 332, 365),
    y = c(240, 238, 204, 262, 188, 182, 195, 67, 148, 194, 290, 290)
  ),
  RAIN_DayProp = data.frame(
    x = c(0, 30.4, 60.8, 91.3, 122, 152, 183, 213, 243, 274, 304, 335, 365),
    y = rep(0.32, 13)
  ))
  
  
  RAIN_month_par$WetD_ratio <- RAIN_month_par$RAIN_Numberof_WetDaypM / RAIN_month_par$RAIN_DaysIn_Order
  RAIN_month_par$RAIN_MeanDailyon_WetDayspM <- ifelse(
    RAIN_month_par$RAIN_Numberof_WetDaypM == 0,
    0,
    RAIN_month_par$RAIN_MonthlyMean_TotalRainfall / RAIN_month_par$RAIN_Numberof_WetDaypM
  )
  

  # weather_df <- read.csv("data/weather.csv")
  
  # RAIN_df <- read.csv("data/rainfall.csv")
  # RAIN_df$date <- as.Date(RAIN_df$date, format = "%d-%b-%y")
  # RAIN_df$DoY <- yday(RAIN_df$date)
  # RAIN_df$time <- c(1:nrow(RAIN_df))
  
  return(list(
    RAIN_par = RAIN_par,
    RAIN_month_par = RAIN_month_par,
    RAIN_graph = RAIN_graph
    # RAIN_df = RAIN_df
  ))
}


get_rain <- function(time,
                     RAIN_par,
                     RAIN_month_par,
                     RAIN_graph,
                     RAIN_df,
                     RAIN_Yesterday_is,
                     CA_DOYStart = 1) {
  day <- time + CA_DOYStart
  # RAIN_DoY = IF (RAIN_AType=1 AND RAIN_Cycle?= 0) THEN (TIME+CA_DOYStart-365*RAIN_YearStart) ELSE
  # if (MOD(TIME+CA_DOYStart-365*RAIN_YearStart,365)) = 0 then 365 else (MOD(TIME+CA_DOYStart-365*RAIN_YearStart,366))
  if (RAIN_par$RAIN_AType == 1 &&
      RAIN_par$RAIN_Cycle == 0) {
    RAIN_DoY <- day - 365 * RAIN_par$RAIN_YearStart
  } else {
    if ((day - 365 * RAIN_par$RAIN_YearStart %% 365) == 0) {
      RAIN_DoY <- 365
    } else {
      RAIN_DoY <- day - 365 * RAIN_par$RAIN_YearStart %% 366
    }
  }
  
  # W_Irrigation_Data <- weather_df[weather_df$day == RAIN_DoY, ]$irrigation
  
  # Rain = IF(INT(RAIN_AType)=2)THEN (RAIN_Type2*RAIN_Multiplier)+W_Irrigation_Data else
  #   IF(INT(RAIN_AType)=4)THEN((MAX(0,RAIN_Today?*RAIN_fTable))*RAIN_Multiplier)+W_Irrigation_Data ELSE IF (INT(RAIN_AType)=3) THEN ((RAIN_fRandom*RAIN_Today?)*RAIN_Multiplier)+W_Irrigation_Data ELSE (RY152*RAIN_Multiplier)+W_Irrigation_Data
  # RAIN_IntercEvapCum(t) = RAIN_IntercEvapCum(t - dt) + (RAIN_InterceptEvapAvg) * dt
  if (RAIN_par$RAIN_AType == 2) {
    Rain <- get_simulated_rain(RAIN_DoY, RAIN_par, RAIN_month_par, RAIN_Yesterday_is) * RAIN_par$RAIN_Multiplier
    # RAIN_Yesterday_is <- RAIN_Type2$RAIN_Yesterday_is
  } else if (RAIN_par$RAIN_AType == 4) {
    Rain <- (
      get_monthly_avg_rain(RAIN_DoY, RAIN_par, RAIN_graph) * RAIN_par$RAIN_Multiplier
    )
  } else if (RAIN_par$RAIN_AType == 3) {
    Rain <- (get_random_rain(RAIN_DoY, RAIN_par, RAIN_graph) * RAIN_par$RAIN_Multiplier)
  } else {
    Rain <- (get_daily_rain(time, RAIN_df) * RAIN_par$RAIN_Multiplier)
  }
  return(Rain)
}

# Rain Type 1
get_daily_rain <- function(time, RAIN_df) {
#TODO: to be updated with the real rain data  
  # Rain_Data = GRAPH(Rain_DoY) never been used
  # RY152 = if time < 1460 or time = 1460 then RY14 else if time < 2920  or time = 2920 then RY58 else if time < 4380 or time = 4380 then RY912 else if time < 5840 or time = 5840 then RY1316 else if time < 7300 or time = 7300 then RY1720 else if time < 8760 or time = 8760 then RY2124 else if time < 10220 or time = 10220 then RY2528 else if time < 11680 or time = 11680 then RY2932 else if time < 13140 or time = 13140 then RY3336 else if time < 14600 or time = 14600 then RY3740 else if time < 16060 or time = 16060 then RY4144 else if time < 17520 or time = 17520 then RY4548 else RY4952
  if(time > 365) {
    time <- time %% 365 + 1
  }  
  RAIN_df[RAIN_df$time == time, ]$Rain_Data
}

# Rain Type 2
get_simulated_rain <- function(RAIN_DoY,
                               RAIN_par,
                               RAIN_month_par,
                               RAIN_Yesterday_is) {
  m_df <- RAIN_month_par
  RAIN_Random1 <- runif(1)
  RAIN_Random2 <- runif(1)
  
  RAIN_Pattern_MonthlyMean <- (RAIN_par$RAIN_MonthlyMean_RainfallMin / RAIN_par$RAIN_MonthlyMean_RainfallMax) *
    RAIN_par$RAIN_Shape_Max
  RAIN_PatternMonthlyMean_minusShapeMin <- RAIN_Pattern_MonthlyMean - RAIN_par$RAIN_Shape_Min
  
  RAIN_CumWet_Season1 <- m_df[m_df$Months == RAIN_par$RAIN_WettestMonth_Season1, ]$RAIN_CumDay1
  RAIN_CumWet_Season2 <- m_df[m_df$Months == RAIN_par$RAIN_WettestMonth_Season2, ]$RAIN_CumDay2
  # RAIN_Pattern2 = IF
  # RAIN_UniorBimodial?= 2
  # THEN
  # RAIN_Probability*
  #   MAX(RAIN_OffsetValue,
  #       (RAIN_Shape_Max-RAIN_Pattern1_Max+
  #          (1-RAIN_PatternMonthlyMean_minusShapeMin)*
  #          (SIN(22/7*(((RAIN_DoY-RAIN_CumWet_Season1)/182.5)+182.5/365))^RAIN_Peakines_Season1)-RAIN_OffsetValue)+
  #         (1-RAIN_Probability)*
  #         (RAIN_Pattern_MonthlyMean-RAIN_Pattern1_Min)*
  #         (1-RAIN_PatternMonthlyMean_minusShapeMin)*
  #         MAX(RAIN_OffsetValue,SIN(22/7*(((RAIN_DoY+RAIN_CumWet_Season2)/182.5)+182.5/365))^RAIN_Peakines_Season2-RAIN_OffsetValue))
  # ELSE
  # RAIN_Probability*
  #   MAX(RAIN_OffsetValue,
  #       (RAIN_Shape_Max-RAIN_Pattern1_Max+
  #          (1-RAIN_PatternMonthlyMean_minusShapeMin)*
  #          (SIN(22/7*(((RAIN_DoY-RAIN_CumWet_Season1)/182.5)+182.5/365))^RAIN_Peakines_Season1)-RAIN_OffsetValue))
  RAIN_season_f <- function(RAIN_CumWet_Season,
                            RAIN_Peakines_Season) {
    sin(22 / 7 * (((RAIN_DoY + RAIN_CumWet_Season) / 182.5
    ) + 182.5 / 365))^RAIN_Peakines_Season
  }
  
  RAIN_pattern_s1 <- RAIN_par$RAIN_Shape_Max - RAIN_par$RAIN_Pattern1_Max +
    (1 - RAIN_PatternMonthlyMean_minusShapeMin) *
    RAIN_season_f(RAIN_CumWet_Season1, RAIN_par$RAIN_Peakines_Season1) -
    RAIN_par$RAIN_OffsetValue
  
  RAIN_Pattern2 <- NULL
  if (RAIN_par$RAIN_UniorBimodial == 2) {
    RAIN_Pattern2 <- max(
      RAIN_par$RAIN_OffsetValue,
      RAIN_pattern_s1  +
        (1 - RAIN_par$RAIN_Probability) *
        (RAIN_Pattern_MonthlyMean - RAIN_par$RAIN_Pattern1_Min) *
        (1 - RAIN_PatternMonthlyMean_minusShapeMin) *
        max(
          RAIN_par$RAIN_OffsetValue,
          RAIN_season_f(RAIN_CumWet_Season2, RAIN_par$RAIN_Peakines_Season2) - RAIN_par$RAIN_OffsetValue
        )
    )
  } else {
    RAIN_Pattern2 <- RAIN_par$RAIN_Probability *
      max(RAIN_par$RAIN_OffsetValue, RAIN_pattern_s1)
  }
  # RAIN_Numberof_WetDaypD = if RAIN_DoY <= 31 then RAIN_Numberof_WetDaypM[january]else
  #   if RAIN_DoY <= 59 then RAIN_Numberof_WetDaypM[february] else
  
  imonth <- min(which(m_df$DoY >= RAIN_DoY, arr.ind = TRUE))
  RAIN_WetD_ratio <- m_df[m_df$Months == imonth, ]$WetD_ratio
  # RAIN_Wet_Fraction = (RAIN_Numberof_WetDaypD/RAIN_DaysIn_Order)*(RAIN_Pattern2/RAIN_Shape_Max)
  RAIN_Wet_Fraction <- RAIN_WetD_ratio * (RAIN_Pattern2 / RAIN_par$RAIN_Shape_Max)
  RAIN_RelWet_PersistencepD <- m_df[m_df$Months == imonth, ]$RAIN_RelWet_PersistencepM
  RAIN_MeanDailyon_WetDayspD <- m_df[m_df$Months == imonth, ]$RAIN_MeanDailyon_WetDayspM
  # RAIN_ProbabilityofDry = IF RAIN_Wet_Fraction=1 then 0 else (RAIN_Wet_Fraction*(1-RAIN_RelWet_PersistencepD*RAIN_Wet_Fraction)/(1-RAIN_Wet_Fraction))
  RAIN_ProbabilityofDry <- ifelse(
    RAIN_Wet_Fraction == 1,
    0,
    RAIN_Wet_Fraction * (1 - RAIN_RelWet_PersistencepD * RAIN_Wet_Fraction) /
      (1 - RAIN_Wet_Fraction)
  )
  # Rain? = IF (RAIN_Yesterday?=1 and RAIN_Random1<RAIN_Wet_Fraction*RAIN_RelWet_PersistencepD) or (RAIN_Yesterday?=0 and RAIN_Random1<RAIN_ProbabilityofDry) then 1 else 0
  is_Rain <- 0
  if ((RAIN_Yesterday_is == 1 &&
       RAIN_Random1 < RAIN_Wet_Fraction * RAIN_RelWet_PersistencepD) ||
      (RAIN_Yesterday_is == 0 &&
       RAIN_Random1 < RAIN_ProbabilityofDry)) {
    is_Rain <- 1
  }
  
  # if is_Rain == 1 then tommorow RAIN_Yesterday_is == 1, else 0 
  # RAIN_YesterdayF = -RAIN_Yesterday?+Rain?
  # RAIN_YesterdayF <- -RAIN_Yesterday_is + is_Rain
  # RAIN_Yesterday?(t) = RAIN_Yesterday?(t - dt) + (RAIN_YesterdayF) * dt
  # RAIN_Yesterday_is <- RAIN_Yesterday_is + RAIN_YesterdayF
  
  # RAIN_Type2 = if EXP(RAIN_Gamma)*(-LOGN(1-RAIN_Random2)^(1/RAIN_Weibull_Param))+1 > 0
  # then Rain?*(((RAIN_MeanDailyon_WetDayspD-1)/EXP(RAIN_Gamma))*(-LOGN(1-RAIN_Random2)^(1/RAIN_Weibull_Param))+1)
  # else 0
  RAIN_Type2 <- 0
  if (exp(RAIN_par$RAIN_Gamma) * ((-log(1 - RAIN_Random2))^(1 / RAIN_par$RAIN_Weibull_Param)) +
      1 > 0) {
    RAIN_Type2 <- is_Rain * (((RAIN_MeanDailyon_WetDayspD - 1) / exp(RAIN_par$RAIN_Gamma)
    ) * ((-log(1 - RAIN_Random2))^(1 / RAIN_par$RAIN_Weibull_Param)) + 1)
  }
  return(RAIN_Type2)
}

# Rain Type 3
get_random_rain <- function(RAIN_DoY, RAIN_par, RAIN_graph) {
  # RAIN_Today? = IF(Time=int(time)AND(Random(0,1,(RAIN_GenSeed+1))<RAIN_DayProp))THEN(1) ELSE(0)
  RAIN_DayProp <- get_graph_y(RAIN_graph$RAIN_DayProp, RAIN_DoY)
  RAIN_Today <- ifelse(runif(1) < RAIN_DayProp, 1, 0)
  # RAIN_fRandom = IF(Random(0,1,(RAIN_GenSeed+2))<RAIN_HeavyP)THEN(MAX(RAIN_BoundHeaLi,
  # NORMAL(RAIN_Heavy ,RAIN_CoefVar3*RAIN_Heavy,(RAIN_GenSeed+3))))ELSE (MIN(MAX(0.5,NORMAL(RAIN_Light,5,RAIN_GenSeed+3 )),RAIN_BoundHeaLi))
  RAIN_fRandom <- ifelse(
    runif(1) < RAIN_par$RAIN_HeavyP,
    max(
      RAIN_par$RAIN_BoundHeaLi,
      rnorm(
        1,
        RAIN_par$RAIN_Heavy ,
        RAIN_par$RAIN_CoefVar3 * RAIN_par$RAIN_Heavy
      )
    ),
    min(max(0.5, rnorm(
      1, RAIN_par$RAIN_Light, 5
    )), RAIN_par$RAIN_BoundHeaLi)
  )
  return(RAIN_fRandom * RAIN_Today)
}

# Rain Type 4
get_monthly_avg_rain <- function(RAIN_DoY, RAIN_par, RAIN_graph) {
  # RAIN_Today? = IF(Time=int(time)AND(Random(0,1,(RAIN_GenSeed+1))<RAIN_DayProp))THEN(1) ELSE(0)
  RAIN_DayProp <- get_graph_y(RAIN_graph$RAIN_DayProp, RAIN_DoY)
  RAIN_Today <- ifelse(runif(1) < RAIN_DayProp, 1, 0)
  
  RAIN_MonthTot <- get_graph_y(RAIN_graph$RAIN_MonthTot, RAIN_DoY)
  # RAIN_fTable = NORMAL(RAIN_MonthTot/(30*RAIN_DayProp),RAIN_CoefVar4*RAIN_MonthTot/(30*RAIN_DayProp),RAIN_GenSeed+3)
  RAIN_fTable <- rnorm(
    1,
    RAIN_MonthTot / (30 * RAIN_DayProp),
    RAIN_par$RAIN_CoefVar4 * RAIN_MonthTot / (30 * RAIN_DayProp)
  )
  return(max(0, RAIN_Today * RAIN_fTable))
}