# WANULCAS MODULE ####################
#
# evapotranspiration:
# Protocol: SFTP | Host: sftp://hydras.ugent.be | Port: 2225 | Username: gleamuser | Password: GLEAM4#h-cel_924
#

# Crop Data
# https://dssat.net/
#
# Soil Data
# https://app.soilhive.ag/availability

# TO CHECK?:
#
# line 11613: W_V3Drain[Zone] =
# * missing 0
#
# W_Drain1 loop variable?
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(lubridate)
library(openxlsx2)
library(yaml)

options("warnPartialMatchDollar" = TRUE)

source("utils.R")
source("wanulcas_rain.R")

maxval <- 1e+308

get_graph_y <- function(graph_df,
                        x,
                        x_column = NULL,
                        y_column = NULL,
                        mode = "continues") {
  x <- as.numeric(x)
  if (!is.null(x_column)) {
    if (!x_column %in% names(graph_df)) {
      return(NA)
    }
    xc <- graph_df[x_column]
  } else {
    xc <- graph_df[1]
  }
  
  if (!is.null(y_column)) {
    if (!y_column %in% names(graph_df)) {
      return(NA)
    }
    yc <- graph_df[y_column]
    graph_df <- cbind(xc, yc)
  }
  if (!is.numeric(x) || ncol(graph_df) < 2) {
    return(NA)
  }
  suppressWarnings(i <- max(which(graph_df[1] < x, arr.ind = TRUE)))
  if (!is.finite(i)) {
    return(graph_df[1, 2])
  }
  if (i == nrow(graph_df)) {
    return(graph_df[i, 2])
  }
  if (mode == "continues") {
    graph_df[i, 2] + (x - graph_df[i, 1]) * (graph_df[i + 1, 2] - graph_df[i, 2]) /
      (graph_df[i + 1, 1] - graph_df[i, 1])
  } else {
    graph_df[i, 2]
  }
}

get_var <- function(df, var, colnames) {
  # df[df$vars == var, colname]
  unlist(lapply(colnames, function(x) {
    # get_var(plant_data$tree_pars_df, "T_LAIMax", x)
    df[df$vars == var, x]
  }))
}

# get_graph_y(plant_data$Cq_LWR_df, 0, y_column = "Maize", mode = "continues")

order_soil <- function(df) {
  df[order(df$layer, df$zone), ]
}

as_var_list <- function(df) {
  l <- as.list(df[[2]])
  names(l) <- df[[1]]
  return(l)
}

to_df <- function(x, is_print = F) {
  n <- unlist(regmatches(x, gregexpr("[0-9.]+", x)))
  n <- as.numeric(n)
  ix <- seq(1, length(n), 2)
  iy <- seq(2, length(n), 2)
  if(is_print) {
    print(paste0("c(", paste(n[ix], collapse = ","), ")"))
    print(paste0("c(", paste(n[iy], collapse = ","), ")"))
  }
  data.frame(x = n[ix], y = n[iy])
}

delay_timer <- list()
delay_value <- list()

#' Adopted 'Delay' function from STELLA:
#' https://www.iseesystems.com/resources/help/v3/Content/08-Reference/07-Builtins/Delay_builtins.htm
#'
#' The function will return the input value after the delay_duration counting call.
#' Otherwise it will return the default value (within the duration call)
#'
#' @param input
#' @param delay_duration
#' @param default_val
#' @param name
#'
#' @returns the input value after delay counting calls, otherwise returning the default value
#' @export
#'
#' @examples
delay <- function(input,
                  delay_duration,
                  default_val = NA,
                  name = deparse(substitute(input))) {
  # 'name' is retrieving the variable name passed to the 'input' parameter and use it as identical timing variable
  if (is.numeric(input)) {
    name <- "default"
  }
  
  if (is.null(delay_timer[[name]])) {
    if (input != default_val) {
      delay_timer[[name]] <<- delay_duration
      delay_value[[name]] <<- input
    }
    return(default_val)
  }
  
  delay_timer[[name]] <<- delay_timer[[name]] - 1
  
  if (input != default_val) {
    delay_timer[[name]] <<- c(delay_timer[[name]], delay_duration)
    delay_value[[name]] <<- c(delay_value[[name]], input)
  }
  
  if (delay_timer[[name]][1] <= 0) {
    v <- delay_value[[name]][1]
    delay_timer[[name]] <<- delay_timer[[name]][-1]
    delay_value[[name]] <<- delay_value[[name]][-1]
    if (length(delay_timer[[name]]) == 0) {
      delay_timer[[name]] <<- NULL
      delay_value[[name]] <<- NULL
    }
    return(v)
  } else {
    return(default_val)
  }
}

### Graph Data #############

wdata_file <- read_yaml("wanulcas_data.yaml")

wdata <- lapply(wdata_file, function(x) {
  if(length(x$y_col) == 1) {
    df <- to_df(x$data)
    names(df) <- c(x$x_col, x$y_col)
  } else {
    df <- to_df(x$data[1])
    for(i in 2:length(x$data)) {
      idf <- to_df(x$data[i])
      df <- cbind(df, idf[2])
    }
    names(df) <- c(x$x_col, x$y_col)
  }
  return(df)
})

names(wdata) <- unlist(lapply(wdata_file, function(x) {
  if(length(x$y_col) == 1) {
    x$y_col
  } else {
    v <- unlist(strsplit(x$y_col[1], "_", fixed = T))
    paste(head(v, -1), collapse = "_")
  }
}))

get_y_df <- function(x,
                     data_name,
                     x_column = NULL,
                     y_column = NULL) {
  df <- wdata[[data_name]]
  cols <- names(df)
  if (is.null(x_column))
    x_column <- cols[1]
  if (is.null(y_column))
    y_column <- cols[-1]
  if (length(y_column) == 1) {
    unlist(lapply(x, function(ix) {
      get_graph_y(df,
                  ix,
                  x_column = x_column,
                  y_column = y_column,
                  mode = "continues")
    }))
  } else {
    ylist <- lapply(x, function(ix) {
      unlist(lapply(y_column, function(iy) {
        get_graph_y(
          df,
          ix,
          x_column = x_column,
          y_column = iy,
          mode = "continues"
        )
      }))
    })
    if(length(ylist) == 1) {
      unlist(ylist)
    } else {
      ylist
    }
  }
}

# get_y_df(c(2, 3),wdata$Ca_ExtOrgApply_is)

# # Generate initial soil data structure as dataframe
# init_soil_df <- function() {
#   #TODO: validate parameter here
#   # nzone <- max(zone_df$zone)
#   # nlayer <- max(layer_df$layer)
#   # zone <- rep(c(1:nzone), nlayer)
#   # layer <- rep(c(1:nlayer), each = nzone)
#   # initiate soil df
#   # zonelayer_df <- data.frame(zone, layer)
#   zonelayer_df$AF_ZoneFrac <- rep(zone_df$AF_ZoneFrac, nlayer)
#
#   # soil zones
#   AF_ZoneFrac_1 <- zone_df$AF_ZoneFrac[-1]
#   AF_ZoneFrac_1 <- c(AF_ZoneFrac_1, zone_df$AF_ZoneFrac[1])
#   zone_df$AF_LatInFlowRatio <- AF_ZoneFrac_1 / zone_df$AF_ZoneFrac
#
#   # AF_Depth1[Zone] = AF_DepthLay1
#   # AF_Depth2[Zone] = AF_DepthLay2*(1-AF_StoneFrac[Zone,2])
#   # AF_Depth3[Zone] = AF_DepthLay3*(1-AF_StoneFrac[Zone,3])
#   # AF_Depth4[Zone] = AF_DepthLay4*(1-AF_StoneFrac[Zone,4])
#   zonelayer_df$AF_DepthLay <- rep(layer_df$AF_DepthLay, each = nzone)
#   zonelayer_df$AF_Depth <- zonelayer_df$AF_DepthLay * (1 - zonelayer_df$AF_StoneFrac)
#
#   # initiate the top layer
#   zone_df$AF_SlopeCurr <- zone_df$AF_SlopeSurfInit
#   zone_df$AF_DepthLay1 <- layer_df[layer_df$layer == 1, ]$AF_DepthLay
#
#   # AF_DepthSlope1[Zn1] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else
#   #   (AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn1]/100))))
#
#   # AF_DepthSlope1[Zn1] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else (AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn1]/100))))
#   # AF_DepthSlope1[Zn2] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else mAX((AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]-AF_ZoneFrac[Zn2]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn2]/100)))),0.0001)
#   # AF_DepthSlope1[Zn3] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else max((AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]-AF_ZoneFrac[Zn2]-AF_ZoneFrac[Zn3]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn3]/100)))),0.0001)
#   # AF_DepthSlope1[Zn4] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else max((AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]-AF_ZoneFrac[Zn2]-AF_ZoneFrac[Zn3]-AF_ZoneFrac[Zn4]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn4]/100)))),0.0001)
#   zone_df$AF_DepthDynamic_is <-  pars$AF_par$AF_DepthDynamic_is
#   zone_df$AF_SlopeSoilHoriz <- pars$AF_par$AF_SlopeSoilHoriz
#   zone_df$AF_DepthSlope1_a <- zone_df$AF_DepthLay1 / cos(atan(zone_df$AF_SlopeSoilHoriz / 100))
#   zone_df$AF_DepthSlope1_b <- tan(atan(zone_df$AF_SlopeSoilHoriz / 100)) - tan(atan(zone_df$AF_SlopeCurr / 100))
#   z1 <- zone_df[zone_df$zone == 1, ]
#   z2 <- zone_df[zone_df$zone == 2, ]
#   z3 <- zone_df[zone_df$zone == 3, ]
#   z4 <- zone_df[zone_df$zone == 4, ]
#   zone_df$AF_DepthSlope1 <- zone_df$AF_DepthLay1
#   zone_df[zone_df$zone == 1, ]$AF_DepthSlope1 <- ifelse(
# z1$AF_DepthDynamic_is == 0,
# z1$AF_DepthLay1,
# z1$AF_DepthSlope1_a + (0.5 - z1$AF_ZoneFrac / 2) * z1$AF_DepthSlope1_b
#   )
#   zone_df[zone_df$zone == 2, ]$AF_DepthSlope1 <- ifelse(
# z2$AF_DepthDynamic_is == 0,
# z2$AF_DepthLay1,
# pmax(z2$AF_DepthSlope1_a + (0.5 - z1$AF_ZoneFrac - z2$AF_ZoneFrac / 2) * z2$AF_DepthSlope1_b, 0.0001)
#   )
#   zone_df[zone_df$zone == 3, ]$AF_DepthSlope1 <- ifelse(
# z3$AF_DepthDynamic_is == 0,
# z3$AF_DepthLay1,
# pmax(z3$AF_DepthSlope1_a + (0.5 - z1$AF_ZoneFrac - z2$AF_ZoneFrac- z3$AF_ZoneFrac / 2) * z3$AF_DepthSlope1_b, 0.0001)
#   )
#   zone_df[zone_df$zone == 4, ]$AF_DepthSlope1 <- ifelse(
# z4$AF_DepthDynamic_is == 0,
# z4$AF_DepthLay1,
# pmax(z4$AF_DepthSlope1_a + (0.5 - z1$AF_ZoneFrac - z2$AF_ZoneFrac- z3$AF_ZoneFrac- z4$AF_ZoneFrac / 2) * z4$AF_DepthSlope1_b, 0.0001)
#   )
#
#   # AF_DepthAct1[Zone] = IF(AF_SlopeCurr[Zone]<>AF_SlopeSurfInit)THEN(AF_Depth1[Zone])*(1-AF_StoneFrac[Zone,1])
#   # ELSE(AF_DepthSlope1[Zone])*(1-AF_StoneFrac[Zone,1])
#   zone_df$AF_DepthAct1 <- ifelse(
# zone_df$AF_SlopeCurr != zone_df$AF_SlopeSurfInit,
# zonelayer_df[zonelayer_df$layer == 1,]$AF_Depth * (1 - zonelayer_df[zonelayer_df$layer == 1,]$AF_StoneFrac),
# zone_df$AF_DepthSlope1 * (1 - zonelayer_df[zonelayer_df$layer == 1,]$AF_StoneFrac)
#   )
#   # Layer 1 AF_Depth is equal to AF_DepthAct1
#   zonelayer_df$AF_Depth_original <- zonelayer_df$AF_Depth
#   zonelayer_df[layer_df$layer == 1, ]$AF_Depth <- zone_df$AF_DepthAct1
#
#
#   # INIT S_BDActOverBDRefInfiltr[Zone] = (0.69-SQRT(-0.69^2-4*-0.52*(1.21-(LOG10(S_RelSurfInfiltrInit[Zone])))))/(2*-0.52)
#   zone_df$S_BDActOverBDRefInfiltr <- (0.69 - sqrt((-0.69)^2 - 4 * (-0.52 *
#(
#1.21 -
#  log10(zone_df$S_RelSurfInfiltrInit)
#)))) / (2 * (-0.52))
#
#
#   ## initiate water stock ####
#   # df$W_ThetaInit <- rep(layer_df$W_ThetaInit, each = nzone)
#   # df$W_BDLayer <- rep(layer_df$W_BDLayer, each = nzone)
#   # df$S_KsatInitV <- rep(layer_df$Ksat, each = nzone) * 10
#   # df$S_KsatDefV <- rep(layer_df$KSatD, each = nzone) * 10
#   # df$S_KSatHperV <- 1
#   # df$W_FieldCapKcrit <- rep(layer_df$FieldC, each = nzone)
#
#   # S_KsatInitV1[Zone] = S_SoilProp[Ksat1]*10
#   # S_KsatInitV2[Zone] = S_SoilProp[Ksat2]*10
#   # S_KsatInitV3[Zone] = S_SoilProp[Ksat3]*10
#   # S_KsatInitV4[Zone] = S_SoilProp[Ksat4]*10
#   zonelayer_df$S_KsatInitV <- rep(layer_df$Ksat, each = nzone )* 10
#   # S_KsatDefV1[Zone] = S_SoilProp[KSatD1]*10
#   # S_KsatDefV2[Zone] = S_SoilProp[KsatD2]*10
#   # S_KsatDefV3[Zone] = S_SoilProp[KsatD3]*10
#   # S_KsatDefV4[Zone] = S_SoilProp[KsatD4]*10
#   zonelayer_df$S_KsatDefV <- rep(layer_df$KSatD, each = nzone ) * 10
#   # W_FieldCapKcrit1[Zone] = S_SoilProp[FieldC1]
#   # W_FieldCapKcrit2[Zone] = S_SoilProp[FieldC2]
#   # W_FieldCapKcrit3[Zone] = S_SoilProp[FieldC3]
#   # W_FieldCapKcrit4[Zone] = S_SoilProp[FieldC4]
#   zonelayer_df$W_FieldCapKcrit <- layer_df$FieldC
#
#
#   ### START DYNAMIC? #####################
#
#   # TODO: this is dynamic variable
#   # INIT S_BDActOverBDRefKsatV1[Zone] = (0.69-SQRT(-0.69^2-4*-0.52*(1.21-(LOG10(S_KsatInitV1[Zone]/S_KsatDefV1[Zone])))))/(2*-0.52)
#   zonelayer_df$S_BDActOverBDRefKsatV <- (0.69 - sqrt(-0.69^2 - 4 * -0.52 * (1.21 -
#   (
# log10(zonelayer_df$S_KsatInitV / zonelayer_df$S_KsatDefV)
#   )))) / (2 * -0.52)
#
#   # S_KsatV1Act[Zone] = S_KsatDefV1[Zone]*10^((-0.52*(S_BDActOverBDRefKsatV1[Zone]^2)-0.69*S_BDActOverBDRefKsatV1[Zone]+1.21))
#   zonelayer_df$S_KsatVAct <- zonelayer_df$S_KsatDefV * 10^((
# -0.52 * (zonelayer_df$S_BDActOverBDRefKsatV^2) - 0.69 * zonelayer_df$S_BDActOverBDRefKsatV +
# 1.21
#   ))
#   # W_KSatH1[Zone] = S_KsatV1Act[Zone]*S_KSatHperV1[Zone]
#   # W_KSatH2[Zone] = S_KsatV2Act[Zone]*S_KsatHperV2[Zone]
#   # W_KSatH3[Zone] = S_KsatV3Act[Zone]*S_KsatHperV3[Zone]
#   # W_KSatH4[Zone] = S_KsatV4Act[Zone]*S_KsatHperV4[Zone]
#   zonelayer_df$W_KSatH <- zonelayer_df$S_KsatVAct * zonelayer_df$S_KSatHperV
#
#   # S_RelBD[Zn1,1] = S_BDActOverBDRefKsatV1[Zn1]+0*(S_BDActOverBDRefKsatV1[Zn1]+S_BDActOverBDRefKsatV2[Zn1]+S_BDActOverBDRefKsatV3[Zn1]+S_BDActOverBDRefKsatV4[Zn1])
#   zonelayer_df$S_RelBD <- zonelayer_df$S_BDActOverBDRefKsatV
#
#   zonelayer_df$W_BDLayer <- rep(layer_df$W_BDLayer, each = nzone)
#   # W_PoreVol[Zone,SoilLayer] = (1-W_BDLayer[SoilLayer]*S_RelBD[Zone,SoilLayer]/2.5)
#   zonelayer_df$W_PoreVol <- (1 - zonelayer_df$W_BDLayer * zonelayer_df$S_RelBD / 2.5)
#   # W_ThetaI1[Zone] = W_ThetaInit1[Zone]*W_PoreVol[Zone,1]
#   zonelayer_df$W_ThetaI <- zonelayer_df$W_ThetaInit * zonelayer_df$W_PoreVol
#
#   # INIT W_Stock1[Zone] = W_ThetaI1[Zone]*AF_DepthAct1[Zone]*1000
#   zonelayer_df$W_Stock <- zonelayer_df$W_ThetaI * zonelayer_df$AF_Depth * 1000
#
#   # layer height above ground water table (cm)
#   # INIT AF_HGW1[Zone] = (.5*AF_Depth1[Zone]+AF_Depth2[Zone]+AF_Depth3[Zone]+AF_Depth4[Zone] + AF_DepthGroundWater_Table  )*100
#   # INIT AF_HGW2[Zone] = (.5*AF_Depth2[Zone]+AF_Depth3[Zone]+AF_Depth4[Zone] + AF_DepthGroundWater_Table  )*100
#   # INIT AF_HGW3[Zone] = (.5*AF_Depth3[Zone]+AF_Depth4[Zone] + AF_DepthGroundWater_Table  )*100
#   # INIT AF_HGW4[Zone] = (.5*AF_Depth4[Zone] + AF_DepthGroundWater_Table  )*100
#   l1 <- zonelayer_df[zonelayer_df$layer == 1, ]
#   l2 <- zonelayer_df[zonelayer_df$layer == 2, ]
#   l3 <- zonelayer_df[zonelayer_df$layer == 3, ]
#   l4 <- zonelayer_df[zonelayer_df$layer == 4, ]
#
#   zonelayer_df$AF_HGW <- NA
#   zonelayer_df[zonelayer_df$layer == 1, ]$AF_HGW <- (
# 0.5 * l1$AF_Depth_original + l2$AF_Depth + l3$AF_Depth + l4$AF_Depth +  pars$AF_par$AF_DepthGroundWater_Table
#   ) * 100
#   zonelayer_df[zonelayer_df$layer == 2, ]$AF_HGW <- (0.5 * l2$AF_Depth + l3$AF_Depth +
#  l4$AF_Depth +  pars$AF_par$AF_DepthGroundWater_Table) * 100
#   zonelayer_df[zonelayer_df$layer == 3, ]$AF_HGW <- (0.5 * l3$AF_Depth +
#  l4$AF_Depth +  pars$AF_par$AF_DepthGroundWater_Table) * 100
#   zonelayer_df[zonelayer_df$layer == 4, ]$AF_HGW <- (0.5 * l4$AF_Depth +  pars$AF_par$AF_DepthGroundWater_Table) * 100
#
#
#
#   zonelayer_df$W_ThetaP <- NA
#   for (i in c(1:nlayer)) {
# gr_df <- w_theta_df[c(1, i + 1)]
# AF_HGW <- zonelayer_df[zonelayer_df$layer == i & zonelayer_df$zone == 1, ]$AF_HGW
# zonelayer_df[zonelayer_df$layer == i, ]$W_ThetaP <- get_graph_y(gr_df, x = -AF_HGW, mode = "continues")
#   }
#
#   # LF_V1MaxDailyFlow[Zone] = (AF_DepthAct1[Zone]*S_KsatV1Act[Zone]+AF_Depth2[Zone]*S_KsatV2Act[Zone])/(AF_DepthAct1[Zone]+AF_Depth2[Zone])
#   # LF_V2MaxDailyFlow[Zone] = (AF_Depth2[Zone]*S_KsatV2Act[Zone]+AF_Depth3[Zone]*S_KsatV3Act[Zone])/(AF_Depth2[Zone]+AF_Depth3[Zone])
#   # LF_V3MaxDailyFlow[Zone] = (AF_Depth3[Zone]*S_KsatV3Act[Zone]+AF_Depth4[Zone]*S_KsatV4Act[Zone])/(AF_Depth3[Zone]+AF_Depth4[Zone])
#   # LF_V4MaxDailyFlow[Zone] = (AF_Depth4[Zone]*S_KsatV4Act[Zone]+AF_DeepSubSoil*S_KsatVDeepSub)/(AF_Depth4[Zone]+AF_DeepSubSoil)
#   zonelayer_df$AF_Depth_down <- c(zonelayer_df[zonelayer_df$layer %in% c(2:4),]$AF_Depth, rep(pars$AF_par$AF_DeepSubSoil, nzone))
#   zonelayer_df$S_KsatVAct_down <- c(zonelayer_df[zonelayer_df$layer %in% c(2:4),]$S_KsatVAct, rep(pars$S_par$S_KsatVDeepSub, nzone))
#
#   zonelayer_df$LF_VMaxDailyFlow <-  (
# zonelayer_df$AF_Depth * zonelayer_df$S_KsatVAct  + zonelayer_df$AF_Depth_down * zonelayer_df$S_KsatVAct_down
#   ) / (zonelayer_df$AF_Depth + zonelayer_df$AF_Depth_down)
#
#
#
#   # INIT N_Stock1[Zone,SlNut] = N_Init1[Zone,SlNut]*AF_DepthAct1[Zone]*1000
#
#   # return(
#   #   list(
#   # zonelayer_df = zonelayer_df,
#   # zone_df = zone_df,
#   # layer_df = layer_df,
#   # nzone = nzone,
#   # nlayer = nlayer
#   #   )
#   # )
# }

AF_Unit <- c(
  "Parkland?",
  "AFTotZn",
  "AFZn1",
  "AFZn2",
  "AFZn3",
  "TSp1",
  "TSp2",
  "TSp3",
  "TRelSp1",
  "TRelSp2",
  "TRelSp3",
  "TDensSp1",
  "TDensSp2",
  "TDensSp3",
  "SL1",
  "SL2",
  "SL3",
  "SL4"
)

Cq_Unit <- c(
  "TimeGen",
  "TimeVeg",
  "SingleCycle?",
  "FlwBegin",
  "FlwEnd",
  "GroMax",
  "SeedInit",
  "TranspRatio",
  "HBiomConv",
  "MaxRemob",
  "kLight",
  "RelLight",
  "LAIMax",
  "RainStorCap",
  "Lp",
  "AlphMax",
  "AlphMin",
  "ClosedCan",
  "ConcOldN",
  "ConcOldP",
  "ConcYN",
  "ConcYP",
  "RtConc",
  "NFix?",
  "DayFrac",
  "FixResp",
  "FixDWMax",
  "FixDWUnit",
  "RtDiam",
  "Lrvm1",
  "Lrvm2",
  "Lrvm3",
  "Lrvm4",
  "LraConst",
  "DecDepth",
  "SRL",
  "HalfLife",
  "AllocResp",
  "DistResp",
  "MycMaxInf",
  "RhizEffKaP",
  "NutMobN",
  "NutMobP",
  "LignRes",
  "LignRootRes",
  "PolypRes",
  "PolypRoot",
  "CovEffEr",
  "EatenbyPigs?",
  "EatenbyMonkeys?",
  "EatenbyLocusts?",
  "EatenbyNematode?",
  "EatenbyGoat?",
  "EatenbyBuffalo?",
  "EatenbyBirds?",
  "AgronYMoist"
)

Cq_var <- c(
  "Cq_CTimeGenCurr",
  "Cq_CTimeVegCurr",
  "Cq_SingleCycle_is_Cur",
  "Cq_DOYFlwDOYBegin",
  "Cq_DOYFlwEnd",
  "Cq_GroMaxCurr",
  "Cq_GSeedCurr",
  "Cq_TranspRatioCurr",
  "Cq_HBiomConvCurr",
  "Cq_RemobFrac",
  "Cq_kLightCurr",
  "Cq_RelLightMaxCurr",
  "C_LAIMax",
  "Cq_RainWStorCapCurr",
  "Cq_ConductivityCurr",
  "Cq_PotSuctAlphMaxCurr",
  "Cq_PotSuctAlphMinCurr",
  "Cq_ClosedCanCurr",
  "",
  "",
  "",
  "",
  "Cq_NRtConcCurr",
  "Cq_NFixVariable_is_Curr",
  "Cq_NFixDailyFracCurr",
  "Cq_NFixRespCurr",
  "Cq_NFixDWMaxFracCurr",
  "Cq_NFixDWUnitCostCurr",
  "Cq_RtDiam",
  "",
  "",
  "",
  "",
  "Rt_CLraC",
  "Rt_CDecDepthC",
  "Rt_CSRLCurr",
  "Rt_CHalfLifeCurr",
  "Rt_CRtAllocRespCurr",
  "Rt_CDistResp",
  "Cq_MaxMycInf",
  "N_CRhizKaPMod",
  "",
  "",
  "Cq_LignResidCurr",
  "Cq_LignRootResCurr",
  "Cq_PolyResid",
  "Cq_PolyRt",
  "Cq_CovEffCurr",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  ""
)


PF_UnitAll <- c(
  "Discrate",
  "BurnLab",
  "FertNS",
  "FertPS",
  "ExtOrg1S",
  "ExtOrg2S",
  "PestS",
  "FenceS",
  "LabUnitS",
  "FertNP",
  "FertPP",
  "ExtOrg1P",
  "ExtOrg2P",
  "PestP",
  "FenceP",
  "UnitLabP"
)

PF_UnitCrop <- c(
  "PlantLab",
  "WeedLab",
  "PestLab",
  "HarvLab",
  "FertLab",
  "SeedS",
  "SeedP",
  "YieldS",
  "YieldP"
)

PF_UnitTree <- c(
  "Plantlab",
  "PrunLab",
  "FruitLab",
  "Woodlab",
  "LatexLab",
  "FertLab",
  "SeedS",
  "SeedP",
  "WoodS",
  "FruitS",
  "LatexS",
  "PrunS",
  "WoodP",
  "FruitP",
  "LatexP",
  "PrunP"
)

S_Unit <- c(
  "Ksat1",
  "KsatD1",
  "FieldC1",
  "Inacc1",
  "Ksat2",
  "KsatD2",
  "FieldC2",
  "Inacc2",
  "Ksat3",
  "KsatD3",
  "FieldC3",
  "Inacc3",
  "Ksat4",
  "KsatD4",
  "FieldC4",
  "Inacc4",
  "BDLayer1",
  "BDLayer2",
  "BDLayer3",
  "BDLayer4",
  "SiltLayer1",
  "SiltLayer2",
  "SiltLayer3",
  "SiltLayer4",
  "ClayLayer1",
  "ClayLayer2",
  "ClayLayer3",
  "ClayLayer4",
  "ThetaSat1",
  "Alpha1",
  "n1",
  "ThetaSat2",
  "Alpha2",
  "n2",
  "ThetaSat3",
  "Alpha3",
  "n3",
  "ThetaSat4",
  "Alpha4",
  "n4"
)

T_PrunUnit <- c("PrunPlant?", "PrunType?", "CropHarv?")

T_Unit <- c(
  "TimeVeg",
  "TimeGen",
  "FlowBeg",
  "FlowEnd",
  "InitStage",
  "StageAftPrun",
  "GroMax",
  "GroResFrac",
  "LWR",
  "SLA",
  "TranspRat",
  "Rubber?",
  "Palm?",
  "RelFruitAllocMax",
  "CanHMax",
  "CanShape",
  "CanWidthMax",
  "LAIMax",
  "LAIMinMaxRatio",
  "RelLight",
  "kLight",
  "RainStorCap",
  "TreeRootCond",
  "AlphaMax",
  "AlphaMin",
  "NFixVar?",
  "DayFrac",
  "DWMaxFrac",
  "DWUnitCost",
  "Resp",
  "GroResN",
  "LfN",
  "TwigN",
  "WoodN",
  "FruitN",
  "RtN",
  "GroResP",
  "LfP",
  "TwigP",
  "WoodP",
  "FruitP",
  "RtP",
  "DroughtFrac",
  "ThreshWStress",
  "LifallRedN",
  "LifallRedP",
  "LignLifall",
  "LignPrun",
  "LignRt",
  "PolypLifall",
  "PolypPrun",
  "PolypRt",
  "ApplyFBA?",
  "BiomDiam1",
  "BiomSlope",
  "BranchDiam1",
  "BranchSlope",
  "LfTwig1",
  "LfTwigSlope",
  "CumLit1",
  "CumLitSlope",
  "WoodDens",
  "RtDiam",
  "Lrv11",
  "Lrv12",
  "Lrv13",
  "Lrv14",
  "Lrv21",
  "Lrv22",
  "Lrv23",
  "Lrv24",
  "Lrv31",
  "Lrv32",
  "Lrv33",
  "Lrv34",
  "Lrv41",
  "Lrv42",
  "Lrv43",
  "Lrv44",
  "LraX0",
  "DistShape",
  "DecDepthC",
  "Root_HalfLifeTime",
  "DistResp",
  "AllocResp",
  "Alloc",
  "LengDiam1",
  "LengSlope",
  "WghtDiam1",
  "WghtSlope",
  "ProxGini",
  "MycMaxT",
  "RhizKaP",
  "TNutMobN",
  "TNutMobP",
  "TempTol",
  "EroCovEff",
  "PigsEatT?",
  "MonkEatT?",
  "LocustEatT?",
  "NematEatT?",
  "GoatEatT?",
  "BuffEatT?",
  "BirdEatT?",
  "TrunkInternode",
  "TrunkIntercept",
  "TrunkHIncStreesfac",
  "StemDHistFrac",
  "StemDStressFac",
  "FrondHistFac",
  "FrondLegthStressFac",
  "FrondWDInterc",
  "FrondWDPower",
  "FrondLDInterc",
  "FrondLDPower",
  "RadFrac",
  "ForgetStress",
  "PhyStressFac",
  "LeafCumInit",
  "MaleThresh",
  "MTreshToWatStress",
  "FirstBud",
  "MaleSinkR",
  "MaleSinkR1",
  "MaleSinkR2",
  "MaleSinkR3",
  "MaleSinkR4",
  "MaleSinkR5",
  "MaleSinkR6",
  "MaleSinkR7",
  "MaleSinkR8",
  "MaleSinkR9",
  "MaleSinkR10",
  "MaleSinkR11",
  "MaleSinkR12",
  "MaleSinkEF",
  "MaleSinkP",
  "MaleSinkA",
  "MaleSinkA1",
  "MaleSinkA2",
  "MaleSinkA3",
  "MaleSinkA4",
  "MaleSinkA5",
  "MaleSinkA6",
  "FemSinkperBunchR",
  "FemSinkperBunchR1",
  "FemSinkperBunchR2",
  "FemSinkperBunchR3",
  "FemSinkperBunchR4",
  "FemSinkperBunchR5",
  "FemSinkperBunchR6",
  "FemSinkperBunchR7",
  "FemSinkperBunchR8",
  "FemSinkperBunchR9",
  "FemSinkperBunchR10",
  "FemSinkperBunchR11",
  "FemSinkperBunchR12",
  "FemSinkperBunchEF",
  "FemSinkperBunchP",
  "FemSinkperBunchA",
  "FemSinkperBunchA1",
  "FemSinkperBunchA2",
  "FemSinkperBunchA3",
  "FemSinkperBunchA4",
  "FemSinkperBunchA5",
  "FemSinkperBunchA6",
  "TargetOilperBR",
  "TargetOilperBR1",
  "TargetOilperBR2",
  "TargetOilperBR3",
  "TargetOilperBR4",
  "TargetOilperBR5",
  "TargetOilperBR6",
  "TargetOilperBR7",
  "TargetOilperBR8",
  "TargetOilperBR9",
  "TargetOilperBR10",
  "TargetOilperBR11",
  "TargetOilperBR12",
  "TargetOilperBEF",
  "TargetOilperBP",
  "TargetOilperBA",
  "TargetOilperBA1",
  "TargetOilperBA2",
  "TargetOilperBA3",
  "TargetOilperBA4",
  "TargetOilperBA5",
  "TargetOilperBA6",
  "DWCostOil",
  "FruitWatPot",
  "CritFruitWatPot",
  "StageAbortSensR",
  "StageAbortSensR1",
  "StageAbortSensR2",
  "StageAbortSensR3",
  "StageAbortSensR4",
  "StageAbortSensR5",
  "StageAbortSensR6",
  "StageAbortSensR7",
  "StageAbortSensR8",
  "StageAbortSensR9",
  "StageAbortSensR10",
  "StageAbortSensR11",
  "StageAbortSensR12",
  "StageAbortSensEF",
  "StageAbortSensP",
  "StageAbortSensA",
  "StageAbortSensA1",
  "StageAbortSensA2",
  "StageAbortSensA3",
  "StageAbortSensA4",
  "StageAbortSensA5",
  "StageAbortSensA6",
  "WatStressAbortFrac",
  "AbRelSizePow"
)

T_var <- c(
  'T_TimeVeg',
  'T_TimeGenCycle',
  'T_DOYFlwBeg',
  'T_DOYFlwEnd',
  'T_InitStage',
  'T_StageAftPrun',
  'T_GroMax',
  'T_GroResFrac',
  'T_LWR',
  'T_SLA',
  'T_TranspRatio',
  'T_Rubber',
  'T_ApplyPalm_is',
  'T_RelFruitAllocMax',
  'T_CanHMax',
  'T_CanShape',
  'T_CanWidthMax',
  'T_LAIMax',
  'T_LAIMinMaxRatio',
  'T_RelLightMaxGr',
  'T_klight',
  'T_RainWStorCap',
  'T_RootConductivity',
  'TW_PotSuctAlphMax',
  'TW_PotSuctAlphMin',
  'T_NFixVariable_is',
  'T_NFixDayFrac',
  'T_NFixDWMaxFrac',
  'T_NFixDWUnitCost',
  'T_NFixResp',
  'T_ConcGroRes_N',
  'T_LfConc_N',
  'T_ConcTwig_N',
  'T_ConcWood_N',
  'T_ConcFruit_N',
  'T_ConcRt_N',
  'T_ConcGroRes_P',
  'T_LfConc_P',
  'T_ConcTwig_P',
  'T_ConcWood_P',
  'T_ConcFruit_P',
  'T_ConcRt_P',
  'T_LifallDroughtFrac',
  'T_LiFallThreshWStress',
  'T_NLifallRed_N',
  'T_NLifallRed_P',
  'T_LignLifall',
  'T_LignPrun',
  'T_LignRt',
  'T_PolypLifall',
  'T_PolypPrun',
  'T_PolypRt',
  'T_ApplyFBA_is',
  'T_DiamBiom1',
  'T_DiamSlopeBiom',
  'T_DiamBranch1',
  'T_DiamSlopeBranch',
  'T_DiamLfTwig1',
  'T_DiamSlopeLfTwig',
  'T_DiamCumLit1',
  'T_DiamSlopeCumLit',
  'T_WoodDens',
  'Rt_TDiam',
  'Rt_TLrvL1_Zn1',
  'Rt_TLrvL1_Zn2',
  'Rt_TLrvL1_Zn3',
  'Rt_TLrvL1_Zn4',
  'Rt_TLrvL2_Zn1',
  'Rt_TLrvL2_Zn2',
  'Rt_TLrvL2_Zn3',
  'Rt_TLrvL2_Zn4',
  'Rt_TLrvL3_Zn1',
  'Rt_TLrvL3_Zn2',
  'Rt_TLrvL3_Zn3',
  'Rt_TLrvL3_Zn4',
  'Rt_TLrvL4_Zn1',
  'Rt_TLrvL4_Zn2',
  'Rt_TLrvL4_Zn3',
  'Rt_TLrvL4_Zn4',
  'Rt_TLraX0',
  'Rt_TDistShapeC',
  'Rt_TDecDepthC',
  'Rt_THalfLife',
  'Rt_TDistResp',
  'Rt_TAllocResp',
  'Rt_TAlloc',
  'T_DiamRtLeng1',
  'T_DiamSlopeRtLeng',
  'T_DiamRtWght1',
  'T_DiamSlopeRtWght',
  'Rt_TProxGini',
  'T_MycMaxInf',
  'N_TRhizKaPMod',
  'T_NutMobT_N',
  'T_NutMobT_P',
  'SB_TTempTol',
  'E_CovEffT',
  'PD_TEatenBy_Pigs',
  "PD_TEatenBy_Monkeys",
  "PD_TEatenBy_Grasshoppers",
  "PD_TEatenBy_Nematodes",
  "PD_TEatenBy_Goats",
  "PD_TEatenBy_Buffalo",
  "PD_TEatenBy_Birds",
  "TF_PalmTrunkInternode",
  "TF_PalmTrunkIntercept",
  "TF_TrunkHIncStressFac",
  "TF_StemDHistFrac",
  "TF_StemDStressFactor",
  "TF_FrondHistFrac",
  "TF_FrondLengthStressFac",
  "TF_FrondWDpetInterc",
  "TF_FrondWDpetPower",
  "TF_FrondLDpetInterc",
  "TF_FrondLDpetPower",
  "TF_RadiometricFraction",
  "TF_ForgetStress",
  "TF_PhyllochronStressFac",
  "TF_LeafCumInit",
  "TF_MaleThresh",
  "TF_MTreshtoWStress",
  "TF_FirstBudtoFlowerInit",
  "TF_MaleSinkperBunch_Ripe",
  "TF_MaleSinkperBunch_Ripe1",
  "TF_MaleSinkperBunch_Ripe2",
  "TF_MaleSinkperBunch_Ripe3",
  "TF_MaleSinkperBunch_Ripe4",
  "TF_MaleSinkperBunch_Ripe5",
  "TF_MaleSinkperBunch_Ripe6",
  "TF_MaleSinkperBunch_Ripe7",
  "TF_MaleSinkperBunch_Ripe8",
  "TF_MaleSinkperBunch_Ripe9",
  "TF_MaleSinkperBunch_Ripe10",
  "TF_MaleSinkperBunch_Ripe11",
  "TF_MaleSinkperBunch_Ripe12",
  "TF_MaleSinkperBunch_EarlyFruit",
  "TF_MaleSinkperBunch_Pollinated",
  "TF_MaleSinkperBunch_Anthesis",
  "TF_MaleSinkperBunch_Anthesis1",
  "TF_MaleSinkperBunch_Anthesis2",
  "TF_MaleSinkperBunch_Anthesis3",
  "TF_MaleSinkperBunch_Anthesis4",
  "TF_MaleSinkperBunch_Anthesis5",
  "TF_MaleSinkperBunch_Anthesis6",
  "TF_FemaleSinkperFruit_Ripe",
  "TF_FemaleSinkperFruit_Ripe1",
  "TF_FemaleSinkperFruit_Ripe2",
  "TF_FemaleSinkperFruit_Ripe3",
  "TF_FemaleSinkperFruit_Ripe4",
  "TF_FemaleSinkperFruit_Ripe5",
  "TF_FemaleSinkperFruit_Ripe6",
  "TF_FemaleSinkperFruit_Ripe7",
  "TF_FemaleSinkperFruit_Ripe8",
  "TF_FemaleSinkperFruit_Ripe9",
  "TF_FemaleSinkperFruit_Ripe10",
  "TF_FemaleSinkperFruit_Ripe11",
  "TF_FemaleSinkperFruit_Ripe12",
  "TF_FemaleSinkperFruit_EarlyFruit",
  "TF_FemaleSinkperFruit_Pollinated",
  "TF_FemaleSinkperFruit_Anthesis",
  "TF_FemaleSinkperFruit_Anthesis1",
  "TF_FemaleSinkperFruit_Anthesis2",
  "TF_FemaleSinkperFruit_Anthesis3",
  "TF_FemaleSinkperFruit_Anthesis4",
  "TF_FemaleSinkperFruit_Anthesis5",
  "TF_FemaleSinkperFruit_Anthesis6",
  "TF_TargetOilperBunch_Ripe",
  "TF_TargetOilperBunch_Ripe1",
  "TF_TargetOilperBunch_Ripe2",
  "TF_TargetOilperBunch_Ripe3",
  "TF_TargetOilperBunch_Ripe4",
  "TF_TargetOilperBunch_Ripe5",
  "TF_TargetOilperBunch_Ripe6",
  "TF_TargetOilperBunch_Ripe7",
  "TF_TargetOilperBunch_Ripe8",
  "TF_TargetOilperBunch_Ripe9",
  "TF_TargetOilperBunch_Ripe10",
  "TF_TargetOilperBunch_Ripe11",
  "TF_TargetOilperBunch_Ripe12",
  "TF_TargetOilperBunch_EarlyFruit",
  "TF_TargetOilperBunch_Pollinated",
  "TF_TargetOilperBunch_Anthesis",
  "TF_TargetOilperBunch_Anthesis1",
  "TF_TargetOilperBunch_Anthesis2",
  "TF_TargetOilperBunch_Anthesis3",
  "TF_TargetOilperBunch_Anthesis4",
  "TF_TargetOilperBunch_Anthesis5",
  "TF_TargetOilperBunch_Anthesis6",
  "TF_DWCostUOil",
  "TF_FruitWaterPot",
  "TF_CritFruitWaterPot",
  "TF_StageAbortSens_Ripe",
  "TF_StageAbortSens_Ripe1",
  "TF_StageAbortSens_Ripe2",
  "TF_StageAbortSens_Ripe3",
  "TF_StageAbortSens_Ripe4",
  "TF_StageAbortSens_Ripe5",
  "TF_StageAbortSens_Ripe6",
  "TF_StageAbortSens_Ripe7",
  "TF_StageAbortSens_Ripe8",
  "TF_StageAbortSens_Ripe9",
  "TF_StageAbortSens_Ripe10",
  "TF_StageAbortSens_Ripe11",
  "TF_StageAbortSens_Ripe12",
  "TF_StageAbortSens_EarlyFruit",
  "TF_StageAbortSens_Pollinated",
  "TF_StageAbortSens_Anthesis",
  "TF_StageAbortSens_Anthesis1",
  "TF_StageAbortSens_Anthesis2",
  "TF_StageAbortSens_Anthesis3",
  "TF_StageAbortSens_Anthesis4",
  "TF_StageAbortSens_Anthesis5",
  "TF_StageAbortSens_Anthesis6",
  "TF_WatStressAbortFrac",
  "TF_AbRelSizePow"
  
)

TF_StageAbortSens_var <- c(
  "TF_StageAbortSens_Ripe",
  "TF_StageAbortSens_Ripe1",
  "TF_StageAbortSens_Ripe2",
  "TF_StageAbortSens_Ripe3",
  "TF_StageAbortSens_Ripe4",
  "TF_StageAbortSens_Ripe5",
  "TF_StageAbortSens_Ripe6",
  "TF_StageAbortSens_Ripe7",
  "TF_StageAbortSens_Ripe8",
  "TF_StageAbortSens_Ripe9",
  "TF_StageAbortSens_Ripe10",
  "TF_StageAbortSens_Ripe11",
  "TF_StageAbortSens_Ripe12",
  "TF_StageAbortSens_EarlyFruit",
  "TF_StageAbortSens_Pollinated",
  "TF_StageAbortSens_Anthesis",
  "TF_StageAbortSens_Anthesis1",
  "TF_StageAbortSens_Anthesis2",
  "TF_StageAbortSens_Anthesis3",
  "TF_StageAbortSens_Anthesis4",
  "TF_StageAbortSens_Anthesis5",
  "TF_StageAbortSens_Anthesis6"
) 

N_One <- c(
  "Pinit11",
  "Pinit12",
  "Pinit13",
  "Pinit14",
  "Pinit21",
  "Pinit22",
  "Pinit23",
  "Pinit24",
  "Pinit31",
  "Pinit32",
  "Pinit33",
  "Pinit34",
  "Pinit41",
  "Pinit42",
  "Pinit43",
  "Pinit44",
  "PStMin_1",
  "PStMin_2",
  "PStMin_3",
  "PStMin_4",
  "PStMax1",
  "PStMax2",
  "PStMax3",
  "PStMax4"
)

### ARRAY def #########################

nzone <- 4
nlayer <- 4
ntree <- 3
ncrop <- 5
zone_df <- data.frame(zone = c(1:nzone))
layer_df <- data.frame(layer = c(1:nlayer))
tree_df <- data.frame(tree_id = c(1:ntree))
crop_df <- data.frame(crop_id = c(1:ncrop))

zonelayer_df <- data.frame(
  zone = rep(zone_df$zone, nlayer),
  layer = rep(layer_df$layer, each = nzone)
)

zonetree_df <- data.frame(
  zone = rep(zone_df$zone, ntree),
  tree_id = rep(tree_df$tree_id, each = nzone)
)

zonelayertree_df <- zonelayer_df[rep(seq_len(nrow(zonelayer_df)), ntree), ]
zonelayertree_df$tree_id <- rep(1:ntree, each = nrow(zonelayer_df))


SlNut <- c("N", "P")
PlantComp <- c("DW", "N", "P")
PriceType <- c("Private", "Social")
# LiteLayer <- c("L1", "L2", "L3", "L4")
Animals <- c("Pigs",
             "Monkeys",
             "Grasshoppers",
             "Nematodes",
             "Goats",
             "Buffalo",
             "Birds")
Limiting_Factors <- c("Water", "N", "P")
Cent_Pools <- c("Met", "Str", "Actv", "Slow", "Pass")
Fruitbunch <- c(
  "Ripe",
  "Ripe_1",
  "Ripe_2",
  "Ripe_3",
  "Ripe_4",
  "Ripe_5",
  "Ripe_6",
  "Ripe_7",
  "Ripe_8",
  "Ripe_9",
  "Ripe_10",
  "Ripe_11",
  "Ripe_12",
  "Early_fruit",
  "Pollinated",
  "Anthesis",
  "Anthesis_1",
  "Anthesis_2",
  "Anthesis_3",
  "Anthesis_4",
  "Anthesis_5",
  "Anthesis_6"
)
InsectLifeStage <- c("Larvae", "Adults")
Tree_Stage <- c("VegGen", "LeafAge")
BufValues <- 1:10
ExtOrgInputs <- 1:2

# zonelite_df <- data.frame(
#   zone = rep(zone_df$zone, length(LiteLayer)),
#   LiteLayer = rep(LiteLayer, each = nzone)
# )
# 
# zonelitetree_df <- zonelite_df[rep(seq_len(nrow(zonelite_df)), ntree), ]
# zonelitetree_df$tree_id <- rep(1:ntree, each = nrow(zonelite_df))

zonenut_df <- data.frame(zone = rep(zone_df$zone, length(SlNut)),
                           SlNut = rep(SlNut, each = nzone))

layertree_df <- data.frame(layer = rep(layer_df$layer, length(ntree)),
                          tree_id = rep(1:ntree, each = nlayer))

layernut_df <- data.frame(layer = rep(layer_df$layer, length(SlNut)),
                         SlNut = rep(SlNut, each = nlayer))

layertreenut_df <- layertree_df[rep(seq_len(nrow(layertree_df)), length(SlNut)), ]
layertreenut_df$SlNut <- rep(SlNut, each = nrow(layertree_df))

zonelayernut_df <- zonelayer_df[rep(seq_len(nrow(zonelayer_df)), length(SlNut)), ]
zonelayernut_df$SlNut <- rep(SlNut, each = nrow(zonelayer_df))

treestage_df <- data.frame(
  tree_id = rep(tree_df$tree_id, length(Tree_Stage)),
  Tree_Stage = rep(Tree_Stage, each = nrow(tree_df))
)

pcomp_df <- data.frame(PlantComp = PlantComp)

treepcomp_df <- data.frame(
  tree_id = rep(tree_df$tree_id, length(PlantComp)),
  PlantComp = rep(PlantComp, each = ntree)
)

zonepcomp_df <- data.frame(
  zone = rep(zone_df$zone, length(PlantComp)),
  PlantComp = rep(PlantComp, each = nzone)
)

zonelayerpcomp_df <- zonelayer_df[rep(seq_len(nrow(zonelayer_df)), length(PlantComp)), ]
zonelayerpcomp_df$PlantComp <- rep(PlantComp, each = nrow(zonelayer_df))

zonelayertreepcomp_df <- zonelayertree_df[rep(seq_len(nrow(zonelayertree_df)), length(PlantComp)), ]
zonelayertreepcomp_df$PlantComp <- rep(PlantComp, each = nrow(zonelayertree_df))

treelimit_df <- data.frame(
  tree_id = rep(tree_df$tree_id, length(Limiting_Factors)),
  Limiting_Factors = rep(Limiting_Factors, each = ntree)
)

zonelimit_df <- data.frame(
  zone = rep(zone_df$zone, length(Limiting_Factors)),
  Limiting_Factors = rep(Limiting_Factors, each = nzone)
)

buf_df <- data.frame(buf_id = 1:10)

zonebuf_df <- data.frame(zone = rep(zone_df$zone, nrow(buf_df)))
zonebuf_df$buf_id <- rep(buf_df$buf_id, each = nzone)

treebuf_df <- data.frame(tree_id = rep(tree_df$tree_id, nrow(buf_df)))
treebuf_df$buf_id <- rep(buf_df$buf_id, each = ntree)

zonetreebuf_df <- data.frame(zone = rep(zonetree_df$zone, nrow(buf_df)))
zonetreebuf_df$tree_id <- rep(zonetree_df$tree_id, nrow(buf_df))
zonetreebuf_df$buf_id <- rep(buf_df$buf_id, each = nrow(zonetree_df))

zonelayerbuf_df <- zonelayer_df[rep(seq_len(nrow(zonelayer_df)), nrow(buf_df)), ]
zonelayerbuf_df$buf_id <- rep(buf_df$buf_id, each = nrow(zonelayer_df))

angle_df <- data.frame(angle_id = 1:35)

nut_df <- data.frame(SlNut = SlNut)
treenut_df <- data.frame(tree_id = rep(tree_df$tree_id, nrow(nut_df)))
treenut_df$SlNut <- rep(nut_df$SlNut, each = ntree)
# zonenut_df <- data.frame(zone = rep(zone_df$zone, nrow(nut_df)))
# zonenut_df$SlNut <- rep(nut_df$SlNut, each = nzone)

animal_df <- data.frame(Animals = Animals)
zoneanimal_df <- data.frame(zone = rep(zone_df$zone, length(Animals)))
zoneanimal_df$Animals <- rep(Animals, each = nzone)
treeanimal_df <- data.frame(tree_id = rep(tree_df$tree_id, length(Animals)))
treeanimal_df$Animals <- rep(Animals, each = ntree)

fruit_df <- data.frame(Fruitbunch = Fruitbunch)

treefruit_df <- data.frame(tree_id = rep(tree_df$tree_id, length(Fruitbunch)))
treefruit_df$Fruitbunch <- rep(Fruitbunch, each = ntree)

#water update module
soil_water_id <- c("MS", "MW", "S", "W")
nwater <- 4
zonelayerwater_df <- zonelayer_df[rep(seq_len(nrow(zonelayer_df)), nwater), ]
zonelayerwater_df$water <- rep(soil_water_id, each = nrow(zonelayer_df))
zonetreewater_df <- zonetree_df[rep(seq_len(nrow(zonetree_df)), nwater), ]
zonetreewater_df$water <- rep(soil_water_id, each = nrow(zonetree_df))

zonelayertreewater_df <- zonelayertree_df[rep(seq_len(nrow(zonelayertree_df)), nwater), ]
zonelayertreewater_df$water <- rep(soil_water_id, each = nrow(zonelayertree_df))

zonelayertreenut_df <- zonelayertree_df[rep(seq_len(nrow(zonelayertree_df)), length(SlNut)), ]
zonelayertreenut_df$SlNut <- rep(nut_df$SlNut, each = nrow(zonelayertree_df))

zonetreenut_df <- zonetree_df[rep(seq_len(nrow(zonetree_df)), length(SlNut)), ]
zonetreenut_df$SlNut <- rep(nut_df$SlNut, each = nrow(zonetree_df))

inp_df <- data.frame(ExtOrgInputs = ExtOrgInputs)

nutinp_df <- data.frame(SlNut = rep(nut_df$SlNut, length(ExtOrgInputs)))
nutinp_df$ExtOrgInputs <- rep(ExtOrgInputs, each = nrow(nut_df))

zoneinp_df <- data.frame(zone = rep(zone_df$zone, length(ExtOrgInputs)))
zoneinp_df$ExtOrgInputs <- rep(ExtOrgInputs, each = nzone)

cpools_df <- data.frame(Cent_Pools = Cent_Pools)

zonecpools_df <- data.frame(zone = rep(zone_df$zone, length(Cent_Pools)))
zonecpools_df$Cent_Pools <- rep(Cent_Pools, each = nzone)

# angle_df <- data.frame(LightAngles = pars$Light_par$LightAngles)
# # TanAngles[LightAngle] = tan(LightAngles[LightAngle]*PI/180)
# angle_df$TanAngles <- tan(angle_df$LightAngles * pi / 180)

### PAR def #########################
pars <- list(
  AF_par = list(
    AF_AnyTrees_is = 1,
    AF_Crop_is = 1,
    AF_DeepSubSoil = 3,
    AF_DepthDynamic_is = 0,
    AF_DepthGroundWater_Table = 0,
    AF_DynPestImpacts_is = 0,
    AF_PlotNumberUphill = 0,
    
    AF_RunOnFrac = 0,
    AF_RunWatLim_is = 1,
    AF_SimulateWeeds_is = 0,
    AF_SlopeSoilHoriz = 0,
    AF_SlopeSurfInit = 0,
    AF_TreeCircleWeedFree_is = 0,
    zone_par = data.frame(
      zone = zone_df$zone,
      AF_LitLayerDepth = 2,
      AF_WeedZn_is = 0,
      AF_ZoneTree = 1:nzone
    ),
    zonelayer_par = data.frame(
      zone = zonelayer_df$zone,
      layer = zonelayer_df$layer,
      AF_StoneFrac = 0
    ),
    nut_par = cbind(nut_df, data.frame(AF_RunNutLim_is = 1))
  ),
  
  Ash_par = list(
    Ash_init_BD = 0.3,
    Ash_Init_Water = 0,
    Ash_SOM_pools = 0,
    Ash_Time = 10000,
    zone_par = data.frame(zone = zone_df$zone, Ash_layer_depth = 0)
  ),
  
  B_par = list(
    BC_C_RespforFix = 0,
    BC_CPhotosynth = 0,
    BC_CropInit = 0,
    BC_ExtOrgInput = 0,
    BC_TPhotosynth = 0,
    BC_TPlantMatTot = 0,
    BC_TRespforFix = 0,
    BW_EvapCum = 0,
    BW_RunOffCum = 0,
    BW_RunOnCum = 0,
    BW_UptCCum = 0,
    BW_UptWCum = 0,
    tree_par = data.frame(tree_id = tree_df$tree_id, BW_UptTCum = 0),
    nut_par = cbind(
      nut_df,
      data.frame(
        BN_CUptCum = 0,
        BN_ExtOrgInputs = 0,
        BN_FertCum = 0,
        BN_RunOffLoss = 0,
        BN_TreeInit = 0
      )
    ),
    treenut_par = cbind(treenut_df, data.frame(
      BN_TNFixAmountCum = 0, BN_TUptCum = 0
    ))
  ),
  
  C_par = list(
    C_AgronYPerType = 0,
    C_ApplyMaintResp_is = 0,
    C_BiomHarvestPast = 0,
    C_CumLim = -1,
    C_DailyWeedSeedDecayFrac = 0.02,
    C_GroResMobFrac = 0.95,
    C_RelRespGroRes = 0.5,
    C_RelRespRt = 0.3,
    C_RelRespStLv = 0.5,
    C_RelRespYieldCurr = 1,
    C_ResidRemovalFrac = 0,
    C_RespperBiom = 0.01,
    C_StressAccLim = 0.9,
    C_TMax = 40,
    C_TMin = 20,
    C_TOpt = 21,
    C_WeedGermFrac = 0.1,
    C_WeedSeedBankInit = 0.01,
    C_WeedSeedExtInflux = 1e-05,
    Ca_DOYStart = 1,
    Ca_PastFertApp = 0,
    Cq_CropGraze = 0,
    Cq_StageAfterGraze = 0.4,
    Cq_WeedType = 5,
    zone_par = data.frame(
      zone = zone_df$zone,
      C_CanLow = 0,
      C_CropDays = 0,
      C_Height = 0,
      C_HydEqFluxes = 0,
      Ca_ComplCrop = 0,
      Cq_CropWeedSwitch = 0,
      Cq_Stage = 0
    ),
    pcomp_par = data.frame(
      PlantComp = pcomp_df$PlantComp,
      C_SeedConc = c(1, 0.05, 0.005),
      C_UnitConv = c(1, 1000, 1000)
      
    ),
    zonepcomp_par = data.frame(
      zone = zonepcomp_df$zone,
      PlantComp = zonepcomp_df$PlantComp,
      C_BiomHarvestCum = 0,
      C_BiomStLv = 0,
      C_GroRes = 0,
      C_HarvestCum = 0,
      C_ResidRecycle = 0,
      C_ResidRemoved = 0,
      C_YieldCurr = 0
    ),
    zonelayerpcomp_par = data.frame(
      zone = zonelayerpcomp_df$zone,
      layer = zonelayerpcomp_df$layer,
      PlantComp = zonelayerpcomp_df$PlantComp,
      C_Root = 0
    ),
    zonelimit_par = data.frame(
      zone = zonelimit_df$zone,
      Limiting_Factors = zonelimit_df$Limiting_Factors,
      C_CumLim = -1
    ),
    zonenut_par = data.frame(
      zone = zonenut_df$zone,
      SlNut = zonenut_df$SlNut,
      C_NDfaTot = 0
    ),
    crop_par = cbind(crop_df, data.frame(C_HostEffForT1 = 0)),
    nut_par = cbind(nut_df, data.frame(Ca_PastImmInp = 0))
  ),
  
  Cent_par = list(nut_par = cbind(
    nut_df,
    data.frame(
      Cent_ExtOrgInputs = 0,
      Cent_LitterNInflow = 0,
      Cent_NOut = 0,
      Cent2_LitSomTransfAcc = 0,
      Cent2_NOut = 0,
      Cent2_SOMNinflow = 0
    )
  )),
  
  CW_par = list(
    CW_Alpha = 0.1,
    CW_DryFactRangeChange = 0.2,
    CW_DryFactRangePowerStart = 1,
    CW_DryPowerMax = 4,
    CW_DryPowerMin = 0.1,
    CW_EnergyDrivenEpot_is = 1,
    zone_par = data.frame(zone = zone_df$zone, CW_DemandPerRoot = 1),
    buf_par = data.frame(
      buf_id = buf_df$buf_id,
      CW_DryFactRangeInit = c(0.1, 0.3, 0.5, 0.8, 0.9, 1, 1.3, 1.5, 1.7, 2)
    )
  ),
  
  E_par = list(
    E_CovEffLitter = 0.002,
    E_EntrailmentCoeffBarePlot = 0.002,
    E_ErosiType = 0,
    E_IntvPloughPlant = 10,
    E_PloughBefPlant_is = 0,
    E_PloughPast = 0,
    E_RainFac = 1,
    E_RelSedConcRunOn = 1,
    E_SoilMovperPlou = 0,
    E_SoilType = 1,
    zone_par = data.frame(
      zone = zone_df$zone,
      E_CumSoilInflowZn = 0,
      E_SoilLossCumZn = 0,
      E_TillZone_is = c(0, 1, 1, 1)
    )
    
  ),
  
  Evap_par = list(
    Evap_InitSlashM = 0.4,
    Evap_InitWoodM = 0.25,
    Evap_MulchEffSurfLit = 1,
    Evap_Pot_Thornthwaite_is = 1,
    Evap_SlashDryFact = 0.5,
    Evap_TranspRedFractrionBy_Can_Intercepted_Water = 0.5,
    Evap_WoodDryFact = 0.25,
    Evap_YesterdayTTransp = 0,
    zone_par = data.frame(
      zone = zone_df$zone,
      Evap_SlashWater = 0,
      Evap_SurfCum = 0,
      Evap_WoodMoist = 0
    )
  ),
  
  G_par = list(
    G_AnimRespFrac_N = 0,
    G_AnimRespFrac_P = 0,
    G_AnimRespFrac_DW = 0.5,
    G_AnmWGfrac = 0.05,
    G_CumFeedSufficiency = 0,
    G_DayDempKgDay_P = 5e-05,
    G_DayDempKgDay_N = 5e-04,
    G_DayDempKgDay_DW = 0.025,
    G_Graze_Offseason_is = 1,
    G_Grazed_Manure_Output = 0,
    G_Grazed_Respired = 0,
    G_GrazedBiomCum = 0,
    G_Grazing_Cycle = 10,
    G_LivestWeightGain = 0,
    G_SLU = 450,
    G_StockingRate_per_ha = 0.3,
    zone_par = data.frame(zone = zone_df$zone, G_Graze_Zn_is = c(0, 0, 0, 0))
  ),
  
  GHG_par = list(
    GHG_AnaerobLayerW = 0.6,
    GHG_AnaerobLayerW = 0.2,
    GHG_AnaerobLayerW = 0.15,
    GHG_AnaerobLayerW = 0.05,
    GHG_AnaerobMem = 0.5,
    GHG_AnaeroThresh = 0.95,
    GHG_CH4_Km = 0.05,
    GHG_GWP_CH4 = 15,
    GHG_GWP_N2O = 310,
    GHG_LittMinMultiplier = 1,
    GHG_PotCH4Em = 0.0011,
    GHG_PotCH4oxid = -0.0011,
    GHG_SatTimeDuringVDrainDay = 0.8,
    zone_par = data.frame(
      zone = zone_df$zone,
      GHG_AnaerobIndic = 0,
      GHG_N2_EmZn = 0,
      GHG_Net_CH4_Em = 0,
      GHG_Nitric_Oxide_EmZn = 0,
      GHG_Nitrous_Oxide_EmZn = 0
    )
  ),
  
  LF_par = list(
    LF_FracGWReleaseAsInflow = 0,
    LF_GW_ReleaseFraction = 0.05,
    LF_H1Drain1Cum = 0,
    LF_H2Drain1Cum = 0,
    LF_H3Drain1Cum = 0,
    LF_H4Drain1Cum = 0,
    LF_LatIn1Cum = 0,
    LF_LatIn2Cum = 0,
    LF_LatIn3Cum = 0,
    LF_LatIn4Cum = 0,
    LF_SubSurfInflowAdd4 = 0,
    LF_UphillGWStore = 0
  ),
  
  Light_par = list(
    Ligh1tPerAngleUniform = 0.05,
    Light_TBAI = 0,
    Light_TBAI1 = 0,
    Light2PerAngleSkewed = 0.05,
    Light3PerAngleVertical = c(rep(0, 17), 1, rep(0, 17)),
    
    LightSwitch = 3,
    zone_par = data.frame(zone = zone_df$zone, Light_CRefRelCapCum = 0),
    tree_par = data.frame(tree_id = tree_df$tree_id, Light_kTB = 0.8),
    
    angle_par = data.frame(
      angle_id = angle_df$angle_id,
      LightAngles = seq(-85, 85, 5)
    )
  ),
  
  #### Mc_par ########
  Mc_par = list(
    Mc_Carbon = 0.42,
    # Mc_CExtOrg = 0.4,
    Mc_EffMetab = 0.45,
    Mc_EffPass = 0.45,
    Mc_EffSlwAct = 0.42,
    Mc_EffSlwPass = 0.03,
    Mc_EffStrucAct = 0.45,
    Mc_EffStrucSlw = 0.7,
    Mc_EffStrucSlwSOM = 0.7,
    # Mc_LignExtOrg = 0.2,
    # Mc_PolypExtOrg = c(0, 0),
    Mc_RelKActLit = 1,
    Mc_RelKMetabLit = 0.8,
    Mc_RelKPassLit = 1,
    Mc_RelKSlwLit = 1,
    Mc_RelKStrucLit = 0.808511,
    Mc_TextLitLayer = 0.01,
    Mc2_ClayCoeffCref = 0.94,
    Mc2_CorgInitMeth3 = 2,
    Mc2_CorgpCref = 0.8,
    Mc2_CrefMeth3 = 3,
    Mc2_CrefOffset = 1.256,
    Mc2_EffPass = 0.45,
    Mc2_EffSlwAct = 0.42,
    Mc2_EffStrucAc = 0.45,
    Mc2_kAct = 0.02,
    Mc2_kMetab = 0.05,
    Mc2_kPass = 1.86e-05,
    Mc2_kRelLayer = c(1, 0.8, 0.7, 0.6),
    Mc2_kSlw = 0.000543,
    Mc2_kStruc = 0.013429,
    Mc2_pH = 5,
    Mc2_pHCoeffCref = -0.156,
    # Mc2_RainTransfer_Met = 0.0001,
    # Mc2_RainTransfer_Actv = 0.001,
    # Mc2_RainTransfer_Pass = 0.001,
    # Mc2_RainTransfer_Slow = 0.001,
    # Mc2_RainTransfer_Str = 0.001,
    Mc2_SiltClayCoeffCref = 0.699/0.994,
    # Mc2_SoilTillTransfer = 1,
    # Mc2_SOMDist = c(1, 0.2, 0.1, 0.05),
    Mc2_SomInitType = 1,
    # Mc2_WormTransfer_Pass = 0.003,
    # Mc2_WormTransfer_Slow = 0.01,
    # Mc2_WormTransfer_Actv = 0.03,
    # Mc2_WormTransfer_Met = 0.03,
    # Mc2_WormTransfer_Str = 0.03,
    zone_par = data.frame(
      zone = zone_df$zone,
      Mc_ActCO2 = 0,
      Mc_CNRatInitMet = 8,
      Mc_MetabCO2 = 0,
      Mc_PassCO2 = 0,
      Mc_SlwCO2 = 0,
      Mc_StrucActCO2 = 0,
      Mc_StrucSlwCO2 = 0,
      Mc2_ActCO2 = 0,
      Mc2_CNRatInitMet = 8,
      Mc2_MetabCO2 = 0,
      MC2_OrgC_Leached = 0,
      Mc2_PassCO2 = 0,
      Mc2_SlwCO2 = 0,
      Mc2_StrucActCO2 = 0,
      Mc2_StrucSlwCO2 = 0
    ),
    layer_par = cbind(layer_df, data.frame(Mc2_SOMDist = c(1, 0.2, 0.1, 0.05))),
    inp_par = cbind(inp_df, data.frame(
      Mc_CExtOrg = 0.4,
      Mc_LignExtOrg = 0.2,
      Mc_PolypExtOrg = 0
      )),
    cpools_par = cbind(cpools_df, data.frame(
      Mc2_RainTransfer = c(0.0001, 0.001, 0.001, 0.001, 0.001),
      Mc2_SoilTillTransfer = 1,
      Mc2_WormTransfer = c(0.03, 0.03, 0.03, 0.01, 0.003)
    ))
  ),
  
  #### Mn_par ########
  Mn_par = list(
    Mn_CNAct = 8,
    Mn_CNPass = 61,
    Mn_CNSlw = 11,
    Mn_CNStruc = 150,
    Mn_LatFlowFertKm = 10,
    Mn2_NSOMMinExch = 0.5,
    MP2_SomMinExchBuffer = 0.1,
    zone_par = cbind(
      zone_df,
      data.frame(
        Mn_InitAct = 2e-05,
        Mn_InitMetab = 0,
        Mn_InitPass = 1e-04,
        Mn_InitSlw = 1e-06,
        Mn_InitStruc = 0,
        Mn2_InitAct = 0.091,
        Mn2_InitMetab = 0,
        Mn2_InitPassx = 0.728,
        Mn2_InitSlw = 1.01,
        Mn2_InitStruc = 0,
        Mn2_OrgNLeached = 0,
        MP2_InitStruc = 0,
        MP2_OrgP_Leached = 0
      )
    ),
    nut_par = cbind(
      nut_df,
      data.frame(
        # Mn_ExtOrgN_1 = c(0.05, 0.005),
        # Mn_ExtOrgN_2 = c(0.1, 0.01),
        Mn_FertDissFrac = c(0.3, 0.5),
        Mn_NutRatAct = c(1, 10),
        Mn_NutRatMetab = c(1, 10),
        Mn_NutRatPas = c(1, 10),
        Mn_NutRatSlw = c(1, 10),
        Mn_NutRatStruc = c(1, 10)
      )
    ),
    zonenut_par = cbind(zonenut_df, data.frame(
      Mn_FertOnSoil = 0, Mn_MinNutpool = 0
    )),
    zonelayer_par = cbind(zonelayer_df, data.frame(
      Mn2_MinNutpool = 0, MP2_MinNutpool = 0
    )),
    layer_par = cbind(layer_df, data.frame(Mn2_PassRelLayer = c(1, 1.2, 1.4, 1.6))),
    nutinp_par = cbind(nutinp_df, data.frame(Mn_ExtOrgN = c(0.05, 0.005, 0.1, 0.01)))
  ),
  
  #### N_par #####################
  N_par = list(
    N_DiffCoef = 1,
    # N_KaNH41 = 5,
    # N_KaNH42 = 5,
    # N_KaNH43 = 5,
    # N_KaNH44 = 5,
    # N_KaNO31 = 0.3,
    # N_KaNO32 = 0.3,
    # N_KaNO33 = 0.3,
    # N_KaNO34 = 0.3,
    N_Lat4InflowRelConc = 1,
    N_LeachCumV = 0,
    N_LittNmin1exchfact = 0.1,
    # N_Loss1iCum = 0,
    # N_Loss2iCum = 0,
    # N_Loss3iCum = 0,
    # N_Loss4iCum = 0,

    N_Nutmob2_N = 0,
    N_Nutmob2_P = 0,
    N_Nutmob3_N = 0,
    N_Nutmob3_P = 0,
    N_Nutmob4_N = 0,
    N_Nutmob4_P = 0,
    # N_RtSynloc1 = 0.5,
    # N_RtSynloc2 = 0.5,
    # N_RtSynloc3 = 0.5,
    # N_RtSynloc4 = 0.5,
    # N_UptCCum1 = 0,
    # N_UptCCum2 = 0,
    # N_UptCCum3 = 0,
    # N_UptCCum4 = 0,
    N_UptT1Cum1 = 0,
    N_UptT1Cum2 = 0,
    N_UptT1Cum3 = 0,
    N_UptT1Cum4 = 0,
    N_UptT2Cum1 = 0,
    N_UptT2Cum2 = 0,
    N_UptT2Cum3 = 0,
    N_UptT2Cum4 = 0,
    N_UptT3Cum1 = 0,
    N_UptT3Cum2 = 0,
    N_UptT3Cum3 = 0,
    N_UptT3Cum4 = 0,
    N_Use_NgassLossEst_is = 0,
    zone_par = data.frame(
      zone = zone_df$zone,
      N15_C_GroRes = 0,
      N15_C_ResidCum = 0,
      N15_C_Root = 0,
      N15_C_StLv = 0,
      N15_C_YieldCurr = 0
    ),
    layer_par = cbind(
      layer_df,
      data.frame(
        N_KaNH4 = 5,
        N_KaNO3 = 0.3, 
        N_LossiCum = 0,
        N_RtSynloc = 0.5,
        N_UptCCum = 0
      )),
    tree_par = data.frame(
      tree_id = tree_df$tree_id,
      N15_T_Can = 0,
      N15_T_Fruit = 0,
      N15_T_GroRes = 0,
      N15_T_Rt = 0,
      N15_T_Wd = 0
    ),
    zonelayer_par = data.frame(
      zone = zonelayer_df$zone,
      layer = zonelayer_df$layer,
      N_BypassMatrix = 0.2,
      N_FracNO3 = 0.4,
      N_N2LossCum = 0,
      N15_Add = 0,
      N15_Stock = 0,
      N_BypassMacro = 1
    ),
    zonelayernut_par =
      cbind(zonelayernut_df, data.frame(N_ImInit = c(rep(
        0.05, nrow(zonelayer_df)
      ), rep(
        0.01, nrow(zonelayer_df)
      )))), 
    nut_par = cbind(
      nut_df,
      data.frame(
        N_AtmosphDepos = 0,
        N_CumAtmInput = 0,
        N_LatInflowCum1 = 0,
        N_LatInflowCum2 = 0,
        N_LatInflowCum3 = 0,
        N_LatInFlowCum4 = 0,
        N_LatOutflowCum1 = 0,
        N_LatOutflowCum2 = 0,
        N_LatOutflowCum3 = 0,
        N_LatOutflowCum4 = 0,
        N_Nutmob1 = 0,
        N_N_is = c(1, 0),
        N_DiffCoef = c(1, 0.89*10^-5*60*60*24)
      )
    )
  ),
  
  P_par = list(
    P_CCostsTot = 0,
    P_CNuFertAppperCropSeason = 1,
    P_CostExtOrg = 1,
    P_CReturnTot = 0,
    P_CropHarvMarker = 0,
    P_CropProfThreshold = 1e+05,
    P_CumLabUse = 0,
    P_CurrentCropCB = 0,
    P_FlagPrevCropOK_is = 1,
    P_GeneralCosts = 0,
    P_Initial_NPV = 0,
    P_LabourforPestContrl_is = 0,
    P_LabourforWeed_is = 0,
    P_TCostsTot = 0,
    P_TNuFertAppperTreeAge = c(1, 1, 1),
    P_TPrunNo = 0,
    P_TReturnTot = 0,
    P_UseCropStopRule_is = 0
  ),
  
  #### PD_par #####################
  
  PD_par = list(
    PD_FenceDecK = 0.02,
    PD_FenceFullQual = 2,
    PD_FenceMaint_is = 0,
    PD_FenceMUnit = 0.25,
    PD_FencePast = 0,
    PD_FenceQ = 0,
    PD_FenceQThresh = 1.1,
    PD_HalfFenceTime = 50,
    PD_PopulOutside = 0,
    tree_par = data.frame(
      tree_id = tree_df$tree_id,
      PD_TFrugiv_Abort = 0,
      PD_THerbivory = 0,
      PD_TLignovory = 0,
      PD_TRhizovory = 0
    ),
    crop_par = cbind(
      crop_df,
      data.frame(
        PD_CFrugivory = 0,
        PD_CHerbivory = 0,
        PD_CRhizovory = 0
        
      )
    ),
    animal_par = cbind(
      animal_df,
      data.frame(
        PD_CFrugivore_is = 0,
        PD_CHerbivore_is = 0,
        PD_CRhizovore_is = 0,
        PD_JumptheFence_is = c(0,1,1,1,0, 0,1),
        PD_NastiesinPlot = 0,
        PD_ResidenceinPlot_is = c(0, 0,1,1,0,0,0),
        PD_TFrugivore_is = 0,
        PD_THerbivore_is = 0,
        PD_TLignovore_is = 0,
        PD_TRhizovore_is = 0
      )
    )
  ),
  
  Rain_par = list(
    Rain_AnMemory = 2,
    Rain_AType = 1,
    Rain_BoundHeaLi = 25,
    Rain_CoefVar3 = 0.05,
    Rain_CoefVar4 = 0.05,
    Rain_Cum = 0,
    Rain_Cycle_is = 1,
    Rain_Gamma = 0.033621,
    Rain_GenSeed = 300,
    Rain_Heavy = 42,
    Rain_HeavyP = 0.5,
    Rain_I_Initial_Value = 1,
    Rain_IntensCoefVar = 0.3,
    Rain_IntensMean = 50,
    Rain_IntercDripRt = 10,
    Rain_IntercEvapCum = 0,
    Rain_IntMult = 3,
    Rain_Light = 9,
    Rain_Max_IntDripDur = 0.5,
    Rain_MonthlyMean_RainfallMax = 333,
    Rain_MonthlyMean_RainfallMin = 102,
    Rain_Months = c(1:12),
    Rain_Multiplier = 1,
    Rain_OffsetValue = -0.5,
    Rain_Pattern1_Max = 0.06,
    Rain_Pattern1_Min = -0.01,
    Rain_Peakines_Season1 = 1,
    Rain_Peakines_Season2 = 12,
    Rain_PondFlwRt = 10,
    Rain_PondStoreCp = 5,
    Rain_Probability = 0.5,
    Rain_Shape_Max = 1.5,
    Rain_Shape_Min = -0.5,
    Rain_UniorBimodial_is = 2,
    Rain_UniorBimodial_is = 2,
    Rain_Weibull_Param = 0.93,
    Rain_WettestMonth_Season1 = 1,
    Rain_WettestMonth_Season2 = 7,
    Rain_YearStart = 0,
    zone_par = data.frame(
      zone = zone_df$zone,
      Rain_AnaerInd = 0,
      Rain_CanopyWater = 0,
      Rain_Weight = 1
    )
  ),
  
  #### Rt_par ###############
  Rt_par = list(
    Rt_ACType = 0,
    Rt_CLrvPlatDep = 0,
    Rt_CMultiplier = 1,
    Rt_CRhizExt = 1,
    Rt_MCHypDiam = 0.01,
    Rt_MCHypL = 100,
    Rt_T_FixedSRL = 20,
    Rt_T_HostEffForT1 = c(0, 0, 0),
    Rt_T_UseFBASRL_is = 0,
    Rt_THalfRtAllocStage = 0.05,
    Rt_TMultiplier = 1,
    RT_TRhizExt = 1,
    Rt_StopGap = 10^-12,
    RT3_PowerAllocRtL = 0.005,
    RT3_TempRespforRtD = 0.5,
    zone_par = data.frame(zone = zone_df$zone, RT_CDecDepthAct = 0),
    zonelayer_par =  cbind(zonelayer_df, data.frame(
      RT3_SoilT = 20)
    ),
    zonelayertree_par = cbind(zonelayertree_df, data.frame(RT3_TFRInit = c(10, rep(
      0, nrow(zonelayertree_df) - 1
    )))), 
    tree_par = data.frame(
      tree_id = tree_df$tree_id,
      Rt_ATType = 0,
      Rt_MTHypDiam = 0.01,
      Rt_MTHypL = 100,
      RT_TRhizExt = 1,
      RT3_AlphaLrv = 1e-15,
      RT3_Beta0 = 0.2,
      RT3_CoarseRtDecayCoeff = 0,
      RT3_LamGeotrop = 0.7,
      RT3_LamHor0 = 0.5,
      RT3_CR_TargFac = 8.5*(10^-7)
    ),
    layer_par = data.frame(
      layer = layer_df$layer,
      Rt_MTInfFrac = 0,
      Rt_MCInfFrac = c(0.5, 0.25, 0.05, 0)
    )
  ),
  
  #### SB_par ###############
  SB_par = list(
    SB_2ndFireafterPileUp = 5,
    SB_CritMoist = 0.05,
    SB_DailyDeadWoodLitTransfer = 0.005,
    SB_DailyNecromLitTransfer = 0.01,
    SB_DeadWoodFuelFact = 0.1,
    SB_FuelloadFactor = 10,
    SB_InitialpH = 4,
    SB_LeafAshCont = 50,
    SB_MaxDryingPer = 30,
    SB_MinDryingPer = 20,
    SB_pHChangePerCationInput = 0.005,
    SB_pHRecFrac = 0.01,
    SB_PileUpFrac = 0.7,
    SB_PsorpRecFrac = 0.01,
    SB_ScorchWRemFra = 0.3,
    SB_TimetoPileUp = 15,
    SB_TimeToWoodRemoval = 10,
    SB_WatRetRecFrac = 0.005,
    SB_WetnessTempImp = 0.5,
    SB_WindEffect = 1,
    SB_WoodAshCont = 20,
    zone_par = data.frame(
      zone = zone_df$zone,
      SB_AerosolProd = 0,
      SB_DWlossfromBurn = 0,
      SB_PsorpModifier = 1,
      SB_WatRetentionMod = 0,
      SB_PileUpWgt = c(0, 0, 0, 1)
    ),
    tree_par = data.frame(tree_id = tree_df$tree_id, SB_PastSlashEvents = 0),
    pcomp_par = cbind(pcomp_df, data.frame(SB_DW_is = c(1, 0, 0))),
    zonepcomp_par = cbind(
      zonepcomp_df,
      data.frame(
        SB_DeadWood = 0,
        SB_FineNecromass = 0,
        SB_Nutvolatilized = 0,
        SB_ScorchWoodRemoved = 0
      )
    )
  ),
  
  S_par = list(
    S_BDBDRefDecay = 1e-04,
    S_BDEqPower = 0.5,
    S_C_Rt_StrucFormFrac = 0.1,
    S_KsatVDeepSub = 20,
    S_RelWormLit = c(1, 0.6, 0.3, 0.1),
    S_RelWormSurf = 1,
    S_SoilStructDyn_is = 0,
    S_WormsLikeLitMetab = 1e-05,
    S_WormsLikeLitStruc = 5e-07,
    S_WormsLikeSOMMetab = 1e-06,
    S_WormsLikeSOMStruc = 5e-08,
    zone_par = data.frame(
      zone = zone_df$zone,
      S_RelSurfInfiltrInit = 4,
      S_SurfInfiltrPerKsatDef = 0.0825
    ),
    zonelayer_par = data.frame(
      zone = zonelayer_df$zone,
      layer = zonelayer_df$layer,
      S_KSatHperV = 1
    ),
    tree_par = data.frame(tree_id = tree_df$tree_id, S_T_Rt_StrucFormFrac = 0.3)
  ),
  
  Temp_par = list(
    Temp_AType = 1,
    Temp_Cons = 28,
    Temp_PotEvapConst = 4,
    Temp_PotEvapConst_is = 0
  ),
  
  #### T_par ####
  T_par = list(
    T_BrownBast_is = 1,
    T_CanMaintResp = 1e-04,
    T_DCanWidthMax = 22,
    T_ExpRetThresh = 30,
    T_GrowthResp = 1,
    T_HeartWoodAllocAftPruned_is = 0,
    T_LatexFormResp = 0.01,
    T_LatexMaintResp = 1e-05,
    T_LatexRecoveryTime = 7,
    T_LifallDelay = 0.5,
    T_MaxBarkLatexContent = 0.3,
    T_MaxGrowthUtFrac = 0.2,
    T_MaxUseFrac = 0.07,
    T_MemExpY = 1,
    T_MinDiamforTappingcm = 15,
    T_PanelRecoveryTime = 7300,
    T_PrunLimit = 100,
    T_PrunPast = 0,
    T_PrunStageLimit = 1.8,
    T_RecoveryExp = 0.01,
    T_RelLatexFormPriority = 0.7,
    T_SapWoodScalingRule = 1,
    T_StressAccLim = 0.9,
    T_StressRatio = 4,
    T_Tapatall_is = 1,
    T_TapGirthFraction = 0.5,
    T_TappableHeight = 100,
    T_TappDOW = 380,
    T_TappingFrac = 0.55,
    T_TappingFracMultiplier = 1,
    T_TappingSlice = 0.15,
    T_TranspRatioConstant_is = 1,
    T_WatStressMem = 0.75,
    T_WoodMaintResp = 1e-07,
    TF_FruitMoistContent = 0.36,
    TF_LiFall_ResWithdrawl = 0.01,
    TF_StreesUpdateFraction = 0.4,
    TP_IncludeTreeParasites_is = 0,
    TP_MaxSizePerparasite = 0.5,
    TP_ParasitesPerM2ofBranch = 3,
    TP_SLA_LWR = 1,
    TP_TimeToReachMaxSize = 730,
    TP_WaterDemandperLeafArea = 0.6,
    TW_Alpha = 0.1,
    TW_DryFactPowerRangeChange = 0.5,
    TW_EnergyDrivenEpot_is = 0,
    
    zone_par = data.frame(zone = zone_df$zone, TW_Water_Limited_is = 0),
    zonelayertreepcomp_par = cbind(zonelayertreepcomp_df, data.frame(T_Root = 0)),
    zonelayertree_par = cbind(zonelayertree_df, data.frame(T_RelRtIncrTyp2 = 1 /
                                                             16)),
    
    tree_par = data.frame(
      tree_id = tree_df$tree_id,
      T_CanBiomInit = 0,
      T_Compl = 0,
      T_CumLim = 0,
      T_CumWatStress = 0,
      T_DiamTreshHarv = 100,
      T_DOY_1_LfFlush = -1,
      T_DOY_2_LfFlush = 400,
      T_DOY_Compl_1_LfFall = 400,
      T_DOY_Compl_2_LfFall = 400,
      T_DOY_SeaLitFall_1_Start = 400,
      T_DOY_SeaLitFall_2_Start = 400,
      T_FracSeasLitFll_1 = 1,
      T_FruitHarvFrac = 1,
      T_FruitLossHeight = 35,
      T_FruitMoistFrac = 0.14,
      T_GenLitFracMax = 0.05,
      T_GraphPhenol_is = 0,
      T_GroResInit = 0.25,
      T_GrowDays = 0,
      T_GrowResMobFrac = 0.2,
      T_GrowResStorFrac = 0.2,
      T_HeartWoodDiam = 0,
      T_HydEqFluxes = 0,
      T_Kill2DOY = 1,
      T_Kill2Y = 1000,
      T_Kill3DOY = 1,
      T_Kill3Y = 1000,
      T_KillDOY = 1,
      T_KillY = 1000,
      T_LatexMoistFrac = 0.14,
      T_LeafHalfLife = 0.5,
      T_NDemandFrac = 0.2,
      T_PanelAlreadyInitiated_is = 0,
      T_PanelAvailable = 0,
      T_PanelQuality1 = 1,
      T_PanelQuality2 = 1,
      T_PrunHarvFracC = 0,
      T_PrunLapse = 1000,
      T_PrunMoistFrac = 0.14,
      T_PrunRecov = 14,
      T_RtAllocInit = 0.1,
      T_SecTimePanelAvailable = 0,
      T_SlashSellWoodFrac = 0,
      T_SRLfineroots = 0.8,
      T_Stage = 0,
      T_StemBefPruning = 0,
      T_TapDaysCum = 0,
      T_TotTappingDays = 0,
      T_WoodBiomInit = 0,
      T_WoodFracHRemain = 100,
      T_WoodH = 0,
      T_WoodHarvFrac = 0.95,
      T_WoodHarvPast = 0,
      T_WoodHInit = 0,
      T_WoodMoistFrac = 0.25,
      
      TF_CumBunchNo = 0,
      TF_CumBunchWeightHarvest = 0,
      TF_CumFruitsHarvNo = 0,
      TF_CumOilHarvest = 0,
      TF_CumOilProd = 0,
      TF_DelayedFrond_BiomassTarget = 0,
      TF_DW_Sufficiency_Yesterday = 1,
      TF_FruitLossHeight = 5,
      TF_LeaffallCumNo = 0,
      TF_LeavesTargetCum = 0,
      TF_NextGender = 0,
      TF_OldFrontBiomass = 0,
      TF_PalmTrunkHeight = 0,
      TF_RecentFrondLength = 0,
      TF_RecentTWPosgro = 0,
      TF_TrunkDiam = 0,
      TF_TrunkTargetCum = 0,
      TF_YNewLeaf_is = 0,
      TP_ParasiteRemoval = 0,
      TP_RelNutSupplyDelayed = 1,
      TW_DemandPerRoot = 1,
      TW_DryFactPowerInit = 1,
      TW_PosgroMin = 1e-05,
      TW_ResistFact = 50,
      T_PrunFracC = 1
    ),
    zonetree_par = data.frame(
      zone = zonetree_df$zone,
      tree_id = zonetree_df$tree_id,
      T_LifallWeight = 1,
      T_PrunWeight = 1
    ),
    treestage_par = data.frame(
      tree_id = treestage_df$tree_id,
      Tree_Stage = treestage_df$Tree_Stage,
      T_Stage = 0,
      T_StageAftPrun_is = 1
    ),
    pcomp_par = data.frame(
      PlantComp = pcomp_df$PlantComp,
      T_RootDecCum = 0,
      T_UnitConv = c(1, 1000, 1000),
      TF_LitFallRedFac = c(1, 0.5, 0.5),
      TP_BiomNutrContent = c(0, 0.2, 0.02)
      
    ),
    treepcomp_par = data.frame(
      tree_id = treepcomp_df$tree_id,
      PlantComp = treepcomp_df$PlantComp,
      T_CanBionTimCum = 0,
      T_CBSlashCum = 0,
      T_CumRtType2Decay = 0,
      T_Fruit = 0,
      T_FruitCum = 0,
      T_GroRes = 0,
      T_GroResLossCum = 0,
      T_GrowStor = 0,
      T_HeartWood = 0,
      T_LatexStock = 0,
      T_LfTwig = 0,
      T_LifallCum = 0,
      T_PrunCum = 0,
      T_PrunHarvCum = 0,
      T_RespCum = 0,
      T_RootIncFix = 10,
      T_RtType0CumInput = 0,
      T_RtType2Biomass = 0,
      T_SapWood = 0,
      T_TappedLatex = 0,
      T_WoodHarvCum = 0,
      TP_ParasiteBiomass = 0
      
    ),
    treelimit_par = data.frame(
      tree_id = treelimit_df$tree_id,
      Limiting_Factors = treelimit_df$Limiting_Factors,
      T_CumLim = 0
    ),
    buf_par = data.frame(
      buf_id = buf_df$buf_id,
      TW_DemActSubtract = c(9e-06, 8e-06, 7e-06, 6e-06, 5e-06, 4e-06, 3e-06, 2e-06, 1e-06, 0),
      TW_OffSetRel = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0.1, 0.2, 0.3, 0.4, 0.5),
      TW_RangeToppingUp = c(1.05, 1.02, 1, 1, 1, 1, 1, 1, 1, 1)
    ),
    treenut_par = cbind(treenut_df, data.frame(T_NPosgroMin = 1e-05)),
    fruit_par = cbind(fruit_df, data.frame(TF_PollinationStage = c(
      rep(0, 14), 1, rep(0, 7)
    ))),
    treefruit_par = cbind(
      treefruit_df,
      data.frame(
        TF_FruitsperBunch = 0,
        TF_BunchGender = 0,
        TF_BunchWeight_kgpbunch = 0
      )
    )
  ), 
  
  W_par = list(
    W_Hyd_is = 0,
    W_HydEqFraction = 0.1,
    W_PMax = 0,
    W_SeepScalar = 0,
    W_WaterLog_is = 0,
    zone_par = data.frame(
      zone = zone_df$zone,
      W_DrainCumV = 0,
      W_WaterLimited_is = 0
    ),
    zonelayer_par = data.frame(
      zone = zonelayer_df$zone,
      layer = zonelayer_df$layer,
      W_ThetaInit = rep(c(1, 0.9, 0.8, 0.7), each = nzone)
    ),
    zonelayertree_par = data.frame(
      zone = zonelayertree_df$zone,
      layer = zonelayertree_df$layer,
      tree_id = zonelayertree_df$tree_id,
      W_UptCum = 0
    )
  )
  
  
)


assign_par <- function(df, par_label) {
  for (p in names(pars)) {
    d <- pars[[p]][[par_label]]
    if (!is.null(d)) {
      vars <- setdiff(names(d), names(df))
      df <- cbind(df, d[vars])
    }
  }
  return(df)
}

zone_df <- assign_par(zone_df, "zone_par")
layer_df <- assign_par(layer_df, "layer_par")
tree_df <- assign_par(tree_df, "tree_par")
crop_df <- assign_par(crop_df, "crop_par")
zonelayer_df <- assign_par(zonelayer_df, "zonelayer_par")
zonetree_df <- assign_par(zonetree_df, "zonetree_par")
zonelayertree_df <- assign_par(zonelayertree_df, "zonelayertree_par")
# zonelite_df <- assign_par(zonelite_df, "zonelite_par")
# zonelitetree_df <- assign_par(zonelitetree_df, "zonelitetree_par")
zonenut_df <- assign_par(zonenut_df, "zonenut_par")
zonelayernut_df <- assign_par(zonelayernut_df, "zonelayernut_par")
treestage_df <- assign_par(treestage_df, "treestage_par")
pcomp_df <- assign_par(pcomp_df, "pcomp_par")
treepcomp_df <- assign_par(treepcomp_df, "treepcomp_par")
zonepcomp_df <- assign_par(zonepcomp_df, "zonepcomp_par")
zonelayerpcomp_df <- assign_par(zonelayerpcomp_df, "zonelayerpcomp_par")
zonelayertreepcomp_df <- assign_par(zonelayertreepcomp_df, "zonelayertreepcomp_par")
treelimit_df <- assign_par(treelimit_df, "treelimit_par")
zonelimit_df <- assign_par(zonelimit_df, "zonelimit_par")
buf_df <- assign_par(buf_df, "buf_par")
angle_df <- assign_par(angle_df, "angle_par")
nut_df <- assign_par(nut_df, "nut_par")
zonenut_df <- assign_par(zonenut_df, "zonenut_par")
treenut_df <- assign_par(treenut_df, "treenut_par")
animal_df <- assign_par(animal_df, "animal_par")
treefruit_df <- assign_par(treefruit_df, "treefruit_par")
fruit_df <- assign_par(fruit_df, "fruit_par")
inp_df <- assign_par(inp_df, "inp_par")
cpools_df <- assign_par(cpools_df, "cpools_par")
nutinp_df <- assign_par(nutinp_df, "nutinp_par")

# TanAngles[LightAngle] = tan(LightAngles[LightAngle]*PI/180)
angle_df$TanAngles <- tan(angle_df$LightAngles * pi / 180)


### STELLA xls par #################

parxls_df <- wb_to_df("data/Wanulcas.xlsm", "LinkToStella")
par_names <- names(parxls_df)

get_par_xls_list <- function(unit_labels, column_names) {
  l <- as.list(parxls_df[1:length(unit_labels), column_names])
  names(l) <- unit_labels
  return(l)
}

### pars$AF_par xls #################

AF_System_par <- get_par_xls_list(AF_Unit, "AF_System")
pars$AF_par$AF_Circ <- AF_System_par$`Parkland?`
pars$AF_par$AF_ZoneTot <- AF_System_par$AFTotZn

zw <- unlist(AF_System_par[c("AFZn1", "AFZn2", "AFZn3")])
zone_df$AF_ZoneWidth <- c(zw, pars$AF_par$AF_ZoneTot - sum(zw))
zone_df$AF_SlopeSurfInit <- pars$AF_par$AF_SlopeSurfInit
zone_df$AF_ZWcum <- cumsum(zone_df$AF_ZoneWidth)

if (pars$AF_par$AF_Circ == 0) {
  zone_df$AF_ZoneFrac <- zone_df$AF_ZoneWidth / pars$AF_par$AF_ZoneTot
} else {
  zone_df$AF_ZWcum_0 <- c(0, head(zone_df$AF_ZWcum, -1))
  zone_df$AF_ZoneFrac <- (zone_df$AF_ZWcum^2 -  zone_df$AF_ZWcum_0^2) / pars$AF_par$AF_ZoneTot^2
}

# AF_DepthLay1 = AF_System[SL1]
# AF_DepthLay2 = AF_System[SL2]
# AF_DepthLay3 = AF_System[SL3]
# AF_DepthLay4 = AF_System[SL1]
layer_df$AF_DepthLay <- unlist(AF_System_par[c("SL1", "SL2", "SL3", "SL4")])
# AF_TreePosit[Sp1] = AF_System[TSp1]
# AF_TreePosit[Sp2] = AF_System[TSp2]
# AF_TreePosit[Sp3] = AF_System[TSp3]
tree_df$AF_TreePosit <- unlist(AF_System_par[c("TSp1", "TSp2", "TSp3")])
# T_RelPosinZone[Sp1] = AF_System[TRelSp1]
# T_RelPosinZone[Sp2] = AF_System[TRelSp2]
# T_RelPosinZone[Sp3] = AF_System[TRelSp3]
tree_df$T_RelPosinZone <- unlist(AF_System_par[c("TRelSp1", "TRelSp2", "TRelSp3")])
# T_TreesperHa[Sp1] = AF_System[TDensSp1]
# T_TreesperHa[Sp2] = AF_System[TDensSp2]
# T_TreesperHa[Sp3] = AF_System[TDensSp3]
tree_df$T_Treesperha <- unlist(AF_System_par[c("TDensSp1", "TDensSp2", "TDensSp3")])

zone_df$Rt_ZoneRight <- ifelse(zone_df$AF_ZoneWidth > 0, zone_df$AF_ZWcum, 0)
zone_df$Rt_ZoneLeft <- ifelse(zone_df$AF_ZoneWidth > 0, c(0, head(zone_df$AF_ZWcum, -1)), 0)






### CROP par ###############

Cq_df <- parxls_df[1:length(Cq_Unit), c(
  "Cq_Parameters1",
  "Cq_Parameters2",
  "Cq_Parameters3",
  "Cq_Parameters4",
  "Cq_Parameters5"
)]
Cq_df$Cq_Unit <- Cq_Unit
Cq_df$Cq_var <- Cq_var

animals_unit <- c(
  "EatenbyPigs?",
  "EatenbyMonkeys?",
  "EatenbyLocusts?",
  "EatenbyNematode?",
  "EatenbyGoat?",
  "EatenbyBuffalo?",
  "EatenbyBirds?"
)

Cq_var_par <- Cq_df$Cq_var[nzchar(Cq_df$Cq_var)]

update_Cq_par <- function() {
  zone_df[Cq_var_par] <<- t(Cq_df[Cq_df$Cq_var %in% Cq_var_par, zone_df$Cq_CType])
  
  zonenut_df$Cq_NConcYoungCurr <<-
    unlist(c(Cq_df[Cq_df$Cq_Unit == "ConcYN", zone_df$Cq_CType], Cq_df[Cq_df$Cq_Unit == "ConcYP", zone_df$Cq_CType]))
  
  zonenut_df$Cq_NConcOldCurr <<-
    unlist(c(Cq_df[Cq_df$Cq_Unit == "ConcOldN", zone_df$Cq_CType], Cq_df[Cq_df$Cq_Unit == "ConcOldP", zone_df$Cq_CType]))
  
  zonenut_df$N_CNutMob <-
    unlist(c(Cq_df[Cq_df$Cq_Unit == "NutMobN", zone_df$Cq_CType], Cq_df[Cq_df$Cq_Unit == "NutMobP", zone_df$Cq_CType]))
  
  zoneanimal_df$PD_CropsEaten_is <<- as.vector(t(Cq_df[Cq_df$Cq_Unit %in% animals_unit, zone_df$Cq_CType]))
  
}


Cq_CType_col <- c(
  "Type1_Zn1",
  "Type1_Zn2",
  "Type1_Zn3",
  "Type1_Zn4",
  "Type2_Zn1",
  "Type2_Zn2",
  "Type2_Zn3",
  "Type2_Zn4",
  "Type3_Zn1",
  "Type3_Zn2",
  "Type3_Zn3",
  "Type3_Zn4",
  "Type4_Zn1",
  "Type4_Zn2",
  "Type4_Zn3",
  "Type4_Zn4",
  "Type5_Zn1",
  "Type5_Zn2",
  "Type5_Zn3",
  "Type5_Zn4"
)

zone_stage_col_df <- data.frame(
  Cq_CType = rep(1:5, each = 4),
  zone = rep(1:4, 5),
  coln = Cq_CType_col
)

Cq_CRelLUE_i <- which(par_names == "Cq_CRelLUE")
Cq_CRelLUE_df <- parxls_df[1:21, Cq_CRelLUE_i:(Cq_CRelLUE_i + 19)]
names(Cq_CRelLUE_df) <- Cq_CType_col
Cq_CRelLUE_df$Cq_Stage <- seq(0, 2, 0.1)

Cq_CLWR_i <- which(par_names == "Cq_CLWR")
Cq_CLWR_df <- parxls_df[1:21, Cq_CLWR_i:(Cq_CLWR_i + 19)]
names(Cq_CLWR_df) <- Cq_CType_col
Cq_CLWR_df$Cq_Stage <- seq(0, 2, 0.1)

Cq_CHarvAlloc_i <- which(par_names == "Cq_CHarvAlloc")
Cq_CHarvAlloc_df <- parxls_df[1:21, Cq_CHarvAlloc_i:(Cq_CHarvAlloc_i + 19)]
names(Cq_CHarvAlloc_df) <- Cq_CType_col
Cq_CHarvAlloc_df$Cq_Stage <- seq(0, 2, 0.1)

Cq_CSLA_i <- which(par_names == "Cq_CSLA")
Cq_CSLA_df <- parxls_df[1:21, Cq_CSLA_i:(Cq_CSLA_i + 19)]
names(Cq_CSLA_df) <- Cq_CType_col
Cq_CSLA_df$Cq_Stage <- seq(0, 2, 0.1)

Cq_CRtAlloc_i <- which(par_names == "Cq_CRtAlloc")
Cq_CRtAlloc_df <- parxls_df[1:11, Cq_CRtAlloc_i:(Cq_CRtAlloc_i + 19)]
names(Cq_CRtAlloc_df) <- Cq_CType_col
Cq_CRtAlloc_df$Cq_Stage <- seq(0, 2, 0.2)

zone_stage_par <- list(
  Cq_CRelLUE = Cq_CRelLUE_df,
  Cq_CLWR = Cq_CLWR_df,
  Cq_CHarvAlloc = Cq_CHarvAlloc_df,
  Cq_CSLA = Cq_CSLA_df,
  Cq_CRtAlloc = Cq_CRtAlloc_df
)

get_zone_stage_par <- function(zone, Cq_Stage, Cq_CType, par) {
  df <- zone_stage_par[[par]]
  cn <- zone_stage_col_df[zone_stage_col_df$zone == zone &
                            zone_stage_col_df$Cq_CType == Cq_CType, ]$coln
  get_graph_y(
    df,
    Cq_Stage,
    x_column = "Cq_Stage",
    y_column = cn,
    mode = "continues"
  )
}





Ca_PlantDoY_i <- which(par_names == "Ca_PlantDoY")
Ca_PlantDoY_df <- parxls_df[1:21, Ca_PlantDoY_i:(Ca_PlantDoY_i + 3)]
names(Ca_PlantDoY_df) <- c("Ca_PlantDoY1",
                           "Ca_PlantDoY2",
                           "Ca_PlantDoY3",
                           "Ca_PlantDoY4")
Ca_PlantDoY_df$Ca_ComplCrop <- c(0:20)

Ca_PlantYear_i <- which(par_names == "Ca_PlantYear")
Ca_PlantYear_df <- parxls_df[1:21, Ca_PlantYear_i:(Ca_PlantYear_i + 3)]
names(Ca_PlantYear_df) <- c("Ca_PlantYear1",
                            "Ca_PlantYear2",
                            "Ca_PlantYear3",
                            "Ca_PlantYear4")
Ca_PlantYear_df$Ca_ComplCrop <- c(0:20)

Cq_CropType_i <- which(par_names == "Cq_CropType")
Cq_CropType_df <- parxls_df[1:21, Cq_CropType_i:(Cq_CropType_i + 3)]
names(Cq_CropType_df) <- c("Cq_CropType1",
                            "Cq_CropType2",
                            "Cq_CropType3",
                            "Cq_CropType4")
Cq_CropType_df$Ca_ComplCrop <- c(0:20)

get_val_by_Ca_ComplCrop <- function(df, Ca_ComplCrop, zone) {
  mapply(function(x, z){
    df[df$Ca_ComplCrop == x, z]
  }, Ca_ComplCrop, zone) 
}


# TODO: should  be read from xls directly
weather_df = read.csv("data/weather.csv")




### TREE par ###############

T_df <- parxls_df[1:length(T_Unit), c("T_Par1", "T_Par2", "T_Par3")]
T_df$T_Unit <- T_Unit
T_df$T_var <- T_var

tree_par_df <- as.data.frame(t(T_df[1:3]))
names(tree_par_df) <- T_var
tree_df[T_var] <- tree_par_df
#TODO: remove the vars below from tree_df, and assign directly to specific df
treefruit_df$TF_StageAbortSens <- unlist(tree_df[TF_StageAbortSens_var])


# T_TreeinZone?[Zone,Tree] = if AF_TreePosit[Tree] = AF_ZoneTree[Zone] then 1 else 0
zone_df$T_TreeinZone <- 0
tp <- tree_df$AF_TreePosit[tree_df$AF_TreePosit > 0]
zone_df$T_TreeinZone[tp] <- 1

# INIT Mc_LAIperNecmss[Zone] = T_LWR[Sp1]*T_SLA[Sp1]
zone_df$Mc_LAIperNecmss <- tree_df$T_LWR[1] * tree_df$T_SLA[1]

# INIT Rt_TDistShapeAct[Tree] = Rt_TDistShapeC[Tree]
tree_df$Rt_TDistShapeAct <- tree_df$Rt_TDistShapeC
# INIT Rt_TDecDepthAct[Tree] = Rt_TDecDepthC[Tree]
tree_df$Rt_TDecDepthAct <- tree_df$Rt_TDecDepthC
# AF_TreePosit2Q[Tree] = IF AF_TreePosit[Tree] = 2 then 1 else 0
# AF_TreePosit3Q[Tree] = IF AF_TreePosit[Tree] = 3 then 1 else 0
# AF_TreePosit4Q[Tree] = IF AF_TreePosit[Tree] = 4 then 1 else 0
tree_df$AF_TreePosit2Q <- ifelse(tree_df$AF_TreePosit == 2, 1, 0)
tree_df$AF_TreePosit3Q <- ifelse(tree_df$AF_TreePosit == 3, 1, 0)
tree_df$AF_TreePosit4Q <- ifelse(tree_df$AF_TreePosit == 4, 1, 0)
# INIT TW_DryFactPower[Tree] = TW_DryFactPowerInit[Tree]
tree_df$TW_DryFactPower <- tree_df$TW_DryFactPowerInit
# INIT TF_CurrentLeafNo[Tree] = TF_LeafCumInit[Tree]
tree_df$TF_CurrentLeafNo <- tree_df$TF_LeafCumInit

# INIT RT3_CoarseRt[Tree] = RT3_CRTarget[Tree]
tree_df$RT3_CoarseRt <- tree_df$RT3_CRTarget

T_PlantY_i <- which(par_names == "T_PlantY")
T_PlantY_df <- parxls_df[1:21, T_PlantY_i:(T_PlantY_i + 2)]
names(T_PlantY_df) <- c("T_PlantY_1", "T_PlantY_2", "T_PlantY_3")
T_PlantY_df$T_Compl <- 0:20

T_PlantDoY_i <- which(par_names == "T_PlantDoY")
T_PlantDoY_df <- parxls_df[1:21, T_PlantDoY_i:(T_PlantDoY_i + 2)]
names(T_PlantDoY_df) <- c("T_PlantDoY_1", "T_PlantDoY_2", "T_PlantDoY_3")
T_PlantDoY_df$T_Compl <- 0:20

Rt_TLrvL_df <- tree_df[c(
  "Rt_TLrvL1_Zn1",
  "Rt_TLrvL1_Zn2",
  "Rt_TLrvL1_Zn3",
  "Rt_TLrvL1_Zn4",
  "Rt_TLrvL2_Zn1",
  "Rt_TLrvL2_Zn2",
  "Rt_TLrvL2_Zn3",
  "Rt_TLrvL2_Zn4",
  "Rt_TLrvL3_Zn1",
  "Rt_TLrvL3_Zn2",
  "Rt_TLrvL3_Zn3",
  "Rt_TLrvL3_Zn4",
  "Rt_TLrvL4_Zn1",
  "Rt_TLrvL4_Zn2",
  "Rt_TLrvL4_Zn3",
  "Rt_TLrvL4_Zn4"
)]
zonelayertree_df$Rt_TLrvL_par <- unlist(c(Rt_TLrvL_df[1, ], Rt_TLrvL_df[2, ], Rt_TLrvL_df[3, ]))

zonelayertree_df$Rt_ATType <- rep(tree_df$Rt_ATType, each = nrow(zonelayer_df))

# T_TranspRatioTime[Tree] = GRAPH(T_Stage[Tree,LeafAge])
# (0.00, 1.00), (31.3, 1.00), (62.5, 0.99), (93.8, 0.97), (125, 0.92), (156, 0.865), (188, 0.78), (219, 0.69), (250, 0.615), (281, 0.52), (313, 0.4), (344, 0.25), (375, 0.115), (406, 0.00), (438, 0.00), (469, 0.00), (500, 0.00), (531, 0.00), (563, 0.00), (594, 0.00), (625, 0.00), (656, 0.00), (688, 0.00), (719, 0.00), (750, 0.00)
T_TranspRatioTime_df <- data.frame(
  T_Stage =  c(
    0,
    31.3,
    62.5,
    93.8,
    125,
    156,
    188,
    219,
    250,
    281,
    313,
    344,
    375,
    406,
    438,
    469,
    500,
    531,
    563,
    594,
    625,
    656,
    688,
    719,
    750
  ),
  T_TranspRatioTime = c(
    1,
    1,
    0.99,
    0.97,
    0.92,
    0.865,
    0.78,
    0.69,
    0.615,
    0.52,
    0.4,
    0.25,
    0.115,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
  )
)

get_T_TranspRatioTime <- function(T_Stage) {
  get_graph_y(T_TranspRatioTime_df, T_Stage, mode = "continues")
}

treeanimal_df$PD_TEatenBy_is <-
  unlist(tree_df[c(
    "PD_TEatenBy_Pigs",
    "PD_TEatenBy_Monkeys",
    "PD_TEatenBy_Grasshoppers",
    "PD_TEatenBy_Nematodes",
    "PD_TEatenBy_Goats",
    "PD_TEatenBy_Buffalo",
    "PD_TEatenBy_Birds"
  )])



T_PrunOption_par <- get_par_xls_list(T_PrunUnit, "T_PrunOption")
C_BiomHarv_is <- T_PrunOption_par$`CropHarv?`

T_PrunY_df <-
  data.frame(T_PrunPast = c(0:40), T_PrunY = parxls_df[1:41, "T_PrunY"])

T_PrunDoY_df <-
  data.frame(T_PrunPast = c(0:40), T_PrunDoY = parxls_df[1:41, "T_PrunDoY"])

T_PrunFracD_i <- which(par_names == "T_PrunFracD")
T_PrunFracD_df <- parxls_df[1:41, T_PrunFracD_i:(T_PrunFracD_i + 2)]
names(T_PrunFracD_df) <- c("T_PrunFracD_1", "T_PrunFracD_2", "T_PrunFracD_3")
T_PrunFracD_df$T_PrunPast <- 0:40

T_PrunHarvFracD_i <- which(par_names == "T_PrunHarvFracD")
T_PrunHarvFracD_df <- parxls_df[1:41, T_PrunHarvFracD_i:(T_PrunHarvFracD_i + 2)]
names(T_PrunHarvFracD_df) <- c("T_PrunHarvFracD_1",
                               "T_PrunHarvFracD_2",
                               "T_PrunHarvFracD_3")
T_PrunHarvFracD_df$T_PrunPast <- 0:40

get_T_PrunFracD <- function(T_PrunPast) {
  unlist(lapply(tree_df$tree_id, function(x) {
    get_graph_y(
      T_PrunFracD_df,
      T_PrunPast,
      x_column = "T_PrunPast",
      y_column = names(T_PrunFracD_df)[x],
      mode = "continues"
    )
  }))
}

get_T_PrunHarvFracD <- function(T_PrunPast) {
  unlist(lapply(tree_df$tree_id, function(x) {
    get_graph_y(
      T_PrunHarvFracD_df,
      T_PrunPast,
      x_column = "T_PrunPast",
      y_column = names(T_PrunHarvFracD_df)[x],
      mode = "continues"
    )
  }))
}



# TF_InitFruitspBunch[Tree] = GRAPH(TF_CurrentLeafNo[Tree])
# (0.00, 54.4), (7.00, 126), (14.0, 229), (21.0, 278), (28.0, 330), (35.0, 349), (42.0, 386), (49.0, 387), (56.0, 349), (63.0, 330), (70.0, 278)
TF_InitFruitspBunch_df <- data.frame(
  TF_CurrentLeafNo = c(0,7,14,21,28,35,42,49,56,63,70),
  TF_InitFruitspBunch = c(54.4,126,229,278,330,349,386,387,349,330,278)
)

# TF_PotPhyllochronTime[Tree] = GRAPH(TF_AgeofPalm[Tree])
# (1.00, 17.0), (379, 17.0), (756, 17.0), (1134, 21.0), (1511, 21.0), (1889, 21.0), (2266, 21.0), (2644, 21.0), (3021, 21.0), (3399, 21.0), (3777, 21.0), (4154, 21.0), (4532, 21.0), (4909, 21.0), (5287, 21.0), (5664, 21.0), (6042, 21.0), (6419, 21.0), (6797, 21.0), (7174, 21.0), (7552, 21.0), (7930, 21.0), (8307, 21.0), (8685, 21.0), (9062, 21.0), (9440, 21.0), (9817, 21.0), (10195, 21.0), (10572, 21.0), (10950, 21.0)
TF_PotPhyllochronTime_df <- data.frame(
  TF_AgeofPalm = c(1,379,756,1134,1511,1889,2266,2644,3021,3399,3777,4154,4532,4909,5287,5664,6042,6419,6797,7174,7552,7930,8307,8685,9062,9440,9817,10195,10572,10950),
TF_PotPhyllochronTime = c(17,17,17,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21)
)

get_TF_PotPhyllochronTime <- function(TF_AgeofPalm) {
  unlist(lapply(TF_AgeofPalm, function(x) {
    get_graph_y(
      TF_PotPhyllochronTime_df,
      x,
      x_column = "TF_AgeofPalm",
      y_column = "TF_PotPhyllochronTime",
      mode = "continues"
    )
  }))
}

# TF_PotentialFrondLength[Tree] = GRAPH(TF_PalmTrunkHeight[Tree])
# (0.00, 4.36), (1.00, 5.19), (2.00, 5.75), (3.00, 6.18), (4.00, 6.54), (5.00, 6.84), (6.00, 7.11), (7.00, 7.35), (8.00, 7.57), (9.00, 7.78), (10.0, 7.97), (11.0, 8.14), (12.0, 8.31), (13.0, 8.46), (14.0, 8.61), (15.0, 8.75), (16.0, 8.88), (17.0, 9.01), (18.0, 9.14), (19.0, 9.26), (20.0, 9.37)

TF_PotentialFrondLength_df <- data.frame(
  TF_PalmTrunkHeight = c(0:20),
  TF_PotentialFrondLength = c(4.36,5.19,5.75,6.18,6.54,6.84,7.11,7.35,7.57,7.78,7.97,8.14,8.31,8.46,8.61,8.75,8.88,9.01,9.14,9.26,9.37)
)

get_TF_PotentialFrondLength <- function(TF_PalmTrunkHeight) {
  unlist(lapply(TF_PalmTrunkHeight, function(x) {
    get_graph_y(
      TF_PotentialFrondLength_df,
      x,
      x_column = "TF_PalmTrunkHeight",
      y_column = "TF_PotentialFrondLength",
      mode = "continues"
    )
  }))
}

# T_LeafFlush?[Sp1] = GRAPH(T_DOY)
# (1.00, 0.42), (8.00, 0.445), (15.0, 0.505), (22.0, 0.505), (29.0, 0.555), (36.0, 0.56), (43.0, 0.595), (50.0, 0.58), (57.0, 0.61), (64.0, 0.74), (71.0, 0.73), (78.0, 0.895), (85.0, 0.97), (92.0, 0.965), (99.0, 0.97), (106, 0.965), (113, 0.97), (120, 0.955), (127, 0.96), (134, 0.955), (141, 0.955), (148, 0.9), (155, 0.64), (162, 0.515), (169, 0.39), (176, 0.25), (183, 0.145), (190, 0.07), (197, 0.02), (204, 0.015), (211, 0.01), (218, 0.02), (225, 0.01), (232, 0.015), (239, 0.02), (246, 0.00), (253, 0.005), (260, 0.01), (267, 0.035), (274, 0.105), (281, 0.32), (288, 0.45), (295, 0.765), (302, 0.865), (309, 0.91), (316, 0.97), (323, 0.98), (330, 0.985), (337, 0.98), (344, 0.98), (351, 0.985), (358, 0.98), (365, 0.98)
# T_LeafFlush?[Sp2] = GRAPH(T_DOY)
# (1.00, 0.51), (8.00, 0.57), (15.0, 0.61), (22.0, 0.61), (29.0, 0.62), (36.0, 0.685), (43.0, 0.745), (50.0, 0.835), (57.0, 0.89), (64.0, 0.915), (71.0, 0.975), (78.0, 0.985), (85.0, 0.995), (92.0, 0.995), (99.0, 0.985), (106, 1.00), (113, 0.985), (120, 0.99), (127, 1.00), (134, 0.985), (141, 0.995), (148, 0.98), (155, 0.86), (162, 0.745), (169, 0.545), (176, 0.26), (183, 0.15), (190, 0.055), (197, 0.015), (204, 0.00), (211, 0.00), (218, 0.00), (225, 0.00), (232, 0.00), (239, 0.005), (246, 0.00), (253, 0.00), (260, 0.03), (267, 0.04), (274, 0.11), (281, 0.16), (288, 0.215), (295, 0.615), (302, 0.82), (309, 0.96), (316, 0.985), (323, 0.98), (330, 0.955), (337, 0.985), (344, 0.97), (351, 0.965), (358, 0.965), (365, 0.97)
# T_LeafFlush?[Sp3] = GRAPH(T_DOY)
# (1.00, 0.855), (8.00, 0.84), (15.0, 0.805), (22.0, 0.79), (29.0, 0.74), (36.0, 0.72), (43.0, 0.665), (50.0, 0.64), (57.0, 0.625), (64.0, 0.6), (71.0, 0.525), (78.0, 0.465), (85.0, 0.38), (92.0, 0.34), (99.0, 0.31), (106, 0.265), (113, 0.195), (120, 0.185), (127, 0.155), (134, 0.115), (141, 0.06), (148, 0.00), (155, 0.005), (162, 0.00), (169, 0.00), (176, 0.00), (183, 0.00), (190, 0.00), (197, 0.00), (204, 0.00), (211, 0.00), (218, 0.00), (225, 0.00), (232, 0.00), (239, 0.00), (246, 0.015), (253, 0.035), (260, 0.08), (267, 0.57), (274, 0.735), (281, 0.845), (288, 0.93), (295, 0.995), (302, 0.995), (309, 1.00), (316, 1.00), (323, 1.00), (330, 0.995), (337, 1.00), (344, 1.00), (351, 1.00), (358, 1.00), (365, 1.00)

T_LeafFlush_is_df <- data.frame(
  T_LeafFlush_is_1 = c(0.42,0.445,0.505,0.505,0.555,0.56,0.595,0.58,0.61,0.74,0.73,0.895,0.97,0.965,0.97,0.965,0.97,0.955,0.96,0.955,0.955,0.9,0.64,0.515,0.39,0.25,0.145,0.07,0.02,0.015,0.01,0.02,0.01,0.015,0.02,0,0.005,0.01,0.035,0.105,0.32,0.45,0.765,0.865,0.91,0.97,0.98,0.985,0.98,0.98,0.985,0.98,0.98),
  T_LeafFlush_is_2 = c(0.51,0.57,0.61,0.61,0.62,0.685,0.745,0.835,0.89,0.915,0.975,0.985,0.995,0.995,0.985,1,0.985,0.99,1,0.985,0.995,0.98,0.86,0.745,0.545,0.26,0.15,0.055,0.015,0,0,0,0,0,0.005,0,0,0.03,0.04,0.11,0.16,0.215,0.615,0.82,0.96,0.985,0.98,0.955,0.985,0.97,0.965,0.965,0.97),
  T_LeafFlush_is_3 = c(0.855,0.84,0.805,0.79,0.74,0.72,0.665,0.64,0.625,0.6,0.525,0.465,0.38,0.34,0.31,0.265,0.195,0.185,0.155,0.115,0.06,0,0.005,0,0,0,0,0,0,0,0,0,0,0,0,0.015,0.035,0.08,0.57,0.735,0.845,0.93,0.995,0.995,1,1,1,0.995,1,1,1,1,1),
  T_DOY = c(1,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,113,120,127,134,141,148,155,162,169,176,183,190,197,204,211,218,225,232,239,246,253,260,267,274,281,288,295,302,309,316,323,330,337,344,351,358,365)
)

get_T_LeafFlush_is <- function(T_DOY) {
  unlist(lapply(tree_df$tree_id, function(x) {
    get_graph_y(
      T_LeafFlush_is_df,
      T_DOY,
      x_column = "T_DOY",
      y_column = names(T_LeafFlush_is_df)[x],
      mode = "continues"
    )
  }))
}

# T_FruitAllocStage[Tree] = GRAPH(T_Stage[Tree,VegGen])
# (1.00, 0.00), (1.10, 0.04), (1.20, 0.235), (1.30, 0.38), (1.40, 0.525), (1.50, 0.8), (1.60, 0.99), (1.70, 1.00), (1.80, 0.985), (1.90, 0.78), (2.00, 0.555)

T_FruitAllocStage_df <- data.frame(
  T_Stage = seq(1,2, 0.1),
  T_FruitAllocStage = c(0,0.04,0.235,0.38,0.525,0.8,0.99,1,0.985,0.78,0.555)
)

get_T_FruitAllocStage <- function(T_Stage) {
  unlist(lapply(T_Stage, function(x) {
    get_graph_y(
      T_FruitAllocStage_df,
      x,
      x_column = "T_Stage",
      y_column = "T_FruitAllocStage",
      mode = "continues"
    )
  }))
}

# T_GraphLeafFall[Sp1] = GRAPH(T_DOY)
# (1.00, 0.00), (8.00, 0.00), (15.0, 0.00), (22.0, 0.00), (29.0, 0.00), (36.0, 0.00), (43.0, 0.00), (50.0, 0.00), (57.0, 0.00), (64.0, 0.00), (71.0, 0.00), (78.0, 0.00), (85.0, 0.00), (92.0, 0.00), (99.0, 0.00), (106, 0.00), (113, 0.00), (120, 0.00), (127, 0.00), (134, 0.00), (141, 0.00), (148, 0.00), (155, 0.00), (162, 0.00), (169, 0.00), (176, 0.00), (183, 0.00), (190, 0.00), (197, 0.00), (204, 0.00), (211, 0.00), (218, 0.00), (225, 0.00), (232, 0.00), (239, 0.00), (246, 0.2), (253, 0.365), (260, 0.395), (267, 0.405), (274, 0.395), (281, 0.29), (288, 0.205), (295, 0.14), (302, 0.095), (309, 0.025), (316, 0.00), (323, 0.00), (330, 0.01), (337, 0.005), (344, 0.005), (351, 0.00), (358, 0.00), (365, 0.00)
# T_GraphLeafFall[Sp2] = GRAPH(T_DOY)
# (1.00, 0.04), (8.00, 0.005), (15.0, 0.005), (22.0, 0.005), (29.0, 0.01), (36.0, 0.01), (43.0, 0.01), (50.0, 0.01), (57.0, 0.005), (64.0, 0.005), (71.0, 0.005), (78.0, 0.005), (85.0, 0.005), (92.0, 0.00), (99.0, 0.005), (106, 0.005), (113, 0.005), (120, 0.01), (127, 0.015), (134, 0.005), (141, 0.025), (148, 0.01), (155, 0.00), (162, 0.00), (169, 0.015), (176, 0.015), (183, 0.035), (190, 0.045), (197, 0.095), (204, 0.125), (211, 0.175), (218, 0.255), (225, 0.39), (232, 0.495), (239, 0.53), (246, 0.585), (253, 0.55), (260, 0.55), (267, 0.55), (274, 0.555), (281, 0.37), (288, 0.195), (295, 0.01), (302, 0.01), (309, 0.005), (316, 0.00), (323, 0.00), (330, 0.00), (337, 0.00), (344, 0.00), (351, 0.005), (358, 0.025), (365, 0.00)
# T_GraphLeafFall[Sp3] = GRAPH(T_DOY)
# (1.00, 0.01), (8.00, 0.01), (15.0, 0.01), (22.0, 0.01), (29.0, 0.015), (36.0, 0.03), (43.0, 0.025), (50.0, 0.025), (57.0, 0.025), (64.0, 0.03), (71.0, 0.03), (78.0, 0.03), (85.0, 0.05), (92.0, 0.05), (99.0, 0.065), (106, 0.1), (113, 0.12), (120, 0.14), (127, 0.19), (134, 0.475), (141, 0.73), (148, 0.83), (155, 0.975), (162, 0.985), (169, 0.995), (176, 0.99), (183, 1.00), (190, 0.995), (197, 0.945), (204, 0.785), (211, 0.49), (218, 0.00), (225, 0.00), (232, 0.00), (239, 0.00), (246, 0.00), (253, 0.00), (260, 0.00), (267, 0.00), (274, 0.00), (281, 0.00), (288, 0.01), (295, 0.005), (302, 0.005), (309, 0.005), (316, 0.00), (323, 0.00), (330, 0.00), (337, 0.00), (344, 0.00), (351, 0.005), (358, 0.00), (365, 0.01)

T_GraphLeafFall_df <- data.frame(
  T_GraphLeafFall_1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.2,0.365,0.395,0.405,0.395,0.29,0.205,0.14,0.095,0.025,0,0,0.01,0.005,0.005,0,0,0),
  T_GraphLeafFall_2 = c(0.04,0.005,0.005,0.005,0.01,0.01,0.01,0.01,0.005,0.005,0.005,0.005,0.005,0,0.005,0.005,0.005,0.01,0.015,0.005,0.025,0.01,0,0,0.015,0.015,0.035,0.045,0.095,0.125,0.175,0.255,0.39,0.495,0.53,0.585,0.55,0.55,0.55,0.555,0.37,0.195,0.01,0.01,0.005,0,0,0,0,0,0.005,0.025,0),
  T_GraphLeafFall_3 = c(0.01,0.01,0.01,0.01,0.015,0.03,0.025,0.025,0.025,0.03,0.03,0.03,0.05,0.05,0.065,0.1,0.12,0.14,0.19,0.475,0.73,0.83,0.975,0.985,0.995,0.99,1,0.995,0.945,0.785,0.49,0,0,0,0,0,0,0,0,0,0,0.01,0.005,0.005,0.005,0,0,0,0,0,0.005,0,0.01),
  T_DOY = c(1,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,113,120,127,134,141,148,155,162,169,176,183,190,197,204,211,218,225,232,239,246,253,260,267,274,281,288,295,302,309,316,323,330,337,344,351,358,365)
)

get_T_GraphLeafFall <- function(T_DOY) {
  unlist(lapply(tree_df$tree_id, function(x) {
    get_graph_y(
      T_GraphLeafFall_df,
      T_DOY,
      x_column = "T_DOY",
      y_column = names(T_GraphLeafFall_df)[x],
      mode = "continues"
    )
  }))
}

# T_WoodHarvDoY[Sp1] = GRAPH(T_WoodHarvPast[Sp1])
# (0.00, 364), (1.00, 364), (2.00, 364), (3.00, 364), (4.00, 364), (5.00, 364), (6.00, 364), (7.00, 364), (8.00, 364), (9.00, 364), (10.0, 364), (11.0, 364), (12.0, 364), (13.0, 364), (14.0, 364), (15.0, 364), (16.0, 364), (17.0, 364), (18.0, 364), (19.0, 364), (20.0, 364)
# T_WoodHarvDoY[Sp2] = GRAPH(T_WoodHarvPast[Sp2])
# (0.00, 364), (1.00, 364), (2.00, 364), (3.00, 364), (4.00, 364), (5.00, 364), (6.00, 364), (7.00, 364), (8.00, 364), (9.00, 364), (10.0, 364), (11.0, 364), (12.0, 364), (13.0, 364), (14.0, 364), (15.0, 364), (16.0, 364), (17.0, 364), (18.0, 364), (19.0, 364), (20.0, 364)
# T_WoodHarvDoY[Sp3] = GRAPH(T_WoodHarvPast[Sp3])
# (0.00, 364), (1.00, 364), (2.00, 364), (3.00, 364), (4.00, 364), (5.00, 364), (6.00, 364), (7.00, 364), (8.00, 364), (9.00, 364), (10.0, 364), (11.0, 364), (12.0, 364), (13.0, 364), (14.0, 364), (15.0, 364), (16.0, 364), (17.0, 364), (18.0, 364), (19.0, 364), (20.0, 364)

T_WoodHarvDoY_df <- data.frame(
  T_WoodHarvDoY_1 = 364,
  T_WoodHarvDoY_2 = 364,
  T_WoodHarvDoY_3 = 364,
  T_WoodHarvPast = c(0:20)
)

get_T_WoodHarvDoY <- function(T_WoodHarvPast) {
  unlist(lapply(tree_df$tree_id, function(x) {
    get_graph_y(
      T_WoodHarvDoY_df,
      T_WoodHarvPast,
      x_column = "T_WoodHarvPast",
      y_column = names(T_WoodHarvDoY_df)[x],
      mode = "continues"
    )
  }))
}

# T_WoodHarvY[Sp1] = GRAPH(T_WoodHarvPast[Sp1])
# (0.00, 100), (1.00, 100), (2.00, 100), (3.00, 100), (4.00, 100), (5.00, 100), (6.00, 100), (7.00, 100), (8.00, 100), (9.00, 100), (10.0, 100), (11.0, 100), (12.0, 100), (13.0, 100), (14.0, 100), (15.0, 100), (16.0, 100), (17.0, 100), (18.0, 100), (19.0, 100), (20.0, 100)
# T_WoodHarvY[Sp2] = GRAPH(T_WoodHarvPast[Sp2])
# (0.00, 100), (1.00, 100), (2.00, 100), (3.00, 100), (4.00, 100), (5.00, 100), (6.00, 100), (7.00, 100), (8.00, 100), (9.00, 100), (10.0, 100), (11.0, 100), (12.0, 100), (13.0, 100), (14.0, 100), (15.0, 100), (16.0, 100), (17.0, 100), (18.0, 100), (19.0, 100), (20.0, 100)
# T_WoodHarvY[Sp3] = GRAPH(T_WoodHarvPast[Sp3])
# (0.00, 100), (1.00, 100), (2.00, 100), (3.00, 100), (4.00, 100), (5.00, 100), (6.00, 100), (7.00, 100), (8.00, 100), (9.00, 100), (10.0, 100), (11.0, 100), (12.0, 100), (13.0, 100), (14.0, 100), (15.0, 100), (16.0, 100), (17.0, 100), (18.0, 100), (19.0, 100), (20.0, 100)

T_WoodHarvY_df <- data.frame(
  T_WoodHarvY_1 = 100,
  T_WoodHarvY_2 = 100,
  T_WoodHarvY_3 = 100,
  T_WoodHarvPast = c(0:20)
)

get_T_WoodHarvY<- function(T_WoodHarvPast) {
  unlist(lapply(tree_df$tree_id, function(x) {
    get_graph_y(
      T_WoodHarvY_df,
      T_WoodHarvPast,
      x_column = "T_WoodHarvPast",
      y_column = names(T_WoodHarvY_df)[x],
      mode = "continues"
    )
  }))
}

# TF_PotentialStemD[Tree] = GRAPH(TF_CurrentLeafNo[Tree])
# (1.00, 0.81), (4.29, 1.44), (7.57, 2.34), (10.9, 5.40), (14.1, 8.78), (17.4, 14.0), (20.7, 20.1), (24.0, 26.4), (27.3, 30.2), (30.6, 32.7), (33.9, 36.5), (37.1, 39.1), (40.4, 41.8), (43.7, 43.1), (47.0, 46.0), (50.3, 47.3), (53.6, 48.7), (56.9, 50.0), (60.1, 51.4), (63.4, 52.7), (66.7, 54.1), (70.0, 55.4)

TF_PotentialStemD_df <- data.frame(
  TF_CurrentLeafNo = c(1,4.29,7.57,10.9,14.1,17.4,20.7,24,27.3,30.6,33.9,37.1,40.4,43.7,47,50.3,53.6,56.9,60.1,63.4,66.7,70),
  TF_PotentialStemD = c(0.81,1.44,2.34,5.4,8.78,14,20.1,26.4,30.2,32.7,36.5,39.1,41.8,43.1,46,47.3,48.7,50,51.4,52.7,54.1,55.4)
)

get_TF_PotentialStemD<- function(TF_CurrentLeafNo) {
  unlist(lapply(TF_CurrentLeafNo, function(x) {
    get_graph_y(
      TF_PotentialStemD_df,
      x,
      x_column = "TF_CurrentLeafNo",
      y_column = "TF_PotentialStemD",
      mode = "continues"
    )
  }))
}




# T_LfConc[DW,Sp1] = 0*(T_Par1[LfN]+T_Par2[LfN]+T_Par3[LfN])+1
# T_LfConc[DW,Sp2] = 0*(T_Par1[LfN]+T_Par2[LfN]+T_Par3[LfN])+1
# T_LfConc[DW,Sp3] = 0*(T_Par1[LfN]+T_Par2[LfN]+T_Par3[LfN])+1
# T_LfConc[N,Sp1] = (T_Par1[LfN]+0*T_Par2[LfN]+0*T_Par3[LfN])
# T_LfConc[N,Sp2] = (0*T_Par1[LfN]+T_Par2[LfN]+0*T_Par3[LfN])
# T_LfConc[N,Sp3] = (0*T_Par1[LfN]+0*T_Par2[LfN]+T_Par3[LfN])
# T_LfConc[P,Sp1] = (T_Par1[LfP]+0*T_Par2[LfP]+0*T_Par3[LfP])
# T_LfConc[P,Sp2] = (0*T_Par1[LfP]+T_Par2[LfP]+0*T_Par3[LfP])
# T_LfConc[P,Sp3] = (0*T_Par1[LfP]+0*T_Par2[LfP]+T_Par3[LfP])
treepcomp_df$T_LfConc <- 1
treepcomp_df[treepcomp_df$PlantComp == "N",]$T_LfConc <- tree_df$T_LfConc_N
treepcomp_df[treepcomp_df$PlantComp == "P",]$T_LfConc <- tree_df$T_LfConc_P

# T_TwigConc[DW,Sp1] = 0*(T_Par1[TwigN]+T_Par2[TwigN]+T_Par3[TwigN])+1
# T_TwigConc[DW,Sp2] = 0*(T_Par1[TwigN]+T_Par2[TwigN]+T_Par3[TwigN])+1
# T_TwigConc[DW,Sp3] = 0*(T_Par1[TwigN]+T_Par2[TwigN]+T_Par3[TwigN])+1
# T_TwigConc[N,Sp1] = T_Par1[TwigN]+0*T_Par2[TwigN]+0*T_Par3[TwigN]
# T_TwigConc[N,Sp2] = 0*T_Par1[TwigN]+T_Par2[TwigN]+0*T_Par3[TwigN]
# T_TwigConc[N,Sp3] = 0*T_Par1[TwigN]+0*T_Par2[TwigN]+T_Par3[TwigN]
# T_TwigConc[P,Sp1] = T_Par1[TwigP]+0*T_Par2[TwigP]+0*T_Par3[TwigP]
# T_TwigConc[P,Sp2] = 0*T_Par1[TwigP]+T_Par2[TwigP]+0*T_Par3[TwigP]
# T_TwigConc[P,Sp3] = 0*T_Par1[TwigP]+0*T_Par2[TwigP]+T_Par3[TwigP]
treepcomp_df$T_TwigConc <- 1
treepcomp_df[treepcomp_df$PlantComp == "N",]$T_TwigConc <- tree_df$T_ConcTwig_N
treepcomp_df[treepcomp_df$PlantComp == "P",]$T_TwigConc <- tree_df$T_ConcTwig_P

# T_FruitConc[DW,Sp1] = 0*(T_Par1[FruitN]+T_Par2[FruitN]+T_Par3[FruitN])+1
# T_FruitConc[DW,Sp2] = 0*(T_Par1[FruitN]+T_Par2[FruitN]+T_Par3[FruitN])+1
# T_FruitConc[DW,Sp3] = 0*(T_Par1[FruitN]+T_Par2[FruitN]+T_Par3[FruitN])+1
# T_FruitConc[N,Sp1] = T_Par1[FruitN]+0*T_Par2[FruitN]+0*T_Par3[FruitN]
# T_FruitConc[N,Sp2] = 0*T_Par1[FruitN]+T_Par2[FruitN]+0*T_Par3[FruitN]
# T_FruitConc[N,Sp3] = 0*T_Par1[FruitN]+0*T_Par2[FruitN]+T_Par3[FruitN]
# T_FruitConc[P,Sp1] = T_Par1[FruitP]+0*T_Par2[FruitP]+0*T_Par3[FruitP]
# T_FruitConc[P,Sp2] = 0*T_Par1[FruitP]+T_Par2[FruitP]+0*T_Par3[FruitP]
# T_FruitConc[P,Sp3] = 0*T_Par1[FruitP]+0*T_Par2[FruitP]+T_Par3[FruitP]
treepcomp_df$T_FruitConc <- 1
treepcomp_df[treepcomp_df$PlantComp == "N",]$T_FruitConc <- tree_df$T_ConcFruit_N
treepcomp_df[treepcomp_df$PlantComp == "P",]$T_FruitConc <- tree_df$T_ConcFruit_P

# T_GroResConc[DW,Sp1] = 0*(T_Par1[GroResN]+T_Par2[GroResN]+T_Par3[GroResN])+1
# T_GroResConc[DW,Sp2] = 0*(T_Par1[GroResN]+T_Par2[GroResN]+T_Par3[GroResN])+1
# T_GroResConc[DW,Sp3] = 0*(T_Par1[GroResN]+T_Par2[GroResN]+T_Par3[GroResN])+1
# T_GroResConc[N,Sp1] = T_Par1[GroResN]+0*T_Par2[GroResN]+0*T_Par3[GroResN]
# T_GroResConc[N,Sp2] = 0*T_Par1[GroResN]+T_Par2[GroResN]+0*T_Par3[GroResN]
# T_GroResConc[N,Sp3] = 0*T_Par1[GroResN]+0*T_Par2[GroResN]+T_Par3[GroResN]
# T_GroResConc[P,Sp1] = T_Par1[GroResP]+0*T_Par2[GroResP]+0*T_Par3[GroResP]
# T_GroResConc[P,Sp2] = 0*T_Par1[GroResP]+T_Par2[GroResP]+0*T_Par3[GroResP]
# T_GroResConc[P,Sp3] = 0*T_Par1[GroResP]+0*T_Par2[GroResP]+T_Par3[GroResP]
treepcomp_df$T_GroResConc <- 1
treepcomp_df[treepcomp_df$PlantComp == "N",]$T_GroResConc <- tree_df$T_ConcGroRes_N
treepcomp_df[treepcomp_df$PlantComp == "P",]$T_GroResConc <- tree_df$T_ConcGroRes_P

# T_RtConc[DW,Sp1] = 0*(T_Par1[RtN]+T_Par2[RtN]+T_Par3[RtN])+1
# T_RtConc[DW,Sp2] = 0*(T_Par1[RtN]+T_Par2[RtN]+T_Par3[RtN])+1
# T_RtConc[DW,Sp3] = 0*(T_Par1[RtN]+T_Par2[RtN]+T_Par3[RtN])+1
# T_RtConc[N,Sp1] = T_Par1[RtN]+0*T_Par2[RtN]+0*T_Par3[RtN]
# T_RtConc[N,Sp2] = 0*T_Par1[RtN]+T_Par2[RtN]+0*T_Par3[RtN]
# T_RtConc[N,Sp3] = 0*T_Par1[RtN]+0*T_Par2[RtN]+T_Par3[RtN]
# T_RtConc[P,Sp1] = T_Par1[RtP]+0*T_Par2[RtP]+0*T_Par3[RtP]
# T_RtConc[P,Sp2] = 0*T_Par1[RtP]+T_Par2[RtP]+0*T_Par3[RtP]
# T_RtConc[P,Sp3] = 0*T_Par1[RtP]+0*T_Par2[RtP]+T_Par3[RtP]

treepcomp_df$T_RtConc <- 1
treepcomp_df[treepcomp_df$PlantComp == "N",]$T_RtConc <- tree_df$T_ConcRt_N
treepcomp_df[treepcomp_df$PlantComp == "P",]$T_RtConc <- tree_df$T_ConcRt_P

# T_WoodConc[DW,Sp1] = 0*(T_Par1[WoodN]+T_Par2[WoodN]+T_Par3[WoodN])+1
# T_WoodConc[DW,Sp2] = 0*(T_Par1[WoodN]+T_Par2[WoodN]+T_Par3[WoodN])+1
# T_WoodConc[DW,Sp3] = 0*(T_Par1[WoodN]+T_Par2[WoodN]+T_Par3[WoodN])+1
# T_WoodConc[N,Sp1] = T_Par1[WoodN]+0*T_Par2[WoodN]+0*T_Par3[WoodN]
# T_WoodConc[N,Sp2] = 0*T_Par1[WoodN]+T_Par2[WoodN]+0*T_Par3[WoodN]
# T_WoodConc[N,Sp3] = 0*T_Par1[WoodN]+0*T_Par2[WoodN]+T_Par3[WoodN]
# T_WoodConc[P,Sp1] = T_Par1[WoodP]+0*T_Par2[WoodP]+0*T_Par3[WoodP]
# T_WoodConc[P,Sp2] = 0*T_Par1[WoodP]+T_Par2[WoodP]+0*T_Par3[WoodP]
# T_WoodConc[P,Sp3] = 0*T_Par1[WoodP]+0*T_Par2[WoodP]+T_Par3[WoodP]

treepcomp_df$T_WoodConc <- 1
treepcomp_df[treepcomp_df$PlantComp == "N",]$T_WoodConc <- tree_df$T_ConcWood_N
treepcomp_df[treepcomp_df$PlantComp == "P",]$T_WoodConc <- tree_df$T_ConcWood_P


### PlantComp array ####################

# plantComp_df <- data.frame(comp_id = 1:3, comp = c("Dw", "P", "N"))
# treepcomp_df <- data.frame(tree_id = rep(tree_df$tree_id, nrow(plantComp_df)))
# treepcomp_df$comp_id <- rep(plantComp_df$comp_id, each = nrow(tree_df))
# treepcomp_df$comp <- rep(plantComp_df$comp, each = nrow(tree_df))
#
# # INIT T_LfTwig[PlantComp,Tree] = 0
# treepcomp_df$T_LfTwig <- 0
# # INIT T_SapWood[PlantComp,Tree] = 0
# treepcomp_df$T_SapWood <- 0
# # T_GroRes[PlantComp,Tree] = 0
# treepcomp_df$T_GroRes <- pars$T_par$T_GroRes

### BufValues array ####################

# TW_DryFactRangeInit[BufValues] = CW_DryFactRangeInit[BufValues]
buf_df$TW_DryFactRangeInit <- buf_df$CW_DryFactRangeInit

treebuf_df$TW_DryFactRangeInit <- rep(buf_df$TW_DryFactRangeInit, each = nrow(tree_df))
treebuf_df$TW_DemActSubtract <- rep(buf_df$TW_DemActSubtract, each = nrow(tree_df))


### C par #############################

# C_BiomHarvestDoY = GRAPH(C_BiomHarvestPast)
# (0.00, 281), (1.00, 283), (2.00, 285), (3.00, 287), (4.00, 289), (5.00, 291), (6.00, 293), (7.00, 295), (8.00, 297), (9.00, 299), (10.0, 301), (11.0, 303), (12.0, 305), (13.0, 307), (14.0, 146), (15.0, 146), (16.0, 146), (17.0, 146), (18.0, 146), (19.0, 146), (20.0, 146), (21.0, 146), (22.0, 146), (23.0, 146), (24.0, 146), (25.0, 146), (26.0, 146), (27.0, 146), (28.0, 146), (29.0, 146), (30.0, 146), (31.0, 146), (32.0, 146), (33.0, 146), (34.0, 146), (35.0, 146), (36.0, 146), (37.0, 146), (38.0, 146), (39.0, 146), (40.0, 146)

C_BiomHarvestDoY_df <- data.frame(
  C_BiomHarvestPast = c(0:40),
  C_BiomHarvestDoY = c(281,283,285,287,289,291,293,295,297,299,301,303,305,307,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146,146)
)

# C_BiomHarvestFracD = GRAPH(C_BiomHarvestPast)
# (0.00, 1.00), (1.00, 1.00), (2.00, 1.00), (3.00, 1.00), (4.00, 1.00), (5.00, 1.00), (6.00, 1.00), (7.00, 1.00), (8.00, 1.00), (9.00, 1.00), (10.0, 1.00), (11.0, 1.00), (12.0, 1.00), (13.0, 1.00), (14.0, 1.00), (15.0, 1.00), (16.0, 1.00), (17.0, 1.00), (18.0, 1.00), (19.0, 1.00), (20.0, 1.00), (21.0, 1.00), (22.0, 1.00), (23.0, 1.00), (24.0, 1.00), (25.0, 1.00), (26.0, 1.00), (27.0, 1.00), (28.0, 1.00), (29.0, 1.00), (30.0, 1.00), (31.0, 1.00), (32.0, 1.00), (33.0, 1.00), (34.0, 1.00), (35.0, 1.00), (36.0, 1.00), (37.0, 1.00), (38.0, 1.00), (39.0, 1.00), (40.0, 1.00)
C_BiomHarvestFracD_df <- data.frame(
  C_BiomHarvestPast = c(0:40),
  C_BiomHarvestFracD = 1
)

# C_BiomHarvestY = GRAPH(C_BiomHarvestPast)
# (0.00, 100), (1.00, 100), (2.00, 100), (3.00, 100), (4.00, 100), (5.00, 100), (6.00, 100), (7.00, 100), (8.00, 100), (9.00, 100), (10.0, 100), (11.0, 100), (12.0, 100), (13.0, 100), (14.0, 100), (15.0, 100), (16.0, 100), (17.0, 100), (18.0, 100), (19.0, 100), (20.0, 100), (21.0, 100), (22.0, 100), (23.0, 100), (24.0, 100), (25.0, 100), (26.0, 100), (27.0, 100), (28.0, 100), (29.0, 100), (30.0, 100), (31.0, 100), (32.0, 100), (33.0, 100), (34.0, 100), (35.0, 100), (36.0, 100), (37.0, 100), (38.0, 100), (39.0, 100), (40.0, 100)
C_BiomHarvestY_df <- data.frame(
  C_BiomHarvestPast = c(0:40),
  C_BiomHarvestY = 100
)



### Soil par ##############################

S_SoilProp_par <- get_par_xls_list(S_Unit, "S_SoilProp")

S_SoilProp_var <- list(
  S_KsatInitV = c("Ksat1", "Ksat2", "Ksat3", "Ksat4"),
  S_KsatDefV = c("KsatD1", "KsatD2", "KsatD3", "KsatD4"),
  W_FieldCapKcrit = c("FieldC1", "FieldC1", "FieldC1", "FieldC1"),
  W_ThetaInacc = c("Inacc1", "Inacc2", "Inacc3", "Inacc4"),
  W_BDLayer = c("BDLayer1", "BDLayer2", "BDLayer3", "BDLayer4"),
  SiltLayer = c("SiltLayer1", "SiltLayer2", "SiltLayer3", "SiltLayer4"),
  ClayLayer = c("ClayLayer1", "ClayLayer2", "ClayLayer3", "ClayLayer4"),
  W_ThetaSat = c("ThetaSat1", "ThetaSat2", "ThetaSat3", "ThetaSat4"),
  W_Alpha = c("Alpha1", "Alpha2", "Alpha3", "Alpha4"),
  W_n = c("n1", "n2", "n3", "n4")
)

S_df <- as.data.frame(lapply(S_SoilProp_var, function(x) {
  unlist(S_SoilProp_par[x])
}))

layer_df[names(S_df)] <- S_df
layer_df$S_KsatInitV <- layer_df$S_KsatInitV * 10
layer_df$S_KsatDefV <- layer_df$S_KsatDefV * 10

S_zl_df <- layer_df[c("S_KsatInitV",
                      "S_KsatDefV",
                      "W_FieldCapKcrit",
                      "W_ThetaInacc")]
S_zl_df <- S_zl_df[rep(seq_len(nrow(S_zl_df)), each = nzone), ]
zonelayer_df[names(S_zl_df)] <- S_zl_df

Theta_x <- seq(0.01, 0.6, 0.59 / 50)
W_PhiTheta_df <- parxls_df[1:length(Theta_x), c("W_PhiTheta1", "W_PhiTheta2", "W_PhiTheta3", "W_PhiTheta4")]
W_PhiTheta_df$Theta <- Theta_x
W_PTheta_df <- parxls_df[1:length(Theta_x), c("W_PTheta1", "W_PTheta2", "W_PTheta3", "W_PTheta4")]
W_PTheta_df$Theta <- Theta_x

P_x <- seq(-250, 0, 10)
W_ThetaP_df <- parxls_df[1:length(P_x), c("W_ThetaP1", "W_ThetaP2", "W_ThetaP3", "W_ThetaP4")]
W_ThetaP_df$P <- P_x

TW_pFPotRhizOpt_x <- seq(0, 5, 0.2)
TW_PhiPot_meta_df <- data.frame(
  varcol = c(
    "TW_PhiPotT1L1",
    "TW_PhiPotT2L1",
    "TW_PhiPotT3L1",
    "TW_PhiPotT1L2",
    "TW_PhiPotT2L2",
    "TW_PhiPotT3L2",
    "TW_PhiPotT1L3",
    "TW_PhiPotT2L3",
    "TW_PhiPotT3L3",
    "TW_PhiPotT1L4",
    "TW_PhiPotT2L4",
    "TW_PhiPotT3L4"
  ),
  tree_id = rep(1:ntree, nlayer),
  layer = rep(1:nlayer, each = ntree)
)
TW_PhiPot_df <- parxls_df[1:length(TW_pFPotRhizOpt_x), TW_PhiPot_meta_df$varcol]
TW_PhiPot_df$TW_pFPotRhizOpt <- TW_pFPotRhizOpt_x

get_TW_PhiPot <- function(TW_pFPotRhizOpt, layer, tree_id) {
  varcol <- TW_PhiPot_meta_df[TW_PhiPot_meta_df$layer == layer &
                                TW_PhiPot_meta_df$tree_id == tree_id, ]$varcol
  get_graph_y(
    TW_PhiPot_df,
    TW_pFPotRhizOpt,
    x_column = "TW_pFPotRhizOpt",
    y_column = varcol,
    mode = "continues"
  )
}


zone_df$CW_DryFactRangePower <- pars$CW_par$CW_DryFactRangePowerStart


# W_PhiPMS1[Zone] = GRAPH(W_pFMS[Zone])
# (0.00, 0.702), (0.2, 0.481), (0.4, 0.323), (0.6, 0.214), (0.8, 0.141), (1.00, 0.0924), (1.20, 0.0605), (1.40, 0.0396), (1.60, 0.0259), (1.80, 0.017), (2.00, 0.0111), (2.20, 0.0073), (2.40, 0.0048), (2.60, 0.0031), (2.80, 0.0021), (3.00, 0.0014), (3.20, 0.0009), (3.40, 0.0006), (3.60, 0.0004), (3.80, 0.0002), (4.00, 0.0002), (4.20, 0.0001), (4.40, 0.0001), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPMS2[Zone] = GRAPH(W_pFMS[Zone])
# (0.00, 1.04), (0.2, 0.704), (0.4, 0.463), (0.6, 0.3), (0.8, 0.192), (1.00, 0.123), (1.20, 0.078), (1.40, 0.0495), (1.60, 0.0314), (1.80, 0.0199), (2.00, 0.0126), (2.20, 0.008), (2.40, 0.0051), (2.60, 0.0032), (2.80, 0.002), (3.00, 0.0013), (3.20, 0.0008), (3.40, 0.0005), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.0001), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPMS3[Zone] = GRAPH(W_pFMS[Zone])
# (0.00, 1.00), (0.2, 0.672), (0.4, 0.438), (0.6, 0.281), (0.8, 0.178), (1.00, 0.113), (1.20, 0.071), (1.40, 0.0446), (1.60, 0.028), (1.80, 0.0176), (2.00, 0.011), (2.20, 0.0069), (2.40, 0.0043), (2.60, 0.0027), (2.80, 0.0017), (3.00, 0.0011), (3.20, 0.0007), (3.40, 0.0004), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.00), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPMS4[Zone] = GRAPH(W_pFMS[Zone])
# (0.00, 1.00), (0.2, 0.672), (0.4, 0.438), (0.6, 0.281), (0.8, 0.178), (1.00, 0.113), (1.20, 0.071), (1.40, 0.0446), (1.60, 0.028), (1.80, 0.0176), (2.00, 0.011), (2.20, 0.0069), (2.40, 0.0043), (2.60, 0.0027), (2.80, 0.0017), (3.00, 0.0011), (3.20, 0.0007), (3.40, 0.0004), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.00), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPMW1[Zone] = GRAPH(W_pFMW[Zone])
# (0.00, 0.702), (0.2, 0.481), (0.4, 0.323), (0.6, 0.214), (0.8, 0.141), (1.00, 0.0924), (1.20, 0.0605), (1.40, 0.0396), (1.60, 0.0259), (1.80, 0.017), (2.00, 0.0111), (2.20, 0.0073), (2.40, 0.0048), (2.60, 0.0031), (2.80, 0.0021), (3.00, 0.0014), (3.20, 0.0009), (3.40, 0.0006), (3.60, 0.0004), (3.80, 0.0002), (4.00, 0.0002), (4.20, 0.0001), (4.40, 0.0001), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPMW2[Zone] = GRAPH(W_pFMW[Zone])
# (0.00, 1.04), (0.2, 0.704), (0.4, 0.463), (0.6, 0.3), (0.8, 0.192), (1.00, 0.123), (1.20, 0.078), (1.40, 0.0495), (1.60, 0.0314), (1.80, 0.0199), (2.00, 0.0126), (2.20, 0.008), (2.40, 0.0051), (2.60, 0.0032), (2.80, 0.002), (3.00, 0.0013), (3.20, 0.0008), (3.40, 0.0005), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.0001), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPMW3[Zone] = GRAPH(W_pFMW[Zone])
# (0.00, 1.00), (0.2, 0.672), (0.4, 0.438), (0.6, 0.281), (0.8, 0.178), (1.00, 0.113), (1.20, 0.071), (1.40, 0.0446), (1.60, 0.028), (1.80, 0.0176), (2.00, 0.011), (2.20, 0.0069), (2.40, 0.0043), (2.60, 0.0027), (2.80, 0.0017), (3.00, 0.0011), (3.20, 0.0007), (3.40, 0.0004), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.00), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPMW4[Zone] = GRAPH(W_pFMW[Zone])
# (0.00, 1.00), (4.00, 0.672), (8.00, 0.438), (12.0, 0.281), (16.0, 0.178), (20.0, 0.113), (24.0, 0.071), (28.0, 0.0446), (32.0, 0.028), (36.0, 0.0176), (40.0, 0.011), (44.0, 0.0069), (48.0, 0.0043), (52.0, 0.0027), (56.0, 0.0017), (60.0, 0.0011), (64.0, 0.0007), (68.0, 0.0004), (72.0, 0.0003), (76.0, 0.0002), (80.0, 0.0001), (84.0, 0.0001), (88.0, 0.00), (92.0, 0.00), (96.0, 0.00), (100, 0.00)
# W_PhiPS1[Zone] = GRAPH(W_pFS[Zone])
# (0.00, 0.702), (0.2, 0.481), (0.4, 0.323), (0.6, 0.214), (0.8, 0.141), (1.00, 0.0924), (1.20, 0.0605), (1.40, 0.0396), (1.60, 0.0259), (1.80, 0.017), (2.00, 0.0111), (2.20, 0.0073), (2.40, 0.0048), (2.60, 0.0031), (2.80, 0.0021), (3.00, 0.0014), (3.20, 0.0009), (3.40, 0.0006), (3.60, 0.0004), (3.80, 0.0002), (4.00, 0.0002), (4.20, 0.0001), (4.40, 0.0001), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPS2[Zone] = GRAPH(W_pFS[Zone])
# (0.00, 1.04), (0.2, 0.704), (0.4, 0.463), (0.6, 0.3), (0.8, 0.192), (1.00, 0.123), (1.20, 0.078), (1.40, 0.0495), (1.60, 0.0314), (1.80, 0.0199), (2.00, 0.0126), (2.20, 0.008), (2.40, 0.0051), (2.60, 0.0032), (2.80, 0.002), (3.00, 0.0013), (3.20, 0.0008), (3.40, 0.0005), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.0001), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPS3[Zone] = GRAPH(W_pFS[Zone])
# (0.00, 1.00), (0.2, 0.672), (0.4, 0.438), (0.6, 0.281), (0.8, 0.178), (1.00, 0.113), (1.20, 0.071), (1.40, 0.0446), (1.60, 0.028), (1.80, 0.0176), (2.00, 0.011), (2.20, 0.0069), (2.40, 0.0043), (2.60, 0.0027), (2.80, 0.0017), (3.00, 0.0011), (3.20, 0.0007), (3.40, 0.0004), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.00), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPS4[Zone] = GRAPH(W_pFS[Zone])
# (0.00, 1.00), (0.2, 0.672), (0.4, 0.438), (0.6, 0.281), (0.8, 0.178), (1.00, 0.113), (1.20, 0.071), (1.40, 0.0446), (1.60, 0.028), (1.80, 0.0176), (2.00, 0.011), (2.20, 0.0069), (2.40, 0.0043), (2.60, 0.0027), (2.80, 0.0017), (3.00, 0.0011), (3.20, 0.0007), (3.40, 0.0004), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.00), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPW1[Zone] = GRAPH(W_pFW[Zone])
# (0.00, 0.702), (0.2, 0.481), (0.4, 0.323), (0.6, 0.214), (0.8, 0.141), (1.00, 0.0924), (1.20, 0.0605), (1.40, 0.0396), (1.60, 0.0259), (1.80, 0.017), (2.00, 0.0111), (2.20, 0.0073), (2.40, 0.0048), (2.60, 0.0031), (2.80, 0.0021), (3.00, 0.0014), (3.20, 0.0009), (3.40, 0.0006), (3.60, 0.0004), (3.80, 0.0002), (4.00, 0.0002), (4.20, 0.0001), (4.40, 0.0001), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPW2[Zone] = GRAPH(W_pFW[Zone])
# (0.00, 1.04), (4.00, 0.704), (8.00, 0.463), (12.0, 0.3), (16.0, 0.192), (20.0, 0.123), (24.0, 0.078), (28.0, 0.0495), (32.0, 0.0314), (36.0, 0.0199), (40.0, 0.0126), (44.0, 0.008), (48.0, 0.0051), (52.0, 0.0032), (56.0, 0.002), (60.0, 0.0013), (64.0, 0.0008), (68.0, 0.0005), (72.0, 0.0003), (76.0, 0.0002), (80.0, 0.0001), (84.0, 0.0001), (88.0, 0.0001), (92.0, 0.00), (96.0, 0.00), (100, 0.00)
# W_PhiPW3[Zone] = GRAPH(W_pFW[Zone])
# (0.00, 1.00), (0.2, 0.672), (0.4, 0.438), (0.6, 0.281), (0.8, 0.178), (1.00, 0.113), (1.20, 0.071), (1.40, 0.0446), (1.60, 0.028), (1.80, 0.0176), (2.00, 0.011), (2.20, 0.0069), (2.40, 0.0043), (2.60, 0.0027), (2.80, 0.0017), (3.00, 0.0011), (3.20, 0.0007), (3.40, 0.0004), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.00), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiPW4[Zone] = GRAPH(W_pFW[Zone])
# (0.00, 1.00), (0.2, 0.672), (0.4, 0.438), (0.6, 0.281), (0.8, 0.178), (1.00, 0.113), (1.20, 0.071), (1.40, 0.0446), (1.60, 0.028), (1.80, 0.0176), (2.00, 0.011), (2.20, 0.0069), (2.40, 0.0043), (2.60, 0.0027), (2.80, 0.0017), (3.00, 0.0011), (3.20, 0.0007), (3.40, 0.0004), (3.60, 0.0003), (3.80, 0.0002), (4.00, 0.0001), (4.20, 0.0001), (4.40, 0.00), (4.60, 0.00), (4.80, 0.00), (5.00, 0.00)
# W_PhiTheta1[Zone] = GRAPH(W_Theta1[Zone])
# (0.01, 0.00), (0.0218, 0.00), (0.0336, 0.00), (0.0454, 0.00), (0.0572, 0.00), (0.069, 0.00), (0.0808, 0.00), (0.0926, 0.00), (0.104, 0.00), (0.116, 0.00), (0.128, 0.00), (0.14, 0.00), (0.152, 0.00), (0.163, 0.00), (0.175, 0.00), (0.187, 0.00), (0.199, 0.00), (0.211, 0.00), (0.222, 0.0008), (0.234, 0.0062), (0.246, 0.018), (0.258, 0.034), (0.27, 0.0644), (0.281, 0.0985), (0.293, 0.122), (0.305, 0.186), (0.317, 0.23), (0.329, 0.283), (0.34, 0.348), (0.352, 0.516), (0.364, 0.625), (0.376, 0.702), (0.388, 0.702), (0.399, 0.702), (0.411, 0.702), (0.423, 0.702), (0.435, 0.702), (0.447, 0.702), (0.458, 0.702), (0.47, 0.702), (0.482, 0.702), (0.494, 0.702), (0.506, 0.702), (0.517, 0.702), (0.529, 0.702), (0.541, 0.702), (0.553, 0.702), (0.565, 0.702), (0.576, 0.702), (0.588, 0.702), (0.6, 0.702)
# W_PhiTheta2[Zone] = GRAPH(W_Theta2[Zone])
# (0.01, 0.00), (0.0218, 0.00), (0.0336, 0.00), (0.0454, 0.00), (0.0572, 0.00), (0.069, 0.00), (0.0808, 0.00), (0.0926, 0.00), (0.104, 0.00), (0.116, 0.00), (0.128, 0.00), (0.14, 0.00), (0.152, 0.00), (0.163, 0.00), (0.175, 0.00), (0.187, 0.00), (0.199, 0.00), (0.211, 0.0005), (0.222, 0.0054), (0.234, 0.0168), (0.246, 0.0334), (0.258, 0.0662), (0.27, 0.105), (0.281, 0.164), (0.293, 0.206), (0.305, 0.258), (0.317, 0.402), (0.329, 0.499), (0.34, 0.617), (0.352, 0.758), (0.364, 0.925), (0.376, 1.04), (0.388, 1.04), (0.399, 1.04), (0.411, 1.04), (0.423, 1.04), (0.435, 1.04), (0.447, 1.04), (0.458, 1.04), (0.47, 1.04), (0.482, 1.04), (0.494, 1.04), (0.506, 1.04), (0.517, 1.04), (0.529, 1.04), (0.541, 1.04), (0.553, 1.04), (0.565, 1.04), (0.576, 1.04), (0.588, 1.04), (0.6, 1.04)
# W_PhiTheta3[Zone] = GRAPH(W_Theta3[Zone])
# (0.01, 0.00), (0.0218, 0.00), (0.0336, 0.00), (0.0454, 0.00), (0.0572, 0.00), (0.069, 0.00), (0.0808, 0.00), (0.0926, 0.00), (0.104, 0.00), (0.116, 0.00), (0.128, 0.00), (0.14, 0.00), (0.152, 0.00), (0.163, 0.00), (0.175, 0.00), (0.187, 0.00), (0.199, 0.00), (0.211, 0.0001), (0.222, 0.0029), (0.234, 0.0117), (0.246, 0.0236), (0.258, 0.0476), (0.27, 0.076), (0.281, 0.121), (0.293, 0.153), (0.305, 0.241), (0.317, 0.303), (0.329, 0.38), (0.34, 0.474), (0.352, 0.589), (0.364, 0.888), (0.376, 1.00), (0.388, 1.00), (0.399, 1.00), (0.411, 1.00), (0.423, 1.00), (0.435, 1.00), (0.447, 1.00), (0.458, 1.00), (0.47, 1.00), (0.482, 1.00), (0.494, 1.00), (0.506, 1.00), (0.517, 1.00), (0.529, 1.00), (0.541, 1.00), (0.553, 1.00), (0.565, 1.00), (0.576, 1.00), (0.588, 1.00), (0.6, 1.00)
# W_PhiTheta4[Zone] = GRAPH(W_Theta4[Zone]
# (0.01, 0.00), (0.0218, 0.00), (0.0336, 0.00), (0.0454, 0.00), (0.0572, 0.00), (0.069, 0.00), (0.0808, 0.00), (0.0926, 0.00), (0.104, 0.00), (0.116, 0.00), (0.128, 0.00), (0.14, 0.00), (0.152, 0.00), (0.163, 0.00), (0.175, 0.00), (0.187, 0.00), (0.199, 0.00), (0.211, 0.0001), (0.222, 0.0029), (0.234, 0.0117), (0.246, 0.0237), (0.258, 0.0476), (0.27, 0.076), (0.281, 0.121), (0.293, 0.153), (0.305, 0.241), (0.317, 0.303), (0.329, 0.38), (0.34, 0.474), (0.352, 0.589), (0.364, 0.888), (0.376, 1.00), (0.388, 1.00), (0.399, 1.00), (0.411, 1.00), (0.423, 1.00), (0.435, 1.00), (0.447, 1.00), (0.458, 1.00), (0.47, 1.00), (0.482, 1.00), (0.494, 1.00), (0.506, 1.00), (0.517, 1.00), (0.529, 1.00), (0.541, 1.00), (0.553, 1.00), (0.565, 1.00), (0.576, 1.00), (0.588, 1.00), (0.6, 1.00)
# W_PhiPRhizRange[Zone,BufValues] = GRAPH(CW_pF_range[Zone,BufValues])
# (0.00, 283), (0.2, 259), (0.4, 196), (0.6, 160), (0.8, 126), (1.00, 94.0), (1.20, 67.6), (1.40, 47.1), (1.60, 32.1), (1.80, 21.5), (2.00, 14.3), (2.20, 11.7), (2.40, 6.24), (2.60, 4.11), (2.80, 2.70), (3.00, 1.78), (3.20, 1.17), (3.40, 0.767), (3.60, 0.502), (3.80, 0.329), (4.00, 0.214), (4.20, 0.139), (4.40, 0.0887), (4.60, 0.056), (4.80, 0.0343), (5.00, 0.0201)


#TODO: to be checked for X range of W_PhiPMW4 and W_PhiPW2. it has X range of 0-100 instead 0-0.2 as the series of similar variables on the STELLA model (?). Assumed as typos for time being, and generalized to use x = 0-0.2 for all series   
# actual X axis for W_PhiPMW4 and W_PhiPW2 is c(0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100)
W_PhiP_df <- data.frame(
  W_pF=  seq(0, 5, 0.2),
  W_PhiPMS1 = c(0.702,0.481,0.323,0.214,0.141,0.0924,0.0605,0.0396,0.0259,0.017,0.0111,0.0073,0.0048,0.0031,0.0021,0.0014,9e-04,6e-04,4e-04,2e-04,2e-04,1e-04,1e-04,0,0,0),
  W_PhiPMS2 = c(1.04,0.704,0.463,0.3,0.192,0.123,0.078,0.0495,0.0314,0.0199,0.0126,0.008,0.0051,0.0032,0.002,0.0013,8e-04,5e-04,3e-04,2e-04,1e-04,1e-04,1e-04,0,0,0),
  W_PhiPMS3 = c(1,0.672,0.438,0.281,0.178,0.113,0.071,0.0446,0.028,0.0176,0.011,0.0069,0.0043,0.0027,0.0017,0.0011,7e-04,4e-04,3e-04,2e-04,1e-04,1e-04,0,0,0,0),
  W_PhiPMS4 = c(1,0.672,0.438,0.281,0.178,0.113,0.071,0.0446,0.028,0.0176,0.011,0.0069,0.0043,0.0027,0.0017,0.0011,7e-04,4e-04,3e-04,2e-04,1e-04,1e-04,0,0,0,0),
  W_PhiPMW1 = c(0.702,0.481,0.323,0.214,0.141,0.0924,0.0605,0.0396,0.0259,0.017,0.0111,0.0073,0.0048,0.0031,0.0021,0.0014,9e-04,6e-04,4e-04,2e-04,2e-04,1e-04,1e-04,0,0,0),
  W_PhiPMW2 = c(1.04,0.704,0.463,0.3,0.192,0.123,0.078,0.0495,0.0314,0.0199,0.0126,0.008,0.0051,0.0032,0.002,0.0013,8e-04,5e-04,3e-04,2e-04,1e-04,1e-04,1e-04,0,0,0),
  W_PhiPMW3 = c(1,0.672,0.438,0.281,0.178,0.113,0.071,0.0446,0.028,0.0176,0.011,0.0069,0.0043,0.0027,0.0017,0.0011,7e-04,4e-04,3e-04,2e-04,1e-04,1e-04,0,0,0,0),
  W_PhiPMW4 = c(1,0.672,0.438,0.281,0.178,0.113,0.071,0.0446,0.028,0.0176,0.011,0.0069,0.0043,0.0027,0.0017,0.0011,7e-04,4e-04,3e-04,2e-04,1e-04,1e-04,0,0,0,0),
  W_PhiPS1 = c(0.702,0.481,0.323,0.214,0.141,0.0924,0.0605,0.0396,0.0259,0.017,0.0111,0.0073,0.0048,0.0031,0.0021,0.0014,9e-04,6e-04,4e-04,2e-04,2e-04,1e-04,1e-04,0,0,0),
  W_PhiPS2 = c(1.04,0.704,0.463,0.3,0.192,0.123,0.078,0.0495,0.0314,0.0199,0.0126,0.008,0.0051,0.0032,0.002,0.0013,8e-04,5e-04,3e-04,2e-04,1e-04,1e-04,1e-04,0,0,0),
  W_PhiPS3 = c(1,0.672,0.438,0.281,0.178,0.113,0.071,0.0446,0.028,0.0176,0.011,0.0069,0.0043,0.0027,0.0017,0.0011,7e-04,4e-04,3e-04,2e-04,1e-04,1e-04,0,0,0,0),
  W_PhiPS4 = c(1,0.672,0.438,0.281,0.178,0.113,0.071,0.0446,0.028,0.0176,0.011,0.0069,0.0043,0.0027,0.0017,0.0011,7e-04,4e-04,3e-04,2e-04,1e-04,1e-04,0,0,0,0),
  W_PhiPW1 = c(0.702,0.481,0.323,0.214,0.141,0.0924,0.0605,0.0396,0.0259,0.017,0.0111,0.0073,0.0048,0.0031,0.0021,0.0014,9e-04,6e-04,4e-04,2e-04,2e-04,1e-04,1e-04,0,0,0),
  W_PhiPW2 = c(1.04,0.704,0.463,0.3,0.192,0.123,0.078,0.0495,0.0314,0.0199,0.0126,0.008,0.0051,0.0032,0.002,0.0013,8e-04,5e-04,3e-04,2e-04,1e-04,1e-04,1e-04,0,0,0),
  W_PhiPW3 = c(1,0.672,0.438,0.281,0.178,0.113,0.071,0.0446,0.028,0.0176,0.011,0.0069,0.0043,0.0027,0.0017,0.0011,7e-04,4e-04,3e-04,2e-04,1e-04,1e-04,0,0,0,0),
  W_PhiPW4 = c(1,0.672,0.438,0.281,0.178,0.113,0.071,0.0446,0.028,0.0176,0.011,0.0069,0.0043,0.0027,0.0017,0.0011,7e-04,4e-04,3e-04,2e-04,1e-04,1e-04,0,0,0,0)
)

W_PhiP_meta_df <- data.frame(
  varcol = names(W_PhiP_df)[-1],
  layer = rep(1:4, 4),
  water = rep(soil_water_id, each = 4)
)

get_W_PhiP <- function(W_pF, layer, water) {
  varcol <- W_PhiP_meta_df[W_PhiP_meta_df$layer == layer &
                             W_PhiP_meta_df$water == water, ]$varcol
  get_graph_y(
    W_PhiP_df,
    W_pF,
    x_column = "W_pF",
    y_column = varcol,
    mode = "continues"
  )
}


# MC2_CSlowFrac = GRAPH(if MC2_SomInitType= 3 then Mc2_CorgInitMeth3/Mc2_CrefMeth3 else MC2_CorgpCref)
# (0.00, 0.025), (0.2, 0.195), (0.4, 0.325), (0.6, 0.415), (0.8, 0.5), (1.00, 0.55), (1.20, 0.585), (1.40, 0.615), (1.60, 0.635), (1.80, 0.665), (2.00, 0.685)
Mc2_CSlowFrac_df <- data.frame(
  x =  seq(0, 2, 0.2),
  Mc2_CSlowFrac = c(0.025,0.195,0.325,0.415,0.5,0.55,0.585,0.615,0.635,0.665,0.685)
)

get_Mc2_CSlowFrac <- function(x) {
  get_graph_y(
    Mc2_CSlowFrac_df,
    x,
    x_column = "x",
    y_column = "Mc2_CSlowFrac",
    mode = "continues"
  )
}


# S&B_NecroBurnFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
# (0.00, 0.00), (50.0, 0.035), (100, 0.085), (150, 0.215), (200, 0.385), (250, 0.655), (300, 0.835), (350, 0.915), (400, 0.945), (450, 0.965), (500, 0.985), (550, 0.995), (600, 0.995), (650, 1.00), (700, 1.00)

SB_NecroBurnFrac_df <- data.frame(
  SB_FireTempIncSurf =  seq(0, 700, 50),
  SB_NecroBurnFrac = c(0,0.035,0.085,0.215,0.385,0.655,0.835,0.915,0.945,0.965,0.985,0.995,0.995,1,1)
)

get_SB_NecroBurnFrac <- function(SB_FireTempIncSurf) {
  unlist(lapply(SB_FireTempIncSurf, function(x) {
    get_graph_y(
      SB_NecroBurnFrac_df,
      x,
      x_column = "SB_FireTempIncSurf",
      y_column = "SB_NecroBurnFrac",
      mode = "continues"
    )
  }))
}


# S&B_AerosolFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
# (0.00, 0.515), (50.0, 0.455), (100, 0.355), (150, 0.265), (200, 0.225), (250, 0.175), (300, 0.145), (350, 0.105), (400, 0.075), (450, 0.055), (500, 0.055), (550, 0.045), (600, 0.043), (650, 0.042), (700, 0.041)

SB_AerosolFrac_df <- data.frame(
  SB_FireTempIncSurf =  seq(0, 700, 50),
  SB_AerosolFrac = c(0.515,0.455,0.355,0.265,0.225,0.175,0.145,0.105,0.075,0.055,0.055,0.045,0.043,0.042,0.041)
)

get_SB_AerosolFrac <- function(SB_FireTempIncSurf) {
  unlist(lapply(SB_FireTempIncSurf, function(x) {
    get_graph_y(
      SB_AerosolFrac_df,
      x,
      x_column = "SB_FireTempIncSurf",
      y_column = "SB_AerosolFrac",
      mode = "continues"
    )
  }))
}

# S&B_NVolatFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
# (0.00, 0.00), (50.0, 0.03), (100, 0.08), (150, 0.28), (200, 0.63), (250, 0.94), (300, 1.00), (350, 1.00), (400, 1.00), (450, 1.00), (500, 1.00), (550, 1.00), (600, 1.00), (650, 1.00), (700, 1.00)

SB_NVolatFrac_df <- data.frame(
  SB_FireTempIncSurf =  seq(0, 700, 50),
  SB_NVolatFrac = c(0,0.03,0.08,0.28,0.63,0.94,1,1,1,1,1,1,1,1,1)
)

get_SB_NVolatFrac <- function(SB_FireTempIncSurf) {
  unlist(lapply(SB_FireTempIncSurf, function(x) {
    get_graph_y(
      SB_NVolatFrac_df,
      x,
      x_column = "SB_FireTempIncSurf",
      y_column = "SB_NVolatFrac",
      mode = "continues"
    )
  }))
}


# S&B_PVolatFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
# (0.00, 0.00), (50.0, 0.015), (100, 0.045), (150, 0.075), (200, 0.105), (250, 0.135), (300, 0.185), (350, 0.255), (400, 0.335), (450, 0.405), (500, 0.47), (550, 0.495), (600, 0.515), (650, 0.52), (700, 0.522)

SB_PVolatFrac_df <- data.frame(
  SB_FireTempIncSurf =  seq(0, 700, 50),
  SB_PVolatFrac = c(0,0.015,0.045,0.075,0.105,0.135,0.185,0.255,0.335,0.405,0.47,0.495,0.515,0.52,0.522)
)

get_SB_PVolatFrac <- function(SB_FireTempIncSurf) {
  unlist(lapply(SB_FireTempIncSurf, function(x) {
    get_graph_y(
      SB_PVolatFrac_df,
      x,
      x_column = "SB_FireTempIncSurf",
      y_column = "SB_PVolatFrac",
      mode = "continues"
    )
  }))
}

# N_KaPDef1[Zone] = GRAPH(N_Soil1[Zone,P])
# (0.00, 16245), (0.02, 15848), (0.04, 15518), (0.06, 15163), (0.08, 14726), (0.1, 14290), (0.12, 13804), (0.14, 13297), (0.16, 12616), (0.18, 12049), (0.2, 11675), (0.22, 11218), (0.24, 10772), (0.26, 10335), (0.28, 9908), (0.3, 9489), (0.32, 9080), (0.34, 8681), (0.36, 8291), (0.38, 7910), (0.4, 7572), (0.42, 7316), (0.44, 7041), (0.46, 6684), (0.48, 6331), (0.5, 5992), (0.52, 5661), (0.54, 5338), (0.56, 5029), (0.58, 4728), (0.6, 4435), (0.62, 4153), (0.64, 3882), (0.66, 3619), (0.68, 3366), (0.7, 3123), (0.72, 2936), (0.74, 2753), (0.76, 2528), (0.78, 2313), (0.8, 2107), (0.82, 1911), (0.84, 1726), (0.86, 1550), (0.88, 1385), (0.9, 1229), (0.92, 1083), (0.94, 946), (0.96, 819), (0.98, 702), (1.00, 593)
# N_KaPDef2[Zone] = GRAPH(N_Soil2[Zone,P])
# (0.00, 16245), (0.02, 15848), (0.04, 15518), (0.06, 15163), (0.08, 14726), (0.1, 14290), (0.12, 13804), (0.14, 13297), (0.16, 12616), (0.18, 12049), (0.2, 11675), (0.22, 11218), (0.24, 10772), (0.26, 10335), (0.28, 9908), (0.3, 9489), (0.32, 9080), (0.34, 8681), (0.36, 8291), (0.38, 7910), (0.4, 7572), (0.42, 7316), (0.44, 7041), (0.46, 6684), (0.48, 6331), (0.5, 5992), (0.52, 5661), (0.54, 5338), (0.56, 5029), (0.58, 4728), (0.6, 4435), (0.62, 4153), (0.64, 3882), (0.66, 3619), (0.68, 3366), (0.7, 3123), (0.72, 2936), (0.74, 2753), (0.76, 2528), (0.78, 2313), (0.8, 2107), (0.82, 1911), (0.84, 1726), (0.86, 1550), (0.88, 1385), (0.9, 1229), (0.92, 1083), (0.94, 946), (0.96, 819), (0.98, 702), (1.00, 593)
# N_KaPDef3[Zone] = GRAPH(N_Soil3[Zone,P])
# (0.00, 16245), (0.02, 15848), (0.04, 15518), (0.06, 15163), (0.08, 14726), (0.1, 14290), (0.12, 13804), (0.14, 13297), (0.16, 12616), (0.18, 12049), (0.2, 11675), (0.22, 11218), (0.24, 10772), (0.26, 10335), (0.28, 9908), (0.3, 9489), (0.32, 9080), (0.34, 8681), (0.36, 8291), (0.38, 7910), (0.4, 7572), (0.42, 7316), (0.44, 7041), (0.46, 6684), (0.48, 6331), (0.5, 5992), (0.52, 5661), (0.54, 5338), (0.56, 5029), (0.58, 4728), (0.6, 4435), (0.62, 4153), (0.64, 3882), (0.66, 3619), (0.68, 3366), (0.7, 3123), (0.72, 2936), (0.74, 2753), (0.76, 2528), (0.78, 2313), (0.8, 2107), (0.82, 1911), (0.84, 1726), (0.86, 1550), (0.88, 1385), (0.9, 1229), (0.92, 1083), (0.94, 946), (0.96, 819), (0.98, 702), (1.00, 593)
# N_KaPDef4[Zone] = GRAPH(N_Soil4[Zone,P])
# (0.00, 16245), (0.02, 15848), (0.04, 15518), (0.06, 15163), (0.08, 14726), (0.1, 14290), (0.12, 13804), (0.14, 13297), (0.16, 12616), (0.18, 12049), (0.2, 11675), (0.22, 11218), (0.24, 10772), (0.26, 10335), (0.28, 9908), (0.3, 9489), (0.32, 9080), (0.34, 8681), (0.36, 8291), (0.38, 7910), (0.4, 7572), (0.42, 7316), (0.44, 7041), (0.46, 6684), (0.48, 6331), (0.5, 5992), (0.52, 5661), (0.54, 5338), (0.56, 5029), (0.58, 4728), (0.6, 4435), (0.62, 4153), (0.64, 3882), (0.66, 3619), (0.68, 3366), (0.7, 3123), (0.72, 2936), (0.74, 2753), (0.76, 2528), (0.78, 2313), (0.8, 2107), (0.82, 1911), (0.84, 1726), (0.86, 1550), (0.88, 1385), (0.9, 1229), (0.92, 1083), (0.94, 946), (0.96, 819), (0.98, 702), (1.00, 593)
N_KaPDef_df <- data.frame(
  N_KaPDef1 = c(16245,15848,15518,15163,14726,14290,13804,13297,12616,12049,11675,11218,10772,10335,9908,9489,9080,8681,8291,7910,7572,7316,7041,6684,6331,5992,5661,5338,5029,4728,4435,4153,3882,3619,3366,3123,2936,2753,2528,2313,2107,1911,1726,1550,1385,1229,1083,946,819,702,593),
  N_KaPDef2 = c(16245,15848,15518,15163,14726,14290,13804,13297,12616,12049,11675,11218,10772,10335,9908,9489,9080,8681,8291,7910,7572,7316,7041,6684,6331,5992,5661,5338,5029,4728,4435,4153,3882,3619,3366,3123,2936,2753,2528,2313,2107,1911,1726,1550,1385,1229,1083,946,819,702,593),
  N_KaPDef3 = c(16245,15848,15518,15163,14726,14290,13804,13297,12616,12049,11675,11218,10772,10335,9908,9489,9080,8681,8291,7910,7572,7316,7041,6684,6331,5992,5661,5338,5029,4728,4435,4153,3882,3619,3366,3123,2936,2753,2528,2313,2107,1911,1726,1550,1385,1229,1083,946,819,702,593),
  N_KaPDef4 = c(16245,15848,15518,15163,14726,14290,13804,13297,12616,12049,11675,11218,10772,10335,9908,9489,9080,8681,8291,7910,7572,7316,7041,6684,6331,5992,5661,5338,5029,4728,4435,4153,3882,3619,3366,3123,2936,2753,2528,2313,2107,1911,1726,1550,1385,1229,1083,946,819,702,593),
  N_Soil = seq(0, 1, 0.02)
)

get_N_KaPDef <- function(N_Soil, layer) {
  mapply(function(x, l){
    get_graph_y(
      N_KaPDef_df,
      x,
      x_column = "N_Soil",
      y_column = names(N_KaPDef_df)[l],
      mode = "continues"
    )
    
  }, N_Soil, layer) 
}



### Irigation #################

irrigation_df = data.frame(day = c(1:365), irrigation = rep(0, 365))

### Calender ##############

calendar_df <- data.frame(month = c(1:12))

calendar_df$Evap_MonthlyMean_DayLength <- c(12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12)
calendar_df$Rain_Numberof_DaysperMonth <- days_in_month(calendar_df$month)
calendar_df$cumsum_days <- cumsum(calendar_df$Rain_Numberof_DaysperMonth)
calendar_df$Evap_MonthlyMean_AirTemp <- c(25.8, 26.3, 26.9, 27.5, 27.6, 27.1, 26.7, 27, 27.6, 27.4, 26, 26.4)

### RAIN #################

# INIT Rain_WeightTot = Rain_Weight[Zn1]*AF_ZoneFrac[Zn1]+Rain_Weight[Zn2]*AF_ZoneFrac[Zn2]+Rain_Weight[Zn3]*AF_ZoneFrac[Zn3]+Rain_Weight[Zn4]*AF_ZoneFrac[Zn4]
Rain_WeightTot <- sum(zone_df$Rain_Weight * zone_df$AF_ZoneFrac)
zone_df$Rain_WeightAct <- zone_df$Rain_Weight / Rain_WeightTot

#TODO: this was double declaration.. to be fixed
rain_pars <- get_rain_pars()

Rain_YestCq_GSeedRain_Yesterday <- rain_pars$rain_par$Rain_I_Initial_Value
Rain_Yesterday <- 0

Ca_DOYStart <- 1
set.seed(rain_pars$rain_par$Rain_GenSeed)
time <- 50

Rain <- get_rain(
  time,
  rain_pars$rain_par,
  rain_pars$rain_month_par,
  rain_pars$rain_graph,
  rain_pars$rain_df,
  Rain_Yesterday,
  Ca_DOYStart = Ca_DOYStart
)
Rain_Yesterday <- Rain$Rain_Yesterday
W_Irrigation_Data <- irrigation_df[irrigation_df$day == (time %% 365 + 1), ]$irrigation



# Rt_CLrvt[Zone] = GRAPH(Cq_Stage[Zone])
# (0.00, 0.01), (0.2, 0.125), (0.4, 0.415), (0.6, 0.715), (0.8, 0.895), (1.00, 1.00), (1.20, 1.00), (1.40, 0.985), (1.60, 0.945), (1.80, 0.84), (2.00, 0.705)
pars$Rt_par$Rt_CLrvt_df <- data.frame(Stage = seq(0, 2, 0.2))
pars$Rt_par$Rt_CLrvt_df$Rt_CLrvt <- c(0.01, 0.125, 0.415, 0.715, 0.895, 1, 1, 0.985, 0.945, 0.84, 0.705)

# S&B_SlashYear[Sp1] = GRAPH( S&B_PastSlashEvents[Sp1])
# (0.00, 100), (1.00, 100), (2.00, 0.00), (3.00, 0.00), (4.00, 0.00), (5.00, 0.00)
# S&B_SlashYear[Sp2] = GRAPH( S&B_PastSlashEvents[Sp2])
# (0.00, 100), (1.00, 100), (2.00, 0.00), (3.00, 0.00), (4.00, 0.00), (5.00, 0.00)
# S&B_SlashYear[Sp3] = GRAPH( S&B_PastSlashEvents[Sp3])
# (0.00, 100), (1.00, 100), (2.00, 0.00), (3.00, 0.00), (4.00, 0.00), (5.00, 0.00)
SB_SlashYear_df <- data.frame(Sp1 = c(100,100,0,0,0,0), Sp2 = c(100,100,0,0,0,0), Sp3 = c(100,100,0,0,0,0), SB_PastSlashEvents = 0:5)

# S&B_SlashDOY[Sp1] = GRAPH(S&B_PastSlashEvents[Sp1])
# (0.00, 10.0), (20.0, 0.00), (40.0, 0.00), (60.0, 0.00), (80.0, 0.00), (100, 0.00)
# S&B_SlashDOY[Sp2] = GRAPH(S&B_PastSlashEvents[Sp2])
# (0.00, 10.0), (20.0, 0.00), (40.0, 0.00), (60.0, 0.00), (80.0, 0.00), (100, 0.00)
# S&B_SlashDOY[Sp3] = GRAPH(S&B_PastSlashEvents[Sp3])
# (0.00, 10.0), (20.0, 0.00), (40.0, 0.00), (60.0, 0.00), (80.0, 0.00), (100, 0.00)
SB_SlashDOY_df <- data.frame(Sp1 = c(10,0,0,0,0,0), Sp2 = c(10,0,0,0,0,0), Sp3 = c(10,0,0,0,0,0), SB_PastSlashEvents = seq(0, 100, 20))

# C_RespTemp[Zone] = GRAPH(Temp[Zone,1])
# (0.00, 0.025), (5.00, 0.045), (10.0, 0.215), (15.0, 0.665), (20.0, 0.895), (25.0, 0.945), (30.0, 0.985), (35.0, 0.995)
C_RespTemp_df <- data.frame(Temp = c(0,5,10,15,20,25,30,35), C_RespTemp = c(0.025,0.045,0.215,0.665,0.895,0.945,0.985,0.995))

# Temp_SoilDailyData = GRAPH(Rain_DoY)
# (1.00, 26.4), (2.00, 26.4), (3.00, 26.6), (4.00, 26.7), (5.00, 26.5), (6.00, 26.6), (7.00, 26.7), (8.00, 26.7), (9.00, 26.5), (10.0, 26.5), (11.0, 26.6), (12.0, 26.5), (13.0, 26.6), (14.0, 26.6), (15.0, 26.3), (16.0, 26.6), (17.0, 26.6), (18.0, 26.6), (19.0, 26.4), (20.0, 26.5), (21.0, 26.7), (22.0, 26.6), (23.0, 26.7), (24.0, 26.7), (25.0, 26.7), (26.0, 26.6), (27.0, 26.6), (28.0, 26.5), (29.0, 26.5), (30.0, 26.6), (31.0, 26.4), (32.0, 26.4), (33.0, 26.3), (34.0, 26.5), (35.0, 26.5), (36.0, 26.4), (37.0, 26.7), (38.0, 26.5), (39.0, 26.5), (40.0, 26.5), (41.0, 26.4), (42.0, 26.4), (43.0, 26.4), (44.0, 26.4), (45.0, 26.4), (46.0, 26.4), (47.0, 26.3), (48.0, 26.3), (49.0, 26.3), (50.0, 26.2), (51.0, 26.5), (52.0, 26.2), (53.0, 26.5), (54.0, 26.5), (55.0, 26.5), (56.0, 26.6), (57.0, 26.6), (58.0, 26.4), (59.0, 26.5), (60.0, 26.5), (61.0, 26.4), (62.0, 26.1), (63.0, 26.2), (64.0, 26.4), (65.0, 26.6), (66.0, 26.4), (67.0, 26.5), (68.0, 26.6), (69.0, 26.5), (70.0, 26.4), (71.0, 26.1), (72.0, 26.4), (73.0, 26.1), (74.0, 26.6), (75.0, 26.6), (76.0, 26.7), (77.0, 26.6), (78.0, 26.6), (79.0, 26.5), (80.0, 26.4), (81.0, 26.2), (82.0, 26.5), (83.0, 26.4), (84.0, 26.5), (85.0, 26.6), (86.0, 26.5), (87.0, 26.5), (88.0, 26.6), (89.0, 26.6), (90.0, 26.4), (91.0, 26.7), (92.0, 26.6), (93.0, 26.6), (94.0, 26.4), (95.0, 26.6), (96.0, 26.5), (97.0, 26.3), (98.0, 26.4), (99.0, 26.1), (100, 26.2), (101, 26.5), (102, 26.4), (103, 26.5), (104, 26.6), (105, 26.4), (106, 26.4), (107, 26.4), (108, 26.6), (109, 26.5), (110, 26.5), (111, 26.5), (112, 26.4), (113, 26.3), (114, 26.3), (115, 26.3), (116, 26.3), (117, 26.3), (118, 26.3), (119, 26.4), (120, 26.3), (121, 26.5), (122, 26.3), (123, 26.6), (124, 26.6), (125, 26.7), (126, 26.5), (127, 26.4), (128, 26.3), (129, 26.5), (130, 26.3), (131, 26.1), (132, 26.2), (133, 26.2), (134, 26.1), (135, 26.3), (136, 26.2), (137, 26.1), (138, 26.4), (139, 26.5), (140, 26.3), (141, 26.2), (142, 26.0), (143, 26.4), (144, 26.2), (145, 26.0), (146, 26.2), (147, 26.1), (148, 26.2), (149, 26.1), (150, 26.0), (151, 26.0), (152, 26.1), (153, 25.9), (154, 25.9), (155, 25.9), (156, 26.2), (157, 26.3), (158, 26.1), (159, 26.1), (160, 25.9), (161, 26.1), (162, 26.0), (163, 26.0), (164, 26.0), (165, 26.1), (166, 26.1), (167, 25.8), (168, 25.9), (169, 25.8), (170, 25.9), (171, 25.7), (172, 25.9), (173, 26.1), (174, 25.8), (175, 25.7), (176, 25.6), (177, 25.9), (178, 25.9), (179, 25.8), (180, 25.7), (181, 25.6), (182, 25.7), (183, 25.5), (184, 25.5), (185, 25.6), (186, 25.5), (187, 25.6), (188, 25.7), (189, 25.6), (190, 25.8), (191, 25.7), (192, 25.7), (193, 25.7), (194, 25.8), (195, 26.0), (196, 25.7), (197, 25.5), (198, 25.6), (199, 25.6), (200, 25.8), (201, 25.8), (202, 25.6), (203, 25.6), (204, 25.6), (205, 25.5), (206, 25.7), (207, 25.8), (208, 25.7), (209, 25.7), (210, 25.5), (211, 25.3), (212, 25.7), (213, 25.4), (214, 25.5), (215, 25.6), (216, 25.7), (217, 25.8), (218, 25.7), (219, 25.6), (220, 25.7), (221, 25.6), (222, 25.7), (223, 25.8), (224, 25.8), (225, 25.5), (226, 25.5), (227, 25.6), (228, 25.7), (229, 25.8), (230, 25.4), (231, 25.4), (232, 25.5), (233, 25.6), (234, 25.6), (235, 25.6), (236, 25.8), (237, 25.7), (238, 25.6), (239, 25.7), (240, 25.8), (241, 25.7), (242, 25.6), (243, 25.7), (244, 25.8), (245, 25.6), (246, 25.7), (247, 25.9), (248, 25.7), (249, 25.8), (250, 25.9), (251, 25.8), (252, 25.6), (253, 25.6), (254, 25.7), (255, 25.8), (256, 25.7), (257, 25.9), (258, 25.9), (259, 26.0), (260, 26.2), (261, 26.1), (262, 26.2), (263, 25.9), (264, 25.8), (265, 26.0), (266, 26.1), (267, 25.8), (268, 25.9), (269, 26.1), (270, 26.2), (271, 26.2), (272, 26.3), (273, 26.1), (274, 26.2), (275, 26.4), (276, 26.3), (277, 26.4), (278, 26.4), (279, 26.4), (280, 26.4), (281, 26.3), (282, 26.4), (283, 26.5), (284, 26.5), (285, 26.6), (286, 26.7), (287, 26.7), (288, 26.5), (289, 26.4), (290, 26.6), (291, 26.6), (292, 26.8), (293, 26.7), (294, 26.8), (295, 26.6), (296, 26.6), (297, 26.5), (298, 26.7), (299, 26.6), (300, 26.7), (301, 26.8), (302, 26.8), (303, 26.8), (304, 27.0), (305, 26.8), (306, 26.7), (307, 26.8), (308, 26.8), (309, 26.9), (310, 27.0), (311, 27.1), (312, 26.9), (313, 27.0), (314, 27.0), (315, 27.1), (316, 27.2), (317, 27.1), (318, 27.1), (319, 27.1), (320, 27.1), (321, 27.1), (322, 27.1), (323, 27.1), (324, 27.1), (325, 27.2), (326, 26.9), (327, 26.9), (328, 27.0), (329, 27.0), (330, 27.0), (331, 26.9), (332, 27.0), (333, 27.2), (334, 27.2), (335, 27.1), (336, 27.0), (337, 26.9), (338, 27.0), (339, 26.8), (340, 26.9), (341, 27.0), (342, 27.2), (343, 26.9), (344, 26.8), (345, 26.8), (346, 26.8), (347, 26.9), (348, 26.7), (349, 26.8), (350, 27.0), (351, 27.0), (352, 27.0), (353, 26.8), (354, 26.6), (355, 27.0), (356, 26.8), (357, 26.8), (358, 26.8), (359, 26.6), (360, 26.9), (361, 26.9), (362, 26.6), (363, 26.7), (364, 26.4), (365, 26.6)
Temp_SoilDailyData_df <- data.frame(
  Rain_DoY = c(1:365),
  Temp_SoilDailyData = c(26.4,26.4,26.6,26.7,26.5,26.6,26.7,26.7,26.5,26.5,26.6,26.5,26.6,26.6,26.3,26.6,26.6,26.6,26.4,26.5,26.7,26.6,26.7,26.7,26.7,26.6,26.6,26.5,26.5,26.6,26.4,26.4,26.3,26.5,26.5,26.4,26.7,26.5,26.5,26.5,26.4,26.4,26.4,26.4,26.4,26.4,26.3,26.3,26.3,26.2,26.5,26.2,26.5,26.5,26.5,26.6,26.6,26.4,26.5,26.5,26.4,26.1,26.2,26.4,26.6,26.4,26.5,26.6,26.5,26.4,26.1,26.4,26.1,26.6,26.6,26.7,26.6,26.6,26.5,26.4,26.2,26.5,26.4,26.5,26.6,26.5,26.5,26.6,26.6,26.4,26.7,26.6,26.6,26.4,26.6,26.5,26.3,26.4,26.1,26.2,26.5,26.4,26.5,26.6,26.4,26.4,26.4,26.6,26.5,26.5,26.5,26.4,26.3,26.3,26.3,26.3,26.3,26.3,26.4,26.3,26.5,26.3,26.6,26.6,26.7,26.5,26.4,26.3,26.5,26.3,26.1,26.2,26.2,26.1,26.3,26.2,26.1,26.4,26.5,26.3,26.2,26,26.4,26.2,26,26.2,26.1,26.2,26.1,26,26,26.1,25.9,25.9,25.9,26.2,26.3,26.1,26.1,25.9,26.1,26,26,26,26.1,26.1,25.8,25.9,25.8,25.9,25.7,25.9,26.1,25.8,25.7,25.6,25.9,25.9,25.8,25.7,25.6,25.7,25.5,25.5,25.6,25.5,25.6,25.7,25.6,25.8,25.7,25.7,25.7,25.8,26,25.7,25.5,25.6,25.6,25.8,25.8,25.6,25.6,25.6,25.5,25.7,25.8,25.7,25.7,25.5,25.3,25.7,25.4,25.5,25.6,25.7,25.8,25.7,25.6,25.7,25.6,25.7,25.8,25.8,25.5,25.5,25.6,25.7,25.8,25.4,25.4,25.5,25.6,25.6,25.6,25.8,25.7,25.6,25.7,25.8,25.7,25.6,25.7,25.8,25.6,25.7,25.9,25.7,25.8,25.9,25.8,25.6,25.6,25.7,25.8,25.7,25.9,25.9,26,26.2,26.1,26.2,25.9,25.8,26,26.1,25.8,25.9,26.1,26.2,26.2,26.3,26.1,26.2,26.4,26.3,26.4,26.4,26.4,26.4,26.3,26.4,26.5,26.5,26.6,26.7,26.7,26.5,26.4,26.6,26.6,26.8,26.7,26.8,26.6,26.6,26.5,26.7,26.6,26.7,26.8,26.8,26.8,27,26.8,26.7,26.8,26.8,26.9,27,27.1,26.9,27,27,27.1,27.2,27.1,27.1,27.1,27.1,27.1,27.1,27.1,27.1,27.2,26.9,26.9,27,27,27,26.9,27,27.2,27.2,27.1,27,26.9,27,26.8,26.9,27,27.2,26.9,26.8,26.8,26.8,26.9,26.7,26.8,27,27,27,26.8,26.6,27,26.8,26.8,26.8,26.6,26.9,26.9,26.6,26.7,26.4,26.6)
)  

# Temp_MonthAvg = GRAPH(Rain_DoY)
# (1.00, 26.0), (31.3, 25.1), (61.7, 24.9), (92.0, 25.0), (122, 25.1), (153, 25.5), (183, 25.5), (213, 25.2), (244, 25.6), (274, 25.0), (304, 25.0), (335, 25.0), (365, 25.0)

Temp_MonthAvg_df <- data.frame(
  Rain_DoY = c(1,31.3,61.7,92,122,153,183,213,244,274,304,335,365),
 Temp_MonthAvg = c(26,25.1,24.9,25,25.1,25.5,25.5,25.2,25.6,25,25,25,25)
)

# W_PhiPRhizRange[Zone,BufValues] = GRAPH(CW_pF_range[Zone,BufValues])
# (0.00, 283), (0.2, 259), (0.4, 196), (0.6, 160), (0.8, 126), (1.00, 94.0), (1.20, 67.6), (1.40, 47.1), (1.60, 32.1), (1.80, 21.5), (2.00, 14.3), (2.20, 11.7), (2.40, 6.24), (2.60, 4.11), (2.80, 2.70), (3.00, 1.78), (3.20, 1.17), (3.40, 0.767), (3.60, 0.502), (3.80, 0.329), (4.00, 0.214), (4.20, 0.139), (4.40, 0.0887), (4.60, 0.056), (4.80, 0.0343), (5.00, 0.0201)

W_PhiPRhizRange_df <- data.frame(
  CW_pF_range = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3,3.2,3.4,3.6,3.8,4,4.2,4.4,4.6,4.8,5),
  W_PhiPRhizRange = c(283,259,196,160,126,94,67.6,47.1,32.1,21.5,14.3,11.7,6.24,4.11,2.7,1.78,1.17,0.767,0.502,0.329,0.214,0.139,0.0887,0.056,0.0343,0.0201)
)

# Temp_AirDailyData = GRAPH(Rain_DoY)
Temp_AirDailyData_df <- data.frame(
  Rain_DoY = 1:365,
  Temp_AirDailyData = 25
)





#TODO: validate parameter here
zonelayer_df$AF_ZoneFrac <- rep(zone_df$AF_ZoneFrac, nlayer)

# soil zones
AF_ZoneFrac_1 <- zone_df$AF_ZoneFrac[-1]
AF_ZoneFrac_1 <- c(AF_ZoneFrac_1, zone_df$AF_ZoneFrac[1])
zone_df$AF_LatInFlowRatio <- AF_ZoneFrac_1 / zone_df$AF_ZoneFrac

# AF_Depth1[Zone] = AF_DepthLay1
# AF_Depth2[Zone] = AF_DepthLay2*(1-AF_StoneFrac[Zone,2])
# AF_Depth3[Zone] = AF_DepthLay3*(1-AF_StoneFrac[Zone,3])
# AF_Depth4[Zone] = AF_DepthLay4*(1-AF_StoneFrac[Zone,4])
zonelayer_df$AF_DepthLay <- rep(layer_df$AF_DepthLay, each = nzone)
zonelayer_df$AF_Depth <- zonelayer_df$AF_DepthLay * (1 - zonelayer_df$AF_StoneFrac)

# RT3_DepthZone[Zn1,1] = AF_DepthAct1[Zn1]+0*(AF_Depth2[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
# RT3_DepthZone[Zn1,2] = AF_Depth2[Zn1]+0*(AF_DepthAct1[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
# RT3_DepthZone[Zn1,3] = AF_Depth3[Zn1]+0*(AF_DepthAct1[Zn1]+AF_Depth2[Zn1]+AF_Depth4[Zn1])
# RT3_DepthZone[Zn1,4] = AF_Depth4[Zn1]+0*(AF_DepthAct1[Zn4]+AF_Depth2[Zn4]+AF_Depth3[Zn4])
# RT3_DepthZone[Zn2,1] = AF_DepthAct1[Zn2]+0*(AF_Depth2[Zn2]+AF_Depth3[Zn2]+AF_Depth4[Zn2])
# RT3_DepthZone[Zn2,2] = AF_Depth2[Zn2]+0*(AF_DepthAct1[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
# RT3_DepthZone[Zn2,3] = AF_Depth3[Zn2]+0*(AF_DepthAct1[Zn1]+AF_Depth2[Zn1]+AF_Depth4[Zn1])
# RT3_DepthZone[Zn2,4] = AF_Depth4[Zn2]+0*(AF_DepthAct1[Zn4]+AF_Depth2[Zn4]+AF_Depth3[Zn4])
zonelayer_df$RT3_DepthZone <- zonelayer_df$AF_Depth

zonelayer_df$AF_ZoneWidth <- rep(zone_df$AF_ZoneWidth, nlayer)
# RT3_VoxVol[Zone,SoilLayer] = AF_ZoneWidth[Zone]*100*RT3_DepthZone[Zone,SoilLayer]*100
zonelayer_df$RT3_VoxVol <- zonelayer_df$AF_ZoneWidth * 100 * zonelayer_df$RT3_DepthZone *
  100







# initiate the top layer
zone_df$AF_SlopeCurr <- zone_df$AF_SlopeSurfInit
zone_df$AF_DepthLay1 <- layer_df[layer_df$layer == 1, ]$AF_DepthLay

# AF_DepthSlope1[Zn1] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else
#   (AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn1]/100))))

# AF_DepthSlope1[Zn1] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else (AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn1]/100))))
# AF_DepthSlope1[Zn2] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else mAX((AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]-AF_ZoneFrac[Zn2]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn2]/100)))),0.0001)
# AF_DepthSlope1[Zn3] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else max((AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]-AF_ZoneFrac[Zn2]-AF_ZoneFrac[Zn3]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn3]/100)))),0.0001)
# AF_DepthSlope1[Zn4] = if AF_DepthDynamic? = 0 then AF_DepthLay1 else max((AF_DepthLay1/COS(ARCTAN(AF_SlopeSoilHoriz/100))+(0.5-AF_ZoneFrac[Zn1]-AF_ZoneFrac[Zn2]-AF_ZoneFrac[Zn3]-AF_ZoneFrac[Zn4]/2)*(TAN(ARCTAN(AF_SlopeSoilHoriz/100))-TAN(ARCTAN(AF_SlopeCurr[Zn4]/100)))),0.0001)
zone_df$AF_DepthDynamic_is <-  pars$AF_par$AF_DepthDynamic_is
zone_df$AF_SlopeSoilHoriz <- pars$AF_par$AF_SlopeSoilHoriz
zone_df$AF_DepthSlope1_a <- zone_df$AF_DepthLay1 / cos(atan(zone_df$AF_SlopeSoilHoriz / 100))
zone_df$AF_DepthSlope1_b <- tan(atan(zone_df$AF_SlopeSoilHoriz / 100)) - tan(atan(zone_df$AF_SlopeCurr / 100))
z1 <- zone_df[zone_df$zone == 1, ]
z2 <- zone_df[zone_df$zone == 2, ]
z3 <- zone_df[zone_df$zone == 3, ]
z4 <- zone_df[zone_df$zone == 4, ]
zone_df$AF_DepthSlope1 <- zone_df$AF_DepthLay1
zone_df[zone_df$zone == 1, ]$AF_DepthSlope1 <- ifelse(
  z1$AF_DepthDynamic_is == 0,
  z1$AF_DepthLay1,
  z1$AF_DepthSlope1_a + (0.5 - z1$AF_ZoneFrac / 2) * z1$AF_DepthSlope1_b
)
zone_df[zone_df$zone == 2, ]$AF_DepthSlope1 <- ifelse(
  z2$AF_DepthDynamic_is == 0,
  z2$AF_DepthLay1,
  pmax(
    z2$AF_DepthSlope1_a + (0.5 - z1$AF_ZoneFrac - z2$AF_ZoneFrac / 2) * z2$AF_DepthSlope1_b,
    0.0001
  )
)
zone_df[zone_df$zone == 3, ]$AF_DepthSlope1 <- ifelse(
  z3$AF_DepthDynamic_is == 0,
  z3$AF_DepthLay1,
  pmax(
    z3$AF_DepthSlope1_a + (0.5 - z1$AF_ZoneFrac - z2$AF_ZoneFrac - z3$AF_ZoneFrac / 2) * z3$AF_DepthSlope1_b,
    0.0001
  )
)
zone_df[zone_df$zone == 4, ]$AF_DepthSlope1 <- ifelse(
  z4$AF_DepthDynamic_is == 0,
  z4$AF_DepthLay1,
  pmax(
    z4$AF_DepthSlope1_a + (
      0.5 - z1$AF_ZoneFrac - z2$AF_ZoneFrac - z3$AF_ZoneFrac - z4$AF_ZoneFrac / 2
    ) * z4$AF_DepthSlope1_b,
    0.0001
  )
)

# AF_DepthAct1[Zone] = IF(AF_SlopeCurr[Zone]<>AF_SlopeSurfInit)THEN(AF_Depth1[Zone])*(1-AF_StoneFrac[Zone,1])
# ELSE(AF_DepthSlope1[Zone])*(1-AF_StoneFrac[Zone,1])
zone_df$AF_DepthAct1 <- ifelse(
  zone_df$AF_SlopeCurr != zone_df$AF_SlopeSurfInit,
  zonelayer_df[zonelayer_df$layer == 1, ]$AF_Depth * (1 - zonelayer_df[zonelayer_df$layer == 1, ]$AF_StoneFrac),
  zone_df$AF_DepthSlope1 * (1 - zonelayer_df[zonelayer_df$layer == 1, ]$AF_StoneFrac)
)

#NOTE: Layer 1 AF_Depth is equal to AF_DepthAct1, the original ia copied to AF_Depth_original
zonelayer_df$AF_Depth_original <- zonelayer_df$AF_Depth
zonelayer_df[layer_df$layer == 1, ]$AF_Depth <- zone_df$AF_DepthAct1


# AF_Depths[1] = AF_DepthAct1[Zn1]+0*(AF_Depth2[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
# AF_Depths[2] = AF_Depth2[Zn1]+  +0*(AF_DepthAct1[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
# AF_Depths[3] = AF_Depth3[Zn1]+  +0*(AF_DepthAct1[Zn1]+AF_Depth2[Zn1]+AF_Depth4[Zn1])
# AF_Depths[4] = AF_Depth4[Zn1]+  +0*(AF_DepthAct1[Zn1]+AF_Depth2[Zn1]+AF_Depth3[Zn1])
layer_df$AF_Depths <- zonelayer_df[zonelayer_df$zone == 1, ]$AF_Depth

# Rt_Depth1[Zone] = AF_Depths[1]
# Rt_Depth2[Zone] = AF_Depths[1]+AF_Depths[2]
# Rt_Depth3[Zone] = AF_Depths[1]+AF_Depths[2]+AF_Depths[3]
# Rt_Depth4[Zone] = ARRAYSUM(AF_Depths[*])
zonelayer_df$Rt_Depth <- layer_df$AF_Depths[1]
zonelayer_df[zonelayer_df$layer == 2, ]$Rt_Depth <- sum(layer_df$AF_Depths[1:2])
zonelayer_df[zonelayer_df$layer == 3, ]$Rt_Depth <- sum(layer_df$AF_Depths[1:3])
zonelayer_df[zonelayer_df$layer == 4, ]$Rt_Depth <- sum(layer_df$AF_Depths)

# Rt_Depth = Rt_Depth4[Zn1]
Rt_Depth <- zonelayer_df[zonelayer_df$zone == 1 &
                           zonelayer_df$layer == 4, ]$Rt_Depth

# INIT Rt_TLraCD[Tree] = Rt_TDecDepthC[Tree]*EXP(-0.25*Rt_TDecDepthC[Tree]*(SQRT(Rt_TDistShapeC[Tree]*AF_ZoneTot^2)+SQRT(Rt_Depth^2)+SQRT(Rt_TDistShapeC[Tree]*AF_ZoneTot^2+Rt_Depth^2)))
tree_df$Rt_TLraCD <- tree_df$Rt_TDecDepthC * exp(-0.25 * tree_df$Rt_TDecDepthC *
                                                   (
                                                     sqrt(tree_df$Rt_TDistShapeC * pars$AF_par$AF_ZoneTot^2) + sqrt(Rt_Depth^2) + sqrt(tree_df$Rt_TDistShapeC *
                                                                                                                                         pars$AF_par$AF_ZoneTot^2 + Rt_Depth^2)
                                                   ))




# INIT S_BDActOverBDRefInfiltr[Zone] = (0.69-SQRT(-0.69^2-4*-0.52*(1.21-(LOG10(S_RelSurfInfiltrInit[Zone])))))/(2*-0.52)
zone_df$S_BDActOverBDRefInfiltr <- (0.69 - sqrt((-0.69)^2 - 4 * (-0.52 *
                                                                   (
                                                                     1.21 -
                                                                       log10(zone_df$S_RelSurfInfiltrInit)
                                                                   )))) / (2 * (-0.52))





# TODO: this is dynamic variable
# INIT S_BDActOverBDRefKsatV1[Zone] = (0.69-SQRT(-0.69^2-4*-0.52*(1.21-(LOG10(S_KsatInitV1[Zone]/S_KsatDefV1[Zone])))))/(2*-0.52)
zonelayer_df$S_BDActOverBDRefKsatV <- (0.69 - sqrt(-0.69^2 - 4 * -0.52 * (1.21 -
                                                                            (
                                                                              log10(zonelayer_df$S_KsatInitV / zonelayer_df$S_KsatDefV)
                                                                            )))) / (2 * -0.52)

# S_KsatV1Act[Zone] = S_KsatDefV1[Zone]*10^((-0.52*(S_BDActOverBDRefKsatV1[Zone]^2)-0.69*S_BDActOverBDRefKsatV1[Zone]+1.21))
zonelayer_df$S_KsatVAct <- zonelayer_df$S_KsatDefV * 10^((
  -0.52 * (zonelayer_df$S_BDActOverBDRefKsatV^2) - 0.69 * zonelayer_df$S_BDActOverBDRefKsatV +
    1.21
))
# W_KSatH1[Zone] = S_KsatV1Act[Zone]*S_KSatHperV1[Zone]
# W_KSatH2[Zone] = S_KsatV2Act[Zone]*S_KsatHperV2[Zone]
# W_KSatH3[Zone] = S_KsatV3Act[Zone]*S_KsatHperV3[Zone]
# W_KSatH4[Zone] = S_KsatV4Act[Zone]*S_KsatHperV4[Zone]
zonelayer_df$W_KSatH <- zonelayer_df$S_KsatVAct * zonelayer_df$S_KSatHperV

# S_RelBD[Zn1,1] = S_BDActOverBDRefKsatV1[Zn1]+0*(S_BDActOverBDRefKsatV1[Zn1]+S_BDActOverBDRefKsatV2[Zn1]+S_BDActOverBDRefKsatV3[Zn1]+S_BDActOverBDRefKsatV4[Zn1])
zonelayer_df$S_RelBD <- zonelayer_df$S_BDActOverBDRefKsatV

zonelayer_df$W_BDLayer <- rep(layer_df$W_BDLayer, each = nzone)
# W_PoreVol[Zone,SoilLayer] = (1-W_BDLayer[SoilLayer]*S_RelBD[Zone,SoilLayer]/2.5)
zonelayer_df$W_PoreVol <- (1 - zonelayer_df$W_BDLayer * zonelayer_df$S_RelBD / 2.5)
# W_ThetaI1[Zone] = W_ThetaInit1[Zone]*W_PoreVol[Zone,1]
zonelayer_df$W_ThetaI <- zonelayer_df$W_ThetaInit * zonelayer_df$W_PoreVol

# INIT W_Stock1[Zone] = W_ThetaI1[Zone]*AF_DepthAct1[Zone]*1000
zonelayer_df$W_Stock <- zonelayer_df$W_ThetaI * zonelayer_df$AF_Depth * 1000

# layer height above ground water table (cm)
# INIT AF_HGW1[Zone] = (.5*AF_Depth1[Zone]+AF_Depth2[Zone]+AF_Depth3[Zone]+AF_Depth4[Zone] + AF_DepthGroundWater_Table  )*100
# INIT AF_HGW2[Zone] = (.5*AF_Depth2[Zone]+AF_Depth3[Zone]+AF_Depth4[Zone] + AF_DepthGroundWater_Table  )*100
# INIT AF_HGW3[Zone] = (.5*AF_Depth3[Zone]+AF_Depth4[Zone] + AF_DepthGroundWater_Table  )*100
# INIT AF_HGW4[Zone] = (.5*AF_Depth4[Zone] + AF_DepthGroundWater_Table  )*100
l1 <- zonelayer_df[zonelayer_df$layer == 1, ]
l2 <- zonelayer_df[zonelayer_df$layer == 2, ]
l3 <- zonelayer_df[zonelayer_df$layer == 3, ]
l4 <- zonelayer_df[zonelayer_df$layer == 4, ]

zonelayer_df$AF_HGW <- NA
zonelayer_df[zonelayer_df$layer == 1, ]$AF_HGW <- (
  0.5 * l1$AF_Depth_original + l2$AF_Depth + l3$AF_Depth + l4$AF_Depth +  pars$AF_par$AF_DepthGroundWater_Table
) * 100
zonelayer_df[zonelayer_df$layer == 2, ]$AF_HGW <- (0.5 * l2$AF_Depth + l3$AF_Depth +
                                                     l4$AF_Depth +  pars$AF_par$AF_DepthGroundWater_Table) * 100
zonelayer_df[zonelayer_df$layer == 3, ]$AF_HGW <- (0.5 * l3$AF_Depth +
                                                     l4$AF_Depth +  pars$AF_par$AF_DepthGroundWater_Table) * 100
zonelayer_df[zonelayer_df$layer == 4, ]$AF_HGW <- (0.5 * l4$AF_Depth +  pars$AF_par$AF_DepthGroundWater_Table) * 100



zonelayer_df$W_ThetaP <- NA
for (i in c(1:nlayer)) {
  gr_df <- W_ThetaP_df[c(1, i + 1)]
  AF_HGW <- zonelayer_df[zonelayer_df$layer == i &
                           zonelayer_df$zone == 1, ]$AF_HGW
  zonelayer_df[zonelayer_df$layer == i, ]$W_ThetaP <- get_graph_y(gr_df, x = -AF_HGW, mode = "continues")
}

# LF_V1MaxDailyFlow[Zone] = (AF_DepthAct1[Zone]*S_KsatV1Act[Zone]+AF_Depth2[Zone]*S_KsatV2Act[Zone])/(AF_DepthAct1[Zone]+AF_Depth2[Zone])
# LF_V2MaxDailyFlow[Zone] = (AF_Depth2[Zone]*S_KsatV2Act[Zone]+AF_Depth3[Zone]*S_KsatV3Act[Zone])/(AF_Depth2[Zone]+AF_Depth3[Zone])
# LF_V3MaxDailyFlow[Zone] = (AF_Depth3[Zone]*S_KsatV3Act[Zone]+AF_Depth4[Zone]*S_KsatV4Act[Zone])/(AF_Depth3[Zone]+AF_Depth4[Zone])
# LF_V4MaxDailyFlow[Zone] = (AF_Depth4[Zone]*S_KsatV4Act[Zone]+AF_DeepSubSoil*S_KsatVDeepSub)/(AF_Depth4[Zone]+AF_DeepSubSoil)
zonelayer_df$AF_Depth_down <- c(zonelayer_df[zonelayer_df$layer %in% c(2:4), ]$AF_Depth,
                                rep(pars$AF_par$AF_DeepSubSoil, nzone))
zonelayer_df$S_KsatVAct_down <- c(zonelayer_df[zonelayer_df$layer %in% c(2:4), ]$S_KsatVAct,
                                  rep(pars$S_par$S_KsatVDeepSub, nzone))

zonelayer_df$LF_VMaxDailyFlow <-  (
  zonelayer_df$AF_Depth * zonelayer_df$S_KsatVAct  + zonelayer_df$AF_Depth_down * zonelayer_df$S_KsatVAct_down
) / (zonelayer_df$AF_Depth + zonelayer_df$AF_Depth_down)





zonenut_df$Mn_NutRatStruc <- rep(nut_df$Mn_NutRatStruc, each = nzone)
zonenut_df$Mn_InitStruc <- rep(zone_df$Mn_InitStruc, nrow(nut_df))
zonenut_df$AF_Depth1 <- rep(zonelayer_df[zonelayer_df$layer == 1, ]$AF_Depth, nrow(nut_df))

# INIT Mn_Struc[Zone,SlNut] = Mn_InitStruc[Zone]/Mn_NutRatStruc[SlNut]*AF_DepthAct1[Zone]*1000
zonenut_df$Mn_Struc <- zonenut_df$Mn_InitStruc / zonenut_df$Mn_NutRatStruc * zonenut_df$AF_Depth1 *
  1000

# INIT Mc_Struc[Zone] = Mn_Struc[Zone,N]*Mn_CNStruc
zone_df$Mc_Struc <- zonenut_df[zonenut_df$SlNut == "N", ]$Mn_Struc * pars$Mn_par$Mn_CNStruc



# INIT Mn_Metab[Zone,SlNut] = Mn_InitMetab[Zone]/Mn_NutRatMetab[SlNut]*AF_DepthAct1[Zone]*1000
zonenut_df$Mn_InitMetab <- rep(zone_df$Mn_InitMetab, nrow(nut_df))
zonenut_df$Mn_NutRatMetab <- rep(nut_df$Mn_NutRatMetab, each = nzone)
zonenut_df$AF_DepthAct1 <- rep(zonelayer_df[zonelayer_df$layer == 1, ]$AF_Depth, nrow(nut_df))
zonenut_df$Mn_Metab <- zonenut_df$Mn_InitMetab / zonenut_df$Mn_NutRatMetab * zonenut_df$AF_DepthAct1 *
  1000

# INIT Mc_Metab[Zone] = Mc_CNRatInitMet[Zone]*Mn_Metab[Zone,N]
zone_df$Mc_Metab <- zone_df$Mc_CNRatInitMet * zonenut_df[zonenut_df$SlNut == "N", ]$Mn_Metab

# INIT Mn_Act[Zone,SlNut] = Mn_InitAct[Zone]/Mn_NutRatAct[SlNut]*AF_DepthAct1[Zone]*1000
zonenut_df$Mn_InitAct <- rep(zone_df$Mn_InitAct, nrow(nut_df))
zonenut_df$Mn_NutRatAct <- rep(nut_df$Mn_NutRatAct, each = nzone)
zonenut_df$Mn_Act <- zonenut_df$Mn_InitAct / zonenut_df$Mn_NutRatAct * zonenut_df$AF_DepthAct1 *
  1000

# INIT Mc_Act[Zone] = Mn_Act[Zone,N]*Mn_CNAct
zone_df$Mc_Act <- zonenut_df[zonenut_df$SlNut == "N", ]$Mn_Act * pars$Mn_par$Mn_CNAct


# INIT Mn_Slw[Zone,SlNut] = Mn_InitSlw[Zone]/Mn_NutRatSlw[SlNut]*AF_DepthAct1[Zone]*1000
zonenut_df$Mn_InitSlw <- rep(zone_df$Mn_InitSlw, nrow(nut_df))
zonenut_df$Mn_NutRatSlw <- rep(nut_df$Mn_NutRatSlw, each = nzone)
zonenut_df$Mn_Slw <- zonenut_df$Mn_InitSlw / zonenut_df$Mn_NutRatSlw * zonenut_df$AF_DepthAct1 *
  1000

# INIT Mc_Slw[Zone] = Mn_Slw[Zone,N]*Mn_CNSlw
zone_df$Mc_Slw <- zonenut_df[zonenut_df$SlNut == "N", ]$Mn_Slw * pars$Mn_par$Mn_CNSlw

# INIT Mn_Pass[Zone,SlNut] = Mn_InitPass[Zone]/Mn_NutRatPas[SlNut]*AF_DepthAct1[Zone]*1000
zonenut_df$Mn_InitPass <- rep(zone_df$Mn_InitPass, nrow(nut_df))
zonenut_df$Mn_NutRatPas <- rep(nut_df$Mn_NutRatPas, each = nzone)
zonenut_df$Mn_Pass <- zonenut_df$Mn_InitPass / zonenut_df$Mn_NutRatPas *
  zonenut_df$AF_DepthAct1 * 1000

# INIT Mc_Pass[Zone] = Mn_Pass[Zone,N]*Mn_CNPass
zone_df$Mc_Pass <- zonenut_df[zonenut_df$SlNut == "N", ]$Mn_Pass * pars$Mn_par$Mn_CNPass

# Mc2_RelImpDeno[Zone] = AF_DepthAct1[Zone]*Mc2_SOMDist[1]+AF_Depth2[Zone]*Mc2_SOMDist[2]+AF_Depth3[Zone]*Mc2_SOMDist[3]+AF_Depth4[Zone]*Mc2_SOMDist[4]
zonelayer_df$Mc2_SOMDist <- rep(layer_df$Mc2_SOMDist, each = nzone)
zonelayer_df$Mc2_RelImpDeno_a <- zonelayer_df$AF_Depth * zonelayer_df$Mc2_SOMDist
zone_df$Mc2_RelImpDeno <- aggregate(zonelayer_df["Mc2_RelImpDeno_a"], zonelayer_df["zone"], sum)$Mc2_RelImpDeno_a

# MC2_ClayperZone[Zone] = (S_SoilProp[ClayLayer1]*AF_DepthAct1[Zone]+S_SoilProp[ClayLayer2]*AF_Depth2[Zone])/(AF_DepthAct1[Zone]+AF_Depth2[Zone])
zone_df$Mc2_ClayperZone <- (
  layer_df$ClayLayer[1] * zonelayer_df[zonelayer_df$layer == 1, ]$AF_Depth +
    layer_df$ClayLayer[2] * zonelayer_df[zonelayer_df$layer == 2, ]$AF_Depth
) / (zonelayer_df[zonelayer_df$layer == 1, ]$AF_Depth + zonelayer_df[zonelayer_df$layer == 2, ]$AF_Depth)

# MC2_Clay = (MC2_ClayperZone[Zn1]*AF_ZoneWidth[Zn1]+MC2_ClayperZone[Zn2]*AF_ZoneWidth[Zn2]+MC2_ClayperZone[Zn3]*AF_ZoneWidth[Zn3]+MC2_ClayperZone[Zn4]*AF_ZoneWidth[Zn4])/ARRAYSUM(AF_ZoneWidth[*])/100
Mc2_Clay <- sum(zone_df$Mc2_ClayperZone * zone_df$AF_ZoneWidth) / sum(zone_df$AF_ZoneWidth) /
  100

# MC2_SiltperZone[Zone] = (S_SoilProp[SiltLayer1]*AF_DepthAct1[Zone]+S_SoilProp[SiltLayer2]*AF_Depth2[Zone])/(AF_DepthAct1[Zone]+AF_Depth2[Zone])
zone_df$Mc2_SiltperZone <- (
  layer_df$SiltLayer[1] * zonelayer_df[zonelayer_df$layer == 1, ]$AF_Depth +
    layer_df$SiltLayer[2] * zonelayer_df[zonelayer_df$layer == 2, ]$AF_Depth
) / (zonelayer_df[zonelayer_df$layer == 1, ]$AF_Depth + zonelayer_df[zonelayer_df$layer == 2, ]$AF_Depth)

# MC2_Silt = (MC2_SiltperZone[Zn1]*AF_ZoneWidth[Zn1]+MC2_SiltperZone[Zn2]*AF_ZoneWidth[Zn2]+MC2_SiltperZone[Zn3]*AF_ZoneWidth[Zn3]+MC2_SiltperZone[Zn4]*AF_ZoneWidth[Zn4])/ARRAYSUM(AF_ZoneWidth[*])/100
Mc2_Silt <- sum(zone_df$Mc2_SiltperZone * zone_df$AF_ZoneWidth) / sum(zone_df$AF_ZoneWidth) /
  100

# Mc2_CrefMeth2 = exp(MC2_CrefOffset+MC2_ClayCoeffCref*(MC2_Clay+MC2_SiltClayCoeffCref*MC2_Silt)+MC2_pHCoeffCref*MC2_pH)
Mc2_CrefMeth2 <- exp(
  pars$Mc_par$Mc2_CrefOffset + pars$Mc_par$Mc2_ClayCoeffCref * (Mc2_Clay + pars$Mc_par$Mc2_SiltClayCoeffCref *
                                                                  Mc2_Silt) + pars$Mc_par$Mc2_pHCoeffCref * pars$Mc_par$Mc2_pH
)

# MC2_CorgInit = if MC2_SomInitType = 2 then MC2_CorgpCref*Mc2_CrefMeth2 else Mc2_CorgInitMeth3
Mc2_CorgInit <- ifelse(
  pars$Mc_par$Mc2_SomInitType == 2,
  pars$Mc_par$Mc2_CorgpCref * Mc2_CrefMeth2,
  pars$Mc_par$Mc2_CorgInitMeth3
)


# INIT Mc2_Metab[Zone,SoilLayer] = if MC2_SomInitType = 1 then (Mn2_InitMetab[Zone]*1000*Mc2_RelImpDeno[Zone]/(Mc2_SOMDist[1]))*Mc2_CNRatInitMet[Zone] else 0.01 * 10000*MC2_CorgInit*Mc2_RelImpDeno[Zone]/Mc2_SOMDist[1]
zonelayer_df$Mn2_InitMetab <- rep(zone_df$Mn2_InitMetab, nlayer)
zonelayer_df$Mc2_RelImpDeno <- rep(zone_df$Mc2_RelImpDeno, nlayer)
zonelayer_df$Mc2_CNRatInitMet <- rep(zone_df$Mc2_CNRatInitMet, nlayer)
zonelayer_df$Mc2_Metab <- ifelse(
  pars$Mc_par$Mc2_SomInitType == 1,
  (
    zonelayer_df$Mn2_InitMetab * 1000 * zonelayer_df$Mc2_RelImpDeno / (layer_df$Mc2_SOMDist[1])
  ) * zonelayer_df$Mc2_CNRatInitMet,
  0.01 * 10000 * Mc2_CorgInit * zonelayer_df$Mc2_RelImpDeno /
    layer_df$Mc2_SOMDist[1]
)

# INIT Mn2_ActInit[Zone] = Mn2_InitAct[Zone]*1000*Mc2_RelImpDeno[Zone]
zone_df$Mn2_ActInit <- zone_df$Mn2_InitAct*1000* zone_df$Mc2_RelImpDeno

# Mn2_ActInitF[Zone] = Mn2_ActInit[Zone]
zone_df$Mn2_ActInitF <- zone_df$Mn2_ActInit

# INIT Mn2_SlwInit[Zone] = Mn2_InitSlw[Zone]*1000*Mc2_RelImpDeno[Zone]
zone_df$Mn2_SlwInit <- zone_df$Mn2_InitSlw*1000* zone_df$Mc2_RelImpDeno

# Mn2_SlwInitF[Zone] = Mn2_SlwInit[Zone]
zone_df$Mn2_SlwInitF <- zone_df$Mn2_SlwInit

# INIT Mn2_PassInit[Zone] = Mn2_InitPassx[Zone]*1000*Mc2_RelImpDeno[Zone]
zone_df$Mn2_PassInit <- zone_df$Mn2_InitPassx*1000* zone_df$Mc2_RelImpDeno

# Mn2_PassInitF[Zone] = Mn2_PassInit[Zone]
zone_df$Mn2_PassInitF <- zone_df$Mn2_PassInit

# Mn2_PassInitLayer[Zn1,1] = Mn2_PassInitF[Zn1]*AF_DepthAct1[Zn1]*Mc2_SOMDist[1]/Mc2_RelImpDeno[Zn1]+0*(AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_Depth4[Zn4])
# Mn2_PassInitLayer[Zn1,2] = Mn2_PassInitF[Zn1]*AF_Depth2[Zn1]*Mc2_SOMDist[2]/Mc2_RelImpDeno[Zn1]+0*(AF_Depth4[Zn4]+AF_Depth3[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn1,3] = Mn2_PassInitF[Zn1]*AF_Depth3[Zn1]*Mc2_SOMDist[3]/Mc2_RelImpDeno[Zn1]+0*(AF_Depth2[Zn4]+AF_Depth4[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn1,4] = Mn2_PassInitF[Zn1]*AF_Depth4[Zn1]*Mc2_SOMDist[4]/Mc2_RelImpDeno[Zn1]+0*(AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn2,1] = Mn2_PassInitF[Zn2]*AF_DepthAct1[Zn2]*Mc2_SOMDist[1]/Mc2_RelImpDeno[Zn2]+0*(AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_Depth4[Zn4])
# Mn2_PassInitLayer[Zn2,2] = Mn2_PassInitF[Zn2]*AF_Depth2[Zn2]*Mc2_SOMDist[2]/Mc2_RelImpDeno[Zn2]+0*(AF_Depth4[Zn4]+AF_Depth3[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn2,3] = Mn2_PassInitF[Zn2]*AF_Depth3[Zn2]*Mc2_SOMDist[3]/Mc2_RelImpDeno[Zn2]+0*(AF_Depth4[Zn4]+AF_Depth2[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn2,4] = Mn2_PassInitF[Zn2]*AF_Depth4[Zn2]*Mc2_SOMDist[4]/Mc2_RelImpDeno[Zn2]+0*(AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn3,1] = Mn2_PassInitF[Zn3]*AF_DepthAct1[Zn3]*Mc2_SOMDist[1]/Mc2_RelImpDeno[Zn3]+0*(AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_Depth4[Zn4])
# Mn2_PassInitLayer[Zn3,2] = Mn2_PassInitF[Zn3]*AF_Depth2[Zn3]*Mc2_SOMDist[2]/Mc2_RelImpDeno[Zn3]+0*(AF_Depth4[Zn4]+AF_Depth3[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn3,3] = Mn2_PassInitF[Zn3]*AF_Depth3[Zn3]*Mc2_SOMDist[3]/Mc2_RelImpDeno[Zn3]+0*(AF_Depth2[Zn4]+AF_Depth4[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn3,4] = Mn2_PassInitF[Zn3]*AF_Depth4[Zn3]*Mc2_SOMDist[4]/Mc2_RelImpDeno[Zn3]+0*(AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn4,1] = Mn2_PassInitF[Zn4]*AF_DepthAct1[Zn4]*Mc2_SOMDist[1]/Mc2_RelImpDeno[Zn4]+0*(AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_Depth4[Zn4])
# Mn2_PassInitLayer[Zn4,2] = Mn2_PassInitF[Zn4]*AF_Depth2[Zn4]*Mc2_SOMDist[2]/Mc2_RelImpDeno[Zn4]+0*(AF_Depth4[Zn4]+AF_Depth3[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn4,3] = Mn2_PassInitF[Zn4]*AF_Depth3[Zn4]*Mc2_SOMDist[3]/Mc2_RelImpDeno[Zn4]+0*(AF_Depth2[Zn4]+AF_Depth4[Zn4]+AF_DepthAct1[Zn4])
# Mn2_PassInitLayer[Zn4,4] = Mn2_PassInitF[Zn4]*AF_Depth4[Zn4]*Mc2_SOMDist[4]/Mc2_RelImpDeno[Zn4]+0*(AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_DepthAct1[Zn4])
zonelayer_df$Mn2_PassInitF <- rep(zone_df$Mn2_PassInitF, nlayer)
zonelayer_df$Mc2_RelImpDeno <- rep(zone_df$Mc2_RelImpDeno, nlayer)
zonelayer_df$Mc2_SOMDist <- rep(layer_df$Mc2_SOMDist, each = nzone)
zonelayer_df$Mn2_PassInitLayer <- zonelayer_df$Mn2_PassInitF* zonelayer_df$AF_Depth* zonelayer_df$Mc2_SOMDist/zonelayer_df$Mc2_RelImpDeno

# Mn2_PassCorrectedLayer[Zone,SoilLayer] = Mn2_PassInitLayer[Zone,SoilLayer]*Mn2_PassRelLayer[SoilLayer]
zonelayer_df$Mn2_PassRelLayer <- rep(layer_df$Mn2_PassRelLayer, each = nzone)
zonelayer_df$Mn2_PassCorrectedLayer <- zonelayer_df$Mn2_PassInitLayer* zonelayer_df$Mn2_PassRelLayer

# Mn2_DeltaPassSum[Zone] = ARRAYSUM(Mn2_PassCorrectedLayer[Zone,*])-ARRAYSUM(Mn2_PassInitLayer[Zone,*])
zone_df$Mn2_DeltaPassSum <- aggregate(zonelayer_df["Mn2_PassCorrectedLayer"], zonelayer_df["zone"], sum)$Mn2_PassCorrectedLayer-
  aggregate(zonelayer_df["Mn2_PassInitLayer"], zonelayer_df["zone"], sum)$Mn2_PassInitLayer


# Mn2_ActCorrectedInit[Zone] = if Mn2_ActInitF[Zone] and Mn2_SlwInitF[Zone] > 0 then Mn2_ActInitF[Zone]-(Mn2_DeltaPassSum[Zone]*Mn2_ActInitF[Zone]/(Mn2_ActInitF[Zone]+Mn2_SlwInitF[Zone])) else 0
zone_df$Mn2_ActCorrectedInit <- ifelse( zone_df$Mn2_ActInitF & zone_df$Mn2_SlwInitF > 0,
                                        zone_df$Mn2_ActInitF-( zone_df$Mn2_DeltaPassSum* zone_df$Mn2_ActInitF/( zone_df$Mn2_ActInitF+ zone_df$Mn2_SlwInitF)), 0)

# INIT Mc2_Act[Zone,SoilLayer] = if MC2_SomInitType = 1 then (Mn2_ActCorrectedInit[Zone])*Mn_CNAct else 0.03 * 10000*MC2_CorgInit*Mc2_RelImpDeno[Zone]/Mc2_SOMDist[1]
zonelayer_df$Mc2_Act <- ifelse(pars$Mc_par$Mc2_SomInitType == 1, 
                               (zone_df$Mn2_ActCorrectedInit)* pars$Mn_par$Mn_CNAct,
                               0.03 * 10000*MC2_CorgInit* zonelayer_df$Mc2_RelImpDeno/layer_df$Mc2_SOMDist[1])

# Mn2_SlwCorrectedInit[Zone] = if Mn2_SlwInitF[Zone] and Mn2_ActInitF[Zone] > 0 then Mn2_SlwInitF[Zone]-(Mn2_DeltaPassSum[Zone]*Mn2_SlwInitF[Zone]/(Mn2_SlwInitF[Zone]+Mn2_ActInitF[Zone])) else 0
zone_df$Mn2_SlwCorrectedInit <- ifelse( zone_df$Mn2_SlwInitF & zone_df$Mn2_ActInitF > 0,  zone_df$Mn2_SlwInitF- (zone_df$Mn2_DeltaPassSum* zone_df$Mn2_SlwInitF/(zone_df$Mn2_SlwInitF+ zone_df$Mn2_ActInitF)), 0)

# MC2_CSlowFrac = GRAPH(if MC2_SomInitType= 3 then Mc2_CorgInitMeth3/Mc2_CrefMeth3 else MC2_CorgpCref)
Mc2_CSlowFrac <- get_Mc2_CSlowFrac(ifelse(pars$Mc_par$Mc2_SomInitType == 3, pars$Mc_par$Mc2_CorgInitMeth3/pars$Mc_par$Mc2_CrefMeth3, pars$Mc_par$Mc2_CorgpCref))

# INIT Mc2_Slw[Zone,SoilLayer] = if MC2_SomInitType = 1 then (Mn2_SlwCorrectedInit[Zone])*Mn_CNSlw else (MC2_CSlowFrac )* 10000*MC2_CorgInit*Mc2_RelImpDeno[Zone]/Mc2_SOMDist[1]
zone_df$Mc2_Slw <- ifelse(pars$Mc_par$Mc2_SomInitType == 1, zone_df$Mn2_SlwCorrectedInit * pars$Mn_par$Mn_CNSlw,
                                (MC2_CSlowFrac )* 10000*MC2_CorgInit*zone_df$Mc2_RelImpDeno/layer_df$Mc2_SOMDist[1])
zonelayer_df$Mc2_Slw <- rep(zone_df$Mc2_Slw, nlayer)  

# Mc2_Struc[Zone,SoilLayer] = if MC2_SomInitType = 1 then (Mn2_InitStruc[Zone]*1000*Mc2_RelImpDeno[Zone]/(Mc2_SOMDist[1]))*Mn_CNStruc  else 0.01 * 10000*MC2_CorgInit*Mc2_RelImpDeno[Zone]/Mc2_SOMDist[1]
zone_df$Mc2_Struc <- ifelse(pars$Mc_par$Mc2_SomInitType == 1,  (zone_df$Mn2_InitStruc*1000* zone_df$Mc2_RelImpDeno/layer_df$Mc2_SOMDist[1])* pars$Mn_par$Mn_CNStruc,
                                  0.01 * 10000*MC2_CorgInit* zone_df$Mc2_RelImpDeno/layer_df$Mc2_SOMDist[1])
zonelayer_df$Mc2_Struc <- rep(zone_df$Mc2_Struc, nlayer)  

# Mn2_PassCorrectedSumInit[Zone] = ARRAYSUM(Mn2_PassCorrectedLayer[Zone,*])
zone_df$Mn2_PassCorrectedSumInit <- aggregate(zonelayer_df["Mn2_PassCorrectedLayer"], zonelayer_df["zone"], sum)$Mn2_PassCorrectedLayer

# INIT Mc2_Pass[Zone,SoilLayer] = if MC2_SomInitType = 1 then (Mn2_PassCorrectedSumInit[Zone])*Mn_CNPass else (1-0.05-MC2_CSlowFrac )* 10000*MC2_CorgInit*Mc2_RelImpDeno[Zone]/Mc2_SOMDist[1]
zonelayer_df$Mn2_PassCorrectedSumInit <- rep(zone_df$Mn2_PassCorrectedSumInit, nlayer)
zonelayer_df$Mc2_RelImpDeno <- rep(zone_df$Mc2_RelImpDeno, nlayer)
zonelayer_df$Mc2_Pass <- ifelse(
  pars$Mc_par$Mc2_SomInitType == 1,
  zonelayer_df$Mn2_PassCorrectedSumInit * pars$Mn_par$Mn_CNPass,
  (1 - 0.05 - MC2_CSlowFrac) * 10000 * MC2_CorgInit *
    Mc2_RelImpDeno[Zone] / layer_df$Mc2_SOMDist[1]
)

# N_Unit[L1Zn1] = 1
# N_Unit[L1Zn2] = 2
# N_Unit[L1Zn3] = 3
# N_Unit[L1Zn4] = 4
# N_Unit[L2Zn1] = 5
# N_Unit[L2Zn2] = 6
# N_Unit[L2Zn3] = 7
# N_Unit[L2Zn4] = 8
# N_Unit[L3Zn1] = 9
# N_Unit[L3Zn2] = 10
# N_Unit[L3Zn3] = 11
# N_Unit[L3Zn4] = 12
# N_Unit[L4Zn1] = 13
# N_Unit[L4Zn2] = 14
# N_Unit[L4Zn3] = 15
# N_Unit[L4Zn4] = 16

zonelayer_df$N_Unit <- c(1:16)

# N_Init[Init_N] = GRAPH(N_Unit[Init_N])
# (1.00, 0.06), (2.00, 0.06), (3.00, 0.06), (4.00, 0.06), (5.00, 0.08), (6.00, 0.08), (7.00, 0.08), (8.00, 0.08), (9.00, 0.08), (10.0, 0.08), (11.0, 0.08), (12.0, 0.08), (13.0, 0.08), (14.0, 0.08), (15.0, 0.08), (16.0, 0.08)

N_Init_df <- data.frame(
  N_Unit = c(1:16),
  N_Init = c(0.06,0.06,0.06,0.06,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08)
)

# N_NInit1[Zn1] = N_Init[L1Zn1]
# N_NInit1[Zn2] = N_Init[L1Zn2]
# N_NInit1[Zn3] = N_Init[L1Zn3]
# N_NInit1[Zn4] = N_Init[L1Zn4]
# N_Ninit2[Zn1] = N_Init[L2Zn1]
# N_Ninit2[Zn2] = N_Init[L2Zn2]
# N_Ninit2[Zn3] = N_Init[L2Zn3]
# N_Ninit2[Zn4] = N_Init[L2Zn4]
# N_NInit3[Zn1] = N_Init[L3Zn1]
# N_NInit3[Zn2] = N_Init[L3Zn2]
# N_NInit3[Zn3] = N_Init[L3Zn3]
# N_NInit3[Zn4] = N_Init[L3Zn4]
# N_NInit4[Zn1] = N_Init[L4Zn1]
# N_NInit4[Zn2] = N_Init[L4Zn2]
# N_NInit4[Zn3] = N_Init[L4Zn3]
# N_NInit4[Zn4] = N_Init[L4Zn4]

zonelayer_df$N_NInit <- N_Init_df$N_Init

# N_PStParam[P_Param] = GRAPH(N_One[P_Param])
# (1.00, 0.234), (2.00, 0.243), (3.00, 0.238), (4.00, 0.238), (5.00, 0.234), (6.00, 0.243), (7.00, 0.238), (8.00, 0.238), (9.00, 0.234), (10.0, 0.243), (11.0, 0.238), (12.0, 0.238), (13.0, 0.234), (14.0, 0.243), (15.0, 0.238), (16.0, 0.238), (17.0, 0.0005), (18.0, 0.0005), (19.0, 0.0005), (20.0, 0.0005), (21.0, 8.39), (22.0, 8.39), (23.0, 8.39), (24.0, 8.39)

# N_PStParam_df <- data.frame(
#   N_One = c(1:24),
#   N_PStParam = c(0.234,0.243,0.238,0.238,0.234,0.243,0.238,0.238,0.234,0.243,0.238,0.238,0.234,0.243,0.238,0.238,5e-04,5e-04,5e-04,5e-04,8.39,8.39,8.39,8.39)
# )

N_PStParam_list <- as.list(c(0.234,0.243,0.238,0.238,0.234,0.243,0.238,0.238,0.234,0.243,0.238,0.238,0.234,0.243,0.238,0.238,5e-04,5e-04,5e-04,5e-04,8.39,8.39,8.39,8.39))
names(N_PStParam_list) <- N_One

zonelayer_df$N_PStParam <- unlist(N_PStParam_list[c("Pinit11", "Pinit21", "Pinit31", "Pinit41", 
                                             "Pinit12", "Pinit22", "Pinit32", "Pinit42",
                                             "Pinit13", "Pinit23", "Pinit33", "Pinit43",
                                             "Pinit14", "Pinit24", "Pinit34", "Pinit44")])
layer_df$PStMin <- unlist(N_PStParam_list[c("PStMin_1", "PStMin_2", "PStMin_3", "PStMin_4")])
layer_df$PStMax <- unlist(N_PStParam_list[c("PStMax1", "PStMax2", "PStMax3", "PStMax4")])


# N_Init1[Zn1,N] = N_NInit1[Zn1] + 0*N_PStParam[Pinit14]
# N_Init1[Zn1,P] = 0*N_NInit1[Zn1]+N_PStParam[Pinit11]
# N_Init1[Zn2,N] = N_NInit1[Zn2] + 0*N_PStParam[Pinit14]
# N_Init1[Zn2,P] = 0*N_NInit1[Zn1]+N_PStParam[Pinit21]
# N_Init1[Zn3,N] = N_NInit1[Zn3] + 0*N_PStParam[Pinit14]
# N_Init1[Zn3,P] = 0*N_NInit1[Zn1]+N_PStParam[Pinit31]
# N_Init1[Zn4,N] = N_NInit1[Zn2]+ 0*N_PStParam[Pinit14]
# N_Init1[Zn4,P] = 0*N_NInit1[Zn1]+N_PStParam[Pinit41]
# N_Init2[Zn1,N] = N_Ninit2[Zn1] + 0*N_PStParam[Pinit23]
# N_Init2[Zn1,P] = N_PStParam[Pinit12]+0*N_Ninit2[Zn1]
# N_Init2[Zn2,N] = N_Ninit2[Zn2] + 0*N_PStParam[Pinit23]
# N_Init2[Zn2,P] = N_PStParam[Pinit22]+0*N_Ninit2[Zn1]
# N_Init2[Zn3,N] = N_Ninit2[Zn3] + 0*N_PStParam[Pinit23]
# N_Init2[Zn3,P] = N_PStParam[Pinit32]+0*N_Ninit2[Zn1]
# N_Init2[Zn4,N] = N_Ninit2[Zn4] + 0*N_PStParam[Pinit23]
# N_Init2[Zn4,P] = N_PStParam[Pinit42]+0*N_Ninit2[Zn1]
# N_Init3[Zn1,N] = N_NInit3[Zn1]+ 0*N_PStParam[Pinit31]
# N_Init3[Zn1,P] = N_PStParam[Pinit13]+0*N_NInit3[Zn4]
# N_Init3[Zn2,N] = N_NInit3[Zn2] + 0*N_PStParam[Pinit31]
# N_Init3[Zn2,P] = N_PStParam[Pinit23]+0*N_NInit3[Zn4]
# N_Init3[Zn3,N] = N_NInit3[Zn3] + 0*N_PStParam[Pinit31]
# N_Init3[Zn3,P] = N_PStParam[Pinit33]+0*N_NInit3[Zn4]
# N_Init3[Zn4,N] = N_NInit3[Zn4]+ 0*N_PStParam[Pinit31]
# N_Init3[Zn4,P] = N_PStParam[Pinit43]+0*N_NInit3[Zn4]
# N_Init4[Zn1,N] = N_NInit4[Zn1]+0*N_PStParam[Pinit41]
# N_Init4[Zn1,P] = N_PStParam[Pinit14]+0*N_NInit4[Zn1]
# N_Init4[Zn2,N] = N_NInit4[Zn2]+0*N_PStParam[Pinit41]
# N_Init4[Zn2,P] = N_PStParam[Pinit24]+0*N_NInit4[Zn1]
# N_Init4[Zn3,N] = N_NInit4[Zn3]+0*N_PStParam[Pinit41]
# N_Init4[Zn3,P] = 0*N_NInit4[Zn4]+N_PStParam[Pinit43]
# N_Init4[Zn4,N] = N_NInit4[Zn4]+0*N_PStParam[Pinit41]
# N_Init4[Zn4,P] = N_PStParam[Pinit44]+0*N_NInit4[Zn1]
zonelayernut_df$N_Init <- NA
zonelayernut_df[zonelayernut_df$SlNut == "N", ]$N_Init <- zonelayer_df$N_NInit
zonelayernut_df[zonelayernut_df$SlNut == "P", ]$N_Init <- zonelayer_df$N_PStParam

# INIT N_Stock1[Zone,SlNut] = N_Init1[Zone,SlNut]*AF_DepthAct1[Zone]*1000
# INIT N_Stock2[Zone,SlNut] = max(0,N_Init2[Zone,SlNut]*AF_Depth2[Zone]*1000)
# INIT N_Stock3[Zone,SlNut] = max(0,N_Init3[Zone,SlNut]*AF_Depth3[Zone]*1000)
# INIT N_Stock4[Zone,SlNut] = N_Init4[Zone,SlNut]*AF_Depth4[Zone]*1000
zonelayernut_df$AF_Depth <- rep(zonelayer_df$AF_Depth, nrow(nut_df))
zonelayernut_df$N_Stock <- zonelayernut_df$N_Init* zonelayernut_df$AF_Depth*1000

zonelayernut_df[zonelayernut_df$layer %in% c(2,3),]$N_Stock <- pmax(0, zonelayernut_df[zonelayernut_df$layer %in% c(2,3),]$N_Stock)

# GHG_N2_per_NOx[Zone] = GRAPH(GHG_EffWaterfPoreF[Zone])
# (0.00, 0.00), (0.1, 0.00), (0.2, 0.00), (0.3, 0.00), (0.4, 0.00), (0.5, 0.1), (0.6, 0.4), (0.7, 1.60), (0.8, 4.90), (0.9, 10.4), (1, 19.8)
GHG_N2_per_NOx_df <- data.frame(
  GHG_EffWaterfPoreF = seq(0, 1, 0.1),
  GHG_N2_per_NOx = c(0,0,0,0,0,0.1,0.4,1.6,4.9,10.4,19.8)
)

get_GHG_N2_per_NOx <- function(GHG_EffWaterfPoreF) {
  unlist(lapply(GHG_EffWaterfPoreF, function(x) {
    get_graph_y(
      GHG_N2_per_NOx_df,
      x,
      x_column = "GHG_EffWaterfPoreF",
      y_column = "GHG_N2_per_NOx",
      mode = "continues"
    )
  }))
}

# S&B_DeadWoodBurnFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
# (0.00, 0.00), (50.0, 0.025), (100, 0.045), (150, 0.055), (200, 0.065), (250, 0.085), (300, 0.125), (350, 0.185), (400, 0.265), (450, 0.355), (500, 0.435), (550, 0.505), (600, 0.565), (650, 0.605), (700, 0.635)
SB_DeadWoodBurnFrac_df <- data.frame(
  SB_FireTempIncSurf = seq(0, 700, 50),
  SB_DeadWoodBurnFrac = c(0,0.025,0.045,0.055,0.065,0.085,0.125,0.185,0.265,0.355,0.435,0.505,0.565,0.605,0.635)
)

get_SB_DeadWoodBurnFrac <- function(SB_FireTempIncSurf) {
  unlist(lapply(SB_FireTempIncSurf, function(x) {
    get_graph_y(
      SB_DeadWoodBurnFrac_df,
      x,
      x_column = "SB_FireTempIncSurf",
      y_column = "SB_DeadWoodBurnFrac",
      mode = "continues"
    )
  }))
}

# S&B_pHmodPsorp[Zone] = GRAPH(S&B_Topsoil_pH[Zone])
# (0.00, 4.28), (1.00, 3.63), (2.00, 1.78), (3.00, 1.03), (4.00, 0.575), (5.00, 0.475), (6.00, 0.425), (7.00, 0.575), (8.00, 1.03), (9.00, 1.58), (10.0, 1.88)
SB_pHmodPsorp_df <- data.frame(
  SB_Topsoil_pH = c(0:10),
  SB_pHmodPsorp = c(4.28,3.63,1.78,1.03,0.575,0.475,0.425,0.575,1.03,1.58,1.88)
)

get_SB_pHmodPsorp <- function(SB_Topsoil_pH) {
  unlist(lapply(SB_Topsoil_pH, function(x) {
    get_graph_y(
      SB_pHmodPsorp_df,
      x,
      x_column = "SB_Topsoil_pH",
      y_column = "SB_pHmodPsorp",
      mode = "continues"
    )
  }))
}

# INIT S&B_Topsoil_pH[Zone] = S&B_InitialpH
zone_df$SB_Topsoil_pH <- pars$SB_par$SB_InitialpH

# INIT RT_L1FRLength[Zone,Tree] = RT3_TL1FRInit[Zone,Tree]
zonelayertree_df$RT_FRLength <- zonelayertree_df$RT3_TFRInit

# INIT AF_DepthAvg1 = AF_ZoneFrac[Zn1]*AF_DepthAct1[Zn1]+AF_ZoneFrac[Zn2]*AF_DepthAct1[Zn2]+AF_ZoneFrac[Zn3]*AF_DepthAct1[Zn3]+AF_ZoneFrac[Zn4]*AF_DepthAct1[Zn4]
# INIT AF_DepthAvg2 = AF_ZoneFrac[Zn1]*AF_Depth2[Zn1]+AF_ZoneFrac[Zn2]*AF_Depth2[Zn2]+AF_ZoneFrac[Zn3]*AF_Depth2[Zn3]+AF_ZoneFrac[Zn4]*AF_Depth2[Zn4]
# INIT AF_DepthAvg3 = AF_ZoneFrac[Zn1]*AF_Depth3[Zn1]+AF_ZoneFrac[Zn2]*AF_Depth3[Zn2]+AF_ZoneFrac[Zn3]*AF_Depth3[Zn3]+AF_ZoneFrac[Zn4]*AF_Depth3[Zn4]
# INIT AF_DepthAvg4 = AF_ZoneFrac[Zn1]*AF_Depth4[Zn1]+AF_ZoneFrac[Zn2]*AF_Depth4[Zn2]+AF_ZoneFrac[Zn3]*AF_Depth4[Zn3]+AF_ZoneFrac[Zn4]*AF_Depth4[Zn4]
zonelayer_df$AF_Depth_Frac <- zonelayer_df$AF_ZoneFrac * zonelayer_df$AF_Depth
layer_df$AF_DepthAvg <- aggregate(zonelayer_df["AF_Depth_Frac"], zonelayer_df["layer"], sum)$AF_Depth_Frac


# S&B_FirMortSeedBank[Zone] = GRAPH(S&B_FireTempIncTopSoil[Zone])
# (0.00, 0.00), (50.0, 0.00), (100, 0.61), (150, 0.95), (200, 1.00), (250, 1.00), (300, 1.00), (350, 1.00)
SB_FirMortSeedBank_df <- data.frame(
  SB_FireTempIncTopSoil = seq(0, 350, 50),
  SB_FirMortSeedBank = c(0,0,0.61,0.95,1,1,1,1)
)

get_SB_FirMortSeedBank <- function(SB_FireTempIncTopSoil) {
  unlist(lapply(SB_FireTempIncTopSoil, function(x) {
    get_graph_y(
      SB_FirMortSeedBank_df,
      x,
      x_column = "SB_FireTempIncTopSoil",
      y_column = "SB_FirMortSeedBank",
      mode = "continues"
    )
  }))
}

# INIT C_WeedSeedBank[Zone,PlantComp] = if AF_SimulateWeeds? = 1 then C_WeedSeedBankInit*C_UnitConv[PlantComp]*C_SeedConc[PlantComp] else 0
zonepcomp_df$C_UnitConv <- rep(pcomp_df$C_UnitConv, each = nzone)
zonepcomp_df$C_SeedConc <- rep(pcomp_df$C_SeedConc, each = nzone)
zonepcomp_df$C_WeedSeedBank <- ifelse( pars$AF_par$AF_SimulateWeeds_is == 1, pars$C_par$C_WeedSeedBankInit* zonepcomp_df$C_UnitConv* zonepcomp_df$C_SeedConc, 0)



# TF_PollinationEffectiveness = GRAPH(Rain)
# (0.00, 1.00), (10.0, 0.995), (20.0, 0.975), (30.0, 0.93), (40.0, 0.86), (50.0, 0.745), (60.0, 0.635), (70.0, 0.405), (80.0, 0.29), (90.0, 0.225), (100, 0.2)
TF_PollinationEffectiveness_df <- data.frame(
  Rain = seq(0, 100, 10),
  TF_PollinationEffectiveness = c(1,0.995,0.975,0.93,0.86,0.745,0.635,0.405,0.29,0.225,0.2)
)

get_TF_PollinationEffectiveness <- function(Rain) {
  unlist(lapply(Rain, function(x) {
    get_graph_y(
      TF_PollinationEffectiveness_df,
      x,
      x_column = "Rain",
      y_column = "TF_PollinationEffectiveness",
      mode = "continues"
    )
  }))
}

# T_GenLitStage[Tree] = GRAPH(T_Stage[Tree,VegGen])
# (1.00, 0.00), (1.10, 0.035), (1.20, 0.145), (1.30, 0.985), (1.40, 0.14), (1.50, 0.09), (1.60, 0.095), (1.70, 0.15), (1.80, 0.18), (1.90, 0.18), (2.00, 0.13)

T_GenLitStage_df <- data.frame(
  T_Stage = seq(1, 2, 0.1),
  T_GenLitStage = c(0,0.035,0.145,0.985,0.14,0.09,0.095,0.15,0.18,0.18,0.13)
)

get_T_GenLitStage <- function(T_Stage) {
  unlist(lapply(T_Stage, function(x) {
    get_graph_y(
      T_GenLitStage_df,
      x,
      x_column = "T_Stage",
      y_column = "T_GenLitStage",
      mode = "continues"
    )
  }))
}


# Mc_ActResp = 0.85 - 0.68*Mc_TextLitLayer
Mc_ActResp <- 0.85 - 0.68*pars$Mc_par$Mc_TextLitLayer

# Mc_EffActSlw = (1-Mc_ActResp-0.004)
Mc_EffActSlw <- (1-Mc_ActResp-0.004)



## INIT END ########




# LF_H2MaxDailyFlow[Zone] = W_KSatH2[Zone]*(AF_SlopeSoilHoriz/100)*AF_Depth2[Zone]
# LF_H3MaxDailyFlow[Zone] = W_KSatH3[Zone]*(AF_SlopeSoilHoriz/100)*AF_Depth3[Zone]
# LF_H4MaxDailyFlow[Zone] = W_KSatH4[Zone]*(AF_SlopeSoilHoriz/100)*AF_Depth4[Zone]
zonelayer_df$LF_HMaxDailyFlow <- zonelayer_df$W_KSatH * (pars$AF_par$AF_SlopeSoilHoriz /
                                                           100) * zonelayer_df$AF_Depth

# LF_H1MaxMailyFlow[Zn1] = (W_KSatH1[Zn1]*AF_DepthAct1[Zn4]*AF_SlopeSoilHoriz/100)
# LF_H1MaxMailyFlow[Zn2] = (W_KSatH1[Zn2]*(AF_DepthAct1[Zn2]+AF_DepthAct1[Zn1])/2*AF_SlopeSoilHoriz/100)
# LF_H1MaxMailyFlow[Zn3] = (W_KSatH1[Zn3]*(AF_DepthAct1[Zn3]+AF_DepthAct1[Zn2])/2*AF_SlopeSoilHoriz/100)
# LF_H1MaxMailyFlow[Zn4] = (W_KSatH1[Zn4]*(AF_DepthAct1[Zn4]+AF_DepthAct1[Zn3])/2*AF_SlopeSoilHoriz/100)

l1 <- zonelayer_df[zonelayer_df$layer == 1, ]
a <- c(l1[l1$zone == 4, ]$AF_Depth, tail(l1$AF_Depth, -1))
b <- c(l1[l1$zone == 4, ]$AF_Depth, head(l1$AF_Depth, -1))
zone_df$LF_H1MaxMailyFlow <- l1$W_KSatH * (a + b) /
  2 * pars$AF_par$AF_SlopeSoilHoriz / 100

#TODO: assuming LF_H1MaxMailyFlow == LF_HMaxDailyFlow?
zonelayer_df[zonelayer_df$layer == 1, ]$LF_HMaxDailyFlow <- zone_df$LF_H1MaxMailyFlow


# LF_MaxProfHOutflow = (LF_H1MaxMailyFlow[Zn1]+LF_H2MaxDailyFlow[Zn1]+LF_H3MaxDailyFlow[Zn1]+LF_H4MaxDailyFlow[Zn1])
LF_MaxProfHOutflow <- zone_df[zone_df$zone == 1, ]$LF_H1MaxMailyFlow + sum(zonelayer_df[zonelayer_df$zone == 1 &
                                                                                          zonelayer_df$layer %in% c(2:4), "LF_HMaxDailyFlow"])


# W_FieldCap1[Zone] = max(W_FieldCapKcrit1[Zone],W_ThetaP1[Zone])
# W_FieldCap2[Zone] = max(W_FieldCapKcrit2[Zone],W_ThetaP2[Zone])
# W_FieldCap3[Zone] = max(W_ThetaP3[Zone],W_FieldCapKcrit3[Zone])
# W_FieldCap4[Zone] = max(W_FieldCapKcrit4[Zone],W_ThetaP4[Zone])
zonelayer_df$W_FieldCap <- pmax(zonelayer_df$W_FieldCapKcrit, zonelayer_df$W_ThetaP)
zonelayer_df$W_WatDef <- zonelayer_df$W_FieldCap * zonelayer_df$AF_Depth * 1000 - zonelayer_df$W_Stock

# W_WatDefLog1[Zone] = W_PoreVol[Zone,1]*AF_DepthAct1[Zone]*1000-W_Stock1[Zone]
# W_WatDefLog2[Zone] = W_PoreVol[Zone,2]*AF_Depth2[Zone]*1000-W_Stock2[Zone]
# W_WatDefLog3[Zone] = W_PoreVol[Zone,3]*AF_Depth3[Zone]*1000-W_Stock3[Zone]
# W_WatDefLog4[Zone] = W_PoreVol[Zone,4]*AF_Depth4[Zone]*1000-W_Stock4[Zone]
zonelayer_df$W_WatDefLog <- zonelayer_df$W_PoreVol * zonelayer_df$AF_Depth * 1000 - zonelayer_df$W_Stock


# LF_MaxVInflow1[Zone] = IF W_WaterLog? = 0 THEN max(W_WatDef1[Zone]+min(LF_V1MaxDailyFlow[Zone],LF_MaxVInflow2[Zone]),0) ELSE max(W_WatDefLog1[Zone]+min(LF_V1MaxDailyFlow[Zone],LF_MaxVInflow2[Zone]),0)
# LF_MaxVInflow2[Zone] = IF W_WaterLog? = 0 then max(W_WatDef2[Zone]+min(LF_V2MaxDailyFlow[Zone],LF_MaxVInflow3[Zone]),0) else max(W_WatDefLog2[Zone]+min(LF_V2MaxDailyFlow[Zone],LF_MaxVInflow3[Zone]),0)
# LF_MaxVInflow3[Zone] = IF W_WaterLog? = 0 then max(W_WatDef3[Zone]+min(LF_V3MaxDailyFlow[Zone],LF_MaxVInflow4[Zone]),0) else max(W_WatDefLog3[Zone]+min(LF_V3MaxDailyFlow[Zone],LF_MaxVInflow4[Zone]),0)
# LF_MaxVInflow4[Zone] = IF W_WaterLog? = 0 then max(W_WatDef4[Zone]+LF_V4MaxDailyFlow[Zone],0)

zonelayer_df$LF_MaxVInflow <- 0
if (pars$W_par$W_WaterLog_is == 0) {
  zonelayer_df$W_WatDef_calc <- zonelayer_df$W_WatDef
} else {
  zonelayer_df$W_WatDef_calc <- zonelayer_df$W_WatDefLog
}

zonelayer_df[zonelayer_df$layer == 4, ]$LF_MaxVInflow <- max(zonelayer_df[zonelayer_df$layer == 4, ]$W_WatDef_calc + zonelayer_df[zonelayer_df$layer == 4, ]$LF_VMaxDailyFlow,
                                                             0)

zonelayer_df[zonelayer_df$layer == 3, ]$LF_MaxVInflow <- max(
  zonelayer_df[zonelayer_df$layer == 3, ]$W_WatDef_calc + min(zonelayer_df[zonelayer_df$layer ==
                                                                             3, ]$LF_VMaxDailyFlow, zonelayer_df[zonelayer_df$layer == 4, ]$LF_MaxVInflow),
  0
)
zonelayer_df[zonelayer_df$layer == 2, ]$LF_MaxVInflow <- max(
  zonelayer_df[zonelayer_df$layer == 2, ]$W_WatDef_calc + min(zonelayer_df[zonelayer_df$layer ==
                                                                             2, ]$LF_VMaxDailyFlow, zonelayer_df[zonelayer_df$layer == 3, ]$LF_MaxVInflow),
  0
)
zonelayer_df[zonelayer_df$layer == 1, ]$LF_MaxVInflow <- max(
  zonelayer_df[zonelayer_df$layer == 1, ]$W_WatDef_calc + min(zonelayer_df[zonelayer_df$layer ==
                                                                             1, ]$LF_VMaxDailyFlow, zonelayer_df[zonelayer_df$layer == 2, ]$LF_MaxVInflow),
  0
)

# LF_MVI1ZF[Zone] = AF_ZoneFrac[Zone]*LF_MaxVInflow1[Zone]
zonelayer_df$LF_MVIZF <- zonelayer_df$AF_ZoneFrac * zonelayer_df$LF_MaxVInflow


# LF_MaxProfVAbsorption = ARRAYSUM(LF_MVI1ZF[*])
LF_MaxProfVAbsorption <- sum(zonelayer_df[zonelayer_df$layer == 1, ]$LF_MVIZF)

# S_SurfInfiltrAct[Zone] = S_SurfInfiltrPerKsatDef[Zone]*S_KsatInitV1[Zone]*10^((-0.52*(S_BDActOverBDRefInfiltr[Zone]^2)-0.69*S_BDActOverBDRefInfiltr[Zone]+1.21))
zone_df$S_SurfInfiltrAct <- zone_df$S_SurfInfiltrPerKsatDef * zonelayer_df[zonelayer_df$layer == 1, ]$S_KsatInitV *
  10^((
    -0.52 * (zone_df$S_BDActOverBDRefInfiltr^2) - 0.69 * zone_df$S_BDActOverBDRefInfiltr +
      1.21
  ))
# LF_SICZF[Zone] = AF_ZoneFrac[Zone]*S_SurfInfiltrAct[Zone]
zone_df$LF_SICZF <- zone_df$AF_ZoneFrac * zone_df$S_SurfInfiltrAct



#TODO: this code below is still incorrect?

zone_df$Cq_CType <- zone_df$Cq_CropWeedSwitch
zone_df$Cq_CType <- ifelse(zone_df$Cq_CType %in% c(1:4), zone_df$Cq_CType, 5)

#### Crop par curr ################################

update_Cq_par()
zone_df$Cq_StageAfterHarvest <- ifelse(zone_df$Cq_SingleCycle_is_Cur == 1, 0, 0.7)



#
# zone_df$Cq_PolyResid <- unlist(Cq_df[Cq_df$Cq_Unit == "PolypRes", zone_df$Cq_CType])
# zone_df$Cq_PolyRt <- unlist(Cq_df[Cq_df$Cq_Unit == "PolypRoot", zone_df$Cq_CType])
# zone_df$Cq_PotSuctAlphMaxCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "AlphMax", zone_df$Cq_CType])
# zone_df$Cq_PotSuctAlphMinCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "AlphMin", zone_df$Cq_CType])
# zone_df$Cq_RainWStorCapCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "RainStorCap", zone_df$Cq_CType])
# zone_df$Cq_RelLightMaxCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "RelLight", zone_df$Cq_CType])
# zone_df$Cq_RemobFrac <- unlist(Cq_df[Cq_df$Cq_Unit == "MaxRemob", zone_df$Cq_CType])
# zone_df$Cq_RtDiam <- unlist(Cq_df[Cq_df$Cq_Unit == "RtDiam", zone_df$Cq_CType])
# zone_df$Cq_SingleCycle_Cur <- unlist(Cq_df[Cq_df$Cq_Unit == "SingleCycle?", zone_df$Cq_CType])
# zone_df$Cq_StageAfterHarvest <- ifelse(zone_df$Cq_SingleCycle_Cur == 1, 0, 0.7)
# zone_df$Cq_TranspRatioCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "TranspRatio", zone_df$Cq_CType])
# zone_df$Rt_CRtAllocRespCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "AllocResp", zone_df$Cq_CType])
# zone_df$Rt_CSRLCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "SRL", zone_df$Cq_CType])
#
# zone_df$Cq_CTimeGenCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "TimeGen", zone_df$Cq_CType])
# zone_df$Cq_CTimeVegCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "TimeVeg", zone_df$Cq_CType])
# zone_df$Cq_DOYFlwDOYBegin <- unlist(Cq_df[Cq_df$Cq_Unit == "FlwBegin", zone_df$Cq_CType])
# zone_df$Cq_DOYFlwEnd <- unlist(Cq_df[Cq_df$Cq_Unit == "FlwEnd", zone_df$Cq_CType])
# zone_df$Cq_GroMaxCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "GroMax", zone_df$Cq_CType])
# zone_df$Cq_GSeedCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "SeedInit", zone_df$Cq_CType])
# zone_df$Cq_HBiomConvCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "HBiomConv", zone_df$Cq_CType])
# zone_df$Cq_kLightCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "kLight", zone_df$Cq_CType])
# zone_df$Cq_LignResidCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "LignRes", zone_df$Cq_CType])
# zone_df$Cq_LignRootResCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "LignRootRes", zone_df$Cq_CType])
# zone_df$Cq_MaxMycInf <- unlist(Cq_df[Cq_df$Cq_Unit == "MycMaxInf", zone_df$Cq_CType])
# zone_df$Cq_NFixDailyFracCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "DayFrac", zone_df$Cq_CType])
# zone_df$Cq_NFixDWMaxFracCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "FixDWMax", zone_df$Cq_CType])
# zone_df$Cq_NFixDWUnitCostCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "FixDWUnit", zone_df$Cq_CType])
# zone_df$Cq_NFixRespCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "FixResp", zone_df$Cq_CType])
# zone_df$Cq_NFixVariable_Curr <- unlist(Cq_df[Cq_df$Cq_Unit == "NFix?", zone_df$Cq_CType])
# zone_df$Cq_NRtConcCurr <- unlist(Cq_df[Cq_df$Cq_Unit == "RtConc", zone_df$Cq_CType])



# Cq_CLWRCurr[Zone] = If(Cq_CType[Zone]=1)then(Cq_CLWR[Type1, Zone]) else if(Cq_CType[Zone]=2)then(Cq_CLWR[Type2, Zone]) else if(Cq_CType[Zone]=3)then(Cq_CLWR[Type3, Zone]) else
#   if(Cq_CType[Zone]=4)then(Cq_CLWR[Type4, Zone]) else
# (Cq_CLWR[Type5, Zone])

zone_df$Cq_CRelLUECurr <- apply(zone_df[c("zone", "Cq_Stage", "Cq_CType")], 1, function(x) {
  get_zone_stage_par(x["zone"], x["Cq_Stage"], x["Cq_CType"], "Cq_CRelLUE")
})

zone_df$Cq_CLWRCurr <- apply(zone_df[c("zone", "Cq_Stage", "Cq_CType")], 1, function(x) {
  get_zone_stage_par(x["zone"], x["Cq_Stage"], x["Cq_CType"], "Cq_CLWR")
})

zone_df$Cq_CHarvAllocCurr <- apply(zone_df[c("zone", "Cq_Stage", "Cq_CType")], 1, function(x) {
  get_zone_stage_par(x["zone"], x["Cq_Stage"], x["Cq_CType"], "Cq_CHarvAlloc")
})

zone_df$Cq_CSLACurr <- apply(zone_df[c("zone", "Cq_Stage", "Cq_CType")], 1, function(x) {
  get_zone_stage_par(x["zone"], x["Cq_Stage"], x["Cq_CType"], "Cq_CSLA")
})

zone_df$Cq_CRtAllocCurr <- apply(zone_df[c("zone", "Cq_Stage", "Cq_CType")], 1, function(x) {
  get_zone_stage_par(x["zone"], x["Cq_Stage"], x["Cq_CType"], "Cq_CRtAlloc")
})


# zone_df$crop_type <- plant_data$crop_types[zone_df$Cq_CType + 1]
#
# #ambil CLWR dari table
#
# zone_df$Cq_CLWRCurr <- apply(zone_df[c("Cq_Stage", "crop_type")], 1, function(x) {
#   get_graph_y(plant_data$Cq_LWR_df,
#   x["Cq_Stage"],
#   y_column = x["crop_type"],
#   mode = "continues")
# })

# zone_df$Cq_CSLACurr <- apply(zone_df[c("Cq_Stage", "crop_type")], 1, function(x) {
#   get_graph_y(plant_data$Cq_SLA_df,
#   x["Cq_Stage"],
#   y_column = x["crop_type"],
#   mode = "continues")
# })

# C_LAI[Zone] = Cq_CLWRCurr[Zone]*Cq_CSLACurr[Zone]*C_BiomStLv[Zone,DW]
zone_df$C_LAI <- zone_df$Cq_CLWRCurr * zone_df$Cq_CSLACurr * zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$C_BiomStLv


# T_LeavesPotPresent?[Tree] = if (T_DOY_Compl_1_LfFall[Tree] < T_DOY_1_LfFlush[Tree]) or (T_DOY_Compl_2_LfFall[Tree] < T_DOY_2_LfFlush[Tree]) then 1 + Max(0,min(1,(mod(time,365) - T_DOY_1_LfFlush[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_Compl_1_LfFall[Tree]))) + Max(0,min(1,(mod(time,365) - T_DOY_2_LfFlush[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_Compl_2_LfFall[Tree])))
# else
#   Max(0,min(1,(mod(time,365) - T_DOY_1_LfFlush[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_Compl_1_LfFall[Tree]))) + Max(0,min(1,(mod(time,365) - T_DOY_2_LfFlush[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_Compl_2_LfFall[Tree])))
tree_df$T_LeavesPotPresent_f <- pmax(0, pmin(1, ((time %% 365) - tree_df$T_DOY_1_LfFlush))) -
  pmax(0, pmin(1, ((time %% 365) - tree_df$T_DOY_Compl_1_LfFall
  ))) +
  pmax(0, pmin(1, ((time %% 365) - tree_df$T_DOY_2_LfFlush))) -
  pmax(0, pmin(1, ((time %% 365) - tree_df$T_DOY_Compl_2_LfFall
  )))

tree_df$T_LeavesPotPresent <- ifelse((tree_df$T_DOY_Compl_1_LfFall < tree_df$T_DOY_1_LfFlush) |
                                       (tree_df$T_DOY_Compl_2_LfFall < tree_df$T_DOY_2_LfFlush),
                                     1 + tree_df$T_LeavesPotPresent_f,
                                     tree_df$T_LeavesPotPresent_f
)


# T_LAI[Tree] = min(T_LAIMax[Tree], (T_LfTwig[DW, Tree])*T_LWR[Tree]*T_SLA[Tree] * T_LeavesPotPresent?[Tree])
tree_df$T_LAI <- pmin(
  tree_df$T_LAIMax,
  treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_LfTwig * tree_df$T_LWR * tree_df$T_SLA *  tree_df$T_LeavesPotPresent
)
# T_LAIMin[Tree] = T_LAIMinMaxRatio[Tree]*T_LAIMax[Tree]
tree_df$T_LAIMin <- tree_df$T_LAIMinMaxRatio * tree_df$T_LAIMax





# TF_CrownRadius[Tree] = (TF_RecentFrondLength[Tree]*TF_RadiometricFraction[Tree])+(TF_TrunkDiam[Tree]*10^-2)/2
tree_df$TF_CrownRadius <- (tree_df$TF_RecentFrondLength * tree_df$TF_RadiometricFraction) +
  (tree_df$TF_TrunkDiam * 10^-2) / 2



# T_BiomAG[PlantComp,Tree] = T_LfTwig[PlantComp,Tree]+T_SapWood[PlantComp,Tree]+T_GroRes[PlantComp,Tree]
treepcomp_df$T_BiomAG <- treepcomp_df$T_LfTwig + treepcomp_df$T_SapWood + treepcomp_df$T_GroRes


# INIT T_SapWoodEqDiam[Tree] = if (T_DiamSlopeBiom[Tree]>0  and T_Treesperha[Tree]*T_DiamBiom1[Tree]>0  )then (T_BiomAG[DW,Tree]*10000/(T_Treesperha[Tree]*T_DiamBiom1[Tree]))^(1/T_DiamSlopeBiom[Tree]) else 0
tree_df$T_SapWoodEqDiam <- ifelse(
  tree_df$T_DiamSlopeBiom > 0  &
    tree_df$T_Treesperha * tree_df$T_DiamBiom1 > 0 ,
  (
    treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_BiomAG * 10000 / (tree_df$T_Treesperha * tree_df$T_DiamBiom1)
  )^(1 / tree_df$T_DiamSlopeBiom),
  0
)

# T_StemDiam[Tree] = (T_HeartWoodDiam[Tree]^2+T_SapWoodEqDiam[Tree]^2)^0.5
tree_df$T_StemDiam <- (tree_df$T_HeartWoodDiam^2 + tree_df$T_SapWoodEqDiam^2)^0.5
# T_CanWidthD[Tree] = if T_ApplyPalm?[Tree]=1 then TF_CrownRadius[Tree] else 0.05*T_CanWidthMax[Tree]+0.95*min(T_StemDiam[Tree]/T_DCanWidthMax,1)*T_CanWidthMax[Tree]
tree_df$T_CanWidthD <- ifelse(
  tree_df$T_ApplyPalm_is == 1,
  tree_df$TF_CrownRadius,
  0.05 * tree_df$T_CanWidthMax +
    0.95 * pmin(tree_df$T_StemDiam / pars$T_par$T_DCanWidthMax, 1) *
    tree_df$T_CanWidthMax
)
# T_CanWidthMaxRel[Tree] = MIN(T_CanWidthD[Tree]/AF_ZoneTot,1)
tree_df$T_CanWidthMaxRel <- pmin(tree_df$T_CanWidthD / pars$AF_par$AF_ZoneTot, 1)
# T_CanWidthRel[Tree] = MIN(T_LAI[Tree]/T_LAIMin[Tree],T_CanWidthMaxRel[Tree])
tree_df$T_CanWidthRel <- pmin(tree_df$T_LAI / tree_df$T_LAIMin, tree_df$T_CanWidthMaxRel)

# z_sp <- rep(zone_df$zone, nrow(tree_df))
sp_z <- rep(tree_df$species, each = nzone)
# zonetree_df <- data.frame(zone = z_sp)
zonetree_df$AF_ZoneTree <- rep(zone_df$AF_ZoneTree, nrow(tree_df))
zonetree_df$AF_ZoneFrac <- rep(zone_df$AF_ZoneFrac, nrow(tree_df))
zonetree_df$T_TreeinZone <- rep(zone_df$T_TreeinZone, nrow(tree_df))

# zonetree_df$tree_id <- rep(1:nrow(tree_df), each = nzone)
# zonetree_df$species <- rep(tree_df$species, each = nzone)
zonetree_df$AF_TreePosit <- rep(tree_df$AF_TreePosit, each = nzone)
zonetree_df$T_RelPosinZone <- rep(tree_df$T_RelPosinZone, each = nzone)
zonetree_df$T_CanWidthRel <- rep(tree_df$T_CanWidthRel, each = nzone)
zonetree_df$T_klight <- rep(tree_df$T_klight, each = nzone)
zonetree_df$T_LAI <- rep(tree_df$T_LAI, each = nzone)
zonetree_df$T_RainWStorCap <- rep(tree_df$T_RainWStorCap, each = nzone)



# INIT T_PrunWeighTot[Tree] = T_PrunWeight[Zn1,Tree]*AF_ZoneFrac[Zn1]+T_PrunWeight[Zn2,Tree]*AF_ZoneFrac[Zn2]+T_PrunWeight[Zn3,Tree]*AF_ZoneFrac[Zn3]+T_PrunWeight[Zn4,Tree]*AF_ZoneFrac[Zn4]
zonetree_df$T_PrunWeighTot_a <- zonetree_df$T_PrunWeight* zonetree_df$AF_ZoneFrac
tree_df$T_PrunWeighTot <- aggregate(zonetree_df["T_PrunWeighTot_a"], zonetree_df["tree_id"], sum)$T_PrunWeighTot_a
tree_df$T_PrunWeigh_sum <- aggregate(zonetree_df["T_PrunWeight"], zonetree_df["tree_id"], sum)$T_PrunWeight
zonetree_df$T_PrunWeighTot <- rep(tree_df$T_PrunWeighTot, each = nzone)
zonetree_df$T_PrunWeigh_sum <- rep(tree_df$T_PrunWeigh_sum, each = nzone)

# INIT T_LifallWeightTot[Zone,Tree] = T_LifallWeight[Zn1,Tree]*AF_ZoneFrac[Zn1]+T_LifallWeight[Zn2,Tree]*AF_ZoneFrac[Zn2]+T_LifallWeight[Zn3,Tree]*AF_ZoneFrac[Zn3]+T_LifallWeight[Zn4,Tree]*AF_ZoneFrac[Zn4]
zonetree_df$T_LifallWeightTot_a <- zonetree_df$T_LifallWeight* zonetree_df$AF_ZoneFrac
tree_df$T_LifallWeightTot <- aggregate(zonetree_df["T_LifallWeightTot_a"], zonetree_df["tree_id"], sum)$T_LifallWeightTot_a
zonetree_df$T_LifallWeightTot <- rep(tree_df$T_LifallWeightTot, each = nzone)



# T_Tree1ToTheLeft?[Zone,Tree] = if AF_TreePosit[Tree]<AF_ZoneTree[Zone] then 1 else 0
# T_Tree1ToTheRight?[Zone,Tree] = if AF_TreePosit[Tree]>AF_ZoneTree[Zone] then 1 else 0
# T_Tree2ToTheLeft?[Zone,Tree] = if AF_TreePosit[Tree] < AF_ZoneTree[Zone]-1 then 1 else 0
# T_Tree2ToTheRight?[Zone,Tree] = if AF_TreePosit[Tree] > AF_ZoneTree[Zone]+1 then 1 else 0
# T_Tree3ToTheLeft?[Zone,Tree] = if AF_TreePosit[Tree] < AF_ZoneTree[Zone]-2 then 1 else 0
# T_Tree3ToTheRight?[Zone,Tree] = if AF_TreePosit[Tree] > AF_ZoneTree[Zone]+2 then 1 else 0
zonetree_df$T_Tree1ToTheLeft <- ifelse(zonetree_df$AF_TreePosit < zonetree_df$AF_ZoneTree, 1, 0)
zonetree_df$T_Tree1ToTheRight <- ifelse(zonetree_df$AF_TreePosit > zonetree_df$AF_ZoneTree, 1, 0)
zonetree_df$T_Tree2ToTheLeft <- ifelse(zonetree_df$AF_TreePosit < zonetree_df$AF_ZoneTree -
                                         1, 1, 0)
zonetree_df$T_Tree2ToTheRight <- ifelse(zonetree_df$AF_TreePosit > zonetree_df$AF_ZoneTree +
                                          1, 1, 0)
zonetree_df$T_Tree3ToTheLeft <- ifelse(zonetree_df$AF_TreePosit < zonetree_df$AF_ZoneTree -
                                         2, 1, 0)
zonetree_df$T_Tree3ToTheRight <- ifelse(zonetree_df$AF_TreePosit > zonetree_df$AF_ZoneTree +
                                          2, 1, 0)

# AF_ZoneTreeLeftRight[Zone,Tree] = if AF_TreePosit[Tree] = AF_ZoneTree[Zn1] then AF_ZoneFrac[Zn1] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn2] then AF_ZoneFrac[Zn2] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn3] then AF_ZoneFrac[Zn3] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn4] then AF_ZoneFrac[Zn4] else 0
zonetree_df$AF_ZoneTreeLeftRight <- zonetree_df$AF_ZoneFrac

# AF_RelZoneTreeLeft[Zone,Tree] = (1-T_RelPosinZone[Tree])*AF_ZoneTreeLeftRight[Zone,Tree]
# AF_RelZoneTreeNext2Left[Zone,Tree] = if AF_TreePosit[Tree] = AF_ZoneTree[Zn1]-2 then AF_ZoneFrac[Zn1] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn2]-2 then AF_ZoneFrac[Zn2] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn3]-2 then AF_ZoneFrac[Zn3] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn4] -2 then AF_ZoneFrac[Zn4] else 0
# AF_RelZoneTreeNext2Right[Zone,Tree] = if AF_TreePosit[Tree] = AF_ZoneTree[Zn1]+2 then AF_ZoneFrac[Zn1] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn2]+2 then AF_ZoneFrac[Zn2] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn3]+2 then AF_ZoneFrac[Zn3] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn4] +2 then AF_ZoneFrac[Zn4] else 0
# AF_RelZoneTreeNextLeft[Zone,Tree] = if AF_TreePosit[Tree] = AF_ZoneTree[Zn1]-1 then AF_ZoneFrac[Zn1] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn2]-1 then AF_ZoneFrac[Zn2] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn3]-1 then AF_ZoneFrac[Zn3] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn4] -1 then AF_ZoneFrac[Zn4] else 0
# AF_RelZoneTreeNextRight[Zone,Tree] = if AF_TreePosit[Tree] = AF_ZoneTree[Zn1]+1 then AF_ZoneFrac[Zn1] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn2]+1 then AF_ZoneFrac[Zn2] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn3]+1 then AF_ZoneFrac[Zn3] else if AF_TreePosit[Tree] = AF_ZoneTree[Zn4] +1 then AF_ZoneFrac[Zn4] else 0
# AF_RelZoneTreeRight[Zone,Tree] = T_RelPosinZone[Tree]*AF_ZoneTreeLeftRight[Zone,Tree]
zonetree_df$AF_RelZoneTreeLeft <- (1 - zonetree_df$T_RelPosinZone) *
  zonetree_df$AF_ZoneTreeLeftRight
zonetree_df$AF_RelZoneTreeNext2Left <- ifelse(zonetree_df$AF_TreePosit == zonetree_df$AF_ZoneTree - 2,
                                              zonetree_df$AF_ZoneFrac,
                                              0)
zonetree_df$AF_RelZoneTreeNext2Right <-  ifelse(zonetree_df$AF_TreePosit == zonetree_df$AF_ZoneTree + 2,
                                                zonetree_df$AF_ZoneFrac,
                                                0)
zonetree_df$AF_RelZoneTreeNextLeft <- ifelse(zonetree_df$AF_TreePosit == zonetree_df$AF_ZoneTree - 1,
                                             zonetree_df$AF_ZoneFrac,
                                             0)
zonetree_df$AF_RelZoneTreeNextRight <- ifelse(zonetree_df$AF_TreePosit ==  zonetree_df$AF_ZoneTree + 1,
                                              zonetree_df$AF_ZoneFrac,
                                              0)
zonetree_df$AF_RelZoneTreeRight <- zonetree_df$T_RelPosinZone * zonetree_df$AF_ZoneTreeLeftRight




# T_RelCanWidthZn[Zone,Tree] = if AF_ZoneFrac[Zone] = 0 then 0 else if T_TreeinZone?[Zone,Tree]=1 then min (1,(max(0,min(T_RelPosinZone[Tree]*AF_ZoneFrac[Zone],T_CanWidthRel[Tree]))+max(0,min((1-T_RelPosinZone[Tree])*AF_ZoneFrac[Zone],T_CanWidthRel[Tree])))/AF_ZoneFrac[Zone]) else max(0,min(1,(T_CanWidthRel[Tree]-(T_Tree1ToTheLeft?[Zone,Tree]*AF_RelZoneTreeLeft[Zone,Tree]+T_Tree2ToTheLeft?[Zone,Tree]*AF_RelZoneTreeNextleft[Zone,Tree]+T_Tree3ToTheLeft?[Zone,Tree]*AF_RelZoneTreeNext2Left[Zone,Tree]+T_Tree1ToTheRight?[Zone,Tree]*AF_RelZoneTreeRight[Zone,Tree]+T_Tree2ToTheRight?[Zone,Tree]*AF_RelZoneTreeNextRight[Zone,Tree]+T_Tree3ToTheRight?[Zone,Tree]*AF_RelZoneTreeNext2Right[Zone,Tree]))/AF_ZoneFrac[Zone]))
zonetree_df$T_RelCanWidthZn <-
  ifelse (zonetree_df$AF_ZoneFrac == 0,
          0,
          ifelse (
            zonetree_df$T_TreeinZone == 1,
            pmin(1, (pmax(
              0,
              pmin(
                zonetree_df$T_RelPosinZone * zonetree_df$AF_ZoneFrac,
                zonetree_df$T_CanWidthRel
              )
            ) +
              pmax(
                0, min((1 - zonetree_df$T_RelPosinZone) * zonetree_df$AF_ZoneFrac,
                       zonetree_df$T_CanWidthRel
                )
              )) / zonetree_df$AF_ZoneFrac)
            ,
            pmax(0, pmin(
              1, (
                zonetree_df$T_CanWidthRel -
                  (
                    zonetree_df$T_Tree1ToTheLeft * zonetree_df$AF_RelZoneTreeLeft +
                      zonetree_df$T_Tree2ToTheLeft * zonetree_df$AF_RelZoneTreeNextLeft +
                      zonetree_df$T_Tree3ToTheLeft * zonetree_df$AF_RelZoneTreeNext2Left +
                      zonetree_df$T_Tree1ToTheRight * zonetree_df$AF_RelZoneTreeRight +
                      zonetree_df$T_Tree2ToTheRight * zonetree_df$AF_RelZoneTreeNextRight +
                      zonetree_df$T_Tree3ToTheRight * zonetree_df$AF_RelZoneTreeNext2Right
                  )
              ) / zonetree_df$AF_ZoneFrac
            ))
          ))




# T_LAICan[Tree] = IF(T_CanWidthRel[Tree]>0)THEN(MAX(T_LAI[Tree]/T_CanWidthRel[Tree],0))ELSE(0)
zonetree_df$T_LAICan <- ifelse(
  zonetree_df$T_CanWidthRel > 0,
  pmax(zonetree_df$T_LAI / zonetree_df$T_CanWidthRel, 0),
  0
)

# T_LAIEff[Zone,Tree] = IF T_klight[Tree]>0 AND (1-T_RelCanWidthZn[Zone,Tree]*(1-exp(-T_klight[Tree]*T_LAICan[Tree])))>0 THEN -LOGN(1-T_RelCanWidthZn[Zone,Tree]*(1-exp(-T_klight[Tree]*T_LAICan[Tree])))/T_klight[Tree] ELSE 0
zonetree_df$T_LAIEff <-
  ifelse(
    zonetree_df$T_klight > 0 &
      (1 - zonetree_df$T_RelCanWidthZn * (
        1 - exp(-zonetree_df$T_klight * zonetree_df$T_LAICan)
      )) > 0,
    -log(1 - zonetree_df$T_RelCanWidthZn * (
      1 - exp(-zonetree_df$T_klight * zonetree_df$T_LAICan)
    )) / zonetree_df$T_klight
    ,
    0
  )

# Rain_WatStorTTot[Zone] = T_LAIEff[Zone,Sp1]*T_RainWStorCap[Sp1]+T_LAIEff[Zone,Sp2]*T_RainWStorCap[Sp2]+T_LAIEff[Zone,Sp3]*T_RainWStorCap[Sp3]
zonetree_df$Rain_WatStorTTot_sp <- zonetree_df$T_LAIEff * zonetree_df$T_RainWStorCap
df <- zonetree_df[c("zone", "Rain_WatStorTTot_sp")]
zdf <- aggregate(zonetree_df$Rain_WatStorTTot_sp,
                 by = list(zone = zonetree_df$zone),
                 sum)
zdf <- zdf[order(zdf$zone), ]
zone_df$Rain_WatStorTTot <- zdf[[2]]

# Rain_WatStorCap[Zone] = C_LAI[Zone]*Cq_RainWStorCapCurr[Zone]+Rain_WatStorTTot[Zone]+S&B_FineNecromass[Zone,DW]*Mc_LAIperNecmss[Zone]*ARRAYMEAN(T_RainWStorCap[*])
zone_df$Rain_WatStorCap <- zone_df$C_LAI * zone_df$Cq_RainWStorCapCurr +
  zone_df$Rain_WatStorTTot + zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$SB_FineNecromass * zone_df$Mc_LAIperNecmss * mean(tree_df$T_RainWStorCap)



# INFLOWS:
#   Rain_Interception[Zone] = IF Rain_WatStorCap[Zone]-Rain_CanopyWater[Zone]>0 THEN
# (Rain_WatStorCap[Zone]-Rain_CanopyWater[Zone])*(1-exp(-Rain*Rain_WeightAct[Zone]/(Rain_WatStorCap[Zone]-Rain_CanopyWater[Zone]))) ELSE 0
zone_df$Rain_Interception <- ifelse(
  zone_df$Rain_WatStorCap - zone_df$Rain_CanopyWater > 0,
  (zone_df$Rain_WatStorCap - zone_df$Rain_CanopyWater) * (1 - exp(
    -Rain * zone_df$Rain_WeightAct / (zone_df$Rain_WatStorCap - zone_df$Rain_CanopyWater)
  )),
  0
)

# Rain_IntercDelay[Zone] = min(Rain_Max_IntDripDur, Rain_IntMult*Rain_Interception[Zone]/Rain_IntercDripRt)
zone_df$Rain_IntercDelay <- pmin(
  pars$Rain_par$Rain_Max_IntDripDur,
  pars$Rain_par$Rain_IntMult * zone_df$Rain_Interception / pars$Rain_par$Rain_IntercDripRt
)


# Rain_Duration = (Rain/Rain_IntensMean)*MIN(MAX (0,1-3*Rain_IntensCoefVar, NORMAL(1,Rain_IntensCoefVar,Rain_GenSeed+11250)), 1+3*Rain_IntensCoefVar)
Rain_Duration <- (Rain$Rain / pars$Rain_par$Rain_IntensMean) *
  min(
    max (
      0,
      1 - 3 * pars$Rain_par$Rain_IntensCoefVar,
      rnorm(1, 1, pars$Rain_par$Rain_IntensCoefVar)
    ),
    1 + 3 * pars$Rain_par$Rain_IntensCoefVar
  )




# Rain_SurfPondDelay[Zone] = if AF_SlopeCurr[Zone]*Rain_PondFlwRt*AF_ZoneWidth[Zone] = 0 then 24 else
#   Rain_PondStoreCp/(AF_SlopeCurr[Zone]*Rain_PondFlwRt*AF_ZoneWidth[Zone])
zone_df$Rain_SurfPondDelay <- ifelse(
  zone_df$AF_SlopeCurr * pars$Rain_par$Rain_PondFlwRt * zone_df$AF_ZoneWidth == 0,
  24,
  pars$Rain_par$Rain_PondStoreCp / (
    zone_df$AF_SlopeCurr * pars$Rain_par$Rain_PondFlwRt * zone_df$AF_ZoneWidth
  )
)

# Rain_TimeAvForInf[Zone] = min(24,Rain_Duration+Rain_IntercDelay[Zone]+Rain_SurfPondDelay[Zone] )
zone_df$Rain_TimeAvForInf <- min(24,
                                 Rain_Duration + zone_df$Rain_IntercDelay + zone_df$Rain_SurfPondDelay)

# Rain_InfilAct = (AF_PlotNumberUphill*ARRAYSUM(LF_SICZF[*]))*ARRAYMEAN(Rain_TimeAvForInf[*])
Rain_InfilAct <- (pars$AF_par$AF_PlotNumberUphill * sum(zone_df$LF_SICZF)) * mean(zone_df$Rain_TimeAvForInf)

# LF_UpHillRainIn = max(min(AF_PlotNumberUphill*Rain,min(Rain_InfilAct,
#  AF_PlotNumberUphill*(LF_MaxProfVAbsorption+LF_MaxProfHOutflow))),0)
LF_UpHillRainIn <- max(min(
  pars$AF_par$AF_PlotNumberUphill * Rain$Rain,
  min(
    Rain_InfilAct,
    pars$AF_par$AF_PlotNumberUphill *
      (LF_MaxProfVAbsorption + LF_MaxProfHOutflow)
  )
), 0)






# LF_AvgWatDef1 = IF W_WaterLog? = 0 then AF_ZoneFrac[Zn1]*W_WatDef1[Zn1]+AF_ZoneFrac[Zn2]*W_WatDef1[Zn2]+
#   AF_ZoneFrac[Zn3]*W_WatDef1[Zn3]+AF_ZoneFrac[Zn4]*W_WatDef1[Zn4] ELSE AF_ZoneFrac[Zn1]*W_WatDefLog1[Zn1]+AF_ZoneFrac[Zn2]*W_WatDefLog1[Zn2]+
#   AF_ZoneFrac[Zn3]*W_WatDefLog1[Zn3]+AF_ZoneFrac[Zn4]*W_WatDefLog1[Zn4]
# LF_AvgWatDef2 = IF W_WaterLog? = 0 then AF_ZoneFrac[Zn1]*W_WatDef2[Zn1]+AF_ZoneFrac[Zn2]*W_WatDef2[Zn2]+
#   AF_ZoneFrac[Zn3]*W_WatDef2[Zn3]+AF_ZoneFrac[Zn4]*W_WatDef2[Zn4]
# else
#   AF_ZoneFrac[Zn1]*W_WatDefLog2[Zn1]+AF_ZoneFrac[Zn2]*W_WatDefLog2[Zn2]+
#   AF_ZoneFrac[Zn3]*W_WatDefLog2[Zn3]+AF_ZoneFrac[Zn4]*W_WatDefLog2[Zn4]
# LF_AvgWatDef3 = IF W_WaterLog? = 0 then
# AF_ZoneFrac[Zn1]*W_WatDef3[Zn1]+AF_ZoneFrac[Zn2]*W_WatDef3[Zn2]+
#   AF_ZoneFrac[Zn3]*W_WatDef3[Zn3]+AF_ZoneFrac[Zn4]*W_WatDef3[Zn4]
# ELSE
# AF_ZoneFrac[Zn1]*W_WatDefLog3[Zn1]+AF_ZoneFrac[Zn2]*W_WatDefLog3[Zn2]+
#   AF_ZoneFrac[Zn3]*W_WatDefLog3[Zn3]+AF_ZoneFrac[Zn4]*W_WatDefLog3[Zn4]
# LF_AvgWatDef4 = IF W_WaterLog? = 0 then AF_ZoneFrac[Zn1]*W_WatDef4[Zn1]+AF_ZoneFrac[Zn2]*W_WatDef4[Zn2]+
#   AF_ZoneFrac[Zn3]*W_WatDef4[Zn3]+AF_ZoneFrac[Zn4]*W_WatDef4[Zn4]
# ELSE
# AF_ZoneFrac[Zn1]*W_WatDefLog4[Zn1]+AF_ZoneFrac[Zn2]*W_WatDefLog4[Zn2]+
#   AF_ZoneFrac[Zn3]*W_WatDefLog4[Zn3]+AF_ZoneFrac[Zn4]*W_WatDefLog4[Zn4]

zonelayer_df$W_WatDef_ZF <- zonelayer_df$W_WatDef * zonelayer_df$AF_ZoneFrac
df <- aggregate(zonelayer_df$W_WatDef_ZF, by = list(layer = zonelayer_df$layer), sum)
df <- df[order(df$layer), ]
layer_df$W_WatDef <- df[[2]]

# zonelayer_df$W_WatDefLog <- rep(layer_df$W_WatDefLog, each = nzone)
zonelayer_df$W_WatDefLog_ZF <- zonelayer_df$W_WatDefLog * zonelayer_df$AF_ZoneFrac
df <- aggregate(zonelayer_df$W_WatDefLog_ZF,
                by = list(layer = zonelayer_df$layer),
                sum)
df <- df[order(df$layer), ]
layer_df$W_WatDefLog <- df[[2]]

layer_df$W_WaterLog_is <- pars$W_par$W_WaterLog_is

layer_df$LF_AvgWatDef <- ifelse(layer_df$W_WaterLog_is == 0,
                                layer_df$W_WatDef,
                                layer_df$W_WatDefLog)

# zonelayer_df$LF_HMaxDailyFlow <- rep(layer_df$LF_HMaxDailyFlow, each = nzone)


# W_H1RelDrain[Zone] = max(0,LF_H1MaxMailyFlow[Zone]/(LF_H1MaxMailyFlow[Zn2]+LF_V1MaxDailyFlow[Zone]))
# W_H2RelDrain[Zone] = max(0,LF_H2MaxDailyFlow[Zone]/(LF_H2MaxDailyFlow[Zone]+LF_V2MaxDailyFlow[Zone]))
# W_H3RelDrain[Zone] = max(0,LF_H3MaxDailyFlow[Zone]/(LF_H3MaxDailyFlow[Zone]+LF_V3MaxDailyFlow[Zone]))
# W_H4RelDrain[Zone] = max(0,LF_H4MaxDailyFlow[Zone]/(LF_H4MaxDailyFlow[Zone]+LF_V4MaxDailyFlow[Zone]))
zonelayer_df$W_HRelDrain <- pmax(
  0,
  zonelayer_df$LF_HMaxDailyFlow / (
    zonelayer_df$LF_HMaxDailyFlow + zonelayer_df$LF_VMaxDailyFlow
  )
)
# zl1 <- zonelayer_df[zonelayer_df$layer == 1, ]
zonelayer_df[zonelayer_df$layer == 1, ]$W_HRelDrain <- pmax(
  0,
  zone_df$LF_H1MaxMailyFlow / (zone_df[zone_df$zone == 2, ]$LF_H1MaxMailyFlow + zonelayer_df[zonelayer_df$layer == 1, ]$LF_VMaxDailyFlow)
)

# W_H1RelDrZoneWid[Zone] = AF_ZoneFrac[Zone]*W_H1RelDrain[Zone]
# W_H2RelDrZoneWid[Zone] = AF_ZoneFrac[Zone]*W_H2RelDrain[Zone]
# W_H3RelDrZoneWid[Zone] = AF_ZoneFrac[Zone]*W_H3RelDrain[Zone]
# W_H4RelDrZoneWid[Zone] = AF_ZoneFrac[Zone]*W_H4RelDrain[Zone]
zonelayer_df$W_HRelDrZoneWid <- zonelayer_df$AF_ZoneFrac * zonelayer_df$W_HRelDrain

# W_Avg1RelHDrain = ARRAYSUM(W_H1RelDrZoneWid[*])
# W_Avg2RelHDrain = ARRAYSUM(W_H2RelDrZoneWid[*])
# W_Avg3RelHDrain = ARRAYSUM(W_H3RelDrZoneWid[*])
# W_Avg4RelHDrain = ARRAYSUM(W_H4RelDrZoneWid[*])
df <- aggregate(zonelayer_df$W_HRelDrZoneWid,
                by = list(layer = zonelayer_df$layer),
                sum)
df <- df[order(df$layer), ]
layer_df$W_AvgRelHDrain <- df[[2]]

# LF_VertFlow1 = min(min((max(0,LF_UpHillRainIn - AF_PlotNumberUphill*LF_AvgWatDef1))*(1-W_Avg1RelHDrain),
#ARRAYSUM(LF_MVI2ZF[*])),
#  (AF_ZoneFrac[Zn1]*LF_V1MaxDailyFlow[Zn1]+AF_ZoneFrac[Zn2]*LF_V1MaxDailyFlow[Zn2]+AF_ZoneFrac[Zn3]*LF_V1MaxDailyFlow[Zn3]+AF_ZoneFrac[Zn4]*LF_V1MaxDailyFlow[Zn4]))
# LF_VertFlow2 = min(min((max(0,LF_VertFlow1 - AF_PlotNumberUphill*LF_AvgWatDef2))*(1-W_Avg2RelHDrain),
#ARRAYSUM(LF_MVI3ZF[*])),
#  (AF_ZoneFrac[Zn1]*LF_V2MaxDailyFlow[Zn1]+AF_ZoneFrac[Zn2]*LF_V2MaxDailyFlow[Zn2]+AF_ZoneFrac[Zn3]*LF_V2MaxDailyFlow[Zn3]+AF_ZoneFrac[Zn4]*LF_V2MaxDailyFlow[Zn4]))
# LF_VertFlow3 = min(min((max(0,LF_VertFlow2 - AF_PlotNumberUphill*LF_AvgWatDef3))*(1-W_Avg3RelHDrain),
#ARRAYSUM(LF_MVI4ZF[*])),
#  (AF_ZoneFrac[Zn1]*LF_V3MaxDailyFlow[Zn1]+AF_ZoneFrac[Zn2]*LF_V3MaxDailyFlow[Zn2]+AF_ZoneFrac[Zn3]*LF_V3MaxDailyFlow[Zn3]+AF_ZoneFrac[Zn4]*LF_V3MaxDailyFlow[Zn4]))
# LF_VertFlow4 = min((max(0,LF_VertFlow3 - AF_PlotNumberUphill*LF_AvgWatDef4))*(1-W_Avg4RelHDrain),
#  (AF_ZoneFrac[Zn1]*LF_V4MaxDailyFlow[Zn1]+AF_ZoneFrac[Zn2]*LF_V4MaxDailyFlow[Zn2]+AF_ZoneFrac[Zn3]*LF_V4MaxDailyFlow[Zn3]+AF_ZoneFrac[Zn4]*LF_V4MaxDailyFlow[Zn4]))

zonelayer_df$fv <- zonelayer_df$AF_ZoneFrac *  zonelayer_df$LF_VMaxDailyFlow
df <- aggregate(zonelayer_df[c("fv", "LF_MVIZF")], list(layer = zonelayer_df$layer), sum)
df <- df[order(df$layer), ]
layer_df$LF_VMaxDailyFlow_ZF <- df$fv
layer_df$LF_MVIZF_sum <- df$LF_MVIZF

l1 <- layer_df[layer_df$layer == 1, ]
LF_VertFlow1 <- min(min((
  max(
    0,
    LF_UpHillRainIn - pars$AF_par$AF_PlotNumberUphill * l1$LF_AvgWatDef
  )
) * (1 - l1$W_AvgRelHDrain), l1$LF_MVIZF_sum), l1$LF_VMaxDailyFlow_ZF)
l2 <- layer_df[layer_df$layer == 2, ]
LF_VertFlow2 <- min(min((
  max(
    0,
    LF_VertFlow1 - pars$AF_par$AF_PlotNumberUphill * l2$LF_AvgWatDef
  )
) * (1 - l2$W_AvgRelHDrain), l2$LF_MVIZF_sum), l2$LF_MVIZF_sum)

l3 <- layer_df[layer_df$layer == 3, ]
LF_VertFlow3 <- min(min((
  max(
    0,
    LF_VertFlow2 - pars$AF_par$AF_PlotNumberUphill * l3$LF_AvgWatDef
  )
) * (1 - l3$W_AvgRelHDrain), l3$LF_MVIZF_sum), l3$LF_MVIZF_sum)

l4 <- layer_df[layer_df$layer == 4, ]
LF_VertFlow4 <- min((
  max(
    0,
    LF_VertFlow3 - pars$AF_par$AF_PlotNumberUphill * l4$LF_AvgWatDef
  )
) * (1 - l4$W_AvgRelHDrain), l4$LF_MVIZF_sum)

layer_df$LF_VertFlow <- c(LF_VertFlow1, LF_VertFlow2, LF_VertFlow3, LF_VertFlow4)




# TODO: W_Drain1 Loop?? down
zonelayer_df$W_Drain <- 0

# W_V1Drain[Zone] = MAX(0,MIN(min(W_Drain1[Zone]*(1-W_H1RelDrain[Zone]),LF_V1MaxDailyFlow[Zone]),LF_MaxVInflow2[Zone]),0)
# W_V2Drain[Zone] = MAX(0,MIN(min(W_Drain2[Zone]*(1-W_H2RelDrain[Zone]),LF_V2MaxDailyFlow[Zone]),LF_MaxVInflow3[Zone]),0)
# W_V3Drain[Zone] = MAX(0,MIN(min(W_Drain3[Zone]*(1-W_H3RelDrain[Zone]),LF_V3MaxDailyFlow[Zone]),LF_MaxVInflow4[Zone]))
# W_V4Drain[Zone] = MAX(0,min(W_Drain4[Zone]*(1-W_H4RelDrain[Zone]),LF_V4MaxDailyFlow[Zone]))
zonelayer_df$W_VDrain <- 0

zonelayer_df$W_VDrain_val <- pmin(
  zonelayer_df$W_Drain * (1 - zonelayer_df$W_HRelDrain),
  zonelayer_df$LF_VMaxDailyFlow
)

zonelayer_df[zonelayer_df$layer == 1, ]$W_VDrain <- pmax(0,
                                                         pmin(zonelayer_df[zonelayer_df$layer == 1, ]$W_VDrain_val, zonelayer_df[zonelayer_df$layer == 2, ]$LF_MaxVInflow),
                                                         0)
zonelayer_df[zonelayer_df$layer == 2, ]$W_VDrain <- pmax(0,
                                                         pmin(zonelayer_df[zonelayer_df$layer == 2, ]$W_VDrain_val, zonelayer_df[zonelayer_df$layer == 3, ]$LF_MaxVInflow),
                                                         0)
zonelayer_df[zonelayer_df$layer == 3, ]$W_VDrain <- pmax(0,
                                                         pmin(zonelayer_df[zonelayer_df$layer == 3, ]$W_VDrain_val, zonelayer_df[zonelayer_df$layer == 4, ]$LF_MaxVInflow),
                                                         0)
zonelayer_df[zonelayer_df$layer == 4, ]$W_VDrain <- pmax(0, zonelayer_df[zonelayer_df$layer == 4, ]$W_VDrain_val)

# AF_AccLatInflowratio[Zn1] = AF_LatInFlowRatio[Zn1]*AF_LatInFlowRatio[Zn2]*AF_LatInFlowRatio[Zn3]*AF_LatInFlowRatio[Zn4]
# AF_AccLatInflowratio[Zn2] = AF_LatInFlowRatio[Zn2]*AF_LatInFlowRatio[Zn3]*AF_LatInFlowRatio[Zn4]
# AF_AccLatInflowratio[Zn3] = AF_LatInFlowRatio[Zn3]*AF_LatInFlowRatio[Zn4]
# AF_AccLatInflowratio[Zn4] = AF_LatInFlowRatio[Zn4]
zone_df$AF_AccLatInFlowRatio <- zone_df$AF_LatInFlowRatio
for (z in 1:(nzone - 1)) {
  zone_df$AF_AccLatInFlowRatio <- zone_df$AF_AccLatInFlowRatio * c(tail(zone_df$AF_LatInFlowRatio, -z), rep(1, z))
}
zonelayer_df$AF_AccLatInFlowRatio <- rep(zone_df$AF_AccLatInFlowRatio, nlayer)



# LF_UphillGWRelease = LF_UphillGWStore*LF_GW_ReleaseFraction
LF_UphillGWRelease <- pars$LF_par$LF_UphillGWStore * pars$LF_par$LF_GW_ReleaseFraction




# W_Theta1[Zone] = min((max(0,W_Stock1[Zone]))/(AF_DepthAct1[Zone]*1000),1)
# W_Theta2[Zone] = min(max(0,W_Stock2[Zone])/(AF_Depth2[Zone]*1000),1)
# W_Theta3[Zone] = min(max(0,W_Stock3[Zone])/(AF_Depth3[Zone]*1000),1)
# W_Theta4[Zone] = min(max(0,W_Stock4[Zone])/(AF_Depth4[Zone]*1000),1)
zonelayer_df$W_Theta <- pmin((pmax(0, zonelayer_df$W_Stock)) / (zonelayer_df$AF_Depth *
                                                                  1000), 1)

# W_WaterfilledPoreF1[Zone] = W_Theta1[Zone]/W_PoreVol[Zone,1]
# W_WaterfilledPoreF2[Zone] = W_Theta2[Zone]/W_PoreVol[Zone,2]
# W_WaterfilledPoreF3[Zone] = W_Theta3[Zone]/W_PoreVol[Zone,3]
# W_WaterfilledPoreF4[Zone] = W_Theta4[Zone]/W_PoreVol[Zone,4]
zonelayer_df$W_WaterfilledPoreF <- zonelayer_df$W_Theta / zonelayer_df$W_PoreVol

# LF_RelDistInflowFromGW[1] = if (W_WaterfilledPoreF4[Zn4] = 1 and W_WaterfilledPoreF3[Zn4] = 1 and W_WaterfilledPoreF2[Zn4] = 1 and W_WaterfilledPoreF1[Zn4] < 1)  then 1 else 0*(W_WaterfilledPoreF4[Zn4]+W_WaterfilledPoreF3[Zn4]+W_WaterfilledPoreF2[Zn4]+W_WaterfilledPoreF1[Zn4])
# LF_RelDistInflowFromGW[2] = if(W_WaterfilledPoreF4[Zn4] = 1 and W_WaterfilledPoreF3[Zn4] = 1 and W_WaterfilledPoreF2[Zn4] < 1) then 1 else 0*(W_WaterfilledPoreF4[Zn4]+W_WaterfilledPoreF3[Zn4]+W_WaterfilledPoreF2[Zn4]+W_WaterfilledPoreF1[Zn4])
# LF_RelDistInflowFromGW[3] = if (W_WaterfilledPoreF4[Zn4] = 1 and W_WaterfilledPoreF3[Zn4] < 1) then 1 else 0*(W_WaterfilledPoreF4[Zn4]+W_WaterfilledPoreF3[Zn4]+W_WaterfilledPoreF2[Zn4]+W_WaterfilledPoreF1[Zn4])
# LF_RelDistInflowFromGW[4] = if W_WaterfilledPoreF4[Zn4] < 1 then 1 else 0*(W_WaterfilledPoreF4[Zn4]+W_WaterfilledPoreF3[Zn4]+W_WaterfilledPoreF2[Zn4]+W_WaterfilledPoreF1[Zn4])

layer_df$LF_RelDistInflowFromGW <- sum(layer_df$W_WaterfilledPoreF)
wfp <- zonelayer_df[zonelayer_df$zone == 4, ]$W_WaterfilledPoreF
if (wfp[4] == 1 && wfp[3] == 1 && wfp[2] == 1 && wfp[1] < 1) {
  layer_df[layer_df$layer == 1, ]$LF_RelDistInflowFromGW <- 1
}
if (wfp[4] == 1 && wfp[3] == 1 && wfp[2] < 1) {
  layer_df[layer_df$layer == 2, ]$LF_RelDistInflowFromGW <- 1
}
if (wfp[4] == 1 && wfp[3] < 1) {
  layer_df[layer_df$layer == 3, ]$LF_RelDistInflowFromGW <- 1
}
if (wfp[4] < 1) {
  layer_df[layer_df$layer == 4, ]$LF_RelDistInflowFromGW <- 1
}

# LF_InflowFromGW[SoilLayer] = LF_UphillGWRelease*LF_FracGWReleaseAsInflow*LF_RelDistInflowFromGW[SoilLayer]
layer_df$LF_InflowFromGW <- LF_UphillGWRelease *  pars$LF_par$LF_FracGWReleaseAsInflow * layer_df$LF_RelDistInflowFromGW

# LF_Lat4Inflow1 = min(max((LF_UpHillRainIn-AF_PlotNumberUphill*LF_AvgWatDef1-LF_VertFlow1),0),LF_H1MaxMailyFlow[Zn1])+LF_InflowFromGW[1]
# LF_Lat4Inflow2 = min((max(0,LF_VertFlow1-AF_PlotNumberUphill*LF_AvgWatDef2-LF_VertFlow2)),LF_H2MaxDailyFlow[Zn1])+LF_InflowFromGW[2]
# LF_Lat4Inflow3 = min((max(0,LF_VertFlow2-AF_PlotNumberUphill*LF_AvgWatDef3-LF_VertFlow3)),LF_H3MaxDailyFlow[Zn1])+LF_InflowFromGW[3]
# LF_Lat4Inflow4 = min((max(0,LF_VertFlow3-AF_PlotNumberUphill*LF_AvgWatDef4-LF_VertFlow4)),LF_H4MaxDailyFlow[Zn1])+LF_SubSurfInflowAdd4 +LF_InflowFromGW[4]
LF_VertFlow_up <- c(LF_UpHillRainIn, LF_VertFlow1, LF_VertFlow2, LF_VertFlow3)
layer_df$LF_Lat4Inflow <- pmin(
  pmax(
    0,
    LF_VertFlow_up - pars$AF_par$AF_PlotNumberUphill * layer_df$LF_AvgWatDef - layer_df$LF_VertFlow
  ),
  zonelayer_df[zonelayer_df$zone == 1, ]$LF_HMaxDailyFlow
) + layer_df$LF_InflowFromGW
layer_df[layer_df$layer == 4, ]$LF_Lat4Inflow <- layer_df[layer_df$layer == 4, ]$LF_Lat4Inflow + pars$LF_par$LF_SubSurfInflowAdd4
zonelayer_df$LF_Lat4Inflow <- rep(layer_df$LF_Lat4Inflow, each = nzone)



# LF_MaxVInflow1[Zone] = IF W_WaterLog? = 0 THEN max(W_WatDef1[Zone]+min(LF_V1MaxDailyFlow[Zone],LF_MaxVInflow2[Zone]),0) ELSE max(W_WatDefLog1[Zone]+min(LF_V1MaxDailyFlow[Zone],LF_MaxVInflow2[Zone]),0)
zl1 <- zonelayer_df[zonelayer_df$layer == 1, ]
zl2 <- zonelayer_df[zonelayer_df$layer == 2, ]

zone_df$W_WaterLog_is <- pars$W_par$W_WaterLog_is

zone_df$LF_MaxVInflow1 <- ifelse(
  zone_df$W_WaterLog_is == 0,
  pmax(
    zl1$W_WatDef + pmin(zl1$LF_VMaxDailyFlow, zl2$LF_MaxVInflow),
    0
  ),
  pmax(
    zl1$W_WatDefLog + pmin(zl1$LF_VMaxDailyFlow, zl2$LF_MaxVInflow),
    0
  )
)

# Rain_InfConstr[Zone] = min(LF_MaxVInflow1[Zone],S_SurfInfiltrAct[Zone]*Rain_TimeAvForInf[Zone]/24)
zone_df$Rain_InfConstr <- pmin(zone_df$LF_MaxVInflow1,
                               zone_df$S_SurfInfiltrAct * zone_df$Rain_TimeAvForInf / 24)


# Rain_In[Zone] = max(0,Rain*Rain_WeightAct[Zone]-Rain_Interception[Zone])
zone_df$Rain_In <- max(0,
                       Rain$Rain * zone_df$Rain_WeightAct - zone_df$Rain_Interception)


# LF_RunOn = (AF_PlotNumberUphill*Rain-LF_UpHillRainIn)*AF_RunOnFrac
LF_RunOn <- (pars$AF_par$AF_PlotNumberUphill * Rain$Rain - LF_UpHillRainIn) *
  pars$AF_par$AF_RunOnFrac

# Rain_Inf4 = min(Rain_InfConstr[Zn4],Rain_In[Zn4]+LF_RunOn*AF_LatInFlowRatio[Zn4])
# Rain_Runoff43 = max(0,Rain_In[Zn4]+AF_LatInFlowRatio[Zn4]*LF_RunOn - Rain_Inf4)
# Rain_Inf3 = min(Rain_InfConstr[Zn3],Rain_In[Zn3]+AF_LatInFlowRatio[Zn3]*Rain_Runoff43)
# Rain_Runoff32 = max(0,Rain_In[Zn3]+AF_LatInFlowRatio[Zn3]*Rain_Runoff43 - Rain_Inf3)
# Rain_Inf2 = min(Rain_InfConstr[Zn2],Rain_In[Zn2]+AF_LatInFlowRatio[Zn2]*Rain_Runoff32)
# Rain_Runoff21 = max(Rain_In[Zn2]+AF_LatInFlowRatio[Zn2]*Rain_Runoff32-Rain_Inf2,0)
# Rain_Inf1 = min(Rain_InfConstr[Zn1],Rain_In[Zn1]+AF_LatInFlowRatio[Zn1]*Rain_Runoff21)
# Rain_Runoff10 = max(0,Rain_In[Zn1]+AF_LatInFlowRatio[Zn1]*Rain_Runoff21-Rain_Inf1)
# Rain_RunOff = AF_ZoneFrac[Zn1]*Rain_Runoff10

z4 <- zone_df[zone_df$zone == 4, ]
Rain_Inf4 <- min(z4$Rain_InfConstr, z4$Rain_In + LF_RunOn * z4$AF_LatInFlowRatio)
Rain_Runoff43 <- max(0, z4$Rain_In + z4$AF_LatInFlowRatio * LF_RunOn - Rain_Inf4)

z3 <- zone_df[zone_df$zone == 3, ]
Rain_Inf3 <- min(z3$Rain_InfConstr,
                 z3$Rain_In + z3$AF_LatInFlowRatio * Rain_Runoff43)
Rain_Runoff32 <- max(0, z3$Rain_In + z3$AF_LatInFlowRatio * Rain_Runoff43 - Rain_Inf3)

z2 <- zone_df[zone_df$zone == 2, ]
Rain_Inf2 <- min(z2$Rain_InfConstr,
                 z2$Rain_In + z2$AF_LatInFlowRatio * Rain_Runoff32)
Rain_Runoff21 <- max(0, z2$Rain_In + z2$AF_LatInFlowRatio * Rain_Runoff32 - Rain_Inf2)

z1 <- zone_df[zone_df$zone == 1, ]
Rain_Inf1 <- min(z1$Rain_InfConstr,
                 z1$Rain_In + z1$AF_LatInFlowRatio * Rain_Runoff21)
Rain_Runoff10 <- max(0, z1$Rain_In + z1$AF_LatInFlowRatio * Rain_Runoff21 - Rain_Inf1)
Rain_RunOff <- z1$AF_ZoneFrac * Rain_Runoff10


# Rain_Infiltr[Zn1] = Rain_Inf1+0*(Rain_Inf2+Rain_Inf3+Rain_Inf4)
# Rain_Infiltr[Zn2] = Rain_Inf2+0*(Rain_Inf1+Rain_Inf3+Rain_Inf4)
# Rain_Infiltr[Zn3] = Rain_Inf3+0*(Rain_Inf1+Rain_Inf2+Rain_Inf4)
# Rain_Infiltr[Zn4] = 0*(Rain_Inf1+Rain_Inf2+Rain_Inf3)+Rain_Inf4
zone_df$Rain_Infiltr <- c(
  Rain_Inf1 + 0 * (Rain_Inf2 + Rain_Inf3 + Rain_Inf4),
  Rain_Inf2 + 0 * (Rain_Inf1 + Rain_Inf3 + Rain_Inf4),
  Rain_Inf3 + 0 * (Rain_Inf1 + Rain_Inf2 + Rain_Inf4),
  0 * (Rain_Inf1 + Rain_Inf2 + Rain_Inf3) + Rain_Inf4
)




# W_EstDrain1[Zone] = if W_WaterLog? = 0 then max(0,Rain_Infiltr[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow1-W_WatDef1[Zone]) else max(0,Rain_Infiltr[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow1-W_WatDefLog1[Zone])
# W_EstDrain2[Zone] = IF W_WaterLog? = 0 then max(0,W_V1Drain[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow2-W_WatDef2[Zone]) ELSE IF (W_V1Drain[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow2) < S_KsatV2Act[Zone] then max(0,W_V1Drain[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow2-W_WatDef2[Zone]) ELSE max(0,W_V1Drain[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow2-W_WatDefLog2[Zone])
# W_EstDrain3[Zone] = if W_WaterLog? = 0 then max(0,W_V2Drain[Zone]+AF_AccLatInflowratio[Zone]*LF_Lat4Inflow3-W_WatDef3[Zone]) else if (W_V2Drain[Zone]+AF_AccLatInflowratio[Zone]*LF_Lat4Inflow3) < S_KsatV3Act[Zone] then max(0,W_V2Drain[Zone]+AF_AccLatInflowratio[Zone]*LF_Lat4Inflow3-W_WatDef3[Zone]) else max(0,W_V2Drain[Zone]+AF_AccLatInflowratio[Zone]*LF_Lat4Inflow3-W_WatDefLog3[Zone])
# W_EstDrain4[Zone] = if W_WaterLog? = 0 then max(0,W_V3Drain[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow4-W_WatDef4[Zone]) else if (W_V3Drain[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow4) < S_KsatV4Act[Zone] then max(0,W_V3Drain[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow4-W_WatDef4[Zone]) else max(0,W_V3Drain[Zone]+AF_AccLatInFlowRatio[Zone]*LF_Lat4Inflow4-W_WatDefLog4[Zone])
zonelayer_df$W_WaterLog_is <- pars$W_par$W_WaterLog_is

zonelayer_df$W_EstDrain_a <- c(zone_df$Rain_Infiltr, zonelayer_df[zonelayer_df$layer != 4, ]$W_VDrain) +  zonelayer_df$AF_AccLatInFlowRatio * zonelayer_df$LF_Lat4Inflow
zonelayer_df$W_EstDrain <- ifelse(
  zonelayer_df$W_WaterLog_is == 0,
  pmax(0, zonelayer_df$W_EstDrain_a - zonelayer_df$W_WatDef),
  ifelse(
    zonelayer_df$W_EstDrain_a < zonelayer_df$S_KsatVAct,
    pmax(0, zonelayer_df$W_EstDrain_a - zonelayer_df$W_WatDef),
    pmax(0, zonelayer_df$W_EstDrain_a - zonelayer_df$W_WatDefLog)
  )
)
zl1 <- zonelayer_df[zonelayer_df$layer == 1, ]
zonelayer_df[zonelayer_df$layer == 1, ]$W_EstDrain <- ifelse(
  zl1$W_WaterLog_is == 0,
  pmax(0, zl1$W_EstDrain_a - zl1$W_WatDef),
  pmax(0, zl1$W_EstDrain_a - zl1$W_WatDefLog)
)


# W_Drain1[Zn1] = MAX(0,W_EstDrain1[Zn1]+AF_LatInFlowRatio[Zn1]*W_H1RelDrain[Zn2]*( W_EstDrain1[Zn2]+AF_LatInFlowRatio[Zn2]*W_H1RelDrain[Zn3]*(W_EstDrain1[Zn3]+AF_LatInFlowRatio[Zn3]*W_H1RelDrain[Zn4]*W_EstDrain1[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow1)-AF_AccLatInflowratio[Zn2]*LF_Lat4Inflow1)-AF_AccLatInflowratio[Zn1]*LF_Lat4Inflow1)
# W_Drain1[Zn2] = MAX(0,W_EstDrain1[Zn2]+AF_LatInFlowRatio[Zn2]*W_H1RelDrain[Zn3]*(W_EstDrain1[Zn3]+AF_LatInFlowRatio[Zn3]*W_H1RelDrain[Zn4]*W_EstDrain1[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow1)-AF_AccLatInflowratio[Zn2]*LF_Lat4Inflow1)
# W_Drain1[Zn3] = MAX(0,W_EstDrain1[Zn3]+AF_LatInFlowRatio[Zn3]*W_H1RelDrain[Zn4]*W_EstDrain1[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow1)
# W_Drain1[Zn4] = MAX(0,W_EstDrain1[Zn4]+0 *(LF_Lat4Inflow1+W_H1RelDrain[Zn4]+AF_LatInFlowRatio[Zn4]+AF_AccLatInflowratio[Zn4]))
# W_Drain2[Zn1] = MAX(0,W_EstDrain2[Zn1]+AF_LatInFlowRatio[Zn1]*W_H2RelDrain[Zn2]*(W_EstDrain2[Zn2]+AF_LatInFlowRatio[Zn2]*W_H2RelDrain[Zn3]*(W_EstDrain2[Zn3]+AF_LatInFlowRatio[Zn3]*W_H2RelDrain[Zn4]*W_EstDrain2[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow2)-AF_AccLatInflowratio[Zn2]*LF_Lat4Inflow2)-AF_AccLatInflowratio[Zn1]*LF_Lat4Inflow2)
# W_Drain2[Zn2] = MAX(0,W_EstDrain2[Zn2]+AF_LatInFlowRatio[Zn2]*W_H2RelDrain[Zn3]*(W_EstDrain2[Zn3]+AF_LatInFlowRatio[Zn3]*W_H2RelDrain[Zn4]*W_EstDrain2[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow2)-AF_AccLatInflowratio[Zn2]*LF_Lat4Inflow2)
# W_Drain2[Zn3] = MAX(0,W_EstDrain2[Zn3]+AF_LatInFlowRatio[Zn3]*W_H2RelDrain[Zn4]*W_EstDrain2[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow2)
# W_Drain2[Zn4] = MAX(0,W_EstDrain2[Zn4]+0 *(LF_Lat4Inflow2+W_H2RelDrain[Zn4] + AF_LatInFlowRatio[Zn4]+AF_AccLatInflowratio[Zn4]))
# W_Drain3[Zn1] = MAX(0,W_EstDrain3[Zn1]+AF_LatInFlowRatio[Zn1]*W_H3RelDrain[Zn2]*(W_EstDrain3[Zn2]+AF_LatInFlowRatio[Zn2]*W_H3RelDrain[Zn3]*(W_EstDrain3[Zn3]+AF_LatInFlowRatio[Zn3]*W_H3RelDrain[Zn4]*W_EstDrain3[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow3)-AF_AccLatInflowratio[Zn2]*LF_Lat4Inflow3)-AF_AccLatInflowratio[Zn1]*LF_Lat4Inflow3)
# W_Drain3[Zn2] = MAX(0,W_EstDrain3[Zn2]+AF_LatInFlowRatio[Zn2]*W_H3RelDrain[Zn3]*(W_EstDrain3[Zn3]+AF_LatInFlowRatio[Zn3]*W_H3RelDrain[Zn4]*W_EstDrain3[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow3)-AF_AccLatInflowratio[Zn2]*LF_Lat4Inflow3)
# W_Drain3[Zn3] = MAX(0,W_EstDrain3[Zn3]+AF_LatInFlowRatio[Zn3]*W_H3RelDrain[Zn4]*W_EstDrain3[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow3)
# W_Drain3[Zn4] = MAX(0,W_EstDrain3[Zn4]+0 *(LF_Lat4Inflow3+W_H3RelDrain[Zn4]+AF_LatInFlowRatio[Zn4]+AF_AccLatInflowratio[Zn4]))
# W_Drain4[Zn1] = MAX(0,W_EstDrain4[Zn1]+AF_LatInFlowRatio[Zn1]*W_H4RelDrain[Zn2]*(W_EstDrain4[Zn2]+AF_LatInFlowRatio[Zn2]*W_H4RelDrain[Zn3]*(W_EstDrain4[Zn3]+AF_LatInFlowRatio[Zn3]*W_H4RelDrain[Zn4]*W_EstDrain4[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow4)-AF_AccLatInflowratio[Zn2]*LF_Lat4Inflow4)-AF_AccLatInflowratio[Zn1]*LF_Lat4Inflow4)
# W_Drain4[Zn2] = MAX(0,W_EstDrain4[Zn2]+AF_LatInFlowRatio[Zn2]*W_H4RelDrain[Zn3]*(W_EstDrain4[Zn3]+AF_LatInFlowRatio[Zn3]*W_H4RelDrain[Zn4]*W_EstDrain4[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow4)-AF_AccLatInflowratio[Zn2]*LF_Lat4Inflow4)
# W_Drain4[Zn3] = MAX(0,W_EstDrain4[Zn3]+AF_LatInFlowRatio[Zn3]*W_H4RelDrain[Zn4]*W_EstDrain4[Zn4]-AF_AccLatInflowratio[Zn3]*LF_Lat4Inflow4)
# W_Drain4[Zn4] = MAX(0,W_EstDrain4[Zn4]+0 *(LF_Lat4Inflow4+W_H4RelDrain[Zn4]+AF_LatInFlowRatio[Zn4]+AF_AccLatInflowratio[Zn4]))
zonelayer_df$W_Drain <- 01
zl1 <- zonelayer_df[zonelayer_df$zone == 1, ]
zl2 <- zonelayer_df[zonelayer_df$zone == 2, ]
zl3 <- zonelayer_df[zonelayer_df$zone == 3, ]
zl4 <- zonelayer_df[zonelayer_df$zone == 4, ]
z1 <- zone_df[zone_df$zone == 1, ]
z2 <- zone_df[zone_df$zone == 2, ]
z3 <- zone_df[zone_df$zone == 3, ]
zonelayer_df[zonelayer_df$zone == 1, ]$W_Drain <- pmax(
  0,
  zl1$W_EstDrain +
    z1$AF_LatInFlowRatio * zl2$W_HRelDrain *
    (
      zl2$W_EstDrain + z2$AF_LatInFlowRatio * zl3$W_HRelDrain *
        (
          zl3$W_EstDrain + z3$AF_LatInFlowRatio * zl4$W_HRelDrain * zl4$W_EstDrain - z3$AF_AccLatInFlowRatio * zl1$LF_Lat4Inflow
        ) -
        z2$AF_AccLatInFlowRatio * zl1$LF_Lat4Inflow
    ) -
    z1$AF_AccLatInFlowRatio * zl1$LF_Lat4Inflow
)
zonelayer_df[zonelayer_df$zone == 2, ]$W_Drain <- pmax(
  0,
  zl2$W_EstDrain + z2$AF_LatInFlowRatio * zl3$W_HRelDrain * (
    zl3$W_EstDrain + z3$AF_LatInFlowRatio * zl4$W_HRelDrain * zl4$W_EstDrain - z3$AF_AccLatInFlowRatio * zl2$LF_Lat4Inflow
  ) - z2$AF_AccLatInFlowRatio * zl2$LF_Lat4Inflow
)
zonelayer_df[zonelayer_df$zone == 3, ]$W_Drain <- pmax(
  0,
  zl3$W_EstDrain + z3$AF_LatInFlowRatio * zl4$W_HRelDrain * zl4$W_EstDrain - z3$AF_AccLatInFlowRatio * zl3$LF_Lat4Inflow
)
zonelayer_df[zonelayer_df$zone == 4, ]$W_Drain <- pmax(0, zl4$W_EstDrain)

# TODO: Loop?? up for W_Drain

# W_H1Drain[Zone] = if AF_ZoneFrac[Zone] > 0 then min(W_Drain1[Zone]-W_V1Drain[Zone],LF_H1MaxMailyFlow[Zone]/AF_ZoneFrac[Zone]) else 0
# W_H2Drain[Zone] = if AF_ZoneFrac[Zone]> 0 then min(W_Drain2[Zone]-W_V2Drain[Zone],LF_H2MaxDailyFlow[Zone]/AF_ZoneFrac[Zone]) else 0
# W_H3Drain[Zone] = if AF_ZoneFrac[Zone]>0 then min(W_Drain3[Zone]-W_V3Drain[Zone],LF_H3MaxDailyFlow[Zone]/AF_ZoneFrac[Zone]) else 0
# W_H4Drain[Zone] = if AF_ZoneFrac[Zone] > 0 then min(W_Drain4[Zone]-W_V4Drain[Zone],LF_H4MaxDailyFlow[Zone]/AF_ZoneFrac[Zone]) else 0

zonelayer_df$W_HDrain <- ifelse(
  zonelayer_df$AF_ZoneFrac > 0,
  pmin(
    zonelayer_df$W_Drain - zonelayer_df$W_VDrain,
    zonelayer_df$LF_HMaxDailyFlow / zonelayer_df$AF_ZoneFrac
  ),
  0
)


# W_LatRecharge1[Zn1] = AF_LatInFlowRatio[Zn1]*W_H1Drain[Zn2]-W_H1Drain[Zn1]+0*LF_Lat4Inflow1
# W_LatRecharge1[Zn2] = AF_LatInFlowRatio[Zn2]*W_H1Drain[Zn3]-W_H1Drain[Zn2]+0*LF_Lat4Inflow1
# W_LatRecharge1[Zn3] = AF_LatInFlowRatio[Zn3]*W_H1Drain[Zn4]-W_H1Drain[Zn3]+0*LF_Lat4Inflow1
# W_LatRecharge1[Zn4] = AF_LatInFlowRatio[Zn4]*LF_Lat4Inflow1 - W_H1Drain[Zn4]

# W_LatRecharge2[Zn1] = AF_LatInFlowRatio[Zn1]*W_H2Drain[Zn2]-W_H2Drain[Zn1]+0*LF_Lat4Inflow2
# W_LatRecharge2[Zn2] = AF_LatInFlowRatio[Zn2]*W_H2Drain[Zn3]-W_H2Drain[Zn2]+0*LF_Lat4Inflow2
# W_LatRecharge2[Zn3] = AF_LatInFlowRatio[Zn3]*W_H2Drain[Zn4] - W_H2Drain[Zn3]+0*LF_Lat4Inflow2
# W_LatRecharge2[Zn4] = AF_LatInFlowRatio[Zn4]*LF_Lat4Inflow2-W_H2Drain[Zn4]
# W_LatRecharge3[Zn1] = AF_LatInFlowRatio[Zn1]*W_H3Drain[Zn2]-W_H3Drain[Zn1]+0*LF_Lat4Inflow3
# W_LatRecharge3[Zn2] = AF_LatInFlowRatio[Zn2]*W_H3Drain[Zn3]+0*LF_Lat4Inflow3-W_H3Drain[Zn2]
# W_LatRecharge3[Zn3] = AF_LatInFlowRatio[Zn3]*W_H3Drain[Zn4]+0*LF_Lat4Inflow3-W_H3Drain[Zn3]
# W_LatRecharge3[Zn4] = AF_LatInFlowRatio[Zn4]*LF_Lat4Inflow3-W_H3Drain[Zn4]
# W_LatRecharge4[Zn1] = AF_LatInFlowRatio[Zn1]*W_H4Drain[Zn2]-W_H4Drain[Zn1]+0*LF_Lat4Inflow4
# W_LatRecharge4[Zn2] = AF_LatInFlowRatio[Zn2]*W_H4Drain[Zn3]+0*LF_Lat4Inflow4-W_H4Drain[Zn2]
# W_LatRecharge4[Zn3] = AF_LatInFlowRatio[Zn3]*W_H4Drain[Zn4]+0*LF_Lat4Inflow4-W_H4Drain[Zn3]
# W_LatRecharge4[Zn4] = AF_LatInFlowRatio[Zn4]*LF_Lat4Inflow4-W_H4Drain[Zn4]

zonelayer_df$W_HDrain_right <- 0
for (i in 1:nlayer) {
  zonelayer_df[zonelayer_df$layer == i, ]$W_HDrain_right <- c(tail(zonelayer_df[zonelayer_df$layer == i, ]$W_HDrain, -1), layer_df[layer_df$layer == i, ]$LF_Lat4Inflow)
}
zonelayer_df$AF_LatInFlowRatio <- rep(zone_df$AF_LatInFlowRatio, nlayer)
zonelayer_df$W_LatRecharge <- zonelayer_df$AF_LatInFlowRatio * zonelayer_df$W_HDrain_right - zonelayer_df$W_HDrain




# Rt3_LRV1[Zone,Tree] = if RT3_VoxVol[Zone,1]>0 then (10^-4)*RT_L1FRLength[Zone,Tree]/RT3_VoxVol[Zone,1] else 0
# Rt3_LRV2[Zone,Tree] = if RT3_VoxVol[Zone,2]>0 then (10^-4)*RT_L2FRLength[Zone,Tree]/RT3_VoxVol[Zone,2] else 0
# Rt3_LRV3[Zone,Tree] = if RT3_VoxVol[Zone,3]>0 then (10^-4)*RT_L3FRLength[Zone,Tree]/RT3_VoxVol[Zone,3] else 0
# Rt3_LRV4[Zone,Tree] = if RT3_VoxVol[Zone,4]>0 then (10^-4)*RT_L4FRLength[Zone,Tree]/RT3_VoxVol[Zone,4] else 0
zonelayertree_df$RT3_VoxVol <- rep(zonelayer_df$RT3_VoxVol, ntree)

zonelayertree_df$Rt3_LRV <- ifelse(
  zonelayertree_df$RT3_VoxVol > 0,
  (10^-4) * zonelayertree_df$RT_FRLength / zonelayertree_df$RT3_VoxVol,
  0
)




# T_Root_DWtot[Sp1] = AF_ZoneFrac[Zn1]*(T_RootT1DW[Zn1,1]+T_RootT1DW[Zn1,2]+T_RootT1DW[Zn1,3]+T_RootT3DW[Zn1,4])+
#   AF_ZoneFrac[Zn2]*(T_RootT1DW[Zn2,1]+T_RootT1DW[Zn2,2]+T_RootT1DW[Zn2,3]+T_RootT1DW[Zn2,4])+
#   AF_ZoneFrac[Zn3]*(T_RootT1DW[Zn3,1]+T_RootT1DW[Zn3,2]+T_RootT1DW[Zn3,3]+T_RootT1DW[Zn3,4])+
#   AF_ZoneFrac[Zn4]*(T_RootT1DW[Zn4,1]+T_RootT1DW[Zn4,2]+T_RootT1DW[Zn4,3]+T_RootT1DW[Zn4,4])+
#   0*(T_RootT1DW[Zn1,1]+T_RootT2DW[Zn1,1]+T_RootT3DW[Zn1,1])
# T_Root_DWtot[Sp2] = AF_ZoneFrac[Zn1]*(T_RootT2DW[Zn1,1]+T_RootT2DW[Zn1,2]+T_RootT2DW[Zn1,3]+T_RootT2DW[Zn1,4])+
#   AF_ZoneFrac[Zn2]*(T_RootT2DW[Zn2,1]+T_RootT2DW[Zn2,2]+T_RootT2DW[Zn2,3]+T_RootT2DW[Zn2,4])+
#   AF_ZoneFrac[Zn3]*(T_RootT2DW[Zn3,1]+T_RootT2DW[Zn3,2]+T_RootT2DW[Zn3,3]+T_RootT2DW[Zn3,4])+
#   AF_ZoneFrac[Zn4]*(T_RootT2DW[Zn4,1]+T_RootT2DW[Zn4,2]+T_RootT2DW[Zn4,3]+T_RootT2DW[Zn4,4])+
#   0*(T_RootT1DW[Zn1,1]+T_RootT2DW[Zn1,1]+T_RootT3DW[Zn1,1])
# T_Root_DWtot[Sp3] = AF_ZoneFrac[Zn1]*(T_RootT3DW[Zn1,1]+T_RootT3DW[Zn1,2]+T_RootT3DW[Zn1,3]+T_RootT3DW[Zn1,4])+
#   AF_ZoneFrac[Zn2]*(T_RootT3DW[Zn2,1]+T_RootT3DW[Zn2,2]+T_RootT3DW[Zn2,3]+T_RootT3DW[Zn2,4])+
#   AF_ZoneFrac[Zn3]*(T_RootT3DW[Zn3,1]+T_RootT3DW[Zn3,2]+T_RootT3DW[Zn3,3]+T_RootT3DW[Zn3,4])+
#   AF_ZoneFrac[Zn4]*(T_RootT3DW[Zn4,1]+T_RootT3DW[Zn4,2]+T_RootT3DW[Zn4,3]+T_RootT3DW[Zn4,4])+
#   0*(T_RootT1DW[Zn1,1]+T_RootT2DW[Zn1,1]+T_RootT3DW[Zn1,1])

T_RootTDW_byzone_df <- aggregate(list(T_Root = zonelayertreepcomp_df[zonelayertreepcomp_df$PlantComp == "DW", "T_Root"]), by = zonelayertree_df[c("zone", "tree_id")], sum)
T_RootTDW_byzone_df <- merge(T_RootTDW_byzone_df, zone_df[c("zone", "AF_ZoneFrac")], by = "zone")
T_RootTDW_byzone_df$T_RootTDW_frac <- T_RootTDW_byzone_df$AF_ZoneFrac * T_RootTDW_byzone_df$T_Root
T_Root_DWtot_df <- aggregate(T_RootTDW_byzone_df[c("T_RootTDW_frac")], by = T_RootTDW_byzone_df[c("tree_id")], sum)
T_Root_DWtot_df <- T_Root_DWtot_df[order(T_Root_DWtot_df$tree_id), ]
tree_df$T_Root_DWtot <- T_Root_DWtot_df$T_RootTDW_frac

# T_ProxRootSumDiamSq[Tree] = IF (Rt_TProxGini[Tree]>0 AND T_DiamRtWght1[Tree]>0 and T_Treesperha[Tree]>0) THEN (Rt_TProxGini[Tree]/(2+Rt_TProxGini[Tree]))*(T_Root_DWtot[Tree]*(10000/T_Treesperha[Tree])*(T_DiamSlopeRtWght[Tree]+Rt_TProxGini[Tree])/(Rt_TProxGini[Tree]*T_DiamRtWght1[Tree]))^(2/T_DiamSlopeRtWght[Tree]) ELSE 0
tree_df$T_ProxRootSumDiamSq <- ifelse(
  tree_df$Rt_TProxGini > 0 &
    tree_df$T_DiamRtWght1 > 0 & tree_df$T_Treesperha > 0,
  (tree_df$Rt_TProxGini / (2 + tree_df$Rt_TProxGini)) *
    (
      tree_df$T_Root_DWtot * (10000 / tree_df$T_Treesperha) *
        (tree_df$T_DiamSlopeRtWght +
           tree_df$Rt_TProxGini) / (tree_df$Rt_TProxGini * tree_df$T_DiamRtWght1)
    )^(2 / tree_df$T_DiamSlopeRtWght),
  0
)


# Rt_TTotLength[Tree] = IF T_DiamSlopeRtLeng[Tree]>0 AND Rt_TProxGini[Tree] >0 THEN (T_Treesperha[Tree]/10000)*(T_DiamRtLeng1[Tree]*Rt_TProxGini[Tree]/(T_DiamSlopeRtLeng[Tree]+Rt_TProxGini[Tree]))*(T_ProxRootSumDiamSq[Tree]*(Rt_TProxGini[Tree]+2)/Rt_TProxGini[Tree])^(T_DiamSlopeRtLeng[Tree]/2) ELSE 0
tree_df$Rt_TTotLength <- ifelse(
  tree_df$T_DiamSlopeRtLeng > 0 & tree_df$Rt_TProxGini > 0,
  (tree_df$T_Treesperha / 10000) * (
    tree_df$T_DiamRtLeng1 * tree_df$Rt_TProxGini / (tree_df$T_DiamSlopeRtLeng +
                                                      tree_df$Rt_TProxGini)
  ) * (
    tree_df$T_ProxRootSumDiamSq * (tree_df$Rt_TProxGini + 2) / tree_df$Rt_TProxGini
  )^(tree_df$T_DiamSlopeRtLeng / 2),
  0
)



# Rt_TSRL[Tree] = if Rt_T_UseFBASRL? then (if T_Root_DWtot[Tree]>0 then (Rt_TTotLength[Tree]/T_Root_DWtot[Tree])*10^-5 else Rt_T_FixedSRL) else Rt_T_FixedSRL
tree_df$Rt_T_UseFBASRL_is <- pars$Rt_par$Rt_T_UseFBASRL_is

tree_df$Rt_TSRL <- ifelse(
  tree_df$Rt_T_UseFBASRL_is == 1,
  ifelse(
    tree_df$T_Root_DWtot > 0,
    (tree_df$Rt_TTotLength / tree_df$T_Root_DWtot) * 10^-5,
    pars$Rt_par$Rt_T_FixedSRL
  ),
  pars$Rt_par$Rt_T_FixedSRL
)

#TODO: Rt_TLraCD is dynamic but seems static here

# Rt_TLraX0D[Tree] = IF(Rt_TLraCD[Tree]>0)THEN(1000*T_Root_DWtot[Tree]*Rt_TSRL[Tree]/Rt_TLraCD[Tree])ELSE(0)
tree_df$Rt_TLraX0D <- ifelse(
  tree_df$Rt_TLraCD > 0,
  1000 * tree_df$T_Root_DWtot * tree_df$Rt_TSRL / tree_df$Rt_TLraCD,
  0
)

# Rt_TlraX0Curr[Tree] = IF(Rt_ATType[Tree]=1)THEN(Rt_TLraX0[Tree])ELSE IF Rt_ATType[Tree]=2 THEN Rt_TLraX0D[Tree] ELSE (0)
tree_df$Rt_TlraX0Curr <- ifelse(
  tree_df$Rt_ATType == 1 ,
  tree_df$Rt_TLraX0,
  ifelse(tree_df$Rt_ATType == 2, tree_df$Rt_TLraX0D, 0)
)


#TODO: this can be moved to init
zonelayertree_df$Rt_ZoneLeft <- rep(zone_df$Rt_ZoneLeft, nlayer * ntree)
zonelayertree_df$Rt_ZoneRight <- rep(zone_df$Rt_ZoneRight, nlayer * ntree)
zonelayertree_df$Rt_Depth <- rep(zonelayer_df$Rt_Depth, ntree)
zonelayertree_df$Rt_Depth_up <- 0
zonelayertree_df[zonelayertree_df$layer %in% 2:4, ]$Rt_Depth_up <- zonelayertree_df[zonelayertree_df$layer %in% 1:3, ]$Rt_Depth
zonelayertree_df$AF_TreePosit2Q <- rep(tree_df$AF_TreePosit2Q, each = nzone *
                                         nlayer)
zonelayertree_df$AF_TreePosit3Q <- rep(tree_df$AF_TreePosit3Q, each = nzone *
                                         nlayer)
zonelayertree_df$AF_TreePosit4Q <- rep(tree_df$AF_TreePosit4Q, each = nzone *
                                         nlayer)


#### up


zonelayertree_df$Rt_TDistShapeAct <- rep(tree_df$Rt_TDistShapeAct, each = nzone *
                                           nlayer)
zonelayertree_df$Rt_TlraX0Curr <- rep(tree_df$Rt_TlraX0Curr, each = nzone *
                                        nlayer)
zonelayertree_df$Rt_TDecDepthAct <- rep(tree_df$Rt_TDecDepthAct, each = nzone *
                                          nlayer)

# Rt_TLrvEllip1[Zone,Tree] = Rt_TlraX0Curr[Tree]*Rt_TDecDepthAct[Tree]*EXP(-0.25*Rt_TDecDepthAct[Tree]*((Rt_TDistShapeAct[Tree]*Rt_ZoneLeft[Zone])+(Rt_TDistShapeAct[Tree]*Rt_ZoneRight[Zone])+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneLeft[Zone])^2+Rt_Depth1[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneRight[Zone])^2+Rt_Depth1[Zone]^2)))/10000
# Rt_TLrvEllip2[Zone,Tree] = Rt_TlraX0Curr[Tree]*Rt_TDecDepthAct[Tree]*EXP(-0.25*Rt_TDecDepthAct[Tree]*(SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneLeft[Zone])^2+Rt_Depth1[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneRight[Zone])^2+Rt_Depth1[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneLeft[Zone])^2+Rt_Depth2[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneRight[Zone])^2+Rt_Depth2[Zone]^2)))/10000
# Rt_TLrvEllip3[Zone,Tree] = Rt_TlraX0Curr[Tree]*Rt_TDecDepthAct[Tree]*EXP(-0.25*Rt_TDecDepthAct[Tree]*(SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneLeft[Zone])^2+Rt_Depth2[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneRight[Zone])^2+Rt_Depth2[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneLeft[Zone])^2+Rt_Depth3[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneRight[Zone])^2+Rt_Depth3[Zone]^2)))/10000
# Rt_TLrvEllip4[Zone,Tree] = Rt_TlraX0Curr[Tree]*Rt_TDecDepthAct[Tree]*EXP(-0.25*Rt_TDecDepthAct[Tree]*(SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneLeft[Zone])^2+Rt_Depth3[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneRight[Zone])^2+Rt_Depth3[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneLeft[Zone])^2+Rt_Depth4[Zone]^2)+SQRT((Rt_TDistShapeAct[Tree]*Rt_ZoneRight[Zone])^2+Rt_Depth4[Zone]^2)))/10000


zonelayertree_df$Rt_TDecDepthAct_a <- sqrt((
  zonelayertree_df$Rt_TDistShapeAct * zonelayertree_df$Rt_ZoneLeft
)^2 + zonelayertree_df$Rt_Depth_up^2
) +
  sqrt((
    zonelayertree_df$Rt_TDistShapeAct * zonelayertree_df$Rt_ZoneRight
  )^2 + zonelayertree_df$Rt_Depth_up^2
  )
zonelayertree_df$Rt_TDecDepthAct_b <- sqrt((
  zonelayertree_df$Rt_TDistShapeAct * zonelayertree_df$Rt_ZoneLeft
)^2 + zonelayertree_df$Rt_Depth^2
) +
  sqrt((
    zonelayertree_df$Rt_TDistShapeAct * zonelayertree_df$Rt_ZoneRight
  )^2 + zonelayertree_df$Rt_Depth^2
  )

zl1t <- zonelayertree_df[zonelayertree_df$layer == 1, ]
zonelayertree_df[zonelayertree_df$layer == 1, ]$Rt_TDecDepthAct_a <- (zl1t$Rt_TDistShapeAct * zl1t$Rt_ZoneLeft) + (zl1t$Rt_TDistShapeAct * zl1t$Rt_ZoneRight)

zonelayertree_df$Rt_TLrvEllip <- zonelayertree_df$Rt_TlraX0Curr * zonelayertree_df$Rt_TDecDepthAct *
  exp(
    -0.25 * zonelayertree_df$Rt_TDecDepthAct *  (
      zonelayertree_df$Rt_TDecDepthAct_a + zonelayertree_df$Rt_TDecDepthAct_b
    )
  ) / 10000





# Rt_TLrvEllipAct1[Zn1,Sp1] = Rt_TLrvEllip1[Zn1,Sp1]*(1-AF_TreePosit4Q[Sp1]) + AF_TreePosit4Q[Sp1]*Rt_TLrvEllip1[Zn4,Sp1]
# Rt_TLrvEllipAct1[Zn1,Sp2] = Rt_TLrvEllip1[Zn1,Sp2]*(1-AF_TreePosit4Q[Sp2]) + AF_TreePosit4Q[Sp2]*Rt_TLrvEllip1[Zn4,Sp2]
# Rt_TLrvEllipAct1[Zn1,Sp3] = Rt_TLrvEllip1[Zn1,Sp3]*(1-AF_TreePosit4Q[Sp3]) + AF_TreePosit4Q[Sp3]*Rt_TLrvEllip1[Zn4,Sp3]
# Rt_TLrvEllipAct1[Zn2,Sp1] = Rt_TLrvEllip1[Zn2,Sp1]*(1-AF_TreePosit4Q[Sp1]) + AF_TreePosit4Q[Sp1]*Rt_TLrvEllip1[Zn3,Sp1]
# Rt_TLrvEllipAct1[Zn2,Sp2] = Rt_TLrvEllip1[Zn2,Sp2]*(1-AF_TreePosit4Q[Sp2]) + AF_TreePosit4Q[Sp2]*Rt_TLrvEllip1[Zn3,Sp2]
# Rt_TLrvEllipAct1[Zn2,Sp3] = Rt_TLrvEllip1[Zn2,Sp3]*(1-AF_TreePosit4Q[Sp3]) + AF_TreePosit4Q[Sp3]*Rt_TLrvEllip1[Zn3,Sp3]
zonelayertree_df$Rt_TLrvEllipAct <- zonelayertree_df$Rt_TLrvEllip * (1 - zonelayertree_df$AF_TreePosit4Q) + zonelayertree_df$AF_TreePosit4Q * zonelayertree_df$Rt_TLrvEllip



tree_df$T_PlantY <- apply(tree_df[c("T_Compl", "tree_id")], 1, function(x) {
  get_graph_y(T_PlantY_df,
              x["T_Compl"],
              y_column = names(T_PlantY_df)[x["tree_id"]],
              mode = "discrete")
})

tree_df$T_PlantDoY <- apply(tree_df[c("T_Compl", "tree_id")], 1, function(x) {
  get_graph_y(T_PlantDoY_df,
              x["T_Compl"],
              y_column = names(T_PlantDoY_df)[x["tree_id"]],
              mode = "discrete")
})


# T_PlantTime[Tree] = T_PlantDoY[Tree] + 365* (T_PlantY[Tree])-Ca_DOYStart
tree_df$T_PlantTime <- tree_df$T_PlantDoY + 365 * (tree_df$T_PlantY) -   Ca_DOYStart

# INIT TF_LeafTime[Tree] = T_PlantTime[Tree]
tree_df$TF_LeafTime <- tree_df$T_PlantTime

# Rt_TPresence[Tree] = IF (TIME = (T_PlantTime[Tree]-1) OR T_GroRes[DW,Tree]>0) THEN 1 ELSE 0
tree_df$time <- time
tree_df$Rt_TPresence <- ifelse(tree_df$time == (tree_df$T_PlantTime - 1) |
                                 treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_GroRes > 0, 1, 0)

zonelayertree_df$Rt_TPresence <- rep(tree_df$Rt_TPresence, each = nzone * nlayer)





# Rt_TLrvL1[Zn1,Sp1] = IF AF_TreePosit4Q[Sp1]=0 AND AF_TreePosit2Q[Sp1]=0 AND AF_TreePosit3Q[Sp1]=0 THEN 0*T_Par2[Lrv31]+T_Par1[Lrv11]*Rt_TMultiplier+0*T_Par3[Lrv24]
# ELSE IF AF_TreePosit2Q[Sp1]=1 THEN T_Par1[Lrv13]*Rt_TMultiplier
# ELSE IF AF_TreePosit3Q[Sp1]=1 THEN T_Par1[Lrv14]*Rt_TMultiplier
# ELSE T_Par2[Lrv14]*Rt_TMultiplier
# Rt_TLrvL1[Zn1,Sp2] =  IF AF_TreePosit4Q[Sp2]=0 AND AF_TreePosit2Q[Sp2]=0 AND AF_TreePosit3Q[Sp2]=0 THEN 0*T_Par1[Lrv31]+T_Par2[Lrv11]*Rt_TMultiplier+0*T_Par3[Lrv24]
# ELSE IF AF_TreePosit2Q[Sp2]=1 THEN T_Par2[Lrv13]*Rt_TMultiplier
# ELSE IF AF_TreePosit3Q[Sp2]=1 THEN T_Par2[Lrv14]*Rt_TMultiplier
# ELSE T_Par2[Lrv14]*Rt_TMultiplier
# Rt_TLrvL1[Zn1,Sp3] =  IF AF_TreePosit4Q[Sp3]=0 AND AF_TreePosit2Q[Sp3]=0 AND AF_TreePosit3Q[Sp3]=0 THEN 0*T_Par1[Lrv31]+0*T_Par2[Lrv14]+T_Par3[Lrv11]*Rt_TMultiplier
# ELSE IF AF_TreePosit2Q[Sp3]=1 THEN T_Par3[Lrv13]*Rt_TMultiplier
# ELSE IF AF_TreePosit3Q[Sp3]=1 THEN T_Par3[Lrv14]*Rt_TMultiplier
# ELSE T_Par3[Lrv14]*Rt_TMultiplier
# Rt_TLrvL1[Zn2,Sp1] =  IF AF_TreePosit4Q[Sp1]=0 AND AF_TreePosit2Q[Sp1]=0 AND AF_TreePosit3Q[Sp1]=0 THEN T_Par1[Lrv12]*Rt_TMultiplier+0*T_Par2[Lrv14]+0*T_Par3[Lrv14]
# ELSE IF AF_TreePosit2Q[Sp1]=1 THEN T_Par1[Lrv11]*Rt_TMultiplier
# ELSE IF AF_TreePosit3Q[Sp1]=1 THEN T_Par1[Lrv13]*Rt_TMultiplier
# ELSE T_Par1[Lrv13]*Rt_TMultiplier
# Rt_TLrvL1[Zn2,Sp2] =  IF AF_TreePosit4Q[Sp2]=0 AND AF_TreePosit2Q[Sp2]=0 AND AF_TreePosit3Q[Sp2]=0 THEN 0*T_Par1[Lrv31]+T_Par2[Lrv12]*Rt_TMultiplier+0*T_Par3[Lrv24]
# ELSE IF AF_TreePosit2Q[Sp2]=1 THEN T_Par2[Lrv11]*Rt_TMultiplier
# ELSE IF AF_TreePosit3Q[Sp2]=1 THEN T_Par2[Lrv13]*Rt_TMultiplier
# ELSE T_Par2[Lrv13]*Rt_TMultiplier
# Rt_TLrvL1[Zn2,Sp3] =  IF AF_TreePosit4Q[Sp3]=0 AND AF_TreePosit2Q[Sp3]=0 AND AF_TreePosit3Q[Sp3]=0 THEN 0*T_Par1[Lrv31]+0*T_Par2[Lrv14]+T_Par3[Lrv12]*Rt_TMultiplier
# ELSE IF AF_TreePosit2Q[Sp3]=1 THEN T_Par3[Lrv11]*Rt_TMultiplier
# ELSE IF AF_TreePosit3Q[Sp3]=1 THEN T_Par3[Lrv13]*Rt_TMultiplier
# ELSE T_Par3[Lrv13]*Rt_TMultiplier
# ...

### EDIT ERROR ########################

zonelayertree_df$Rt_TLrvL_2Q <- 0
zonelayertree_df$Rt_TLrvL_3Q <- 0
zonelayertree_df$Rt_TLrvL_def <- 0

zonelayertree_df[zonelayertree_df$zone == 1, ]$Rt_TLrvL_2Q <- zonelayertree_df[zonelayertree_df$zone == 3, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 2, ]$Rt_TLrvL_2Q <- zonelayertree_df[zonelayertree_df$zone == 1, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 3, ]$Rt_TLrvL_2Q <- zonelayertree_df[zonelayertree_df$zone == 2, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 4, ]$Rt_TLrvL_2Q <- zonelayertree_df[zonelayertree_df$zone == 4, ]$Rt_TLrvL_par

zonelayertree_df[zonelayertree_df$zone == 1, ]$Rt_TLrvL_3Q <- zonelayertree_df[zonelayertree_df$zone == 4, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 2, ]$Rt_TLrvL_3Q <- zonelayertree_df[zonelayertree_df$zone == 3, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 3, ]$Rt_TLrvL_3Q <- zonelayertree_df[zonelayertree_df$zone == 1, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 4, ]$Rt_TLrvL_3Q <- zonelayertree_df[zonelayertree_df$zone == 2, ]$Rt_TLrvL_par

zonelayertree_df[zonelayertree_df$zone == 1, ]$Rt_TLrvL_def <- zonelayertree_df[zonelayertree_df$zone == 4, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 2, ]$Rt_TLrvL_def <- zonelayertree_df[zonelayertree_df$zone == 3, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 3, ]$Rt_TLrvL_def <- zonelayertree_df[zonelayertree_df$zone == 2, ]$Rt_TLrvL_par
zonelayertree_df[zonelayertree_df$zone == 4, ]$Rt_TLrvL_def <- zonelayertree_df[zonelayertree_df$zone == 1, ]$Rt_TLrvL_par


zonelayertree_df$Rt_TLrvL <- ifelse(
  zonelayertree_df$AF_TreePosit4Q == 0 &
    zonelayertree_df$AF_TreePosit2Q == 0 &
    zonelayertree_df$AF_TreePosit3Q == 0,
  zonelayertree_df$Rt_TLrvL_par * pars$Rt_par$Rt_TMultiplier,
  ifelse(
    zonelayertree_df$AF_TreePosit2Q == 1,
    zonelayertree_df$Rt_TLrvL_2Q * pars$Rt_par$Rt_TMultiplier,
    ifelse(
      zonelayertree_df$AF_TreePosit3Q == 1,
      zonelayertree_df$Rt_TLrvL_3Q * pars$Rt_par$Rt_TMultiplier,
      zonelayertree_df$Rt_TLrvL_def * pars$Rt_par$Rt_TMultiplier
    )
  )
)


# W_PoreVol[Zone,SoilLayer] = (1-W_BDLayer[SoilLayer]*S_RelBD[Zone,SoilLayer]/2.5)
zonelayer_df$W_PoreVol <- (1 - zonelayer_df$W_BDLayer * zonelayer_df$S_RelBD / 2.5)



# W_PoreVolAbFC[Zn1,1] = W_PoreVol[Zn1,1]-W_FieldCap1[Zn1]+(0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn1,2] = W_PoreVol[Zn1,2]-W_FieldCap2[Zn1]+(0*(W_FieldCap1[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn1,3] = W_PoreVol[Zn1,3]-W_FieldCap3[Zn1]+(0*(W_FieldCap2[Zn1]+W_FieldCap1[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn1,4] = W_PoreVol[Zn1,4]-W_FieldCap4[Zn1]+(0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap1[Zn1]))
# W_PoreVolAbFC[Zn2,1] = W_PoreVol[Zn2,1]-W_FieldCap1[Zn2]+(0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn2,2] = W_PoreVol[Zn2,2]-W_FieldCap2[Zn2]+(0*(W_FieldCap1[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn2,3] = W_PoreVol[Zn2,3]-W_FieldCap3[Zn2]+(0*(W_FieldCap2[Zn1]+W_FieldCap1[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn2,4] = W_PoreVol[Zn2,4]-W_FieldCap4[Zn2]+(0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap1[Zn1]))
# W_PoreVolAbFC[Zn3,1] = W_PoreVol[Zn3,1]-W_FieldCap1[Zn3]+(0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn3,2] = W_PoreVol[Zn3,2]-W_FieldCap2[Zn3]+(0*(W_FieldCap1[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn3,3] = W_PoreVol[Zn3,3]-W_FieldCap3[Zn3]+(0*(W_FieldCap2[Zn1]+W_FieldCap1[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn3,4] = W_PoreVol[Zn3,4]-W_FieldCap4[Zn3]+(0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap1[Zn1]))
# W_PoreVolAbFC[Zn4,1] = W_PoreVol[Zn4,1]-W_FieldCap1[Zn4]+(0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn4,2] = W_PoreVol[Zn4,2]-W_FieldCap2[Zn4]+(0*(W_FieldCap1[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn4,3] = W_PoreVol[Zn4,3]-W_FieldCap3[Zn4]+(0*(W_FieldCap2[Zn1]+W_FieldCap1[Zn1]+W_FieldCap4[Zn1]))
# W_PoreVolAbFC[Zn4,4] = W_PoreVol[Zn4,4]-W_FieldCap4[Zn4]+(0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap1[Zn1]))

zonelayer_df$W_PoreVolAbFC <- zonelayer_df$W_PoreVol - zonelayer_df$W_FieldCap


# W_WaterLog[Zn1,1] = if (W_WaterfilledPoreF1[Zn1]*W_PoreVol[Zn1,1])-W_FieldCap1[Zn1] < 0 then 0 else (W_WaterfilledPoreF1[Zn1]*W_PoreVol[Zn1,1])-W_FieldCap1[Zn1] + (0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn1,2] = if (W_WaterfilledPoreF2[Zn1]*W_PoreVol[Zn1,2])-W_FieldCap2[Zn1] < 0 then 0 else (W_WaterfilledPoreF2[Zn1]*W_PoreVol[Zn1,2])-W_FieldCap2[Zn1] + (0*(W_FieldCap1[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF1[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn1,3] = if (W_WaterfilledPoreF3[Zn1]*W_PoreVol[Zn1,3])-W_FieldCap3[Zn1] < 0 then 0 else (W_WaterfilledPoreF3[Zn1]*W_PoreVol[Zn1,3])-W_FieldCap3[Zn1] + (0*(W_FieldCap2[Zn1]+W_FieldCap1[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF1[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn1,4] = if (W_WaterfilledPoreF4[Zn1]*W_PoreVol[Zn1,4])-W_FieldCap4[Zn1] < 0 then 0 else (W_WaterfilledPoreF4[Zn1]*W_PoreVol[Zn1,4])-W_FieldCap4[Zn1] + (0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap1[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF1[Zn1]))
# W_WaterLog[Zn2,1] = if (W_WaterfilledPoreF1[Zn2]*W_PoreVol[Zn2,1])-W_FieldCap1[Zn2] < 0 then 0 else (W_WaterfilledPoreF1[Zn2]*W_PoreVol[Zn2,1])-W_FieldCap1[Zn2] + (0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn2,2] = if (W_WaterfilledPoreF2[Zn2]*W_PoreVol[Zn2,2])-W_FieldCap2[Zn2] < 0 then 0 else (W_WaterfilledPoreF2[Zn2]*W_PoreVol[Zn2,2])-W_FieldCap2[Zn2] + (0*(W_FieldCap1[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF1[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn2,3] = if (W_WaterfilledPoreF3[Zn2]*W_PoreVol[Zn2,3])-W_FieldCap3[Zn2] < 0 then 0 else (W_WaterfilledPoreF3[Zn2]*W_PoreVol[Zn2,3])-W_FieldCap3[Zn2] + (0*(W_FieldCap2[Zn1]+W_FieldCap1[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF1[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn2,4] = if (W_WaterfilledPoreF4[Zn2]*W_PoreVol[Zn2,4])-W_FieldCap4[Zn2] < 0 then 0 else (W_WaterfilledPoreF4[Zn2]*W_PoreVol[Zn2,4])-W_FieldCap4[Zn2] + (0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap1[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF1[Zn1]))
# W_WaterLog[Zn3,1] = if (W_WaterfilledPoreF1[Zn3]*W_PoreVol[Zn3,1])-W_FieldCap1[Zn3] < 0 then 0 else (W_WaterfilledPoreF1[Zn3]*W_PoreVol[Zn3,1])-W_FieldCap1[Zn3] + (0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn3,2] = if (W_WaterfilledPoreF2[Zn3]*W_PoreVol[Zn3,2])-W_FieldCap2[Zn3] < 0 then 0 else (W_WaterfilledPoreF2[Zn3]*W_PoreVol[Zn3,2])-W_FieldCap2[Zn3] + (0*(W_FieldCap1[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF1[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn3,3] = if (W_WaterfilledPoreF3[Zn3]*W_PoreVol[Zn3,3])-W_FieldCap3[Zn3] < 0 then 0 else (W_WaterfilledPoreF3[Zn3]*W_PoreVol[Zn3,3])-W_FieldCap3[Zn3] + (0*(W_FieldCap2[Zn1]+W_FieldCap1[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF1[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn3,4] = if (W_WaterfilledPoreF4[Zn3]*W_PoreVol[Zn3,4])-W_FieldCap4[Zn3] < 0 then 0 else (W_WaterfilledPoreF4[Zn3]*W_PoreVol[Zn3,4])-W_FieldCap4[Zn3] + (0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap1[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF1[Zn1]))
# W_WaterLog[Zn4,1] = if (W_WaterfilledPoreF1[Zn4]*W_PoreVol[Zn4,1])-W_FieldCap1[Zn4] < 0 then 0 else (W_WaterfilledPoreF1[Zn4]*W_PoreVol[Zn4,1])-W_FieldCap1[Zn4] + (0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn4,2] = if (W_WaterfilledPoreF2[Zn4]*W_PoreVol[Zn4,2])-W_FieldCap2[Zn4] < 0 then 0 else (W_WaterfilledPoreF2[Zn4]*W_PoreVol[Zn4,2])-W_FieldCap2[Zn4] + (0*(W_FieldCap1[Zn1]+W_FieldCap3[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF1[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn4,3] = if (W_WaterfilledPoreF3[Zn4]*W_PoreVol[Zn4,3])-W_FieldCap3[Zn4] < 0 then 0 else (W_WaterfilledPoreF3[Zn4]*W_PoreVol[Zn4,3])-W_FieldCap3[Zn4] + (0*(W_FieldCap2[Zn1]+W_FieldCap1[Zn1]+W_FieldCap4[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF1[Zn1]+W_WaterfilledPoreF4[Zn1]))
# W_WaterLog[Zn4,4] = if (W_WaterfilledPoreF4[Zn4]*W_PoreVol[Zn4,4])-W_FieldCap4[Zn4] < 0 then 0 else (W_WaterfilledPoreF4[Zn4]*W_PoreVol[Zn4,4])-W_FieldCap4[Zn4] + (0*(W_FieldCap2[Zn1]+W_FieldCap3[Zn1]+W_FieldCap1[Zn1]+W_WaterfilledPoreF2[Zn1]+W_WaterfilledPoreF3[Zn1]+W_WaterfilledPoreF1[Zn1]))

zonelayer_df$W_WaterLog <- ifelse(
  (zonelayer_df$W_WaterfilledPoreF * zonelayer_df$W_PoreVol) - zonelayer_df$W_FieldCap < 0,
  0,
  (zonelayer_df$W_WaterfilledPoreF * zonelayer_df$W_PoreVol) - zonelayer_df$W_FieldCap
)


# W_AnaerobiosisIndex[Zone,SoilLayer] = if W_WaterLog[Zone,SoilLayer]/W_PoreVolAbFC[Zone,SoilLayer] < 0 then 0 else W_WaterLog[Zone,SoilLayer]/W_PoreVolAbFC[Zone,SoilLayer]
zonelayer_df$W_AnaerobiosisIndex <- ifelse(
  zonelayer_df$W_WaterLog / zonelayer_df$W_PoreVolAbFC < 0,
  0,
  zonelayer_df$W_WaterLog / zonelayer_df$W_PoreVolAbFC
)

zonelayertree_df$W_AnaerobiosisIndex <- rep(zonelayer_df$W_AnaerobiosisIndex, nrow(tree_df))

# Rt_TLrvEff1[Zone,Tree] = Rt_TLrvL1[Zone,Tree]*(1-W_AnaerobiosisIndex1[Zone])
# Rt_TLrvEff2[Zone,Tree] = Rt_TLrvL2[Zone,Tree]*(1-W_AnaerobiosisIndex2[Zone])
# Rt_TLrvEff3[Zone,Tree] = Rt_TLrvL3[Zone,Tree]*(1-W_AnaerobiosisIndex3[Zone])
# Rt_TLrvEff4[Zone,Tree] = Rt_TLrvL4[Zone,Tree]*(1-W_AnaerobiosisIndex4[Zone])
zonelayertree_df$Rt_TLrvEff <- zonelayertree_df$Rt_TLrvL * (1 - zonelayertree_df$W_AnaerobiosisIndex)






# Rt_TLrvMinM1[Zone,Tree] = if AF_AnyTrees? < 0.5 then 0 else IF(Rt_ATType[Tree]=3) THEN (Rt3_LRV1[Zone,Tree]) else IF(Rt_ATType[Tree]=2)THEN (Rt_TLrvEllipAct1[Zone,Tree]) else IF(Rt_ATType[Tree]=1)THEN (Rt_TLrvEllipAct1[Zone,Tree]) ELSE if W_WaterLog? = 1 then (Rt_TPresence[Tree]*Rt_TLrvEff1[Zone,Tree]) else (Rt_TPresence[Tree]*Rt_TLrvL1[Zone,Tree])
# Rt_TLrvMinM2[Zone,Tree] = if AF_AnyTrees? < 0.5 then 0 else IF(Rt_ATType[Tree]=3) THEN (Rt3_LRV2[Zone,Tree]) else IF(Rt_ATType[Tree]=2)THEN (Rt_TLrvEllipAct2[Zone,Tree]) else IF(Rt_ATType[Tree]=1)THEN (Rt_TLrvEllipAct2[Zone,Tree]) ELSE if W_WaterLog? = 1 then (Rt_TPresence[Tree]*Rt_TLrvEff2[Zone,Tree]) else (Rt_TPresence[Tree]*Rt_TLrvL2[Zone,Tree])
# Rt_TLrvMinM3[Zone,Tree] = if AF_AnyTrees? < 0.5 then 0 else IF(Rt_ATType[Tree]=3) THEN (Rt3_LRV3[Zone,Tree]) else IF(Rt_ATType[Tree]=2)THEN (Rt_TLrvEllipAct3[Zone,Tree]) else IF(Rt_ATType[Tree]=1)THEN (Rt_TLrvEllipAct3[Zone,Tree]) ELSE if W_WaterLog? = 1 then (Rt_TPresence[Tree]*Rt_TLrvEff3[Zone,Tree]) else (Rt_TPresence[Tree]*Rt_TLrvL3[Zone,Tree])
# Rt_TLrvMinM4[Zone,Tree] = if AF_AnyTrees? < 0.5 then 0 else IF(Rt_ATType[Tree]=3) THEN (Rt3_LRV3[Zone,Tree]) else IF(Rt_ATType[Tree]=2)THEN (Rt_TLrvEllipAct4[Zone,Tree]) else IF(Rt_ATType[Tree]=1)THEN (Rt_TLrvEllipAct4[Zone,Tree]) ELSE if W_WaterLog? = 1 then (Rt_TPresence[Tree]*Rt_TLrvEff4[Zone,Tree]) else (Rt_TPresence[Tree]*Rt_TLrvL4[Zone,Tree])
zonelayertree_df$AF_AnyTrees_is <- pars$AF_par$AF_AnyTrees_is
zonelayertree_df$W_WaterLog_is <- pars$W_par$W_WaterLog_is

zonelayertree_df$Rt_TLrvMinM <- ifelse(
  zonelayertree_df$AF_AnyTrees_is < 0.5,
  0,
  ifelse(
    zonelayertree_df$Rt_ATType == 3,
    zonelayertree_df$Rt3_LRV,
    ifelse(
      zonelayertree_df$Rt_ATType == 2,
      zonelayertree_df$Rt_TLrvEllipAct,
      ifelse(
        zonelayertree_df$Rt_ATType == 1,
        zonelayertree_df$Rt_TLrvEllipAct,
        ifelse(
          zonelayertree_df$W_WaterLog_is == 1,
          zonelayertree_df$Rt_TPresence * zonelayertree_df$Rt_TLrvEff,
          zonelayertree_df$Rt_TPresence * zonelayertree_df$Rt_TLrvL
        )
      )
    )
  )
)

# Rt_TLrvM1[Zone,Tree] = if Rt_TDiam[Tree]>0 then Rt_TLrvMinM1[Zone,Tree]*(1+Rt_MTInfFrac1*T_MycMaxInf[Tree]*Rt_MTHypL[Tree]*SQRT(Rt_MTHypDiam[Tree])/SQRT(Rt_TDiam[Tree])) ELSE 0
# Rt_TLrvM2[Zone,Tree] = if Rt_TDiam[Tree]>0 then Rt_TLrvMinM2[Zone,Tree]*(1+Rt_MTInfFrac2*T_MycMaxInf[Tree]*Rt_MTHypL[Tree]*SQRT(Rt_MTHypDiam[Tree])/SQRT(Rt_TDiam[Tree])) ELSE 0
# Rt_TLrvM3[Zone,Tree] = if Rt_TDiam[Tree]>0 then Rt_TLrvMinM3[Zone,Tree]*(1+Rt_MTInfFrac3*T_MycMaxInf[Tree]*Rt_MTHypL[Tree]*SQRT(Rt_MTHypDiam[Tree])/SQRT(Rt_TDiam[Tree])) ELSE 0
# Rt_TLrvM4[Zone,Tree] = if Rt_TDiam[Tree]>0 then Rt_TLrvMinM4[Zone,Tree]*(1+Rt_MTInfFrac4*T_MycMaxInf[Tree]*Rt_MTHypL[Tree]*SQRT(Rt_MTHypDiam[Tree])/SQRT(Rt_TDiam[Tree])) ELSE 0

zonelayertree_df$Rt_TDiam <- rep(tree_df$Rt_TDiam, each = nrow(zone_df) * nrow(layer_df))
zonelayertree_df$T_MycMaxInf <- rep(tree_df$T_MycMaxInf, each = nrow(zone_df) * nrow(layer_df))
zonelayertree_df$Rt_MTHypL <- rep(tree_df$Rt_MTHypL, each = nrow(zone_df) * nrow(layer_df))
zonelayertree_df$Rt_MTHypDiam <- rep(tree_df$Rt_MTHypDiam, each = nrow(zone_df) * nrow(layer_df))
zonelayertree_df$Rt_MTInfFrac <- rep(rep(layer_df$Rt_MTInfFrac, each = nrow(zone_df)), nrow(tree_df))

zonelayertree_df$Rt_TLrvM <- ifelse(
  zonelayertree_df$Rt_TDiam > 0,
  zonelayertree_df$Rt_TLrvMinM * (
    1 + zonelayertree_df$Rt_MTInfFrac * zonelayertree_df$T_MycMaxInf * zonelayertree_df$Rt_MTHypL *
      sqrt(zonelayertree_df$Rt_MTHypDiam) /
      sqrt(zonelayertree_df$Rt_TDiam)
  ),
  0
)


# Cq_C_HostEffForT1[Zone] = if Cq_CType[Zone]=1 then C_HostEffForT1[Type1] else
#   if Cq_CType[Zone]=2 then C_HostEffForT1[Type2] else
# if Cq_CType[Zone]=3 then C_HostEffForT1[Type3] else
# if Cq_CType[Zone]=4 then C_HostEffForT1[Type4] else
#   if Cq_CType[Zone]=5 then C_HostEffForT1[Type5] else 0

# TODO: to be revised
zone_df$Cq_C_HostEffForT1 <- crop_df$C_HostEffForT1[zone_df$Cq_CType]



zonelayer_df$Lrvm <- 0
zonelayer_df[zonelayer_df$layer == 1, ]$Lrvm <- unlist(lapply(zone_df$crop_type, function(x) {
  get_var(plant_data$crop_pars_df, "Rt_CLrvm1", x)
}))
zonelayer_df[zonelayer_df$layer == 2, ]$Lrvm <- unlist(lapply(zone_df$crop_type, function(x) {
  get_var(plant_data$crop_pars_df, "Rt_CLrvm2", x)
}))
zonelayer_df[zonelayer_df$layer == 3, ]$Lrvm <- unlist(lapply(zone_df$crop_type, function(x) {
  get_var(plant_data$crop_pars_df, "Rt_CLrvm3", x)
}))
zonelayer_df[zonelayer_df$layer == 4, ]$Lrvm <- unlist(lapply(zone_df$crop_type, function(x) {
  get_var(plant_data$crop_pars_df, "Rt_CLrvm4", x)
}))

# Rt_CLrvM1Curr[Zone] = Cq_ParametersCurr[Zone,Lrvm1]*Rt_CMultiplier
# Rt_CLrvM2Curr[Zone] = Cq_ParametersCurr[Zone,Lrvm2]*Rt_CMultiplier
# Rt_CLrvM3Curr[Zone] = Cq_ParametersCurr[Zone,Lrvm3]*Rt_CMultiplier
# Rt_CLrvM4Curr[Zone] = Cq_ParametersCurr[Zone,Lrvm4]*Rt_CMultiplier
zonelayer_df$Rt_CLrvMCurr <- zonelayer_df$Lrvm * pars$Rt_par$Rt_CMultiplier

zone_df$Rt_CLrvt <- unlist(lapply(zone_df$Cq_Stage, function(x) {
  get_graph_y(pars$Rt_par$Rt_CLrvt_df, x, mode = "continues")
}))

zonelayer_df$Rt_CLrvt <- rep(zone_df$Rt_CLrvt, nlayer)


zone_df$Rt_ACType <- pars$Rt_par$Rt_ACType

# Rt_CLraC[Zone] = Cq_ParametersCurr[Zone,LraConst]
zone_df$Rt_CLraC <- unlist(lapply(zone_df$crop_type, function(x) {
  get_var(plant_data$crop_pars_df, "Rt_CLraConst", x)
}))



C_Root_DW_zone_df <- aggregate(list(C_Root_sum = zonelayerpcomp_df[zonelayerpcomp_df$PlantComp == "DW", "C_Root"]), by = zonelayer_df[c("zone")], sum)
zone_df$C_Root_DW_zone <- C_Root_DW_zone_df$C_Root_sum

# Rt_CLraDyn[Zone] = 1000*ARRAYSUM(C_Root_DW[Zone,*])*Rt_CSRLCurr[Zone]
zone_df$Rt_CLraDyn <- 1000 * zone_df$C_Root_DW_zone * zone_df$Rt_CSRLCurr

# Rt_CLraCurr[Zone] = IF(Rt_ACType=1)THEN(Rt_CLraC[Zone])ELSE(IF(Rt_ACType>1)THEN(Rt_CLraDyn[Zone])ELSE(0))
zone_df$Rt_CLraCurr <- ifelse(
  zone_df$Rt_ACType == 1,
  zone_df$Rt_CLraC,
  ifelse(zone_df$Rt_ACType > 1, zone_df$Rt_CLraDyn, 0)
)


zonelayer_df$RT_CDecDepthAct <- rep(zone_df$RT_CDecDepthAct, nlayer)
zonelayer_df$Rt_CLraCurr <- rep(zone_df$Rt_CLraCurr, nlayer)
# zonelayer_df$Rt_Depth <- rep(layer_df$Rt_Depth, each = nzone)
# zonelayer_df$Rt_Depth_down <- rep(c(0, head(layer_df$Rt_Depth, -1)), each = nzone)
zonelayer_df$Rt_Depth_up <- 0
zonelayer_df[zonelayer_df$layer %in% 2:4, ]$Rt_Depth_up <- zonelayer_df[zonelayer_df$layer %in% 1:3, ]$Rt_Depth


# Rt_CLrvExp1[Zone] = Rt_CLraCurr[Zone]*RT_CDecDepthAct[Zone]/((Rt_CLrvPlatDep+1)*EXP(-RT_CDecDepthAct[Zone]*Rt_CLrvPlatDep))*EXP(-RT_CDecDepthAct[Zone]*MAX(Rt_CLrvPlatDep,(Rt_Depth1[Zone]/2)))/100
# Rt_CLrvExp2[Zone] = Rt_CLraCurr[Zone]*RT_CDecDepthAct[Zone]/((Rt_CLrvPlatDep+1)*EXP(-RT_CDecDepthAct[Zone]*Rt_CLrvPlatDep))*EXP(-RT_CDecDepthAct[Zone]*MAX(Rt_CLrvPlatDep,(Rt_Depth1[Zone]+Rt_Depth2[Zone]/2)))/100
# Rt_CLrvExp3[Zone] = Rt_CLraCurr[Zone]*RT_CDecDepthAct[Zone]/((Rt_CLrvPlatDep+1)*EXP(-RT_CDecDepthAct[Zone]*Rt_CLrvPlatDep))*EXP(-RT_CDecDepthAct[Zone]*MAX(Rt_CLrvPlatDep,(Rt_Depth2[Zone]+Rt_Depth3[Zone]/2)))/100
# Rt_CLrvExp4[Zone] = Rt_CLraCurr[Zone]*RT_CDecDepthAct[Zone]/((Rt_CLrvPlatDep+1)*EXP(-RT_CDecDepthAct[Zone]*Rt_CLrvPlatDep))*EXP(-RT_CDecDepthAct[Zone]*MAX(Rt_CLrvPlatDep,(Rt_Depth3[Zone]+Rt_Depth4[Zone]/2)))/100

zonelayer_df$Rt_CLrvExp <- zonelayer_df$Rt_CLraCurr * zonelayer_df$RT_CDecDepthAct /
  ((pars$Rt_par$Rt_CLrvPlatDep + 1) * exp(-zonelayer_df$RT_CDecDepthAct * pars$Rt_par$Rt_CLrvPlatDep)
  ) *
  exp(-zonelayer_df$RT_CDecDepthAct * pmax(pars$Rt_par$Rt_CLrvPlatDep, ((zonelayer_df$Rt_Depth +
                                                                           zonelayer_df$Rt_Depth_up) / 2
  ))) / 100



# Rt_CLrvMinM1[Zone] = IF(Rt_ACType=0)THEN(Rt_CLrvM1Curr[Zone]*Rt_CLrvt[Zone])ELSE IF(Rt_ACType=1) THEN (Rt_CLrvExp1[Zone]*Rt_CLrvt[Zone])ELSE (Rt_CLrvExp1[Zone])
# Rt_CLrvMinM2[Zone] = IF(Rt_ACType=0)THEN(Rt_CLrvM2Curr[Zone]*Rt_CLrvt[Zone])ELSE IF(Rt_ACType=1) THEN (Rt_CLrvExp2[Zone]*Rt_CLrvt[Zone])ELSE (Rt_CLrvExp2[Zone])
# Rt_CLrvMinM3[Zone] = IF(Rt_ACType=0)THEN(Rt_CLrvM3Curr[Zone]*Rt_CLrvt[Zone])ELSE IF(Rt_ACType=1) THEN (Rt_CLrvExp3[Zone]*Rt_CLrvt[Zone])ELSE (Rt_CLrvExp3[Zone])
# Rt_CLrvMinM4[Zone] = IF(Rt_ACType=0)THEN(Rt_CLrvM4Curr[Zone]*Rt_CLrvt[Zone])ELSE IF(Rt_ACType=1) THEN (Rt_CLrvExp4[Zone]*Rt_CLrvt[Zone])ELSE (Rt_CLrvExp4[Zone])
zonelayer_df$Rt_ACType <- pars$Rt_par$Rt_ACType

zonelayer_df$Rt_CLrvMinM <- ifelse(
  zonelayer_df$Rt_ACType == 0,
  zonelayer_df$Rt_CLrvMCurr * zonelayer_df$Rt_CLrvt,
  ifelse(
    zonelayer_df$Rt_ACType == 1 ,
    zonelayer_df$Rt_CLrvExp * zonelayer_df$Rt_CLrvt,
    zonelayer_df$Rt_CLrvExp
  )
)


zonelayer_df$Cq_MaxMycInf <- rep(zone_df$Cq_MaxMycInf, nlayer)
zonelayer_df$Cq_RtDiam <- rep(zone_df$Cq_RtDiam, nlayer)
zonelayer_df$Rt_MCInfFrac <- rep(layer_df$Rt_MCInfFrac, each = nzone)


# Rt_CLrvM1[Zone] = Rt_CLrvMinM1[Zone]*(1+Cq_MaxMycInf[Zone]*Rt_MCInfFrac1*Rt_MCHypL*SQRT(Rt_MCHypDiam)/SQRT(Cq_RtDiam[Zone]))
# Rt_CLrvM2[Zone] = Rt_CLrvMinM2[Zone]*(1+Cq_MaxMycInf[Zone]*Rt_MCInfFrac2*Rt_MCHypL*SQRT(Rt_MCHypDiam)/SQRT(Cq_RtDiam[Zone]))
# Rt_CLrvM3[Zone] = Rt_CLrvMinM3[Zone]*(1+Cq_MaxMycInf[Zone]*Rt_MCInfFrac3*Rt_MCHypL*SQRT(Rt_MCHypDiam)/SQRT(Cq_RtDiam[Zone]))
# Rt_CLrvM4[Zone] = Rt_CLrvMinM4[Zone]*(1+Cq_MaxMycInf[Zone]*Rt_MCInfFrac4*Rt_MCHypL*SQRT(Rt_MCHypDiam)/SQRT(Cq_RtDiam[Zone]))
zonelayer_df$Rt_CLrvM <- zonelayer_df$Rt_CLrvMinM * (
  1 + zonelayer_df$Cq_MaxMycInf * zonelayer_df$Rt_MCInfFrac *
    pars$Rt_par$Rt_MCHypL *
    sqrt(pars$Rt_par$Rt_MCHypDiam) / sqrt(zonelayer_df$Cq_RtDiam)
)

zone_df$Rt_TLrvM1 <- zonelayertree_df[zonelayertree_df$layer == 1 &
                                        zonelayertree_df$tree_id == 1, ]$Rt_TLrvM
zone_df$Rt_TLrvM2 <- zonelayertree_df[zonelayertree_df$layer == 2 &
                                        zonelayertree_df$tree_id == 1, ]$Rt_TLrvM
zone_df$Rt_CLrvM1 <- zonelayer_df[zonelayer_df$layer == 1, ]$Rt_CLrvM
zone_df$Rt_CLrvM2 <- zonelayer_df[zonelayer_df$layer == 2, ]$Rt_CLrvM

# Rt_CParasitFrac[Zone] = min(1,Cq_C_HostEffForT1[Zone]*(Rt_TLrvM1[Zone,Sp1]*Rt_CLrvM1[Zone]+Rt_TLrvM2[Zone,Sp1]*Rt_CLrvM2[Zone]))
zone_df$Rt_CParasitFrac <- min(
  1,
  zone_df$Cq_C_HostEffForT1 * (
    zone_df$Rt_TLrvM1 * zone_df$Rt_CLrvM1 + zone_df$Rt_TLrvM2 * zone_df$Rt_CLrvM2
  )
)



zonetree_df$Rt_T_HostEffForT1 <- rep(tree_df$Rt_T_HostEffForT1, each = nzone)

zonetree_df$Rt_TLrvM1 <- zonelayertree_df[zonelayertree_df$layer == 1, ]$Rt_TLrvM
zonetree_df$Rt_TLrvM2 <- zonelayertree_df[zonelayertree_df$layer == 2, ]$Rt_TLrvM

# Rt_TParasitFrac[Zone,Tree] = min(1,Rt_T_HostEffForT1[Tree]*(Rt_TLrvM1[Zone,Tree]*Rt_TLrvM1[Zone,Tree]+Rt_TLrvM2[Zone,Tree]*Rt_TLrvM2[Zone,Tree]))
zonetree_df$Rt_TParasitFrac <- min(
  1,
  zonetree_df$Rt_T_HostEffForT1 *
    (zonetree_df$Rt_TLrvM1^2 +
       zonetree_df$Rt_TLrvM2^2)
)


# Rt_TLrv1[Zn1,Sp1] = Rt_TLrvM1[Zn1,Sp1]+Rt_CParasitFrac[Zn1]*Rt_CLrvM1[Zn1]+Rt_TParasitFrac[Zn1,Sp2]*Rt_TLrvM1[Zn1,Sp2]+Rt_TParasitFrac[Zn1,Sp3]*Rt_TLrvM1[Zn1,Sp3]
# Rt_TLrv1[Zn1,Sp2] = Rt_TLrvM1[Zn1,Sp2]*(1-Rt_TParasitFrac[Zn1,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM1[Zn1])
# Rt_TLrv1[Zn1,Sp3] = Rt_TLrvM1[Zn1,Sp3]*(1-Rt_TParasitFrac[Zn1,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM1[Zn1])
# Rt_TLrv1[Zn2,Sp1] = Rt_TLrvM1[Zn2,Sp1]+Rt_TParasitFrac[Zn2,Sp2]*Rt_TLrvM1[Zn2,Sp2]+Rt_TParasitFrac[Zn2,Sp3]*Rt_TLrvM1[Zn2,Sp3]+Rt_CParasitFrac[Zn2]*Rt_CLrvM1[Zn2]
# Rt_TLrv1[Zn2,Sp2] = Rt_TLrvM1[Zn2,Sp2]*(1-Rt_TParasitFrac[Zn2,Sp2])+0*(Rt_CParasitFrac[Zn2]+Rt_CLrvM1[Zn2])
# Rt_TLrv1[Zn2,Sp3] = Rt_TLrvM1[Zn2,Sp3]*(1-Rt_TParasitFrac[Zn2,Sp3])+0*(Rt_CParasitFrac[Zn2]+Rt_CLrvM1[Zn2])
# Rt_TLrv1[Zn3,Sp1] = Rt_TLrvM1[Zn3,Sp1]+Rt_TParasitFrac[Zn3,Sp2]*Rt_TLrvM1[Zn3,Sp2]+Rt_TParasitFrac[Zn3,Sp3]*Rt_TLrvM1[Zn3,Sp3]+Rt_CParasitFrac[Zn3]*Rt_CLrvM1[Zn3]
# Rt_TLrv1[Zn3,Sp2] = Rt_TLrvM1[Zn3,Sp2]*(1-Rt_TParasitFrac[Zn3,Sp2])+0*(Rt_CParasitFrac[Zn3]+Rt_CLrvM1[Zn3])
# Rt_TLrv1[Zn3,Sp3] = Rt_TLrvM1[Zn3,Sp3]*(1-Rt_TParasitFrac[Zn3,Sp3])+0*(Rt_CParasitFrac[Zn3]+Rt_CLrvM1[Zn3])
# Rt_TLrv1[Zn4,Sp1] = Rt_TLrvM1[Zn4,Sp1]+Rt_TParasitFrac[Zn4,Sp2]*Rt_TLrvM1[Zn4,Sp2]+ Rt_TParasitFrac[Zn4,Sp3]*Rt_TLrvM1[Zn4,Sp3]+Rt_CParasitFrac[Zn4]*Rt_CLrvM1[Zn4]
# Rt_TLrv1[Zn4,Sp2] = Rt_TLrvM1[Zn4,Sp2]*(1-Rt_TParasitFrac[Zn4,Sp2])+0*(Rt_CParasitFrac[Zn4]+Rt_CLrvM1[Zn4])
# Rt_TLrv1[Zn4,Sp3] = Rt_TLrvM1[Zn4,Sp3]*(1-Rt_TParasitFrac[Zn4,Sp3])+0*(Rt_CParasitFrac[Zn4]+Rt_CLrvM1[Zn4])
# Rt_TLrv2[Zn1,Sp1] = Rt_TLrvM2[Zn1,Sp1]+Rt_CParasitFrac[Zn1]*Rt_CLrvM2[Zn1]+Rt_TParasitFrac[Zn1,Sp2]*Rt_TLrvM2[Zn1,Sp2]+Rt_TParasitFrac[Zn1,Sp3]*Rt_TLrvM2[Zn1,Sp3]
# Rt_TLrv2[Zn1,Sp2] = Rt_TLrvM2[Zn1,Sp2]*(1-Rt_TParasitFrac[Zn1,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM2[Zn1])
# Rt_TLrv2[Zn1,Sp3] = Rt_TLrvM2[Zn1,Sp3]*(1-Rt_TParasitFrac[Zn1,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM2[Zn1])
# Rt_TLrv2[Zn2,Sp1] = Rt_TLrvM2[Zn2,Sp1]+Rt_CParasitFrac[Zn2]*Rt_CLrvM2[Zn2]+Rt_TParasitFrac[Zn2,Sp2]*Rt_TLrvM2[Zn2,Sp2]+Rt_TParasitFrac[Zn2,Sp3]*Rt_TLrvM2[Zn2,Sp3]
# Rt_TLrv2[Zn2,Sp2] = Rt_TLrvM2[Zn2,Sp2]*(1-Rt_TParasitFrac[Zn2,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM2[Zn1])
# Rt_TLrv2[Zn2,Sp3] = Rt_TLrvM2[Zn2,Sp3]*(1-Rt_TParasitFrac[Zn2,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM2[Zn1])
# Rt_TLrv2[Zn3,Sp1] = Rt_TLrvM2[Zn3,Sp1]+Rt_CParasitFrac[Zn3]*Rt_CLrvM2[Zn3]+Rt_TParasitFrac[Zn3,Sp2]*Rt_TLrvM2[Zn3,Sp2]+Rt_TParasitFrac[Zn3,Sp3]*Rt_TLrvM2[Zn3,Sp3]
# Rt_TLrv2[Zn3,Sp2] = Rt_TLrvM2[Zn3,Sp2]*(1-Rt_TParasitFrac[Zn3,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM2[Zn1])
# Rt_TLrv2[Zn3,Sp3] = Rt_TLrvM2[Zn3,Sp3]*(1-Rt_TParasitFrac[Zn3,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM2[Zn1])
# Rt_TLrv2[Zn4,Sp1] = Rt_TLrvM2[Zn4,Sp1]+Rt_CParasitFrac[Zn4]*Rt_CLrvM2[Zn4]+Rt_TParasitFrac[Zn4,Sp2]*Rt_TLrvM2[Zn4,Sp2]+Rt_TParasitFrac[Zn4,Sp3]*Rt_TLrvM2[Zn4,Sp3]
# Rt_TLrv2[Zn4,Sp2] = Rt_TLrvM2[Zn4,Sp2]*(1-Rt_TParasitFrac[Zn4,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM2[Zn1])
# Rt_TLrv2[Zn4,Sp3] = Rt_TLrvM2[Zn4,Sp3]*(1-Rt_TParasitFrac[Zn4,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM2[Zn1])
# Rt_TLrv3[Zn1,Sp1] = Rt_TLrvM3[Zn1,Sp1]+Rt_CParasitFrac[Zn1]*Rt_CLrvM3[Zn1]+Rt_TParasitFrac[Zn1,Sp2]*Rt_TLrvM3[Zn1,Sp2]+Rt_TParasitFrac[Zn1,Sp3]*Rt_TLrvM3[Zn1,Sp3]
# Rt_TLrv3[Zn1,Sp2] = Rt_TLrvM3[Zn1,Sp2]*(1-Rt_TParasitFrac[Zn1,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM3[Zn1])
# Rt_TLrv3[Zn1,Sp3] = Rt_TLrvM3[Zn1,Sp3]*(1-Rt_TParasitFrac[Zn1,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM3[Zn1])
# Rt_TLrv3[Zn2,Sp1] = Rt_TLrvM3[Zn2,Sp1]+Rt_CParasitFrac[Zn2]*Rt_CLrvM3[Zn2]+Rt_TParasitFrac[Zn2,Sp2]*Rt_TLrvM3[Zn2,Sp2]+Rt_TParasitFrac[Zn2,Sp3]*Rt_TLrvM3[Zn2,Sp3]
# Rt_TLrv3[Zn2,Sp2] = Rt_TLrvM3[Zn2,Sp2]*(1-Rt_TParasitFrac[Zn2,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM3[Zn1])
# Rt_TLrv3[Zn2,Sp3] = Rt_TLrvM3[Zn2,Sp3]*(1-Rt_TParasitFrac[Zn2,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM3[Zn1])
# Rt_TLrv3[Zn3,Sp1] = Rt_TLrvM3[Zn3,Sp1]+Rt_CParasitFrac[Zn3]*Rt_CLrvM3[Zn3]+Rt_TParasitFrac[Zn3,Sp2]*Rt_TLrvM3[Zn3,Sp2]+Rt_TParasitFrac[Zn3,Sp3]*Rt_TLrvM3[Zn3,Sp3]
# Rt_TLrv3[Zn3,Sp2] = Rt_TLrvM3[Zn3,Sp2]*(1-Rt_TParasitFrac[Zn3,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM3[Zn1])
# Rt_TLrv3[Zn3,Sp3] = Rt_TLrvM3[Zn3,Sp3]*(1-Rt_TParasitFrac[Zn3,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM3[Zn1])
# Rt_TLrv3[Zn4,Sp1] = Rt_TLrvM3[Zn4,Sp1]+Rt_CParasitFrac[Zn4]*Rt_CLrvM3[Zn4]+Rt_TParasitFrac[Zn4,Sp2]*Rt_TLrvM3[Zn4,Sp2]+Rt_TParasitFrac[Zn4,Sp3]*Rt_TLrvM3[Zn4,Sp3]
# Rt_TLrv3[Zn4,Sp2] = Rt_TLrvM3[Zn4,Sp2]*(1-Rt_TParasitFrac[Zn4,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM3[Zn1])
# Rt_TLrv3[Zn4,Sp3] = Rt_TLrvM3[Zn4,Sp3]*(1-Rt_TParasitFrac[Zn4,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM3[Zn1])
# Rt_TLrv4[Zn1,Sp1] = Rt_TLrvM4[Zn1,Sp1]+Rt_CParasitFrac[Zn1]*Rt_CLrvM4[Zn1]+Rt_TParasitFrac[Zn1,Sp2]*Rt_TLrvM4[Zn1,Sp2]+Rt_TParasitFrac[Zn1,Sp3]*Rt_TLrvM4[Zn1,Sp3]
# Rt_TLrv4[Zn1,Sp2] = Rt_TLrvM4[Zn1,Sp2]*(1-Rt_TParasitFrac[Zn1,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM4[Zn1])
# Rt_TLrv4[Zn1,Sp3] = Rt_TLrvM4[Zn1,Sp3]*(1-Rt_TParasitFrac[Zn1,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM4[Zn1])
# Rt_TLrv4[Zn2,Sp1] = Rt_TLrvM4[Zn2,Sp1]+Rt_CParasitFrac[Zn2]*Rt_CLrvM4[Zn2]+Rt_TParasitFrac[Zn2,Sp2]*Rt_TLrvM4[Zn2,Sp2]+Rt_TParasitFrac[Zn2,Sp3]*Rt_TLrvM4[Zn2,Sp3]
# Rt_TLrv4[Zn2,Sp2] = Rt_TLrvM4[Zn2,Sp2]*(1-Rt_TParasitFrac[Zn2,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM4[Zn1])
# Rt_TLrv4[Zn2,Sp3] = Rt_TLrvM4[Zn2,Sp3]*(1-Rt_TParasitFrac[Zn2,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM4[Zn1])
# Rt_TLrv4[Zn3,Sp1] = Rt_TLrvM4[Zn3,Sp1]+Rt_CParasitFrac[Zn3]*Rt_CLrvM4[Zn3]+Rt_TParasitFrac[Zn3,Sp2]*Rt_TLrvM4[Zn3,Sp2]+Rt_TParasitFrac[Zn3,Sp3]*Rt_TLrvM4[Zn3,Sp3]
# Rt_TLrv4[Zn3,Sp2] = Rt_TLrvM4[Zn3,Sp2]*(1-Rt_TParasitFrac[Zn3,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM4[Zn1])
# Rt_TLrv4[Zn3,Sp3] = Rt_TLrvM4[Zn3,Sp3]*(1-Rt_TParasitFrac[Zn3,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM4[Zn1])
# Rt_TLrv4[Zn4,Sp1] = Rt_TLrvM4[Zn4,Sp1]+Rt_CParasitFrac[Zn4]*Rt_CLrvM4[Zn4]+Rt_TParasitFrac[Zn4,Sp2]*Rt_TLrvM4[Zn4,Sp2]+Rt_TParasitFrac[Zn4,Sp3]*Rt_TLrvM4[Zn4,Sp3]
# Rt_TLrv4[Zn4,Sp2] = Rt_TLrvM4[Zn4,Sp2]*(1-Rt_TParasitFrac[Zn4,Sp2])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM4[Zn1])
# Rt_TLrv4[Zn4,Sp3] = Rt_TLrvM4[Zn4,Sp3]*(1-Rt_TParasitFrac[Zn4,Sp3])+0*(Rt_CParasitFrac[Zn1]+Rt_CLrvM4[Zn1])

zonelayertree_df$Rt_CParasitFrac <- rep(zone_df$Rt_CParasitFrac, nrow(zone_df) * nrow(tree_df))

rt_df <- zonetree_df[c("zone", "tree_id", "Rt_TParasitFrac")]
rt2_df <- rt_df[rep(row.names(rt_df), times = rep(nlayer, nrow(rt_df))), ]
rt2_df$layer <- rep(c(1:nlayer), nrow(rt_df))
rt2_df <- rt2_df[order(rt2_df$tree_id, rt2_df$layer, rt2_df$zone), ]
zonelayertree_df$Rt_TParasitFrac <- rt2_df$Rt_TParasitFrac

zonelayertree_df$Rt_TLrv <- zonelayertree_df$Rt_TLrvM * (1 - zonelayertree_df$Rt_TParasitFrac)
sp1_df <- zonelayertree_df[zonelayertree_df$tree_id == 1, ]
sp2_df <- zonelayertree_df[zonelayertree_df$tree_id == 2, ]
sp3_df <- zonelayertree_df[zonelayertree_df$tree_id == 3, ]
zonelayertree_df[zonelayertree_df$tree_id == 1, ]$Rt_TLrv <- sp1_df$Rt_TLrvM * sp1_df$Rt_CParasitFrac + sp2_df$Rt_TLrvM * sp2_df$Rt_TParasitFrac + sp3_df$Rt_TLrvM * sp3_df$Rt_TParasitFrac



# TW_Condutc1[Zone,Tree] = AF_ZoneFrac[Zone]*AF_DepthAct1[Zone]*Rt_TLrv1[Zone,Tree]
# TW_Condutc2[Zone,Tree] = AF_ZoneFrac[Zone]*AF_Depth2[Zone]*Rt_TLrv2[Zone,Tree]
# TW_Condutc3[Zone,Tree] = AF_ZoneFrac[Zone]*AF_Depth3[Zone]*Rt_TLrv3[Zone,Tree]
# TW_Condutc4[Zone,Tree] = AF_ZoneFrac[Zone]*AF_Depth4[Zone]*Rt_TLrv4[Zone,Tree]
zonelayertree_df$AF_ZoneFrac <- rep(zone_df$AF_ZoneFrac, nrow(zone_df) * nrow(tree_df))
zonelayertree_df$AF_Depth <- rep(zonelayer_df$AF_Depth, nrow(tree_df))
zonelayertree_df$TW_Condutc <- zonelayertree_df$AF_ZoneFrac * zonelayertree_df$AF_Depth *
  zonelayertree_df$Rt_TLrv

# TW_ConductanceSum[Tree] = ARRAYSUM(TW_Condutc1[*,Tree])+ARRAYSUM(TW_Condutc2[*,Tree])+ARRAYSUM(TW_Condutc3[*,Tree])+ARRAYSUM(TW_Condutc4[*,Tree])
tree_df$TW_ConductanceSum <- aggregate(zonelayertree_df["TW_Condutc"], by = zonelayertree_df["tree_id"], sum)$TW_Condutc

zonelayer_df$W_PTheta <- NA
for (i in 1:nrow(zonelayer_df)) {
  zonelayer_df[i, ]$W_PTheta <- get_graph_y(
    W_PTheta_df,
    zonelayer_df[i, ]$W_Theta,
    x_column = "Theta",
    y_column = names(W_PTheta_df)[zonelayer_df[i, ]$layer],
    mode = "continues"
  )
}

# TW_PStem1[Tree] = W_PTheta1[Zn1]*TW_Condutc1[Zn1,Tree]+ W_PTheta1[Zn2]*TW_Condutc1[Zn2,Tree]+ W_PTheta1[Zn3]*TW_Condutc1[Zn3,Tree]+ W_PTheta1[Zn4]*TW_Condutc1[Zn4,Tree]
# TW_PStem2[Tree] = W_PTheta2[Zn1]*TW_Condutc2[Zn1,Tree]+ W_PTheta2[Zn2]*TW_Condutc2[Zn2,Tree]+ W_PTheta2[Zn3]*TW_Condutc2[Zn3,Tree]+ W_PTheta2[Zn4]*TW_Condutc2[Zn4,Tree]
# TW_PStem3[Tree] = W_PTheta3[Zn1]*TW_Condutc3[Zn1,Tree]+ W_PTheta3[Zn2]*TW_Condutc3[Zn2,Tree]+ W_PTheta3[Zn3]*TW_Condutc3[Zn3,Tree]+ W_PTheta3[Zn4]*TW_Condutc3[Zn4,Tree]
# TW_PStem4[Tree] = W_PTheta4[Zn1]*TW_Condutc4[Zn1,Tree]+ W_PTheta4[Zn2]*TW_Condutc4[Zn2,Tree]+ W_PTheta4[Zn3]*TW_Condutc4[Zn3,Tree]+ W_PTheta4[Zn4]*TW_Condutc4[Zn4,Tree]
zonelayertree_df$W_PTheta <- rep(zonelayer_df$W_PTheta, ntree)
zonelayertree_df$TW_PStem <- zonelayertree_df$W_PTheta * zonelayertree_df$TW_Condutc

# TW_PStemSum[Tree] = TW_PStem1[Tree]+TW_PStem2[Tree]+TW_PStem3[Tree]+TW_PStem4[Tree]
tree_df$TW_PStemSum <- aggregate(zonelayertree_df["TW_PStem"], by = zonelayertree_df["tree_id"], sum)$TW_PStem

# TW_PStem[Tree] = if TW_ConductanceSum[Tree] > 0 then TW_PStemSum[Tree]/TW_ConductanceSum[Tree] else 0
tree_df$TW_PStem <- ifelse(tree_df$TW_ConductanceSum > 0,
                           tree_df$TW_PStemSum / tree_df$TW_ConductanceSum,
                           0)


zonelayertree_df$W_ThetaSat <- rep(rep(layer_df$W_ThetaSat, each = nrow(zone_df)), nrow(tree_df))
zonelayertree_df$W_Alpha <- rep(rep(layer_df$W_Alpha, each = nrow(zone_df)), nrow(tree_df))
zonelayertree_df$W_n <- rep(rep(layer_df$W_n, each = nrow(zone_df)), nrow(tree_df))
zonelayertree_df$TW_PStem <- rep(tree_df$TW_PStem, each = nrow(zone_df) * nrow(layer_df))

# TW_EqTheta1[Zone,Tree] = W_ThetaSat1/(1+(ABS(W_Alpha1*TW_PStem[Tree]))^W_n1)^(1-1/W_n1)
# TW_EqTheta2[Zone,Tree] = W_ThetaSat2/(1+(ABS(W_Alpha2*TW_PStem[Tree]))^W_n2)^(1-1/W_n2)
# TW_EqTheta3[Zone,Tree] = W_ThetaSat3/(1+(ABS(W_Alpha3*TW_PStem[Tree]))^W_n3)^(1-1/W_n3)
# TW_EqTheta4[Zone,Tree] = W_ThetaSat4/(1+(ABS(W_Alpha4*TW_PStem[Tree]))^W_n4)^(1-1/W_n4)
zonelayertree_df$TW_EqTheta <- zonelayertree_df$W_ThetaSat / (1 + (abs(
  zonelayertree_df$W_Alpha * zonelayertree_df$TW_PStem
))^zonelayertree_df$W_n)^(1 - 1 / zonelayertree_df$W_n)



# W_TCW_Constant = W_Hyd?*W_HydEqFraction
W_TCW_Constant <- pars$W_par$W_Hyd_is * pars$W_par$W_HydEqFraction


# TW_PotSuctHalf[Tree] = -((TW_PotSuctAlphMax[Tree]*TW_PotSuctAlphMin[Tree])^0.5)
tree_df$TW_PotSuctHalf <- -((tree_df$TW_PotSuctAlphMax * tree_df$TW_PotSuctAlphMin)^0.5)




# Rain_DoY = IF (Rain_AType=1 AND Rain_Cycle?= 0) THEN (TIME+Ca_DOYStart-365*Rain_YearStart) ELSE
# if (MOD(TIME+Ca_DOYStart-365*Rain_YearStart,365)) = 0 then 365 else (MOD(TIME+Ca_DOYStart-365*Rain_YearStart,366))
Rain_DoY <- ifelse (
  pars$Rain_par$Rain_AType == 1 &
    pars$Rain_par$Rain_Cycle_is == 0,
  time + Ca_DOYStart - 365 * Rain_YearStart,
  ifelse (
    (time + Ca_DOYStart - 365 * pars$Rain_par$Rain_YearStart %% 365) == 0 ,
    365,
    time + Ca_DOYStart - 365 * pars$Rain_par$Rain_YearStart %% 366
  )
)



# Evap_SumAirTemp[Calender] = (Evap_MonthlyMean_AirTemp[Calender]/5)^1.514
calendar_df$Evap_SumAirTemp <- (calendar_df$Evap_MonthlyMean_AirTemp /
                                  5)^1.514

# E_Heat_Index = ARRAYSUM(Evap_SumAirTemp[*])
E_Heat_Index <- sum(calendar_df$Evap_SumAirTemp)

# E_Alpha = (6.75*10^-7)*E_Heat_Index^3-(7.71*10^-5)*E_Heat_Index^2+(1.792*10^-2)*E_Heat_Index+0.49239
E_Alpha <- (6.75 * 10^-7) * E_Heat_Index^3 - (7.71 * 10^-5) * E_Heat_Index^2 +
  (1.792 * 10^-2) * E_Heat_Index + 0.49239

# Evap_Monthly_PotThornthwaite[Calender] = 1.6*(Evap_MonthlyMean_DayLength[Calender]/12)*(Rain_Numberof_DaysperMonth[Calender]/30)*(10*Evap_MonthlyMean_AirTemp[Calender]/E_Heat_Index)^E_Alpha
calendar_df$Evap_Monthly_PotThornthwaite <- 1.6 * (calendar_df$Evap_MonthlyMean_DayLength /
                                                     12) * (calendar_df$Rain_Numberof_DaysperMonth / 30) * (10 * calendar_df$Evap_MonthlyMean_AirTemp /
                                                                                                              E_Heat_Index)^E_Alpha

# Evap_DailyPot_Thornthwaite = 10*(if Rain_DoY <= 31 then Evap_Monthly_PotThornthwaite[january]/Rain_Numberof_DaysperMonth[january] else
#   if Rain_DoY <= 59 then Evap_Monthly_PotThornthwaite[february]/Rain_Numberof_DaysperMonth[february] else
# if Rain_DoY <= 90 then Evap_Monthly_PotThornthwaite[march]/Rain_Numberof_DaysperMonth[march] else
# if Rain_DoY <= 120 then Evap_Monthly_PotThornthwaite[april]/Rain_Numberof_DaysperMonth[april] else
#   if Rain_DoY <= 151 then Evap_Monthly_PotThornthwaite[may]/Rain_Numberof_DaysperMonth[may] else
# if Rain_DoY <= 181 then Evap_Monthly_PotThornthwaite[june]/Rain_Numberof_DaysperMonth[june] else
# if Rain_DoY <= 212 then Evap_Monthly_PotThornthwaite[july]/Rain_Numberof_DaysperMonth[july] else
#   if Rain_DoY <= 243 then Evap_Monthly_PotThornthwaite[august]/Rain_Numberof_DaysperMonth[august] else
# if Rain_DoY <= 273 then Evap_Monthly_PotThornthwaite[september]/Rain_Numberof_DaysperMonth[september] else
# if Rain_DoY <= 304 then Evap_Monthly_PotThornthwaite[october]/Rain_Numberof_DaysperMonth[october] else
#   if Rain_DoY <= 334 then Evap_Monthly_PotThornthwaite[november]/Rain_Numberof_DaysperMonth[november] else Evap_Monthly_PotThornthwaite[december]/Rain_Numberof_DaysperMonth[december])

i_month <- min(max(which(
  c(0, calendar_df$cumsum_days) < Rain_DoY, arr.ind = TRUE
)), 12)
cdf <- calendar_df[i_month, ]
Evap_DailyPot_Thornthwaite <- 10 * cdf$Evap_Monthly_PotThornthwaite /
  cdf$Rain_Numberof_DaysperMonth

### TODO: EDIT ###################

Temp_DailyPotEvap <- get_graph_y(weather_df, time %% 365, y_column = "Temp_DailyPotEvap", mode = "continues")

# Evap_Pot = if Temp_PotEvapConst? = 1 then Temp_PotEvapConst else if Evap_Pot_Thornthwaite? = 2 then Evap_DailyPot_Thornthwaite else Temp_DailyPotEvap
Evap_Pot <- ifelse(
  pars$Temp_par$Temp_PotEvapConst_is == 1,
  pars$Temp_par$Temp_PotEvapConst,
  ifelse(
    pars$Evap_par$Evap_Pot_Thornthwaite_is == 2,
    Evap_DailyPot_Thornthwaite,
    Temp_DailyPotEvap
  )
)


# Rain_InterceptEvap[Zone] = min(Evap_Pot,Rain_CanopyWater[Zone])
zone_df$Rain_InterceptEvap <- pmin(Evap_Pot, zone_df$Rain_CanopyWater)

# Rain_InterceptEvapAvg = AF_ZoneFrac[Zn1]*Rain_InterceptEvap[Zn1]+AF_ZoneFrac[Zn2]*Rain_InterceptEvap[Zn2]+AF_ZoneFrac[Zn3]*Rain_InterceptEvap[Zn3]+AF_ZoneFrac[Zn4]*Rain_InterceptEvap[Zn4]
Rain_InterceptEvapAvg <- sum(zone_df$AF_ZoneFrac * zone_df$Rain_InterceptEvap)

# Evap_EpotDemandNotMetBy_CanInterc = Evap_Pot-Rain_InterceptEvapAvg*Evap_TranspRedFractrionBy_Can_Intercepted_Water
Evap_EpotDemandNotMetBy_CanInterc <- Evap_Pot - Rain_InterceptEvapAvg * pars$Evap_par$Evap_TranspRedFractrionBy_Can_Intercepted_Water



# T_LAICan[Tree] = IF(T_CanWidthRel[Tree]>0)THEN(MAX(T_LAI[Tree]/T_CanWidthRel[Tree],0))ELSE(0)
tree_df$T_LAICan <- ifelse(tree_df$T_CanWidthRel > 0,
                           pmax(tree_df$T_LAI / tree_df$T_CanWidthRel, 0),
                           0)


zonetree_df$T_klight <- rep(tree_df$T_klight, each = nzone)
zonetree_df$T_LAICan <- rep(tree_df$T_LAICan, each = nzone)

# T_LAIEff[Zone,Tree] = IF T_klight[Tree]>0 AND (1-T_RelCanWidthZn[Zone,Tree]*(1-exp(-T_klight[Tree]*T_LAICan[Tree])))>0 THEN -LOGN(1-T_RelCanWidthZn[Zone,Tree]*(1-exp(-T_klight[Tree]*T_LAICan[Tree])))/T_klight[Tree] ELSE 0
zonetree_df$T_LAIEff_a <- 1 - zonetree_df$T_RelCanWidthZn * (1 - exp(-zonetree_df$T_klight * zonetree_df$T_LAICan))
zonetree_df$T_LAIEff <- ifelse(
  zonetree_df$T_klight > 0 &
    zonetree_df$T_LAIEff_a > 0,
  -log(zonetree_df$T_LAIEff_a) / zonetree_df$T_klight,
  0
)


# T_CanWidth[Tree] = T_CanWidthRel[Tree]*AF_ZoneTot
tree_df$T_CanWidth <- tree_df$T_CanWidthRel * pars$AF_par$AF_ZoneTot

# T_CanH[Tree] = if (T_CanShape[Tree])>0 THEN T_CanWidth[Tree]/T_CanShape[Tree] ELSE 0
tree_df$T_CanH <- ifelse(tree_df$T_CanShape > 0,
                         tree_df$T_CanWidth / tree_df$T_CanShape,
                         0)

# T_CanUp[Tree] = T_WoodH[Tree]+T_CanH[Tree]
tree_df$T_CanUp <- tree_df$T_WoodH + tree_df$T_CanH



# C_CanUp[Zone] = C_Height[Zone]
zone_df$C_CanUp <- zone_df$C_Height

# T_CanUpZn[Zone,Tree] = if T_LAIEff[Zone,Tree] > 0 then T_CanUp[Tree] else 0
zonetree_df$T_CanUp <- rep(tree_df$T_CanUp, each = nzone)
zonetree_df$T_CanUpZn <- ifelse(zonetree_df$T_LAIEff > 0, zonetree_df$T_CanUp, 0)

sp1_df <- zonetree_df[zonetree_df$tree_id == 1, ]
sp2_df <- zonetree_df[zonetree_df$tree_id == 2, ]
sp3_df <- zonetree_df[zonetree_df$tree_id == 3, ]
# L_Top[Zone] = MAX(C_CanUp[Zone],T_CanUpZn[Zone,Sp1],T_CanUpZn[Zone,Sp2],T_CanUpZn[Zone,Sp3])
zone_df$L_Top <- pmax(zone_df$C_CanUp,
                      sp1_df$T_CanUpZn,
                      sp2_df$T_CanUpZn,
                      sp3_df$T_CanUpZn)

# L_Bottom[Zone] = MIN(C_CanUp[Zone],T_CanUpZn[Zone,Sp1],T_CanUpZn[Zone,Sp2],T_CanUpZn[Zone,Sp3])
zone_df$L_Bottom <- pmin(zone_df$C_CanUp,
                         sp1_df$T_CanUpZn,
                         sp2_df$T_CanUpZn,
                         sp3_df$T_CanUpZn)

# L_Mid1[Zone] = IF (C_CanUp[Zone]-L_Bottom[Zone])>0.0001 AND (L_Top[Zone]-C_CanUp[Zone])>0.0001 THEN C_CanUp[Zone] ELSE IF (T_CanUpZn[Zone,Sp1]-L_Bottom[Zone])>0.0001 AND (L_Top[Zone]-T_CanUpZn[Zone,Sp1])>0.0001 THEN T_CanUpZn[Zone,Sp1] ELSE  IF (T_CanUpZn[Zone,Sp2]-L_Bottom[Zone])>0.0001 AND (L_Top[Zone]-T_CanUpZn[Zone,Sp2])>0.0001 THEN T_CanUpZn[Zone,Sp2] ELSE if (T_CanUpZn[Zone,Sp3]-L_Bottom[Zone])>0.0001 AND (L_Top[Zone]-T_CanUpZn[Zone,Sp3])>0.0001 THEN T_CanUpZn[Zone,Sp3] else 0
zone_df$L_Mid1 <- ifelse(
  (zone_df$C_CanUp - zone_df$L_Bottom) > 0.0001 &
    (zone_df$L_Top - zone_df$C_CanUp) > 0.0001,
  zone_df$C_CanUp,
  ifelse(
    (sp1_df$T_CanUpZn - zone_df$L_Bottom) > 0.0001 &
      (zone_df$L_Top - sp1_df$T_CanUpZn) > 0.0001,
    sp1_df$T_CanUpZn,
    ifelse((sp2_df$T_CanUpZn - zone_df$L_Bottom) > 0.0001 &
             (zone_df$L_Top - sp2_df$T_CanUpZn) > 0.0001,
           sp2_df$T_CanUpZn,
           ifelse((sp3_df$T_CanUpZn - zone_df$L_Bottom) > 0.0001 &
                    (zone_df$L_Top - sp3_df$T_CanUpZn) > 0.0001,
                  sp3_df$T_CanUpZn,
                  0
           )
    )
  )
)

# L_Mid2[Zone] = IF (C_CanUp[Zone]-L_Bottom[Zone])>0.0001 AND (L_Top[Zone]-C_CanUp[Zone])>0.0001 AND (L_Mid1[Zone]-C_CanUp[Zone])>0.0001 THEN C_CanUp[Zone] ELSE IF (T_CanUpZn[Zone,Sp1]-L_Bottom[Zone])>0.0001 AND (L_Top[Zone]-T_CanUpZn[Zone,Sp1])>0.0001 THEN T_CanUpZn[Zone,Sp1] ELSE  IF (T_CanUpZn[Zone,Sp2]-L_Bottom[Zone])>0.0001 AND (L_Top[Zone]-T_CanUpZn[Zone,Sp2])>0.0001 THEN T_CanUpZn[Zone,Sp2] ELSE if (T_CanUpZn[Zone,Sp3]-L_Bottom[Zone])>0.0001 AND (L_Top[Zone]-T_CanUpZn[Zone,Sp3])>0.0001 THEN T_CanUpZn[Zone,Sp3] else 0
zone_df$L_Mid2 <- ifelse(
  (zone_df$C_CanUp - zone_df$L_Bottom) > 0.0001 &
    (zone_df$L_Top - zone_df$C_CanUp) > 0.0001 &
    (zone_df$L_Mid1 - zone_df$C_CanUp) > 0.0001,
  zone_df$C_CanUp,
  ifelse(
    (sp1_df$T_CanUpZn - zone_df$L_Bottom) > 0.0001 &
      (zone_df$L_Top - sp1_df$T_CanUpZn) > 0.0001,
    sp1_df$T_CanUpZn,
    ifelse((sp2_df$T_CanUpZn - zone_df$L_Bottom) > 0.0001 &
             (zone_df$L_Top - sp2_df$T_CanUpZn) > 0.0001,
           sp2_df$T_CanUpZn,
           ifelse((sp3_df$T_CanUpZn -
                     zone_df$L_Bottom) > 0.0001 &
                    (zone_df$L_Top - sp3_df$T_CanUpZn) > 0.0001,
                  sp3_df$T_CanUpZn,
                  0
           )
    )
  )
)

# L_MidTop[Zone] = MAX(L_Mid1[Zone],L_Mid2[Zone])
zone_df$L_MidTop <- pmax(zone_df$L_Mid1, zone_df$L_Mid2)

# L_MidBot[Zone] = MIN(L_Mid1[Zone],L_Mid2[Zone])
zone_df$L_MidBot <- pmin(zone_df$L_Mid1, zone_df$L_Mid2)


zonelayer_df$L_Can <- 0
zonelayer_df[zonelayer_df$layer == 1, ]$L_Can <- zone_df$L_Top
zonelayer_df[zonelayer_df$layer == 2, ]$L_Can <- zone_df$L_MidTop
zonelayer_df[zonelayer_df$layer == 3, ]$L_Can <- zone_df$L_MidBot
zonelayer_df[zonelayer_df$layer == 4, ]$L_Can <- zone_df$L_Bottom

zonelayer_df$L_Can_down <- 0
zonelayer_df[zonelayer_df$layer == 1, ]$L_Can_down <- zone_df$L_MidTop
zonelayer_df[zonelayer_df$layer == 2, ]$L_Can_down <- zone_df$L_MidBot
zonelayer_df[zonelayer_df$layer == 3, ]$L_Can_down <- zone_df$L_Bottom
zonelayer_df[zonelayer_df$layer == 4, ]$L_Can_down <- 0

zonelayertree_df$L_Can <- rep(zonelayer_df$L_Can, nrow(tree_df))
zonelayertree_df$L_Can_down <- rep(zonelayer_df$L_Can_down, nrow(tree_df))


zonetree_to_zonelayertree <- function(x) {
  df <- zonelayertree_df["zone"]
  df$x <- rep(x, nlayer)
  df$tree_id <- rep(zonetree_df$tree_id, nlayer)
  df$layer <- rep(layer_df$layer, each = nzone * nrow(tree_df))
  df <- df[order(df$tree_id, df$layer, df$zone), ]
  return(df$x)
}

# df <- zonelayertree_df["zone"]
# df$T_LAIEff <- rep(zonetree_df$T_LAIEff, nlayer)
# df$tree_id <- rep(zonetree_df$tree_id, nlayer)
# df$layer <- rep(layer_df$layer, each = nzone * nrow(tree_df))
# df <- df[order(df$tree_id, df$layer, df$zone),]
# zonelayertree_df$T_LAIEff <- df$T_LAIEff

zonelayertree_df$T_LAIEff <- zonetree_to_zonelayertree(zonetree_df$T_LAIEff)
zonelayertree_df$T_CanUp <- zonetree_to_zonelayertree(zonetree_df$T_CanUp)

# T_CanLow[Tree] = T_WoodH[Tree]
tree_df$T_CanLow <- tree_df$T_WoodH
zonelayertree_df$T_CanLow <- rep(tree_df$T_CanLow, each = nrow(zone_df) * nrow(layer_df))

# Light_LAIT1[Zone,Tree] = IF(T_LAIEff[Zone,Tree]>0)THEN(T_LAIEff[Zone,Tree]*MAX(0, (MIN(T_CanUp[Tree],L_Top[Zone]) -MAX(L_MidTop[Zone],T_CanLow[Tree]))/(T_CanUp[Tree]-T_CanLow[Tree])))ELSE(0)
# Light_LAIT2[Zone,Tree] = IF(T_LAIEff[Zone,Tree]>0)THEN(T_LAIEff[Zone,Tree]*MAX(0, (MIN(T_CanUp[Tree],L_MidTop[Zone]) -MAX(L_MidBot[Zone],T_CanLow[Tree]))/(T_CanUp[Tree]-T_CanLow[Tree])))ELSE(0)
# Light_LAIT3[Zone,Tree] = IF(T_LAIEff[Zone,Tree]>0)THEN(T_LAIEff[Zone,Tree]*MAX(0, (MIN(T_CanUp[Tree],L_MidBot[Zone]) -MAX(L_Bottom[Zone],T_CanLow[Tree]))/(T_CanUp[Tree]-T_CanLow[Tree])))ELSE(0)
# Light_LAIT4[Zone,Tree] = IF(T_LAIEff[Zone,Tree]>0)THEN(T_LAIEff[Zone,Tree]*MAX(0, (MIN(T_CanUp[Tree],L_Bottom[Zone]) -T_CanLow[Tree])/(T_CanUp[Tree]-T_CanLow[Tree])))ELSE(0)

zonelayertree_df$Light_LAIT <- ifelse(zonelayertree_df$T_LAIEff > 0,
                                      zonelayertree_df$T_LAIEff * pmax(
                                        0,
                                        (
                                          pmin(zonelayertree_df$T_CanUp, zonelayertree_df$L_Can) -
                                            pmax(zonelayertree_df$L_Can_down, zonelayertree_df$T_CanLow)
                                        ) / (zonelayertree_df$T_CanUp - zonelayertree_df$T_CanLow)
                                      ),
                                      0)


# Light_TBAI[Zone,Tree] = 0
zonetree_df$Light_TBAI <- 0

# Light_TBAI1[Zone,Tree] = 0
# Light_TBAI2[Zone,Tree] = IF Light_LAIT3[Zone,Tree]>0 THEN 0 ELSE Light_TBAI[Zone,Tree]
# Light_TBAI3[Zone,Tree] = IF Light_LAIT3[Zone,Tree]>0 THEN Light_TBAI[Zone,Tree] ELSE 0
# Light_TBAI4[Zone,Tree] = IF Light_LAIT3[Zone,Tree]>0 THEN Light_TBAI[Zone,Tree] ELSE 0

l3_df <- zonelayertree_df[zonelayertree_df$layer == 3, ]

zonelayertree_df$Light_TBAI <- 0
zonelayertree_df[zonelayertree_df$layer == 2, ]$Light_TBAI <- ifelse(l3_df$Light_LAIT > 0, 0, zonetree_df$Light_TBAI)
zonelayertree_df[zonelayertree_df$layer == 3, ]$Light_TBAI <- ifelse(l3_df$Light_LAIT > 0, zonetree_df$Light_TBAI, 0)
zonelayertree_df[zonelayertree_df$layer == 4, ]$Light_TBAI <- ifelse(l3_df$Light_LAIT > 0, zonetree_df$Light_TBAI, 0)


zonelayer_df$Cq_kLightCurr <- rep(zone_df$Cq_kLightCurr, nlayer)



# Light_LAIC[Zn1,L1] = IF(C_LAI[Zn1]>0)THEN(C_LAI[Zn1]*MAX(0, (MIN(C_CanUp[Zn1],L_Top[Zn1]) -MAX(L_MidTop[Zn1],C_CanLow[Zn1]))/(C_CanUp[Zn1]-C_CanLow[Zn1])))ELSE(0)*L_MidBot[Zn1]*L_Bottom[Zn1]
# Light_LAIC[Zn1,L2] = IF(C_LAI[Zn1]>0)THEN(C_LAI[Zn1]*MAX(0, (MIN(C_CanUp[Zn1],L_MidTop[Zn1]) -MAX(L_MidBot[Zn1],C_CanLow[Zn1]))/(C_CanUp[Zn1]-C_CanLow[Zn1])))ELSE(0)*L_Bottom[Zn1]*L_Top[Zn1]
# Light_LAIC[Zn1,L3] = IF(C_LAI[Zn1]>0)THEN(C_LAI[Zn1]*MAX(0, (MIN(C_CanUp[Zn1],L_MidBot[Zn1]) -MAX(L_Bottom[Zn1],C_CanLow[Zn1]))/(C_CanUp[Zn1]-C_CanLow[Zn1])))ELSE(0)*L_MidTop[Zn1]*L_Top[Zn1]
# Light_LAIC[Zn1,L4] = IF(C_LAI[Zn1]>0)THEN(C_LAI[Zn1]*MAX(0, (MIN(C_CanUp[Zn1],L_Bottom[Zn1]) -MAX(0,C_CanLow[Zn1]))/(C_CanUp[Zn1]-C_CanLow[Zn1])))ELSE(0)*L_MidBot[Zn1]*L_Top[Zn1]*L_MidTop[Zn1]
# Light_LAIC[Zn2,L1] = IF(C_LAI[Zn2]>0)THEN(C_LAI[Zn2]*MAX(0, (MIN(C_CanUp[Zn2],L_Top[Zn2]) -MAX(L_MidTop[Zn2],C_CanLow[Zn2]))/(C_CanUp[Zn2]-C_CanLow[Zn2])))ELSE(0)*L_MidBot[Zn2]*L_Bottom[Zn2]
# Light_LAIC[Zn2,L2] = IF(C_LAI[Zn2]>0)THEN(C_LAI[Zn2]*MAX(0, (MIN(C_CanUp[Zn2],L_MidTop[Zn2]) -MAX(L_MidBot[Zn2],C_CanLow[Zn2]))/(C_CanUp[Zn2]-C_CanLow[Zn2])))ELSE(0)*L_Bottom[Zn2]*L_Top[Zn2]
# Light_LAIC[Zn2,L3] = IF(C_LAI[Zn2]>0)THEN(C_LAI[Zn2]*MAX(0, (MIN(C_CanUp[Zn2],L_MidBot[Zn2]) -MAX(L_Bottom[Zn2],C_CanLow[Zn2]))/(C_CanUp[Zn2]-C_CanLow[Zn2])))ELSE(0)*L_Top[Zn2]*L_MidTop[Zn2]
# Light_LAIC[Zn2,L4] = IF(C_LAI[Zn2]>0)THEN(C_LAI[Zn2]*MAX(0, (MIN(C_CanUp[Zn2],L_Bottom[Zn2]) -MAX(0,C_CanLow[Zn2]))/(C_CanUp[Zn2]-C_CanLow[Zn2])))ELSE(0)*L_MidBot[Zn2]*L_Top[Zn2]*L_MidTop[Zn2]
# Light_LAIC[Zn3,L1] = IF(C_LAI[Zn3]>0)THEN(C_LAI[Zn3]*MAX(0, (MIN(C_CanUp[Zn3],L_Top[Zn3]) -MAX(L_MidTop[Zn3],C_CanLow[Zn3]))/(C_CanUp[Zn3]-C_CanLow[Zn3])))ELSE(0)*L_MidBot[Zn3]*L_Bottom[Zn3]
# Light_LAIC[Zn3,L2] = IF(C_LAI[Zn3]>0)THEN(C_LAI[Zn3]*MAX(0, (MIN(C_CanUp[Zn3],L_MidTop[Zn3]) -MAX(L_MidBot[Zn3],C_CanLow[Zn3]))/(C_CanUp[Zn3]-C_CanLow[Zn3])))ELSE(0)*L_Bottom[Zn3]*L_Top[Zn3]
# Light_LAIC[Zn3,L3] = IF(C_LAI[Zn3]>0)THEN(C_LAI[Zn3]*MAX(0, (MIN(C_CanUp[Zn3],L_MidBot[Zn3]) -MAX(L_Bottom[Zn3],C_CanLow[Zn3]))/(C_CanUp[Zn3]-C_CanLow[Zn3])))ELSE(0)*L_Top[Zn3]*L_MidTop[Zn3]
# Light_LAIC[Zn3,L4] = IF(C_LAI[Zn3]>0)THEN(C_LAI[Zn3]*MAX(0, (MIN(C_CanUp[Zn3],L_Bottom[Zn3]) -MAX(0,C_CanLow[Zn3]))/(C_CanUp[Zn3]-C_CanLow[Zn3])))ELSE(0)*L_MidBot[Zn3]*L_Top[Zn3]*L_MidTop[Zn3]
# Light_LAIC[Zn4,L1] = IF(C_LAI[Zn4]>0)THEN(C_LAI[Zn4]*MAX(0, (MIN(C_CanUp[Zn4],L_Top[Zn4]) -MAX(L_MidTop[Zn4],C_CanLow[Zn4]))/(C_CanUp[Zn4]-C_CanLow[Zn4])))ELSE(0)*L_MidBot[Zn4]*L_Bottom[Zn4]
# Light_LAIC[Zn4,L2] = IF(C_LAI[Zn4]>0)THEN(C_LAI[Zn4]*MAX(0, (MIN(C_CanUp[Zn4],L_MidTop[Zn4]) -MAX(L_MidBot[Zn4],C_CanLow[Zn4]))/(C_CanUp[Zn4]-C_CanLow[Zn4])))ELSE(0)*L_Top[Zn4]*L_Bottom[Zn4]
# Light_LAIC[Zn4,L3] = IF(C_LAI[Zn4]>0)THEN(C_LAI[Zn4]*MAX(0, (MIN(C_CanUp[Zn4],L_MidBot[Zn4]) -MAX(L_Bottom[Zn4],C_CanLow[Zn4]))/(C_CanUp[Zn4]-C_CanLow[Zn4])))ELSE(0)*L_Top[Zn4]*L_MidTop[Zn4]
# Light_LAIC[Zn4,L4] = IF(C_LAI[Zn4]>0)THEN(C_LAI[Zn4]*MAX(0, (MIN(C_CanUp[Zn4],L_Bottom[Zn4]) -MAX(0,C_CanLow[Zn4]))/(C_CanUp[Zn4]-C_CanLow[Zn4])))ELSE(0)*L_MidBot[Zn4]*L_Top[Zn4]*L_MidTop[Zn4]

zonelayer_df$C_LAI <- rep(zone_df$C_LAI, nlayer)
zonelayer_df$C_CanUp <- rep(zone_df$C_CanUp, nlayer)
zonelayer_df$C_CanLow <- rep(zone_df$C_CanLow, nlayer)

zonelayer_df$L_fac <- 0
zonelayer_df[zonelayer_df$layer == 1, ]$L_fac <- zone_df$L_MidBot * zone_df$L_Bottom
zonelayer_df[zonelayer_df$layer == 2, ]$L_fac <- zone_df$L_Bottom * zone_df$L_Top
zonelayer_df[zonelayer_df$layer == 3, ]$L_fac <- zone_df$L_MidTop * zone_df$L_Top
zonelayer_df[zonelayer_df$layer == 4, ]$L_fac <- zone_df$L_MidBot * zone_df$L_Top * zone_df$L_MidTop

zonelayer_df$Light_LAIC <- ifelse(zonelayer_df$C_LAI > 0, zonelayer_df$C_LAI * pmax(
  0,
  (
    pmin(zonelayer_df$C_CanUp, zonelayer_df$L_Can) -
      pmax(zonelayer_df$L_Can_down, zonelayer_df$C_CanLow)
  ) / (zonelayer_df$C_CanUp - zonelayer_df$C_CanLow)
), 0) * zonelayer_df$L_fac


# Light_TCCap1[Zone] = 1-EXP(-T_klight[Sp1]*Light_LAIT1[Zone,Sp1]-T_klight[Sp2]*Light_LAIT1[Zone,Sp2]-T_klight[Sp3]*Light_LAIT1[Zone,Sp3]-Light_kTB[Sp1]*Light_TBAI1[Zone,Sp1]-Light_kTB[Sp2]*Light_TBAI1[Zone,Sp2]-Light_kTB[Sp3]*Light_TBAI1[Zone,Sp3]-Cq_kLightCurr[Zone]*Light_LAIC[Zone,L1])
# Light_TCCap2[Zone] = (1-Light_TCCap1[Zone])*(1-EXP(-T_klight[Sp1]*Light_LAIT2[Zone,Sp1]-T_klight[Sp2]*Light_LAIT2[Zone,Sp2]-T_klight[Sp3]*Light_LAIT2[Zone,Sp3]-Light_kTB[Sp1]*Light_TBAI2[Zone,Sp1]-Light_kTB[Sp2]*Light_TBAI2[Zone,Sp2]-Light_kTB[Sp3]*Light_TBAI2[Zone,Sp3]-Cq_kLightCurr[Zone]*Light_LAIC[Zone,L2]))
# Light_TCCap3[Zone] = (1-Light_TCCap1[Zone])*(1-Light_TCCap2[Zone])* (1-EXP(-T_klight[Sp1]*Light_LAIT3[Zone,Sp1]-T_klight[Sp2]*Light_LAIT3[Zone,Sp2]-T_klight[Sp3]*Light_LAIT3[Zone,Sp3]-Light_kTB[Sp1]*Light_TBAI3[Zone,Sp1]-Light_kTB[Sp2]*Light_TBAI3[Zone,Sp2]-Light_kTB[Sp3]*Light_TBAI3[Zone,Sp3]-Cq_kLightCurr[Zone]*Light_LAIC[Zone,L3]))
# Light_TCCap4[Zone] = (1-Light_TCCap1[Zone])*(1-Light_TCCap2[Zone])*(1-Light_TCCap3[Zone])*(1-EXP(-T_klight[Sp1]*Light_LAIT4[Zone,Sp1]-T_klight[Sp2]*Light_LAIT4[Zone,Sp2]-T_klight[Sp3]*Light_LAIT4[Zone,Sp3]-Light_kTB[Sp1]*Light_TBAI4[Zone,Sp1]-Light_kTB[Sp2]*Light_TBAI4[Zone,Sp2]-Light_kTB[Sp3]*Light_TBAI4[Zone,Sp3]-Cq_kLightCurr[Zone]*Light_LAIC[Zone,L4]))

zlt_sp1_df <- zonelayertree_df[zonelayertree_df$tree_id == 1, ]
zlt_sp2_df <- zonelayertree_df[zonelayertree_df$tree_id == 2, ]
zlt_sp3_df <- zonelayertree_df[zonelayertree_df$tree_id == 3, ]
t_sp1_df <- tree_df[tree_df$tree_id == 1, ]
t_sp2_df <- tree_df[tree_df$tree_id == 2, ]
t_sp3_df <- tree_df[tree_df$tree_id == 3, ]

zonelayer_df$Light_TCCap <- 1 - exp(
  -t_sp1_df$T_klight * zlt_sp1_df$Light_LAIT -
    t_sp2_df$T_klight * zlt_sp1_df$Light_LAIT -
    t_sp3_df$T_klight * zlt_sp1_df$Light_LAIT -
    t_sp1_df$Light_kTB * zlt_sp1_df$Light_TBAI -
    t_sp2_df$Light_kTB * zlt_sp2_df$Light_TBAI -
    t_sp3_df$Light_kTB * zlt_sp3_df$Light_TBAI -
    zonelayer_df$Cq_kLightCurr * zonelayer_df$Light_LAIC
)
zl1_df <-  zonelayer_df[zonelayer_df$layer == 1, ]
zonelayer_df[zonelayer_df$layer == 2, ]$Light_TCCap <- (1 - zl1_df$Light_TCCap) * zonelayer_df[zonelayer_df$layer == 2, ]$Light_TCCap
zl2_df <-  zonelayer_df[zonelayer_df$layer == 2, ]
zonelayer_df[zonelayer_df$layer == 3, ]$Light_TCCap <- (1 - zl1_df$Light_TCCap) * (1 - zl2_df$Light_TCCap) * zonelayer_df[zonelayer_df$layer == 3, ]$Light_TCCap
zl3_df <-  zonelayer_df[zonelayer_df$layer == 3, ]
zonelayer_df[zonelayer_df$layer == 4, ]$Light_TCCap <- (1 - zl1_df$Light_TCCap) * (1 - zl2_df$Light_TCCap) * (1 - zl3_df$Light_TCCap) * zonelayer_df[zonelayer_df$layer == 4, ]$Light_TCCap



# Light_TCap1[Zone,Tree] = IF(T_klight[Sp1]*Light_LAIT1[Zone,Sp1]+T_klight[Sp2]*Light_LAIT1[Zone,Sp2]+T_klight[Sp3]*Light_LAIT1[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L2]+Light_kTB[Sp1]*Light_TBAI1[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI1[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI1[Zone,Sp3])>0 THEN(Light_TCCap1[Zone]*(T_klight[Tree]*Light_LAIT1[Zone,Tree])/(T_klight[Sp1]*Light_LAIT1[Zone,Sp1]+T_klight[Sp2]*Light_LAIT1[Zone,Sp2]+T_klight[Sp3]*Light_LAIT1[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L2]+Light_kTB[Sp1]*Light_TBAI1[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI1[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI1[Zone,Sp3]))ELSE(0)
# Light_TCap2[Zone,Tree] = IF(Light_TCCap2[Zone]>0)THEN(Light_TCCap2[Zone]*(T_klight[Tree]*Light_LAIT2[Zone,Tree])/(T_klight[Sp1]*Light_LAIT2[Zone,Sp1]+T_klight[Sp2]*Light_LAIT2[Zone,Sp2]+T_klight[Sp3]*Light_LAIT2[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L2]+Light_kTB[Sp1]*Light_TBAI2[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI2[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI2[Zone,Sp3]))ELSE(0)
# Light_TCap3[Zone,Tree] = IF(Light_TCCap3[Zone]>0)THEN(Light_TCCap3[Zone]*T_klight[Tree]*Light_LAIT3[Zone,Tree]/(T_klight[Sp1]*Light_LAIT3[Zone,Sp1]+T_klight[Sp2]*Light_LAIT3[Zone,Sp2]+T_klight[Sp3]*Light_LAIT3[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L3]+Light_kTB[Sp1]*Light_TBAI3[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI3[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI3[Zone,Sp3]))ELSE(0)
# Light_TCap4[Zone,Tree] = IF(Light_TCCap4[Zone]>0)THEN(Light_TCCap4[Zone]*T_klight[Tree]*Light_LAIT4[Zone,Tree]/(T_klight[Sp1]*Light_LAIT4[Zone,Sp1]+T_klight[Sp2]*Light_LAIT4[Zone,Sp2]+T_klight[Sp3]*Light_LAIT4[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L4]+Light_kTB[Sp1]*Light_TBAI4[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI4[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI4[Zone,Sp3]))ELSE(0)

zonelayertree_df$Light_TCCap <- rep(zonelayer_df$Light_TCCap, nrow(tree_df))
zonelayertree_df$T_klight <- rep(tree_df$T_klight, each = nzone * nlayer)

zonelayer_df$Light_sp <- t_sp1_df$T_klight * zlt_sp1_df$Light_LAIT +
  t_sp2_df$T_klight * zlt_sp2_df$Light_LAIT +
  t_sp3_df$T_klight * zlt_sp3_df$Light_LAIT +
  zonelayer_df$Cq_kLightCurr * zonelayer_df$Light_LAIC +
  t_sp1_df$Light_kTB * zlt_sp1_df$Light_TBAI +
  t_sp2_df$Light_kTB * zlt_sp2_df$Light_TBAI +
  t_sp3_df$Light_kTB * zlt_sp3_df$Light_TBAI
zonelayertree_df$Light_sp <- rep(zonelayer_df$Light_sp, nrow(tree_df))

zonelayertree_df$Light_TCap <- ifelse(
  zonelayertree_df$Light_TCCap > 0,
  zonelayertree_df$Light_TCCap * (zonelayertree_df$T_klight * zonelayertree_df$Light_LAIT) / zonelayertree_df$Light_sp,
  0
)

zl1t_df <- zonelayertree_df[zonelayertree_df$layer == 1, ]
zonelayertree_df[zonelayertree_df$layer == 1, ]$Light_TCap <- ifelse(
  zl1t_df$Light_sp > 0,
  zl1t_df$Light_TCCap * (zl1t_df$T_klight * zl1t_df$Light_LAIT) /
    zl1t_df$Light_sp ,
  0
)


# angle_df <- data.frame(LightAngles = pars$Light_par$LightAngles)
#
# # TanAngles[LightAngle] = tan(LightAngles[LightAngle]*PI/180)
# angle_df$TanAngles <- tan(angle_df$LightAngles * pi / 180)

# LightCapPerAngleZn1[LightAngle] = AF_Zonewidth[Zn1]+
#   (min(AF_Zonewidth[Zn2],max(0,(L_Top[Zn1]-L_Top[Zn2]))*max(0,TanAngles[LightAngle]))-
#min(AF_Zonewidth[Zn1],max(0,(L_Top[Zn2]-L_Top[Zn1]))*max(0,TanAngles[LightAngle])))/2
# LightCapPerAngleZn2[LightAngle] = AF_Zonewidth[Zn2]+
#   (min(AF_Zonewidth[Zn1],max(0,(L_Top[Zn2]-L_Top[Zn1]))*max(0,TanAngles[LightAngle]))+
#min(AF_Zonewidth[Zn3],max(0,(L_Top[Zn2]-L_Top[Zn3]))*max(0,TanAngles[LightAngle]))-
#min(AF_Zonewidth[Zn2],max(0,(L_Top[Zn1]-L_Top[Zn2]))*max(0,TanAngles[LightAngle]))-
#min(AF_Zonewidth[Zn2],max(0,(L_Top[Zn3]-L_Top[Zn2]))*max(0,TanAngles[LightAngle])))/2
# LightCapPerAngleZn3[LightAngle] = AF_Zonewidth[Zn3]+
#   (min(AF_Zonewidth[Zn2],max(0,(L_Top[Zn3]-L_Top[Zn2]))*max(0,TanAngles[LightAngle]))+
#min(AF_Zonewidth[Zn4],max(0,(L_Top[Zn3]-L_Top[Zn4]))*max(0,TanAngles[LightAngle]))-
#min(AF_Zonewidth[Zn3],max(0,(L_Top[Zn2]-L_Top[Zn3]))*max(0,TanAngles[LightAngle]))-
#min(AF_Zonewidth[Zn3],max(0,(L_Top[Zn4]-L_Top[Zn3]))*max(0,TanAngles[LightAngle])))/2
# LightCapPerAngleZn4[LightAngle] = AF_Zonewidth[Zn4]+
#   (min(AF_Zonewidth[Zn3],max(0,(L_Top[Zn4]-L_Top[Zn3]))*max(0,TanAngles[LightAngle]))+
#min(AF_Zonewidth[Zn4],max(0,(L_Top[Zn3]-L_Top[Zn4]))*max(0,TanAngles[LightAngle])))/2


z1 <- zone_df[zone_df$zone == 1, ]
z2 <- zone_df[zone_df$zone == 2, ]
z3 <- zone_df[zone_df$zone == 3, ]
z4 <- zone_df[zone_df$zone == 4, ]

angle_df$LightCapPerAngleZn1 <- z1$AF_ZoneWidth +
  (pmin(z2$AF_ZoneWidth, pmax(0, (z1$L_Top - z2$L_Top)) * pmax(0, angle_df$TanAngles)) -
     pmin(z1$AF_ZoneWidth, pmax(0, (z2$L_Top - z1$L_Top)) * pmax(0, angle_df$TanAngles))) /
  2

angle_df$LightCapPerAngleZn2 <- z2$AF_ZoneWidth +
  (
    pmin(z1$AF_ZoneWidth, pmax(0, (z2$L_Top - z1$L_Top)) * pmax(0, angle_df$TanAngles)) +
      pmin(z3$AF_ZoneWidth, pmax(0, (z2$L_Top - z3$L_Top)) * pmax(0, angle_df$TanAngles)) -
      pmin(z2$AF_ZoneWidth, pmax(0, (z1$L_Top - z2$L_Top)) * pmax(0, angle_df$TanAngles)) -
      pmin(z2$AF_ZoneWidth, pmax(0, (z3$L_Top - z2$L_Top)) * pmax(0, angle_df$TanAngles))
  ) / 2

angle_df$LightCapPerAngleZn3 <- z3$AF_ZoneWidth +
  (
    pmin(z2$AF_ZoneWidth, pmax(0, (z3$L_Top - z2$L_Top)) * pmax(0, angle_df$TanAngles)) +
      pmin(z4$AF_ZoneWidth, pmax(0, (z3$L_Top - z4$L_Top)) * pmax(0, angle_df$TanAngles)) -
      pmin(z3$AF_ZoneWidth, pmax(0, (z2$L_Top - z3$L_Top)) * pmax(0, angle_df$TanAngles)) -
      pmin(z3$AF_ZoneWidth, pmax(0, (z4$L_Top - z3$L_Top)) * pmax(0, angle_df$TanAngles))
  ) / 2

angle_df$LightCapPerAngleZn4 <- z4$AF_ZoneWidth +
  (pmin(z3$AF_ZoneWidth, pmax(0, (z4$L_Top - z3$L_Top)) * pmax(0, angle_df$TanAngles)) +
     pmin(z4$AF_ZoneWidth, pmax(0, (z3$L_Top - z4$L_Top)) * pmax(0, angle_df$TanAngles))) /
  2

angle_df$LightSwitch <- pars$Light_par$LightSwitch

# LightPerAngle[LightAngle] = if(LightSwitch=1) then Ligh1tPerAngleUniform[LightAngle] else
#   if(LightSwitch=2) then Light2PerAngleSkewed[LightAngle] else
# if(LightSwitch=3) then Light3PerAngleVertical[LightAngle] else 0
angle_df$LightPerAngle <- ifelse(
  angle_df$LightSwitch == 1,
  pars$Light_par$Ligh1tPerAngleUniform,
  ifelse(
    angle_df$LightSwitch == 2,
    pars$Light_par$Light2PerAngleSkewed,
    ifelse(
      angle_df$LightSwitch == 3,
      pars$Light_par$Light3PerAngleVertical,
      0
    )
  )
)

# TotLight = ARRAYSUM(LightPerAngle[*])
angle_df$TotLight <- sum(angle_df$LightPerAngle)

# RelLightEnergyPerAngle[LightAngle] = if TotLight=0 then 0 else LightPerAngle[LightAngle]/TotLight
angle_df$RelLightEnergyPerAngle <- ifelse(angle_df$TotLight == 0,
                                          0,
                                          angle_df$LightPerAngle / angle_df$TotLight)

# ZoneIdentity[Zn1] = 1
# ZoneIdentity[Zn2] = 2
# ZoneIdentity[Zn3] = 3
# ZoneIdentity[Zn4] = 4

# RelLightCapPerAnglePerZone[LightAngle,Zone] = if ZoneIdentity[Zone] = 1 then LightCapPerAngleZn1[LightAngle] *AF_ZoneTot/(LightCapPerAngleZn1[LightAngle]+LightCapPerAngleZn2[LightAngle]+LightCapPerAngleZn3[LightAngle]+LightCapPerAngleZn4[LightAngle])   else
#   if ZoneIdentity[Zone] = 2 then LightCapPerAngleZn2[LightAngle] *AF_ZoneTot/(LightCapPerAngleZn1[LightAngle]+LightCapPerAngleZn2[LightAngle]+LightCapPerAngleZn3[LightAngle]+LightCapPerAngleZn4[LightAngle]) else
# if ZoneIdentity[Zone] = 3 then LightCapPerAngleZn3[LightAngle] *AF_ZoneTot/(LightCapPerAngleZn1[LightAngle]+LightCapPerAngleZn2[LightAngle]+LightCapPerAngleZn3[LightAngle]+LightCapPerAngleZn4[LightAngle]) else
# if ZoneIdentity[Zone] = 4 then LightCapPerAngleZn4[LightAngle] *AF_ZoneTot/(LightCapPerAngleZn1[LightAngle]+LightCapPerAngleZn2[LightAngle]+LightCapPerAngleZn3[LightAngle]+LightCapPerAngleZn4[LightAngle]) else 0

# LightEnergyPerAnglePerZone[LightAngle,Zone] = RelLightCapPerAnglePerZone[LightAngle,Zone]*RelLightEnergyPerAngle[LightAngle]

angle_df$LightCapPerAngle_sum <- angle_df$LightCapPerAngleZn1 + angle_df$LightCapPerAngleZn2 + angle_df$LightCapPerAngleZn3 + angle_df$LightCapPerAngleZn4

angle_df$LightEnergyPerAnglePerZone1 <- angle_df$RelLightEnergyPerAngle * angle_df$LightCapPerAngleZn1 * pars$AF_par$AF_ZoneTot /
  angle_df$LightCapPerAngle_sum
angle_df$LightEnergyPerAnglePerZone2 <- angle_df$RelLightEnergyPerAngle * angle_df$LightCapPerAngleZn2 * pars$AF_par$AF_ZoneTot /
  angle_df$LightCapPerAngle_sum
angle_df$LightEnergyPerAnglePerZone3 <- angle_df$RelLightEnergyPerAngle * angle_df$LightCapPerAngleZn3 * pars$AF_par$AF_ZoneTot /
  angle_df$LightCapPerAngle_sum
angle_df$LightEnergyPerAnglePerZone4 <- angle_df$RelLightEnergyPerAngle * angle_df$LightCapPerAngleZn4 * pars$AF_par$AF_ZoneTot /
  angle_df$LightCapPerAngle_sum

# LightCapPerZone[Zone] = ARRAYSUM(LightEnergyPerAnglePerZone[*,Zone])
zone_df$LightCapPerZone <- 0
zone_df[zone_df$zone == 1, ]$LightCapPerZone <- sum(angle_df$LightEnergyPerAnglePerZone1)
zone_df[zone_df$zone == 2, ]$LightCapPerZone <- sum(angle_df$LightEnergyPerAnglePerZone2)
zone_df[zone_df$zone == 3, ]$LightCapPerZone <- sum(angle_df$LightEnergyPerAnglePerZone3)
zone_df[zone_df$zone == 4, ]$LightCapPerZone <- sum(angle_df$LightEnergyPerAnglePerZone4)

# RelLightCapPerZone[Zone] = if AF_Zonewidth[Zone]= 0 then 0 else LightCapPerZone[Zone]/AF_Zonewidth[Zone]
zone_df$RelLightCapPerZone <- ifelse(zone_df$AF_ZoneWidth == 0,
                                     0,
                                     zone_df$LightCapPerZone / zone_df$AF_ZoneWidth)
zonetree_df$RelLightCapPerZone <- rep(zone_df$RelLightCapPerZone, nrow(tree_df))

zonetree_df$LightSwitch <- pars$Light_par$LightSwitch

Light_TCap_sum_df <- aggregate(zonelayertree_df["Light_TCap"], by = zonelayertree_df[c("zone", "tree_id")], sum)
# Light_TCap1234[Zone,Tree] = if LightSwitch = 1 or LightSwitch = 2 then
# (Light_TCap1[Zone,Tree]+Light_TCap2[Zone,Tree]+Light_TCap3[Zone,Tree]+Light_TCap4[Zone,Tree])*RelLightCapPerZone[Zone]
# else (Light_TCap1[Zone,Tree]+Light_TCap2[Zone,Tree]+Light_TCap3[Zone,Tree]+Light_TCap4[Zone,Tree])
zonetree_df$Light_TCap1234 <- ifelse(
  zonetree_df$LightSwitch == 1 | zonetree_df$LightSwitch == 2,
  Light_TCap_sum_df$Light_TCap * zonetree_df$RelLightCapPerZone,
  Light_TCap_sum_df$Light_TCap
)


z1_df <- zonetree_df[zonetree_df$zone == 1, ]
z2_df <- zonetree_df[zonetree_df$zone == 2, ]
z3_df <- zonetree_df[zonetree_df$zone == 3, ]
z4_df <- zonetree_df[zonetree_df$zone == 4, ]

# Evap_TreeWatDem[Tree] = Evap_EpotDemandNotMetBy_CanInterc*(Light_TCap1234[Zn1,Tree]*AF_ZoneFrac[Zn1]+Light_TCap1234[Zn2,Tree]*AF_ZoneFrac[Zn2]+Light_TCap1234[Zn3,Tree]*AF_ZoneFrac[Zn3]+Light_TCap1234[Zn4,Tree]*AF_ZoneFrac[Zn4])
tree_df$Light_TCap1234_sumf <- z1_df$Light_TCap1234 * z1_df$AF_ZoneFrac + z2_df$Light_TCap1234 * z2_df$AF_ZoneFrac + z3_df$Light_TCap1234 * z3_df$AF_ZoneFrac + z4_df$Light_TCap1234 * z4_df$AF_ZoneFrac
tree_df$Evap_TreeWatDem <- Evap_EpotDemandNotMetBy_CanInterc * tree_df$Light_TCap1234_sumf

# T_Light[Tree] =  MIN(1,(Light_TCap1234[Zn1, Tree]*AF_ZoneFrac[Zn1]+ Light_TCap1234[Zn2, Tree]*AF_ZoneFrac[Zn2]+
#   Light_TCap1234[Zn3, Tree]*AF_ZoneFrac[Zn3]+
#   Light_TCap1234[Zn4, Tree]*AF_ZoneFrac[Zn4]))
tree_df$T_Light <-  pmin(1, tree_df$Light_TCap1234_sumf)

tree_df$TW_EnergyDrivenEpot_is <- pars$T_par$TW_EnergyDrivenEpot_is

# TP_WaterDemand[Tree] = TP_ParasiteBiomass[Tree,DW]*TP_SLA_LWR*TP_WaterDemandperLeafArea
tree_df$TP_WaterDemand <- treepcomp_df[treepcomp_df$PlantComp == "DW",]$TP_ParasiteBiomass * pars$T_par$TP_SLA_LWR * pars$T_par$TP_WaterDemandperLeafArea




# T_TranspRatioTime[Tree] = GRAPH(T_Stage[Tree,LeafAge])
tree_df$T_TranspRatioTime <- unlist(lapply(treestage_df[treestage_df$Tree_Stage == "LeafAge", ]$T_Stage, get_T_TranspRatioTime))

# TW_DemandPot[Tree] = If TW_EnergyDrivenEpot? = 1 then Evap_TreeWatDem[Tree] + TP_WaterDemand[Tree] else if T_TranspRatioConstant? = 1 then
# T_Light[Tree]*T_GroMax[Tree]*T_TranspRatio[Tree] + TP_WaterDemand[Tree] else T_Light[Tree]*T_GroMax[Tree]*T_TranspRatio[Tree]/T_TranspRatioTime[Tree] + TP_WaterDemand[Tree]
tree_df$T_TranspRatioConstant_is <-  pars$T_par$T_TranspRatioConstant_is

tree_df$TW_DemandPot <- ifelse(
  tree_df$TW_EnergyDrivenEpot_is == 1,
  tree_df$Evap_TreeWatDem + tree_df$TP_WaterDemand,
  ifelse(
    tree_df$T_TranspRatioConstant_is == 1,
    tree_df$T_Light * tree_df$T_GroMax * tree_df$T_TranspRatio + tree_df$TP_WaterDemand,
    tree_df$T_Light * tree_df$T_GroMax * tree_df$T_TranspRatio /
      tree_df$T_TranspRatioTime + tree_df$TP_WaterDemand
  )
)

# TW_PotSuctHalf[Tree] = -((TW_PotSuctAlphMax[Tree]*TW_PotSuctAlphMin[Tree])^0.5)
tree_df$TW_PotSuctHalf <- -((tree_df$TW_PotSuctAlphMax * tree_df$TW_PotSuctAlphMin)^0.5)

# TW_DrySoilWeightFac[Tree,BufValues] = TW_DryFactRangeInit[BufValues]^TW_DryFactPower[Tree]
treebuf_df$TW_DryFactPower <- rep(tree_df$TW_DryFactPower, nrow(buf_df))
treebuf_df$TW_DrySoilWeightFac <- treebuf_df$TW_DryFactRangeInit^treebuf_df$TW_DryFactPower





# TW_PotSoilStep1_Sp1[Zone,BufValues] = 100*AF_ZoneFrac[Zone]*(AF_DepthAct1[Zone]*Rt_TLrv1[Zone,Sp1]/ABS(W_PTheta1[Zone])^TW_DrySoilWeightFac[Sp1,BufValues]+AF_Depth2[Zone]*Rt_TLrv2[Zone,Sp1]/ABS(W_PTheta2[Zone])^TW_DrySoilWeightFac[Sp1,BufValues]+AF_Depth3[Zone]*Rt_TLrv3[Zone,Sp1]/ABS(W_PTheta3[Zone])^TW_DrySoilWeightFac[Sp1,BufValues]+AF_Depth4[Zone]*Rt_TLrv4[Zone,Sp1]/ABS(W_PTheta4[Zone])^TW_DrySoilWeightFac[Sp1,BufValues])
# TW_PotSoilStep1_Sp2[Zone,BufValues] = 100*AF_ZoneFrac[Zone]*(AF_DepthAct1[Zone]*Rt_TLrv1[Zone,Sp2]/ABS(W_PTheta1[Zone])^TW_DrySoilWeightFac[Sp2,BufValues]+AF_Depth2[Zone]*Rt_TLrv2[Zone,Sp2]/ABS(W_PTheta2[Zone])^TW_DrySoilWeightFac[Sp2,BufValues]+AF_Depth3[Zone]*Rt_TLrv3[Zone,Sp2]/ABS(W_PTheta3[Zone])^TW_DrySoilWeightFac[Sp2,BufValues]+AF_Depth4[Zone]*Rt_TLrv4[Zone,Sp2]/ABS(W_PTheta4[Zone])^TW_DrySoilWeightFac[Sp2,BufValues])
# TW_PotSoilStep1_Sp3[Zone,BufValues] = 100*AF_ZoneFrac[Zone]*(AF_DepthAct1[Zone]*Rt_TLrv1[Zone,Sp3]/ABS(W_PTheta1[Zone])^TW_DrySoilWeightFac[Sp3,BufValues]+AF_Depth2[Zone]*Rt_TLrv2[Zone,Sp3]/ABS(W_PTheta2[Zone])^TW_DrySoilWeightFac[Sp3,BufValues]+AF_Depth3[Zone]*Rt_TLrv3[Zone,Sp3]/ABS(W_PTheta3[Zone])^TW_DrySoilWeightFac[Sp3,BufValues]+AF_Depth4[Zone]*Rt_TLrv4[Zone,Sp3]/ABS(W_PTheta4[Zone])^TW_DrySoilWeightFac[Sp3,BufValues])

zonetreebuf_df$AF_ZoneFrac <- rep(zonetree_df$AF_ZoneFrac, nrow(buf_df))
zonetreebuf_df$W_PTheta1 <- zonelayer_df[zonelayer_df$layer == 1, ]$W_PTheta
zonetreebuf_df$W_PTheta2 <- zonelayer_df[zonelayer_df$layer == 2, ]$W_PTheta
zonetreebuf_df$W_PTheta3 <- zonelayer_df[zonelayer_df$layer == 3, ]$W_PTheta
zonetreebuf_df$W_PTheta4 <- zonelayer_df[zonelayer_df$layer == 4, ]$W_PTheta

zonelayertree_df$Rt_TLrv_depth <- zonelayertree_df$AF_Depth * zonelayertree_df$Rt_TLrv
zonetreebuf_df$Rt_TLrv_d1 <- rep(zonelayertree_df[zonelayertree_df$layer == 1, ]$Rt_TLrv_depth, nrow(buf_df))
zonetreebuf_df$Rt_TLrv_d2 <- rep(zonelayertree_df[zonelayertree_df$layer == 2, ]$Rt_TLrv_depth, nrow(buf_df))
zonetreebuf_df$Rt_TLrv_d3 <- rep(zonelayertree_df[zonelayertree_df$layer == 3, ]$Rt_TLrv_depth, nrow(buf_df))
zonetreebuf_df$Rt_TLrv_d4 <- rep(zonelayertree_df[zonelayertree_df$layer == 4, ]$Rt_TLrv_depth, nrow(buf_df))

df <- treebuf_df[c("tree_id", "buf_id", "TW_DrySoilWeightFac")]
df <- df[rep(seq_len(nrow(df)), nzone), ]
df$zone <- rep(zone_df$zone, each = nrow(treebuf_df))
df <- df[order(df$buf_id, df$tree_id, df$zone), ]
zonetreebuf_df$TW_DrySoilWeightFac <- df$TW_DrySoilWeightFac

zonetreebuf_df$TW_PotSoilStep1 <- 100 * zonetreebuf_df$AF_ZoneFrac * (
  zonetreebuf_df$Rt_TLrv_d1 / abs(zonetreebuf_df$W_PTheta1)^zonetreebuf_df$TW_DrySoilWeightFac +
    zonetreebuf_df$Rt_TLrv_d2 /
    abs(zonetreebuf_df$W_PTheta2)^zonetreebuf_df$TW_DrySoilWeightFac +
    zonetreebuf_df$Rt_TLrv_d3 /
    abs(zonetreebuf_df$W_PTheta3)^zonetreebuf_df$TW_DrySoilWeightFac +
    zonetreebuf_df$Rt_TLrv_d4 /
    abs(zonetreebuf_df$W_PTheta4)^zonetreebuf_df$TW_DrySoilWeightFac
)

# Rt_TLra[Zone,Tree] = (Rt_TLrv1[Zone,Tree]*AF_DepthAct1[Zone]+Rt_TLrv2[Zone,Tree]*AF_Depth2[Zone]+Rt_TLrv3[Zone,Tree]*AF_Depth3[Zone]+Rt_TLrv4[Zone,Tree]*AF_Depth4[Zone])*100
zonetree_df$Rt_TLra <- aggregate(zonelayertree_df["Rt_TLrv_depth"], zonelayertree_df[c("zone", "tree_id")], sum)$Rt_TLrv_depth *
  100

# Rt_TField[Tree] = AF_ZoneFrac[Zn1]*Rt_TLra[Zn1, Tree]+AF_ZoneFrac[Zn2]*Rt_TLra[Zn2, Tree]+AF_ZoneFrac[Zn3]*Rt_TLra[Zn3, Tree]+Rt_TLra[Zn4, Tree]*AF_ZoneFrac[Zn4]
zonetree_df$Rt_TLra_act <- zonetree_df$AF_ZoneFrac * zonetree_df$Rt_TLra
tree_df$Rt_TField <- aggregate(zonetree_df["Rt_TLra_act"], zonetree_df[c("tree_id")], sum)$Rt_TLra_act


# TW_PotSoilMeanPerceived[Tree,BufValues] = If T_TreeSpId[Tree] = 1 then
# IF Rt_TField[Sp1]>0 and ARRAYSUM(TW_PotSoilStep1_Sp1[*,BufValues]) <>0 THEN -TW_RangeToppingUp[BufValues]*((Rt_TField[Sp1]/ARRAYSUM(TW_PotSoilStep1_Sp1[*,BufValues]) )^(1/TW_DrySoilWeightFac[Sp1,BufValues])) ELSE 0
# Else If T_TreeSpId[Tree] = 2 then
# IF Rt_TField[Sp2]>0 and ARRAYSUM(TW_PotSoilStep1_Sp2[*,BufValues]) <>0 THEN -TW_RangeToppingUp[BufValues]*((Rt_TField[Sp2]/ARRAYSUM(TW_PotSoilStep1_Sp2[*,BufValues]) )^(1/TW_DrySoilWeightFac[Sp2,BufValues])) ELSE 0
# Else If T_TreeSpId[Tree] = 3 then
# IF Rt_TField[Sp3]>0 and ARRAYSUM(TW_PotSoilStep1_Sp3[*,BufValues]) <>0 THEN -TW_RangeToppingUp[BufValues]*((Rt_TField[Sp3]/ARRAYSUM(TW_PotSoilStep1_Sp3[*,BufValues]) )^(1/TW_DrySoilWeightFac[Sp3,BufValues])) ELSE 0 Else 0
treebuf_df$Rt_TField <- rep(tree_df$Rt_TField, nrow(buf_df))
treebuf_df$TW_PotSoilStep1 <- aggregate(zonetreebuf_df["TW_PotSoilStep1"], zonetreebuf_df[c("tree_id", "buf_id")], sum)$TW_PotSoilStep1
treebuf_df$TW_RangeToppingUp <- rep(buf_df$TW_RangeToppingUp, each = nrow(tree_df))

treebuf_df$TW_PotSoilMeanPerceived <- ifelse(
  treebuf_df$Rt_TField > 0 & treebuf_df$TW_PotSoilStep1 != 0,
  -treebuf_df$TW_RangeToppingUp *
    ((treebuf_df$Rt_TField / treebuf_df$TW_PotSoilStep1)^(1 / treebuf_df$TW_DrySoilWeightFac)
    ),
  0
)

# TW_PotRadial[Tree] = IF(Rt_TField[Tree]>0 AND T_RootConductivity[Tree]>0)THEN(-TW_DemandPot[Tree]*0.1/(T_RootConductivity[Tree]*(Rt_TField[Tree])))ELSE(0)
tree_df$TW_PotRadial <- ifelse(
  tree_df$Rt_TField > 0 &
    tree_df$T_RootConductivity > 0,
  -tree_df$TW_DemandPot * 0.1 / (tree_df$T_RootConductivity * tree_df$Rt_TField),
  0
)



zonetree_df$AF_TreePosit4Q <- rep(tree_df$AF_TreePosit4Q, each = nzone)
zonetree_df$Rt_ZoneLeft <- rep(zone_df$Rt_ZoneLeft, nrow(tree_df))
zonetree_df$Rt_ZoneRight <- rep(zone_df$Rt_ZoneRight, nrow(tree_df))

# TW_DistAxialTransp[Tree,Zone] = if AF_TreePosit4Q[Tree] = 0 then (Rt_ZoneLeft[Zone]+Rt_ZoneRight[Zone])/2 else AF_ZoneTot-(Rt_ZoneLeft[Zone]+Rt_ZoneRight[Zone])/2
zonetree_df$TW_DistAxialTransp <- ifelse(
  zonetree_df$AF_TreePosit4Q == 0,
  (zonetree_df$Rt_ZoneLeft + zonetree_df$Rt_ZoneRight) / 2,
  AF_ZoneTot - (zonetree_df$Rt_ZoneLeft + zonetree_df$Rt_ZoneRight) / 2
)

# TW_MeanDist[Tree] = if ARRAYSUM(Rt_TLra[*,Tree])>0 then
# (Rt_TLra[Zn1,Tree]*TW_DistAxialTransp[Tree,Zn1]+
# Rt_TLra[Zn2,Tree]*TW_DistAxialTransp[Tree,Zn2]+
# Rt_TLra[Zn3,Tree]*TW_DistAxialTransp[Tree,Zn3]+
# Rt_TLra[Zn4,Tree]*TW_DistAxialTransp[Tree,Zn4])/ARRAYSUM(Rt_TLra[*,Tree]) else 0


zonetree_df$Rt_TLra_tw <- zonetree_df$Rt_TLra * zonetree_df$TW_DistAxialTransp
tree_df$Rt_TLra <- aggregate(zonetree_df["Rt_TLra"], zonetree_df["tree_id"], sum)$Rt_TLra
tree_df$Rt_TLra_tw <- aggregate(zonetree_df["Rt_TLra_tw"], zonetree_df["tree_id"], sum)$Rt_TLra_tw
tree_df$TW_MeanDist <- ifelse(tree_df$Rt_TLra > 0, tree_df$Rt_TLra_tw /
                                tree_df$Rt_TLra, 0)

# TW_PotLongitudinal[Tree] = if TW_MeanDist[Tree]>0 then -TW_DemandPot[Tree]/(TW_MeanDist[Tree]*TW_ResistFact[Tree]) else 0
tree_df$TW_PotLongitudinal <- ifelse(
  tree_df$TW_MeanDist > 0,
  -tree_df$TW_DemandPot / (tree_df$TW_MeanDist * tree_df$TW_ResistFact),
  0
)

treebuf_df$TW_PotRadial <- rep(tree_df$TW_PotRadial, nrow(buf_df))
treebuf_df$TW_PotLongitudinal <- rep(tree_df$TW_PotLongitudinal, nrow(buf_df))

# TW_PotRange[Tree,BufValues] = TW_PotSoilMeanPerceived[Tree,BufValues]+TW_PotRadial[Tree]+TW_PotLongitudinal[Tree]
treebuf_df$TW_PotRange <- treebuf_df$TW_PotSoilMeanPerceived + treebuf_df$TW_PotRadial + treebuf_df$TW_PotLongitudinal

# TW_m[Tree] = IF((TW_PotSuctAlphMin[Tree] <>0) AND(1-TW_Alpha)>0 AND TW_Alpha/(1-TW_Alpha)>0 AND (TW_PotSuctAlphMax[Tree]/TW_PotSuctAlphMin[Tree])>0 )  THEN  (2*LOGN(TW_Alpha/(1-TW_Alpha))/LOGN(TW_PotSuctAlphMax[Tree]/TW_PotSuctAlphMin[Tree]))ELSE(0)
tree_df$TW_m <- ifelse(
  tree_df$TW_PotSuctAlphMin != 0 &
    (1 - pars$T_par$TW_Alpha) > 0 &
    pars$T_par$TW_Alpha / (1 - pars$T_par$TW_Alpha) > 0 &
    (tree_df$TW_PotSuctAlphMax / tree_df$TW_PotSuctAlphMin) >
    0,
  2 * log(pars$T_par$TW_Alpha / (1 - pars$T_par$TW_Alpha)) /
    log(tree_df$TW_PotSuctAlphMax / tree_df$TW_PotSuctAlphMin),
  0
)


treebuf_df$TW_m <- rep(tree_df$TW_m, nrow(buf_df))
treebuf_df$TW_PotSuctHalf <- rep(tree_df$TW_PotSuctHalf, nrow(buf_df))
treebuf_df$TW_DemandPot <- rep(tree_df$TW_DemandPot, nrow(buf_df))

# TW_DemandRedFacRange[Tree,BufValues] = IF(TW_PotSuctHalf[Tree]<>0) and (TW_PotRange[Tree,BufValues]<>0 and (1+(min(0,(TW_PotRange[Tree,BufValues]))/TW_PotSuctHalf[Tree])^TW_m[Tree])<>0)
# THEN(1/(1+(min(0,(TW_PotRange[Tree,BufValues]))/TW_PotSuctHalf[Tree])^TW_m[Tree])) - TW_DemActSubtract[BufValues]ELSE(0)
treebuf_df$TW_DemandRedFacRange <- ifelse(
  treebuf_df$TW_PotSuctHalf != 0 &
    treebuf_df$TW_PotRange != 0 &
    (1 + (
      pmin(0, treebuf_df$TW_PotRange) / treebuf_df$TW_PotSuctHalf
    )^treebuf_df$TW_m) != 0,
  (1 / (
    1 + (pmin(0, treebuf_df$TW_PotRange) / treebuf_df$TW_PotSuctHalf)^treebuf_df$TW_m
  )) - treebuf_df$TW_DemActSubtract,
  0
)


# TW_DemandActRange[Tree,BufValues] = TW_DemandPot[Tree]*TW_DemandRedFacRange[Tree,BufValues]
treebuf_df$TW_DemandActRange <- treebuf_df$TW_DemandPot * treebuf_df$TW_DemandRedFacRange

# TW_DistAxialTransp[Tree,Zone] = if AF_TreePosit4Q[Tree] = 0 then (Rt_ZoneLeft[Zone]+Rt_ZoneRight[Zone])/2 else AF_ZoneTot-(Rt_ZoneLeft[Zone]+Rt_ZoneRight[Zone])/2
# Tw_DistAxialTranspMean[Tree] = ARRAYMEAN(TW_DistAxialTransp[Tree,*])
zonetree_df$TW_DistAxialTransp <- ifelse(
  zonetree_df$AF_TreePosit4Q == 0,
  (zonetree_df$Rt_ZoneLeft + zonetree_df$Rt_ZoneRight) /
    2,
  AF_ZoneTot - (zonetree_df$Rt_ZoneLeft +
                  zonetree_df$Rt_ZoneRight) / 2
)

tree_df$Tw_DistAxialTranspMean <- aggregate(zonetree_df["TW_DistAxialTransp"], zonetree_df["tree_id"], sum)$TW_DistAxialTransp

zonetreebuf_df$Tw_DistAxialTranspMean <- rep(rep(tree_df$Tw_DistAxialTranspMean, each = nzone),
                                             nrow(buf_df))
zonetreebuf_df$TW_PotRange <- rep(treebuf_df$TW_PotRange, each = nzone)
zonetreebuf_df$TW_DemandRedFacRange <- rep(treebuf_df$TW_DemandRedFacRange, each = nzone)
zonetreebuf_df$TW_PotRadial <- rep(treebuf_df$TW_PotRadial, each = nzone)
zonetreebuf_df$TW_PotLongitudinal <- rep(treebuf_df$TW_PotLongitudinal, each = nzone)
zonetreebuf_df$TW_DistAxialTransp <- rep(zonetree_df$TW_DistAxialTransp, nrow(buf_df))

# TW_PotRhizOptT1[Zone,BufValues] = TW_PotRange[Sp1,BufValues]-(1-TW_DemandRedFacRange[Sp1,BufValues])*(TW_PotRadial[Sp1]+TW_PotLongitudinal[Sp1])-
#   (TW_DistAxialTransp[Sp1,Zone]/Tw_DistAxialTranspMean[Sp1])*TW_DemandRedFacRange[Sp1,BufValues]*TW_PotLongitudinal[Sp1]
# TW_PotRhizOptT2[Zone,BufValues] = TW_PotRange[Sp2,BufValues]-(1-TW_DemandRedFacRange[Sp2,BufValues])*(TW_PotRadial[Sp2]+TW_PotLongitudinal[Sp2])-
#   (TW_DistAxialTransp[Sp2,Zone]/Tw_DistAxialTranspMean[Sp2])*TW_DemandRedFacRange[Sp2,BufValues]*TW_PotLongitudinal[Sp2]
# TW_PotRhizOptT3[Zone,BufValues] = TW_PotRange[Sp3,BufValues]-(1-TW_DemandRedFacRange[Sp3,BufValues])*(TW_PotRadial[Sp3]+TW_PotLongitudinal[Sp3])-
#   (TW_DistAxialTransp[Sp3,Zone]/Tw_DistAxialTranspMean[Sp3])*TW_DemandRedFacRange[Sp3,BufValues]*TW_PotLongitudinal[Sp3]-TW_DemandRedFacRange[Sp3,BufValues]*TW_PotRadial[Sp3]

zonetreebuf_df$TW_PotRhizOpt <- zonetreebuf_df$TW_PotRange - (1 - zonetreebuf_df$TW_DemandRedFacRange) *
  (zonetreebuf_df$TW_PotRadial + zonetreebuf_df$TW_PotLongitudinal) -
  (zonetreebuf_df$TW_DistAxialTransp / zonetreebuf_df$Tw_DistAxialTranspMean) * zonetreebuf_df$TW_DemandRedFacRange * zonetreebuf_df$TW_PotLongitudinal
zt3b_df <- zonetreebuf_df[zonetreebuf_df$tree_id == 3, ]
zonetreebuf_df[zonetreebuf_df$tree_id == 3, ]$TW_PotRhizOpt <- zt3b_df$TW_PotRhizOpt - zt3b_df$TW_DemandRedFacRange * zt3b_df$TW_PotRadial

df <- zonelayertree_df[c("zone", "layer", "tree_id", "W_Alpha", "W_n", "W_ThetaSat")]
zonelayertreebuf_df <- df[rep(seq_len(nrow(df)), nrow(buf_df)), ]
zonelayertreebuf_df$buf_id <- rep(buf_df$buf_id, each = nrow(zonelayertree_df))

df <- zonetreebuf_df[c("zone", "tree_id", "buf_id", "TW_PotRhizOpt")]
df <- df[rep(seq_len(nrow(df)), nlayer), ]
df$layer <- rep(layer_df$layer, each = nrow(zonetreebuf_df))
df <- df[order(df$buf_id, df$tree_id, df$layer, df$zone), ]
zonelayertreebuf_df$TW_PotRhizOpt <- df$TW_PotRhizOpt

# TW_PotRhizThetaT1L1[Zone,BufValues] = if (1+(ABS(W_Alpha1*TW_PotRhizOptT1[Zone,BufValues]))^W_n1)^(1-1/W_n1)<>0 then W_ThetaSat1/(1+(ABS(W_Alpha1*TW_PotRhizOptT1[Zone,BufValues]))^W_n1)^(1-1/W_n1) else 0
# TW_PotRhizThetaT1L2[Zone,BufValues] = if (1+(ABS(W_Alpha2*TW_PotRhizOptT1[Zone,BufValues]))^W_n2)^(1-1/W_n2)<>0 then W_ThetaSat2 /(1+(ABS(W_Alpha2*TW_PotRhizOptT1[Zone,BufValues]))^W_n2)^(1-1/W_n2) else 0
# TW_PotRhizThetaT1L3[Zone,BufValues] = if (1+(ABS(W_Alpha3*TW_PotRhizOptT1[Zone,BufValues]))^W_n3)^(1-1/W_n3)<>0 then W_ThetaSat3 /(1+(ABS(W_Alpha3*TW_PotRhizOptT1[Zone,BufValues]))^W_n3)^(1-1/W_n3) else 0
# TW_PotRhizThetaT1L4[Zone,BufValues] = if (1+(ABS(W_Alpha4*TW_PotRhizOptT1[Zone,BufValues]))^W_n4)^(1-1/W_n4)<>0 then W_ThetaSat4 /(1+(ABS(W_Alpha4*TW_PotRhizOptT1[Zone,BufValues]))^W_n4)^(1-1/W_n4) else 0
# TW_PotRhizThetaT2L1[Zone,BufValues] = If (1+(ABS(W_Alpha1*TW_PotRhizOptT2[Zone,BufValues]))^W_n1)^(1-1/W_n1)<>0 then W_ThetaSat1/(1+(ABS(W_Alpha1*TW_PotRhizOptT2[Zone,BufValues]))^W_n1)^(1-1/W_n1) else 0
# TW_PotRhizThetaT2L2[Zone,BufValues] = if (1+(ABS(W_Alpha2*TW_PotRhizOptT2[Zone,BufValues]))^W_n2)^(1-1/W_n2)<>0 then W_ThetaSat2 /(1+(ABS(W_Alpha2*TW_PotRhizOptT2[Zone,BufValues]))^W_n2)^(1-1/W_n2) else 0
# TW_PotRhizThetaT2L3[Zone,BufValues] = if (1+(ABS(W_Alpha3*TW_PotRhizOptT2[Zone,BufValues]))^W_n3)^(1-1/W_n3)<>0 then W_ThetaSat3 /(1+(ABS(W_Alpha3*TW_PotRhizOptT2[Zone,BufValues]))^W_n3)^(1-1/W_n3) else 0
# TW_PotRhizThetaT2L4[Zone,BufValues] = if (1+(ABS(W_Alpha4*TW_PotRhizOptT2[Zone,BufValues]))^W_n4)^(1-1/W_n4)<>0 then W_ThetaSat4 /(1+(ABS(W_Alpha4*TW_PotRhizOptT2[Zone,BufValues]))^W_n4)^(1-1/W_n4) else 0
# TW_PotRhizThetaT3L1[Zone,BufValues] = if (1+(ABS(W_Alpha1*TW_PotRhizOptT3[Zone,BufValues]))^W_n1)^(1-1/W_n1)<>0 then W_ThetaSat1/(1+(ABS(W_Alpha1*TW_PotRhizOptT3[Zone,BufValues]))^W_n1)^(1-1/W_n1) else 0
# TW_PotRhizThetaT3L2[Zone,BufValues] = if (1+(ABS(W_Alpha2*TW_PotRhizOptT3[Zone,BufValues]))^W_n2)^(1-1/W_n2)<>0 then W_ThetaSat2 /(1+(ABS(W_Alpha2*TW_PotRhizOptT3[Zone,BufValues]))^W_n2)^(1-1/W_n2) else 0
# TW_PotRhizThetaT3L3[Zone,BufValues] = if (1+(ABS(W_Alpha3*TW_PotRhizOptT3[Zone,BufValues]))^W_n3)^(1-1/W_n3)<>0 then W_ThetaSat3 /(1+(ABS(W_Alpha3*TW_PotRhizOptT3[Zone,BufValues]))^W_n3)^(1-1/W_n3) else 0
# TW_PotRhizThetaT3L4[Zone,BufValues] = if (1+(ABS(W_Alpha4*TW_PotRhizOptT3[Zone,BufValues]))^W_n4)^(1-1/W_n4)<>0 then W_ThetaSat4 /(1+(ABS(W_Alpha4*TW_PotRhizOptT3[Zone,BufValues]))^W_n4)^(1-1/W_n4) else 0

zonelayertreebuf_df$TW_PotRhizTheta_a <- (
  1 + abs(
    zonelayertreebuf_df$W_Alpha * zonelayertreebuf_df$TW_PotRhizOpt
  )^zonelayertreebuf_df$W_n
)^(1 - 1 / zonelayertreebuf_df$W_n)

zonelayertreebuf_df$TW_PotRhizTheta <- ifelse(
  zonelayertreebuf_df$TW_PotRhizTheta_a != 0,
  zonelayertreebuf_df$W_ThetaSat / zonelayertreebuf_df$TW_PotRhizTheta_a,
  0
)



zonelayer_df$Rt_CParasitFrac <- rep(zone_df$Rt_CParasitFrac, nlayer)

# Rt_CLrv1[Zone] = Rt_CLrvM1[Zone]*(1-Rt_CParasitFrac[Zone])
# Rt_CLrv2[Zone] = Rt_CLrvM2[Zone]*(1-Rt_CParasitFrac[Zone])
# Rt_CLrv3[Zone] = Rt_CLrvM3[Zone]*(1-Rt_CParasitFrac[Zone])
# Rt_CLrv4[Zone] = Rt_CLrvM4[Zone]*(1-Rt_CParasitFrac[Zone])
zonelayer_df$Rt_CLrv <- zonelayer_df$Rt_CLrvM * (1 - zonelayer_df$Rt_CParasitFrac)


#
# Rt_T_RelImp_L1[Tree,Zone] = if (CW_DemandPerRoot[Zone]*Rt_CLrv1[Zone]
#   +TW_DemandPerRoot[Sp1]*Rt_TLrv1[Zone,Sp1]+
#   +TW_DemandPerRoot[Sp2]*Rt_TLrv1[Zone,Sp2]+
#   +TW_DemandPerRoot[Sp3]*Rt_TLrv1[Zone,Sp3]) = 0 then 1 else TW_DemandPerRoot[Tree]*Rt_TLrv1[Zone,Tree]
# /(CW_DemandPerRoot[Zone]*Rt_CLrv1[Zone]
#   +TW_DemandPerRoot[Sp1]*Rt_TLrv1[Zone,Sp1]+
# +TW_DemandPerRoot[Sp2]*Rt_TLrv1[Zone,Sp2]+
# +TW_DemandPerRoot[Sp3]*Rt_TLrv1[Zone,Sp3])
#
# Rt_T_RelImp_L2[Tree,Zone] = if (CW_DemandPerRoot[Zone]*Rt_CLrv2[Zone]
#   +TW_DemandPerRoot[Sp1]*Rt_TLrv2[Zone,Sp1]+
#   +TW_DemandPerRoot[Sp2]*Rt_TLrv2[Zone,Sp2]+
#   +TW_DemandPerRoot[Sp3]*Rt_TLrv2[Zone,Sp3]) = 0 then 1 else TW_DemandPerRoot[Tree]*Rt_TLrv2[Zone,Tree]
# /(CW_DemandPerRoot[Zone]*Rt_CLrv2[Zone]
#   +TW_DemandPerRoot[Sp1]*Rt_TLrv2[Zone,Sp1]+
# +TW_DemandPerRoot[Sp2]*Rt_TLrv2[Zone,Sp2]+
# +TW_DemandPerRoot[Sp3]*Rt_TLrv2[Zone,Sp3])
#
# Rt_T_RelImp_L3[Tree,Zone] = if (CW_DemandPerRoot[Zone]*Rt_CLrv3[Zone]
#   +TW_DemandPerRoot[Sp1]*Rt_TLrv3[Zone,Sp1]+
#   +TW_DemandPerRoot[Sp2]*Rt_TLrv3[Zone,Sp2]+
#   +TW_DemandPerRoot[Sp3]*Rt_TLrv3[Zone,Sp3]) = 0 then 1 else TW_DemandPerRoot[Tree]*Rt_TLrv3[Zone,Tree]
# /(CW_DemandPerRoot[Zone]*Rt_CLrv3[Zone]
#   +TW_DemandPerRoot[Sp1]*Rt_TLrv3[Zone,Sp1]+
# +TW_DemandPerRoot[Sp2]*Rt_TLrv3[Zone,Sp2]+
# +TW_DemandPerRoot[Sp3]*Rt_TLrv3[Zone,Sp3])
#
# Rt_T_RelImp_L4[Tree,Zone] = if (CW_DemandPerRoot[Zone]*Rt_CLrv4[Zone]
#   +TW_DemandPerRoot[Sp1]*Rt_TLrv4[Zone,Sp1]+
#   +TW_DemandPerRoot[Sp2]*Rt_TLrv4[Zone,Sp2]+
#   +TW_DemandPerRoot[Sp3]*Rt_TLrv4[Zone,Sp3]) = 0 then 1 else TW_DemandPerRoot[Tree]*Rt_TLrv4[Zone,Tree]
# /(CW_DemandPerRoot[Zone]*Rt_CLrv4[Zone]
#   +TW_DemandPerRoot[Sp1]*Rt_TLrv4[Zone,Sp1]+
# +TW_DemandPerRoot[Sp2]*Rt_TLrv4[Zone,Sp2]+
# +TW_DemandPerRoot[Sp3]*Rt_TLrv4[Zone,Sp3])


zonelayer_df$CW_DemandPerRoot <- rep(zone_df$CW_DemandPerRoot, nlayer)
zonelayertree_df$TW_DemandPerRoot <- rep(tree_df$TW_DemandPerRoot, each = nzone *
                                           nlayer)

zlt1 <- zonelayertree_df[zonelayertree_df$tree_id == 1, ]
zlt2 <- zonelayertree_df[zonelayertree_df$tree_id == 2, ]
zlt3 <- zonelayertree_df[zonelayertree_df$tree_id == 3, ]

zonelayer_df$Rt_T_RelImp_a <- zonelayer_df$CW_DemandPerRoot * zonelayer_df$Rt_CLrv +
  zlt1$TW_DemandPerRoot * zlt1$Rt_TLrv + zlt2$TW_DemandPerRoot * zlt2$Rt_TLrv + zlt3$TW_DemandPerRoot * zlt3$Rt_TLrv

zonelayertree_df$Rt_T_RelImp_a <- rep(zonelayer_df$Rt_T_RelImp_a, ntree)
zonelayertree_df$Rt_T_RelImp <- ifelse(
  zonelayertree_df$Rt_T_RelImp_a == 0,
  1,
  zonelayertree_df$TW_DemandPerRoot * zonelayertree_df$Rt_TLrv / zonelayertree_df$Rt_T_RelImp_a
)


zonelayertree_df$Rt_TLrv_by_TD <- zonelayertree_df$Rt_TLrv * sqrt(zonelayertree_df$Rt_TDiam)
tree_agg_df <- aggregate(zonelayertree_df[c("Rt_TLrv", "Rt_TLrv_by_TD")], zonelayertree_df[c("zone", "layer")], sum)
zonelayer_df$Rt_TLrv_sum <- tree_agg_df$Rt_TLrv
zonelayer_df$Rt_TLrv_by_TD_sum <- tree_agg_df$Rt_TLrv_by_TD

# Rt_TCDiam1[Zone] = IF((Rt_CLrv1[Zone]+ARRAYSUM(Rt_TLrv1[Zone,*]))>0)THEN(((Rt_CLrv1[Zone]*SQRT(Cq_RtDiam[Zone])+Rt_TLrv1[Zone,Sp1]*SQRT(Rt_TDiam[Sp1])+Rt_TLrv1[Zone,Sp2]*SQRT(Rt_TDiam[Sp2])+Rt_TLrv1[Zone,Sp3]*SQRT(Rt_TDiam[Sp3]))/(Rt_CLrv1[Zone]+ARRAYSUM(Rt_TLrv1[Zone,*])))^2)ELSE(0)
# Rt_TCDiam2[Zone] = IF((Rt_CLrv2[Zone]+ARRAYSUM(Rt_TLrv2[Zone,*]))>0)THEN(((Rt_CLrv2[Zone]*SQRT(Cq_RtDiam[Zone])+Rt_TLrv2[Zone,Sp1]*SQRT(Rt_TDiam[Sp1])+Rt_TLrv2[Zone,Sp2]*SQRT(Rt_TDiam[Sp2])+Rt_TLrv2[Zone,Sp3]*SQRT(Rt_TDiam[Sp3]))/(Rt_CLrv2[Zone]+ARRAYSUM(Rt_TLrv2[Zone,*])))^2)ELSE(0)
# Rt_TCDiam3[Zone] = IF((Rt_CLrv3[Zone]+ARRAYSUM(Rt_TLrv3[Zone,*]))>0)THEN(((Rt_CLrv3[Zone]*SQRT(Cq_RtDiam[Zone])+Rt_TLrv3[Zone,Sp1]*SQRT(Rt_TDiam[Sp1])+Rt_TLrv3[Zone,Sp2]*SQRT(Rt_TDiam[Sp2])+Rt_TLrv3[Zone,Sp3]*SQRT(Rt_TDiam[Sp3]))/(Rt_CLrv3[Zone]+ARRAYSUM(Rt_TLrv3[Zone,*])))^2)ELSE(0)
# Rt_TCDiam4[Zone] = IF((Rt_CLrv4[Zone]+ARRAYSUM(Rt_TLrv4[Zone,*]))>0)THEN(((Rt_CLrv4[Zone]*SQRT(Cq_RtDiam[Zone])+Rt_TLrv4[Zone,Sp1]*SQRT(Rt_TDiam[Sp1])+Rt_TLrv4[Zone,Sp2]*SQRT(Rt_TDiam[Sp2])+Rt_TLrv4[Zone,Sp3]*SQRT(Rt_TDiam[Sp3]))/(Rt_CLrv4[Zone]+ARRAYSUM(Rt_TLrv4[Zone,*])))^2)ELSE(0)
zonelayer_df$Rt_TCDiam <- ifelse((zonelayer_df$Rt_CLrv + zonelayer_df$Rt_TLrv_sum) >
                                   0, (((
                                     zonelayer_df$Rt_CLrv * sqrt(zonelayer_df$Cq_RtDiam) + zonelayer_df$Rt_TLrv_by_TD_sum
                                   ) / (zonelayer_df$Rt_CLrv + zonelayer_df$Rt_TLrv_sum)
                                   ))^2, 0)

# Rt_RhoTot1[Zone] = if Rt_TCDiam1[Zone]*.5*SQRT(PI*(Rt_CLrv1[Zone]+ARRAYSUM(Rt_TLrv1[Zone,*]))) > 0 then
# 1/(Rt_TCDiam1[Zone]*.5*SQRT(PI*(Rt_CLrv1[Zone]+ARRAYSUM(Rt_TLrv1[Zone,*])))) else Rt_StopGap
# Rt_RhoTot2[Zone] = if Rt_TCDiam2[Zone]*.5*SQRT(PI*(Rt_CLrv2[Zone]+ARRAYSUM(Rt_TLrv2[Zone,*]))) > 0 then
# 1/(Rt_TCDiam2[Zone]*.5*SQRT(PI*(Rt_CLrv2[Zone]+ARRAYSUM(Rt_TLrv2[Zone,*])))) else Rt_StopGap
# Rt_RhoTot3[Zone] = if Rt_TCDiam3[Zone]*.5*SQRT(PI*(Rt_CLrv3[Zone]+ARRAYSUM(Rt_TLrv3[Zone,*]))) > 0 then
# 1/(Rt_TCDiam3[Zone]*.5*SQRT(PI*(Rt_CLrv3[Zone]+ARRAYSUM(Rt_TLrv3[Zone,*])))) else Rt_StopGap
# Rt_RhoTot4[Zone] = if Rt_TCDiam4[Zone]*.5*SQRT(PI*(Rt_CLrv4[Zone]+ARRAYSUM(Rt_TLrv4[Zone,*]))) >0 then
# 1/(Rt_TCDiam4[Zone]*.5*SQRT(PI*(Rt_CLrv4[Zone]+ARRAYSUM(Rt_TLrv4[Zone,*])))) else Rt_StopGap

zonelayer_df$Rt_RhoTot_a <- zonelayer_df$Rt_TCDiam * 0.5 * sqrt(pi * (zonelayer_df$Rt_CLrv + zonelayer_df$Rt_TLrv_sum))
zonelayer_df$Rt_RhoTot <- ifelse(zonelayer_df$Rt_RhoTot_a > 0,
                                 1 / zonelayer_df$Rt_RhoTot_a,
                                 pars$Rt_par$Rt_StopGap)



# Rt_TC_G1[Zone] = if Rt_RhoTot1[Zone] <> Rt_StopGap then
# (Rt_RhoTot1[Zone]^2-1)/(0.5*(((1-(3*Rt_RhoTot1[Zone]^2))/4)+((Rt_RhoTot1[Zone]^4*LOGN(Rt_RhoTot1[Zone]))/(Rt_RhoTot1[Zone]^2-1)))) else 0
# Rt_TC_G2[Zone] = if Rt_RhoTot2[Zone] <> Rt_StopGap then
# (Rt_RhoTot2[Zone]^2-1)/(0.5*(((1-(3*Rt_RhoTot2[Zone]^2))/4)+((Rt_RhoTot2[Zone]^4*LOGN(Rt_RhoTot2[Zone]))/(Rt_RhoTot2[Zone]^2-1)))) else 0
# Rt_TC_G3[Zone] = if Rt_RhoTot3[Zone] <> Rt_StopGap then
# (Rt_RhoTot3[Zone]^2-1)/(0.5*(((1-(3*Rt_RhoTot3[Zone]^2))/4)+((Rt_RhoTot3[Zone]^4*LOGN(Rt_RhoTot3[Zone]))/(Rt_RhoTot3[Zone]^2-1)))) else 0
# Rt_TC_G4[Zone] = if Rt_RhoTot4[Zone] <> Rt_StopGap then
# (Rt_RhoTot4[Zone]^2-1)/(0.5*(((1-(3*Rt_RhoTot4[Zone]^2))/4)+((Rt_RhoTot4[Zone]^4*LOGN(Rt_RhoTot4[Zone]))/(Rt_RhoTot4[Zone]^2-1)))) else 0

zonelayer_df$Rt_TC_G <- ifelse(
  zonelayer_df$Rt_RhoTot != pars$Rt_par$Rt_StopGap,
  (zonelayer_df$Rt_RhoTot^2 - 1) / (0.5 * (((
    1 - (3 * zonelayer_df$Rt_RhoTot^2)
  ) / 4) + ((zonelayer_df$Rt_RhoTot^4 * log(zonelayer_df$Rt_RhoTot)) / (zonelayer_df$Rt_RhoTot^2 - 1)
  ))),
  0
)




zonelayer_df$W_PhiTheta <- NA
for (i in 1:nrow(zonelayer_df)) {
  zonelayer_df[i, ]$W_PhiTheta <- get_graph_y(
    W_PhiTheta_df,
    zonelayer_df[i, ]$W_Theta,
    x_column = "Theta",
    y_column = names(W_PhiTheta_df)[zonelayer_df[i, ]$layer],
    mode = "continues"
  )
}

zonelayertreebuf_df$Rt_TC_G <- rep(zonelayer_df$Rt_TC_G, ntree * nrow(buf_df))
zonelayertreebuf_df$W_PhiTheta <- rep(zonelayer_df$W_PhiTheta, ntree * nrow(buf_df))

# TW_pFPotRhizOptT1[Zone,BufValues] = IF TW_PotRhizOptT1[Zone,BufValues]<0 THEN LOG10(-TW_PotRhizOptT1[Zone,BufValues])ELSE 0
# TW_pFPotRhizOptT2[Zone,BufValues] = IF TW_PotRhizOptT2[Zone,BufValues]<0 THEN LOG10(-TW_PotRhizOptT2[Zone,BufValues])ELSE 0
# TW_pFPotRhizOptT3[Zone,BufValues] = IF TW_PotRhizOptT3[Zone,BufValues]<0 THEN LOG10(-TW_PotRhizOptT3[Zone,BufValues])ELSE 0
zonetreebuf_df$TW_pFPotRhizOpt <- ifelse(zonetreebuf_df$TW_PotRhizOpt <
                                           0,
                                         log10(-zonetreebuf_df$TW_PotRhizOpt),
                                         0)

zonelayertreebuf_df$TW_PhiPot <- NA

for (i in 1:nrow(zonelayertreebuf_df)) {
  x <- zonetreebuf_df[zonetreebuf_df$zone == zonelayertreebuf_df[i, ]$zone &
                        zonetreebuf_df$tree_id == zonelayertreebuf_df[i, ]$tree_id &
                        zonetreebuf_df$buf_id == zonelayertreebuf_df[i, ]$buf_id, ]$TW_pFPotRhizOpt
  zonelayertreebuf_df[i, ]$TW_PhiPot <- get_TW_PhiPot(x,
                                                      zonelayertreebuf_df[i, ]$layer,
                                                      zonelayertreebuf_df[i, ]$tree_id)
}

# W_PotUptOptT1L1[Zone,BufValues] = IF Rt_TC_G1[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta1[Zone]-TW_PhiPotT1L1[Zone,BufValues]))*Rt_TC_G1[Zone]*Rt_TLrv1[Zone,Sp1] ELSE 0
# W_PotUptOptT1L2[Zone,BufValues] = IF Rt_TC_G2[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta2[Zone]-TW_PhiPotT1L2[Zone,BufValues]))*Rt_TC_G2[Zone]*Rt_TLrv2[Zone,Sp1] ELSE 0
# W_PotUptOptT1L3[Zone,BufValues] = IF Rt_TC_G3[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta3[Zone]-TW_PhiPotT1L3[Zone,BufValues]))*Rt_TC_G3[Zone]*Rt_TLrv3[Zone,Sp1] ELSE 0
# W_PotUptOptT1L4[Zone,BufValues] = IF Rt_TC_G4[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta4[Zone]-TW_PhiPotT1L4[Zone,BufValues]))*Rt_TC_G4[Zone]*Rt_TLrv4[Zone,Sp1] ELSE 0
# W_PotUptOptT2L1[Zone,BufValues] = IF Rt_TC_G1[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta1[Zone]-TW_PhiPotT2L1[Zone,BufValues]))*Rt_TC_G1[Zone]*Rt_TLrv1[Zone,Sp2] ELSE 0
# W_PotUptOptT2L2[Zone,BufValues] = IF Rt_TC_G2[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta2[Zone]-TW_PhiPotT2L2[Zone,BufValues]))*Rt_TC_G2[Zone]*Rt_TLrv2[Zone,Sp2] ELSE 0
# W_PotUptOptT2L3[Zone,BufValues] = IF Rt_TC_G3[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta3[Zone]-TW_PhiPotT2L3[Zone,BufValues]))*Rt_TC_G3[Zone]*Rt_TLrv3[Zone,Sp2] ELSE 0
# W_PotUptOptT2L4[Zone,BufValues] = IF Rt_TC_G4[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta4[Zone]-TW_PhiPotT2L4[Zone,BufValues]))*Rt_TC_G4[Zone]*Rt_TLrv4[Zone,Sp2] ELSE 0
# W_PotUptOptT3L1[Zone,BufValues] = IF Rt_TC_G1[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta1[Zone]-TW_PhiPotT3L1[Zone,BufValues]))*Rt_TC_G1[Zone]*Rt_TLrv1[Zone,Sp3] ELSE 0
# W_PotUptOptT3L2[Zone,BufValues] = IF Rt_TC_G2[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta2[Zone]-TW_PhiPotT3L2[Zone,BufValues]))*Rt_TC_G2[Zone]*Rt_TLrv2[Zone,Sp3] ELSE 0
# W_PotUptOptT3L3[Zone,BufValues] = IF Rt_TC_G3[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta3[Zone]-TW_PhiPotT3L3[Zone,BufValues]))*Rt_TC_G3[Zone]*Rt_TLrv3[Zone,Sp3] ELSE 0
# W_PotUptOptT3L4[Zone,BufValues] = IF Rt_TC_G4[Zone]<>0 THEN 10*PI*(max(0,+W_PhiTheta4[Zone]-TW_PhiPotT3L4[Zone,BufValues]))*Rt_TC_G4[Zone]*Rt_TLrv4[Zone,Sp3] ELSE 0

zonelayertreebuf_df$Rt_TLrv <- rep(zonelayertree_df$Rt_TLrv, nrow(buf_df))
zonelayertreebuf_df$W_PotUptOpt <- ifelse(
  zonelayertreebuf_df$Rt_TC_G != 0,
  10 * pi * (
    pmax(
      0,
      zonelayertreebuf_df$W_PhiTheta -
        zonelayertreebuf_df$TW_PhiPot
    )
  ) * zonelayertreebuf_df$Rt_TC_G * zonelayertreebuf_df$Rt_TLrv,
  0
)


# W_PotUptOptT1profileZn[Zone,BufValues] = if TW_Water_Limited?[Zone]=1 then Max(0,min((W_Theta1[Zone]-TW_PotRhizThetaT1L1[Zone,BufValues])*AF_DepthAct1[Zone]*1000,100*Rt_T_RelImp_L1[Sp1,Zone]*AF_DepthAct1[Zone]*W_PotUptOptT1L1[Zone,BufValues]))+Max(0,min((W_Theta2[Zone]-Tw_PotRhizThetaT1L2[Zone,BufValues])*AF_Depth2[Zone]*1000,100*Rt_T_RelImp_L2[Sp1,Zone]*AF_Depth2[Zone]*W_PotUptOptT1L2[Zone,BufValues]))+Max(0,min((W_Theta3[Zone]-Tw_PotRhizThetaT1L3[Zone,BufValues])*AF_Depth3[Zone]*1000,100*Rt_T_RelImp_L3[Sp1,Zone]*AF_Depth3[Zone]*W_PotUptOptT1L3[Zone,BufValues]))+Max(0,min((W_Theta4[Zone]-Tw_PotRhizThetaT1L4[Zone,BufValues])*AF_Depth4[Zone]*1000,100*Rt_T_RelImp_L4[Sp1,Zone]*AF_Depth4[Zone]*W_PotUptOptT1L4[Zone,BufValues])) else Max(0,100*Rt_T_RelImp_L1[Sp1,Zone]*AF_DepthAct1[Zone]*W_PotUptOptT1L1[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L2[Sp1,Zone]*AF_Depth2[Zone]*W_PotUptOptT1L2[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L3[Sp1,Zone]*AF_Depth3[Zone]*W_PotUptOptT1L3[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L4[Sp1,Zone]*AF_Depth4[Zone]*W_PotUptOptT1L4[Zone,BufValues])
# W_PotUptOptT2ProfileZn[Zone,BufValues] = if TW_Water_Limited?[Zone]=1 then Max(0,min((W_Theta1[Zone]-Tw_PotRhizThetaT2L1[Zone,BufValues])*AF_DepthAct1[Zone]*1000,100*Rt_T_RelImp_L1[Sp2,Zone]*AF_DepthAct1[Zone]*W_PotUptOptT2L1[Zone,BufValues]))+Max(0,min((W_Theta2[Zone]-Tw_PotRhizThetaT2L2[Zone,BufValues])*AF_Depth2[Zone]*1000,100*Rt_T_RelImp_L2[Sp2,Zone]*AF_Depth2[Zone]*W_PotUptOptT2L2[Zone,BufValues]))+Max(0,min((W_Theta3[Zone]-Tw_PotRhizThetaT2L3[Zone,BufValues])*AF_Depth3[Zone]*1000,100*Rt_T_RelImp_L3[Sp2,Zone]*AF_Depth3[Zone]*W_PotUptOptT2L3[Zone,BufValues]))+Max(0,min((W_Theta4[Zone]-Tw_PotRhizThetaT2L4[Zone,BufValues])*AF_Depth4[Zone]*1000,100*Rt_T_RelImp_L4[Sp2,Zone]*AF_Depth4[Zone]*W_PotUptOptT2L4[Zone,BufValues])) else Max(0,100*Rt_T_RelImp_L1[Sp2,Zone]*AF_DepthAct1[Zone]*W_PotUptOptT2L1[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L2[Sp2,Zone]*AF_Depth2[Zone]*W_PotUptOptT2L2[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L3[Sp2,Zone]*AF_Depth3[Zone]*W_PotUptOptT2L3[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L4[Sp2,Zone]*AF_Depth4[Zone]*W_PotUptOptT2L4[Zone,BufValues])
# W_PotUptOptT3ProfileZn[Zone,BufValues] = if TW_Water_Limited?[Zone]=1 then Max(0,min((W_Theta1[Zone]-Tw_PotRhizThetaT3L1[Zone,BufValues])*AF_DepthAct1[Zone]*1000,100*Rt_T_RelImp_L1[Sp3,Zone]*AF_DepthAct1[Zone]*W_PotUptOptT3L1[Zone,BufValues]))+Max(0,min((W_Theta2[Zone]-Tw_PotRhizThetaT3L2[Zone,BufValues])*AF_Depth2[Zone]*1000,100*Rt_T_RelImp_L2[Sp3,Zone]*AF_Depth2[Zone]*W_PotUptOptT3L2[Zone,BufValues]))+Max(0,min((W_Theta3[Zone]-Tw_PotRhizThetaT3L3[Zone,BufValues])*AF_Depth3[Zone]*1000,100*Rt_T_RelImp_L3[Sp3,Zone]*AF_Depth3[Zone]*W_PotUptOptT3L3[Zone,BufValues]))+Max(0,min((W_Theta4[Zone]-Tw_PotRhizThetaT3L4[Zone,BufValues])*AF_Depth4[Zone]*1000,100*Rt_T_RelImp_L4[Sp3,Zone]*AF_Depth4[Zone]*W_PotUptOptT3L4[Zone,BufValues])) else Max(0,100*Rt_T_RelImp_L1[Sp3,Zone]*AF_DepthAct1[Zone]*W_PotUptOptT3L1[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L2[Sp3,Zone]*AF_Depth2[Zone]*W_PotUptOptT3L2[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L3[Sp3,Zone]*AF_Depth3[Zone]*W_PotUptOptT3L3[Zone,BufValues])+Max(0,100*Rt_T_RelImp_L4[Sp3,Zone]*AF_Depth4[Zone]*W_PotUptOptT3L4[Zone,BufValues])

zonelayertreebuf_df$W_Theta <- rep(zonelayer_df$W_Theta, ntree * nrow(buf_df))
zonelayertreebuf_df$AF_Depth <- rep(zonelayer_df$AF_Depth, ntree * nrow(buf_df))
zonelayertreebuf_df$Rt_T_RelImp <- rep(zonelayertree_df$Rt_T_RelImp, nrow(buf_df))
zonetreebuf_df$TW_Water_Limited_is <- rep(zone_df$TW_Water_Limited_is, ntree * nrow(buf_df))

zonelayertreebuf_df$W_PotUptOpt_a <- pmax(
  0,
  pmin((
    zonelayertreebuf_df$W_Theta - zonelayertreebuf_df$TW_PotRhizTheta
  ) * zonelayertreebuf_df$AF_Depth * 1000,
  100 * zonelayertreebuf_df$Rt_T_RelImp *
    zonelayertreebuf_df$AF_Depth * zonelayertreebuf_df$W_PotUptOpt
  )
)
zonelayertreebuf_df$W_PotUptOpt_b <- pmax(
  0,
  100 * zonelayertreebuf_df$Rt_T_RelImp * zonelayertreebuf_df$AF_Depth * zonelayertreebuf_df$W_PotUptOpt
)
zonetreebuf_df <- cbind(zonetreebuf_df,
                        aggregate(zonelayertreebuf_df[c("W_PotUptOpt_a", "W_PotUptOpt_b")], zonelayertreebuf_df[c("zone", "tree_id", "buf_id")], sum)[c("W_PotUptOpt_a", "W_PotUptOpt_b")])

zonetreebuf_df$W_PotUptOptProfileZn <- ifelse(
  zonetreebuf_df$TW_Water_Limited_is == 1,
  zonetreebuf_df$W_PotUptOpt_a,
  zonetreebuf_df$W_PotUptOpt_b
)


# W_PotUptT1Profile[BufValues] = AF_ZoneFrac[Zn1]*W_PotUptOptT1profileZn[Zn1,BufValues]+
#   AF_ZoneFrac[Zn2]*W_PotUptOptT1profileZn[Zn2,BufValues]+
#   AF_ZoneFrac[Zn3]*W_PotUptOptT1profileZn[Zn3,BufValues]+
#   AF_ZoneFrac[Zn4]*W_PotUptOptT1profileZn[Zn4,BufValues]
# W_PotUptT2Profile[BufValues] = AF_ZoneFrac[Zn1]*W_PotUptOptT2ProfileZn[Zn1,BufValues]+
#   AF_ZoneFrac[Zn2]*W_PotUptOptT2ProfileZn[Zn2,BufValues]+
#   AF_ZoneFrac[Zn3]*W_PotUptOptT2ProfileZn[Zn3,BufValues]+
#   AF_ZoneFrac[Zn4]*W_PotUptOptT2ProfileZn[Zn4,BufValues]
# W_PotUptT3Profile[BufValues] = AF_ZoneFrac[Zn1]*W_PotUptOptT3ProfileZn[Zn1,BufValues]+
#   AF_ZoneFrac[Zn2]*W_PotUptOptT3ProfileZn[Zn2,BufValues]+
#   AF_ZoneFrac[Zn3]*W_PotUptOptT3ProfileZn[Zn3,BufValues]+
#   AF_ZoneFrac[Zn4]*W_PotUptOptT3ProfileZn[Zn4,BufValues]


zonetreebuf_df$W_PotUptProfile_a <- zonetreebuf_df$AF_ZoneFrac * zonetreebuf_df$W_PotUptOptProfileZn
treebuf_df$W_PotUptProfile <- aggregate(list(W_PotUptProfile_sum = zonetreebuf_df$W_PotUptProfile_a),
                                        zonetreebuf_df[c("tree_id", "buf_id")],
                                        sum)$W_PotUptProfile_sum


# TW_PotUptOptAll[Sp1,1] = W_PotUptT1Profile[1]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,2] = W_PotUptT1Profile[2]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,3] = W_PotUptT1Profile[3]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,4] = W_PotUptT1Profile[4]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,5] = W_PotUptT1Profile[5]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,6] = W_PotUptT1Profile[6]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,7] = W_PotUptT1Profile[7]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,8] = W_PotUptT1Profile[8]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,9] = W_PotUptT1Profile[9]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp1,10] = W_PotUptT1Profile[10]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,1] = W_PotUptT2Profile[1]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,2] = W_PotUptT2Profile[2]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,3] = W_PotUptT2Profile[3]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,4] = W_PotUptT2Profile[4]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,5] = W_PotUptT2Profile[5]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,6] = W_PotUptT2Profile[6]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,7] = W_PotUptT2Profile[7]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,8] = W_PotUptT2Profile[8]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,9] = W_PotUptT2Profile[9]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp2,10] = W_PotUptT2Profile[10]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,1] = W_PotUptT3Profile[1]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,2] = W_PotUptT3Profile[2]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,3] = W_PotUptT3Profile[3]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,4] = W_PotUptT3Profile[4]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,5] = W_PotUptT3Profile[5]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,6] = W_PotUptT3Profile[6]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,7] = W_PotUptT3Profile[7]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,8] = W_PotUptT3Profile[8]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,9] = W_PotUptT3Profile[9]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
# TW_PotUptOptAll[Sp3,10] = W_PotUptT3Profile[10]+0*(W_PotUptT1Profile[1]+W_PotUptT2Profile[1]+W_PotUptT3Profile[1])
treebuf_df$TW_PotUptOptAll <- treebuf_df$W_PotUptProfile

# TW_PU[Tree,BufValues] = min(TW_DemandActRange[Tree,BufValues],TW_PotUptOptAll[Tree,BufValues])
treebuf_df$TW_PU <- pmin(treebuf_df$TW_DemandActRange, treebuf_df$TW_PotUptOptAll)

# TW_MaxUptPot[Tree] = max(TW_PU[Tree,1],TW_PU[Tree,2],TW_PU[Tree,3],TW_PU[Tree,4],TW_PU[Tree,5],TW_PU[Tree,6],TW_PU[Tree,7],TW_PU[Tree,8],TW_PU[Tree,9],TW_PU[Tree,10])
tree_df$TW_MaxUptPot <- NA
for (i in 1:ntree) {
  tree_df[tree_df$tree_id == i, ]$TW_MaxUptPot <- max(treebuf_df[treebuf_df$tree_id == i, ]$TW_PU)
}

treebuf_df$TW_MaxUptPot <- rep(tree_df$TW_MaxUptPot, nrow(buf_df))

# TW_Best?[Sp1,1] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,1] then 1 else 0
# TW_Best?[Sp1,2] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,2] and TW_PU[Sp1,2] > TW_PU[Sp1,1] then 1 else 0
# TW_Best?[Sp1,3] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,3] and TW_PU[Sp1,3] > TW_PU[Sp1,2] then 1 else 0
# TW_Best?[Sp1,4] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,4] and TW_PU[Sp1,4] > TW_PU[Sp1,3] then 1 else 0
# TW_Best?[Sp1,5] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,5] and TW_PU[Sp1,5] > TW_PU[Sp1,4] then 1 else 0
# TW_Best?[Sp1,6] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,6] and TW_PU[Sp1,6] > TW_PU[Sp1,5] then 1 else 0
# TW_Best?[Sp1,7] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,7] and TW_PU[Sp1,7] > TW_PU[Sp1,6] then 1 else 0
# TW_Best?[Sp1,8] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,8] and TW_PU[Sp1,8] > TW_PU[Sp1,7] then 1 else 0
# TW_Best?[Sp1,9] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] = TW_PU[Sp1,9] and TW_PU[Sp1,9] > TW_PU[Sp1,8] then 1 else 0
# TW_Best?[Sp1,10] = if TW_MaxUptPot[Sp1]>0 and TW_MaxUptPot[Sp1] > TW_PU[Sp1,9]  then 1 else 0
# TW_Best?[Sp2,1] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,1] then 1 else 0
# TW_Best?[Sp2,2] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,2] and TW_PU[Sp2,2] > TW_PU[Sp2,1] then 1 else 0
# TW_Best?[Sp2,3] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,3] and TW_PU[Sp2,3] > TW_PU[Sp2,2] then 1 else 0
# TW_Best?[Sp2,4] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,4] and TW_PU[Sp2,4] > TW_PU[Sp2,3] then 1 else 0
# TW_Best?[Sp2,5] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,5] and TW_PU[Sp2,5] > TW_PU[Sp2,4] then 1 else 0
# TW_Best?[Sp2,6] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,6] and TW_PU[Sp2,6] > TW_PU[Sp2,5] then 1 else 0
# TW_Best?[Sp2,7] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,7] and TW_PU[Sp2,7] > TW_PU[Sp2,6] then 1 else 0
# TW_Best?[Sp2,8] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,8] and TW_PU[Sp2,8] > TW_PU[Sp2,7] then 1 else 0
# TW_Best?[Sp2,9] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] = TW_PU[Sp2,9] and TW_PU[Sp2,9] > TW_PU[Sp2,8] then 1 else 0
# TW_Best?[Sp2,10] = if TW_MaxUptPot[Sp2]>0 and TW_MaxUptPot[Sp2] > TW_PU[Sp2,9]  then 1 else 0
# TW_Best?[Sp3,1] = if TW_MaxUptPot[Sp3] > 0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,1] then 1 else 0
# TW_Best?[Sp3,2] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,2] and TW_PU[Sp3,2] > TW_PU[Sp3,1] then 1 else 0
# TW_Best?[Sp3,3] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,3] and TW_PU[Sp3,3] > TW_PU[Sp3,2] then 1 else 0
# TW_Best?[Sp3,4] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,4] and TW_PU[Sp3,4] > TW_PU[Sp3,3] then 1 else 0
# TW_Best?[Sp3,5] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,5] and TW_PU[Sp3,5] > TW_PU[Sp3,4] then 1 else 0
# TW_Best?[Sp3,6] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,6] and TW_PU[Sp3,6] > TW_PU[Sp3,5] then 1 else 0
# TW_Best?[Sp3,7] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,7] and TW_PU[Sp3,7] > TW_PU[Sp3,6] then 1 else 0
# TW_Best?[Sp3,8] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,8] and TW_PU[Sp3,8] > TW_PU[Sp3,7] then 1 else 0
# TW_Best?[Sp3,9] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] = TW_PU[Sp3,9] and TW_PU[Sp3,9] > TW_PU[Sp3,8] then 1 else 0
# TW_Best?[Sp3,10] = if TW_MaxUptPot[Sp3]>0 and TW_MaxUptPot[Sp3] > TW_PU[Sp3,9]  then 1 else 0

treebuf_df$TW_PU_before <- 0
treebuf_df[treebuf_df$buf_id %in% c(2:10), ]$TW_PU_before <- treebuf_df[treebuf_df$buf_id %in% c(1:9), ]$TW_PU
treebuf_df$TW_Best_is <- ifelse(
  treebuf_df$TW_MaxUptPot > 0 &
    treebuf_df$TW_MaxUptPot == treebuf_df$TW_PU &
    treebuf_df$TW_PU > treebuf_df$TW_PU_before,
  1,
  0
)
tb1 <- treebuf_df[treebuf_df$buf_id == 1, ]
tb10 <- treebuf_df[treebuf_df$buf_id == 10, ]
treebuf_df[treebuf_df$buf_id == 1, ]$TW_Best_is <- ifelse(tb1$TW_MaxUptPot > 0 &
                                                            tb1$TW_MaxUptPot == tb1$TW_PU, 1, 0)
treebuf_df[treebuf_df$buf_id == 10, ]$TW_Best_is <- ifelse(tb10$TW_MaxUptPot > 0 &
                                                             tb10$TW_MaxUptPot > tb10$TW_PU_before,
                                                           1,
                                                           0)


# TW_PotBest[Tree] = TW_Best?[Tree,1]*TW_PotRange[Tree,1]+
#   TW_Best?[Tree,2]*TW_PotRange[Tree,2]+
#   TW_Best?[Tree,3]*TW_PotRange[Tree,3]+
#   TW_Best?[Tree,4]*TW_PotRange[Tree,4]+
#   TW_Best?[Tree,5]*TW_PotRange[Tree,5]+
#   TW_Best?[Tree,6]*TW_PotRange[Tree,6]+
#   TW_Best?[Tree,7]*TW_PotRange[Tree,7]+
#   TW_Best?[Tree,8]*TW_PotRange[Tree,8]+
#   TW_Best?[Tree,9]*TW_PotRange[Tree,9]+
#   TW_Best?[Tree,10]*TW_PotRange[Tree,10]
treebuf_df$TW_PotRange_sum <- treebuf_df$TW_Best_is * treebuf_df$TW_PotRange
tree_df$TW_PotBest <- aggregate(list(x = treebuf_df$TW_PotRange_sum), treebuf_df["tree_id"], sum)$x

# TW_DemandRedFac[Tree] = IF(TW_PotSuctHalf[Tree]<>0) and (TW_PotBest[Tree]<>0 and (1+((TW_PotBest[Tree])/TW_PotSuctHalf[Tree])^TW_m[Tree])<>0)
# THEN(1/(1+((TW_PotBest[Tree])/TW_PotSuctHalf[Tree])^TW_m[Tree]))ELSE(0)
tree_df$TW_DemandRedFac <- ifelse(
  tree_df$TW_PotSuctHalf != 0 &
    tree_df$TW_PotBest != 0 &
    (1 + (
      tree_df$TW_PotBest / tree_df$TW_PotSuctHalf
    )^tree_df$TW_m) != 0,
  1 / (1 + ((tree_df$TW_PotBest) / tree_df$TW_PotSuctHalf
  )^tree_df$TW_m),
  0
)


# TW_HEqFactor[Tree] = T_RootConductivity[Tree]*PI*Rt_TDiam[Tree]*(0.5+0.5*TW_DemandRedFac[Tree])*100
tree_df$TW_HEqFactor <- tree_df$T_RootConductivity * pi * tree_df$Rt_TDiam *
  (0.5 + 0.5 * tree_df$TW_DemandRedFac) * 100


zonelayertree_df$TW_HEqFactor <- rep(tree_df$TW_HEqFactor, each = nzone *
                                       nlayer)

# TW_HEgrad1[Zone,Tree] = if (TW_EqTheta1[Zone,Tree]-W_Theta1[Zone]) <> 0 then
# (TW_PStem[Tree]-W_PTheta1[Zone])/(TW_EqTheta1[Zone,Tree]-W_Theta1[Zone]) else 1
# TW_HEgrad2[Zone,Tree] = if (TW_EqTheta2[Zone,Tree]-W_Theta2[Zone]) <> 0 then
# (TW_PStem[Tree]-W_PTheta2[Zone])/(TW_EqTheta2[Zone,Tree]-W_Theta2[Zone]) else 1
# TW_HEgrad3[Zone,Tree] = if (TW_EqTheta3[Zone,Tree]-W_Theta3[Zone]) <> 0 then
# (TW_PStem[Tree]-W_PTheta3[Zone])/(TW_EqTheta3[Zone,Tree]-W_Theta3[Zone]) else 1
# TW_HEgrad4[Zone,Tree] = if (TW_EqTheta4[Zone,Tree]-W_Theta4[Zone]) <> 0 then
# (TW_PStem[Tree]-W_PTheta4[Zone])/(TW_EqTheta4[Zone,Tree]-W_Theta4[Zone]) else 1
zonelayertree_df$W_Theta <- rep(zonelayer_df$W_Theta, ntree)

zonelayertree_df$TW_HEgrad <- ifelse(
  (zonelayertree_df$TW_EqTheta - zonelayertree_df$W_Theta) != 0,
  (zonelayertree_df$TW_PStem - zonelayertree_df$W_PTheta) / (zonelayertree_df$TW_EqTheta - zonelayertree_df$W_Theta),
  1
)



# TW_PotDelta1[Zone,Tree] = (TW_EqTheta1[Zone,Tree]-W_Theta1[Zone])*min(W_TCW_Constant, Rt_TLrv1[Zone,Tree]*TW_HEqFactor[Tree]*TW_HEgrad1[Zone,Tree])
# TW_PotDelta2[Zone,Tree] = (TW_EqTheta2[Zone,Tree]-W_Theta2[Zone])*min(W_TCW_Constant, Rt_TLrv2[Zone,Tree]*TW_HEqFactor[Tree]*TW_HEgrad2[Zone,Tree])
# TW_PotDelta3[Zone,Tree] = (TW_EqTheta3[Zone,Tree]-W_Theta3[Zone])*min(W_TCW_Constant, Rt_TLrv3[Zone,Tree]*TW_HEqFactor[Tree]*TW_HEgrad3[Zone,Tree])
# TW_PotDelta4[Zone,Tree] = (TW_EqTheta4[Zone,Tree]-W_Theta4[Zone])*min(W_TCW_Constant, Rt_TLrv4[Zone,Tree]*TW_HEqFactor[Tree]*TW_HEgrad4[Zone,Tree])
zonelayertree_df$TW_PotDelta <- (zonelayertree_df$TW_EqTheta - zonelayertree_df$W_Theta) *
  pmin(
    W_TCW_Constant,
    zonelayertree_df$Rt_TLrv * zonelayertree_df$TW_HEqFactor * zonelayertree_df$TW_HEgrad
  )

# TW_PotFlux1[Zone,Tree] = TW_PotDelta1[Zone,Tree]*AF_DepthAct1[Zone]*1000*AF_ZoneFrac[Zone]
# TW_PotFlux2[Zone,Tree] = TW_PotDelta2[Zone,Tree]*AF_Depth2[Zone]*1000*AF_ZoneFrac[Zone]
# TW_PotFlux3[Zone,Tree] = TW_PotDelta3[Zone,Tree]*AF_Depth3[Zone]*1000*AF_ZoneFrac[Zone]
# TW_PotFlux4[Zone,Tree] = TW_PotDelta4[Zone,Tree]*AF_Depth4[Zone]*1000*AF_ZoneFrac[Zone]

zonelayertree_df$TW_PotFlux <- zonelayertree_df$TW_PotDelta * zonelayertree_df$AF_Depth * 1000 * zonelayertree_df$AF_ZoneFrac

# TW_PotFluxSum[Tree] = ARRAYSUM(TW_PotFlux1[*,Tree])+ARRAYSUM(TW_PotFlux2[*,Tree])+ARRAYSUM(TW_PotFlux3[*,Tree])+ARRAYSUM(TW_PotFlux4[*,Tree])
tree_df$TW_PotFluxSum <- aggregate(list(x = zonelayertree_df$TW_PotFlux), zonelayertree_df["tree_id"], sum)$x

# TW_PotFluxSumAbs1[Zone,Tree] = ABS(TW_PotFlux1[Zone,Tree])
# TW_PotFluxSumAbs2[Zone,Tree] = ABS(TW_PotFlux2[Zone,Tree])
# TW_PotFluxSumAbs3[Zone,Tree] = ABS(TW_PotFlux3[Zone,Tree])
# TW_PotFluxSumAbs4[Zone,Tree] = ABS(TW_PotFlux4[Zone,Tree])
# TW_PotFluxSumAbs[Tree] = ARRAYSUM(TW_PotFluxSumAbs1[*,Tree])+ARRAYSUM(TW_PotFluxSumAbs2[*,Tree])+ARRAYSUM(TW_PotFluxSumAbs3[*,Tree])+ARRAYSUM(TW_PotFluxSumAbs4[*,Tree])
tree_df$TW_PotFluxSumAbs <- aggregate(list(x = abs(zonelayertree_df$TW_PotFlux)), zonelayertree_df["tree_id"], sum)$x

# TW_PotFluxPos[Tree] = 0.5*(TW_PotFluxSum[Tree]+TW_PotFluxSumAbs[Tree])
tree_df$TW_PotFluxPos <- 0.5 * (tree_df$TW_PotFluxSum + tree_df$TW_PotFluxSumAbs)

# TW_PotFluxNeg[Tree] =  0.5*(TW_PotFluxSumAbs[Tree]-TW_PotFluxSum[Tree])
tree_df$TW_PotFluxNeg <-  0.5 * (tree_df$TW_PotFluxSumAbs - tree_df$TW_PotFluxSum)

# TW_ScalingFacPosFluxes[Tree] = if TW_PotFluxPos[Tree]> 0 then if TW_PotFluxPos[Tree] > TW_PotFluxNeg[Tree] then
# TW_PotFluxNeg[Tree] /( TW_PotFluxPos[Tree] ) else 1 else 0
tree_df$TW_ScalingFacPosFluxes <- ifelse(
  tree_df$TW_PotFluxPos > 0,
  ifelse(
    tree_df$TW_PotFluxPos > tree_df$TW_PotFluxNeg,
    tree_df$TW_PotFluxNeg / (tree_df$TW_PotFluxPos),
    1
  ),
  0
)
# TW_ScalingFacNegFluxes[Tree] = if TW_PotFluxNeg[Tree]> 0 then if TW_PotFluxNeg[Tree] > TW_PotFluxPos[Tree] then
# TW_PotFluxPos[Tree] /( TW_PotFluxNeg[Tree] ) else 1 else 0

tree_df$TW_ScalingFacNegFluxes <- ifelse(
  tree_df$TW_PotFluxNeg > 0,
  ifelse(
    tree_df$TW_PotFluxNeg > tree_df$TW_PotFluxPos,
    tree_df$TW_PotFluxPos / tree_df$TW_PotFluxNeg,
    1
  ),
  0
)


zonelayertree_df$TW_ScalingFacPosFluxes <- rep(tree_df$TW_ScalingFacPosFluxes, each = nzone *
                                                 nlayer)
zonelayertree_df$TW_ScalingFacNegFluxes <- rep(tree_df$TW_ScalingFacNegFluxes, each = nzone *
                                                 nlayer)

# TW_HydFluxTot1[Zone,Tree] = if TW_PotFlux1[Zone,Tree] > 0 then TW_PotFlux1[Zone,Tree]*TW_ScalingFacPosFluxes[Tree] else TW_PotFlux1[Zone,Tree]*TW_ScalingFacNegFluxes[Tree]
# TW_HydFluxTot2[Zone,Tree] = if TW_PotFlux2[Zone,Tree] > 0 then TW_PotFlux2[Zone,Tree]*TW_ScalingFacPosFluxes[Tree] else TW_PotFlux2[Zone,Tree]*TW_ScalingFacNegFluxes[Tree]
# TW_HydFluxTot3[Zone,Tree] = if TW_PotFlux3[Zone,Tree] > 0 then TW_PotFlux3[Zone,Tree]*TW_ScalingFacPosFluxes[Tree] else TW_PotFlux3[Zone,Tree]*TW_ScalingFacNegFluxes[Tree]
# TW_HydFluxTot4[Zone,Tree] = if TW_PotFlux4[Zone,Tree] > 0 then TW_PotFlux4[Zone,Tree]*TW_ScalingFacPosFluxes[Tree] else TW_PotFlux4[Zone,Tree]*TW_ScalingFacNegFluxes[Tree]

zonelayertree_df$TW_HydFluxTot <- ifelse(
  zonelayertree_df$TW_PotFlux > 0,
  zonelayertree_df$TW_PotFlux * zonelayertree_df$TW_ScalingFacPosFluxes,
  zonelayertree_df$TW_PotFlux * zonelayertree_df$TW_ScalingFacNegFluxes
)

# TW_HydFlux1[Zone,Tree] = TW_HydFluxTot1[Zone,Tree]/AF_ZoneFrac[Zone]
# TW_HydFlux2[Zone,Tree] = TW_HydFluxTot2[Zone,Tree]/AF_ZoneFrac[Zone]
# TW_HydFlux3[Zone,Tree] = TW_HydFluxTot3[Zone,Tree]/AF_ZoneFrac[Zone]
# TW_HydFlux4[Zone,Tree] = TW_HydFluxTot4[Zone,Tree]/AF_ZoneFrac[Zone]
zonelayertree_df$TW_HydFlux <- zonelayertree_df$TW_HydFluxTot / zonelayertree_df$AF_ZoneFrac


# CW_Conduct1[Zone] = AF_DepthAct1[Zone]*Rt_CLrv1[Zone]
# CW_Conduct2[Zone] = AF_Depth2[Zone]*Rt_CLrv2[Zone]
# CW_Conduct3[Zone] = AF_Depth3[Zone]*Rt_CLrv3[Zone]
# CW_Conduct4[Zone] = AF_Depth4[Zone]*Rt_CLrv4[Zone]
zonelayer_df$CW_Conduct <- zonelayer_df$AF_Depth * zonelayer_df$Rt_CLrv

# CW_ConductSum[Zone] = CW_Conduct1[Zone]+CW_Conduct2[Zone]+CW_Conduct3[Zone]+CW_Conduct4[Zone]
zone_df$CW_ConductSum <- aggregate(list(x = zonelayer_df$CW_Conduct), zonelayer_df["zone"], sum)$x

# CW_PStem1[Zone] = CW_Conduct1[Zone]*W_PTheta1[Zone]
# CW_PStem2[Zone] = CW_Conduct2[Zone]*W_PTheta2[Zone]
# CW_PStem3[Zone] = CW_Conduct3[Zone]*W_PTheta3[Zone]
# CW_PStem4[Zone] = CW_Conduct4[Zone]*W_PTheta4[Zone]
# CW_PStemSum[Zone] = CW_PStem1[Zone]+CW_PStem2[Zone]+CW_PStem3[Zone]+CW_PStem4[Zone]

zonelayer_df$CW_PStem_zl <- zonelayer_df$CW_Conduct * zonelayer_df$W_PTheta
zone_df$CW_PStemSum <- aggregate(list(x = zonelayer_df$CW_PStem_zl), zonelayer_df["zone"], sum)$x

# CW_PStem[Zone] = If CW_ConductSum[Zone] <> 0 then CW_PStemSum[Zone]/CW_ConductSum[Zone] else 10
zone_df$CW_PStem <- ifelse(zone_df$CW_ConductSum != 0,
                           zone_df$CW_PStemSum / zone_df$CW_ConductSum,
                           10)
zonelayer_df$CW_PStem <- rep(zone_df$CW_PStem, nlayer)

zonelayer_df$W_ThetaSat <- rep(layer_df$W_ThetaSat, each = nzone)
zonelayer_df$W_Alpha <- rep(layer_df$W_Alpha, each = nzone)
zonelayer_df$W_n <- rep(layer_df$W_n, each = nzone)

# CW_EqTheta1[Zone] = W_ThetaSat1/(1+(ABS(W_Alpha1*CW_PStem[Zone]))^W_n1)^(1-1/W_n1)
# CW_EqTheta2[Zone] = W_ThetaSat2/(1+(ABS(W_Alpha2*CW_PStem[Zone]))^W_n2)^(1-1/W_n2)
# CW_EqTheta3[Zone] = W_ThetaSat3/(1+(ABS(W_Alpha3*CW_PStem[Zone]))^W_n3)^(1-1/W_n3)
# CW_EqTheta4[Zone] = W_ThetaSat4/(1+(ABS(W_Alpha4*CW_PStem[Zone]))^W_n4)^(1-1/W_n4)
zonelayer_df$CW_EqTheta <- zonelayer_df$W_ThetaSat / (1 + (abs(
  zonelayer_df$W_Alpha * zonelayer_df$CW_PStem
))^zonelayer_df$W_n)^(1 - 1 / zonelayer_df$W_n)

# CW_HEGrad1[Zone] = if (CW_EqTheta1[Zone]-W_Theta1[Zone]) <> 0 then (CW_PStem[Zone]-W_PTheta1[Zone])/(CW_EqTheta1[Zone]-W_Theta1[Zone]) else 1
# CW_HEGrad2[Zone] = if (CW_EqTheta2[Zone]-W_Theta2[Zone]) <> 0 then (CW_PStem[Zone]-W_PTheta2[Zone])/(CW_EqTheta2[Zone]-W_Theta2[Zone]) else 1
# CW_HEGrad3[Zone] = if (CW_EqTheta3[Zone]-W_Theta3[Zone]) <> 0 then (CW_PStem[Zone]-W_PTheta3[Zone])/(CW_EqTheta3[Zone]-W_Theta3[Zone]) else 1
# CW_HEGrad4[Zone] = if (CW_EqTheta4[Zone]-W_Theta4[Zone]) <> 0 then (CW_PStem[Zone]-W_PTheta4[Zone])/(CW_EqTheta4[Zone]-W_Theta4[Zone]) else 1

zonelayer_df$CW_HEGrad <- ifelse((zonelayer_df$CW_EqTheta - zonelayer_df$W_Theta) != 0,
                                 (zonelayer_df$CW_PStem - zonelayer_df$W_PTheta) / (zonelayer_df$CW_EqTheta -
                                                                                      zonelayer_df$W_Theta),
                                 1
)


# Rt_CAmount1[Zone] = AF_DepthAct1[Zone]*Rt_CLrv1[Zone]*100
# Rt_CAmount2[Zone] = AF_Depth2[Zone]*Rt_CLrv2[Zone]*100
# Rt_CAmount3[Zone] = AF_Depth3[Zone]*Rt_CLrv3[Zone]*100
# Rt_CAmount4[Zone] = AF_Depth4[Zone]*Rt_CLrv4[Zone]*100
# Rt_CAmount[Zone] = Rt_CAmount1[Zone]+Rt_CAmount2[Zone]+Rt_CAmount3[Zone]+Rt_CAmount4[Zone]

zonelayer_df$Rt_CAmount <- zonelayer_df$AF_Depth * zonelayer_df$Rt_CLrv *
  100
zone_df$Rt_CAmount <- aggregate(list(x = zonelayer_df$Rt_CAmount), zonelayer_df["zone"], sum)$x

zonebuf_df$Rt_CAmount <- rep(zone_df$Rt_CAmount, nrow(buf_df))

# CW_PotSuctHalf[Zone] = -((Cq_PotSuctAlphMaxCurr[Zone]*Cq_PotSuctAlphMinCurr[Zone])^0.5)
zone_df$CW_PotSuctHalf <- -((
  zone_df$Cq_PotSuctAlphMaxCurr * zone_df$Cq_PotSuctAlphMinCurr
)^0.5)

zonelayer_df$Rt_CLrv_depth <- zonelayer_df$AF_Depth * zonelayer_df$Rt_CLrv
zonelayerbuf_df$Rt_CLrv_depth <- rep(zonelayer_df$Rt_CLrv_depth, nrow(buf_df))
zonelayerbuf_df$W_PTheta <- rep(zonelayer_df$W_PTheta, nrow(buf_df))

zonebuf_df$CW_DryFactRangeInit <- rep(buf_df$CW_DryFactRangeInit, each = nzone)
zonebuf_df$CW_DryFactRangePower <- rep(zone_df$CW_DryFactRangePower, nrow(buf_df))
zonebuf_df$TW_RangeToppingUp <- rep(buf_df$TW_RangeToppingUp, each = nzone)

# CW_DrySoilFact[BufValues,Zone] = max(min(CW_DryFactRangeInit[BufValues]^CW_DryFactRangePower[Zone],CW_DryPowerMax),CW_DryPowerMin)
zonebuf_df$CW_DrySoilFact <- pmax(
  pmin(
    zonebuf_df$CW_DryFactRangeInit^zonebuf_df$CW_DryFactRangePower,
    pars$CW_par$CW_DryPowerMax
  ),
  pars$CW_par$CW_DryPowerMin
)

zl1b <- zonelayerbuf_df[zonelayerbuf_df$layer == 1, ]
zl2b <- zonelayerbuf_df[zonelayerbuf_df$layer == 2, ]
zl3b <- zonelayerbuf_df[zonelayerbuf_df$layer == 3, ]
zl4b <- zonelayerbuf_df[zonelayerbuf_df$layer == 4, ]

# CW_PotSoil[Zone,BufValues] = if AF_DepthAct1[Zone]*Rt_CLrv1[Zone] > 0 then -TW_RangeToppingUp[BufValues]*((0.01*Rt_CAmount[Zone]/(AF_DepthAct1[Zone]*Rt_CLrv1[Zone]/ABS(W_PTheta1[Zone])^CW_DrySoilFact[BufValues,Zone]+AF_Depth2[Zone]*Rt_CLrv2[Zone]/ABS(W_PTheta2[Zone])^CW_DrySoilFact[BufValues,Zone]+AF_Depth3[Zone]*Rt_CLrv3[Zone]/ABS(W_PTheta3[Zone])^CW_DrySoilFact[BufValues,Zone]+AF_Depth4[Zone]*Rt_CLrv4[Zone]/ABS(W_PTheta4[Zone])^CW_DrySoilFact[BufValues,Zone]))^(1/CW_DrySoilFact[BufValues,Zone])) else W_PTheta1[Zone]
zonebuf_df$CW_PotSoil <- ifelse(zl1b$Rt_CLrv_depth > 0,
                                -zonebuf_df$TW_RangeToppingUp * ((
                                  0.01 * zonebuf_df$Rt_CAmount /
                                    (
                                      zl1b$Rt_CLrv_depth / abs(zl1b$W_PTheta)^zonebuf_df$CW_DrySoilFact +
                                        zl2b$Rt_CLrv_depth /
                                        abs(zl2b$W_PTheta)^zonebuf_df$CW_DrySoilFact +
                                        zl3b$Rt_CLrv_depth /
                                        abs(zl3b$W_PTheta)^zonebuf_df$CW_DrySoilFact +
                                        zl4b$Rt_CLrv_depth /
                                        abs(zl4b$W_PTheta)^zonebuf_df$CW_DrySoilFact
                                    )
                                )^(1 / zonebuf_df$CW_DrySoilFact)
                                ),
                                zl1b$W_PTheta)


# Light_CCap1[Zone] = IF(Light_TCCap1[Zone]>0)THEN(Light_TCCap1[Zone]*(Cq_kLightCurr[Zone]*Light_LAIC[Zone,L1])/(T_klight[Sp1]*Light_LAIT1[Zone,Sp1]+T_klight[Sp2]*Light_LAIT1[Zone,Sp2]+T_klight[Sp3]*Light_LAIT1[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L1]+Light_kTB[Sp1]*Light_TBAI1[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI1[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI1[Zone,Sp3]))ELSE(0)
# Light_CCap2[Zone] = IF(Light_TCCap2[Zone]>0)THEN(Light_TCCap2[Zone]*(Cq_kLightCurr[Zone]*Light_LAIC[Zone,L2])/(T_klight[Sp1]*Light_LAIT2[Zone,Sp1]+T_klight[Sp2]*Light_LAIT2[Zone,Sp2]+T_klight[Sp3]*Light_LAIT2[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L2]+Light_kTB[Sp1]*Light_TBAI2[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI2[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI2[Zone,Sp3]))ELSE(0)
# Light_CCap3[Zone] = IF(Light_TCCap3[Zone]>0)THEN(Light_TCCap3[Zone]*Cq_kLightCurr[Zone]*Light_LAIC[Zone,L3]/(T_klight[Sp1]*Light_LAIT3[Zone,Sp1]+T_klight[Sp2]*Light_LAIT3[Zone,Sp2]+T_klight[Sp3]*Light_LAIT3[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L3]+Light_kTB[Sp1]*Light_TBAI3[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI3[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI3[Zone,Sp3]))ELSE(0)
# Light_CCap4[Zone] = IF(Light_TCCap4[Zone]>0)THEN(Light_TCCap4[Zone]*Cq_kLightCurr[Zone]*Light_LAIC[Zone,L4]/(T_klight[Sp1]*Light_LAIT4[Zone,Sp1]+T_klight[Sp2]*Light_LAIT4[Zone,Sp2]+T_klight[Sp3]*Light_LAIT4[Zone,Sp3]+Cq_kLightCurr[Zone]*Light_LAIC[Zone,L4]+Light_kTB[Sp1]*Light_TBAI4[Zone,Sp1]+Light_kTB[Sp2]*Light_TBAI4[Zone,Sp2]+Light_kTB[Sp3]*Light_TBAI4[Zone,Sp3]))ELSE(0)

zonelayertree_df$Light_CCap_a <- zonelayertree_df$T_klight * zonelayertree_df$Light_LAIT
zonelayertree_df$Light_kTB <- rep(tree_df$Light_kTB, each = nzone * nlayer)
zonelayertree_df$Light_CCap_b <- zonelayertree_df$Light_kTB * zonelayertree_df$Light_TBAI

zonelayer_df <- cbind(zonelayer_df,
                      aggregate(zonelayertree_df[c("Light_CCap_a", "Light_CCap_b")], zonelayertree_df[c("zone", "layer")], sum)[c("Light_CCap_a", "Light_CCap_b")])

zonelayer_df$Light_CCap <- ifelse(zonelayer_df$Light_TCCap > 0,
                                  (
                                    zonelayer_df$Light_TCCap * (zonelayer_df$Cq_kLightCurr * zonelayer_df$Light_LAIC) / (
                                      zonelayertree_df$Light_CCap_a + zonelayertree_df$Light_CCap_b +
                                        zonelayer_df$Cq_kLightCurr * zonelayer_df$Light_LAIC
                                    )
                                  ),
                                  0)


zone_df$Light_CCap_sum <- aggregate(list(x = zonelayer_df$Light_CCap), zonelayer_df["zone"], sum)$x

zone_df$LightSwitch <- pars$Light_par$LightSwitch

# Light_CRelCap[Zone] = if LightSwitch = 1 or LightSwitch = 2 then (Light_CCap1[Zone]+Light_CCap2[Zone]+Light_CCap3[Zone]+Light_CCap4[Zone])*RelLightCapPerZone[Zone] else (Light_CCap1[Zone]+Light_CCap2[Zone]+Light_CCap3[Zone]+Light_CCap4[Zone])
zone_df$Light_CRelCap <- ifelse(
  zone_df$LightSwitch == 1 | zone_df$LightSwitch == 2,
  zone_df$Light_CCap_sum * zone_df$RelLightCapPerZone,
  zone_df$Light_CCap_sum
)


# Evap_CropWatDem[Zone] = Evap_EpotDemandNotMetBy_CanInterc*Light_CRelCap[Zone]
zone_df$Evap_CropWatDem <- Evap_EpotDemandNotMetBy_CanInterc * zone_df$Light_CRelCap



tree_df$SB_SlashYear <- apply(tree_df[c("SB_PastSlashEvents", "tree_id")], 1, function(x) {
  get_graph_y(SB_SlashYear_df,
              x["SB_PastSlashEvents"],
              y_column = names(SB_SlashYear_df)[x["tree_id"]],
              mode = "continues")
})

tree_df$SB_SlashDOY <- apply(tree_df[c("SB_PastSlashEvents", "tree_id")], 1, function(x) {
  get_graph_y(SB_SlashDOY_df,
              x["SB_PastSlashEvents"],
              y_column = names(SB_SlashYear_df)[x["tree_id"]],
              mode = "continues")
})
# S&B_SlashTime[Tree] = S&B_SlashDOY[Tree] + 365* (S&B_SlashYear[Tree])-Ca_DOYStart
tree_df$SB_SlashTime <- tree_df$SB_SlashDOY + 365 * (tree_df$SB_SlashYear) -
  Ca_DOYStart

# S&B_FireTime? = if (time > S&B_SlashTime[Sp1]+S&B_MinDryingPer and time < S&B_SlashTime[Sp1]+S&B_MaxDryingPer) or (time > S&B_SlashTime[Sp2]+S&B_MinDryingPer and time < S&B_SlashTime[Sp2]+S&B_MaxDryingPer) or (time > S&B_SlashTime[Sp3]+S&B_MinDryingPer and time < S&B_SlashTime[Sp3]+S&B_MaxDryingPer) then 1 else 0
SB_FireTime_is = ifelse(
  (
    time > tree_df$SB_SlashTime[1] + pars$SB_par$SB_MinDryingPer &
      time < tree_df$SB_SlashTime[1] + pars$SB_par$SB_MaxDryingPer
  ) |
    (
      time > tree_df$SB_SlashTime[2] + pars$SB_par$SB_MinDryingPer &
        time < tree_df$SB_SlashTime[2] + pars$SB_par$SB_MaxDryingPer
    ) |
    (
      time > tree_df$SB_SlashTime[3] + pars$SB_par$SB_MinDryingPer &
        time < tree_df$SB_SlashTime[3] + pars$SB_par$SB_MaxDryingPer
    ),
  1,
  0
)

zone_df$SB_SlashMoist <- ifelse(
  zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$SB_FineNecromass > 0,
  zone_df$Evap_SlashWater / zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$SB_FineNecromass,
  0
)


# S&B_IsSlashDry?[Zone] = if S&B_FineNecromass[Zone,DW] > 0 then if (S&B_FineNecromass[Zone,DW]*S&B_SlashMoist[Zone]+Rain_CanopyWater[Zone]) /S&B_FineNecromass[Zone,DW] < S&B_CritMoist  then 1 else 0 else 0
zone_df$SB_IsSlashDry_is <- ifelse(
  zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$SB_FineNecromass > 0,
  ifelse((
    zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$SB_FineNecromass * zone_df$SB_SlashMoist + zone_df$Rain_CanopyWater
  ) /
    zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$SB_FineNecromass < pars$SB_par$SB_CritMoist,
  1,
  0
  ),
  0
)

# S&B_Fire? = if S&B_FireTime? = 1 and ARRAYMEAN(S&B_IsSlashDry?[*]) = 1 then 1 else 0
SB_Fire_is <- ifelse(SB_FireTime_is == 1 &
                       sum(zone_df$SB_IsSlashDry_is) == 1, 1, 0)

zone_df$Ca_PlantDoY <- get_val_by_Ca_ComplCrop(Ca_PlantDoY_df, zone_df$Ca_ComplCrop, zone_df$zone)
zone_df$Ca_PlantYear <- get_val_by_Ca_ComplCrop(Ca_PlantYear_df, zone_df$Ca_ComplCrop, zone_df$zone)
# 
# zone_df$Ca_PlantDoY <- NA
# for (i in c(1:nzone)) {
#   x <- zone_df[zone_df$zone == i, ]$Ca_ComplCrop
#   zone_df[zone_df$zone == i, ]$Ca_PlantDoY <- get_graph_y(
#     Ca_PlantDoY_df,
#     x = x,
#     x_column = "Ca_ComplCrop",
#     y_column = names(Ca_PlantDoY_df)[i],
#     mode = "continues"
#   )
# }
# 
# zone_df$Ca_PlantYear <- NA
# for (i in c(1:nzone)) {
#   x <- zone_df[zone_df$zone == i, ]$Ca_ComplCrop
#   zone_df[zone_df$zone == i, ]$Ca_PlantYear <- get_graph_y(
#     Ca_PlantYear_df,
#     x = x,
#     x_column = "Ca_ComplCrop",
#     y_column = names(Ca_PlantYear_df)[i],
#     mode = "continues"
#   )
# }

# Ca_PlantTime[Zone] = Ca_PlantDoY[Zone] + 365* (Ca_PlantYear[Zone])-Ca_DOYStart
zone_df$Ca_PlantTime <- zone_df$Ca_PlantDoY + 365 * (zone_df$Ca_PlantYear) -
  Ca_DOYStart


# C_PlantDiesToday?[Zone] = if Cq_Stage[Zone]>=2 and Cq_StageAfterHarvest[Zone] = 0 or time = int(S&B_SlashTime[Sp1]) or time = int(S&B_SlashTime[Sp2]) or time = int(S&B_SlashTime[Sp3]) or  time = int(Ca_PlantTime[Zone])  or S&B_Fire? = 1 or C_GroRes[Zone,DW] = 0 then 1 else 0
zone_df$time <- time
zone_df$C_PlantDiesToday_is <- ifelse(
  zone_df$Cq_Stage >= 2 &
    zone_df$Cq_StageAfterHarvest == 0 |
    zone_df$time == floor(tree_df$SB_SlashTime[1]) |
    zone_df$time == floor(tree_df$SB_SlashTime[2]) |
    zone_df$time == floor(tree_df$SB_SlashTime[3]) |
    zone_df$time == floor(zone_df$Ca_PlantTime)  |
    SB_Fire_is == 1 |
    zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$C_GroRes == 0,
  1,
  0
)

# C_BiomCan[Zone,PlantComp] = C_BiomStLv[Zone,PlantComp]+C_YieldCurr[Zone,PlantComp]+C_GroRes[Zone,PlantComp]
zonepcomp_df$C_BiomCan <-  zonepcomp_df$C_BiomStLv + zonepcomp_df$C_YieldCurr + zonepcomp_df$C_GroRes


zonenut_df$Cq_ClosedCanCurr <- rep(zone_df$Cq_ClosedCanCurr, nrow(nut_df))
zonenut_df$C_BiomCan_DW <- rep(zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$C_BiomCan, nrow(nut_df))
zonenut_df$C_Root_DW_zone <- rep(zone_df$C_Root_DW_zone, nrow(nut_df))

# C_NTarget[Zone,SlNut] = (MIN(C_BiomCan[Zone,DW],Cq_ClosedCanCurr[Zone])*Cq_NConcYoungCurr[Zone,SlNut]+MAX((C_BiomCan[Zone,DW]-Cq_ClosedCanCurr[Zone]),0)*Cq_NConcOldCurr[Zone,SlNut]+ARRAYSUM(C_Root_DW[Zone,*])*Cq_NConcOldCurr[Zone,SlNut])*1000
zonenut_df$C_NTarget <- (
  pmin(zonenut_df$C_BiomCan_DW, zonenut_df$Cq_ClosedCanCurr) * zonenut_df$Cq_NConcYoungCurr +
    pmax((
      zonenut_df$C_BiomCan_DW - zonenut_df$Cq_ClosedCanCurr
    ), 0) * zonenut_df$Cq_NConcOldCurr +
    zonenut_df$C_Root_DW_zone * zonenut_df$Cq_NConcOldCurr
) * 1000

# C_RootPlComp[Zn1,DW] = ARRAYSUM(C_Root_DW[Zn1,*])+0*ARRAYSUM(C_Root_N[Zn1,*])+0*ARRAYSUM(C_Root_P[Zn1,*])
# C_RootPlComp[Zn1,N] = 0*ARRAYSUM(C_Root_DW[Zn1,*])+ARRAYSUM(C_Root_N[Zn1,*])+0*ARRAYSUM(C_Root_P[Zn1,*])
# C_RootPlComp[Zn1,P] = 0*ARRAYSUM(C_Root_DW[Zn1,*])+0*ARRAYSUM(C_Root_N[Zn1,*])+ARRAYSUM(C_Root_P[Zn1,*])
# C_RootPlComp[Zn2,DW] = ARRAYSUM(C_Root_DW[Zn2,*])+0*ARRAYSUM(C_Root_N[Zn2,*])+0*ARRAYSUM(C_Root_P[Zn2,*])
# C_RootPlComp[Zn2,N] = 0*ARRAYSUM(C_Root_DW[Zn2,*])+ARRAYSUM(C_Root_N[Zn2,*])+0*ARRAYSUM(C_Root_P[Zn2,*])
# C_RootPlComp[Zn2,P] = 0*ARRAYSUM(C_Root_DW[Zn2,*])+0*ARRAYSUM(C_Root_N[Zn2,*])+ARRAYSUM(C_Root_P[Zn2,*])
# C_RootPlComp[Zn3,DW] = ARRAYSUM(C_Root_DW[Zn3,*])+0*ARRAYSUM(C_Root_N[Zn3,*])+0*ARRAYSUM(C_Root_P[Zn3,*])
# C_RootPlComp[Zn3,N] = 0*ARRAYSUM(C_Root_DW[Zn3,*])+ARRAYSUM(C_Root_N[Zn3,*])+0*ARRAYSUM(C_Root_P[Zn3,*])
# C_RootPlComp[Zn3,P] = 0*ARRAYSUM(C_Root_DW[Zn3,*])+0*ARRAYSUM(C_Root_N[Zn3,*])+ARRAYSUM(C_Root_P[Zn3,*])
# C_RootPlComp[Zn4,DW] = ARRAYSUM(C_Root_DW[Zn4,*])+0*ARRAYSUM(C_Root_N[Zn4,*])+0*ARRAYSUM(C_Root_P[Zn4,*])
# C_RootPlComp[Zn4,N] = 0*ARRAYSUM(C_Root_DW[Zn4,*])+ARRAYSUM(C_Root_N[Zn4,*])+0*ARRAYSUM(C_Root_P[Zn4,*])
# C_RootPlComp[Zn4,P] = 0*ARRAYSUM(C_Root_DW[Zn4,*])+0*ARRAYSUM(C_Root_N[Zn4,*])+ARRAYSUM(C_Root_P[Zn4,*])
zonepcomp_df$C_RootPlComp <- aggregate(zonelayerpcomp_df["C_Root"], zonelayerpcomp_df[c("zone", "PlantComp")], sum)$C_Root

# C_Biom[Zone,PlantComp] = C_RootPlComp[Zone,PlantComp]+C_BiomCan[Zone,PlantComp]
zonepcomp_df$C_Biom <- zonepcomp_df$C_RootPlComp + zonepcomp_df$C_BiomCan

# C_NBiom[Zn1,N] = C_Biom[Zn1,N]
# C_NBiom[Zn1,P] = C_Biom[Zn1,P]
# C_NBiom[Zn2,N] = C_Biom[Zn2,N]
# C_NBiom[Zn2,P] = C_Biom[Zn2,P]
# C_NBiom[Zn3,N] = C_Biom[Zn3,N]
# C_NBiom[Zn3,P] = C_Biom[Zn3,P]
# C_NBiom[Zn4,N] = C_Biom[Zn4,N]
# C_NBiom[Zn4,P] = C_Biom[Zn4,P]
zonenut_df$C_NBiom <- zonepcomp_df[zonepcomp_df$PlantComp %in% c("N", "P"), ]$C_Biom

zonenut_df$AF_RunNutLim_is <- rep(nut_df$AF_RunNutLim_is, each = nzone)
# C_NPosgro[Zone,SlNut] = if AF_RunNutLim?[SlNut]> 0.5 and (C_NTarget[Zone,SlNut]-.4) >0 then MAX(0,MIN(1,(2*(C_NBiom[Zone,SlNut]/ C_NTarget[Zone,SlNut]-.4)))) else 1
zonenut_df$C_NPosgro <- ifelse(zonenut_df$AF_RunNutLim_is > 0.5 &
                                 (zonenut_df$C_NTarget - .4) > 0,
                               pmax(0, pmin(1, (
                                 2 * (zonenut_df$C_NBiom / zonenut_df$C_NTarget - .4)
                               ))),
                               1)

z_dw <- zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]

# C_BiomforResp[Zone] = C_GroRes[Zone,DW]*C_RelRespGroRes+C_BiomStLv[Zone,DW]*C_RelRespStLv+C_YieldCurr[Zone,DW]*C_RelRespYieldCurr+C_RootPlComp[Zone,DW]*C_RelRespRt
zone_df$C_BiomforResp <- z_dw$C_GroRes * pars$C_par$C_RelRespGroRes +
  z_dw$C_BiomStLv * pars$C_par$C_RelRespStLv +
  z_dw$C_YieldCurr * pars$C_par$C_RelRespYieldCurr +
  z_dw$C_RootPlComp * pars$C_par$C_RelRespRt

# Temp_SoilDailyData = GRAPH(Rain_DoY)
Temp_SoilDailyData <- get_graph_y(
  Temp_SoilDailyData_df,
  Rain_DoY,
  x_column = "Rain_DoY",
  y_column = "Temp_SoilDailyData",
  mode = "continues"
)

# Temp_MonthAvg = GRAPH(Rain_DoY)
Temp_MonthAvg <- get_graph_y(
  Temp_MonthAvg_df,
  Rain_DoY,
  x_column = "Rain_DoY",
  y_column = "Temp_MonthAvg",
  mode = "continues"
)

# Temp[Zone,SoilLayer] = IF(Temp_AType=1)THEN(Temp_Cons)ELSE(IF(Temp_AType=3)THEN(Temp_SoilDailyData)ELSE(Temp_MonthAvg))
zonelayer_df$Temp <- ifelse(
  pars$Temp_par$Temp_AType == 1,
  pars$Temp_par$Temp_Cons,
  ifelse(
    pars$Temp_par$Temp_AType == 3,
    Temp_SoilDailyData,
    Temp_MonthAvg
  )
)

# C_RespTemp[Zone] = GRAPH(Temp[Zone,1])
zone_df$C_RespTemp <- NA
for (i in c(1:nzone)) {
  x <- zonelayer_df[zonelayer_df$zone == i &
                      zonelayer_df$layer == 1, ]$Temp
  zone_df[zone_df$zone == i, ]$C_RespTemp <- get_graph_y(
    C_RespTemp_df,
    x = x,
    x_column = "Temp",
    y_column = "C_RespTemp",
    mode = "continues"
  )
}

# C_MaintResp[Zone] = C_BiomforResp[Zone]*C_RespperBiom*C_RespTemp[Zone]
zone_df$C_MaintResp <- zone_df$C_BiomforResp * pars$C_par$C_RespperBiom * zone_df$C_RespTemp

# C_PotGroRed[Zone] = if C_PlantDiesToday?[Zone] = 0 then min(1,min(C_NPosgro[Zone,P],C_NPosgro[Zone,N])*Light_CRelCap[Zone]/Cq_RelLightMaxCurr[Zone])
# *Cq_CRelLUECurr[Zone] *( Cq_GroMaxCurr[Zone]+C_ApplyMaintResp?*C_MaintResp[Zone])/(1-Cq_CRtAllocCurr[Zone]) else 0
zone_df$C_NPosgro_N <- zonenut_df[zonenut_df$SlNut == "N", ]$C_NPosgro
zone_df$C_NPosgro_P <- zonenut_df[zonenut_df$SlNut == "P", ]$C_NPosgro

zone_df$C_PotGroRed <- ifelse(
  zone_df$C_PlantDiesToday_is == 0,
  pmin(
    1,
    pmin(zone_df$C_NPosgro_P, zone_df$C_NPosgro_N) * zone_df$Light_CRelCap / zone_df$Cq_RelLightMaxCurr
  )
  * zone_df$Cq_CRelLUECurr * (
    zone_df$Cq_GroMaxCurr + pars$C_par$C_ApplyMaintResp_is * zone_df$C_MaintResp
  ) / (1 - zone_df$Cq_CRtAllocCurr),
  0
)


# CW_DemandPot[Zone] = if CW_EnergyDrivenEpot? = 1 then Evap_CropWatDem[Zone] else C_PotGroRed[Zone]*Cq_TranspRatioCurr[Zone]
zone_df$CW_DemandPot <- ifelse(
  pars$CW_par$CW_EnergyDrivenEpot_is == 1,
  zone_df$Evap_CropWatDem,
  zone_df$C_PotGroRed * zone_df$Cq_TranspRatioCurr
)

# CW_PotRadial[Zone] = if Rt_CAmount[Zone]>0 then -CW_DemandPot[Zone]*0.1/(Cq_ConductivityCurr[Zone]*(Rt_CAmount[Zone])) else -16000
zone_df$CW_PotRadial <- ifelse(
  zone_df$Rt_CAmount > 0,
  -zone_df$CW_DemandPot * 0.1 / (zone_df$Cq_ConductivityCurr * zone_df$Rt_CAmount),
  -16000
)

zonebuf_df$CW_PotRadial <- rep(zone_df$CW_PotRadial, nrow(buf_df))

# CW_PotRange[Zone,BufValues] = IF(Rt_CAmount[Zone]>0)THEN CW_PotSoil[Zone,BufValues]+CW_PotRadial[Zone]  ELSE -10
zonebuf_df$CW_PotRange <- ifelse(zonebuf_df$Rt_CAmount > 0,
                                 zonebuf_df$CW_PotSoil + zonebuf_df$CW_PotRadial,
                                 -10)
zonebuf_df$CW_PotSuctHalf <- rep(zone_df$CW_PotSuctHalf, nrow(buf_df))

# CW_m[Zone] = 2*LOGN(CW_Alpha/(1-CW_Alpha))/LOGN(Cq_PotSuctAlphMaxCurr[Zone]/Cq_PotSuctAlphMinCurr[Zone])
zone_df$CW_m <- 2 * log(pars$CW_par$CW_Alpha / (1 - pars$CW_par$CW_Alpha)) /
  log(zone_df$Cq_PotSuctAlphMaxCurr / zone_df$Cq_PotSuctAlphMinCurr)

zonebuf_df$CW_m <- rep(zone_df$CW_m, nrow(buf_df))
zonebuf_df$TW_DemActSubtract <- rep(buf_df$TW_DemActSubtract, each = nzone)

# CW_DemRedFacRange[Zone,BufValues] = IF CW_PotSuctHalf[Zone] <>0 and (1+(MIN(0,CW_PotRange[Zone,BufValues])/CW_PotSuctHalf[Zone])^CW_m[Zone])<>0THEN (1/(1+(MIN(0,CW_PotRange[Zone,BufValues])/CW_PotSuctHalf[Zone])^CW_m[Zone])) - TW_DemActSubtract[BufValues]ELSE 0
zonebuf_df$CW_DemRedFacRange <- ifelse(
  zonebuf_df$CW_PotSuctHalf  != 0 &
    (1 + (
      pmin(0, zonebuf_df$CW_PotRange) / zonebuf_df$CW_PotSuctHalf
    )^zonebuf_df$CW_m) != 0,
  (1 / (
    1 + (pmin(0, zonebuf_df$CW_PotRange) / zonebuf_df$CW_PotSuctHalf)^zonebuf_df$CW_m
  )) - zonebuf_df$TW_DemActSubtract,
  0
)

zonebuf_df$CW_DemandPot <- rep(zone_df$CW_DemandPot, nrow(buf_df))
# CW_DemandActRange[Zone,BufValues] = CW_DemRedFacRange[Zone,BufValues]*CW_DemandPot[Zone]
zonebuf_df$CW_DemandActRange <- zonebuf_df$CW_DemRedFacRange * zonebuf_df$CW_DemandPot

# Rt_RhoCL1[Zone] = if Rt_CLrv1[Zone]> 0 then 1/(Cq_RtDiam[Zone]*.5*SQRT(PI*(Rt_CLrv1[Zone]))) else Rt_StopGap
# Rt_RhoCL2[Zone] = if Rt_CLrv2[Zone]> 0 then 1/(Cq_RtDiam[Zone]*.5*SQRT(PI*(Rt_CLrv2[Zone]))) else Rt_StopGap
# Rt_RhoCL3[Zone] = if Rt_CLrv3[Zone]> 0 then 1/(Cq_RtDiam[Zone]*.5*SQRT(PI*(Rt_CLrv3[Zone]))) else Rt_StopGap
# Rt_RhoCL4[Zone] = if Rt_CLrv4[Zone]> 0 then 1/(Cq_RtDiam[Zone]*.5*SQRT(PI*(Rt_CLrv4[Zone]))) else Rt_StopGap
zonelayer_df$Rt_RhoCL <- ifelse(
  zonelayer_df$Rt_CLrv > 0,
  1 / (zonelayer_df$Cq_RtDiam * .5 * sqrt(pi * (
    zonelayer_df$Rt_CLrv
  ))),
  pars$Rt_par$Rt_StopGap
)


# Rt_C_G1[Zone] = if Rt_RhoCL1[Zone] <> Rt_StopGap then (Rt_RhoCL1[Zone]^2-1)/(0.5*(((1-(3*Rt_RhoCL1[Zone]^2))/4)+((Rt_RhoCL1[Zone]^4*LOGN(Rt_RhoCL1[Zone]))/(Rt_RhoCL1[Zone]^2-1)))) else 0
# Rt_C_G2[Zone] = if Rt_RhoCL2[Zone] <> Rt_StopGap then (Rt_RhoCL2[Zone]^2-1)/(0.5*(((1-(3*Rt_RhoCL2[Zone]^2))/4)+((Rt_RhoCL2[Zone]^4*LOGN(Rt_RhoCL2[Zone]))/(Rt_RhoCL2[Zone]^2-1)))) else 0
# Rt_C_G3[Zone] = if Rt_RhoCL3[Zone] <> Rt_StopGap then (Rt_RhoCL3[Zone]^2-1)/(0.5*(((1-(3*Rt_RhoCL3[Zone]^2))/4)+((Rt_RhoCL3[Zone]^4*LOGN(Rt_RhoCL3[Zone]))/(Rt_RhoCL3[Zone]^2-1)))) else 0
# Rt_C_G4[Zone] = if Rt_RhoCL4[Zone] <> Rt_StopGap then (Rt_RhoCL4[Zone]^2-1)/(0.5*(((1-(3*Rt_RhoCL4[Zone]^2))/4)+((Rt_RhoCL4[Zone]^4*LOGN(Rt_RhoCL4[Zone]))/(Rt_RhoCL4[Zone]^2-1)))) else 0

zonelayer_df$Rt_C_G <- ifelse(
  zonelayer_df$Rt_RhoCL != pars$Rt_par$Rt_StopGap,
  (zonelayer_df$Rt_RhoCL^2 - 1) / (0.5 * (((
    1 - (3 * zonelayer_df$Rt_RhoCL^2)
  ) / 4) +
    ((zonelayer_df$Rt_RhoCL^4 *
        log(zonelayer_df$Rt_RhoCL)) / (zonelayer_df$Rt_RhoCL^2 - 1)
    ))),
  0
)

zonelayerbuf_df$Rt_C_G <- rep(zonelayer_df$Rt_C_G, nrow(buf_df))
zonelayerbuf_df$W_Theta <- rep(zonelayer_df$W_Theta, nrow(buf_df))
zonelayerbuf_df$W_ThetaSat <- rep(zonelayer_df$W_ThetaSat, nrow(buf_df))
zonelayerbuf_df$W_Alpha <- rep(zonelayer_df$W_Alpha, nrow(buf_df))

zonebuf_df$CW_PotRadial <- rep(zone_df$CW_PotRadial, nrow(buf_df))

# zb_df <- zonebuf_df[c("zone", "buf_id", "CW_PotRange", "CW_DemRedFacRange")]
# zlb_df <- zb_df[rep(seq_len(nrow(zb_df)), nlayer), ]
# zlb_df$layer <- rep(layer_df$layer, each = nzone * nrow(buf_df))
# zlb_df[order(zlb_df$buf_id, zlb_df$layer, zlb_df$zone),]
# zonelayerbuf_df <- cbind(zonelayerbuf_df, zlb_df[c("CW_PotRange", "CW_DemRedFacRange")])

# CW_PotActRange[Zone,BufValues] = CW_PotRange[Zone,BufValues] - (1-CW_DemRedFacRange[Zone,BufValues])*CW_PotRadial[Zone]
zonebuf_df$CW_PotActRange <- zonebuf_df$CW_PotRange - (1 - zonebuf_df$CW_DemRedFacRange) * zonebuf_df$CW_PotRadial

zb_df <- zonebuf_df[c("zone", "buf_id", "CW_PotActRange")]
zlb_df <- zb_df[rep(seq_len(nrow(zb_df)), nlayer), ]
zlb_df$layer <- rep(layer_df$layer, each = nzone * nrow(buf_df))
zlb_df <- zlb_df[order(zlb_df$buf_id, zlb_df$layer, zlb_df$zone), ]
zonelayerbuf_df$CW_PotActRange <- zlb_df$CW_PotActRange


zonelayerbuf_df$W_n <- rep(zonelayer_df$W_n, nrow(buf_df))
zonelayerbuf_df$AF_Depth <- rep(zonelayer_df$AF_Depth, nrow(buf_df))

# CW_ThetaRange1[Zone,BufValues] = max(0,(W_Theta1[Zone]-(W_ThetaSat1/(1+abs(W_Alpha1*CW_PotActRange[Zone,BufValues])^W_n1)^(1-1/W_n1))))*AF_DepthAct1[Zone]*1000
# CW_ThetaRange2[Zone,BufValues] = max(0,(W_Theta2[Zone]-(W_ThetaSat2/(1+abs(W_Alpha2*CW_PotActRange[Zone,BufValues])^W_n2)^(1-1/W_n2))))*AF_Depth2[Zone]*1000
# CW_ThetaRange3[Zone,BufValues] = max(0,(W_Theta3[Zone]-(W_ThetaSat3/(1+abs(W_Alpha3*CW_PotActRange[Zone,BufValues])^W_n3)^(1-1/W_n3))))*AF_Depth3[Zone]*1000
# CW_ThetaRange4[Zone,BufValues] = max(0,(W_Theta4[Zone]-(W_ThetaSat4/(1+abs(W_Alpha4*CW_PotActRange[Zone,BufValues])^W_n4)^(1-1/W_n4))))*AF_Depth4[Zone]*1000
zonelayerbuf_df$CW_ThetaRange <- max(0, (zonelayerbuf_df$W_Theta - (
  zonelayerbuf_df$W_ThetaSat /
    (
      1 + abs(zonelayerbuf_df$W_Alpha * zonelayerbuf_df$CW_PotActRange)^zonelayerbuf_df$W_n
    )^(1 - 1 / zonelayerbuf_df$W_n)
))) * zonelayerbuf_df$AF_Depth * 1000

# Rt_C_RelImp_L1[Zone] = if (CW_DemandPerRoot[Zone]*Rt_CLrv1[Zone]+TW_DemandPerRoot[Sp1]*Rt_TLrv1[Zone,Sp1]+TW_DemandPerRoot[Sp2]*Rt_TLrv1[Zone,Sp2]+TW_DemandPerRoot[Sp3]*Rt_TLrv1[Zone,Sp3])  = 0 then 1 else
#CW_DemandPerRoot[Zone]*Rt_CLrv1[Zone]/(CW_DemandPerRoot[Zone]*Rt_CLrv1[Zone]+TW_DemandPerRoot[Sp1]*Rt_TLrv1[Zone,Sp1]+TW_DemandPerRoot[Sp2]*Rt_TLrv1[Zone,Sp2]+TW_DemandPerRoot[Sp3]*Rt_TLrv1[Zone,Sp3])
# Rt_C_RelImp_L2[Zone] = if (CW_DemandPerRoot[Zone]*Rt_CLrv2[Zone]+TW_DemandPerRoot[Sp1]*Rt_TLrv2[Zone,Sp1]+TW_DemandPerRoot[Sp2]*Rt_TLrv2[Zone,Sp2]+TW_DemandPerRoot[Sp3]*Rt_TLrv2[Zone,Sp3])  = 0 then 1 else
#CW_DemandPerRoot[Zone]*Rt_CLrv2[Zone]/(CW_DemandPerRoot[Zone]*Rt_CLrv2[Zone]+TW_DemandPerRoot[Sp1]*Rt_TLrv2[Zone,Sp1]+TW_DemandPerRoot[Sp2]*Rt_TLrv2[Zone,Sp2]+TW_DemandPerRoot[Sp3]*Rt_TLrv2[Zone,Sp3])
# Rt_C_RelImp_L3[Zone] = if (CW_DemandPerRoot[Zone]*Rt_CLrv3[Zone]+TW_DemandPerRoot[Sp1]*Rt_TLrv3[Zone,Sp1]+TW_DemandPerRoot[Sp2]*Rt_TLrv3[Zone,Sp2]+TW_DemandPerRoot[Sp3]*Rt_TLrv3[Zone,Sp3])  = 0 then 1 else
#CW_DemandPerRoot[Zone]*Rt_CLrv3[Zone]/(CW_DemandPerRoot[Zone]*Rt_CLrv3[Zone]+TW_DemandPerRoot[Sp1]*Rt_TLrv3[Zone,Sp1]+TW_DemandPerRoot[Sp2]*Rt_TLrv3[Zone,Sp2]+TW_DemandPerRoot[Sp3]*Rt_TLrv3[Zone,Sp3])
# Rt_C_RelImp_L4[Zone] = if (CW_DemandPerRoot[Zone]*Rt_CLrv4[Zone]+TW_DemandPerRoot[Sp1]*Rt_TLrv4[Zone,Sp1]+TW_DemandPerRoot[Sp2]*Rt_TLrv4[Zone,Sp2]+TW_DemandPerRoot[Sp3]*Rt_TLrv4[Zone,Sp3])  = 0 then 1 else
#CW_DemandPerRoot[Zone]*Rt_CLrv4[Zone]/(CW_DemandPerRoot[Zone]*Rt_CLrv4[Zone]+TW_DemandPerRoot[Sp1]*Rt_TLrv4[Zone,Sp1]+TW_DemandPerRoot[Sp2]*Rt_TLrv4[Zone,Sp2]+TW_DemandPerRoot[Sp3]*Rt_TLrv4[Zone,Sp3])

# zonelayer_df$Rt_T_RelImp_a <- zonelayer_df$CW_DemandPerRoot * zonelayer_df$Rt_CLrv +
#   zlt1$TW_DemandPerRoot * zlt1$Rt_TLrv + zlt2$TW_DemandPerRoot * zlt2$Rt_TLrv + zlt3$TW_DemandPerRoot * zlt3$Rt_TLrv
zonelayer_df$Rt_C_RelImp_a <- zonelayer_df$Rt_T_RelImp_a
zonelayer_df$Rt_C_RelImp <- ifelse(
  zonelayer_df$Rt_C_RelImp_a  == 0,
  1,
  zonelayer_df$CW_DemandPerRoot * zonelayer_df$Rt_CLrv / zonelayer_df$Rt_C_RelImp_a
)

# CW_pF_range[Zone,BufValues] = IF(CW_PotActRange[Zone,BufValues]=0)THEN 0 else (LOG10(-MIN(-1,CW_PotActRange[Zone,BufValues])))
zonebuf_df$CW_pF_range <- ifelse(zonebuf_df$CW_PotActRange == 0, 0, log10(-pmin(-1, zonebuf_df$CW_PotActRange)))

# W_PhiPRhizRange[Zone,BufValues] = GRAPH(CW_pF_range[Zone,BufValues])
zonebuf_df$W_PhiPRhizRange <- unlist(lapply(zonebuf_df$CW_pF_range, function(x) {
  get_graph_y(
    W_PhiPRhizRange_df,
    x,
    x_column = "CW_pF_range",
    y_column = "W_PhiPRhizRange",
    mode = "continues"
  )
}))

zb_df <- zonebuf_df[c("zone", "buf_id", "W_PhiPRhizRange")]
zlb_df <- zb_df[rep(seq_len(nrow(zb_df)), nlayer), ]
zlb_df$layer <- rep(layer_df$layer, each = nzone * nrow(buf_df))
zlb_df <- zlb_df[order(zlb_df$buf_id, zlb_df$layer, zlb_df$zone), ]
zonelayerbuf_df$W_PhiPRhizRange <- zlb_df$W_PhiPRhizRange


zonelayerbuf_df$W_PhiTheta <- rep(zonelayer_df$W_PhiTheta, nrow(buf_df))
zonelayerbuf_df$Rt_CLrv <- rep(zonelayer_df$Rt_CLrv, nrow(buf_df))
zonelayerbuf_df$Rt_C_RelImp <- rep(zonelayer_df$Rt_C_RelImp, nrow(buf_df))

# CW_PU1[Zone,BufValues] = if Rt_C_G1[Zone] = 0 then 0 else min(CW_ThetaRange1[Zone,BufValues],AF_DepthAct1[Zone]*Rt_C_RelImp_L1[Zone]*10*PI*100*max(0,-W_PhiPRhizRange[Zone,BufValues]+W_PhiTheta1[Zone])*Rt_CLrv1[Zone]*Rt_C_G1[Zone])
# CW_PU2[Zone,BufValues] = if Rt_C_G2[Zone] = 0 then 0 else min(CW_ThetaRange2[Zone,BufValues],AF_Depth2[Zone]*Rt_C_RelImp_L2[Zone]*10*PI*100*max(0,-W_PhiPRhizRange[Zone,BufValues]+W_PhiTheta2[Zone])*Rt_CLrv2[Zone]*Rt_C_G2[Zone])
# CW_PU3[Zone,BufValues] =  if Rt_C_G3[Zone] = 0 then 0 else min(CW_ThetaRange3[Zone,BufValues],AF_Depth3[Zone]*Rt_C_RelImp_L3[Zone]*10*PI*100*max(0,-W_PhiPRhizRange[Zone,BufValues]+W_PhiTheta3[Zone])*Rt_CLrv3[Zone]*Rt_C_G3[Zone])
# CW_PU4[Zone,BufValues] = if Rt_C_G4[Zone] = 0 then 0 else min(CW_ThetaRange4[Zone,BufValues],AF_Depth4[Zone]*Rt_C_RelImp_L4[Zone]*10*PI*100*max(0,-W_PhiPRhizRange[Zone,BufValues]+W_PhiTheta4[Zone])*Rt_CLrv4[Zone]*Rt_C_G4[Zone])

zonelayerbuf_df$CW_PU <- ifelse(
  zonelayerbuf_df$Rt_C_G == 0,
  0,
  pmin(
    zonelayerbuf_df$CW_ThetaRange,
    zonelayerbuf_df$AF_Depth * zonelayerbuf_df$Rt_C_RelImp * 10 * pi * 100 *
      pmax(
        0,
        -zonelayerbuf_df$W_PhiPRhizRange + zonelayerbuf_df$W_PhiTheta
      ) * zonelayerbuf_df$Rt_CLrv * zonelayerbuf_df$Rt_C_G
  )
)


# CW_PU[Zone,BufValues] = CW_PU1[Zone,BufValues] + CW_PU2[Zone,BufValues]+ CW_PU3[Zone,BufValues]+CW_PU4[Zone,BufValues]
zonebuf_df$CW_PU <- aggregate(zonelayerbuf_df["CW_PU"], zonelayerbuf_df[c("zone", "buf_id")], sum)$CW_PU

# CW_PotUptRange[Zone,BufValues] = min(CW_DemandActRange[Zone,BufValues],CW_PU[Zone,BufValues])
zonebuf_df$CW_PotUptRange <- pmin(zonebuf_df$CW_DemandActRange, zonebuf_df$CW_PU)

zone_df$CW_PotUptRange_sum <- aggregate(zonebuf_df["CW_PotUptRange"], zonebuf_df[c("zone")], sum)$CW_PotUptRange
zone_df$CW_PotUptRange_max <- aggregate(zonebuf_df["CW_PotUptRange"], zonebuf_df[c("zone")], max)$CW_PotUptRange

# CW_MaxUptofRange[Zone] = if arraysum(CW_PotUptRange[Zone,*]) = 0 then 0 else max(CW_PotUptRange[Zone,1],CW_PotUptRange[Zone,2],CW_PotUptRange[Zone,3],CW_PotUptRange[Zone,4],CW_PotUptRange[Zone,5],CW_PotUptRange[Zone,6],CW_PotUptRange[Zone,7],CW_PotUptRange[Zone,8],CW_PotUptRange[Zone,9],CW_PotUptRange[Zone,10])
zone_df$CW_MaxUptofRange <- ifelse(zone_df$CW_PotUptRange_sum == 0, 0, zone_df$CW_PotUptRange_max)


# CW_Best[Zn1,1] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,1] then 1 else 0
# CW_Best[Zn1,2] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,2] and CW_PotUptRange[Zn1,2]>CW_PotUptRange[Zn1,1] then 1 else 0
# CW_Best[Zn1,3] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,3]  and CW_PotUptRange[Zn1,3]>CW_PotUptRange[Zn1,2] then 1 else 0
# CW_Best[Zn1,4] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,4]  and CW_PotUptRange[Zn1,4]>CW_PotUptRange[Zn1,3] then 1 else 0
# CW_Best[Zn1,5] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,5]  and CW_PotUptRange[Zn1,5]>CW_PotUptRange[Zn1,4] then 1 else 0
# CW_Best[Zn1,6] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,6] and CW_PotUptRange[Zn1,6]>CW_PotUptRange[Zn1,5] then 1 else 0
# CW_Best[Zn1,7] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,7]  and CW_PotUptRange[Zn1,7]>CW_PotUptRange[Zn1,6] then 1 else 0
# CW_Best[Zn1,8] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,8]  and CW_PotUptRange[Zn1,8]>CW_PotUptRange[Zn1,7] then 1 else 0
# CW_Best[Zn1,9] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,9]  and CW_PotUptRange[Zn1,9]>CW_PotUptRange[Zn1,8] then 1 else 0
# CW_Best[Zn1,10] = if CW_MaxUptofRange[Zn1]>0 and CW_MaxUptofRange[Zn1]=CW_PotUptRange[Zn1,10]  and CW_PotUptRange[Zn1,10]>CW_PotUptRange[Zn1,9] then 1 else 0
# CW_Best[Zn2,1] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,1]  then 1 else 0
# CW_Best[Zn2,2] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,2]  and CW_PotUptRange[Zn2,2]>CW_PotUptRange[Zn2,1] then 1 else 0
# CW_Best[Zn2,3] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,3]  and CW_PotUptRange[Zn2,3]>CW_PotUptRange[Zn2,2] then 1 else 0
# CW_Best[Zn2,4] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,4]  and CW_PotUptRange[Zn2,4]>CW_PotUptRange[Zn2,3] then 1 else 0
# CW_Best[Zn2,5] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,5]  and CW_PotUptRange[Zn2,5]>CW_PotUptRange[Zn2,4] then 1 else 0
# CW_Best[Zn2,6] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,6]  and CW_PotUptRange[Zn2,6]>CW_PotUptRange[Zn2,5] then 1 else 0
# CW_Best[Zn2,7] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,7]  and CW_PotUptRange[Zn2,7]>CW_PotUptRange[Zn2,6] then 1 else 0
# CW_Best[Zn2,8] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,8]  and CW_PotUptRange[Zn2,8]>CW_PotUptRange[Zn2,7] then 1 else 0
# CW_Best[Zn2,9] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,9]  and CW_PotUptRange[Zn2,9]>CW_PotUptRange[Zn2,8] then 1 else 0
# CW_Best[Zn2,10] = if CW_MaxUptofRange[Zn2]>0 and CW_MaxUptofRange[Zn2]=CW_PotUptRange[Zn2,10]  and CW_PotUptRange[Zn2,10]>CW_PotUptRange[Zn2,9] then 1 else 0
# CW_Best[Zn3,1] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,1] then 1 else 0
# CW_Best[Zn3,2] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,2]   and CW_PotUptRange[Zn3,2]>CW_PotUptRange[Zn3,1] then 1 else 0
# CW_Best[Zn3,3] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,3]   and CW_PotUptRange[Zn3,3]>CW_PotUptRange[Zn3,2] then 1 else 0
# CW_Best[Zn3,4] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,4]   and CW_PotUptRange[Zn3,4]>CW_PotUptRange[Zn3,3] then 1 else 0
# CW_Best[Zn3,5] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,5]   and CW_PotUptRange[Zn3,5]>CW_PotUptRange[Zn3,4] then 1 else 0
# CW_Best[Zn3,6] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,6]   and CW_PotUptRange[Zn3,6]>CW_PotUptRange[Zn3,5] then 1 else 0
# CW_Best[Zn3,7] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,7]   and CW_PotUptRange[Zn3,7]>CW_PotUptRange[Zn3,6] then 1 else 0
# CW_Best[Zn3,8] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,8]  and CW_PotUptRange[Zn3,8]>CW_PotUptRange[Zn3,7]  then 1 else 0
# CW_Best[Zn3,9] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,9]   and CW_PotUptRange[Zn3,9]>CW_PotUptRange[Zn3,8] then 1 else 0
# CW_Best[Zn3,10] = if CW_MaxUptofRange[Zn3]>0 and CW_MaxUptofRange[Zn3]=CW_PotUptRange[Zn3,10]   and CW_PotUptRange[Zn3,10]>CW_PotUptRange[Zn3,9] then 1 else 0
# CW_Best[Zn4,1] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,1] then 1 else 0
# CW_Best[Zn4,2] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,2]   and CW_PotUptRange[Zn4,2]>CW_PotUptRange[Zn4,1] then 1 else 0
# CW_Best[Zn4,3] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,3]   and CW_PotUptRange[Zn4,3]>CW_PotUptRange[Zn4,2] then 1 else 0
# CW_Best[Zn4,4] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,4]   and CW_PotUptRange[Zn4,4]>CW_PotUptRange[Zn4,3] then 1 else 0
# CW_Best[Zn4,5] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,5]  and CW_PotUptRange[Zn4,5]>CW_PotUptRange[Zn4,4] then 1 else 0
# CW_Best[Zn4,6] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,6]   and CW_PotUptRange[Zn4,6]>CW_PotUptRange[Zn4,5] then 1 else 0
# CW_Best[Zn4,7] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,7]  and CW_PotUptRange[Zn4,7]>CW_PotUptRange[Zn4,6]  then 1 else 0
# CW_Best[Zn4,8] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,8] and CW_PotUptRange[Zn4,8]>CW_PotUptRange[Zn4,7] then 1 else 0
# CW_Best[Zn4,9] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,9]  and CW_PotUptRange[Zn4,9]>CW_PotUptRange[Zn4,8] then 1 else 0
# CW_Best[Zn4,10] = if CW_MaxUptofRange[Zn4]>0 and CW_MaxUptofRange[Zn4]=CW_PotUptRange[Zn4,10]   and CW_PotUptRange[Zn4,10]>CW_PotUptRange[Zn4,9] then 1 else 0

zonebuf_df$CW_PotUptRange_before <- NA
zonebuf_df[zonebuf_df$buf_id %in% 2:10, ]$CW_PotUptRange_before <- zonebuf_df[zonebuf_df$buf_id %in% 1:9, ]$CW_PotUptRange

zonebuf_df$CW_MaxUptofRange <- rep(zone_df$CW_MaxUptofRange, nrow(buf_df))
zonebuf_df$CW_Best <- ifelse(
  zonebuf_df$CW_MaxUptofRange > 0 &
    zonebuf_df$CW_MaxUptofRange == zonebuf_df$CW_PotUptRange &
    zonebuf_df$CW_PotUptRange > zonebuf_df$CW_PotUptRange_before,
  1,
  0
)
zb1 <- zonebuf_df[zonebuf_df$buf_id == 1, ]
zonebuf_df[zonebuf_df$buf_id == 1, ]$CW_Best <- ifelse(zb1$CW_MaxUptofRange >
                                                         0 & zb1$CW_MaxUptofRange == zb1$CW_PotUptRange,
                                                       1,
                                                       0)

# CW_PotSoilbest[Zone] = CW_Best[Zone,1]*CW_PotSoil[Zone,1]+
#   CW_Best[Zone,2]*CW_PotSoil[Zone,2]+
#   CW_Best[Zone,3]*CW_PotSoil[Zone,3]+
#   CW_Best[Zone,4]*CW_PotSoil[Zone,4]+
#   CW_Best[Zone,5]*CW_PotSoil[Zone,5]+
#   CW_Best[Zone,6]*CW_PotSoil[Zone,6]+
#   CW_Best[Zone,7]*CW_PotSoil[Zone,7]+
#   CW_Best[Zone,8]*CW_PotSoil[Zone,8]+
#   CW_Best[Zone,9]*CW_PotSoil[Zone,9]+
#   CW_Best[Zone,10]*CW_PotSoil[Zone,10]

zonebuf_df$CW_PotSoilbest_a <- zonebuf_df$CW_Best * zonebuf_df$CW_PotSoil
zone_df$CW_PotSoilbest <- aggregate(zonebuf_df["CW_PotSoilbest_a"], zonebuf_df["zone"], sum)$CW_PotSoilbest_a


# CW_Pot[Zone] = IF(Rt_CAmount[Zone]>0)THEN CW_PotSoilbest[Zone]+  CW_PotRadial[Zone] ELSE(Cq_PotSuctAlphMinCurr[Zone])
zone_df$CW_Pot <- ifelse(
  zone_df$Rt_CAmount > 0,
  zone_df$CW_PotSoilbest +  zone_df$CW_PotRadial,
  zone_df$Cq_PotSuctAlphMinCurr
)


# CW_DemandRedFac[Zone] = IF(CW_PotSuctHalf[Zone]<CW_Pot[Zone]*0.00001) and (1+(CW_Pot[Zone]/CW_PotSuctHalf[Zone])^CW_m[Zone])<>0THEN (1/(1+(CW_Pot[Zone]/CW_PotSuctHalf[Zone])^CW_m[Zone])) ELSE 0
zone_df$CW_DemandRedFac <- ifelse((zone_df$CW_PotSuctHalf < zone_df$CW_Pot *
                                     0.00001) &
                                    (1 + (zone_df$CW_Pot / zone_df$CW_PotSuctHalf)^zone_df$CW_m) != 0,
                                  1 / (1 + (zone_df$CW_Pot / zone_df$CW_PotSuctHalf)^zone_df$CW_m),
                                  0
)

# CW_HEqFactor[Zone] = Cq_ConductivityCurr[Zone]*PI*Cq_RtDiam[Zone]*(0.5+0.5*CW_DemandRedFac[Zone])*10
zone_df$CW_HEqFactor <- zone_df$Cq_ConductivityCurr * pi * zone_df$Cq_RtDiam *
  (0.5 + 0.5 * zone_df$CW_DemandRedFac) * 10

# CW_EqTheta1[Zone] = W_ThetaSat1/(1+(ABS(W_Alpha1*CW_PStem[Zone]))^W_n1)^(1-1/W_n1)
# CW_EqTheta2[Zone] = W_ThetaSat2/(1+(ABS(W_Alpha2*CW_PStem[Zone]))^W_n2)^(1-1/W_n2)
# CW_EqTheta3[Zone] = W_ThetaSat3/(1+(ABS(W_Alpha3*CW_PStem[Zone]))^W_n3)^(1-1/W_n3)
# CW_EqTheta4[Zone] = W_ThetaSat4/(1+(ABS(W_Alpha4*CW_PStem[Zone]))^W_n4)^(1-1/W_n4)

zonelayer_df$CW_EqTheta <- zonelayer_df$W_ThetaSat / (1 + (abs(
  zonelayer_df$W_Alpha * zonelayer_df$CW_PStem
))^zonelayer_df$W_n)^(1 - 1 / zonelayer_df$W_n)


# P_PrevCropOK? = if P_UseCropStopRule? = 1 then P_FlagPrevCropOK? else 1
P_PrevCropOK_is <- ifelse(pars$P_par$P_UseCropStopRule_is == 1,
                          pars$P_par$P_FlagPrevCropOK_is,
                          1)
zonelayer_df$P_PrevCropOK_is <- P_PrevCropOK_is 

# C_GrowsToday? = if AF_Crop? = 0 then 0 else 1
zonelayer_df$C_GrowsToday_is <- pars$AF_par$AF_Crop_is

# CW_PotDelta1[Zone] = IF TIME=(int(Ca_PlantTime[Zone])) and C_GrowsToday? = 1 and  P_PrevCropOK? = 1 then(CW_EqTheta1[Zone]-W_Theta1[Zone])*min(W_TCW_Constant, Rt_CLrv1[Zone]*CW_HEqFactor[Zone]*CW_HEGrad1[Zone]) else 0
# CW_PotDelta2[Zone] = IF TIME=(int(Ca_PlantTime[Zone])) and C_GrowsToday? = 1 and  P_PrevCropOK? = 1 then(CW_EqTheta2[Zone]-W_Theta2[Zone])*min(W_TCW_Constant, Rt_CLrv2[Zone]*CW_HEqFactor[Zone]*CW_HEGrad2[Zone]) else 0
# CW_PotDelta3[Zone] = IF TIME=(int(Ca_PlantTime[Zone])) and C_GrowsToday? = 1 and  P_PrevCropOK? = 1 then(CW_EqTheta3[Zone]-W_Theta3[Zone])*min(W_TCW_Constant, Rt_CLrv3[Zone]*CW_HEqFactor[Zone]*CW_HEGrad3[Zone]) else 0
# CW_PotDelta4[Zone] = IF TIME=(int(Ca_PlantTime[Zone])) and C_GrowsToday? = 1 and  P_PrevCropOK? = 1 then(CW_EqTheta4[Zone]-W_Theta4[Zone])*min(W_TCW_Constant, Rt_CLrv4[Zone]*CW_HEqFactor[Zone]*CW_HEGrad4[Zone]) else 0

zonelayer_df$CW_HEqFactor <- rep(zone_df$CW_HEqFactor, nlayer)
zonelayer_df$Ca_PlantTime <- rep(zone_df$Ca_PlantTime, nlayer)
zonelayer_df$time <- time

zonelayer_df$CW_PotDelta <- ifelse(
  zonelayer_df$time == (floor(zonelayer_df$Ca_PlantTime)) &
    zonelayer_df$C_GrowsToday_is == 1 &
    zonelayer_df$P_PrevCropOK_is == 1,
  (zonelayer_df$CW_EqTheta - zonelayer_df$W_Theta) *
    pmin(
      W_TCW_Constant,
      zonelayer_df$Rt_CLrv * zonelayer_df$CW_HEqFactor * zonelayer_df$CW_HEGrad
    ),
  0
)

# CW_PotFlux1[Zone] = CW_PotDelta1[Zone]*AF_DepthAct1[Zone]*1000
# CW_PotFlux2[Zone] = CW_PotDelta2[Zone]*AF_Depth2[Zone]*1000
# CW_PotFlux3[Zone] = CW_PotDelta3[Zone]*AF_Depth3[Zone]*1000
# CW_PotFlux4[Zone] = CW_PotDelta4[Zone]*AF_Depth4[Zone]*1000
zonelayer_df$CW_PotFlux <- zonelayer_df$CW_PotDelta * zonelayer_df$AF_Depth *
  1000

# CW_PotFluxSumAbs1[Zone] = ABS(CW_PotFlux1[Zone])
# CW_PotFluxSumAbs2[Zone] = ABS(CW_PotFlux2[Zone])
# CW_PotFluxSumAbs3[Zone] = ABS(CW_PotFlux3[Zone])
# CW_PotFluxSumAbs4[Zone] = ABS(CW_PotFlux4[Zone])
zonelayer_df$CW_PotFluxSumAbs <- abs(zonelayer_df$CW_PotFlux)

# CW_PotFluxSumAbs[Zone] = CW_PotFluxSumAbs1[Zone]+CW_PotFluxSumAbs2[Zone]+CW_PotFluxSumAbs3[Zone]+CW_PotFluxSumAbs4[Zone]
zone_df$CW_PotFluxSumAbs <- aggregate(zonelayer_df["CW_PotFluxSumAbs"], zonelayer_df["zone"], sum)$CW_PotFluxSumAbs

# CW_PotFluxSum[Zone] = CW_PotFlux1[Zone]+CW_PotFlux2[Zone]+CW_PotFlux3[Zone]+CW_PotFlux4[Zone]
zone_df$CW_PotFluxSum <- aggregate(zonelayer_df["CW_PotFlux"], zonelayer_df["zone"], sum)$CW_PotFlux

# CW_PotFluxNeg[Zone] = 0.5*(CW_PotFluxSumAbs[Zone]-CW_PotFluxSum[Zone])
zone_df$CW_PotFluxNeg <- 0.5 * (zone_df$CW_PotFluxSumAbs - zone_df$CW_PotFluxSum)

# CW_PotFluxPos[Zone] = 0.5*(CW_PotFluxSumAbs[Zone]+CW_PotFluxSum[Zone])
zone_df$CW_PotFluxPos <- 0.5 * (zone_df$CW_PotFluxSumAbs + zone_df$CW_PotFluxSum)

# CW_ScalingFacPos[Zone] = if CW_PotFluxPos[Zone]> 0 then if CW_PotFluxPos[Zone] > CW_PotFluxNeg[Zone]then CW_PotFluxNeg[Zone]/( CW_PotFluxPos[Zone]) else 1 else 0
zone_df$CW_ScalingFacPos <- ifelse(
  zone_df$CW_PotFluxPos > 0,
  ifelse(
    zone_df$CW_PotFluxPos > zone_df$CW_PotFluxNeg,
    zone_df$CW_PotFluxNeg / zone_df$CW_PotFluxPos,
    1
  ),
  0
)

# CW_ScalingFacNeg[Zone] = if CW_PotFluxNeg[Zone]> 0 then if CW_PotFluxNeg[Zone] > CW_PotFluxPos[Zone]then CW_PotFluxPos[Zone]/( CW_PotFluxNeg[Zone]) else 1 else 0
zone_df$CW_ScalingFacNeg <- ifelse(
  zone_df$CW_PotFluxNeg > 0,
  ifelse(
    zone_df$CW_PotFluxNeg > zone_df$CW_PotFluxPos,
    zone_df$CW_PotFluxPos / zone_df$CW_PotFluxNeg,
    1
  ),
  0
)

zonelayer_df$CW_ScalingFacPos <- rep(zone_df$CW_ScalingFacPos, nlayer)
zonelayer_df$CW_ScalingFacNeg <- rep(zone_df$CW_ScalingFacNeg, nlayer)
# CW_HydFlux1[Zone] = if CW_PotFlux1[Zone]> 0 then CW_ScalingFacPos[Zone]*CW_PotFlux1[Zone] else CW_ScalingFacNeg[Zone]*CW_PotFlux1[Zone]
# CW_HydFlux2[Zone] = if CW_PotFlux2[Zone]> 0 then CW_ScalingFacPos[Zone]*CW_PotFlux2[Zone] else CW_ScalingFacNeg[Zone]*CW_PotFlux2[Zone]
# CW_HydFlux3[Zone] = if CW_PotFlux3[Zone]> 0 then CW_ScalingFacPos[Zone]*CW_PotFlux3[Zone] else CW_ScalingFacNeg[Zone]*CW_PotFlux3[Zone]
# CW_HydFlux4[Zone] = if CW_PotFlux4[Zone]> 0 then CW_ScalingFacPos[Zone]*CW_PotFlux4[Zone] else CW_ScalingFacNeg[Zone]*CW_PotFlux4[Zone]
zonelayer_df$CW_HydFlux <- ifelse(
  zonelayer_df$CW_PotFlux > 0,
  zonelayer_df$CW_ScalingFacPos * zonelayer_df$CW_PotFlux,
  zonelayer_df$CW_ScalingFacNeg * zonelayer_df$CW_PotFlux
)

# W_HydEquil1[Zone] = (ARRAYSUM(TW_HydFlux1[Zone,*])+CW_HydFlux1[Zone])
# W_HydEquil2[Zone] = (ARRAYSUM(TW_HydFlux2[Zone,*])+CW_HydFlux2[Zone])
# W_HydEquil3[Zone] = (ARRAYSUM(TW_HydFlux3[Zone,*])+CW_HydFlux3[Zone])
# W_HydEquil4[Zone] = (ARRAYSUM(TW_HydFlux4[Zone,*])+CW_HydFlux4[Zone])

zonelayer_df$TW_HydFlux_sum  <- aggregate(zonelayertree_df["TW_HydFlux"], zonelayertree_df[c("zone", "layer")], sum)$TW_HydFlux
zonelayer_df$W_HydEquil <- zonelayer_df$TW_HydFlux_sum + zonelayer_df$CW_HydFlux

# W_Seep12[Zone] = If W_WatDef1[Zone]<0 then 0 else 0.01*W_SeepScalar*W_Stock1[Zone]*W_PhiTheta1[Zone]/AF_DepthAct1[Zone]
# W_Seep23[Zone] = If W_WatDef2[Zone]<0 then 0 else 0.01*W_SeepScalar*W_Stock2[Zone]*W_PhiTheta2[Zone]/AF_Depth2[Zone]
# W_Seep34[Zone] = If W_WatDef3[Zone]<0 then 0 else 0.01*W_SeepScalar*W_Stock3[Zone]*W_PhiTheta3[Zone]/AF_Depth3[Zone]
# W_Seep4Out[Zone] = If W_WatDef4[Zone]<0 then 0 else 0.01*W_SeepScalar*W_Stock4[Zone]*W_PhiTheta4[Zone]/AF_Depth4[Zone]

zonelayer_df$W_Seep <- ifelse(
  zonelayer_df$W_WatDef < 0,
  0,
  0.01 * pars$W_par$W_SeepScalar * zonelayer_df$W_Stock * zonelayer_df$W_PhiTheta /
    zonelayer_df$AF_Depth
)



zonelayer_df$W_Seep_before <- 0
zonelayer_df[zonelayer_df$layer %in% 2:4, ]$W_Seep_before <- zonelayer_df[zonelayer_df$layer %in% 1:3, ]$W_Seep

# W_NetSeep1[Zone] = W_Seep12[Zone]
# W_NetSeep2[Zone] = -W_Seep12[Zone]+W_Seep23[Zone]
# W_NetSeep3[Zone] = -W_Seep23[Zone]+W_Seep34[Zone]
# W_NetSeep4[Zone] = -W_Seep34[Zone]+W_Seep4Out[Zone]
zonelayer_df$W_NetSeep <- zonelayer_df$W_Seep - zonelayer_df$W_Seep_before

# W_In1[Zone] = W_LatRecharge1[Zone]+Rain_Infiltr[Zone]-W_V1Drain[Zone]+W_HydEquil1[Zone]-W_Seep12[Zone]
# W_In2[Zone] = W_V1Drain[Zone]-W_V2Drain[Zone]+W_LatRecharge2[Zone]+W_HydEquil2[Zone]-W_NetSeep2[Zone]
# W_In3[Zone] = W_V2Drain[Zone]-W_V3Drain[Zone]+W_LatRecharge3[Zone]+W_HydEquil3[Zone]-W_NetSeep3[Zone]
# W_In4[Zone] = W_V3Drain[Zone]-W_V4Drain[Zone]+W_LatRecharge4[Zone]+W_HydEquil4[Zone]-W_NetSeep4[Zone]

zonelayer_df$W_VDrain_up <- c(zone_df$Rain_Infiltr, zonelayer_df[zonelayer_df$layer %in% 1:3, ]$W_VDrain)
zonelayer_df$W_In <- zonelayer_df$W_VDrain_up - zonelayer_df$W_VDrain + zonelayer_df$W_LatRecharge + zonelayer_df$W_HydEquil -
  c(zonelayer_df[zonelayer_df$layer == 1, ]$W_Seep, zonelayer_df[zonelayer_df$layer %in% 2:4, ]$W_NetSeep)


# W_MaxSoilEvap[Zone] = W_Stock1[Zone]-AF_DepthAct1[Zone]*W_ThetaInacc1[Zone]*1000
zl1 <- zonelayer_df[zonelayer_df$layer == 1, ]
zone_df$W_MaxSoilEvap <- zl1$W_Stock - zl1$AF_Depth * zl1$W_ThetaInacc *
  1000

# Evap_LightNotCaptured[Zone] = 1-ARRAYSUM(Light_TCap1234[Zone,*])-Light_CRelCap[Zone]
zone_df$Evap_LightNotCaptured <- 1 - aggregate(zonetree_df["Light_TCap1234"], zonetree_df["zone"], sum)$Light_TCap1234 -
  zone_df$Light_CRelCap

# Evap_PotSoilEvap[Zone] = Evap_EpotDemandNotMetBy_CanInterc*Evap_LightNotCaptured[Zone]
zone_df$Evap_PotSoilEvap <- Evap_EpotDemandNotMetBy_CanInterc * zone_df$Evap_LightNotCaptured

# Evap_SlashEvap[Zone] = Evap_SlashDryFact*min(Evap_SlashWater[Zone],max(0,(Evap_Pot-Rain_InterceptEvapAvg)))
zone_df$Evap_SlashEvap <- pars$Evap_par$Evap_SlashDryFact * pmin(zone_df$Evap_SlashWater, pmax(0, (Evap_Pot -
                                                                                                     Rain_InterceptEvapAvg)))

# Evap_RedFact[Zone] = min(((W_Theta1[Zone]-W_ThetaInacc1[Zone])/(W_FieldCap1[Zone]-W_ThetaInacc1[Zone]))*EXP(-0.0034*(C_LAI[Zone]+ARRAYSUM(T_LAIEff[Zone,*])+Evap_MulchEffSurfLit*Mc_LAIperNecmss[Zone]*Mc_Struc[Zone])),1)
zone_df$T_LAIEff_sum <- aggregate(zonetree_df["T_LAIEff"], zonetree_df["zone"], sum)$T_LAIEff
zone_df$Evap_RedFact <- pmin(((zl1$W_Theta - zl1$W_ThetaInacc) / (zl1$W_FieldCap - zl1$W_ThetaInacc)
) *
  exp(
    -0.0034 * (
      zone_df$C_LAI + zone_df$T_LAIEff_sum + pars$Evap_par$Evap_MulchEffSurfLit * zone_df$Mc_LAIperNecmss * zone_df$Mc_Struc
    )
  ), 1)


zl1 <- zonelayer_df[zonelayer_df$layer == 1, ]
zdw <- zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]

# S&B_SlashMoist[Zone] = if S&B_FineNecromass[Zone,DW]>0 then Evap_SlashWater[Zone]/S&B_FineNecromass[Zone,DW] else 0
zone_df$SB_SlashMoist <- ifelse(zdw$SB_FineNecromass > 0,
                                zone_df$Evap_SlashWater / zdw$SB_FineNecromass,
                                0)


# S&B_IsSlashDry?[Zone] = if S&B_FineNecromass[Zone,DW] > 0 then if (S&B_FineNecromass[Zone,DW]*S&B_SlashMoist[Zone]+Rain_CanopyWater[Zone]) /S&B_FineNecromass[Zone,DW] < S&B_CritMoist  then 1 else 0 else 0
zone_df$SB_IsSlashDry_is <- ifelse(
  zdw$SB_FineNecromass > 0,
  ifelse(
    (
      zdw$SB_FineNecromass * zone_df$SB_SlashMoist + zone_df$Rain_CanopyWater
    ) / zdw$SB_FineNecromass < pars$SB_par$SB_CritMoist,
    1,
    0
  ),
  0
)


# S&B_Fire? = if S&B_FireTime? = 1 and ARRAYMEAN(S&B_IsSlashDry?[*]) = 1 then 1 else 0
SB_Fire_is <- ifelse(SB_FireTime_is == 1 &
                       mean(zone_df$SB_IsSlashDry_is) == 1, 1, 0)


# S&B_PileUpT? = DELAY(S&B_Fire?,S&B_TimetoPileUp)
SB_PileUpT_is <- delay(SB_Fire_is, pars$SB_par$SB_TimetoPileUp, 0)

# S&B_SecondFire? = delay(S&B_PileUpT?,S&B_2ndFireafterPileUp)
SB_SecondFire_is <- delay(SB_PileUpT_is, pars$SB_par$SB_2ndFireafterPileUp, 0)

# S&B_WoodMoist[Zone] = if S&B_DeadWood[Zone,DW]>0 then Evap_WoodMoist[Zone]/S&B_DeadWood[Zone,DW] else 0
zone_df$SB_WoodMoist <- ifelse(zdw$SB_DeadWood > 0, zone_df$Evap_WoodMoist / zdw$SB_DeadWood, 0)

zone_df$SB_Fire_is <- SB_Fire_is
zone_df$SB_SecondFire_is <- SB_SecondFire_is

# S&B_FireTempIncSurf[Zone] = if S&B_Fire? = 1 or S&B_SecondFire? = 1  then max(0,S&B_DeadWoodFuelFact*S&B_DeadWood[Zone,DW]*(1-S&B_WoodMoist[Zone]*S&B_WetnessTempImp))+
#   S&B_FuelloadFactor*(S&B_FineNecromass[Zone,DW]-S&B_WetnessTempImp*(S&B_SlashMoist[Zone]*S&B_FineNecromass[Zone,DW]+Rain_CanopyWater[Zone])+ Mc_Struc[Zone]/(1000*Mc_Carbon)*(1-S&B_WetnessTempImp*W_Theta1[Zone]))*S&B_WindEffect else 0
zone_df$SB_FireTempIncSurf <- ifelse(
  zone_df$SB_Fire_is == 1 | zone_df$SB_SecondFire_is == 1,
  pmax(
    0,
    pars$SB_par$SB_DeadWoodFuelFact * zdw$SB_DeadWood * (1 - zone_df$SB_WoodMoist * pars$SB_par$SB_WetnessTempImp)
  ) +
    pars$SB_par$SB_FuelloadFactor * (
      zdw$SB_FineNecromass - pars$SB_par$SB_WetnessTempImp * (
        zone_df$SB_SlashMoist * zdw$SB_FineNecromass +
          zone_df$Rain_CanopyWater
      ) + zone_df$Mc_Struc / (1000 * pars$Mc_par$Mc_Carbon) * (1 - pars$SB_par$SB_WetnessTempImp * zl1$W_Theta)
    ) * pars$SB_par$SB_WindEffect,
  0
)

# S&B_FireTempIncTopSoil[Zone] = S&B_FireTempIncSurf[Zone]*((1-W_Theta1[Zone])^2)*(0.05/AF_DepthAct1[Zone])
zone_df$SB_FireTempIncTopSoil <- zone_df$SB_FireTempIncSurf * ((1 - zl1$W_Theta)^2) *
  (0.05 / zl1$AF_Depth)

# S&B_FirEvapS&W[Zone] = if  S&B_FireTempIncTopSoil[Zone]>100 then 1 else 0
zone_df$SB_FirEvapSW <- ifelse(zone_df$SB_FireTempIncTopSoil > 100, 1, 0)

# Evap_Surf[Zone] = min(W_MaxSoilEvap[Zone],max(0,Evap_PotSoilEvap[Zone]-Evap_SlashEvap[Zone])*Evap_RedFact[Zone]+S&B_FirEvapS&W[Zone]*W_Stock1[Zone])
zone_df$Evap_Surf <- pmin(
  zone_df$W_MaxSoilEvap,
  pmax(0, zone_df$Evap_PotSoilEvap - zone_df$Evap_SlashEvap) * zone_df$Evap_RedFact + zone_df$SB_FirEvapSW * zl1$W_Stock
)

# W_StockAcc1[Zone] =  max(0,W_Stock1[Zone]- Evap_Surf[Zone]- W_ThetaInacc1[Zone]*AF_DepthAct1[Zone]*1000)
# W_StockAcc2[Zone] = max(0,W_Stock2[Zone]-W_ThetaInacc2[Zone]*AF_Depth2[Zone]*1000)
# W_StockAcc3[Zone] = max(0,W_Stock3[Zone]-W_ThetaInacc3[Zone]*AF_Depth3[Zone]*1000)
# W_StockAcc4[Zone] = max(0,W_Stock4[Zone]-W_ThetaInacc4[Zone]*AF_Depth4[Zone]*1000)

zonelayer_df$W_StockAcc <- pmax(0,
                                zonelayer_df$W_Stock - zonelayer_df$W_ThetaInacc * zonelayer_df$AF_Depth *
                                  1000)
zonelayer_df[zonelayer_df$layer == 1, ]$W_StockAcc <- pmax(0,
                                                           zl1$W_Stock - zone_df$Evap_Surf - zl1$W_ThetaInacc * zl1$AF_Depth * 1000)

# CW_PotAct[Zone] = CW_Pot[Zone] - (1-CW_DemandRedFac[Zone])*CW_PotRadial[Zone]
zone_df$CW_PotAct <- zone_df$CW_Pot - (1 - zone_df$CW_DemandRedFac) * zone_df$CW_PotRadial

# TW_PotAct[Tree] = min(0,TW_PotBest[Tree]-(1-TW_DemandRedFac[Tree])*(TW_PotRadial[Tree]+TW_PotLongitudinal[Tree]))
tree_df$TW_PotAct <- pmin(
  0,
  tree_df$TW_PotBest - (1 - tree_df$TW_DemandRedFac) * (tree_df$TW_PotRadial + tree_df$TW_PotLongitudinal)
)
zonetree_df$TW_PotAct <- rep(tree_df$TW_PotAct, each = nzone)

tree_df$TW_DistAxialTransp_mean <- aggregate(zonetree_df["TW_DistAxialTransp"], zonetree_df["tree_id"], mean)$TW_DistAxialTransp
zonetree_df$TW_DistAxialTransp_mean <- rep(tree_df$TW_DistAxialTransp_mean, each = nzone)

zonetree_df$TW_DemandRedFac <- rep(tree_df$TW_DemandRedFac, each = nzone)
zonetree_df$TW_PotLongitudinal <- rep(tree_df$TW_PotLongitudinal, each = nzone)
# TW_PotRhizZn[Zone,Tree] = TW_PotAct[Tree]-(TW_DistAxialTransp[Tree,Zone]/ARRAYMEAN(TW_DistAxialTransp[Tree,*]))*TW_DemandRedFac[Tree]*TW_PotLongitudinal[Tree]
zonetree_df$TW_PotRhizZn <- zonetree_df$TW_PotAct - (zonetree_df$TW_DistAxialTransp /
                                                       zonetree_df$TW_DistAxialTransp_mean) * zonetree_df$TW_DemandRedFac * zonetree_df$TW_PotLongitudinal

# CW_PotActCor[Zone] = if CW_PotAct[Zone]<0 then CW_PotAct[Zone] else -50000
zone_df$CW_PotActCor <- ifelse(zone_df$CW_PotAct < 0, zone_df$CW_PotAct, -50000)

# TW_PotRhizZnCor[Tree,Zone] = if TW_PotRhizZn[Zone,Tree]<0 then TW_PotRhizZn[Zone,Tree] else -50000
zonetree_df$TW_PotRhizZnCor <- ifelse(zonetree_df$TW_PotRhizZn < 0, zonetree_df$TW_PotRhizZn, -50000)

# W_PRS[Zone] = min(CW_PotAct[Zone],TW_PotRhizZn[Zone,Sp1],TW_PotRhizZn[Zone,Sp2],TW_PotRhizZn[Zone,Sp3])
# W_PRW[Zone] = MAX(CW_PotActCor[Zone],TW_PotRhizZnCor[Sp1,Zone],TW_PotRhizZnCor[Sp2,Zone],TW_PotRhizZnCor[Sp3,Zone])
zone_df$W_PRS <- pmin(
  zone_df$CW_PotAct,
  zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZn,
  zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZn,
  zonetree_df[zonetree_df$tree_id == 3, ]$TW_PotRhizZn
)
zone_df$W_PRW <- pmax(
  zone_df$CW_PotActCor,
  zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZnCor,
  zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZnCor,
  zonetree_df[zonetree_df$tree_id == 3, ]$TW_PotRhizZnCor
)

# W_FlagCropS[Zone] = IF  abs(CW_PotAct[Zone]- W_PRS[Zone])<0.0001 THEN 1 ELSE 0
# W_FlagCropW[Zone] = IF abs(CW_PotAct[Zone]- W_PRW[Zone])<0.0001 THEN 1 ELSE 0
zone_df$W_FlagCropS <- ifelse(abs(zone_df$CW_PotAct - zone_df$W_PRS) <
                                0.0001, 1, 0)
zone_df$W_FlagCropW <- ifelse(abs(zone_df$CW_PotAct - zone_df$W_PRW) <
                                0.0001, 1, 0)

zonetree_df$W_PRS <- rep(zone_df$W_PRS, ntree)
# W_FlagTreeS[Zone,Tree] = IF  abs(TW_PotRhizZn[Zone,Tree] - W_PRS[Zone] )< 0.0001 THEN 1 ELSE 0
zonetree_df$W_FlagTreeS <- ifelse(abs(zonetree_df$TW_PotRhizZn - zonetree_df$W_PRS) < 0.0001, 1, 0)

zone_df$W_FlagTreeS_sum <- aggregate(zonetree_df["W_FlagTreeS"], zonetree_df["zone"], sum)$W_FlagTreeS

# W_TotSFlags[Zone] = W_FlagCropS[Zone]+ARRAYSUM(W_FlagTreeS[Zone,*])
zone_df$W_TotSFlags <- zone_df$W_FlagCropS + zone_df$W_FlagTreeS_sum



# W_PRMid1[Zone] = if W_TotSFlags[Zone]>1  then W_PRS[Zone] else IF abs(CW_PotAct[Zone]-W_PRW[Zone])>0.00000001 AND abs(CW_PotAct[Zone]-W_PRS[Zone])>0.00000001 THEN CW_PotAct[Zone] ELSE IF abs(TW_PotRhizZn[Zone,Sp1]-W_PRW[Zone])>0.00000001 AND abs(TW_PotRhizZn[Zone,Sp1]-W_PRS[Zone])>0.00000001 THEN TW_PotRhizZn[Zone,Sp1] ELSE  IF abs(TW_PotRhizZn[Zone,Sp2]-W_PRW[Zone])>0.00000001 AND abs(TW_PotRhizZn[Zone,Sp2]-W_PRS[Zone])>0.00000001 THEN TW_PotRhizZn[Zone,Sp2] ELSE TW_PotRhizZn[Zone,Sp3]
# W_PRMid2[Zone] = if W_TotSFlags[Zone]>2 then W_PRS[Zone] else IF abs(CW_PotAct[Zone]-W_PRW[Zone])>0.00000001 AND abs(CW_PotAct[Zone]-W_PRS[Zone])>0.00000001 AND abs(CW_PotAct[Zone]-W_PRMid1[Zone])>0.00000001 THEN CW_PotAct[Zone] ELSE IF abs(TW_PotRhizZn[Zone,Sp1]-W_PRW[Zone])>0.00000001 AND abs(TW_PotRhizZn[Zone,Sp1]-W_PRS[Zone])>0.00000001 AND abs(TW_PotRhizZn[Zone,Sp1]-W_PRMid1[Zone])>0.00000001 THEN TW_PotRhizZn[Zone,Sp1] ELSE  IF abs(TW_PotRhizZn[Zone,Sp2]-W_PRW[Zone])>0.00000001 AND abs(TW_PotRhizZn[Zone,Sp2]-W_PRS[Zone])>0.00000001 AND abs(TW_PotRhizZn[Zone,Sp2]-W_PRMid1[Zone])>0.00000001 THEN TW_PotRhizZn[Zone,Sp2] ELSE TW_PotRhizZn[Zone,Sp3]
zone_df$W_PRMid1 <- ifelse(
  zone_df$W_TotSFlags > 1,
  zone_df$W_PRS,
  ifelse(
    abs(zone_df$CW_PotAct - zone_df$W_PRW) > 0.00000001 &
      abs(zone_df$CW_PotAct - zone_df$W_PRS) > 0.00000001,
    zone_df$CW_PotAct,
    ifelse(
      abs(zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZn - zone_df$W_PRW) > 0.00000001 &
        abs(zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZn - zone_df$W_PRS) > 0.00000001,
      zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZn,
      ifelse(
        abs(zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZn - zone_df$W_PRW) > 0.00000001 &
          abs(zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZn - zone_df$W_PRS) > 0.00000001,
        zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZn,
        zonetree_df[zonetree_df$tree_id == 3, ]$TW_PotRhizZn
      )
    )
  )
)

zone_df$W_PRMid2 <- ifelse(
  zone_df$W_TotSFlags > 2,
  zone_df$W_PRS,
  ifelse(
    abs(zone_df$CW_PotAct - zone_df$W_PRW) > 0.00000001 &
      abs(zone_df$CW_PotAct - zone_df$W_PRS) > 0.00000001 &
      abs(zone_df$CW_PotAct - zone_df$W_PRMid1) > 0.00000001,
    zone_df$CW_PotAct,
    ifelse(
      abs(zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZn - zone_df$W_PRW) > 0.00000001 &
        abs(zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZn - zone_df$W_PRS) > 0.00000001 &
        abs(zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZn - zone_df$W_PRMid1) > 0.00000001,
      zonetree_df[zonetree_df$tree_id == 1, ]$TW_PotRhizZn,
      ifelse(
        abs(zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZn - zone_df$W_PRW) > 0.00000001 &
          abs(zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZn - zone_df$W_PRS) > 0.00000001 &
          abs(zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZn - zone_df$W_PRMid1) > 0.00000001,
        zonetree_df[zonetree_df$tree_id == 2, ]$TW_PotRhizZn,
        zonetree_df[zonetree_df$tree_id == 3, ]$TW_PotRhizZn
      )
    )
  )
)

# W_PRMS[Zone] = min(W_PRMid1[Zone],W_PRMid2[Zone])
# W_PRMW[Zone] = max(W_PRMid1[Zone],W_PRMid2[Zone])
zone_df$W_PRMS <- pmin(zone_df$W_PRMid1, zone_df$W_PRMid2)
zone_df$W_PRMW <- pmax(zone_df$W_PRMid1, zone_df$W_PRMid2)

# W_FlagCropMS[Zone] = IF abs(CW_PotAct[Zone]- W_PRMS[Zone])<0.0001 THEN 1 ELSE 0
# W_FlagCropMW[Zone] = IF abs(CW_PotAct[Zone]- W_PRMW[Zone])<0.0001 THEN 1 ELSE 0
zone_df$W_FlagCropMS <- ifelse(abs(zone_df$CW_PotAct - zone_df$W_PRMS) <
                                 0.0001, 1, 0)
zone_df$W_FlagCropMW <- ifelse(abs(zone_df$CW_PotAct - zone_df$W_PRMW) <
                                 0.0001, 1, 0)

# W_FlagTreeMS[Zone,Tree] = IF abs(TW_PotRhizZn[Zone,Tree]- W_PRMS[Zone])<0.0001 THEN 1 ELSE 0
# W_FlagTreeMW[Zone,Tree] = IF abs(TW_PotRhizZn[Zone,Tree]- W_PRMW[Zone])<0.0001 THEN 1 ELSE 0
# W_FlagTreeS[Zone,Tree] = IF  abs(TW_PotRhizZn[Zone,Tree] - W_PRS[Zone])< 0.0001 THEN 1 ELSE 0
# W_FlagTreeW[Zone,Tree] = IF abs(TW_PotRhizZn[Zone,Tree] - W_PRW[Zone])<0.0001  THEN 1 ELSE 0

zonetreewater_df$TW_PotRhizZn <- rep(zonetree_df$TW_PotRhizZn, nwater)
zonetreewater_df$W_PR <- NA
zonetreewater_df[zonetreewater_df$water == "MS", ]$W_PR <- rep(zone_df$W_PRMS, ntree)
zonetreewater_df[zonetreewater_df$water == "MW", ]$W_PR <- rep(zone_df$W_PRMW, ntree)
zonetreewater_df[zonetreewater_df$water == "S", ]$W_PR <- rep(zone_df$W_PRS, ntree)
zonetreewater_df[zonetreewater_df$water == "W", ]$W_PR <- rep(zone_df$W_PRW, ntree) 

zonetreewater_df$W_FlagTree <- ifelse(abs(zonetreewater_df$TW_PotRhizZn - zonetreewater_df$W_PR) < 0.0001, 1, 0)

  
  # Rt_LrvMS[Zn1,1] = W_FlagCropMS[Zn1]*Rt_CLrv1[Zn1]+W_FlagTreeMS[Zn1,Sp1]*Rt_TLrv1[Zn1,Sp1]+W_FlagTreeMS[Zn1,Sp2]*Rt_TLrv1[Zn1,Sp2]+W_FlagTreeMS[Zn1,Sp3]*Rt_TLrv1[Zn1,Sp3] + 0*(W_FlagCropMS[Zn1]+W_FlagTreeMS[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvMS[Zn1,2] = W_FlagCropMS[Zn1]*Rt_CLrv2[Zn1]+W_FlagTreeMS[Zn1,Sp1]*Rt_TLrv2[Zn1,Sp1]+W_FlagTreeMS[Zn1,Sp2]*Rt_TLrv2[Zn1,Sp2]+W_FlagTreeMS[Zn1,Sp3]*Rt_TLrv2[Zn1,Sp3] + 0*(W_FlagCropMS[Zn1]+W_FlagTreeMS[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvMS[Zn1,3] = W_FlagCropMS[Zn1]*Rt_CLrv3[Zn1]+W_FlagTreeMS[Zn1,Sp1]*Rt_TLrv3[Zn1,Sp1]+W_FlagTreeMS[Zn1,Sp2]*Rt_TLrv3[Zn1,Sp2]+W_FlagTreeMS[Zn1,Sp3]*Rt_TLrv3[Zn1,Sp3] + 0*(W_FlagCropMS[Zn1]+W_FlagTreeMS[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvMS[Zn1,4] = W_FlagCropMS[Zn1]*Rt_CLrv4[Zn1]+W_FlagTreeMS[Zn1,Sp1]*Rt_TLrv4[Zn1,Sp1]+W_FlagTreeMS[Zn1,Sp2]*Rt_TLrv4[Zn1,Sp2]+W_FlagTreeMS[Zn1,Sp3]*Rt_TLrv4[Zn1,Sp3] + 0*(W_FlagCropMS[Zn1]+W_FlagTreeMS[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvMS[Zn2,1] = W_FlagCropMS[Zn2]*Rt_CLrv1[Zn2]+W_FlagTreeMS[Zn2,Sp1]*Rt_TLrv1[Zn2,Sp1]+W_FlagTreeMS[Zn2,Sp2]*Rt_TLrv1[Zn2,Sp2]+W_FlagTreeMS[Zn2,Sp3]*Rt_TLrv1[Zn2,Sp3] + 0*(W_FlagCropMS[Zn2]+W_FlagTreeMS[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvMS[Zn2,2] = W_FlagCropMS[Zn2]*Rt_CLrv2[Zn2]+W_FlagTreeMS[Zn2,Sp1]*Rt_TLrv2[Zn2,Sp1]+W_FlagTreeMS[Zn2,Sp2]*Rt_TLrv2[Zn2,Sp2]+W_FlagTreeMS[Zn2,Sp3]*Rt_TLrv2[Zn2,Sp3] + 0*(W_FlagCropMS[Zn2]+W_FlagTreeMS[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvMS[Zn2,3] = W_FlagCropMS[Zn2]*Rt_CLrv3[Zn2]+W_FlagTreeMS[Zn2,Sp1]*Rt_TLrv3[Zn2,Sp1]+W_FlagTreeMS[Zn2,Sp2]*Rt_TLrv3[Zn2,Sp2]+W_FlagTreeMS[Zn2,Sp3]*Rt_TLrv3[Zn2,Sp3] + 0*(W_FlagCropMS[Zn2]+W_FlagTreeMS[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvMS[Zn2,4] = W_FlagCropMS[Zn2]*Rt_CLrv4[Zn2]+W_FlagTreeMS[Zn2,Sp1]*Rt_TLrv4[Zn2,Sp1]+W_FlagTreeMS[Zn2,Sp2]*Rt_TLrv4[Zn2,Sp2]+W_FlagTreeMS[Zn2,Sp3]*Rt_TLrv4[Zn2,Sp3] + 0*(W_FlagCropMS[Zn2]+W_FlagTreeMS[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvMS[Zn3,1] = W_FlagCropMS[Zn3]*Rt_CLrv1[Zn3]+W_FlagTreeMS[Zn3,Sp1]*Rt_TLrv1[Zn3,Sp1]+W_FlagTreeMS[Zn3,Sp2]*Rt_TLrv1[Zn3,Sp2]+W_FlagTreeMS[Zn3,Sp3]*Rt_TLrv1[Zn3,Sp3] + 0*(W_FlagCropMS[Zn3]+W_FlagTreeMS[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvMS[Zn3,2] = W_FlagCropMS[Zn3]*Rt_CLrv2[Zn3]+W_FlagTreeMS[Zn3,Sp1]*Rt_TLrv2[Zn3,Sp1]+W_FlagTreeMS[Zn3,Sp2]*Rt_TLrv2[Zn3,Sp2]+W_FlagTreeMS[Zn3,Sp3]*Rt_TLrv2[Zn3,Sp3] + 0*(W_FlagCropMS[Zn3]+W_FlagTreeMS[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvMS[Zn3,3] = W_FlagCropMS[Zn3]*Rt_CLrv3[Zn3]+W_FlagTreeMS[Zn3,Sp1]*Rt_TLrv3[Zn3,Sp1]+W_FlagTreeMS[Zn3,Sp2]*Rt_TLrv3[Zn3,Sp2]+W_FlagTreeMS[Zn3,Sp3]*Rt_TLrv3[Zn3,Sp3] + 0*(W_FlagCropMS[Zn3]+W_FlagTreeMS[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvMS[Zn3,4] = W_FlagCropMS[Zn3]*Rt_CLrv4[Zn3]+W_FlagTreeMS[Zn3,Sp1]*Rt_TLrv4[Zn3,Sp1]+W_FlagTreeMS[Zn3,Sp2]*Rt_TLrv4[Zn3,Sp2]+W_FlagTreeMS[Zn3,Sp3]*Rt_TLrv4[Zn3,Sp3] + 0*(W_FlagCropMS[Zn3]+W_FlagTreeMS[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvMS[Zn4,1] = W_FlagCropMS[Zn4]*Rt_CLrv1[Zn4]+W_FlagTreeMS[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]+W_FlagTreeMS[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]+W_FlagTreeMS[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3] + 0*(W_FlagCropMS[Zn4]+W_FlagTreeMS[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvMS[Zn4,2] = W_FlagCropMS[Zn4]*Rt_CLrv2[Zn4]+W_FlagTreeMS[Zn4,Sp1]*Rt_TLrv2[Zn4,Sp1]+W_FlagTreeMS[Zn4,Sp2]*Rt_TLrv2[Zn4,Sp2]+W_FlagTreeMS[Zn4,Sp3]*Rt_TLrv2[Zn4,Sp3] + 0*(W_FlagCropMS[Zn4]+W_FlagTreeMS[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvMS[Zn4,3] = W_FlagCropMS[Zn4]*Rt_CLrv3[Zn4]+W_FlagTreeMS[Zn4,Sp1]*Rt_TLrv3[Zn4,Sp1]+W_FlagTreeMS[Zn4,Sp2]*Rt_TLrv3[Zn4,Sp2]+W_FlagTreeMS[Zn4,Sp3]*Rt_TLrv3[Zn4,Sp3] + 0*(W_FlagCropMS[Zn4]+W_FlagTreeMS[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvMS[Zn4,4] = W_FlagCropMS[Zn4]*Rt_CLrv4[Zn4]+W_FlagTreeMS[Zn4,Sp1]*Rt_TLrv4[Zn4,Sp1]+W_FlagTreeMS[Zn4,Sp2]*Rt_TLrv4[Zn4,Sp2]+W_FlagTreeMS[Zn4,Sp3]*Rt_TLrv4[Zn4,Sp3] + 0*(W_FlagCropMS[Zn4]+W_FlagTreeMS[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  #
  # Rt_LrvMW[Zn1,1] = W_FlagCropMW[Zn1]*Rt_CLrv1[Zn1]+W_FlagTreeMW[Zn1,Sp1]*Rt_TLrv1[Zn1,Sp1]+W_FlagTreeMW[Zn1,Sp2]*Rt_TLrv1[Zn1,Sp2]+W_FlagTreeMW[Zn1,Sp3]*Rt_TLrv1[Zn1,Sp3] + 0*(W_FlagCropMW[Zn1]+W_FlagTreeMW[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvMW[Zn1,2] = W_FlagCropMW[Zn1]*Rt_CLrv2[Zn1]+W_FlagTreeMW[Zn1,Sp1]*Rt_TLrv2[Zn1,Sp1]+W_FlagTreeMW[Zn1,Sp2]*Rt_TLrv2[Zn1,Sp2]+W_FlagTreeMW[Zn1,Sp3]*Rt_TLrv2[Zn1,Sp3] + 0*(W_FlagCropMW[Zn1]+W_FlagTreeMW[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvMW[Zn1,3] = W_FlagCropMW[Zn1]*Rt_CLrv3[Zn1]+W_FlagTreeMW[Zn1,Sp1]*Rt_TLrv3[Zn1,Sp1]+W_FlagTreeMW[Zn1,Sp2]*Rt_TLrv3[Zn1,Sp2]+W_FlagTreeMW[Zn1,Sp3]*Rt_TLrv3[Zn1,Sp3] + 0*(W_FlagCropMW[Zn1]+W_FlagTreeMW[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvMW[Zn1,4] = W_FlagCropMW[Zn1]*Rt_CLrv4[Zn1]+W_FlagTreeMW[Zn1,Sp1]*Rt_TLrv4[Zn1,Sp1]+W_FlagTreeMW[Zn1,Sp2]*Rt_TLrv4[Zn1,Sp2]+W_FlagTreeMW[Zn1,Sp3]*Rt_TLrv4[Zn1,Sp3] + 0*(W_FlagCropMW[Zn1]+W_FlagTreeMW[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvMW[Zn2,1] = W_FlagCropMW[Zn2]*Rt_CLrv1[Zn2]+W_FlagTreeMW[Zn2,Sp1]*Rt_TLrv1[Zn2,Sp1]+W_FlagTreeMW[Zn2,Sp2]*Rt_TLrv1[Zn2,Sp2]+W_FlagTreeMW[Zn2,Sp3]*Rt_TLrv1[Zn2,Sp3] + 0*(W_FlagCropMW[Zn2]+W_FlagTreeMW[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvMW[Zn2,2] = W_FlagCropMW[Zn2]*Rt_CLrv2[Zn2]+W_FlagTreeMW[Zn2,Sp1]*Rt_TLrv2[Zn2,Sp1]+W_FlagTreeMW[Zn2,Sp2]*Rt_TLrv2[Zn2,Sp2]+W_FlagTreeMW[Zn2,Sp3]*Rt_TLrv2[Zn2,Sp3] + 0*(W_FlagCropMW[Zn2]+W_FlagTreeMW[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvMW[Zn2,3] = W_FlagCropMW[Zn2]*Rt_CLrv3[Zn2]+W_FlagTreeMW[Zn2,Sp1]*Rt_TLrv3[Zn2,Sp1]+W_FlagTreeMW[Zn2,Sp2]*Rt_TLrv3[Zn2,Sp2]+W_FlagTreeMW[Zn2,Sp3]*Rt_TLrv3[Zn2,Sp3] + 0*(W_FlagCropMW[Zn2]+W_FlagTreeMW[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvMW[Zn2,4] = W_FlagCropMW[Zn2]*Rt_CLrv4[Zn2]+W_FlagTreeMW[Zn2,Sp1]*Rt_TLrv4[Zn2,Sp1]+W_FlagTreeMW[Zn2,Sp2]*Rt_TLrv4[Zn2,Sp2]+W_FlagTreeMW[Zn2,Sp3]*Rt_TLrv4[Zn2,Sp3] + 0*(W_FlagCropMW[Zn2]+W_FlagTreeMW[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvMW[Zn3,1] = W_FlagCropMW[Zn3]*Rt_CLrv1[Zn3]+W_FlagTreeMW[Zn3,Sp1]*Rt_TLrv1[Zn3,Sp1]+W_FlagTreeMW[Zn3,Sp2]*Rt_TLrv1[Zn3,Sp2]+W_FlagTreeMW[Zn3,Sp3]*Rt_TLrv1[Zn3,Sp3] + 0*(W_FlagCropMW[Zn3]+W_FlagTreeMW[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvMW[Zn3,2] = W_FlagCropMW[Zn3]*Rt_CLrv2[Zn3]+W_FlagTreeMW[Zn3,Sp1]*Rt_TLrv2[Zn3,Sp1]+W_FlagTreeMW[Zn3,Sp2]*Rt_TLrv2[Zn3,Sp2]+W_FlagTreeMW[Zn3,Sp3]*Rt_TLrv2[Zn3,Sp3] + 0*(W_FlagCropMW[Zn3]+W_FlagTreeMW[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvMW[Zn3,3] = W_FlagCropMW[Zn3]*Rt_CLrv3[Zn3]+W_FlagTreeMW[Zn3,Sp1]*Rt_TLrv3[Zn3,Sp1]+W_FlagTreeMW[Zn3,Sp2]*Rt_TLrv3[Zn3,Sp2]+W_FlagTreeMW[Zn3,Sp3]*Rt_TLrv3[Zn3,Sp3] + 0*(W_FlagCropMW[Zn3]+W_FlagTreeMW[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvMW[Zn3,4] = W_FlagCropMW[Zn3]*Rt_CLrv4[Zn3]+W_FlagTreeMW[Zn3,Sp1]*Rt_TLrv4[Zn3,Sp1]+W_FlagTreeMW[Zn3,Sp2]*Rt_TLrv4[Zn3,Sp2]+W_FlagTreeMW[Zn3,Sp3]*Rt_TLrv4[Zn3,Sp3] + 0*(W_FlagCropMW[Zn3]+W_FlagTreeMW[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvMW[Zn4,1] = W_FlagCropMW[Zn4]*Rt_CLrv1[Zn4]+W_FlagTreeMW[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]+W_FlagTreeMW[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]+W_FlagTreeMW[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3] + 0*(W_FlagCropMW[Zn4]+W_FlagTreeMW[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvMW[Zn4,2] = W_FlagCropMW[Zn4]*Rt_CLrv2[Zn4]+W_FlagTreeMW[Zn4,Sp1]*Rt_TLrv2[Zn4,Sp1]+W_FlagTreeMW[Zn4,Sp2]*Rt_TLrv2[Zn4,Sp2]+W_FlagTreeMW[Zn4,Sp3]*Rt_TLrv2[Zn4,Sp3] + 0*(W_FlagCropMW[Zn4]+W_FlagTreeMW[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvMW[Zn4,3] = W_FlagCropMW[Zn4]*Rt_CLrv3[Zn4]+W_FlagTreeMW[Zn4,Sp1]*Rt_TLrv3[Zn4,Sp1]+W_FlagTreeMW[Zn4,Sp2]*Rt_TLrv3[Zn4,Sp2]+W_FlagTreeMW[Zn4,Sp3]*Rt_TLrv3[Zn4,Sp3] + 0*(W_FlagCropMW[Zn4]+W_FlagTreeMW[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvMW[Zn4,4] = W_FlagCropMW[Zn4]*Rt_CLrv4[Zn4]+W_FlagTreeMW[Zn4,Sp1]*Rt_TLrv4[Zn4,Sp1]+W_FlagTreeMW[Zn4,Sp2]*Rt_TLrv4[Zn4,Sp2]+W_FlagTreeMW[Zn4,Sp3]*Rt_TLrv4[Zn4,Sp3] + 0*(W_FlagCropMW[Zn4]+W_FlagTreeMW[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  #
  # Rt_LrvS[Zn1,1] = W_FlagCropS[Zn1]*Rt_CLrv1[Zn1]+W_FlagTreeS[Zn1,Sp1]*Rt_TLrv1[Zn1,Sp1]+W_FlagTreeS[Zn1,Sp2]*Rt_TLrv1[Zn1,Sp2]+W_FlagTreeS[Zn1,Sp3]*Rt_TLrv1[Zn1,Sp3] + 0*(W_FlagCropS[Zn1]+W_FlagTreeS[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvS[Zn1,2] = W_FlagCropS[Zn1]*Rt_CLrv2[Zn1]+W_FlagTreeS[Zn1,Sp1]*Rt_TLrv2[Zn1,Sp1]+W_FlagTreeS[Zn1,Sp2]*Rt_TLrv2[Zn1,Sp2]+W_FlagTreeS[Zn1,Sp3]*Rt_TLrv2[Zn1,Sp3] + 0*(W_FlagCropS[Zn1]+W_FlagTreeS[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvS[Zn1,3] = W_FlagCropS[Zn1]*Rt_CLrv3[Zn1]+W_FlagTreeS[Zn1,Sp1]*Rt_TLrv3[Zn1,Sp1]+W_FlagTreeS[Zn1,Sp2]*Rt_TLrv3[Zn1,Sp2]+W_FlagTreeS[Zn1,Sp3]*Rt_TLrv3[Zn1,Sp3] + 0*(W_FlagCropS[Zn1]+W_FlagTreeS[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvS[Zn1,4] = W_FlagCropS[Zn1]*Rt_CLrv4[Zn1]+W_FlagTreeS[Zn1,Sp1]*Rt_TLrv4[Zn1,Sp1]+W_FlagTreeS[Zn1,Sp2]*Rt_TLrv4[Zn1,Sp2]+W_FlagTreeS[Zn1,Sp3]*Rt_TLrv4[Zn1,Sp3] + 0*(W_FlagCropS[Zn1]+W_FlagTreeS[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvS[Zn2,1] = W_FlagCropS[Zn2]*Rt_CLrv1[Zn2]+W_FlagTreeS[Zn2,Sp1]*Rt_TLrv1[Zn2,Sp1]+W_FlagTreeS[Zn2,Sp2]*Rt_TLrv1[Zn2,Sp2]+W_FlagTreeS[Zn2,Sp3]*Rt_TLrv1[Zn2,Sp3] + 0*(W_FlagCropS[Zn2]+W_FlagTreeS[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvS[Zn2,2] = W_FlagCropS[Zn2]*Rt_CLrv2[Zn2]+W_FlagTreeS[Zn2,Sp1]*Rt_TLrv2[Zn2,Sp1]+W_FlagTreeS[Zn2,Sp2]*Rt_TLrv2[Zn2,Sp2]+W_FlagTreeS[Zn2,Sp3]*Rt_TLrv2[Zn2,Sp3] + 0*(W_FlagCropS[Zn2]+W_FlagTreeS[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvS[Zn2,3] = W_FlagCropS[Zn2]*Rt_CLrv3[Zn2]+W_FlagTreeS[Zn2,Sp1]*Rt_TLrv3[Zn2,Sp1]+W_FlagTreeS[Zn2,Sp2]*Rt_TLrv3[Zn2,Sp2]+W_FlagTreeS[Zn2,Sp3]*Rt_TLrv3[Zn2,Sp3] + 0*(W_FlagCropS[Zn2]+W_FlagTreeS[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvS[Zn2,4] = W_FlagCropS[Zn2]*Rt_CLrv4[Zn2]+W_FlagTreeS[Zn2,Sp1]*Rt_TLrv4[Zn2,Sp1]+W_FlagTreeS[Zn2,Sp2]*Rt_TLrv4[Zn2,Sp2]+W_FlagTreeS[Zn2,Sp3]*Rt_TLrv4[Zn2,Sp3] + 0*(W_FlagCropS[Zn2]+W_FlagTreeS[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvS[Zn3,1] = W_FlagCropS[Zn3]*Rt_CLrv1[Zn3]+W_FlagTreeS[Zn3,Sp1]*Rt_TLrv1[Zn3,Sp1]+W_FlagTreeS[Zn3,Sp2]*Rt_TLrv1[Zn3,Sp2]+W_FlagTreeS[Zn3,Sp3]*Rt_TLrv1[Zn3,Sp3] + 0*(W_FlagCropS[Zn3]+W_FlagTreeS[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvS[Zn3,2] = W_FlagCropS[Zn3]*Rt_CLrv2[Zn3]+W_FlagTreeS[Zn3,Sp1]*Rt_TLrv2[Zn3,Sp1]+W_FlagTreeS[Zn3,Sp2]*Rt_TLrv2[Zn3,Sp2]+W_FlagTreeS[Zn3,Sp3]*Rt_TLrv2[Zn3,Sp3] + 0*(W_FlagCropS[Zn3]+W_FlagTreeS[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvS[Zn3,3] = W_FlagCropS[Zn3]*Rt_CLrv3[Zn3]+W_FlagTreeS[Zn3,Sp1]*Rt_TLrv3[Zn3,Sp1]+W_FlagTreeS[Zn3,Sp2]*Rt_TLrv3[Zn3,Sp2]+W_FlagTreeS[Zn3,Sp3]*Rt_TLrv3[Zn3,Sp3] + 0*(W_FlagCropS[Zn3]+W_FlagTreeS[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvS[Zn3,4] = W_FlagCropS[Zn3]*Rt_CLrv4[Zn3]+W_FlagTreeS[Zn3,Sp1]*Rt_TLrv4[Zn3,Sp1]+W_FlagTreeS[Zn3,Sp2]*Rt_TLrv4[Zn3,Sp2]+W_FlagTreeS[Zn3,Sp3]*Rt_TLrv4[Zn3,Sp3] + 0*(W_FlagCropS[Zn3]+W_FlagTreeS[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvS[Zn4,1] = W_FlagCropS[Zn4]*Rt_CLrv1[Zn4]+W_FlagTreeS[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]+W_FlagTreeS[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]+W_FlagTreeS[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3] + 0*(W_FlagCropS[Zn4]+W_FlagTreeS[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvS[Zn4,2] = W_FlagCropS[Zn4]*Rt_CLrv2[Zn4]+W_FlagTreeS[Zn4,Sp1]*Rt_TLrv2[Zn4,Sp1]+W_FlagTreeS[Zn4,Sp2]*Rt_TLrv2[Zn4,Sp2]+W_FlagTreeS[Zn4,Sp3]*Rt_TLrv2[Zn4,Sp3] + 0*(W_FlagCropS[Zn4]+W_FlagTreeS[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvS[Zn4,3] = W_FlagCropS[Zn4]*Rt_CLrv3[Zn4]+W_FlagTreeS[Zn4,Sp1]*Rt_TLrv3[Zn4,Sp1]+W_FlagTreeS[Zn4,Sp2]*Rt_TLrv3[Zn4,Sp2]+W_FlagTreeS[Zn4,Sp3]*Rt_TLrv3[Zn4,Sp3] + 0*(W_FlagCropS[Zn4]+W_FlagTreeS[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvS[Zn4,4] = W_FlagCropS[Zn4]*Rt_CLrv4[Zn4]+W_FlagTreeS[Zn4,Sp1]*Rt_TLrv4[Zn4,Sp1]+W_FlagTreeS[Zn4,Sp2]*Rt_TLrv4[Zn4,Sp2]+W_FlagTreeS[Zn4,Sp3]*Rt_TLrv4[Zn4,Sp3] + 0*(W_FlagCropS[Zn4]+W_FlagTreeS[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  #
  # Rt_LrvW[Zn1,1] = W_FlagCropW[Zn1]*Rt_CLrv1[Zn1]+W_FlagTreeW[Zn1,Sp1]*Rt_TLrv1[Zn1,Sp1]+W_FlagTreeW[Zn1,Sp2]*Rt_TLrv1[Zn1,Sp2]+W_FlagTreeW[Zn1,Sp3]*Rt_TLrv1[Zn1,Sp3] + 0*(W_FlagCropW[Zn1]+W_FlagTreeW[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvW[Zn1,2] = W_FlagCropW[Zn1]*Rt_CLrv2[Zn1]+W_FlagTreeW[Zn1,Sp1]*Rt_TLrv2[Zn1,Sp1]+W_FlagTreeW[Zn1,Sp2]*Rt_TLrv2[Zn1,Sp2]+W_FlagTreeW[Zn1,Sp3]*Rt_TLrv2[Zn1,Sp3] + 0*(W_FlagCropW[Zn1]+W_FlagTreeW[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvW[Zn1,3] = W_FlagCropW[Zn1]*Rt_CLrv3[Zn1]+W_FlagTreeW[Zn1,Sp1]*Rt_TLrv3[Zn1,Sp1]+W_FlagTreeW[Zn1,Sp2]*Rt_TLrv3[Zn1,Sp2]+W_FlagTreeW[Zn1,Sp3]*Rt_TLrv3[Zn1,Sp3] + 0*(W_FlagCropW[Zn1]+W_FlagTreeW[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvW[Zn1,4] = W_FlagCropW[Zn1]*Rt_CLrv4[Zn1]+W_FlagTreeW[Zn1,Sp1]*Rt_TLrv4[Zn1,Sp1]+W_FlagTreeW[Zn1,Sp2]*Rt_TLrv4[Zn1,Sp2]+W_FlagTreeW[Zn1,Sp3]*Rt_TLrv4[Zn1,Sp3] + 0*(W_FlagCropW[Zn1]+W_FlagTreeW[Zn1,Sp1]+Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv1[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_LrvW[Zn2,1] = W_FlagCropW[Zn2]*Rt_CLrv1[Zn2]+W_FlagTreeW[Zn2,Sp1]*Rt_TLrv1[Zn2,Sp1]+W_FlagTreeW[Zn2,Sp2]*Rt_TLrv1[Zn2,Sp2]+W_FlagTreeW[Zn2,Sp3]*Rt_TLrv1[Zn2,Sp3] + 0*(W_FlagCropW[Zn2]+W_FlagTreeW[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvW[Zn2,2] = W_FlagCropW[Zn2]*Rt_CLrv2[Zn2]+W_FlagTreeW[Zn2,Sp1]*Rt_TLrv2[Zn2,Sp1]+W_FlagTreeW[Zn2,Sp2]*Rt_TLrv2[Zn2,Sp2]+W_FlagTreeW[Zn2,Sp3]*Rt_TLrv2[Zn2,Sp3] + 0*(W_FlagCropW[Zn2]+W_FlagTreeW[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvW[Zn2,3] = W_FlagCropW[Zn2]*Rt_CLrv3[Zn2]+W_FlagTreeW[Zn2,Sp1]*Rt_TLrv3[Zn2,Sp1]+W_FlagTreeW[Zn2,Sp2]*Rt_TLrv3[Zn2,Sp2]+W_FlagTreeW[Zn2,Sp3]*Rt_TLrv3[Zn2,Sp3] + 0*(W_FlagCropW[Zn2]+W_FlagTreeW[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvW[Zn2,4] = W_FlagCropW[Zn2]*Rt_CLrv4[Zn2]+W_FlagTreeW[Zn2,Sp1]*Rt_TLrv4[Zn2,Sp1]+W_FlagTreeW[Zn2,Sp2]*Rt_TLrv4[Zn2,Sp2]+W_FlagTreeW[Zn2,Sp3]*Rt_TLrv4[Zn2,Sp3] + 0*(W_FlagCropW[Zn2]+W_FlagTreeW[Zn2,Sp1]+Rt_CLrv1[Zn2]+Rt_CLrv2[Zn2]+Rt_CLrv3[Zn2]+Rt_CLrv4[Zn2]+Rt_TLrv2[Zn2,Sp1]+Rt_TLrv1[Zn2,Sp1]+Rt_TLrv3[Zn2,Sp1]+Rt_TLrv4[Zn2,Sp1])
  # Rt_LrvW[Zn3,1] = W_FlagCropW[Zn3]*Rt_CLrv1[Zn3]+W_FlagTreeW[Zn3,Sp1]*Rt_TLrv1[Zn3,Sp1]+W_FlagTreeW[Zn3,Sp2]*Rt_TLrv1[Zn3,Sp2]+W_FlagTreeW[Zn3,Sp3]*Rt_TLrv1[Zn3,Sp3] + 0*(W_FlagCropW[Zn3]+W_FlagTreeW[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvW[Zn3,2] = W_FlagCropW[Zn3]*Rt_CLrv2[Zn3]+W_FlagTreeW[Zn3,Sp1]*Rt_TLrv2[Zn3,Sp1]+W_FlagTreeW[Zn3,Sp2]*Rt_TLrv2[Zn3,Sp2]+W_FlagTreeW[Zn3,Sp3]*Rt_TLrv2[Zn3,Sp3] + 0*(W_FlagCropW[Zn3]+W_FlagTreeW[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvW[Zn3,3] = W_FlagCropW[Zn3]*Rt_CLrv3[Zn3]+W_FlagTreeW[Zn3,Sp1]*Rt_TLrv3[Zn3,Sp1]+W_FlagTreeW[Zn3,Sp2]*Rt_TLrv3[Zn3,Sp2]+W_FlagTreeW[Zn3,Sp3]*Rt_TLrv3[Zn3,Sp3] + 0*(W_FlagCropW[Zn3]+W_FlagTreeW[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvW[Zn3,4] = W_FlagCropW[Zn3]*Rt_CLrv4[Zn3]+W_FlagTreeW[Zn3,Sp1]*Rt_TLrv4[Zn3,Sp1]+W_FlagTreeW[Zn3,Sp2]*Rt_TLrv4[Zn3,Sp2]+W_FlagTreeW[Zn3,Sp3]*Rt_TLrv4[Zn3,Sp3] + 0*(W_FlagCropW[Zn3]+W_FlagTreeW[Zn3,Sp1]+Rt_CLrv1[Zn3]+Rt_CLrv2[Zn3]+Rt_CLrv3[Zn3]+Rt_CLrv4[Zn3]+Rt_TLrv2[Zn3,Sp1]+Rt_TLrv1[Zn3,Sp1]+Rt_TLrv3[Zn3,Sp1]+Rt_TLrv4[Zn3,Sp1])
  # Rt_LrvW[Zn4,1] = W_FlagCropW[Zn4]*Rt_CLrv1[Zn4]+W_FlagTreeW[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]+W_FlagTreeW[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]+W_FlagTreeW[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3] + 0*(W_FlagCropW[Zn4]+W_FlagTreeW[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvW[Zn4,2] = W_FlagCropW[Zn4]*Rt_CLrv2[Zn4]+W_FlagTreeW[Zn4,Sp1]*Rt_TLrv2[Zn4,Sp1]+W_FlagTreeW[Zn4,Sp2]*Rt_TLrv2[Zn4,Sp2]+W_FlagTreeW[Zn4,Sp3]*Rt_TLrv2[Zn4,Sp3] + 0*(W_FlagCropW[Zn4]+W_FlagTreeW[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvW[Zn4,3] = W_FlagCropW[Zn4]*Rt_CLrv3[Zn4]+W_FlagTreeW[Zn4,Sp1]*Rt_TLrv3[Zn4,Sp1]+W_FlagTreeW[Zn4,Sp2]*Rt_TLrv3[Zn4,Sp2]+W_FlagTreeW[Zn4,Sp3]*Rt_TLrv3[Zn4,Sp3] + 0*(W_FlagCropW[Zn4]+W_FlagTreeW[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  # Rt_LrvW[Zn4,4] = W_FlagCropW[Zn4]*Rt_CLrv4[Zn4]+W_FlagTreeW[Zn4,Sp1]*Rt_TLrv4[Zn4,Sp1]+W_FlagTreeW[Zn4,Sp2]*Rt_TLrv4[Zn4,Sp2]+W_FlagTreeW[Zn4,Sp3]*Rt_TLrv4[Zn4,Sp3] + 0*(W_FlagCropW[Zn4]+W_FlagTreeW[Zn4,Sp1]+Rt_CLrv1[Zn4]+Rt_CLrv2[Zn4]+Rt_CLrv3[Zn4]+Rt_CLrv4[Zn4]+Rt_TLrv2[Zn4,Sp1]+Rt_TLrv1[Zn4,Sp1]+Rt_TLrv3[Zn4,Sp1]+Rt_TLrv4[Zn4,Sp1])
  
  
  zonelayerwater_df$W_FlagCrop <- NA
  zonelayerwater_df[zonelayerwater_df$water == "MS", ]$W_FlagCrop <- zone_df$W_FlagCropMS
  zonelayerwater_df[zonelayerwater_df$water == "MW", ]$W_FlagCrop <- zone_df$W_FlagCropMW
  zonelayerwater_df[zonelayerwater_df$water == "S", ]$W_FlagCrop <- zone_df$W_FlagCropS
  zonelayerwater_df[zonelayerwater_df$water == "W", ]$W_FlagCrop <- zone_df$W_FlagCropW
  
  zonelayerwater_df$Rt_CLrv <- rep(zonelayer_df$Rt_CLrv, nwater)
  
  # add layer index of array
  df <- zonetreewater_df[c("zone", "tree_id", "water", "W_FlagTree")]
  l_df <- df[rep(seq_len(nrow(df)), nlayer), ]
  l_df$layer <- rep(layer_df$layer, each = nrow(zonetreewater_df))
  l_df <- l_df[order(l_df$water,l_df$tree_id, l_df$layer, l_df$zone), ]
  zonelayertreewater_df$W_FlagTree <- l_df$W_FlagTree
  
  zonelayertreewater_df$Rt_TLrv <- rep(zonelayertree_df$Rt_TLrv, nwater)
  zonelayertreewater_df$Rt_TLrv_flag <- zonelayertreewater_df$W_FlagTree * zonelayertreewater_df$Rt_TLrv
  zonelayerwater_df$Rt_TLrv_flag_sum <- aggregate(zonelayertreewater_df["Rt_TLrv_flag"], zonelayertreewater_df[c("zone", "layer", "water")], sum)$Rt_TLrv_flag
  
  zonelayerwater_df$Rt_Lrv <- zonelayerwater_df$W_FlagCrop * zonelayerwater_df$Rt_CLrv + zonelayerwater_df$Rt_TLrv_flag_sum
  
  
  # W_Theta1MS[Zone] = if (1+(ABS(W_Alpha1*W_PRMS[Zone]))^W_n1)^(1-1/W_n1)<>0 then W_ThetaSat1/(1+(ABS(W_Alpha1*W_PRMS[Zone]))^W_n1)^(1-1/W_n1) else 0
  # W_Theta2MS[Zone] = if (1+(ABS(W_Alpha2*W_PRMS[Zone]))^W_n2)^(1-1/W_n2)<>0 then W_ThetaSat2/(1+(ABS(W_Alpha2*W_PRMS[Zone]))^W_n2)^(1-1/W_n2) else 0
  # W_Theta3MS[Zone] = if (1+(ABS(W_Alpha3*W_PRMS[Zone]))^W_n3)^(1-1/W_n3)<>0 then W_ThetaSat3/(1+(ABS(W_Alpha3*W_PRMS[Zone]))^W_n3)^(1-1/W_n3) else 0
  # W_Theta4MS[Zone] = if (1+(ABS(W_Alpha4*W_PRMS[Zone]))^W_n4)^(1-1/W_n4)<>0 then W_ThetaSat4/(1+(ABS(W_Alpha4*W_PRMS[Zone]))^W_n4)^(1-1/W_n4) else 0
  #
  # W_Theta1MW[Zone] = if (1+(ABS(W_Alpha1*W_PRMW[Zone]))^W_n1)^(1-1/W_n1)<>0 then W_ThetaSat1/(1+(ABS(W_Alpha1*W_PRMW[Zone]))^W_n1)^(1-1/W_n1) else 0
  # W_Theta2MW[Zone] = if (1+(ABS(W_Alpha2*W_PRMW[Zone]))^W_n2)^(1-1/W_n2)<>0 then W_ThetaSat2/(1+(ABS(W_Alpha2*W_PRMW[Zone]))^W_n2)^(1-1/W_n2) else 0
  # W_Theta3MW[Zone] = if (1+(ABS(W_Alpha3*W_PRMW[Zone]))^W_n3)^(1-1/W_n3)<>0 then W_ThetaSat3/(1+(ABS(W_Alpha3*W_PRMW[Zone]))^W_n3)^(1-1/W_n3) else 0
  # W_Theta4MW[Zone] = if (1+(ABS(W_Alpha4*W_PRMW[Zone]))^W_n4)^(1-1/W_n4)<>0 then W_ThetaSat4/(1+(ABS(W_Alpha4*W_PRMW[Zone]))^W_n4)^(1-1/W_n4) else 0
  #
  # W_Theta1S[Zone] = if (1+(ABS(W_Alpha1*W_PRS[Zone]))^W_n1)^(1-1/W_n1)<>0 then W_ThetaSat1/(1+(ABS(W_Alpha1*W_PRS[Zone]))^W_n1)^(1-1/W_n1) else 0
  # W_Theta2S[Zone] = if (1+(ABS(W_Alpha2*W_PRS[Zone]))^W_n2)^(1-1/W_n2)<>0 then W_ThetaSat2/(1+(ABS(W_Alpha2*W_PRS[Zone]))^W_n2)^(1-1/W_n2) else 0
  # W_Theta3S[Zone] = if (1+(ABS(W_Alpha3*W_PRS[Zone]))^W_n3)^(1-1/W_n3)<>0 then W_ThetaSat3/(1+(ABS(W_Alpha3*W_PRS[Zone]))^W_n3)^(1-1/W_n3) else 0
  # W_Theta4S[Zone] = if (1+(ABS(W_Alpha4*W_PRS[Zone]))^W_n4)^(1-1/W_n4)<>0 then W_ThetaSat4/(1+(ABS(W_Alpha4*W_PRS[Zone]))^W_n4)^(1-1/W_n4) else 0
  #
  # W_Theta1W[Zone] = if (1+(ABS(W_Alpha1*W_PRW[Zone]))^W_n1)^(1-1/W_n1)<>0 then W_ThetaSat1/(1+(ABS(W_Alpha1*W_PRW[Zone]))^W_n1)^(1-1/W_n1) else 0
  # W_Theta2W[Zone] = if (1+(ABS(W_Alpha2*W_PRW[Zone]))^W_n2)^(1-1/W_n2)<>0 then W_ThetaSat2/(1+(ABS(W_Alpha2*W_PRW[Zone]))^W_n2)^(1-1/W_n2) else 0
  # W_Theta3W[Zone] = if (1+(ABS(W_Alpha3*W_PRW[Zone]))^W_n3)^(1-1/W_n3)<>0 then W_ThetaSat3/(1+(ABS(W_Alpha3*W_PRW[Zone]))^W_n3)^(1-1/W_n3) else 0
  # W_Theta4W[Zone] = if (1+(ABS(W_Alpha4*W_PRW[Zone]))^W_n4)^(1-1/W_n4)<>0 then W_ThetaSat4/(1+(ABS(W_Alpha4*W_PRW[Zone]))^W_n4)^(1-1/W_n4) else 0
  
  zonelayerwater_df$W_Alpha <- rep(zonelayer_df$W_Alpha, 4)
  zonelayerwater_df$W_n <- rep(zonelayer_df$W_n, 4)
  zonelayerwater_df$W_ThetaSat <- rep(zonelayer_df$W_ThetaSat, 4)
  zonelayerwater_df$W_PR <- NA
  zonelayerwater_df[zonelayerwater_df$water == "MS", ]$W_PR <- rep(zone_df$W_PRMS, nlayer)
  zonelayerwater_df[zonelayerwater_df$water == "MW", ]$W_PR <- rep(zone_df$W_PRMW, nlayer)
  zonelayerwater_df[zonelayerwater_df$water == "S", ]$W_PR <- rep(zone_df$W_PRS, nlayer)
  zonelayerwater_df[zonelayerwater_df$water == "W", ]$W_PR <- rep(zone_df$W_PRW, nlayer)
  
  zonelayerwater_df$W_Theta_water_a <- (1 + (
    abs(zonelayerwater_df$W_Alpha * zonelayerwater_df$W_PR)
  )^zonelayerwater_df$W_n)^(1 - 1 / zonelayerwater_df$W_n)
  zonelayerwater_df$W_Theta_water <- ifelse(
    zonelayerwater_df$W_Theta_water_a != 0,
    zonelayerwater_df$W_ThetaSat / zonelayerwater_df$W_Theta_water_a,
    0
  )
  
  # W_Theta1_MS_MW[Zone] = 1000*AF_DepthAct1[Zone]*max(0,min(W_Theta1[Zone],W_Theta1MW[Zone])-W_Theta1MS[Zone])
  # W_Theta2_MS_MW[Zone] = 1000*AF_Depth2[Zone]*max(0,min(W_Theta2[Zone],W_Theta2MW[Zone])-W_Theta2MS[Zone])
  # W_Theta3_MS_MW[Zone] = 1000*AF_Depth3[Zone]*max(0,min(W_Theta3[Zone],W_Theta3MW[Zone])-W_Theta3MS[Zone])
  # W_Theta4_MS_MW[Zone] = 1000*AF_Depth4[Zone]*max(0,min(W_Theta4[Zone],W_Theta4MW[Zone])-W_Theta4MS[Zone])
  #
  # W_Theta1_MW_W[Zone] = 1000*AF_DepthAct1[Zone]*max(0,min(W_Theta1[Zone],W_Theta1W[Zone])-W_Theta1MW[Zone])
  # W_Theta2_MW_W[Zone] = 1000*AF_Depth2[Zone]*max(0,min(W_Theta2[Zone],W_Theta2W[Zone])-W_Theta2MW[Zone])
  # W_Theta3_MW_W[Zone] = 1000*AF_Depth3[Zone]*max(0,min(W_Theta3[Zone],W_Theta3W[Zone])-W_Theta3MW[Zone])
  # W_Theta4_MW_W[Zone] = 1000*AF_Depth4[Zone]*max(0,min(W_Theta4[Zone],W_Theta4W[Zone])-W_Theta4MW[Zone])
  #
  # W_Theta1S_MS[Zone] = 1000*AF_DepthAct1[Zone]*max(0,min(W_Theta1[Zone],W_Theta1MS[Zone])-W_Theta1S[Zone])
  # W_Theta2_S_MS[Zone] = 1000*AF_Depth2[Zone]*max(0,min(W_Theta2[Zone],W_Theta2MS[Zone])-W_Theta2S[Zone])
  # W_Theta3_S_MS[Zone] = 1000*AF_Depth3[Zone]*max(0,min(W_Theta3[Zone],W_Theta3MS[Zone])-W_Theta3S[Zone])
  # W_Theta4_S_MS[Zone] = 1000*AF_Depth4[Zone]*max(0,min(W_Theta4[Zone],W_Theta4MS[Zone])-W_Theta4S[Zone])
  #
  # W_Theta1_W_Soil[Zone] = 1000*AF_DepthAct1[Zone]*max(0,W_Theta1[Zone]-W_Theta1W[Zone])
  # W_Theta2_W_Soil[Zone] = 1000*AF_Depth2[Zone]*max(0,W_Theta2[Zone]-W_Theta2W[Zone])
  # W_Theta3_W_Soil[Zone] = 1000*AF_Depth3[Zone]*max(0,W_Theta3[Zone]-W_Theta3W[Zone])
  # W_Theta4_W_Soil[Zone] = 1000*AF_Depth4[Zone]*max(0,W_Theta4[Zone]-W_Theta4W[Zone])
  
  
  zonelayerwater_df$AF_Depth <- rep(zonelayer_df$AF_Depth, 4)
  zonelayerwater_df$W_Theta <- rep(zonelayer_df$W_Theta, 4)
  zonelayerwater_df$W_Theta_water_x <- c(
    zonelayerwater_df[zonelayerwater_df$water == "MW", ]$W_Theta_water,
    zonelayerwater_df[zonelayerwater_df$water == "W", ]$W_Theta_water,
    zonelayerwater_df[zonelayerwater_df$water == "MS", ]$W_Theta_water,
    rep(maxval, nzone * nlayer)
  )
  
  zonelayerwater_df$W_Theta_water_pair <- 1000 * zonelayerwater_df$AF_Depth *
    pmax(
      0,
      pmin(
        zonelayerwater_df$W_Theta,
        zonelayerwater_df$W_Theta_water_x
      ) - zonelayerwater_df$W_Theta_water
    )
  
  
  # W_pFMS[Zone] = IF W_PRMS[Zone]<0 THEN LOG10(-W_PRMS[Zone]) ELSE 0
  # W_pFMW[Zone] = IF W_PRMW[Zone]<0 THEN LOG10(-W_PRMW[Zone]) ELSE 0
  # W_pFS[Zone] = IF W_PRS[Zone]<0  THEN LOG10(-W_PRS[Zone]) ELSE 0
  # W_pFW[Zone] = IF W_PRW[Zone]<0 THEN LOG10(-W_PRW[Zone]) ELSE 0
  zonelayerwater_df$W_pF <- ifelse(zonelayerwater_df$W_PR < 0, log10(-zonelayerwater_df$W_PR), 0)
  
  # W_PhiPMW1[Zone] = GRAPH(W_pFMW[Zone])
  zonelayerwater_df$W_PhiP <- NA
  for (i in 1:nrow(zonelayerwater_df)) {
    zonelayerwater_df[i, ]$W_PhiP <- get_W_PhiP(zonelayerwater_df[i, ]$W_pF,
                                                zonelayerwater_df[i, ]$layer,
                                                zonelayerwater_df[i, ]$water)
  }
  
  zonelayerwater_df$W_PhiTheta <- rep(zonelayer_df$W_PhiTheta, 4)
  zonelayerwater_df[zonelayerwater_df$water == "W", ]$W_PhiTheta <- maxval #to be neglected when compared with min()
  
  
  # Rt_Diam_MS[Zn1,1] = If Rt_LrvMS[Zn1,1]> 0 then ((W_FlagCropMS[Zn1]*Rt_CLrv1[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeMS[Zn1,Sp1]*Rt_TLrv1[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn1,Sp2]*Rt_TLrv1[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn1,Sp3]*Rt_TLrv1[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn1,2] = If Rt_LrvMS[Zn1,1]> 0 then ((W_FlagCropMS[Zn1]*Rt_CLrv2[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeMS[Zn1,Sp1]*Rt_TLrv2[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn1,Sp2]*Rt_TLrv2[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn1,Sp3]*Rt_TLrv2[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn1,3] = If Rt_LrvMS[Zn1,1]> 0 then ((W_FlagCropMS[Zn1]*Rt_CLrv3[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeMS[Zn1,Sp1]*Rt_TLrv3[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn1,Sp2]*Rt_TLrv3[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn1,Sp3]*Rt_TLrv3[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn1,4] = If Rt_LrvMS[Zn1,1]> 0 then ((W_FlagCropMS[Zn1]*Rt_CLrv4[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeMS[Zn1,Sp1]*Rt_TLrv4[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn1,Sp2]*Rt_TLrv4[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn1,Sp3]*Rt_TLrv4[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn2,1] = If Rt_LrvMS[Zn2,1]> 0 then ((W_FlagCropMS[Zn2]*Rt_CLrv1[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeMS[Zn2,Sp1]*Rt_TLrv1[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn2,Sp2]*Rt_TLrv1[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn2,Sp3]*Rt_TLrv1[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn2,2] = If Rt_LrvMS[Zn2,1]> 0 then ((W_FlagCropMS[Zn2]*Rt_CLrv2[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeMS[Zn2,Sp1]*Rt_TLrv2[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn2,Sp2]*Rt_TLrv2[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn2,Sp3]*Rt_TLrv2[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn2,3] = If Rt_LrvMS[Zn2,1]> 0 then ((W_FlagCropMS[Zn2]*Rt_CLrv3[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeMS[Zn2,Sp1]*Rt_TLrv3[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn2,Sp2]*Rt_TLrv3[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn2,Sp3]*Rt_TLrv3[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn2,4] = If Rt_LrvMS[Zn2,1]> 0 then ((W_FlagCropMS[Zn2]*Rt_CLrv4[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeMS[Zn2,Sp1]*Rt_TLrv4[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn2,Sp2]*Rt_TLrv4[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn2,Sp3]*Rt_TLrv4[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn3,1] = If Rt_LrvMS[Zn3,1]> 0 then ((W_FlagCropMS[Zn3]*Rt_CLrv1[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeMS[Zn3,Sp1]*Rt_TLrv1[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn3,Sp2]*Rt_TLrv1[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn3,Sp3]*Rt_TLrv1[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn3,2] = If Rt_LrvMS[Zn3,1]> 0 then ((W_FlagCropMS[Zn3]*Rt_CLrv2[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeMS[Zn3,Sp1]*Rt_TLrv2[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn3,Sp2]*Rt_TLrv2[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn3,Sp3]*Rt_TLrv2[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn3,3] = If Rt_LrvMS[Zn3,1]> 0 then ((W_FlagCropMS[Zn3]*Rt_CLrv3[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeMS[Zn3,Sp1]*Rt_TLrv3[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn3,Sp2]*Rt_TLrv3[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn3,Sp3]*Rt_TLrv3[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn3,4] = If Rt_LrvMS[Zn3,1]> 0 then ((W_FlagCropMS[Zn3]*Rt_CLrv4[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeMS[Zn3,Sp1]*Rt_TLrv4[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn3,Sp2]*Rt_TLrv4[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn3,Sp3]*Rt_TLrv4[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn4,1] = If Rt_LrvMS[Zn4,1]> 0 then ((W_FlagCropMS[Zn4]*Rt_CLrv1[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeMS[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn4,2] = If Rt_LrvMS[Zn4,1]> 0 then ((W_FlagCropMS[Zn4]*Rt_CLrv2[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeMS[Zn4,Sp1]*Rt_TLrv2[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn4,Sp2]*Rt_TLrv2[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn4,Sp3]*Rt_TLrv2[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn4,3] = If Rt_LrvMS[Zn4,1]> 0 then ((W_FlagCropMS[Zn4]*Rt_CLrv3[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeMS[Zn4,Sp1]*Rt_TLrv3[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn4,Sp2]*Rt_TLrv3[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn4,Sp3]*Rt_TLrv3[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MS[Zn4,4] = If Rt_LrvMS[Zn4,1]> 0 then ((W_FlagCropMS[Zn4]*Rt_CLrv4[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeMS[Zn4,Sp1]*Rt_TLrv4[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMS[Zn4,Sp2]*Rt_TLrv4[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMS[Zn4,Sp3]*Rt_TLrv4[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMS[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn1,1] = If Rt_LrvMW[Zn1,1]> 0 then ((W_FlagCropMW[Zn1]*Rt_CLrv1[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeMW[Zn1,Sp1]*Rt_TLrv1[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn1,Sp2]*Rt_TLrv1[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn1,Sp3]*Rt_TLrv1[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn1,2] = If Rt_LrvMW[Zn1,1]> 0 then ((W_FlagCropMW[Zn1]*Rt_CLrv2[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeMW[Zn1,Sp1]*Rt_TLrv2[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn1,Sp2]*Rt_TLrv2[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn1,Sp3]*Rt_TLrv2[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn1,3] = If Rt_LrvMW[Zn1,1]> 0 then ((W_FlagCropMW[Zn1]*Rt_CLrv3[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeMW[Zn1,Sp1]*Rt_TLrv3[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn1,Sp2]*Rt_TLrv3[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn1,Sp3]*Rt_TLrv3[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn1,4] = If Rt_LrvMW[Zn1,1]> 0 then ((W_FlagCropMW[Zn1]*Rt_CLrv4[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeMW[Zn1,Sp1]*Rt_TLrv4[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn1,Sp2]*Rt_TLrv4[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn1,Sp3]*Rt_TLrv4[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn2,1] = If Rt_LrvMW[Zn2,1]> 0 then ((W_FlagCropMW[Zn2]*Rt_CLrv1[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeMW[Zn2,Sp1]*Rt_TLrv1[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn2,Sp2]*Rt_TLrv1[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn2,Sp3]*Rt_TLrv1[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn2,2] = If Rt_LrvMW[Zn2,1]> 0 then ((W_FlagCropMW[Zn2]*Rt_CLrv2[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeMW[Zn2,Sp1]*Rt_TLrv2[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn2,Sp2]*Rt_TLrv2[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn2,Sp3]*Rt_TLrv2[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn2,3] = If Rt_LrvMW[Zn2,1]> 0 then ((W_FlagCropMW[Zn2]*Rt_CLrv3[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeMW[Zn2,Sp1]*Rt_TLrv3[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn2,Sp2]*Rt_TLrv3[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn2,Sp3]*Rt_TLrv3[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn2,4] = If Rt_LrvMW[Zn2,1]> 0 then ((W_FlagCropMW[Zn2]*Rt_CLrv4[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeMW[Zn2,Sp1]*Rt_TLrv4[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn2,Sp2]*Rt_TLrv4[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn2,Sp3]*Rt_TLrv4[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn3,1] = If Rt_LrvMW[Zn3,1]> 0 then ((W_FlagCropMW[Zn3]*Rt_CLrv1[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeMW[Zn3,Sp1]*Rt_TLrv1[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn3,Sp2]*Rt_TLrv1[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn3,Sp3]*Rt_TLrv1[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn3,2] = If Rt_LrvMW[Zn3,1]> 0 then ((W_FlagCropMW[Zn3]*Rt_CLrv2[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeMW[Zn3,Sp1]*Rt_TLrv2[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn3,Sp2]*Rt_TLrv2[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn3,Sp3]*Rt_TLrv2[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn3,3] = If Rt_LrvMW[Zn3,1]> 0 then ((W_FlagCropMW[Zn3]*Rt_CLrv3[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeMW[Zn3,Sp1]*Rt_TLrv3[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn3,Sp2]*Rt_TLrv3[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn3,Sp3]*Rt_TLrv3[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn3,4] = If Rt_LrvMW[Zn3,1]> 0 then ((W_FlagCropMW[Zn3]*Rt_CLrv4[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeMW[Zn3,Sp1]*Rt_TLrv4[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn3,Sp2]*Rt_TLrv4[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn3,Sp3]*Rt_TLrv4[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn4,1] = If Rt_LrvMW[Zn4,1]> 0 then ((W_FlagCropMW[Zn4]*Rt_CLrv1[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeMW[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn4,2] = If Rt_LrvMW[Zn4,1]> 0 then ((W_FlagCropMW[Zn4]*Rt_CLrv2[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeMW[Zn4,Sp1]*Rt_TLrv2[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn4,Sp2]*Rt_TLrv2[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn4,Sp3]*Rt_TLrv2[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn4,3] = If Rt_LrvMW[Zn4,1]> 0 then ((W_FlagCropMW[Zn4]*Rt_CLrv3[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeMW[Zn4,Sp1]*Rt_TLrv3[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn4,Sp2]*Rt_TLrv3[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn4,Sp3]*Rt_TLrv3[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_MW[Zn4,4] = If Rt_LrvMW[Zn4,1]> 0 then ((W_FlagCropMW[Zn4]*Rt_CLrv4[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeMW[Zn4,Sp1]*Rt_TLrv4[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeMW[Zn4,Sp2]*Rt_TLrv4[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeMW[Zn4,Sp3]*Rt_TLrv4[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvMW[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn1,1] = If Rt_LrvS[Zn1,1]> 0 then  ((W_FlagCropS[Zn1]*Rt_CLrv1[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeS[Zn1,Sp1]*Rt_TLrv1[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn1,Sp2]*Rt_TLrv1[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn1,Sp3]*Rt_TLrv1[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn1,2] = If Rt_LrvS[Zn1,1]> 0 then  ((W_FlagCropS[Zn1]*Rt_CLrv2[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeS[Zn1,Sp1]*Rt_TLrv2[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn1,Sp2]*Rt_TLrv2[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn1,Sp3]*Rt_TLrv2[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn1,3] = If Rt_LrvS[Zn1,1]> 0 then  ((W_FlagCropS[Zn1]*Rt_CLrv3[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeS[Zn1,Sp1]*Rt_TLrv3[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn1,Sp2]*Rt_TLrv3[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn1,Sp3]*Rt_TLrv3[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn1,4] = If Rt_LrvS[Zn1,1]> 0 then  ((W_FlagCropS[Zn1]*Rt_CLrv4[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeS[Zn1,Sp1]*Rt_TLrv4[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn1,Sp2]*Rt_TLrv4[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn1,Sp3]*Rt_TLrv4[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn2,1] = If Rt_LrvS[Zn2,1]> 0 then  ((W_FlagCropS[Zn2]*Rt_CLrv1[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeS[Zn2,Sp1]*Rt_TLrv1[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn2,Sp2]*Rt_TLrv1[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn2,Sp3]*Rt_TLrv1[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn2,2] = If Rt_LrvS[Zn2,1]> 0 then  ((W_FlagCropS[Zn2]*Rt_CLrv2[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeS[Zn2,Sp1]*Rt_TLrv2[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn2,Sp2]*Rt_TLrv2[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn2,Sp3]*Rt_TLrv2[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn2,3] = If Rt_LrvS[Zn2,1]> 0 then  ((W_FlagCropS[Zn2]*Rt_CLrv3[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeS[Zn2,Sp1]*Rt_TLrv3[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn2,Sp2]*Rt_TLrv3[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn2,Sp3]*Rt_TLrv3[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn2,4] = If Rt_LrvS[Zn2,1]> 0 then  ((W_FlagCropS[Zn2]*Rt_CLrv4[Zn2]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeS[Zn2,Sp1]*Rt_TLrv4[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn2,Sp2]*Rt_TLrv4[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn2,Sp3]*Rt_TLrv4[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn3,1] = If Rt_LrvS[Zn3,1]> 0 then  ((W_FlagCropS[Zn3]*Rt_CLrv1[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeS[Zn3,Sp1]*Rt_TLrv1[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn3,Sp2]*Rt_TLrv1[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn3,Sp3]*Rt_TLrv1[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn3,2] = If Rt_LrvS[Zn3,1]> 0 then  ((W_FlagCropS[Zn3]*Rt_CLrv2[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeS[Zn3,Sp1]*Rt_TLrv2[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn3,Sp2]*Rt_TLrv2[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn3,Sp3]*Rt_TLrv2[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn3,3] = If Rt_LrvS[Zn3,1]> 0 then  ((W_FlagCropS[Zn3]*Rt_CLrv3[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeS[Zn3,Sp1]*Rt_TLrv3[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn3,Sp2]*Rt_TLrv3[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn3,Sp3]*Rt_TLrv3[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn3,4] = If Rt_LrvS[Zn3,1]> 0 then  ((W_FlagCropS[Zn3]*Rt_CLrv4[Zn3]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeS[Zn3,Sp1]*Rt_TLrv4[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn3,Sp2]*Rt_TLrv4[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn3,Sp3]*Rt_TLrv4[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn4,1] = If Rt_LrvS[Zn4,1]> 0 then  ((W_FlagCropS[Zn4]*Rt_CLrv1[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeS[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn4,2] = If Rt_LrvS[Zn4,1]> 0 then  ((W_FlagCropS[Zn4]*Rt_CLrv2[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeS[Zn4,Sp1]*Rt_TLrv2[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn4,Sp2]*Rt_TLrv2[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn4,Sp3]*Rt_TLrv2[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn4,3] = If Rt_LrvS[Zn4,1]> 0 then  ((W_FlagCropS[Zn4]*Rt_CLrv3[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeS[Zn4,Sp1]*Rt_TLrv3[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn4,Sp2]*Rt_TLrv3[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn4,Sp3]*Rt_TLrv3[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_S[Zn4,4] = If Rt_LrvS[Zn4,1]> 0 then  ((W_FlagCropS[Zn4]*Rt_CLrv4[Zn4]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeS[Zn4,Sp1]*Rt_TLrv4[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeS[Zn4,Sp2]*Rt_TLrv4[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeS[Zn4,Sp3]*Rt_TLrv4[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvS[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn1,1] = If Rt_LrvW[Zn1,1]> 0 then  ((W_FlagCropW[Zn1]*Rt_CLrv1[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeW[Zn1,Sp1]*Rt_TLrv1[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn1,Sp2]*Rt_TLrv1[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn1,Sp3]*Rt_TLrv1[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn1,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn1,2] = If Rt_LrvW[Zn1,2]> 0 then  ((W_FlagCropW[Zn1]*Rt_CLrv2[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeW[Zn1,Sp1]*Rt_TLrv2[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn1,Sp2]*Rt_TLrv2[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn1,Sp3]*Rt_TLrv2[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn1,2])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn1,3] = If Rt_LrvW[Zn1,3]> 0 then  ((W_FlagCropW[Zn1]*Rt_CLrv3[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeW[Zn1,Sp1]*Rt_TLrv3[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn1,Sp2]*Rt_TLrv3[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn1,Sp3]*Rt_TLrv3[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn1,3])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn1,4] = If Rt_LrvW[Zn1,4]> 0 then  ((W_FlagCropW[Zn1]*Rt_CLrv4[Zn1]*SQRT(Cq_RtDiam[Zn1])+W_FlagTreeW[Zn1,Sp1]*Rt_TLrv4[Zn1,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn1,Sp2]*Rt_TLrv4[Zn1,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn1,Sp3]*Rt_TLrv4[Zn1,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn1,4])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn2,1] = If Rt_LrvW[Zn2,1]> 0 then  ((W_FlagCropW[Zn2]*Rt_CLrv1[Zn1]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeW[Zn2,Sp1]*Rt_TLrv1[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn2,Sp2]*Rt_TLrv1[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn2,Sp3]*Rt_TLrv1[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn2,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn2,2] = If Rt_LrvW[Zn2,2]> 0 then  ((W_FlagCropW[Zn2]*Rt_CLrv2[Zn1]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeW[Zn2,Sp1]*Rt_TLrv2[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn2,Sp2]*Rt_TLrv2[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn2,Sp3]*Rt_TLrv2[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn2,2])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn2,3] = If Rt_LrvW[Zn2,3]> 0 then  ((W_FlagCropW[Zn2]*Rt_CLrv3[Zn1]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeW[Zn2,Sp1]*Rt_TLrv3[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn2,Sp2]*Rt_TLrv3[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn2,Sp3]*Rt_TLrv3[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn2,3])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn2,4] = If Rt_LrvW[Zn2,4]> 0 then  ((W_FlagCropW[Zn2]*Rt_CLrv4[Zn1]*SQRT(Cq_RtDiam[Zn2])+W_FlagTreeW[Zn2,Sp1]*Rt_TLrv4[Zn2,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn2,Sp2]*Rt_TLrv4[Zn2,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn2,Sp3]*Rt_TLrv4[Zn2,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn2,4])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn3,1] = If Rt_LrvW[Zn3,1]> 0 then  ((W_FlagCropW[Zn3]*Rt_CLrv1[Zn1]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeW[Zn3,Sp1]*Rt_TLrv1[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn3,Sp2]*Rt_TLrv1[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn3,Sp3]*Rt_TLrv1[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn3,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn3,2] = If Rt_LrvW[Zn3,2]> 0 then  ((W_FlagCropW[Zn3]*Rt_CLrv2[Zn1]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeW[Zn3,Sp1]*Rt_TLrv2[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn3,Sp2]*Rt_TLrv2[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn3,Sp3]*Rt_TLrv2[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn3,2])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn3,3] = If Rt_LrvW[Zn3,3]> 0 then  ((W_FlagCropW[Zn3]*Rt_CLrv3[Zn1]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeW[Zn3,Sp1]*Rt_TLrv3[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn3,Sp2]*Rt_TLrv3[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn3,Sp3]*Rt_TLrv3[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn3,3])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn3,4] = If Rt_LrvW[Zn3,4]> 0 then  ((W_FlagCropW[Zn3]*Rt_CLrv4[Zn1]*SQRT(Cq_RtDiam[Zn3])+W_FlagTreeW[Zn3,Sp1]*Rt_TLrv4[Zn3,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn3,Sp2]*Rt_TLrv4[Zn3,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn3,Sp3]*Rt_TLrv4[Zn3,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn3,4])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn4,1] = If Rt_LrvW[Zn4,1]> 0 then  ((W_FlagCropW[Zn4]*Rt_CLrv1[Zn1]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeW[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn4,1])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn4,2] = If Rt_LrvW[Zn4,2]> 0 then  ((W_FlagCropW[Zn4]*Rt_CLrv1[Zn1]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeW[Zn4,Sp1]*Rt_TLrv1[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn4,Sp2]*Rt_TLrv1[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn4,Sp3]*Rt_TLrv1[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn4,2])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn4,3] = If Rt_LrvW[Zn4,3]> 0 then  ((W_FlagCropW[Zn4]*Rt_CLrv3[Zn1]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeW[Zn4,Sp1]*Rt_TLrv3[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn4,Sp2]*Rt_TLrv3[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn4,Sp3]*Rt_TLrv3[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn4,3])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  # Rt_Diam_W[Zn4,4] = If Rt_LrvW[Zn4,4]> 0 then  ((W_FlagCropW[Zn4]*Rt_CLrv4[Zn1]*SQRT(Cq_RtDiam[Zn4])+W_FlagTreeW[Zn4,Sp1]*Rt_TLrv4[Zn4,Sp1]*SQRT(Rt_TDiam[Sp1])+W_FlagTreeW[Zn4,Sp2]*Rt_TLrv4[Zn4,Sp2]*SQRT(Rt_TDiam[Sp2])+W_FlagTreeW[Zn4,Sp3]*Rt_TLrv4[Zn4,Sp3]*SQRT(Rt_TDiam[Sp3]))/Rt_LrvW[Zn4,4])^2 else Rt_Stopgap + 0*(Rt_CLrv1[Zn1]+Rt_CLrv2[Zn1]+Rt_CLrv3[Zn1]+Rt_CLrv4[Zn1])+0*(Rt_TLrv1[Zn1,Sp1]+Rt_TLrv2[Zn1,Sp1]+Rt_TLrv3[Zn1,Sp1]+Rt_TLrv4[Zn1,Sp1])
  
  
  zonelayertreewater_df$Rt_TDiam <- rep(zonelayertree_df$Rt_TDiam, nwater)
  zonelayertreewater_df$Rt_vol_flag <- zonelayertreewater_df$W_FlagTree * zonelayertreewater_df$Rt_TLrv * sqrt(zonelayertreewater_df$Rt_TDiam)
  zonelayerwater_df$Rt_vol_flag_sum <- aggregate(zonelayertreewater_df["Rt_vol_flag"], zonelayertreewater_df[c("zone", "layer", "water")], sum)$Rt_vol_flag
    
  zonelayerwater_df$Cq_RtDiam <- rep(zone_df$Cq_RtDiam, nlayer * nwater)

  zonelayerwater_df$Rt_Diam <- ifelse(
    zonelayerwater_df$Rt_Lrv > 0,
    ((
      zonelayerwater_df$W_FlagCrop * zonelayerwater_df$Rt_CLrv * sqrt(zonelayerwater_df$Cq_RtDiam) +
        zonelayerwater_df$Rt_vol_flag_sum
    ) / zonelayerwater_df$Rt_Lrv
    )^2,
    pars$Rt_par$Rt_StopGap
  )
    
  
    # Rt_Rho_MS[Zone,SoilLayer] = if Rt_LrvMS[Zone,SoilLayer] > 0 and Rt_Diam_MS[Zone,SoilLayer]>0 then 1/(((PI*Rt_LrvMS[Zone,SoilLayer])^0.5)*0.5*Rt_Diam_MS[Zone,SoilLayer]) else Rt_StopGap
    # Rt_Rho_MW[Zone,SoilLayer] = if Rt_LrvMW[Zone,SoilLayer] > 0 and Rt_Diam_MW[Zone,SoilLayer]>0 then 1/(((PI*Rt_LrvMW[Zone,SoilLayer])^0.5)*0.5*Rt_Diam_MW[Zone,SoilLayer]) else Rt_StopGap
    # Rt_Rho_S[Zone,SoilLayer] = if Rt_LrvS[Zone,SoilLayer] > 0 and Rt_Diam_S[Zone,SoilLayer]>0 then 1/(((PI*Rt_LrvS[Zone,SoilLayer])^0.5)*0.5*Rt_Diam_S[Zone,SoilLayer]) else Rt_StopGap
    # Rt_Rho_W[Zone,SoilLayer] = if Rt_LrvW[Zone,SoilLayer] > 0 and Rt_Diam_W[Zone,SoilLayer]>0 then 1/(((PI*Rt_LrvW[Zone,SoilLayer])^0.5)*0.5*Rt_Diam_W[Zone,SoilLayer]) else Rt_StopGap
    
    zonelayerwater_df$Rt_Rho <- ifelse(
      zonelayerwater_df$Rt_Lrv > 0 & zonelayerwater_df$Rt_Diam > 0,
      1 / (((
        pi * zonelayerwater_df$Rt_Lrv
      )^0.5) * 0.5 * zonelayerwater_df$Rt_Diam),
      pars$Rt_par$Rt_StopGap
    )

    # Rt_GMS[Zone,SoilLayer] = if Rt_Rho_MS[Zone,SoilLayer] <> Rt_StopGap then (Rt_Rho_MS[Zone,SoilLayer]^2-1)/(0.5*(((1-(3*Rt_Rho_MS[Zone,SoilLayer]^2))/4)+((Rt_Rho_MS[Zone,SoilLayer]^4*LOGN(Rt_Rho_MS[Zone,SoilLayer]))/(Rt_Rho_MS[Zone,SoilLayer]^2-1)))) else Rt_StopGap
    # Rt_GMW[Zone,SoilLayer] = if Rt_Rho_MW[Zone,SoilLayer] <> Rt_StopGap then (Rt_Rho_MW[Zone,SoilLayer]^2-1)/(0.5*(((1-(3*Rt_Rho_MW[Zone,SoilLayer]^2))/4)+((Rt_Rho_MW[Zone,SoilLayer]^4*LOGN(Rt_Rho_MW[Zone,SoilLayer]))/(Rt_Rho_MW[Zone,SoilLayer]^2-1)))) else Rt_StopGap
    # Rt_GS[Zone,SoilLayer]  = if Rt_Rho_S[Zone,SoilLayer]  <> Rt_StopGap then (Rt_Rho_S[Zone,SoilLayer]^2-1) /(0.5*(((1-(3*Rt_Rho_S[Zone,SoilLayer]^2))/4)+((Rt_Rho_S[Zone,SoilLayer]^4*LOGN(Rt_Rho_S[Zone,SoilLayer]))/(Rt_Rho_S[Zone,SoilLayer]^2-1)))) else Rt_StopGap
    # Rt_GW[Zone,SoilLayer]  = if Rt_Rho_W[Zone,SoilLayer]  <> Rt_StopGap then (Rt_Rho_W[Zone,SoilLayer]^2-1) /(0.5*(((1-(3*Rt_Rho_W[Zone,SoilLayer]^2))/4)+((Rt_Rho_W[Zone,SoilLayer]^4*LOGN(Rt_Rho_W[Zone,SoilLayer]))/(Rt_Rho_W[Zone,SoilLayer]^2-1)))) else Rt_StopGap
    
    zonelayerwater_df$Rt_G <- ifelse(
      zonelayerwater_df$Rt_Rho != pars$Rt_par$Rt_StopGap,
      (zonelayerwater_df$Rt_Rho^2 - 1) / (0.5 * (((
        1 - (3 * zonelayerwater_df$Rt_Rho^2)
      ) / 4) + ((zonelayerwater_df$Rt_Rho^4 * log(zonelayerwater_df$Rt_Rho)) /
                  (zonelayerwater_df$Rt_Rho^2 - 1)
      ))),
      pars$Rt_par$Rt_StopGap
    )

    
  # W_PotUptMS1[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvMS[Zone,1]>0 THEN min(W_Theta1_MS_MW[Zone],10*PI*100*AF_DepthAct1[Zone]*(max(0,+min(W_PhiPMW1[Zone],W_PhiTheta1[Zone])-W_PhiPMS1[Zone]))*Rt_GMS[Zone,1]*Rt_LrvMS[Zone,1]) ELSE 0 else IF Rt_LrvMS[Zone,1]>0 THEN 10*PI*100*AF_DepthAct1[Zone]*(max(0,+min(W_PhiPMW1[Zone],W_PhiTheta1[Zone])-W_PhiPMS1[Zone]))*Rt_GMS[Zone,1]*Rt_LrvMS[Zone,1] ELSE 0
  # W_PotUptMS2[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvMS[Zone,2]>0 THEN min(W_Theta2_MS_MW[Zone],10*PI*100*AF_Depth2[Zone]*(max(0,+min(W_PhiPMW2[Zone],W_PhiTheta2[Zone])-W_PhiPMS2[Zone]))*Rt_GMS[Zone,2]*Rt_LrvMS[Zone,2]) ELSE 0 else IF Rt_LrvMS[Zone,2]>0 THEN 10*PI*100*AF_Depth2[Zone]*(max(0,+min(W_PhiPMW2[Zone],W_PhiTheta2[Zone])-W_PhiPMS2[Zone]))*Rt_GMS[Zone,2]*Rt_LrvMS[Zone,2] ELSE 0
  # W_PotUptMS3[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvMS[Zone,3]>0 THEN min(W_Theta3_MS_MW[Zone],10*PI*100*AF_Depth3[Zone]*(max(0,+min(W_PhiPMW3[Zone],W_PhiTheta3[Zone])-W_PhiPMS3[Zone]))*Rt_GMS[Zone,3]*Rt_LrvMS[Zone,3]) ELSE 0 else IF Rt_LrvMS[Zone,3]>0 THEN 10*PI*100*AF_Depth3[Zone]*(max(0,+min(W_PhiPMW3[Zone],W_PhiTheta3[Zone])-W_PhiPMS3[Zone]))*Rt_GMS[Zone,3]*Rt_LrvMS[Zone,3] ELSE 0
  # W_PotUptMS4[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvMS[Zone,4]>0 THEN min(W_Theta4_MS_MW[Zone],10*PI*100*AF_Depth4[Zone]*(max(0,+min(W_PhiPMW4[Zone],W_PhiTheta4[Zone])-W_PhiPMS4[Zone]))*Rt_GMS[Zone,4]*Rt_LrvMS[Zone,4]) ELSE 0 else IF Rt_LrvMS[Zone,4]>0 THEN 10*PI*100*AF_Depth4[Zone]*(max(0,+min(W_PhiPMW4[Zone],W_PhiTheta4[Zone])-W_PhiPMS4[Zone]))*Rt_GMS[Zone,4]*Rt_LrvMS[Zone,4] ELSE 0
  # W_PotUptMW1[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvMW[Zone,1]>0 THEN min(W_Theta1_MW_W[Zone],10*PI*100*AF_DepthAct1[Zone]*(max(0,+min(W_PhiPW1[Zone],W_PhiTheta1[Zone])-W_PhiPMW1[Zone]))*Rt_GMW[Zone,1]*Rt_LrvMW[Zone,1]) ELSE 0 else IF Rt_LrvMW[Zone,1]>0 THEN 10*PI*100*AF_DepthAct1[Zone]*(max(0,+min(W_PhiPW1[Zone],W_PhiTheta1[Zone])-W_PhiPMW1[Zone]))*Rt_GMW[Zone,1]*Rt_LrvMW[Zone,1] ELSE 0
  # W_PotUptMW2[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvMW[Zone,2]>0 THEN min(W_Theta2_MW_W[Zone],10*PI*100*AF_Depth2[Zone]*(max(0,+min(W_PhiPW2[Zone],W_PhiTheta2[Zone])-W_PhiPMW2[Zone]))*Rt_GMW[Zone,2]*Rt_LrvMW[Zone,2]) ELSE 0 else IF Rt_LrvMW[Zone,2]>0 THEN 10*PI*100*AF_Depth2[Zone]*(max(0,+min(W_PhiPW2[Zone],W_PhiTheta2[Zone])-W_PhiPMW2[Zone]))*Rt_GMW[Zone,2]*Rt_LrvMW[Zone,2] ELSE 0
  # W_PotUptMW3[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvMW[Zone,3]>0 THEN min(W_Theta3_MW_W[Zone],10*PI*100*AF_Depth3[Zone]*(max(0,+min(W_PhiPW3[Zone],W_PhiTheta3[Zone])-W_PhiPMW3[Zone]))*Rt_GMW[Zone,3]*Rt_LrvMW[Zone,3]) ELSE 0 else IF Rt_LrvMW[Zone,3]>0 THEN 10*PI*100*AF_Depth3[Zone]*(max(0,+min(W_PhiPW3[Zone],W_PhiTheta3[Zone])-W_PhiPMW3[Zone]))*Rt_GMW[Zone,3]*Rt_LrvMW[Zone,3] ELSE 0
  # W_PotUptMW4[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvMW[Zone,4]>0 THEN min(W_Theta4_MW_W[Zone],10*PI*100*AF_Depth4[Zone]*(max(0,+min(W_PhiPW4[Zone],W_PhiTheta4[Zone])-W_PhiPMW4[Zone]))*Rt_GMW[Zone,4]*Rt_LrvMW[Zone,4]) ELSE 0 else IF Rt_LrvMW[Zone,4]>0 THEN 10*PI*100*AF_Depth4[Zone]*(max(0,+min(W_PhiPW4[Zone],W_PhiTheta4[Zone])-W_PhiPMW4[Zone]))*Rt_GMW[Zone,4]*Rt_LrvMW[Zone,4] ELSE 0
  # W_PotUptS1[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvS[Zone,1]>0 THEN min(W_Theta1S_MS[Zone], 10*PI*100*AF_DepthAct1[Zone]*(max(0,min(W_PhiPMS1[Zone],W_PhiTheta1[Zone])-W_PhiPS1[Zone]))*Rt_GS[Zone,1]*Rt_LrvS[Zone,1]) ELSE 0 else IF Rt_LrvS[Zone,1]>0 THEN  10*PI*100*AF_DepthAct1[Zone]*(max(0,min(W_PhiPMS1[Zone],W_PhiTheta1[Zone])-W_PhiPS1[Zone]))*Rt_GS[Zone,1]*Rt_LrvS[Zone,1] ELSE 0
  # W_PotUptS2[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvS[Zone,2]>0 THEN min(W_Theta2_S_MS[Zone], 10*PI*100*AF_Depth2[Zone]*(max(0,min(W_PhiPMS2[Zone],W_PhiTheta2[Zone])-W_PhiPS2[Zone]))*Rt_GS[Zone,2]*Rt_LrvS[Zone,2]) ELSE 0 else IF Rt_LrvS[Zone,2]>0 THEN 10*PI*100*AF_Depth2[Zone]*(max(0,min(W_PhiPMS2[Zone],W_PhiTheta2[Zone])-W_PhiPS2[Zone]))*Rt_GS[Zone,2]*Rt_LrvS[Zone,2] ELSE 0
  # W_PotUptS3[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvS[Zone,3]>0 THEN min(W_Theta3_S_MS[Zone], 10*PI*100*AF_Depth3[Zone]*(max(0,min(W_PhiPMS3[Zone],W_PhiTheta3[Zone])-W_PhiPS3[Zone]))*Rt_GS[Zone,3]*Rt_LrvS[Zone,3]) ELSE 0 else IF Rt_LrvS[Zone,3]>0 THEN  10*PI*100*AF_Depth3[Zone]*(max(0,min(W_PhiPMS3[Zone],W_PhiTheta3[Zone])-W_PhiPS3[Zone]))*Rt_GS[Zone,3]*Rt_LrvS[Zone,3] ELSE 0
  # W_PotUptS4[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvS[Zone,4]>0 THEN min(W_Theta4_S_MS[Zone], 10*PI*100*AF_Depth4[Zone]*(max(0,min(W_PhiPMS4[Zone],W_PhiTheta4[Zone])-W_PhiPS4[Zone]))*Rt_GS[Zone,4]*Rt_LrvS[Zone,4]) ELSE 0 else IF Rt_LrvS[Zone,4]>0 THEN  10*PI*100*AF_Depth4[Zone]*(max(0,min(W_PhiPMS4[Zone],W_PhiTheta4[Zone])-W_PhiPS4[Zone]))*Rt_GS[Zone,4]*Rt_LrvS[Zone,4] ELSE 0
  # W_PotUptW1[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvW[Zone,1]>0 THEN min(W_Theta1_W_Soil[Zone],10*PI*100*AF_DepthAct1[Zone]*(max(0,+W_PhiTheta1[Zone]-W_PhiPW1[Zone]))*Rt_GW[Zone,1]*Rt_LrvW[Zone,1]) ELSE 0 else IF Rt_LrvW[Zone,1]>0 THEN 10*PI*100*AF_DepthAct1[Zone]*(max(0,+W_PhiTheta1[Zone]-W_PhiPW1[Zone]))*Rt_GW[Zone,1]*Rt_LrvW[Zone,1] ELSE 0
  # W_PotUptW2[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvW[Zone,2]>0 THEN min(W_Theta2_W_Soil[Zone],10*PI*100*AF_Depth2[Zone]*(max(0,+W_PhiTheta2[Zone]-W_PhiPW2[Zone]))*Rt_GW[Zone,2]*Rt_LrvW[Zone,2]) ELSE 0 else IF Rt_LrvW[Zone,2]>0 THEN 10*PI*100*AF_Depth2[Zone]*(max(0,+W_PhiTheta2[Zone]-W_PhiPW2[Zone]))*Rt_GW[Zone,2]*Rt_LrvW[Zone,2] ELSE 0
  # W_PotUptW3[Zone] = if W_WaterLimited?[Zone]=1 then IF Rt_LrvW[Zone,3]>0 THEN min(W_Theta3_W_Soil[Zone],10*PI*100*AF_Depth3[Zone]*(max(0,+W_PhiTheta3[Zone]-W_PhiPW3[Zone]))*Rt_GW[Zone,3]*Rt_LrvW[Zone,3]) ELSE 0 else IF Rt_LrvW[Zone,3]>0 THEN 10*PI*100*AF_Depth3[Zone]*(max(0,+W_PhiTheta3[Zone]-W_PhiPW3[Zone]))*Rt_GW[Zone,3]*Rt_LrvW[Zone,3] ELSE 0
  # W_PotUptW4[Zone] = IF W_WaterLimited?[Zone]=1 then IF Rt_LrvW[Zone,2]>0 THEN min(W_Theta4_W_Soil[Zone],10*PI*100*AF_Depth4[Zone]*(max(0,+W_PhiTheta4[Zone]-W_PhiPW4[Zone]))*Rt_GW[Zone,4]*Rt_LrvW[Zone,4]) ELSE 0 else IF Rt_LrvW[Zone,2]>0 THEN 10*PI*100*AF_Depth4[Zone]*(max(0,+W_PhiTheta4[Zone]-W_PhiPW4[Zone]))*Rt_GW[Zone,4]*Rt_LrvW[Zone,4] ELSE 0
    zonelayerwater_df$W_WaterLimited_is <- rep(zone_df$W_WaterLimited_is, nlayer)
    
    zonelayerwater_df$W_PhiP_x <- c(zonelayerwater_df[zonelayerwater_df$water == "MW",]$W_PhiP,
                                    zonelayerwater_df[zonelayerwater_df$water == "W",]$W_PhiP,
                                    zonelayerwater_df[zonelayerwater_df$water == "MS",]$W_PhiP,
                                    rep(maxval, nzone*nlayer))
    zonelayerwater_df$W_PotUpt_a <- 10 * pi * 100 * zonelayerwater_df$AF_Depth * (pmax(
      0,
      pmin(zonelayerwater_df$W_PhiP_x, zonelayerwater_df$W_PhiTheta) - zonelayerwater_df$W_PhiP
    )) * zonelayerwater_df$Rt_G * zonelayerwater_df$Rt_Lrv
      
    zonelayerwater_df$W_PotUpt <- ifelse(
      zonelayerwater_df$W_WaterLimited_is == 1,
      ifelse(  
        zonelayerwater_df$Rt_Lrv > 0,
        pmin(zonelayerwater_df$W_Theta_water_pair, zonelayerwater_df$W_PotUpt_a),
        0
      ),
      ifelse(zonelayerwater_df$Rt_Lrv > 0, zonelayerwater_df$W_PotUpt_a, 0)
    )
    
  # W_CUptPot1[Zone] = Rt_CLrv1[Zone]*(W_FlagCropS[Zone]*W_PotUptS1[Zone]+W_FlagCropMS[Zone]*W_PotUptMS1[Zone]+W_FlagCropMW[Zone]*W_PotUptMW1[Zone]+W_FlagCropW[Zone]*W_PotUptW1[Zone])
  # W_CUptPot2[Zone] = Rt_CLrv2[Zone]*(W_FlagCropS[Zone]*W_PotUptS2[Zone]+W_FlagCropMS[Zone]*W_PotUptMS2[Zone]+W_FlagCropMW[Zone]*W_PotUptMW2[Zone]+W_FlagCropW[Zone]*W_PotUptW2[Zone])
  # W_CUptPot3[Zone] = Rt_CLrv3[Zone]*(W_FlagCropS[Zone]*W_PotUptS3[Zone]+W_FlagCropMS[Zone]*W_PotUptMS3[Zone]+W_FlagCropMW[Zone]*W_PotUptMW3[Zone]+W_FlagCropW[Zone]*W_PotUptW3[Zone])
  # W_CUptPot4[Zone] = Rt_CLrv4[Zone]*(W_FlagCropS[Zone]*W_PotUptS4[Zone]+W_FlagCropMS[Zone]*W_PotUptMS4[Zone]+W_FlagCropMW[Zone]*W_PotUptMW4[Zone]+W_FlagCropW[Zone]*W_PotUptW4[Zone])

    zonelayerwater_df$W_PotUpt_flagC <- zonelayerwater_df$W_FlagCrop * zonelayerwater_df$W_PotUpt
    zonelayer_df$W_PotUpt_flagC_sum <- aggregate(zonelayerwater_df["W_PotUpt_flagC"], zonelayerwater_df[c("zone", "layer")], sum)$W_PotUpt_flagC
    zonelayer_df$W_CUptPot <- zonelayer_df$Rt_CLrv * zonelayer_df$W_PotUpt_flagC_sum

    
    # W_TUptPot1[Zone,Tree] = Rt_TLrv1[Zone,Tree]*(W_FlagTreeS[Zone,Tree]*W_PotUptS1[Zone]+W_FlagTreeMS[Zone,Tree]*W_PotUptMS1[Zone]+W_FlagTreeMW[Zone,Tree]*W_PotUptMW1[Zone]+W_FlagTreeW[Zone,Tree]*W_PotUptW1[Zone])
    # W_TUptPot2[Zone,Tree] = Rt_TLrv2[Zone,Tree]*(W_FlagTreeS[Zone,Tree]*W_PotUptS2[Zone]+W_FlagTreeMS[Zone,Tree]*W_PotUptMS2[Zone]+W_FlagTreeMW[Zone,Tree]*W_PotUptMW2[Zone]+W_FlagTreeW[Zone,Tree]*W_PotUptW2[Zone])
    # W_TUptPot3[Zone,Tree] = Rt_TLrv3[Zone,Tree]*(W_FlagTreeS[Zone,Tree]*W_PotUptS3[Zone]+W_FlagTreeMS[Zone,Tree]*W_PotUptMS3[Zone]+W_FlagTreeMW[Zone,Tree]*W_PotUptMW3[Zone]+W_FlagTreeW[Zone,Tree]*W_PotUptW3[Zone])
    # W_TUptPot4[Zone,Tree] = Rt_TLrv4[Zone,Tree]*(W_FlagTreeS[Zone,Tree]*W_PotUptS4[Zone]+W_FlagTreeMS[Zone,Tree]*W_PotUptMS4[Zone]+W_FlagTreeMW[Zone,Tree]*W_PotUptMW4[Zone]+W_FlagTreeW[Zone,Tree]*W_PotUptW4[Zone])
    

    # add tree index of array
    df <- zonelayerwater_df[c("zone", "layer", "water", "W_PotUpt")]
    t_df <- df[rep(seq_len(nrow(df)), ntree), ]
    t_df$tree_id <- rep(tree_df$tree_id, each = nrow(zonelayerwater_df))
    t_df <- t_df[order(t_df$water,t_df$tree_id, t_df$layer, t_df$zone), ]
    zonelayertreewater_df$W_PotUpt <- t_df$W_PotUpt

    zonelayertreewater_df$W_PotUpt_flagT <- zonelayertreewater_df$W_FlagTree * zonelayertreewater_df$W_PotUpt
    
    zonelayertree_df$W_PotUpt_flagT_sum <- aggregate(zonelayertreewater_df["W_PotUpt_flagT"], zonelayertreewater_df[c("zone", "layer", "tree_id")], sum)$W_PotUpt_flagT
    zonelayertree_df$W_TUptPot <- zonelayertree_df$Rt_TLrv * zonelayertree_df$W_PotUpt_flagT_sum
    
      # W_CUptPotAct1[Zone] = IF W_StockAcc1[Zone]>0 then if (W_CUptPot1[Zone]+ARRAYSUM(W_TUptPot1[Zone,*]))>W_StockAcc1[Zone] then (W_StockAcc1[Zone]*W_CUptPot1[Zone]/(W_CUptPot1[Zone]+ARRAYSUM(W_TUptPot1[Zone,*]))) ELSE W_CUptPot1[Zone] ELSE 0
      # W_CUptPotAct2[Zone] = IF(W_StockAcc2[Zone]>0)THEN IF (W_CUptPot2[Zone]+ARRAYSUM(W_TUptPot2[Zone,*]))>0 THEN IF (W_CUptPot2[Zone]+ARRAYSUM(W_TUptPot2[Zone,*]))>W_StockAcc2[Zone] THEN (W_StockAcc2[Zone]*W_CUptPot2[Zone]/(W_CUptPot2[Zone]+ARRAYSUM(W_TUptPot2[Zone,*])))ELSE(W_CUptPot2[Zone])ELSE(0)ELSE(0)
      # W_CUptPotAct3[Zone] = IF(W_StockAcc3[Zone]>0)THEN IF (W_CUptPot3[Zone]+ARRAYSUM(W_TUptPot3[Zone,*]))>0 THEN IF (W_CUptPot3[Zone]+ARRAYSUM(W_TUptPot3[Zone,*]))>W_StockAcc3[Zone] THEN (W_StockAcc3[Zone]*W_CUptPot3[Zone]/(W_CUptPot3[Zone]+ARRAYSUM(W_TUptPot3[Zone,*])))ELSE(W_CUptPot3[Zone])ELSE(0)ELSE(0)
      # W_CUptPotAct4[Zone] = IF(W_StockAcc4[Zone]>0)THEN IF (W_CUptPot4[Zone]+ARRAYSUM(W_TUptPot4[Zone,*]))>0 THEN IF (W_CUptPot4[Zone]+ARRAYSUM(W_TUptPot4[Zone,*]))>W_StockAcc4[Zone] THEN (W_StockAcc4[Zone]*W_CUptPot4[Zone]/(W_CUptPot4[Zone]+ARRAYSUM(W_TUptPot4[Zone,*])))ELSE(W_CUptPot4[Zone])ELSE(0)ELSE(0)
      
    zonelayer_df$W_TUptPot_sum <- aggregate(zonelayertree_df["W_TUptPot"], zonelayertree_df[c("zone", "layer")], sum)$W_TUptPot
    zonelayer_df$W_CTUptPot_sum <- zonelayer_df$W_CUptPot + zonelayer_df$W_TUptPot_sum
    zonelayer_df$W_CUptPotAct <- ifelse(zonelayer_df$W_StockAcc > 0,
                                        ifelse(
                                          zonelayer_df$W_CTUptPot_sum > 0,
                                          ifelse(
                                            zonelayer_df$W_CTUptPot_sum > zonelayer_df$W_StockAcc,
                                            zonelayer_df$W_StockAcc * zonelayer_df$W_CUptPot / zonelayer_df$W_CTUptPot_sum,
                                            zonelayer_df$W_CUptPot
                                          ),
                                          0
                                        ),
                                        0)
      

      # CW_UptPot[Zone] = W_CUptPotAct1[Zone]+W_CUptPotAct2[Zone]+W_CUptPotAct3[Zone]+W_CUptPotAct4[Zone]
    zone_df$CW_UptPot <- aggregate(zonelayer_df["W_CUptPotAct"], zonelayer_df[c("zone")], sum)$W_CUptPotAct

    
    # CW_DemandAct[Zone] = CW_DemandPot[Zone]*CW_DemandRedFac[Zone]
    zone_df$CW_DemandAct <- zone_df$CW_DemandPot * zone_df$CW_DemandRedFac
    
    # CW_UptDeno[Zone] = (W_CUptPotAct1[Zone]*Rt_CLrv1[Zone]+Rt_CLrv2[Zone]*W_CUptPotAct2[Zone]+Rt_CLrv3[Zone]*W_CUptPotAct3[Zone]+Rt_CLrv4[Zone]*W_CUptPotAct4[Zone])
    zonelayer_df$CW_UptDeno_a <- zonelayer_df$W_CUptPotAct*zonelayer_df$Rt_CLrv
    zone_df$CW_UptDeno <- aggregate(zonelayer_df["CW_UptDeno_a"], zonelayer_df[c("zone")], sum)$CW_UptDeno_a
    
    
    
    
      # W_CUpt1[Zone] = IF(CW_UptPot[Zone]>CW_DemandAct[Zone] and CW_UptDeno[Zone]>0)THEN(CW_DemandAct[Zone]*W_CUptPotAct1[Zone]*Rt_CLrv1[Zone]/CW_UptDeno[Zone])ELSE(W_CUptPotAct1[Zone])
      # W_CUpt2[Zone] = if CW_UptPot[Zone]>CW_DemandAct[Zone] and CW_UptDeno[Zone]>0 then(CW_DemandAct[Zone]*W_CUptPotAct2[Zone]*Rt_CLrv2[Zone]/CW_UptDeno[Zone])ELSE(W_CUptPotAct2[Zone])
      # W_CUpt3[Zone] = IF(CW_UptPot[Zone]>CW_DemandAct[Zone])and CW_UptDeno[Zone]>0 THEN(CW_DemandAct[Zone]*W_CUptPotAct3[Zone]*Rt_CLrv3[Zone]/CW_UptDeno[Zone])ELSE(W_CUptPotAct3[Zone])
      # W_CUpt4[Zone] = IF(CW_UptPot[Zone]>CW_DemandAct[Zone] and CW_UptDeno[Zone]>0)THEN(CW_DemandAct[Zone]*W_CUptPotAct4[Zone]*Rt_CLrv4[Zone]/CW_UptDeno[Zone])ELSE(W_CUptPotAct4[Zone])
      
    zonelayer_df$CW_UptPot <- rep(zone_df$CW_UptPot, nlayer)
    zonelayer_df$CW_DemandAct <- rep(zone_df$CW_DemandAct, nlayer)
    zonelayer_df$CW_UptDeno <- rep(zone_df$CW_UptDeno, nlayer)
    
      zonelayer_df$W_CUpt <- ifelse(
        zonelayer_df$CW_UptPot > zonelayer_df$CW_DemandAct &
          zonelayer_df$CW_UptDeno > 0,
        zonelayer_df$CW_DemandAct * zonelayer_df$W_CUptPotAct * zonelayer_df$Rt_CLrv / zonelayer_df$CW_UptDeno,
        zonelayer_df$W_CUptPotAct
      )

      
      ### Update dynamic variables ####################
      
      # W_TUptPotAct1[Zone,Tree] = IF W_StockAcc1[Zone]>0 then                                                           if (W_CUptPot1[Zone]+ARRAYSUM(W_TUptPot1[Zone,*]))>W_StockAcc1[Zone] then (W_StockAcc1[Zone]*W_TUptPot1[Zone,Tree]/(W_CUptPot1[Zone]+ARRAYSUM(W_TUptPot1[Zone,*])))ELSE W_TUptPot1[Zone,Tree] ELSE 0
      # W_TUptPotAct2[Zone,Tree] = IF(W_StockAcc2[Zone]>0)THEN IF (W_CUptPot2[Zone]+ARRAYSUM(W_TUptPot2[Zone,*]))>0 THEN IF (W_CUptPot2[Zone]+ARRAYSUM(W_TUptPot2[Zone,*]))>W_StockAcc2[Zone] THEN (W_StockAcc2[Zone]*W_TUptPot2[Zone,Tree]/(W_CUptPot2[Zone]+ARRAYSUM(W_TUptPot2[Zone,*])))ELSE(W_TUptPot2[Zone,Tree])ELSE(0)ELSE(0)
      # W_TUptPotAct3[Zone,Tree] = IF(W_StockAcc3[Zone]>0)THEN IF (W_CUptPot3[Zone]+ARRAYSUM(W_TUptPot3[Zone,*]))>0 THEN IF (W_CUptPot3[Zone]+ARRAYSUM(W_TUptPot3[Zone,*]))>W_StockAcc3[Zone] THEN (W_StockAcc3[Zone]*W_TUptPot3[Zone,Tree]/(W_CUptPot3[Zone]+ARRAYSUM(W_TUptPot3[Zone,*])))ELSE(W_TUptPot3[Zone,Tree])ELSE(0)ELSE(0)
      # W_TUptPotAct4[Zone,Tree] = IF(W_StockAcc4[Zone]>0)THEN IF (W_CUptPot4[Zone]+ARRAYSUM(W_TUptPot4[Zone,*]))>0 THEN IF (W_CUptPot4[Zone]+ARRAYSUM(W_TUptPot4[Zone,*]))>W_StockAcc4[Zone] THEN (W_StockAcc4[Zone]*W_TUptPot4[Zone,Tree]/(W_CUptPot4[Zone]+ARRAYSUM(W_TUptPot4[Zone,*])))ELSE(W_TUptPot4[Zone,Tree])ELSE(0)ELSE(0)
      zonelayertree_df$W_StockAcc <- rep(zonelayer_df$W_StockAcc, ntree)
      zonelayertree_df$W_CTUptPot_sum <- rep(zonelayer_df$W_CTUptPot_sum, ntree)
      zonelayertree_df$W_TUptPotAct <- ifelse(zonelayertree_df$W_StockAcc > 0,
                                          ifelse(
                                            zonelayertree_df$W_CTUptPot_sum > 0,
                                            ifelse(
                                              zonelayertree_df$W_CTUptPot_sum > zonelayertree_df$W_StockAcc,
                                              zonelayertree_df$W_StockAcc * zonelayertree_df$W_TUptPot / zonelayertree_df$W_CTUptPot_sum,
                                              zonelayertree_df$W_TUptPot
                                            ),
                                            0
                                          ),
                                          0)
            
      # TW_UptDenoZn[Zone,Tree] = (Rt_TLrv1[Zone,Tree]*W_TUptPotAct1[Zone,Tree]+Rt_TLrv2[Zone,Tree]*W_TUptPotAct2[Zone,Tree]+Rt_TLrv3[Zone,Tree]*W_TUptPotAct3[Zone,Tree]+Rt_TLrv4[Zone,Tree]*W_TUptPotAct4[Zone,Tree])*AF_ZoneWidth[Zone] 
      zonelayertree_df$TW_UptDenoZn_a <- zonelayertree_df$Rt_TLrv* zonelayertree_df$W_TUptPotAct
      zonetree_df$TW_UptDenoZn <- aggregate(zonelayertree_df["TW_UptDenoZn_a"], zonelayertree_df[c("zone", "tree_id")], sum)$TW_UptDenoZn_a

      # TW_UptDeno[Tree] = ARRAYSUM(TW_UptDenoZn[*,Tree])
      tree_df$TW_UptDeno <- aggregate(zonetree_df["TW_UptDenoZn"], zonetree_df[c("tree_id")], sum)$TW_UptDenoZn
      
      # TW_UptPotZn[Zone,Tree] = AF_ZoneFrac[Zone]*(W_TUptPotAct1[Zone,Tree]+W_TUptPotAct2[Zone,Tree]+W_TUptPotAct3[Zone,Tree]+W_TUptPotAct4[Zone,Tree])
      zonetree_df$TW_UptPotZn <- zonetree_df$AF_ZoneFrac* aggregate(zonelayertree_df["W_TUptPotAct"], zonelayertree_df[c("zone", "tree_id")], sum)$W_TUptPotAct

      # TW_UptPot[Tree] = ARRAYSUM(TW_UptPotZn[*,Tree])
      tree_df$TW_UptPot <- aggregate(zonetree_df["TW_UptPotZn"], zonetree_df[c("tree_id")], sum)$TW_UptPotZn
      
      # TW_DemandAct[Tree] = TW_DemandPot[Tree]*TW_DemandRedFac[Tree]
      tree_df$TW_DemandAct <- tree_df$TW_DemandPot * tree_df$TW_DemandRedFac
      
      # W_T1Upt1[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp1]>0) THEN (IF(TW_UptPot[Sp1]>TW_DemandAct[Sp1])THEN (TW_DemandAct[Sp1]/AF_ZoneFrac[Zone]*W_TUptPotAct1[Zone,Sp1]*Rt_TLrv1[Zone,Sp1]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp1])ELSE(W_TUptPotAct1[Zone,Sp1]))ELSE(0)
      # W_T2Upt1[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp2]>0) THEN (IF(TW_UptPot[Sp2]>TW_DemandAct[Sp2])THEN (TW_DemandAct[Sp2]/AF_ZoneFrac[Zone]*W_TUptPotAct1[Zone,Sp2]*Rt_TLrv1[Zone,Sp2]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp2])ELSE(W_TUptPotAct1[Zone,Sp2]))ELSE(0)
      # W_T3Upt1[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp3]>0) THEN (IF(TW_UptPot[Sp3]>TW_DemandAct[Sp3])THEN (TW_DemandAct[Sp3]/AF_ZoneFrac[Zone]*W_TUptPotAct1[Zone,Sp3]*Rt_TLrv1[Zone,Sp3]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp3])ELSE(W_TUptPotAct1[Zone,Sp3]))ELSE(0)
      # 
      # W_T1Upt2[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp1]>0) THEN (IF(TW_UptPot[Sp1]>TW_DemandAct[Sp1])THEN (TW_DemandAct[Sp1]/AF_ZoneFrac[Zone]*W_TUptPotAct2[Zone,Sp1]*Rt_TLrv2[Zone,Sp1]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp1])ELSE(W_TUptPotAct2[Zone,Sp1]))ELSE(0)
      # W_T2Upt2[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp2]>0) THEN (IF(TW_UptPot[Sp2]>TW_DemandAct[Sp2])THEN (TW_DemandAct[Sp2]/AF_ZoneFrac[Zone]*W_TUptPotAct2[Zone,Sp2]*Rt_TLrv2[Zone,Sp2]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp2])ELSE(W_TUptPotAct2[Zone,Sp2]))ELSE(0)
      # W_T3Upt2[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp3]>0) THEN (IF(TW_UptPot[Sp3]>TW_DemandAct[Sp3])THEN (TW_DemandAct[Sp3]/AF_ZoneFrac[Zone]*W_TUptPotAct2[Zone,Sp3]*Rt_TLrv2[Zone,Sp3]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp3])ELSE(W_TUptPotAct2[Zone,Sp3]))ELSE(0)
      # 
      # W_T1Upt3[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp1]>0) THEN (IF(TW_UptPot[Sp1]>TW_DemandAct[Sp1])THEN (TW_DemandAct[Sp1]/AF_ZoneFrac[Zone]*W_TUptPotAct3[Zone,Sp1]*Rt_TLrv3[Zone,Sp1]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp1])ELSE(W_TUptPotAct3[Zone,Sp1]))ELSE(0)
      # W_T2Upt3[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp2]>0) THEN (IF(TW_UptPot[Sp2]>TW_DemandAct[Sp2])THEN (TW_DemandAct[Sp2]/AF_ZoneFrac[Zone]*W_TUptPotAct3[Zone,Sp2]*Rt_TLrv3[Zone,Sp2]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp2])ELSE(W_TUptPotAct3[Zone,Sp2]))ELSE(0)
      # W_T3Upt3[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp3]>0) THEN (IF(TW_UptPot[Sp3]>TW_DemandAct[Sp3])THEN (TW_DemandAct[Sp3]/AF_ZoneFrac[Zone]*W_TUptPotAct3[Zone,Sp3]*Rt_TLrv3[Zone,Sp3]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp3])ELSE(W_TUptPotAct3[Zone,Sp3]))ELSE(0)
      # 
      # W_T1Upt4[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp1]>0) THEN (IF(TW_UptPot[Sp1]>TW_DemandAct[Sp1])THEN (TW_DemandAct[Sp1]/AF_ZoneFrac[Zone]*W_TUptPotAct4[Zone,Sp1]*Rt_TLrv4[Zone,Sp1]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp1])ELSE(W_TUptPotAct4[Zone,Sp1]))ELSE(0)
      # W_T2Upt4[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp2]>0) THEN (IF(TW_UptPot[Sp2]>TW_DemandAct[Sp2])THEN (TW_DemandAct[Sp2]/AF_ZoneFrac[Zone]*W_TUptPotAct4[Zone,Sp2]*Rt_TLrv4[Zone,Sp2]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp2])ELSE(W_TUptPotAct4[Zone,Sp2]))ELSE(0)
      # W_T3Upt4[Zone] = IF(AF_ZoneFrac[Zone]>0 AND TW_UptDeno[Sp3]>0) THEN (IF(TW_UptPot[Sp3]>TW_DemandAct[Sp3])THEN (TW_DemandAct[Sp3]/AF_ZoneFrac[Zone]*W_TUptPotAct4[Zone,Sp3]*Rt_TLrv4[Zone,Sp3]*AF_ZoneWidth[Zone]/TW_UptDeno[Sp3])ELSE(W_TUptPotAct4[Zone,Sp3]))ELSE(0)

      zonelayertree_df$TW_UptDeno <- rep(tree_df$TW_UptDeno, each = nzone * nlayer)
      zonelayertree_df$TW_UptPot <- rep(tree_df$TW_UptPot, each = nzone * nlayer)
      zonelayertree_df$TW_DemandAct <- rep(tree_df$TW_DemandAct, each = nzone * nlayer)
      zonelayertree_df$AF_ZoneWidth <- rep(zone_df$AF_ZoneWidth, nlayer * ntree)
      
      zonelayertree_df$W_Upt <- ifelse(
        zonelayertree_df$AF_ZoneFrac > 0 & zonelayertree_df$TW_UptDeno > 0,
        ifelse(
          zonelayertree_df$TW_UptPot > zonelayertree_df$TW_DemandAct,
          zonelayertree_df$TW_DemandAct /
            zonelayertree_df$AF_ZoneFrac * zonelayertree_df$W_TUptPotAct * zonelayertree_df$Rt_TLrv * zonelayertree_df$AF_ZoneWidth / zonelayertree_df$TW_UptDeno,
          zonelayertree_df$W_TUptPotAct
        ),
        0
      )

      # W_Stock1[Zone](t) = W_Stock1[Zone](t - dt) + (W_In1[Zone] - Evap_Surf[Zone] - W_CUpt1[Zone] - W_T1Upt1[Zone] - W_T2Upt1[Zone] - W_T3Upt1[Zone]) * dt
      # W_Stock2[Zone](t) = W_Stock2[Zone](t - dt) + (W_In2[Zone] - W_CUpt2[Zone] - W_T1Upt2[Zone] - W_T2Upt2[Zone] - W_T3Upt2[Zone]) * dt
      # W_Stock3[Zone](t) = W_Stock3[Zone](t - dt) + (W_In3[Zone] - W_CUpt3[Zone] - W_T1Upt3[Zone] - W_T2Upt3[Zone] - W_T3Upt3[Zone]) * dt
      # W_Stock4[Zone](t) = W_Stock4[Zone](t - dt) + (W_In4[Zone] - W_CUpt4[Zone] - W_T1Upt4[Zone] - W_T2Upt4[Zone] - W_T3Upt4[Zone]) * dt
      zonelayer_df$W_Upt_sum <- aggregate(zonelayertree_df["W_Upt"], zonelayertree_df[c("zone", "layer")], sum)$W_Upt
      zonelayer_df$W_Stock <- zonelayer_df$W_Stock + zonelayer_df$W_In - zonelayer_df$W_CUpt - zonelayer_df$W_Upt_sum
      zonelayer_df[zonelayer_df$layer == 1, ]$W_Stock <- zonelayer_df[zonelayer_df$layer == 1, ]$W_Stock - zone_df$Evap_Surf

      # S_LFoodForWorms[Zone] = S_WormsLikeLitMetab*(Mc_Metab[Zone]+Mc_Act[Zone]+ 0.5 * Mc_Slw[Zone])+S_WormsLikeLitStruc*(Mc_Struc[Zone]+Mc_Pass[Zone]+ 0.5 *Mc_Slw[Zone] )
      zone_df$S_LFoodForWorms <- pars$S_par$S_WormsLikeLitMetab*(zone_df$Mc_Metab+ zone_df$Mc_Act+ 0.5 * zone_df$Mc_Slw)+ pars$S_par$S_WormsLikeLitStruc*(zone_df$Mc_Struc+ zone_df$Mc_Pass+ 0.5 * zone_df$Mc_Slw )
      
      # S_SOMFoodForWorms[Zone,SoilLayer] = S_WormsLikeSOMMetab*(Mc2_Metab[Zone,SoilLayer]+Mc2_Act[Zone,SoilLayer]+ 0.5 * Mc2_Slw[Zone,SoilLayer])+S_WormsLikeSOMStruc*(Mc2_Struc[Zone,SoilLayer]+Mc2_Pass[Zone,SoilLayer]+ 0.5 *Mc2_Slw[Zone,SoilLayer] )
      zonelayer_df$S_SOMFoodForWorms <- pars$S_par$S_WormsLikeSOMMetab*(zonelayer_df$Mc2_Metab+ zonelayer_df$Mc2_Act + 0.5 * zonelayer_df$Mc2_Slw)+
        pars$S_par$S_WormsLikeSOMStruc*(zonelayer_df$Mc2_Struc+ zonelayer_df$Mc2_Pass + 0.5 * zonelayer_df$Mc2_Slw)
      
      # Mn2_RelImpLayer[Zn1,1] = AF_DepthAct1[Zn1]*Mc2_SOMDist[1]/Mc2_RelImpDeno[Zn1]+0*(AF_DepthAct1[Zn1]+AF_Depth2[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
      # Mn2_RelImpLayer[Zn1,2] = AF_Depth2[Zn1]*Mc2_SOMDist[2]/Mc2_RelImpDeno[Zn1]+0*(AF_DepthAct1[Zn1]+AF_Depth2[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
      # Mn2_RelImpLayer[Zn1,3] = AF_Depth3[Zn1]*Mc2_SOMDist[3]/Mc2_RelImpDeno[Zn1]+0*(AF_DepthAct1[Zn1]+AF_Depth2[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
      # Mn2_RelImpLayer[Zn1,4] = AF_Depth4[Zn1]*Mc2_SOMDist[4]/Mc2_RelImpDeno[Zn1]+0*(AF_DepthAct1[Zn1]+AF_Depth2[Zn1]+AF_Depth3[Zn1]+AF_Depth4[Zn1])
      # Mn2_RelImpLayer[Zn2,1] = AF_DepthAct1[Zn2]*Mc2_SOMDist[1]/Mc2_RelImpDeno[Zn2]+0*(AF_DepthAct1[Zn2]+AF_Depth2[Zn2]+AF_Depth3[Zn2]+AF_Depth4[Zn2])
      # Mn2_RelImpLayer[Zn2,2] = AF_Depth2[Zn2]*Mc2_SOMDist[2]/Mc2_RelImpDeno[Zn2]+0*(AF_DepthAct1[Zn2]+AF_Depth2[Zn2]+AF_Depth3[Zn2]+AF_Depth4[Zn2])
      # Mn2_RelImpLayer[Zn2,3] = AF_Depth3[Zn2]*Mc2_SOMDist[3]/Mc2_RelImpDeno[Zn2]+0*(AF_DepthAct1[Zn2]+AF_Depth2[Zn2]+AF_Depth3[Zn2]+AF_Depth4[Zn2])
      # Mn2_RelImpLayer[Zn2,4] = AF_Depth4[Zn2]*Mc2_SOMDist[4]/Mc2_RelImpDeno[Zn2]+0*(AF_DepthAct1[Zn2]+AF_Depth2[Zn2]+AF_Depth3[Zn2]+AF_Depth4[Zn2])
      # Mn2_RelImpLayer[Zn3,1] = AF_DepthAct1[Zn3]*Mc2_SOMDist[1]/Mc2_RelImpDeno[Zn3]+0*(AF_DepthAct1[Zn3]+AF_Depth2[Zn3]+AF_Depth3[Zn3]+AF_Depth4[Zn3])
      # Mn2_RelImpLayer[Zn3,2] = AF_Depth2[Zn3]*Mc2_SOMDist[2]/Mc2_RelImpDeno[Zn3]+0*(AF_DepthAct1[Zn3]+AF_Depth2[Zn3]+AF_Depth3[Zn3]+AF_Depth4[Zn3])
      # Mn2_RelImpLayer[Zn3,3] = AF_Depth3[Zn3]*Mc2_SOMDist[3]/Mc2_RelImpDeno[Zn3]+0*(AF_DepthAct1[Zn3]+AF_Depth2[Zn3]+AF_Depth3[Zn3]+AF_Depth4[Zn3])
      # Mn2_RelImpLayer[Zn3,4] = AF_Depth4[Zn3]*Mc2_SOMDist[4]/Mc2_RelImpDeno[Zn3]+0*(AF_DepthAct1[Zn3]+AF_Depth2[Zn3]+AF_Depth3[Zn3]+AF_Depth4[Zn3])
      # Mn2_RelImpLayer[Zn4,1] = AF_DepthAct1[Zn4]*Mc2_SOMDist[1]/Mc2_RelImpDeno[Zn4]+0*(AF_DepthAct1[Zn4]+AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_Depth4[Zn4])
      # Mn2_RelImpLayer[Zn4,2] = AF_Depth2[Zn4]*Mc2_SOMDist[2]/Mc2_RelImpDeno[Zn4]+0*(AF_DepthAct1[Zn4]+AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_Depth4[Zn4])
      # Mn2_RelImpLayer[Zn4,3] = AF_Depth3[Zn4]*Mc2_SOMDist[3]/Mc2_RelImpDeno[Zn4]+0*(AF_DepthAct1[Zn4]+AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_Depth4[Zn4])
      # Mn2_RelImpLayer[Zn4,4] = AF_Depth4[Zn4]*Mc2_SOMDist[4]/Mc2_RelImpDeno[Zn4]+0*(AF_DepthAct1[Zn4]+AF_Depth2[Zn4]+AF_Depth3[Zn4]+AF_Depth4[Zn4])
      zonelayer_df$Mn2_RelImpLayer <- zonelayer_df$AF_Depth* zonelayer_df$Mc2_SOMDist/zonelayer_df$Mc2_RelImpDeno

      # S_WormAct[Zone,SoilLayer] = S_LFoodForWorms[Zone]*S_RelWormLit[1]+S_SOMFoodForWorms[Zone,SoilLayer]*Mn2_RelImpLayer[Zone,SoilLayer]
      zonelayer_df$S_LFoodForWorms <- rep(zone_df$S_LFoodForWorms, nlayer)
      zonelayer_df$S_WormAct <- zonelayer_df$S_LFoodForWorms* pars$S_par$S_RelWormLit[1]+ zonelayer_df$S_SOMFoodForWorms*zonelayer_df$Mn2_RelImpLayer
      
      
      # PD_CRhizConst[Zone] = if Cq_CropType[Zone] = 1 then PD_CRhizovory[Type1] else if Cq_CropType[Zone] = 2 then PD_CRhizovory[Type2] else if Cq_CropType[Zone] = 3 then PD_CRhizovory[Type3] else if Cq_CropType[Zone] = 4 then PD_CRhizovory[Type4] else if Cq_CropType[Zone] = 5 then PD_CRhizovory[Type5] else 0
      zone_df$PD_CRhizConst <- crop_df$PD_CRhizovory[zone_df$Cq_CType]
      
      # PD_CRhizImp[Zone,Animals] = PD_NastiesinPlot[Animals]*PD_CRhizovore?[Animals]*PD_CropsEaten?[Zone,Animals]
      zoneanimal_df$PD_NastiesinPlot <- rep(animal_df$PD_NastiesinPlot, each = nzone)
      zoneanimal_df$PD_CRhizovore_is <- rep(animal_df$PD_CRhizovore_is, each = nzone)
      zoneanimal_df$PD_CRhizImp <- zoneanimal_df$PD_NastiesinPlot * zoneanimal_df$PD_CRhizovore_is* zoneanimal_df$PD_CropsEaten_is

      # PD_CRhizoVFrac[Zone] = min(1,PD_CRhizConst[Zone]+AF_DynPestImpacts?*ARRAYSUM(PD_CRhizImp[Zone,*]))
      zone_df$PD_CRhizoVFrac <- pmin(1, zone_df$PD_CRhizConst + pars$AF_par$AF_DynPestImpacts_is * aggregate(zoneanimal_df["PD_CRhizImp"], zoneanimal_df["zone"], sum)$PD_CRhizImp)
      
      # C_RtDecay_DW[Zone,SoilLayer] = if C_PlantDiesToday?[Zone] = 1 then C_Root_DW[Zone,SoilLayer]/dt  else (0.69/Rt_CHalfLifeCurr[Zone] + PD_CRhizoVFrac[Zone]) *C_Root_DW[Zone,SoilLayer]
      # C_RtDecay_N[Zone,SoilLayer] = if C_PlantDiesToday?[Zone] = 1 then C_Root_N[Zone,SoilLayer]/dt  else (0.69/Rt_CHalfLifeCurr[Zone] + PD_CRhizoVFrac[Zone]) *C_Root_N[Zone,SoilLayer]
      # C_RtDecay_P[Zone,SoilLayer] = if C_PlantDiesToday?[Zone] = 1 then C_Root_P[Zone,SoilLayer]/dt  else (0.69/Rt_CHalfLifeCurr[Zone] + PD_CRhizoVFrac[Zone]) *C_Root_P[Zone,SoilLayer]
      
      zonelayerpcomp_df$C_PlantDiesToday_is <- rep(zone_df$C_PlantDiesToday_is, nlayer*nrow(pcomp_df))
      zonelayerpcomp_df$Rt_CHalfLifeCurr <- rep(zone_df$Rt_CHalfLifeCurr, nlayer*nrow(pcomp_df))
      zonelayerpcomp_df$PD_CRhizoVFrac <- rep(zone_df$PD_CRhizoVFrac, nlayer*nrow(pcomp_df))
      zonelayerpcomp_df$C_RtDecay <- ifelse(zonelayerpcomp_df$C_PlantDiesToday_is == 1, zonelayerpcomp_df$C_Root, 
                                            (0.69/zonelayerpcomp_df$Rt_CHalfLifeCurr + zonelayerpcomp_df$PD_CRhizoVFrac) * zonelayerpcomp_df$C_Root)
      
      #TODO: check whether the Cq_RtDiam multiplied twice (quadratic)
      # S_C_RootVolDecay[Zone,SoilLayer] = S_C_Rt_StrucFormFrac*C_RtDecay_DW[Zone,SoilLayer]*Rt_CSRLCurr[Zone]*Cq_RtDiam[Zone]*Cq_RtDiam[Zone]*3.14/4*0.1
      zonelayer_df$Rt_CSRLCurr <- rep(zone_df$Rt_CSRLCurr, nlayer)
      zonelayer_df$S_C_RootVolDecay <- pars$S_par$S_C_Rt_StrucFormFrac * zonelayerpcomp_df[zonelayerpcomp_df$PlantComp == "DW",]$C_RtDecay * 
        zonelayer_df$Rt_CSRLCurr* zonelayer_df$Cq_RtDiam*zonelayer_df$Cq_RtDiam*3.14/4*0.1
        
      # Rt_T_MeanResTime[Sp1] = T_Par1[Root_HalfLifeTime]/0.69 +0*(T_Par2[Root_HalfLifeTime]+T_Par3[Root_HalfLifeTime])
      # Rt_T_MeanResTime[Sp2] = 0*T_Par1[Root_HalfLifeTime] +T_Par2[Root_HalfLifeTime]/0.69 +0*T_Par3[Root_HalfLifeTime]
      # Rt_T_MeanResTime[Sp3] = 0*(T_Par1[Root_HalfLifeTime] +T_Par2[Root_HalfLifeTime])+T_Par3[Root_HalfLifeTime]/0.69 
      tree_df$Rt_T_MeanResTime <- tree_df$Rt_THalfLife/0.69

      # PD_TRhizImp[Tree,Animals] = PD_NastiesinPlot[Animals]*PD_TEatenBy?[Tree,Animals]*PD_TRhizovore?[Animals]
      treeanimal_df$PD_NastiesinPlot <- rep(animal_df$PD_NastiesinPlot, each = ntree)
      treeanimal_df$PD_TRhizovore_is <- rep(animal_df$PD_TRhizovore_is, each = ntree)
      treeanimal_df$PD_TRhizImp <- treeanimal_df$PD_NastiesinPlot * treeanimal_df$PD_TEatenBy_is* treeanimal_df$PD_TRhizovore_is
      
      # PD_TRhizVFrac[Tree] = min(1,PD_TRhizovory[Tree]+AF_DynPestImpacts?*ARRAYSUM(PD_TRhizImp[Tree,*]))
      tree_df$PD_TRhizVFrac <- pmin(1, tree_df$PD_TRhizovory + pars$AF_par$AF_DynPestImpacts_is * aggregate(treeanimal_df["PD_TRhizImp"], treeanimal_df["tree_id"], sum)$PD_TRhizImp)
  
      # S&B_FirTreeMort?[Tree] = if S&B_FireTempIncTopSoil[Zn1] >S&B_TTempTol[Tree] then 1 else 0
      tree_df$SB_FirTreeMort_is <- ifelse(zone_df[zone_df$zone == 1,]$SB_FireTempIncTopSoil > tree_df$SB_TTempTol, 1, 0)
      
      
      # T_DiesToday?[Tree] = if time = (T_KillDOY[Tree]+ 365* T_KillY[Tree]-Ca_DOYStart) or 
      # time = (T_Kill2DOY[Tree]+ 365* T_Kill2Y[Tree]-Ca_DOYStart) or 
      # time = (T_Kill3DOY[Tree]+ 365* T_Kill3Y[Tree]-Ca_DOYStart) 
      # then 1 else if T_StemDiam[Tree]>T_DiamTreshHarv[Tree] then 1 else
      #   S&B_FirTreeMort?[Tree]
      tree_df$time <- time
      tree_df$T_DiesToday_is <- ifelse(
        tree_df$time == (tree_df$T_KillDOY + 365 * tree_df$T_KillY - Ca_DOYStart) |
          tree_df$time == (tree_df$T_Kill2DOY + 365 * tree_df$T_Kill2Y - Ca_DOYStart) |
          tree_df$time == (tree_df$T_Kill3DOY + 365 * tree_df$T_Kill3Y - Ca_DOYStart),
        1,
        ifelse(
          tree_df$T_StemDiam > tree_df$T_DiamTreshHarv,
          1,
          tree_df$SB_FirTreeMort_is
        )
      )

      # T_Root_T1_DWdecay[Zone,SoilLayer] = if T_DiesToday?[Sp1]= 1 then T_RootT1DW[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp1]>0 THEN (1/Rt_T_MeanResTime[Sp1]+PD_TRhizVFrac[Sp1])*T_RootT1DW[Zone,SoilLayer] ELSE 0
      # T_Root_T2_DWdecay[Zone,SoilLayer] = if T_DiesToday?[Sp2]= 1 then T_RootT2DW[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp2]>0 THEN (1/Rt_T_MeanResTime[Sp2]+PD_TRhizVFrac[Sp2])*T_RootT2DW[Zone,SoilLayer] ELSE 0
      # T_Root_T3_DWdecay[Zone,SoilLayer] = if T_DiesToday?[Sp3]= 1 then T_RootT3DW[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp3]>0 THEN (1/Rt_T_MeanResTime[Sp3]+PD_TRhizVFrac[Sp3])*T_RootT3DW[Zone,SoilLayer] ELSE 0
      # T_Root_T1_Ndecay[Zone,SoilLayer] = if T_DiesToday?[Sp1]= 1 then T_RootT1N[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp1]>0 THEN (1/Rt_T_MeanResTime[Sp1]+PD_TRhizVFrac[Sp1])*T_RootT1N[Zone,SoilLayer] ELSE 0
      # T_Root_T2_Ndecay[Zone,SoilLayer] = if T_DiesToday?[Sp2]= 1 then T_RootT2N[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp2]>0 THEN (1/Rt_T_MeanResTime[Sp2]+PD_TRhizVFrac[Sp2])*T_RootT2N[Zone,SoilLayer] ELSE 0
      # T_Root_T3_Ndecay[Zone,SoilLayer] = if T_DiesToday?[Sp3]= 1 then T_RootT3N[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp3]>0 THEN (1/Rt_T_MeanResTime[Sp3]+PD_TRhizVFrac[Sp3])*T_RootT3N[Zone,SoilLayer] ELSE 0
      # T_Root_T1_Pdecay[Zone,SoilLayer] = if T_DiesToday?[Sp1]= 1 then T_RootT1P[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp1]>0 THEN (1/Rt_T_MeanResTime[Sp1]+PD_TRhizVFrac[Sp1])*T_RootT1P[Zone,SoilLayer] ELSE 0
      # T_Root_T2_Pdecay[Zone,SoilLayer] = if T_DiesToday?[Sp2]= 1 then T_RootT2P[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp2]>0 THEN (1/Rt_T_MeanResTime[Sp2]+PD_TRhizVFrac[Sp2])*T_RootT2P[Zone,SoilLayer] ELSE 0
      # T_Root_T3_Pdecay[Zone,SoilLayer] = if T_DiesToday?[Sp3]= 1 then T_RootT3P[Zone,SoilLayer]/dt else IF Rt_T_MeanResTime[Sp3]>0 THEN (1/Rt_T_MeanResTime[Sp3]+PD_TRhizVFrac[Sp3])*T_RootT3P[Zone,SoilLayer] ELSE 0
      
      colvar <- c("T_DiesToday_is", "Rt_T_MeanResTime", "PD_TRhizVFrac", "S_T_Rt_StrucFormFrac", "Rt_TSRL", "Rt_TDiam")
      df <- tree_df[c("tree_id", colvar)]
      t_df <- df[rep(seq_len(nrow(df)), each = nzone * nlayer * nrow(pcomp_df)), ]
      zlp_df <- zonelayerpcomp_df[c("zone", "layer", "PlantComp")]
      t2_df <- zlp_df[rep(seq_len(nrow(zlp_df)), ntree), ]
      t_df <- cbind(t_df, t2_df)
      t_df <- t_df[order(t_df$PlantComp, t_df$tree_id, t_df$layer, t_df$zone), ]
      zonelayertreepcomp_df[colvar] <- t_df[colvar]
      
      zonelayertreepcomp_df$T_Root_decay <- ifelse(
        zonelayertreepcomp_df$T_DiesToday_is == 1,
        zonelayertreepcomp_df$T_Root,
        ifelse(
          zonelayertreepcomp_df$Rt_T_MeanResTime > 0,
          (
            1 / zonelayertreepcomp_df$Rt_T_MeanResTime + zonelayertreepcomp_df$PD_TRhizVFrac
          ) * zonelayertreepcomp_df$T_Root,
          0
        )
      )
      
      # S_T_RootVolDecay[Zone,SoilLayer] = 3.14*(S_T_Rt_StrucFormFrac[Sp1]*T_Root_T1_DWdecay[Zone,SoilLayer]*Rt_TSRL[Sp1]*Rt_TDiam[Sp1]*Rt_TDiam[Sp1]+
      #                                            S_T_Rt_StrucFormFrac[Sp2]*T_Root_T2_DWdecay[Zone,SoilLayer]*Rt_TSRL[Sp2]*Rt_TDiam[Sp2]*Rt_TDiam[Sp2]+
      #                                            S_T_Rt_StrucFormFrac[Sp3]*T_Root_T3_DWdecay[Zone,SoilLayer]*Rt_TSRL[Sp3]*Rt_TDiam[Sp3]*Rt_TDiam[Sp3]) /4*0.1
      colvar <- c("S_T_Rt_StrucFormFrac", "Rt_TSRL", "Rt_TDiam")
      df <- tree_df[colvar]
      t_df <- df[rep(seq_len(nrow(df)), each = nzone * nlayer), ]
      zonelayertree_df[colvar] <- t_df[colvar]
      zonelayertree_df$S_T_RootVolDecay_a <- zonelayertree_df$S_T_Rt_StrucFormFrac * zonelayertreepcomp_df[zonelayertreepcomp_df$PlantComp == "DW",]$T_Root_decay * zonelayertree_df$Rt_TSRL * zonelayertree_df$Rt_TDiam * zonelayertree_df$Rt_TDiam
      zonelayer_df$S_T_RootVolDecay <- 3.14*(aggregate(zonelayertree_df["S_T_RootVolDecay_a"], zonelayertree_df[c("zone", "layer")], sum)$S_T_RootVolDecay_a) /4*0.1

      # S_TC_OldRC[Zone,SoilLayer] = S_C_RootVolDecay[Zone,SoilLayer]+S_T_RootVolDecay[Zone,SoilLayer]
      zonelayer_df$S_TC_OldRC <- zonelayer_df$S_C_RootVolDecay + zonelayer_df$S_T_RootVolDecay


      # INFLOWS:
      # S_BDModifierKsatV1[Zone] = if S_SoilStructDyn? =1   then -((S_WormAct[Zone,1]+S_TC_OldRC[Zone,1])* S_BDActOverBDRefKsatV1[Zone])+ S_BDBDrefDecay* (max(0,1- S_BDActOverBDRefKsatV1[Zone])^ S_BDEqPower) else 0
      # S_BDModifierKsatV2[Zone] = if S_SoilStructDyn? =1   then -((S_WormAct[Zone,2]+S_TC_OldRC[Zone,2])* S_BDActOverBDRefKsatV2[Zone])+ S_BDBDrefDecay* (max(0,1- S_BDActOverBDRefKsatV2[Zone])^ S_BDEqPower) else 0
      # S_BDModifierKsatV3[Zone] = if S_SoilStructDyn? =1   then -((S_WormAct[Zone,3]+S_TC_OldRC[Zone,3])* S_BDActOverBDRefKsatV3[Zone])+ S_BDBDrefDecay* (max(0,1- S_BDActOverBDRefKsatV3[Zone])^ S_BDEqPower) else 0
      # S_BDModifierKsatV4[Zone] = if S_SoilStructDyn? =1   then -((S_WormAct[Zone,4]+S_TC_OldRC[Zone,4])* S_BDActOverBDRefKsatV4[Zone])+ S_BDBDrefDecay* (max(0,1- S_BDActOverBDRefKsatV4[Zone])^ S_BDEqPower) else 0
       
      
      zonelayer_df$S_SoilStructDyn_is <-  pars$S_par$S_SoilStructDyn_is
      zonelayer_df$S_BDModifierKsatV <- ifelse(
        zonelayer_df$S_SoilStructDyn_is == 1,
        -((zonelayer_df$S_WormAct + zonelayer_df$S_TC_OldRC) * zonelayer_df$S_BDActOverBDRefKsatV) + pars$S_par$S_BDBDrefDecay *
          (max(0, 1 - zonelayer_df$S_BDActOverBDRefKsatV)^pars$S_par$S_BDEqPower) ,
        0
      )
      
      
      # S_WormActSurf[Zone] = S_LFoodForWorms[Zone]*S_RelWormSurf
      zone_df$S_WormActSurf <- zone_df$S_LFoodForWorms* pars$S_par$S_RelWormSurf
      
      # S_BDModifierInfiltr[Zone] = if S_SoilStructDyn? =1   then -(S_WormActSurf[Zone]*S_BDActOverBDRefInfiltr[Zone])+S_BDBDrefDecay*(max(0,1-S_BDActOverBDRefInfiltr[Zone])^S_BDEqPower)  else 0
      zone_df$S_BDModifierInfiltr <- ifelse(pars$S_par$S_SoilStructDyn_is == 1,
                                            -(zone_df$S_WormActSurf* zone_df$S_BDActOverBDRefInfiltr)+
                                              pars$S_par$S_BDBDrefDecay*(max(0,1- zone_df$S_BDActOverBDRefInfiltr)^pars$S_par$S_BDEqPower), 0)

      # S_BDActOverBDRefInfiltr[Zone](t) = S_BDActOverBDRefInfiltr[Zone](t - dt) + (S_BDModifierInfiltr[Zone]) * dt
      zone_df$S_BDActOverBDRefInfiltr <- zone_df$S_BDActOverBDRefInfiltr + zone_df$S_BDModifierInfiltr
      

      
      zone_df$Cq_CropType <- get_val_by_Ca_ComplCrop(Cq_CropType_df, zone_df$Ca_ComplCrop, zone_df$zone)
      
      # Cq_WeedRestart?[Zn1] = if AF_SimulateWeeds? = 1 and Cq_Stage[Zn1] = 0 then 1*AF_WeedZn?[Zn1] else 0
      # Cq_WeedRestart?[Zn2] = if AF_SimulateWeeds? = 1 and Cq_Stage[Zn2] = 0 then 1*AF_WeedZn?[Zn2] else 0
      # Cq_WeedRestart?[Zn3] = if AF_SimulateWeeds? = 1 and Cq_Stage[Zn3] = 0 then 1*AF_WeedZn?[Zn3] else 0
      # Cq_WeedRestart?[Zn4] = if AF_SimulateWeeds? = 1 and Cq_Stage[Zn4] = 0 then 1*AF_WeedZn?[Zn4] else 0
      zone_df$AF_SimulateWeeds_is <- pars$AF_par$AF_SimulateWeeds_is
      zone_df$Cq_WeedRestart_is <- ifelse(zone_df$AF_SimulateWeeds_is == 1 & zone_df$Cq_Stage == 0, zone_df$AF_WeedZn_is, 0)


      
      # Cq_CWswitch[Zone] = if time = int(Ca_PlantTime[Zone]-1) then Cq_CropType[Zone]-Cq_CropWeedSwitch[Zone]/dt  else if Cq_WeedRestart?[Zone] = 1 then Cq_WeedType- Cq_CropWeedSwitch[Zone]/dt else 0 
      zone_df$time <- time
      zone_df$Cq_CWswitch <- ifelse(zone_df$time == floor(zone_df$Ca_PlantTime-1), zone_df$Cq_CropType- zone_df$Cq_CropWeedSwitch, 
                                    ifelse( zone_df$Cq_WeedRestart_is == 1, pars$C_par$Cq_WeedType - zone_df$Cq_CropWeedSwitch, 0)) 
      
      # Cq_CropWeedSwitch[Zone](t) = Cq_CropWeedSwitch[Zone](t - dt) + (Cq_CWswitch[Zone]) * dt
      zone_df$Cq_CropWeedSwitch <- zone_df$Cq_CropWeedSwitch + zone_df$Cq_CWswitch

      Temp_AirDailyData <- Temp_AirDailyData_df[Temp_AirDailyData_df$Rain_DoY == Rain_DoY,]$Temp_AirDailyData
      
      # C_TempDevStage = IF(C_TOpt-C_TMin)>0 THEN  min(1,max(0,Temp_AirDailyData-C_TMin)/(C_TOpt-C_TMin)) ELSE 0
      C_TempDevStage <- ifelse((pars$C_par$C_TOpt- pars$C_par$C_TMin)>0 ,  min(1,max(0,Temp_AirDailyData- pars$C_par$C_TMin)/(pars$C_par$C_TOpt- pars$C_par$C_TMin)), 0)
            
      # Cq_StageInc[Zone] = if time = int(Ca_PlantTime[Zone]) or Cq_WeedRestart?[Zone] = 1 then (0.0001-Cq_Stage[Zone]/dt ) else
      # if Cq_Stage[Zone] = 0 then 0 else
      # if Cq_CropGraze >0 then (Cq_StageAfterGraze-Cq_Stage[Zone]/ dt ) else
      # if (Cq_Stage[Zone] >=2 ) then ((-Cq_Stage[Zone]+Cq_StageAfterHarvest[Zone])/dt) else
      # if (Cq_Stage[Zone] > 0 and Cq_Stage[Zone] < 1) then min(1-Cq_Stage[Zone], (1/(Cq_CTimeVegCurr[Zone]))) else
      # if (Cq_Stage[Zone] >1 and Cq_Stage[Zone]< 2) then (1/(Cq_CTimeGenCurr[Zone])*C_TempDevStage) else
      # if (mod (Time,365)  > Cq_DOYFlwDOYBegin[Zone]  and mod (Time,365)  < Cq_DOYFlwEnd[Zone]) then (1/Cq_CTimeGenCurr[Zone]*C_TempDevStage ) else 0
      
      zone_df$Cq_StageInc <- ifelse(
        zone_df$time == zone_df$Ca_PlantTime | zone_df$Cq_WeedRestart_is == 1,
        0.0001 - zone_df$Cq_Stage,
        ifelse(
          zone_df$Cq_Stage == 0,
          0,
          ifelse(
            Cq_CropGraze > 0,
            pars$C_par$Cq_StageAfterGraze - zone_df$Cq_Stage,
            ifelse(
              zone_df$Cq_Stage >= 2,
              -zone_df$Cq_Stage + zone_df$Cq_StageAfterHarvest,
              ifelse(
                zone_df$Cq_Stage > 0 & zone_df$Cq_Stage < 1,
                pmin(1 - zone_df$Cq_Stage, 1 / zone_df$Cq_CTimeVegCurr),
                ifelse(
                  zone_df$Cq_Stage > 1 & zone_df$Cq_Stage < 2,
                  1 / zone_df$Cq_CTimeGenCurr * C_TempDevStage,
                  ifelse((Time %% 365) > zone_df$Cq_DOYFlwDOYBegin &
                           (Time %% 365) < zone_df$Cq_DOYFlwEnd,
                         1 / zone_df$Cq_CTimeGenCurr * C_TempDevStage,
                         0
                  )
                )
              )
            )
          )
        )
      )

      # Cq_Stage[Zone](t) = Cq_Stage[Zone](t - dt) + (Cq_StageInc[Zone]) * dt
      zone_df$Cq_Stage <- zone_df$Cq_Stage + zone_df$Cq_StageInc

      zonepcomp_df$C_PlantDiesToday_is <-rep(zone_df$C_PlantDiesToday_is, each = nrow(pcomp_df))
      zonepcomp_df$Cq_CHarvAllocCurr <- rep(zone_df$Cq_CHarvAllocCurr, each = nrow(pcomp_df))
      zonepcomp_df$Cq_RemobFrac <- rep(zone_df$Cq_RemobFrac, each = nrow(pcomp_df))
      # C_StLeafInc[Zone,PlantComp] = IF (C_PlantDiesToday?[Zone]=0 )THEN(C_GroResMobFrac*(1-Cq_CHarvAllocCurr[Zone])*C_GroRes[Zone,PlantComp]-Cq_RemobFrac[Zone]*C_BiomStLv[Zone,PlantComp])ELSE(0)
      zonepcomp_df$C_StLeafInc <- ifelse(
        zonepcomp_df$C_PlantDiesToday_is == 0,
        pars$C_par$C_GroResMobFrac * (1 - zonepcomp_df$Cq_CHarvAllocCurr) * zonepcomp_df$C_GroRes -
          zonepcomp_df$Cq_RemobFrac * zonepcomp_df$C_BiomStLv,
        0
      )
      
      # C_Resid[Zone,PlantComp] = IF C_PlantDiesToday?[Zone] THEN C_ResidRemovalFrac*C_BiomStLv[Zone,PlantComp]/dt ELSE  0
      zonepcomp_df$C_Resid <- ifelse(zonepcomp_df$C_PlantDiesToday_is, pars$C_par$C_ResidRemovalFrac * zonepcomp_df$C_BiomStLv,  0)
      
      # C_StLeaveMulch[Zone,PlantComp] = if C_PlantDiesToday?[Zone]=1 then (1-C_ResidRemovalFrac)*C_BiomStLv[Zone,PlantComp]/dt else  if C_LAI[Zone]>C_LAIMax[Zone] then C_BiomStLv[Zone,PlantComp]*(C_LAI[Zone]-C_LAIMax[Zone])/C_LAI[Zone] else PD_CHerbVFrac[Zone]* C_BiomStLv[Zone,PlantComp]
      zonepcomp_df$C_StLeaveMulch <- ifelse( zonepcomp_df$C_PlantDiesToday_is ==1, 
                                             (1- pars$C_par$C_ResidRemovalFrac)* zonepcomp_df$C_BiomStLv,  
                                             ifelse( zonepcomp_df$C_LAI[Zone]> zonepcomp_df$C_LAIMax[Zone], 
                                                     zonepcomp_df$C_BiomStLv*(zonepcomp_df$C_LAI[Zone]-zonepcomp_df$C_LAIMax[Zone])/zonepcomp_df$C_LAI[Zone], 
                                                     zonepcomp_df$PD_CHerbVFrac[Zone]* zonepcomp_df$C_BiomStLv))
      # C_BiomHarvestDoY = GRAPH(C_BiomHarvestPast)
      C_BiomHarvestDoY <- get_graph_y(C_BiomHarvestDoY_df, pars$C_par$C_BiomHarvestPast) 
      C_BiomHarvestY <- get_graph_y(C_BiomHarvestY_df, pars$C_par$C_BiomHarvestPast) 
      C_BiomHarvestFracD <- get_graph_y(C_BiomHarvestFracD_df, pars$C_par$C_BiomHarvestPast) 
      
      # C_BiomHarvestDay = C_BiomHarvestDoY+365*(C_BiomHarvestY)-Ca_DOYStart
      C_BiomHarvestDay <- C_BiomHarvestDoY+365*(C_BiomHarvestY)-Ca_DOYStart
      
      zonepcomp_df$C_BiomHarv_is <- C_BiomHarv_is
      zonepcomp_df$C_BiomHarvestDay <- C_BiomHarvestDay
      zonepcomp_df$time <- time
      # C_BiomHarvest[Zone,PlantComp] = IF C_BiomHarv?=1 and TIME=INT(C_BiomHarvestDay) then C_BiomHarvestFracD*C_BiomStLv[Zone,PlantComp]/dt else 0 
      zonepcomp_df$C_BiomHarvest <- ifelse( zonepcomp_df$C_BiomHarv_is ==1 & zonepcomp_df$time == floor(zonepcomp_df$C_BiomHarvestDay), 
                                            C_BiomHarvestFracD* zonepcomp_df$C_BiomStLv, 0)  
      
      # C_BiomStLv[Zone,PlantComp](t) = C_BiomStLv[Zone,PlantComp](t - dt) + (C_StLeafInc[Zone,PlantComp] - C_Resid[Zone,PlantComp] - C_StLeaveMulch[Zone,PlantComp] - C_BiomHarvest[Zone,PlantComp]) * dt
      zonepcomp_df$C_BiomStLv <- zonepcomp_df$C_BiomStLv + (zonepcomp_df$C_StLeafInc - zonepcomp_df$C_Resid - zonepcomp_df$C_StLeaveMulch - zonepcomp_df$C_BiomHarvest)
      
      treepcomp_df$T_PrunPlant_is <- T_PrunOption_par$`PrunPlant?`
      
      T_PrunDoY <- get_graph_y(T_PrunDoY_df, pars$T_par$T_PrunPast)
      T_PrunY <- get_graph_y(T_PrunY_df, pars$T_par$T_PrunPast)
      
      # T_PrunDay = T_PrunDoY+365*(T_PrunY)-Ca_DOYStart
      T_PrunDay <- T_PrunDoY+365*(T_PrunY)-Ca_DOYStart
      
      
      # C_NewCropPlanted = IF(TIME=INT(Ca_PlantTime[Zn1]) OR TIME=INT(Ca_PlantTime[Zn2]) OR TIME=INT(Ca_PlantTime[Zn3]) OR TIME=INT(Ca_PlantTime[Zn4]))THEN 1 ELSE 0
      C_NewCropPlanted <- ifelse(any(time == floor(zone_df$Ca_PlantTime)), 1, 0)
      
      # T_CropinZone?[Zone] = IF C_Biom[Zone,DW]>0 THEN 1 ELSE 0
      zone_df$T_CropinZone_is <- ifelse(zonepcomp_df[zonepcomp_df$PlantComp == "DW",]$C_Biom > 0, 1, 0)
      
      
      # T_LAIcropzoneTot = (AF_ZoneWidth[Zn1]*T_CropinZone?[Zn1]*(T_LAIEff[Zn1,Sp1]+T_LAIEff[Zn1,Sp2]+T_LAIEff[Zn1,Sp3])+AF_ZoneWidth[Zn2]*T_CropinZone?[Zn2]* (T_LAIEff[Zn2,Sp1]+T_LAIEff[Zn2,Sp2]+T_LAIEff[Zn2,Sp3])+AF_ZoneWidth[Zn3]*T_CropinZone?[Zn3]* (T_LAIEff[Zn3,Sp1]+T_LAIEff[Zn3,Sp2]+T_LAIEff[Zn3,Sp3])+(AF_ZoneWidth[Zn4]*T_CropinZone?[Zn4]* (T_LAIEff[Zn4,Sp1]+T_LAIEff[Zn4,Sp2]+T_LAIEff[Zn4,Sp3])))/(AF_ZoneWidth[Zn1]+AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn3]+AF_ZoneWidth[Zn4])
      zone_df$T_LAIcropzoneTot_a <- zone_df$AF_ZoneWidth * zone_df$T_CropinZone_is * zone_df$T_LAIEff_sum 
      T_LAIcropzoneTot <- sum(zone_df$T_LAIcropzoneTot_a)/sum(zone_df$AF_ZoneWidth)
      

      # Cq_WeedZn?[Zone] = if Cq_CType[Zone]=Cq_WeedType then 1 else 0
      zone_df$Cq_WeedZn_is <- ifelse(zone_df$Cq_CType == pars$C_par$Cq_WeedType,  1, 0)
      
      # T_CropinField? = if ARRAYMEAN(Cq_WeedZn?[*]) <1 then IF(Cq_Stage[Zn1] <T_PrunStageLimit OR Cq_Stage[Zn2] <T_PrunStageLimit OR  Cq_Stage[Zn3] <T_PrunStageLimit OR  Cq_Stage[Zn4] <T_PrunStageLimit)THEN(1)ELSE(0) else 0
      T_CropinField_is <- ifelse(mean(zone_df$Cq_WeedZn_is) < 1, ifelse(any(
        zone_df$Cq_Stage < pars$T_par$T_PrunStageLimit
      ), 1, 0), 0)

      T_PrunType_is <- T_PrunOption_par$`PrunType?`
      
      
      tree_df$T_PrunFracD <- get_T_PrunFracD(pars$T_par$T_PrunPast)

      # T_PrunFrac[Tree] = T_PrunType?*T_PrunFracD[Tree] + (1-T_PrunType?)*T_PrunFracC[Tree]
      tree_df$T_PrunFrac <- T_PrunType_is* tree_df$T_PrunFracD + (1-T_PrunType_is)* tree_df$T_PrunFracC
  
      # T_Prun[PlantComp,Tree] = IF (T_PrunPlant?=1)THEN(if (TIME=INT(T_PrunDay) OR C_NewCropPlanted=1) OR (T_LAIcropzoneTot>T_PrunLimit and T_CropinField?=1) 
      # then (T_PrunFrac[Tree]*T_LfTwig[PlantComp,Tree] / dt)  else 0) ELSE  
      # (if (TIME=INT(T_PrunDay) OR(T_LAIcropzoneTot>T_PrunLimit) and T_CropinField?=1) 
      # then (T_PrunFrac[Tree]*T_LfTwig[PlantComp,Tree]/ dt) else 0)   
      
      treepcomp_df$T_Prun <- ifelse(
        treepcomp_df$T_PrunPlant_is == 1,
        ifelse(
          (time == floor(T_PrunDay) |
             C_NewCropPlanted == 1) |
            (T_LAIcropzoneTot > pars$T_par$T_PrunLimit &
               T_CropinField_is == 1),
          tree_df$T_PrunFrac * treepcomp_df$T_LfTwig,
          0
        ),
        ifelse (
          time == floor(T_PrunDay) |
            (T_LAIcropzoneTot > pars$T_par$T_PrunLimit) &
            T_CropinField_is == 1,
          tree_df$T_PrunFrac * treepcomp_df$T_LfTwig,
          0
        )
      )   
      

      tree_df$AF_AnyTrees_is <- pars$AF_par$AF_AnyTrees_is
      
      # T_GrowsToday?[Tree] = if AF_AnyTrees?= 0 then 0 else if (T_DiesToday?[Tree]=1) or (T_CumWatStress[Tree]>T_LiFallThreshWStress[Tree]^T_StressRatio) or (T_PrunLapse[Tree]<T_PrunRecov[Tree]) OR (T_Prun[DW, Tree]>0) or (time = int(S&B_SlashTime[Tree])) or (S&B_FireTime? = 1 ) THEN (0) else 1
      tree_df$T_GrowsToday_is <- ifelse(tree_df$AF_AnyTrees_is == 0, 0, ifelse(
        (tree_df$T_DiesToday_is == 1) |
          (
            tree_df$T_CumWatStress > tree_df$T_LiFallThreshWStress^pars$T_par$T_StressRatio
          ) |
          (tree_df$T_PrunLapse <
             tree_df$T_PrunRecov) |
          (treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_Prun > 0) |
          (time == floor(tree_df$SB_SlashTime)) |
          (SB_FireTime_is == 1),
        0,
        1
      ))
      
      treepcomp_df$T_GrowsToday_is <- rep(tree_df$T_GrowsToday_is, nrow(pcomp_df))
      treepcomp_df$T_ApplyPalm_is <- rep(tree_df$T_ApplyPalm_is, nrow(pcomp_df))
      treepcomp_df$T_GroResFrac <- rep(tree_df$T_GroResFrac, nrow(pcomp_df))
      
      treenut_df$AF_RunNutLim_is <- rep(nut_df$AF_RunNutLim_is, each = ntree)
      
      tree_df$T_LfTwig_DW <- treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_LfTwig
      treepcomp_df$T_LfTwig_DW <- rep(tree_df$T_LfTwig_DW, nrow(pcomp_df))
      treepcomp_df$T_LWR <- rep(tree_df$T_LWR, nrow(pcomp_df))
      
            
      # T_CanTargConc[PlantComp,Tree] = T_LWR[Tree]*T_LfConc[PlantComp,Tree]+(1-T_LWR[Tree])*T_TwigConc[PlantComp,Tree]
      treepcomp_df$T_CanTargConc <- treepcomp_df$T_LWR * treepcomp_df$T_LfConc + (1-treepcomp_df$T_LWR)*treepcomp_df$T_TwigConc
      
      treepcomp_df$T_UnitConv <- rep(pcomp_df$T_UnitConv, each = ntree)
      
      # T_TargetCan[PlantComp,Tree] = T_LfTwig[DW, Tree]*T_CanTargConc[PlantComp,Tree]*T_UnitConv[PlantComp]
      treepcomp_df$T_TargetCan <- treepcomp_df$T_LfTwig_DW* treepcomp_df$T_CanTargConc* treepcomp_df$T_UnitConv
      
      # T_NTargetCan[N,Sp1] = T_TargetCan[N,Sp1]
      # T_NTargetCan[N,Sp2] = T_TargetCan[N,Sp2]
      # T_NTargetCan[N,Sp3] = T_TargetCan[N,Sp3]
      # T_NTargetCan[P,Sp1] = T_TargetCan[P,Sp1]
      # T_NTargetCan[P,Sp2] = T_TargetCan[P,Sp2]
      # T_NTargetCan[P,Sp3] = T_TargetCan[P,Sp3]
      treenut_df$T_NTargetCan <- treepcomp_df[treepcomp_df$PlantComp %in% c("N", "P"),]$T_TargetCan

      # T_Root_DWtot[Sp1] = AF_ZoneFrac[Zn1]*(T_RootT1DW[Zn1,1]+T_RootT1DW[Zn1,2]+T_RootT1DW[Zn1,3]+T_RootT3DW[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT1DW[Zn2,1]+T_RootT1DW[Zn2,2]+T_RootT1DW[Zn2,3]+T_RootT1DW[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT1DW[Zn3,1]+T_RootT1DW[Zn3,2]+T_RootT1DW[Zn3,3]+T_RootT1DW[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT1DW[Zn4,1]+T_RootT1DW[Zn4,2]+T_RootT1DW[Zn4,3]+T_RootT1DW[Zn4,4])+
      #   0*(T_RootT1DW[Zn1,1]+T_RootT2DW[Zn1,1]+T_RootT3DW[Zn1,1])
      # T_Root_DWtot[Sp2] = AF_ZoneFrac[Zn1]*(T_RootT2DW[Zn1,1]+T_RootT2DW[Zn1,2]+T_RootT2DW[Zn1,3]+T_RootT2DW[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT2DW[Zn2,1]+T_RootT2DW[Zn2,2]+T_RootT2DW[Zn2,3]+T_RootT2DW[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT2DW[Zn3,1]+T_RootT2DW[Zn3,2]+T_RootT2DW[Zn3,3]+T_RootT2DW[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT2DW[Zn4,1]+T_RootT2DW[Zn4,2]+T_RootT2DW[Zn4,3]+T_RootT2DW[Zn4,4])+
      #   0*(T_RootT1DW[Zn1,1]+T_RootT2DW[Zn1,1]+T_RootT3DW[Zn1,1])
      # T_Root_DWtot[Sp3] = AF_ZoneFrac[Zn1]*(T_RootT3DW[Zn1,1]+T_RootT3DW[Zn1,2]+T_RootT3DW[Zn1,3]+T_RootT3DW[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT3DW[Zn2,1]+T_RootT3DW[Zn2,2]+T_RootT3DW[Zn2,3]+T_RootT3DW[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT3DW[Zn3,1]+T_RootT3DW[Zn3,2]+T_RootT3DW[Zn3,3]+T_RootT3DW[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT3DW[Zn4,1]+T_RootT3DW[Zn4,2]+T_RootT3DW[Zn4,3]+T_RootT3DW[Zn4,4])+
      #   0*(T_RootT1DW[Zn1,1]+T_RootT2DW[Zn1,1]+T_RootT3DW[Zn1,1])
      # T_RootNtot[Sp1] = AF_ZoneFrac[Zn1]*(T_RootT1N[Zn1,1]+T_RootT1N[Zn1,2]+T_RootT1N[Zn1,3]+T_RootT1N[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT1N[Zn2,1]+T_RootT1N[Zn2,2]+T_RootT1N[Zn2,3]+T_RootT1N[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT1N[Zn3,1]+T_RootT1N[Zn3,2]+T_RootT1N[Zn3,3]+T_RootT1N[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT1N[Zn4,1]+T_RootT1N[Zn4,2]+T_RootT1N[Zn4,3]+T_RootT1N[Zn4,4])+
      #   0*(T_RootT1N[Zn1,1]+T_RootT2N[Zn1,1]+T_RootT3N[Zn1,1])
      # T_RootNtot[Sp2] = AF_ZoneFrac[Zn1]*(T_RootT2N[Zn1,1]+T_RootT2N[Zn1,2]+T_RootT2N[Zn1,3]+T_RootT2N[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT2N[Zn2,1]+T_RootT2N[Zn2,2]+T_RootT2N[Zn2,3]+T_RootT2N[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT2N[Zn3,1]+T_RootT2N[Zn3,2]+T_RootT2N[Zn3,3]+T_RootT2N[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT2N[Zn4,1]+T_RootT2N[Zn4,2]+T_RootT2N[Zn4,3]+T_RootT2N[Zn4,4])+
      #   0*(T_RootT1N[Zn1,1]+T_RootT2N[Zn1,1]+T_RootT3N[Zn1,1])
      # T_RootNtot[Sp3] = AF_ZoneFrac[Zn1]*(T_RootT3N[Zn1,1]+T_RootT3N[Zn1,2]+T_RootT3N[Zn1,3]+T_RootT3N[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT3N[Zn2,1]+T_RootT3N[Zn2,2]+T_RootT3N[Zn2,3]+T_RootT3N[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT3N[Zn3,1]+T_RootT3N[Zn3,2]+T_RootT3N[Zn3,3]+T_RootT3N[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT3N[Zn4,1]+T_RootT3N[Zn4,2]+T_RootT3N[Zn4,3]+T_RootT3N[Zn4,4])+
      #   0*(T_RootT1N[Zn1,1]+T_RootT2N[Zn1,1]+T_RootT3N[Zn1,1])
      # T_RootPtot[Sp1] = AF_ZoneFrac[Zn1]*(T_RootT1P[Zn1,1]+T_RootT1P[Zn1,2]+T_RootT1P[Zn1,3]+T_RootT1P[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT1P[Zn2,1]+T_RootT1P[Zn2,2]+T_RootT1P[Zn2,3]+T_RootT1P[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT1P[Zn3,1]+T_RootT1P[Zn3,2]+T_RootT1P[Zn3,3]+T_RootT1P[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT1P[Zn4,1]+T_RootT1P[Zn4,2]+T_RootT1P[Zn4,3]+T_RootT1P[Zn4,4])+
      #   0*(T_RootT1P[Zn1,1]+T_RootT2P[Zn1,1]+T_RootT3P[Zn1,1])
      # T_RootPtot[Sp2] = AF_ZoneFrac[Zn1]*(T_RootT2P[Zn1,1]+T_RootT2P[Zn1,2]+T_RootT2P[Zn1,3]+T_RootT2P[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT2P[Zn2,1]+T_RootT2P[Zn2,2]+T_RootT2P[Zn2,3]+T_RootT2P[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT2P[Zn3,1]+T_RootT2P[Zn3,2]+T_RootT2P[Zn3,3]+T_RootT2P[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT2P[Zn4,1]+T_RootT2P[Zn4,2]+T_RootT2P[Zn4,3]+T_RootT2P[Zn4,4])+
      #   0*(T_RootT1P[Zn1,1]+T_RootT2P[Zn1,1]+T_RootT3P[Zn1,1])
      # T_RootPtot[Sp3] = AF_ZoneFrac[Zn1]*(T_RootT3P[Zn1,1]+T_RootT3P[Zn1,2]+T_RootT3P[Zn1,3]+T_RootT3P[Zn1,4])+
      #   AF_ZoneFrac[Zn2]*(T_RootT3P[Zn2,1]+T_RootT3P[Zn2,2]+T_RootT3P[Zn2,3]+T_RootT3P[Zn2,4])+
      #   AF_ZoneFrac[Zn3]*(T_RootT3P[Zn3,1]+T_RootT3P[Zn3,2]+T_RootT3P[Zn3,3]+T_RootT3P[Zn3,4])+
      #   AF_ZoneFrac[Zn4]*(T_RootT3P[Zn4,1]+T_RootT3P[Zn4,2]+T_RootT3P[Zn4,3]+T_RootT3P[Zn4,4])+
      #   0*(T_RootT1P[Zn1,1]+T_RootT2P[Zn1,1]+T_RootT3P[Zn1,1])
      
      zonetreepcomp_df <- aggregate(zonelayertreepcomp_df["T_Root"], zonelayertreepcomp_df[c("zone", "tree_id", "PlantComp")], sum)
      zonetreepcomp_df$AF_ZoneFrac <- rep(zone_df$AF_ZoneFrac, ntree*nrow(pcomp_df))
      zonetreepcomp_df$T_Root_zf <- zonetreepcomp_df$AF_ZoneFrac * zonetreepcomp_df$T_Root 
      treepcomp_df$T_RootTot <- aggregate(zonetreepcomp_df["T_Root_zf"], zonetreepcomp_df[c("tree_id", "PlantComp")], sum)$T_Root_zf
      
      # T_RootPlCompTot[Sp1,DW] = T_Root_DWtot[Sp1]+0*(T_RootNtot[Sp1]+T_RootPtot[Sp1])
      # T_RootPlCompTot[Sp1,N] = 0*T_Root_DWtot[Sp1]+T_RootNtot[Sp1]+0*T_RootPtot[Sp1]
      # T_RootPlCompTot[Sp1,P] = 0*T_Root_DWtot[Sp1]+0*T_RootNtot[Sp1]+T_RootPtot[Sp1]
      # T_RootPlCompTot[Sp2,DW] = T_Root_DWtot[Sp2]+0*T_RootNtot[Sp2]+0*T_RootPtot[Sp2]
      # T_RootPlCompTot[Sp2,N] = 0*T_Root_DWtot[Sp2]+T_RootNtot[Sp2]+0*T_RootPtot[Sp2]
      # T_RootPlCompTot[Sp2,P] = 0*T_Root_DWtot[Sp2]+0*T_RootNtot[Sp2]+T_RootPtot[Sp2]
      # T_RootPlCompTot[Sp3,DW] = T_Root_DWtot[Sp3]+0*T_RootNtot[Sp3]+0*T_RootPtot[Sp3]
      # T_RootPlCompTot[Sp3,N] = 0*T_Root_DWtot[Sp3]+T_RootNtot[Sp3]+0*T_RootPtot[Sp3]
      # T_RootPlCompTot[Sp3,P] = 0*T_Root_DWtot[Sp3]+0*T_RootNtot[Sp3]+T_RootPtot[Sp3]
      treepcomp_df$T_RootPlCompTot <- treepcomp_df$T_RootTot
      
      treepcomp_df$T_Fruit_DW <- rep(treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_Fruit, nrow(pcomp_df))
      treepcomp_df$T_GroRes_DW <- rep( treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_GroRes, nrow(pcomp_df))
      treepcomp_df$T_RootPlCompTot_DW <- rep(treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_RootPlCompTot, nrow(pcomp_df))
      treepcomp_df$T_SapWood_DW <- rep(treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_SapWood, nrow(pcomp_df))
      treepcomp_df$T_LatexStock_DW <- rep(treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_LatexStock, nrow(pcomp_df))
      treepcomp_df$T_HeartWood_DW <- rep(treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_HeartWood, nrow(pcomp_df))

      # T_TargetNonCan[PlantComp,Tree] = (T_Fruit[DW, Tree]*T_FruitConc[PlantComp,Tree]+T_GroRes[DW, Tree]*T_GroResConc[PlantComp,Tree]+T_RootPlCompTot[Tree,DW]*T_RtConc[PlantComp,Tree]+(T_SapWood[DW, Tree]+T_LatexStock[DW,Tree]+T_HeartWood[DW,Tree])*T_WoodConc[PlantComp,Tree])*T_UnitConv[PlantComp]
      treepcomp_df$T_TargetNonCan <- (
        treepcomp_df$T_Fruit_DW * treepcomp_df$T_FruitConc +
          treepcomp_df$T_GroRes_DW * treepcomp_df$T_GroResConc +
          treepcomp_df$T_RootPlCompTot_DW * treepcomp_df$T_RtConc +
          (
            treepcomp_df$T_SapWood_DW + treepcomp_df$T_LatexStock_DW +
              treepcomp_df$T_HeartWood_DW
          ) * treepcomp_df$T_WoodConc
      ) * treepcomp_df$T_UnitConv
      
      # T_NTargetNonCan[N,Sp1] = T_TargetNonCan[N,Sp1]
      # T_NTargetNonCan[N,Sp2] = T_TargetNonCan[N,Sp2]
      # T_NTargetNonCan[N,Sp3] = T_TargetNonCan[N,Sp3]
      # T_NTargetNonCan[P,Sp1] = T_TargetNonCan[P,Sp1]
      # T_NTargetNonCan[P,Sp2] = T_TargetNonCan[P,Sp2]
      # T_NTargetNonCan[P,Sp3] = T_TargetNonCan[P,Sp3]
      treenut_df$T_NTargetNonCan <- treepcomp_df[treepcomp_df$PlantComp %in% c("N", "P"),]$T_TargetNonCan
      
      # T_NTarget[SlNut,Tree] = T_NTargetCan[SlNut,Tree]+T_NTargetNonCan[SlNut,Tree]
      treenut_df$T_NTarget <- treenut_df$T_NTargetCan + treenut_df$T_NTargetNonCan
      
      # T_Biom[PlantComp,Tree] = T_GroRes[PlantComp,Tree]+T_LfTwig[PlantComp,Tree]+T_SapWood[PlantComp,Tree]+T_Fruit[PlantComp,Tree]+T_RootPlCompTot[Tree,PlantComp]+T_HeartWood[PlantComp,Tree]+T_LatexStock[PlantComp,Tree]
      treepcomp_df$T_Biom <- treepcomp_df$T_GroRes + treepcomp_df$T_LfTwig + treepcomp_df$T_SapWood + treepcomp_df$T_Fruit + treepcomp_df$T_RootPlCompTot + treepcomp_df$T_HeartWood + treepcomp_df$T_LatexStock
      
      # T_NBiom[N,Sp1] = T_Biom[N,Sp1]
      # T_NBiom[N,Sp2] = T_Biom[N,Sp2]
      # T_NBiom[N,Sp3] = T_Biom[N,Sp3]
      # T_NBiom[P,Sp1] = T_Biom[P,Sp1]
      # T_NBiom[P,Sp2] = T_Biom[P,Sp2]
      # T_NBiom[P,Sp3] = T_Biom[P,Sp3]
      treenut_df$T_NBiom <- treepcomp_df[treepcomp_df$PlantComp %in% c("N", "P"),]$T_Biom
      
      # T_NTargetNbiomRatio[SlNut,Tree] = if T_NTarget[SlNut,Tree]>0 then min(T_NBiom[SlNut,Tree]/T_NTarget[SlNut,Tree],1) else 1
      treenut_df$T_NTargetNbiomRatio <- ifelse(treenut_df$T_NTarget>0, min(treenut_df$T_NBiom/treenut_df$T_NTarget,1), 1)
      
      # T_NPosgro[SlNut,Tree] = if AF_RunNutLim?[SlNut] >0.5 then MAX(0,MIN(1,(2*(T_NTargetNbiomRatio[SlNut,Tree]-.4)))) else 1
      treenut_df$T_NPosgro <- ifelse(treenut_df$AF_RunNutLim_is >0.5, pmax(0, pmin(1,(2*(treenut_df$T_NTargetNbiomRatio-.4)))), 1) 
      
      # TF_WatNutSuff[Tree] = TF_RecentTWPosgro[Tree]*T_NPosgro[N,Tree]*T_NPosgro[P,Tree]
      tree_df$TF_WatNutSuff <- tree_df$TF_RecentTWPosgro* treenut_df[treenut_df$SlNut == "N",]$T_NPosgro* treenut_df[treenut_df$SlNut == "P",]$T_NPosgro
      
      # TF_PhyllochronStressed[Tree] = TF_PhyllochronStressFac[Tree]+(1-TF_PhyllochronStressFac[Tree])*TF_WatNutSuff[Tree]
      tree_df$TF_PhyllochronStressed <- tree_df$TF_PhyllochronStressFac+(1- tree_df$TF_PhyllochronStressFac)* tree_df$TF_WatNutSuff
      # Simulation_Time = TIME
      Simulation_Time <- time
      
      # TF_AgeofPalm[Tree] = max(0, Simulation_Time-(T_PlantTime[Tree]-1))
      tree_df$TF_AgeofPalm <- pmax(0, Simulation_Time-(tree_df$T_PlantTime-1))
      
      
      # TF_PotPhyllochronTime[Tree] = GRAPH(TF_AgeofPalm[Tree])
      tree_df$TF_PotPhyllochronTime <- get_TF_PotPhyllochronTime(tree_df$TF_AgeofPalm)
      
      # TF_PhyllochronTime[Tree] = if TF_PhyllochronStressed[Tree]> 0 then TF_PotPhyllochronTime[Tree]/TF_PhyllochronStressed[Tree] else 0
      tree_df$TF_PhyllochronTime <- ifelse( tree_df$TF_PhyllochronStressed> 0,  tree_df$TF_PotPhyllochronTime/tree_df$TF_PhyllochronStressed, 0) 
      
      # TF_TrunkHIncr[Tree] = if (TF_CurrentLeafNo[Tree]>TF_PalmTrunkIntercept[Tree]) then TF_PalmTrunkInternode[Tree]/TF_PhyllochronTime[Tree] else 0
      tree_df$TF_TrunkHIncr <- ifelse(tree_df$TF_CurrentLeafNo > tree_df$TF_PalmTrunkIntercept, tree_df$TF_PalmTrunkInternode/tree_df$TF_PhyllochronTime,0)
      
      # TF_TrunkVolIncrement[Tree] = TF_TrunkHIncr[Tree]*3.14*(TF_TrunkDiam[Tree]/2*10^-2)^2
      tree_df$TF_TrunkVolIncrement <- tree_df$TF_TrunkHIncr*3.14*(tree_df$TF_TrunkDiam/2*10^-2)^2
      
      # TF_TrunkBiomass_GrowthAllocation[Tree] = TF_TrunkVolIncrement[Tree]*T_WoodDens[Tree]* T_TreesperHa[Tree]/10^4
      tree_df$TF_TrunkBiomass_GrowthAllocation <- tree_df$TF_TrunkVolIncrement * tree_df$T_WoodDens* tree_df$T_Treesperha/10^4
      
      # TF_PotentialFrondLength[Tree] = GRAPH(TF_PalmTrunkHeight[Tree])
      tree_df$TF_PotentialFrondLength <- get_TF_PotentialFrondLength(tree_df$TF_PalmTrunkHeight)
      
      
      # TF_StressedFrondLength[Tree] = if TF_AgeofPalm[Tree] >0 then max(0,(1-TF_FrondLengthStressFac[Tree]+TF_FrondLengthStressFac[Tree]* TF_DW_Sufficiency_Yesterday[Tree])*TF_PotentialFrondLength[Tree]) else 0
      tree_df$TF_StressedFrondLength <- ifelse(tree_df$TF_AgeofPalm > 0, 
                                               pmax(0,(1-tree_df$TF_FrondLengthStressFac + 
                                                         tree_df$TF_FrondLengthStressFac * tree_df$TF_DW_Sufficiency_Yesterday)* tree_df$TF_PotentialFrondLength), 0)
      
      # TF_TargetNewFrondBiom[Tree] = if TF_FrondLDpetPower[Tree] > 0 and TF_FrondLDpetInterc[Tree]> 0 then TF_FrondWDpetInterc[Tree]*(TF_StressedFrondLength[Tree]/TF_FrondLDpetInterc[Tree])^(TF_FrondWDpetPower[Tree]/TF_FrondLDpetPower[Tree]) else 0
      tree_df$TF_TargetNewFrondBiom <- ifelse(tree_df$TF_FrondLDpetPower > 0 & tree_df$TF_FrondLDpetInterc> 0, 
                                              tree_df$TF_FrondWDpetInterc*( tree_df$TF_StressedFrondLength/tree_df$TF_FrondLDpetInterc)^(tree_df$TF_FrondWDpetPower/tree_df$TF_FrondLDpetPower), 0)
      
      # TF_TargetLeafAlloc[Tree] = if TF_PhyllochronTime[Tree]>0 then (TF_TargetNewFrondBiom[Tree]/TF_PhyllochronTime[Tree])*T_TreesperHa[Tree]/10^4  else 0
      tree_df$TF_TargetLeafAlloc <- ifelse(tree_df$TF_PhyllochronTime>0, (tree_df$TF_TargetNewFrondBiom/tree_df$TF_PhyllochronTime)*tree_df$T_Treesperha/10^4, 0)
      
      
      # TF_PalmTtBiomToLvs[Tree] = TF_TrunkBiomass_GrowthAllocation[Tree]+TF_TargetLeafAlloc[Tree]
      tree_df$TF_PalmTtBiomToLvs <- tree_df$TF_TrunkBiomass_GrowthAllocation+ tree_df$TF_TargetLeafAlloc
      
      treepcomp_df$TF_PalmTtBiomToLvs <- rep(tree_df$TF_PalmTtBiomToLvs, nrow(pcomp_df))
      
      # TF_VegDemand[PlantComp,Tree] = TF_PalmTtBiomToLvs[Tree]*T_UnitConv[PlantComp]
      treepcomp_df$TF_VegDemand <- treepcomp_df$TF_PalmTtBiomToLvs*treepcomp_df$T_UnitConv
      
      # T_DOY = mod(time,365)
      T_DOY <- time %% 365
      
      # T_LeafFlush?[Sp1] = GRAPH(T_DOY)
      # T_LeafFlush?[Sp2] = GRAPH(T_DOY)
      # T_LeafFlush?[Sp3] = GRAPH(T_DOY)
      tree_df$T_LeafFlush_is <- get_T_LeafFlush_is(T_DOY)
      
      # T_LeavesPotFormed?[Tree] = if (T_DOY_SeaLitFall_1_Start[Tree] < T_DOY_1_LfFlush[Tree]) or (T_DOY_SeaLitFall_2_Start[Tree] < T_DOY_2_LfFlush[Tree]) then 1 + Max(0,min(1,(mod(time,365) - T_DOY_1_LfFlush[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_SeaLitFall_1_Start[Tree]))) + Max(0,min(1,(mod(time,365) - T_DOY_2_LfFlush[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_SeaLitFall_2_Start[Tree]))) 
      # else Max(0,min(1,(mod(time,365) - T_DOY_1_LfFlush[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_SeaLitFall_1_Start[Tree]))) + Max(0,min(1,(mod(time,365) - T_DOY_2_LfFlush[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_SeaLitFall_2_Start[Tree]))) 
      
      tree_df$T_LeavesPotFormed_is_a <- pmax(0, pmin(1, ((time %% 365) - tree_df$T_DOY_1_LfFlush))) -  pmax(0, pmin(1, ((time %% 365) - tree_df$T_DOY_SeaLitFall_1_Start
      ))) + pmax(0, pmin(1, ((time %% 365) - tree_df$T_DOY_2_LfFlush))) -  pmax(0, pmin(1, ((time %% 365) - tree_df$T_DOY_SeaLitFall_2_Start
      )))
      
      tree_df$T_LeavesPotFormed_is <- ifelse(
        (tree_df$T_DOY_SeaLitFall_1_Start < tree_df$T_DOY_1_LfFlush) |
          (tree_df$T_DOY_SeaLitFall_2_Start < tree_df$T_DOY_2_LfFlush),
        1 + tree_df$T_LeavesPotFormed_is_a,
        tree_df$T_LeavesPotFormed_is_a
      )
      
      # T_LvsFormed?[Tree] = if T_GraphPhenol?[Tree] = 1 then T_LeafFlush?[Tree] else T_LeavesPotFormed?[Tree]
      tree_df$T_LvsFormed_is <- ifelse(tree_df$T_GraphPhenol_is == 1, tree_df$T_LeafFlush_is, tree_df$T_LeavesPotFormed_is)
      
      tree_df$T_Stage_VegGen <- treestage_df[treestage_df$Tree_Stage == "VegGen", ]$T_Stage
      
      # T_FruitAllocStage[Tree] = GRAPH(T_Stage[Tree,VegGen])
      tree_df$T_FruitAllocStage <- get_T_FruitAllocStage(tree_df$T_Stage_VegGen)
      
      # TW_UptTot[Sp1] = AF_ZoneFrac[Zn1]*(W_T1Upt1[Zn1]+W_T1Upt2[Zn1]+W_T1Upt3[Zn1]+W_T1Upt4[Zn1])+
      #   AF_ZoneFrac[Zn2]*(W_T1Upt1[Zn2]+W_T1Upt2[Zn2]+W_T1Upt3[Zn2]+W_T1Upt4[Zn2])+
      #   AF_ZoneFrac[Zn3]*(W_T1Upt1[Zn3]+W_T1Upt2[Zn3]+W_T1Upt3[Zn3]+W_T1Upt4[Zn3])+
      #   AF_ZoneFrac[Zn4]*(W_T1Upt1[Zn4]+W_T1Upt2[Zn4]+W_T1Upt3[Zn4]+W_T1Upt4[Zn4])+
      #   0*(W_T2Upt1[Zn1]+W_T2Upt2[Zn1]+W_T2Upt3[Zn1]+W_T2Upt4[Zn1] +
      #        W_T3Upt1[Zn1]+W_T3Upt2[Zn1]+W_T3Upt3[Zn1]+W_T3Upt4[Zn1])
      # TW_UptTot[Sp2] = AF_ZoneFrac[Zn1]*(W_T2Upt1[Zn1]+W_T2Upt2[Zn1]+W_T2Upt3[Zn1]+W_T2Upt4[Zn1])+
      #   AF_ZoneFrac[Zn2]*(W_T2Upt1[Zn2]+W_T2Upt2[Zn2]+W_T2Upt3[Zn2]+W_T2Upt4[Zn2])+
      #   AF_ZoneFrac[Zn3]*(W_T2Upt1[Zn3]+W_T2Upt2[Zn3]+W_T2Upt3[Zn3]+W_T2Upt4[Zn3])+
      #   AF_ZoneFrac[Zn4]*(W_T2Upt1[Zn4]+W_T2Upt2[Zn4]+W_T2Upt3[Zn4]+W_T2Upt4[Zn4])+
      #   0*(W_T1Upt1[Zn1]+W_T1Upt2[Zn1]+W_T1Upt3[Zn1]+W_T1Upt4[Zn1] +
      #        W_T3Upt1[Zn1]+W_T3Upt2[Zn1]+W_T3Upt3[Zn1]+W_T3Upt4[Zn1])
      # TW_UptTot[Sp3] = AF_ZoneFrac[Zn1]*(W_T3Upt1[Zn1]+W_T3Upt2[Zn1]+W_T3Upt3[Zn1]+W_T3Upt4[Zn1])+
      #   AF_ZoneFrac[Zn2]*(W_T3Upt1[Zn2]+W_T3Upt2[Zn2]+W_T3Upt3[Zn2]+W_T3Upt4[Zn2])+
      #   AF_ZoneFrac[Zn3]*(W_T3Upt1[Zn3]+W_T3Upt2[Zn3]+W_T3Upt3[Zn3]+W_T3Upt4[Zn3])+
      #   AF_ZoneFrac[Zn4]*(W_T3Upt1[Zn4]+W_T3Upt2[Zn4]+W_T3Upt3[Zn4]+W_T3Upt4[Zn4])+
      #   0*(W_T2Upt1[Zn1]+W_T2Upt2[Zn1]+W_T2Upt3[Zn1]+W_T2Upt4[Zn1] +
      #        W_T1Upt1[Zn1]+W_T1Upt2[Zn1]+W_T1Upt3[Zn1]+W_T1Upt4[Zn1])
      zonetree_df$W_Upt_sum <- aggregate(zonelayertree_df["W_Upt"], zonelayertree_df[c("zone", "tree_id")], sum)
      zonetree_df$W_Upt_sum_frac <- zonetree_df$AF_ZoneFrac * zonetree_df$W_Upt_sum
      tree_df$TW_UptTot <- aggregate(zonetree_df["W_Upt_sum_frac"], zonetree_df["tree_id"], sum)$W_Upt_sum_frac
      
      tree_df$AF_RunWatLim_is <- pars$AF_par$AF_RunWatLim_is
      # TW_Posgro[Tree] = IF (TW_DemandPot[Tree]>0 AND AF_RunWatLim? > 0.5)  THEN max(0,((TW_UptTot[Tree]-TP_WaterDemand[Tree])/(TW_DemandPot[Tree]-TP_WaterDemand[Tree]))) ELSE 1
      tree_df$TW_Posgro <- ifelse (tree_df$TW_DemandPot > 0 &
                                     tree_df$AF_RunWatLim_is > 0.5,
                                   pmax(0, ((tree_df$TW_UptTot - tree_df$TP_WaterDemand) / (tree_df$TW_DemandPot - tree_df$TP_WaterDemand)
                                   )),
                                   1)
      
      # T_RtAllocAct[Tree] = IF(Rt_ATType[Tree]>1 )THEN MIN(0.98,MAX(Rt_TAlloc[Tree],(1-(T_Stage[Tree,VegGen]/(T_Stage[Tree,VegGen]+Rt_THalfRtAllocStage)))+(0.98*(T_Stage[Tree,VegGen]/(T_Stage[Tree,VegGen]+Rt_THalfRtAllocStage)) - (1-(T_Stage[Tree,VegGen]/(T_Stage[Tree,VegGen]+Rt_THalfRtAllocStage))))*(1 -(min( min(T_NPosgro[N, Tree], T_NPosgro[P, Tree],T_NPosgroMin[N,Tree], T_NPosgroMin[P,Tree]),TW_PosgroMin[Tree], TW_Posgro[Tree]))^(0.0001+Rt_TAllocResp[Tree])))) else 0
      tree_df$T_RtAllocAct_a <- tree_df$T_Stage_VegGen / (tree_df$T_Stage_VegGen + pars$Rt_par$Rt_THalfRtAllocStage)
      tree_df$T_RtAllocAct <- ifelse(tree_df$Rt_ATType > 1, pmin(0.98, pmax(
        tree_df$Rt_TAlloc,
        (1 - tree_df$T_RtAllocAct_a) +
          (0.98 * tree_df$T_RtAllocAct_a -
             (1 - tree_df$T_RtAllocAct_a)) *
          (1 - (
            pmin(
              pmin(
                treenut_df[treenut_df$SlNut == "N", ]$T_NPosgro,
                treenut_df[treenut_df$SlNut == "P", ]$T_NPosgro,
                treenut_df[treenut_df$SlNut == "N", ]$T_NPosgroMin,
                treenut_df[treenut_df$SlNut == "P", ]$T_NPosgroMin
              ),
              tree_df$TW_PosgroMin,
              tree_df$TW_Posgro
            )
          )^(0.0001 + tree_df$Rt_TAllocResp))
      )), 0)
      
      treepcomp_df$T_LvsFormed_is <- rep(tree_df$T_LvsFormed_is, nrow(pcomp_df))
      treepcomp_df$T_FruitAllocStage <- rep(tree_df$T_FruitAllocStage, nrow(pcomp_df))
      treepcomp_df$T_RelFruitAllocMax <- rep(tree_df$T_RelFruitAllocMax, nrow(pcomp_df))
      treepcomp_df$T_RtAllocAct <- rep(tree_df$T_RtAllocAct, nrow(pcomp_df))
      # T_CanBiomInc[PlantComp,Tree] = if T_GrowsToday?[Tree] = 0 then 0  else if T_ApplyPalm?[Tree] = 1 then min(T_GroRes[PlantComp,Tree]*T_GroResFrac[Tree],TF_VegDemand[PlantComp,Tree]) else T_LvsFormed?[Tree]*MAX((T_LfTwig[DW, Tree]+(T_GroResFrac[Tree]*(1-T_FruitAllocStage[Tree]*T_RelFruitAllocMax[Tree])*T_GroRes[DW, Tree])*(1-T_RtAllocAct[Tree]))*T_UnitConv[PlantComp]*T_CanTargConc[PlantComp,Tree]-T_LfTwig[PlantComp,Tree],(T_GroResFrac[Tree]*T_GroRes[PlantComp,Tree])*(1-T_RtAllocAct[Tree])) 
      treepcomp_df$T_CanBiomInc <- ifelse(
        treepcomp_df$T_GrowsToday_is == 0,
        0,
        ifelse(
          treepcomp_df$T_ApplyPalm_is == 1,
          pmin(
            treepcomp_df$T_GroRes * treepcomp_df$T_GroResFrac,
            treepcomp_df$TF_VegDemand
          ),
          treepcomp_df$T_LvsFormed_is *
            pmax((
              treepcomp_df$T_LfTwig_DW + (
                treepcomp_df$T_GroResFrac * (
                  1 - treepcomp_df$T_FruitAllocStage * treepcomp_df$T_RelFruitAllocMax
                ) *
                  treepcomp_df$T_GroRes_DW
              ) * (1 - treepcomp_df$T_RtAllocAct)
            ) * treepcomp_df$T_UnitConv * treepcomp_df$T_CanTargConc -
              treepcomp_df$T_LfTwig,
            (treepcomp_df$T_GroResFrac * treepcomp_df$T_GroRes) * (1 - treepcomp_df$T_RtAllocAct)
            )
        )
      )
      
      treepcomp_df$T_PlantTime <- rep(tree_df$T_PlantTime, nrow(pcomp_df))
      treepcomp_df$AF_AnyTrees_is <- pars$AF_par$AF_AnyTrees_is
      treepcomp_df$T_CanBiomInit <- rep(tree_df$T_CanBiomInit, nrow(pcomp_df))
      treepcomp_df$T_Treesperha <- rep(tree_df$T_Treesperha, nrow(pcomp_df))
      # T_CanInit[PlantComp,Tree] = if  time =  T_PlantTime[Tree] and  AF_AnyTrees? = 1 then T_UnitConv[PlantComp]*T_CanTargConc[PlantComp,Tree]*T_CanBiomInit[Tree]*T_TreesperHa[Tree]/10000 else 0
      treepcomp_df$T_CanInit <- ifelse( time ==  treepcomp_df$T_PlantTime &  treepcomp_df$AF_AnyTrees_is == 1, treepcomp_df$T_UnitConv* treepcomp_df$T_CanTargConc* treepcomp_df$T_CanBiomInit* treepcomp_df$T_Treesperha/10000, 0)
      
      # T_CanBiomIni[PlantComp,Tree] = T_CanInit[PlantComp,Tree]
      treepcomp_df$T_CanBiomIni <- treepcomp_df$T_CanInit
      
      # T_LifallRed[DW,Sp1] = 0*(T_Par1[LifallRedN]+T_Par2[LifallRedN]+T_Par3[LifallRedN])
      # T_LifallRed[DW,Sp2] = 0*(T_Par1[LifallRedN]+T_Par2[LifallRedN]+T_Par3[LifallRedN])
      # T_LifallRed[DW,Sp3] = 0*(T_Par1[LifallRedN]+T_Par2[LifallRedN]+T_Par3[LifallRedN])
      # T_LifallRed[N,Sp1] = T_Par1[LifallRedN]+0*T_Par2[LifallRedN]+0*T_Par3[LifallRedN]
      # T_LifallRed[N,Sp2] = 0*T_Par1[LifallRedN]+T_Par2[LifallRedN]+0*T_Par3[LifallRedN]
      # T_LifallRed[N,Sp3] = 0*T_Par1[LifallRedN]+0*T_Par2[LifallRedN]+T_Par3[LifallRedN]
      # T_LifallRed[P,Sp1] = T_Par1[LifallRedP]+0*T_Par2[LifallRedP]+0*T_Par3[LifallRedP]
      # T_LifallRed[P,Sp2] = 0*T_Par1[LifallRedP]+T_Par2[LifallRedP]+0*T_Par3[LifallRedP]
      # T_LifallRed[P,Sp3] = 0*T_Par1[LifallRedP]+0*T_Par2[LifallRedP]+T_Par3[LifallRedP]
      treepcomp_df$T_LifallRed <- 0
      treepcomp_df[treepcomp_df$PlantComp == "N",]$T_LifallRed <- tree_df$T_NLifallRed_N
      treepcomp_df[treepcomp_df$PlantComp == "P",]$T_LifallRed <- tree_df$T_NLifallRed_P

      # T_LifallRedFac[PlantComp,Tree] = IF(T_TargetCan[PlantComp,Tree]>0)THEN((T_LfTwig[PlantComp,Tree]/T_TargetCan[PlantComp,Tree])^T_LifallRed[PlantComp,Tree])ELSE(1)
      treepcomp_df$T_LifallRedFac <- ifelse(treepcomp_df$T_TargetCan>0, (treepcomp_df$T_LfTwig/treepcomp_df$T_TargetCan)^treepcomp_df$T_LifallRed, 1)
      
      # T_LeafAgeTurnover[Tree] = 1/365*T_LeafHalfLife[Tree]
      tree_df$T_LeafAgeTurnover <- 1/365*tree_df$T_LeafHalfLife
      
      # T_LitFalFracDrought[Tree] = min(T_LifallDroughtFrac[Tree],max(0, T_CumWatStress[Tree] - T_LiFallThreshWStress[Tree])) 
      tree_df$T_LitFalFracDrought <- pmin(tree_df$T_LifallDroughtFrac, pmax(0, tree_df$T_CumWatStress - tree_df$T_LiFallThreshWStress)) 
      
      # T_GraphLeafFall[Sp1] = GRAPH(T_DOY)
      # T_GraphLeafFall[Sp2] = GRAPH(T_DOY)
      # T_GraphLeafFall[Sp3] = GRAPH(T_DOY)
      tree_df$T_GraphLeafFall <- get_T_GraphLeafFall(T_DOY)
      
      # T_PotSeasLiFall[Tree] = if (T_DOY_Compl_1_LfFall[Tree] < T_DOY_SeaLitFall_1_Start[Tree]) or (T_DOY_Compl_2_LfFall[Tree] < T_DOY_SeaLitFall_2_Start[Tree]) then 1 + Max(0,min(1,(mod(time,365) - T_DOY_SeaLitFall_1_Start[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_Compl_1_LfFall[Tree]))) + Max(0,min(1,(mod(time,365) - T_DOY_SeaLitFall_2_Start[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_Compl_2_LfFall[Tree]))) else Max(0,min(1,(mod(time,365) - T_DOY_SeaLitFall_1_Start[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_Compl_1_LfFall[Tree]))) + Max(0,min(1,(mod(time,365) - T_DOY_SeaLitFall_2_Start[Tree]))) -  Max(0,min(1,(mod(time,365) - T_DOY_Compl_2_LfFall[Tree]))) 
      tree_df$T_PotSeasLiFall_a <- pmax(0, pmin(1, T_DOY - tree_df$T_DOY_SeaLitFall_1_Start)) -
        pmax(0, pmin(1, T_DOY - tree_df$T_DOY_Compl_1_LfFall)) +
        pmax(0, pmin(1, T_DOY - tree_df$T_DOY_SeaLitFall_2_Start)) -
        pmax(0, pmin(1, T_DOY - tree_df$T_DOY_Compl_2_LfFall))
      
      tree_df$T_PotSeasLiFall <- ifelse(
        (tree_df$T_DOY_Compl_1_LfFall < tree_df$T_DOY_SeaLitFall_1_Start) |
          (tree_df$T_DOY_Compl_2_LfFall < tree_df$T_DOY_SeaLitFall_2_Start),
        1 + tree_df$T_PotSeasLiFall_a,
        tree_df$T_PotSeasLiFall_a
      )
      
      # T_PotSeasLiFallFrac[Tree] = if T_GraphPhenol?[Tree] = 1 then T_GraphLeafFall[Tree] else if T_DOY_Compl_1_LfFall[Tree]>T_DOY_SeaLitFall_1_Start[Tree] then (if mod(time,365) = T_DOY_Compl_1_LfFall[Tree] or mod(time,365) = T_DOY_Compl_2_LfFall[Tree] then T_FracSeasLitFll_1[Tree]*(1 - (1/(T_DOY_Compl_1_LfFall[Tree]-T_DOY_SeaLitFall_1_Start[Tree]))^(T_DOY_Compl_1_LfFall[Tree]-T_DOY_SeaLitFall_1_Start[Tree])) else T_FracSeasLitFll_1[Tree]*T_PotSeasLiFall[Tree]/(T_DOY_Compl_1_LfFall[Tree]-T_DOY_SeaLitFall_1_Start[Tree])) else (if mod(time,365) = T_DOY_Compl_1_LfFall[Tree] or mod(time,365) = T_DOY_Compl_2_LfFall[Tree] then T_FracSeasLitFll_1[Tree]*(1 - (1/(365 - T_DOY_SeaLitFall_1_Start[Tree] + T_DOY_Compl_1_LfFall[Tree]))^(365 - T_DOY_SeaLitFall_1_Start[Tree] + T_DOY_Compl_1_LfFall[Tree])) else T_FracSeasLitFll_1[Tree]*T_PotSeasLiFall[Tree]/ min(10,(365 - T_DOY_SeaLitFall_1_Start[Tree] + T_DOY_Compl_1_LfFall[Tree])))
      tree_df$T_PotSeasLiFallFrac <- ifelse(
        tree_df$T_GraphPhenol_is == 1,
        tree_df$T_GraphLeafFall,
        ifelse(
          tree_df$T_DOY_Compl_1_LfFall > tree_df$T_DOY_SeaLitFall_1_Start,
          ifelse(
            T_DOY == tree_df$T_DOY_Compl_1_LfFall |
              T_DOY == tree_df$T_DOY_Compl_2_LfFall,
            tree_df$T_FracSeasLitFll_1 *
              (
                1 - (
                  1 / (
                    tree_df$T_DOY_Compl_1_LfFall - tree_df$T_DOY_SeaLitFall_1_Start
                  )
                )^(
                  tree_df$T_DOY_Compl_1_LfFall - tree_df$T_DOY_SeaLitFall_1_Start
                )
              ),
            tree_df$T_FracSeasLitFll_1 * tree_df$T_PotSeasLiFall /
              (
                tree_df$T_DOY_Compl_1_LfFall - tree_df$T_DOY_SeaLitFall_1_Start
              )
          ),
          ifelse(
            T_DOY == tree_df$T_DOY_Compl_1_LfFall |
              T_DOY == tree_df$T_DOY_Compl_2_LfFall,
            tree_df$T_FracSeasLitFll_1 *
              (
                1 - (
                  1 / (
                    365 - tree_df$T_DOY_SeaLitFall_1_Start + tree_df$T_DOY_Compl_1_LfFall
                  )
                )^(
                  365 - tree_df$T_DOY_SeaLitFall_1_Start + tree_df$T_DOY_Compl_1_LfFall
                )
              ),
            tree_df$T_FracSeasLitFll_1 * tree_df$T_PotSeasLiFall / min(
              10,
              (
                365 - tree_df$T_DOY_SeaLitFall_1_Start + tree_df$T_DOY_Compl_1_LfFall
              )
            )
          )
        )
      )
      
      # T_StemDAllom[Tree] = if T_LfTwig[DW,Tree] > 0.001 AND T_DiamSlopeLfTwig[Tree]>0  then (T_LfTwig[DW, Tree]*10000/(T_Treesperha[Tree]*T_DiamLfTwig1[Tree]))^(1/T_DiamSlopeLfTwig[Tree]) else 0
      tree_df$T_StemDAllom <- ifelse(
        tree_df$T_LfTwig_DW > 0.001 & tree_df$T_DiamSlopeLfTwig > 0,
        (
          tree_df$T_LfTwig_DW * 10000 / (tree_df$T_Treesperha * tree_df$T_DiamLfTwig1)
        )^(1 / tree_df$T_DiamSlopeLfTwig),
        0
      )
      
      
      # T_CumLitTarget[Tree] = T_Treesperha[Tree]*0.0001*T_DiamCumLit1[Tree]*(T_StemDAllom[Tree]^T_DiamSlopeCumLit[Tree])
      tree_df$T_CumLitTarget <- tree_df$T_Treesperha*0.0001*tree_df$T_DiamCumLit1*(tree_df$T_StemDAllom^tree_df$T_DiamSlopeCumLit)

      # T_FBALitFal[Tree] = T_ApplyFBA?[Tree]*max(0,T_LifallDelay*(T_CumLitTarget[Tree]-T_LifallCum[DW,Tree]))
      tree_df$T_FBALitFal <- tree_df$T_ApplyFBA_is* pmax(0, pars$T_par$T_LifallDelay*(tree_df$T_CumLitTarget- treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_LifallCum))
      
      # TF_LeafClockTicks?[Tree] = if Simulation_Time<T_PlantTime[Tree] then 0 else if Simulation_Time - TF_LeafTime[Tree] >=TF_PhyllochronTime[Tree] then TF_PhyllochronTime[Tree] else 0
      tree_df$TF_LeafClockTicks_is <- ifelse(Simulation_Time< tree_df$T_PlantTime, 0, ifelse(Simulation_Time - tree_df$TF_LeafTime >= tree_df$TF_PhyllochronTime, tree_df$TF_PhyllochronTime, 0))

      # TF_NewLeaf?[Tree] = if TF_LeafClockTicks?[Tree] <> 0 then 1 else 0
      tree_df$TF_NewLeaf_is <- ifelse(tree_df$TF_LeafClockTicks_is != 0, 1, 0)
      
      # TF_FruitHarvNoperBunch[Sp1,Ripe] = if TF_NewLeaf?[Sp1] = 1 then TF_FruitsperBunch[Sp1,Ripe] else 0
      # TF_FruitHarvNoperBunch[Sp1,Ripe_1] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_2] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_3] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_4] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_5] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_6] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_7] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_8] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_9] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_10] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_11] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Ripe_12] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Early_fruit] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Pollinated] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Anthesis] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Anthesis_1] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Anthesis_2] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Anthesis_3] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Anthesis_4] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Anthesis_5] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp1,Anthesis_6] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe] = if TF_NewLeaf?[Sp2] = 1 then TF_FruitsperBunch[Sp2,Ripe] else 0
      # TF_FruitHarvNoperBunch[Sp2,Ripe_1] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_2] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_3] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_4] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_5] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_6] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_7] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_8] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_9] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_10] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_11] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Ripe_12] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Early_fruit] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Pollinated] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Anthesis] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Anthesis_1] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Anthesis_2] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Anthesis_3] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Anthesis_4] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Anthesis_5] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp2,Anthesis_6] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe] = if TF_NewLeaf?[Sp3] = 1 then TF_FruitsperBunch[Sp3,Ripe] else 0
      # TF_FruitHarvNoperBunch[Sp3,Ripe_1] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_2] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_3] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_4] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_5] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_6] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_7] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_8] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_9] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_10] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_11] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Ripe_12] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Early_fruit] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Pollinated] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Anthesis] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Anthesis_1] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Anthesis_2] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Anthesis_3] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Anthesis_4] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Anthesis_5] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      # TF_FruitHarvNoperBunch[Sp3,Anthesis_6] = 0*(TF_FruitsperBunch[Sp3,Ripe_1]+TF_NewLeaf?[Sp1])
      treefruit_df$TF_FruitHarvNoperBunch <- 0
      treefruit_df[treefruit_df$Fruitbunch == "Ripe",]$TF_FruitHarvNoperBunch <- ifelse(tree_df$TF_NewLeaf_is == 1, 1, 0)
      
      # T_LitFalFracShade[Tree] =  Max(0,T_LAICan[Tree]-T_LAIMax[Tree], T_CanH[Tree]-T_CanHMax[Tree])
      tree_df$T_LitFalFracShade <-  pmax(0,tree_df$T_LAICan- tree_df$T_LAIMax, tree_df$T_CanH- tree_df$T_CanHMax)
      
      # TF_OldFrondChange2[Tree] = if TF_FruitHarvNoperBunch[Tree,Ripe]>0 then TF_OldFrontBiomass[Tree] else 0
      tree_df$TF_OldFrondChange2 <- ifelse(treefruit_df[treefruit_df$Fruitbunch == "Ripe",]$TF_FruitHarvNoperBunch > 0, tree_df$TF_OldFrontBiomass, 0)
      
      # TF_LiFall[Tree] = TF_OldFrondChange2[Tree]
      tree_df$TF_LiFall <- tree_df$TF_OldFrondChange2
      

      treepcomp_df$T_LeafAgeTurnover <- rep(tree_df$T_LeafAgeTurnover, nrow(pcomp_df))
      treepcomp_df$T_LitFalFracDrought <- rep(tree_df$T_LitFalFracDrought, nrow(pcomp_df))
      treepcomp_df$T_PotSeasLiFallFrac <- rep(tree_df$T_PotSeasLiFallFrac, nrow(pcomp_df))
      treepcomp_df$T_FBALitFal <- rep(tree_df$T_FBALitFal, nrow(pcomp_df))
      treepcomp_df$T_LitFalFracShade <- rep(tree_df$T_LitFalFracShade, nrow(pcomp_df))
      treepcomp_df$TF_LiFall <- rep(tree_df$TF_LiFall, nrow(pcomp_df))
      treepcomp_df$TF_LitFallRedFac <- rep(pcomp_df$TF_LitFallRedFac, each = ntree)
      # T_LifallInc[PlantComp,Tree] = if T_ApplyPalm?[Tree] = 0 then min(1,T_LifallRedFac[PlantComp,Tree]*T_LWR[Tree]) * ((T_LfTwig[PlantComp,Tree]*(T_LeafAgeTurnover[Tree]+T_LitFalFracDrought[Tree] +T_PotSeasLiFallFrac[Tree] +T_FBALitFal[Tree]) + T_CanBiomInc[PlantComp,Tree]*T_LitFalFracShade[Tree])) else TF_LiFall[Tree]*TF_LitFallRedFac[PlantComp] * TF_LiFall_ResWithdrawl 
      treepcomp_df$T_LifallInc <- ifelse(
        treepcomp_df$T_ApplyPalm_is == 0,
        pmin(1, treepcomp_df$T_LifallRedFac * treepcomp_df$T_LWR) *
          ((
            treepcomp_df$T_LfTwig * (
              treepcomp_df$T_LeafAgeTurnover + treepcomp_df$T_LitFalFracDrought + treepcomp_df$T_PotSeasLiFallFrac + treepcomp_df$T_FBALitFal
            ) +
              treepcomp_df$T_CanBiomInc * treepcomp_df$T_LitFalFracShade
          )
          ),
        treepcomp_df$TF_LiFall * treepcomp_df$TF_LitFallRedFac * pars$T_par$TF_LiFall_ResWithdrawl
      )
      
      # TF_RelStemAlloc[Tree] = if TF_PalmTtBiomToLvs[Tree] = 0 then 0 else TF_TrunkBiomass_GrowthAllocation[Tree]/TF_PalmTtBiomToLvs[Tree]
      tree_df$TF_RelStemAlloc <- ifelse(tree_df$TF_PalmTtBiomToLvs == 0, 0, tree_df$TF_TrunkBiomass_GrowthAllocation/tree_df$TF_PalmTtBiomToLvs)
      
      tree_df$T_BiomAG_DW <- treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_BiomAG
      
      # T_SapWoodDiam[Tree] = if T_BiomAG[DW,Tree] > 0.001 AND T_DiamSlopeBiom[Tree]>0  then (T_BiomAG[DW, Tree]*10000/(T_Treesperha[Tree]*T_DiamBiom1[Tree]))^(1/T_DiamSlopeBiom[Tree]) else 0
      tree_df$T_SapWoodDiam <- ifelse(tree_df$T_BiomAG_DW > 0.001 & tree_df$T_DiamSlopeBiom>0, (tree_df$T_BiomAG_DW*10000/(tree_df$T_Treesperha* tree_df$T_DiamBiom1))^(1/tree_df$T_DiamSlopeBiom), 0)
      # T_TargetLeaf&Twig[Tree] = 0.0001*T_Treesperha[Tree]*T_DiamLfTwig1[Tree]*(T_SapWoodDiam[Tree]^T_DiamSlopeLfTwig[Tree])
      tree_df$T_TargetLeafTwig <- 0.0001*tree_df$T_Treesperha* tree_df$T_DiamLfTwig1*(tree_df$T_SapWoodDiam^tree_df$T_DiamSlopeLfTwig)
      
      treepcomp_df$TF_TrunkBiomass_GrowthAllocation <- rep(tree_df$TF_TrunkBiomass_GrowthAllocation, nrow(pcomp_df))
      treepcomp_df$T_ApplyFBA_is <- rep(tree_df$T_ApplyFBA_is, nrow(pcomp_df))
      treepcomp_df$T_LAICan <- rep(tree_df$T_LAICan, nrow(pcomp_df))
      treepcomp_df$T_LAIMax <- rep(tree_df$T_LAIMax, nrow(pcomp_df))
      treepcomp_df$T_CanH <- rep(tree_df$T_CanH, nrow(pcomp_df))
      treepcomp_df$T_CanHMax <- rep(tree_df$T_CanHMax, nrow(pcomp_df))
      treepcomp_df$T_TargetLeafTwig <- rep(tree_df$T_TargetLeafTwig, nrow(pcomp_df))
      
      # T_WoodInc[PlantComp,Tree] = if T_ApplyPalm?[Tree] = 1 then 
      # min(TF_RelStemAlloc[Tree]*T_CanBiomInc[PlantComp,Tree],TF_TrunkBiomass_GrowthAllocation[Tree]*T_WoodConc[PlantComp,Tree] ) else
      #   if T_ApplyFBA?[Tree] = 0 then 
      #     (IF(T_LAICan[Tree]>T_LAIMax[Tree])THEN(T_CanBiomInc[PlantComp,Tree]*(1-T_LWR[Tree])) ELSE
      #       (IF(T_CanH[Tree]>T_CanHMax[Tree])THEN (T_CanBiomInc[PlantComp,Tree]*(1-T_LWR[Tree])*(1-T_CanHMax[Tree]/T_CanH[Tree])^2)ELSE(0))) else
      #     max(0,-T_TargetLeaf&Twig[Tree]+T_LfTwig[DW,Tree])*T_UnitConv[PlantComp]*T_WoodConc[PlantComp,Tree]
      treepcomp_df$T_WoodInc <- ifelse(
        treepcomp_df$T_ApplyPalm_is == 1,
        pmin(
          treepcomp_df$TF_RelStemAlloc * treepcomp_df$T_CanBiomInc,
          treepcomp_df$TF_TrunkBiomass_GrowthAllocation * treepcomp_df$T_WoodConc
        ),
        ifelse(
          treepcomp_df$T_ApplyFBA_is == 0,
          ifelse(
            treepcomp_df$T_LAICan > treepcomp_df$T_LAIMax,
            treepcomp_df$T_CanBiomInc * (1 - treepcomp_df$T_LWR),
            ifelse(
              treepcomp_df$T_CanH > treepcomp_df$T_CanHMax,
              treepcomp_df$T_CanBiomInc * (1 - treepcomp_df$T_LWR) * (1 - treepcomp_df$T_CanHMax / treepcomp_df$T_CanH)^2,
              0
            )
          ),
          pmax(0, -treepcomp_df$T_TargetLeafTwig + treepcomp_df$T_LfTwig_DW) *
            treepcomp_df$T_UnitConv * treepcomp_df$T_WoodConc
        )
      )
      
      treeanimal_df$PD_THerbivore_is <- rep(animal_df$PD_THerbivore_is, each = ntree)
      
      # PD_THerbImp[Tree,Animals] = PD_NastiesinPlot[Animals]*PD_THerbivore?[Animals]*PD_TEatenBy?[Tree,Animals]
      treeanimal_df$PD_THerbImp <- treeanimal_df$PD_NastiesinPlot*treeanimal_df$PD_THerbivore_is* treeanimal_df$PD_TEatenBy_is
      
      # PD_THerbVFrac[Tree] = min(1,PD_THerbivory[Tree]+AF_DynPestImpacts?*ARRAYSUM(PD_THerbImp[Tree,*]))
      tree_df$PD_THerbVFrac <- pmin(1, tree_df$PD_THerbivory+ pars$AF_par$AF_DynPestImpacts_is* aggregate(treeanimal_df["PD_THerbImp"], treeanimal_df["tree_id"], sum)$PD_THerbImp)
      
      treepcomp_df$PD_THerbVFrac <- rep(tree_df$PD_THerbVFrac, nrow(pcomp_df))
      
      # T_Herbivory[PlantComp,Tree] = PD_THerbVFrac[Tree]*T_LfTwig[PlantComp,Tree]
      treepcomp_df$T_Herbivory <- treepcomp_df$PD_THerbVFrac* treepcomp_df$T_LfTwig
      
      treepcomp_df$SB_SlashTime <- rep(tree_df$SB_SlashTime, nrow(pcomp_df))
      treepcomp_df$T_DiesToday_is <- rep(tree_df$T_DiesToday_is, nrow(pcomp_df))
      
      # T_CanBiomSlashed[PlantComp,Tree] = if time = int(S&B_SlashTime[Tree]) or S&B_FireTime? = 1 or T_DiesToday?[Tree] = 1  then T_LfTwig[PlantComp,Tree]/dt else 0
      treepcomp_df$T_CanBiomSlashed <- ifelse(time == floor(treepcomp_df$SB_SlashTime) | SB_FireTime_is == 1 | treepcomp_df$T_DiesToday_is == 1, treepcomp_df$T_LfTwig, 0)
      
      # T_WoodHarvDoY[Sp1] = GRAPH(T_WoodHarvPast[Sp1])
      # T_WoodHarvDoY[Sp2] = GRAPH(T_WoodHarvPast[Sp2])
      # T_WoodHarvDoY[Sp3] = GRAPH(T_WoodHarvPast[Sp3])
      tree_df$T_WoodHarvDoY <- get_T_WoodHarvDoY(tree_df$T_WoodHarvPast)
      tree_df$T_WoodHarvY <- get_T_WoodHarvY(tree_df$T_WoodHarvPast)
      
      # T_WoodHarvDay[Tree] = T_WoodHarvDoY[Tree]+365*(T_WoodHarvY[Tree])-Ca_DOYStart
      tree_df$T_WoodHarvDay <- tree_df$T_WoodHarvDoY+ 365*(tree_df$T_WoodHarvY)-Ca_DOYStart
      
      treepcomp_df$T_WoodHarvDay <- rep(tree_df$T_WoodHarvDay, nrow(pcomp_df))
      # T_CanBiomTimHarv[PlantComp,Tree] = IF TIME = T_WoodHarvDay[Tree] THEN T_LfTwig[PlantComp,Tree] ELSE 0
      treepcomp_df$T_CanBiomTimHarv <- ifelse(time == treepcomp_df$T_WoodHarvDay, treepcomp_df$T_LfTwig, 0)
      
      # T_LfTwig[PlantComp,Tree](t) = T_LfTwig[PlantComp,Tree](t - dt) + (T_CanBiomInc[PlantComp,Tree] + T_CanBiomIni[PlantComp,Tree] - T_Prun[PlantComp,Tree] - T_LifallInc[PlantComp,Tree] - T_WoodInc[PlantComp,Tree] - T_Herbivory[PlantComp,Tree] - T_CanBiomSlashed[PlantComp,Tree] - T_CanBiomTimHarv[PlantComp,Tree]) * dt
      treepcomp_df$T_LfTwig <- treepcomp_df$T_LfTwig + (
        treepcomp_df$T_CanBiomInc + treepcomp_df$T_CanBiomIni - treepcomp_df$T_Prun - treepcomp_df$T_LifallInc - treepcomp_df$T_WoodInc - treepcomp_df$T_Herbivory - treepcomp_df$T_CanBiomSlashed - treepcomp_df$T_CanBiomTimHarv
      ) 
      
      # TF_DWSufficiency[Tree] = if TF_PalmTtBiomToLvs[Tree] > 0 then T_CanBiomInc[DW,Tree]/TF_PalmTtBiomToLvs[Tree] else 1
      tree_df$TF_DWSufficiency <- ifelse(tree_df$TF_PalmTtBiomToLvs > 0, treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_CanBiomInc/tree_df$TF_PalmTtBiomToLvs, 1)
      
      # TF_FrondGrowth[Tree] = if TF_AgeofPalm[Tree] > 0 then if TF_TrunkHIncr[Tree] > 0 then 
      # (1-TF_FrondHistFrac[Tree]*TF_DWSufficiency[Tree])*(TF_StressedFrondLength[Tree]- TF_RecentFrondLength[Tree]) else 0 else 0
      tree_df$TF_FrondGrowth <- ifelse(tree_df$TF_AgeofPalm > 0,
                                       ifelse(
                                         tree_df$TF_TrunkHIncr > 0,
                                         (1 - tree_df$TF_FrondHistFrac * tree_df$TF_DWSufficiency) * (
                                           tree_df$TF_StressedFrondLength - tree_df$TF_RecentFrondLength
                                         ),
                                         0
                                       ),
                                       0) 

      # TF_RecentFrondLength[Tree](t) = TF_RecentFrondLength[Tree](t - dt) + (TF_FrondGrowth[Tree]) * dt
      tree_df$TF_RecentFrondLength <- tree_df$TF_RecentFrondLength + tree_df$TF_FrondGrowth

      # TF_PotentialStemD[Tree] = GRAPH(TF_CurrentLeafNo[Tree])
      tree_df$TF_PotentialStemD <- get_TF_PotentialStemD(tree_df$TF_CurrentLeafNo)
      
      # TF_TrunkDiamGrowth[Tree] = if TF_AgeofPalm[Tree] > 0 then if TF_NewLeaf?[Tree]  = 1 then (1-TF_StemDHistFrac[Tree]*TF_DWSufficiency[Tree])*(1-TF_StemDStressFactor[Tree]+TF_StemDStressFactor[Tree]*TF_DWSufficiency[Tree])*(TF_PotentialStemD[Tree]-TF_TrunkDiam[Tree]) else 0 else 0
      tree_df$TF_TrunkDiamGrowth <- ifelse(tree_df$TF_AgeofPalm > 0,
                                           ifelse(
                                             tree_df$TF_NewLeaf_is == 1,
                                             (1 - tree_df$TF_StemDHistFrac *
                                                tree_df$TF_DWSufficiency) *
                                               (
                                                 1 - tree_df$TF_StemDStressFactor + tree_df$TF_StemDStressFactor * tree_df$TF_DWSufficiency
                                               ) *
                                               (tree_df$TF_PotentialStemD -
                                                  tree_df$TF_TrunkDiam),
                                             0
                                           ),
                                           0)
      
      # TF_TrunkDiam[Tree](t) = TF_TrunkDiam[Tree](t - dt) + (TF_TrunkDiamGrowth[Tree]) * dt
      tree_df$TF_TrunkDiam <- tree_df$TF_TrunkDiam + (tree_df$TF_TrunkDiamGrowth) 
      
      # T_HeartWoodDiamInc[Tree] = max((T_SapWoodEqDiam[Tree]^(2/T_SapWoodScalingRule)-T_SapWoodEqDiam[Tree]^2)^0.5 - T_HeartWoodDiam[Tree],0)
      tree_df$T_HeartWoodDiamInc <- pmax((tree_df$T_SapWoodEqDiam^(2/pars$T_par$T_SapWoodScalingRule)-tree_df$T_SapWoodEqDiam^2)^0.5 - tree_df$T_HeartWoodDiam,0)
      
      # T_DiamHeartWoodTDies[Tree] = if T_DiesToday?[Tree] = 1 then T_HeartWoodDiam[Tree] else 0
      tree_df$T_DiamHeartWoodTDies <- ifelse(tree_df$T_DiesToday_is == 1, tree_df$T_HeartWoodDiam, 0)
      
      # T_HeartWoodDiam[Tree](t) = T_HeartWoodDiam[Tree](t - dt) + (T_HeartWoodDiamInc[Tree] - T_DiamHeartWoodTDies[Tree]) * dt
      tree_df$T_HeartWoodDiam <- tree_df$T_HeartWoodDiam + (tree_df$T_HeartWoodDiamInc - tree_df$T_DiamHeartWoodTDies)
      
      # T_StemDiamGrowth[Tree] = if T_ApplyPalm?[Tree] = 1 then TF_TrunkDiamGrowth[Tree] else max(0,(T_SapWoodDiam[Tree]-T_SapWoodEqDiam[Tree]))
      tree_df$T_StemDiamGrowth <- ifelse(tree_df$T_ApplyPalm_is == 1, tree_df$TF_TrunkDiamGrowth, pmax(0, tree_df$T_SapWoodDiam-tree_df$T_SapWoodEqDiam))
      
      # T_StemBefPruningInc[Tree] = if T_HeartWoodAllocAftPruned? = 1 and T_Prun[DW,Tree]>0 then T_SapWoodEqDiam[Tree] else 0
      tree_df$T_StemBefPruningInc <- ifelse( pars$T_par$T_HeartWoodAllocAftPruned_is== 1 & treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_Prun>0, tree_df$T_SapWoodEqDiam, 0)
      
      # T_DiamSapWoodTreeDies[Tree] = if T_DiesToday?[Tree] = 1 then T_SapWoodEqDiam[Tree] else 0
      tree_df$T_DiamSapWoodTreeDies <- ifelse(tree_df$T_DiesToday_is == 1, tree_df$T_SapWoodEqDiam, 0)
      
      # T_SapWoodEqDiam[Tree](t) = T_SapWoodEqDiam[Tree](t - dt) + (T_StemDiamGrowth[Tree] - T_StemBefPruningInc[Tree] - T_DiamSapWoodTreeDies[Tree]) * dt
      tree_df$T_SapWoodEqDiam <- tree_df$T_SapWoodEqDiam + (tree_df$T_StemDiamGrowth - tree_df$T_StemBefPruningInc - tree_df$T_DiamSapWoodTreeDies) 
      
      # T_CanBiomSlashed[PlantComp,Tree] = if time = int(S&B_SlashTime[Tree]) or S&B_FireTime? = 1 or T_DiesToday?[Tree] = 1  then T_LfTwig[PlantComp,Tree]/dt else 0
      treepcomp_df$T_CanBiomSlashed <- ifelse(
        time == floor(treepcomp_df$SB_SlashTime) |
          SB_FireTime_is == 1 | treepcomp_df$T_DiesToday_is == 1,
        treepcomp_df$T_LfTwig,
        0
      )
      
      # T_FruitSlashed[PlantComp,Tree] = if time = int(S&B_SlashTime[Tree]) then T_Fruit[PlantComp,Tree]/dt else 0
      treepcomp_df$T_FruitSlashed <- ifelse(time == floor(treepcomp_df$SB_SlashTime), treepcomp_df$T_Fruit, 0)
      
      # T_BiomassSlashed[PlantComp] = ARRAYSUM(T_CanBiomSlashed[PlantComp,*])+ARRAYSUM(T_FruitSlashed[PlantComp,*])
      pcomp_df$T_BiomassSlashed <- aggregate(treepcomp_df["T_CanBiomSlashed"], treepcomp_df["PlantComp"], sum)$T_CanBiomSlashed+
        aggregate(treepcomp_df["T_FruitSlashed"], treepcomp_df["PlantComp"], sum)$T_FruitSlashed
  
      # S&B_SlashVegetation[Zone,PlantComp] =  T_BiomassSlashed[PlantComp]
      zonepcomp_df$SB_SlashVegetation <-  rep(pcomp_df$T_BiomassSlashed, each = nzone)
      
      # S&B_AerosolFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
      zone_df$SB_AerosolFrac <- get_SB_AerosolFrac(zone_df$SB_FireTempIncSurf)
      
      # S&B_NecroBurnFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
      zone_df$SB_NecroBurnFrac <- get_SB_NecroBurnFrac(zone_df$SB_FireTempIncSurf)
      
      zonepcomp_df$SB_AerosolFrac <- rep(zone_df$SB_AerosolFrac, nrow(pcomp_df))
      zonepcomp_df$SB_NecroBurnFrac <- rep(zone_df$SB_NecroBurnFrac, nrow(pcomp_df))
      zonepcomp_df$SB_DW_is <- rep(pcomp_df$SB_DW_is, each = nzone)
      # S&B_NecromassBurn[Zone,PlantComp] = S&B_DW?[PlantComp]*S&B_FineNecromass[Zone,PlantComp]*S&B_NecroBurnFrac[Zone]*(1-S&B_AerosolFrac[Zone])
      zonepcomp_df$SB_NecromassBurn <- zonepcomp_df$SB_DW_is * zonepcomp_df$SB_FineNecromass* zonepcomp_df$SB_NecroBurnFrac*(1- zonepcomp_df$SB_AerosolFrac)
      
      # S&B_NVolatFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
      # S&B_PVolatFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
      # S&B_NutVolatFrac[Zn1,DW] = 0*(S&B_NVolatFrac[Zn1]+S&B_PVolatFrac[Zn1])
      # S&B_NutVolatFrac[Zn1,N] = S&B_NVolatFrac[Zn1]+0*S&B_PVolatFrac[Zn1]
      # S&B_NutVolatFrac[Zn1,P] = 0*S&B_NVolatFrac[Zn1]+S&B_PVolatFrac[Zn1]
      # S&B_NutVolatFrac[Zn2,DW] = 0*(S&B_NVolatFrac[Zn2]+S&B_PVolatFrac[Zn2])
      # S&B_NutVolatFrac[Zn2,N] = S&B_NVolatFrac[Zn2]+0*S&B_PVolatFrac[Zn2]
      # S&B_NutVolatFrac[Zn2,P] = 0*S&B_NVolatFrac[Zn2]+S&B_PVolatFrac[Zn2]
      # S&B_NutVolatFrac[Zn3,DW] = 0*(S&B_NVolatFrac[Zn3]+S&B_PVolatFrac[Zn3])
      # S&B_NutVolatFrac[Zn3,N] = S&B_NVolatFrac[Zn3]+0*S&B_PVolatFrac[Zn3]
      # S&B_NutVolatFrac[Zn3,P] = 0*S&B_NVolatFrac[Zn3]+S&B_PVolatFrac[Zn3]
      # S&B_NutVolatFrac[Zn4,DW] = 0*(S&B_NVolatFrac[Zn4]+S&B_PVolatFrac[Zn4])
      # S&B_NutVolatFrac[Zn4,N] = S&B_NVolatFrac[Zn4]+0*S&B_PVolatFrac[Zn4]
      # S&B_NutVolatFrac[Zn4,P] = 0*S&B_NVolatFrac[Zn4]+S&B_PVolatFrac[Zn4]
      
      zonepcomp_df$SB_NutVolatFrac <- 0
      zonepcomp_df[zonepcomp_df$PlantComp == "N",]$SB_NutVolatFrac <- get_SB_NVolatFrac(zone_df$SB_FireTempIncSurf)
      zonepcomp_df[zonepcomp_df$PlantComp == "P",]$SB_NutVolatFrac <- get_SB_PVolatFrac(zone_df$SB_FireTempIncSurf)

      # S&B_NecroFNutVolat[Zone,PlantComp] = (1-S&B_DW?[PlantComp])*S&B_NutVolatFrac[Zone,PlantComp]*S&B_FineNecromass[Zone,PlantComp]*S&B_NecroBurnFrac[Zone]
      zonepcomp_df$SB_NecroFNutVolat <- (1-zonepcomp_df$SB_DW_is)* zonepcomp_df$SB_NutVolatFrac* zonepcomp_df$SB_FineNecromass* zonepcomp_df$SB_NecroBurnFrac
      
      # S&B_MinNutRele[Zone,PlantComp] = (1-S&B_DW?[PlantComp])*(1-S&B_NutVolatFrac[Zone,PlantComp])*S&B_FineNecromass[Zone,PlantComp]*S&B_NecroBurnFrac[Zone]
      zonepcomp_df$SB_MinNutRele <- (1- zonepcomp_df$SB_DW_is)*(1- zonepcomp_df$SB_NutVolatFrac)* zonepcomp_df$SB_FineNecromass* zonepcomp_df$SB_NecroBurnFrac
      
      # S&B_PileSum = AF_ZoneFrac[Zn1]*S&B_PileUpWgt[Zn1]+AF_ZoneFrac[Zn2]*S&B_PileUpWgt[Zn2]+AF_ZoneFrac[Zn3]*S&B_PileUpWgt[Zn3]+AF_ZoneFrac[Zn4]*S&B_PileUpWgt[Zn4]
      SB_PileSum <- sum(zone_df$AF_ZoneFrac* zone_df$SB_PileUpWgt)
      
      # S&B_RelPileUp[Zone] = if S&B_PileSum > 0 then S&B_PileUpWgt[Zone]/S&B_PileSum else 0
      zone_df$SB_RelPileUp <- ifelse(SB_PileSum > 0, zone_df$SB_PileUpWgt/SB_PileSum, 0)
      
      # S&B_NecromLitTransf[Zone,PlantComp] = S&B_DailyNecromLitTransfer*S&B_FineNecromass[Zone,PlantComp]
      zonepcomp_df$SB_NecromLitTransf <- pars$SB_par$SB_DailyNecromLitTransfer* zonepcomp_df$SB_FineNecromass
      
      # BC_DWFineNecroM[PlantComp] = AF_ZoneFrac[Zn1]* (S&B_FineNecromass[Zn1,PlantComp])+ AF_ZoneFrac[Zn2]* (S&B_FineNecromass[Zn2,PlantComp])+ AF_ZoneFrac[Zn3]* (S&B_FineNecromass[Zn3,PlantComp])+ AF_ZoneFrac[Zn4]* (S&B_FineNecromass[Zn4,PlantComp])
      
      zonepcomp_df$BC_DWFineNecroM_a <- rep(zone_df$AF_ZoneFrac, nrow(pcomp_df))  * zonepcomp_df$SB_FineNecromass
      
      pcomp_df$BC_DWFineNecroM <- aggregate(zonepcomp_df["BC_DWFineNecroM_a"], zonepcomp_df["PlantComp"], sum)$BC_DWFineNecroM_a 
      
      zonepcomp_df$SB_RelPileUp <- rep(zone_df$SB_RelPileUp, nrow(pcomp_df))
      zonepcomp_df$BC_DWFineNecroM <- rep(pcomp_df$BC_DWFineNecroM, each = nzone)
      # S&B_SPileUp[Zone,PlantComp] = if S&B_PileUpT? = 1 then S&B_PileUpFrac*(S&B_FineNecromass[Zone,PlantComp]-S&B_RelPileUp[Zone]*BC_DWFineNecroM[PlantComp]) else 0
      zonepcomp_df$SB_SPileUp <- ifelse(SB_PileUpT_is == 1,  pars$SB_par$SB_PileUpFrac*( zonepcomp_df$SB_FineNecromass- 
                                                                                           zonepcomp_df$SB_RelPileUp*zonepcomp_df$BC_DWFineNecroM), 0)
      # S&B_HazeFromSlash[Zone,PlantComp] = S&B_DW?[PlantComp]*S&B_FineNecromass[Zone,PlantComp]*S&B_NecroBurnFrac[Zone]*(S&B_AerosolFrac[Zone])
      zonepcomp_df$SB_HazeFromSlash <- zonepcomp_df$SB_DW_is* zonepcomp_df$SB_FineNecromass* zonepcomp_df$SB_NecroBurnFrac*( zonepcomp_df$SB_AerosolFrac)

      # S&B_FineNecromass[Zone,PlantComp](t) = S&B_FineNecromass[Zone,PlantComp](t - dt) + (S&B_SlashVegetation[Zone,PlantComp] - S&B_NecromassBurn[Zone,PlantComp] - S&B_NecroFNutVolat[Zone,PlantComp] - S&B_MinNutRele[Zone,PlantComp] - S&B_NecromLitTransf[Zone,PlantComp] - S&B_SPileUp[Zone,PlantComp] - S&B_HazeFromSlash[Zone,PlantComp]) * dt
      zonepcomp_df$SB_FineNecromass <- zonepcomp_df$SB_FineNecromass + (
        zonepcomp_df$SB_SlashVegetation - zonepcomp_df$SB_NecromassBurn - zonepcomp_df$SB_NecroFNutVolat -
          zonepcomp_df$SB_MinNutRele - zonepcomp_df$SB_NecromLitTransf - zonepcomp_df$SB_SPileUp - zonepcomp_df$SB_HazeFromSlash
      ) 

      # T_PrunWtAct[Zone,Tree] = if T_PrunWeighTot[Tree]>0 then (T_PrunWeight[Zone,Tree]/ARRAYSUM(T_PrunWeight[*,Tree]))/AF_ZoneFrac[Zone] else 0
      zonetree_df$T_PrunWtAct <- ifelse(
        zonetree_df$T_PrunWeighTot > 0,
        (zonetree_df$T_PrunWeight / zonetree_df$T_PrunWeigh_sum) / zonetree_df$AF_ZoneFrac,
        0
      )
      
      tree_df$T_LifallWeight_sum <- aggregate(zonetree_df["T_LifallWeight"], zonetree_df["tree_id"], sum)$T_LifallWeight
      zonetree_df$T_LifallWeight_sum <- rep(tree_df$T_LifallWeight_sum, each = nzone)
      
      # T_LifallWtAct[Zone,Tree] = if T_LifallWeightTot[Zone,Tree] > 0 then (T_LifallWeight[Zone,Tree]/ARRAYSUM(T_LifallWeight[*,Tree]))/AF_ZoneFrac[Zone] else 0
      zonetree_df$T_LifallWtAct <- ifelse(zonetree_df$T_LifallWeightTot > 0, (zonetree_df$T_LifallWeight/zonetree_df$T_LifallWeight_sum)/zonetree_df$AF_ZoneFrac, 0)
      
      tree_dw_df <- treepcomp_df[treepcomp_df$PlantComp == "DW", c("T_CanBiomSlashed", "T_Prun", "T_LifallInc")]
      zonetree_dw_df <- tree_dw_df[rep(seq_len(nrow(tree_dw_df)), each = nzone), ]
      
      # T_LfTwMulch[Zone,Tree] = (T_CanBiomSlashed[DW,Tree] + T_Prun[DW,Tree])*T_PrunWtAct[Zone,Tree]+T_LifallInc[DW,Tree]*T_LifallWtAct[Zone,Tree]
      zonetree_df$T_LfTwMulch <- (zonetree_dw_df$T_CanBiomSlashed + zonetree_dw_df$T_Prun)* zonetree_df$T_PrunWtAct+ zonetree_dw_df$T_LifallInc* zonetree_df$T_LifallWtAct
      
      zone_df$T_LfTwMulch_sum <- aggregate(zonetree_df["T_LfTwMulch"], zonetree_df["zone"], sum)$T_LfTwMulch
      zonetree_df$T_LAINecrCh_a <- zonetree_df$T_LfTwMulch * rep(tree_df$T_LWR, each = nzone) * rep(tree_df$T_SLA, each = nzone)
      zone_df$T_LAINecrCh_a_sum <- aggregate(zonetree_df["T_LAINecrCh_a"], zonetree_df["zone"], sum)$T_LAINecrCh_a
      zone_df$C_StLeaveMulch_DW <- zonepcomp_df[zonepcomp_df$PlantComp == "DW",]$C_StLeaveMulch
      # T_LAINecrCh[Zone] = if (ARRAYSUM(T_LfTwMulch[Zone,*]) + C_StLeaveMulch[Zone,DW]) > 0 then - Mc_LAIperNecmss[Zone] + (+Mc_Struc[Zone]*Mc_LAIperNecmss[Zone]+ Cq_CLWRCurr[Zone]*Cq_CSLACurr[Zone]*C_StLeaveMulch[Zone,DW]+ T_LfTwMulch[Zone,Sp1]*T_LWR[Sp1]*T_SLA[Sp1]+ T_LfTwMulch[Zone,Sp2]*T_LWR[Sp2]*T_SLA[Sp2]+ T_LfTwMulch[Zone,Sp3]*T_LWR[Sp3]*T_SLA[Sp3])/((ARRAYSUM(T_LfTwMulch[Zone,*]) + C_StLeaveMulch[Zone,DW] + Mc_Struc[Zone])) else 0
      zone_df$T_LAINecrCh <- ifelse(
        (zone_df$T_LfTwMulch_sum + zone_df$C_StLeaveMulch_DW) > 0,
        -zone_df$Mc_LAIperNecmss + (
          zone_df$Mc_Struc * zone_df$Mc_LAIperNecmss + zone_df$Cq_CLWRCurr * zone_df$Cq_CSLACurr *
            zone_df$C_StLeaveMulch_DW +
            zone_df$T_LAINecrCh_a_sum
        ) / (
          zone_df$T_LfTwMulch_sum + zone_df$C_StLeaveMulch_DW + zone_df$Mc_Struc
        ),
        0
      )
      
      # Mc_LAIperNecmss[Zone](t) = Mc_LAIperNecmss[Zone](t - dt) + (T_LAINecrCh[Zone]) * dt
      zone_df$Mc_LAIperNecmss <- zone_df$Mc_LAIperNecmss + (zone_df$T_LAINecrCh)
      
      # Rain_CanopyWater[Zone](t) = Rain_CanopyWater[Zone](t - dt) + (Rain_Interception[Zone] - Rain_InterceptEvap[Zone]) * dt
      zone_df$Rain_CanopyWater <- zone_df$Rain_CanopyWater + (zone_df$Rain_Interception - zone_df$Rain_InterceptEvap)
      
      
      # LF_UphillGWIncr = AF_PlotNumberUphill*(AF_ZoneFrac[Zn1]*W_V4Drain[Zn1]+AF_ZoneFrac[Zn2]*W_V4Drain[Zn2])/(AF_ZoneFrac[Zn1]+AF_ZoneFrac[Zn2])
      zone_df$W_V4Drain_frac <- zone_df$AF_ZoneFrac * zonelayer_df[zonelayer_df$layer == 4, ]$W_VDrain
      LF_UphillGWIncr <- pars$AF_par$AF_PlotNumberUphill * (zone_df[zone_df$zone == 1, ]$W_V4Drain_frac + zone_df[zone_df$zone == 2, ]$W_V4Drain_frac) /
        (zone_df[zone_df$zone == 1, ]$AF_ZoneFrac + zone_df[zone_df$zone == 2, ]$AF_ZoneFrac)
      
      # LF_UphillGWRelease = LF_UphillGWStore*LF_GW_ReleaseFraction
      LF_UphillGWRelease <- pars$LF_par$LF_UphillGWStore* pars$LF_par$LF_GW_ReleaseFraction
      
      # LF_UphillGWStore(t) = LF_UphillGWStore(t - dt) + (LF_UphillGWIncr - LF_UphillGWRelease) * dt
      pars$LF_par$LF_UphillGWStore <- pars$LF_par$LF_UphillGWStore + (LF_UphillGWIncr - LF_UphillGWRelease)
      
      tree_df$T_NPosgro_N <- treenut_df[treenut_df$SlNut == "N",]$T_NPosgro
      tree_df$T_NPosgro_P <- treenut_df[treenut_df$SlNut == "P",]$T_NPosgro
      # RT3_CurrentStress[Tree] = if TW_Posgro[Tree] < T_NPosgro[N,Tree] and TW_Posgro[Tree] < T_NPosgro[P,Tree] then 1 else if T_NPosgro[N,Tree] < T_NPosgro[P,Tree] and T_NPosgro[N,Tree] < TW_Posgro[Tree] then 2 else 3
      tree_df$RT3_CurrentStress <- ifelse(
        tree_df$TW_Posgro < tree_df$T_NPosgro_N &
          tree_df$TW_Posgro < tree_df$T_NPosgro_P,
        1,
        ifelse(
          tree_df$T_NPosgro_N < tree_df$T_NPosgro_P &
            tree_df$T_NPosgro_N < tree_df$TW_Posgro,
          2,
          3
        )
      )
      # RT3_LrvDepth1[Zone,Tree] = AF_DepthAct1[Zone]*100*Rt_TLrv1[Zone,Tree]
      # RT3_LrvDepth2[Zone,Tree] = AF_Depth2[Zone]*100*Rt_TLrv2[Zone,Tree]
      # RT3_LrvDepth3[Zone,Tree] = AF_Depth3[Zone]*100*Rt_TLrv3[Zone,Tree]
      # RT3_LrvDepth4[Zone,Tree] = AF_Depth4[Zone]*100*Rt_TLrv4[Zone,Tree]
      zonelayertree_df$RT3_LrvDepth <- zonelayertree_df$AF_Depth*100* zonelayertree_df$Rt_TLrv
      
      # RT3_AvgWUptperRoot[Tree] = IF(Rt_TField[Tree])>0 then TW_UptTot[Tree]/Rt_TField[Tree] else 0
      tree_df$RT3_AvgWUptperRoot <- ifelse(tree_df$Rt_TField>0, tree_df$TW_UptTot/tree_df$Rt_TField, 0)
      
      # RT3_L1W_RelUptperLrv[Zn1,Sp1] = if RT3_LrvDepth1[Zn1,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt1[Zn1]/RT3_LrvDepth1[Zn1,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt1[Zn1]+W_T3Upt1[Zn1])
      # RT3_L1W_RelUptperLrv[Zn1,Sp2] = if RT3_LrvDepth1[Zn1,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt1[Zn1]/RT3_LrvDepth1[Zn1,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt1[Zn1]+W_T3Upt1[Zn1])
      # RT3_L1W_RelUptperLrv[Zn1,Sp3] = if RT3_LrvDepth1[Zn1,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>1 then (W_T3Upt1[Zn1]/RT3_LrvDepth1[Zn1,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt1[Zn1]+W_T2Upt1[Zn1])
      # RT3_L1W_RelUptperLrv[Zn2,Sp1] = if RT3_LrvDepth1[Zn2,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt1[Zn2]/RT3_LrvDepth1[Zn2,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt1[Zn2]+W_T3Upt1[Zn2])
      # RT3_L1W_RelUptperLrv[Zn2,Sp2] = if RT3_LrvDepth1[Zn2,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt1[Zn2]/RT3_LrvDepth1[Zn2,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt1[Zn2]+W_T3Upt1[Zn2])
      # RT3_L1W_RelUptperLrv[Zn2,Sp3] = if RT3_LrvDepth1[Zn2,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt1[Zn2]/RT3_LrvDepth1[Zn2,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt1[Zn2]+W_T2Upt1[Zn2])
      # RT3_L1W_RelUptperLrv[Zn3,Sp1] = if RT3_LrvDepth1[Zn3,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt1[Zn3]/RT3_LrvDepth1[Zn3,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt1[Zn3]+W_T3Upt1[Zn3])
      # RT3_L1W_RelUptperLrv[Zn3,Sp2] = if RT3_LrvDepth1[Zn3,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt1[Zn3]/RT3_LrvDepth1[Zn3,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt1[Zn3]+W_T3Upt1[Zn3])
      # RT3_L1W_RelUptperLrv[Zn3,Sp3] = if RT3_LrvDepth1[Zn3,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt1[Zn3]/RT3_LrvDepth1[Zn3,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt1[Zn3]+W_T2Upt1[Zn3])
      # RT3_L1W_RelUptperLrv[Zn4,Sp1] = if RT3_LrvDepth1[Zn4,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt1[Zn4]/RT3_LrvDepth1[Zn4,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt1[Zn4]+W_T3Upt1[Zn4])
      # RT3_L1W_RelUptperLrv[Zn4,Sp2] = if RT3_LrvDepth1[Zn4,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt1[Zn4]/RT3_LrvDepth1[Zn4,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt1[Zn4]+W_T3Upt1[Zn4])
      # RT3_L1W_RelUptperLrv[Zn4,Sp3] = if RT3_LrvDepth1[Zn4,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt1[Zn4]/RT3_LrvDepth1[Zn4,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt1[Zn4]+W_T2Upt1[Zn4])
      # 
      # RT3_L2W_RelUptperLrv[Zn1,Sp1] = if RT3_LrvDepth2[Zn1,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt2[Zn1]/RT3_LrvDepth2[Zn1,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt2[Zn1]+W_T3Upt2[Zn1])
      # RT3_L2W_RelUptperLrv[Zn1,Sp2] = if RT3_LrvDepth2[Zn1,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt2[Zn1]/RT3_LrvDepth2[Zn1,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt2[Zn1]+W_T3Upt2[Zn1])
      # RT3_L2W_RelUptperLrv[Zn1,Sp3] = if RT3_LrvDepth2[Zn1,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt2[Zn1]/RT3_LrvDepth2[Zn1,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt2[Zn1]+W_T2Upt2[Zn1])
      # RT3_L2W_RelUptperLrv[Zn2,Sp1] = if RT3_LrvDepth2[Zn2,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt2[Zn2]/RT3_LrvDepth2[Zn2,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt2[Zn2]+W_T3Upt2[Zn2])
      # RT3_L2W_RelUptperLrv[Zn2,Sp2] = if RT3_LrvDepth2[Zn2,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt2[Zn2]/RT3_LrvDepth2[Zn2,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt2[Zn2]+W_T3Upt2[Zn2])
      # RT3_L2W_RelUptperLrv[Zn2,Sp3] = if RT3_LrvDepth2[Zn2,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt2[Zn2]/RT3_LrvDepth2[Zn2,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt2[Zn2]+W_T2Upt2[Zn2])
      # RT3_L2W_RelUptperLrv[Zn3,Sp1] = if RT3_LrvDepth2[Zn3,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt2[Zn3]/RT3_LrvDepth2[Zn3,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt2[Zn3]+W_T3Upt2[Zn3])
      # RT3_L2W_RelUptperLrv[Zn3,Sp2] = if RT3_LrvDepth2[Zn3,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt2[Zn3]/RT3_LrvDepth2[Zn3,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt2[Zn3]+W_T3Upt2[Zn3])
      # RT3_L2W_RelUptperLrv[Zn3,Sp3] = if RT3_LrvDepth2[Zn3,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt2[Zn3]/RT3_LrvDepth2[Zn3,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt2[Zn3]+W_T2Upt2[Zn3])
      # RT3_L2W_RelUptperLrv[Zn4,Sp1] = if RT3_LrvDepth2[Zn4,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt2[Zn4]/RT3_LrvDepth2[Zn4,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt2[Zn4]+W_T3Upt2[Zn4])
      # RT3_L2W_RelUptperLrv[Zn4,Sp2] = if RT3_LrvDepth2[Zn4,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt2[Zn4]/RT3_LrvDepth2[Zn4,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt2[Zn4]+W_T3Upt2[Zn4])
      # RT3_L2W_RelUptperLrv[Zn4,Sp3] = if RT3_LrvDepth2[Zn4,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt2[Zn4]/RT3_LrvDepth2[Zn4,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt2[Zn4]+W_T2Upt2[Zn4])
      # 
      # RT3_L3W_RelUptperLrv[Zn1,Sp1] = if RT3_LrvDepth3[Zn1,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt3[Zn1]/RT3_LrvDepth3[Zn1,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt3[Zn1]+W_T3Upt3[Zn1])
      # RT3_L3W_RelUptperLrv[Zn1,Sp2] = if RT3_LrvDepth3[Zn1,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt3[Zn1]/RT3_LrvDepth3[Zn1,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt3[Zn1]+W_T3Upt3[Zn1])
      # RT3_L3W_RelUptperLrv[Zn1,Sp3] = if RT3_LrvDepth3[Zn1,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt3[Zn1]/RT3_LrvDepth3[Zn1,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt3[Zn1]+W_T2Upt3[Zn1])
      # RT3_L3W_RelUptperLrv[Zn2,Sp1] = if RT3_LrvDepth3[Zn2,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt3[Zn2]/RT3_LrvDepth3[Zn2,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt3[Zn2]+W_T3Upt3[Zn2])
      # RT3_L3W_RelUptperLrv[Zn2,Sp2] = if RT3_LrvDepth3[Zn2,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt3[Zn2]/RT3_LrvDepth3[Zn2,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt3[Zn2]+W_T3Upt3[Zn2])
      # RT3_L3W_RelUptperLrv[Zn2,Sp3] = if RT3_LrvDepth3[Zn2,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt3[Zn2]/RT3_LrvDepth3[Zn2,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt3[Zn2]+W_T2Upt3[Zn2])
      # RT3_L3W_RelUptperLrv[Zn3,Sp1] = if RT3_LrvDepth3[Zn3,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt3[Zn3]/RT3_LrvDepth3[Zn3,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt3[Zn3]+W_T3Upt3[Zn3])
      # RT3_L3W_RelUptperLrv[Zn3,Sp2] = if RT3_LrvDepth3[Zn3,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt3[Zn3]/RT3_LrvDepth3[Zn3,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt3[Zn3]+W_T3Upt3[Zn3])
      # RT3_L3W_RelUptperLrv[Zn3,Sp3] = if RT3_LrvDepth3[Zn3,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt3[Zn3]/RT3_LrvDepth3[Zn3,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt3[Zn3]+W_T2Upt3[Zn3])
      # RT3_L3W_RelUptperLrv[Zn4,Sp1] = if RT3_LrvDepth3[Zn4,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt3[Zn4]/RT3_LrvDepth3[Zn4,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt3[Zn4]+W_T3Upt3[Zn4])
      # RT3_L3W_RelUptperLrv[Zn4,Sp2] = if RT3_LrvDepth3[Zn4,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt3[Zn4]/RT3_LrvDepth3[Zn4,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt3[Zn4]+W_T3Upt3[Zn4])
      # RT3_L3W_RelUptperLrv[Zn4,Sp3] = if RT3_LrvDepth3[Zn4,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt3[Zn4]/RT3_LrvDepth3[Zn4,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt3[Zn4]+W_T2Upt3[Zn4])
      # 
      # RT3_L4W_RelUptperLrv[Zn1,Sp1] = if RT3_LrvDepth4[Zn1,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt4[Zn1]/RT3_LrvDepth4[Zn1,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt4[Zn1]+W_T3Upt4[Zn1])
      # RT3_L4W_RelUptperLrv[Zn1,Sp2] = if RT3_LrvDepth4[Zn1,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt4[Zn1]/RT3_LrvDepth4[Zn1,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt4[Zn1]+W_T3Upt4[Zn1])
      # RT3_L4W_RelUptperLrv[Zn1,Sp3] = if RT3_LrvDepth4[Zn1,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt4[Zn1]/RT3_LrvDepth4[Zn1,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt4[Zn1]+W_T2Upt4[Zn1])
      # RT3_L4W_RelUptperLrv[Zn2,Sp1] = if RT3_LrvDepth4[Zn2,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt4[Zn2]/RT3_LrvDepth4[Zn2,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt4[Zn2]+W_T3Upt4[Zn2])
      # RT3_L4W_RelUptperLrv[Zn2,Sp2] = if RT3_LrvDepth4[Zn2,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt4[Zn2]/RT3_LrvDepth4[Zn2,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt4[Zn2]+W_T3Upt4[Zn2])
      # RT3_L4W_RelUptperLrv[Zn2,Sp3] = if RT3_LrvDepth4[Zn2,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt4[Zn2]/RT3_LrvDepth4[Zn2,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt4[Zn2]+W_T2Upt4[Zn2])
      # RT3_L4W_RelUptperLrv[Zn3,Sp1] = if RT3_LrvDepth4[Zn3,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt4[Zn3]/RT3_LrvDepth4[Zn3,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt4[Zn3]+W_T3Upt4[Zn3])
      # RT3_L4W_RelUptperLrv[Zn3,Sp2] = if RT3_LrvDepth4[Zn3,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt4[Zn3]/RT3_LrvDepth4[Zn3,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt4[Zn3]+W_T3Upt4[Zn3])
      # RT3_L4W_RelUptperLrv[Zn3,Sp3] = if RT3_LrvDepth4[Zn3,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt4[Zn3]/RT3_LrvDepth4[Zn3,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt4[Zn3]+W_T2Upt4[Zn3])
      # RT3_L4W_RelUptperLrv[Zn4,Sp1] = if RT3_LrvDepth4[Zn4,Sp1]>0 and RT3_AvgWUptperRoot[Sp1]>0 then (W_T1Upt4[Zn4]/RT3_LrvDepth4[Zn4,Sp1])/RT3_AvgWUptperRoot[Sp1] else 0*(W_T2Upt4[Zn4]+W_T3Upt4[Zn4])
      # RT3_L4W_RelUptperLrv[Zn4,Sp2] = if RT3_LrvDepth4[Zn4,Sp2]>0 and RT3_AvgWUptperRoot[Sp2]>0 then (W_T2Upt4[Zn4]/RT3_LrvDepth4[Zn4,Sp2])/RT3_AvgWUptperRoot[Sp2] else 0*(W_T1Upt4[Zn4]+W_T3Upt4[Zn4])
      # RT3_L4W_RelUptperLrv[Zn4,Sp3] = if RT3_LrvDepth4[Zn4,Sp3]>0 and RT3_AvgWUptperRoot[Sp3]>0 then (W_T3Upt4[Zn4]/RT3_LrvDepth4[Zn4,Sp3])/RT3_AvgWUptperRoot[Sp3] else 0*(W_T1Upt4[Zn4]+W_T2Upt4[Zn4])
      
      zonelayertree_df$RT3_AvgWUptperRoot <- rep(tree_df$RT3_AvgWUptperRoot, each = nzone * nlayer)
      zonelayertree_df$W_Upt
      zonelayertree_df$RT3_LW_RelUptperLrv <- ifelse(
        zonelayertree_df$RT3_LrvDepth > 0 &
          zonelayertree_df$RT3_AvgWUptperRoot > 0,
        (zonelayertree_df$W_Upt /
           zonelayertree_df$RT3_LrvDepth) / zonelayertree_df$RT3_AvgWUptperRoot,
        0
      )
      
      # Rt_RhoTL1[Zone,Tree] = if Rt_TLrv1[Zone,Tree]> 0 then 1/(Rt_TDiam[Tree]*.5*SQRT(PI*(Rt_TLrv1[Zone,Tree]))) else Rt_StopGap
      # Rt_RhoTL2[Zone,Tree] = if Rt_TLrv2[Zone,Tree]> 0 then 1/(Rt_TDiam[Tree]*.5*SQRT(PI*(Rt_TLrv2[Zone,Tree]))) else Rt_StopGap
      # Rt_RhoTL3[Zone,Tree] = if Rt_TLrv3[Zone,Tree]> 0 then 1/(Rt_TDiam[Tree]*.5*SQRT(PI*(Rt_TLrv3[Zone,Tree]))) else Rt_StopGap
      # Rt_RhoTL4[Zone,Tree] = if Rt_TLrv4[Zone,Tree]> 0 then 1/(Rt_TDiam[Tree]*.5*SQRT(PI*(Rt_TLrv4[Zone,Tree]))) else Rt_StopGap
      zonelayertree_df$Rt_RhoTL <- ifelse(
        zonelayertree_df$Rt_TLrv > 0,
        1 / (
          zonelayertree_df$Rt_TDiam * .5 * sqrt(pi * zonelayertree_df$Rt_TLrv)
        ),
        pars$Rt_par$Rt_StopGap
      )
      
      # Rt_T_G1[Zone,Tree] = if Rt_RhoTL1[Zone,Tree] <> Rt_StopGap then (Rt_RhoTL1[Zone,Tree]^2-1)/(0.5*(((1-(3*Rt_RhoTL1[Zone,Tree]^2))/4)+((Rt_RhoTL1[Zone,Tree]^4*LOGN(Rt_RhoTL1[Zone,Tree]))/(Rt_RhoTL1[Zone,Tree]^2-1)))) else 0
      # Rt_T_G2[Zone,Tree] = if Rt_RhoTL2[Zone,Tree] <> Rt_StopGap then (Rt_RhoTL2[Zone,Tree]^2-1)/(0.5*(((1-(3*Rt_RhoTL2[Zone,Tree]^2))/4)+((Rt_RhoTL2[Zone,Tree]^4*LOGN(Rt_RhoTL2[Zone,Tree]))/(Rt_RhoTL2[Zone,Tree]^2-1)))) else 0
      # Rt_T_G3[Zone,Tree] = if Rt_RhoTL3[Zone,Tree] <> Rt_StopGap then (Rt_RhoTL3[Zone,Tree]^2-1)/(0.5*(((1-(3*Rt_RhoTL3[Zone,Tree]^2))/4)+((Rt_RhoTL3[Zone,Tree]^4*LOGN(Rt_RhoTL3[Zone,Tree]))/(Rt_RhoTL3[Zone,Tree]^2-1)))) else 0
      # Rt_T_G4[Zone,Tree] = if Rt_RhoTL4[Zone,Tree] <> Rt_StopGap then (Rt_RhoTL4[Zone,Tree]^2-1)/(0.5*(((1-(3*Rt_RhoTL4[Zone,Tree]^2))/4)+((Rt_RhoTL4[Zone,Tree]^4*LOGN(Rt_RhoTL4[Zone,Tree]))/(Rt_RhoTL4[Zone,Tree]^2-1)))) else 0
      
      zonelayertree_df$Rt_T_G <- ifelse(
        zonelayertree_df$Rt_RhoTL != pars$Rt_par$Rt_StopGap,
        (zonelayertree_df$Rt_RhoTL^2 -
           1) / (0.5 * (((
             1 - (3 * zonelayertree_df$Rt_RhoTL^2)
           ) / 4) + ((zonelayertree_df$Rt_RhoTL^4 * log(zonelayertree_df$Rt_RhoTL)) /
                       (zonelayertree_df$Rt_RhoTL^2 - 1)
           ))),
        0
      )
      
      # N_Diff1[Zone,SlNut] = N_DiffCoef[SlNut]*W_Theta1[Zone]*MAX(0.1*W_Theta1[Zone],(1.5*W_Theta1[Zone]-0.11))
      # N_Diff2[Zone,SlNut] = N_DiffCoef[SlNut]*W_Theta2[Zone]*MAX(0.1*W_Theta2[Zone],(1.5*W_Theta2[Zone]-0.11))
      # N_Diff3[Zone,SlNut] = N_DiffCoef[SlNut]*W_Theta3[Zone]*MAX(0.1*W_Theta3[Zone],(1.5*W_Theta3[Zone]-0.11))
      # N_Diff4[Zone,SlNut] = N_DiffCoef[SlNut]*W_Theta4[Zone]*MAX(0.1*W_Theta4[Zone],(1.5*W_Theta4[Zone]-0.11))
      zonelayernut_df$N_DiffCoef <- rep(nut_df$N_DiffCoef, each = nzone*nlayer)
      zonelayernut_df$W_Theta <- rep(zonelayer_df$W_Theta, nrow(nut_df))
      zonelayernut_df$N_Diff <- zonelayernut_df$N_DiffCoef* zonelayernut_df$W_Theta*pmax(0.1* zonelayernut_df$W_Theta,(1.5* zonelayernut_df$W_Theta-0.11))

      zonenut_df$Mn_FertDissFrac <- rep(nut_df$Mn_FertDissFrac, each = nzone)
      # Mn_FertDissF[Zone,SlNut] = Mn_FertOnSoil[Zone,SlNut]*Mn_FertDissFrac[SlNut]
      zonenut_df$Mn_FertDissF <- zonenut_df$Mn_FertOnSoil* zonenut_df$Mn_FertDissFrac
      
      # W_RelDrain1[Zone] = if AF_DepthAct1[Zone] > 0 then W_Drain1[Zone]/(W_FieldCap1[Zone]*AF_DepthAct1[Zone]*1000) else 0
      # W_RelDrain2[Zone] = if AF_Depth2[Zone] > 0 then W_Drain2[Zone]/(W_FieldCap2[Zone]*AF_Depth2[Zone]*1000) else 0
      # W_RelDrain3[Zone] = if AF_Depth3[Zone] > 0 then W_Drain3[Zone]/(W_FieldCap3[Zone]*AF_Depth3[Zone]*1000)else 0
      # W_RelDrain4[Zone] = if AF_Depth4[Zone] > 0 then W_Drain4[Zone]/(W_FieldCap4[Zone]*AF_Depth4[Zone]*1000)else 0
      zonelayer_df$W_RelDrain <- ifelse(zonelayer_df$AF_Depth > 0, zonelayer_df$W_Drain/(zonelayer_df$W_FieldCap*zonelayer_df$AF_Depth*1000), 0)

      # N_StockZoneLayer[Zn1,1] = N_Stock1[Zn1,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn1,2] = N_Stock2[Zn1,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn1,3] = N_Stock3[Zn1,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn1,4] = N_Stock4[Zn1,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn2,1] = N_Stock1[Zn2,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn2,2] = N_Stock2[Zn2,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn2,3] = N_Stock3[Zn2,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn2,4] = N_Stock4[Zn2,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn3,1] = N_Stock1[Zn3,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn3,2] = N_Stock2[Zn3,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn3,3] = N_Stock3[Zn3,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn3,4] = N_Stock4[Zn3,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn4,1] = N_Stock1[Zn4,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn4,2] = N_Stock2[Zn4,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn4,3] = N_Stock3[Zn4,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # N_StockZoneLayer[Zn4,4] = N_Stock4[Zn4,N]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      zonelayer_df$N_StockZoneLayer <- zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_Stock
      
      # Mn2_SomMinExch[Zone,SoilLayer] = Mn2_NSOMMinExch*(MN2_MinNutpool[Zone,SoilLayer]-N_StockZoneLayer[Zone,SoilLayer])
      zonelayer_df$Mn2_SomMinExch <- pars$Mn_par$Mn2_NSOMMinExch*(zonelayer_df$Mn2_MinNutpool- zonelayer_df$N_StockZoneLayer)
      
      # P_StockZoneLayer[Zn1,1] = N_Stock1[Zn1,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn1,2] = N_Stock2[Zn1,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn1,3] = N_Stock3[Zn1,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn1,4] = N_Stock4[Zn1,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn2,1] = N_Stock1[Zn2,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn2,2] = N_Stock2[Zn2,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn2,3] = N_Stock3[Zn2,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn2,4] = N_Stock4[Zn2,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn3,1] = N_Stock1[Zn3,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn3,2] = N_Stock2[Zn3,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn3,3] = N_Stock3[Zn3,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn3,4] = N_Stock4[Zn3,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn4,1] = N_Stock1[Zn4,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn4,2] = N_Stock2[Zn4,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn4,3] = N_Stock3[Zn4,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      # P_StockZoneLayer[Zn4,4] = N_Stock4[Zn4,P]+ 0*(N_Stock1[Zn1,P]+N_Stock2[Zn1,P]+N_Stock3[Zn1,P]+N_Stock4[Zn1,P])
      
      zonelayer_df$P_StockZoneLayer <- zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_Stock
      
      # MP2_SomMinExch[Zone,SoilLayer] = MP2_SomMinExchBuffer*(MP2_MinNutpool[Zone,SoilLayer]-P_StockZoneLayer[Zone,SoilLayer])
      zonelayer_df$MP2_SomMinExch <- pars$Mn_par$MP2_SomMinExchBuffer*(zonelayer_df$MP2_MinNutpool-zonelayer_df$P_StockZoneLayer)
      
      # N_SomMin1Exch[Zn1,N] = Mn2_SomMinExch[Zn1,1]+0*MP2_SomMinExch[Zn1,1]
      # N_SomMin1Exch[Zn1,P] = 0*Mn2_SomMinExch[Zn4,1]+MP2_SomMinExch[Zn1,1]
      # N_SomMin1Exch[Zn2,N] = Mn2_SomMinExch[Zn2,1]+0*MP2_SomMinExch[Zn1,1]
      # N_SomMin1Exch[Zn2,P] = 0*Mn2_SomMinExch[Zn4,1]+MP2_SomMinExch[Zn2,1]
      # N_SomMin1Exch[Zn3,N] = Mn2_SomMinExch[Zn3,1]+0*MP2_SomMinExch[Zn1,1]
      # N_SomMin1Exch[Zn3,P] = 0*Mn2_SomMinExch[Zn4,1]+MP2_SomMinExch[Zn3,1]
      # N_SomMin1Exch[Zn4,N] = Mn2_SomMinExch[Zn4,1]+0*MP2_SomMinExch[Zn1,1]
      # N_SomMin1Exch[Zn4,P] = 0*Mn2_SomMinExch[Zn4,1]+MP2_SomMinExch[Zn4,1]
      # 
      # N_SomMin2Exch[Zn1,N] = Mn2_SomMinExch[Zn1,2]+0*MP2_SomMinExch[Zn1,2]
      # N_SomMin2Exch[Zn1,P] = 0*Mn2_SomMinExch[Zn1,2]+MP2_SomMinExch[Zn1,2]
      # N_SomMin2Exch[Zn2,N] = Mn2_SomMinExch[Zn2,2]+0*MP2_SomMinExch[Zn2,2]
      # N_SomMin2Exch[Zn2,P] = 0*Mn2_SomMinExch[Zn2,2]+MP2_SomMinExch[Zn2,2]
      # N_SomMin2Exch[Zn3,N] = Mn2_SomMinExch[Zn3,2]+0*MP2_SomMinExch[Zn3,2]
      # N_SomMin2Exch[Zn3,P] = 0*Mn2_SomMinExch[Zn3,2]+MP2_SomMinExch[Zn3,2]
      # N_SomMin2Exch[Zn4,N] = Mn2_SomMinExch[Zn4,2]+0*MP2_SomMinExch[Zn4,2]
      # N_SomMin2Exch[Zn4,P] = 0*Mn2_SomMinExch[Zn4,2]+MP2_SomMinExch[Zn4,2]
      # 
      # N_SomMin3Exch[Zn1,N] = Mn2_SomMinExch[Zn1,3]+0*MP2_SomMinExch[Zn1,3]
      # N_SomMin3Exch[Zn1,P] = 0*Mn2_SomMinExch[Zn1,3]+MP2_SomMinExch[Zn1,3]
      # N_SomMin3Exch[Zn2,N] = Mn2_SomMinExch[Zn2,3]+0*MP2_SomMinExch[Zn2,3]
      # N_SomMin3Exch[Zn2,P] = 0*Mn2_SomMinExch[Zn2,3]+MP2_SomMinExch[Zn2,3]
      # N_SomMin3Exch[Zn3,N] = Mn2_SomMinExch[Zn3,3]+0*MP2_SomMinExch[Zn3,3]
      # N_SomMin3Exch[Zn3,P] = 0*Mn2_SomMinExch[Zn3,3]+MP2_SomMinExch[Zn3,3]
      # N_SomMin3Exch[Zn4,N] = Mn2_SomMinExch[Zn4,3]+0*MP2_SomMinExch[Zn4,3]
      # N_SomMin3Exch[Zn4,P] = 0*Mn2_SomMinExch[Zn4,3]+MP2_SomMinExch[Zn4,3]
      # 
      # N_SomMin4Exch[Zn1,N] = Mn2_SomMinExch[Zn1,4]+0*MP2_SomMinExch[Zn1,4]
      # N_SomMin4Exch[Zn1,P] = 0*Mn2_SomMinExch[Zn1,4]+MP2_SomMinExch[Zn1,4]
      # N_SomMin4Exch[Zn2,N] = Mn2_SomMinExch[Zn2,4]+0*MP2_SomMinExch[Zn2,4]
      # N_SomMin4Exch[Zn2,P] = 0*Mn2_SomMinExch[Zn2,4]+MP2_SomMinExch[Zn2,4]
      # N_SomMin4Exch[Zn3,N] = Mn2_SomMinExch[Zn3,4]+0*MP2_SomMinExch[Zn3,4]
      # N_SomMin4Exch[Zn3,P] = 0*Mn2_SomMinExch[Zn3,4]+MP2_SomMinExch[Zn3,4]
      # N_SomMin4Exch[Zn4,N] = Mn2_SomMinExch[Zn4,4]+0*MP2_SomMinExch[Zn4,4]
      # N_SomMin4Exch[Zn4,P] = 0*Mn2_SomMinExch[Zn4,4]+MP2_SomMinExch[Zn4,4]

      zonelayernut_df$N_SomMinExch <- NA
      zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_SomMinExch <- zonelayer_df$Mn2_SomMinExch
      zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_SomMinExch <- zonelayer_df$MP2_SomMinExch
      
      # Mn_Mineralization[Zone,SlNut] = N_LittNmin1exchfact*(Mn_MinNutpool[Zone,SlNut]-N_Stock1[Zone,SlNut])
      zonenut_df$Mn_Mineralization <- pars$N_par$N_LittNmin1exchfact*(zonenut_df$Mn_MinNutpool- zonelayernut_df[zonelayernut_df$layer == 1,]$N_Stock)
      
      #TODO: layer 1,2 and 3,4 (nut = "N") were not consistent (?)
      # N_Soil1[Zn1,N] = max(0,(N_Stock1[Zn1,N])/(AF_DepthAct1[Zn1]*1000))-N_PStParam[PStMin_1]
      # N_Soil1[Zn1,P] = max(0,(N_Stock1[Zn1,P])/(AF_DepthAct1[Zn1]*1000)-N_PStParam[PStMin_1])/(N_PStParam[PStMax1]-N_PStParam[PStMin_1])
      # N_Soil1[Zn2,N] = max(0,(N_Stock1[Zn2,N])/(AF_DepthAct1[Zn2]*1000))-N_PStParam[PStMin_1]
      # N_Soil1[Zn2,P] = max(0,(N_Stock1[Zn2,P])/(AF_DepthAct1[Zn2]*1000)-N_PStParam[PStMin_1])/(N_PStParam[PStMax1]-N_PStParam[PStMin_1])
      # N_Soil1[Zn3,N] = max(0,(N_Stock1[Zn3,N])/(AF_DepthAct1[Zn3]*1000))-N_PStParam[PStMin_1]
      # N_Soil1[Zn3,P] = max(0,(N_Stock1[Zn3,P])/(AF_DepthAct1[Zn3]*1000)-N_PStParam[PStMin_1])/(N_PStParam[PStMax1]-N_PStParam[PStMin_1])
      # N_Soil1[Zn4,N] = max(0,(N_Stock1[Zn4,N])/(AF_DepthAct1[Zn4]*1000))-N_PStParam[PStMin_1]
      # N_Soil1[Zn4,P] = max(0,(N_Stock1[Zn4,P])/(AF_DepthAct1[Zn4]*1000)-N_PStParam[PStMin_1])/(N_PStParam[PStMax1]-N_PStParam[PStMin_1])
      # N_Soil2[Zn1,N] = max(0,(N_Stock2[Zn1,N])/(AF_Depth2[Zn1]*1000))-N_PStParam[PStMin_2]
      # N_Soil2[Zn1,P] = max(0,(N_Stock2[Zn1,P])/(AF_Depth2[Zn1]*1000)-N_PStParam[PStMin_2])/(N_PStParam[PStMax2]-N_PStParam[PStMin_2])
      # N_Soil2[Zn2,N] = max(0,(N_Stock2[Zn2,N])/(AF_Depth2 [Zn2]*1000))-N_PStParam[PStMin_2]
      # N_Soil2[Zn2,P] = max(0,(N_Stock2[Zn2,P])/(AF_Depth2[Zn2]*1000)-N_PStParam[PStMin_2])/(N_PStParam[PStMax2]-N_PStParam[PStMin_2])
      # N_Soil2[Zn3,N] = max(0,(N_Stock2[Zn3,N])/(AF_Depth2[Zn3]*1000))-N_PStParam[PStMin_2]
      # N_Soil2[Zn3,P] = max(0,(N_Stock2[Zn3,P])/(AF_Depth2[Zn3]*1000)-N_PStParam[PStMin_2])/(N_PStParam[PStMax2]-N_PStParam[PStMin_2])
      # N_Soil2[Zn4,N] = max(0,(N_Stock2[Zn4,N])/(AF_Depth2[Zn4]*1000))-N_PStParam[PStMin_2]
      # N_Soil2[Zn4,P] = max(0,(N_Stock2[Zn4,P])/(AF_Depth2[Zn4]*1000)-N_PStParam[PStMin_2])/(N_PStParam[PStMax2]-N_PStParam[PStMin_2])
      # N_Soil3[Zn1,N] = max(0,(N_Stock3[Zn1,N])/(AF_Depth3[Zn1]*1000))-0*N_PStParam[PStMin_3]
      # N_Soil3[Zn1,P] = max(0,(N_Stock3[Zn1,P])/(AF_Depth3[Zn1]*1000)-N_PStParam[PStMin_3])/(N_PStParam[PStMax3]-N_PStParam[PStMin_3])
      # N_Soil3[Zn2,N] = max(0,(N_Stock3[Zn2,N])/(AF_Depth3[Zn2]*1000))-0*N_PStParam[PStMin_3]
      # N_Soil3[Zn2,P] = max(0,(N_Stock3[Zn2,P])/(AF_Depth3[Zn2]*1000)-N_PStParam[PStMin_3])/(N_PStParam[PStMax3]-N_PStParam[PStMin_3])
      # N_Soil3[Zn3,N] = max(0,(N_Stock3[Zn3,N])/(AF_Depth3[Zn3]*1000))-0*N_PStParam[PStMin_3]
      # N_Soil3[Zn3,P] = max(0,(N_Stock3[Zn3,P])/(AF_Depth3[Zn3]*1000)-N_PStParam[PStMin_3])/(N_PStParam[PStMax3]-N_PStParam[PStMin_3])
      # N_Soil3[Zn4,N] = max(0,(N_Stock3[Zn4,N])/(AF_Depth3[Zn4]*1000))-0*N_PStParam[PStMin_3]
      # N_Soil3[Zn4,P] = max(0,(N_Stock3[Zn4,P])/(AF_Depth3[Zn4]*1000)-N_PStParam[PStMin_3])/(N_PStParam[PStMax3]-N_PStParam[PStMin_3])
      # N_Soil4[Zn1,N] = max(0,(N_Stock4[Zn1,N])/(AF_Depth4[Zn1]*1000))-0*N_PStParam[PStMin_4]
      # N_Soil4[Zn1,P] = max(0,(N_Stock4[Zn1,P])/(AF_Depth4[Zn1]*1000)-N_PStParam[PStMin_4])/(N_PStParam[PStMax4]-N_PStParam[PStMin_4])
      # N_Soil4[Zn2,N] = max(0,(N_Stock4[Zn2,N])/(AF_Depth4[Zn2]*1000))-0*N_PStParam[PStMin_4]
      # N_Soil4[Zn2,P] = max(0,(N_Stock4[Zn2,P])/(AF_Depth4[Zn2]*1000)-N_PStParam[PStMin_4])/(N_PStParam[PStMax4]-N_PStParam[PStMin_4])
      # N_Soil4[Zn3,N] = max(0,(N_Stock4[Zn3,N])/(AF_Depth4[Zn3]*1000))-0*N_PStParam[PStMin_4]
      # N_Soil4[Zn3,P] = max(0,(N_Stock4[Zn3,P])/(AF_Depth4[Zn3]*1000)-N_PStParam[PStMin_4])/(N_PStParam[PStMax4]-N_PStParam[PStMin_4])
      # N_Soil4[Zn4,N] = max(0,(N_Stock4[Zn4,N])/(AF_Depth4[Zn4]*1000))-0*N_PStParam[PStMin_4]
      # N_Soil4[Zn4,P] = max(0,(N_Stock4[Zn4,P])/(AF_Depth4[Zn4]*1000)-N_PStParam[PStMin_4])/(N_PStParam[PStMax4]-N_PStParam[PStMin_4])
      zonelayer_df$PStMin <- rep(layer_df$PStMin, each = nzone)
      zonelayer_df$PStMax <- rep(layer_df$PStMax, each = nzone)
      
      zonelayernut_df$N_Soil <- NA
      zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_Soil <- pmax(0,(zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_Stock)/(zonelayer_df$AF_Depth*1000))
      zonelayernut_df[zonelayernut_df$SlNut == "N" & zonelayernut_df$layer %in% c(1,2),]$N_Soil <- zonelayernut_df[zonelayernut_df$SlNut == "N" & zonelayernut_df$layer %in% c(1,2),]$N_Soil- zonelayer_df[zonelayer_df$layer %in% c(1,2),]$PStMin
      zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_Soil <- pmax(0,(zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_Stock)/(zonelayer_df$AF_Depth*1000)-zonelayer_df$PStMin)/(zonelayer_df$PStMax- zonelayer_df$PStMin)

      # N_KaPDef1[Zone] = GRAPH(N_Soil1[Zone,P])
      # N_KaPDef2[Zone] = GRAPH(N_Soil2[Zone,P])
      # N_KaPDef3[Zone] = GRAPH(N_Soil3[Zone,P])
      # N_KaPDef4[Zone] = GRAPH(N_Soil4[Zone,P])
      zln_P <- zonelayernut_df[zonelayernut_df$SlNut == "P",]
      zonelayer_df$N_KaPDef <- get_N_KaPDef(zln_P$N_Soil, zln_P$layer) 
      
      # N_CRhizVol1[Zone] = Rt_CLrvMinM1[Zone]*PI*Cq_RtDiam[Zone]^2* ((1+Rt_CRhizExt)^2-1)/4
      # N_CRhizVol2[Zone] = Rt_CLrvMinM2[Zone]*PI*Cq_RtDiam[Zone]^2* ((1+Rt_CRhizExt)^2-1)/4
      # N_CRhizVol3[Zone] = Rt_CLrvMinM3[Zone]*PI*Cq_RtDiam[Zone]^2* ((1+Rt_CRhizExt)^2-1)/4
      # N_CRhizVol4[Zone] = Rt_CLrvMinM4[Zone]*PI*Cq_RtDiam[Zone]^2* ((1+Rt_CRhizExt)^2-1)/4
      zonelayer_df$N_CRhizVol <- zonelayer_df$Rt_CLrvMinM*pi*zonelayer_df$Cq_RtDiam^2* ((1+ pars$Rt_par$Rt_CRhizExt)^2-1)/4
      
      # N_CRhizKaPM1[Zone] = N_CRhizKaPMod[Zone]*N_CRhizVol1[Zone]
      # N_CRhizKaPM2[Zone] = N_CRhizKaPMod[Zone]*N_CRhizVol2[Zone]
      # N_CRhizKaPM3[Zone] = N_CRhizKaPMod[Zone]*N_CRhizVol3[Zone]
      # N_CRhizKaPM4[Zone] = N_CRhizKaPMod[Zone]*N_CRhizVol4[Zone]
      zonelayer_df$N_CRhizKaPMod <- rep(zone_df$N_CRhizKaPMod, nlayer)
      zonelayer_df$N_CRhizKaPM <- zonelayer_df$N_CRhizKaPMod*zonelayer_df$N_CRhizVol

      # Rt_TRhizVol1[Zone,Tree] = Rt_TLrvMinM1[Zone,Tree]*PI*Rt_TDiam[Tree]^2* ((1+RT_TRhizExt[Tree])^2-1)/4
      # Rt_TRhizVol2[Zone,Tree] = Rt_TLrvMinM2[Zone,Tree]*PI*Rt_TDiam[Tree]^2* ((1+RT_TRhizExt[Tree])^2-1)/4
      # Rt_TRhizVol3[Zone,Tree] = Rt_TLrvMinM3[Zone,Tree]*PI*Rt_TDiam[Tree]^2* ((1+RT_TRhizExt[Tree])^2-1)/4
      # Rt_TRhizVol4[Zone,Tree] = Rt_TLrvMinM4[Zone,Tree]*PI*Rt_TDiam[Tree]^2* ((1+RT_TRhizExt[Tree])^2-1)/4
      zonelayertree_df$RT_TRhizExt <- rep(tree_df$RT_TRhizExt, each = nzone*nlayer)
      zonelayertree_df$Rt_TRhizVol <- zonelayertree_df$Rt_TLrvMinM*pi*zonelayertree_df$Rt_TDiam^2* ((1+zonelayertree_df$RT_TRhizExt)^2-1)/4

      # N_TRhizKaPM1[Zone,Tree] = N_TRhizKaPMod[Tree]*Rt_TRhizVol1[Zone,Tree]
      # N_TRhizKaPM2[Zone,Tree] = N_TRhizKaPMod[Tree]*Rt_TRhizVol2[Zone,Tree]
      # N_TRhizKaPM3[Zone,Tree] = N_TRhizKaPMod[Tree]*Rt_TRhizVol3[Zone,Tree]
      # N_TRhizKaPM4[Zone,Tree] = N_TRhizKaPMod[Tree]*Rt_TRhizVol4[Zone,Tree]
      zonelayertree_df$N_TRhizKaPMod <- rep(tree_df$N_TRhizKaPMod, each = nzone*nlayer)
      zonelayertree_df$N_TRhizKaPM <- zonelayertree_df$N_TRhizKaPMod*zonelayertree_df$Rt_TRhizVol

      # N_RhizEffect1[Zone] = N_CRhizKaPM1[Zone]+ARRAYSUM(N_TRhizKaPM1[Zone,*])
      # N_RhizEffect2[Zone] = N_CRhizKaPM2[Zone]+ARRAYSUM(N_TRhizKaPM2[Zone,*])
      # N_RhizEffect3[Zone] = N_CRhizKaPM3[Zone]+ARRAYSUM(N_TRhizKaPM3[Zone,*])
      # N_RhizEffect4[Zone] = N_CRhizKaPM4[Zone]+ARRAYSUM(N_TRhizKaPM4[Zone,*])
      zonelayer_df$N_RhizEffect <- zonelayer_df$N_CRhizKaPM+aggregate(zonelayertree_df["N_TRhizKaPM"], zonelayertree_df[c("zone", "layer")], sum)$N_TRhizKaPM
      
      # N_KaP1[Zone] = N_KaPDef1[Zone]*1/(1+N_RhizEffect1[Zone])
      # N_KaP2[Zone] = N_KaPDef2[Zone]*1/(1+N_RhizEffect2[Zone])
      # N_KaP3[Zone] = N_KaPDef3[Zone]*1/(1+N_RhizEffect3[Zone])
      # N_KaP4[Zone] = N_KaPDef4[Zone]*1/(1+N_RhizEffect4[Zone])
      zonelayer_df$N_KaP <- zonelayer_df$N_KaPDef*1/(1+zonelayer_df$N_RhizEffect)
      
      # N_KaLeach1[Zn1,N] = (-1 + (N_KaNO31 + 1)*(N_KaNH41+1)/(N_KaNO31+N_FracNO31[Zn1]*(N_KaNH41-N_KaNO31)+1))+0*N_KaP1[Zn1]
      # N_KaLeach1[Zn1,P] = (-1 + (N_KaNO31 + 1)*(N_KaNH41+1)/(N_KaNO31+N_FracNO31[Zn1]*(N_KaNH41-N_KaNO31)+1))*0+N_KaP1[Zn1]
      # N_KaLeach1[Zn2,N] = (-1 + (N_KaNO31 + 1)*(N_KaNH41+1)/(N_KaNO31+N_FracNO31[Zn2]*(N_KaNH41-N_KaNO31)+1))+0*N_KaP1[Zn2]
      # N_KaLeach1[Zn2,P] = (-1 + (N_KaNO31 + 1)*(N_KaNH41+1)/(N_KaNO31+N_FracNO31[Zn2]*(N_KaNH41-N_KaNO31)+1))*0+N_KaP1[Zn2]
      # N_KaLeach1[Zn3,N] = (-1 + (N_KaNO31 + 1)*(N_KaNH41+1)/(N_KaNO31+N_FracNO31[Zn3]*(N_KaNH41-N_KaNO31)+1))+0*N_KaP1[Zn3]
      # N_KaLeach1[Zn3,P] = (-1 + (N_KaNO31 + 1)*(N_KaNH41+1)/(N_KaNO31+N_FracNO31[Zn3]*(N_KaNH41-N_KaNO31)+1))*0+N_KaP1[Zn3]
      # N_KaLeach1[Zn4,N] = (-1 + (N_KaNO31 + 1)*(N_KaNH41+1)/(N_KaNO31+N_FracNO31[Zn4]*(N_KaNH41-N_KaNO31)+1))+0*N_KaP1[Zn4]
      # N_KaLeach1[Zn4,P] = (-1 + (N_KaNO31 + 1)*(N_KaNH41+1)/(N_KaNO31+N_FracNO31[Zn4]*(N_KaNH41-N_KaNO31)+1))*0+N_KaP1[Zn4]
      # N_KaLeach2[Zn1,N] =  -1 + (N_KaNO32 + 1)*(N_KaNH42+1)/(N_KaNO32+N_FracNO32[Zn1]*(N_KaNH42-N_KaNO32)+1) +0*N_KaP2[Zn1]
      # N_KaLeach2[Zn1,P] = (-1 + (N_KaNO32 + 1)*(N_KaNH42+1)/(N_KaNO32+N_FracNO32[Zn1]*(N_KaNH42-N_KaNO32)+1))*0+ N_KaP2[Zn1]
      # N_KaLeach2[Zn2,N] = -1 + (N_KaNO32 + 1)*(N_KaNH42+1)/(N_KaNO32+N_FracNO32[Zn2]*(N_KaNH42-N_KaNO32)+1) + 0*N_KaP2[Zn2]
      # N_KaLeach2[Zn2,P] = (-1 + (N_KaNO32 + 1)*(N_KaNH42+1)/(N_KaNO32+N_FracNO32[Zn2]*(N_KaNH42-N_KaNO32)+1))*0 + N_KaP2[Zn2]
      # N_KaLeach2[Zn3,N] = -1 + (N_KaNO32 + 1)*(N_KaNH42+1)/(N_KaNO32+N_FracNO32[Zn3]*(N_KaNH42-N_KaNO32)+1) + 0*N_KaP2[Zn3]
      # N_KaLeach2[Zn3,P] = (-1 + (N_KaNO32 + 1)*(N_KaNH42+1)/(N_KaNO32+N_FracNO32[Zn3]*(N_KaNH42-N_KaNO32)+1))*0 + N_KaP2[Zn3]
      # N_KaLeach2[Zn4,N] = -1 + (N_KaNO32 + 1)*(N_KaNH42+1)/(N_KaNO32+N_FracNO32[Zn4]*(N_KaNH42-N_KaNO32)+1) + 0*N_KaP2[Zn4]
      # N_KaLeach2[Zn4,P] = (-1 + (N_KaNO32 + 1)*(N_KaNH42+1)/(N_KaNO32+N_FracNO32[Zn4]*(N_KaNH42-N_KaNO32)+1))*0 + N_KaP2[Zn4]
      # N_KaLeach3[Zn1,N] = -1 + (N_KaNO33 + 1)*(N_KaNH43+1)/(N_KaNO33+N_FracNO33[Zn1]*(N_KaNH43-N_KaNO33)+1) + 0 * N_KaP3[Zn1]
      # N_KaLeach3[Zn1,P] = (-1 + (N_KaNO33 + 1)*(N_KaNH43+1)/(N_KaNO33+N_FracNO33[Zn1]*(N_KaNH43-N_KaNO33)+1))*0 + N_KaP3[Zn1]
      # N_KaLeach3[Zn2,N] = -1 + (N_KaNO33 + 1)*(N_KaNH43+1)/(N_KaNO33+N_FracNO33[Zn2]*(N_KaNH43-N_KaNO33)+1) + 0 * N_KaP3[Zn2]
      # N_KaLeach3[Zn2,P] = (-1 + (N_KaNO33 + 1)*(N_KaNH43+1)/(N_KaNO33+N_FracNO33[Zn2]*(N_KaNH43-N_KaNO33)+1))*0 + N_KaP3[Zn2]
      # N_KaLeach3[Zn3,N] = -1 + (N_KaNO33 + 1)*(N_KaNH43+1)/(N_KaNO33+N_FracNO33[Zn3]*(N_KaNH43-N_KaNO33)+1) + 0 * N_KaP3[Zn3]
      # N_KaLeach3[Zn3,P] = (-1 + (N_KaNO33 + 1)*(N_KaNH43+1)/(N_KaNO33+N_FracNO33[Zn3]*(N_KaNH43-N_KaNO33)+1))*0+N_KaP3[Zn3]
      # N_KaLeach3[Zn4,N] = -1 + (N_KaNO33 + 1)*(N_KaNH43+1)/(N_KaNO33+N_FracNO33[Zn4]*(N_KaNH43-N_KaNO33)+1) + 0 * N_KaP3[Zn4]
      # N_KaLeach3[Zn4,P] = (-1 + (N_KaNO33 + 1)*(N_KaNH43+1)/(N_KaNO33+N_FracNO33[Zn4]*(N_KaNH43-N_KaNO33)+1)) * 0 + N_KaP3[Zn4]
      # N_KaLeach4[Zn1,N] = -1 + (N_KaNO34 + 1)*(N_KaNH44+1)/(N_KaNO34+N_FracNO34[Zn1]*(N_KaNH44-N_KaNO34)+1) + 0*N_KaP4[Zn1]
      # N_KaLeach4[Zn1,P] = (-1 + (N_KaNO34 + 1)*(N_KaNH44+1)/(N_KaNO34+N_FracNO34[Zn1]*(N_KaNH44-N_KaNO34)+1)) * 0 + N_KaP4[Zn1]
      # N_KaLeach4[Zn2,N] = -1 + (N_KaNO34 + 1)*(N_KaNH44+1)/(N_KaNO34+N_FracNO34[Zn2]*(N_KaNH44-N_KaNO34)+1) + 0*N_KaP4[Zn2]
      # N_KaLeach4[Zn2,P] = (-1 + (N_KaNO34 + 1)*(N_KaNH44+1)/(N_KaNO34+N_FracNO34[Zn2]*(N_KaNH44-N_KaNO34)+1)) * 0 + N_KaP4[Zn2]
      # N_KaLeach4[Zn3,N] = -1 + (N_KaNO34 + 1)*(N_KaNH44+1)/(N_KaNO34+N_FracNO34[Zn3]*(N_KaNH44-N_KaNO34)+1) + 0*N_KaP4[Zn3]
      # N_KaLeach4[Zn3,P] = (-1 + (N_KaNO34 + 1)*(N_KaNH44+1)/(N_KaNO34+N_FracNO34[Zn3]*(N_KaNH44-N_KaNO34)+1)) * 0 +N_KaP4[Zn3]
      # N_KaLeach4[Zn4,N] = -1 + (N_KaNO34 + 1)*(N_KaNH44+1)/(N_KaNO34+N_FracNO34[Zn4]*(N_KaNH44-N_KaNO34)+1) + 0*N_KaP4[Zn4]
      # N_KaLeach4[Zn4,P] = (-1 + (N_KaNO34 + 1)*(N_KaNH44+1)/(N_KaNO34+N_FracNO34[Zn4]*(N_KaNH44-N_KaNO34)+1)) * 0 + N_KaP4[Zn4]
      
      zonelayer_df$N_KaNO3 <- rep(layer_df$N_KaNO3, each = nzone)
      zonelayer_df$N_KaNH4 <- rep(layer_df$N_KaNH4, each = nzone)

      zonelayernut_df$N_KaLeach <- NA
      zonelayernut_df[zonelayernut_df$SlNut == "N", ]$N_KaLeach <- -1 + (zonelayer_df$N_KaNO3 + 1) *
        (zonelayer_df$N_KaNH4 + 1) /
        (
          zonelayer_df$N_KaNO3 + zonelayer_df$N_FracNO3 * (zonelayer_df$N_KaNH4 - zonelayer_df$N_KaNO3) +
            1
        )
      zonelayernut_df[zonelayernut_df$SlNut == "P", ]$N_KaLeach <- zonelayer_df$N_KaP

      # N_LeachConc1[Zone,SlNut] = if W_RelDrain1[Zone]>0 then ((N_Stock1[Zone,SlNut]+Mn_Mineralization[Zone,SlNut]+N_SomMin1Exch[Zone,SlNut])*(N_BypassMatrix1[Zone]^W_RelDrain1[Zone]) +(Mn_FertDissF[Zone,SlNut])*(N_BypassMacro1[Zone]^W_RelDrain1[Zone]))/((W_FieldCap1[Zone]*AF_DepthAct1[Zone]*1000+W_V1Drain[Zone]+W_LatRecharge1[Zone])*(N_KaLeach1[Zone,SlNut]+1)) else 0
      # N_LeachConc2[Zone,SlNut] = if W_RelDrain2[Zone]>0 then ((N_Stock2[Zone,SlNut]+N_SomMin2Exch[Zone,SlNut])*(N_BypassMatrix2[Zone]^W_RelDrain2[Zone])+(N_VLeach1[Zone,SlNut])*(N_BypassMacro2[Zone]^W_RelDrain2[Zone]))/((W_FieldCap2[Zone]*AF_Depth2[Zone]*1000+W_V2Drain[Zone]+W_LatRecharge2[Zone])*(N_KaLeach2[Zone,SlNut]+1)) else 0
      # N_LeachConc3[Zone,SlNut] = if W_RelDrain3[Zone]>0 then ((N_Stock3[Zone,SlNut]+N_SomMin3Exch[Zone,SlNut])*(N_BypassMatrix3[Zone]^W_RelDrain3[Zone])+(N_VLeach2[Zone,SlNut])*(N_BypassMacro3[Zone]^W_RelDrain3[Zone]))/((W_FieldCap3[Zone]*AF_Depth3[Zone]*1000+W_V3Drain[Zone]+W_LatRecharge3[Zone])*(N_KaLeach3[Zone,SlNut]+1)) else 0
      # N_LeachConc4[Zone,SlNut] = if W_V4Drain[Zone]> 0 then ((N_Stock4[Zone,SlNut]+N_SomMin4Exch[Zone,SlNut])*(N_BypassMatrix4[Zone]^W_RelDrain4[Zone])+(N_VLeach3[Zone,SlNut])*(N_BypassMacro4[Zone]^W_RelDrain4[Zone]))/((W_FieldCap4[Zone]*AF_Depth4[Zone]*1000+W_V4Drain[Zone]+W_LatRecharge4[Zone])*(N_KaLeach4[Zone,SlNut]+1)) else 0
      
      zonelayernut_df$N_BypassMatrix <- rep(zonelayer_df$N_BypassMatrix, nrow(nut_df))
      zonelayernut_df$N_BypassMacro <- rep(zonelayer_df$N_BypassMacro, nrow(nut_df))
      zonelayernut_df$W_FieldCap <- rep(zonelayer_df$W_FieldCap, nrow(nut_df))
      zonelayernut_df$W_RelDrain <- rep(zonelayer_df$W_RelDrain, nrow(nut_df))
      zonelayernut_df$W_VDrain <- rep(zonelayer_df$W_VDrain, nrow(nut_df))
      zonelayernut_df$W_LatRecharge <- rep(zonelayer_df$W_LatRecharge, nrow(nut_df))

      zonelayernut_df$N_VLeach <- NA
      zonelayernut_df$N_LeachConc <- NA

      zl1n <- zonelayernut_df[zonelayernut_df$layer == 1,]                                      
      zonelayernut_df[zonelayernut_df$layer == 1,]$N_LeachConc <- 
        ifelse( zl1n$W_RelDrain > 0,
                ((zl1n$N_Stock+ zonenut_df$Mn_Mineralization+ zl1n$N_SomMinExch)*
                   (zl1n$N_BypassMatrix^zl1n$W_RelDrain) +
                   zonenut_df$Mn_FertDissF*(zl1n$N_BypassMacro^zl1n$W_RelDrain))/
                  ((zl1n$W_FieldCap*zl1n$AF_Depth*1000+zl1n$W_VDrain+ zl1n$W_LatRecharge)*(zl1n$N_KaLeach+1)), 0)
      
      zl1n <- zonelayernut_df[zonelayernut_df$layer == 1,]
      # N_VLeach1[Zone,SlNut] = N_LeachConc1[Zone,SlNut]*W_V1Drain[Zone]          
      zonelayernut_df[zonelayernut_df$layer == 1,]$N_VLeach <- zl1n$N_LeachConc*zl1n$W_VDrain
      
      zl1n <- zonelayernut_df[zonelayernut_df$layer == 1,]
      zl2n <- zonelayernut_df[zonelayernut_df$layer == 2,]                                      
      zonelayernut_df[zonelayernut_df$layer == 2,]$N_LeachConc <- 
        ifelse( zl2n$W_RelDrain > 0,
                ((zl2n$N_Stock+ zl2n$N_SomMinExch)*
                   (zl2n$N_BypassMatrix^zl2n$W_RelDrain) +
                   zl1n$N_VLeach*(zl2n$N_BypassMacro^zl2n$W_RelDrain))/
                  ((zl2n$W_FieldCap*zl2n$AF_Depth*1000+zl2n$W_VDrain+ zl2n$W_LatRecharge)*(zl2n$N_KaLeach+1)), 0)
      
      zl2n <- zonelayernut_df[zonelayernut_df$layer == 2,]
      # N_VLeach2[Zone,SlNut] = N_LeachConc2[Zone,SlNut]*W_V2Drain[Zone]          
      zonelayernut_df[zonelayernut_df$layer == 2,]$N_VLeach <- zl2n$N_LeachConc*zl2n$W_VDrain
      
      zl2n <- zonelayernut_df[zonelayernut_df$layer == 2,]
      zl3n <- zonelayernut_df[zonelayernut_df$layer == 3,]                                      
      zonelayernut_df[zonelayernut_df$layer == 3,]$N_LeachConc <- 
        ifelse( zl3n$W_RelDrain > 0,
                ((zl3n$N_Stock+ zl3n$N_SomMinExch)*
                   (zl3n$N_BypassMatrix^zl3n$W_RelDrain) +
                   zl2n$N_VLeach*(zl3n$N_BypassMacro^zl3n$W_RelDrain))/
                  ((zl3n$W_FieldCap*zl3n$AF_Depth*1000+zl3n$W_VDrain+ zl3n$W_LatRecharge)*(zl3n$N_KaLeach+1)), 0)
      
      zl3n <- zonelayernut_df[zonelayernut_df$layer == 3,]
      # N_VLeach3[Zone,SlNut] = N_LeachConc3[Zone,SlNut]*W_V3Drain[Zone]          
      zonelayernut_df[zonelayernut_df$layer == 3,]$N_VLeach <- zl3n$N_LeachConc*zl3n$W_VDrain
      
      zl3n <- zonelayernut_df[zonelayernut_df$layer == 3,]
      zl4n <- zonelayernut_df[zonelayernut_df$layer == 4,]                                      
      zonelayernut_df[zonelayernut_df$layer == 4,]$N_LeachConc <- 
        ifelse( zl4n$W_RelDrain > 0,
                ((zl4n$N_Stock+ zl4n$N_SomMinExch)*
                   (zl4n$N_BypassMatrix^zl4n$W_RelDrain) +
                   zl3n$N_VLeach*(zl4n$N_BypassMacro^zl4n$W_RelDrain))/
                  ((zl4n$W_FieldCap*zl4n$AF_Depth*1000+zl4n$W_VDrain+ zl4n$W_LatRecharge)*(zl4n$N_KaLeach+1)), 0)
      
      zl4n <- zonelayernut_df[zonelayernut_df$layer == 4,]
      # N_VLeach4[Zone,SlNut] = N_LeachConc4[Zone,SlNut]*W_V4Drain[Zone]          
      zonelayernut_df[zonelayernut_df$layer == 4,]$N_VLeach <- zl4n$N_LeachConc*zl4n$W_VDrain
      
      # N_HLeach1[Zone,SlNut] = N_LeachConc1[Zone,SlNut]*W_H1Drain[Zone]
      # N_HLeach2[Zone,SlNut] = N_LeachConc2[Zone,SlNut]*W_H2Drain[Zone]
      # N_HLeach3[Zone,SlNut] = N_LeachConc3[Zone,SlNut]*W_H3Drain[Zone]
      # N_HLeach4[Zone,SlNut] = N_LeachConc4[Zone,SlNut]*W_H4Drain[Zone]
      zonelayernut_df$W_HDrain <- rep(zonelayer_df$W_HDrain, nrow(nut_df))
      zonelayernut_df$N_HLeach <- zonelayernut_df$N_LeachConc * zonelayernut_df$W_HDrain

      # N_AvgLeachConc1[SlNut] = max(0,AF_ZoneFrac[Zn1]*N_LeachConc1[Zn1,SlNut]+AF_ZoneFrac[Zn2]*N_LeachConc1[Zn2,SlNut]+AF_ZoneFrac[Zn3]*N_LeachConc1[Zn3,SlNut]+AF_ZoneFrac[Zn4]*N_LeachConc1[Zn4,SlNut])
      # N_AvgLeachConc2[SlNut] = max(0,AF_ZoneFrac[Zn1]*N_LeachConc2[Zn1,SlNut]+AF_ZoneFrac[Zn2]*N_LeachConc2[Zn2,SlNut]+AF_ZoneFrac[Zn3]*N_LeachConc2[Zn3,SlNut]+AF_ZoneFrac[Zn4]*N_LeachConc2[Zn4,SlNut])
      # N_AvgLeachConc3[SlNut] = max(0,AF_ZoneFrac[Zn1]*N_LeachConc3[Zn1,SlNut]+AF_ZoneFrac[Zn2]*N_LeachConc3[Zn2,SlNut]+AF_ZoneFrac[Zn3]*N_LeachConc3[Zn3,SlNut]+AF_ZoneFrac[Zn4]*N_LeachConc3[Zn4,SlNut])
      # N_AvgLeachConc4[SlNut] = max(0,AF_ZoneFrac[Zn1]*N_LeachConc4[Zn1,SlNut]+AF_ZoneFrac[Zn2]*N_LeachConc4[Zn2,SlNut]+AF_ZoneFrac[Zn3]*N_LeachConc4[Zn3,SlNut]+AF_ZoneFrac[Zn4]*N_LeachConc4[Zn4,SlNut])
      zonelayernut_df$AF_ZoneFrac <- rep(zone_df$AF_ZoneFrac, nlayer * nrow(nut_df))
      zonelayernut_df$N_LeachConc_frac <- zonelayernut_df$AF_ZoneFrac * zonelayernut_df$N_LeachConc
      layernut_df$N_AvgLeachConc <- pmax(0, aggregate(zonelayernut_df["N_LeachConc_frac"], zonelayernut_df[c("layer", "SlNut")], sum)$N_LeachConc_frac)
      
      # N_Lat4NutInflow1[SlNut] = LF_Lat4Inflow1*N_AvgLeachConc1[SlNut]*N_Lat4InflowRelConc
      # N_Lat4NutInflow2[SlNut] = LF_Lat4Inflow2*N_AvgLeachConc2[SlNut]*N_Lat4InflowRelConc
      # N_Lat4NutInflow3[SlNut] = LF_Lat4Inflow3*N_AvgLeachConc3[SlNut]*N_Lat4InflowRelConc
      # N_Lat4NutInflow4[SlNut] = LF_Lat4Inflow4*N_Lat4InflowRelConc*N_AvgLeachConc4[SlNut]
      layernut_df$LF_Lat4Inflow <- rep(layer_df$LF_Lat4Inflow, nrow(nut_df))
      layernut_df$N_Lat4NutInflow <- layernut_df$LF_Lat4Inflow*layernut_df$N_AvgLeachConc*pars$N_par$N_Lat4InflowRelConc
      
      # GHG_EffWaterfPoreF[Zone] = if W_V1Drain[Zone] = 0 then W_WaterfilledPoreF1[Zone] else min(1,GHG_SatTimeDuringVDrainDay+W_WaterfilledPoreF1[Zone]*(1-GHG_SatTimeDuringVDrainDay))
      zl1 <- zonelayer_df[zonelayer_df$layer == 1,]
      zone_df$GHG_EffWaterfPoreF <- ifelse(zl1$W_VDrain == 0, zl1$W_WaterfilledPoreF, pmin(1, pars$GHG_par$GHG_SatTimeDuringVDrainDay+zl1$W_WaterfilledPoreF*(1-pars$GHG_par$GHG_SatTimeDuringVDrainDay)))
      
      # GHG_N2O_EmFraction[Zone] =  (10^(0.030001*100*GHG_EffWaterfPoreF[Zone]-1.446925)/(1+ (10^(0.030001*100*GHG_EffWaterfPoreF[Zone]-1.446925))))
      zone_df$GHG_N2O_EmFraction <-  (10^(0.030001*100*zone_df$GHG_EffWaterfPoreF-1.446925)/(1+ (10^(0.030001*100*zone_df$GHG_EffWaterfPoreF-1.446925))))
      
      # GHG_NitOxEm1[Zone] = 24*10^(-5) * 0.7212* ( 1.5*(Mn2_SomMinExch[Zone,1]+GHG_LittMinMultiplier*Mn_Mineralization[Zone,N])/( AF_DepthAct1[Zone]*W_BDLayer[1])^1.1699)
      # GHG_NitOxEm2[Zone] = 24*10^(-5) * 0.7212* ( 1.5*Mn2_SomMinExch[Zone,2]/( AF_Depth2[Zone]*W_BDLayer[2])^1.1699)
      # GHG_NitOxEm3[Zone] = 24*10^(-5) * 0.7212* ( 1.5*Mn2_SomMinExch[Zone,3]/( AF_Depth3[Zone]*W_BDLayer[3])^1.1699)
      # GHG_NitOxEm4[Zone] = 24*10^(-5) * 0.7212* ( 1.5*Mn2_SomMinExch[Zone,4]/( AF_Depth4[Zone]*W_BDLayer[4])^1.1699)
      
      zonelayer_df$Mn2_SomMinExch_calc <- zonelayer_df$Mn2_SomMinExch
      zonelayer_df[zonelayer_df$layer == 1, ]$Mn2_SomMinExch_calc <- zonelayer_df[zonelayer_df$layer == 1, ]$Mn2_SomMinExch + pars$GHG_par$GHG_LittMinMultiplier *
        zonenut_df[zonenut_df$SlNut == "N", ]$Mn_Mineralization
      zonelayer_df$GHG_NitOxEm <- 24*10^(-5) * 0.7212* ( 1.5* zonelayer_df$Mn2_SomMinExch_calc/( zonelayer_df$AF_Depth* zonelayer_df$W_BDLayer)^1.1699)

      # GHG_N2O_Em[Zone] = GHG_N2O_EmFraction[Zone]*GHG_NitOxEm1[Zone]
      zone_df$GHG_N2O_Em <- zone_df$GHG_N2O_EmFraction*zonelayer_df[zonelayer_df$layer == 1,]$GHG_NitOxEm
      
      # GHG_N2_per_NOx[Zone] = GRAPH(GHG_EffWaterfPoreF[Zone])
      zone_df$GHG_N2_per_NOx <- get_GHG_N2_per_NOx(zone_df$GHG_EffWaterfPoreF)
      
      # GHG_N2_Em[Zone] = GHG_N2_per_NOx[Zone]*GHG_NitOxEm1[Zone]
      zone_df$GHG_N2_Em <- zone_df$GHG_N2_per_NOx* zonelayer_df[zonelayer_df$layer == 1,]$GHG_NitOxEm
      
      # GHG_NO_Em[Zone] = (1-GHG_N2O_EmFraction[Zone])*GHG_NitOxEm1[Zone]
      zone_df$GHG_NO_Em <- (1- zone_df$GHG_N2O_EmFraction)* zonelayer_df[zonelayer_df$layer == 1,]$GHG_NitOxEm
      
      # GHG_NgasEmissionFlux[Zone] = GHG_N2O_Em[Zone]+GHG_N2_Em[Zone]+GHG_NO_Em[Zone]
      zone_df$GHG_NgasEmissionFlux <- zone_df$GHG_N2O_Em + zone_df$GHG_N2_Em+zone_df$GHG_NO_Em
      
      # N_NGassLoss1[Zone] = GHG_NgasEmissionFlux[Zone]*N_Use_NgassLossEst?
      # N_NgassLoss2[Zone] = N_Use_NgassLossEst?*GHG_NitOxEm2[Zone]
      # N_NgassLoss3[Zone] = N_Use_NgassLossEst?*GHG_NitOxEm3[Zone]
      # N_NgassLoss4[Zone] = N_Use_NgassLossEst?*GHG_NitOxEm4[Zone]
      zonelayer_df$N_NGassLoss <- pars$N_par$N_Use_NgassLossEst_is*zonelayer_df$GHG_NitOxEm
      zonelayer_df[zonelayer_df$layer == 1,]$N_NGassLoss <- pars$N_par$N_Use_NgassLossEst_is*zone_df$GHG_NgasEmissionFlux
      
      # S&B_DeadWoodBurnFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
      zone_df$SB_DeadWoodBurnFrac <- get_SB_DeadWoodBurnFrac(zone_df$SB_FireTempIncSurf)
      zonepcomp_df$SB_DeadWoodBurnFrac <- rep(zone_df$SB_DeadWoodBurnFrac, nrow(pcomp_df))
      
      # S&B_WoodFNutMin[Zone,PlantComp] = (1-S&B_DW?[PlantComp])*S&B_DeadWoodBurnFrac[Zone]*(1-S&B_NutVolatFrac[Zone,PlantComp])*S&B_DeadWood[Zone,PlantComp]
      zonepcomp_df$SB_WoodFNutMin <- (1- zonepcomp_df$SB_DW_is)* zonepcomp_df$SB_DeadWoodBurnFrac *(1- zonepcomp_df$SB_NutVolatFrac)* zonepcomp_df$SB_DeadWood
      
      # S&B_MinNutRel[Zone,PlantComp] = S&B_WoodFNutMin[Zone,PlantComp]+S&B_MinNutRele[Zone,PlantComp]
      zonepcomp_df$SB_MinNutRel <- zonepcomp_df$SB_WoodFNutMin + zonepcomp_df$SB_MinNutRele
      
      # N_In1[Zn1,N] = Mn_FertDissF[Zn1,N]-N_VLeach1[Zn1,N]+AF_LatInFlowRatio[Zn1]*N_HLeach1[Zn2,N]                    +S&B_MinNutRel[Zn1,N]+N_AtmosphDepos[N]-N_NGassLoss1[Zn1]
      # N_In1[Zn2,N] = Mn_FertDissF[Zn2,N]-N_VLeach1[Zn2,N]+AF_LatInFlowRatio[Zn2]*N_HLeach1[Zn3,N]   -N_HLeach1[Zn2,N]+S&B_MinNutRel[Zn2,N]+N_AtmosphDepos[N]-N_NGassLoss1[Zn2]
      # N_In1[Zn3,N] = Mn_FertDissF[Zn3,N]-N_VLeach1[Zn3,N]+AF_LatInFlowRatio[Zn3]*N_HLeach1[Zn4,N]   -N_HLeach1[Zn3,N]+S&B_MinNutRel[Zn3,N]+N_AtmosphDepos[N]-N_NGassLoss1[Zn3]
      # N_In1[Zn4,N] = Mn_FertDissF[Zn4,N]-N_VLeach1[Zn4,N]+AF_LatInFlowRatio[Zn4]*N_Lat4NutInflow1[N]-N_HLeach1[Zn4,N]+S&B_MinNutRel[Zn4,N]+N_AtmosphDepos[N]-N_NGassLoss1[Zn4]
      # 
      # N_In1[Zn1,P] = Mn_FertDissF[Zn1,P]-N_VLeach1[Zn1,P]+AF_LatInFlowRatio[Zn1]*N_HLeach1[Zn2,P]                    +S&B_MinNutRel[Zn1,P]+N_AtmosphDepos[P]
      # N_In1[Zn2,P] = Mn_FertDissF[Zn2,P]-N_VLeach1[Zn2,P]+AF_LatInFlowRatio[Zn2]*N_HLeach1[Zn3,P]   -N_HLeach1[Zn2,P]+S&B_MinNutRel[Zn2,P]+N_AtmosphDepos[P]
      # N_In1[Zn3,P] = Mn_FertDissF[Zn3,P]-N_VLeach1[Zn3,P]+AF_LatInFlowRatio[Zn3]*N_HLeach1[Zn4,P]   -N_HLeach1[Zn3,P]+S&B_MinNutRel[Zn3,P]+N_AtmosphDepos[P]
      # N_In1[Zn4,P] = Mn_FertDissF[Zn4,P]-N_VLeach1[Zn4,P]+AF_LatInFlowRatio[Zn4]*N_Lat4NutInflow1[P]-N_HLeach1[Zn4,P]+S&B_MinNutRel[Zn4,P]+N_AtmosphDepos[P]
      # 
      # 
      # N_In2[Zn1,N] = N_VLeach1[Zn1,N]-N_VLeach2[Zn1,N]+AF_LatInFlowRatio[Zn1]*N_HLeach2[Zn2,N]                    -N_NgassLoss2[Zn1]
      # N_In2[Zn2,N] = N_VLeach1[Zn2,N]-N_VLeach2[Zn2,N]+AF_LatInFlowRatio[Zn2]*N_HLeach2[Zn3,N]   -N_HLeach2[Zn2,N]-N_NgassLoss2[Zn2]
      # N_In2[Zn3,N] = N_VLeach1[Zn3,N]-N_VLeach2[Zn3,N]+AF_LatInFlowRatio[Zn3]*N_HLeach2[Zn4,N]   -N_HLeach2[Zn3,N]-N_NgassLoss2[Zn3]
      # N_In2[Zn4,N] = N_VLeach1[Zn4,N]-N_VLeach2[Zn4,N]+AF_LatInFlowRatio[Zn4]*N_Lat4NutInflow2[N]-N_HLeach2[Zn4,N]-N_NgassLoss2[Zn4]
      # 
      # N_In2[Zn1,P] = N_VLeach1[Zn1,P]-N_VLeach2[Zn1,P]+AF_LatInFlowRatio[Zn1]*N_HLeach2[Zn2,P]
      # N_In2[Zn2,P] = N_VLeach1[Zn2,P]-N_VLeach2[Zn2,P]+AF_LatInFlowRatio[Zn2]*N_HLeach2[Zn3,P]   -N_HLeach2[Zn2,P]
      # N_In2[Zn3,P] = N_VLeach1[Zn3,P]-N_VLeach2[Zn3,P]+AF_LatInFlowRatio[Zn3]*N_HLeach2[Zn4,P]   -N_HLeach2[Zn3,P]
      # N_In2[Zn4,P] = N_VLeach1[Zn4,P]-N_VLeach2[Zn4,P]+AF_LatInFlowRatio[Zn4]*N_Lat4NutInflow2[P]-N_HLeach2[Zn4,P]
      # 
      # 
      # N_In3[Zn1,N] = N_VLeach2[Zn1,N]-N_VLeach3[Zn1,N]+AF_LatInFlowRatio[Zn1]*N_HLeach3[Zn2,N]                    -N_NgassLoss3[Zn1]
      # N_In3[Zn2,N] = N_VLeach2[Zn2,N]-N_VLeach3[Zn2,N]+AF_LatInFlowRatio[Zn2]*N_HLeach3[Zn3,N]   -N_HLeach3[Zn2,N]-N_NgassLoss3[Zn2]
      # N_In3[Zn3,N] = N_VLeach2[Zn3,N]-N_VLeach3[Zn3,N]+AF_LatInFlowRatio[Zn3]*N_HLeach3[Zn4,N]   -N_HLeach3[Zn3,N]-N_NgassLoss3[Zn3]
      # N_In3[Zn4,N] = N_VLeach2[Zn4,N]-N_VLeach3[Zn4,N]+AF_LatInFlowRatio[Zn4]*N_Lat4NutInflow3[N]-N_HLeach3[Zn4,N]-N_NgassLoss3[Zn4]
      # 
      # N_In3[Zn1,P] = N_VLeach2[Zn1,P]-N_VLeach3[Zn1,P]+AF_LatInFlowRatio[Zn1]*N_HLeach3[Zn2,P]
      # N_In3[Zn2,P] = N_VLeach2[Zn2,P]-N_VLeach3[Zn2,P]+AF_LatInFlowRatio[Zn2]*N_HLeach3[Zn3,P]   -N_HLeach3[Zn2,P]
      # N_In3[Zn3,P] = N_VLeach2[Zn3,P]-N_VLeach3[Zn3,P]+AF_LatInFlowRatio[Zn3]*N_HLeach3[Zn4,P]   -N_HLeach3[Zn3,P]
      # N_In3[Zn4,P] = N_VLeach2[Zn4,P]-N_VLeach3[Zn4,P]+AF_LatInFlowRatio[Zn4]*N_Lat4NutInflow3[P]-N_HLeach3[Zn4,P]
      # 
      # 
      # N_In4[Zn1,N] = N_VLeach3[Zn1,N]                 +AF_LatInFlowRatio[Zn1]*N_HLeach4[Zn2,N]                    -N_NgassLoss4[Zn1]
      # N_In4[Zn2,N] = N_VLeach3[Zn2,N]                 +AF_LatInFlowRatio[Zn2]*N_HLeach4[Zn3,N]   -N_HLeach4[Zn2,N]-N_NgassLoss4[Zn2]
      # N_In4[Zn3,N] = N_VLeach3[Zn3,N]                 +AF_LatInFlowRatio[Zn3]*N_HLeach4[Zn4,N]   -N_HLeach4[Zn3,N]-N_NgassLoss4[Zn3]
      # N_In4[Zn4,N] = N_VLeach3[Zn4,N]                 +AF_LatInFlowRatio[Zn4]*N_Lat4NutInflow4[N]-N_HLeach4[Zn4,N]-N_NgassLoss4[Zn4]
      # 
      # N_In4[Zn1,P] = N_VLeach3[Zn1,P]                 +AF_LatInFlowRatio[Zn1]*N_HLeach4[Zn2,P]
      # N_In4[Zn2,P] = N_VLeach3[Zn2,P]                 +AF_LatInFlowRatio[Zn2]*N_HLeach4[Zn3,P]   -N_HLeach4[Zn2,P]
      # N_In4[Zn3,P] = N_VLeach3[Zn3,P]                 +AF_LatInFlowRatio[Zn3]*N_HLeach4[Zn4,P]   -N_HLeach4[Zn3,P]
      # N_In4[Zn4,P] = N_VLeach3[Zn4,P]                 +AF_LatInFlowRatio[Zn4]*N_Lat4NutInflow4[P]-N_HLeach4[Zn4,P]
      
      zonelayernut_df$N_VLeach_up <- NA
      zonelayernut_df[zonelayernut_df$layer %in% c(2:4),]$N_VLeach_up <- zonelayernut_df[zonelayernut_df$layer %in% c(1:3),]$N_VLeach
      zonelayernut_df[zonelayernut_df$layer == 1,]$N_VLeach_up <- zonenut_df$Mn_FertDissF
      
      zonelayernut_df$N_VLeach_wo_l4 <- zonelayernut_df$N_VLeach
      zonelayernut_df[zonelayernut_df$layer == 4,]$N_VLeach_wo_l4 <- 0
      
      zonelayernut_df$N_HLeach_right <- NA
      zonelayernut_df[zonelayernut_df$zone %in% c(1:3),]$N_HLeach_right <- zonelayernut_df[zonelayernut_df$zone %in% c(2:4),]$N_HLeach
      zonelayernut_df[zonelayernut_df$zone == 4,]$N_HLeach_right <- layernut_df$N_Lat4NutInflow
      
      zonelayernut_df$N_HLeach_wo_z1 <- zonelayernut_df$N_HLeach
      zonelayernut_df[zonelayernut_df$zone == 1,]$N_HLeach_wo_z1 <- 0
      
      zonelayernut_df$AF_LatInFlowRatio <- rep(zone_df$AF_LatInFlowRatio, nlayer*nrow(nut_df))
      
      zonelayernut_df$N_In <- zonelayernut_df$N_VLeach_up - zonelayernut_df$N_VLeach_wo_l4 + 
        zonelayernut_df$AF_LatInFlowRatio*zonelayernut_df$N_HLeach_right - zonelayernut_df$N_HLeach_wo_z1
      
      zonenut_df$N_AtmosphDepos <- rep(nut_df$N_AtmosphDepos, each = nzone)
      
      zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_In <- zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_In - zonelayer_df$N_NGassLoss
      zonelayernut_df[zonelayernut_df$layer == 1,]$N_In <- zonelayernut_df[zonelayernut_df$layer == 1,]$N_In + zonepcomp_df[zonepcomp_df$PlantComp %in% c("N", "P"),]$SB_MinNutRel+zonenut_df$N_AtmosphDepos
      
      # S&B_pHmodPsorp[Zone] = GRAPH(S&B_Topsoil_pH[Zone])
      zone_df$SB_pHmodPsorp <- get_SB_pHmodPsorp(zone_df$SB_Topsoil_pH)
      
      # S&B_RelPSorption[Zone] = S&B_PsorpModifier[Zone]*S&B_pHmodPsorp[Zone]
      zone_df$SB_RelPSorption <- zone_df$SB_PsorpModifier* zone_df$SB_pHmodPsorp
      
      # N_Ka1[Zn1,N] = -W_Theta1[Zn1] + (N_KaNO31 +W_Theta1[Zn1])*(N_KaNH41+W_Theta1[Zn1])/(N_KaNO31 + N_FracNO31[Zn1] * (N_KaNH41-N_KaNO31) + W_Theta1[Zn1])+0*N_KaP1[Zn1]*S&B_RelPSorption[Zn1]
      # N_Ka1[Zn1,P] = N_KaP1[Zn1]*S&B_RelPSorption[Zn1]+ 0*(N_FracNO31[Zn1]+N_KaNH41+N_KaNO31+W_Theta1[Zn1])
      # N_Ka1[Zn2,N] = -W_Theta1[Zn2] + (N_KaNO31 +W_Theta1[Zn2])*(N_KaNH41+W_Theta1[Zn2])/(N_KaNO31 + N_FracNO31[Zn2] * (N_KaNH41-N_KaNO31) + W_Theta1[Zn2])+0*N_KaP1[Zn2]*S&B_RelPSorption[Zn2]
      # N_Ka1[Zn2,P] = N_KaP1[Zn2]*S&B_RelPSorption[Zn2]+ 0*(N_FracNO31[Zn2]+N_KaNH41+N_KaNO31+W_Theta1[Zn2])
      # N_Ka1[Zn3,N] = -W_Theta1[Zn3] + (N_KaNO31 +W_Theta1[Zn3])*(N_KaNH41+W_Theta1[Zn3])/(N_KaNO31 + N_FracNO31[Zn3] * (N_KaNH41-N_KaNO31) + W_Theta1[Zn3])+0*N_KaP1[Zn3]*S&B_RelPSorption[Zn3]
      # N_Ka1[Zn3,P] = N_KaP1[Zn3]*S&B_RelPSorption[Zn3]+ 0*(N_FracNO31[Zn3]+N_KaNH41+N_KaNO31+W_Theta1[Zn3])
      # N_Ka1[Zn4,N] = -W_Theta1[Zn4] + (N_KaNO31 +W_Theta1[Zn4])*(N_KaNH41+W_Theta1[Zn4])/(N_KaNO31 + N_FracNO31[Zn4] * (N_KaNH41-N_KaNO31) + W_Theta1[Zn4])+0*N_KaP1[Zn4]*S&B_RelPSorption[Zn4]
      # N_Ka1[Zn4,P] = N_KaP1[Zn4]*S&B_RelPSorption[Zn4]+ 0*(N_FracNO31[Zn4]+N_KaNH41+N_KaNO31+W_Theta1[Zn4])
      # 
      # N_Ka2[Zn1,N] = -W_Theta2[Zn1] + (N_KaNO32 +W_Theta2[Zn1])*(N_KaNH42+W_Theta2[Zn1])/(N_KaNO32+N_FracNO32[Zn1]*(N_KaNH42-N_KaNO32)+W_Theta2[Zn1])+0*N_KaP2[Zn1]
      # N_Ka2[Zn1,P] = N_KaP2[Zn1]+ 0*(N_FracNO32[Zn1]+N_KaNH42+N_KaNO32+W_Theta2[Zn1])
      # N_Ka2[Zn2,N] = -W_Theta2[Zn2] + (N_KaNO32 +W_Theta2[Zn2])*(N_KaNH42+W_Theta2[Zn2])/(N_KaNO32+N_FracNO32[Zn2]*(N_KaNH42-N_KaNO32)+W_Theta2[Zn2])+0*N_KaP2[Zn2]
      # N_Ka2[Zn2,P] = N_KaP2[Zn2]+ 0*(N_FracNO32[Zn2]+N_KaNH42+N_KaNO32+W_Theta2[Zn2])
      # N_Ka2[Zn3,N] = -W_Theta2[Zn3] + (N_KaNO32 +W_Theta2[Zn3])*(N_KaNH42+W_Theta2[Zn3])/(N_KaNO32+N_FracNO32[Zn3]*(N_KaNH42-N_KaNO32)+W_Theta2[Zn3])+0*N_KaP2[Zn3]
      # N_Ka2[Zn3,P] = N_KaP2[Zn3]+ 0*(N_FracNO32[Zn3]+N_KaNH42+N_KaNO32+W_Theta2[Zn3])
      # N_Ka2[Zn4,N] = -W_Theta2[Zn4] + (N_KaNO32 +W_Theta2[Zn4])*(N_KaNH42+W_Theta2[Zn4])/(N_KaNO32+N_FracNO32[Zn4]*(N_KaNH42-N_KaNO32)+W_Theta2[Zn4])+0*N_KaP2[Zn4]
      # N_Ka2[Zn4,P] = N_KaP2[Zn4]+ 0*(N_FracNO32[Zn4]+N_KaNH42+N_KaNO32+W_Theta2[Zn4])
      # N_Ka3[Zn1,N] = -W_Theta3[Zn1] + (N_KaNO33 +W_Theta3[Zn1])*(N_KaNH43+W_Theta3[Zn1])/(N_KaNO33+N_FracNO33[Zn1]*(N_KaNH43-N_KaNO33)+W_Theta3[Zn1])+0*N_KaP3[Zn1]
      # N_Ka3[Zn1,P] = N_KaP3[Zn1]+ 0*(N_FracNO33[Zn1]+N_KaNH43+N_KaNO33+W_Theta3[Zn1])
      # N_Ka3[Zn2,N] = -W_Theta3[Zn2] + (N_KaNO33 +W_Theta3[Zn2])*(N_KaNH43+W_Theta3[Zn2])/(N_KaNO33+N_FracNO33[Zn2]*(N_KaNH43-N_KaNO33)+W_Theta3[Zn2])+0*N_KaP3[Zn2]
      # N_Ka3[Zn2,P] = N_KaP3[Zn2]+ 0*(N_FracNO33[Zn2]+N_KaNH43+N_KaNO33+W_Theta3[Zn2])
      # N_Ka3[Zn3,N] = -W_Theta3[Zn3] + (N_KaNO33 +W_Theta3[Zn3])*(N_KaNH43+W_Theta3[Zn3])/(N_KaNO33+N_FracNO33[Zn3]*(N_KaNH43-N_KaNO33)+W_Theta3[Zn3])+0*N_KaP3[Zn3]
      # N_Ka3[Zn3,P] = N_KaP3[Zn3]+ 0*(N_FracNO33[Zn3]+N_KaNH43+N_KaNO33+W_Theta3[Zn3])
      # N_Ka3[Zn4,N] = -W_Theta3[Zn4] + (N_KaNO33 +W_Theta3[Zn4])*(N_KaNH43+W_Theta3[Zn4])/(N_KaNO33+N_FracNO33[Zn4]*(N_KaNH43-N_KaNO33)+W_Theta3[Zn4])+0*N_KaP3[Zn4]
      # N_Ka3[Zn4,P] = N_KaP3[Zn4]+ 0*(N_FracNO33[Zn4]+N_KaNH43+N_KaNO33+W_Theta3[Zn4])
      # N_Ka4[Zn1,N] = -W_Theta4[Zn1] + (N_KaNO34 +W_Theta4[Zn1])*(N_KaNH44+W_Theta4[Zn1])/(N_KaNO34+N_FracNO34[Zn1]*(N_KaNH44-N_KaNO34)+W_Theta4[Zn1])+0*N_KaP4[Zn1]
      # N_Ka4[Zn1,P] = N_KaP4[Zn1]+ 0*(N_FracNO34[Zn1]+N_KaNH44+N_KaNO34+W_Theta4[Zn1])
      # N_Ka4[Zn2,N] = -W_Theta4[Zn2] + (N_KaNO34 +W_Theta4[Zn2])*(N_KaNH44+W_Theta4[Zn2])/(N_KaNO34+N_FracNO34[Zn2]*(N_KaNH44-N_KaNO34)+W_Theta4[Zn2])+0*N_KaP4[Zn2]
      # N_Ka4[Zn2,P] = N_KaP4[Zn1]+ 0*(N_FracNO34[Zn2]+N_KaNH44+N_KaNO34+W_Theta4[Zn2])
      # N_Ka4[Zn3,N] = -W_Theta4[Zn3] + (N_KaNO34 +W_Theta4[Zn3])*(N_KaNH44+W_Theta4[Zn3])/(N_KaNO34+N_FracNO34[Zn3]*(N_KaNH44-N_KaNO34)+W_Theta4[Zn3])+0*N_KaP4[Zn3]
      # N_Ka4[Zn3,P] = N_KaP4[Zn3]+ 0*(N_FracNO34[Zn3]+N_KaNH44+N_KaNO34+W_Theta4[Zn3])
      # N_Ka4[Zn4,N] = -W_Theta4[Zn4] + (N_KaNO34 +W_Theta4[Zn4])*(N_KaNH44+W_Theta4[Zn4])/(N_KaNO34+N_FracNO34[Zn4]*(N_KaNH44-N_KaNO34)+W_Theta4[Zn4])+0*N_KaP4[Zn4]
      # N_Ka4[Zn4,P] = N_KaP4[Zn4]+ 0*(N_FracNO34[Zn4]+N_KaNH44+N_KaNO34+W_Theta4[Zn4])

      zonelayernut_df$N_Ka <- NA
      zonelayernut_df[zonelayernut_df$SlNut == "N", ]$N_Ka <- -zonelayer_df$W_Theta +
        (zonelayer_df$N_KaNO3 + zonelayer_df$W_Theta) * (zonelayer_df$N_KaNH4 + zonelayer_df$W_Theta) /
        (
          zonelayer_df$N_KaNO3 + zonelayer_df$N_FracNO3 * (zonelayer_df$N_KaNH4 -
                                                             zonelayer_df$N_KaNO3) + zonelayer_df$W_Theta
        )
      zonelayernut_df[zonelayernut_df$SlNut == "P", ]$N_Ka <- zonelayer_df$N_KaP
      zonelayernut_df[zonelayernut_df$SlNut == "P" & zonelayernut_df$layer == 1, ]$N_Ka <- zonelayer_df[zonelayer_df$layer == 1,]$N_KaP * zone_df$SB_RelPSorption
      
      # N_Cterm1[Zone,SlNut] = MAX(0,(N_Stock1[Zone,SlNut]-N_LeachConc1[Zone,SlNut]*W_Drain1[Zone]+MIN(0,Mn_Mineralization[Zone,SlNut]+N_SomMin1Exch[Zone,SlNut]))/(N_Ka1[Zone,SlNut]+W_Theta1[Zone]))
      # N_Cterm2[Zone,SlNut] = max(0,(N_Stock2[Zone,SlNut]                                        +min(0,N_In2[Zone,SlNut]            +N_SomMin2Exch[Zone,SlNut]))/(N_Ka2[Zone,SlNut]+W_Theta2[Zone]))
      # N_Cterm3[Zone,SlNut] = max(0,(N_Stock3[Zone,SlNut]                                        +min(0,N_In3[Zone,SlNut]            +N_SomMin3Exch[Zone,SlNut]))/(N_Ka3[Zone,SlNut]+W_Theta3[Zone]))
      # N_Cterm4[Zone,SlNut] = max(0,(N_Stock4[Zone,SlNut]                                        +min(0,N_In4[Zone,SlNut]            +N_SomMin4Exch[Zone,SlNut]))/(N_Ka4[Zone,SlNut]+W_Theta4[Zone]))
      zonelayernut_df$N_Stock_calc <- zonelayernut_df$N_Stock
      zl1n <- zonelayernut_df[zonelayernut_df$layer == 1,]
      zonelayernut_df[zonelayernut_df$layer == 1,]$N_Stock_calc <- zl1n$N_Stock - zl1n$N_LeachConc * rep(zonelayer_df[zonelayer_df$layer ==  1,]$W_Drain, nrow(nut_df)) 
      
      zonelayernut_df$N_In_calc <- zonelayernut_df$N_In
      zonelayernut_df[zonelayernut_df$layer == 1, ]$N_In_calc <- zonenut_df$Mn_Mineralization
      
      zonelayernut_df$N_Cterm <- pmax(
        0,
        (
          zonelayernut_df$N_Stock_calc + pmin(0, zonelayernut_df$N_In_calc + zonelayernut_df$N_SomMinExch)
        ) / (zonelayernut_df$N_Ka + zonelayernut_df$W_Theta)
      )
      
      # N_TPUptPot1[Zone,Tree] = IF (Rt_TLrv1[Zone,Tree]>0 AND Rt_T_G1[Zone,Tree]<>0) THEN (PI*Rt_TLrv1[Zone,Tree]*N_Diff1[Zone,P]*N_Cterm1[Zone,P]/(Rt_T_G1[Zone,Tree])) ELSE (0)
      # N_TPUptPot2[Zone,Tree] = IF (Rt_TLrv2[Zone,Tree]>0 AND Rt_T_G2[Zone,Tree]<>0) THEN (PI*Rt_TLrv2[Zone,Tree]*N_Diff2[Zone,P]*N_Cterm2[Zone,P]/(Rt_T_G2[Zone,Tree])) ELSE (0)
      # N_TPUptPot3[Zone,Tree] = IF (Rt_TLrv3[Zone,Tree]>0 AND Rt_T_G3[Zone,Tree]<>0) THEN (PI*Rt_TLrv3[Zone,Tree]*N_Diff3[Zone,P]*N_Cterm3[Zone,P]/(Rt_T_G3[Zone,Tree])) ELSE (0)
      # N_TPUptPot4[Zone,Tree] = IF (Rt_TLrv4[Zone,Tree]>0 AND Rt_T_G4[Zone,Tree]<>0) THEN (PI*Rt_TLrv4[Zone,Tree]*N_Diff4[Zone,P]*N_Cterm4[Zone,P]/(Rt_T_G4[Zone,Tree])) ELSE (0)
      zonelayertree_df$N_Diff_P <- rep(zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_Diff, ntree)
      zonelayertree_df$N_Cterm_P <- rep(zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_Cterm, ntree)
      zonelayertree_df$N_TPUptPot <- ifelse(
        zonelayertree_df$Rt_TLrv > 0 & zonelayertree_df$Rt_T_G != 0,
        pi * zonelayertree_df$Rt_TLrv * zonelayertree_df$N_Diff_P * zonelayertree_df$N_Cterm_P /
          zonelayertree_df$Rt_T_G,
        0
      )
      
      # T_NDeficit[SlNut,Tree] = MAX(0,T_NTarget[SlNut,Tree] - T_NBiom[SlNut,Tree])
      treenut_df$T_NDeficit <- pmax(0,treenut_df$T_NTarget - treenut_df$T_NBiom)

      # T_NFix[N,Sp1] = if T_NFixResp[Sp1]>10^-9 then (if INT(T_NFixVariable?[Sp1]) = 1 then (if T_NTargetNbiomRatio[N,Sp1]> 0 then min(T_NDeficit[N,Sp1]*(1 - (T_NTargetNbiomRatio[N,Sp1])^(T_NFixResp[Sp1])),T_GroRes[DW,Sp1]*T_NFixDWMaxFrac[Sp1]/T_NFixDWUnitCost[Sp1]) else 0) else T_NDeficit[N,Sp1]*T_NFixDayFrac[Sp1] ) else 0
      # T_NFix[N,Sp2] = if T_NFixResp[Sp2]>10^-7 then (if INT(T_NFixVariable?[Sp2]) = 1 then (if T_NTargetNbiomRatio[N,Sp2]> 0 then min(T_NDeficit[N,Sp2]*(1 - (T_NTargetNbiomRatio[N,Sp2])^(T_NFixResp[Sp2])),T_GroRes[DW,Sp2]*T_NFixDWMaxFrac[Sp2]/T_NFixDWUnitCost[Sp2]) else 0) else T_NDeficit[N,Sp2]*T_NFixDayFrac[Sp2] ) else 0
      # T_NFix[N,Sp3] = if T_NFixResp[Sp3]>10^-7 then (if INT(T_NFixVariable?[Sp3]) = 1 then (if T_NTargetNbiomRatio[N,Sp3]> 0 then min(T_NDeficit[N,Sp3]*(1 - (T_NTargetNbiomRatio[N,Sp3])^(T_NFixResp[Sp3])),T_GroRes[DW,Sp3]*T_NFixDWMaxFrac[Sp3]/T_NFixDWUnitCost[Sp3]) else 0) else T_NDeficit[N,Sp3]*T_NFixDayFrac[Sp3] ) else 0
      
      # T_NFix[P,Sp1] = 0*(T_NTargetNbiomRatio[P,Sp3]+T_NFixResp[Sp3]+T_GroRes[DW,Sp3]+T_NFixDWMaxFrac[Sp3]+T_NFixDWUnitCost[Sp3]+ T_NDeficit[P,Sp3]+T_NFixDayFrac[Sp3] +T_NFixVariable?[Sp1])
      # T_NFix[P,Sp2] = 0*(T_NTargetNbiomRatio[P,Sp3]+T_NFixResp[Sp3]+T_GroRes[DW,Sp3]+T_NFixDWMaxFrac[Sp3]+T_NFixDWUnitCost[Sp3]+ T_NDeficit[P,Sp3]+T_NFixDayFrac[Sp3] +T_NFixVariable?[Sp2])
      # T_NFix[P,Sp3] = 0*(T_NTargetNbiomRatio[P,Sp3]+T_NFixResp[Sp3]+T_GroRes[DW,Sp3]+T_NFixDWMaxFrac[Sp3]+T_NFixDWUnitCost[Sp3]+ T_NDeficit[P,Sp3]+T_NFixDayFrac[Sp3] +T_NFixVariable?[Sp3])
      
      treenut_df$T_NFix <- 0
      treenut_N <- treenut_df[treenut_df$SlNut == "N",]
      treenut_df[treenut_df$SlNut == "N", ]$T_NFix <- ifelse(
        tree_df$T_NFixResp > 10^-7,
        ifelse(
          floor(tree_df$T_NFixVariable_is) == 1,
          ifelse(
            treenut_N$T_NTargetNbiomRatio > 0,
            pmin(
              treenut_N$T_NDeficit * (1 - treenut_N$T_NTargetNbiomRatio^tree_df$T_NFixResp),
              treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_GroRes * tree_df$T_NFixDWMaxFrac /
                tree_df$T_NFixDWUnitCost
            ),
            0
          ),
          treenut_N$T_NDeficit * tree_df$T_NFixDayFrac
        ) ,
        0
      )

      # TP_NumberofParasites[Tree] = TP_IncludeTreeParasites?*TP_ParasitesPerM2ofBranch*(AF_ZoneFrac[Zn1]*Light_TBAI1[Zn1,Tree]+ AF_ZoneFrac[Zn2]*Light_TBAI1[Zn2,Tree]+ AF_ZoneFrac[Zn3]*Light_TBAI1[Zn3,Tree]+ AF_ZoneFrac[Zn4]*Light_TBAI1[Zn4,Tree])
      zonetree_df$Light_TBAI_frac <- zonetree_df$AF_ZoneFrac * zonelayertree_df[zonelayertree_df$layer == 1,]$Light_TBAI
      tree_df$TP_NumberofParasites <- pars$T_par$TP_IncludeTreeParasites_is * pars$T_par$TP_ParasitesPerM2ofBranch *
        (aggregate(zonetree_df["Light_TBAI_frac"], zonetree_df["tree_id"], sum)$Light_TBAI_frac)
      
      # TP_RelWaterSupply[Tree] = if TP_WaterDemand[Tree] = 0 then 0 else min(1,TW_UptTot[Tree]/TP_WaterDemand[Tree]) 
      tree_df$TP_RelWaterSupply <- ifelse( tree_df$TP_WaterDemand == 0, 0, pmin(1, tree_df$TW_UptTot/tree_df$TP_WaterDemand) )
      
      # TP_GrowthRate = TP_MaxSizePerparasite/TP_TimeToReachMaxSize
      TP_GrowthRate <- pars$T_par$TP_MaxSizePerparasite/pars$T_par$TP_TimeToReachMaxSize
      
      # TP_ParasiteGrowth[Tree,PlantComp] = If TP_NumberofParasites[Tree]>0 and TP_MaxSizePerparasite > 0 and TP_ParasiteBiomass[Tree,DW] > 0 then TP_RelWaterSupply[Tree]*TP_RelNutSupplyDelayed[Tree]*TP_GrowthRate*((TP_NumberofParasites[Tree]*TP_MaxSizePerparasite - TP_ParasiteBiomass[Tree,DW])/(TP_NumberofParasites[Tree]*TP_MaxSizePerparasite+TP_ParasiteBiomass[Tree,DW])) else 0
      treepcomp_df$TP_NumberofParasites <- rep(tree_df$TP_NumberofParasites, nrow(pcomp_df))
      treepcomp_df$TP_RelWaterSupply <- rep(tree_df$TP_RelWaterSupply, nrow(pcomp_df))
      treepcomp_df$TP_RelNutSupplyDelayed <- rep(tree_df$TP_RelNutSupplyDelayed, nrow(pcomp_df))
      treepcomp_df$TP_ParasiteBiomass_DW <- rep(treepcomp_df[treepcomp_df$PlantComp == "DW", ]$TP_ParasiteBiomass, nrow(pcomp_df))
      
      treepcomp_df$TP_ParasiteGrowth <- ifelse(
        treepcomp_df$TP_NumberofParasites > 0 &
          pars$T_par$TP_MaxSizePerparasite > 0 &
          treepcomp_df$TP_ParasiteBiomass_DW > 0,
        treepcomp_df$TP_RelWaterSupply *
          treepcomp_df$TP_RelNutSupplyDelayed * TP_GrowthRate *
          ((
            treepcomp_df$TP_NumberofParasites * pars$T_par$TP_MaxSizePerparasite - treepcomp_df$TP_ParasiteBiomass_DW
          ) /
            (
              treepcomp_df$TP_NumberofParasites * pars$T_par$TP_MaxSizePerparasite + treepcomp_df$TP_ParasiteBiomass_DW
            )
          ),
        0
      )
      
      # TP_Nutrient_Demand[Sp1,N] = TP_ParasiteGrowth[Sp1,DW]*TP_BiomNutrContent[N]
      # TP_Nutrient_Demand[Sp1,P] = TP_ParasiteGrowth[Sp1,DW]*TP_BiomNutrContent[P]
      # TP_Nutrient_Demand[Sp2,N] = TP_ParasiteGrowth[Sp2,DW]*TP_BiomNutrContent[N]
      # TP_Nutrient_Demand[Sp2,P] = TP_ParasiteGrowth[Sp2,DW]*TP_BiomNutrContent[P]
      # TP_Nutrient_Demand[Sp3,N] = TP_ParasiteGrowth[Sp3,DW]*TP_BiomNutrContent[N]
      # TP_Nutrient_Demand[Sp3,P] = TP_ParasiteGrowth[Sp3,DW]*TP_BiomNutrContent[P]
      treenut_df$TP_ParasiteGrowth_DW <- rep(treepcomp_df[treepcomp_df$PlantComp == "DW", ]$TP_ParasiteGrowth, nrow(nut_df))
      treenut_df$TP_BiomNutrContent <- rep(pcomp_df[pcomp_df$PlantComp %in% c("N", "P"),]$TP_BiomNutrContent, each = ntree)
      treenut_df$TP_Nutrient_Demand <- treenut_df$TP_ParasiteGrowth_DW * treenut_df$TP_BiomNutrContent

      # T_NDemand[SlNut,Tree] = if T_DiesToday?[Tree] = 1 or TW_Posgro[Tree]<0.01 then 0 else (T_NDeficit[SlNut,Tree] - T_NFix[SlNut,Tree])*T_NDemandFrac[Tree]+TP_Nutrient_Demand[Tree,SlNut]
      treenut_df$T_DiesToday_is <- rep(tree_df$T_DiesToday_is, nrow(nut_df))
      treenut_df$TW_Posgro <- rep(tree_df$TW_Posgro, nrow(nut_df))
      treenut_df$T_NDemandFrac <- rep(tree_df$T_NDemandFrac, nrow(nut_df))
      
      treenut_df$T_NDemand <- ifelse( treenut_df$T_DiesToday_is == 1 | treenut_df$TW_Posgro<0.01, 0, 
                                      (treenut_df$T_NDeficit - treenut_df$T_NFix)* treenut_df$T_NDemandFrac + treenut_df$TP_Nutrient_Demand)
      
      # T_NPDemandZn[Zone,Tree] = IF(Rt_TField[Tree]>0)THEN(Rt_TLra[Zone,Tree]*T_NDemand[P, Tree]/Rt_TField[Tree])ELSE(0)
      zonetree_df$Rt_TField <- rep(tree_df$Rt_TField, each = nzone)
      zonetree_df$T_NDemand_P <- rep(treenut_df[treenut_df$SlNut == "P"]$T_NDemand, each = nzone)
      zonetree_df$T_NPDemandZn <- ifelse(zonetree_df$Rt_TField>0, zonetree_df$Rt_TLra * zonetree_df$T_NDemand_P/zonetree_df$Rt_TField, 0)
      
      # T_NNDemandZn[Zone,Tree] = IF(Rt_TField[Tree]>0)THEN(Rt_TLra[Zone,Tree]*T_NDemand[N, Tree]/Rt_TField[Tree])ELSE(0)
      zonetree_df$T_NDemand_N <- rep(treenut_df[treenut_df$SlNut == "N", ]$T_NDemand, each = nzone)
      zonetree_df$T_NNDemandZn <- ifelse(zonetree_df$Rt_TField>0, zonetree_df$Rt_TLra * zonetree_df$T_NDemand_N/zonetree_df$Rt_TField, 0)
      
      # N_UptPot1[Zone,SlNut] = IF( (Rt_CLrv1[Zone]>0 OR ARRAYSUM(Rt_TLrv1[Zone,*])>0)AND Rt_TC_G1[Zone]<>0) THEN(MIN(N_Stock1[Zone,SlNut], (PI*(Rt_CLrv1[Zone]+ARRAYSUM(Rt_TLrv1[Zone,*]))*N_Diff1[Zone,SlNut]*N_Cterm1[Zone,N]/(Rt_TC_G1[Zone])))) ELSE (0)
      # N_UptPot2[Zone,SlNut] = IF( (Rt_CLrv2[Zone]>0 OR ARRAYSUM(Rt_TLrv2[Zone,*])>0)AND Rt_TC_G2[Zone]<>0) THEN(MIN(N_Stock2[Zone,SlNut], (PI*(Rt_CLrv2[Zone]+ARRAYSUM(Rt_TLrv2[Zone,*]))*N_Diff2[Zone,SlNut]*N_Cterm2[Zone,N]/(Rt_TC_G2[Zone])))) ELSE (0)
      # N_UptPot3[Zone,SlNut] = IF( (Rt_CLrv3[Zone]>0 OR ARRAYSUM(Rt_TLrv3[Zone,*])>0)AND Rt_TC_G3[Zone]<>0) THEN(MIN(N_Stock3[Zone,SlNut], (PI*(Rt_CLrv3[Zone]+ARRAYSUM(Rt_TLrv3[Zone,*]))*N_Diff3[Zone,SlNut]*N_Cterm3[Zone,N]/(Rt_TC_G3[Zone])))) ELSE (0)
      # N_UptPot4[Zone,SlNut] = IF( (Rt_CLrv4[Zone]>0 OR ARRAYSUM(Rt_TLrv4[Zone,*])>0)AND Rt_TC_G4[Zone]<>0) THEN(MIN(N_Stock4[Zone,SlNut], (PI*(Rt_CLrv4[Zone]+ARRAYSUM(Rt_TLrv4[Zone,*]))*N_Diff4[Zone,SlNut]*N_Cterm4[Zone,N]/(Rt_TC_G4[Zone])))) ELSE (0)
      zonelayernut_df$Rt_CLrv <- rep(zonelayer_df$Rt_CLrv, nrow(nut_df))
      zonelayernut_df$Rt_TLrv_sum <- rep(aggregate(zonelayertree_df["Rt_TLrv"], zonelayertree_df[c("zone", "layer")], sum)$Rt_TLrv, nrow(nut_df))
      zonelayernut_df$Rt_TC_G <- rep(zonelayer_df$Rt_TC_G, nrow(nut_df))
      zonelayernut_df$N_Cterm_N <- rep(zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_Cterm, nrow(nut_df))
      
      zonelayernut_df$N_UptPot <- ifelse(
        (zonelayernut_df$Rt_CLrv > 0 |
           zonelayernut_df$Rt_TLrv_sum > 0) &
          zonelayernut_df$Rt_TC_G != 0,
        pmin(
          zonelayernut_df$N_Stock,
          (
            pi * (zonelayernut_df$Rt_CLrv + zonelayernut_df$Rt_TLrv_sum) * zonelayernut_df$N_Diff * zonelayernut_df$N_Cterm_N /
              zonelayernut_df$Rt_TC_G
          )
        ),
        0
      )
      
      # N_CUptPot1[Zone,SlNut] = IF (Rt_CLrv1[Zone]>0 AND Rt_C_G1[Zone]<>0) THEN (PI*Rt_CLrv1[Zone]*N_Diff1[Zone,SlNut]*N_Cterm1[Zone,SlNut]/(Rt_C_G1[Zone])) ELSE (0)
      # N_CUptPot2[Zone,SlNut] = IF (Rt_CLrv2[Zone]>0 AND Rt_C_G2[Zone]<>0) THEN (PI*Rt_CLrv2[Zone]*N_Diff2[Zone,SlNut]*N_Cterm2[Zone,SlNut]/(Rt_C_G2[Zone])) ELSE (0)
      # N_CUptPot3[Zone,SlNut] = IF (Rt_CLrv3[Zone]>0 AND Rt_C_G3[Zone]<>0) THEN (PI*Rt_CLrv3[Zone]*N_Diff3[Zone,SlNut]*N_Cterm3[Zone,SlNut]/(Rt_C_G3[Zone])) ELSE (0)
      # N_CUptPot4[Zone,SlNut] = IF (Rt_CLrv4[Zone]>0 AND Rt_C_G4[Zone]<>0) THEN (PI*Rt_CLrv4[Zone]*N_Diff4[Zone,SlNut]*N_Cterm4[Zone,SlNut]/(Rt_C_G4[Zone])) ELSE (0)
      zonelayernut_df$Rt_C_G <- rep(zonelayer_df$Rt_C_G, nrow(nut_df))
      zonelayernut_df$N_CUptPot <- ifelse (
        zonelayernut_df$Rt_CLrv > 0 &
          zonelayernut_df$Rt_C_G != 0,
        pi * zonelayernut_df$Rt_CLrv * zonelayernut_df$N_Diff * zonelayernut_df$N_Cterm /
          zonelayernut_df$Rt_C_G,
        0
      )

      # CW_UptTot[Zone] = W_CUpt1[Zone]+W_CUpt2[Zone]+W_CUpt3[Zone]+W_CUpt4[Zone]
      zone_df$CW_UptTot <- aggregate(zonelayer_df["W_CUpt"], zonelayer_df["zone"], sum)$W_CUpt
      
      # CW_Posgro[Zone] = If (CW_DemandPot[Zone]>0 and AF_RunWatLim?>0.5) THEN (CW_UptTot[Zone]/CW_DemandPot[Zone]) ELSE (1)
      zone_df$CW_Posgro <- ifelse( zone_df$CW_DemandPot>0 & pars$AF_par$AF_RunWatLim_is>0.5,  zone_df$CW_UptTot/zone_df$CW_DemandPot, 1) 
      
      # C_NDeficit[Zone,SlNut] = MAX(0,C_NTarget[Zone,SlNut]-C_NBiom[Zone,SlNut])
      zonenut_df$C_NDeficit <- pmax(0, zonenut_df$C_NTarget-zonenut_df$C_NBiom)
      
      # C_NTargetNBiomRatio[Zone,SlNut] = IF(C_NTarget[Zone,SlNut]>0)THEN(C_NBiom[Zone,SlNut]/C_NTarget[Zone,SlNut])ELSE(0)
      zonenut_df$C_NTargetNBiomRatio <- ifelse(zonenut_df$C_NTarget>0, zonenut_df$C_NBiom/zonenut_df$C_NTarget, 0)
      
      # C_NFIX[Zone,SlNut] = if Cq_NFixRespCurr[Zone]>0 then 
      # (if Cq_NFixVariable?Curr[Zone] = 1 then 
      #   (if C_NTargetNBiomRatio[Zone,SlNut]> 0 then
      #     min(C_NDeficit[Zone,N]*(1 - (C_NTargetNBiomRatio[Zone,SlNut])^(Cq_NFixRespCurr[Zone])),C_GroRes[Zone,DW]*Cq_NFixDWMaxFracCurr[Zone]/Cq_NFixDWUnitCostCurr[Zone]) else 0) else
      #       C_NDeficit[Zone,SlNut]*Cq_NFixDailyFracCurr[Zone] ) else 0
      
      zonenut_df$Cq_NFixRespCurr <- rep(zone_df$Cq_NFixRespCurr, nrow(nut_df))
      zonenut_df$Cq_NFixVariable_is_Curr <- rep(zone_df$Cq_NFixVariable_is_Curr, nrow(nut_df))
      zonenut_df$C_NDeficit_N <- rep(zonenut_df[zonenut_df$SlNut == "N", ]$C_NDeficit, nrow(nut_df))
      zonenut_df$C_GroRes_DW <- rep(zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$C_GroRes, nrow((nut_df)))
      zonenut_df$Cq_NFixDWMaxFracCurr <- rep(zone_df$Cq_NFixDWMaxFracCurr, nrow((nut_df)))
      zonenut_df$Cq_NFixDWUnitCostCurr <- rep(zone_df$Cq_NFixDWUnitCostCurr, nrow((nut_df)))
      zonenut_df$Cq_NFixDailyFracCurr <-  rep(zone_df$Cq_NFixDailyFracCurr, nrow((nut_df)))
      
      zonenut_df$C_NFIX <- ifelse( zonenut_df$Cq_NFixRespCurr>0, 
      ifelse( zonenut_df$Cq_NFixVariable_is_Curr == 1,  
        ifelse( zonenut_df$C_NTargetNBiomRatio> 0, 
          pmin(zonenut_df$C_NDeficit_N*(1 - ( zonenut_df$C_NTargetNBiomRatio)^(zonenut_df$Cq_NFixRespCurr)), zonenut_df$C_GroRes_DW* zonenut_df$Cq_NFixDWMaxFracCurr/zonenut_df$Cq_NFixDWUnitCostCurr), 0), 
        zonenut_df$C_NDeficit* zonenut_df$Cq_NFixDailyFracCurr ), 0)
      
      
      # C_NDemand[Zone,SlNut] = if C_PlantDiesToday?[Zone] = 0 or CW_Posgro[Zone]>0.01 then MAX(0,(C_NDeficit[Zone,SlNut]-C_NFIX[Zone,SlNut])*(1 - 0.5 * Cq_Stage[Zone])^2) else 0
      zonenut_df$C_PlantDiesToday_is <- rep(zone_df$C_PlantDiesToday_is, nrow(nut_df))
      zonenut_df$CW_Posgro <- rep(zone_df$CW_Posgro, nrow(nut_df))
      zonenut_df$Cq_Stage <- rep(zone_df$Cq_Stage, nrow(nut_df))
      
      zonenut_df$C_NDemand <- ifelse(zonenut_df$C_PlantDiesToday_is == 0 | zonenut_df$CW_Posgro>0.01, pmax(0,(zonenut_df$C_NDeficit - zonenut_df$C_NFIX)*(1 - 0.5 * zonenut_df$Cq_Stage)^2), 0)
      
      # N_RhizTShare1[Zone,Tree] = if N_RhizEffect1[Zone] > 0 and (ARRAYSUM(Rt_TLrvMinM1[Zone,*])+Rt_CLrvMinM1[Zone]) > 0 then
      # (1-N_RtSynloc1)*N_TRhizKaPM1[Zone,Tree]/(N_RhizEffect1[Zone])+N_RtSynloc1*Rt_TLrvMinM1[Zone,Tree]/(ARRAYSUM(Rt_TLrvMinM1[Zone,*])+Rt_CLrvMinM1[Zone]) else 0
      # N_RhizTShare2[Zone,Tree] = if N_RhizEffect2[Zone] > 0 and (ARRAYSUM(Rt_TLrvMinM2[Zone,*])+Rt_CLrvMinM2[Zone]) > 0 then
      # (1-N_RtSynloc2)*N_TRhizKaPM2[Zone,Tree]/(N_RhizEffect2[Zone])+N_RtSynloc2*Rt_TLrvMinM2[Zone,Tree]/(ARRAYSUM(Rt_TLrvMinM2[Zone,*])+Rt_CLrvMinM2[Zone]) else 0
      # N_RhizTShare3[Zone,Tree] = if N_RhizEffect3[Zone] > 0 and (ARRAYSUM(Rt_TLrvMinM3[Zone,*])+Rt_CLrvMinM3[Zone]) > 0 then
      # (1-N_RtSynloc3)*N_TRhizKaPM3[Zone,Tree]/(N_RhizEffect3[Zone])+N_RtSynloc3*Rt_TLrvMinM3[Zone,Tree]/(ARRAYSUM(Rt_TLrvMinM3[Zone,*])+Rt_CLrvMinM3[Zone]) else 0
      # N_RhizTShare4[Zone,Tree] = if N_RhizEffect4[Zone] > 0 and (ARRAYSUM(Rt_TLrvMinM4[Zone,*])+Rt_CLrvMinM4[Zone]) > 0 then
      # (1-N_RtSynloc4)*N_TRhizKaPM4[Zone,Tree]/(N_RhizEffect4[Zone])+N_RtSynloc4*Rt_TLrvMinM4[Zone,Tree]/(ARRAYSUM(Rt_TLrvMinM4[Zone,*])+Rt_CLrvMinM4[Zone]) else 0
      
      zonelayertree_df$Rt_TLrvMinM_sum <- aggregate(zonelayertree_df["Rt_TLrvMinM"], zonelayertree_df[c("zone", "layer")], sum)$Rt_TLrvMinM
      zonelayertree_df$N_RhizEffect <- rep(zonelayer_df$N_RhizEffect, ntree)
      zonelayertree_df$Rt_CLrvMinM <- rep(zonelayer_df$Rt_CLrvMinM, ntree)
      zonelayertree_df$N_RtSynloc <- rep(rep(layer_df$N_RtSynloc, each = nzone), ntree)
      
      zonelayertree_df$N_RhizTShare <- ifelse(
        zonelayertree_df$N_RhizEffect > 0 &
          (
            zonelayertree_df$Rt_TLrvMinM_sum + zonelayertree_df$Rt_CLrvMinM
          ) > 0,
        (1 - zonelayertree_df$N_RtSynloc) * zonelayertree_df$N_TRhizKaPM /
          zonelayertree_df$N_RhizEffect + zonelayertree_df$N_RtSynloc * zonelayertree_df$Rt_TLrvMinM /
          (
            zonelayertree_df$Rt_TLrvMinM_sum + zonelayertree_df$Rt_CLrvMinM
          ),
        0
      )
      
      # N_TTPUptPot1[Zone,Tree] = IF(N_TPUptPot1[Zone,Tree]>0 AND T_NPDemandZn[Zone,Tree]>0) THEN  N_UptPot1[Zone,P] * ((N_TPUptPot1[Zone,Tree]* T_NPDemandZn[Zone,Tree] / (N_CUptPot1[Zone,P] * C_NDemand[Zone,P] + N_TPUptPot1[Zone,Tree] * T_NPDemandZn[Zone,Tree])) + N_RhizEffect1[Zone] * N_RhizTShare1[Zone,Tree]) / (1 + N_RhizEffect1[Zone]) ELSE (0)
      # N_TTPUptPot2[Zone,Tree] = IF(N_TPUptPot2[Zone,Tree]>0 AND T_NPDemandZn[Zone,Tree]>0) THEN  N_UptPot2[Zone,P] * ((N_TPUptPot2[Zone,Tree]* T_NPDemandZn[Zone,Tree] / (N_CUptPot2[Zone,P] * C_NDemand[Zone,P] + N_TPUptPot2[Zone,Tree] * T_NPDemandZn[Zone,Tree])) + N_RhizEffect2[Zone] * N_RhizTShare2[Zone,Tree]) / (1 + N_RhizEffect2[Zone]) ELSE (0)
      # N_TTPUptPot3[Zone,Tree] = IF(N_TPUptPot3[Zone,Tree]>0 AND T_NPDemandZn[Zone,Tree]>0) THEN  N_UptPot3[Zone,P] * ((N_TPUptPot3[Zone,Tree]* T_NPDemandZn[Zone,Tree] / (N_CUptPot3[Zone,P] * C_NDemand[Zone,P] + N_TPUptPot3[Zone,Tree] * T_NPDemandZn[Zone,Tree])) + N_RhizEffect3[Zone] * N_RhizTShare3[Zone,Tree]) / (1 + N_RhizEffect3[Zone]) ELSE (0)
      # N_TTPUptPot4[Zone,Tree] = IF(N_TPUptPot4[Zone,Tree]>0 AND T_NPDemandZn[Zone,Tree]>0) THEN  N_UptPot4[Zone,P] * ((N_TPUptPot4[Zone,Tree]* T_NPDemandZn[Zone,Tree] / (N_CUptPot4[Zone,P] * C_NDemand[Zone,P] + N_TPUptPot4[Zone,Tree] * T_NPDemandZn[Zone,Tree])) + N_RhizEffect4[Zone] * N_RhizTShare4[Zone,Tree]) / (1 + N_RhizEffect4[Zone]) ELSE (0)
      
      df <- zonetree_df[c("zone", "tree_id", "T_NPDemandZn", "T_NNDemandZn")]
      t_df <- df[rep(seq_len(nrow(df)), nlayer), ]
      t_df$layer <- rep(layer_df$layer, each = nrow(zonetree_df))
      t_df <- t_df[order(t_df$tree_id, t_df$layer, t_df$zone), ]
      zonelayertree_df[c("T_NPDemandZn", "T_NNDemandZn")] <- t_df[c("T_NPDemandZn", "T_NNDemandZn")]
      
      zonelayertree_df$N_UptPot_P <- rep(zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_UptPot, ntree)
      zonelayertree_df$N_UptPot_N <- rep(zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_UptPot, ntree)
      zonelayertree_df$N_CUptPot_P <- rep(zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_CUptPot, ntree)
      zonelayertree_df$N_CUptPot_N <- rep(zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_CUptPot, ntree)
      zonelayertree_df$C_NDemand_P <- rep(zonenut_df[zonenut_df$SlNut == "P",]$C_NDemand, nlayer*ntree)
      zonelayertree_df$C_NDemand_N <- rep(zonenut_df[zonenut_df$SlNut == "N",]$C_NDemand, nlayer*ntree)
      zonelayertree_df$N_RhizEffect <- rep(zonelayer_df$N_RhizEffect, ntree)
      
      zonelayertree_df$N_TTPUptPot <- ifelse(zonelayertree_df$N_TPUptPot>0 & zonelayertree_df$T_NPDemandZn>0,
                                             zonelayertree_df$N_UptPot_P * ((zonelayertree_df$N_TPUptPot * zonelayertree_df$T_NPDemandZn / 
                                                                               (zonelayertree_df$N_CUptPot_P * zonelayertree_df$C_NDemand_P + 
                                                                                  zonelayertree_df$N_TPUptPot * zonelayertree_df$T_NPDemandZn)) + 
                                                                              zonelayertree_df$N_RhizEffect * zonelayertree_df$N_RhizTShare) / (1 + zonelayertree_df$N_RhizEffect), 0)
      
      # N_TPUptPotAct1[Zone,Tree] = MIN(N_TTPUptPot1[Zone,Tree],N_TPUptPot1[Zone,Tree])
      # N_TPUptPotAct2[Zone,Tree] = MIN(N_TTPUptPot2[Zone,Tree],N_TPUptPot2[Zone,Tree])
      # N_TPUptPotAct3[Zone,Tree] = MIN(N_TTPUptPot3[Zone,Tree],N_TPUptPot3[Zone,Tree])
      # N_TPUptPotAct4[Zone,Tree] = MIN(N_TTPUptPot4[Zone,Tree],N_TPUptPot4[Zone,Tree])
      zonelayertree_df$N_TPUptPotAct <- pmin(zonelayertree_df$N_TTPUptPot,zonelayertree_df$N_TPUptPot)

      # T_NPUptPot[Tree] = (N_TPUptPotAct1[Zn1, Tree]+N_TPUptPotAct2[Zn1, Tree]+N_TPUptPotAct3[Zn1, Tree]+N_TPUptPotAct4[Zn1, Tree])*AF_ZoneFrac[Zn1]+(N_TPUptPotAct1[Zn2, Tree]+N_TPUptPotAct2[Zn2, Tree]+N_TPUptPotAct3[Zn2, Tree]+N_TPUptPotAct4[Zn2, Tree])*AF_ZoneFrac[Zn2]+(N_TPUptPotAct1[Zn3, Tree]+N_TPUptPotAct2[Zn3, Tree]+N_TPUptPotAct3[Zn3, Tree]+N_TPUptPotAct4[Zn3, Tree])*AF_ZoneFrac[Zn3]+(N_TPUptPotAct1[Zn4, Tree]+N_TPUptPotAct2[Zn4, Tree]+N_TPUptPotAct3[Zn4, Tree]+N_TPUptPotAct4[Zn4, Tree])*AF_ZoneFrac[Zn4]
      zonetree_df$N_TPUptPotAct_sumf <- aggregate(zonelayertree_df["N_TPUptPotAct"], zonelayertree_df[c("zone", "tree_id")], sum)$N_TPUptPotAct * zonetree_df$AF_ZoneFrac
      tree_df$T_NPUptPot <- aggregate(zonetree_df["N_TPUptPotAct_sumf"], zonetree_df["tree_id"], sum)$N_TPUptPotAct_sumf
      
      # N_TNUptPot1[Zone,Tree] = IF (Rt_TLrv1[Zone,Tree]>0 AND Rt_T_G1[Zone,Tree]<>0) THEN (PI*Rt_TLrv1[Zone,Tree]*N_Diff1[Zone,N]*N_Cterm1[Zone,N]/(Rt_T_G1[Zone,Tree])) ELSE (0)
      # N_TNUptPot2[Zone,Tree] = IF (Rt_TLrv2[Zone,Tree]>0 AND Rt_T_G2[Zone,Tree]<>0) THEN (PI*Rt_TLrv2[Zone,Tree]*N_Diff2[Zone,N]*N_Cterm2[Zone,N]/(Rt_T_G2[Zone,Tree])) ELSE (0)
      # N_TNUptPot3[Zone,Tree] = IF (Rt_TLrv3[Zone,Tree]>0 AND Rt_T_G3[Zone,Tree]<>0) THEN (PI*Rt_TLrv3[Zone,Tree]*N_Diff3[Zone,N]*N_Cterm3[Zone,N]/(Rt_T_G3[Zone,Tree])) ELSE (0)
      # N_TNUptPot4[Zone,Tree] = IF (Rt_TLrv4[Zone,Tree]>0 AND Rt_T_G4[Zone,Tree]<>0) THEN (PI*Rt_TLrv4[Zone,Tree]*N_Diff4[Zone,N]*N_Cterm4[Zone,N]/(Rt_T_G4[Zone,Tree])) ELSE (0)
      zonelayertree_df$N_Diff_N <- rep(zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_Diff, ntree)
      zonelayertree_df$N_Cterm_N <- rep(zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_Cterm, ntree)
      
      zonelayertree_df$N_TNUptPot <- ifelse(zonelayertree_df$Rt_TLrv>0 & zonelayertree_df$Rt_T_G != 0, 
                                            pi* zonelayertree_df$Rt_TLrv* zonelayertree_df$N_Diff_N* zonelayertree_df$N_Cterm_N/(zonelayertree_df$Rt_T_G), 0)
      
      # N_TTNUptPot1[Zone,Tree] = IF(N_TNUptPot1[Zone,Tree]>0 AND T_NNDemandZn[Zone,Tree]>0) THEN
      # N_UptPot1[Zone,N] * ((N_TNUptPot1[Zone,Tree]* T_NNDemandZn[Zone,Tree] / (N_CUptPot1[Zone,N] * C_NDemand[Zone,N] + N_TNUptPot1[Zone,Tree] * T_NNDemandZn[Zone,Tree]))) ELSE (0)
      # N_TTNUptPot2[Zone,Tree] = IF(N_TNUptPot2[Zone,Tree]>0 AND T_NNDemandZn[Zone,Tree]>0) THEN
      # N_UptPot2[Zone,N] * ((N_TNUptPot2[Zone,Tree]* T_NNDemandZn[Zone,Tree] / (N_CUptPot2[Zone,N] * C_NDemand[Zone,N] + N_TNUptPot2[Zone,Tree] * T_NNDemandZn[Zone,Tree]))) ELSE (0)
      # N_TTNUptPot3[Zone,Tree] = IF(N_TNUptPot3[Zone,Tree]>0 AND T_NNDemandZn[Zone,Tree]>0) THEN
      # N_UptPot3[Zone,N] * ((N_TNUptPot3[Zone,Tree]* T_NNDemandZn[Zone,Tree] / (N_CUptPot3[Zone,N] * C_NDemand[Zone,N] + N_TNUptPot3[Zone,Tree] * T_NNDemandZn[Zone,Tree]))) ELSE (0)
      # N_TTNUptPot4[Zone,Tree] = IF(N_TNUptPot4[Zone,Tree]>0 AND T_NNDemandZn[Zone,Tree]>0) THEN
      # N_UptPot4[Zone,N] * ((N_TNUptPot4[Zone,Tree]* T_NNDemandZn[Zone,Tree] / (N_CUptPot4[Zone,N] * C_NDemand[Zone,N] + N_TNUptPot4[Zone,Tree] * T_NNDemandZn[Zone,Tree]))) ELSE (0)
      zonelayertree_df$N_TTNUptPot <- ifelse(
        zonelayertree_df$N_TNUptPot > 0 & zonelayertree_df$T_NNDemandZn > 0,
        zonelayertree_df$N_UptPot_N * (
          zonelayertree_df$N_TNUptPot * zonelayertree_df$T_NNDemandZn /
            (
              zonelayertree_df$N_CUptPot_N * zonelayertree_df$C_NDemand_N + zonelayertree_df$N_TNUptPot * zonelayertree_df$T_NNDemandZn
            )
        ),
        0
      )

      # N_TNUptPotAct1[Zone,Tree] = MIN(N_TTNUptPot1[Zone,Tree],N_TNUptPot1[Zone,Tree])
      # N_TNUptPotAct2[Zone,Tree] = MIN(N_TTNUptPot2[Zone,Tree],N_TNUptPot2[Zone,Tree])
      # N_TNUptPotAct3[Zone,Tree] = MIN(N_TTNUptPot3[Zone,Tree],N_TNUptPot3[Zone,Tree])
      # N_TNUptPotAct4[Zone,Tree] = MIN(N_TTNUptPot4[Zone,Tree],N_TNUptPot4[Zone,Tree])
      zonelayertree_df$N_TNUptPotAct <- pmin(zonelayertree_df$N_TTNUptPot, zonelayertree_df$N_TNUptPot)

      # T_NNUptPotZn[Zone,Tree] = (N_TNUptPotAct1[Zone,Tree]+ N_TNUptPotAct2[Zone,Tree]+N_TNUptPotAct3[Zone,Tree]+N_TNUptPotAct4[Zone,Tree])*AF_ZoneFrac[Zone]
      zonetree_df$T_NNUptPotZn <- aggregate(zonelayertree_df["N_TNUptPotAct"], zonelayertree_df[c("zone", "tree_id")], sum)$N_TNUptPotAct * zonetree_df$AF_ZoneFrac

      # T_NNUptPot[Tree] = ARRAYSUM(T_NNUptPotZn[*,Tree])
      tree_df$T_NNUptPot <- aggregate(zonetree_df["T_NNUptPotZn"], zonetree_df["tree_id"], sum)$T_NNUptPotZn

      # T_NNUptDenoZn[Zone,Tree] = (Rt_TLrv1[Zone,Tree]*N_TNUptPotAct1[Zone,Tree]+Rt_TLrv2[Zone,Tree]*N_TNUptPotAct2[Zone,Tree]+Rt_TLrv3[Zone,Tree]*N_TNUptPotAct3[Zone,Tree]+Rt_TLrv4[Zone,Tree]*N_TNUptPotAct4[Zone,Tree])*AF_ZoneWidth[Zone]
      zonelayertree_df$N_TNUptPotAct_Rt <- zonelayertree_df$Rt_TLrv* zonelayertree_df$N_TNUptPotAct
      zonetree_df$AF_ZoneWidth <- rep(zone_df$AF_ZoneWidth, ntree)
      zonetree_df$T_NNUptDenoZn <- aggregate(zonelayertree_df["N_TNUptPotAct_Rt"], zonelayertree_df[c("zone", "tree_id")], sum)$N_TNUptPotAct_Rt*zonetree_df$AF_ZoneWidth
      
      
      # T_NNUptDeno[Tree] = ARRAYSUM(T_NNUptDenoZn[*,Tree])
      tree_df$T_NNUptDeno <- aggregate(zonetree_df["T_NNUptDenoZn"], zonetree_df[ "tree_id"], sum)$T_NNUptDenoZn
      
      # T_NPUptDenoZn[Zone,Tree] = (Rt_TLrv1[Zone,Tree]*N_TPUptPotAct1[Zone,Tree]+Rt_TLrv2[Zone,Tree]*N_TPUptPotAct2[Zone,Tree]+Rt_TLrv3[Zone,Tree]*N_TPUptPotAct3[Zone,Tree]+Rt_TLrv4[Zone,Tree]*N_TPUptPotAct4[Zone,Tree])*AF_ZoneWidth[Zone]
      zonelayertree_df$N_TPUptPotAct_Rt <- zonelayertree_df$Rt_TLrv* zonelayertree_df$N_TPUptPotAct
      zonetree_df$T_NPUptDenoZn <- aggregate(zonelayertree_df["N_TPUptPotAct_Rt"], zonelayertree_df[c("zone", "tree_id")], sum)$N_TPUptPotAct_Rt*zonetree_df$AF_ZoneWidth

      # T_NPUptDeno[Tree] = ARRAYSUM(T_NPUptDenoZn[*,Tree])
      tree_df$T_NPUptDeno <- aggregate(zonetree_df["T_NPUptDenoZn"], zonetree_df[ "tree_id"], sum)$T_NPUptDenoZn
      
      # N_T1Upt1[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp1]+(1-N_N?[SlNut])*T_NPUptPot[Sp1])> T_NDemand[SlNut,Sp1] THEN min(N_Stock1[Zone,SlNut],(T_NDemand[SlNut,Sp1]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct1[Zone,Sp1]+(1-N_N?[SlNut])*N_TPUptPotAct1[Zone,Sp1])*AF_ZoneWidth[Zone]*Rt_TLrv1[Zone,Sp1]/(N_N?[SlNut]*T_NNUptDeno[Sp1]+(1-N_N?[SlNut])*T_NPUptDeno[Sp1]))) ELSE min(N_Stock1[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct1[Zone,Sp1]+(1-N_N?[SlNut])*N_TPUptPotAct1[Zone,Sp1])))ELSE 0
      # N_T2Upt1[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp2]+(1-N_N?[SlNut])*T_NPUptPot[Sp2])> T_NDemand[SlNut,Sp2] THEN min(N_Stock1[Zone,SlNut],(T_NDemand[SlNut,Sp2]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct1[Zone,Sp2]+(1-N_N?[SlNut])*N_TPUptPotAct1[Zone,Sp2])*AF_ZoneWidth[Zone]*Rt_TLrv1[Zone,Sp2]/(N_N?[SlNut]*T_NNUptDeno[Sp2]+(1-N_N?[SlNut])*T_NPUptDeno[Sp2]))) ELSE min(N_Stock1[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct1[Zone,Sp2]+(1-N_N?[SlNut])*N_TPUptPotAct1[Zone,Sp2])))ELSE 0
      # N_T3Upt1[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp3]+(1-N_N?[SlNut])*T_NPUptPot[Sp3])> T_NDemand[SlNut,Sp3] THEN min(N_Stock1[Zone,SlNut],(T_NDemand[SlNut,Sp3]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct1[Zone,Sp3]+(1-N_N?[SlNut])*N_TPUptPotAct1[Zone,Sp3])*AF_ZoneWidth[Zone]*Rt_TLrv1[Zone,Sp3]/(N_N?[SlNut]*T_NNUptDeno[Sp3]+(1-N_N?[SlNut])*T_NPUptDeno[Sp3]))) ELSE min(N_Stock1[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct1[Zone,Sp3]+(1-N_N?[SlNut])*N_TPUptPotAct1[Zone,Sp3])))ELSE 0
      # 
      # N_T1Upt2[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp1]+(1-N_N?[SlNut])*T_NPUptPot[Sp1])> T_NDemand[SlNut,Sp1] THEN min(N_Stock2[Zone,SlNut],(T_NDemand[SlNut,Sp1]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct2[Zone,Sp1]+(1-N_N?[SlNut])*N_TPUptPotAct2[Zone,Sp1])*AF_ZoneWidth[Zone]*Rt_TLrv2[Zone,Sp1]/(N_N?[SlNut]*T_NNUptDeno[Sp1]+(1-N_N?[SlNut])*T_NPUptDeno[Sp1]))) ELSE min(N_Stock2[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct2[Zone,Sp1]+(1-N_N?[SlNut])*N_TPUptPotAct2[Zone,Sp1])))ELSE 0
      # N_T2Upt2[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp2]+(1-N_N?[SlNut])*T_NPUptPot[Sp2])> T_NDemand[SlNut,Sp2] THEN min(N_Stock2[Zone,SlNut],(T_NDemand[SlNut,Sp2]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct2[Zone,Sp2]+(1-N_N?[SlNut])*N_TPUptPotAct2[Zone,Sp2])*AF_ZoneWidth[Zone]*Rt_TLrv2[Zone,Sp2]/(N_N?[SlNut]*T_NNUptDeno[Sp2]+(1-N_N?[SlNut])*T_NPUptDeno[Sp2]))) ELSE min(N_Stock2[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct2[Zone,Sp2]+(1-N_N?[SlNut])*N_TPUptPotAct2[Zone,Sp2])))ELSE 0
      # N_T3Upt2[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp3]+(1-N_N?[SlNut])*T_NPUptPot[Sp3])> T_NDemand[SlNut,Sp3] THEN min(N_Stock2[Zone,SlNut],(T_NDemand[SlNut,Sp3]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct2[Zone,Sp3]+(1-N_N?[SlNut])*N_TPUptPotAct2[Zone,Sp3])*AF_ZoneWidth[Zone]*Rt_TLrv2[Zone,Sp3]/(N_N?[SlNut]*T_NNUptDeno[Sp3]+(1-N_N?[SlNut])*T_NPUptDeno[Sp3]))) ELSE min(N_Stock2[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct2[Zone,Sp3]+(1-N_N?[SlNut])*N_TPUptPotAct2[Zone,Sp3])))ELSE 0
      # 
      # N_T1Upt3[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp1]+(1-N_N?[SlNut])*T_NPUptPot[Sp1])> T_NDemand[SlNut,Sp1] THEN min(N_Stock3[Zone,SlNut],(T_NDemand[SlNut,Sp1]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct3[Zone,Sp1]+(1-N_N?[SlNut])*N_TPUptPotAct3[Zone,Sp1])*AF_ZoneWidth[Zone]*Rt_TLrv3[Zone,Sp1]/(N_N?[SlNut]*T_NNUptDeno[Sp1]+(1-N_N?[SlNut])*T_NPUptDeno[Sp1]))) ELSE min(N_Stock3[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct3[Zone,Sp1]+(1-N_N?[SlNut])*N_TPUptPotAct3[Zone,Sp1])))ELSE 0
      # N_T2Upt3[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp2]+(1-N_N?[SlNut])*T_NPUptPot[Sp2])> T_NDemand[SlNut,Sp2] THEN min(N_Stock3[Zone,SlNut],(T_NDemand[SlNut,Sp2]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct3[Zone,Sp2]+(1-N_N?[SlNut])*N_TPUptPotAct3[Zone,Sp2])*AF_ZoneWidth[Zone]*Rt_TLrv3[Zone,Sp2]/(N_N?[SlNut]*T_NNUptDeno[Sp2]+(1-N_N?[SlNut])*T_NPUptDeno[Sp2]))) ELSE min(N_Stock3[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct3[Zone,Sp2]+(1-N_N?[SlNut])*N_TPUptPotAct3[Zone,Sp2])))ELSE 0
      # N_T3Upt3[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp3]+(1-N_N?[SlNut])*T_NPUptPot[Sp3])> T_NDemand[SlNut,Sp3] THEN min(N_Stock3[Zone,SlNut],(T_NDemand[SlNut,Sp3]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct3[Zone,Sp3]+(1-N_N?[SlNut])*N_TPUptPotAct3[Zone,Sp3])*AF_ZoneWidth[Zone]*Rt_TLrv3[Zone,Sp3]/(N_N?[SlNut]*T_NNUptDeno[Sp3]+(1-N_N?[SlNut])*T_NPUptDeno[Sp3]))) ELSE min(N_Stock3[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct3[Zone,Sp3]+(1-N_N?[SlNut])*N_TPUptPotAct3[Zone,Sp3])))ELSE 0
      # 
      # N_T1Upt4[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp1]+(1-N_N?[SlNut])*T_NPUptPot[Sp1])> T_NDemand[SlNut,Sp1] THEN min(N_Stock4[Zone,SlNut],(T_NDemand[SlNut,Sp1]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct4[Zone,Sp1]+(1-N_N?[SlNut])*N_TPUptPotAct4[Zone,Sp1])*AF_ZoneWidth[Zone]*Rt_TLrv4[Zone,Sp1]/(N_N?[SlNut]*T_NNUptDeno[Sp1]+(1-N_N?[SlNut])*T_NPUptDeno[Sp1]))) ELSE min(N_Stock4[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct4[Zone,Sp1]+(1-N_N?[SlNut])*N_TPUptPotAct4[Zone,Sp1])))ELSE 0
      # N_T2Upt4[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp2]+(1-N_N?[SlNut])*T_NPUptPot[Sp2])> T_NDemand[SlNut,Sp2] THEN min(N_Stock4[Zone,SlNut],(T_NDemand[SlNut,Sp2]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct4[Zone,Sp2]+(1-N_N?[SlNut])*N_TPUptPotAct4[Zone,Sp2])*AF_ZoneWidth[Zone]*Rt_TLrv4[Zone,Sp2]/(N_N?[SlNut]*T_NNUptDeno[Sp2]+(1-N_N?[SlNut])*T_NPUptDeno[Sp2]))) ELSE min(N_Stock4[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct4[Zone,Sp2]+(1-N_N?[SlNut])*N_TPUptPotAct4[Zone,Sp2])))ELSE 0
      # N_T3Upt4[Zone,SlNut] = If (AF_ZoneFrac[Zone]>0) THEN (IF (N_N?[SlNut]*T_NNUptPot[Sp3]+(1-N_N?[SlNut])*T_NPUptPot[Sp3])> T_NDemand[SlNut,Sp3] THEN min(N_Stock4[Zone,SlNut],(T_NDemand[SlNut,Sp3]/AF_ZoneFrac[Zone]*(N_N?[SlNut]*N_TNUptPotAct4[Zone,Sp3]+(1-N_N?[SlNut])*N_TPUptPotAct4[Zone,Sp3])*AF_ZoneWidth[Zone]*Rt_TLrv4[Zone,Sp3]/(N_N?[SlNut]*T_NNUptDeno[Sp3]+(1-N_N?[SlNut])*T_NPUptDeno[Sp3]))) ELSE min(N_Stock4[Zone,SlNut],(N_N?[SlNut]*N_TNUptPotAct4[Zone,Sp3]+(1-N_N?[SlNut])*N_TPUptPotAct4[Zone,Sp3])))ELSE 0
      
      zonelayertreenut_df$AF_ZoneFrac <- rep(zonelayertree_df$AF_ZoneFrac, nrow(nut_df))
      zonelayertreenut_df$N_N_is <- rep(nut_df$N_N_is, each = nrow(zonelayertree_df))
      zonelayertreenut_df$T_NNUptPot <- rep(rep(tree_df$T_NNUptPot, each = nzone*nlayer), nrow(nut_df))
      zonelayertreenut_df$T_NPUptPot <- rep(rep(tree_df$T_NPUptPot, each = nzone*nlayer), nrow(nut_df))
      zonelayertreenut_df$T_NNUptDeno <- rep(rep(tree_df$T_NNUptDeno, each = nzone*nlayer), nrow(nut_df))
      zonelayertreenut_df$T_NPUptDeno <- rep(rep(tree_df$T_NPUptDeno, each = nzone*nlayer), nrow(nut_df))
      zonelayertreenut_df$T_NDemand <- rep(treenut_df$T_NDemand, each = nzone*nlayer)
      zonelayertreenut_df$N_Stock <- c(rep(zonelayernut_df[zonelayernut_df$SlNut == "N",]$N_Stock, ntree), rep(zonelayernut_df[zonelayernut_df$SlNut == "P",]$N_Stock, ntree))
      zonelayertreenut_df$N_TNUptPotAct <- rep(zonelayertree_df$N_TNUptPotAct, nrow(nut_df))
      zonelayertreenut_df$N_TPUptPotAct <- rep(zonelayertree_df$N_TPUptPotAct, nrow(nut_df))
      zonelayertreenut_df$AF_ZoneWidth <- rep(zonelayertree_df$AF_ZoneWidth, nrow(nut_df))
      zonelayertreenut_df$Rt_TLrv <- rep(zonelayertree_df$Rt_TLrv, nrow(nut_df))
      
      zonelayertreenut_df$N_Upt <- ifelse(
        zonelayertreenut_df$AF_ZoneFrac > 0,
        ifelse(
          (
            zonelayertreenut_df$N_N_is * zonelayertreenut_df$T_NNUptPot + (1 - zonelayertreenut_df$N_N_is) * zonelayertreenut_df$T_NPUptPot
          ) > zonelayertreenut_df$T_NDemand,
          pmin(
            zonelayertreenut_df$N_Stock,
            (
              zonelayertreenut_df$T_NDemand /
                zonelayertreenut_df$AF_ZoneFrac *
                (
                  zonelayertreenut_df$N_N_is * zonelayertreenut_df$N_TNUptPotAct + (1 - zonelayertreenut_df$N_N_is) * zonelayertreenut_df$N_TPUptPotAct
                ) * zonelayertreenut_df$AF_ZoneWidth * zonelayertreenut_df$Rt_TLrv /
                (
                  zonelayertreenut_df$N_N_is * zonelayertreenut_df$T_NNUptDeno + (1 - zonelayertreenut_df$N_N_is) *
                    zonelayertreenut_df$T_NPUptDeno
                )
            )
          ),
          pmin(
            zonelayertreenut_df$N_Stock,
            (
              zonelayertreenut_df$N_N_is * zonelayertreenut_df$N_TNUptPotAct + (1 - zonelayertreenut_df$N_N_is) *
                zonelayertreenut_df$N_TPUptPotAct
            )
          )
        ),
        0
      )

      # T_NUptTot[N,Sp1] =  AF_ZoneFrac[Zn1]* (N_T1Upt1[Zn1,N]+N_T1Upt2[Zn1,N]+N_T1Upt3[Zn1,N]+N_T1Upt4[Zn1,N])+ AF_ZoneFrac[Zn2]*  (N_T1Upt1[Zn2,N]+N_T1Upt2[Zn2,N]+N_T1Upt3[Zn2,N]+N_T1Upt4[Zn2,N])+ AF_ZoneFrac[Zn3]*  (N_T1Upt1[Zn3,N]+N_T1Upt2[Zn3,N]+N_T1Upt3[Zn3,N]+N_T1Upt4[Zn3,N])+ AF_ZoneFrac[Zn4]*   (N_T1Upt1[Zn4,N]+N_T1Upt2[Zn4,N]+N_T1Upt3[Zn4,N]+N_T1Upt4[Zn4,N])  + 0*(N_T2Upt1[Zn1,N]+N_T2Upt2[Zn1,N]+N_T2Upt3[Zn1,N]+N_T2Upt4[Zn1,N]+ N_T3Upt1[Zn1,N]+N_T3Upt2[Zn1,N]+N_T3Upt3[Zn1,N]+N_T3Upt4[Zn1,N]) 
      # T_NUptTot[N,Sp2] =  AF_ZoneFrac[Zn1]* (N_T2Upt1[Zn1,N]+N_T2Upt2[Zn1,N]+N_T2Upt3[Zn1,N]+N_T2Upt4[Zn1,N])+ AF_ZoneFrac[Zn2]*  (N_T2Upt1[Zn2,N]+N_T2Upt2[Zn2,N]+N_T2Upt3[Zn2,N]+N_T2Upt4[Zn2,N])+ AF_ZoneFrac[Zn3]*  (N_T2Upt1[Zn3,N]+N_T2Upt2[Zn3,N]+N_T2Upt3[Zn3,N]+N_T2Upt4[Zn3,N])+ AF_ZoneFrac[Zn4]*   (N_T2Upt1[Zn4,N]+N_T2Upt2[Zn4,N]+N_T2Upt3[Zn4,N]+N_T2Upt4[Zn4,N])  + 0*(N_T1Upt1[Zn1,N]+N_T1Upt2[Zn1,N]+N_T1Upt3[Zn1,N]+N_T1Upt4[Zn1,N]+ N_T3Upt1[Zn1,N]+N_T3Upt2[Zn1,N]+N_T3Upt3[Zn1,N]+N_T3Upt4[Zn1,N]) 
      # T_NUptTot[N,Sp3] =  AF_ZoneFrac[Zn1]* (N_T3Upt1[Zn1,N]+N_T3Upt2[Zn1,N]+N_T3Upt3[Zn1,N]+N_T3Upt4[Zn1,N])+ AF_ZoneFrac[Zn2]*  (N_T3Upt1[Zn2,N]+N_T3Upt2[Zn2,N]+N_T3Upt3[Zn2,N]+N_T3Upt4[Zn2,N])+ AF_ZoneFrac[Zn3]*  (N_T3Upt1[Zn3,N]+N_T3Upt2[Zn3,N]+N_T3Upt3[Zn3,N]+N_T3Upt4[Zn3,N])+ AF_ZoneFrac[Zn4]*   (N_T3Upt1[Zn4,N]+N_T3Upt2[Zn4,N]+N_T3Upt3[Zn4,N]+N_T3Upt4[Zn4,N])  + 0*(N_T2Upt1[Zn1,N]+N_T2Upt2[Zn1,N]+N_T2Upt3[Zn1,N]+N_T2Upt4[Zn1,N]+ N_T1Upt1[Zn1,N]+N_T1Upt2[Zn1,N]+N_T1Upt3[Zn1,N]+N_T1Upt4[Zn1,N]) 
      # T_NUptTot[P,Sp1] =  AF_ZoneFrac[Zn1]* (N_T1Upt1[Zn1,P]+N_T1Upt2[Zn1,P]+N_T1Upt3[Zn1,P]+N_T1Upt4[Zn1,P])+ AF_ZoneFrac[Zn2]*  (N_T1Upt1[Zn2,P]+N_T1Upt2[Zn2,P]+N_T1Upt3[Zn2,P]+N_T1Upt4[Zn2,P])+ AF_ZoneFrac[Zn3]*  (N_T1Upt1[Zn3,P]+N_T1Upt2[Zn3,P]+N_T1Upt3[Zn3,P]+N_T1Upt4[Zn3,P])+ AF_ZoneFrac[Zn4]*   (N_T1Upt1[Zn4,P]+N_T1Upt2[Zn4,P]+N_T1Upt3[Zn4,P]+N_T1Upt4[Zn4,P])  + 0*(N_T2Upt1[Zn1,P]+N_T2Upt2[Zn1,P]+N_T2Upt3[Zn1,P]+N_T2Upt4[Zn1,P]+ N_T3Upt1[Zn1,P]+N_T3Upt2[Zn1,P]+N_T3Upt3[Zn1,P]+N_T3Upt4[Zn1,P]) 
      # T_NUptTot[P,Sp2] =  AF_ZoneFrac[Zn1]* (N_T2Upt1[Zn1,P]+N_T2Upt2[Zn1,P]+N_T2Upt3[Zn1,P]+N_T2Upt4[Zn1,P])+ AF_ZoneFrac[Zn2]*  (N_T2Upt1[Zn2,P]+N_T2Upt2[Zn2,P]+N_T2Upt3[Zn2,P]+N_T2Upt4[Zn2,P])+ AF_ZoneFrac[Zn3]*  (N_T2Upt1[Zn3,P]+N_T2Upt2[Zn3,P]+N_T2Upt3[Zn3,P]+N_T2Upt4[Zn3,P])+ AF_ZoneFrac[Zn4]*   (N_T2Upt1[Zn4,P]+N_T2Upt2[Zn4,P]+N_T2Upt3[Zn4,P]+N_T2Upt4[Zn4,P])  + 0*(N_T1Upt1[Zn1,P]+N_T1Upt2[Zn1,P]+N_T1Upt3[Zn1,P]+N_T1Upt4[Zn1,P]+ N_T3Upt1[Zn1,P]+N_T3Upt2[Zn1,P]+N_T3Upt3[Zn1,P]+N_T3Upt4[Zn1,P]) 
      # T_NUptTot[P,Sp3] =  AF_ZoneFrac[Zn1]* (N_T3Upt1[Zn1,P]+N_T3Upt2[Zn1,P]+N_T3Upt3[Zn1,P]+N_T3Upt4[Zn1,P])+ AF_ZoneFrac[Zn2]*  (N_T3Upt1[Zn2,P]+N_T3Upt2[Zn2,P]+N_T3Upt3[Zn2,P]+N_T3Upt4[Zn2,P])+ AF_ZoneFrac[Zn3]*  (N_T3Upt1[Zn3,P]+N_T3Upt2[Zn3,P]+N_T3Upt3[Zn3,P]+N_T3Upt4[Zn3,P])+ AF_ZoneFrac[Zn4]*   (N_T3Upt1[Zn4,P]+N_T3Upt2[Zn4,P]+N_T3Upt3[Zn4,P]+N_T3Upt4[Zn4,P])  + 0*(N_T2Upt1[Zn1,P]+N_T2Upt2[Zn1,P]+N_T2Upt3[Zn1,P]+N_T2Upt4[Zn1,P]+ N_T1Upt1[Zn1,P]+N_T1Upt2[Zn1,P]+N_T1Upt3[Zn1,P]+N_T1Upt4[Zn1,P]) 
      zonetreenut_df$N_TUpt_ZnFrac <- aggregate(zonelayertreenut_df["N_Upt"], zonelayertreenut_df[c("zone", "tree_id", "SlNut")], sum)$N_Upt * rep(zone_df$AF_ZoneFrac, nrow(treenut_df))
      treenut_df$T_NUptTot <- aggregate(zonetreenut_df["N_TUpt_ZnFrac"], zonetreenut_df[c("tree_id", "SlNut")], sum)$N_TUpt_ZnFrac
      
      # RT3_AvgNutUptperRoot[SlNut,Tree] = if Rt_TField[Tree]>0 then T_NUptTot[SlNut,Tree]/Rt_TField[Tree] else 0
      treenut_df$Rt_TField <- rep(tree_df$Rt_TField, nrow(nut_df))
      treenut_df$RT3_AvgNutUptperRoot <- ifelse(treenut_df$Rt_TField > 0 ,
                                                treenut_df$T_NUptTot / treenut_df$Rt_TField,
                                                0)
      
      # RT3_L1N_RelUptperLrv[Zn1,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth1[Zn1,Sp1]> 0 then  (N_T1Upt1[Zn1,N]/RT3_LrvDepth1[Zn1,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt1[Zn1,N]+N_T3Upt1[Zn1,N])
      # RT3_L1N_RelUptperLrv[Zn1,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth1[Zn1,Sp2]> 0 then  (N_T2Upt1[Zn1,N]/RT3_LrvDepth1[Zn1,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt1[Zn1,N]+N_T3Upt1[Zn1,N])
      # RT3_L1N_RelUptperLrv[Zn1,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth1[Zn1,Sp3]> 0 then  (N_T3Upt1[Zn1,N]/RT3_LrvDepth1[Zn1,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt1[Zn1,N]+N_T2Upt1[Zn1,N])
      # RT3_L1N_RelUptperLrv[Zn2,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth1[Zn2,Sp1]> 0 then  (N_T1Upt1[Zn2,N]/RT3_LrvDepth1[Zn2,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt1[Zn2,N]+N_T3Upt1[Zn2,N])
      # RT3_L1N_RelUptperLrv[Zn2,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth1[Zn2,Sp2]> 0 then  (N_T2Upt1[Zn2,N]/RT3_LrvDepth1[Zn2,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt1[Zn2,N]+N_T3Upt1[Zn2,N])
      # RT3_L1N_RelUptperLrv[Zn2,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth1[Zn2,Sp3]> 0 then  (N_T3Upt1[Zn2,N]/RT3_LrvDepth1[Zn2,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt1[Zn2,N]+N_T2Upt1[Zn2,N])
      # RT3_L1N_RelUptperLrv[Zn3,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth1[Zn3,Sp1]> 0 then  (N_T1Upt1[Zn3,N]/RT3_LrvDepth1[Zn3,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt1[Zn3,N]+N_T3Upt1[Zn3,N])
      # RT3_L1N_RelUptperLrv[Zn3,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth1[Zn3,Sp2]> 0 then  (N_T2Upt1[Zn3,N]/RT3_LrvDepth1[Zn3,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt1[Zn3,N]+N_T3Upt1[Zn3,N])
      # RT3_L1N_RelUptperLrv[Zn3,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth1[Zn3,Sp3]> 0 then  (N_T3Upt1[Zn3,N]/RT3_LrvDepth1[Zn3,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt1[Zn3,N]+N_T2Upt1[Zn3,N])
      # RT3_L1N_RelUptperLrv[Zn4,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth1[Zn4,Sp1]> 0 then  (N_T1Upt1[Zn4,N]/RT3_LrvDepth1[Zn4,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt1[Zn4,N]+N_T3Upt1[Zn4,N])
      # RT3_L1N_RelUptperLrv[Zn4,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth1[Zn4,Sp2]> 0 then  (N_T2Upt1[Zn4,N]/RT3_LrvDepth1[Zn4,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt1[Zn4,N]+N_T3Upt1[Zn4,N])
      # RT3_L1N_RelUptperLrv[Zn4,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth1[Zn4,Sp3]> 0 then  (N_T3Upt1[Zn4,N]/RT3_LrvDepth1[Zn4,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt1[Zn4,N]+N_T2Upt1[Zn4,N])
      # 
      # RT3_L1P_RelUptperLrv[Zn1,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth1[Zn1,Sp1]> 0 then  (N_T1Upt1[Zn1,P]/RT3_LrvDepth1[Zn1,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt1[Zn1,P]+N_T3Upt1[Zn1,P])
      # RT3_L1P_RelUptperLrv[Zn1,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth1[Zn1,Sp2]> 0 then  (N_T2Upt1[Zn1,P]/RT3_LrvDepth1[Zn1,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt1[Zn1,P]+N_T3Upt1[Zn1,P])
      # RT3_L1P_RelUptperLrv[Zn1,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth1[Zn1,Sp3]> 0 then  (N_T3Upt1[Zn1,P]/RT3_LrvDepth1[Zn1,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt1[Zn1,P]+N_T2Upt1[Zn1,P])
      # RT3_L1P_RelUptperLrv[Zn2,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth1[Zn2,Sp1]> 0 then  (N_T1Upt1[Zn2,P]/RT3_LrvDepth1[Zn2,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt1[Zn2,P]+N_T3Upt1[Zn2,P])
      # RT3_L1P_RelUptperLrv[Zn2,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth1[Zn2,Sp2]> 0 then  (N_T2Upt1[Zn2,P]/RT3_LrvDepth1[Zn2,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt1[Zn2,P]+N_T3Upt1[Zn2,P])
      # RT3_L1P_RelUptperLrv[Zn2,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth1[Zn2,Sp3]> 0 then  (N_T3Upt1[Zn2,P]/RT3_LrvDepth1[Zn2,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt1[Zn2,P]+N_T2Upt1[Zn2,P])
      # RT3_L1P_RelUptperLrv[Zn3,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth1[Zn3,Sp1]> 0 then  (N_T1Upt1[Zn3,P]/RT3_LrvDepth1[Zn3,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt1[Zn3,P]+N_T3Upt1[Zn3,P])
      # RT3_L1P_RelUptperLrv[Zn3,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth1[Zn3,Sp2]> 0 then  (N_T2Upt1[Zn3,P]/RT3_LrvDepth1[Zn3,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt1[Zn3,P]+N_T3Upt1[Zn3,P])
      # RT3_L1P_RelUptperLrv[Zn3,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth1[Zn3,Sp3]> 0 then  (N_T3Upt1[Zn3,P]/RT3_LrvDepth1[Zn3,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt1[Zn3,P]+N_T2Upt1[Zn3,P])
      # RT3_L1P_RelUptperLrv[Zn4,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth1[Zn4,Sp1]> 0 then  (N_T1Upt1[Zn4,P]/RT3_LrvDepth1[Zn4,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt1[Zn4,P]+N_T3Upt1[Zn4,P])
      # RT3_L1P_RelUptperLrv[Zn4,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth1[Zn4,Sp2]> 0 then  (N_T2Upt1[Zn4,P]/RT3_LrvDepth1[Zn4,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt1[Zn4,P]+N_T3Upt1[Zn4,P])
      # RT3_L1P_RelUptperLrv[Zn4,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth1[Zn4,Sp3]> 0 then  (N_T3Upt1[Zn4,P]/RT3_LrvDepth1[Zn4,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt1[Zn4,P]+N_T2Upt1[Zn4,P])
      # 
      # 
      # RT3_L2N_RelUptperLrv[Zn1,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth2[Zn1,Sp1]> 0 then  (N_T1Upt2[Zn1,N]/RT3_LrvDepth2[Zn1,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt2[Zn1,N]+N_T3Upt2[Zn1,N])
      # RT3_L2N_RelUptperLrv[Zn1,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth2[Zn1,Sp2]> 0 then  (N_T2Upt2[Zn1,N]/RT3_LrvDepth2[Zn1,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt2[Zn1,N]+N_T3Upt2[Zn1,N])
      # RT3_L2N_RelUptperLrv[Zn1,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth2[Zn1,Sp3]> 0 then  (N_T3Upt2[Zn1,N]/RT3_LrvDepth2[Zn1,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt2[Zn1,N]+N_T2Upt2[Zn1,N])
      # RT3_L2N_RelUptperLrv[Zn2,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth2[Zn2,Sp1]> 0 then  (N_T1Upt2[Zn2,N]/RT3_LrvDepth2[Zn2,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt2[Zn2,N]+N_T3Upt2[Zn2,N])
      # RT3_L2N_RelUptperLrv[Zn2,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth2[Zn2,Sp2]> 0 then  (N_T2Upt2[Zn2,N]/RT3_LrvDepth2[Zn2,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt2[Zn2,N]+N_T3Upt2[Zn2,N])
      # RT3_L2N_RelUptperLrv[Zn2,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth2[Zn2,Sp3]> 0 then  (N_T3Upt2[Zn2,N]/RT3_LrvDepth2[Zn2,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt2[Zn2,N]+N_T2Upt2[Zn2,N])
      # RT3_L2N_RelUptperLrv[Zn3,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth2[Zn3,Sp1]> 0 then  (N_T1Upt2[Zn3,N]/RT3_LrvDepth2[Zn3,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt2[Zn3,N]+N_T3Upt2[Zn3,N])
      # RT3_L2N_RelUptperLrv[Zn3,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth2[Zn3,Sp2]> 0 then  (N_T2Upt2[Zn3,N]/RT3_LrvDepth2[Zn3,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt2[Zn3,N]+N_T3Upt2[Zn3,N])
      # RT3_L2N_RelUptperLrv[Zn3,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth2[Zn3,Sp3]> 0 then  (N_T3Upt2[Zn3,N]/RT3_LrvDepth2[Zn3,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt2[Zn3,N]+N_T2Upt2[Zn3,N])
      # RT3_L2N_RelUptperLrv[Zn4,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth2[Zn4,Sp1]> 0 then  (N_T1Upt2[Zn4,N]/RT3_LrvDepth2[Zn4,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt2[Zn4,N]+N_T3Upt2[Zn4,N])
      # RT3_L2N_RelUptperLrv[Zn4,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth2[Zn4,Sp2]> 0 then  (N_T2Upt2[Zn4,N]/RT3_LrvDepth2[Zn4,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt2[Zn4,N]+N_T3Upt2[Zn4,N])
      # RT3_L2N_RelUptperLrv[Zn4,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth2[Zn4,Sp3]> 0 then  (N_T3Upt2[Zn4,N]/RT3_LrvDepth2[Zn4,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt2[Zn4,N]+N_T2Upt2[Zn4,N])
      # 
      # RT3_L2P_RelUptperLrv[Zn1,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth2[Zn1,Sp1]> 0 then  (N_T1Upt2[Zn1,P]/RT3_LrvDepth2[Zn1,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt2[Zn1,P]+N_T3Upt2[Zn1,P])
      # RT3_L2P_RelUptperLrv[Zn1,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth2[Zn1,Sp2]> 0 then  (N_T2Upt2[Zn1,P]/RT3_LrvDepth2[Zn1,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt2[Zn1,P]+N_T3Upt2[Zn1,P])
      # RT3_L2P_RelUptperLrv[Zn1,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth2[Zn1,Sp3]> 0 then  (N_T3Upt2[Zn1,P]/RT3_LrvDepth2[Zn1,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt2[Zn1,P]+N_T2Upt2[Zn1,P])
      # RT3_L2P_RelUptperLrv[Zn2,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth2[Zn2,Sp1]> 0 then  (N_T1Upt2[Zn2,P]/RT3_LrvDepth2[Zn2,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt2[Zn2,P]+N_T3Upt2[Zn2,P])
      # RT3_L2P_RelUptperLrv[Zn2,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth2[Zn2,Sp2]> 0 then  (N_T2Upt2[Zn2,P]/RT3_LrvDepth2[Zn2,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt2[Zn2,P]+N_T3Upt2[Zn2,P])
      # RT3_L2P_RelUptperLrv[Zn2,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth2[Zn2,Sp3]> 0 then  (N_T3Upt2[Zn2,P]/RT3_LrvDepth2[Zn2,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt2[Zn2,P]+N_T2Upt2[Zn2,P])
      # RT3_L2P_RelUptperLrv[Zn3,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth2[Zn3,Sp1]> 0 then  (N_T1Upt2[Zn3,P]/RT3_LrvDepth2[Zn3,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt2[Zn3,P]+N_T3Upt2[Zn3,P])
      # RT3_L2P_RelUptperLrv[Zn3,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth2[Zn3,Sp2]> 0 then  (N_T2Upt2[Zn3,P]/RT3_LrvDepth2[Zn3,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt2[Zn3,P]+N_T3Upt2[Zn3,P])
      # RT3_L2P_RelUptperLrv[Zn3,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth2[Zn3,Sp3]> 0 then  (N_T3Upt2[Zn3,P]/RT3_LrvDepth2[Zn3,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt2[Zn3,P]+N_T2Upt2[Zn3,P])
      # RT3_L2P_RelUptperLrv[Zn4,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth2[Zn4,Sp1]> 0 then  (N_T1Upt2[Zn4,P]/RT3_LrvDepth2[Zn4,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt2[Zn4,P]+N_T3Upt2[Zn4,P])
      # RT3_L2P_RelUptperLrv[Zn4,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth2[Zn4,Sp2]> 0 then  (N_T2Upt2[Zn4,P]/RT3_LrvDepth2[Zn4,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt2[Zn4,P]+N_T3Upt2[Zn4,P])
      # RT3_L2P_RelUptperLrv[Zn4,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth2[Zn4,Sp3]> 0 then  (N_T3Upt2[Zn4,P]/RT3_LrvDepth2[Zn4,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt2[Zn4,P]+N_T2Upt2[Zn4,P])
      # 
      # 
      # RT3_L3N_RelUptperLrv[Zn1,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth3[Zn1,Sp1]> 0 then  (N_T1Upt3[Zn1,N]/RT3_LrvDepth3[Zn1,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt3[Zn1,N]+N_T3Upt3[Zn1,N])
      # RT3_L3N_RelUptperLrv[Zn1,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth3[Zn1,Sp2]> 0 then  (N_T2Upt3[Zn1,N]/RT3_LrvDepth3[Zn1,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt3[Zn1,N]+N_T3Upt3[Zn1,N])
      # RT3_L3N_RelUptperLrv[Zn1,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth3[Zn1,Sp3]> 0 then  (N_T3Upt3[Zn1,N]/RT3_LrvDepth3[Zn1,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt3[Zn1,N]+N_T2Upt3[Zn1,N])
      # RT3_L3N_RelUptperLrv[Zn2,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth3[Zn2,Sp1]> 0 then  (N_T1Upt3[Zn2,N]/RT3_LrvDepth3[Zn2,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt3[Zn2,N]+N_T3Upt3[Zn2,N])
      # RT3_L3N_RelUptperLrv[Zn2,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth3[Zn2,Sp2]> 0 then  (N_T2Upt3[Zn2,N]/RT3_LrvDepth3[Zn2,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt3[Zn2,N]+N_T3Upt3[Zn2,N])
      # RT3_L3N_RelUptperLrv[Zn2,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth3[Zn2,Sp3]> 0 then  (N_T3Upt3[Zn2,N]/RT3_LrvDepth3[Zn2,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt3[Zn2,N]+N_T2Upt3[Zn2,N])
      # RT3_L3N_RelUptperLrv[Zn3,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth3[Zn3,Sp1]> 0 then  (N_T1Upt3[Zn3,N]/RT3_LrvDepth3[Zn3,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt3[Zn3,N]+N_T3Upt3[Zn3,N])
      # RT3_L3N_RelUptperLrv[Zn3,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth3[Zn3,Sp2]> 0 then  (N_T2Upt3[Zn3,N]/RT3_LrvDepth3[Zn3,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt3[Zn3,N]+N_T3Upt3[Zn3,N])
      # RT3_L3N_RelUptperLrv[Zn3,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth3[Zn3,Sp3]> 0 then  (N_T3Upt3[Zn3,N]/RT3_LrvDepth3[Zn3,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt3[Zn3,N]+N_T2Upt3[Zn3,N])
      # RT3_L3N_RelUptperLrv[Zn4,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth3[Zn4,Sp1]> 0 then  (N_T1Upt3[Zn4,N]/RT3_LrvDepth3[Zn4,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt3[Zn4,N]+N_T3Upt3[Zn4,N])
      # RT3_L3N_RelUptperLrv[Zn4,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth3[Zn4,Sp2]> 0 then  (N_T2Upt3[Zn4,N]/RT3_LrvDepth3[Zn4,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt3[Zn4,N]+N_T3Upt3[Zn4,N])
      # RT3_L3N_RelUptperLrv[Zn4,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth3[Zn4,Sp3]> 0 then  (N_T3Upt3[Zn4,N]/RT3_LrvDepth3[Zn4,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt3[Zn4,N]+N_T2Upt3[Zn4,N])
      # 
      # RT3_L3P_RelUptperLrv[Zn1,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth3[Zn1,Sp1]> 0 then  (N_T1Upt3[Zn1,P]/RT3_LrvDepth3[Zn1,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt3[Zn1,P]+N_T3Upt3[Zn1,P])
      # RT3_L3P_RelUptperLrv[Zn1,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth3[Zn1,Sp2]> 0 then  (N_T2Upt3[Zn1,P]/RT3_LrvDepth3[Zn1,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt3[Zn1,P]+N_T3Upt3[Zn1,P])
      # RT3_L3P_RelUptperLrv[Zn1,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth3[Zn1,Sp3]> 0 then  (N_T3Upt3[Zn1,P]/RT3_LrvDepth3[Zn1,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt3[Zn1,P]+N_T2Upt3[Zn1,P])
      # RT3_L3P_RelUptperLrv[Zn2,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth3[Zn2,Sp1]> 0 then  (N_T1Upt3[Zn2,P]/RT3_LrvDepth3[Zn2,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt3[Zn2,P]+N_T3Upt3[Zn2,P])
      # RT3_L3P_RelUptperLrv[Zn2,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth3[Zn2,Sp2]> 0 then  (N_T2Upt3[Zn2,P]/RT3_LrvDepth3[Zn2,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt3[Zn2,P]+N_T3Upt3[Zn2,P])
      # RT3_L3P_RelUptperLrv[Zn2,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth3[Zn2,Sp3]> 0 then  (N_T3Upt3[Zn2,P]/RT3_LrvDepth3[Zn2,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt3[Zn2,P]+N_T2Upt3[Zn2,P])
      # RT3_L3P_RelUptperLrv[Zn3,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth3[Zn3,Sp1]> 0 then  (N_T1Upt3[Zn3,P]/RT3_LrvDepth3[Zn3,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt3[Zn3,P]+N_T3Upt3[Zn3,P])
      # RT3_L3P_RelUptperLrv[Zn3,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth3[Zn3,Sp2]> 0 then  (N_T2Upt3[Zn3,P]/RT3_LrvDepth3[Zn3,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt3[Zn3,P]+N_T3Upt3[Zn3,P])
      # RT3_L3P_RelUptperLrv[Zn3,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth3[Zn3,Sp3]> 0 then  (N_T3Upt3[Zn3,P]/RT3_LrvDepth3[Zn3,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt3[Zn3,P]+N_T2Upt3[Zn3,P])
      # RT3_L3P_RelUptperLrv[Zn4,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth3[Zn4,Sp1]> 0 then  (N_T1Upt3[Zn4,P]/RT3_LrvDepth3[Zn4,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt3[Zn4,P]+N_T3Upt3[Zn4,P])
      # RT3_L3P_RelUptperLrv[Zn4,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth3[Zn4,Sp2]> 0 then  (N_T2Upt3[Zn4,P]/RT3_LrvDepth3[Zn4,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt3[Zn4,P]+N_T3Upt3[Zn4,P])
      # RT3_L3P_RelUptperLrv[Zn4,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth3[Zn4,Sp3]> 0 then  (N_T3Upt3[Zn4,P]/RT3_LrvDepth3[Zn4,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt3[Zn4,P]+N_T2Upt3[Zn4,P])
      # 
      # 
      # RT3_L4N_RelUptperLrv[Zn1,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth4[Zn1,Sp1]> 0 then  (N_T1Upt4[Zn1,N]/RT3_LrvDepth4[Zn1,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt4[Zn1,N]+N_T3Upt4[Zn1,N])
      # RT3_L4N_RelUptperLrv[Zn1,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth4[Zn1,Sp2]> 0 then  (N_T2Upt4[Zn1,N]/RT3_LrvDepth4[Zn1,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt4[Zn1,N]+N_T3Upt4[Zn1,N])
      # RT3_L4N_RelUptperLrv[Zn1,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth4[Zn1,Sp3]> 0 then  (N_T3Upt4[Zn1,N]/RT3_LrvDepth4[Zn1,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt4[Zn1,N]+N_T2Upt4[Zn1,N])
      # RT3_L4N_RelUptperLrv[Zn2,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth4[Zn2,Sp1]> 0 then  (N_T1Upt4[Zn2,N]/RT3_LrvDepth4[Zn2,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt4[Zn2,N]+N_T3Upt4[Zn2,N])
      # RT3_L4N_RelUptperLrv[Zn2,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth4[Zn2,Sp2]> 0 then  (N_T2Upt4[Zn2,N]/RT3_LrvDepth4[Zn2,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt4[Zn2,N]+N_T3Upt4[Zn2,N])
      # RT3_L4N_RelUptperLrv[Zn2,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth4[Zn2,Sp3]> 0 then  (N_T3Upt4[Zn2,N]/RT3_LrvDepth4[Zn2,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt4[Zn2,N]+N_T2Upt4[Zn2,N])
      # RT3_L4N_RelUptperLrv[Zn3,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth4[Zn3,Sp1]> 0 then  (N_T1Upt4[Zn3,N]/RT3_LrvDepth4[Zn3,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt4[Zn3,N]+N_T3Upt4[Zn3,N])
      # RT3_L4N_RelUptperLrv[Zn3,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth4[Zn3,Sp2]> 0 then  (N_T2Upt4[Zn3,N]/RT3_LrvDepth4[Zn3,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt4[Zn3,N]+N_T3Upt4[Zn3,N])
      # RT3_L4N_RelUptperLrv[Zn3,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth4[Zn3,Sp3]> 0 then  (N_T3Upt4[Zn3,N]/RT3_LrvDepth4[Zn3,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt4[Zn3,N]+N_T2Upt4[Zn3,N])
      # RT3_L4N_RelUptperLrv[Zn4,Sp1] = if  RT3_AvgNutUptperRoot[N,Sp1] > 0 and RT3_LrvDepth4[Zn4,Sp1]> 0 then  (N_T1Upt4[Zn4,N]/RT3_LrvDepth4[Zn4,Sp1])/RT3_AvgNutUptperRoot[N,Sp1] else 0*(N_T2Upt4[Zn4,N]+N_T3Upt4[Zn4,N])
      # RT3_L4N_RelUptperLrv[Zn4,Sp2] = if  RT3_AvgNutUptperRoot[N,Sp2] > 0 and RT3_LrvDepth4[Zn4,Sp2]> 0 then  (N_T2Upt4[Zn4,N]/RT3_LrvDepth4[Zn4,Sp2])/RT3_AvgNutUptperRoot[N,Sp2] else 0*(N_T1Upt4[Zn4,N]+N_T3Upt4[Zn4,N])
      # RT3_L4N_RelUptperLrv[Zn4,Sp3] = if  RT3_AvgNutUptperRoot[N,Sp3] > 0 and RT3_LrvDepth4[Zn4,Sp3]> 0 then  (N_T3Upt4[Zn4,N]/RT3_LrvDepth4[Zn4,Sp3])/RT3_AvgNutUptperRoot[N,Sp3] else 0*(N_T1Upt4[Zn4,N]+N_T2Upt4[Zn4,N])
      # 
      # RT3_L4P_RelUptperLrv[Zn1,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth4[Zn1,Sp1]> 0 then  (N_T1Upt4[Zn1,P]/RT3_LrvDepth4[Zn1,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt4[Zn1,P]+N_T3Upt4[Zn1,P])
      # RT3_L4P_RelUptperLrv[Zn1,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth4[Zn1,Sp2]> 0 then  (N_T2Upt4[Zn1,P]/RT3_LrvDepth4[Zn1,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt4[Zn1,P]+N_T3Upt4[Zn1,P])
      # RT3_L4P_RelUptperLrv[Zn1,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth4[Zn1,Sp3]> 0 then  (N_T3Upt4[Zn1,P]/RT3_LrvDepth4[Zn1,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt4[Zn1,P]+N_T2Upt4[Zn1,P])
      # RT3_L4P_RelUptperLrv[Zn2,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth4[Zn2,Sp1]> 0 then  (N_T1Upt4[Zn2,P]/RT3_LrvDepth4[Zn2,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt4[Zn2,P]+N_T3Upt4[Zn2,P])
      # RT3_L4P_RelUptperLrv[Zn2,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth4[Zn2,Sp2]> 0 then  (N_T2Upt4[Zn2,P]/RT3_LrvDepth4[Zn2,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt4[Zn2,P]+N_T3Upt4[Zn2,P])
      # RT3_L4P_RelUptperLrv[Zn2,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth4[Zn2,Sp3]> 0 then  (N_T3Upt4[Zn2,P]/RT3_LrvDepth4[Zn2,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt4[Zn2,P]+N_T2Upt4[Zn2,P])
      # RT3_L4P_RelUptperLrv[Zn3,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth4[Zn3,Sp1]> 0 then  (N_T1Upt4[Zn3,P]/RT3_LrvDepth4[Zn3,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt4[Zn3,P]+N_T3Upt4[Zn3,P])
      # RT3_L4P_RelUptperLrv[Zn3,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth4[Zn3,Sp2]> 0 then  (N_T2Upt4[Zn3,P]/RT3_LrvDepth4[Zn3,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt4[Zn3,P]+N_T3Upt4[Zn3,P])
      # RT3_L4P_RelUptperLrv[Zn3,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth4[Zn3,Sp3]> 0 then  (N_T3Upt4[Zn3,P]/RT3_LrvDepth4[Zn3,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt4[Zn3,P]+N_T2Upt4[Zn3,P])
      # RT3_L4P_RelUptperLrv[Zn4,Sp1] = if  RT3_AvgNutUptperRoot[P,Sp1] > 0 and RT3_LrvDepth4[Zn4,Sp1]> 0 then  (N_T1Upt4[Zn4,P]/RT3_LrvDepth4[Zn4,Sp1])/RT3_AvgNutUptperRoot[P,Sp1] else 0*(N_T2Upt4[Zn4,P]+N_T3Upt4[Zn4,P])
      # RT3_L4P_RelUptperLrv[Zn4,Sp2] = if  RT3_AvgNutUptperRoot[P,Sp2] > 0 and RT3_LrvDepth4[Zn4,Sp2]> 0 then  (N_T2Upt4[Zn4,P]/RT3_LrvDepth4[Zn4,Sp2])/RT3_AvgNutUptperRoot[P,Sp2] else 0*(N_T1Upt4[Zn4,P]+N_T3Upt4[Zn4,P])
      # RT3_L4P_RelUptperLrv[Zn4,Sp3] = if  RT3_AvgNutUptperRoot[P,Sp3] > 0 and RT3_LrvDepth4[Zn4,Sp3]> 0 then  (N_T3Upt4[Zn4,P]/RT3_LrvDepth4[Zn4,Sp3])/RT3_AvgNutUptperRoot[P,Sp3] else 0*(N_T1Upt4[Zn4,P]+N_T2Upt4[Zn4,P])
      
      zonelayertreenut_df$RT3_AvgNutUptperRoot <- rep(treenut_df$RT3_AvgNutUptperRoot, each = nzone*nlayer)
      zonelayertreenut_df$RT3_LrvDepth <- rep(zonelayertree_df$RT3_LrvDepth, nrow(nut_df))
      
      zonelayertreenut_df$RT3_RelUptperLrv <- ifelse(zonelayertreenut_df$RT3_AvgNutUptperRoot > 0 & zonelayertreenut_df$RT3_LrvDepth> 0,
                                                      (zonelayertreenut_df$N_Upt/zonelayertreenut_df$RT3_LrvDepth)/zonelayertreenut_df$RT3_AvgNutUptperRoot, 0)

      # RT3_L1RelUptperLrv[Zone,Tree] = if RT3_CurrentStress[Tree] = 1 then RT3_L1W_RelUptperLrv[Zone,Tree] else if RT3_CurrentStress[Tree] = 2 then RT3_L1N_RelUptperLrv[Zone,Tree] else RT3_L1P_RelUptperLrv[Zone,Tree]
      # RT3_L2RelUptperLrv[Zone,Tree] = if RT3_CurrentStress[Tree] = 1 then RT3_L2W_RelUptperLrv[Zone,Tree] else if RT3_CurrentStress[Tree] = 2 then RT3_L2N_RelUptperLrv[Zone,Tree] else RT3_L2P_RelUptperLrv[Zone,Tree]
      # RT3_L3RelUptperLrv[Zone,Tree] = if RT3_CurrentStress[Tree] = 1 then RT3_L3W_RelUptperLrv[Zone,Tree] else if RT3_CurrentStress[Tree] = 2 then RT3_L3N_RelUptperLrv[Zone,Tree] else RT3_L3P_RelUptperLrv[Zone,Tree]
      # RT3_L4RelUptperLrv[Zone,Tree] = if RT3_CurrentStress[Tree] = 1 then RT3_L4W_RelUptperLrv[Zone,Tree] else if RT3_CurrentStress[Tree] = 2 then RT3_L4N_RelUptperLrv[Zone,Tree] else RT3_L4P_RelUptperLrv[Zone,Tree]
      zonelayertree_df$RT3_CurrentStress <- rep(tree_df$RT3_CurrentStress, each = nzone * nlayer)
      zonelayertree_df$RT3_LRelUptperLrv <- ifelse(
        zonelayertree_df$RT3_CurrentStress == 1,
        zonelayertree_df$RT3_LW_RelUptperLrv,
        ifelse(
          zonelayertree_df$RT3_CurrentStress == 2,
          zonelayertreenut_df[zonelayertreenut_df$SlNut == "N", ]$RT3_RelUptperLrv,
          zonelayertreenut_df[zonelayertreenut_df$SlNut == "P", ]$RT3_RelUptperLrv
        )
      )
      
      # RT3_RelUptWL1[Tree,Zone] = RT3_VoxVol[Zone,1]*RT3_L1RelUptperLrv[Zone,Tree]
      # RT3_RelUptWL2[Tree,Zone] = RT3_VoxVol[Zone,2]*RT3_L2RelUptperLrv[Zone,Tree]
      # RT3_RelUptWL3[Tree,Zone] = RT3_VoxVol[Zone,3]*RT3_L3RelUptperLrv[Zone,Tree]
      # RT3_RelUptWL4[Tree,Zone] = RT3_VoxVol[Zone,4]*RT3_L4RelUptperLrv[Zone,Tree]
      zonelayertree_df$RT3_RelUptWL <- zonelayertree_df$RT3_VoxVol* zonelayertree_df$RT3_LRelUptperLrv

      # RT3_RelUptTot[Tree] = ARRAYSUM(RT3_RelUptWL1[Tree,*])+ARRAYSUM(RT3_RelUptWL2[Tree,*])+ARRAYSUM(RT3_RelUptWL3[Tree,*])+ARRAYSUM(RT3_RelUptWL4[Tree,*])
      tree_df$RT3_RelUptTot <- aggregate(zonelayertree_df["RT3_RelUptWL"], zonelayertree_df["tree_id"], sum)$RT3_RelUptWL
      
      # RT3_FRAlloc1[Zone,Tree] = if RT3_RelUptTot[Tree]>0 then ((RT3_L1RelUptperLrv[Zone,Tree]/RT3_RelUptTot[Tree])^(1-RT3_PowerAllocRtL))*RT_L1FRLength[Zone,Tree]^RT3_PowerAllocRtL else 0
      # RT3_FRAlloc2[Zone,Tree] = if RT3_RelUptTot[Tree]>0 then ((RT3_L2RelUptperLrv[Zone,Tree]/RT3_RelUptTot[Tree])^(1-RT3_PowerAllocRtL))*RT_L2FRLength[Zone,Tree]^RT3_PowerAllocRtL else 0
      # RT3_FRAlloc3[Zone,Tree] = if RT3_RelUptTot[Tree]>0 then ((RT3_L3RelUptperLrv[Zone,Tree]/RT3_RelUptTot[Tree])^(1-RT3_PowerAllocRtL))*RT_L3FRLength[Zone,Tree]^RT3_PowerAllocRtL else 0
      # RT3_FRAlloc4[Zone,Tree] = if RT3_RelUptTot[Tree]>0 then ((RT3_L4RelUptperLrv[Zone,Tree]/RT3_RelUptTot[Tree])^(1-RT3_PowerAllocRtL))*RT_L4FRLength[Zone,Tree]^RT3_PowerAllocRtL else 0
      zonelayertree_df$RT3_RelUptTot <- rep(tree_df$RT3_RelUptTot, each = nzone*nlayer)
      zonelayertree_df$RT3_FRAlloc <- ifelse( zonelayertree_df$RT3_RelUptTot>0, ((zonelayertree_df$RT3_LRelUptperLrv/zonelayertree_df$RT3_RelUptTot)^(1-pars$Rt_par$RT3_PowerAllocRtL))*
                                                zonelayertree_df$RT_FRLength^pars$Rt_par$RT3_PowerAllocRtL, 0)

      # RT3_RtAllocTot[Tree] = ARRAYSUM(RT3_FRAlloc1[*,Tree])+ARRAYSUM(RT3_FRAlloc2[*,Tree])+ARRAYSUM(RT3_FRAlloc3[*,Tree]) +ARRAYSUM(RT3_FRAlloc4[*,Tree])
      tree_df$RT3_RtAllocTot <- aggregate(zonelayertree_df["RT3_FRAlloc"], zonelayertree_df["tree_id"], sum)$RT3_FRAlloc

      # RT3_L1RtExp?[Zone,Tree] = if Rt3_LRV1[Zone,Tree]>RT3_AlphaLrv[Tree] then 1 else 0
      # RT3_L2RtExp?[Zone,Tree] = if Rt3_LRV2[Zone,Tree]>RT3_AlphaLrv[Tree] then 1 else 0
      # RT3_L3RtExp?[Zone,Tree] = if Rt3_LRV3[Zone,Tree]>RT3_AlphaLrv[Tree] then 1 else 0
      # RT3_L4RtExp?[Zone,Tree] = if Rt3_LRV4[Zone,Tree]>RT3_AlphaLrv[Tree] then 1 else 0
      zonelayertree_df$RT3_AlphaLrv <- rep(tree_df$RT3_AlphaLrv, each = nzone*nlayer)
      zonelayertree_df$RT3_RtExp_is <- ifelse( zonelayertree_df$Rt3_LRV > zonelayertree_df$RT3_AlphaLrv, 1, 0)

      # RT3_DepthWidthRatio1[Zone] = if AF_ZoneWidth[Zone] = 0 then 1 else AF_DepthAct1[Zone]/AF_ZoneWidth[Zone]
      # RT3_DepthWidthRatio2[Zone] = if AF_ZoneWidth[Zone] = 0 then 1 else AF_Depth2[Zone]/AF_ZoneWidth[Zone]
      # RT3_DepthWidthRatio3[Zone] = if AF_ZoneWidth[Zone] = 0 then 1 else AF_Depth3[Zone]/AF_ZoneWidth[Zone]
      # RT3_DepthWidthRatio4[Zone] = if AF_ZoneWidth[Zone] = 0 then 1 else AF_Depth4[Zone]/AF_ZoneWidth[Zone]
      zonelayer_df$RT3_DepthWidthRatio <- ifelse( zonelayer_df$AF_ZoneWidth == 0, 1, zonelayer_df$AF_Depth/zonelayer_df$AF_ZoneWidth)

      # RT3_Beta1[Zone,Tree] = 1-((1-RT3_Beta0[Tree])/(100*RT3_DepthWidthRatio1[Zone]*AF_ZoneWidth[Zone]))*(1-RT3_LamHor0[Tree] *(1-RT3_DepthWidthRatio1[Zone]))
      # RT3_Beta2[Zone,Tree] = 1-((1-RT3_Beta0[Tree])/(100*RT3_DepthWidthRatio2[Zone]*AF_ZoneWidth[Zone]))*(1-RT3_LamHor0[Tree] *(1-RT3_DepthWidthRatio2[Zone]))
      # RT3_Beta3[Zone,Tree] = 1-((1-RT3_Beta0[Tree])/(100*RT3_DepthWidthRatio3[Zone]*AF_ZoneWidth[Zone]))*(1-RT3_LamHor0[Tree] *(1-RT3_DepthWidthRatio3[Zone]))
      # RT3_Beta4[Zone,Tree] = 1-((1-RT3_Beta0[Tree])/(100*RT3_DepthWidthRatio4[Zone]*AF_ZoneWidth[Zone]))*(1-RT3_LamHor0[Tree] *(1-RT3_DepthWidthRatio4[Zone]))
      zonelayertree_df$RT3_Beta0 <- rep(tree_df$RT3_Beta0, each = nzone*nlayer)
      zonelayertree_df$RT3_DepthWidthRatio <- rep(zonelayer_df$RT3_DepthWidthRatio, ntree)
      zonelayertree_df$RT3_LamHor0 <- rep(tree_df$RT3_LamHor0, each = nzone*nlayer)
      zonelayertree_df$RT3_Beta <- 1-((1-zonelayertree_df$RT3_Beta0)/(100* zonelayertree_df$RT3_DepthWidthRatio* zonelayertree_df$AF_ZoneWidth))*(1-zonelayertree_df$RT3_LamHor0 *(1- zonelayertree_df$RT3_DepthWidthRatio))
      
      # RT3_LamHor1[Zone,Tree] = RT3_LamHor0[Tree]*(RT3_DepthWidthRatio1[Zone])/(1-RT3_LamHor0[Tree]*(1-RT3_DepthWidthRatio1[Zone]))
      # RT3_LamHor2[Zone,Tree] = RT3_LamHor0[Tree]*(RT3_DepthWidthRatio2[Zone])/(1-RT3_LamHor0[Tree]*(1-RT3_DepthWidthRatio2[Zone]))
      # RT3_LamHor3[Zone,Tree] = RT3_LamHor0[Tree]*(RT3_DepthWidthRatio3[Zone])/(1-RT3_LamHor0[Tree]*(1-RT3_DepthWidthRatio3[Zone]))
      # RT3_LamHor4[Zone,Tree] = RT3_LamHor0[Tree]*(RT3_DepthWidthRatio4[Zone])/(1-RT3_LamHor0[Tree]*(1-RT3_DepthWidthRatio4[Zone]))
      zonelayertree_df$RT3_LamHor <- zonelayertree_df$RT3_LamHor0*zonelayertree_df$RT3_DepthWidthRatio/(1- zonelayertree_df$RT3_LamHor0*(1-zonelayertree_df$RT3_DepthWidthRatio))

      # RT3_Alloc1Down[Zone,Tree] = RT3_L1RtExp?[Zone,Tree]*(1-RT3_Beta1[Zone,Tree])*(1-RT3_LamHor1[Zone,Tree])*(RT3_LamGeotrop[Tree])
      # RT3_Alloc2Down[Zone,Tree] = RT3_L2RtExp?[Zone,Tree]*(1-RT3_Beta2[Zone,Tree])*(1-RT3_LamHor2[Zone,Tree])*(RT3_LamGeotrop[Tree])
      # RT3_Alloc3Down[Zone,Tree] = RT3_L3RtExp?[Zone,Tree]*(1-RT3_Beta3[Zone,Tree])*(1-RT3_LamHor3[Zone,Tree])*(RT3_LamGeotrop[Tree])
      zonelayertree_df$RT3_LamGeotrop <- rep(tree_df$RT3_LamGeotrop, each = nzone*nlayer)
      zonelayertree_df$RT3_AllocDown <- zonelayertree_df$RT3_RtExp_is*(1- zonelayertree_df$RT3_Beta)*(1- zonelayertree_df$RT3_LamHor)*zonelayertree_df$RT3_LamGeotrop
      
      # RT3_Alloc2Upw[Zone,Tree] = RT3_L2RtExp?[Zone,Tree]*(1-RT3_Beta2[Zone,Tree])*(1-RT3_LamHor2[Zone,Tree])*(1-RT3_LamGeotrop[Tree])
      # RT3_Alloc3Upw[Zone,Tree] = RT3_L3RtExp?[Zone,Tree]*(1-RT3_Beta3[Zone,Tree])*(1-RT3_LamHor3[Zone,Tree])*(1-RT3_LamGeotrop[Tree])
      # RT3_Alloc4Upw[Zone,Tree] = RT3_L4RtExp?[Zone,Tree]*(1-RT3_Beta4[Zone,Tree])*(1-RT3_LamHor4[Zone,Tree])*(1-RT3_LamGeotrop[Tree])
      zonelayertree_df$RT3_AllocUpw <- zonelayertree_df$RT3_RtExp_is*(1-zonelayertree_df$RT3_Beta)*(1-zonelayertree_df$RT3_LamHor)*(1-zonelayertree_df$RT3_LamGeotrop)
      zonelayertree_df[zonelayertree_df$layer == 1,]$RT3_AllocUpw <- 0
      
      # RT3_Alloc1Within[Zone,Tree] = 1-RT3_Alloc1Down[Zone,Tree]
      # RT3_Alloc2Within[Zone,Tree] = 1-RT3_Alloc2Down[Zone,Tree]-RT3_Alloc2Upw[Zone,Tree]
      # RT3_Alloc3Within[Zone,Tree] = 1-RT3_Alloc3Down[Zone,Tree]-RT3_Alloc3Upw[Zone,Tree]
      # RT3_Alloc4Within[Zn1,Sp1] = 1-RT3_Alloc4Upw[Zn1,Sp1]
      # RT3_Alloc4Within[Zn1,Sp2] = 1-RT3_Alloc4Upw[Zn1,Sp2]
      # RT3_Alloc4Within[Zn1,Sp3] = 1-RT3_Alloc4Upw[Zn1,Sp3]
      # RT3_Alloc4Within[Zn2,Sp1] = 1-RT3_Alloc4Upw[Zn2,Sp1]
      # RT3_Alloc4Within[Zn2,Sp2] = 1-RT3_Alloc4Upw[Zn2,Sp2]
      # RT3_Alloc4Within[Zn2,Sp3] = 1-RT3_Alloc4Upw[Zn2,Sp3]
      # RT3_Alloc4Within[Zn3,Sp1] = 1-RT3_Alloc4Upw[Zn3,Sp1]
      # RT3_Alloc4Within[Zn3,Sp2] = 1-RT3_Alloc4Upw[Zn3,Sp2]
      # RT3_Alloc4Within[Zn3,Sp3] = 1-RT3_Alloc4Upw[Zn3,Sp3]
      # RT3_Alloc4Within[Zn4,Sp1] = 1-RT3_Alloc4Upw[Zn4,Sp1]
      # RT3_Alloc4Within[Zn4,Sp2] = 1-RT3_Alloc4Upw[Zn4,Sp2]
      # RT3_Alloc4Within[Zn4,Sp3] = 1-RT3_Alloc4Upw[Zn4,Sp3]
      zonelayertree_df$RT3_AllocWithin <- 1- zonelayertree_df$RT3_AllocDown - zonelayertree_df$RT3_AllocUpw
      zonelayertree_df[zonelayertree_df$layer == 4,]$RT3_AllocWithin <- 1 - zonelayertree_df[zonelayertree_df$layer == 4,]$RT3_AllocUpw
      
      # T_RootInc[PlantComp,Tree] = If Rt_ATType[Tree] = 2 then IF T_GrowsToday?[Tree] = 0 then 0 else (T_RtConc[PlantComp,Tree]*T_RtAllocAct[Tree]*T_UnitConv[PlantComp]*T_CanBiomInc[PlantComp,Tree]/((1-T_RtAllocAct[Tree]))) else 0
      treepcomp_df$Rt_ATType <- rep(tree_df$Rt_ATType, nrow(pcomp_df))
      treepcomp_df$T_RootInc <- ifelse(
        treepcomp_df$Rt_ATType == 2,
        ifelse(
          treepcomp_df$T_GrowsToday_is == 0,
          0,
          treepcomp_df$T_RtConc * treepcomp_df$T_RtAllocAct * treepcomp_df$T_UnitConv * treepcomp_df$T_CanBiomInc /
            ((1 - treepcomp_df$T_RtAllocAct))
        ),
        0
      )

      # RT3_Voxel_Tree_DistZn[Zn1,Sp1] = 0.5*AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn1,Sp2] = 0.5*AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn1,Sp3] = 0.5*AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn2,Sp1] = 0.5*AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn2,Sp2] = 0.5*AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn2,Sp3] = 0.5*AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn3,Sp1] = 0.5*AF_ZoneWidth[Zn3]+AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn3,Sp2] = 0.5*AF_ZoneWidth[Zn3]+AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn3,Sp3] = 0.5*AF_ZoneWidth[Zn3]+AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn4,Sp1] = 0.5*AF_ZoneWidth[Zn4]+AF_ZoneWidth[Zn3]+AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn4,Sp2] = 0.5*AF_ZoneWidth[Zn4]+AF_ZoneWidth[Zn3]+AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      # RT3_Voxel_Tree_DistZn[Zn4,Sp3] = 0.5*AF_ZoneWidth[Zn4]+AF_ZoneWidth[Zn3]+AF_ZoneWidth[Zn2]+AF_ZoneWidth[Zn1]
      
      zonetree_df$AF_ZoneWidth_cum <- 0
      zonetree_df[zonetree_df$zone == 2,]$AF_ZoneWidth_cum <- zonetree_df[zonetree_df$zone == 1,]$AF_ZoneWidth 
      zonetree_df[zonetree_df$zone == 3,]$AF_ZoneWidth_cum <- zonetree_df[zonetree_df$zone == 1,]$AF_ZoneWidth + zonetree_df[zonetree_df$zone == 2,]$AF_ZoneWidth 
      zonetree_df[zonetree_df$zone == 4,]$AF_ZoneWidth_cum <- zonetree_df[zonetree_df$zone == 1,]$AF_ZoneWidth + zonetree_df[zonetree_df$zone == 2,]$AF_ZoneWidth + zonetree_df[zonetree_df$zone == 3,]$AF_ZoneWidth 
      
      zonetree_df$RT3_Voxel_Tree_DistZn <- 0.5* zonetree_df$AF_ZoneWidth + zonetree_df$AF_ZoneWidth_cum
      
      # RT3_VoxTreeDist1[Tree,Zone] = ((                                                   0.5*AF_DepthAct1[Zone])^2+RT3_Voxel_Tree_DistZn[Zone,Tree]^2)^0.5
      # RT3_VoxTreeDist2[Tree,Zone] = ((AF_DepthAct1[Zone]+                                0.5*AF_Depth2[Zone])^2+RT3_Voxel_Tree_DistZn[Zone,Tree]^2)^0.5
      # RT3_VoxTreeDist3[Tree,Zone] = ((AF_DepthAct1[Zone]+AF_Depth2[Zone]+                0.5*AF_Depth3[Zone])^2+RT3_Voxel_Tree_DistZn[Zone,Tree]^2)^0.5
      # RT3_VoxTreeDist4[Tree,Zone] = ((AF_DepthAct1[Zone]+AF_Depth2[Zone]+AF_Depth3[Zone]+0.5*AF_Depth4[Zone])^2+RT3_Voxel_Tree_DistZn[Zone,Tree]^2)^0.5      
      zonelayertree_df$AF_Depth_cum <- 0
      zonelayertree_df[zonelayertree_df$layer == 2, ]$AF_Depth_cum <- zonelayertree_df[zonelayertree_df$layer == 1, ]$AF_Depth
      zonelayertree_df[zonelayertree_df$layer == 3, ]$AF_Depth_cum <- zonelayertree_df[zonelayertree_df$layer == 1, ]$AF_Depth + zonelayertree_df[zonelayertree_df$layer == 2, ]$AF_Depth
      zonelayertree_df[zonelayertree_df$layer == 4, ]$AF_Depth_cum <- zonelayertree_df[zonelayertree_df$layer == 1, ]$AF_Depth + zonelayertree_df[zonelayertree_df$layer == 2, ]$AF_Depth + zonelayertree_df[zonelayertree_df$layer == 3, ]$AF_Depth
      
      zonelayertree_df$RT3_Voxel_Tree_DistZn <- c(
        rep(zonetree_df[zonetree_df$tree_id == 1, ]$RT3_Voxel_Tree_DistZn, nlayer),
        rep(zonetree_df[zonetree_df$tree_id == 2, ]$RT3_Voxel_Tree_DistZn, nlayer),
        rep(zonetree_df[zonetree_df$tree_id == 3, ]$RT3_Voxel_Tree_DistZn, nlayer)
      )
      
      zonelayertree_df$RT3_VoxTreeDist <- ((zonelayertree_df$AF_Depth_cum + 0.5 * zonelayertree_df$AF_Depth)^2+zonelayertree_df$RT3_Voxel_Tree_DistZn^2)^0.5
      
      # RT3_SumTransp1[Zone,Tree] = RT_L1FRLength[Zone,Tree]*RT3_VoxTreeDist1[Tree,Zone]*100
      # RT3_SumTransp2[Zone,Tree] = RT_L2FRLength[Zone,Tree]*RT3_VoxTreeDist2[Tree,Zone]*100
      # RT3_SumTransp3[Zone,Tree] = RT_L3FRLength[Zone,Tree]*RT3_VoxTreeDist3[Tree,Zone]*100
      # RT3_SumTransp4[Zone,Tree] = RT_L4FRLength[Zone,Tree]*RT3_VoxTreeDist4[Tree,Zone]*100
      zonelayertree_df$RT3_SumTransp <- zonelayertree_df$RT_FRLength* zonelayertree_df$RT3_VoxTreeDist*100

      # RT3_CRTarget[Tree] = (ARRAYSUM(RT3_SumTransp1[*,Tree])+ARRAYSUM(RT3_SumTransp2[*,Tree])+ARRAYSUM(RT3_SumTransp3[*,Tree])+ARRAYSUM(RT3_SumTransp4[*,Tree]))*RT3_CR_TargFac[Tree]
      tree_df$RT3_CRTarget <- aggregate(zonelayertree_df["RT3_SumTransp"], zonelayertree_df["tree_id"], sum)$RT3_SumTransp * tree_df$RT3_CR_TargFac

      # RT3_CRincr[Tree] = min(T_RootInc[DW,Tree],max(0,RT3_CRTarget[Tree]-RT3_CoarseRt[Tree]))
      tree_df$RT3_CRincr <- pmin(treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_RootInc, pmax(0, tree_df$RT3_CRTarget - tree_df$RT3_CoarseRt))
      
      # RT3_FineRootInc[Tree] = max(0,T_RootInc[DW,Tree]-RT3_CRincr[Tree])*T_SRLfineroots[Tree]
      tree_df$RT3_FineRootInc <- pmax(0, treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_RootInc- tree_df$RT3_CRincr)*tree_df$T_SRLfineroots
      
      # RT3_VoxHorExp1[Zone,Tree] = RT3_L1RtExp?[Zone,Tree]*(1-RT3_Beta1[Zone,Tree])*RT3_LamHor1[Zone,Tree]
      # RT3_VoxHorExp2[Zone,Tree] = RT3_L2RtExp?[Zone,Tree]*(1-RT3_Beta2[Zone,Tree])*RT3_LamHor2[Zone,Tree]
      # RT3_VoxHorExp3[Zone,Tree] = RT3_L3RtExp?[Zone,Tree]*(1-RT3_Beta3[Zone,Tree])*RT3_LamHor3[Zone,Tree]
      # RT3_VoxHorExp4[Zone,Tree] = RT3_L4RtExp?[Zone,Tree]*(1-RT3_Beta4[Zone,Tree])*RT3_LamHor4[Zone,Tree]
      zonelayertree_df$RT3_VoxHorExp <- zonelayertree_df$RT3_RtExp_is*(1- zonelayertree_df$RT3_Beta)* zonelayertree_df$RT3_LamHor
            
      # RT3_L1FRtGr[Zn1,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(                                                RT3_FRAlloc1[Zn1,Sp1]*RT3_Alloc1Within[Zn1,Sp1]*(1-RT3_VoxHorExp1[Zn1,Sp1]/2)+  RT3_FRAlloc1[Zn2,Sp1]*RT3_Alloc1Within[Zn2,Sp1]*(RT3_VoxHorExp1[Zn2,Sp1]/2)+                                                                                RT3_FRAlloc2[Zn1,Sp1]*RT3_Alloc2Upw[Zn1,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L1FRtGr[Zn1,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(                                                RT3_FRAlloc1[Zn1,Sp2]*RT3_Alloc1Within[Zn1,Sp2]*(1-RT3_VoxHorExp1[Zn1,Sp2]/2)+  RT3_FRAlloc1[Zn2,Sp2]*RT3_Alloc1Within[Zn2,Sp2]*(RT3_VoxHorExp1[Zn2,Sp2]/2)+                                                                                RT3_FRAlloc2[Zn1,Sp2]*RT3_Alloc2Upw[Zn1,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L1FRtGr[Zn1,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(                                                RT3_FRAlloc1[Zn1,Sp3]*RT3_Alloc1Within[Zn1,Sp3]*(1-RT3_VoxHorExp1[Zn1,Sp3]/2)+  RT3_FRAlloc1[Zn2,Sp3]*RT3_Alloc1Within[Zn2,Sp3]*(RT3_VoxHorExp1[Zn2,Sp3]/2)+                                                                                RT3_FRAlloc2[Zn1,Sp3]*RT3_Alloc2Upw[Zn1,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L1FRtGr[Zn2,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(                                                RT3_FRAlloc1[Zn2,Sp1]*RT3_Alloc1Within[Zn2,Sp1]*(1-RT3_VoxHorExp1[Zn2,Sp1]  )+  RT3_FRAlloc1[Zn1,Sp1]*RT3_Alloc1Within[Zn1,Sp1]*(RT3_VoxHorExp1[Zn1,Sp1]/2)+  RT3_FRAlloc1[Zn3,Sp1]*RT3_Alloc1Within[Zn3,Sp1]*(RT3_VoxHorExp1[Zn3,Sp1]/2)+  RT3_FRAlloc2[Zn2,Sp1]*RT3_Alloc2Upw[Zn2,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L1FRtGr[Zn2,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(                                                RT3_FRAlloc1[Zn2,Sp2]*RT3_Alloc1Within[Zn2,Sp2]*(1-RT3_VoxHorExp1[Zn2,Sp2]  )+  RT3_FRAlloc1[Zn1,Sp2]*RT3_Alloc1Within[Zn1,Sp2]*(RT3_VoxHorExp1[Zn1,Sp2]/2)+  RT3_FRAlloc1[Zn3,Sp2]*RT3_Alloc1Within[Zn3,Sp2]*(RT3_VoxHorExp1[Zn3,Sp2]/2)+  RT3_FRAlloc2[Zn2,Sp2]*RT3_Alloc2Upw[Zn2,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L1FRtGr[Zn2,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(                                                RT3_FRAlloc1[Zn2,Sp3]*RT3_Alloc1Within[Zn2,Sp3]*(1-RT3_VoxHorExp1[Zn2,Sp3]  )+  RT3_FRAlloc1[Zn1,Sp3]*RT3_Alloc1Within[Zn1,Sp3]*(RT3_VoxHorExp1[Zn1,Sp3]/2)+  RT3_FRAlloc1[Zn3,Sp3]*RT3_Alloc1Within[Zn3,Sp3]*(RT3_VoxHorExp1[Zn3,Sp3]/2)+  RT3_FRAlloc2[Zn2,Sp3]*RT3_Alloc2Upw[Zn2,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L1FRtGr[Zn3,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(                                                RT3_FRAlloc1[Zn3,Sp1]*RT3_Alloc1Within[Zn3,Sp1]*(1-RT3_VoxHorExp1[Zn3,Sp1]  )+  RT3_FRAlloc1[Zn2,Sp1]*RT3_Alloc1Within[Zn2,Sp1]*(RT3_VoxHorExp1[Zn2,Sp1]/2)+  RT3_FRAlloc1[Zn4,Sp1]*RT3_Alloc1Within[Zn4,Sp1]*(RT3_VoxHorExp1[Zn4,Sp1]/2)+  RT3_FRAlloc2[Zn3,Sp1]*RT3_Alloc2Upw[Zn3,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L1FRtGr[Zn3,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(                                                RT3_FRAlloc1[Zn3,Sp2]*RT3_Alloc1Within[Zn3,Sp2]*(1-RT3_VoxHorExp1[Zn3,Sp2]  )+  RT3_FRAlloc1[Zn2,Sp2]*RT3_Alloc1Within[Zn2,Sp2]*(RT3_VoxHorExp1[Zn2,Sp2]/2)+  RT3_FRAlloc1[Zn4,Sp2]*RT3_Alloc1Within[Zn4,Sp2]*(RT3_VoxHorExp1[Zn4,Sp2]/2)+  RT3_FRAlloc2[Zn3,Sp2]*RT3_Alloc2Upw[Zn3,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L1FRtGr[Zn3,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(                                                RT3_FRAlloc1[Zn3,Sp3]*RT3_Alloc1Within[Zn3,Sp3]*(1-RT3_VoxHorExp1[Zn3,Sp3]  )+  RT3_FRAlloc1[Zn2,Sp3]*RT3_Alloc1Within[Zn2,Sp3]*(RT3_VoxHorExp1[Zn2,Sp3]/2)+  RT3_FRAlloc1[Zn4,Sp3]*RT3_Alloc1Within[Zn4,Sp3]*(RT3_VoxHorExp1[Zn4,Sp3]/2)+  RT3_FRAlloc2[Zn3,Sp3]*RT3_Alloc2Upw[Zn3,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L1FRtGr[Zn4,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(                                                RT3_FRAlloc1[Zn4,Sp1]*RT3_Alloc1Within[Zn4,Sp1]*(1-RT3_VoxHorExp1[Zn4,Sp1]/2)+  RT3_FRAlloc1[Zn3,Sp1]*RT3_Alloc1Within[Zn3,Sp1]*(RT3_VoxHorExp1[Zn3,Sp1]/2)+                                                                                RT3_FRAlloc2[Zn4,Sp1]*RT3_Alloc2Upw[Zn4,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L1FRtGr[Zn4,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(                                                RT3_FRAlloc1[Zn4,Sp2]*RT3_Alloc1Within[Zn4,Sp2]*(1-RT3_VoxHorExp1[Zn4,Sp2]/2)+  RT3_FRAlloc1[Zn3,Sp2]*RT3_Alloc1Within[Zn3,Sp2]*(RT3_VoxHorExp1[Zn3,Sp2]/2)+                                                                                RT3_FRAlloc2[Zn4,Sp2]*RT3_Alloc2Upw[Zn4,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L1FRtGr[Zn4,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(                                                RT3_FRAlloc1[Zn4,Sp3]*RT3_Alloc1Within[Zn4,Sp3]*(1-RT3_VoxHorExp1[Zn4,Sp3]/2)+  RT3_FRAlloc1[Zn3,Sp3]*RT3_Alloc1Within[Zn3,Sp3]*(RT3_VoxHorExp1[Zn3,Sp3]/2)+                                                                                RT3_FRAlloc2[Zn4,Sp3]*RT3_Alloc2Upw[Zn4,Sp3]))/RT3_RtAllocTot[Sp3]
      # 
      # RT3_L2FRtGr[Zn1,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc1[Zn1,Sp1]*RT3_Alloc1Down[Zn1,Sp1]+  RT3_FRAlloc2[Zn1,Sp1]*RT3_Alloc2Within[Zn1,Sp1]*(1-RT3_VoxHorExp2[Zn1,Sp1]/2)+  RT3_FRAlloc2[Zn2,Sp1]*RT3_Alloc2Within[Zn2,Sp1]*(RT3_VoxHorExp2[Zn2,Sp1]/2)+                                                                                RT3_FRAlloc3[Zn1,Sp1]*RT3_Alloc3Upw[Zn1,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L2FRtGr[Zn1,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc1[Zn1,Sp2]*RT3_Alloc1Down[Zn1,Sp2]+  RT3_FRAlloc2[Zn1,Sp2]*RT3_Alloc2Within[Zn1,Sp2]*(1-RT3_VoxHorExp2[Zn1,Sp2]/2)+  RT3_FRAlloc2[Zn2,Sp2]*RT3_Alloc2Within[Zn2,Sp2]*(RT3_VoxHorExp2[Zn2,Sp2]/2)+                                                                                RT3_FRAlloc3[Zn1,Sp2]*RT3_Alloc3Upw[Zn1,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L2FRtGr[Zn1,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc1[Zn1,Sp3]*RT3_Alloc1Down[Zn1,Sp3]+  RT3_FRAlloc2[Zn1,Sp3]*RT3_Alloc2Within[Zn1,Sp3]*(1-RT3_VoxHorExp2[Zn1,Sp3]/2)+  RT3_FRAlloc2[Zn2,Sp3]*RT3_Alloc2Within[Zn2,Sp3]*(RT3_VoxHorExp2[Zn2,Sp3]/2)+                                                                                RT3_FRAlloc3[Zn1,Sp3]*RT3_Alloc3Upw[Zn1,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L2FRtGr[Zn2,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc1[Zn2,Sp1]*RT3_Alloc1Down[Zn2,Sp1]+  RT3_FRAlloc2[Zn2,Sp1]*RT3_Alloc2Within[Zn2,Sp1]*(1-RT3_VoxHorExp2[Zn2,Sp1]  )+  RT3_FRAlloc2[Zn1,Sp1]*RT3_Alloc2Within[Zn1,Sp1]*(RT3_VoxHorExp2[Zn1,Sp1]/2)+  RT3_FRAlloc2[Zn3,Sp1]*RT3_Alloc2Within[Zn3,Sp1]*(RT3_VoxHorExp2[Zn3,Sp1]/2)+  RT3_FRAlloc3[Zn2,Sp1]*RT3_Alloc3Upw[Zn2,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L2FRtGr[Zn2,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc1[Zn2,Sp2]*RT3_Alloc1Down[Zn2,Sp2]+  RT3_FRAlloc2[Zn2,Sp2]*RT3_Alloc2Within[Zn2,Sp2]*(1-RT3_VoxHorExp2[Zn2,Sp2]  )+  RT3_FRAlloc2[Zn1,Sp2]*RT3_Alloc2Within[Zn1,Sp2]*(RT3_VoxHorExp2[Zn1,Sp2]/2)+  RT3_FRAlloc2[Zn3,Sp2]*RT3_Alloc2Within[Zn3,Sp2]*(RT3_VoxHorExp2[Zn3,Sp2]/2)+  RT3_FRAlloc3[Zn2,Sp2]*RT3_Alloc3Upw[Zn2,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L2FRtGr[Zn2,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc1[Zn2,Sp3]*RT3_Alloc1Down[Zn2,Sp3]+  RT3_FRAlloc2[Zn2,Sp3]*RT3_Alloc2Within[Zn2,Sp3]*(1-RT3_VoxHorExp2[Zn2,Sp3]  )+  RT3_FRAlloc2[Zn1,Sp3]*RT3_Alloc2Within[Zn1,Sp3]*(RT3_VoxHorExp2[Zn1,Sp3]/2)+  RT3_FRAlloc2[Zn3,Sp3]*RT3_Alloc2Within[Zn3,Sp3]*(RT3_VoxHorExp2[Zn3,Sp3]/2)+  RT3_FRAlloc3[Zn2,Sp3]*RT3_Alloc3Upw[Zn2,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L2FRtGr[Zn3,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc1[Zn3,Sp1]*RT3_Alloc1Down[Zn3,Sp1]+  RT3_FRAlloc2[Zn3,Sp1]*RT3_Alloc2Within[Zn3,Sp1]*(1-RT3_VoxHorExp2[Zn3,Sp1]  )+  RT3_FRAlloc2[Zn2,Sp1]*RT3_Alloc2Within[Zn2,Sp1]*(RT3_VoxHorExp2[Zn2,Sp1]/2)+  RT3_FRAlloc2[Zn4,Sp1]*RT3_Alloc2Within[Zn4,Sp1]*(RT3_VoxHorExp2[Zn4,Sp1]/2)+  RT3_FRAlloc3[Zn3,Sp1]*RT3_Alloc3Upw[Zn3,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L2FRtGr[Zn3,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc1[Zn3,Sp2]*RT3_Alloc1Down[Zn3,Sp2]+  RT3_FRAlloc2[Zn3,Sp2]*RT3_Alloc2Within[Zn3,Sp2]*(1-RT3_VoxHorExp2[Zn3,Sp2]  )+  RT3_FRAlloc2[Zn2,Sp2]*RT3_Alloc2Within[Zn2,Sp2]*(RT3_VoxHorExp2[Zn2,Sp2]/2)+  RT3_FRAlloc2[Zn4,Sp2]*RT3_Alloc2Within[Zn4,Sp2]*(RT3_VoxHorExp2[Zn4,Sp2]/2)+  RT3_FRAlloc3[Zn3,Sp2]*RT3_Alloc3Upw[Zn3,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L2FRtGr[Zn3,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc1[Zn3,Sp3]*RT3_Alloc1Down[Zn3,Sp3]+  RT3_FRAlloc2[Zn3,Sp3]*RT3_Alloc2Within[Zn3,Sp3]*(1-RT3_VoxHorExp2[Zn3,Sp3]  )+  RT3_FRAlloc2[Zn2,Sp3]*RT3_Alloc2Within[Zn2,Sp3]*(RT3_VoxHorExp2[Zn2,Sp3]/2)+  RT3_FRAlloc2[Zn4,Sp3]*RT3_Alloc2Within[Zn4,Sp3]*(RT3_VoxHorExp2[Zn4,Sp3]/2)+  RT3_FRAlloc3[Zn3,Sp3]*RT3_Alloc3Upw[Zn3,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L2FRtGr[Zn4,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc1[Zn4,Sp1]*RT3_Alloc1Down[Zn4,Sp1]+  RT3_FRAlloc2[Zn4,Sp1]*RT3_Alloc2Within[Zn4,Sp1]*(1-RT3_VoxHorExp2[Zn4,Sp1]/2)+  RT3_FRAlloc2[Zn3,Sp1]*RT3_Alloc2Within[Zn3,Sp1]*(RT3_VoxHorExp2[Zn3,Sp1]/2)+                                                                                RT3_FRAlloc3[Zn4,Sp1]*RT3_Alloc3Upw[Zn4,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L2FRtGr[Zn4,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc1[Zn4,Sp2]*RT3_Alloc1Down[Zn4,Sp2]+  RT3_FRAlloc2[Zn4,Sp2]*RT3_Alloc2Within[Zn4,Sp2]*(1-RT3_VoxHorExp2[Zn4,Sp2]/2)+  RT3_FRAlloc2[Zn3,Sp2]*RT3_Alloc2Within[Zn3,Sp2]*(RT3_VoxHorExp2[Zn3,Sp2]/2)+                                                                                RT3_FRAlloc3[Zn4,Sp2]*RT3_Alloc3Upw[Zn4,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L2FRtGr[Zn4,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc1[Zn4,Sp3]*RT3_Alloc1Down[Zn4,Sp3]+  RT3_FRAlloc2[Zn4,Sp3]*RT3_Alloc2Within[Zn4,Sp3]*(1-RT3_VoxHorExp2[Zn4,Sp3]/2)+  RT3_FRAlloc2[Zn3,Sp3]*RT3_Alloc2Within[Zn3,Sp3]*(RT3_VoxHorExp2[Zn3,Sp3]/2)+                                                                                RT3_FRAlloc3[Zn4,Sp3]*RT3_Alloc3Upw[Zn4,Sp3]))/RT3_RtAllocTot[Sp3]
      # 
      # RT3_L3FRtGr[Zn1,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc2[Zn1,Sp1]*RT3_Alloc2Down[Zn1,Sp1]+  RT3_FRAlloc3[Zn1,Sp1]*RT3_Alloc3Within[Zn1,Sp1]*(1-RT3_VoxHorExp3[Zn1,Sp1]/2)+  RT3_FRAlloc3[Zn2,Sp1]*RT3_Alloc3Within[Zn2,Sp1]*(RT3_VoxHorExp3[Zn2,Sp1]/2)+                                                                                RT3_FRAlloc4[Zn1,Sp1]*RT3_Alloc4Upw[Zn1,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L3FRtGr[Zn1,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc2[Zn1,Sp2]*RT3_Alloc2Down[Zn1,Sp2]+  RT3_FRAlloc3[Zn1,Sp2]*RT3_Alloc3Within[Zn1,Sp2]*(1-RT3_VoxHorExp3[Zn1,Sp2]/2)+  RT3_FRAlloc3[Zn2,Sp2]*RT3_Alloc3Within[Zn2,Sp2]*(RT3_VoxHorExp3[Zn2,Sp2]/2)+                                                                                RT3_FRAlloc4[Zn1,Sp2]*RT3_Alloc4Upw[Zn1,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L3FRtGr[Zn1,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc2[Zn1,Sp3]*RT3_Alloc2Down[Zn1,Sp3]+  RT3_FRAlloc3[Zn1,Sp3]*RT3_Alloc3Within[Zn1,Sp3]*(1-RT3_VoxHorExp3[Zn1,Sp3]/2)+  RT3_FRAlloc3[Zn2,Sp3]*RT3_Alloc3Within[Zn2,Sp3]*(RT3_VoxHorExp3[Zn2,Sp3]/2)+                                                                                RT3_FRAlloc4[Zn1,Sp3]*RT3_Alloc4Upw[Zn1,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L3FRtGr[Zn2,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc2[Zn2,Sp1]*RT3_Alloc2Down[Zn2,Sp1]+  RT3_FRAlloc3[Zn2,Sp1]*RT3_Alloc3Within[Zn2,Sp1]*(1-RT3_VoxHorExp3[Zn2,Sp1]  )+  RT3_FRAlloc3[Zn1,Sp1]*RT3_Alloc3Within[Zn1,Sp1]*(RT3_VoxHorExp3[Zn1,Sp1]/2)+  RT3_FRAlloc3[Zn3,Sp1]*RT3_Alloc3Within[Zn3,Sp1]*(RT3_VoxHorExp3[Zn3,Sp1]/2)+  RT3_FRAlloc4[Zn2,Sp1]*RT3_Alloc4Upw[Zn2,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L3FRtGr[Zn2,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc2[Zn2,Sp2]*RT3_Alloc2Down[Zn2,Sp2]+  RT3_FRAlloc3[Zn2,Sp2]*RT3_Alloc3Within[Zn2,Sp2]*(1-RT3_VoxHorExp3[Zn2,Sp2]  )+  RT3_FRAlloc3[Zn1,Sp2]*RT3_Alloc3Within[Zn1,Sp2]*(RT3_VoxHorExp3[Zn1,Sp2]/2)+  RT3_FRAlloc3[Zn3,Sp2]*RT3_Alloc3Within[Zn3,Sp2]*(RT3_VoxHorExp3[Zn3,Sp2]/2)+  RT3_FRAlloc4[Zn2,Sp2]*RT3_Alloc4Upw[Zn2,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L3FRtGr[Zn2,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc2[Zn2,Sp3]*RT3_Alloc2Down[Zn2,Sp3]+  RT3_FRAlloc3[Zn2,Sp3]*RT3_Alloc3Within[Zn2,Sp3]*(1-RT3_VoxHorExp3[Zn2,Sp3]  )+  RT3_FRAlloc3[Zn1,Sp3]*RT3_Alloc3Within[Zn1,Sp3]*(RT3_VoxHorExp3[Zn1,Sp3]/2)+  RT3_FRAlloc3[Zn3,Sp3]*RT3_Alloc3Within[Zn3,Sp3]*(RT3_VoxHorExp3[Zn3,Sp3]/2)+  RT3_FRAlloc4[Zn2,Sp3]*RT3_Alloc4Upw[Zn2,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L3FRtGr[Zn3,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc2[Zn3,Sp1]*RT3_Alloc2Down[Zn3,Sp1]+  RT3_FRAlloc3[Zn3,Sp1]*RT3_Alloc3Within[Zn3,Sp1]*(1-RT3_VoxHorExp3[Zn3,Sp1]  )+  RT3_FRAlloc3[Zn2,Sp1]*RT3_Alloc3Within[Zn2,Sp1]*(RT3_VoxHorExp3[Zn2,Sp1]/2)+  RT3_FRAlloc3[Zn4,Sp1]*RT3_Alloc3Within[Zn4,Sp1]*(RT3_VoxHorExp3[Zn4,Sp1]/2)+  RT3_FRAlloc4[Zn3,Sp1]*RT3_Alloc4Upw[Zn3,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L3FRtGr[Zn3,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc2[Zn3,Sp2]*RT3_Alloc2Down[Zn3,Sp2]+  RT3_FRAlloc3[Zn3,Sp2]*RT3_Alloc3Within[Zn3,Sp2]*(1-RT3_VoxHorExp3[Zn3,Sp2]  )+  RT3_FRAlloc3[Zn2,Sp2]*RT3_Alloc3Within[Zn2,Sp2]*(RT3_VoxHorExp3[Zn2,Sp2]/2)+  RT3_FRAlloc3[Zn4,Sp2]*RT3_Alloc3Within[Zn4,Sp2]*(RT3_VoxHorExp3[Zn4,Sp2]/2)+  RT3_FRAlloc4[Zn3,Sp2]*RT3_Alloc4Upw[Zn3,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L3FRtGr[Zn3,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc2[Zn3,Sp3]*RT3_Alloc2Down[Zn3,Sp3]+  RT3_FRAlloc3[Zn3,Sp3]*RT3_Alloc3Within[Zn3,Sp3]*(1-RT3_VoxHorExp3[Zn3,Sp3]  )+  RT3_FRAlloc3[Zn2,Sp3]*RT3_Alloc3Within[Zn2,Sp3]*(RT3_VoxHorExp3[Zn2,Sp3]/2)+  RT3_FRAlloc3[Zn4,Sp3]*RT3_Alloc3Within[Zn4,Sp3]*(RT3_VoxHorExp3[Zn4,Sp3]/2)+  RT3_FRAlloc4[Zn3,Sp3]*RT3_Alloc4Upw[Zn3,Sp3]))/RT3_RtAllocTot[Sp3]
      # RT3_L3FRtGr[Zn4,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc2[Zn4,Sp1]*RT3_Alloc2Down[Zn4,Sp1]+  RT3_FRAlloc3[Zn4,Sp1]*RT3_Alloc3Within[Zn4,Sp1]*(1-RT3_VoxHorExp3[Zn4,Sp1]/2)+  RT3_FRAlloc3[Zn3,Sp1]*RT3_Alloc3Within[Zn3,Sp1]*(RT3_VoxHorExp3[Zn3,Sp1]/2)+                                                                                RT3_FRAlloc4[Zn4,Sp1]*RT3_Alloc4Upw[Zn4,Sp1]))/RT3_RtAllocTot[Sp1]
      # RT3_L3FRtGr[Zn4,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc2[Zn4,Sp2]*RT3_Alloc2Down[Zn4,Sp2]+  RT3_FRAlloc3[Zn4,Sp2]*RT3_Alloc3Within[Zn4,Sp2]*(1-RT3_VoxHorExp3[Zn4,Sp2]/2)+  RT3_FRAlloc3[Zn3,Sp2]*RT3_Alloc3Within[Zn3,Sp2]*(RT3_VoxHorExp3[Zn3,Sp2]/2)+                                                                                RT3_FRAlloc4[Zn4,Sp2]*RT3_Alloc4Upw[Zn4,Sp2]))/RT3_RtAllocTot[Sp2]
      # RT3_L3FRtGr[Zn4,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc2[Zn4,Sp3]*RT3_Alloc2Down[Zn4,Sp3]+  RT3_FRAlloc3[Zn4,Sp3]*RT3_Alloc3Within[Zn4,Sp3]*(1-RT3_VoxHorExp3[Zn4,Sp3]/2)+  RT3_FRAlloc3[Zn3,Sp3]*RT3_Alloc3Within[Zn3,Sp3]*(RT3_VoxHorExp3[Zn3,Sp3]/2)+                                                                                RT3_FRAlloc4[Zn4,Sp3]*RT3_Alloc4Upw[Zn4,Sp3]))/RT3_RtAllocTot[Sp3]
      # 
      # RT3_L4FRtGr[Zn1,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc3[Zn1,Sp1]*RT3_Alloc3Down[Zn1,Sp1]+  RT3_FRAlloc4[Zn1,Sp1]*RT3_Alloc4Within[Zn1,Sp1]*(1-RT3_VoxHorExp4[Zn1,Sp1]/2)+  RT3_FRAlloc4[Zn2,Sp1]*RT3_Alloc4Within[Zn2,Sp1]*(RT3_VoxHorExp4[Zn2,Sp1]/2)                                                                                                                             ))/RT3_RtAllocTot[Sp1]
      # RT3_L4FRtGr[Zn1,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc3[Zn1,Sp2]*RT3_Alloc3Down[Zn1,Sp2]+  RT3_FRAlloc4[Zn1,Sp2]*RT3_Alloc4Within[Zn1,Sp2]*(1-RT3_VoxHorExp4[Zn1,Sp2]/2)+  RT3_FRAlloc4[Zn2,Sp2]*RT3_Alloc4Within[Zn2,Sp2]*(RT3_VoxHorExp4[Zn2,Sp2]/2)                                                                                                                             ))/RT3_RtAllocTot[Sp2]
      # RT3_L4FRtGr[Zn1,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc3[Zn1,Sp3]*RT3_Alloc3Down[Zn1,Sp3]+  RT3_FRAlloc4[Zn1,Sp3]*RT3_Alloc4Within[Zn1,Sp3]*(1-RT3_VoxHorExp4[Zn1,Sp3]/2)+  RT3_FRAlloc4[Zn2,Sp3]*RT3_Alloc4Within[Zn2,Sp3]*(RT3_VoxHorExp4[Zn2,Sp3]/2)                                                                                                                             ))/RT3_RtAllocTot[Sp3]
      # RT3_L4FRtGr[Zn2,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc3[Zn2,Sp1]*RT3_Alloc3Down[Zn2,Sp1]+  RT3_FRAlloc4[Zn2,Sp1]*RT3_Alloc4Within[Zn2,Sp1]*(1-RT3_VoxHorExp4[Zn2,Sp1]  )+  RT3_FRAlloc4[Zn1,Sp1]*RT3_Alloc4Within[Zn1,Sp1]*(RT3_VoxHorExp4[Zn1,Sp1]/2)+  RT3_FRAlloc4[Zn3,Sp1]*RT3_Alloc4Within[Zn3,Sp1]*(RT3_VoxHorExp4[Zn3,Sp1]/2)                                               ))/RT3_RtAllocTot[Sp1]
      # RT3_L4FRtGr[Zn2,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc3[Zn2,Sp2]*RT3_Alloc3Down[Zn2,Sp2]+  RT3_FRAlloc4[Zn2,Sp2]*RT3_Alloc4Within[Zn2,Sp2]*(1-RT3_VoxHorExp4[Zn2,Sp2]  )+  RT3_FRAlloc4[Zn1,Sp2]*RT3_Alloc4Within[Zn1,Sp2]*(RT3_VoxHorExp4[Zn1,Sp2]/2)+  RT3_FRAlloc4[Zn3,Sp2]*RT3_Alloc4Within[Zn3,Sp2]*(RT3_VoxHorExp4[Zn3,Sp2]/2)                                               ))/RT3_RtAllocTot[Sp2]
      # RT3_L4FRtGr[Zn2,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc3[Zn2,Sp3]*RT3_Alloc3Down[Zn2,Sp3]+  RT3_FRAlloc4[Zn2,Sp3]*RT3_Alloc4Within[Zn2,Sp3]*(1-RT3_VoxHorExp4[Zn2,Sp3]  )+  RT3_FRAlloc4[Zn1,Sp3]*RT3_Alloc4Within[Zn1,Sp3]*(RT3_VoxHorExp4[Zn1,Sp3]/2)+  RT3_FRAlloc4[Zn3,Sp3]*RT3_Alloc4Within[Zn3,Sp3]*(RT3_VoxHorExp4[Zn3,Sp3]/2)                                               ))/RT3_RtAllocTot[Sp3]
      # RT3_L4FRtGr[Zn3,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc3[Zn3,Sp1]*RT3_Alloc3Down[Zn3,Sp1]+  RT3_FRAlloc4[Zn3,Sp1]*RT3_Alloc4Within[Zn3,Sp1]*(1-RT3_VoxHorExp4[Zn3,Sp1]  )+  RT3_FRAlloc4[Zn2,Sp1]*RT3_Alloc4Within[Zn2,Sp1]*(RT3_VoxHorExp4[Zn2,Sp1]/2)+  RT3_FRAlloc4[Zn4,Sp1]*RT3_Alloc4Within[Zn4,Sp1]*(RT3_VoxHorExp4[Zn4,Sp1]/2)                                               ))/RT3_RtAllocTot[Sp1]
      # RT3_L4FRtGr[Zn3,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc3[Zn3,Sp2]*RT3_Alloc3Down[Zn3,Sp2]+  RT3_FRAlloc4[Zn3,Sp2]*RT3_Alloc4Within[Zn3,Sp2]*(1-RT3_VoxHorExp4[Zn3,Sp2]  )+  RT3_FRAlloc4[Zn2,Sp2]*RT3_Alloc4Within[Zn2,Sp2]*(RT3_VoxHorExp4[Zn2,Sp2]/2)+  RT3_FRAlloc4[Zn4,Sp2]*RT3_Alloc4Within[Zn4,Sp2]*(RT3_VoxHorExp4[Zn4,Sp2]/2)                                               ))/RT3_RtAllocTot[Sp2]
      # RT3_L4FRtGr[Zn3,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc3[Zn3,Sp3]*RT3_Alloc3Down[Zn3,Sp3]+  RT3_FRAlloc4[Zn3,Sp3]*RT3_Alloc4Within[Zn3,Sp3]*(1-RT3_VoxHorExp4[Zn3,Sp3]  )+  RT3_FRAlloc4[Zn2,Sp3]*RT3_Alloc4Within[Zn2,Sp3]*(RT3_VoxHorExp4[Zn2,Sp3]/2)+  RT3_FRAlloc4[Zn4,Sp3]*RT3_Alloc4Within[Zn4,Sp3]*(RT3_VoxHorExp4[Zn4,Sp3]/2)                                               ))/RT3_RtAllocTot[Sp3]
      # RT3_L4FRtGr[Zn4,Sp1] = if RT3_RtAllocTot[Sp1]=0 then 0 else (RT3_FineRootInc[Sp1]*(RT3_FRAlloc3[Zn4,Sp1]*RT3_Alloc3Down[Zn4,Sp1]+  RT3_FRAlloc4[Zn4,Sp1]*RT3_Alloc4Within[Zn4,Sp1]*(1-RT3_VoxHorExp4[Zn4,Sp1]/2)+  RT3_FRAlloc4[Zn3,Sp1]*RT3_Alloc4Within[Zn3,Sp1]*(RT3_VoxHorExp4[Zn3,Sp1]/2)                                                                                                                             ))/RT3_RtAllocTot[Sp1]
      # RT3_L4FRtGr[Zn4,Sp2] = if RT3_RtAllocTot[Sp2]=0 then 0 else (RT3_FineRootInc[Sp2]*(RT3_FRAlloc3[Zn4,Sp2]*RT3_Alloc3Down[Zn4,Sp2]+  RT3_FRAlloc4[Zn4,Sp2]*RT3_Alloc4Within[Zn4,Sp2]*(1-RT3_VoxHorExp4[Zn4,Sp2]/2)+  RT3_FRAlloc4[Zn3,Sp2]*RT3_Alloc4Within[Zn3,Sp2]*(RT3_VoxHorExp4[Zn3,Sp2]/2)                                                                                                                             ))/RT3_RtAllocTot[Sp2]
      # RT3_L4FRtGr[Zn4,Sp3] = if RT3_RtAllocTot[Sp3]=0 then 0 else (RT3_FineRootInc[Sp3]*(RT3_FRAlloc3[Zn4,Sp3]*RT3_Alloc3Down[Zn4,Sp3]+  RT3_FRAlloc4[Zn4,Sp3]*RT3_Alloc4Within[Zn4,Sp3]*(1-RT3_VoxHorExp4[Zn4,Sp3]/2)+  RT3_FRAlloc4[Zn3,Sp3]*RT3_Alloc4Within[Zn3,Sp3]*(RT3_VoxHorExp4[Zn3,Sp3]/2)                                                                                                                             ))/RT3_RtAllocTot[Sp3]
      zonelayertree_df$RT3_RtAllocTot <- rep(tree_df$RT3_RtAllocTot, each = nzone*nlayer)
      
      zonelayertree_df$RT3_AllocDown_FRA <- zonelayertree_df$RT3_FRAlloc * zonelayertree_df$RT3_AllocDown
      zonelayertree_df$RT3_AllocDown_calc <- 0
      zonelayertree_df[zonelayertree_df$layer %in% c(2:4),]$RT3_AllocDown_calc <- zonelayertree_df[zonelayertree_df$layer %in% c(1:3),]$RT3_AllocDown_FRA
      
      zonelayertree_df$RT3_VoxHorExp_z14half <- zonelayertree_df$RT3_VoxHorExp
      zonelayertree_df[zonelayertree_df$zone %in% c(1,4),]$RT3_VoxHorExp_z14half <- zonelayertree_df[zonelayertree_df$zone %in% c(1,4),]$RT3_VoxHorExp/2
      
      zonelayertree_df$RT3_AllocWithin_calc <- zonelayertree_df$RT3_FRAlloc* zonelayertree_df$RT3_AllocWithin*(zonelayertree_df$RT3_VoxHorExp/2)
      zonelayertree_df$RT3_AllocWithin_calc_zswitch <- NA
      zonelayertree_df[zonelayertree_df$zone == 1, ]$RT3_AllocWithin_calc_zswitch <- zonelayertree_df[zonelayertree_df$zone == 2, ]$RT3_AllocWithin_calc
      zonelayertree_df[zonelayertree_df$zone == 2, ]$RT3_AllocWithin_calc_zswitch <- zonelayertree_df[zonelayertree_df$zone == 1, ]$RT3_AllocWithin_calc
      zonelayertree_df[zonelayertree_df$zone == 3, ]$RT3_AllocWithin_calc_zswitch <- zonelayertree_df[zonelayertree_df$zone == 2, ]$RT3_AllocWithin_calc
      zonelayertree_df[zonelayertree_df$zone == 4, ]$RT3_AllocWithin_calc_zswitch <- zonelayertree_df[zonelayertree_df$zone == 3, ]$RT3_AllocWithin_calc
      zonelayertree_df[zonelayertree_df$zone %in% c(2, 3), ]$RT3_AllocWithin_calc_zswitch <- zonelayertree_df[zonelayertree_df$zone %in% c(2, 3), ]$RT3_AllocWithin_calc_zswitch + zonelayertree_df[zonelayertree_df$zone %in% c(3, 4), ]$RT3_AllocWithin_calc
      
      zonelayertree_df$RT3_AllocUpw_calc <- 0
      zonelayertree_df$RT3_AllocUpw_FRA <- zonelayertree_df$RT3_FRAlloc *
        zonelayertree_df$RT3_AllocUpw
      zonelayertree_df[zonelayertree_df$layer %in% c(1:3), ]$RT3_AllocUpw_calc <- zonelayertree_df[zonelayertree_df$layer %in% c(2:4), ]$RT3_AllocUpw_FRA
      
      zonelayertree_df$RT3_LFRtGr <- ifelse(
        zonelayertree_df$RT3_RtAllocTot == 0,
        0,
        (
          zonelayertree_df$RT3_FineRootInc * (
            zonelayertree_df$RT3_AllocDown_calc +
              zonelayertree_df$RT3_FRAlloc * zonelayertree_df$RT3_AllocWithin * (1 - zonelayertree_df$RT3_VoxHorExp_z14half) +
              zonelayertree_df$RT3_AllocWithin_calc_zswitch +
              zonelayertree_df$RT3_AllocUpw_calc
          )
        ) / zonelayertree_df$RT3_RtAllocTot
      )
      
      # RT3_DecFrac_L1[Zone,Tree] = ((RT3_SoilTL1[Zone]/20)^RT3_TempRespforRtD)/Rt_T_MeanResTime[Tree] 
      # RT3_DecFrac_L2[Zone,Tree] = ((RT3_SoilTL2[Zone]/20)^RT3_TempRespforRtD)/Rt_T_MeanResTime[Tree] 
      # RT3_DecFrac_L3[Zone,Tree] = ((RT3_SoilTL3[Zone]/20)^RT3_TempRespforRtD)/Rt_T_MeanResTime[Tree] 
      # RT3_DecFrac_L4[Zone,Tree] = ((RT3_SoilTL4[Zone]/20)^RT3_TempRespforRtD)/Rt_T_MeanResTime[Tree] 
      zonelayertree_df$RT3_SoilT <- rep(zonelayer_df$RT3_SoilT, ntree)
      zonelayertree_df$Rt_T_MeanResTime <- rep(tree_df$Rt_T_MeanResTime, each = nzone*nlayer)
      zonelayertree_df$RT3_DecFrac <- ((zonelayertree_df$RT3_SoilT/20)^pars$Rt_par$RT3_TempRespforRtD)/zonelayertree_df$Rt_T_MeanResTime

      # RT3_L1FRtMort[Zone,Tree] = RT_L1FRLength[Zone,Tree]*RT3_DecFrac_L1[Zone,Tree]
      # RT3_L2FRtMort[Zone,Tree] = RT_L2FRLength[Zone,Tree]*RT3_DecFrac_L2[Zone,Tree]
      # RT3_L3FRtMort[Zone,Tree] = RT_L3FRLength[Zone,Tree]*RT3_DecFrac_L3[Zone,Tree]
      # RT3_L4FRtMort[Zone,Tree] = RT_L4FRLength[Zone,Tree]*RT3_DecFrac_L4[Zone,Tree]
      zonelayertree_df$RT3_FRtMort <- zonelayertree_df$RT_FRLength * zonelayertree_df$RT3_DecFrac
      
      # RT_L1FRLength[Zone,Tree](t) = RT_L1FRLength[Zone,Tree](t - dt) + (RT3_L1FRtGr[Zone,Tree] - RT3_L1FRtMort[Zone,Tree]) * dt
      # RT_L2FRLength[Zone,Tree](t) = RT_L2FRLength[Zone,Tree](t - dt) + (RT3_L2FRtGr[Zone,Tree] - RT3_L2FRtMort[Zone,Tree]) * dt
      # RT_L3FRLength[Zone,Tree](t) = RT_L3FRLength[Zone,Tree](t - dt) + (RT3_L3FRtGr[Zone,Tree] - RT3_L3FRtMort[Zone,Tree]) * dt
      # RT_L4FRLength[Zone,Tree](t) = RT_L4FRLength[Zone,Tree](t - dt) + (RT3_L4FRtGr[Zone,Tree] - RT3_L4FRtMort[Zone,Tree]) * dt
      zonelayertree_df$RT_FRLength <- zonelayertree_df$RT_FRLength + (zonelayertree_df$RT3_LFRtGr - zonelayertree_df$RT3_FRtMort)
      
      # Rt_TLraCD[Tree](t) = Rt_TLraCD[Tree](t - dt)
      # Rt_TLraCD[Tree](t) = Rt_TLraCD[Tree](t - dt)
      
      # Rt_TSpecrolT1[Zone,SoilLayer] = T_SRLfineroots[Sp1]
      # Rt_TSpecrolT2[Zone,SoilLayer] = T_SRLfineroots[Sp2]
      # Rt_TSpecrolT3[Zone,SoilLayer] = T_SRLfineroots[Sp3]
      zonelayertree_df$Rt_TSpecrol <- rep(tree_df$T_SRLfineroots, each = nzone*nlayer)
      
      
      # T_Root_Drv_T1[Zn1,1] = if Rt_TSpecrolT1[Zn1,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn1,Sp1]/Rt_TSpecrolT1[Zn1,1]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn1,2] = if Rt_TSpecrolT1[Zn1,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn1,Sp1]/Rt_TSpecrolT1[Zn1,2]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn1,3] = if Rt_TSpecrolT1[Zn1,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn1,Sp1]/Rt_TSpecrolT1[Zn1,3]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn1,4] = if Rt_TSpecrolT1[Zn1,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn1,Sp1]/Rt_TSpecrolT1[Zn1,4]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn2,1] = if Rt_TSpecrolT1[Zn2,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn2,Sp1]/Rt_TSpecrolT1[Zn2,1]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn2,2] = if Rt_TSpecrolT1[Zn2,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn2,Sp1]/Rt_TSpecrolT1[Zn2,1]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn2,3] = if Rt_TSpecrolT1[Zn2,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn2,Sp1]/Rt_TSpecrolT1[Zn2,3]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn2,4] = if Rt_TSpecrolT1[Zn2,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn2,Sp1]/Rt_TSpecrolT1[Zn2,4]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn3,1] = if Rt_TSpecrolT1[Zn3,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn3,Sp1]/Rt_TSpecrolT1[Zn3,1]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn3,2] = if Rt_TSpecrolT1[Zn3,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn3,Sp1]/Rt_TSpecrolT1[Zn3,2]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn3,3] = if Rt_TSpecrolT1[Zn3,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn3,Sp1]/Rt_TSpecrolT1[Zn3,3]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn3,4] = if Rt_TSpecrolT1[Zn3,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn3,Sp1]/Rt_TSpecrolT1[Zn3,4]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn4,1] = if Rt_TSpecrolT1[Zn4,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn4,Sp1]/Rt_TSpecrolT1[Zn4,1]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn4,2] = if Rt_TSpecrolT1[Zn4,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn4,Sp1]/Rt_TSpecrolT1[Zn4,2]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn4,3] = if Rt_TSpecrolT1[Zn4,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn4,Sp1]/Rt_TSpecrolT1[Zn4,3]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T1[Zn4,4] = if Rt_TSpecrolT1[Zn4,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn4,Sp1]/Rt_TSpecrolT1[Zn4,4]+ 0*(Rt_TLrvL1[Zn1,Sp1]+Rt_TLrvL2[Zn1,Sp1]+Rt_TLrvL3[Zn1,Sp1]+Rt_TLrvL4[Zn1,Sp1])
      # T_Root_Drv_T2[Zn1,1] = if Rt_TSpecrolT2[Zn1,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn1,Sp2]/Rt_TSpecrolT2[Zn1,1]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn1,2] = if Rt_TSpecrolT2[Zn1,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn1,Sp2]/Rt_TSpecrolT2[Zn1,2]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn1,3] = if Rt_TSpecrolT2[Zn1,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn1,Sp2]/Rt_TSpecrolT2[Zn1,3]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn1,4] = if Rt_TSpecrolT2[Zn1,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn1,Sp2]/Rt_TSpecrolT2[Zn1,4]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn2,1] = if Rt_TSpecrolT2[Zn2,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn2,Sp2]/Rt_TSpecrolT2[Zn2,1]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn2,2] = if Rt_TSpecrolT2[Zn2,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn2,Sp2]/Rt_TSpecrolT2[Zn2,2]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn2,3] = if Rt_TSpecrolT2[Zn2,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn2,Sp2]/Rt_TSpecrolT2[Zn2,3]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn2,4] = if Rt_TSpecrolT2[Zn2,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn2,Sp2]/Rt_TSpecrolT2[Zn2,4]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn3,1] = if Rt_TSpecrolT2[Zn3,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn3,Sp2]/Rt_TSpecrolT2[Zn3,1]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn3,2] = if Rt_TSpecrolT2[Zn3,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn3,Sp2]/Rt_TSpecrolT2[Zn3,2]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn3,3] = if Rt_TSpecrolT2[Zn3,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn3,Sp2]/Rt_TSpecrolT2[Zn3,3]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn3,4] = if Rt_TSpecrolT2[Zn3,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn3,Sp2]/Rt_TSpecrolT2[Zn3,4]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn4,1] = if Rt_TSpecrolT2[Zn4,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn4,Sp2]/Rt_TSpecrolT2[Zn4,1]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn4,2] = if Rt_TSpecrolT2[Zn4,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn4,Sp2]/Rt_TSpecrolT2[Zn4,2]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn4,3] = if Rt_TSpecrolT2[Zn4,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn4,Sp2]/Rt_TSpecrolT2[Zn4,3]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T2[Zn4,4] = if Rt_TSpecrolT2[Zn4,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn4,Sp2]/Rt_TSpecrolT2[Zn4,4]+ 0*(Rt_TLrvL1[Zn1,Sp2]+Rt_TLrvL2[Zn1,Sp2]+Rt_TLrvL3[Zn1,Sp2]+Rt_TLrvL4[Zn1,Sp2])
      # T_Root_Drv_T3[Zn1,1] = if Rt_TSpecrolT3[Zn1,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn1,Sp3]/Rt_TSpecrolT3[Zn1,1]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn1,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn1,2] = if Rt_TSpecrolT3[Zn1,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn1,Sp3]/Rt_TSpecrolT3[Zn1,2]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn1,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn1,3] = if Rt_TSpecrolT3[Zn1,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn1,Sp3]/Rt_TSpecrolT3[Zn1,3]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn1,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn1,4] = if Rt_TSpecrolT3[Zn1,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn1,Sp3]/Rt_TSpecrolT3[Zn1,4]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn1,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn2,1] = if Rt_TSpecrolT3[Zn2,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn2,Sp3]/Rt_TSpecrolT3[Zn2,1]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn2,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn2,2] = if Rt_TSpecrolT3[Zn2,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn2,Sp3]/Rt_TSpecrolT3[Zn2,2]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn2,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn2,3] = if Rt_TSpecrolT3[Zn2,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn2,Sp3]/Rt_TSpecrolT3[Zn2,3]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn2,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn2,4] = if Rt_TSpecrolT3[Zn2,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn2,Sp3]/Rt_TSpecrolT3[Zn2,4]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn2,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn3,1] = if Rt_TSpecrolT3[Zn3,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn3,Sp3]/Rt_TSpecrolT3[Zn3,1]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn3,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn3,2] = if Rt_TSpecrolT3[Zn3,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn3,Sp3]/Rt_TSpecrolT3[Zn3,2]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn3,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn3,3] = if Rt_TSpecrolT3[Zn3,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn3,Sp3]/Rt_TSpecrolT3[Zn3,3]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn3,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn3,4] = if Rt_TSpecrolT3[Zn3,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn3,Sp3]/Rt_TSpecrolT3[Zn3,4]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn3,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn4,1] = if Rt_TSpecrolT3[Zn4,1] = 0 then 0 else 0.01*Rt_TLrvL1[Zn4,Sp3]/Rt_TSpecrolT3[Zn4,1]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn4,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn4,2] = if Rt_TSpecrolT3[Zn4,2] = 0 then 0 else 0.01*Rt_TLrvL2[Zn4,Sp3]/Rt_TSpecrolT3[Zn4,2]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn4,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn4,3] = if Rt_TSpecrolT3[Zn4,3] = 0 then 0 else 0.01*Rt_TLrvL3[Zn4,Sp3]/Rt_TSpecrolT3[Zn4,3]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn4,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      # T_Root_Drv_T3[Zn4,4] = if Rt_TSpecrolT3[Zn4,4] = 0 then 0 else 0.01*Rt_TLrvL4[Zn4,Sp3]/Rt_TSpecrolT3[Zn4,4]+ 0*(Rt_TLrvL1[Zn1,Sp3]+Rt_TLrvL2[Zn4,Sp3]+Rt_TLrvL3[Zn1,Sp3]+Rt_TLrvL4[Zn1,Sp3])
      zonelayertree_df$T_Root_Drv <- ifelse( zonelayertree_df$Rt_TSpecrol == 0, 0, 0.01* zonelayertree_df$Rt_TLrv/ zonelayertree_df$Rt_TSpecrol)
      
      # T_Root_Dra_T1_ZnL[Zone,SoilLayer] = 1000*AF_Depths[SoilLayer]*T_Root_Drv_T1[Zone,SoilLayer]
      # T_Root_Dra_T2_ZnL[Zone,SoilLayer] = 1000*AF_Depths[SoilLayer]*T_Root_Drv_T2[Zone,SoilLayer]
      # T_Root_Dra_T3_ZnL[Zone,SoilLayer] = 1000*AF_Depths[SoilLayer]*T_Root_Drv_T3[Zone,SoilLayer]
      zonelayertree_df$AF_Depths <- rep(rep(layer_df$AF_Depths, each = nzone), ntree)
      zonelayertree_df$T_Root_Dra_ZnL <- 1000* zonelayertree_df$AF_Depths* zonelayertree_df$T_Root_Drv
      
      # T_RootInp_T1_ZnL[Zone,SoilLayer] = T_Root_Dra_T1_ZnL[Zone,SoilLayer]*(if time = T_PlantTime[Sp1] then 1 else (if time > T_PlantTime[Sp1] then 1/Rt_T_MeanResTime[Sp1] else 0))
      # T_Root_Inp_T2_ZnL[Zone,SoilLayer] = T_Root_Dra_T2_ZnL[Zone,SoilLayer]*(if time = T_PlantTime[Sp2] then 1 else (if time > T_PlantTime[Sp2] then 1/Rt_T_MeanResTime[Sp2] else 0))
      # T_Root_Inp_T3_ZnL[Zone,SoilLayer] = T_Root_Dra_T3_ZnL[Zone,SoilLayer]*(if time = T_PlantTime[Sp3] then 1 else (if time > T_PlantTime[Sp3] then 1/Rt_T_MeanResTime[Sp3] else 0))
      zonelayertree_df$T_PlantTime <- rep(tree_df$T_PlantTime, each = nzone*nlayer)
      zonelayertree_df$T_Root_Inp_ZnL <- zonelayertree_df$T_Root_Dra_ZnL * ifelse(
        time == zonelayertree_df$T_PlantTime,
        1,
        ifelse(
          time > zonelayertree_df$T_PlantTime,
          1 / zonelayertree_df$Rt_T_MeanResTime,
          0
        )
      )
      
      # T_Root_T1_DWInc[Zone,SoilLayer] = if Rt_ATType[Sp1] < 2 then T_RootInp_T1_ZnL[Zone,SoilLayer] else T_RootInc[DW,Sp1]*T_T1RelRtIncrTyp2[Zone,SoilLayer]
      # T_Root_T2_DWInc[Zone,SoilLayer] = if Rt_ATType[Sp2] < 2 then T_Root_Inp_T2_ZnL[Zone,SoilLayer] else T_RootInc[DW,Sp2]*T_T2RelRtIncrTyp2[Zone,SoilLayer]
      # T_Root_T3_DWInc[Zone,SoilLayer] = if Rt_ATType[Sp3] < 2 then T_Root_Inp_T3_ZnL[Zone,SoilLayer] else T_RootInc[DW,Sp3]*T_T3RelRtIncrTyp2[Zone,SoilLayer]
      # T_Root_T1_NInc[Zone,SoilLayer] = if Rt_ATType[Sp1] < 2 then T_RootInp_T1_ZnL[Zone,SoilLayer]*(T_UnitConv[N]*T_RtConc[N,Sp1]) else T_RootInc[N,Sp1]*T_T1RelRtIncrTyp2[Zone,SoilLayer]
      # T_Root_T2_NInc[Zone,SoilLayer] = if Rt_ATType[Sp2] < 2 then T_Root_Inp_T2_ZnL[Zone,SoilLayer]*(T_UnitConv[N]*T_RtConc[N,Sp2]) else T_RootInc[N,Sp2]*T_T2RelRtIncrTyp2[Zone,SoilLayer]
      # T_Root_T3_NInc[Zone,SoilLayer] = if Rt_ATType[Sp3] < 2 then T_Root_Inp_T3_ZnL[Zone,SoilLayer]*(T_UnitConv[N]*T_RtConc[N,Sp3]) else T_RootInc[N,Sp3]*T_T3RelRtIncrTyp2[Zone,SoilLayer]
      # T_Root_T1_PInc[Zone,SoilLayer] = if Rt_ATType[Sp1] < 2 then T_RootInp_T1_ZnL[Zone,SoilLayer]*(T_UnitConv[P]*T_RtConc[P,Sp1]) else T_RootInc[P,Sp1]*T_T1RelRtIncrTyp2[Zone,SoilLayer]
      # T_Root_T2_PInc[Zone,SoilLayer] = if Rt_ATType[Sp2] < 2 then T_Root_Inp_T2_ZnL[Zone,SoilLayer]*(T_UnitConv[P]*T_RtConc[P,Sp2]) else T_RootInc[P,Sp2]*T_T2RelRtIncrTyp2[Zone,SoilLayer]
      # T_Root_T3_PInc[Zone,SoilLayer] = if Rt_ATType[Sp3] < 2 then T_Root_Inp_T3_ZnL[Zone,SoilLayer]*(T_UnitConv[P]*T_RtConc[P,Sp3]) else T_RootInc[P,Sp3]*T_T3RelRtIncrTyp2[Zone,SoilLayer]
      zonelayertreepcomp_df$Rt_ATType <- rep(rep(tree_df$Rt_ATType, each = nzone*nlayer), nrow(pcomp_df))
      zonelayertreepcomp_df$T_Root_Inp_ZnL <- rep(zonelayertree_df$T_Root_Inp_ZnL, nrow(pcomp_df))
      zonelayertreepcomp_df$T_RootInc <- rep(treepcomp_df$T_RootInc, each = nzone*nlayer)
      zonelayertreepcomp_df$T_T1RelRtIncrTyp2 <- rep(zonelayertree_df$T_RelRtIncrTyp2, nrow(pcomp_df))
      zonelayertreepcomp_df$T_Root_Inc <- ifelse( zonelayertreepcomp_df$Rt_ATType < 2, zonelayertreepcomp_df$T_Root_Inp_ZnL, zonelayertreepcomp_df$T_RootInc * zonelayertreepcomp_df$T_RelRtIncrTyp2)
      
      # T_RootT1DW[Zone,SoilLayer](t) = T_RootT1DW[Zone,SoilLayer](t - dt) + (T_Root_T1_DWInc[Zone,SoilLayer] - T_Root_T1_DWdecay[Zone,SoilLayer]) * dt
      # T_RootT2DW[Zone,SoilLayer](t) = T_RootT2DW[Zone,SoilLayer](t - dt) + (T_Root_T2_DWInc[Zone,SoilLayer] - T_Root_T2_DWdecay[Zone,SoilLayer]) * dt
      # T_RootT3DW[Zone,SoilLayer](t) = T_RootT3DW[Zone,SoilLayer](t - dt) + (T_Root_T3_DWInc[Zone,SoilLayer] - T_Root_T3_DWdecay[Zone,SoilLayer]) * dt
      # T_RootT1N[Zone,SoilLayer](t) = T_RootT1N[Zone,SoilLayer](t - dt) + (T_Root_T1_NInc[Zone,SoilLayer] - T_Root_T1_Ndecay[Zone,SoilLayer]) * dt
      # T_RootT2N[Zone,SoilLayer](t) = T_RootT2N[Zone,SoilLayer](t - dt) + (T_Root_T2_NInc[Zone,SoilLayer] - T_Root_T2_Ndecay[Zone,SoilLayer]) * dt
      # T_RootT3N[Zone,SoilLayer](t) = T_RootT3N[Zone,SoilLayer](t - dt) + (T_Root_T3_NInc[Zone,SoilLayer] - T_Root_T3_Ndecay[Zone,SoilLayer]) * dt
      # T_RootT1P[Zone,SoilLayer](t) = T_RootT1P[Zone,SoilLayer](t - dt) + (T_Root_T1_PInc[Zone,SoilLayer] - T_Root_T1_Pdecay[Zone,SoilLayer]) * dt
      # T_RootT2P[Zone,SoilLayer](t) = T_RootT2P[Zone,SoilLayer](t - dt) + (T_Root_T2_PInc[Zone,SoilLayer] - T_Root_T2_Pdecay[Zone,SoilLayer]) * dt
      # T_RootT3P[Zone,SoilLayer](t) = T_RootT3P[Zone,SoilLayer](t - dt) + (T_Root_T3_PInc[Zone,SoilLayer] - T_Root_T3_Pdecay[Zone,SoilLayer]) * dt
      zonelayertreepcomp_df$T_Root <- zonelayertreepcomp_df$T_Root + (zonelayertreepcomp_df$T_Root_Inc - zonelayertreepcomp_df$T_Root_decay) 
      
      # Rt_TRelChangeV[Tree] = IF(Rt_ATType[Tree]=2 AND T_Prun[DW, Tree]=0 AND T_LfTwig[DW,Tree]>0 AND T_PrunLapse[Tree]>T_PrunRecov[Tree])THEN((T_RootInc[DW, Tree])*1000*Rt_TSRL[Tree]/Rt_TlraX0Curr[Tree])ELSE(0)
      # Rt_TRelChangeH[Tree] = IF(Rt_ATType[Tree]=2 AND T_Prun[DW, Tree]=0 AND T_LfTwig[DW,Tree]>0 AND T_PrunLapse[Tree]>T_PrunRecov[Tree])THEN((T_RootInc[DW, Tree])*1000*Rt_TSRL[Tree]/Rt_TlraX0Curr[Tree])ELSE(0)
      tree_df$Rt_TRelChangeH <- ifelse(
        tree_df$Rt_ATType == 2 &
          treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_Prun == 0 &
          treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_LfTwig > 0 &
          tree_df$T_PrunLapse > tree_df$T_PrunRecov,
        treepcomp_df[treepcomp_df$PlantComp == "DW", ]$T_RootInc *
          1000 * tree_df$Rt_TSRL / tree_df$Rt_TlraX0Curr,
        0
      )
      #TODO: confirm that this is equals
      tree_df$Rt_TRelChangeV <- tree_df$Rt_TRelChangeH
      
      # T_NT1UptZn[Zone,SlNut] = N_T1Upt1[Zone,SlNut]+N_T1Upt2[Zone,SlNut]+N_T1Upt3[Zone,SlNut]+N_T1Upt4[Zone,SlNut]
      # T_NT2UptZn[Zone,SlNut] = N_T2Upt1[Zone,SlNut]+N_T2Upt2[Zone,SlNut]+N_T2Upt3[Zone,SlNut]+N_T2Upt4[Zone,SlNut]
      # T_NT3UptZn[Zone,SlNut] = N_T3Upt1[Zone,SlNut]+N_T3Upt2[Zone,SlNut]+N_T3Upt3[Zone,SlNut]+N_T3Upt4[Zone,SlNut]
      zonetreenut_df$T_NUptZn <- aggregate(zonelayertreenut_df["N_Upt"], zonelayertreenut_df[c("zone", "tree_id", "SlNut")], sum)$N_Upt

      # T_NUptTree[N,Sp1] = T_NT1UptZn[Zn1,N]*AF_ZoneFrac[Zn1]+T_NT1UptZn[Zn2,N]*AF_ZoneFrac[Zn2]+T_NT1UptZn[Zn3,N]*AF_ZoneFrac[Zn3]+T_NT1UptZn[Zn4,N]*AF_ZoneFrac[Zn4]+0*(T_NT2UptZn[Zn1,N]+T_NT2UptZn[Zn2,N]+T_NT2UptZn[Zn3,N]+T_NT2UptZn[Zn4,N]+T_NT3UptZn[Zn1,N]+T_NT3UptZn[Zn2,N]+T_NT3UptZn[Zn3,N]+T_NT3UptZn[Zn4,N])
      # T_NUptTree[N,Sp2] = T_NT2UptZn[Zn1,N]*AF_ZoneFrac[Zn1]+T_NT2UptZn[Zn2,N]*AF_ZoneFrac[Zn2]+T_NT2UptZn[Zn3,N]*AF_ZoneFrac[Zn3]+T_NT2UptZn[Zn4,N]*AF_ZoneFrac[Zn4]+0*(T_NT1UptZn[Zn1,N]+T_NT1UptZn[Zn2,N]+T_NT1UptZn[Zn3,N]+T_NT1UptZn[Zn4,N]+T_NT3UptZn[Zn1,N]+T_NT3UptZn[Zn2,N]+T_NT3UptZn[Zn3,N]+T_NT3UptZn[Zn4,N])
      # T_NUptTree[N,Sp3] = T_NT3UptZn[Zn1,N]*AF_ZoneFrac[Zn1]+T_NT3UptZn[Zn2,N]*AF_ZoneFrac[Zn2]+T_NT3UptZn[Zn3,N]*AF_ZoneFrac[Zn3]+T_NT3UptZn[Zn4,N]*AF_ZoneFrac[Zn4]+0*(T_NT2UptZn[Zn1,N]+T_NT2UptZn[Zn2,N]+T_NT2UptZn[Zn3,N]+T_NT2UptZn[Zn4,N]+T_NT1UptZn[Zn1,N]+T_NT1UptZn[Zn2,N]+T_NT1UptZn[Zn3,N]+T_NT1UptZn[Zn4,N])
      # T_NUptTree[P,Sp1] = T_NT1UptZn[Zn1,P]*AF_ZoneFrac[Zn1]+T_NT1UptZn[Zn2,P]*AF_ZoneFrac[Zn2]+T_NT1UptZn[Zn3,P]*AF_ZoneFrac[Zn3]+T_NT1UptZn[Zn4,P]*AF_ZoneFrac[Zn4]+0*(T_NT2UptZn[Zn1,P]+T_NT2UptZn[Zn2,P]+T_NT2UptZn[Zn3,P]+T_NT2UptZn[Zn4,P]+T_NT3UptZn[Zn1,P]+T_NT3UptZn[Zn2,P]+T_NT3UptZn[Zn3,P]+T_NT3UptZn[Zn4,P])
      # T_NUptTree[P,Sp2] = T_NT2UptZn[Zn1,P]*AF_ZoneFrac[Zn1]+T_NT2UptZn[Zn2,P]*AF_ZoneFrac[Zn2]+T_NT2UptZn[Zn3,P]*AF_ZoneFrac[Zn3]+T_NT2UptZn[Zn4,P]*AF_ZoneFrac[Zn4]+0*(T_NT1UptZn[Zn1,P]+T_NT1UptZn[Zn2,P]+T_NT1UptZn[Zn3,P]+T_NT1UptZn[Zn4,P]+T_NT3UptZn[Zn1,P]+T_NT3UptZn[Zn2,P]+T_NT3UptZn[Zn3,P]+T_NT3UptZn[Zn4,P])
      # T_NUptTree[P,Sp3] = T_NT3UptZn[Zn1,P]*AF_ZoneFrac[Zn1]+T_NT3UptZn[Zn2,P]*AF_ZoneFrac[Zn2]+T_NT3UptZn[Zn3,P]*AF_ZoneFrac[Zn3]+T_NT3UptZn[Zn4,P]*AF_ZoneFrac[Zn4]+0*(T_NT1UptZn[Zn1,P]+T_NT1UptZn[Zn2,P]+T_NT1UptZn[Zn3,P]+T_NT1UptZn[Zn4,P]+T_NT2UptZn[Zn1,P]+T_NT2UptZn[Zn2,P]+T_NT2UptZn[Zn3,P]+T_NT2UptZn[Zn4,P])
      zonetreenut_df$AF_ZoneFrac <- rep(zonetree_df$AF_ZoneFrac, nrow(nut_df))
      zonetreenut_df$T_NUptZn_Frac <- zonetreenut_df$T_NUptZn* zonetreenut_df$AF_ZoneFrac
      treenut_df$T_NUptTree <- aggregate(zonetreenut_df["T_NUptZn_Frac"], zonetreenut_df[c("tree_id", "SlNut")], sum)$T_NUptZn_Frac
      
      # Rt_TAmountH[Zone,Tree] = Rt_TLrv1[Zone,Tree]*AF_DepthAct1[Zone]+Rt_TLrv2[Zone,Tree]*AF_Depth2[Zone]+Rt_TLrv3[Zone,Tree]*AF_Depth3[Zone]+Rt_TLrv4[Zone,Tree]*AF_Depth4[Zone]
      zonelayertree_df$Rt_TLrv_depth <- zonelayertree_df$Rt_TLrv*zonelayertree_df$AF_Depth
      zonetree_df$Rt_TAmountH <- aggregate(zonelayertree_df["Rt_TLrv_depth"], zonelayertree_df[c("zone", "tree_id")], sum)$Rt_TLrv_depth 
      
      # Rt_TAmountHTree[Sp1] = Rt_TAmountH[Zn1,Sp1]*AF_ZoneFrac[Zn1]+Rt_TAmountH[Zn2,Sp1]*AF_ZoneFrac[Zn2]+Rt_TAmountH[Zn3,Sp1]*AF_ZoneFrac[Zn3]+Rt_TAmountH[Zn4,Sp1]*AF_ZoneFrac[Zn4]
      # Rt_TAmountHTree[Sp2] = Rt_TAmountH[Zn1,Sp2]*AF_ZoneFrac[Zn1]+Rt_TAmountH[Zn2,Sp2]*AF_ZoneFrac[Zn2]+Rt_TAmountH[Zn3,Sp2]*AF_ZoneFrac[Zn3]+Rt_TAmountH[Zn4,Sp2]*AF_ZoneFrac[Zn4]
      # Rt_TAmountHTree[Sp3] = Rt_TAmountH[Zn1,Sp3]*AF_ZoneFrac[Zn1]+Rt_TAmountH[Zn2,Sp3]*AF_ZoneFrac[Zn2]+Rt_TAmountH[Zn3,Sp3]*AF_ZoneFrac[Zn3]+Rt_TAmountH[Zn4,Sp3]*AF_ZoneFrac[Zn4]
      zonetree_df$Rt_TAmountH_Frac <- zonetree_df$Rt_TAmountH*zonetree_df$AF_ZoneFrac
      tree_df$Rt_TAmountHTree <- aggregate(zonetree_df["Rt_TAmountH_Frac"], zonetree_df["tree_id"], sum)$Rt_TAmountH_Frac

      treenut_df$Rt_ATType <- rep(tree_df$Rt_ATType, nrow(nut_df))
      
      # T_NUptRelH[SlNut,Tree] = IF( Rt_ATType[Tree]=2 AND T_LfTwig[DW,Tree]>0 AND T_Prun[DW, Tree]=0 AND T_PrunLapse[Tree]>T_PrunRecov[Tree] AND T_NUptTot[SlNut,Tree]>0)  THEN (AF_DepthAvg1*T_NUptTree[SlNut,Tree]/Rt_TAmountHTree[Tree]+AF_DepthAvg2*T_NUptTree[SlNut,Tree]/Rt_TAmountHTree[Tree]-AF_DepthAvg3*T_NUptTree[SlNut,Tree]/Rt_TAmountHTree[Tree]-AF_DepthAvg4*T_NUptTree[SlNut,Tree]/Rt_TAmountHTree[Tree])/(((AF_DepthAvg1+AF_DepthAvg2+AF_DepthAvg3+AF_DepthAvg4)*T_NUptTot[SlNut,Tree])/Rt_TField[Tree])ELSE(1)
      treenut_df$T_LfTwig_DW <- rep(treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_LfTwig, nrow(nut_df))
      tree_df$T_Prun_DW <- treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_Prun
      treenut_df$T_Prun_DW <- rep(tree_df$T_Prun_DW, nrow(nut_df))
      treenut_df$T_PrunLapse <- rep(tree_df$T_PrunLapse, nrow(nut_df))
      treenut_df$T_PrunRecov <- rep(tree_df$T_PrunRecov, nrow(nut_df))
      treenut_df$Rt_TAmountHTree <- rep(tree_df$Rt_TAmountHTree, nrow(nut_df))
      
      treenut_df$T_NUptRelH <- ifelse(
        treenut_df$Rt_ATType == 2 &
          treenut_df$T_LfTwig_DW > 0 &
          treenut_df$T_Prun_DW == 0 &
          treenut_df$T_PrunLapse > treenut_df$T_PrunRecov &
          treenut_df$T_NUptTot > 0,
        (
          layer_df[layer_df$layer == 1, ]$AF_DepthAvg * treenut_df$T_NUptTree / treenut_df$Rt_TAmountHTree +
            layer_df[layer_df$layer == 2, ]$AF_DepthAvg * treenut_df$T_NUptTree /
            treenut_df$Rt_TAmountHTree -
            layer_df[layer_df$layer == 3, ]$AF_DepthAvg * treenut_df$T_NUptTree /
            treenut_df$Rt_TAmountHTree -
            layer_df[layer_df$layer == 4, ]$AF_DepthAvg * treenut_df$T_NUptTree /
            treenut_df$Rt_TAmountHTree
        ) / ((
          sum(layer_df$AF_DepthAvg) * treenut_df$T_NUptTot
        ) / treenut_df$Rt_TField),
        1
      )
      
      #TODO: to be check whether TW_UptTree and TW_UptTot are equal
      # TW_T1UptZn[Zone] = W_T1Upt1[Zone]+W_T1Upt2[Zone]+W_T1Upt3[Zone]+W_T1Upt4[Zone]
      # TW_T2UptZn[Zone] = W_T2Upt1[Zone]+W_T2Upt2[Zone]+W_T2Upt3[Zone]+W_T2Upt4[Zone]
      # TW_T3UptZn[Zone] = W_T3Upt1[Zone]+W_T3Upt2[Zone]+W_T3Upt3[Zone]+W_T3Upt4[Zone]
      zonetree_df$TW_UptZn <- zonetree_df$W_Upt_sum
      
      
      # TW_UptTree[Sp1] = TW_T1UptZn[Zn1]*AF_ZoneFrac[Zn1]+TW_T1UptZn[Zn2]*AF_ZoneFrac[Zn2]+TW_T1UptZn[Zn3]*AF_ZoneFrac[Zn3]+TW_T1UptZn[Zn3]*AF_ZoneFrac[Zn3]+0*(TW_T2UptZn[Zn1]+TW_T2UptZn[Zn2]+TW_T2UptZn[Zn3]+TW_T2UptZn[Zn4]+TW_T3UptZn[Zn1]+TW_T3UptZn[Zn2]+TW_T3UptZn[Zn3]+TW_T3UptZn[Zn4])
      # TW_UptTree[Sp2] = TW_T2UptZn[Zn1]*AF_ZoneFrac[Zn1]+TW_T2UptZn[Zn2]*AF_ZoneFrac[Zn2]+TW_T2UptZn[Zn3]*AF_ZoneFrac[Zn3]+TW_T2UptZn[Zn3]*AF_ZoneFrac[Zn3]+0*(TW_T1UptZn[Zn1]+TW_T1UptZn[Zn2]+TW_T1UptZn[Zn3]+TW_T1UptZn[Zn4]+TW_T3UptZn[Zn1]+TW_T3UptZn[Zn2]+TW_T3UptZn[Zn3]+TW_T3UptZn[Zn4])
      # TW_UptTree[Sp3] = TW_T3UptZn[Zn1]*AF_ZoneFrac[Zn1]+TW_T3UptZn[Zn2]*AF_ZoneFrac[Zn2]+TW_T3UptZn[Zn3]*AF_ZoneFrac[Zn3]+TW_T3UptZn[Zn3]*AF_ZoneFrac[Zn3]+0*(TW_T2UptZn[Zn1]+TW_T2UptZn[Zn2]+TW_T2UptZn[Zn3]+TW_T2UptZn[Zn4]+TW_T1UptZn[Zn1]+TW_T1UptZn[Zn2]+TW_T1UptZn[Zn3]+TW_T1UptZn[Zn4])
      tree_df$TW_UptTree <- tree_df$TW_UptTot
      
      # TW_UptRelH[Tree] = IF( Rt_ATType[Tree]=2 AND T_LfTwig[DW,Tree]>0 AND T_Prun[DW, Tree]=0 AND T_PrunLapse[Tree]>T_PrunRecov[Tree] AND TW_UptTot[Tree]>0)  THEN (AF_DepthAvg1*TW_UptTree[Tree]/Rt_TAmountHTree[Tree]+AF_DepthAvg2*TW_UptTree[Tree]/Rt_TAmountHTree[Tree]-AF_DepthAvg3*TW_UptTree[Tree]/Rt_TAmountHTree[Tree]-AF_DepthAvg4*TW_UptTree[Tree]/Rt_TAmountHTree[Tree])/(((AF_DepthAvg1+AF_DepthAvg2+AF_DepthAvg3+AF_DepthAvg4)*TW_UptTot[Tree])/Rt_TField[Tree])ELSE(1)
      tree_df$TW_UptRelH <- ifelse(
        tree_df$Rt_ATType == 2 &
          tree_df$T_LfTwig_DW > 0 &
          tree_df$T_Prun_DW == 0 &
          tree_df$T_PrunLapse > tree_df$T_PrunRecov &
          tree_df$TW_UptTot > 0,
        (
          layer_df[layer_df$layer == 1, ]$AF_DepthAvg * tree_df$TW_UptTree / tree_df$Rt_TAmountHTree +
            layer_df[layer_df$layer == 2, ]$AF_DepthAvg * tree_df$TW_UptTree /
            tree_df$Rt_TAmountHTree -
            layer_df[layer_df$layer == 3, ]$AF_DepthAvg * tree_df$TW_UptTree /
            tree_df$Rt_TAmountHTree -
            layer_df[layer_df$layer == 4, ]$AF_DepthAvg * tree_df$TW_UptTree /
            tree_df$Rt_TAmountHTree
        ) /
          ((
            sum(layer_df$AF_DepthAvg) * tree_df$TW_UptTot
          ) / tree_df$Rt_TField),
        1
      )
      
      # Rt_TUptRelH[Tree] = IF (T_NPosgro[N, Tree]< 1 OR T_NPosgro[P, Tree] <1 OR TW_Posgro[Tree] <1) THEN IF (min(T_NPosgro[N, Tree],T_NPosgro[P, Tree],TW_Posgro[Tree])=T_NPosgro[N, Tree]) THEN T_NUptRelH[N, Tree] ELSE if (min(T_NPosgro[N, Tree],T_NPosgro[P, Tree],TW_Posgro[Tree])=T_NPosgro[P, Tree]) then T_NUptRelH[P, Tree] ELSE  TW_UptRelH[Tree] ELSE 1
      
      tree_df$Rt_TUptRelH <- ifelse(
        tree_df$T_NPosgro_N < 1 |
          tree_df$T_NPosgro_P < 1 | tree_df$TW_Posgro < 1,
        ifelse(
          pmin(tree_df$T_NPosgro_N, tree_df$T_NPosgro_P, tree_df$TW_Posgro) == tree_df$T_NPosgro_N,
          treenut_df[treenut_df$SlNut == "N", ]$T_NUptRelH,
          ifelse(
            pmin(tree_df$T_NPosgro_N, tree_df$T_NPosgro_P, tree_df$TW_Posgro) == tree_df$T_NPosgro_P,
            treenut_df[treenut_df$SlNut == "P", ]$T_NUptRelH,
            tree_df$TW_UptRelH
          )
        ),
        1
      )
      
      # Rt_TDistShapeD[Tree] = IF(Rt_ATType[Tree]=2 AND T_Prun[DW, Tree]=0 AND T_LfTwig[DW,Tree]>0 AND T_PrunLapse[Tree]>T_PrunRecov[Tree])THEN (MIN(1+Rt_TRelChangeH[Tree], MAX((1+Rt_TRelChangeH[Tree])^-1, Rt_TUptRelH[Tree]^-Rt_TDistResp[Tree])))ELSE(1)
      tree_df$Rt_TDistShapeD <- ifelse(
        tree_df$Rt_ATType == 2 &
          tree_df$T_Prun_DW == 0 &
          tree_df$T_LfTwig_DW > 0 &
          tree_df$T_PrunLapse > tree_df$T_PrunRecov,
        pmin(
          1 + tree_df$Rt_TRelChangeH,
          pmax((1 + tree_df$Rt_TRelChangeH)^-1,
               tree_df$Rt_TUptRelH^-tree_df$Rt_TDistResp
          )
        ),
        1
      )
      
      # Rt_TDistShapeInc[Tree] = if T_DiesToday?[Tree] = 1 then - Rt_TDistShapeAct[Tree] else IF(Rt_ATType[Tree]=2)THEN(Rt_TDistShapeAct[Tree]*(Rt_TDistShapeD[Tree]-1))ELSE(0)
      tree_df$Rt_TDistShapeInc <- ifelse(
        tree_df$T_DiesToday_is == 1,
        -tree_df$Rt_TDistShapeAct,
        ifelse(
          tree_df$Rt_ATType == 2,
          tree_df$Rt_TDistShapeAct * (tree_df$Rt_TDistShapeD - 1),
          0
        )
      )
      
      # Rt_TDistShapeActInitPlantTime[Tree] = if time = T_PlantTime[Tree] then Rt_TDistShapeC[Tree] else 0
      tree_df$Rt_TDistShapeActInitPlantTime <- ifelse(time == tree_df$T_PlantTime, tree_df$Rt_TDistShapeC, 0)
      
      # Rt_TDistShapeAct[Tree](t) = Rt_TDistShapeAct[Tree](t - dt) + (Rt_TDistShapeInc[Tree] + Rt_TDistShapeActInitPlantTime[Tree]) * dt
      tree_df$Rt_TDistShapeAct <- tree_df$Rt_TDistShapeAct + (tree_df$Rt_TDistShapeInc + tree_df$Rt_TDistShapeActInitPlantTime)
      
      # Rt_TAmountV1[Tree] = AF_ZoneFrac[Zn1]*AF_DepthAct1[Zn1]*Rt_TLrv1[Zn1, Tree]+AF_ZoneFrac[Zn2]*AF_DepthAct1[Zn2]*Rt_TLrv1[Zn2, Tree]+AF_ZoneFrac[Zn3]*AF_DepthAct1[Zn3]*Rt_TLrv1[Zn3, Tree]+AF_ZoneFrac[Zn4]*AF_DepthAct1[Zn4]*Rt_TLrv1[Zn4, Tree]
      # Rt_TAmountV2[Tree] = AF_ZoneFrac[Zn1]*AF_Depth2[Zn1]*Rt_TLrv2[Zn1, Tree]+AF_ZoneFrac[Zn2]*AF_Depth2[Zn2]*Rt_TLrv2[Zn2, Tree]+AF_ZoneFrac[Zn3]*AF_Depth2[Zn3]*Rt_TLrv2[Zn3, Tree]+AF_ZoneFrac[Zn4]*AF_Depth2[Zn4]*Rt_TLrv2[Zn4, Tree]
      # Rt_TAmountV3[Tree] = AF_ZoneFrac[Zn1]*AF_Depth3[Zn1]*Rt_TLrv3[Zn1, Tree]+AF_ZoneFrac[Zn2]*AF_Depth3[Zn2]*Rt_TLrv3[Zn2, Tree]+AF_ZoneFrac[Zn3]*AF_Depth3[Zn3]*Rt_TLrv3[Zn3, Tree]+AF_ZoneFrac[Zn4]*AF_Depth3[Zn4]*Rt_TLrv3[Zn4, Tree]
      # Rt_TAmountV4[Tree] = AF_ZoneFrac[Zn1]*AF_Depth4[Zn1]*Rt_TLrv4[Zn1, Tree]+AF_ZoneFrac[Zn2]*AF_Depth4[Zn2]*Rt_TLrv4[Zn2, Tree]+AF_ZoneFrac[Zn3]*AF_Depth4[Zn3]*Rt_TLrv4[Zn3, Tree]+AF_ZoneFrac[Zn4]*AF_Depth4[Zn4]*Rt_TLrv4[Zn4, Tree]
      zonelayertree_df$Rt_TLrv_Depth_Frac <- zonelayertree_df$AF_ZoneFrac * zonelayertree_df$AF_Depth * zonelayertree_df$Rt_TLrv
      layertree_df$Rt_TAmountV <- aggregate(zonelayertree_df["Rt_TLrv_Depth_Frac"], zonelayertree_df[c("layer", "tree_id")], sum)$Rt_TLrv_Depth_Frac
      
      # T_NUpt1[N,Sp1] = N_T1Upt1[Zn1,N]*AF_ZoneFrac[Zn1]+N_T1Upt1[Zn2,N]*AF_ZoneFrac[Zn2]+N_T1Upt1[Zn3,N]*AF_ZoneFrac[Zn3]+N_T1Upt1[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T2Upt1[Zn1,N]+N_T2Upt1[Zn2,N]+N_T2Upt1[Zn3,N]+N_T2Upt1[Zn4,N]+N_T3Upt1[Zn1,N]+N_T3Upt1[Zn2,N]+N_T3Upt1[Zn3,N]+N_T3Upt1[Zn4,N])
      # T_NUpt1[N,Sp2] = N_T2Upt1[Zn1,N]*AF_ZoneFrac[Zn1]+N_T2Upt1[Zn2,N]*AF_ZoneFrac[Zn2]+N_T2Upt1[Zn3,N]*AF_ZoneFrac[Zn3]+N_T2Upt1[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T1Upt1[Zn1,N]+N_T1Upt1[Zn2,N]+N_T1Upt1[Zn3,N]+N_T1Upt1[Zn4,N]+N_T3Upt1[Zn1,N]+N_T3Upt1[Zn2,N]+N_T3Upt1[Zn3,N]+N_T3Upt1[Zn4,N])
      # T_NUpt1[N,Sp3] = N_T3Upt1[Zn1,N]*AF_ZoneFrac[Zn1]+N_T3Upt1[Zn2,N]*AF_ZoneFrac[Zn2]+N_T3Upt1[Zn3,N]*AF_ZoneFrac[Zn3]+N_T3Upt1[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T2Upt1[Zn1,N]+N_T2Upt1[Zn2,N]+N_T2Upt1[Zn3,N]+N_T2Upt1[Zn4,N]+N_T1Upt1[Zn1,N]+N_T1Upt1[Zn2,N]+N_T1Upt1[Zn3,N]+N_T1Upt1[Zn4,N])
      # T_NUpt1[P,Sp1] = N_T1Upt1[Zn1,P]*AF_ZoneFrac[Zn1]+N_T1Upt1[Zn2,P]*AF_ZoneFrac[Zn2]+N_T1Upt1[Zn3,P]*AF_ZoneFrac[Zn3]+N_T1Upt1[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T2Upt1[Zn1,P]+N_T2Upt1[Zn2,P]+N_T2Upt1[Zn3,P]+N_T2Upt1[Zn4,P]+N_T3Upt1[Zn1,P]+N_T3Upt1[Zn2,P]+N_T3Upt1[Zn3,P]+N_T3Upt1[Zn4,P])
      # T_NUpt1[P,Sp2] = N_T2Upt1[Zn1,P]*AF_ZoneFrac[Zn1]+N_T2Upt1[Zn2,P]*AF_ZoneFrac[Zn2]+N_T2Upt1[Zn3,P]*AF_ZoneFrac[Zn3]+N_T2Upt1[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T1Upt1[Zn1,P]+N_T1Upt1[Zn2,P]+N_T1Upt1[Zn3,P]+N_T1Upt1[Zn4,P]+N_T3Upt1[Zn1,P]+N_T3Upt1[Zn2,P]+N_T3Upt1[Zn3,P]+N_T3Upt1[Zn4,P])
      # T_NUpt1[P,Sp3] = N_T3Upt1[Zn1,P]*AF_ZoneFrac[Zn1]+N_T3Upt1[Zn2,P]*AF_ZoneFrac[Zn2]+N_T3Upt1[Zn3,P]*AF_ZoneFrac[Zn3]+N_T3Upt1[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T2Upt1[Zn1,P]+N_T2Upt1[Zn2,P]+N_T2Upt1[Zn3,P]+N_T2Upt1[Zn4,P]+N_T1Upt1[Zn1,P]+N_T1Upt1[Zn2,P]+N_T1Upt1[Zn3,P]+N_T1Upt1[Zn4,P])
      # T_NUpt2[N,Sp1] = N_T1Upt2[Zn1,N]*AF_ZoneFrac[Zn1]+N_T1Upt2[Zn2,N]*AF_ZoneFrac[Zn2]+N_T1Upt2[Zn3,N]*AF_ZoneFrac[Zn3]+N_T1Upt2[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T2Upt2[Zn1,N]+N_T2Upt2[Zn2,N]+N_T2Upt2[Zn3,N]+N_T2Upt2[Zn4,N]+N_T3Upt2[Zn1,N]+N_T3Upt2[Zn2,N]+N_T3Upt2[Zn3,N]+N_T3Upt2[Zn4,N])
      # T_NUpt2[N,Sp2] = N_T2Upt2[Zn1,N]*AF_ZoneFrac[Zn1]+N_T2Upt2[Zn2,N]*AF_ZoneFrac[Zn2]+N_T2Upt2[Zn3,N]*AF_ZoneFrac[Zn3]+N_T2Upt2[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T1Upt2[Zn1,N]+N_T1Upt2[Zn2,N]+N_T1Upt2[Zn3,N]+N_T1Upt2[Zn4,N]+N_T3Upt2[Zn1,N]+N_T3Upt2[Zn2,N]+N_T3Upt2[Zn3,N]+N_T3Upt2[Zn4,N])
      # T_NUpt2[N,Sp3] = N_T3Upt2[Zn1,N]*AF_ZoneFrac[Zn1]+N_T3Upt2[Zn2,N]*AF_ZoneFrac[Zn2]+N_T3Upt2[Zn3,N]*AF_ZoneFrac[Zn3]+N_T3Upt2[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T2Upt2[Zn1,N]+N_T2Upt2[Zn2,N]+N_T2Upt2[Zn3,N]+N_T2Upt2[Zn4,N]+N_T1Upt2[Zn1,N]+N_T1Upt2[Zn2,N]+N_T1Upt2[Zn3,N]+N_T1Upt2[Zn4,N])
      # T_NUpt2[P,Sp1] = N_T1Upt2[Zn1,P]*AF_ZoneFrac[Zn1]+N_T1Upt2[Zn2,P]*AF_ZoneFrac[Zn2]+N_T1Upt2[Zn3,P]*AF_ZoneFrac[Zn3]+N_T1Upt2[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T2Upt2[Zn1,P]+N_T2Upt2[Zn2,P]+N_T2Upt2[Zn3,P]+N_T2Upt2[Zn4,P]+N_T3Upt2[Zn1,P]+N_T3Upt2[Zn2,P]+N_T3Upt2[Zn3,P]+N_T3Upt2[Zn4,P])
      # T_NUpt2[P,Sp2] = N_T2Upt2[Zn1,P]*AF_ZoneFrac[Zn1]+N_T2Upt2[Zn2,P]*AF_ZoneFrac[Zn2]+N_T2Upt2[Zn3,P]*AF_ZoneFrac[Zn3]+N_T2Upt2[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T1Upt2[Zn1,P]+N_T1Upt2[Zn2,P]+N_T1Upt2[Zn3,P]+N_T1Upt2[Zn4,P]+N_T3Upt2[Zn1,P]+N_T3Upt2[Zn2,P]+N_T3Upt2[Zn3,P]+N_T3Upt2[Zn4,P])
      # T_NUpt2[P,Sp3] = N_T3Upt2[Zn1,P]*AF_ZoneFrac[Zn1]+N_T3Upt2[Zn2,P]*AF_ZoneFrac[Zn2]+N_T3Upt2[Zn3,P]*AF_ZoneFrac[Zn3]+N_T3Upt2[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T2Upt2[Zn1,P]+N_T2Upt2[Zn2,P]+N_T2Upt2[Zn3,P]+N_T2Upt2[Zn4,P]+N_T1Upt2[Zn1,P]+N_T1Upt2[Zn2,P]+N_T1Upt2[Zn3,P]+N_T1Upt2[Zn4,P])
      # T_NUpt3[N,Sp1] = N_T1Upt3[Zn1,N]*AF_ZoneFrac[Zn1]+N_T1Upt3[Zn2,N]*AF_ZoneFrac[Zn2]+N_T1Upt3[Zn3,N]*AF_ZoneFrac[Zn3]+N_T1Upt3[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T2Upt3[Zn1,N]+N_T2Upt3[Zn2,N]+N_T2Upt3[Zn3,N]+N_T2Upt3[Zn4,N]+N_T3Upt3[Zn1,N]+N_T3Upt3[Zn2,N]+N_T3Upt3[Zn3,N]+N_T3Upt3[Zn4,N])
      # T_NUpt3[N,Sp2] = N_T2Upt3[Zn1,N]*AF_ZoneFrac[Zn1]+N_T2Upt3[Zn2,N]*AF_ZoneFrac[Zn2]+N_T2Upt3[Zn3,N]*AF_ZoneFrac[Zn3]+N_T2Upt3[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T1Upt3[Zn1,N]+N_T1Upt3[Zn2,N]+N_T1Upt3[Zn3,N]+N_T1Upt3[Zn4,N]+N_T3Upt3[Zn1,N]+N_T3Upt3[Zn2,N]+N_T3Upt3[Zn3,N]+N_T3Upt3[Zn4,N])
      # T_NUpt3[N,Sp3] = N_T3Upt3[Zn1,N]*AF_ZoneFrac[Zn1]+N_T3Upt3[Zn2,N]*AF_ZoneFrac[Zn2]+N_T3Upt3[Zn3,N]*AF_ZoneFrac[Zn3]+N_T3Upt3[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T1Upt3[Zn1,N]+N_T1Upt3[Zn2,N]+N_T1Upt3[Zn3,N]+N_T1Upt3[Zn4,N]+N_T2Upt3[Zn1,N]+N_T2Upt3[Zn2,N]+N_T2Upt3[Zn3,N]+N_T2Upt3[Zn4,N])
      # T_NUpt3[P,Sp1] = N_T1Upt3[Zn1,P]*AF_ZoneFrac[Zn1]+N_T1Upt3[Zn2,P]*AF_ZoneFrac[Zn2]+N_T1Upt3[Zn3,P]*AF_ZoneFrac[Zn3]+N_T1Upt3[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T2Upt3[Zn1,P]+N_T2Upt3[Zn2,P]+N_T2Upt3[Zn3,P]+N_T2Upt3[Zn4,P]+N_T3Upt3[Zn1,P]+N_T3Upt3[Zn2,P]+N_T3Upt3[Zn3,P]+N_T3Upt3[Zn4,P])
      # T_NUpt3[P,Sp2] = N_T2Upt3[Zn1,P]*AF_ZoneFrac[Zn1]+N_T2Upt3[Zn2,P]*AF_ZoneFrac[Zn2]+N_T2Upt3[Zn3,P]*AF_ZoneFrac[Zn3]+N_T2Upt3[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T1Upt3[Zn1,P]+N_T1Upt3[Zn2,P]+N_T1Upt3[Zn3,P]+N_T1Upt3[Zn4,P]+N_T3Upt3[Zn1,P]+N_T3Upt3[Zn2,P]+N_T3Upt3[Zn3,P]+N_T3Upt3[Zn4,P])
      # T_NUpt3[P,Sp3] = N_T3Upt3[Zn1,P]*AF_ZoneFrac[Zn1]+N_T3Upt3[Zn2,P]*AF_ZoneFrac[Zn2]+N_T3Upt3[Zn3,P]*AF_ZoneFrac[Zn3]+N_T3Upt3[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T2Upt3[Zn1,P]+N_T2Upt3[Zn2,P]+N_T2Upt3[Zn3,P]+N_T2Upt3[Zn4,P]+N_T1Upt3[Zn1,P]+N_T1Upt3[Zn2,P]+N_T1Upt3[Zn3,P]+N_T1Upt3[Zn4,P])
      # T_NUpt4[N,Sp1] = N_T1Upt4[Zn1,N]*AF_ZoneFrac[Zn1]+N_T1Upt4[Zn2,N]*AF_ZoneFrac[Zn2]+N_T1Upt4[Zn3,N]*AF_ZoneFrac[Zn3]+N_T1Upt4[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T2Upt4[Zn1,N]+N_T2Upt4[Zn2,N]+N_T2Upt4[Zn3,N]+N_T2Upt4[Zn4,N]+N_T3Upt4[Zn1,N]+N_T3Upt4[Zn2,N]+N_T3Upt4[Zn3,N]+N_T3Upt4[Zn4,N])
      # T_NUpt4[N,Sp2] = N_T2Upt4[Zn1,N]*AF_ZoneFrac[Zn1]+N_T2Upt4[Zn2,N]*AF_ZoneFrac[Zn2]+N_T2Upt4[Zn3,N]*AF_ZoneFrac[Zn3]+N_T2Upt4[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T1Upt4[Zn1,N]+N_T1Upt4[Zn2,N]+N_T1Upt4[Zn3,N]+N_T1Upt4[Zn4,N]+N_T3Upt4[Zn1,N]+N_T3Upt4[Zn2,N]+N_T3Upt4[Zn3,N]+N_T3Upt4[Zn4,N])
      # T_NUpt4[N,Sp3] = N_T3Upt4[Zn1,N]*AF_ZoneFrac[Zn1]+N_T3Upt4[Zn2,N]*AF_ZoneFrac[Zn2]+N_T3Upt4[Zn3,N]*AF_ZoneFrac[Zn3]+N_T3Upt4[Zn4,N]*AF_ZoneFrac[Zn4]+0*(N_T2Upt4[Zn1,N]+N_T2Upt4[Zn2,N]+N_T2Upt4[Zn3,N]+N_T2Upt4[Zn4,N]+N_T1Upt4[Zn1,N]+N_T1Upt4[Zn2,N]+N_T1Upt4[Zn3,N]+N_T1Upt4[Zn4,N])
      # T_NUpt4[P,Sp1] = N_T1Upt4[Zn1,P]*AF_ZoneFrac[Zn1]+N_T1Upt4[Zn2,P]*AF_ZoneFrac[Zn2]+N_T1Upt4[Zn3,P]*AF_ZoneFrac[Zn3]+N_T1Upt4[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T2Upt4[Zn1,P]+N_T2Upt4[Zn2,P]+N_T2Upt4[Zn3,P]+N_T2Upt4[Zn4,P]+N_T3Upt4[Zn1,P]+N_T3Upt4[Zn2,P]+N_T3Upt4[Zn3,P]+N_T3Upt4[Zn4,P])
      # T_NUpt4[P,Sp2] = N_T2Upt4[Zn1,P]*AF_ZoneFrac[Zn1]+N_T2Upt4[Zn2,P]*AF_ZoneFrac[Zn2]+N_T2Upt4[Zn3,P]*AF_ZoneFrac[Zn3]+N_T2Upt4[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T1Upt4[Zn1,P]+N_T1Upt4[Zn2,P]+N_T1Upt4[Zn3,P]+N_T1Upt4[Zn4,P]+N_T3Upt4[Zn1,P]+N_T3Upt4[Zn2,P]+N_T3Upt4[Zn3,P]+N_T3Upt4[Zn4,P])
      # T_NUpt4[P,Sp3] = N_T3Upt4[Zn1,P]*AF_ZoneFrac[Zn1]+N_T3Upt4[Zn2,P]*AF_ZoneFrac[Zn2]+N_T3Upt4[Zn3,P]*AF_ZoneFrac[Zn3]+N_T3Upt4[Zn4,P]*AF_ZoneFrac[Zn4]+0*(N_T2Upt4[Zn1,P]+N_T2Upt4[Zn2,P]+N_T2Upt4[Zn3,P]+N_T2Upt4[Zn4,P]+N_T1Upt4[Zn1,P]+N_T1Upt4[Zn2,P]+N_T1Upt4[Zn3,P]+N_T1Upt4[Zn4,P])
      zonelayertreenut_df$N_Upt_Frac <- zonelayertreenut_df$N_Upt * zonelayertreenut_df$AF_ZoneFrac
      layertreenut_df$T_NUpt <- aggregate(zonelayertreenut_df["N_Upt_Frac"], zonelayertreenut_df[c("layer", "tree_id", "SlNut")], sum)$N_Upt_Frac 
      
      # T_NUptRelV1[SlNut,Tree] = IF( Rt_TAmountV1[Tree]>0.0001)THEN(100*AF_DepthAvg1*(T_NUpt1[SlNut,Tree])/Rt_TAmountV1[Tree])ELSE(0)
      # T_NUptRelV2[SlNut,Tree] = IF( Rt_TAmountV2[Tree]>0.0001)THEN(100*AF_DepthAvg2*(T_NUpt2[SlNut,Tree])/Rt_TAmountV2[Tree])ELSE(0)
      # T_NUptRelV3[SlNut,Tree] = IF( Rt_TAmountV3[Tree]>0.0001)THEN(100*AF_DepthAvg3*(T_NUpt3[SlNut,Tree])/Rt_TAmountV3[Tree])ELSE(0)
      # T_NUptRelV4[SlNut,Tree] = IF( Rt_TAmountV4[Tree]>0.0001)THEN(100*AF_DepthAvg4*(T_NUpt4[SlNut,Tree])/Rt_TAmountV4[Tree])ELSE(0)
      layertreenut_df$Rt_TAmountV <- rep(layertree_df$Rt_TAmountV, nrow(nut_df))
      layertreenut_df$AF_DepthAvg <- rep(layer_df$AF_DepthAvg, ntree*nrow(nut_df))
      layertreenut_df$T_NUptRelV <- ifelse( layertreenut_df$Rt_TAmountV>0.0001, 100*layertreenut_df$AF_DepthAvg*(layertreenut_df$T_NUpt)/layertreenut_df$Rt_TAmountV, 0)
      
      # T_NUptRelV[SlNut,Tree] = IF( Rt_ATType[Tree]=2 AND T_LfTwig[DW,Tree]>0 AND T_Prun[DW, Tree]=0 AND T_PrunLapse[Tree]>T_PrunRecov[Tree] AND T_NUptTot[SlNut,Tree] >0)  THEN ((T_NUptRelV1[SlNut,Tree]+T_NUptRelV2[SlNut,Tree]-T_NUptRelV3[SlNut,Tree]-T_NUptRelV4[SlNut,Tree])/((AF_DepthAvg1+AF_DepthAvg2+AF_DepthAvg3+AF_DepthAvg4)*T_NUptTot[SlNut,Tree])/Rt_TField[Tree])ELSE(1)
      treenut_df$AF_DepthAvg_sum <- aggregate(layertreenut_df["AF_DepthAvg"], layertreenut_df[c("tree_id", "SlNut")], sum)$AF_DepthAvg
      treenut_df$T_NUptRelV <- ifelse(
        treenut_df$Rt_ATType == 2 &
          treenut_df$T_LfTwig_DW > 0 &
          treenut_df$T_Prun_DW == 0 &
          treenut_df$T_PrunLapse > treenut_df$T_PrunRecov &
          treenut_df$T_NUptTot > 0,
        (
          layertreenut_df[layertreenut_df$layer == 1, ]$T_NUptRelV +
            layertreenut_df[layertreenut_df$layer == 2, ]$T_NUptRelV -
            layertreenut_df[layertreenut_df$layer == 3, ]$T_NUptRelV -
            layertreenut_df[layertreenut_df$layer == 4, ]$T_NUptRelV
        ) / (treenut_df$AF_DepthAvg_sum * treenut_df$T_NUptTot) / treenut_df$Rt_TField,
        1
      )

      # TW_Upt1[Sp1] = W_T1Upt1[Zn1]*AF_ZoneFrac[Zn1]+W_T1Upt1[Zn2]*AF_ZoneFrac[Zn2]+W_T1Upt1[Zn3]*AF_ZoneFrac[Zn3]+W_T1Upt1[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T2Upt1[Zn1]+W_T2Upt1[Zn2]+W_T2Upt1[Zn3]+W_T2Upt1[Zn4]+W_T3Upt1[Zn1]+W_T3Upt1[Zn2]+W_T3Upt1[Zn3]+W_T3Upt1[Zn4])
      # TW_Upt1[Sp2] = W_T2Upt1[Zn1]*AF_ZoneFrac[Zn1]+W_T2Upt1[Zn2]*AF_ZoneFrac[Zn2]+W_T2Upt1[Zn3]*AF_ZoneFrac[Zn3]+W_T2Upt1[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T1Upt1[Zn1]+W_T1Upt1[Zn2]+W_T1Upt1[Zn3]+W_T1Upt1[Zn4]+W_T3Upt1[Zn1]+W_T3Upt1[Zn2]+W_T3Upt1[Zn3]+W_T3Upt1[Zn4])
      # TW_Upt1[Sp3] = W_T3Upt1[Zn1]*AF_ZoneFrac[Zn1]+W_T3Upt1[Zn2]*AF_ZoneFrac[Zn2]+W_T3Upt1[Zn3]*AF_ZoneFrac[Zn3]+W_T3Upt1[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T2Upt1[Zn1]+W_T2Upt1[Zn2]+W_T2Upt1[Zn3]+W_T2Upt1[Zn4]+W_T1Upt1[Zn1]+W_T1Upt1[Zn2]+W_T1Upt1[Zn3]+W_T1Upt1[Zn4])
      # TW_Upt2[Sp1] = W_T1Upt2[Zn1]*AF_ZoneFrac[Zn1]+W_T1Upt2[Zn2]*AF_ZoneFrac[Zn2]+W_T1Upt2[Zn3]*AF_ZoneFrac[Zn3]+W_T1Upt2[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T2Upt2[Zn1]+W_T2Upt2[Zn2]+W_T2Upt2[Zn3]+W_T2Upt2[Zn4]+W_T3Upt2[Zn1]+W_T3Upt2[Zn2]+W_T3Upt2[Zn3]+W_T3Upt2[Zn4])
      # TW_Upt2[Sp2] = W_T2Upt2[Zn1]*AF_ZoneFrac[Zn1]+W_T2Upt2[Zn2]*AF_ZoneFrac[Zn2]+W_T2Upt2[Zn3]*AF_ZoneFrac[Zn3]+W_T2Upt2[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T1Upt2[Zn1]+W_T1Upt2[Zn2]+W_T1Upt2[Zn3]+W_T1Upt2[Zn4]+W_T3Upt2[Zn1]+W_T3Upt2[Zn2]+W_T3Upt2[Zn3]+W_T3Upt2[Zn4])
      # TW_Upt2[Sp3] = W_T3Upt2[Zn1]*AF_ZoneFrac[Zn1]+W_T3Upt2[Zn2]*AF_ZoneFrac[Zn2]+W_T3Upt2[Zn3]*AF_ZoneFrac[Zn3]+W_T3Upt2[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T2Upt2[Zn1]+W_T2Upt2[Zn2]+W_T2Upt2[Zn3]+W_T2Upt2[Zn4]+W_T1Upt2[Zn1]+W_T1Upt2[Zn2]+W_T1Upt2[Zn3]+W_T1Upt2[Zn4])
      # TW_Upt3[Sp1] = W_T1Upt3[Zn1]*AF_ZoneFrac[Zn1]+W_T1Upt3[Zn2]*AF_ZoneFrac[Zn2]+W_T1Upt3[Zn3]*AF_ZoneFrac[Zn3]+W_T1Upt3[Zn4]*AF_ZoneFrac[Zn4]+ 0*(W_T2Upt3[Zn1]+W_T2Upt3[Zn2]+W_T2Upt3[Zn3]+W_T2Upt3[Zn4]+W_T3Upt3[Zn1]+W_T3Upt3[Zn2]+W_T3Upt3[Zn3]+W_T3Upt3[Zn4])
      # TW_Upt3[Sp2] = W_T2Upt3[Zn1]*AF_ZoneFrac[Zn1]+W_T2Upt3[Zn2]*AF_ZoneFrac[Zn2]+W_T2Upt3[Zn3]*AF_ZoneFrac[Zn3]+W_T2Upt3[Zn4]*AF_ZoneFrac[Zn4]+ 0*(W_T1Upt3[Zn1]+W_T1Upt3[Zn2]+W_T1Upt3[Zn3]+W_T1Upt3[Zn4]+W_T3Upt3[Zn1]+W_T3Upt3[Zn2]+W_T3Upt3[Zn3]+W_T3Upt3[Zn4])
      # TW_Upt3[Sp3] = W_T3Upt3[Zn1]*AF_ZoneFrac[Zn1]+W_T3Upt3[Zn2]*AF_ZoneFrac[Zn2]+W_T3Upt3[Zn3]*AF_ZoneFrac[Zn3]+W_T3Upt3[Zn4]*AF_ZoneFrac[Zn4]+ 0*(W_T2Upt3[Zn1]+W_T2Upt3[Zn2]+W_T2Upt3[Zn3]+W_T2Upt3[Zn4]+W_T1Upt3[Zn1]+W_T1Upt3[Zn2]+W_T1Upt3[Zn3]+W_T1Upt3[Zn4])
      # TW_Upt4[Sp1] = W_T1Upt4[Zn1]*AF_ZoneFrac[Zn1]+W_T1Upt4[Zn2]*AF_ZoneFrac[Zn2]+W_T1Upt4[Zn3]*AF_ZoneFrac[Zn3]+W_T1Upt4[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T2Upt4[Zn1]+W_T2Upt4[Zn2]+W_T2Upt4[Zn3]+W_T2Upt4[Zn4]+W_T3Upt4[Zn1]+W_T3Upt4[Zn2]+W_T3Upt4[Zn3]+W_T3Upt4[Zn4])
      # TW_Upt4[Sp2] = W_T2Upt4[Zn1]*AF_ZoneFrac[Zn1]+W_T2Upt4[Zn2]*AF_ZoneFrac[Zn2]+W_T2Upt4[Zn3]*AF_ZoneFrac[Zn3]+W_T2Upt4[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T1Upt4[Zn1]+W_T1Upt4[Zn2]+W_T1Upt4[Zn3]+W_T1Upt4[Zn4]+W_T3Upt4[Zn1]+W_T3Upt4[Zn2]+W_T3Upt4[Zn3]+W_T3Upt4[Zn4])
      # TW_Upt4[Sp3] = W_T3Upt4[Zn1]*AF_ZoneFrac[Zn1]+W_T3Upt4[Zn2]*AF_ZoneFrac[Zn2]+W_T3Upt4[Zn3]*AF_ZoneFrac[Zn3]+W_T3Upt4[Zn4]*AF_ZoneFrac[Zn4]+0*(W_T2Upt4[Zn1]+W_T2Upt4[Zn2]+W_T2Upt4[Zn3]+W_T2Upt4[Zn4]+W_T1Upt4[Zn1]+W_T1Upt4[Zn2]+W_T1Upt4[Zn3]+W_T1Upt4[Zn4])
      zonelayertree_df$W_Upt_Frac <- zonelayertree_df$W_Upt* zonelayertree_df$AF_ZoneFrac
      layertree_df$TW_Upt <- aggregate(zonelayertree_df["W_Upt_Frac"], zonelayertree_df[c("layer", "tree_id")], sum)$W_Upt_Frac 
      
      # TW_UptRelV1[Tree] = IF( Rt_TAmountV1[Tree]>0.0001)THEN(100*AF_DepthAvg1*(TW_Upt1[Tree])/Rt_TAmountV1[Tree])ELSE(0)
      # TW_UptRelV2[Tree] = IF( Rt_TAmountV2[Tree]>0.0001)THEN(100*AF_DepthAvg2*(TW_Upt2[Tree])/Rt_TAmountV2[Tree])ELSE(0)
      # TW_UptRelV3[Tree] = IF( Rt_TAmountV3[Tree]>0.0001)THEN(100*AF_DepthAvg3*TW_Upt3[Tree]/Rt_TAmountV3[Tree])ELSE(0)
      # TW_UptRelV4[Tree] = IF( Rt_TAmountV4[Tree]>0.0001)THEN(100*AF_DepthAvg4*(TW_Upt4[Tree])/Rt_TAmountV4[Tree])ELSE(0)
      layertree_df$AF_DepthAvg <- rep(layer_df$AF_DepthAvg, ntree)
      layertree_df$TW_UptRelV <- ifelse( layertree_df$Rt_TAmountV>0.0001,100* layertree_df$AF_DepthAvg*(layertree_df$TW_Upt)/layertree_df$Rt_TAmountV, 0)
      
      # TW_UptRelV[Tree] = IF( Rt_ATType[Tree]=2 AND T_LfTwig[DW,Tree]>0 AND T_Prun[DW, Tree]=0 AND T_PrunLapse[Tree]>T_PrunRecov[Tree] AND TW_UptTot[Tree]>0)  THEN ((TW_UptRelV1[Tree]+TW_UptRelV2[Tree]-TW_UptRelV3[Tree]-TW_UptRelV4[Tree])/((AF_DepthAvg1+AF_DepthAvg2+AF_DepthAvg3+AF_DepthAvg4)*TW_UptTot[Tree])/Rt_TField[Tree])ELSE(1)
      tree_df$TW_UptRelV <- ifelse(
        tree_df$Rt_ATType == 2 &
          tree_df$T_LfTwig_DW > 0 &
          tree_df$T_Prun_DW == 0 &
          tree_df$T_PrunLapse > tree_df$T_PrunRecov & tree_df$TW_UptTot > 0,
        (
          layertree_df[layertree_df$layer == 1, ]$TW_UptRelV +
            layertree_df[layertree_df$layer == 2, ]$TW_UptRelV -
            layertree_df[layertree_df$layer == 3, ]$TW_UptRelV -
            layertree_df[layertree_df$layer == 4, ]$TW_UptRelV
        ) / (sum(layer_df$AF_DepthAvg) * tree_df$TW_UptTot) / tree_df$Rt_TField,
        1
      )
      
      # Rt_TUptRelV[Tree] = IF (T_NPosgro[N, Tree] < 1 OR T_NPosgro[P, Tree] < 1 OR TW_Posgro[Tree] <1) THEN IF (MIN(T_NPosgro[N, Tree],T_NPosgro[P, Tree],TW_Posgro[Tree]) = T_NPosgro[N, Tree]) then T_NUptRelV[N, Tree] else if (MIN(T_NPosgro[N, Tree],T_NPosgro[P, Tree],TW_Posgro[Tree]) = T_NPosgro[P, Tree]) then T_NUptRelV[P, Tree] else TW_UptRelV[Tree] ELSE 1
      tree_df$Rt_TUptRelV <- ifelse (
        tree_df$T_NPosgro_N < 1 |
          tree_df$T_NPosgro_P < 1 | tree_df$TW_Posgro < 1,
        ifelse (
          pmin(tree_df$T_NPosgro_N, tree_df$T_NPosgro_P, tree_df$TW_Posgro) == tree_df$T_NPosgro_N,
          treenut_df[treenut_df$SlNut == "N", ]$T_NUptRelV,
          ifelse (
            pmin(tree_df$T_NPosgro_N, tree_df$T_NPosgro_P, tree_df$TW_Posgro) == tree_df$T_NPosgro_P,
            treenut_df[treenut_df$SlNut == "P", ]$T_NUptRelV,
            tree_df$TW_UptRelV
          )
        ),
        1
      )

      # Rt_TDecDepthD[Tree] = IF(Rt_ATType[Tree]=2 AND T_Prun[DW, Tree]=0 AND T_LfTwig[DW,Tree]>0 AND T_PrunLapse[Tree]>T_PrunRecov[Tree])THEN (MIN(1+Rt_TRelChangeV[Tree], MAX((1+Rt_TRelChangeV[Tree])^-1, Rt_TUptRelV[Tree]^-Rt_TDistResp[Tree])))ELSE(1)
      tree_df$Rt_TDecDepthD <- ifelse(
        tree_df$Rt_ATType == 2 &
          tree_df$T_Prun_DW == 0 &
          tree_df$T_LfTwig_DW > 0 & tree_df$T_PrunLapse > tree_df$T_PrunRecov,
        pmin(
          1 + tree_df$Rt_TRelChangeV,
          pmax((1 + tree_df$Rt_TRelChangeV)^-1,
               tree_df$Rt_TUptRelV^-tree_df$Rt_TDistResp
          )
        ),
        1
      )
      
      # Rt_TDecDepthInc[Tree] = if T_DiesToday?[Tree]= 1 then - Rt_TDecDepthAct[Tree] else IF(Rt_ATType[Tree]=2)THEN(Rt_TDecDepthAct[Tree]*(Rt_TDecDepthD[Tree]-1))ELSE(0)
      tree_df$Rt_TDecDepthInc <- ifelse(
        tree_df$T_DiesToday_is == 1,
        -tree_df$Rt_TDecDepthAct,
        ifelse(
          tree_df$Rt_ATType == 2,
          tree_df$Rt_TDecDepthAct * (tree_df$Rt_TDecDepthD - 1),
          0
        )
      )
      
      # Rt_TDecDepthActInitPlantTree[Tree] = if time = T_PlantTime[Tree] then Rt_TDecDepthC[Tree] else 0
      tree_df$Rt_TDecDepthActInitPlantTree <- ifelse(time == tree_df$T_PlantTime, tree_df$Rt_TDecDepthC, 0)
      
      # Rt_TDecDepthAct[Tree](t) = Rt_TDecDepthAct[Tree](t - dt) + (Rt_TDecDepthInc[Tree] + Rt_TDecDepthActInitPlantTree[Tree]) * dt
      tree_df$Rt_TDecDepthAct <- tree_df$Rt_TDecDepthAct + (tree_df$Rt_TDecDepthInc + tree_df$Rt_TDecDepthActInitPlantTree)
      
      # T_Sequen[Tree] = if ((T_Stage[Tree,VegGen] = 0) and( time > T_PlantTime[Tree] + 2)) then 1 else if T_Stage[Tree,VegGen] >= 2   then 1 else 0
      tree_df$T_Sequen <- ifelse ((tree_df$T_Stage_VegGen == 0) & ( time > tree_df$T_PlantTime + 2),  1, ifelse(tree_df$T_Stage_VegGen >= 2, 1, 0))
      
      # T_Compl[Tree](t) = T_Compl[Tree](t - dt) + (T_Sequen[Tree]) * dt
      tree_df$T_Compl <- tree_df$T_Compl + (tree_df$T_Sequen) 
      
      # C_RtAllocAct[Zone,SoilLayer] = IF Rt_ACType<2  then 0 else if Cq_Stage[Zone]>0.05THEN(Cq_CRtAllocCurr[Zone] +(0.98 - Cq_CRtAllocCurr[Zone] )*(1 -( min(MIN(C_NPosgro[Zone,N],C_NPosgro[Zone,P]), CW_Posgro[Zone]))^(.0001 + Rt_CRtAllocRespCurr[Zone]))) else Cq_CRtAllocCurr[Zone]
      zonelayer_df$Cq_Stage <- rep(zone_df$Cq_Stage, nlayer)
      zonelayer_df$Cq_CRtAllocCurr <- rep(zone_df$Cq_CRtAllocCurr, nlayer)
      zonelayer_df$C_NPosgro_N <- rep(zone_df$C_NPosgro_N, nlayer)
      zonelayer_df$C_NPosgro_P <- rep(zone_df$C_NPosgro_P, nlayer)
      zonelayer_df$CW_Posgro <- rep(zone_df$CW_Posgro, nlayer)
      zonelayer_df$Rt_CRtAllocRespCurr <- rep(zone_df$Rt_CRtAllocRespCurr, nlayer)
      
      zonelayer_df$C_RtAllocAct <- ifelse(
        zonelayer_df$Rt_ACType < 2,
        0,
        ifelse(
          zonelayer_df$Cq_Stage > 0.05,
          zonelayer_df$Cq_CRtAllocCurr + (0.98 - zonelayer_df$Cq_CRtAllocCurr) *
            (1 - (pmin(
              pmin(zonelayer_df$C_NPosgro_N, zonelayer_df$C_NPosgro_P),
              zonelayer_df$CW_Posgro
            ))^(.0001 + zonelayer_df$Rt_CRtAllocRespCurr)),
          zonelayer_df$Cq_CRtAllocCurr
        )
      )
      
      # N_TNUptDem1[Zone] = N_TNUptPot1[Zone,Sp1]*T_NNDemandZn[Zone,Sp1]+N_TNUptPot1[Zone,Sp2]*T_NNDemandZn[Zone,Sp2]+N_TNUptPot1[Zone,Sp3]*T_NNDemandZn[Zone,Sp3]
      # N_TNUptDem2[Zone] = N_TNUptPot2[Zone,Sp1]*T_NNDemandZn[Zone,Sp1]+N_TNUptPot2[Zone,Sp2]*T_NNDemandZn[Zone,Sp2]+N_TNUptPot2[Zone,Sp3]*T_NNDemandZn[Zone,Sp3]
      # N_TNUptDem3[Zone] = N_TNUptPot3[Zone,Sp1]*T_NNDemandZn[Zone,Sp1]+N_TNUptPot3[Zone,Sp2]*T_NNDemandZn[Zone,Sp2]+N_TNUptPot3[Zone,Sp3]*T_NNDemandZn[Zone,Sp3]
      # N_TNUptDem4[Zone] = N_TNUptPot4[Zone,Sp1]*T_NNDemandZn[Zone,Sp1]+N_TNUptPot4[Zone,Sp2]*T_NNDemandZn[Zone,Sp2]+N_TNUptPot4[Zone,Sp3]*T_NNDemandZn[Zone,Sp3]
      zonelayertree_df$N_TNUptPot_Demand <- zonelayertree_df$N_TNUptPot * zonelayertree_df$T_NNDemandZn
      zonelayer_df$N_TNUptDem <- aggregate(zonelayertree_df["N_TNUptPot_Demand"], zonelayertree_df[c("zone", "layer")], sum)$N_TNUptPot_Demand
      
      # N_RhizCShare1[Zone] = 1-ARRAYSUM(N_RhizTShare1[Zone,*])
      # N_RhizCShare2[Zone] = 1-ARRAYSUM(N_RhizTShare2[Zone,*])
      # N_RhizCShare3[Zone] = 1-ARRAYSUM(N_RhizTShare3[Zone,*])
      # N_RhizCShare4[Zone] = 1-ARRAYSUM(N_RhizTShare4[Zone,*])
      zonelayer_df$N_RhizTShare_sum <- aggregate(zonelayertree_df["N_RhizTShare"], zonelayertree_df[c("zone", "layer")], sum)$N_RhizTShare
      zonelayer_df$N_RhizCShare <- 1-zonelayer_df$N_RhizTShare_sum
      
      # N_CTUptPot1[Zn1,N] = IF(N_CUptPot1[Zn1,N]>0 AND C_NDemand[Zn1,N]>0) THEN N_UptPot1[Zn1,N] * ((N_CUptPot1[Zn1,N] * C_NDemand[Zn1,N] / (N_CUptPot1[Zn1,N] * C_NDemand[Zn1,N] + N_TNUptDem1[Zn1]+0*(N_TPUptDem1[Zn1]+ N_RhizEffect1[Zn1]+N_RhizCShare1[Zn1])))) ELSE (0)
      # N_CTUptPot1[Zn1,P] = IF(N_CUptPot1[Zn1,P]>0 AND C_NDemand[Zn1,P]>0) THEN N_UptPot1[Zn1,P] * ((N_CUptPot1[Zn1,P] * C_NDemand[Zn1,P] / (N_CUptPot1[Zn1,P] * C_NDemand[Zn1,P] + 0*N_TNUptDem1[Zn1]+N_TPUptDem1[Zn1])) + N_RhizEffect1[Zn1] * N_RhizCShare1[Zn1]) / (1 + N_RhizEffect1[Zn1]) ELSE (0)
      # N_CTUptPot1[Zn2,N] = IF(N_CUptPot1[Zn2,N]>0 AND C_NDemand[Zn2,N]>0) THEN N_UptPot1[Zn2,N] * ((N_CUptPot1[Zn2,N] * C_NDemand[Zn2,N] / (N_CUptPot1[Zn2,N] * C_NDemand[Zn2,N] + N_TNUptDem1[Zn2]+0*(N_TPUptDem1[Zn2]+N_RhizEffect1[Zn2]+N_RhizCShare1[Zn2])))) ELSE (0)
      # N_CTUptPot1[Zn2,P] = IF(N_CUptPot1[Zn2,P]>0 AND C_NDemand[Zn2,P]>0) THEN N_UptPot1[Zn2,P] * ((N_CUptPot1[Zn2,P] * C_NDemand[Zn2,P] / (N_CUptPot1[Zn2,P] * C_NDemand[Zn2,P] + 0*N_TNUptDem1[Zn2]+N_TPUptDem1[Zn2])) + N_RhizEffect1[Zn2] * N_RhizCShare1[Zn2]) / (1 + N_RhizEffect1[Zn2]) ELSE (0)
      # N_CTUptPot1[Zn3,N] = IF(N_CUptPot1[Zn3,N]>0 AND C_NDemand[Zn3,N]>0) THEN N_UptPot1[Zn3,N] * ((N_CUptPot1[Zn3,N] * C_NDemand[Zn3,N] / (N_CUptPot1[Zn3,N] * C_NDemand[Zn3,N] + N_TNUptDem1[Zn3]+0*(N_TPUptDem1[Zn3]+N_RhizEffect1[Zn3]+N_RhizCShare1[Zn3])))) ELSE (0)
      # N_CTUptPot1[Zn3,P] = IF(N_CUptPot1[Zn3,P]>0 AND C_NDemand[Zn3,P]>0) THEN N_UptPot1[Zn3,P] * ((N_CUptPot1[Zn3,P] * C_NDemand[Zn3,P] / (N_CUptPot1[Zn3,P] * C_NDemand[Zn3,P] + 0*N_TNUptDem1[Zn3]+N_TPUptDem1[Zn3])) + N_RhizEffect1[Zn3] * N_RhizCShare1[Zn3]) / (1 + N_RhizEffect1[Zn3]) ELSE (0)
      # N_CTUptPot1[Zn4,N] = IF(N_CUptPot1[Zn4,N]>0 AND C_NDemand[Zn4,N]>0) THEN N_UptPot1[Zn4,N] * ((N_CUptPot1[Zn4,N] * C_NDemand[Zn4,N] / (N_CUptPot1[Zn4,N] * C_NDemand[Zn4,N] + N_TNUptDem1[Zn4]+0*(N_TPUptDem1[Zn4]+N_RhizCShare1[Zn4]+N_RhizEffect1[Zn4]))))ELSE (0)
      # N_CTUptPot1[Zn4,P] = IF(N_CUptPot1[Zn4,P]>0 AND C_NDemand[Zn4,P]>0) THEN N_UptPot1[Zn4,P] * ((N_CUptPot1[Zn4,P] * C_NDemand[Zn4,P] / (N_CUptPot1[Zn4,P] * C_NDemand[Zn4,P] + 0*N_TNUptDem1[Zn4]+N_TPUptDem1[Zn4])) + N_RhizEffect1[Zn4] * N_RhizCShare1[Zn4]) / (1 + N_RhizEffect1[Zn4]) ELSE (0)
      # N_CTUptPot2[Zn1,N] = IF(N_CUptPot2[Zn1,N]>0 AND C_NDemand[Zn1,N]>0) THEN N_UptPot2[Zn1,N] * ((N_CUptPot2[Zn1,N] * C_NDemand[Zn1,N] / (N_CUptPot2[Zn1,N] * C_NDemand[Zn1,N] + N_TNUptDem2[Zn1]+0*(N_TPUptDem2[Zn1] + N_RhizEffect2[Zn1] + N_RhizCShare2[Zn1])))) ELSE (0)
      # N_CTUptPot2[Zn1,P] = IF(N_CUptPot2[Zn1,P]>0 AND C_NDemand[Zn1,P]>0) THEN N_UptPot2[Zn1,P] * ((N_CUptPot2[Zn1,P] * C_NDemand[Zn1,P] / (N_CUptPot2[Zn1,P] * C_NDemand[Zn1,P] + 0*N_TNUptDem2[Zn1]+N_TPUptDem2[Zn1])) + N_RhizEffect2[Zn1] * N_RhizCShare2[Zn1]) / (1 + N_RhizEffect2[Zn1]) ELSE (0)
      # N_CTUptPot2[Zn2,N] = IF(N_CUptPot2[Zn2,N]>0 AND C_NDemand[Zn2,N]>0) THEN N_UptPot2[Zn2,N] * ((N_CUptPot2[Zn2,N] * C_NDemand[Zn2,N] / (N_CUptPot2[Zn2,N] * C_NDemand[Zn2,N] + N_TNUptDem2[Zn2]+0*(N_TPUptDem2[Zn2]+N_RhizEffect2[Zn2]+N_RhizCShare2[Zn2])))) ELSE (0)
      # N_CTUptPot2[Zn2,P] = IF(N_CUptPot2[Zn2,P]>0 AND C_NDemand[Zn2,P]>0) THEN N_UptPot2[Zn2,P] * ((N_CUptPot2[Zn2,P] * C_NDemand[Zn2,P] / (N_CUptPot2[Zn2,P] * C_NDemand[Zn2,P] + 0*N_TNUptDem2[Zn2]+N_TPUptDem2[Zn2])) + N_RhizEffect2[Zn2] * N_RhizCShare2[Zn2]) / (1 + N_RhizEffect2[Zn2]) ELSE (0)
      # N_CTUptPot2[Zn3,N] = IF(N_CUptPot2[Zn3,N]>0 AND C_NDemand[Zn3,N]>0) THEN N_UptPot2[Zn3,N] * ((N_CUptPot2[Zn3,N] * C_NDemand[Zn3,N] / (N_CUptPot2[Zn3,N] * C_NDemand[Zn3,N] + N_TNUptDem2[Zn3]+0*(N_TPUptDem2[Zn3] + N_RhizEffect2[Zn3]+ N_RhizCShare2[Zn3])))) ELSE (0)
      # N_CTUptPot2[Zn3,P] = IF(N_CUptPot2[Zn3,P]>0 AND C_NDemand[Zn3,P]>0) THEN N_UptPot2[Zn3,P] * ((N_CUptPot2[Zn3,P] * C_NDemand[Zn3,P] / (N_CUptPot2[Zn3,P] * C_NDemand[Zn3,P] + 0*N_TNUptDem2[Zn3]+N_TPUptDem2[Zn3])) + N_RhizEffect2[Zn3] * N_RhizCShare2[Zn3]) / (1 + N_RhizEffect2[Zn3]) ELSE (0)
      # N_CTUptPot2[Zn4,N] = IF(N_CUptPot2[Zn4,N]>0 AND C_NDemand[Zn4,N]>0) THEN N_UptPot2[Zn4,N] * ((N_CUptPot2[Zn4,N] * C_NDemand[Zn4,N] / (N_CUptPot2[Zn4,N] * C_NDemand[Zn4,N] + N_TNUptDem2[Zn4]+0*(N_TPUptDem2[Zn4] + N_RhizEffect2[Zn4] + N_RhizCShare2[Zn4])))) ELSE (0)
      # N_CTUptPot2[Zn4,P] = IF(N_CUptPot2[Zn4,P]>0 AND C_NDemand[Zn4,P]>0) THEN N_UptPot2[Zn4,P] * ((N_CUptPot2[Zn4,P] * C_NDemand[Zn4,P] / (N_CUptPot2[Zn4,P] * C_NDemand[Zn4,P] + 0*N_TNUptDem2[Zn4]+N_TPUptDem2[Zn4])) + N_RhizEffect2[Zn4] * N_RhizCShare2[Zn4]) / (1 + N_RhizEffect2[Zn4]) ELSE (0)
      # N_CTUptPot3[Zn1,N] = IF(N_CUptPot3[Zn1,N]>0 AND C_NDemand[Zn1,N]>0) THEN N_UptPot3[Zn1,N] * ((N_CUptPot3[Zn1,N] * C_NDemand[Zn1,N] / (N_CUptPot3[Zn1,N] * C_NDemand[Zn1,N] + N_TNUptDem3[Zn1]+0*(N_TPUptDem3[Zn1] + N_RhizEffect3[Zn1]+ N_RhizCShare3[Zn1])))) ELSE (0)
      # N_CTUptPot3[Zn1,P] = IF(N_CUptPot3[Zn1,P]>0 AND C_NDemand[Zn1,P]>0) THEN N_UptPot3[Zn1,P] * ((N_CUptPot3[Zn1,P] * C_NDemand[Zn1,P] / (N_CUptPot3[Zn1,P] * C_NDemand[Zn1,P] + 0*N_TNUptDem3[Zn1]+N_TPUptDem3[Zn1])) + N_RhizEffect3[Zn1] * N_RhizCShare3[Zn1]) / (1 + N_RhizEffect3[Zn1]) ELSE (0)
      # N_CTUptPot3[Zn2,N] = IF(N_CUptPot3[Zn2,N]>0 AND C_NDemand[Zn2,N]>0) THEN N_UptPot3[Zn2,N] * ((N_CUptPot3[Zn2,N] * C_NDemand[Zn2,N] / (N_CUptPot3[Zn2,N] * C_NDemand[Zn2,N] + N_TNUptDem3[Zn2]+0*(N_TPUptDem3[Zn2] + N_RhizEffect3[Zn2] +N_RhizCShare3[Zn2])))) ELSE (0)
      # N_CTUptPot3[Zn2,P] = IF(N_CUptPot3[Zn2,P]>0 AND C_NDemand[Zn2,P]>0) THEN N_UptPot3[Zn2,P] * ((N_CUptPot3[Zn2,P] * C_NDemand[Zn2,P] / (N_CUptPot3[Zn2,P] * C_NDemand[Zn2,P] + 0*N_TNUptDem3[Zn2]+N_TPUptDem3[Zn2])) + N_RhizEffect3[Zn2] * N_RhizCShare3[Zn2]) / (1 + N_RhizEffect3[Zn2]) ELSE (0)
      # N_CTUptPot3[Zn3,N] = IF(N_CUptPot3[Zn3,N]>0 AND C_NDemand[Zn3,N]>0) THEN N_UptPot3[Zn3,N] * ((N_CUptPot3[Zn3,N] * C_NDemand[Zn3,N] / (N_CUptPot3[Zn3,N] * C_NDemand[Zn3,N] + N_TNUptDem3[Zn3]+0*(N_TPUptDem3[Zn3] + N_RhizEffect3[Zn3]+ N_RhizCShare3[Zn3])))) ELSE (0)
      # N_CTUptPot3[Zn3,P] = IF(N_CUptPot3[Zn3,P]>0 AND C_NDemand[Zn3,P]>0) THEN N_UptPot3[Zn3,P] * ((N_CUptPot3[Zn3,P] * C_NDemand[Zn3,P] / (N_CUptPot3[Zn3,P] * C_NDemand[Zn3,P] + 0*N_TNUptDem3[Zn3]+N_TPUptDem3[Zn3])) + N_RhizEffect3[Zn3] * N_RhizCShare3[Zn3]) / (1 + N_RhizEffect3[Zn3]) ELSE (0)
      # N_CTUptPot3[Zn4,N] = IF(N_CUptPot3[Zn4,N]>0 AND C_NDemand[Zn4,N]>0) THEN N_UptPot3[Zn4,N] * ((N_CUptPot3[Zn4,N] * C_NDemand[Zn4,N] / (N_CUptPot3[Zn4,N] * C_NDemand[Zn4,N] + N_TNUptDem3[Zn4]+0*(N_TPUptDem3[Zn4] + N_RhizEffect3[Zn4]+ N_RhizCShare3[Zn4])))) ELSE (0)
      # N_CTUptPot3[Zn4,P] = IF(N_CUptPot3[Zn4,P]>0 AND C_NDemand[Zn4,P]>0) THEN N_UptPot3[Zn4,P] * ((N_CUptPot3[Zn4,P] * C_NDemand[Zn4,P] / (N_CUptPot3[Zn4,P] * C_NDemand[Zn4,P] + 0*N_TNUptDem3[Zn4]+N_TPUptDem3[Zn4])) + N_RhizEffect3[Zn4] * N_RhizCShare3[Zn4]) / (1 + N_RhizEffect3[Zn4]) ELSE (0)
      # N_CTUptPot4[Zn1,N] = IF(N_CUptPot4[Zn1,N]>0 AND C_NDemand[Zn1,N]>0) THEN N_UptPot4[Zn1,N] * ((N_CUptPot4[Zn1,N] * C_NDemand[Zn1,N] / (N_CUptPot4[Zn1,N] * C_NDemand[Zn1,N] + N_TNUptDem4[Zn1]+0*(N_TPUptDem4[Zn1] + N_RhizEffect4[Zn1] + N_RhizCShare4[Zn1])))) ELSE (0)
      # N_CTUptPot4[Zn1,P] = IF(N_CUptPot4[Zn1,P]>0 AND C_NDemand[Zn1,P]>0) THEN N_UptPot4[Zn1,P] * ((N_CUptPot4[Zn1,P] * C_NDemand[Zn1,P] / (N_CUptPot4[Zn1,P] * C_NDemand[Zn1,P] + 0*N_TNUptDem4[Zn1]+N_TPUptDem4[Zn1])) + N_RhizEffect4[Zn1] * N_RhizCShare4[Zn1]) / (1 + N_RhizEffect4[Zn1]) ELSE (0)
      # N_CTUptPot4[Zn2,N] = IF(N_CUptPot4[Zn2,N]>0 AND C_NDemand[Zn2,N]>0) THEN N_UptPot4[Zn2,N] * ((N_CUptPot4[Zn2,N] * C_NDemand[Zn2,N] / (N_CUptPot4[Zn2,N] * C_NDemand[Zn2,N] + N_TNUptDem4[Zn2]+0*(N_TPUptDem4[Zn2] + N_RhizEffect4[Zn2] + N_RhizCShare4[Zn2])))) ELSE (0)
      # N_CTUptPot4[Zn2,P] = IF(N_CUptPot4[Zn2,P]>0 AND C_NDemand[Zn2,P]>0) THEN N_UptPot4[Zn2,P] * ((N_CUptPot4[Zn2,P] * C_NDemand[Zn2,P] / (N_CUptPot4[Zn2,P] * C_NDemand[Zn2,P] + 0*N_TNUptDem4[Zn2]+N_TPUptDem4[Zn2])) + N_RhizEffect4[Zn2] * N_RhizCShare4[Zn2]) / (1 + N_RhizEffect4[Zn2]) ELSE (0)
      # N_CTUptPot4[Zn3,N] = IF(N_CUptPot4[Zn3,N]>0 AND C_NDemand[Zn3,N]>0) THEN N_UptPot4[Zn3,N] * ((N_CUptPot4[Zn3,N] * C_NDemand[Zn3,N] / (N_CUptPot4[Zn3,N] * C_NDemand[Zn3,N] + N_TNUptDem4[Zn3]+0*(N_TPUptDem4[Zn3] + N_RhizEffect4[Zn3]+ N_RhizCShare4[Zn3])))) ELSE (0)
      # N_CTUptPot4[Zn3,P] = IF(N_CUptPot4[Zn3,P]>0 AND C_NDemand[Zn3,P]>0) THEN N_UptPot4[Zn3,P] * ((N_CUptPot4[Zn3,P] * C_NDemand[Zn3,P] / (N_CUptPot4[Zn3,P] * C_NDemand[Zn3,P] + 0*N_TNUptDem4[Zn3]+N_TPUptDem4[Zn3])) + N_RhizEffect4[Zn3] * N_RhizCShare4[Zn3]) / (1 + N_RhizEffect4[Zn3]) ELSE (0)
      # N_CTUptPot4[Zn4,N] = IF(N_CUptPot4[Zn4,N]>0 AND C_NDemand[Zn4,N]>0) THEN N_UptPot4[Zn4,N] * ((N_CUptPot4[Zn4,N] * C_NDemand[Zn4,N] / (N_CUptPot4[Zn4,N] * C_NDemand[Zn4,N] + N_TNUptDem4[Zn4]+0*(N_TPUptDem4[Zn4] + N_RhizEffect4[Zn4] + N_RhizCShare4[Zn4])))) ELSE (0)
      # N_CTUptPot4[Zn4,P] = IF(N_CUptPot4[Zn4,P]>0 AND C_NDemand[Zn4,P]>0) THEN N_UptPot4[Zn4,P] * ((N_CUptPot4[Zn4,P] * C_NDemand[Zn4,P] / (N_CUptPot4[Zn4,P] * C_NDemand[Zn4,P] + 0*N_TNUptDem4[Zn4]+N_TPUptDem4[Zn4])) + N_RhizEffect4[Zn4] * N_RhizCShare4[Zn4]) / (1 + N_RhizEffect4[Zn4]) ELSE (0)
      zonelayernut_df$N_CTUptPot <- NA
      zonelayer_df$C_NDemand_N <- rep(zonenut_df[zonenut_df$SlNut == "N", ]$C_NDemand, nlayer)
      zonelayer_df$C_NDemand_P <- rep(zonenut_df[zonenut_df$SlNut == "P", ]$C_NDemand, nlayer)
      zln_N <- zonelayernut_df[zonelayernut_df$SlNut == "N", ]
      zln_P <- zonelayernut_df[zonelayernut_df$SlNut == "P", ]
      
      zonelayernut_df[zonelayernut_df$SlNut == "N", ]$N_CTUptPot <- ifelse(
        zln_N$N_CUptPot > 0 &
          zonelayer_df$C_NDemand_N > 0,
        zln_N$N_UptPot * (
          zln_N$N_CUptPot * zonelayer_df$C_NDemand_N / (
            zln_N$N_CUptPot * zonelayer_df$C_NDemand_N + zonelayer_df$N_TNUptDem
          )
        ),
        0
      )
      zonelayernut_df[zonelayernut_df$SlNut == "P", ]$N_CTUptPot <- ifelse(
        zln_P$N_CUptPot > 0 &
          zonelayer_df$C_NDemand_P > 0,
        zln_P$N_UptPot * ((
          zln_P$N_CUptPot * zonelayer_df$C_NDemand_P / (
            zln_P$N_CUptPot * zonelayer_df$C_NDemand_P + zonelayer_df$N_TPUptDem
          )
        ) + zonelayer_df$N_RhizEffect * zonelayer_df$N_RhizCShare
        ) / (1 + zonelayer_df$N_RhizEffect),
        0
      )
      
      # N_CUptPotAct1[Zone,SlNut] = MIN(N_CTUptPot1[Zone,SlNut],N_CUptPot1[Zone,SlNut])
      # N_CUptPotAct2[Zone,SlNut] = MIN(N_CTUptPot2[Zone,SlNut],N_CUptPot2[Zone,SlNut])
      # N_CUptPotAct3[Zone,SlNut] = MIN(N_CTUptPot3[Zone,SlNut],N_CUptPot3[Zone,SlNut])
      # N_CUptPotAct4[Zone,SlNut] = MIN(N_CTUptPot4[Zone,SlNut],N_CUptPot4[Zone,SlNut])
      zonelayernut_df$N_CUptPotAct <- pmin(zonelayernut_df$N_CTUptPot, zonelayernut_df$N_CUptPot)

      # C_NUptDeno[Zone,SlNut] = Rt_CLrv1[Zone]*N_CUptPotAct1[Zone,SlNut]+Rt_CLrv2[Zone]*N_CUptPotAct2[Zone,SlNut]+Rt_CLrv3[Zone]*N_CUptPotAct3[Zone,SlNut]+Rt_CLrv4[Zone]*N_CUptPotAct4[Zone,SlNut]
      zonelayernut_df$N_CUptPotAct_CLrv <- zonelayernut_df$Rt_CLrv* zonelayernut_df$N_CUptPotAct
      zonenut_df$C_NUptDeno <- aggregate(zonelayernut_df["N_CUptPotAct_CLrv"], zonelayernut_df[c("zone", "SlNut")], sum)$N_CUptPotAct_CLrv
      
      # C_NUptPot[Zone,SlNut] = N_CUptPotAct1[Zone,SlNut]+N_CUptPotAct2[Zone,SlNut]+N_CUptPotAct3[Zone,SlNut]+N_CUptPotAct4[Zone,SlNut]
      zonenut_df$C_NUptPot <- aggregate(zonelayernut_df["N_CUptPotAct"], zonelayernut_df[c("zone", "SlNut")], sum)$N_CUptPotAct

      # N_CUpt1[Zone,SlNut] = IF(C_NUptDeno[Zone,SlNut]>0)THEN(IF(C_NUptPot[Zone,SlNut]>C_NDemand[Zone,SlNut])THEN(C_NDemand[Zone,SlNut]*N_CUptPotAct1[Zone,SlNut]*Rt_CLrv1[Zone]/C_NUptDeno[Zone,SlNut])ELSE(N_CUptPotAct1[Zone,SlNut]))ELSE(0)
      # N_CUpt2[Zone,SlNut] = IF(C_NUptDeno[Zone,SlNut]>0)THEN(IF(C_NUptPot[Zone,SlNut]>C_NDemand[Zone,SlNut])THEN(C_NDemand[Zone,SlNut]*N_CUptPotAct2[Zone,SlNut]*Rt_CLrv2[Zone]/C_NUptDeno[Zone,SlNut])ELSE(N_CUptPotAct2[Zone,SlNut]))ELSE(0)
      # N_CUpt3[Zone,SlNut] = IF(C_NUptDeno[Zone,SlNut]>0)THEN(IF(C_NUptPot[Zone,SlNut]>C_NDemand[Zone,SlNut])THEN(C_NDemand[Zone,SlNut]*N_CUptPotAct3[Zone,SlNut]*Rt_CLrv3[Zone]/C_NUptDeno[Zone,SlNut])ELSE(N_CUptPotAct3[Zone,SlNut]))ELSE(0)
      # N_CUpt4[Zone,SlNut] = IF(C_NUptDeno[Zone,SlNut]>0)THEN(IF(C_NUptPot[Zone,SlNut]>C_NDemand[Zone,SlNut])THEN(C_NDemand[Zone,SlNut]*N_CUptPotAct4[Zone,SlNut]*Rt_CLrv4[Zone]/C_NUptDeno[Zone,SlNut])ELSE(N_CUptPotAct4[Zone,SlNut]))ELSE(0)
      zn_N <- zonenut_df[zonenut_df$SlNut == "N",]
      zn_P <- zonenut_df[zonenut_df$SlNut == "P",]
      zonelayernut_df$C_NUptDeno <- c(rep(zn_N$C_NUptDeno, nlayer), rep(zn_P$C_NUptDeno, nlayer))
      zonelayernut_df$C_NUptPot <- c(rep(zn_N$C_NUptPot, nlayer), rep(zn_P$C_NUptPot, nlayer))
      zonelayernut_df$C_NDemand <- c(rep(zn_N$C_NDemand, nlayer), rep(zn_P$C_NDemand, nlayer))
      
      zonelayernut_df$N_CUpt <- ifelse(
        zonelayernut_df$C_NUptDeno > 0,
        ifelse(
          zonelayernut_df$C_NUptPot > zonelayernut_df$C_NDemand,
          zonelayernut_df$C_NDemand * zonelayernut_df$N_CUptPotAct * zonelayernut_df$Rt_CLrv /
            zonelayernut_df$C_NUptDeno,
          zonelayernut_df$N_CUptPotAct
        ),
        0
      )
      
      # C_NUptTot[Zone,SlNut] = N_CUpt1[Zone,SlNut]+N_CUpt2[Zone,SlNut]+N_CUpt3[Zone,SlNut]+N_CUpt4[Zone,SlNut]
      zonenut_df$C_NUptTot <- aggregate(zonelayernut_df["N_CUpt"],zonelayernut_df[c("zone", "SlNut")], sum)$N_CUpt 

      # C_Init[Zone,PlantComp] = IF(TIME=(int(Ca_PlantTime[Zone]) )) and  P_PrevCropOK? =  1 THEN AF_Crop?*Cq_GSeedCurr[Zone]*C_UnitConv[PlantComp]*C_SeedConc[PlantComp] else 0
      zonepcomp_df$Ca_PlantTime <- rep(zone_df$Ca_PlantTime, nrow(pcomp_df))
      zonepcomp_df$P_PrevCropOK_is <- P_PrevCropOK_is
      zonepcomp_df$Cq_GSeedCurr <- rep(zone_df$Cq_GSeedCurr, nrow(pcomp_df))
      zonepcomp_df$C_UnitConv <- rep(pcomp_df$C_UnitConv, each = nzone)
      # zonepcomp_df$C_SeedConc <- rep(pcomp_df$C_SeedConc, each = nzone)
      
      zonepcomp_df$C_Init <- ifelse(
        time == floor(zonepcomp_df$Ca_PlantTime)  &
          zonepcomp_df$P_PrevCropOK_is ==  1,
        pars$AF_par$AF_Crop_is * zonepcomp_df$Cq_GSeedCurr * zonepcomp_df$C_UnitConv * zonepcomp_df$C_SeedConc,
        0
      )
      
      # C_BiomInc[Zn1,DW]=  C_Init[Zn1,DW] + (C_PotGroRed[Zn1]*MIN(CW_Posgro[Zn1],C_NPosgro[Zn1,N],C_NPosgro[Zn1,P])) + 0*(C_NUptTot[Zn1,N]+C_NFIX[Zn1,N])
      # C_BiomInc[Zn1,N] =  C_Init[Zn1,N]+   C_NUptTot[Zn1,N]+C_NFIX[Zn1,N]+ 0*(C_PotGroRed[Zn1]+CW_Posgro[Zn1]+C_NPosgro[Zn1,N])
      # C_BiomInc[Zn1,P] =  C_Init[Zn1,P]+   C_NUptTot[Zn1,P]+C_NFIX[Zn1,P]+ 0*(C_PotGroRed[Zn1]+CW_Posgro[Zn1]+C_NPosgro[Zn1,N])
      # C_BiomInc[Zn2,DW]=  C_Init[Zn2,DW] + (C_PotGroRed[Zn2]*MIN(CW_Posgro[Zn2],C_NPosgro[Zn2,N],C_NPosgro[Zn2,P])) + 0*(C_NUptTot[Zn2,N]+C_NFIX[Zn2,N]) 
      # C_BiomInc[Zn2,N] =  C_Init[Zn2,N]+   C_NUptTot[Zn2,N]+C_NFIX[Zn2,N]+ 0*(C_PotGroRed[Zn1]+CW_Posgro[Zn1]+C_NPosgro[Zn1,N])
      # C_BiomInc[Zn2,P] =  C_Init[Zn2,P]+   C_NUptTot[Zn2,P]+C_NFIX[Zn2,P]+ 0*(C_PotGroRed[Zn1]+CW_Posgro[Zn1]+C_NPosgro[Zn1,N])
      # C_BiomInc[Zn3,DW]=  C_Init[Zn3,DW] + (C_PotGroRed[Zn3]*MIN(CW_Posgro[Zn3],C_NPosgro[Zn3,N],C_NPosgro[Zn3,P])) + 0*(C_NUptTot[Zn3,N]+C_NFIX[Zn1,N]) 
      # C_BiomInc[Zn3,N] =  C_Init[Zn3,N]+   C_NUptTot[Zn3,N]+C_NFIX[Zn3,N]+ 0*(C_PotGroRed[Zn1]+CW_Posgro[Zn1]+C_NPosgro[Zn1,N])
      # C_BiomInc[Zn3,P] =  C_Init[Zn3,P]+   C_NUptTot[Zn3,P]+C_NFIX[Zn3,P]+ 0*(C_PotGroRed[Zn1]+CW_Posgro[Zn1]+C_NPosgro[Zn1,N])
      # C_BiomInc[Zn4,DW]=  C_Init[Zn4,DW] + (C_PotGroRed[Zn4]*MIN(CW_Posgro[Zn4],C_NPosgro[Zn4,N],C_NPosgro[Zn4,P])) + 0*(C_NUptTot[Zn4,N]+C_NFIX[Zn1,N])
      # C_BiomInc[Zn4,N] =  C_Init[Zn4,N]  + C_NUptTot[Zn4,N]+C_NFIX[Zn4,N]+ 0*(C_PotGroRed[Zn1]+CW_Posgro[Zn1]+C_NPosgro[Zn1,N])  
      # C_BiomInc[Zn4,P] =  C_Init[Zn4,P]+   C_NUptTot[Zn4,P]+C_NFIX[Zn4,P]+ 0*(C_PotGroRed[Zn1]+CW_Posgro[Zn1]+C_NPosgro[Zn1,N])
      zonepcomp_df$C_BiomInc <- NA
      zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$C_BiomInc <- zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$C_Init + (
        zone_df$C_PotGroRed * pmin(
          zone_df$CW_Posgro,
          zone_df$C_NPosgro_N,
          zone_df$C_NPosgro_P
        )
      )
      zonepcomp_df[zonepcomp_df$PlantComp %in% c("N", "P"), ]$C_BiomInc <-  zonepcomp_df[zonepcomp_df$PlantComp %in% c("N", "P"), ]$C_Init + zonenut_df$C_NUptTot + zonenut_df$C_NFIX
      
      # C_RootGrowth[Zone,PlantComp] = IF C_PlantDiesToday?[Zone]=0 then C_GroResMobFrac*C_BiomInc[Zone,PlantComp]*(ARRAYSUM(C_RtAllocAct[Zone,*]))/(1-ARRAYSUM(C_RtAllocAct[Zone,*])) else 0
      zone_df$C_RtAllocAct_sum <- aggregate(zonelayer_df["C_RtAllocAct"], zonelayer_df["zone"], sum)$C_RtAllocAct
      zonepcomp_df$C_RtAllocAct_sum <- rep(zone_df$C_RtAllocAct_sum, nrow(pcomp_df))
      zonepcomp_df$C_RootGrowth <- ifelse( zonepcomp_df$C_PlantDiesToday_is ==0, pars$C_par$C_GroResMobFrac * zonepcomp_df$C_BiomInc*zonepcomp_df$C_RtAllocAct_sum/(1-zonepcomp_df$C_RtAllocAct_sum), 0)
      

      
      # C_RtIncr_DW[Zone,SoilLayer] = C_RtAllocAct[Zone,SoilLayer]*(C_Init[Zone,DW]+C_RootGrowth[Zone,DW])
      # C_RtIncr_N[Zone,SoilLayer] = C_RtAllocAct[Zone,SoilLayer]*(C_Init[Zone,N]+C_RootGrowth[Zone,N])
      # C_RtIncr_P[Zone,SoilLayer] = C_RtAllocAct[Zone,SoilLayer]*(C_Init[Zone,P]+C_RootGrowth[Zone,P])
      zonelayerpcomp_df$C_RtAllocAct <- rep(zonelayer_df$C_RtAllocAct)
      zp_DW <- zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]
      zp_N <- zonepcomp_df[zonepcomp_df$PlantComp == "N", ]
      zp_P <- zonepcomp_df[zonepcomp_df$PlantComp == "P", ]
      zonelayerpcomp_df$C_Init <- c(rep(zp_DW$C_Init, nlayer),
                                    rep(zp_N$C_Init, nlayer),
                                    rep(zp_P$C_Init, nlayer))
      zonelayerpcomp_df$C_RootGrowth <- c(
        rep(zp_DW$C_RootGrowth, nlayer),
        rep(zp_N$C_RootGrowth, nlayer),
        rep(zp_P$C_RootGrowth, nlayer)
      )
      
      zonelayerpcomp_df$C_RtIncr <- zonelayerpcomp_df$C_RtAllocAct * (zonelayerpcomp_df$C_Init + zonelayerpcomp_df$C_RootGrowth)

      # C_Root_DW[Zone,SoilLayer](t) = C_Root_DW[Zone,SoilLayer](t - dt) + (C_RtIncr_DW[Zone,SoilLayer] - C_RtDecay_DW[Zone,SoilLayer]) * dt
      # C_Root_N[Zone,SoilLayer](t) = C_Root_N[Zone,SoilLayer](t - dt) + (C_RtIncr_N[Zone,SoilLayer] - C_RtDecay_N[Zone,SoilLayer]) * dt
      # C_Root_P[Zone,SoilLayer](t) = C_Root_P[Zone,SoilLayer](t - dt) + (C_RtIncr_P[Zone,SoilLayer] - C_RtDecay_P[Zone,SoilLayer]) * dt
      zonelayerpcomp_df$C_Root <- zonelayerpcomp_df$C_Root + (zonelayerpcomp_df$C_RtIncr - zonelayerpcomp_df$C_RtDecay)

      # Rt_CRelChange[Zone] = IF(Rt_ACType=2 AND Cq_Stage[Zone]>0.07  AND Rt_CLraCurr[Zone]>0)THEN((ARRAYSUM(C_RtIncr_DW[Zone,*])-ARRAYSUM(C_RtDecay_DW[Zone,*]))*1000*Rt_CSRLCurr[Zone]/Rt_CLraCurr[Zone])ELSE(0)
      zonelayer_df$C_RtIncr_DW <- zonelayerpcomp_df[zonelayerpcomp_df$PlantComp == "DW",]$C_RtIncr
      zone_df$C_RtIncr_DW_sum <- aggregate(zonelayer_df["C_RtIncr_DW"],zonelayer_df["zone"], sum)$C_RtIncr_DW
      zonelayer_df$C_RtDecay_DW <- zonelayerpcomp_df[zonelayerpcomp_df$PlantComp == "DW",]$C_RtDecay
      zone_df$C_RtDecay_DW_sum <- aggregate(zonelayer_df["C_RtDecay_DW"],zonelayer_df["zone"], sum)$C_RtDecay_DW
      
      zone_df$Rt_CRelChange <- ifelse(zone_df$Rt_ACType==2 & zone_df$Cq_Stage>0.07  & zone_df$Rt_CLraCurr>0, (zone_df$C_RtIncr_DW_sum-zone_df$C_RtDecay_DW_sum)*1000*zone_df$Rt_CSRLCurr/zone_df$Rt_CLraCurr, 0)
      
      # C_NUptRelDeno[Zone,SlNut] = ((AF_HGW1[Zone]+AF_HGW2[Zone]+AF_HGW3[Zone]+AF_HGW4[Zone])*(N_CUpt1[Zone,SlNut]+N_CUpt2[Zone,SlNut]+N_CUpt3[Zone,SlNut]+N_CUpt4[Zone,SlNut]))
      zone_df$AF_HGW_sum <- aggregate(zonelayer_df["AF_HGW"], zonelayer_df["zone"], sum)$AF_HGW
      zonenut_df$AF_HGW_sum <- rep(zone_df$AF_HGW_sum, nrow(nut_df))
      zonenut_df$N_CUpt_sum <- aggregate(zonelayernut_df["N_CUpt"], zonelayernut_df[c("zone", "SlNut")] , sum)$N_CUpt
      zonenut_df$C_NUptRelDeno <- zonenut_df$AF_HGW_sum * zonenut_df$N_CUpt_sum
      
      #TODO: make sure the rep array is updated after the value updated "on the dynamic loop!" 
      
      # C_NUptRel1[Zone,SlNut] = IF( Rt_CAmount1[Zone]>0.0001)THEN(100*AF_HGW1[Zone]*N_CUpt1[Zone,SlNut]/Rt_CAmount1[Zone])ELSE(0)
      # C_NUptRel2[Zone,SlNut] = IF( Rt_CAmount2[Zone]>0.0001)THEN(100*AF_HGW2[Zone]*N_CUpt2[Zone,SlNut]/Rt_CAmount2[Zone])ELSE(0)
      # C_NUptRel3[Zone,SlNut] = IF( Rt_CAmount3[Zone]>0.0001)THEN(100*AF_HGW3[Zone]*N_CUpt3[Zone,SlNut]/Rt_CAmount3[Zone])ELSE(0)
      # C_NUptRel4[Zone,SlNut] = IF( Rt_CAmount4[Zone]>0.0001)THEN(100*AF_HGW4[Zone]*N_CUpt4[Zone,SlNut]/Rt_CAmount4[Zone])ELSE(0)
      zonelayernut_df$Rt_CAmount <- rep(zonelayer_df$Rt_CAmount, nrow(nut_df))
      zonelayernut_df$AF_HGW <- rep(zonelayer_df$AF_HGW, nrow(nut_df))
      zonelayernut_df$C_NUptRel <- ifelse( zonelayernut_df$Rt_CAmount >0.0001, 100*zonelayernut_df$AF_HGW* zonelayernut_df$N_CUpt/zonelayernut_df$Rt_CAmount, 0)

      # C_NUptRel[Zone,SlNut] = IF(Cq_Stage[Zone]>0.1  AND Rt_ACType=2 AND C_NUptRelDeno[Zone,SlNut]>0)  THEN ((C_NUptRel1[Zone,SlNut]+C_NUptRel2[Zone,SlNut]+C_NUptRel3[Zone,SlNut]+C_NUptRel4[Zone,SlNut])*Rt_CAmount[Zone]/C_NUptRelDeno[Zone,SlNut])ELSE(1)
      zonenut_df$Rt_CAmount <- rep(zone_df$Rt_CAmount, nrow(nut_df))
      zonenut_df$Rt_ACType <- pars$Rt_par$Rt_ACType
      zonenut_df$C_NUptRel_sum <- aggregate(zonelayernut_df["C_NUptRel"], zonelayernut_df[c("zone", "SlNut")], sum)$C_NUptRel
      zonenut_df$C_NUptRel <- ifelse(
        zonenut_df$Cq_Stage > 0.1  &
          zonenut_df$Rt_ACType == 2 & zonenut_df$C_NUptRelDeno > 0,
        zonenut_df$C_NUptRel_sum * zonenut_df$Rt_CAmount / zonenut_df$C_NUptRelDeno,
        1
      )
      
      # CW_UptRelDeno[Zone] = ((AF_HGW1[Zone]+AF_HGW2[Zone]+AF_HGW3[Zone]+AF_HGW4[Zone])*(W_CUpt1[Zone]+W_CUpt2[Zone]+W_CUpt3[Zone]+W_CUpt4[Zone]))
      # zone_df$AF_HGW_sum <- aggregate(zonelayer_df["AF_HGW"], zonelayer_df["zone"], sum)$AF_HGW
      # zonenut_df$AF_HGW_sum <- rep(zone_df$AF_HGW_sum, nrow(nut_df))
      zone_df$N_CUpt_sum <- aggregate(zonelayer_df["W_CUpt"], zonelayer_df[c("zone")] , sum)$W_CUpt
      zone_df$CW_UptRelDeno <- zone_df$AF_HGW_sum*zone_df$N_CUpt_sum
      
      # CW_UptRel1[Zone] = IF(Rt_CAmount1[Zone]>0.0001)THEN (100*AF_HGW1[Zone]*W_CUpt1[Zone]/Rt_CAmount1[Zone]) ELSE(0)
      # CW_UptRel2[Zone] = IF(Rt_CAmount2[Zone]>0.0001)THEN (100*AF_HGW2[Zone]*W_CUpt2[Zone]/Rt_CAmount2[Zone]) ELSE(0)
      # CW_UptRel3[Zone] = IF(Rt_CAmount3[Zone]>0.0001)THEN (100*AF_HGW3[Zone]*W_CUpt3[Zone]/Rt_CAmount3[Zone]) ELSE(0)
      # CW_UptRel4[Zone] = IF(Rt_CAmount4[Zone]>0.0001)THEN (100*AF_HGW4[Zone]*W_CUpt4[Zone]/Rt_CAmount4[Zone]) ELSE(0)
      zonelayer_df$CW_UptRel <- ifelse(zonelayer_df$Rt_CAmount > 0.0001, 100*zonelayer_df$AF_HGW* zonelayer_df$W_CUpt/zonelayer_df$Rt_CAmount, 0)

      # CW_UptRel[Zone] = IF(Cq_Stage[Zone]>0.05 AND  Rt_ACType=2 AND CW_UptRelDeno[Zone]>0)  THEN ((CW_UptRel1[Zone]+CW_UptRel2[Zone]+CW_UptRel3[Zone]+CW_UptRel4[Zone])*Rt_CAmount[Zone]/CW_UptRelDeno[Zone])ELSE(1)
      zone_df$CW_UptRel_sum <- aggregate(zonelayer_df["CW_UptRel"], zonelayer_df["zone"], sum)$CW_UptRel
      zone_df$CW_UptRel <- ifelse(zone_df$Cq_Stage>0.05 &  zone_df$Rt_ACType==2 & zone_df$CW_UptRelDeno>0,  
                                  zone_df$CW_UptRel_sum* zone_df$Rt_CAmount/ zone_df$CW_UptRelDeno, 1)
      
      # Rt_CUptRel[Zone] = If (C_NPosgro[Zone,N]< 1 OR C_NPosgro[Zone,P] <1 OR CW_Posgro[Zone] <1) then if  (min(C_NPosgro[Zone,N],C_NPosgro[Zone,P],CW_Posgro[Zone])=C_NPosgro[Zone,N]) then C_NUptRel[Zone,N] else if (min(C_NPosgro[Zone,N],C_NPosgro[Zone,P],CW_Posgro[Zone])=C_NPosgro[Zone,P]) then C_NUptRel[Zone,P] else CW_UptRel[Zone] else 1
      zone_df$C_NUptRel_N <- zonenut_df[zonenut_df$SlNut == "N",]$C_NUptRel
      zone_df$C_NUptRel_P <- zonenut_df[zonenut_df$SlNut == "P",]$C_NUptRel
      
      zone_df$Rt_CUptRel <- ifelse (
        zone_df$C_NPosgro_N < 1 |
          zone_df$C_NPosgro_P < 1 | zone_df$CW_Posgro < 1,
        ifelse  (
          pmin(zone_df$C_NPosgro_N, zone_df$C_NPosgro_P, zone_df$CW_Posgro) == zone_df$C_NPosgro_N,
          zone_df$C_NUptRel_N,
          ifelse (
            pmin(zone_df$C_NPosgro_N, zone_df$C_NPosgro_P, zone_df$CW_Posgro) == zone_df$C_NPosgro_P,
            zone_df$C_NUptRel_P,
            zone_df$CW_UptRel
          )
        ),
        1
      )
      
      # Rt_CDecDepthD[Zone] = IF(Rt_ACType=2 AND Cq_Stage[Zone]>0 )THEN (MIN(1+Rt_CRelChange[Zone], MAX((1+Rt_CRelChange[Zone])^-1, Rt_CUptRel[Zone]^-Rt_CDistResp[Zone])))ELSE(1)
      zone_df$Rt_CDecDepthD <- ifelse(zone_df$Rt_ACType==2 & zone_df$Cq_Stage>0, pmin(1+zone_df$Rt_CRelChange, pmax((1+zone_df$Rt_CRelChange)^-1, zone_df$Rt_CUptRel^-zone_df$Rt_CDistResp)), 1)
      
      # Rt_CDecDepthInc[Zone] = if C_PlantDiesToday?[Zone] = 1 then - RT_CDecDepthAct[Zone] else IF(Rt_ACType=2)THEN IF Cq_Stage[Zone]<2 THEN (RT_CDecDepthAct[Zone]*(Rt_CDecDepthD[Zone]-1)) ELSE-RT_CDecDepthAct[Zone] ELSE 0
      zone_df$Rt_CDecDepthInc <- ifelse( zone_df$C_PlantDiesToday_is == 1, - zone_df$RT_CDecDepthAct, ifelse(zone_df$Rt_ACType==2, ifelse(zone_df$Cq_Stage<2, (zone_df$RT_CDecDepthAct*(zone_df$Rt_CDecDepthD-1)), -zone_df$RT_CDecDepthAct), 0))
      
      # Rt_CDecDepth_ActInitSeason[Zone] = If TIME= Ca_PlantTime[Zone] THEN Rt_CDecDepthC[Zone] ELSE 0
      zone_df$Rt_CDecDepth_ActInitSeason <- ifelse( time== zone_df$Ca_PlantTime, zone_df$Rt_CDecDepthC, 0)
      
      # RT_CDecDepthAct[Zone](t) = RT_CDecDepthAct[Zone](t - dt) + (Rt_CDecDepthInc[Zone] + Rt_CDecDepth_ActInitSeason[Zone]) * dt
      zone_df$RT_CDecDepthAct <- zone_df$RT_CDecDepthAct + (zone_df$Rt_CDecDepthInc + zone_df$Rt_CDecDepth_ActInitSeason) 
      
      # T_WoodHarvest[PlantComp,Tree] = if time = int(S&B_SlashTime[Tree]) then T_SapWood[PlantComp,Tree]*T_SlashSellWoodFrac[Tree]/dt else 
      #   if time = T_WoodHarvDay[Tree] then T_SapWood[PlantComp,Tree] * T_WoodHarvFrac[Tree]/dt else if T_StemDiam[Tree]>T_DiamTreshHarv[Tree] then T_SapWood[PlantComp,Tree] * T_WoodHarvFrac[Tree]/dt else
      #     if T_SapWood[DW, Tree] = 0 then 0 else
      #       if T_Prun[DW, Tree]> 0 then max(0,(T_WoodH[Tree]*(1-T_WoodFracHRemain[Tree])))*pi*T_StemDiam[Tree]^2*T_WoodDens[Tree]*T_TreesperHa[Tree]*T_UnitConv[PlantComp]*T_WoodConc[PlantComp,Tree]/(10^8*4)else 0
      treepcomp_df$T_SlashSellWoodFrac <- rep(tree_df$T_SlashSellWoodFrac, nrow(pcomp_df))
      treepcomp_df$T_WoodHarvFrac <- rep(tree_df$T_WoodHarvFrac, nrow(pcomp_df))
      treepcomp_df$T_StemDiam <- rep(tree_df$T_StemDiam, nrow(pcomp_df))
      treepcomp_df$T_DiamTreshHarv <- rep(tree_df$T_DiamTreshHarv, nrow(pcomp_df))
      tree_df$T_Prun_DW <- treepcomp_df[treepcomp_df$PlantComp == "DW",]$T_Prun
      treepcomp_df$T_Prun_DW <- rep(tree_df$T_Prun_DW, nrow(pcomp_df))
      treepcomp_df$T_WoodH <- rep(tree_df$T_WoodH, nrow(pcomp_df))
      treepcomp_df$T_WoodFracHRemain <- rep(tree_df$T_WoodFracHRemain, nrow(pcomp_df))
      treepcomp_df$T_WoodDens <- rep(tree_df$T_WoodDens, nrow(pcomp_df))
      
      treepcomp_df$T_WoodHarvest <- ifelse(
        time == floor(treepcomp_df$SB_SlashTime),
        treepcomp_df$T_SapWood * treepcomp_df$T_SlashSellWoodFrac,
        ifelse(
          time == treepcomp_df$T_WoodHarvDay,
          treepcomp_df$T_SapWood * treepcomp_df$T_WoodHarvFrac,
          ifelse(
            treepcomp_df$T_StemDiam > treepcomp_df$T_DiamTreshHarv,
            treepcomp_df$T_SapWood * treepcomp_df$T_WoodHarvFrac,
            ifelse(
              treepcomp_df$T_SapWood_DW == 0,
              0,
              ifelse(
                treepcomp_df$T_Prun_DW > 0,
                pmax(0, (treepcomp_df$T_WoodH * (
                  1 - treepcomp_df$T_WoodFracHRemain
                ))) * pi *  treepcomp_df$T_StemDiam^2 *  treepcomp_df$T_WoodDens * treepcomp_df$T_Treesperha *
                  treepcomp_df$T_UnitConv * treepcomp_df$T_WoodConc / (10^8 * 4),
                0
              )
            )
          )
        )
      )

      # T_GroResLoss[PlantComp,Tree] = IF T_DiesToday?[Tree]=1 THEN T_GroRes[PlantComp,Tree] ELSE IF TIME=T_WoodHarvDay[Tree] THEN T_GroRes[PlantComp,Tree]*T_WoodHarvFrac[Tree] ELSE 0
      treepcomp_df$T_GroResLoss <- ifelse(
        treepcomp_df$T_DiesToday_is == 1,
        treepcomp_df$T_GroRes,
        ifelse(
          time == treepcomp_df$T_WoodHarvDay,
          treepcomp_df$T_GroRes * treepcomp_df$T_WoodHarvFrac,
          0
        )
      )
      
      # T_WoodHCurr[Tree] = IF T_StemDiam[Tree]>0 THEN(4*T_SapWood[DW,Tree]*10000/T_TreesperHa[Tree])/(T_WoodDens[Tree]*pi*(T_StemDiam[Tree]/100)^2) ELSE 0
      tree_df$T_WoodHCurr <- ifelse( tree_df$T_StemDiam >0, (4*T_SapWood[DW,Tree]*10000/T_TreesperHa[Tree])/(T_WoodDens[Tree]*pi*(T_StemDiam[Tree]/100)^2), 0)
      
      
      # T_WoodHInc[Tree] = if (time=T_PlantTime[Tree]) then T_WoodHInit[Tree] else if T_ApplyPalm?[Tree] = 1 then TF_TrunkHIncr[Tree] else (IF T_DiesToday?[Tree] THEN -T_WoodH[Tree] 
      # ELSE if T_ApplyFBA?[Tree]=1 then if  T_WoodHarvest[DW,Tree]>0  then -(T_WoodHarvest[DW,Tree]+T_GroResLoss[DW,Tree])*10000/(T_TreesperHa[Tree]*T_WoodDens[Tree]*pi*(T_StemDiam[Tree]/(2*100))^2)
      # else if T_StemDiam[Tree]>0 then max(T_WoodHCurr[Tree]-T_WoodH[Tree],0) else 0 else if T_WoodHarvest[DW,Tree]>0 then -(T_GroResLoss[DW,Tree]+T_WoodHarvFrac[Tree])*T_WoodH[Tree] else (T_CanBiomInc[DW,Tree]*T_LWR[Tree])/(T_LAIMax[Tree]*T_CanWidthMax[Tree]))
      tp_DW <- treepcomp_df[treepcomp_df$PlantComp == "DW",]
      tree_df$T_WoodHInc <- ifelse (
        time == tree_df$T_PlantTime,
        tree_df$T_WoodHInit,
        ifelse(
          tree_df$T_ApplyPalm_is == 1,
          tree_df$TF_TrunkHIncr,
          ifelse(
            tree_df$T_DiesToday_is == 1,
            -tree_df$T_WoodH,
            ifelse(
              tree_df$T_ApplyFBA_is == 1,
              ifelse(
                tp_DW$T_WoodHarvest > 0,
                -(tp_DW$T_WoodHarvest + tp_DW$T_GroResLoss) * 10000 / (tree_df$T_Treesperha *
                                                                         tree_df$T_WoodDens * pi * (tree_df$T_StemDiam / (2 * 100))^2),
                ifelse(tree_df$T_StemDiam >
                         0, pmax(tree_df$T_WoodHCurr - tree_df$T_WoodH, 0), 0)
              ),
              ifelse(
                tp_DW$T_WoodHarvest > 0,
                -(tp_DW$T_GroResLoss + tree_df$T_WoodHarvFrac) * tree_df$T_WoodH,
                (tp_DW$T_CanBiomInc * tree_df$T_LWR) / (tree_df$T_LAIMax * tree_df$T_CanWidthMax)
              )
            )
          )
        )
      )
      
      # T_WoodH[Tree](t) = T_WoodH[Tree](t - dt) + (T_WoodHInc[Tree]) * dt
      tree_df$T_WoodH <- tree_df$T_WoodH + (tree_df$T_WoodHInc) 

      # C_StLeafInc[Zone,PlantComp] = IF (C_PlantDiesToday?[Zone]=0 )THEN(C_GroResMobFrac *(1-Cq_CHarvAllocCurr[Zone])*C_GroRes[Zone,PlantComp]-Cq_RemobFrac[Zone]*C_BiomStLv[Zone,PlantComp])ELSE(0)
      zonepcomp_df$C_StLeafInc <- ifelse (zonepcomp_df$C_PlantDiesToday_is ==0, pars$C_par$C_GroResMobFrac *(1-zonepcomp_df$Cq_CHarvAllocCurr)* zonepcomp_df$C_GroRes- zonepcomp_df$Cq_RemobFrac* zonepcomp_df$C_BiomStLv, 0)
      
      # C_HeightInc[Zone] = if (Cq_Stage[Zone] < 1) then (Cq_HBiomConvCurr[Zone] * C_StLeafInc[Zone,DW]) else 0
      zone_df$C_HeightInc <- ifelse (zone_df$Cq_Stage < 1, zone_df$Cq_HBiomConvCurr * zonepcomp_df[zonepcomp_df$PlantComp == "DW",]$C_StLeafInc, 0)
            
      # C_HeightDec[Zone] = if (int(Cq_Stage[Zone]) = 2 and Cq_StageAfterHarvest[Zone]=0) then C_Height[Zone] /dt  else 0
      zone_df$C_HeightDec <- ifelse (floor(zone_df$Cq_Stage) == 2 & zone_df$Cq_StageAfterHarvest==0, zone_df$C_Height, 0)
      
      # C_Height[Zone](t) = C_Height[Zone](t - dt) + (C_HeightInc[Zone] - C_HeightDec[Zone]) * dt
      zone_df$C_Height <- zone_df$C_Height + (zone_df$C_HeightInc - zone_df$C_HeightDec) 
      
      # T_StageInc[Sp1,VegGen] = if T_DiesToday?[Sp1] = 1 then -T_Stage[Sp1,VegGen]/dt elseif time = int(T_PlantTime[Sp1]) then T_InitStage[Sp1] else  if T_Prun[DW, Sp1]>0 then T_StageAftPrun?[Sp1,VegGen]*(T_StageAftPrun[Sp1]-T_Stage[Sp1,VegGen])/ dt else     if (T_Stage[Sp1,VegGen] >=2 ) then ((-T_Stage[Sp1,VegGen]+1)/dt) else      if (T_Stage[Sp1,VegGen] > 0 and T_Stage[Sp1,VegGen] < 1) then min(1-T_Stage[Sp1,VegGen], (1/(T_TimeVeg[Sp1]))) else         if (T_Stage[Sp1,VegGen] >=1 and T_Stage[Sp1,VegGen] < 2) then (1/(T_TimeGenCycle[Sp1])) else           if T_Stage[Sp1,VegGen]>=1 and (mod (Time,365)  > T_DOYFlwBeg[Sp1] and mod (Time,365)  < T_DOYFlwEnd[Sp1]) then (1/T_TimeGenCycle[Sp1]) else 0 *(T_LfTwig[DW,Sp1]+T_CanBiomInc[DW,Sp1]) 
      # T_StageInc[Sp2,VegGen] = if T_DiesToday?[Sp2] = 1 then -T_Stage[Sp2,VegGen]/dt elseif time = int(T_PlantTime[Sp2]) then T_InitStage[Sp2] else  if T_Prun[DW, Sp2]>0 then T_StageAftPrun?[Sp2,VegGen]*(T_StageAftPrun[Sp2]-T_Stage[Sp2,VegGen])/ dt else     if (T_Stage[Sp2,VegGen] >=2 ) then ((-T_Stage[Sp2,VegGen]+1)/dt) else      if (T_Stage[Sp2,VegGen] > 0 and T_Stage[Sp2,VegGen] < 1) then min(1-T_Stage[Sp2,VegGen], (1/(T_TimeVeg[Sp2]))) else         if (T_Stage[Sp2,VegGen] >=1 and T_Stage[Sp2,VegGen] < 2) then (1/(T_TimeGenCycle[Sp2])) else           if T_Stage[Sp2,VegGen]>= 1 and (mod (Time,365)  > T_DOYFlwBeg[Sp2] and mod (Time,365)  < T_DOYFlwEnd[Sp2]) then (1/T_TimeGenCycle[Sp2]) else 0 *(T_LfTwig[DW,Sp1]+T_CanBiomInc[DW,Sp1]) 
      # T_StageInc[Sp3,VegGen] = if T_DiesToday?[Sp3] = 1 then -T_Stage[Sp3,VegGen]/dt elseif time = int(T_PlantTime[Sp3]) then T_InitStage[Sp3] else  if T_Prun[DW, Sp3]>0 then T_StageAftPrun?[Sp3,VegGen]*(T_StageAftPrun[Sp3]-T_Stage[Sp3,VegGen])/ dt else     if (T_Stage[Sp3,VegGen] >=2 ) then ((-T_Stage[Sp3,VegGen]+1)/dt) else      if (T_Stage[Sp3,VegGen] > 0 and T_Stage[Sp3,VegGen] < 1) then min(1-T_Stage[Sp3,VegGen], (1/(T_TimeVeg[Sp3]))) else         if (T_Stage[Sp3,VegGen] >=1 and T_Stage[Sp3,VegGen] < 2) then (1/(T_TimeGenCycle[Sp3])) else           if T_Stage[Sp3,VegGen]>= 1 and (mod (Time,365)  > T_DOYFlwBeg[Sp3] and mod (Time,365)  < T_DOYFlwEnd[Sp3]) then (1/T_TimeGenCycle[Sp3]) else 0 *( T_LfTwig[DW,Sp1]+T_CanBiomInc[DW,Sp1]) 
      # 
      # T_StageInc[Sp1,LeafAge] = if T_LfTwig[DW,Sp1]+T_CanBiomInc[DW,Sp1]> 0 then -T_Stage[Sp1,LeafAge]+(T_CanBiomInc[DW,Sp1]+T_LfTwig[DW,Sp1]*(T_Stage[Sp1,LeafAge]+1))/(T_LfTwig[DW,Sp1]+T_CanBiomInc[DW,Sp1]) else -T_Stage[Sp1,LeafAge]+0*(T_StageAftPrun?[Sp1,LeafAge]*T_Stage[Sp1,LeafAge]+T_DiesToday?[Sp1]+T_DOYFlwBeg[Sp1]+T_DOYFlwEnd[Sp1]+T_InitStage[Sp1]+T_PlantTime[Sp1]+T_Prun[DW,Sp1]+T_StageAftPrun[Sp1]+T_TimeGenCycle[Sp1]+T_TimeVeg[Sp1])
      # T_StageInc[Sp2,LeafAge] = if T_LfTwig[DW,Sp2]+T_CanBiomInc[DW,Sp2]> 0 then -T_Stage[Sp2,LeafAge]+(T_CanBiomInc[DW,Sp2]+T_LfTwig[DW,Sp2]*(T_Stage[Sp2,LeafAge]+1))/(T_LfTwig[DW,Sp2]+T_CanBiomInc[DW,Sp2]) else -T_Stage[Sp2,LeafAge]+0*(T_StageAftPrun?[Sp2,LeafAge]*T_Stage[Sp1,LeafAge]+T_DiesToday?[Sp1]+T_DOYFlwBeg[Sp1]+T_DOYFlwEnd[Sp1]+T_InitStage[Sp1]+T_PlantTime[Sp1]+T_Prun[DW,Sp1]+T_StageAftPrun[Sp1]+T_TimeGenCycle[Sp1]+T_TimeVeg[Sp1])
      # T_StageInc[Sp3,LeafAge] = if T_LfTwig[DW,Sp3]+T_CanBiomInc[DW,Sp3]> 0 then -T_Stage[Sp3,LeafAge]+(T_CanBiomInc[DW,Sp3]+T_LfTwig[DW,Sp3]*(T_Stage[Sp3,LeafAge]+1))/(T_LfTwig[DW,Sp3]+T_CanBiomInc[DW,Sp3]) else -T_Stage[Sp3,LeafAge]+0*(T_StageAftPrun?[Sp3,LeafAge]*T_Stage[Sp1,LeafAge]+T_DiesToday?[Sp1]+T_DOYFlwBeg[Sp1]+T_DOYFlwEnd[Sp1]+T_InitStage[Sp1]+T_PlantTime[Sp1]+T_Prun[DW,Sp1]+T_StageAftPrun[Sp1]+T_TimeGenCycle[Sp1]+T_TimeVeg[Sp1])
      treestage_df$T_StageInc<-NA
      ts_VG <- treestage_df[treestage_df$Tree_Stage == "VegGen",]
      treestage_df[treestage_df$Tree_Stage == "VegGen", ]$T_StageInc <- ifelse(
        tree_df$T_DiesToday_is == 1,
        -ts_VG$T_Stage,
        ifelse(
          time == floor(tree_df$T_PlantTime),
          tree_df$T_InitStage,
          ifelse(
            tree_df$T_Prun_DW > 0,
            ts_VG$T_StageAftPrun_is * (tree_df$T_StageAftPrun - ts_VG$T_Stage),
            ifelse (
              ts_VG$T_Stage >= 2,
              -ts_VG$T_Stage + 1,
              ifelse(
                ts_VG$T_Stage > 0 &
                  ts_VG$T_Stage < 1,
                pmin(1 - ts_VG$T_Stage, 1 / tree_df$T_TimeVeg),
                ifelse (
                  ts_VG$T_Stage >= 1 &
                    ts_VG$T_Stage < 2,
                  1 / tree_df$T_TimeGenCycle,
                  ifelse(
                    ts_VG$T_Stage >= 1 &
                      ((time %% 365)  > tree_df$T_DOYFlwBeg &
                         (time %% 365)  < tree_df$T_DOYFlwEnd
                      ),
                    1 / tree_df$T_TimeGenCycle,
                    0
                  )
                )
              )
            )
          )
        )
      )

      ts_LA <- treestage_df[treestage_df$Tree_Stage == "LeafAge",]
      tp_DW <- treepcomp_df[treepcomp_df$PlantComp == "DW",]
      treestage_df[treestage_df$Tree_Stage == "LeafAge", ]$T_StageInc <- ifelse(
        tp_DW$T_LfTwig + tp_DW$T_CanBiomInc > 0,
        -ts_LA$T_Stage +
          (tp_DW$T_CanBiomInc + tp_DW$T_LfTwig * (ts_LA$T_Stage + 1)) / (tp_DW$T_LfTwig + tp_DW$T_CanBiomInc),
        -ts_LA$T_Stage
      )
      
      # T_Stage[Tree,Tree_Stage](t) = T_Stage[Tree,Tree_Stage](t - dt) + (T_StageInc[Tree,Tree_Stage]) * dt
      treestage_df$T_Stage <- treestage_df$T_Stage + (treestage_df$T_StageInc) 
      
      # TW_PDryFactRUpd[Tree] = if T_LfTwig[DW,Tree] = 0 then -TW_DryFactPower[Tree] + TW_DryFactPowerInit[Tree] else if TW_Best?[Tree,1] = 1 or TW_Best?[Tree,2] = 1 or TW_Best?[Tree,9] = 1 or TW_Best?[Tree,10] = 1 then (if (TW_DryFactPower[Tree] +TW_DryFactPowerRangeChange) < TW_DryPowerMax then +TW_DryFactPowerRangeChange else 0) else if TW_Best?[Tree,5] = 1 or TW_Best?[Tree,6] = 1 and (1+TW_DryFactPowerRangeChange)<>0 then (TW_DryFactPower[Tree]/(1+TW_DryFactPowerRangeChange))-TW_DryFactPower[Tree] else if TW_Best?[Tree,3] = 1 or TW_Best?[Tree,4] = 1 or TW_Best?[Tree,7] = 1 or TW_Best?[Tree,8] = 1 then 0 else 0
      TW_DryPowerMax <- pars$CW_par$CW_DryPowerMax
      TW_DryPowerMin <- pars$CW_par$CW_DryPowerMin
      
      tree_df$TW_PDryFactRUpd <- ifelse(
        tp_DW$T_LfTwig == 0,
        -tree_df$TW_DryFactPower + tree_df$TW_DryFactPowerInit,
        ifelse(
          treebuf_df[treebuf_df$buf_id == 1, ]$TW_Best_is == 1 |
            treebuf_df[treebuf_df$buf_id == 2, ]$TW_Best_is == 1 |
            treebuf_df[treebuf_df$buf_id == 9, ]$TW_Best_is == 1 |
            treebuf_df[treebuf_df$buf_id == 10, ]$TW_Best_is == 1,
          ifelse(
            (
              tree_df$TW_DryFactPower + pars$T_par$TW_DryFactPowerRangeChange
            ) < TW_DryPowerMax,
            pars$T_par$TW_DryFactPowerRangeChange,
            0
          ),
          ifelse(
            treebuf_df[treebuf_df$buf_id == 5, ]$TW_Best_is == 1 |
              treebuf_df[treebuf_df$buf_id == 6, ]$TW_Best_is == 1 &
              (1 + pars$T_par$TW_DryFactPowerRangeChange) != 0,
            (
              tree_df$TW_DryFactPower / (1 + pars$T_par$TW_DryFactPowerRangeChange)
            ) - tree_df$TW_DryFactPower,
            ifelse(
              treebuf_df[treebuf_df$buf_id == 3, ]$TW_Best_is == 1 |
                treebuf_df[treebuf_df$buf_id == 4, ]$TW_Best_is == 1 |
                treebuf_df[treebuf_df$buf_id == 7, ]$TW_Best_is == 1 |
                treebuf_df[treebuf_df$buf_id == 8, ]$TW_Best_is == 1,
              0,
              0
            )
          )
        )
      )
      
      # TW_DryFactPower[Tree](t) = TW_DryFactPower[Tree](t - dt) + (TW_PDryFactRUpd[Tree]) * dt
      tree_df$TW_DryFactPower <- tree_df$TW_DryFactPower + (tree_df$TW_PDryFactRUpd)
      
      # CW_DryFactRangeUpdate[Zone] = if Cq_Stage[Zone]=0 then  -CW_DryFactRangePower[Zone]+CW_DryFactRangePowerStart 
      # else if ((CW_Best[Zone,1]=1) or (CW_Best[Zone,2]=1) or (CW_Best[Zone,9]=1) or (CW_Best[Zone,10]=1)) then 
      # ((1+CW_DryFactRangeChange)*CW_DryFactRangePower[Zone])-CW_DryFactRangePower[Zone] else  if ((CW_Best[Zone,5]=1) or (CW_Best[Zone,6]=1)) then 
      # (CW_DryFactRangePower[Zone]/(1+CW_DryFactRangeChange))-CW_DryFactRangePower[Zone] 
      # else if ((CW_Best[Zone,3]=1) or (CW_Best[Zone,4]=1)) or ((CW_Best[Zone,7]=1) or (CW_Best[Zone,8]=1)) then CW_DryFactRangePower[Zone]-CW_DryFactRangePower[Zone] else 0
      zone_df$CW_DryFactRangeUpdate <- ifelse(
        zone_df$Cq_Stage == 0,
        -zone_df$CW_DryFactRangePower + pars$CW_par$CW_DryFactRangePowerStart,
        ifelse ((treebuf_df[treebuf_df$buf_id == 1, ]$CW_Best[Zone, 1] == 1) |
                  (treebuf_df[treebuf_df$buf_id == 2, ]$CW_Best[Zone, 2] == 1) |
                  (treebuf_df[treebuf_df$buf_id == 9, ]$CW_Best == 1) |
                  (treebuf_df[treebuf_df$buf_id == 10, ]$CW_Best[Zone, 10] == 1),
                ((1 + pars$CW_par$CW_DryFactRangeChange) * zone_df$CW_DryFactRangePower
                ) - zone_df$CW_DryFactRangePower,
                ifelse ((treebuf_df[treebuf_df$buf_id == 5, ]$CW_Best == 1) |
                          (treebuf_df[treebuf_df$buf_id == 6, ]$CW_Best == 1),
                        (
                          zone_df$CW_DryFactRangePower / (1 + pars$CW_par$CW_DryFactRangeChange)
                        ) - zone_df$CW_DryFactRangePower,
                        ifelse(
                          ((treebuf_df[treebuf_df$buf_id == 3, ]$CW_Best == 1) |
                             (treebuf_df[treebuf_df$buf_id == 4, ]$CW_Best[Zone, 4] == 1)) |
                            ((treebuf_df[treebuf_df$buf_id == 7, ]$CW_Best == 1) |
                               (treebuf_df[treebuf_df$buf_id == 8, ]$CW_Best == 1)),
                          zone_df$CW_DryFactRangePower - zone_df$CW_DryFactRangePower,
                          0
                        )
                )
        )
      )
      
      # CW_DryFactRangePower[Zone](t) = CW_DryFactRangePower[Zone](t - dt) + (CW_DryFactRangeUpdate[Zone]) * dt
      zone_df$CW_DryFactRangePower <- zone_df$CW_DryFactRangePower + (zone_df$CW_DryFactRangeUpdate)
      
      # Ca_CropSequen[Zone] = if Cq_Stage[Zone] >= 2 and Cq_StageAfterHarvest[Zone]= 0  and Cq_CropWeedSwitch[Zone] <> Cq_WeedType then 1 else if ((Cq_Stage[Zone] = 0) and( time > Ca_PlantTime[Zone] + 2)) then 1 else 0
      zone_df$Ca_CropSequen <- ifelse(
        zone_df$Cq_Stage >= 2 &
          zone_df$Cq_StageAfterHarvest == 0 &
          zone_df$Cq_CropWeedSwitch != pars$C_par$Cq_WeedType,
        1,
        ifelse ((zone_df$Cq_Stage = 0) &
                  (time > zone_df$Ca_PlantTime + 2), 1, 0)
      )
      
      # Ca_ComplCrop[Zone](t) = Ca_ComplCrop[Zone](t - dt) + (Ca_CropSequen[Zone]) * dt
      zone_df$Ca_ComplCrop <- zone_df$Ca_ComplCrop + (zone_df$Ca_CropSequen) 
      
      # PD_CFrugConst[Zone] = if Cq_CropType[Zone]=1 then PD_CFrugivory[Type1] else if Cq_CropType[Zone]=2 then PD_CFrugivory[Type2] else if Cq_CropType[Zone]=3 then PD_CFrugivory[Type3] else if Cq_CropType[Zone]=4 then PD_CFrugivory[Type4] else if Cq_CropType[Zone]=5 then PD_CFrugivory[Type5] else 0
      zone_df$PD_CFrugConst <- crop_df[ zone_df$Cq_CropType, "PD_CFrugivory"]

      # PD_CFrugImp[Zone,Animals] = PD_NastiesinPlot[Animals]*PD_CFrugivore?[Animals]*PD_CropsEaten?[Zone,Animals]
      zoneanimal_df$PD_NastiesinPlot <- rep(animal_df$PD_NastiesinPlot, each = nzone)
      zoneanimal_df$PD_CFrugivore_is <- rep(animal_df$PD_CFrugivore_is, each = nzone)
      zoneanimal_df$PD_CFrugImp <- zoneanimal_df$PD_NastiesinPlot * zoneanimal_df$PD_CFrugivore_is * zoneanimal_df$PD_CropsEaten_is
      
      # PD_CFrugiVFrac[Zone] = min(1,PD_CFrugConst[Zone]+AF_DynPestImpacts?*ARRAYSUM(PD_CFrugImp[Zone,*]))
      zone_df$PD_CFrugImp_sum <- aggregate(zoneanimal_df["PD_CFrugImp"], zoneanimal_df["zone"], sum)$PD_CFrugImp
      zone_df$PD_CFrugiVFrac <- pmin(1, zone_df$PD_CFrugConst + pars$AF_par$AF_DynPestImpacts_is*zone_df$PD_CFrugImp_sum)
      
      # C_FruitLoss[Zone,PlantComp] = if time = int(S&B_SlashTime[Sp1]) or time = int(S&B_SlashTime[Sp2]) or time = int(S&B_SlashTime[Sp3]) or time = int(Ca_PlantTime[Zone]) or S&B_Fire? = 1 or (Cq_WeedZn?[Zone]=1 and C_PlantDiesToday?[Zone]=1) then C_YieldCurr[Zone,PlantComp]/dt else PD_CFrugiVFrac[Zone]*C_YieldCurr[Zone,PlantComp]
      zonepcomp_df$SB_SlashTime_Sp1 <- tree_df[tree_df$tree_id == 1, ]$SB_SlashTime
      zonepcomp_df$SB_SlashTime_Sp2 <- tree_df[tree_df$tree_id == 2, ]$SB_SlashTime
      zonepcomp_df$SB_SlashTime_Sp3 <- tree_df[tree_df$tree_id == 3, ]$SB_SlashTime
      zonepcomp_df$SB_Fire_is <- SB_Fire_is
      zonepcomp_df$Cq_WeedZn_is <- rep(zone_df$Cq_WeedZn_is, nrow(pcomp_df))
      zonepcomp_df$PD_CFrugiVFrac <- rep(zone_df$PD_CFrugiVFrac, nrow(pcomp_df))
      
      zonepcomp_df$C_FruitLoss <- ifelse(
        time == floor(zonepcomp_df$SB_SlashTime_Sp1) |
          time == floor(zonepcomp_df$SB_SlashTime_Sp3) |
          time == floor(zonepcomp_df$SB_SlashTime_Sp3) |
          time == floor(zonepcomp_df$Ca_PlantTime) |
          zonepcomp_df$SB_Fire_is == 1 |
          (
            zonepcomp_df$Cq_WeedZn_is == 1 &
              zonepcomp_df$C_PlantDiesToday_is == 1
          ),
        zonepcomp_df$C_YieldCurr ,
        zonepcomp_df$PD_CFrugiVFrac * zonepcomp_df$C_YieldCurr
      )
      
      # C_ResidLast[Zone,PlantComp] = if C_PlantDiesToday?[Zone] = 1 then C_GroRes[Zone,PlantComp]/dt else 0
      zonepcomp_df$C_ResidLast <- ifelse( zonepcomp_df$C_PlantDiesToday_is == 1, zonepcomp_df$C_GroRes, 0)
      
      # S&B_FirMortSeedBank[Zone] = GRAPH(S&B_FireTempIncTopSoil[Zone])
      zone_df$SB_FirMortSeedBank <- get_SB_FirMortSeedBank(zone_df$SB_FireTempIncTopSoil)
      zonepcomp_df$SB_FirMortSeedBank <- rep(zone_df$SB_FirMortSeedBank, nrow(pcomp_df))
      
      # C_WeedSeedDecay[Zone,PlantComp] = min(1,C_DailyWeedSeedDecayFrac+S&B_FirMortSeedBank[Zone])*C_WeedSeedBank[Zone,PlantComp]
      zonepcomp_df$C_WeedSeedDecay <- pmin(1, pars$C_par$C_DailyWeedSeedDecayFrac+ zonepcomp_df$SB_FirMortSeedBank)* zonepcomp_df$C_WeedSeedBank
      # C_ResidMulch[Zone,PlantComp] = C_FruitLoss[Zone,PlantComp]+C_ResidLast[Zone,PlantComp]+C_StLeaveMulch[Zone,PlantComp]+C_WeedSeedDecay[Zone,PlantComp]
      zonepcomp_df$C_ResidMulch <- zonepcomp_df$C_FruitLoss+ zonepcomp_df$C_ResidLast + zonepcomp_df$C_StLeaveMulch+ zonepcomp_df$C_WeedSeedDecay
      
      # TF_MaleDWloss[Tree,Fruitbunch] = if TF_NewLeaf?[Tree] = 1 and TF_BunchGender[Tree,Fruitbunch] < 2 then TF_BunchWeight_kgpbunch[Tree,Fruitbunch]/dt else 0
      treefruit_df$TF_NewLeaf_is <- rep(tree_df$TF_NewLeaf_is, length(Fruitbunch))
      treefruit_df$TF_MaleDWloss <- ifelse( treefruit_df$TF_NewLeaf_is == 1 & treefruit_df$TF_BunchGender < 2, treefruit_df$TF_BunchWeight_kgpbunch, 0)
      
      # TF_PollinationEffectiveness = GRAPH(Rain)
      TF_PollinationEffectiveness <- get_TF_PollinationEffectiveness(Rain$Rain)
      
      # TF_FruitNumbLoss[Tree,Fruitbunch] = if TF_FruitWaterPot[Tree]>TF_CritFruitWaterPot[Tree] then TF_PollinationStage[Fruitbunch]*(1-TF_PollinationEffectiveness) else TF_PollinationStage[Fruitbunch]*(1-TF_PollinationEffectiveness)+TF_FruitsperBunch[Tree,Fruitbunch]*TF_WatStressAbortFrac[Tree]*TF_StageAbortSens[Tree,Fruitbunch]
      treefruit_df$TF_FruitWaterPot <- rep(tree_df$TF_FruitWaterPot, length(Fruitbunch))
      treefruit_df$TF_CritFruitWaterPot <- rep(tree_df$TF_CritFruitWaterPot, length(Fruitbunch))
      treefruit_df$TF_PollinationStage <- rep(fruit_df$TF_PollinationStage, each = ntree)
      treefruit_df$TF_WatStressAbortFrac <- rep(tree_df$TF_WatStressAbortFrac, length(Fruitbunch))
      
      treefruit_df$TF_FruitNumbLoss <- ifelse(
        treefruit_df$TF_FruitWaterPot > treefruit_df$TF_CritFruitWaterPot,
        treefruit_df$TF_PollinationStage *
          (1 - TF_PollinationEffectiveness),
        treefruit_df$TF_PollinationStage *
          (1 - TF_PollinationEffectiveness) + treefruit_df$TF_FruitsperBunch * treefruit_df$TF_WatStressAbortFrac *
          treefruit_df$TF_StageAbortSens
      )
      
      
      # TF_FruitAbDWLoss[Tree,Fruitbunch] = TF_BunchWeight_kgpbunch[Tree,Fruitbunch]*(TF_FruitNumbLoss[Tree,Fruitbunch]^TF_AbRelSizePow[Tree])
      treefruit_df$TF_AbRelSizePow <- rep(tree_df$TF_AbRelSizePow, length(Fruitbunch))
      treefruit_df$TF_FruitAbDWLoss <- treefruit_df$TF_BunchWeight_kgpbunch *
        (treefruit_df$TF_FruitNumbLoss^treefruit_df$TF_AbRelSizePow)
      
      # TF_FruitLitterFalloAm[Tree,Fruitbunch] = TF_MaleDWloss[Tree,Fruitbunch]+TF_FruitAbDWLoss[Tree,Fruitbunch]
      treefruit_df$TF_FruitLitterFalloAm <- treefruit_df$TF_MaleDWloss + treefruit_df$TF_FruitAbDWLoss
      
      # PD_TFrugImp[Tree,Animals] = PD_TFrugivore?[Animals]*PD_TEatenBy?[Tree,Animals]*PD_NastiesinPlot[Animals]
      treeanimal_df$PD_TFrugivore_is <- rep(animal_df$PD_TFrugivore_is, each = ntree)
      treeanimal_df$PD_TFrugImp <- treeanimal_df$PD_TFrugivore_is * treeanimal_df$PD_TEatenBy_is * treeanimal_df$PD_NastiesinPlot
      tree_df$PD_TFrugImp_sum <- aggregate(treeanimal_df["PD_TFrugImp"], treeanimal_df["tree_id"], sum)$PD_TFrugImp
      
      # PD_TFrugVFrac[Tree] = min(1,PD_TFrugiv&Abort[Tree]+AF_DynPestImpacts?*ARRAYSUM(PD_TFrugImp[Tree,*]))
      tree_df$PD_TFrugVFrac <- pmin(
        1,
        tree_df$PD_TFrugiv_Abort + pars$AF_par$AF_DynPestImpacts_is * tree_df$PD_TFrugImp_sum
      )
      
      # T_GenLitStage[Tree] = GRAPH(T_Stage[Tree,VegGen])
      tree_df$T_GenLitStage <- get_T_GenLitStage(treestage_df[treestage_df$Tree_Stage == "VegGen", ]$T_Stage)
      
      # T_Frug_and_Littfall[PlantComp,Tree] = if T_ApplyPalm?[Tree]=1 then arraysum(TF_FruitLitterFalloAm[Tree,*]) * T_TreesperHa[Tree]/10^4 else if T_DiesToday?[Tree] = 1 then T_Fruit[PlantComp,Tree]/dt else T_Fruit[PlantComp,Tree]*PD_TFrugVFrac[Tree]+T_GenLitFracMax[Tree]*T_GenLitStage[Tree]*T_Fruit[PlantComp,Tree]
      tree_df$TF_FruitLitterFalloAm_sum <- aggregate(treefruit_df["TF_FruitLitterFalloAm"], treefruit_df["tree_id"], sum)$TF_FruitLitterFalloAm
      treepcomp_df$TF_FruitLitterFalloAm_sum <- rep(tree_df$TF_FruitLitterFalloAm_sum, nrow(pcomp_df))
      treepcomp_df$PD_TFrugVFrac <- rep(tree_df$PD_TFrugVFrac, nrow(pcomp_df))
      treepcomp_df$T_GenLitFracMax <- rep(tree_df$T_GenLitFracMax, nrow(pcomp_df))
      treepcomp_df$T_GenLitStage <- rep(tree_df$T_GenLitStage, nrow(pcomp_df))
      
      treepcomp_df$T_Frug_and_Littfall <- ifelse(
        treepcomp_df$T_ApplyPalm_is == 1,
        treepcomp_df$TF_FruitLitterFalloAm_sum * treepcomp_df$T_Treesperha / 10^4,
        ifelse(
          treepcomp_df$T_DiesToday_is == 1,
          treepcomp_df$T_Fruit,
          treepcomp_df$T_Fruit * treepcomp_df$PD_TFrugVFrac + treepcomp_df$T_GenLitFracMax *
            treepcomp_df$T_GenLitStage * treepcomp_df$T_Fruit
        )
      )
      
      # TF_BunchWeightHarvest[Sp1,Ripe] = if TF_NewLeaf?[Sp1] = 1 and TF_BunchGender[Sp1,Ripe] = 2 then TF_BunchWeight_kgpbunch[Sp1,Ripe]/dt else 0
      # TF_BunchWeightHarvest[Sp1,Ripe_1] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_2] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_3] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_4] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_5] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_6] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_7] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_8] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_9] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_10] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_11] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Ripe_12] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Early_fruit] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Pollinated] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Anthesis] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Anthesis_1] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Anthesis_2] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Anthesis_3] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Anthesis_4] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Anthesis_5] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp1,Anthesis_6] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe] = if TF_NewLeaf?[Sp2] = 1 and TF_BunchGender[Sp2,Ripe] = 2 then TF_BunchWeight_kgpbunch[Sp2,Ripe]/dt else 0
      # TF_BunchWeightHarvest[Sp2,Ripe_1] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_2] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_3] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_4] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_5] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_6] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_7] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_8] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_9] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_10] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_11] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Ripe_12] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Early_fruit] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Pollinated] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Anthesis] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Anthesis_1] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Anthesis_2] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Anthesis_3] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Anthesis_4] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Anthesis_5] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp2,Anthesis_6] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe] = if TF_NewLeaf?[Sp3] = 1 and TF_BunchGender[Sp3,Ripe] = 2 then TF_BunchWeight_kgpbunch[Sp3,Ripe]/dt else 0
      # TF_BunchWeightHarvest[Sp3,Ripe_1] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_2] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_3] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_4] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_5] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_6] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_7] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_8] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_9] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_10] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_11] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Ripe_12] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Early_fruit] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Pollinated] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Anthesis] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Anthesis_1] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Anthesis_2] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Anthesis_3] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Anthesis_4] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Anthesis_5] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      # TF_BunchWeightHarvest[Sp3,Anthesis_6] = 0*(TF_NewLeaf?[Sp1]+TF_BunchWeight_kgpbunch[Sp1,Ripe]+TF_BunchGender[Sp1,Ripe])
      
      treefruit_df$TF_BunchWeightHarvest <- 0
      tf_Ripe <- treefruit_df[treefruit_df$Fruitbunch == "Ripe",]
      treefruit_df[treefruit_df$Fruitbunch == "Ripe",]$TF_BunchWeightHarvest <- ifelse( tree_df$TF_NewLeaf_is == 1 & tf_Ripe$TF_BunchGender == 2, tf_Ripe$TF_BunchWeight_kgpbunch, 0)
      
      # T_RipeFruitHarvest[PlantComp,Tree] = if T_ApplyPalm?[Tree] then TF_BunchWeightHarvest[Tree,Ripe] * T_TreesperHa[Tree]/10^4 else if (T_Stage[Tree,VegGen]>=2)then(T_Fruit[PlantComp,Tree])  else(0)
      tree_df$TF_BunchWeightHarvest_ripe <- treefruit_df[treefruit_df$Fruitbunch == "Ripe", ]$TF_BunchWeightHarvest
      treepcomp_df$TF_BunchWeightHarvest_ripe <- rep(tree_df$TF_BunchWeightHarvest_ripe, nrow(pcomp_df))
      treepcomp_df$T_Stage_VegGen <- rep(tree_df$T_Stage_VegGen, nrow(pcomp_df))
      treepcomp_df$T_RipeFruitHarvest <- ifelse(
        treepcomp_df$T_ApplyPalm_is == 1,
        treepcomp_df$TF_BunchWeightHarvest_ripe * treepcomp_df$T_Treesperha / 10^4,
        ifelse (treepcomp_df$T_Stage_VegGen >=
                  2, treepcomp_df$T_Fruit, 0)
      )
      
      # T_PrunFruit[PlantComp,Tree] = if T_Prun[DW, Tree]> 0 then T_PrunFrac[Tree]*T_Fruit[PlantComp,Tree]/dt  else 0
      treepcomp_df$T_PrunFrac <- rep(tree_df$T_PrunFrac, nrow(pcomp_df))
      treepcomp_df$T_PrunFruit <- ifelse(treepcomp_df$T_Prun_DW > 0,
                                         treepcomp_df$T_PrunFrac * treepcomp_df$T_Fruit,
                                         0)
      
      # T_PrunHarvFracD[Sp1] = GRAPH(T_PrunPast)
      tree_df$T_PrunHarvFracD <- get_T_PrunHarvFracD(pars$T_par$T_PrunPast)
      
      # T_PrunHarvInc[PlantComp,Tree] = (1-T_PrunType?)* T_PrunHarvFracC[Tree]*(T_Prun[PlantComp,Tree] + T_PrunFruit[PlantComp,Tree]) + T_PrunType?*T_PrunHarvFracD[Tree] *(T_Prun[PlantComp,Tree]+T_PrunFruit[PlantComp,Tree])
      treepcomp_df$T_PrunHarvFracC <- rep(tree_df$T_PrunHarvFracC, nrow(pcomp_df))
      treepcomp_df$T_PrunHarvFracD <- rep(tree_df$T_PrunHarvFracD, nrow(pcomp_df))
      
      treepcomp_df$T_PrunHarvInc <-  (1 - T_PrunType_is) * treepcomp_df$T_PrunHarvFracC *
        (treepcomp_df$T_Prun + treepcomp_df$T_PrunFruit) +
        T_PrunType_is * treepcomp_df$T_PrunHarvFracD * (treepcomp_df$T_Prun +
                                                          treepcomp_df$T_PrunFruit)
      
      # T_PrunMulch[PlantComp,Tree] = T_Herbivory[PlantComp,Tree]+ T_Frug_and_Littfall[PlantComp,Tree]  + (1 - T_FruitHarvFrac[Tree]) * T_RipeFruitHarvest[PlantComp,Tree] + T_Prun[PlantComp,Tree] + T_PrunFruit[PlantComp,Tree] - T_PrunHarvInc[PlantComp,Tree]
      treepcomp_df$T_FruitHarvFrac <- rep(tree_df$T_FruitHarvFrac, nrow(pcomp_df))
      
      treepcomp_df$T_PrunMulch <- treepcomp_df$T_Herbivory + treepcomp_df$T_Frug_and_Littfall  +
        (1 - treepcomp_df$T_FruitHarvFrac) * treepcomp_df$T_RipeFruitHarvest +
        treepcomp_df$T_Prun + treepcomp_df$T_PrunFruit - treepcomp_df$T_PrunHarvInc
      
      # S&B_DeadWoodLitTransfer[Zone,PlantComp] = S&B_DailyDeadWoodLitTransfer*S&B_DeadWood[Zone,PlantComp]
      zonepcomp_df$SB_DeadWoodLitTransfer <- pars$SB_par$SB_DailyDeadWoodLitTransfer* zonepcomp_df$SB_DeadWood
      
      # Mc_LitterAmount[Zone] = C_ResidMulch[Zone,DW]+(T_LifallInc[DW,Sp1]*T_LifallWtAct[Zone,Sp1]+T_LifallInc[DW,Sp2]*T_LifallWtAct[Zone,Sp2]+T_LifallInc[DW,Sp3]*T_LifallWtAct[Zone,Sp3])+(T_PrunMulch[DW,Sp1]*T_PrunWtAct[Zone,Sp1]+T_PrunMulch[DW,Sp2]*T_PrunWtAct[Zone,Sp2]+T_PrunMulch[DW,Sp3]*T_PrunWtAct[Zone,Sp3])+S&B_NecromLitTransf[Zone,DW]+S&B_DeadWoodLitTransfer[Zone,DW]/1000
      tp_DW <- treepcomp_df[treepcomp_df$PlantComp == "DW", c("T_LifallInc", "T_PrunMulch")]
      tp_DW <- tp_DW[rep(seq_len(nrow(tp_DW)), each = nzone), ]
      zonetree_df[c("T_LifallInc_DW", "T_PrunMulch_DW")] <- tp_DW
      zonetree_df$T_LifallInc_DW_WtAct <- zonetree_df$T_LifallInc_DW * zonetree_df$T_LifallWtAct
      zonetree_df$T_PrunMulch_DW_WtAct <- zonetree_df$T_PrunMulch_DW * zonetree_df$T_PrunWtAct
      sum_df <- aggregate(zonetree_df[c("T_LifallInc_DW_WtAct", "T_PrunMulch_DW_WtAct")], zonetree_df["zone"], sum)
      zp_DW <- zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]
      zone_df$Mc_LitterAmount <- zp_DW$C_ResidMulch + sum_df$T_LifallInc_DW_WtAct +
        sum_df$T_PrunMulch_DW_WtAct + zp_DW$SB_NecromLitTransf + zp_DW$SB_DeadWoodLitTransfer /
        1000
      
      # Ca_FertAppDoY = GRAPH(Ca_PastFertApp)
      Ca_FertAppDoY <- get_y_df(pars$C_par$Ca_PastFertApp, "Ca_FertAppDoY")
      
      # Ca_FertAppYear = GRAPH(Ca_PastFertApp)
      Ca_FertAppYear <- get_y_df(pars$C_par$Ca_PastFertApp, "Ca_FertAppYear")
      
      # Ca_FertAppTime = Ca_FertAppDoY + 365*Ca_FertAppYear-Ca_DOYStart
      Ca_FertAppTime <- Ca_FertAppDoY + 365*Ca_FertAppYear-Ca_DOYStart
      
      # Ca_FertApp = if time = int(Ca_FertAppTime) then 1 else 0
      Ca_FertApp <- ifelse( time == floor(Ca_FertAppTime), 1 ,0)

      # Ca_ExtOrgApply?[1] = GRAPH(Ca_PastFertApp)
      # Ca_ExtOrgApply?[2] = GRAPH(Ca_PastFertApp)
      inp_df$Ca_ExtOrgApply_is <- get_y_df(pars$C_par$Ca_PastFertApp, "Ca_ExtOrgApply_is")
      
      # Ca_FertAmount[Zn1] = GRAPH(Ca_PastFertApp)
      # Ca_FertAmount[Zn2] = GRAPH(Ca_PastFertApp)
      # Ca_FertAmount[Zn3] = GRAPH(Ca_PastFertApp)
      # Ca_FertAmount[Zn4] = GRAPH(Ca_PastFertApp)
      zone_df$Ca_FertAmount <- get_y_df(pars$C_par$Ca_PastFertApp, "Ca_FertAmount")
      
      # Ca_ExtOrgAppZn[ExtOrgInputs,Zone] = if Ca_FertApp = 1 then Ca_ExtOrgApply?[ExtOrgInputs]*Ca_FertAmount[Zone] else 0
      zoneinp_df$Ca_FertApp <- Ca_FertApp
      zoneinp_df$Ca_ExtOrgApply_is <- rep(inp_df$Ca_ExtOrgApply_is, each = nzone)
      zoneinp_df$Ca_FertAmount <- rep(zone_df$Ca_FertAmount, nrow(inp_df))
      
      zoneinp_df$Ca_ExtOrgAppZn <- ifelse( zoneinp_df$Ca_FertApp == 1, zoneinp_df$Ca_ExtOrgApply_is * zoneinp_df$Ca_FertAmount, 0)
      
      # Mc_LitterC[Zone] = Mc_Carbon*1000*Mc_LitterAmount[Zone]+Ca_ExtOrgAppZn[1,Zone]*Mc_CExtOrg[1]+Ca_ExtOrgAppZn[2,Zone]*Mc_CExtOrg[2]
      zoneinp_df$Ca_ExtOrgAppZn_x <-  zoneinp_df$Ca_ExtOrgAppZn * rep(inp_df$Mc_CExtOrg, each = nzone)
      zone_df$Ca_ExtOrgAppZn_sum <- aggregate(zoneinp_df["Ca_ExtOrgAppZn_x"], zoneinp_df["zone"], sum)$Ca_ExtOrgAppZn_x
      zone_df$Mc_LitterC <- pars$Mc_par$Mc_Carbon*1000*zone_df$Mc_LitterAmount+ zone_df$Ca_ExtOrgAppZn_sum
      
      # T_NLifallInc[N,Sp1] = T_LifallInc[N,Sp1]
      # T_NLifallInc[N,Sp2] = T_LifallInc[N,Sp2]
      # T_NLifallInc[N,Sp3] = T_LifallInc[N,Sp3]
      # T_NLifallInc[P,Sp1] = T_LifallInc[P,Sp1]
      # T_NLifallInc[P,Sp2] = T_LifallInc[P,Sp2]
      # T_NLifallInc[P,Sp3] = T_LifallInc[P,Sp3]
      treenut_df$T_NLifallInc <- treepcomp_df[treepcomp_df$PlantComp %in% c("N", "P"),]$T_LifallInc

      # T_NPrunMulch[N,Sp1] = T_PrunMulch[N,Sp1]
      # T_NPrunMulch[N,Sp2] = T_PrunMulch[N,Sp2]
      # T_NPrunMulch[N,Sp3] = T_PrunMulch[N,Sp3]
      # T_NPrunMulch[P,Sp1] = T_PrunMulch[P,Sp1]
      # T_NPrunMulch[P,Sp2] = T_PrunMulch[P,Sp2]
      # T_NPrunMulch[P,Sp3] = T_PrunMulch[P,Sp3]
      treenut_df$T_NPrunMulch <- treepcomp_df[treepcomp_df$PlantComp %in% c("N", "P"),]$T_PrunMulch

      # C_NResidMulch[Zn1,N] = C_ResidMulch[Zn1,N]
      # C_NResidMulch[Zn1,P] = C_ResidMulch[Zn1,P]
      # C_NResidMulch[Zn2,N] = C_ResidMulch[Zn2,N]
      # C_NResidMulch[Zn2,P] = C_ResidMulch[Zn2,P]
      # C_NResidMulch[Zn3,N] = C_ResidMulch[Zn3,N]
      # C_NResidMulch[Zn3,P] = C_ResidMulch[Zn3,P]
      # C_NResidMulch[Zn4,N] = C_ResidMulch[Zn4,N]
      # C_NResidMulch[Zn4,P] = C_ResidMulch[Zn4,P]
      zonenut_df$C_NResidMulch <- zonepcomp_df[zonepcomp_df$PlantComp %in% c("N", "P"), ]$C_ResidMulch
      
      # C_NNecroLitF[Zn1,N] = S&B_NecromLitTransf[Zn1,N]+S&B_DeadWoodLitTransfer[Zn1,N]
      # C_NNecroLitF[Zn1,P] = S&B_NecromLitTransf[Zn1,P]+S&B_DeadWoodLitTransfer[Zn1,P]
      # C_NNecroLitF[Zn2,N] = S&B_NecromLitTransf[Zn2,N]+S&B_DeadWoodLitTransfer[Zn2,N]
      # C_NNecroLitF[Zn2,P] = S&B_NecromLitTransf[Zn2,P]+S&B_DeadWoodLitTransfer[Zn2,P]
      # C_NNecroLitF[Zn3,N] = S&B_NecromLitTransf[Zn3,N]+S&B_DeadWoodLitTransfer[Zn3,N]
      # C_NNecroLitF[Zn3,P] = S&B_NecromLitTransf[Zn3,P]+S&B_DeadWoodLitTransfer[Zn3,P]
      # C_NNecroLitF[Zn4,N] = S&B_NecromLitTransf[Zn4,N]+S&B_DeadWoodLitTransfer[Zn4,N]
      # C_NNecroLitF[Zn4,P] = S&B_NecromLitTransf[Zn4,P]+S&B_DeadWoodLitTransfer[Zn4,P]
      zonenut_df$C_NNecroLitF <- zonepcomp_df[zonepcomp_df$PlantComp %in% c("N", "P"), ]$SB_NecromLitTransf + zonepcomp_df[zonepcomp_df$PlantComp %in% c("N", "P"), ]$SB_DeadWoodLitTransfer
      
      # Mn_LitInpN[Zone,SlNut] = C_NResidMulch[Zone,SlNut]+T_NLifallInc[SlNut,Sp1]*T_LifallWtAct[Zone,Sp1]+T_NLifallInc[SlNut,Sp2]*T_LifallWtAct[Zone,Sp2]+T_NLifallInc[SlNut,Sp3]*T_LifallWtAct[Zone,Sp3]
      # + T_NPrunMulch[SlNut,Sp1]*T_PrunWtAct[Zone,Sp1]+ T_NPrunMulch[SlNut,Sp2]*T_PrunWtAct[Zone,Sp2]+ T_NPrunMulch[SlNut,Sp3]*T_PrunWtAct[Zone,Sp3]+ C_NNecroLitF[Zone,SlNut]
      tn_col <- c("T_NLifallInc", "T_NPrunMulch")
      zonetreenut_df[tn_col] <- treenut_df[tn_col][rep(seq_len(nrow(treenut_df)), each = nzone), ]
      zt_col <- c("T_LifallWtAct", "T_PrunWtAct")
      zonetreenut_df[zt_col] <- zonetree_df[zt_col][rep(seq_len(nrow(zonetree_df)), nrow(nut_df)), ]
      zonetreenut_df$T_NLifallInc_Act <- zonetreenut_df$T_NLifallInc * zonetreenut_df$T_LifallWtAct
      zonetreenut_df$T_NPrunMulch_Act <- zonetreenut_df$T_NPrunMulch * zonetreenut_df$T_PrunWtAct
      sum_col <- c("T_NLifallInc_Act", "T_NPrunMulch_Act")
      zonenut_df[c("T_NLifallInc_Act_sum", "T_NPrunMulch_Act_sum")] <- aggregate(zonetreenut_df[sum_col], zonetreenut_df[c("zone", "SlNut")], sum)[sum_col]
      zonenut_df$Mn_LitInpN <- zonenut_df$C_NResidMulch + zonenut_df$T_NLifallInc_Act_sum + zonenut_df$T_NPrunMulch_Act_sum + zonenut_df$C_NNecroLitF
      
      # Mn_LitterNConc[Zone,SlNut] = IF(Mc_LitterAmount[Zone]>0 OR Ca_ExtOrgAppZn[1,Zone]>0 OR Ca_ExtOrgAppZn[2,Zone]>0)THEN(Mn_LitInpN[Zone,SlNut]/(Mc_LitterAmount[Zone]*1000+Ca_ExtOrgAppZn[1,Zone]+Ca_ExtOrgAppZn[2,Zone]))ELSE(0)
      zonenut_df$Mc_LitterAmount <- rep(zone_df$Mc_LitterAmount, nrow(nut_df))
      zone_df$Ca_ExtOrgAppZn_inp1 <- zoneinp_df[zoneinp_df$ExtOrgInputs == 1, ]$Ca_ExtOrgAppZn
      zone_df$Ca_ExtOrgAppZn_inp2 <- zoneinp_df[zoneinp_df$ExtOrgInputs == 2, ]$Ca_ExtOrgAppZn
      zonenut_df$Ca_ExtOrgAppZn_inp1 <- rep(zone_df$Ca_ExtOrgAppZn_inp1, nrow(nut_df))
      zonenut_df$Ca_ExtOrgAppZn_inp2 <- rep(zone_df$Ca_ExtOrgAppZn_inp2, nrow(nut_df))
      
      zonenut_df$Mn_LitterNConc <- ifelse(
        zonenut_df$Mc_LitterAmount > 0 | zonenut_df$Ca_ExtOrgAppZn_inp1 > 0 |
          zonenut_df$Ca_ExtOrgAppZn_inp2 >
          0,
        zonenut_df$Mn_LitInpN / (
          zonenut_df$Mc_LitterAmount * 1000 + zonenut_df$Ca_ExtOrgAppZn_inp1 + zonenut_df$Ca_ExtOrgAppZn_inp2
        ),
        0
      )
      
      # Mc_LitterLignPolyp[Zone] = IF(Mc_LitterAmount[Zone]>0 or Ca_ExtOrgAppZn[1,Zone]>0 or Ca_ExtOrgAppZn[2,Zone]>0)
      # THEN((Ca_ExtOrgAppZn[1,Zone]*(Mc_LignExtOrg[1]+Mc_PolypExtOrg[1])+Ca_ExtOrgAppZn[2,Zone]*(Mc_LignExtOrg[2]+Mc_PolypExtOrg[2])+(Cq_LignResidCurr[Zone]+Cq_PolyResid[Zone])*C_ResidMulch[Zone,DW]+((T_LignLifall[Sp1]+T_PolypLifall[Sp1])*T_LifallInc[DW,Sp1]*T_LifallWtAct[Zone,Sp1]+(T_LignLifall[Sp2]+T_PolypLifall[Sp2])*T_LifallInc[DW,Sp2]*T_LifallWtAct[Zone,Sp2]+(T_LignLifall[Sp3]+T_PolypLifall[Sp3])*T_LifallInc[DW,Sp3]*T_LifallWtAct[Zone,Sp3])+((T_LignPrun[Sp1]+T_PolypPrun[Sp1])*T_PrunMulch[DW,Sp1]*T_PrunWtAct[Zone,Sp1]+(T_LignPrun[Sp2]+T_PolypPrun[Sp2])*T_PrunMulch[DW,Sp2]*T_PrunWtAct[Zone,Sp2]+(T_LignPrun[Sp3]+T_PolypPrun[Sp3])*T_PrunMulch[DW,Sp3]*T_PrunWtAct[Zone,Sp3]))/(Mc_LitterAmount[Zone]+Ca_ExtOrgAppZn[1,Zone]+Ca_ExtOrgAppZn[2,Zone]))ELSE(0)
      tp_DW <- treepcomp_df[treepcomp_df$PlantComp == "DW",]
      inp_df$Mc_Lign_Polyp <- inp_df$Mc_LignExtOrg + inp_df$Mc_PolypExtOrg
      zoneinp_df$Ca_ExtOrgAppZn_Lign_Polyp <- zoneinp_df$Ca_ExtOrgAppZn * rep(inp_df$Mc_Lign_Polyp, each = nzone )
      tree_df$T_Lign_Polyp_LifallInc <- (tree_df$T_LignLifall + tree_df$T_PolypLifall) * tp_DW$T_LifallInc
      tree_df$T_Lign_Polyp_PrunMulch <- (tree_df$T_LignPrun + tree_df$T_PolypPrun) * tp_DW$T_PrunMulch
      zonetree_df$T_LifallWtAct_xLifallInc <- zonetree_df$T_LifallWtAct * rep(tree_df$T_Lign_Polyp_LifallInc, each = nzone)
      zonetree_df$T_PrunWtAct_xPrunMulch <- zonetree_df$T_PrunWtAct * rep(tree_df$T_Lign_Polyp_PrunMulch, each = nzone)
      
      zone_df$Mc_LitterLignPolyp <- ifelse(
        zone_df$Mc_LitterAmount > 0 |
          zone_df$Ca_ExtOrgAppZn_inp1 > 0 |
          zone_df$Ca_ExtOrgAppZn_inp2 > 0,
        (
          aggregate(zoneinp_df["Ca_ExtOrgAppZn_Lign_Polyp"], zoneinp_df["zone"], sum)$Ca_ExtOrgAppZn_Lign_Polyp +
            (zone_df$Cq_LignResidCurr + zone_df$Cq_PolyResid) * zonepcomp_df[zonepcomp_df$PlantComp == "DW", ]$C_ResidMulch +
            aggregate(zonetree_df["T_LifallWtAct_xLifallInc"], zonetree_df["zone"], sum)$T_LifallWtAct_xLifallInc +
            aggregate(zonetree_df["T_PrunWtAct_xPrunMulch"], zonetree_df["zone"], sum)$T_PrunWtAct_xPrunMulch
        ) / (
          zone_df$Mc_LitterAmount + zone_df$Ca_ExtOrgAppZn_inp1 + zone_df$Ca_ExtOrgAppZn_inp2
        ),
        0
      )
      

      # Mc_SplitMet[Zone] = IF(Mn_LitterNConc[Zone,N]>0)THEN MAX((0.85 - 0.018*Mc_LitterLignPolyp[Zone]/Mn_LitterNConc[Zone,N]),0)ELSE(0)
      zn_N <- zonenut_df[zonenut_df$SlNut == "N",]
      zone_df$Mc_SplitMet <- ifelse(zn_N$Mn_LitterNConc>0, pmax((0.85 - 0.018*zone_df$Mc_LitterLignPolyp/zn_N$Mn_LitterNConc),0), 0)
      
      # Mc_MetabIn[Zone] = Mc_LitterC[Zone]*Mc_SplitMet[Zone]
      zone_df$Mc_MetabIn <- zone_df$Mc_LitterC * zone_df$Mc_SplitMet
      
      # Mc_StrucIn[Zone] = Mc_LitterC[Zone]-Mc_MetabIn[Zone]
      zone_df$Mc_StrucIn <- zone_df$Mc_LitterC - zone_df$Mc_MetabIn
      
      # Mc_StrucLig[Zone] = Mc_LitterLignPolyp[Zone]/(1-Mc_SplitMet[Zone])
      zone_df$Mc_StrucLig <- zone_df$Mc_LitterLignPolyp/(1-zone_df$Mc_SplitMet)
      
      # Mc_LitTemp[Zone] = Temp[Zone,1]
      zone_df$Mc_LitTemp <- zonelayer_df[zonelayer_df$layer == 1,]$Temp
      
      # Mc_LitTempLim[Zone] = GRAPH(Mc_LitTemp[Zone])
      zone_df$Mc_LitTempLim <- get_y_df(zone_df$Mc_LitTemp, "Mc_LitTempLim")
      
      # Mc_ThetaLitter[Zone] = W_Theta1[Zone]
      zone_df$Mc_ThetaLitter <- zonelayer_df[zonelayer_df$layer == 1,]$W_Theta
      
      # W_ThetaPMax[Zone] = GRAPH(W_PMax)
      zone_df$W_ThetaPMax <- get_y_df(pars$W_par$W_PMax, "W_ThetaPMax")
      
      # Mc_ThetaLim[Zone] = MAX(0,MIN(MIN(MAX(0,3.33*Mc_ThetaLitter[Zone]/W_ThetaPMax[Zone])-0.667,1),-1.2*Mc_ThetaLitter[Zone]/W_ThetaPMax[Zone]+1.9))
      zone_df$Mc_ThetaLim <- pmax(0, pmin(pmin(pmax(0,3.33* zone_df$Mc_ThetaLitter/zone_df$W_ThetaPMax)-0.667,1),-1.2*zone_df$Mc_ThetaLitter/zone_df$W_ThetaPMax+1.9))
      
      # Mc_kStrucCur[Zone] = Mc_RelKStrucLit*Mc2_kStruc*EXP(-3.0*Mc_StrucLig[Zone])*Mc_LitTempLim[Zone]*Mc_ThetaLim[Zone]
      zone_df$Mc_kStrucCur <- pars$Mc_par$Mc_RelKStrucLit* pars$Mc_par$Mc2_kStruc*exp(-3.0*zone_df$Mc_StrucLig)*zone_df$Mc_LitTempLim*zone_df$Mc_ThetaLim
      
      # S&B_SurfLitBurnFrac[Zone] = GRAPH(S&B_FireTempIncSurf[Zone])
      zone_df$SB_SurfLitBurnFrac <- get_y_df(zone_df$SB_FireTempIncSurf, "SB_SurfLitBurnFrac")
      
      # Mc_StrucActCO2In[Zone] = Mc_Struc[Zone]*((1-Mc_StrucLig[Zone])*Mc_kStrucCur[Zone]*(1-Mc_EffStrucAct)+S&B_SurfLitBurnFrac[Zone])
      zone_df$Mc_StrucActCO2In <- zone_df$Mc_Struc*((1-zone_df$Mc_StrucLig)*zone_df$Mc_kStrucCur*(1- pars$Mc_par$Mc_EffStrucAct)+zone_df$SB_SurfLitBurnFrac)
      
      # Mc_StrucSlwCO2In[Zone] = (Mc_Struc[Zone]-Mc_StrucActCO2In[Zone])*Mc_StrucLig[Zone]*Mc_kStrucCur[Zone]*(1-Mc_EffStrucSlw)
      zone_df$Mc_StrucSlwCO2In <- (zone_df$Mc_Struc- zone_df$Mc_StrucActCO2In)* zone_df$Mc_StrucLig* zone_df$Mc_kStrucCur*(1- pars$Mc_par$Mc_EffStrucSlw)
      
      # Mn_CNActAct[Zone,SlNut] = IF(TIME>1)THEN (if Mn_Act[Zone,SlNut]>0 then (Mc_Act[Zone]/Mn_Act[Zone,SlNut])  else 100)  ELSE(Mn_CNAct*Mn_NutRatAct[SlNut])
      zonenut_df$Mc_Act <- rep(zone_df$Mc_Act, nrow(nut_df))
      zonenut_df$Mn_NutRatAct <- rep(nut_df$Mn_NutRatAct, each = nzone)
      zonenut_df$Mn_CNActAct <- ifelse(
        time > 1,
        ifelse(
          zonenut_df$Mn_Act > 0,
          zonenut_df$Mc_Act / zonenut_df$Mn_Act,
          100
        ),
        pars$Mn_par$Mn_CNAct * zonenut_df$Mn_NutRatAct
      )
      
      # Mc_ActDecomDec[Zone,SlNut] = MAX(0,1+MIN(0,-2*(Mn_CNActAct[Zone,SlNut]-Mn_CNAct*Mn_NutRatAct[SlNut])/Mn_CNAct*Mn_NutRatAct[SlNut]-0.25))
      zonenut_df$Mc_ActDecomDec <- pmax(0,
                                        1 + pmin(
                                          0,
                                          -2 * (
                                            zonenut_df$Mn_CNActAct - pars$Mn_par$Mn_CNAct * zonenut_df$Mn_NutRatAct
                                          ) / pars$Mn_par$Mn_CNAct * zonenut_df$Mn_NutRatAct - 0.25
                                        ))
      
      # Mc_ActDecomDecMin[Zone] = MIN(Mc_ActDecomDec[Zone,N],Mc_ActDecomDec[Zone,P])
      zone_df$Mc_ActDecomDecMin <- pmin(zonenut_df[zonenut_df$SlNut == "N", ]$Mc_ActDecomDec, zonenut_df[zonenut_df$SlNut == "P", ]$Mc_ActDecomDec)
      
      # Mc_StrucActF[Zone] = (Mc_Struc[Zone]-Mc_StrucActCO2In[Zone]-Mc_StrucSlwCO2In[Zone])*(1-Mc_StrucLig[Zone])*Mc_kStrucCur[Zone]*Mc_EffStrucAct*Mc_ActDecomDecMin[Zone]
      zone_df$Mc_StrucActF <- (zone_df$Mc_Struc - zone_df$Mc_StrucActCO2In -
                                 zone_df$Mc_StrucSlwCO2In) * (1 - zone_df$Mc_StrucLig) * zone_df$Mc_kStrucCur *
        pars$Mc_par$Mc_EffStrucAct * zone_df$Mc_ActDecomDecMin
      
      # Mn_CNSlwAct[Zone,SlNut] = IF(TIME>1)THEN(if Mn_Slw[Zone,SlNut]>0 then Mc_Slw[Zone]/Mn_Slw[Zone,SlNut] else 0)ELSE(Mn_CNSlw*Mn_NutRatSlw[SlNut])
      zonenut_df$Mc_Slw <- rep(zone_df$Mc_Slw, nrow(nut_df))
      zonenut_df$Mn_CNSlwAct <- ifelse(
        time > 1,
        ifelse(zonenut_df$Mn_Slw > 0, zonenut_df$Mc_Slw / zonenut_df$Mn_Slw, 0),
        pars$Mn_par$Mn_CNSlw * zonenut_df$Mn_NutRatSlw
      )
      
      # Mc_SlwDecomDecMin[Zone,SlNut] = MAX(0,1+MIN(0,-2*(Mn_CNSlwAct[Zone,SlNut]-Mn_CNSlw*Mn_NutRatSlw[SlNut])/Mn_CNSlw*Mn_NutRatSlw[SlNut]-0.25))
      zonenut_df$Mc_SlwDecomDecMin <- pmax(0,
                                           1 + pmin(
                                             0,
                                             -2 * (
                                               zonenut_df$Mn_CNSlwAct - pars$Mn_par$Mn_CNSlw * zonenut_df$Mn_NutRatSlw
                                             ) / pars$Mn_par$Mn_CNSlw * zonenut_df$Mn_NutRatSlw - 0.25
                                           ))
      
      # Mc_SlwDecomDec[Zone] = MIN(Mc_SlwDecomDecMin[Zone,N],Mc_SlwDecomDecMin[Zone,P])
      zone_df$Mc_SlwDecomDec <- pmin(zonenut_df[zonenut_df$SlNut == "N", ]$Mc_SlwDecomDecMin,
                                     zonenut_df[zonenut_df$SlNut == "P", ]$Mc_SlwDecomDecMin)
      
      # Mc_StructSlwF_act[Zone] = Mc_StrucLig[Zone]*Mc_kStrucCur[Zone]*Mc_EffStrucSlw*Mc_SlwDecomDec[Zone]
      zone_df$Mc_StructSlwF_act <- zone_df$Mc_StrucLig * zone_df$Mc_kStrucCur * pars$Mc_par$Mc_EffStrucSlw *
        zone_df$Mc_SlwDecomDec
      
      # Mc_StrucSlwF[Zone] = (Mc_Struc[Zone]-Mc_StrucActCO2In[Zone]-Mc_StrucSlwCO2In[Zone]-Mc_StrucActF[Zone])*Mc_StructSlwF_act[Zone]
      zone_df$Mc_StrucSlwF <- (
        zone_df$Mc_Struc - zone_df$Mc_StrucActCO2In - zone_df$Mc_StrucSlwCO2In - zone_df$Mc_StrucActF
      ) * zone_df$Mc_StructSlwF_act
      
      # E_PloughDoY = GRAPH(E_PloughPast)
      E_PloughDoY <- get_y_df(pars$E_par$E_PloughPast, "E_PloughDoY")
      
      # E_PloughY = GRAPH(E_PloughPast)
      E_PloughY <- get_y_df(pars$E_par$E_PloughPast, "E_PloughY")
      
      # E_PloughTimeCal = E_PloughDoY+365*(E_PloughY)-Ca_DOYStart
      E_PloughTimeCal <- E_PloughDoY + 365 * (E_PloughY) - Ca_DOYStart
      
      # E_PloughTime?[Zone] = if time = int(E_PloughTimeCal) or (time = int(Ca_PlantTime[Zone]-E_IntvPloughPlant) and E_PloughBefPlant? = 1) then 1 else 0
      zone_df$E_PloughTime_is <- ifelse(
        time == floor(E_PloughTimeCal) |
          (
            time == floor(zone_df$Ca_PlantTime - pars$E_par$E_IntvPloughPlant) &
              pars$E_par$E_PloughBefPlant_is == 1
          ),
        1,
        0
      )
      
      # MC2_TillageEvent[Zone] = if E_PloughTime?[Zone] = 1 then E_TillZone?[Zone] else 0
      zone_df$Mc2_TillageEvent <- ifelse(zone_df$E_PloughTime_is == 1, zone_df$E_TillZone_is, 0)
      
      # MC2_LitSomTrans[Zone,Cent_Pools] = min(1,MC2_RainTransfer[Cent_Pools]*Rain_Infiltr[Zone]+S_LFoodForWorms[Zone]*MC2_WormTransfer[Cent_Pools]+MC2_TillageEvent[Zone]*MC2_SoilTillTransfer[Cent_Pools])
      zonecpools_df$Mc2_RainTransfer <- rep(cpools_df$Mc2_RainTransfer, each = nzone)
      zonecpools_df$Rain_Infiltr <- rep(zone_df$Rain_Infiltr, nrow(cpools_df))
      zonecpools_df$S_LFoodForWorms <- rep(zone_df$S_LFoodForWorms, nrow(cpools_df))
      zonecpools_df$Mc2_WormTransfer <- rep(cpools_df$Mc2_WormTransfer, each = nzone)
      zonecpools_df$Mc2_TillageEvent <- rep(zone_df$Mc2_TillageEvent, nrow(cpools_df))
      zonecpools_df$Mc2_SoilTillTransfer <- rep(cpools_df$Mc2_SoilTillTransfer, each = nzone)
      
      zonecpools_df$Mc2_LitSomTrans <- pmin(
        1,
        zonecpools_df$Mc2_RainTransfer * zonecpools_df$Rain_Infiltr +
          zonecpools_df$S_LFoodForWorms * zonecpools_df$Mc2_WormTransfer + zonecpools_df$Mc2_TillageEvent *
          zonecpools_df$Mc2_SoilTillTransfer
      )
      
      # Mc_Str_LitSomTrans[Zone] = MC2_LitSomTrans[Zone,Str]*(Mc_Struc[Zone]+Mc_StrucIn[Zone]-Mc_StrucActCO2In[Zone]-Mc_StrucSlwCO2In[Zone]-Mc_StrucActF[Zone]-Mc_StrucSlwF[Zone])
      zone_df$Mc_Str_LitSomTrans <- zonecpools_df[zonecpools_df$Cent_Pools == "Str", ]$Mc2_LitSomTrans *
        (
          zone_df$Mc_Struc + zone_df$Mc_StrucIn - zone_df$Mc_StrucActCO2In - zone_df$Mc_StrucSlwCO2In -
            zone_df$Mc_StrucActF - zone_df$Mc_StrucSlwF
        )

      # Mc_Struc[Zone](t) = Mc_Struc[Zone](t - dt) + (Mc_StrucIn[Zone] - Mc_StrucActF[Zone] - Mc_StrucSlwF[Zone] - Mc_StrucSlwCO2In[Zone] - Mc_StrucActCO2In[Zone] - Mc_Str_LitSomTrans[Zone]) * dt
      zone_df$Mc_Struc <- zone_df$Mc_Struc + (
        zone_df$Mc_StrucIn - zone_df$Mc_StrucActF - zone_df$Mc_StrucSlwF - zone_df$Mc_StrucSlwCO2In - zone_df$Mc_StrucActCO2In - zone_df$Mc_Str_LitSomTrans
      )
      
      # Mn_CNStrucActual[Zone,SlNut] = if Mn_LitInpN[Zone,SlNut] > 0.01 then max(Mc_StrucIn[Zone]/Mn_LitInpN[Zone,N],Mn_CNStruc*Mn_NutRatStruc[SlNut]) else Mn_CNStruc*Mn_NutRatStruc[SlNut]
      zonenut_df$Mc_StrucIn <- rep(zone_df$Mc_StrucIn, nrow(nut_df))
      zonenut_df$Mn_LitInpN_N <- rep(zonenut_df[zonenut_df$SlNut == "N", ]$Mn_LitInpN, nrow(nut_df))
      zonenut_df$Mn_CNStrucActual <- ifelse(
        zonenut_df$Mn_LitInpN > 0.01,
        pmax(
          zonenut_df$Mc_StrucIn / zonenut_df$Mn_LitInpN_N,
          pars$Mn_par$Mn_CNStruc * zonenut_df$Mn_NutRatStruc
        ),
        pars$Mn_par$Mn_CNStruc * zonenut_df$Mn_NutRatStruc
      )
      
      # Mn_StrucIn[Zone,SlNut] = IF Mn_CNStrucActual[Zone,SlNut] > 0 THEN Mc_StrucIn[Zone]/Mn_CNStrucActual[Zone,SlNut] ELSE 0
      zonenut_df$Mn_StrucIn <- ifelse( zonenut_df$Mn_CNStrucActual > 0, zonenut_df$Mc_StrucIn/zonenut_df$Mn_CNStrucActual, 0)
      
      # Mn_DecActSt[Zone,SlNut] = Mc_StrucActF[Zone]/(Mc_EffStrucAct*Mn_CNStruc*Mn_NutRatStruc[SlNut])
      zonenut_df$Mc_StrucActF <- rep(zone_df$Mc_StrucActF, nrow(nut_df))
      zonenut_df$Mn_DecActSt <- zonenut_df$Mc_StrucActF/( pars$Mc_par$Mc_EffStrucAct* pars$Mn_par$Mn_CNStruc*zonenut_df$Mn_NutRatStruc)
      
      # Mn_DemActSt[Zone,SlNut] = Mc_StrucActF[Zone]/(Mn_CNAct*Mn_NutRatAct[SlNut])
      zonenut_df$Mn_DemActSt <- zonenut_df$Mc_StrucActF/(pars$Mn_par$Mn_CNAct*zonenut_df$Mn_NutRatAct)
      
      # Mn_StrucActF[Zone,SlNut] = MIN(Mn_DecActSt[Zone,SlNut],Mn_DemActSt[Zone,SlNut])
      zonenut_df$Mn_StrucActF <- pmin(zonenut_df$Mn_DecActSt, zonenut_df$Mn_DemActSt)
      
      # Mn_DecStrucSlw[Zone,SlNut] = Mc_StrucSlwF[Zone]/(Mc_EffStrucSlw*Mn_CNStruc*Mn_NutRatStruc[SlNut])
      zonenut_df$Mc_StrucSlwF <- rep(zone_df$Mc_StrucSlwF, nrow(nut_df))
      zonenut_df$Mn_DecStrucSlw <- zonenut_df$Mc_StrucSlwF/(pars$Mc_par$Mc_EffStrucSlw*pars$Mn_par$Mn_CNStruc*zonenut_df$Mn_NutRatStruc)
      
      # Mn_DemStrucSlw[Zone,SlNut] = Mc_StrucSlwF[Zone]/(Mn_CNSlw*Mn_NutRatSlw[SlNut])
      zonenut_df$Mn_DemStrucSlw <- zonenut_df$Mc_StrucSlwF/(pars$Mn_par$Mn_CNSlw*zonenut_df$Mn_NutRatSlw)
      
      # Mn_StrucSlwF[Zone,SlNut] = MIN(Mn_DecStrucSlw[Zone,SlNut],Mn_DemStrucSlw[Zone,SlNut])
      zonenut_df$Mn_StrucSlwF <- pmin(zonenut_df$Mn_DecStrucSlw, zonenut_df$Mn_DemStrucSlw)
      
      # MC__StrLitSomTransFrac[Zone] = if Mc_Struc[Zone] = 0 then 0 else Mc_Str_LitSomTrans[Zone]/Mc_Struc[Zone]
      zone_df$Mc_StrLitSomTransFrac <- ifelse( zone_df$Mc_Struc == 0, 0, zone_df$Mc_Str_LitSomTrans/zone_df$Mc_Struc)
      
      # Mn_Str_LitSomTrans[Zone,SlNut] = Mn_Struc[Zone,SlNut]*MC__StrLitSomTransFrac[Zone]
      zonenut_df$Mc_StrLitSomTransFrac <- rep(zone_df$Mc_StrLitSomTransFrac, nrow(nut_df))
      zonenut_df$Mn_Str_LitSomTrans <- zonenut_df$Mn_Struc* zonenut_df$Mc_StrLitSomTransFrac
      
      # Mn_StrucMinF[Zone,SlNut] = (Mn_DecStrucSlw[Zone,SlNut]-Mn_StrucSlwF[Zone,SlNut])+(Mn_DecActSt[Zone,SlNut]-Mn_StrucActF[Zone,SlNut])+S&B_SurfLitBurnFrac[Zone]*(Mn_Struc[Zone,SlNut]-Mn_StrucActF[Zone,SlNut]-Mn_StrucSlwF[Zone,SlNut])
      zonenut_df$SB_SurfLitBurnFrac <- rep(zone_df$SB_SurfLitBurnFrac, nrow(nut_df))
      zonenut_df$Mn_StrucMinF <- (zonenut_df$Mn_DecStrucSlw - zonenut_df$Mn_StrucSlwF) +
        (zonenut_df$Mn_DecActSt - zonenut_df$Mn_StrucActF) +
        zonenut_df$SB_SurfLitBurnFrac * (zonenut_df$Mn_Struc - zonenut_df$Mn_StrucActF -
                                           zonenut_df$Mn_StrucSlwF)
      
      # Mn_Struc[Zone,SlNut](t) = Mn_Struc[Zone,SlNut](t - dt) + (Mn_StrucIn[Zone,SlNut] - Mn_StrucActF[Zone,SlNut] - Mn_StrucSlwF[Zone,SlNut] - Mn_Str_LitSomTrans[Zone,SlNut] - Mn_StrucMinF[Zone,SlNut]) * dt
      zonenut_df$Mn_Struc <- zonenut_df$Mn_Struc + (
        zonenut_df$Mn_StrucIn - zonenut_df$Mn_StrucActF - zonenut_df$Mn_StrucSlwF - zonenut_df$Mn_Str_LitSomTrans - zonenut_df$Mn_StrucMinF
      )
      
      # S&B_FineNecromass[Zone,PlantComp](t) = S&B_FineNecromass[Zone,PlantComp](t - dt) + (S&B_SlashVegetation[Zone,PlantComp] - S&B_NecromassBurn[Zone,PlantComp] - S&B_NecroFNutVolat[Zone,PlantComp] - S&B_MinNutRele[Zone,PlantComp] - S&B_NecromLitTransf[Zone,PlantComp] - S&B_SPileUp[Zone,PlantComp] - S&B_HazeFromSlash[Zone,PlantComp]) * dt
      zonepcomp_df$SB_FineNecromass <- zonepcomp_df$SB_FineNecromass + (
        zonepcomp_df$SB_SlashVegetation - zonepcomp_df$SB_NecromassBurn - zonepcomp_df$SB_NecroFNutVolat - zonepcomp_df$SB_MinNutRele -  zonepcomp_df$SB_NecromLitTransf -  zonepcomp_df$SB_SPileUp - zonepcomp_df$SB_HazeFromSlash
      )
      
      # Mc_kMetabCur[Zone] = Mc2_kMetab*Mc_RelKMetabLit*Mc_LitTempLim[Zone]*Mc_ThetaLim[Zone]
      zone_df$Mc_kMetabCur <- pars$Mc_par$Mc2_kMetab*pars$Mc_par$Mc_RelKMetabLit*zone_df$Mc_LitTempLim*zone_df$Mc_ThetaLim
      
      # Mc_MetabCO2In[Zone] = Mc_Metab[Zone]*(Mc_kMetabCur[Zone]*(1-Mc_EffMetab)+S&B_SurfLitBurnFrac[Zone])
      zone_df$Mc_MetabCO2In <- zone_df$Mc_Metab*(zone_df$Mc_kMetabCur*(1-pars$Mc_par$Mc_EffMetab)+zone_df$SB_SurfLitBurnFrac)
      
      # Mc_MetabActF[Zone] = (Mc_Metab[Zone]-Mc_MetabCO2In[Zone])*Mc_kMetabCur[Zone]*Mc_EffMetab*Mc_ActDecomDecMin[Zone]
      zone_df$Mc_MetabActF <- (zone_df$Mc_Metab-zone_df$Mc_MetabCO2In)*zone_df$Mc_kMetabCur*pars$Mc_par$Mc_EffMetab*zone_df$Mc_ActDecomDecMin
      
      # Mc_Met_LitSomTrans[Zone] = MC2_LitSomTrans[Zone,Met]*(Mc_Metab[Zone]+Mc_MetabIn[Zone]-Mc_MetabActF[Zone]-Mc_MetabCO2In[Zone])
      zone_df$Mc_Met_LitSomTrans <- zonecpools_df[zonecpools_df$Cent_Pools == "Met",]$Mc2_LitSomTrans* (zone_df$Mc_Metab+zone_df$Mc_MetabIn-zone_df$Mc_MetabActF-zone_df$Mc_MetabCO2In)
      
      # Mc_Metab[Zone](t) = Mc_Metab[Zone](t - dt) + (Mc_MetabIn[Zone] - Mc_MetabActF[Zone] - Mc_MetabCO2In[Zone] - Mc_Met_LitSomTrans[Zone]) * dt
      zone_df$Mc_Metab <- zone_df$Mc_Metab + (zone_df$Mc_MetabIn - zone_df$Mc_MetabActF - zone_df$Mc_MetabCO2In - zone_df$Mc_Met_LitSomTrans)
      
      # Mn_ExtOrgInpN[Zone,SlNut] = Ca_ExtOrgAppZn[1,Zone]*Mn_ExtOrgN[1,SlNut]+Ca_ExtOrgAppZn[2,Zone]*Mn_ExtOrgN[2,SlNut]
      zonenut_df$Mn_ExtOrgInpN <- zoneinp_df[zoneinp_df$ExtOrgInputs == 1, ]$Ca_ExtOrgAppZn * nutinp_df[nutinp_df$ExtOrgInputs == 1, ]$Mn_ExtOrgN +
        zoneinp_df[zoneinp_df$ExtOrgInputs == 2, ]$Ca_ExtOrgAppZn * nutinp_df[nutinp_df$ExtOrgInputs == 2, ]$Mn_ExtOrgN
      
      # Mn_MetabIn[Zone,SlNut] = Mn_ExtOrgInpN[Zone,SlNut]+Mn_LitInpN[Zone,SlNut]-Mn_StrucIn[Zone,SlNut]
      zonenut_df$Mn_MetabIn <- zonenut_df$Mn_ExtOrgInpN+zonenut_df$Mn_LitInpN-zonenut_df$Mn_StrucIn
      
      # Mn_CNMetab[Zone,SlNut] = IF(Mn_Metab[Zone,SlNut]>0)THEN(Mc_Metab[Zone]/Mn_Metab[Zone,SlNut])ELSE(0)
      zonenut_df$Mc_Metab <- rep(zone_df$Mc_Metab, nrow(nut_df))
      zonenut_df$Mn_CNMetab <- ifelse(zonenut_df$Mn_Metab>0, zonenut_df$Mc_Metab/zonenut_df$Mn_Metab, 0)
      
      # Mn_DecActMet[Zone,SlNut] = IF(Mn_CNMetab[Zone,SlNut]>0)THEN(Mc_MetabActF[Zone]/(Mc_EffMetab*Mn_CNMetab[Zone,SlNut]))ELSE(0)
      zonenut_df$Mc_MetabActF <- rep(zone_df$Mc_MetabActF, nrow(nut_df))
      zonenut_df$Mn_DecActMet <- ifelse(zonenut_df$Mn_CNMetab>0, zonenut_df$Mc_MetabActF/(pars$Mc_par$Mc_EffMetab*zonenut_df$Mn_CNMetab), 0)
      
      # Mn_DemActMet[Zone,SlNut] = Mc_MetabActF[Zone]/(Mn_CNAct*Mn_NutRatAct[SlNut])
      zonenut_df$Mn_DemActMet <- zonenut_df$Mc_MetabActF/(pars$Mn_par$Mn_CNAct*zonenut_df$Mn_NutRatAct)
      
      # Mn_MetabActF[Zone,SlNut] = MIN(Mn_DecActMet[Zone,SlNut],Mn_DemActMet[Zone,SlNut])
      zonenut_df$Mn_MetabActF <- pmin(zonenut_df$Mn_DecActMet,zonenut_df$Mn_DemActMet)
      
      # MC_MetLitSomTransFrac[Zone] = if Mc_Metab[Zone] = 0 then 0 else Mc_Met_LitSomTrans[Zone]/Mc_Metab[Zone]
      zone_df$MC_MetLitSomTransFrac <- ifelse( zone_df$Mc_Metab == 0, 0, zone_df$Mc_Met_LitSomTrans/zone_df$Mc_Metab)
      
      # Mn_Met_LitSomTrans[Zone,SlNut] = Mn_Metab[Zone,SlNut]*MC_MetLitSomTransFrac[Zone]
      zonenut_df$MC_MetLitSomTransFrac <- rep(zone_df$MC_MetLitSomTransFrac, nrow(nut_df))
      zonenut_df$Mn_Met_LitSomTrans <- zonenut_df$Mn_Metab*zonenut_df$MC_MetLitSomTransFrac
      
      # Mn_MetabMinF[Zone,SlNut] = Mn_DecActMet[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*(Mn_Metab[Zone,SlNut]-Mn_MetabActF[Zone,SlNut])
      zonenut_df$Mn_MetabMinF <- zonenut_df$Mn_DecActMet + zonenut_df$SB_SurfLitBurnFrac*(zonenut_df$Mn_Metab-zonenut_df$Mn_MetabActF)
      
      # Mn_Metab[Zone,SlNut](t) = Mn_Metab[Zone,SlNut](t - dt) + (Mn_MetabIn[Zone,SlNut] - Mn_MetabActF[Zone,SlNut] - Mn_Met_LitSomTrans[Zone,SlNut] - Mn_MetabMinF[Zone,SlNut]) * dt
      zonenut_df$Mn_Metab <- zonenut_df$Mn_Metab + (zonenut_df$Mn_MetabIn - zonenut_df$Mn_MetabActF - zonenut_df$Mn_Met_LitSomTrans - zonenut_df$Mn_MetabMinF)
      
      # Mc_kActCur[Zone] = Mc_RelKActLit*Mc2_kAct*(1-0.75*Mc_TextLitLayer)*Mc_LitTempLim[Zone]*Mc_ThetaLim[Zone]
      zone_df$Mc_kActCur <- pars$Mc_par$Mc_RelKActLit*pars$Mc_par$Mc2_kAct*(1-0.75*pars$Mc_par$Mc_TextLitLayer)*zone_df$Mc_LitTempLim*zone_df$Mc_ThetaLim
      
      # Mc_ActCO2In[Zone] = Mc_Act[Zone]*(Mc_kActCur[Zone]*Mc_ActResp+S&B_SurfLitBurnFrac[Zone])
      zone_df$Mc_ActCO2In <- zone_df$Mc_Act*(zone_df$Mc_kActCur*Mc_ActResp+zone_df$SB_SurfLitBurnFrac)
      
      # Mc_ActSlw[Zone] = (Mc_Act[Zone]-Mc_ActCO2In[Zone])*Mc_kActCur[Zone]*Mc_EffActSlw
      zone_df$Mc_ActSlw <- (zone_df$Mc_Act-zone_df$Mc_ActCO2In)*zone_df$Mc_kActCur*Mc_EffActSlw
      
      # Mc_kSlwCur[Zone] = Mc_RelKSlwLit*Mc2_kSlw*Mc_LitTempLim[Zone]*Mc_ThetaLim[Zone]
      zone_df$Mc_kSlwCur <- pars$Mc_par$Mc_RelKSlwLit* pars$Mc_par$Mc2_kSlw*zone_df$Mc_LitTempLim*zone_df$Mc_ThetaLim
      
      # Mc_SlwAct[Zone] = Mc_Slw[Zone]*Mc_kSlwCur[Zone]*Mc_EffSlwAct
      zone_df$Mc_SlwAct <- zone_df$Mc_Slw*zone_df$Mc_kSlwCur*pars$Mc_par$Mc_EffSlwAct
      
      # Mc_ActSlwF[Zone] = Mc_ActSlw[Zone]-Mc_SlwAct[Zone]
      zone_df$Mc_ActSlwF <- zone_df$Mc_ActSlw-zone_df$Mc_SlwAct
      
      # Mc_ActPass[Zone] = (Mc_Act[Zone]-Mc_ActCO2In[Zone]-Mc_ActSlw[Zone])*Mc_kActCur[Zone]*0.004
      zone_df$Mc_ActPass <- (zone_df$Mc_Act- zone_df$Mc_ActCO2In- zone_df$Mc_ActSlw)* zone_df$Mc_kActCur*0.004
      
      # Mc_kPassCur[Zone] = Mc_RelKPassLit*Mc2_kPass*Mc_LitTempLim[Zone]*Mc_ThetaLim[Zone]
      zone_df$Mc_kPassCur <- pars$Mc_par$Mc_RelKPassLit*pars$Mc_par$Mc2_kPass*zone_df$Mc_LitTempLim*zone_df$Mc_ThetaLim
      
      # Mc_PassAct[Zone] = Mc_Pass[Zone]*Mc_kPassCur[Zone]*Mc_EffPass
      zone_df$Mc_PassAct <- zone_df$Mc_Pass*zone_df$Mc_kPassCur* pars$Mc_par$Mc_EffPass
      
      # Mc_ActPassF[Zone] = Mc_ActPass[Zone]-Mc_PassAct[Zone]
      zone_df$Mc_ActPassF <- zone_df$Mc_ActPass-zone_df$Mc_PassAct
      
      # Mc_Act_LitSomTrans[Zone] = MC2_LitSomTrans[Zone,Actv]*(Mc_Act[Zone]+Mc_StrucActF[Zone]+Mc_MetabActF[Zone] -Mc_ActCO2In[Zone]-Mc_ActSlwF[Zone]-Mc_ActPassF[Zone])
      zone_df$Mc_Act_LitSomTrans <- zonecpools_df[zonecpools_df$Cent_Pools == "Actv", ]$Mc2_LitSomTrans*
        (zone_df$Mc_Act+zone_df$Mc_StrucActF+zone_df$Mc_MetabActF -zone_df$Mc_ActCO2In-zone_df$Mc_ActSlwF-zone_df$Mc_ActPassF)
      
      # Mc_Act[Zone](t) = Mc_Act[Zone](t - dt) + (Mc_MetabActF[Zone] + Mc_StrucActF[Zone] - Mc_ActCO2In[Zone] - Mc_ActSlwF[Zone] - Mc_ActPassF[Zone] - Mc_Act_LitSomTrans[Zone]) * dt
      zone_df$Mc_Act <- zone_df$Mc_Act + (zone_df$Mc_MetabActF + zone_df$Mc_StrucActF - zone_df$Mc_ActCO2In - zone_df$Mc_ActSlwF - zone_df$Mc_ActPassF - zone_df$Mc_Act_LitSomTrans) 
      
      # Mc_ActSlw[Zone] = (Mc_Act[Zone]-Mc_ActCO2In[Zone])*Mc_kActCur[Zone]*Mc_EffActSlw
      zone_df$Mc_ActSlw <- (zone_df$Mc_Act-zone_df$Mc_ActCO2In)*zone_df$Mc_kActCur*Mc_EffActSlw
      
      # Mn_ActSlw[Zone,SlNut] = Mc_ActSlw[Zone]/(Mn_CNSlw*Mn_NutRatSlw[SlNut])
      zonenut_df$Mc_ActSlw <- rep(zone_df$Mc_ActSlw, nrow(nut_df))
      zonenut_df$Mn_ActSlw <- zonenut_df$Mc_ActSlw/(pars$Mn_par$Mn_CNSlw* zonenut_df$Mn_NutRatSlw)
      
      # Mn_SlwAct[Zone,SlNut] = Mc_SlwAct[Zone]/(Mn_CNAct*Mn_NutRatAct[SlNut])
      zonenut_df$Mc_SlwAct <- rep(zone_df$Mc_SlwAct, nrow(nut_df))
      zonenut_df$Mn_SlwAct <- zonenut_df$Mc_SlwAct/(pars$Mn_par$Mn_CNAct*zonenut_df$Mn_NutRatAct)
      
      # Mn_ActSlwF[Zone,SlNut] = MAX(0,Mn_ActSlw[Zone,SlNut])-MAX(0,Mn_SlwAct[Zone,SlNut])
      zonenut_df$Mn_ActSlwF <- pmax(0, zonenut_df$Mn_ActSlw)-pmax(0, zonenut_df$Mn_SlwAct)
      
      # Mn_ActPass[Zone,SlNut] = Mc_ActPass[Zone]/(Mn_CNPass*Mn_NutRatPas[SlNut])
      zonenut_df$Mc_ActPass <- rep(zone_df$Mc_ActPass, nrow(nut_df))
      zonenut_df$Mn_ActPass <- zonenut_df$Mc_ActPass/(pars$Mn_par$Mn_CNPass*zonenut_df$Mn_NutRatPas)
      
      # Mn_PassAct[Zone,SlNut] = Mc_PassAct[Zone]/(Mn_CNAct*Mn_NutRatAct[SlNut])
      zonenut_df$Mc_PassAct <- rep(zone_df$Mc_PassAct, nrow(nut_df))
      zonenut_df$Mn_PassAct <- zonenut_df$Mc_PassAct/(pars$Mn_par$Mn_CNAct*zonenut_df$Mn_NutRatAct)
      
      # Mn_ActPassF[Zone,SlNut] = MAX(0,Mn_ActPass[Zone,SlNut])-MAX(0,Mn_PassAct[Zone,SlNut])
      zonenut_df$Mn_ActPassF <- pmax(0, zonenut_df$Mn_ActPass)-pmax(0, zonenut_df$Mn_PassAct)
      
      # Mn_DecAct[Zone,SlNut] = if Mc_EffActSlw <> 0 and Mn_CNActAct[Zone,SlNut] <> 0 then Mc_ActSlw[Zone]/(Mc_EffActSlw*Mn_CNActAct[Zone,SlNut]) else 0
      zonenut_df$Mc_EffActSlw <- Mc_EffActSlw
      zonenut_df$Mn_DecAct <- ifelse( zonenut_df$Mc_EffActSlw != 0 & zonenut_df$Mn_CNActAct != 0, zonenut_df$Mc_ActSlw/(Mc_EffActSlw*zonenut_df$Mn_CNActAct), 0)
      
      # Mn_ActMin[Zone,SlNut] = MIN(MAX(0,Mn_DecAct[Zone,SlNut]-Mn_ActSlw[Zone,SlNut]-Mn_ActPass[Zone,SlNut]),Mn_Act[Zone,SlNut])
      zonenut_df$Mn_ActMin <- pmin(pmax(0, zonenut_df$Mn_DecAct-zonenut_df$Mn_ActSlw-zonenut_df$Mn_ActPass),zonenut_df$Mn_Act)
      
      # Mn_ImmobAct[Zone,SlNut] = MIN(Mn_MinNutpool[Zone,SlNut],MAX(0,Mn_DemActMet[Zone,SlNut]-Mn_DecActMet[Zone,SlNut])+MAX(0,Mn_DemActSt[Zone,SlNut]-Mn_DecActSt[Zone,SlNut])+MAX(0,Mn_Act[Zone,SlNut]*Mn_CNActAct[Zone,SlNut]*(1/(Mn_CNAct*Mn_NutRatAct[SlNut]) - 1/Mn_CNActAct[Zone,SlNut]) ))
      zonenut_df$Mn_ImmobAct <- pmin(
        zonenut_df$Mn_MinNutpool,
        pmax(0, zonenut_df$Mn_DemActMet - zonenut_df$Mn_DecActMet) +
          pmax(0, zonenut_df$Mn_DemActSt -
                 zonenut_df$Mn_DecActSt) + pmax(
                   0,
                   zonenut_df$Mn_Act * zonenut_df$Mn_CNActAct * (
                     1 / (pars$Mn_par$Mn_CNAct * zonenut_df$Mn_NutRatAct) - 1 / zonenut_df$Mn_CNActAct
                   )
                 )
      )
      
      # Mn_ActMinF[Zone,SlNut] = IF (Mn_ActMin[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Act[Zone,SlNut]-Mn_ImmobAct[Zone,SlNut])>0 THEN MIN((Mn_ActMin[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Act[Zone,SlNut]-Mn_ImmobAct[Zone,SlNut]),Mn_ActMin[Zone,SlNut]) ELSE IF (Mn_ActMin[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Act[Zone,SlNut]-Mn_ImmobAct[Zone,SlNut])=0 THEN 0 ELSE -MIN(ABS(Mn_ActMin[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Act[Zone,SlNut]-Mn_ImmobAct[Zone,SlNut]),Mn_MinNutpool[Zone,SlNut]-Mn_Mineralization[Zone,SlNut])
      
      zonenut_df$Mn_ActMinF_a <- zonenut_df$Mn_ActMin + zonenut_df$SB_SurfLitBurnFrac *
        zonenut_df$Mn_Act - zonenut_df$Mn_ImmobAct
      
      zonenut_df$Mn_ActMinF <- ifelse(
        zonenut_df$Mn_ActMinF_a > 0,
        pmin(zonenut_df$Mn_ActMinF_a, zonenut_df$Mn_ActMin),
        ifelse(
          zonenut_df$Mn_ActMinF_a == 0,
          0,
          -pmin(
            abs(zonenut_df$Mn_ActMinF_a),
            zonenut_df$Mn_MinNutpool - zonenut_df$Mn_Mineralization
          )
        )
      )
     
      # MC_ActLitSomTransFrac[Zone] = if Mc_Act[Zone]= 0 then 0 else Mc_Act_LitSomTrans[Zone]/Mc_Act[Zone]
      zone_df$MC_ActLitSomTransFrac <- ifelse( zone_df$Mc_Act == 0, 0, zone_df$Mc_Act_LitSomTrans/zone_df$Mc_Act)
      
      # Mn_Act_LitSomTrans[Zone,SlNut] = Mn_Act[Zone,SlNut]*MC_ActLitSomTransFrac[Zone]
      zonenut_df$MC_ActLitSomTransFrac <- rep(zone_df$MC_ActLitSomTransFrac, nrow(nut_df))
      zonenut_df$Mn_Act_LitSomTrans <- zonenut_df$Mn_Act*zonenut_df$MC_ActLitSomTransFrac
      
      # Mn_Act[Zone,SlNut](t) = Mn_Act[Zone,SlNut](t - dt) + (Mn_MetabActF[Zone,SlNut] + Mn_StrucActF[Zone,SlNut] - Mn_ActSlwF[Zone,SlNut] - Mn_ActPassF[Zone,SlNut] - Mn_ActMinF[Zone,SlNut] - Mn_Act_LitSomTrans[Zone,SlNut]) * dt
      zonenut_df$Mn_Act <- zonenut_df$Mn_Act + (zonenut_df$Mn_MetabActF + zonenut_df$Mn_StrucActF - zonenut_df$Mn_ActSlwF - zonenut_df$Mn_ActPassF - zonenut_df$Mn_ActMinF - zonenut_df$Mn_Act_LitSomTrans)
      
      # Mc_SlwPassF[Zone] = Mc_Slw[Zone]*Mc_kSlwCur[Zone]*Mc_EffSlwPass
      zone_df$Mc_SlwPassF <- zone_df$Mc_Slw*zone_df$Mc_kSlwCur* pars$Mc_par$Mc_EffSlwPass
      
      # Mc_SlwCO2In[Zone] = Mc_Slw[Zone]*(Mc_kSlwCur[Zone]*(1-Mc_EffSlwAct-Mc_EffSlwPass)+S&B_SurfLitBurnFrac[Zone])
      zone_df$Mc_SlwCO2In <- zone_df$Mc_Slw*(zone_df$Mc_kSlwCur*(1-pars$Mc_par$Mc_EffSlwAct-pars$Mc_par$Mc_EffSlwPass)+zone_df$SB_SurfLitBurnFrac)
      
      # Mc_Slw_LitSomTrans[Zone] = MC2_LitSomTrans[Zone,Slow]*(Mc_Slw[Zone]+Mc_StrucSlwF[Zone]+Mc_ActSlwF[Zone]-Mc_SlwPassF[Zone]-Mc_SlwCO2In[Zone]   )
      zone_df$Mc_Slw_LitSomTrans <- zonecpools_df[zonecpools_df$Cent_Pools == "Slow", ]$Mc2_LitSomTrans *
        (
          zone_df$Mc_Slw + zone_df$Mc_StrucSlwF + zone_df$Mc_ActSlwF - zone_df$Mc_SlwPassF -
            zone_df$Mc_SlwCO2In
        )
      
      # Mc_Slw[Zone](t) = Mc_Slw[Zone](t - dt) + (Mc_StrucSlwF[Zone] + Mc_ActSlwF[Zone] - Mc_SlwPassF[Zone] - Mc_SlwCO2In[Zone] - Mc_Slw_LitSomTrans[Zone]) * dt
      zone_df$Mc_Slw <- zone_df$Mc_Slw + (zone_df$Mc_StrucSlwF + zone_df$Mc_ActSlwF - zone_df$Mc_SlwPassF - zone_df$Mc_SlwCO2In - zone_df$Mc_Slw_LitSomTrans) 
      
      # Mn_SlwPassF[Zone,SlNut] = Mc_SlwPassF[Zone]/(Mn_CNPass*Mn_NutRatPas[SlNut])
      zonenut_df$Mc_SlwPassF <- rep(zone_df$Mc_SlwPassF, nrow(nut_df))
      zonenut_df$Mn_SlwPassF <- zonenut_df$Mc_SlwPassF/(pars$Mn_par$Mn_CNPass*zonenut_df$Mn_NutRatPas)
      
      # Mn_DecSlw[Zone,SlNut] = if Mn_CNSlwAct[Zone,SlNut]> 0 then Mc_SlwPassF[Zone]/(Mc_EffSlwPass*Mn_CNSlwAct[Zone,SlNut]) else 0
      zonenut_df$Mn_DecSlw <- ifelse( zonenut_df$Mn_CNSlwAct> 0, zonenut_df$Mc_SlwPassF/(pars$Mc_par$Mc_EffSlwPass*zonenut_df$Mn_CNSlwAct),0)
      
      # Mn_SlwMin[Zone,SlNut] = MAX(0,Mn_DecSlw[Zone,SlNut]-Mn_SlwAct[Zone,SlNut]-Mn_SlwPassF[Zone,SlNut])
      zonenut_df$Mn_SlwMin <- pmax(0, zonenut_df$Mn_DecSlw- zonenut_df$Mn_SlwAct- zonenut_df$Mn_SlwPassF)
      
      # Mn_ImmobStrucSlw[Zone,SlNut] = MIN(Mn_MinNutpool[Zone,SlNut]-Mn_ImmobAct[Zone,SlNut],MAX(0,Mn_DemStrucSlw[Zone,SlNut]-Mn_DecStrucSlw[Zone,SlNut])+MAX(0, Mn_Slw[Zone,SlNut]*Mn_CNSlwAct[Zone,SlNut]*(1/(Mn_CNSlw*Mn_NutRatSlw[SlNut]) - 1/Mn_CNSlwAct[Zone,SlNut]) ))
      zonenut_df$Mn_ImmobStrucSlw <- pmin(
        zonenut_df$Mn_MinNutpool - zonenut_df$Mn_ImmobAct,
        pmax(0, zonenut_df$Mn_DemStrucSlw -
               zonenut_df$Mn_DecStrucSlw) +
          pmax(
            0,
            zonenut_df$Mn_Slw * zonenut_df$Mn_CNSlwAct * (
              1 / (pars$Mn_par$Mn_CNSlw * zonenut_df$Mn_NutRatSlw) - 1 / zonenut_df$Mn_CNSlwAct
            )
          )
      )
      
      # Mn_SlwMinF[Zone,SlNut] = IF (Mn_SlwMin[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Slw[Zone,SlNut]-Mn_ImmobStrucSlw[Zone,SlNut])>0 THEN MIN( (Mn_SlwMin[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Slw[Zone,SlNut]-Mn_ImmobStrucSlw[Zone,SlNut]),Mn_Slw[Zone,SlNut]) ELSE IF (Mn_SlwMin[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Slw[Zone,SlNut]-Mn_ImmobStrucSlw[Zone,SlNut])=0 THEN 0 ELSE MAX((Mn_SlwMin[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Slw[Zone,SlNut]-Mn_ImmobStrucSlw[Zone,SlNut]),-Mn_MinNutpool[Zone,SlNut])
      zonenut_df$Mn_SlwMinF_a <- zonenut_df$Mn_SlwMin + zonenut_df$SB_SurfLitBurnFrac * zonenut_df$Mn_Slw -
        zonenut_df$Mn_ImmobStrucSlw
      
      zonenut_df$Mn_SlwMinF <- ifelse(
        zonenut_df$Mn_SlwMinF_a > 0,
        pmin(zonenut_df$Mn_SlwMinF_a, zonenut_df$Mn_Slw),
        ifelse(
          zonenut_df$Mn_SlwMinF_a == 0,
          0,
          pmax(zonenut_df$Mn_SlwMinF_a, -zonenut_df$Mn_MinNutpool)
        )
      )
      
      # MC_SlwLitSomTransFrac[Zone] = if Mc_Slw[Zone] = 0 then 0 else Mc_Slw_LitSomTrans[Zone]/Mc_Slw[Zone]
      zone_df$MC_SlwLitSomTransFrac <- ifelse( zone_df$Mc_Slw == 0, 0, zone_df$Mc_Slw_LitSomTrans/zone_df$Mc_Slw)
      
      # Mn_Slw_LitSomTrans[Zone,SlNut] = MC_SlwLitSomTransFrac[Zone]*Mn_Slw[Zone,SlNut]
      zonenut_df$MC_SlwLitSomTransFrac <- rep(zone_df$MC_SlwLitSomTransFrac, nrow(nut_df))
      zonenut_df$Mn_Slw_LitSomTrans <-zonenut_df$MC_SlwLitSomTransFrac*zonenut_df$Mn_Slw
      
      # Mn_Slw[Zone,SlNut](t) = Mn_Slw[Zone,SlNut](t - dt) + (Mn_StrucSlwF[Zone,SlNut] + Mn_ActSlwF[Zone,SlNut] - Mn_SlwPassF[Zone,SlNut] - Mn_SlwMinF[Zone,SlNut] - Mn_Slw_LitSomTrans[Zone,SlNut]) * dt
      zonenut_df$Mn_Slw <- zonenut_df$Mn_Slw + (zonenut_df$Mn_StrucSlwF + zonenut_df$Mn_ActSlwF - zonenut_df$Mn_SlwPassF - zonenut_df$Mn_SlwMinF - zonenut_df$Mn_Slw_LitSomTrans)
      
      # Mc_PassCO2In[Zone] = Mc_Pass[Zone]*(Mc_kPassCur[Zone]*(1-Mc_EffPass)+S&B_SurfLitBurnFrac[Zone])
      zone_df$Mc_PassCO2In <- zone_df$Mc_Pass*(zone_df$Mc_kPassCur*(1-pars$Mc_par$Mc_EffPass)+zone_df$SB_SurfLitBurnFrac)
      
      # Mc_Pas_LitSomTrans[Zone] = MC2_LitSomTrans[Zone,Pass]*(Mc_Pass[Zone]+Mc_SlwPassF[Zone]-Mc_PassCO2In[Zone])
      zone_df$Mc_Pas_LitSomTrans <- zonecpools_df[zonecpools_df$Cent_Pools == "Pass", ]$Mc2_LitSomTrans*(zone_df$Mc_Pass+zone_df$Mc_SlwPassF-zone_df$Mc_PassCO2In)
      
      # Mc_Pass[Zone](t) = Mc_Pass[Zone](t - dt) + (Mc_SlwPassF[Zone] + Mc_ActPassF[Zone] - Mc_PassCO2In[Zone] - Mc_Pas_LitSomTrans[Zone]) * dt
      zone_df$Mc_Pass <- zone_df$Mc_Pass + (zone_df$Mc_SlwPassF + zone_df$Mc_ActPassF - zone_df$Mc_PassCO2In - zone_df$Mc_Pas_LitSomTrans)
      
      # MC_PassLitSomTransFrac[Zone] = if Mc_Pass[Zone]= 0 then 0 else Mc_Pas_LitSomTrans[Zone]/Mc_Pass[Zone]
      zone_df$MC_PassLitSomTransFrac <- ifelse( zone_df$Mc_Pass == 0, 0, zone_df$Mc_Pas_LitSomTrans/zone_df$Mc_Pass)
      
      # Mn_Pas_LitSomTrans[Zone,SlNut] = Mn_Pass[Zone,SlNut]*MC_PassLitSomTransFrac[Zone]
      zonenut_df$MC_PassLitSomTransFrac <- rep(zone_df$MC_PassLitSomTransFrac, nrow(nut_df))
      zonenut_df$Mn_Pas_LitSomTrans <- zonenut_df$Mn_Pass * zonenut_df$MC_PassLitSomTransFrac
      
      # Mn_DecPass[Zone,SlNut] = Mc_PassAct[Zone]/(Mc_EffPass*Mn_CNPass*Mn_NutRatPas[SlNut])
      zonenut_df$Mn_DecPass <- zonenut_df$Mc_PassAct/(pars$Mc_par$Mc_EffPass*pars$Mn_par$Mn_CNPass*zonenut_df$Mn_NutRatPas)
      
      # Mn_PasMinF[Zone,SlNut] = Mn_DecPass[Zone,SlNut]-Mn_PassAct[Zone,SlNut]+S&B_SurfLitBurnFrac[Zone]*Mn_Pass[Zone,SlNut]
      zonenut_df$Mn_PasMinF <- zonenut_df$Mn_DecPass-zonenut_df$Mn_PassAct+ zonenut_df$SB_SurfLitBurnFrac* zonenut_df$Mn_Pass
      
      # Mn_Pass[Zone,SlNut](t) = Mn_Pass[Zone,SlNut](t - dt) + (Mn_SlwPassF[Zone,SlNut] + Mn_ActPassF[Zone,SlNut] - Mn_Pas_LitSomTrans[Zone,SlNut] - Mn_PasMinF[Zone,SlNut]) * dt
      zonenut_df$Mn_Pass <- zonenut_df$Mn_Pass + (zonenut_df$Mn_SlwPassF + zonenut_df$Mn_ActPassF - zonenut_df$Mn_Pas_LitSomTrans - zonenut_df$Mn_PasMinF)
      
      # T_RtDec_DW[Zone,SoilLayer] = T_Root_T1_DWdecay[Zone,SoilLayer]+T_Root_T2_DWdecay[Zone,SoilLayer]+T_Root_T3_DWdecay[Zone,SoilLayer]
      # T_RtDec_N[Zone,SoilLayer] = T_Root_T1_Ndecay[Zone,SoilLayer]+T_Root_T2_Ndecay[Zone,SoilLayer]+T_Root_T3_Ndecay[Zone,SoilLayer]
      # T_RtDec_P[Zone,SoilLayer] = T_Root_T1_Pdecay[Zone,SoilLayer]+T_Root_T2_Pdecay[Zone,SoilLayer]+T_Root_T3_Pdecay[Zone,SoilLayer]
      zonelayerpcomp_df$T_RtDec <-  aggregate( zonelayertreepcomp_df["T_Root_decay"], zonelayertreepcomp_df[c("zone", "layer", "PlantComp")], sum)$T_Root_decay

      # Mc2_InputRtAmount[Zone,SoilLayer] = T_RtDec_DW[Zone,SoilLayer]+C_RtDecay_DW[Zone,SoilLayer]
      zonelayer_df$Mc2_InputRtAmount <- zonelayerpcomp_df[zonelayerpcomp_df$PlantComp == "DW",]$T_RtDec+zonelayer_df$C_RtDecay_DW
      
      # Mc2_OrgInpC[Zone,SoilLayer] = Mc_Carbon*1000*Mc2_InputRtAmount[Zone,SoilLayer]
      zonelayer_df$Mc2_OrgInpC <- pars$Mc_par$Mc_Carbon*1000*zonelayer_df$Mc2_InputRtAmount
      
      # T_RtDec_LignPoly[Zone,SoilLayer] = T_Root_T1_DWdecay[Zone,SoilLayer]*(T_LignRt[Sp1]+T_PolypRt[Sp1])+T_Root_T2_DWdecay[Zone,SoilLayer]*(T_LignRt[Sp2]+T_PolypRt[Sp2])+T_Root_T3_DWdecay[Zone,SoilLayer]*(T_LignRt[Sp3]+T_PolypRt[Sp3])
      zonelayertree_df$T_Root_decay_DW <- zonelayertreepcomp_df[zonelayertreepcomp_df$PlantComp == "DW",]$T_Root_decay
      tree_df$T_LignRt_PolypRt <- tree_df$T_LignRt+tree_df$T_PolypRt
      zonelayertree_df$T_Root_decay_DW_Light <- zonelayertree_df$T_Root_decay_DW * rep(tree_df$T_LignRt_PolypRt, each = nzone*nlayer)
      zonelayer_df$T_RtDec_LignPoly <- aggregate(zonelayertree_df["T_Root_decay_DW_Light"], zonelayertree_df[c("zone", "layer")], sum)$T_Root_decay_DW_Light 

      # Mc2_InputLign[Zone,SoilLayer] = IF(Mc2_InputRtAmount[Zone,SoilLayer]>0)THEN T_RtDec_LignPoly[Zone,SoilLayer]+(Cq_LignRootResCurr[Zone]+Cq_PolyRt[Zone])*C_RtDecay_DW[Zone,SoilLayer]/Mc2_InputRtAmount[Zone,SoilLayer] ELSE(0)
      zonelayer_df$Cq_LignRootResCurr <- rep(zone_df$Cq_LignRootResCurr, nlayer)
      zonelayer_df$Cq_PolyRt <- rep(zone_df$Cq_PolyRt, nlayer)
      zonelayer_df$Mc2_InputLign <- ifelse(zonelayer_df$Mc2_InputRtAmount>0, zonelayer_df$T_RtDec_LignPoly+
                                             (zonelayer_df$Cq_LignRootResCurr+zonelayer_df$Cq_PolyRt)*zonelayer_df$C_RtDecay_DW/zonelayer_df$Mc2_InputRtAmount, 0)
      
      # Mc2_SplitMet[Zone,SoilLayer] = IF(Mn_LitterNConc[Zone,N]>0)THEN(0.85 - 0.018*Mc2_InputLign[Zone,SoilLayer]/Mn_LitterNConc[Zone,N])ELSE(10^-6)
      zonelayer_df$Mn_LitterNConc_N <- rep(zonenut_df[zonenut_df$SlNut == "N",]$Mn_LitterNConc, nlayer)
      zonelayer_df$Mc2_SplitMet <- ifelse(zonelayer_df$Mn_LitterNConc_N>0, 0.85 - 0.018*zonelayer_df$Mc2_InputLign/zonelayer_df$Mn_LitterNConc_N, 10^-6)
      
      # Mc2_MetabIn[Zone,SoilLayer] = Mc2_OrgInpC[Zone,SoilLayer]*Mc2_SplitMet[Zone,SoilLayer]
      zonelayer_df$Mc2_MetabIn <- zonelayer_df$Mc2_OrgInpC*zonelayer_df$Mc2_SplitMet
      
      
      

            
      ### NEXT EDIT ###########################
      
      

      
      # MC2_RH1Drain[Zone] = W_H1RelDr[Zone]
      # MC2_RH2Drain[Zone] = iF AF_Depth2[Zone]>0 THEN (AF_DepthAct1[Zone]/AF_Depth2[Zone])*W_H2RelDr[Zone] ELSE 0
      # MC2_RH3Drain[Zone] = IF AF_Depth2[Zone]>0 THEN (AF_Depth2[Zone]/AF_Depth3[Zone])*W_H3RelDr[Zone] ELSE 0
      # MC2_RH4Drain[Zone] = IF AF_Depth3[Zone]>0 THEN (AF_Depth3[Zone]/AF_Depth4[Zone])*W_H4RellDr[Zone] eLSE 0
      
      zonelayer_df$MC2_RH2Drain[Zone] = iF AF_Depth2[Zone]>0 THEN (AF_DepthAct1[Zone]/AF_Depth2[Zone])*W_H2RelDr[Zone] ELSE 0
      
      MC2_RH1Drain[Zone] = W_H1RelDr[Zone]
      MC2_RH2Drain[Zone] = iF AF_Depth2[Zone]>0 THEN (AF_DepthAct1[Zone]/AF_Depth2[Zone])*W_H2RelDr[Zone] ELSE 0
      MC2_RH3Drain[Zone] = IF AF_Depth2[Zone]>0 THEN (AF_Depth2[Zone]/AF_Depth3[Zone])*W_H3RelDr[Zone] ELSE 0
      MC2_RH4Drain[Zone] = IF AF_Depth3[Zone]>0 THEN (AF_Depth3[Zone]/AF_Depth4[Zone])*W_H4RellDr[Zone] eLSE 0
      
      
      # MC2_Met_LayerTrans[Zn1,1] = (MC2_RH1Drain[Zn1]+0*(MC2_RH2Drain[Zn1]+MC2_RH3Drain[Zn1]+MC2_RH4Drain[Zn1]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn1,1]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn1,2] = (MC2_RH2Drain[Zn1]+0*(MC2_RH1Drain[Zn1]+MC2_RH3Drain[Zn1]+MC2_RH4Drain[Zn1]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn1,2]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn1,3] = (MC2_RH3Drain[Zn1]+0*(MC2_RH2Drain[Zn1]+MC2_RH1Drain[Zn1]+MC2_RH4Drain[Zn1]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn1,3]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn1,4] = (MC2_RH4Drain[Zn1]+0*(MC2_RH2Drain[Zn1]+MC2_RH3Drain[Zn1]+MC2_RH1Drain[Zn1]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn1,4]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn2,1] = (MC2_RH1Drain[Zn2]+0*(MC2_RH2Drain[Zn2]+MC2_RH3Drain[Zn2]+MC2_RH4Drain[Zn2]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn2,1]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn2,2] = (MC2_RH2Drain[Zn2]+0*(MC2_RH1Drain[Zn2]+MC2_RH3Drain[Zn2]+MC2_RH4Drain[Zn2]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn2,2]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn2,3] = (MC2_RH3Drain[Zn2]+0*(MC2_RH2Drain[Zn2]+MC2_RH1Drain[Zn2]+MC2_RH4Drain[Zn2]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn2,3]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn2,4] = (MC2_RH4Drain[Zn2]+0*(W_Rh2Drain[Zn2]+MC2_RH3Drain[Zn2]+MC2_RH1Drain[Zn2]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn2,4]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn3,1] = (MC2_RH1Drain[Zn3]+0*(MC2_RH2Drain[Zn3]+MC2_RH3Drain[Zn3]+MC2_RH4Drain[Zn3]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn3,1]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn3,2] = (MC2_RH2Drain[Zn3]+0*(MC2_RH1Drain[Zn3]+MC2_RH3Drain[Zn3]+MC2_RH4Drain[Zn3]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn3,2]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn3,3] = (MC2_RH3Drain[Zn3]+0*(MC2_RH2Drain[Zn3]+MC2_RH1Drain[Zn3]+MC2_RH4Drain[Zn3]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn3,3]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn3,4] = (MC2_RH4Drain[Zn3]+0*(MC2_RH2Drain[Zn3]+MC2_RH3Drain[Zn3]+MC2_RH1Drain[Zn3]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn3,4]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn4,1] = (MC2_RH1Drain[Zn4]+0*(MC2_RH2Drain[Zn4]+MC2_RH3Drain[Zn4]+MC2_RH4Drain[Zn4]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn4,1]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn4,2] = (MC2_RH2Drain[Zn4]+0*(MC2_RH1Drain[Zn4]+MC2_RH3Drain[Zn4]+MC2_RH4Drain[Zn4]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn4,2]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn4,3] = (MC2_RH3Drain[Zn4]+0*(MC2_RH2Drain[Zn4]+MC2_RH1Drain[Zn4]+MC2_RH4Drain[Zn4]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn4,3]*MC2_WormTransfer[Met]
      # MC2_Met_LayerTrans[Zn4,4] = (MC2_RH4Drain[Zn4]+0*(MC2_RH2Drain[Zn4]+MC2_RH3Drain[Zn4]+MC2_RH1Drain[Zn4]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn4,4]*MC2_WormTransfer[Met]
      
      zonelayer_df$MC2_Met_LayerTrans <- (zonelayer_df$MC2_RH1Drain[Zn1])*zonelayer_df$MC2_RainTransfer[Met]+zonelayer_df$S_SOMFoodForWorms[Zn1,1]*zonelayer_df$MC2_WormTransfer[Met]
      
      
      MC2_Met_LayerTrans[Zn1,1] = (MC2_RH1Drain[Zn1]+0*(MC2_RH2Drain[Zn1]+MC2_RH3Drain[Zn1]+MC2_RH4Drain[Zn1]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn1,1]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn1,2] = (MC2_RH2Drain[Zn1]+0*(MC2_RH1Drain[Zn1]+MC2_RH3Drain[Zn1]+MC2_RH4Drain[Zn1]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn1,2]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn1,3] = (MC2_RH3Drain[Zn1]+0*(MC2_RH2Drain[Zn1]+MC2_RH1Drain[Zn1]+MC2_RH4Drain[Zn1]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn1,3]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn1,4] = (MC2_RH4Drain[Zn1]+0*(MC2_RH2Drain[Zn1]+MC2_RH3Drain[Zn1]+MC2_RH1Drain[Zn1]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn1,4]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn2,1] = (MC2_RH1Drain[Zn2]+0*(MC2_RH2Drain[Zn2]+MC2_RH3Drain[Zn2]+MC2_RH4Drain[Zn2]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn2,1]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn2,2] = (MC2_RH2Drain[Zn2]+0*(MC2_RH1Drain[Zn2]+MC2_RH3Drain[Zn2]+MC2_RH4Drain[Zn2]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn2,2]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn2,3] = (MC2_RH3Drain[Zn2]+0*(MC2_RH2Drain[Zn2]+MC2_RH1Drain[Zn2]+MC2_RH4Drain[Zn2]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn2,3]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn2,4] = (MC2_RH4Drain[Zn2]+0*(W_Rh2Drain[Zn2]+MC2_RH3Drain[Zn2]+MC2_RH1Drain[Zn2]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn2,4]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn3,1] = (MC2_RH1Drain[Zn3]+0*(MC2_RH2Drain[Zn3]+MC2_RH3Drain[Zn3]+MC2_RH4Drain[Zn3]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn3,1]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn3,2] = (MC2_RH2Drain[Zn3]+0*(MC2_RH1Drain[Zn3]+MC2_RH3Drain[Zn3]+MC2_RH4Drain[Zn3]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn3,2]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn3,3] = (MC2_RH3Drain[Zn3]+0*(MC2_RH2Drain[Zn3]+MC2_RH1Drain[Zn3]+MC2_RH4Drain[Zn3]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn3,3]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn3,4] = (MC2_RH4Drain[Zn3]+0*(MC2_RH2Drain[Zn3]+MC2_RH3Drain[Zn3]+MC2_RH1Drain[Zn3]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn3,4]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn4,1] = (MC2_RH1Drain[Zn4]+0*(MC2_RH2Drain[Zn4]+MC2_RH3Drain[Zn4]+MC2_RH4Drain[Zn4]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn4,1]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn4,2] = (MC2_RH2Drain[Zn4]+0*(MC2_RH1Drain[Zn4]+MC2_RH3Drain[Zn4]+MC2_RH4Drain[Zn4]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn4,2]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn4,3] = (MC2_RH3Drain[Zn4]+0*(MC2_RH2Drain[Zn4]+MC2_RH1Drain[Zn4]+MC2_RH4Drain[Zn4]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn4,3]*MC2_WormTransfer[Met]
      MC2_Met_LayerTrans[Zn4,4] = (MC2_RH4Drain[Zn4]+0*(MC2_RH2Drain[Zn4]+MC2_RH3Drain[Zn4]+MC2_RH1Drain[Zn4]))*MC2_RainTransfer[Met]+S_SOMFoodForWorms[Zn4,4]*MC2_WormTransfer[Met]
      
      
      # MC2_Met_SomTrans[Zn1,1] = Mc_Met_LitSomTrans[Zn1] - Mc2_Metab[Zn1,1]*MC2_Met_LayerTrans[Zn1,1]
      # MC2_Met_SomTrans[Zn1,2] = 0*Mc_Met_LitSomTrans[Zn1] + Mc2_Metab[Zn1,1]*MC2_Met_LayerTrans[Zn1,1] - Mc2_Metab[Zn1,2]*MC2_Met_LayerTrans[Zn1,2]
      # MC2_Met_SomTrans[Zn1,3] = 0*Mc_Met_LitSomTrans[Zn1] + Mc2_Metab[Zn1,2]*MC2_Met_LayerTrans[Zn1,2] - Mc2_Metab[Zn1,3]*MC2_Met_LayerTrans[Zn1,3]
      # MC2_Met_SomTrans[Zn1,4] = 0*Mc_Met_LitSomTrans[Zn1] + Mc2_Metab[Zn1,3]*MC2_Met_LayerTrans[Zn1,3] - Mc2_Metab[Zn1,4]*MC2_Met_LayerTrans[Zn1,4]
      # MC2_Met_SomTrans[Zn2,1] = Mc_Met_LitSomTrans[Zn2] - Mc2_Metab[Zn2,1]*MC2_Met_LayerTrans[Zn2,1]
      # MC2_Met_SomTrans[Zn2,2] = 0*Mc_Met_LitSomTrans[Zn2] + Mc2_Metab[Zn2,1]*MC2_Met_LayerTrans[Zn2,1] - Mc2_Metab[Zn2,2]*MC2_Met_LayerTrans[Zn2,2]
      # MC2_Met_SomTrans[Zn2,3] = 0*Mc_Met_LitSomTrans[Zn2] + Mc2_Metab[Zn2,2]*MC2_Met_LayerTrans[Zn2,2] - Mc2_Metab[Zn2,3]*MC2_Met_LayerTrans[Zn2,3]
      # MC2_Met_SomTrans[Zn2,4] = 0*Mc_Met_LitSomTrans[Zn2] + Mc2_Metab[Zn2,3]*MC2_Met_LayerTrans[Zn2,3] - Mc2_Metab[Zn2,4]*MC2_Met_LayerTrans[Zn2,4]
      # MC2_Met_SomTrans[Zn3,1] = Mc_Met_LitSomTrans[Zn3] - Mc2_Metab[Zn3,1]*MC2_Met_LayerTrans[Zn3,1]
      # MC2_Met_SomTrans[Zn3,2] = 0*Mc_Met_LitSomTrans[Zn3] + Mc2_Metab[Zn3,1]*MC2_Met_LayerTrans[Zn3,1] - Mc2_Metab[Zn3,2]*MC2_Met_LayerTrans[Zn3,2]
      # MC2_Met_SomTrans[Zn3,3] = 0*Mc_Met_LitSomTrans[Zn3] + Mc2_Metab[Zn3,2]*MC2_Met_LayerTrans[Zn3,2] - Mc2_Metab[Zn3,3]*MC2_Met_LayerTrans[Zn3,3]
      # MC2_Met_SomTrans[Zn3,4] = 0*Mc_Met_LitSomTrans[Zn3] + Mc2_Metab[Zn3,3]*MC2_Met_LayerTrans[Zn3,3] - Mc2_Metab[Zn3,4]*MC2_Met_LayerTrans[Zn3,4]
      # MC2_Met_SomTrans[Zn4,1] = Mc_Met_LitSomTrans[Zn4] - Mc2_Metab[Zn4,1]*MC2_Met_LayerTrans[Zn4,1]
      # MC2_Met_SomTrans[Zn4,2] = 0*Mc_Met_LitSomTrans[Zn4] + Mc2_Metab[Zn4,1]*MC2_Met_LayerTrans[Zn4,1] - Mc2_Metab[Zn4,2]*MC2_Met_LayerTrans[Zn4,2]
      # MC2_Met_SomTrans[Zn4,3] = 0*Mc_Met_LitSomTrans[Zn4] + Mc2_Metab[Zn4,2]*MC2_Met_LayerTrans[Zn4,2] - Mc2_Metab[Zn4,3]*MC2_Met_LayerTrans[Zn4,3]
      # MC2_Met_SomTrans[Zn4,4] = 0*Mc_Met_LitSomTrans[Zn4] + Mc2_Metab[Zn4,3]*MC2_Met_LayerTrans[Zn4,3] - Mc2_Metab[Zn4,4]*MC2_Met_LayerTrans[Zn4,4]
      
      
      zonelayer_df$Mc2_Metab_Layer <- zonelayer_df$Mc2_Metab-zonelayer_df$MC2_Met_LayerTrans
      
      
      MC2_Met_SomTrans[Zn1,1] = Mc_Met_LitSomTrans[Zn1]                                                - Mc2_Metab[Zn1,1]*MC2_Met_LayerTrans[Zn1,1]
      MC2_Met_SomTrans[Zn1,2] = 0*Mc_Met_LitSomTrans[Zn1] + Mc2_Metab[Zn1,1]*MC2_Met_LayerTrans[Zn1,1] - Mc2_Metab[Zn1,2]*MC2_Met_LayerTrans[Zn1,2]
      MC2_Met_SomTrans[Zn1,3] = 0*Mc_Met_LitSomTrans[Zn1] + Mc2_Metab[Zn1,2]*MC2_Met_LayerTrans[Zn1,2] - Mc2_Metab[Zn1,3]*MC2_Met_LayerTrans[Zn1,3]
      MC2_Met_SomTrans[Zn1,4] = 0*Mc_Met_LitSomTrans[Zn1] + Mc2_Metab[Zn1,3]*MC2_Met_LayerTrans[Zn1,3] - Mc2_Metab[Zn1,4]*MC2_Met_LayerTrans[Zn1,4]
      MC2_Met_SomTrans[Zn2,1] = Mc_Met_LitSomTrans[Zn2]                                                - Mc2_Metab[Zn2,1]*MC2_Met_LayerTrans[Zn2,1]
      MC2_Met_SomTrans[Zn2,2] = 0*Mc_Met_LitSomTrans[Zn2] + Mc2_Metab[Zn2,1]*MC2_Met_LayerTrans[Zn2,1] - Mc2_Metab[Zn2,2]*MC2_Met_LayerTrans[Zn2,2]
      MC2_Met_SomTrans[Zn2,3] = 0*Mc_Met_LitSomTrans[Zn2] + Mc2_Metab[Zn2,2]*MC2_Met_LayerTrans[Zn2,2] - Mc2_Metab[Zn2,3]*MC2_Met_LayerTrans[Zn2,3]
      MC2_Met_SomTrans[Zn2,4] = 0*Mc_Met_LitSomTrans[Zn2] + Mc2_Metab[Zn2,3]*MC2_Met_LayerTrans[Zn2,3] - Mc2_Metab[Zn2,4]*MC2_Met_LayerTrans[Zn2,4]
      MC2_Met_SomTrans[Zn3,1] = Mc_Met_LitSomTrans[Zn3]                                                - Mc2_Metab[Zn3,1]*MC2_Met_LayerTrans[Zn3,1]
      MC2_Met_SomTrans[Zn3,2] = 0*Mc_Met_LitSomTrans[Zn3] + Mc2_Metab[Zn3,1]*MC2_Met_LayerTrans[Zn3,1] - Mc2_Metab[Zn3,2]*MC2_Met_LayerTrans[Zn3,2]
      MC2_Met_SomTrans[Zn3,3] = 0*Mc_Met_LitSomTrans[Zn3] + Mc2_Metab[Zn3,2]*MC2_Met_LayerTrans[Zn3,2] - Mc2_Metab[Zn3,3]*MC2_Met_LayerTrans[Zn3,3]
      MC2_Met_SomTrans[Zn3,4] = 0*Mc_Met_LitSomTrans[Zn3] + Mc2_Metab[Zn3,3]*MC2_Met_LayerTrans[Zn3,3] - Mc2_Metab[Zn3,4]*MC2_Met_LayerTrans[Zn3,4]
      MC2_Met_SomTrans[Zn4,1] = Mc_Met_LitSomTrans[Zn4]                                                - Mc2_Metab[Zn4,1]*MC2_Met_LayerTrans[Zn4,1]
      MC2_Met_SomTrans[Zn4,2] = 0*Mc_Met_LitSomTrans[Zn4] + Mc2_Metab[Zn4,1]*MC2_Met_LayerTrans[Zn4,1] - Mc2_Metab[Zn4,2]*MC2_Met_LayerTrans[Zn4,2]
      MC2_Met_SomTrans[Zn4,3] = 0*Mc_Met_LitSomTrans[Zn4] + Mc2_Metab[Zn4,2]*MC2_Met_LayerTrans[Zn4,2] - Mc2_Metab[Zn4,3]*MC2_Met_LayerTrans[Zn4,3]
      MC2_Met_SomTrans[Zn4,4] = 0*Mc_Met_LitSomTrans[Zn4] + Mc2_Metab[Zn4,3]*MC2_Met_LayerTrans[Zn4,3] - Mc2_Metab[Zn4,4]*MC2_Met_LayerTrans[Zn4,4]
      
      
      
      # Mc2_Metab[Zone,SoilLayer](t) = Mc2_Metab[Zone,SoilLayer](t - dt) + (Mc2_MetabIn[Zone,SoilLayer] + MC2_Met_SomTrans[Zone,SoilLayer] - Mc2_MetabActF[Zone,SoilLayer] - Mc2_MetabCO2In[Zone,SoilLayer]) * dt
      zonelayer_df$Mc2_Metab <- zonelayer_df$Mc2_Metab + (zonelayer_df$Mc2_MetabIn + zonelayer_df$MC2_Met_SomTrans[Zone,SoilLayer] - zonelayer_df$Mc2_MetabActF[Zone,SoilLayer] - zonelayer_df$Mc2_MetabCO2In[Zone,SoilLayer])
      
      # Mc2_Act[Zone,SoilLayer](t) = Mc2_Act[Zone,SoilLayer](t - dt) + (Mc2_MetabActF[Zone,SoilLayer] + Mc2_StrucActF[Zone,SoilLayer] + MC2_Act_SomTrans[Zone,SoilLayer] - Mc2_ActCO2In[Zone,SoilLayer] - Mc2_ActSlwF[Zone,SoilLayer] - Mc2_ActPassF[Zone,SoilLayer]) * dt
      Mc2_Act[Zone,SoilLayer](t) = Mc2_Act[Zone,SoilLayer](t - dt) + (Mc2_MetabActF[Zone,SoilLayer] + Mc2_StrucActF[Zone,SoilLayer] + MC2_Act_SomTrans[Zone,SoilLayer] - Mc2_ActCO2In[Zone,SoilLayer] - Mc2_ActSlwF[Zone,SoilLayer] - Mc2_ActPassF[Zone,SoilLayer]) * dt
      
      # Mn2_ActInit[Zone](t) = Mn2_ActInit[Zone](t - dt) + (- Mn2_ActInitF[Zone]) * dt
      Mn2_ActInit[Zone](t) = Mn2_ActInit[Zone](t - dt) + (- Mn2_ActInitF[Zone]) * dt
      
      # Mn2_SlwInit[Zone](t) = Mn2_SlwInit[Zone](t - dt) + (- Mn2_SlwInitF[Zone]) * dt
      Mn2_SlwInit[Zone](t) = Mn2_SlwInit[Zone](t - dt) + (- Mn2_SlwInitF[Zone]) * dt
      
      # Mn2_PassInit[Zone](t) = Mn2_PassInit[Zone](t - dt) + (- Mn2_PassInitF[Zone]) * dt
      Mn2_PassInit[Zone](t) = Mn2_PassInit[Zone](t - dt) + (- Mn2_PassInitF[Zone]) * dt
      
      # Mc2_Slw[Zone,SoilLayer](t) = Mc2_Slw[Zone,SoilLayer](t - dt) + (Mc2_StrucSlwF[Zone,SoilLayer] + Mc2_ActSlwF[Zone,SoilLayer] + Mc2_Slw_SomTrans[Zone,SoilLayer] - Mc2_SlwPassF[Zone,SoilLayer] - Mc2_SlwCO2In[Zone,SoilLayer]) * dt
      Mc2_Slw[Zone,SoilLayer](t) = Mc2_Slw[Zone,SoilLayer](t - dt) + (Mc2_StrucSlwF[Zone,SoilLayer] + Mc2_ActSlwF[Zone,SoilLayer] + Mc2_Slw_SomTrans[Zone,SoilLayer] - Mc2_SlwPassF[Zone,SoilLayer] - Mc2_SlwCO2In[Zone,SoilLayer]) * dt
      
      # Mc2_Struc[Zone,SoilLayer](t) = Mc2_Struc[Zone,SoilLayer](t - dt) + (Mc2_StrucIn[Zone,SoilLayer] + MC2_Struc_SomT[Zone,SoilLayer] - Mc2_StrucActF[Zone,SoilLayer] - Mc2_StrucSlwF[Zone,SoilLayer] - Mc_StrucSlwCO2In_SOM[Zone,SoilLayer] - Mc2_StrucActCO2In[Zone,SoilLayer]) * dt
      Mc2_Struc[Zone,SoilLayer](t) = Mc2_Struc[Zone,SoilLayer](t - dt) + (Mc2_StrucIn[Zone,SoilLayer] + MC2_Struc_SomT[Zone,SoilLayer] - Mc2_StrucActF[Zone,SoilLayer] - Mc2_StrucSlwF[Zone,SoilLayer] - Mc_StrucSlwCO2In_SOM[Zone,SoilLayer] - Mc2_StrucActCO2In[Zone,SoilLayer]) * dt
      
      # Mc2_Pass[Zone,SoilLayer](t) = Mc2_Pass[Zone,SoilLayer](t - dt) + (Mc2_SlwPassF[Zone,SoilLayer] + Mc2_ActPassF[Zone,SoilLayer] + MC2_Pass_SomTrans[Zone,SoilLayer] - Mc2_PassCO2In[Zone,SoilLayer]) * dt
      Mc2_Pass[Zone,SoilLayer](t) = Mc2_Pass[Zone,SoilLayer](t - dt) + (Mc2_SlwPassF[Zone,SoilLayer] + Mc2_ActPassF[Zone,SoilLayer] + MC2_Pass_SomTrans[Zone,SoilLayer] - Mc2_PassCO2In[Zone,SoilLayer]) * dt
      
      # PD_NastiesinPlot[Animals](t) = PD_NastiesinPlot[Animals](t - dt) + (PD_ChangeAnPop[Animals]) * dt
      PD_NastiesinPlot[Animals](t) = PD_NastiesinPlot[Animals](t - dt) + (PD_ChangeAnPop[Animals]) * dt
      
      # C_BiomHarvestPast(t) = C_BiomHarvestPast(t - dt) + (C_BiomHarvestEvent) * dt
      C_BiomHarvestPast(t) = C_BiomHarvestPast(t - dt) + (C_BiomHarvestEvent) * dt
      
      # T_PrunPast(t) = T_PrunPast(t - dt) + (T_PrunEvent) * dt
      T_PrunPast(t) = T_PrunPast(t - dt) + (T_PrunEvent) * dt
      
      # TF_CurrentLeafNo[Tree](t) = TF_CurrentLeafNo[Tree](t - dt) + (TF_NewLeaf?[Tree] - TF_NewLeaffall?[Tree]) * dt
      TF_CurrentLeafNo[Tree](t) = TF_CurrentLeafNo[Tree](t - dt) + (TF_NewLeaf?[Tree] - TF_NewLeaffall?[Tree]) * dt
      
      
      

      # T_CanBionTimCum[PlantComp,Tree](t) = T_CanBionTimCum[PlantComp,Tree](t - dt) + (T_CanBiomTimHarv[PlantComp,Tree]) * dt
      T_CanBionTimCum[PlantComp,Tree](t) = T_CanBionTimCum[PlantComp,Tree](t - dt) + (T_CanBiomTimHarv[PlantComp,Tree]) * dt
      
      # T_CBSlashCum[PlantComp,Tree](t) = T_CBSlashCum[PlantComp,Tree](t - dt) + (T_CanBiomSlashed[PlantComp,Tree]) * dt
      T_CBSlashCum[PlantComp,Tree](t) = T_CBSlashCum[PlantComp,Tree](t - dt) + (T_CanBiomSlashed[PlantComp,Tree]) * dt
      
      # T_CumRtType2Decay[PlantComp,Tree](t) = T_CumRtType2Decay[PlantComp,Tree](t - dt) + (T_RootDecay[PlantComp,Tree]) * dt
      T_CumRtType2Decay[PlantComp,Tree](t) = T_CumRtType2Decay[PlantComp,Tree](t - dt) + (T_RootDecay[PlantComp,Tree]) * dt

      # T_Fruit[PlantComp,Tree](t) = T_Fruit[PlantComp,Tree](t - dt) + (T_FruitInc[PlantComp,Tree] - T_Frug_and_Littfall[PlantComp,Tree] - T_PrunFruit[PlantComp,Tree] - T_RipeFruitHarvest[PlantComp,Tree] - T_FruitSlashed[PlantComp,Tree]) * dt
      T_Fruit[PlantComp,Tree](t) = T_Fruit[PlantComp,Tree](t - dt) + (T_FruitInc[PlantComp,Tree] - T_Frug_and_Littfall[PlantComp,Tree] - T_PrunFruit[PlantComp,Tree] - T_RipeFruitHarvest[PlantComp,Tree] - T_FruitSlashed[PlantComp,Tree]) * dt
      
      # T_FruitCum[PlantComp,Tree](t) = T_FruitCum[PlantComp,Tree](t - dt) + (T_FruitHarvInc[PlantComp,Tree]) * dt
      T_FruitCum[PlantComp,Tree](t) = T_FruitCum[PlantComp,Tree](t - dt) + (T_FruitHarvInc[PlantComp,Tree]) * dt
      
      # T_GroRes[PlantComp,Tree](t) = T_GroRes[PlantComp,Tree](t - dt) + (T_GroResInc[PlantComp,Tree] - T_CanBiomInc[PlantComp,Tree] - T_FruitInc[PlantComp,Tree] - T_LatexFormation[PlantComp,Tree] - T_GroResLoss[PlantComp,Tree] - T_Respiration[PlantComp,Tree] - T_RootInc[PlantComp,Tree] - T_GroResStorInc[PlantComp,Tree]) * dt
      T_GroRes[PlantComp,Tree](t) = T_GroRes[PlantComp,Tree](t - dt) + (T_GroResInc[PlantComp,Tree] - T_CanBiomInc[PlantComp,Tree] - T_FruitInc[PlantComp,Tree] - T_LatexFormation[PlantComp,Tree] - T_GroResLoss[PlantComp,Tree] - T_Respiration[PlantComp,Tree] - T_RootInc[PlantComp,Tree] - T_GroResStorInc[PlantComp,Tree]) * dt
      
      # T_GroResLossCum[PlantComp,Tree](t) = T_GroResLossCum[PlantComp,Tree](t - dt) + (T_GroResLoss[PlantComp,Tree]) * dt
      T_GroResLossCum[PlantComp,Tree](t) = T_GroResLossCum[PlantComp,Tree](t - dt) + (T_GroResLoss[PlantComp,Tree]) * dt
      
      # T_GrowStor[PlantComp,Tree](t) = T_GrowStor[PlantComp,Tree](t - dt) + (T_GroResStorInc[PlantComp,Tree]) * dt
      T_GrowStor[PlantComp,Tree](t) = T_GrowStor[PlantComp,Tree](t - dt) + (T_GroResStorInc[PlantComp,Tree]) * dt
      
      # T_HeartWood[PlantComp,Tree](t) = T_HeartWood[PlantComp,Tree](t - dt) + (T_HeartWoodInc[PlantComp,Tree]) * dt
      T_HeartWood[PlantComp,Tree](t) = T_HeartWood[PlantComp,Tree](t - dt) + (T_HeartWoodInc[PlantComp,Tree]) * dt
      
      # T_LatexStock[PlantComp,Tree](t) = T_LatexStock[PlantComp,Tree](t - dt) + (T_LatexFormation[PlantComp,Tree] - T_Tapping[PlantComp,Tree]) * dt
      T_LatexStock[PlantComp,Tree](t) = T_LatexStock[PlantComp,Tree](t - dt) + (T_LatexFormation[PlantComp,Tree] - T_Tapping[PlantComp,Tree]) * dt
      
      
      # T_LifallCum[PlantComp,Tree](t) = T_LifallCum[PlantComp,Tree](t - dt) + (T_LifallInc[PlantComp,Tree] - T_LifallTreeDies[PlantComp,Tree]) * dt
      T_LifallCum[PlantComp,Tree](t) = T_LifallCum[PlantComp,Tree](t - dt) + (T_LifallInc[PlantComp,Tree] - T_LifallTreeDies[PlantComp,Tree]) * dt
      
      # T_PrunCum[PlantComp,Tree](t) = T_PrunCum[PlantComp,Tree](t - dt) + (T_Prun[PlantComp,Tree] - T_PrunCumTreeDies[PlantComp,Tree]) * dt
      T_PrunCum[PlantComp,Tree](t) = T_PrunCum[PlantComp,Tree](t - dt) + (T_Prun[PlantComp,Tree] - T_PrunCumTreeDies[PlantComp,Tree]) * dt
      
      # T_RtType0CumInput[PlantComp,Tree](t) = T_RtType0CumInput[PlantComp,Tree](t - dt) + (T_RtType0Input[PlantComp,Tree]) * dt
      T_RtType0CumInput[PlantComp,Tree](t) = T_RtType0CumInput[PlantComp,Tree](t - dt) + (T_RtType0Input[PlantComp,Tree]) * dt
      
      # T_RtType2Biomass[PlantComp,Tree](t) = T_RtType2Biomass[PlantComp,Tree](t - dt) + (T_RootInc[PlantComp,Tree] - T_RootDecay[PlantComp,Tree]) * dt
      T_RtType2Biomass[PlantComp,Tree](t) = T_RtType2Biomass[PlantComp,Tree](t - dt) + (T_RootInc[PlantComp,Tree] - T_RootDecay[PlantComp,Tree]) * dt
      
      # T_SapWood[PlantComp,Tree](t) = T_SapWood[PlantComp,Tree](t - dt) + (T_WoodInc[PlantComp,Tree] + T_WoodIni[PlantComp,Tree] - T_WoodHarvest[PlantComp,Tree] - T_Lignovory[PlantComp,Tree] - T_WoodSlashed[PlantComp,Tree] - T_HeartWoodInc[PlantComp,Tree]) * dt
      T_SapWood[PlantComp,Tree](t) = T_SapWood[PlantComp,Tree](t - dt) + (T_WoodInc[PlantComp,Tree] + T_WoodIni[PlantComp,Tree] - T_WoodHarvest[PlantComp,Tree] - T_Lignovory[PlantComp,Tree] - T_WoodSlashed[PlantComp,Tree] - T_HeartWoodInc[PlantComp,Tree]) * dt
      
      # T_TappedLatex[PlantComp,Tree](t) = T_TappedLatex[PlantComp,Tree](t - dt) + (T_Tapping[PlantComp,Tree]) * dt
      T_TappedLatex[PlantComp,Tree](t) = T_TappedLatex[PlantComp,Tree](t - dt) + (T_Tapping[PlantComp,Tree]) * dt
      
      # T_WoodHarvCum[PlantComp,Tree](t) = T_WoodHarvCum[PlantComp,Tree](t - dt) + (T_WoodHarvest[PlantComp,Tree]) * dt
      T_WoodHarvCum[PlantComp,Tree](t) = T_WoodHarvCum[PlantComp,Tree](t - dt) + (T_WoodHarvest[PlantComp,Tree]) * dt
      
      # TF_LeafTime[Tree](t) = TF_LeafTime[Tree](t - dt) + (TF_LeafClockTicks?[Tree]) * dt
      TF_LeafTime[Tree](t) = TF_LeafTime[Tree](t - dt) + (TF_LeafClockTicks?[Tree]) * dt
      
      # TF_FruitsperBunch[Tree,Fruitbunch](t) = TF_FruitsperBunch[Tree,Fruitbunch](t - dt) + (TF_FruitNoPassOn[Tree,Fruitbunch] - TF_FruitNumbLoss[Tree,Fruitbunch] - TF_FruitHarvNoperBunch[Tree,Fruitbunch]) * dt
      TF_FruitsperBunch[Tree,Fruitbunch](t) = TF_FruitsperBunch[Tree,Fruitbunch](t - dt) + (TF_FruitNoPassOn[Tree,Fruitbunch] - TF_FruitNumbLoss[Tree,Fruitbunch] - TF_FruitHarvNoperBunch[Tree,Fruitbunch]) * dt
      
      # T_WoodHarvPast[Tree](t) = T_WoodHarvPast[Tree](t - dt) + (T_WoodHarvEvent[Tree]) * dt
      T_WoodHarvPast[Tree](t) = T_WoodHarvPast[Tree](t - dt) + (T_WoodHarvEvent[Tree]) * dt
      
      # T_PrunWeighTot[Tree](t) = T_PrunWeighTot[Tree](t - dt)
      T_PrunWeighTot[Tree](t) = T_PrunWeighTot[Tree](t - dt)
      
      
      # N_Stock1[Zone,SlNut](t) = N_Stock1[Zone,SlNut](t - dt) + (N_In1[Zone,SlNut] + N_NutMobil1[Zone,SlNut] + Mn_Mineralization[Zone,SlNut] + N_SomMin1Exch[Zone,SlNut] - N_T1Upt1[Zone,SlNut] - N_CUpt1[Zone,SlNut] - N_T2Upt1[Zone,SlNut] - N_T3Upt1[Zone,SlNut] - N_LatOutFlow11[Zone,SlNut]) * dt
      # N_Stock2[Zone,SlNut](t) = N_Stock2[Zone,SlNut](t - dt) + (N_In2[Zone,SlNut] + N_NutMobil2[Zone,SlNut] + N_SomMin2Exch[Zone,SlNut] - N_T1Upt2[Zone,SlNut] - N_CUpt2[Zone,SlNut] - N_T2Upt2[Zone,SlNut] - N_T3Upt2[Zone,SlNut] - N_LatOutFlow21[Zone,SlNut]) * dt
      # N_Stock3[Zone,SlNut](t) = N_Stock3[Zone,SlNut](t - dt) + (N_In3[Zone,SlNut] + N_NutMobil3[Zone,SlNut] + N_SomMin3Exch[Zone,SlNut] - N_T1Upt3[Zone,SlNut] - N_CUpt3[Zone,SlNut] - N_T2Upt3[Zone,SlNut] - N_T3Upt3[Zone,SlNut] - N_LatOutFlow31[Zone,SlNut]) * dt
      # N_Stock4[Zone,SlNut](t) = N_Stock4[Zone,SlNut](t - dt) + (N_In4[Zone,SlNut] + N_NutMobil4[Zone,SlNut] + N_SomMin4Exch[Zone,SlNut] - N_T1Upt4[Zone,SlNut] - N_CUpt4[Zone,SlNut] - N_T2Upt4[Zone,SlNut] - N_T3Upt4[Zone,SlNut] - N_LatOutFlow41[Zone,SlNut] - N_LeachOutx4[Zone,SlNut]) * dt
      N_Stock1[Zone,SlNut](t) = N_Stock1[Zone,SlNut](t - dt) + (N_In1[Zone,SlNut] + N_NutMobil1[Zone,SlNut] + Mn_Mineralization[Zone,SlNut] + N_SomMin1Exch[Zone,SlNut] - N_T1Upt1[Zone,SlNut] - N_CUpt1[Zone,SlNut] - N_T2Upt1[Zone,SlNut] - N_T3Upt1[Zone,SlNut] - N_LatOutFlow11[Zone,SlNut]) * dt
      N_Stock2[Zone,SlNut](t) = N_Stock2[Zone,SlNut](t - dt) + (N_In2[Zone,SlNut] + N_NutMobil2[Zone,SlNut] + N_SomMin2Exch[Zone,SlNut] - N_T1Upt2[Zone,SlNut] - N_CUpt2[Zone,SlNut] - N_T2Upt2[Zone,SlNut] - N_T3Upt2[Zone,SlNut] - N_LatOutFlow21[Zone,SlNut]) * dt
      N_Stock3[Zone,SlNut](t) = N_Stock3[Zone,SlNut](t - dt) + (N_In3[Zone,SlNut] + N_NutMobil3[Zone,SlNut] + N_SomMin3Exch[Zone,SlNut] - N_T1Upt3[Zone,SlNut] - N_CUpt3[Zone,SlNut] - N_T2Upt3[Zone,SlNut] - N_T3Upt3[Zone,SlNut] - N_LatOutFlow31[Zone,SlNut]) * dt
      N_Stock4[Zone,SlNut](t) = N_Stock4[Zone,SlNut](t - dt) + (N_In4[Zone,SlNut] + N_NutMobil4[Zone,SlNut] + N_SomMin4Exch[Zone,SlNut] - N_T1Upt4[Zone,SlNut] - N_CUpt4[Zone,SlNut] - N_T2Upt4[Zone,SlNut] - N_T3Upt4[Zone,SlNut] - N_LatOutFlow41[Zone,SlNut] - N_LeachOutx4[Zone,SlNut]) * dt
      
      # S&B_Topsoil_pH[Zone](t) = S&B_Topsoil_pH[Zone](t - dt) + (S&B_pH_change[Zone]) * dt
      S&B_Topsoil_pH[Zone](t) = S&B_Topsoil_pH[Zone](t - dt) + (S&B_pH_change[Zone]) * dt
      
      # RT3_CoarseRt[Tree](t) = RT3_CoarseRt[Tree](t - dt) + (RT3_CRincr[Tree] - RT3_CRdecay[Tree]) * dt
      RT3_CoarseRt[Tree](t) = RT3_CoarseRt[Tree](t - dt) + (RT3_CRincr[Tree] - RT3_CRdecay[Tree]) * dt
      
      # C_WeedSeedBank[Zone,PlantComp](t) = C_WeedSeedBank[Zone,PlantComp](t - dt) + (C_WeedSeedInflux[Zone,PlantComp] + C_WeedSeedFall[Zone,PlantComp] - C_WeedSeedDecay[Zone,PlantComp] - C_WeedGermin[Zone,PlantComp]) * dt
      C_WeedSeedBank[Zone,PlantComp](t) = C_WeedSeedBank[Zone,PlantComp](t - dt) + (C_WeedSeedInflux[Zone,PlantComp] + C_WeedSeedFall[Zone,PlantComp] - C_WeedSeedDecay[Zone,PlantComp] - C_WeedGermin[Zone,PlantComp]) * dt
      
      
      
      
      
      
      
#   return(soil_df)
# }
