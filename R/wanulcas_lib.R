### WANULCAS FUNCTION AND DATA LIBRARY ###########
library(yaml)
library(openxlsx2)

maxval <- 1e+308

# set the active folder similar with this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### Defaul Parameters ###############

yaml_handler <- list(
  seq = unlist,
  'bool#no' = function(x)
    ifelse(x == "N", x, T)
)

default_params <- read_yaml("R/default_params.yaml", handlers = yaml_handler)

### Functions ###############

generate_graph_functions <- function(graphs) {
  sapply(graphs, function(x) {
    method = "linear"
    if (x$type == "discrete") {
      method <- "constant"
    }
    sapply(x$xy_data, function(d, m) {
      approxfun(
        unlist(d$x_val),
        unlist(d$y_val),
        rule = 2,
        ties = list("ordered", mean),
        method = m
      )
    }, m = method, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)
}

delay_timer <- list()
delay_value <- list()

#' Adopted 'Delay' function on STELLA:
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
  if (!is.na(suppressWarnings(as.numeric(name)))) {
    name <- "default"
  }
  
  if (is.na(suppressWarnings(as.numeric(delay_duration)))) {
    print(name)
    print(input)
    print(delay_duration)
    delay_duration <- 0
  }
  
  if (delay_duration < 1) {
    return(input)
  }
  
  if (is.null(delay_timer[[name]]) ||
      length(delay_timer[[name]]) == 0) {
    if (!is.na(input) && !is.null(input)) {
      delay_timer[[name]] <<- delay_duration
      delay_value[[name]] <<- input
    }
    return(default_val)
  }
  
  delay_timer[[name]] <<- delay_timer[[name]] - 1
  
  if (!is.na(input) && !is.null(input)) {
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

#' Reset the delay memory
reset_delay_memory <- function() {
  delay_timer <<- list()
  delay_value <<- list()
}


### ARRAY def #########################

wanulcas_arr_dim <- list(
  nzone = 4,
  nlayer = 4,
  ntree = 3,
  ncrop = 5,
  npcomp = 3,
  nprice = 2,
  nanimals = 7,
  nlimit = 3,
  ncpools = 5,
  nfruit = 22,
  nstage = 2,
  nbuf = 10,
  ninp = 2,
  nwater = 4,
  nnut = 2,
  SlNut = c("N", "P"),
  PlantComp = c("DW", "N", "P"),
  PriceType = c("Private", "Social"),
  Animals = c(
    "Pigs",
    "Monkeys",
    "Grasshoppers",
    "Nematodes",
    "Goats",
    "Buffalo",
    "Birds"
  ),
  Limiting_Factors = c("Water", "N", "P"),
  CENT_Pools = c("Met", "Str", "Actv", "Slow", "Pass"),
  Fruitbunch = c(
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
  ),
  InsectLifeStage = c("Larvae", "Adults"),
  Tree_Stage = c("VegGen", "LeafAge"),
  BufValues = 1:10,
  ExtOrgInputs = 1:2,
  soil_water_id = c("MS", "MW", "S", "W"),
  Calender = 1:12
)

get_wanulcas_def_arr <- function() {
  for (v in names(wanulcas_arr_dim)) {
    assign(v, wanulcas_arr_dim[[v]])
  }
  
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
  
  zonecrop_df <- data.frame(
    zone = rep(zone_df$zone, ncrop),
    crop_id = rep(crop_df$crop_id, each = nzone)
  )
  
  zonelayertree_df <- zonelayer_df[rep(seq_len(nrow(zonelayer_df)), ntree), ]
  zonelayertree_df$tree_id <- rep(1:ntree, each = nrow(zonelayer_df))
  
  zonenut_df <- data.frame(zone = rep(zone_df$zone, length(SlNut)),
                           SlNut = rep(SlNut, each = nzone))
  
  layertree_df <- data.frame(
    layer = rep(layer_df$layer, length(ntree)),
    tree_id = rep(1:ntree, each = nlayer)
  )
  
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
  
  limit_df <- data.frame(Limiting_Factors = Limiting_Factors)
  
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
  
  zonelayertreebuf_df <- zonelayertree_df[rep(seq_len(nrow(zonelayertree_df)), nrow(buf_df)), ]
  zonelayertreebuf_df$buf_id <- rep(buf_df$buf_id, each = nrow(zonelayertree_df))
  
  angle_df <- data.frame(angle_id = 1:35)
  
  nut_df <- data.frame(SlNut = SlNut)
  treenut_df <- data.frame(tree_id = rep(tree_df$tree_id, nrow(nut_df)))
  treenut_df$SlNut <- rep(nut_df$SlNut, each = ntree)
  
  animal_df <- data.frame(Animals = Animals)
  zoneanimal_df <- data.frame(zone = rep(zone_df$zone, length(Animals)))
  zoneanimal_df$Animals <- rep(Animals, each = nzone)
  treeanimal_df <- data.frame(tree_id = rep(tree_df$tree_id, length(Animals)))
  treeanimal_df$Animals <- rep(Animals, each = ntree)
  
  fruit_df <- data.frame(Fruitbunch = Fruitbunch)
  
  treefruit_df <- data.frame(tree_id = rep(tree_df$tree_id, length(Fruitbunch)))
  treefruit_df$Fruitbunch <- rep(Fruitbunch, each = ntree)
  
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
  
  cpools_df <- data.frame(CENT_Pools = CENT_Pools)
  
  zonecpools_df <- data.frame(zone = rep(zone_df$zone, length(CENT_Pools)))
  zonecpools_df$CENT_Pools <- rep(CENT_Pools, each = nzone)
  
  zonelayercpools_df <- zonelayer_df[rep(seq_len(nrow(zonelayer_df)), length(CENT_Pools)), ]
  zonelayercpools_df$CENT_Pools <- rep(CENT_Pools, each = nrow(zonelayer_df))
  
  price_df <- data.frame(PriceType = PriceType)
  
  inpprice_df <- data.frame(ExtOrgInputs = rep(inp_df$ExtOrgInputs, length(PriceType)))
  inpprice_df$PriceType <- rep(PriceType, each = nrow(inp_df))
  
  nutprice_df <- data.frame(SlNut = rep(nut_df$SlNut, length(PriceType)))
  nutprice_df$PriceType <- rep(PriceType, each = nrow(nut_df))
  
  zoneprice_df <- data.frame(zone = rep(zone_df$zone, length(PriceType)))
  zoneprice_df$PriceType <- rep(PriceType, each = nzone)
  
  treeprice_df <- data.frame(tree_id = rep(tree_df$tree_id, length(PriceType)))
  treeprice_df$PriceType <- rep(PriceType, each = ntree)
  
  cropprice_df <- data.frame(crop_id = rep(crop_df$crop_id, length(PriceType)))
  cropprice_df$PriceType <- rep(PriceType, each = nrow(crop_df))
  
  zonecropprice_df <- zonecrop_df[rep(seq_len(nrow(zonecrop_df)), length(PriceType)), ]
  zonecropprice_df$PriceType <- rep(PriceType, each = nrow(zonecrop_df))
  
  cropnut_df <- data.frame(crop_id = rep(crop_df$crop_id, nnut))
  cropnut_df$SlNut <- rep(SlNut, each = ncrop)
  
  croplayer_df <- data.frame(crop_id = rep(crop_df$crop_id, nlayer))
  croplayer_df$layer <- rep(layer_df$layer, each = ncrop)
  
  cropanimal_df <- data.frame(crop_id = rep(crop_df$crop_id, nanimals))
  cropanimal_df$Animals <- rep(Animals, each = ncrop)
  
  calender_df <- data.frame(Calender = Calender)
  
  arr_init <- list(
    angle_df = angle_df,
    animal_df = animal_df,
    buf_df = buf_df,
    cpools_df = cpools_df,
    crop_df = crop_df,
    cropanimal_df = cropanimal_df,
    croplayer_df = croplayer_df,
    cropnut_df = cropnut_df,
    cropprice_df = cropprice_df,
    fruit_df = fruit_df,
    inp_df = inp_df,
    inpprice_df = inpprice_df,
    layer_df = layer_df,
    layernut_df = layernut_df,
    layertree_df = layertree_df,
    layertreenut_df = layertreenut_df,
    limit_df = limit_df,
    nut_df = nut_df,
    nutinp_df = nutinp_df,
    nutprice_df = nutprice_df,
    pcomp_df = pcomp_df,
    price_df = price_df,
    single_df = data.frame(time = 1),
    tree_df = tree_df,
    treeanimal_df = treeanimal_df,
    treebuf_df = treebuf_df,
    treefruit_df = treefruit_df,
    treelimit_df = treelimit_df,
    treenut_df = treenut_df,
    treepcomp_df = treepcomp_df,
    treeprice_df = treeprice_df,
    treestage_df = treestage_df,
    zone_df = zone_df,
    zoneanimal_df = zoneanimal_df,
    zonebuf_df = zonebuf_df,
    zonecpools_df = zonecpools_df,
    zonecrop_df = zonecrop_df,
    zonecropprice_df = zonecropprice_df,
    zoneinp_df = zoneinp_df,
    zonelayer_df = zonelayer_df,
    zonelayerbuf_df = zonelayerbuf_df,
    zonelayercpools_df = zonelayercpools_df,
    zonelayernut_df = zonelayernut_df,
    zonelayerpcomp_df = zonelayerpcomp_df,
    zonelayertree_df = zonelayertree_df,
    zonelayertreebuf_df = zonelayertreebuf_df,
    zonelayertreenut_df = zonelayertreenut_df,
    zonelayertreepcomp_df = zonelayertreepcomp_df,
    zonelimit_df = zonelimit_df,
    zonenut_df = zonenut_df,
    zonepcomp_df = zonepcomp_df,
    zoneprice_df = zoneprice_df,
    zonetree_df = zonetree_df,
    zonetreebuf_df = zonetreebuf_df,
    zonetreenut_df = zonetreenut_df,
    zonelayerwater_df = zonelayerwater_df,
    zonetreewater_df = zonetreewater_df,
    zonelayertreewater_df = zonelayertreewater_df,
    calender_df = calender_df
  )
  
  return(arr_init)
}

wanulcas_def_arr <- get_wanulcas_def_arr()

# ### Stock Vars ##########################
get_wanulcas_stock_vars <- function() {
  a <- wanulcas_def_arr
  
  stock_vars = list(
    single_vars = list(
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
      C_BiomHarvestPast = 0,
      CA_PastFertApp = 0,
      E_PloughPast = 0,
      EVAP_YesterdayTTransp = 0,
      LF_UphillGWStore = 0,
      P_CropHarvMarker = 0,
      P_CumLabUse = 0,
      P_CurrentCropCB = 0,
      P_FlagPrevCropOK_is = 1,
      PD_FencePast = 0,
      PD_FenceQ = 0,
      RAIN_Cum = 0,
      RAIN_IntercEvapCum = 0,
      T_PrunPast = 0
    ),
    zone_df = cbind(
      a$zone_df,
      data.frame(
        C_CropDays = 0,
        C_Height = 0,
        C_HydEqFluxes = 0,
        CW_DemandPerRoot = 1,
        E_CumSoilInflowZn = 0,
        E_SoilLossCumZn = 0,
        EVAP_SlashWater = 0,
        EVAP_SurfCum = 0,
        EVAP_WoodMoist = 0,
        GHG_AnaerobIndic = 0,
        GHG_N2_EmZn = 0,
        GHG_Net_CH4_Em = 0,
        GHG_Nitric_Oxide_EmZn = 0,
        GHG_Nitrous_Oxide_EmZn = 0,
        LIGHT_CRefRelCapCum = 0,
        MC_ActCO2 = 0,
        MC_MetabCO2 = 0,
        MC_PassCO2 = 0,
        MC_SlwCO2 = 0,
        MC_StrucActCO2 = 0,
        MC_StrucSlwCO2 = 0,
        MC2_ActCO2 = 0,
        MC2_MetabCO2 = 0,
        MC2_OrgC_Leached = 0,
        MC2_PassCO2 = 0,
        MC2_SlwCO2 = 0,
        MC2_StrucActCO2 = 0,
        MC2_StrucSlwCO2 = 0,
        MN2_OrgNLeached = 0,
        MP2_OrgP_Leached = 0,
        N15_C_GroRes = 0,
        N15_C_ResidCum = 0,
        N15_C_Root = 0,
        N15_C_StLv = 0,
        N15_C_YieldCurr = 0,
        RAIN_AnaerInd = 0,
        RAIN_CanopyWater = 0,
        SB_DWlossfromBurn = 0,
        SB_AerosolProd = 0,
        SB_PsorpModifier = 1,
        SB_WatRetentionMod = 0,
        W_DrainCumV = 0
      )
    ),
    zonelayer_df = cbind(
      a$zonelayer_df,
      data.frame(
        MN2_MinNutpool = 0,
        MP2_MinNutpool = 0,
        N_N2LossCum = 0,
        W_UptCCum = 0
      )
    ),
    # zonetree_df = cbind(a$zonetree_df, data.frame(T_LifallWeight = 1)),
    zonepcomp_df = cbind(
      a$zonepcomp_df,
      data.frame(
        SB_DeadWood = 0,
        SB_FineNecromass = 0,
        SB_Nutvolatilized = 0,
        SB_ScorchWoodRemoved = 0,
        C_BiomHarvestCum = 0,
        C_BiomStLv = 0,
        C_GroRes = 0,
        C_HarvestCum = 0,
        C_ResidRecycle = 0,
        C_ResidRemoved = 0,
        C_YieldCurr = 0
      )
    ),
    zonelimit_df = cbind(a$zonelimit_df, data.frame(C_CumLim = -1)),
    zonecrop_df = cbind(a$zonecrop_df, data.frame(C_AgronYPerType = 0)),
    zonenut_df = cbind(
      a$zonenut_df,
      data.frame(
        C_NDfaTot = 0,
        MN_FertOnSoil = 0,
        MN_MinNutpool = 0,
        N_LeachCumV = 0
      )
    ),
    treestage_df = cbind(a$treestage_df, data.frame(T_Stage = 0)),
    zonelayertree_df = cbind(a$zonelayertree_df, data.frame(W_UptCum = 0)),
    zonelayernut_df = cbind(a$zonelayernut_df, data.frame(N_UptCCum = 0)),
    zonelayerpcomp_df = cbind(a$zonelayerpcomp_df, data.frame(C_Root = 0)),
    zonelayertreenut_df = cbind(a$zonelayertreenut_df, data.frame(N_UptTCum = 0)),
    layer_df = cbind(a$layer_df, data.frame(
      LF_HDrain1Cum = 0, LF_LatInCum = 0
    )),
    layernut_df = cbind(
      a$layernut_df,
      data.frame(N_LatInflowCum = 0, N_LatOutflowCum = 0)
    ),
    tree_df = cbind(
      a$tree_df,
      data.frame(
        BW_UptTCum = 0,
        SB_PastSlashEvents = 0,
        T_Compl = 0,
        T_CumWatStress = 0,
        T_GrowDays = 0,
        T_HeartWoodDiam = 0,
        T_HydEqFluxes = 0,
        T_PanelAlreadyInitiated_is = 0,
        T_PanelAvailable = 0,
        T_PrunLapse = 1000,
        T_SecTimePanelAvailable = 0,
        T_StemBefPruning = 0,
        T_TapDaysCum = 0,
        T_TotTappingDays = 0,
        T_WoodH = 0,
        T_WoodHarvPast = 0,
        TF_CumBunchNo = 0,
        TF_CumBunchWeightHarvest = 0,
        TF_CumFruitsHarvNo = 0,
        TF_CumOilHarvest = 0,
        TF_CumOilProd = 0,
        TF_DelayedFrond_BiomassTarget = 0,
        TF_DW_Sufficiency_Yesterday = 1,
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
        TP_RelNutSupplyDelayed = 1,
        TW_DemandPerRoot = 1
      )
    ),
    treepcomp_df = cbind(
      a$treepcomp_df,
      data.frame(
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
        T_RtType0CumInput = 0,
        T_RtType2Biomass = 0,
        T_SapWood = 0,
        T_TappedLatex = 0,
        T_WoodHarvCum = 0,
        TP_ParasiteBiomass = 0,
        P_TPrunNo = 0
      )
    ),
    treenut_df = cbind(
      a$treenut_df,
      data.frame(BN_TNFixAmountCum = 0, BN_TUptCum = 0)
    ),
    treelimit_df = cbind(a$treelimit_df, data.frame(T_CumLim = 0)),
    treefruit_df = cbind(
      a$treefruit_df,
      data.frame(
        TF_FruitsperBunch = 0,
        TF_BunchGender = 0,
        TF_BunchWeight_kgpbunch = 0
      )
    ),
    nut_df = cbind(
      a$nut_df,
      data.frame(
        BN_CUptCum = 0,
        BN_ExtOrgInputs = 0,
        BN_FertCum = 0,
        BN_RunOffLoss = 0,
        BN_TreeInit = 0,
        CA_PastImmInp = 0,
        CENT_ExtOrgInputs = 0,
        CENT_LitterNInflow = 0,
        CENT_NOut = 0,
        CENT2_LitSomTransfAcc = 0,
        CENT2_NOut = 0,
        CENT2_SOMNinflow = 0,
        N_CumAtmInput = 0
      )
    ),
    pcomp_df = cbind(
      a$pcomp_df,
      data.frame(
        G_CumFeedSufficiency = 0,
        G_Grazed_Manure_Output = 0,
        G_Grazed_Respired = 0,
        G_GrazedBiomCum = 0,
        G_LivestWeightGain = 0,
        T_RootDecCum = 0
      )
    ),
    price_df = cbind(
      a$price_df,
      data.frame(
        P_CCostsTot = 0,
        P_CReturnTot = 0,
        P_GeneralCosts = 0,
        P_Initial_NPV = 0,
        P_TCostsTot = 0,
        P_TReturnTot = 0
      )
    ),
    inpprice_df = cbind(a$inpprice_df, data.frame(P_CostExtOrg = 1)),
    animal_df = cbind(a$animal_df, data.frame(PD_NastiesinPlot = 0))
  )
}

CQ_Curr_Vars <- data.frame(
  var = c(
    "CQ_CRelLUECurr",
    "CQ_CLWRCurr",
    "CQ_CHarvAllocCurr",
    "CQ_CSLACurr",
    "CQ_CRtAllocCurr"
  ),
  graph = c(
    "CQ_CRelLUE",
    "CQ_CLWR",
    "CQ_CHarvAlloc",
    "CQ_CSLA",
    "CQ_CRtAlloc"
  )
)

CQ_CType_params <- c(
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
  CQ_CType = rep(1:5, each = 4),
  zone = rep(1:4, 5),
  coln = CQ_CType_params
)

### XLS input params ###############

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

CQ_Unit <- c(
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

CQ_vars <- c(
  "CQ_CTimeGenCurr",
  "CQ_CTimeVegCurr",
  "CQ_SingleCycle_is_Cur",
  "CQ_DOYFlwDOYBegin",
  "CQ_DOYFlwEnd",
  "CQ_GroMaxCurr",
  "CQ_GSeedCurr",
  "CQ_TranspRatioCurr",
  "CQ_HBiomConvCurr",
  "CQ_RemobFrac",
  "CQ_kLightCurr",
  "CQ_RelLightMaxCurr",
  "C_LAIMax",
  "CQ_RainWStorCapCurr",
  "CQ_ConductivityCurr",
  "CQ_PotSuctAlphMaxCurr",
  "CQ_PotSuctAlphMinCurr",
  "CQ_ClosedCanCurr",
  "ConcOldN",
  "ConcOldP",
  "ConcYN",
  "ConcYP",
  "CQ_NRtConcCurr",
  "CQ_NFixVariable_is_Curr",
  "CQ_NFixDailyFracCurr",
  "CQ_NFixRespCurr",
  "CQ_NFixDWMaxFracCurr",
  "CQ_NFixDWUnitCostCurr",
  "CQ_RtDiam",
  "Lrvm1",
  "Lrvm2",
  "Lrvm3",
  "Lrvm4",
  "RT_CLraC",
  "RT_CDecDepthC",
  "RT_CSRLCurr",
  "RT_CHalfLifeCurr",
  "RT_CRtAllocRespCurr",
  "RT_CDistResp",
  "CQ_MaxMycInf",
  "N_CRhizKaPMod",
  "NutMobN",
  "NutMobP",
  "CQ_LignResidCurr",
  "CQ_LignRootResCurr",
  "CQ_PolyResid",
  "CQ_PolyRt",
  "CQ_CovEffCurr",
  "EatenbyPigs?",
  "EatenbyMonkeys?",
  "EatenbyLocusts?",
  "EatenbyNematode?",
  "EatenbyGoat?",
  "EatenbyBuffalo?",
  "EatenbyBirds?",
  "CQ_AgronYieldMoistFrac",
  "P_CPlantLab",
  "P_CWeedLab",
  "P_CPestConLab",
  "P_CHarvLab",
  "P_CFertLab",
  "SeedS",
  "SeedP",
  "YieldS",
  "YieldP"
)

crop_df_vars <- c(
  "CQ_CTimeGenCurr",
  "CQ_CTimeVegCurr",
  "CQ_SingleCycle_is_Cur",
  "CQ_DOYFlwDOYBegin",
  "CQ_DOYFlwEnd",
  "CQ_GroMaxCurr",
  "CQ_GSeedCurr",
  "CQ_TranspRatioCurr",
  "CQ_HBiomConvCurr",
  "CQ_RemobFrac",
  "CQ_kLightCurr",
  "CQ_RelLightMaxCurr",
  "C_LAIMax",
  "CQ_RainWStorCapCurr",
  "CQ_ConductivityCurr",
  "CQ_PotSuctAlphMaxCurr",
  "CQ_PotSuctAlphMinCurr",
  "CQ_ClosedCanCurr",
  "CQ_NRtConcCurr",
  "CQ_NFixVariable_is_Curr",
  "CQ_NFixDailyFracCurr",
  "CQ_NFixRespCurr",
  "CQ_NFixDWMaxFracCurr",
  "CQ_NFixDWUnitCostCurr",
  "CQ_RtDiam",
  "RT_CLraC",
  "RT_CDecDepthC",
  "RT_CSRLCurr",
  "RT_CHalfLifeCurr",
  "RT_CRtAllocRespCurr",
  "RT_CDistResp",
  "CQ_MaxMycInf",
  "N_CRhizKaPMod",
  "CQ_LignResidCurr",
  "CQ_LignRootResCurr",
  "CQ_PolyResid",
  "CQ_PolyRt",
  "CQ_CovEffCurr",
  "CQ_AgronYieldMoistFrac",
  "P_CPlantLab",
  "P_CWeedLab",
  "P_CPestConLab",
  "P_CHarvLab",
  "P_CFertLab"
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

T_params <- c(
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
  'T_Rubber_is',
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
  'T_ConcRT_N',
  'T_ConcGroRes_P',
  'T_LfConc_P',
  'T_ConcTwig_P',
  'T_ConcWood_P',
  'T_ConcFruit_P',
  'T_ConcRT_P',
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
  'RT_TDiam',
  'RT_TLrvL1_Zn1',
  'RT_TLrvL1_Zn2',
  'RT_TLrvL1_Zn3',
  'RT_TLrvL1_Zn4',
  'RT_TLrvL2_Zn1',
  'RT_TLrvL2_Zn2',
  'RT_TLrvL2_Zn3',
  'RT_TLrvL2_Zn4',
  'RT_TLrvL3_Zn1',
  'RT_TLrvL3_Zn2',
  'RT_TLrvL3_Zn3',
  'RT_TLrvL3_Zn4',
  'RT_TLrvL4_Zn1',
  'RT_TLrvL4_Zn2',
  'RT_TLrvL4_Zn3',
  'RT_TLrvL4_Zn4',
  'RT_TLraX0',
  'RT_TDistShapeC',
  'RT_TDecDepthC',
  'RT_THalfLife',
  'RT_TDistResp',
  'RT_TAllocResp',
  'RT_TAlloc',
  'T_DiamRtLeng1',
  'T_DiamSlopeRtLeng',
  'T_DiamRtWght1',
  'T_DiamSlopeRtWght',
  'RT_TProxGini',
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
  "TF_MThreshtoWatStress",
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

zonelayertree_df_params <- c(
  'RT_TLrvL1_Zn1',
  'RT_TLrvL1_Zn2',
  'RT_TLrvL1_Zn3',
  'RT_TLrvL1_Zn4',
  'RT_TLrvL2_Zn1',
  'RT_TLrvL2_Zn2',
  'RT_TLrvL2_Zn3',
  'RT_TLrvL2_Zn4',
  'RT_TLrvL3_Zn1',
  'RT_TLrvL3_Zn2',
  'RT_TLrvL3_Zn3',
  'RT_TLrvL3_Zn4',
  'RT_TLrvL4_Zn1',
  'RT_TLrvL4_Zn2',
  'RT_TLrvL4_Zn3',
  'RT_TLrvL4_Zn4'
)

tree_df_params <- c(
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
  'T_Rubber_is',
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
  'T_LifallDroughtFrac',
  'T_LiFallThreshWStress',
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
  'RT_TDiam',
  'RT_TLraX0',
  'RT_TDistShapeC',
  'RT_TDecDepthC',
  'RT_THalfLife',
  'RT_TDistResp',
  'RT_TAllocResp',
  'RT_TAlloc',
  'T_DiamRtLeng1',
  'T_DiamSlopeRtLeng',
  'T_DiamRtWght1',
  'T_DiamSlopeRtWght',
  'RT_TProxGini',
  'T_MycMaxInf',
  'N_TRhizKaPMod',
  'SB_TTempTol',
  'E_CovEffT',
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
  "TF_MThreshtoWatStress",
  "TF_FirstBudtoFlowerInit",
  "TF_DWCostUOil",
  "TF_FruitWaterPot",
  "TF_CritFruitWaterPot",
  "TF_WatStressAbortFrac",
  "TF_AbRelSizePow"
)

TF_TargetOilperBunch_params <- c(
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
  "TF_TargetOilperBunch_Anthesis6"
)

TF_StageAbortSens_params <- c(
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

Pinit_params <- c(
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

Eatenby_params <- c(
  "EatenbyPigs?",
  "EatenbyMonkeys?",
  "EatenbyLocusts?",
  "EatenbyNematode?",
  "EatenbyGoat?",
  "EatenbyBuffalo?",
  "EatenbyBirds?"
)

RT_TLrvL_params <- c(
  "RT_TLrvL1_Zn1",
  "RT_TLrvL1_Zn2",
  "RT_TLrvL1_Zn3",
  "RT_TLrvL1_Zn4",
  "RT_TLrvL2_Zn1",
  "RT_TLrvL2_Zn2",
  "RT_TLrvL2_Zn3",
  "RT_TLrvL2_Zn4",
  "RT_TLrvL3_Zn1",
  "RT_TLrvL3_Zn2",
  "RT_TLrvL3_Zn3",
  "RT_TLrvL3_Zn4",
  "RT_TLrvL4_Zn1",
  "RT_TLrvL4_Zn2",
  "RT_TLrvL4_Zn3",
  "RT_TLrvL4_Zn4"
)

PD_TEatenBy_params <-
  c(
    "PD_TEatenBy_Pigs",
    "PD_TEatenBy_Monkeys",
    "PD_TEatenBy_Grasshoppers",
    "PD_TEatenBy_Nematodes",
    "PD_TEatenBy_Goats",
    "PD_TEatenBy_Buffalo",
    "PD_TEatenBy_Birds"
  )


TF_FemSinkperFruit_params <- c(
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
  "TF_FemaleSinkperFruit_Anthesis6"
)

TF_MaleSinkperBunch_params <- c(
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
  "TF_MaleSinkperBunch_Anthesis6"
)

apply_xls_params <- function(xls_input_file, wanulcas_params = NULL) {
  if (is.null(wanulcas_params)) {
    wpars <- default_params
  } else {
    wpars <- wanulcas_params
  }
  
  parxls_df <- wb_to_df(xls_input_file, "LinkToStella")
  par_names <- names(parxls_df)
  xls_df <- read.csv("xls_param_config.csv")
  
  get_par_xls_list <- function(unit_labels, coluMN_names) {
    l <- as.list(parxls_df[1:length(unit_labels), coluMN_names])
    names(l) <- unit_labels
    return(l)
  }
  
  ### vars ################
  
  AF_System_par <- get_par_xls_list(AF_Unit, "AF_System")
  wpars$vars$AF_Circ <- AF_System_par$`Parkland?`
  wpars$vars$AF_ZoneTot <- AF_System_par[["AFTotZn"]]
  
  T_PrunOption_var <- c("T_PrunPlant_is", "T_PrunType_is", "C_BiomHarv_is")
  wpars$vars[T_PrunOption_var] <- get_par_xls_list(T_PrunOption_var, "T_PrunOption")
  
  PFAll <- get_par_xls_list(PF_UnitAll, "P_ParamAll")
  wpars$vars$P_BurnLab <- PFAll[["BurnLab"]]
  wpars$vars$P_DiscountRate <- PFAll[["Discrate"]]
  
  ### arrays ################
  
  zw <- as.vector(unlist(AF_System_par[c("AFZn1", "AFZn2", "AFZn3")]))
  wpars$arrays$zone_df$vars$AF_ZoneWidth <- c(zw, wpars$vars$AF_ZoneTot - sum(zw))
  if (wpars$vars$AF_Circ == 0) {
    wpars$arrays$zone_df$vars$AF_ZoneFrac <- wpars$arrays$zone_df$vars$AF_ZoneWidth / wpars$vars$AF_ZoneTot
  } else {
    AF_ZWcum <- cumsum(arrays$zone_df$vars$AF_ZoneWidth)
    AF_ZWcum_0 <- c(0, head(AF_ZWcum, -1))
    wpars$arrays$zone_df$vars$AF_ZoneFrac <- (AF_ZWcum^2 -  AF_ZWcum_0^2) / wpars$vars$AF_ZoneTot^2
  }
  
  wpars$arrays$layer_df$vars$AF_DepthLay <- as.numeric(AF_System_par[c("SL1", "SL2", "SL3", "SL4")])
  wpars$arrays$tree_df$vars$AF_TreePosit <- as.numeric(AF_System_par[c("TSp1", "TSp2", "TSp3")])
  wpars$arrays$tree_df$vars$T_RelPosinZone <- as.numeric(AF_System_par[c("TRelSp1", "TRelSp2", "TRelSp3")])
  wpars$arrays$tree_df$vars$T_Treesperha <- as.numeric(AF_System_par[c("TDensSp1", "TDensSp2", "TDensSp3")])
  wpars$arrays$zonelayer_df$vars$N_Init <- parxls_df[1:16, "N_Init"]
  
  N_PStParam <- get_par_xls_list(Pinit_params, "N_PStParam")
  wpars$arrays$zonelayer_df$vars$N_PStParam <- as.numeric(N_PStParam[c(
    "Pinit11",
    "Pinit21",
    "Pinit31",
    "Pinit41",
    "Pinit12",
    "Pinit22",
    "Pinit32",
    "Pinit42",
    "Pinit13",
    "Pinit23",
    "Pinit33",
    "Pinit43",
    "Pinit14",
    "Pinit24",
    "Pinit34",
    "Pinit44"
  )])
  wpars$arrays$layer_df$vars$PStMin <- as.numeric(N_PStParam[c("PStMin_1", "PStMin_2", "PStMin_3", "PStMin_4")])
  wpars$arrays$layer_df$vars$PStMax <- as.numeric(N_PStParam[c("PStMax1", "PStMax2", "PStMax3", "PStMax4")])
  
  #### crop params ###############
  P_ParamC <- c("P_ParamC1",
                "P_ParamC2",
                "P_ParamC3",
                "P_ParamC4",
                "P_ParamC5")
  P_ParamC_df <- parxls_df[1:9, P_ParamC]
  
  cq_pars <- c(
    "Cq_Parameters1",
    "Cq_Parameters2",
    "Cq_Parameters3",
    "Cq_Parameters4",
    "Cq_Parameters5"
  )
  CQ_df <- parxls_df[1:length(CQ_Unit), cq_pars]
  names(P_ParamC_df) <- names(CQ_df)
  CQ_df <- rbind(CQ_df, P_ParamC_df)
  tCQ_df <- as.data.frame(t(CQ_df))
  colnames(tCQ_df) <- CQ_vars
  
  pCQ_df <- tCQ_df[, crop_df_vars]
  CQ_list <- as.list(pCQ_df)
  wpars$arrays$crop_df$vars[names(CQ_list)] <- CQ_list
  
  wpars$arrays$cropnut_df$vars$CQ_NConcYoungCurr <- c(tCQ_df$ConcYN, tCQ_df$ConcYP)
  wpars$arrays$cropnut_df$vars$CQ_NConcOldCurr <- c(tCQ_df$ConcOldN, tCQ_df$ConcOldP)
  wpars$arrays$cropnut_df$vars$N_CNutMob <- c(tCQ_df$NutMobN, tCQ_df$NutMobP)
  wpars$arrays$cropanimal_df$vars$PD_CropsEaten_is <-  unlist(tCQ_df[c(
    "EatenbyPigs?",
    "EatenbyMonkeys?",
    "EatenbyLocusts?",
    "EatenbyNematode?",
    "EatenbyGoat?",
    "EatenbyBuffalo?",
    "EatenbyBirds?"
  )])
  wpars$arrays$croplayer_df$vars$Lrvm <-  unlist(tCQ_df[c("Lrvm1", "Lrvm2", "Lrvm3", "Lrvm4")])
  wpars$arrays$cropprice_df$vars$P_PriceCSeed <- c(tCQ_df$SeedP, tCQ_df$SeedS)
  wpars$arrays$cropprice_df$vars$P_CYieldPrice <- c(tCQ_df$YieldP, tCQ_df$YieldS)
  
  #### tree params ###############
  T_df <- parxls_df[1:length(T_params), c("T_Par1", "T_Par2", "T_Par3")]
  tree_par_df <- as.data.frame(t(T_df))
  names(tree_par_df) <- T_params
  
  T_par_list <- as.list(tree_par_df[tree_df_params])
  wpars$arrays$tree_df$vars[names(T_par_list)] <- T_par_list
  
  wpars$arrays$treefruit_df$vars$TF_StageAbortSens <- as.numeric(unlist(tree_par_df[TF_StageAbortSens_params]))
  wpars$arrays$treefruit_df$vars$TF_TargetOilperBunch <- as.numeric(unlist(tree_par_df[TF_TargetOilperBunch_params]))
  wpars$arrays$treefruit_df$vars$TF_FemSinkperFruit <- as.numeric(unlist(tree_par_df[TF_FemSinkperFruit_params]))
  wpars$arrays$treefruit_df$vars$TF_MaleSinkperBunch <- as.numeric(unlist(tree_par_df[TF_MaleSinkperBunch_params]))
  wpars$arrays$treeanimal_df$vars$PD_TEatenBy_is <- as.numeric(unlist(tree_par_df[PD_TEatenBy_params]))
  
  RT_TLrvL_df <- tree_par_df[RT_TLrvL_params]
  wpars$arrays$zonelayertree_df$vars$RT_TLrvL_par <- as.numeric(unlist(c(
    RT_TLrvL_df[1, ], RT_TLrvL_df[2, ], RT_TLrvL_df[3, ]
  )))
  
  wpars$arrays$treenut_df$vars$T_NutMob <- c(tree_par_df$T_NutMobT_N, tree_par_df$T_NutMobT_P)
  wpars$arrays$treepcomp_df$vars$T_LfConc <- c(1, 1, 1, tree_par_df$T_LfConc_N, tree_par_df$T_LfConc_P)
  wpars$arrays$treepcomp_df$vars$T_TwigConc <- c(1, 1, 1, tree_par_df$T_ConcTwig_N, tree_par_df$T_ConcTwig_N)
  wpars$arrays$treepcomp_df$vars$T_FruitConc <- c(1,
                                                  1,
                                                  1,
                                                  tree_par_df$T_ConcFruit_N,
                                                  tree_par_df$T_ConcFruit_P)
  wpars$arrays$treepcomp_df$vars$T_GroResConc <- c(1,
                                                   1,
                                                   1,
                                                   tree_par_df$T_ConcGroRes_N,
                                                   tree_par_df$T_ConcGroRes_P)
  wpars$arrays$treepcomp_df$vars$T_RtConc <- c(1, 1, 1, tree_par_df$T_ConcRT_N, tree_par_df$T_ConcRT_P)
  wpars$arrays$treepcomp_df$vars$T_WoodConc <- c(1, 1, 1, tree_par_df$T_ConcWood_N, tree_par_df$T_ConcWood_P)
  wpars$arrays$treepcomp_df$vars$T_LifallRed <- c(0,
                                                  0,
                                                  0,
                                                  tree_par_df$T_NLifallRed_N,
                                                  tree_par_df$T_NLifallRed_P)
  
  
  PF_UnitTree_var <- c(
    "P_TPlantLab",
    "P_TPrunLab",
    "P_TFruitHarvLab",
    "P_TWoodHarvLab",
    "P_TLatexHarvLab",
    "P_TFertLab",
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
  P_ParamT <- c("P_ParamT1", "P_ParamT2", "P_ParamT3")
  a <- parxls_df[1:length(PF_UnitTree_var), P_ParamT]
  
  PT_df <- parxls_df[1:length(PF_UnitTree_var), c("P_ParamT1", "P_ParamT2", "P_ParamT3")]
  P_ParamT_df <- as.data.frame(t(PT_df))
  names(P_ParamT_df) <- PF_UnitTree_var
  
  wpars$arrays$tree_df$vars[PF_UnitTree_var[1:6]] <- P_ParamT_df[1:6]
  wpars$arrays$treeprice_df$vars$P_TFruitPrice <- c(P_ParamT_df$FruitP, P_ParamT_df$FruitS)
  wpars$arrays$treeprice_df$vars$P_TLatexPrice <- c(P_ParamT_df$LatexP, P_ParamT_df$LatexS)
  wpars$arrays$treeprice_df$vars$P_TPrunPrice <- c(P_ParamT_df$PrunP, P_ParamT_df$PrunS)
  wpars$arrays$treeprice_df$vars$P_TWoodPrice <- c(P_ParamT_df$WoodP, P_ParamT_df$WoodS)
  wpars$arrays$treeprice_df$vars$P_TSeedPrice <- c(P_ParamT_df$SeedP, P_ParamT_df$SeedS)
  
  #### soil params ###############
  S_SoilProp_par <- get_par_xls_list(S_Unit, "S_SoilProp")
  S_SoilProp_var <- list(
    S_KsatInitV = c("Ksat1", "Ksat2", "Ksat3", "Ksat4"),
    S_KsatDefV = c("KsatD1", "KsatD2", "KsatD3", "KsatD4"),
    W_FieldCapKcrit = c("FieldC1", "FieldC2", "FieldC3", "FieldC4"),
    W_ThetaInacc = c("Inacc1", "Inacc2", "Inacc3", "Inacc4"),
    W_BDLayer = c("BDLayer1", "BDLayer2", "BDLayer3", "BDLayer4"),
    SiltLayer = c("SiltLayer1", "SiltLayer2", "SiltLayer3", "SiltLayer4"),
    ClayLayer = c("ClayLayer1", "ClayLayer2", "ClayLayer3", "ClayLayer4"),
    W_ThetaSat = c("ThetaSat1", "ThetaSat2", "ThetaSat3", "ThetaSat4"),
    W_Alpha = c("Alpha1", "Alpha2", "Alpha3", "Alpha4"),
    W_n = c("n1", "n2", "n3", "n4")
  )
  wpars$arrays$layer_df$vars[names(S_SoilProp_var)] <- lapply(S_SoilProp_var, function(x)
    as.numeric(S_SoilProp_par[x]))
  
  #### price params ###############
  wpars$arrays$nutprice_df$vars$P_PriceFert <- c(PFAll[["FertNP"]], PFAll[["FertPP"]], PFAll[["FertNS"]], PFAll[["FertPS"]])
  wpars$arrays$price_df$vars$P_CPestContPrice <- c(PFAll[["PestP"]], PFAll[["PestS"]])
  wpars$arrays$price_df$vars$P_UnitLabCost <- c(PFAll[["UnitLabP"]], PFAll[["LabUnitS"]])
  wpars$arrays$price_df$vars$P_FenceMatCost <- c(PFAll[["FenceP"]], PFAll[["FenceS"]])
  
  ### graphs ################
  
  for (i in 1:nrow(xls_df)) {
    r <- xls_df[i, ]
    if (r$width == 1) {
      wpars$graphs[[r$group]]$xy_data[[r$var]]$y_val <- parxls_df[1:r$n, r$xls]
    } else {
      vi <- which(par_names == r$xls)
      df <- parxls_df[1:r$n, vi:(vi + r$width - 1)]
      for (j in 1:r$width) {
        wpars$graphs[[r$group]]$xy_data[[j]]$y_val <- df[[j]]
      }
    }
  }
  
  return(wpars)
}


write_params <- function(wanulcas_params, filename) {
  s <- "  "
  t <- c("vars:")
  p <- wanulcas_params$vars
  p <- p[order(names(p))]
  t <- c(t, paste0(s, names(p), ": ", p))
  
  pa <- wanulcas_params$arrays
  t <- c(t, "arrays:")
  for (a in sort(names(pa))) {
    t <- c(t, paste0(s, a, ":"))
    t <- c(t, paste0(s, s, "keys:"))
    for (k in names(pa[[a]]$keys)) {
      t <- c(t, paste0(s, s, s, k, ": [", paste(pa[[a]]$keys[[k]], collapse = ", "), "]"))
    }
    t <- c(t, paste0(s, s, "vars:"))
    for (v in sort(names(pa[[a]]$vars))) {
      t <- c(t, paste0(s, s, s, v, ": [", paste(pa[[a]]$vars[[v]], collapse = ", "), "]"))
    }
  }
  
  gx <- wanulcas_params$graphs
  t <- c(t, "graphs:")
  for (g in sort(names(gx))) {
    t <- c(t, paste0(s, g, ":"))
    t <- c(t, paste0(s, s, "type: ", gx[[g]]$type))
    t <- c(t, paste0(s, s, "x_var: ", gx[[g]]$x_var))
    t <- c(t, paste0(s, s, "xy_data: "))
    for (d in names(gx[[g]]$xy_data)) {
      t <- c(t, paste0(s, s, s, d, ": "))
      x <- gx[[g]]$xy_data[[d]]$x_val
      y <- gx[[g]]$xy_data[[d]]$y_val
      t <- c(t, paste0(s, s, s, s, "x_val: [", paste0(x, collapse = ", "), "]"))
      t <- c(t, paste0(s, s, s, s, "y_val: [", paste0(y, collapse = ", "), "]"))
    }
  }
  write(t, filename)
}



### Rain ###########################


# Rain Type 2
get_simulated_rain <- function(RAIN_DoY, p, RAIN_Yesterday_is) {
  #TODO: some of codes can be initiated outside the function, so it wouldn't executed inside the loop!
  
  DoY <- c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
  
  RAIN_Random1 <- runif(1)
  RAIN_Random2 <- runif(1)
  
  RAIN_Pattern_MonthlyMean <- (p$vars$RAIN_MonthlyMean_RainfallMin / p$vars$RAIN_MonthlyMean_RainfallMax) *
    p$vars$RAIN_Shape_Max
  RAIN_PatternMonthlyMean_minusShapeMin <- RAIN_Pattern_MonthlyMean - p$vars$RAIN_Shape_Min
  
  RAIN_CumWet_Season1 <- p$graphs$RAIN_CumDay$xy_data$RAIN_CumDay1$y_val[p$vars$RAIN_WettestMonth_Season1]
  RAIN_CumWet_Season2 <- p$graphs$RAIN_CumDay$xy_data$RAIN_CumDay2$y_val[p$vars$RAIN_WettestMonth_Season2]
  
  
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
  
  RAIN_pattern_s1 <- p$vars$RAIN_Shape_Max - p$vars$RAIN_Pattern1_Max +
    (1 - RAIN_PatternMonthlyMean_minusShapeMin) *
    RAIN_season_f(RAIN_CumWet_Season1, p$vars$RAIN_Peakines_Season1) -
    p$vars$RAIN_OffsetValue
  
  RAIN_Pattern2 <- NULL
  if (p$vars$RAIN_UniorBimodial_is == 2) {
    RAIN_Pattern2 <- max(
      p$vars$RAIN_OffsetValue,
      RAIN_pattern_s1  +
        (1 - p$vars$RAIN_Probability) *
        (RAIN_Pattern_MonthlyMean - p$vars$RAIN_Pattern1_Min) *
        (1 - RAIN_PatternMonthlyMean_minusShapeMin) *
        max(
          p$vars$RAIN_OffsetValue,
          RAIN_season_f(RAIN_CumWet_Season2, p$vars$RAIN_Peakines_Season2) - p$vars$RAIN_OffsetValue
        )
    )
  } else {
    RAIN_Pattern2 <- p$vars$RAIN_Probability *
      max(p$vars$RAIN_OffsetValue, RAIN_pattern_s1)
  }
  # RAIN_Numberof_WetDaypD = if RAIN_DoY <= 31 then RAIN_Numberof_WetDaypM[january]else
  #   if RAIN_DoY <= 59 then RAIN_Numberof_WetDaypM[february] else
  
  imonth <- min(which(DoY >= RAIN_DoY, arr.ind = TRUE))
  RAIN_Numberof_WetDaypD <- p$arrays$calender_df$vars$RAIN_Numberof_WetDaypM[imonth]
  RAIN_Numberof_DaysperMonth <- p$arrays$calender_df$vars$RAIN_Numberof_DaysperMonth[imonth]
  RAIN_RelWet_PersistencepD <- p$arrays$calender_df$vars$RAIN_RelWet_PersistencepM[imonth]
  RAIN_MonthlyMean_TotalRainfall <- p$arrays$calender_df$vars$RAIN_MonthlyMean_TotalRainfall[imonth]
  RAIN_DaysIn_Order <- RAIN_Numberof_DaysperMonth
  
  # Rain_MeanDailyon_WetDayspM[Calender] = IF Rain_Numberof_WetDaypM[Calender]=0 then 0 else Rain_MonthlyMean_TotalRainfall[Calender]/Rain_Numberof_WetDaypM[Calender]
  RAIN_MeanDailyon_WetDayspM <- ifelse(
    RAIN_Numberof_WetDaypM == 0,
    0,
    RAIN_MonthlyMean_TotalRainfall / RAIN_Numberof_WetDaypM
  )
  RAIN_MeanDailyon_WetDayspD <- RAIN_MeanDailyon_WetDayspM
  
  # RAIN_Wet_Fraction = (RAIN_Numberof_WetDaypD/RAIN_DaysIn_Order)*(RAIN_Pattern2/RAIN_Shape_Max)
  RAIN_Wet_Fraction <- (RAIN_Numberof_WetDaypD / RAIN_DaysIn_Order) * (RAIN_Pattern2 / p$vars$RAIN_Shape_Max)
  
  # RAIN_ProbabilityofDry = IF RAIN_Wet_Fraction=1 then 0 else (RAIN_Wet_Fraction*(1-RAIN_RelWet_PersistencepD*RAIN_Wet_Fraction)/(1-RAIN_Wet_Fraction))
  RAIN_ProbabilityofDry <- ifelse(
    RAIN_Wet_Fraction == 1,
    0,
    RAIN_Wet_Fraction * (1 - RAIN_RelWet_PersistencepD * RAIN_Wet_Fraction) /
      (1 - RAIN_Wet_Fraction)
  )
  
  # Rain? = IF (RAIN_Yesterday?=1 and RAIN_Random1<RAIN_Wet_Fraction*RAIN_RelWet_PersistencepD) or (RAIN_Yesterday?=0 and RAIN_Random1<RAIN_ProbabilityofDry) then 1 else 0
  is_Rain <- 0
  if ((RAIN_Yesterday_is &&
       RAIN_Random1 < RAIN_Wet_Fraction * RAIN_RelWet_PersistencepD) ||
      (!RAIN_Yesterday_is &&
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
  if (exp(p$vars$RAIN_Gamma) * ((-log(1 - RAIN_Random2))^(1 / p$vars$RAIN_Weibull_Param)) +
      1 > 0) {
    RAIN_Type2 <- is_Rain * (((RAIN_MeanDailyon_WetDayspD - 1) / exp(p$vars$RAIN_Gamma)) * ((-log(1 - RAIN_Random2))^(1 / p$vars$RAIN_Weibull_Param)) + 1)
  }
  return(RAIN_Type2)
}

# Rain Type 3
get_random_rain <- function(RAIN_DoY, p) {
  # RAIN_Today? = IF(Time=int(time)AND(Random(0,1,(RAIN_GenSeed+1))<RAIN_DayProp))THEN(1) ELSE(0)
  RAIN_DayProp <- p$graph_functions$RAIN_DayProp$RAIN_DayProp(RAIN_DoY)
  RAIN_Today <- runif(1) < RAIN_DayProp
  if (!RAIN_Today)
    return(0)
  
  # RAIN_fRandom = IF(Random(0,1,(RAIN_GenSeed+2))<RAIN_HeavyP)THEN(MAX(RAIN_BoundHeaLi,
  # NORMAL(RAIN_Heavy ,RAIN_CoefVar3*RAIN_Heavy,(RAIN_GenSeed+3))))ELSE (MIN(MAX(0.5,NORMAL(RAIN_Light,5,RAIN_GenSeed+3 )),RAIN_BoundHeaLi))
  RAIN_fRandom <- ifelse(
    runif(1) < p$vars$RAIN_HeavyP,
    max(
      p$vars$RAIN_BoundHeaLi,
      rnorm(
        1,
        p$vars$RAIN_Heavy ,
        p$vars$RAIN_CoefVar3 * p$vars$RAIN_Heavy
      )
    ),
    min(max(0.5, rnorm(
      1, p$vars$RAIN_Light, 5
    )), p$vars$RAIN_BoundHeaLi)
  )
  return(RAIN_fRandom)
}

# Rain Type 4
get_monthly_avg_rain <- function(RAIN_DoY, p) {
  # RAIN_Today? = IF(Time=int(time)AND(Random(0,1,(RAIN_GenSeed+1))<RAIN_DayProp))THEN(1) ELSE(0)
  RAIN_DayProp <- p$graph_functions$RAIN_DayProp$RAIN_DayProp(RAIN_DoY)
  RAIN_Today <- runif(1) < RAIN_DayProp
  if (!RAIN_Today)
    return(0)
  
  RAIN_MonthTot <- p$graph_functions$RAIN_MonthTot$RAIN_MonthTot(RAIN_DoY)
  # RAIN_fTable = NORMAL(RAIN_MonthTot/(30*RAIN_DayProp),RAIN_CoefVar4*RAIN_MonthTot/(30*RAIN_DayProp),RAIN_GenSeed+3)
  RAIN_fTable <- rnorm(
    1,
    RAIN_MonthTot / (30 * RAIN_DayProp),
    p$vars$RAIN_CoefVar4 * RAIN_MonthTot / (30 * RAIN_DayProp)
  )
  return(max(0, RAIN_fTable))
}


### Output var default ##############




default_output_vars <- c(
  "AF_DepthLay",
  "BC_ChangStock",
  "BC_CO2fromBurn",
  "BC_CPhotosynth",
  "BC_Crop",
  "BC_CropInit",
  "BC_CstocksInit",
  "BC_CurrentCStock",
  "BC_ExtOrgInput",
  "BC_GWeffect_CO2_eq_g_per_m2",
  "BC_HarvestedC",
  "BC_HarvestedT",
  "BC_Inflows",
  "BC_NecromasC",
  "BC_NetBal",
  "BC_Outflows",
  "BC_SOM",
  "BC_SOMInit",
  "BC_TimeAvg_CStock",
  "BC_TotalRespired",
  "BC_TPhotosynth",
  "BC_Tree",
  "BC_TreeInitTot",
  "BC_Weed",
  "BC_WeedSeedIn",
  "BC_WeedSeeds",
  "BN_CBiomInit",
  "BN_CHarvCum",
  "BN_CNdfaFrac",
  "BN_CNFixCum",
  "BN_CropBiom",
  "BN_EffluxTot",
  "BN_ExtOrgInputs",
  "BN_FertCum",
  "BN_ImmInit",
  "BN_Immob",
  "BN_InfluxTot",
  "BN_LatInCum",
  "BN_LatOutCum",
  "BN_LeachingTot",
  "BN_Lit",
  "BN_LitInit",
  "BN_NetBal",
  "BN_NutVolatCum",
  "BN_Som",
  "BN_SOMInit",
  "BN_StockInit",
  "BN_StocksTotInit",
  "BN_StockTot",
  "BN_SumofFluxes",
  "BN_THarvCumAll",
  "BN_TNdfaFrac",
  "BN_TNFixAmountCum",
  "BN_TreeInit",
  "BN_WeedBiom",
  "BN_WeedSeedBank",
  "BN_WeedSeedInit",
  "BS_GrossErosionCum",
  "BS_SoilCurr",
  "BS_SoilDelta",
  "BS_SoilInflowCum",
  "BS_SoilInit",
  "BW_DrainCumV",
  "BW_EvapCum",
  "BW_LatInCum",
  "BW_LatOutCum",
  "BW_NetBal",
  "BW_RunOffCum",
  "BW_RunOnCum",
  "BW_StockInit",
  "BW_StockTot",
  "BW_UptCCum",
  "BW_UptTCum",
  "BW_UptWCum",
  "C_AgronYieldsCum",
  "C_Biom",
  "C_FracLim",
  "C_HydEqFluxes",
  "C_NPosgro",
  "C_NUptTot",
  "CENT_Bal_NP_Total",
  "CW_Posgro",
  "E_TopSoilDepthAct",
  "GHG_Cum_CH4_emission",
  "GHG_GWP_N2O_CH4",
  "GHG_N2_Fraction",
  "GHG_N2O_Fraction",
  "GHG_NO_Fraction",
  "LIGHT_CRelCap",
  "LIGHT_CRelSupply",
  "N_EdgeFFH",
  "N_EdgeFFV",
  "N_LocFFi",
  "N_Stock",
  "N_TotFFTot",
  "P_CCosts",
  "P_CCostsAvg",
  "P_CCostsTot",
  "P_CReturnAvg",
  "P_CReturnperType",
  "P_GeneralCosts",
  "P_NPV",
  "P_TReturnTot",
  "P_TWoodPrice",
  "Rain",
  "RAIN_Cum",
  "RAIN_IntercEvapCum",
  "T_Biom",
  "T_BiomAG",
  "T_BiomCumTot",
  "T_CanH",
  "T_CanWidth",
  "T_CumLatexHarv",
  "T_CumLitfall",
  "T_FruitHarvCum",
  "T_HarvPrunCum",
  "T_FracLim",
  "T_Fruit",
  "T_FruitCum",
  "T_FruitHarvInc",
  "T_GroRes",
  "T_Height",
  "T_HydEqFluxes",
  "T_LAI",
  "T_LatexStock",
  "T_LfTwig",
  "T_LifallInc",
  "T_Light",
  "T_NUptTotAll",
  "T_NBiom",
  "T_PlantTime",
  "T_PrunHarvCum",
  "T_RootPlCompTot",
  "T_SapWood",
  "T_SapWoodDiam",
  "T_StemDiam",
  "T_WoodH",
  "T_WoodHarvCum",
  "TF_BunchWeightHarvest",
  "TF_CrownRadius",
  "TF_CumBunchWeightHarvest",
  "TF_CumFruitsHarvNo",
  "TF_CurrentLeafNo",
  "TF_FruitHarvNoperBunch",
  "TF_PalmTrunkHeight",
  "TF_RecentFrondLength",
  "TF_TrunkDiam",
  "W_TUpt"
)






plot.wanulcas <- function(x) {
  print("test")
}
