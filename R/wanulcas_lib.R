### WANULCAS FUNCTION AND DATA LIBRARY ###########


maxval <- 1e+308




### Graph Data #############

wgraph_file <- read_yaml("wanulcas_graph.yaml")

to_df <- function(x, is_print = F) {
  n <- unlist(regmatches(x, gregexpr("[0-9.]+", x)))
  n <- as.numeric(n)
  ix <- seq(1, length(n), 2)
  iy <- seq(2, length(n), 2)
  if (is_print) {
    print(paste0("c(", paste(n[ix], collapse = ","), ")"))
    print(paste0("c(", paste(n[iy], collapse = ","), ")"))
  }
  data.frame(x = n[ix], y = n[iy])
}

wgraph <- lapply(wgraph_file, function(x) {
  if (length(x$y_col) == 1) {
    df <- to_df(x$data)
    names(df) <- c(x$x_col, x$y_col)
  } else {
    df <- to_df(x$data[1])
    for (i in 2:length(x$data)) {
      idf <- to_df(x$data[i])
      df <- cbind(df, idf[2])
    }
    names(df) <- c(x$x_col, x$y_col)
  }
  return(df)
})

names(wgraph) <- unlist(lapply(wgraph_file, function(x) {
  if (length(x$y_col) == 1) {
    x$y_col
  } else {
    v <- unlist(strsplit(x$y_col[1], "_", fixed = T))
    paste(head(v, -1), collapse = "_")
  }
}))

get_y_df <- function(x,
                     graph_name,
                     x_column = NULL,
                     y_column = NULL) {
  df <- wgraph[[graph_name]]
  
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
    if (length(ylist) == 1) {
      unlist(ylist)
    } else {
      as.vector(t(as.data.frame(ylist)))
    }
  }
}

### FUNCTIONS ####################

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


order_soil <- function(df) {
  df[order(df$layer, df$zone), ]
}

as_var_list <- function(df) {
  l <- as.list(df[[2]])
  names(l) <- df[[1]]
  return(l)
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
  nwater = 4
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
  
  arr_init <- list(
    angle_df = angle_df,
    animal_df = animal_df,
    buf_df = buf_df,
    cpools_df = cpools_df,
    crop_df = crop_df,
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
    zonelayertreewater_df = zonelayertreewater_df
  )
  
  return(arr_init)
}


get_wanulcas_def_inp <- function() {
  a <- get_wanulcas_def_arr()
  
  ### PAR def #########################
  pars <- list(
    AF_par = list(
      single_vars = list(
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
        AF_TreeCircleWeedFree_is = 0
      ),
      zone_df = cbind(
        a$zone_df,
        data.frame(
          AF_LitLayerDepth = 2,
          AF_WeedZn_is = 0,
          AF_ZoneTree = 1:nrow(a$zone_df)
        )
      ),
      zonelayer_df = cbind(a$zonelayer_df, data.frame(AF_StoneFrac = 0)),
      nut_df = cbind(a$nut_df, data.frame(AF_RunNutLim_is = 1))
    ),
    
    ASH_par = list(
      single_vars = list(
        ASH_init_BD = 0.3,
        ASH_Init_Water = 0,
        ASH_SOM_pools = 0,
        ASH_Time = 10000
      ),
      zone_df = cbind(a$zone_df, data.frame(ASH_layer_depth = 0))
    ),
    
    C_par = list(
      single_vars = list(
        C_ApplyMaintResp_is = 0,
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
        CA_DOYStart = 1,
        CQ_CropGraze = 0,
        CQ_StageAfterGraze = 0.4,
        CQ_WeedType = 5
      ),
      zone_df = cbind(
        a$zone_df,
        data.frame(
          C_CanLow = 0,
          CA_ComplCrop = 0,
          CQ_CropWeedSwitch = 0,
          CQ_Stage = 0
        )
      ),
      pcomp_df = cbind(a$pcomp_df, data.frame(
        C_SeedConc = c(1, 0.05, 0.005),
        C_UnitConv = c(1, 1000, 1000)
      )),
      crop_df = cbind(a$crop_df, data.frame(C_HostEffForT1 = 0))
    ),
    
    CW_par = list(
      single_vars = list(
        CW_Alpha = 0.1,
        CW_DryFactRangeChange = 0.2,
        CW_DryFactRangePowerStart = 1,
        CW_DryPowerMax = 4,
        CW_DryPowerMin = 0.1,
        CW_EnergyDrivenEpot_is = 1
      ),
      buf_df = cbind(a$buf_df, data.frame(
        CW_DryFactRangeInit = c(0.1, 0.3, 0.5, 0.8, 0.9, 1, 1.3, 1.5, 1.7, 2)
      ))
    ),
    
    E_par = list(
      single_vars = list(
        E_CovEffLitter = 0.002,
        E_EntrailmentCoeffBarePlot = 0.002,
        E_ErosiType = 0,
        E_IntvPloughPlant = 10,
        E_PloughBefPlant_is = 0,
        E_RainFac = 1,
        E_RelSedConcRunOn = 1,
        E_SoilMovperPlou = 0,
        E_SoilType = 1
      ),
      zone_df = cbind(a$zone_df, data.frame(E_TillZone_is = c(0, 1, 1, 1)))
    ),
    
    EVAP_par = list(
      single_vars = list(
        EVAP_InitSlashM = 0.4,
        EVAP_InitWoodM = 0.25,
        EVAP_MulchEffSurfLit = 1,
        EVAP_Pot_Thornthwaite_is = 1,
        EVAP_SlashDryFact = 0.5,
        EVAP_TranspRedFractrionBy_Can_Intercepted_Water = 0.5,
        EVAP_WoodDryFact = 0.25
      )
    ),
    
    #### G_par ########
    G_par = list(
      single_vars = list(
        G_Graze_Offseason_is = 1,
        G_Grazing_Cycle = 10,
        G_LivestWeightGain = 0,
        G_SLU = 450,
        G_StockingRate_per_ha = 0.3
      ),
      zone_df = cbind(a$zone_df, data.frame(G_Graze_Zn_is = c(0, 0, 0, 0))),
      pcomp_df = cbind(
        a$pcomp_df,
        data.frame(
          G_DayDempKgDay = c(0.025, 5e-04, 5e-05),
          G_AnimRespFrac = c(0.5, 0, 0),
          G_AnmWGfrac = 0.05
        )
      )
    ),
    
    GHG_par = list(
      single_vars = list(
        GHG_AnaerobMem = 0.5,
        GHG_AnaeroThresh = 0.95,
        GHG_CH4_Km = 0.05,
        GHG_GWP_CH4 = 15,
        GHG_GWP_N2O = 310,
        GHG_LittMinMultiplier = 1,
        GHG_PotCH4Em = 0.0011,
        GHG_PotCH4oxid = -0.0011,
        GHG_SatTimeDuringVDrainDay = 0.8
      ),
      layer_df = cbind(a$layer_df, data.frame(
        GHG_AnaerobLayerW = c(0.6, 0.2, 0.15, 0.05)
      ))
    ),
    
    LF_par = list(
      single_vars = list(
        LF_FracGWReleaseAsInflow = 0,
        LF_GW_ReleaseFraction = 0.05,
        LF_SubSurfInflowAdd4 = 0
      )
    ),
    
    LIGHT_par = list(
      single_vars = list(
        Ligh1tPerAngleUniform = 0.05,
        LIGHT_TBAI = 0,
        LIGHT_TBAI1 = 0,
        Light2PerAngleSkewed = 0.05,
        Light3PerAngleVertical = c(rep(0, 17), 1, rep(0, 17)),
        LightSwitch = 3
      ),
      tree_df = cbind(a$tree_df, data.frame(LIGHT_kTB = 0.8)),
      angle_df = cbind(a$angle_df, data.frame(LightAngles = seq(-85, 85, 5)))
    ),
    
    #### MC_par ########
    MC_par = list(
      single_vars = list(
        MC_Carbon = 0.42,
        MC_EffMetab = 0.45,
        MC_EffPass = 0.45,
        MC_EffSlwAct = 0.42,
        MC_EffSlwPass = 0.03,
        MC_EffStrucAct = 0.45,
        MC_EffStrucSlw = 0.7,
        MC_EffStrucSlwSOM = 0.7,
        MC_RelKActLit = 1,
        MC_RelKMetabLit = 0.8,
        MC_RelKPassLit = 1,
        MC_RelKSlwLit = 1,
        MC_RelKStrucLit = 0.808511,
        MC_TextLitLayer = 0.01,
        MC2_ClayCoeffCref = 0.94,
        MC2_CorgInitMeth3 = 2,
        MC2_CorgpCref = 0.8,
        MC2_CrefMeth3 = 3,
        MC2_CrefOffset = 1.256,
        MC2_EffPass = 0.45,
        MC2_EffSlwAct = 0.42,
        MC2_EffStrucAc = 0.45,
        MC2_kAct = 0.02,
        MC2_kMetab = 0.05,
        MC2_kPass = 1.86e-05,
        MC2_kRelLayer = c(1, 0.8, 0.7, 0.6),
        MC2_kSlw = 0.000543,
        MC2_kStruc = 0.013429,
        MC2_pH = 5,
        MC2_pHCoeffCref = -0.156,
        MC2_SiltClayCoeffCref = 0.699 / 0.994,
        MC2_SomInitType = 1
      ),
      zone_df = cbind(
        a$zone_df,
        data.frame(MC_CNRatInitMet = 8, MC2_CNRatInitMet = 8)
      ),
      layer_df = cbind(a$layer_df, data.frame(MC2_SOMDist = c(1, 0.2, 0.1, 0.05))),
      inp_df = cbind(
        a$inp_df,
        data.frame(
          MC_CExtOrg = 0.4,
          MC_LignExtOrg = 0.2,
          MC_PolypExtOrg = 0
        )
      ),
      cpools_df = cbind(
        a$cpools_df,
        data.frame(
          MC2_RainTransfer = c(0.0001, 0.001, 0.001, 0.001, 0.001),
          MC2_SoilTillTransfer = 1,
          MC2_WormTransfer = c(0.03, 0.03, 0.03, 0.01, 0.003)
        )
      )
    ),
    
    #### MN_par ########
    MN_par = list(
      single_vars = list(
        MN_CNAct = 8,
        MN_CNPass = 61,
        MN_CNSlw = 11,
        MN_CNStruc = 150,
        MN_LatFlowFertKm = 10,
        MN2_NSOMMinExch = 0.5,
        MP2_SomMinExchBuffer = 0.1
      ),
      zone_df = cbind(
        a$zone_df,
        data.frame(
          MN_InitAct = 2e-05,
          MN_InitMetab = 0,
          MN_InitPass = 1e-04,
          MN_InitSlw = 1e-06,
          MN_InitStruc = 0,
          MN2_InitAct = 0.091,
          MN2_InitMetab = 0,
          MN2_InitPassx = 0.728,
          MN2_InitSlw = 1.01,
          MN2_InitStruc = 0,
          MP2_InitStruc = 0
        )
      ),
      nut_df = cbind(
        a$nut_df,
        data.frame(
          MN_FertDissFrac = c(0.3, 0.5),
          MN_NutRatAct = c(1, 10),
          MN_NutRatMetab = c(1, 10),
          MN_NutRatPas = c(1, 10),
          MN_NutRatSlw = c(1, 10),
          MN_NutRatStruc = c(1, 10)
        )
      ),
      
      layer_df = cbind(a$layer_df, data.frame(MN2_PassRelLayer = c(1, 1.2, 1.4, 1.6))),
      nutinp_df = cbind(a$nutinp_df, data.frame(MN_ExtOrgN = c(
        0.05, 0.005, 0.1, 0.01
      )))
    ),
    
    #### N_par #####################
    N_par = list(
      single_vars = list(
        N_DiffCoef = 1,
        N_Lat4InflowRelConc = 1,
        N_LittNmin1exchfact = 0.1,
        N_Use_NgassLossEst_is = 0
      ),
      layer_df = cbind(
        a$layer_df,
        data.frame(
          N_KaNH4 = 5,
          N_KaNO3 = 0.3,
          # N_LossiCum = 0,
          N_RtSynloc = 0.5
        )
      ),
      tree_df = cbind(
        a$tree_df,
        data.frame(
          N15_T_Can = 0,
          N15_T_Fruit = 0,
          N15_T_GroRes = 0,
          N15_T_Rt = 0,
          N15_T_Wd = 0
        )
      ),
      zonelayer_df = cbind(
        a$zonelayer_df,
        data.frame(
          N_BypassMatrix = 0.2,
          N_FracNO3 = 0.4,
          N15_Add = 0,
          N15_Stock = 0,
          N_BypassMacro = 1
        )
      ),
      zonelayernut_df =
        cbind(a$zonelayernut_df, data.frame(
          N_ImInit = c(rep(0.05, nrow(a$zonelayer_df)), rep(0.01, nrow(a$zonelayer_df))), N_LossiCum = 0
        )),
      nut_df = cbind(
        a$nut_df,
        data.frame(
          N_AtmosphDepos = 0,
          N_N_is = c(1, 0),
          N_DiffCoef = c(1, 0.89 * 10^-5 * 60 * 60 * 24)
        )
      ),
      layernut_df = cbind(a$layernut_df, data.frame(N_Nutmob = 0))
    ),
    
    P_par = list(
      single_vars = list(
        P_CNuFertAppperCropSeason = 1,
        P_CropProfThreshold = 1e+05,
        P_LabourforPestContrl_is = 0,
        P_LabourforWeed_is = 0,
        P_UseCropStopRule_is = 0
      ),
      tree_df = cbind(a$tree_df, data.frame(P_TNuFertAppperTreeAge = c(1, 1, 1)))
    ),
    
    #### PD_par #####################
    PD_par = list(
      single_vars = list(
        PD_FenceDecK = 0.02,
        PD_FenceFullQual = 2,
        PD_FenceMaint_is = 0,
        PD_FenceMUnit = 0.25,
        PD_FenceQThresh = 1.1,
        PD_HalfFenceTime = 50
      ),
      tree_df = cbind(
        a$tree_df,
        data.frame(
          PD_TFrugiv_Abort = 0,
          PD_THerbivory = 0,
          PD_TLignovory = 0,
          PD_TRhizovory = 0
        )
      ),
      crop_df = cbind(
        a$crop_df,
        data.frame(
          PD_CFrugivory = 0,
          PD_CHerbivory = 0,
          PD_CRhizovory = 0
          
        )
      ),
      animal_df = cbind(
        a$animal_df,
        data.frame(
          PD_CFrugivore_is = 0,
          PD_CHerbivore_is = 0,
          PD_CRhizovore_is = 0,
          PD_JumptheFence_is = c(0, 1, 1, 1, 0, 0, 1),
          PD_ResidenceinPlot_is = c(0, 0, 1, 1, 0, 0, 0),
          PD_TFrugivore_is = 0,
          PD_THerbivore_is = 0,
          PD_TLignovore_is = 0,
          PD_TRhizovore_is = 0,
          PD_PopulOutside = 0
        )
      )
    ),
    
    RAIN_par = list(
      single_vars = list(
        RAIN_AnMemory = 2,
        RAIN_AType = 1,
        RAIN_BoundHeaLi = 25,
        RAIN_CoefVar3 = 0.05,
        RAIN_CoefVar4 = 0.05,
        RAIN_Cycle_is = 1,
        RAIN_Gamma = 0.033621,
        RAIN_GenSeed = 300,
        RAIN_Heavy = 42,
        RAIN_HeavyP = 0.5,
        RAIN_I_Initial_Value = 1,
        RAIN_IntensCoefVar = 0.3,
        RAIN_IntensMean = 50,
        RAIN_IntercDripRt = 10,
        RAIN_IntMult = 3,
        RAIN_Light = 9,
        RAIN_Max_IntDripDur = 0.5,
        RAIN_MonthlyMean_RainfallMax = 333,
        RAIN_MonthlyMean_RainfallMin = 102,
        RAIN_Months = c(1:12),
        RAIN_Multiplier = 1,
        RAIN_OffsetValue = -0.5,
        RAIN_Pattern1_Max = 0.06,
        RAIN_Pattern1_Min = -0.01,
        RAIN_Peakines_Season1 = 1,
        RAIN_Peakines_Season2 = 12,
        RAIN_PondFlwRt = 10,
        RAIN_PondStoreCp = 5,
        RAIN_Probability = 0.5,
        RAIN_Shape_Max = 1.5,
        RAIN_Shape_Min = -0.5,
        RAIN_UniorBimodial_is = 2,
        RAIN_UniorBimodial_is = 2,
        RAIN_Weibull_Param = 0.93,
        RAIN_WettestMonth_Season1 = 1,
        RAIN_WettestMonth_Season2 = 7,
        RAIN_YearStart = 0
      ),
      zone_df = cbind(a$zone_df, data.frame(RAIN_Weight = 1))
    ),
    
    #### RT_par ###############
    RT_par = list(
      single_vars = list(
        RT_ACType = 0,
        RT_CLrvPlatDep = 0,
        RT_CMultiplier = 1,
        RT_CRhizExt = 1,
        RT_MCHypDiam = 0.01,
        RT_MCHypL = 100,
        RT_T_FixedSRL = 20,
        # RT_T_HostEffForT1 = c(0, 0, 0),
        RT_T_UseFBASRL_is = 0,
        RT_THalfRtAllocStage = 0.05,
        RT_TMultiplier = 1,
        RT_TRhizExt = 1,
        RT_StopGap = 10^-12,
        RT3_PowerAllocRtL = 0.005,
        RT3_TempRespforRtD = 0.5
      ),
      zone_df = cbind(a$zone_df, data.frame(RT_CDecDepthAct = 0)),
      zonelayer_df =  cbind(a$zonelayer_df, data.frame(RT3_SoilT = 20)),
      zonelayertree_df = cbind(a$zonelayertree_df, data.frame(RT3_TFRInit = c(
        10, rep(0, nrow(a$zonelayertree_df) - 1)
      ))),
      tree_df = cbind(
        a$tree_df,
        data.frame(
          RT_ATType = 0,
          RT_MTHypDiam = 0.01,
          RT_MTHypL = 100,
          RT_TRhizExt = 1,
          RT_T_HostEffForT1 = 0,
          RT3_AlphaLrv = 1e-15,
          RT3_Beta0 = 0.2,
          RT3_CoarseRtDecayCoeff = 0,
          RT3_LamGeotrop = 0.7,
          RT3_LamHor0 = 0.5,
          RT3_CR_TargFac = 8.5 * (10^-7)
        )
      ),
      layer_df = cbind(a$layer_df, data.frame(
        RT_MTInfFrac = 0,
        RT_MCInfFrac = c(0.5, 0.25, 0.05, 0)
      ))
    ),
    
    #### SB_par ###############
    SB_par = list(
      single_vars = list(
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
        SB_WoodAshCont = 20
      ),
      zone_df = cbind(a$zone_df, data.frame(SB_PileUpWgt = c(0, 0, 0, 1))),
      pcomp_df = cbind(a$pcomp_df, data.frame(SB_DW_is = c(1, 0, 0)))
    ),
    
    S_par = list(
      single_vars = list(
        S_BDBDRefDecay = 1e-04,
        S_BDEqPower = 0.5,
        S_C_RT_StrucFormFrac = 0.1,
        S_KsatVDeepSub = 20,
        S_RelWormLit = c(1, 0.6, 0.3, 0.1),
        S_RelWormSurf = 1,
        S_SoilStructDyn_is = 0,
        S_WormsLikeLitMetab = 1e-05,
        S_WormsLikeLitStruc = 5e-07,
        S_WormsLikeSOMMetab = 1e-06,
        S_WormsLikeSOMStruc = 5e-08
      ),
      zone_df = cbind(
        a$zone_df,
        data.frame(
          S_RelSurfInfiltrInit = 4,
          S_SurfInfiltrPerKsatDef = 0.0825
        )
      ),
      zonelayer_df = cbind(a$zonelayer_df, data.frame(S_KSatHperV = 1)),
      tree_df = cbind(a$tree_df, data.frame(S_T_RT_StrucFormFrac = 0.3))
    ),
    
    TEMP_par = list(
      single_vars = list(
        TEMP_AType = 1,
        TEMP_Cons = 28,
        TEMP_PotEvapConst = 4,
        TEMP_PotEvapConst_is = 0
      )
    ),
    
    #### T_par ####
    T_par = list(
      single_vars = list(
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
        TW_EnergyDrivenEpot_is = 0
      ),
      zone_df = cbind(a$zone_df, data.frame(TW_Water_Limited_is = 0)),
      zonelayertreepcomp_df = cbind(a$zonelayertreepcomp_df, data.frame(T_Root = 0)),
      zonelayertree_df = cbind(a$zonelayertree_df, data.frame(T_RelRtIncrTyp2 = 1 /
                                                                16)),
      tree_df = cbind(
        a$tree_df,
        data.frame(
          T_CanBiomInit = 0,
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
          T_GrowResMobFrac = 0.2,
          T_GrowResStorFrac = 0.2,
          T_Kill2DOY = 1,
          T_Kill2Y = 1000,
          T_Kill3DOY = 1,
          T_Kill3Y = 1000,
          T_KillDOY = 1,
          T_KillY = 1000,
          T_LatexMoistFrac = 0.14,
          T_LeafHalfLife = 0.5,
          T_NDemandFrac = 0.2,
          T_PanelQuality1 = 1,
          T_PanelQuality2 = 1,
          T_PrunHarvFracC = 0,
          T_PrunMoistFrac = 0.14,
          T_PrunRecov = 14,
          T_RtAllocInit = 0.1,
          T_SlashSellWoodFrac = 0,
          T_SRLfineroots = 0.8,
          T_Stage = 0,
          T_WoodBiomInit = 0,
          T_WoodFracHRemain = 100,
          T_WoodHarvFrac = 0.95,
          T_WoodHInit = 0,
          T_WoodMoistFrac = 0.25,
          TF_FruitLossHeight = 5,
          TW_DryFactPowerInit = 1,
          TW_PosgroMin = 1e-05,
          TW_ResistFact = 50,
          T_PrunFracC = 1
        )
      ),
      zonetree_df = cbind(a$zonetree_df, data.frame(T_PrunWeight = 1)),
      treestage_df = cbind(a$treestage_df, data.frame(T_StageAftPrun_is = 1)),
      pcomp_df = cbind(
        a$pcomp_df,
        data.frame(
          T_UnitConv = c(1, 1000, 1000),
          TF_LitFallRedFac = c(1, 0.5, 0.5),
          TP_BiomNutrContent = c(0, 0.2, 0.02)
          
        )
      ),
      treepcomp_df = cbind(
        a$treepcomp_df,
        data.frame(T_RootIncFix = 10, TP_ParasiteRemoval = 0)
      ),
      buf_df = cbind(
        a$buf_df,
        data.frame(
          TW_DemActSubtract = c(9e-06, 8e-06, 7e-06, 6e-06, 5e-06, 4e-06, 3e-06, 2e-06, 1e-06, 0),
          TW_OffSetRel = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0.1, 0.2, 0.3, 0.4, 0.5),
          TW_RangeToppingUp = c(1.05, 1.02, 1, 1, 1, 1, 1, 1, 1, 1)
        )
      ),
      treenut_df = cbind(a$treenut_df, data.frame(T_NPosgroMin = 1e-05)),
      fruit_df = cbind(a$fruit_df, data.frame(TF_PollinationStage = c(
        rep(0, 14), 1, rep(0, 7)
      )))
    ),
    
    W_par = list(
      single_vars = list(
        W_Hyd_is = 0,
        W_HydEqFraction = 0.1,
        W_PMax = 0,
        W_SeepScalar = 0,
        W_WaterLog_is = 0
      ),
      zone_df = cbind(a$zone_df, data.frame(W_WaterLimited_is = 0)),
      zonelayer_df = cbind(a$zonelayer_df, data.frame(W_ThetaInit = rep(
        c(1, 0.9, 0.8, 0.7), each = nrow(a$zone_df)
      )))
    )
  )
  
  return(pars)
}

### Stock Vars ##########################
get_wanulcas_stock_vars <- function() {
  a <- get_wanulcas_def_arr()
  
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
    zonetree_df = cbind(a$zonetree_df, data.frame(T_LifallWeight = 1)),
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

CQ_var <- c(
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
  "",
  "",
  "",
  "",
  "CQ_NRtConcCurr",
  "CQ_NFixVariable_is_Curr",
  "CQ_NFixDailyFracCurr",
  "CQ_NFixRespCurr",
  "CQ_NFixDWMaxFracCurr",
  "CQ_NFixDWUnitCostCurr",
  "CQ_RtDiam",
  "",
  "",
  "",
  "",
  "RT_CLraC",
  "RT_CDecDepthC",
  "RT_CSRLCurr",
  "RT_CHalfLifeCurr",
  "RT_CRtAllocRespCurr",
  "RT_CDistResp",
  "CQ_MaxMycInf",
  "N_CRhizKaPMod",
  "",
  "",
  "CQ_LignResidCurr",
  "CQ_LignRootResCurr",
  "CQ_PolyResid",
  "CQ_PolyRt",
  "CQ_CovEffCurr",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "CQ_AgronYieldMoistFrac"
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

### XLS input ###########

get_xls_params <- function(xls_input_file) {
  parxls_df <- wb_to_df(xls_input_file, "LinkToStella")
  par_names <- names(parxls_df)
  
  get_par_xls_list <- function(unit_labels, coluMN_names) {
    l <- as.list(parxls_df[1:length(unit_labels), coluMN_names])
    names(l) <- unit_labels
    return(l)
  }
  
  AF_System_par <- get_par_xls_list(AF_Unit, "AF_System")
  
  CQ_df <- parxls_df[1:length(CQ_Unit), c(
    "Cq_Parameters1",
    "Cq_Parameters2",
    "Cq_Parameters3",
    "Cq_Parameters4",
    "Cq_Parameters5"
  )]
  CQ_df$CQ_Unit <- CQ_Unit
  CQ_df$CQ_var <- CQ_var
  
  
  CQ_CRelLUE_i <- which(par_names == "Cq_CRelLUE")
  CQ_CRelLUE_df <- parxls_df[1:21, CQ_CRelLUE_i:(CQ_CRelLUE_i + 19)]
  names(CQ_CRelLUE_df) <- CQ_CType_params
  CQ_CRelLUE_df$CQ_Stage <- seq(0, 2, 0.1)
  
  CQ_CLWR_i <- which(par_names == "Cq_CLWR")
  CQ_CLWR_df <- parxls_df[1:21, CQ_CLWR_i:(CQ_CLWR_i + 19)]
  names(CQ_CLWR_df) <- CQ_CType_params
  CQ_CLWR_df$CQ_Stage <- seq(0, 2, 0.1)
  
  CQ_CHarvAlloc_i <- which(par_names == "Cq_CHarvAlloc")
  CQ_CHarvAlloc_df <- parxls_df[1:21, CQ_CHarvAlloc_i:(CQ_CHarvAlloc_i + 19)]
  names(CQ_CHarvAlloc_df) <- CQ_CType_params
  CQ_CHarvAlloc_df$CQ_Stage <- seq(0, 2, 0.1)
  
  CQ_CSLA_i <- which(par_names == "Cq_CSLA")
  CQ_CSLA_df <- parxls_df[1:21, CQ_CSLA_i:(CQ_CSLA_i + 19)]
  names(CQ_CSLA_df) <- CQ_CType_params
  CQ_CSLA_df$CQ_Stage <- seq(0, 2, 0.1)
  
  CQ_CRtAlloc_i <- which(par_names == "Cq_CRtAlloc")
  CQ_CRtAlloc_df <- parxls_df[1:11, CQ_CRtAlloc_i:(CQ_CRtAlloc_i + 19)]
  names(CQ_CRtAlloc_df) <- CQ_CType_params
  CQ_CRtAlloc_df$CQ_Stage <- seq(0, 2, 0.2)
  
  zone_stage_par <- list(
    CQ_CRelLUE = CQ_CRelLUE_df,
    CQ_CLWR = CQ_CLWR_df,
    CQ_CHarvAlloc = CQ_CHarvAlloc_df,
    CQ_CSLA = CQ_CSLA_df,
    CQ_CRtAlloc = CQ_CRtAlloc_df
  )

  CA_PlantDoY_i <- which(par_names == "Ca_PlantDoY")
  CA_PlantDoY_df <- parxls_df[1:21, CA_PlantDoY_i:(CA_PlantDoY_i + 3)]
  names(CA_PlantDoY_df) <- c("CA_PlantDoY1",
                             "CA_PlantDoY2",
                             "CA_PlantDoY3",
                             "CA_PlantDoY4")
  CA_PlantDoY_df$CA_ComplCrop <- c(0:20)
  
  CA_PlantYear_i <- which(par_names == "Ca_PlantYear")
  CA_PlantYear_df <- parxls_df[1:21, CA_PlantYear_i:(CA_PlantYear_i + 3)]
  names(CA_PlantYear_df) <- c("CA_PlantYear1",
                              "CA_PlantYear2",
                              "CA_PlantYear3",
                              "CA_PlantYear4")
  CA_PlantYear_df$CA_ComplCrop <- c(0:20)
  
  CQ_CropType_i <- which(par_names == "Cq_CropType")
  CQ_CropType_df <- parxls_df[1:21, CQ_CropType_i:(CQ_CropType_i + 3)]
  names(CQ_CropType_df) <- c("CQ_CropType1",
                             "CQ_CropType2",
                             "CQ_CropType3",
                             "CQ_CropType4")
  CQ_CropType_df$CA_ComplCrop <- c(0:20)
  
  
  evap_df <- data.frame(time = 1:365)
  evap_df$TEMP_DailyPotEvap <- parxls_df[1:365, c("Temp_DailyPotEvap")]
  
  T_df <- parxls_df[1:length(T_params), c("T_Par1", "T_Par2", "T_Par3")]
  T_df$T_params <- T_params
  # T_df$T_var <- T_var
  tree_par_df <- as.data.frame(t(T_df[1:3]))
  # names(tree_par_df) <- T_var
  names(tree_par_df) <- T_params
  
  
  T_PrunOption_par <- get_par_xls_list(T_PrunUnit, "T_PrunOption")
  
  
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
  
  
  T_PlantY_i <- which(par_names == "T_PlantY")
  T_PlantY_df <- parxls_df[1:21, T_PlantY_i:(T_PlantY_i + 2)]
  names(T_PlantY_df) <- c("T_PlantY_1", "T_PlantY_2", "T_PlantY_3")
  T_PlantY_df$T_Compl <- 0:20
  
  T_PlantDoY_i <- which(par_names == "T_PlantDoY")
  T_PlantDoY_df <- parxls_df[1:21, T_PlantDoY_i:(T_PlantDoY_i + 2)]
  names(T_PlantDoY_df) <- c("T_PlantDoY_1", "T_PlantDoY_2", "T_PlantDoY_3")
  T_PlantDoY_df$T_Compl <- 0:20

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
  
  soil_df <- as.data.frame(lapply(S_SoilProp_var, function(x) {
    unlist(S_SoilProp_par[x])
  }))
  
  
  Theta_x <- seq(0.01, 0.6, 0.59 / 50)
  W_PhiTheta_df <- parxls_df[1:length(Theta_x), c("W_PhiTheta1", "W_PhiTheta2", "W_PhiTheta3", "W_PhiTheta4")]
  W_PhiTheta_df$Theta <- Theta_x
  W_PTheta_df <- parxls_df[1:length(Theta_x), c("W_PTheta1", "W_PTheta2", "W_PTheta3", "W_PTheta4")]
  W_PTheta_df$Theta <- Theta_x
  P_x <- seq(-250, 0, 10)
  W_ThetaP_df <- parxls_df[1:length(P_x), c("W_ThetaP1", "W_ThetaP2", "W_ThetaP3", "W_ThetaP4")]
  W_ThetaP_df$P <- P_x
  
  ntree <- 3
  nlayer <- 4
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
  
  
  rain_df <- data.frame(time = 1:365)
  rain_df$Rain_Data <- parxls_df[1:365, c("Rain_Data")]
  
  
  pars <- list(
    AF_System_par = AF_System_par,
    CQ_df = CQ_df,
    zone_stage_par = zone_stage_par,
    
    CA_PlantDoY_df = CA_PlantDoY_df,
    CA_PlantYear_df = CA_PlantYear_df,
    CQ_CropType_df = CQ_CropType_df,
    
    evap_df = evap_df,
    
    tree_par_df = tree_par_df,
    T_PrunOption_par = T_PrunOption_par,
    
    T_PrunY_df = T_PrunY_df,
    T_PrunDoY_df = T_PrunDoY_df,
    T_PrunFracD_df = T_PrunFracD_df,
    T_PrunHarvFracD_df = T_PrunHarvFracD_df,
    
    T_PlantY_df = T_PlantY_df,
    T_PlantDoY_df = T_PlantDoY_df,
    
    soil_df = soil_df,
    W_PhiTheta_df = W_PhiTheta_df,
    W_PTheta_df = W_PTheta_df,
    W_ThetaP_df = W_ThetaP_df,
    TW_PhiPot_meta_df = TW_PhiPot_meta_df,
    TW_PhiPot_df = TW_PhiPot_df,
    rain_df = rain_df
  )
  
  return(pars)
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
