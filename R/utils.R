## GENERAL UTILITY LIBRARY #############

shp_ext <- c(".shp", ".shx", ".prj", ".dbf")

write_data <- function(data, fname, type) {
  if(is.null(data)) return()
  if (type == "df") {
    if(nrow(data) == 0) return()
    fname <- paste0(fname, ".csv")
    write.csv(data, fname, row.names = F, na = "")
  } else if (type == "cfg") {
    fname <- paste0(fname, ".yaml")
    write_yaml(data, fname)
  } else if (type == "stars") {
    fname <- paste0(fname, ".tif")
    write_stars(data, fname)
  } else if (type == "terra") {
    fname <- paste0(fname, ".tif")
    writeRaster(data, fname, T)
  } else if (type == "sf") {
    st_write(data,
             paste0(fname, ".shp"),
             append = F,
             quiet = T)
    fname <- paste0(fname, shp_ext)
  }
  return(fname)
}

save_variables <- function(io_file_df, vlist) {
  fs <- c()
  for(i in 1:nrow(io_file_df)) {  
    f <- io_file_df[i, "file"]
    vr <- unlist(strsplit(io_file_df[i, "var"], split = ".", fixed = T))
    vrlab <- vr[1]
    data <- vlist[[vrlab]]
    i <- 2
    #trace the variable within the list for a nested variable
    while (i <= length(vr)) {
      vrlab <- vr[i]
      data <- vlist[[vrlab]]
      i <- i + 1
    }
    if (!is.null(data)) {
      type <- suffix(vrlab)
      if (type == "list") {
        type2 <- suffix(vrlab, n = 2)[1]
        ns <- names(vlist[[vrlab]])
        for (id in ns) {
          data <- vlist[[vrlab]][[id]]
          f2 <- paste0(f, "-", id)
          fs <- c(fs, write_data(data, f2, type2))
        }
      } else {
        fs <- c(fs, write_data(data, f, type))
      }
    }
  }
  return(fs)
}

read_data <- function(fpath, type) {
  data <- NULL
  if (type == "stars") {
    fpath <- paste0(fpath, ".tif")
    if (file.exists(fpath))
      data <- suppressWarnings(read_stars(fpath, proxy = T))
  } else if (type == "terra") {
    fpath <- paste0(fpath, ".tif")
    if (file.exists(fpath))
      data <- rast(fpath)
  } else if (type == "sf") {
    fpath <- paste0(fpath, ".shp")
    if (file.exists(fpath))
      data <- st_read(fpath, quiet = T)
  } else if (type == "df") {
    fpath <- paste0(fpath, ".csv")
    if (file.exists(fpath))
      data <- read.csv(fpath)
  } else if (type == "cfg") {
    fpath <- paste0(fpath, ".yaml")
    if (file.exists(fpath))
      data <- read_yaml(fpath)
  }
  return(data)
}

upload_variables <- function(io_file_df, data_dir = "/", vlist = NULL) {
  if(is.null(vlist))
    vlist <- list()
  for(i in 1:nrow(io_file_df)) {  
    f <- io_file_df[i, "file"]
    fpath <- paste0(data_dir, "/", f)
    iv <- unlist(strsplit(io_file_df[i,"var"], split = ".", fixed = T))
    vr <- tail(iv, 1)
    type <- suffix(vr)
    if (type == "list") {
      type2 <- suffix(vr, n = 2)[1]
      fs <- list.files(data_dir)
      fs <- fs[substr(fs, 1, nchar(f)) == f]
      for (fn in fs) {
        ff <- unlist(strsplit(fn, ".", fixed = T))[1]
        id <- suffix(ff, sep = "-")
        fpath <- paste0(data_dir, "/", ff)
        data <- read_data(fpath, type2)
        vlist[[iv[1]]][[id]] <- data
      }
    } else {
      data <- read_data(fpath, type)
      if(is.null(data)) next
      i1 <- iv[1]
      if (length(iv) == 1) {
        vlist[[i1]] <- data
      } else if (length(iv) == 2) {
        i2 <- iv[2]
        vlist[[i1]][[i2]] <- data
      } else if (length(iv) == 3) {
        i2 <- iv[2]
        i3 <- iv[3]
        vlist[[i1]][[i2]][[i3]] <- data
      }
    }
  }
  return(vlist)
}

suffix <- function(v, sep = "_", n = 1) {
  tail(unlist(strsplit(v, sep, fixed = T)), n)
}

prefix <- function(v, sep = "_", n = 1) {
  p <- unlist(strsplit(v, sep, fixed = T))
  return(p[1:min(length(p), n)])
}

download_as_zip <- function(filename, vars, vlist) {
  downloadHandler(
    filename = function() {
      paste(filename)
    },
    content = function(fname) {
      setwd(tempdir())
      fs <- c()
      for(v in vars) {
        type <- suffix(v)
        fs <- c(fs, write_data(vlist[[v]], v, type))
      }
      return(zip::zip(zipfile = fname, files = fs))
    },
    contentType = "application/zip"
  )
}


#TODO: to compare with JSON method, which one is faster
list_to_df <- function(d) {
  id <- names(d)
  if (is.null(id))
    id <- c(1:length(d))
  df <- NULL
  for (i in id) {
    e <- d[[i]]
    edf <- data.frame(id = i)
    if (class(e) == "list") {
      f <- names(e)
      if (is.null(f))
        f <- c(1:length(e))
      for (x in f) {
        edf[[x]] <- e[[x]]
      }
    } else {
      edf[["x"]] <- e
    }
    if (is.null(df)) {
      df <- edf
    } else {
      df <- rbind(df, edf)
    }
  }
  return(df)
}

list_to_df_json <- function(d) {
  return(jsonlite::fromJSON(jsonlite::toJSON(d)))
}

day_to_seconds <- 86400

convert_flow_to_mmdaily <- function(flow_m3psec, area_m2) {
  return((flow_m3psec/area_m2)*day_to_seconds*1000)
}

get_rainfall_data <- function() {
  require(GSODR)
  ## bogor
  n <- nearest_stations(LAT = -6.5790138, LON = 106.7352587, distance = 100)
  tbar <- get_GSOD(years = 2023, station = "967510-99999")
}




