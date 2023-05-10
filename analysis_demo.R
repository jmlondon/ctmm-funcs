library(wcUtils)
library(tidyverse)

# get data from wildlife computers data portal and filter
# to just include data from cook inlet study and splash tags
cookinlet_ids <- wcUtils::wcGetProjectIDs(wcPOST(), 'CookInlet')

wc_data <- purrr::map(cookinlet_ids, wcGetDownload)
locs_tbl <- map(wc_data,"locations") |> 
  bind_rows() |> 
  drop_na(any_of(c("latitude", "longitude"))) |> 
  filter(instr %in% c('Splash')) |> 
  filter(deployid != 'PV2005_0112_03T0111')

# function to convert downloaded data into a telemetry object
as_telem <- function(locs_df) {
  # remove duplicate records
  locs_df <- locs_df |> 
    group_by(deployid) |> 
    arrange(date_time, error_radius) |> 
    mutate(
      rank = 1L,
      rank = case_when(duplicated(date_time, fromLast = FALSE) ~
                         lag(rank) + 1L, TRUE ~ rank)) |> 
    dplyr::filter(rank == 1) |>
    ungroup() |> 
    arrange(deployid, date_time)
  
  locs_df <- locs_df |> 
    rename(
      individual.local.identifier = deployid,
      timestamp = date_time,
      location.long = longitude,
      location.lat = latitude,
      Argos.orientation = error_ellipse_orientation,
      Argos.semi.minor = error_semi_minor_axis,
      Argos.semi.major = error_semi_major_axis,
      Argos.location.class = quality
    )
  locs_df <- ctmm::as.telemetry(
    object = locs_df)

  return(locs_df)
}

locs_telem <- as_telem(locs_tbl)

# speed filter with 2.5 m/s
speed_filter <- function(telem) {
  out <- outlie(telem,plot=FALSE)
  flt <- which(out$speed < 2.5)
  return(telem[flt,])
}

locs_filt <- lapply(locs_telem, speed_filter)

# let's pull out a random sample of 20 tracks to work with
DATA <- sample(locs_filt, 20)
VG_MEAN <- DATA |> map(variogram) |> mean()

GUESS <- map(DATA, ctmm.guess, interactive=FALSE)
FITS <- map(DATA, ctmm.select, GUESS)

#inclusion of hard boundary
library(rnaturalearth)
library(sp)

usa <- ne_states("United States of America")
ak <- subset(usa, name == "Alaska")

#calculating UDs

UDS <- DATA |> map2(FITS,akde,SP=ak,SP.in=FALSE)
POP_UD <- DATA |> pkde(UDS,SP=ak, SP.in=FALSE)

