library(wcUtils)
library(tidyverse)

cookinlet_ids <- wcUtils::wcGetProjectIDs(wcPOST(), 'CookInlet')

wc_data <- purrr::map(cookinlet_ids, wcGetDownload)
locs_tbl <- map(wc_data,"locations") |> 
  bind_rows() |> 
  drop_na(any_of(c("latitude", "longitude")))

as_telem <- function(locs_df) {
  
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
    object = locs_df,
    keep=TRUE,
    drop=FALSE)

  return(locs_df)
}

locs_telem <- as_telem(locs_tbl)

