add_typologies <- function(data){

        sites <-
                dplyr::distinct_at(data, "site_id", .keep_all = TRUE) |>
                sf::st_as_sf(coords = c("x.coord", "y.coord"), crs = data4$EPSG[1]) |>
                dplyr::select(site_id) |>
                sf::st_transform(crs = sf::st_crs(typologies))

        nn <- sf::st_nearest_feature(sites, typologies)
        nn <- typologies[nn,]
        distances <- sf::st_distance(sites,
                                     y = nn,
                                     by_element = TRUE)

        sites <- dplyr::mutate(sites,
                               distance = as.numeric(distances),
                               brt12    = nn$brt,
                               ife      = nn$illies,
                               bgr      = nn$bgr,
                               enz  = nn$enz,
                               few  = nn$feow,
                               least.impacted = nn$least_disturbed
        )
        sites <- sf::st_drop_geometry(sites)
        #- join sites with data
        data.out <- dplyr::left_join(data,
                                     sites,
                                     by = "site_id")

        return(data.out)
}
