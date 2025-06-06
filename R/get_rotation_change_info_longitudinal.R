#' info for testlet rotation change for longitudinal estimation
#'
#' @param data data.frame; xTargetCompetencies etc.
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#'
#' @return data.frame containing ID_t and and testlet position indicator; or NULL
#' @noRd

get_rotation_change_info_longitudinal <- function(SC, domain, data) {
  position <- NULL
  if ((SC == "SC4" & domain %in% c("RE", "MA")) |
      (SC == "SC3" & domain %in% c("RE", "SC")) |
      (SC == "SC2" & domain %in% c("VO", "MA"))) {
    wave <- ifelse(SC == "SC4", "w7",
                   ifelse(SC == "SC3" & domain == "RE", "w6",
                          ifelse(SC == "SC3" & domain == "SC", "w8",
                          ifelse(SC == "SC2" & domain == "VO", "w3", "w4"))))
    position <- data.frame(ID_t = data[["ID_t"]],
                           position = data[[paste0("tx80211_", wave)]])
    position[["position"]][
      !is.na(position[["position"]]) &
        (position[["position"]] %in%
           testlet_position[[SC]][[domain]][[wave]][, 1])] <- 1
    position[["position"]][
      !is.na(position[["position"]]) &
        (position[["position"]] %in%
           testlet_position[[SC]][[domain]][[wave]][, 2])] <- 2
  }
  position
}
