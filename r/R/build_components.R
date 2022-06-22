build_components <- function(){
  components <- jsonlite::read_json(
    "https://raw.githubusercontent.com/hal9ai/hal9ai/main/scripts/components.json", simplifyVector = TRUE
  ) |>
  purrr::map_dfr(identity) |>
  dplyr::filter(stringr::str_detect(source, "transforms|charts|visualizations")) |>
  dplyr::filter(!stringr::str_detect(source, "violin")) |>
  dplyr::group_by(name) |>
  dplyr::mutate(
    component_params = purrr::map(source, ~parse_txt_js(paste0("../scripts/", .x)))
  ) |>
  dplyr::ungroup()

  usethis::use_data(components, internal = TRUE, overwrite = TRUE)
}
