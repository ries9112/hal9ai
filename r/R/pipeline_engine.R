parse_value <- function(value){

  if(is.null(value)){
    list(
      list(
        id = 0,
        name = "",
        label = ""
      )
    )
  } else {

    value |>
      purrr::imap(
        function(x, y){
          c(x, list(id = y))
        }
      )
  }
}

read_txt_js_header <- function(file) {

  lines <- readLines(file,  warn = FALSE)

  last_line <- which(lines == "**/")

  if(length(last_line) == 0){
    # no **/

    return(list())
  }

  lines <- lines[2:(last_line - 1)]
  yaml <- lines |>
    paste(collapse = "\n")
  yaml::read_yaml(text = yaml)
}

build_param_list <- function(x){

  if(length(x) == 0){
    return(list(error = ""))
  }

  param_list <- x |>
    magrittr::extract2("params") |>
    purrr::map(function(x){
      if(is.null(names(x))){
        list(
          name = x,
          label = x,
          value = NULL
        )
      } else {
        return(x)
      }
    })

  param_names <- param_list |>
    purrr::map_chr(magrittr::extract2, "name")

  param_list |>
    purrr::imap(~list(
      id = .y-1,
      static = FALSE,
      value = parse_value(.x$value),
      name = .x$name,
      label = .x$label,
      single = .x$single
    )) |>
    purrr::set_names(
      param_names
    )
}

parse_txt_js <- function(file){
  file |>
    read_txt_js_header() |>
    build_param_list()
}
