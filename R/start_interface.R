
#' @export
start_interface <- function(){

  config_path <- system.file("config", package = "estatisticapesqueiraSTP")


  conf <- estatisticapesqueiraSTP::get_config_from_local(config = config_path)


  artfish <- list(
    dat = readxl::read_excel(path = file.path(config_path, "artfish.xlsx"), sheet = "dat"),
    vars = readxl::read_excel(path = file.path(config_path, "artfish.xlsx"),
                              sheet = "vars"))


  shiny::shinyApp(ui     = estatisticapesqueiraSTP::create_ui("Estatistica pesqueira STP"),
                  server = estatisticapesqueiraSTP::create_server(
                    wd = NULL,
                    cache = NULL,
                    last_session = NULL,
                    local_data_directory = conf$config$local_data_directory,
                    repository_name = conf$repository_name,
                    repository_location = conf$repository_location,
                    forms = conf$forms,
                    databases = conf$databases,
                    variables = conf$variables,
                    config = conf$config,
                    cleaning_function = conf$cleaning_function,
                    zenodo = conf$zenodo,
                    artfish = artfish

                  ),
                  options = list(launch.browser = TRUE)

  )


}
