


#' @export
create_ui <- function(header)
  bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(header),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    body = bs4Dash::dashboardBody(
      tags$head(
        tags$style(HTML("
          .modal-dialog {
            width: 100vw;
            max-width: 100vw;
            height: 100vh;
            margin: 0;
          }

          .modal-content {
            height: 100vh;
            border-radius: 0;
          }

          .modal-body {
            height: calc(100vh - 120px);
            overflow-y: auto;
          }
        "))
      ),
      shiny::uiOutput("page")
    )
  )




#' @export
create_server <- function(wd = getwd(),
                          cache = NULL,
                          last_session = NULL,
                          local_data_directory,
                          repository_name,
                          repository_location,
                          forms,
                          databases,
                          variables,
                          config,
                          cleaning_function,
                          zenodo,
                          artfish)

  function(input, output, session){

    rv <- shiny::reactiveValues(
      step         = 0,
      wd           = wd,
      cache        = cache,
      last_session = last_session,
      language     = NULL,
      messages     = list(),
      token        = NULL,
      kobo         = NULL,
      log          = character(0),
      proc         = NULL,
      log_file     = NULL,
      running      = FALSE
    )




    # ---------------------------  UIs  ---------------------------- #



    output$page <- shiny::renderUI({

      if(rv$step == 0){
        shiny::tagList(
          shiny::h1("\n"),
          shiny::actionButton("lang_en", "English"),
          shiny::actionButton("lang_pt", "Português"),
          shiny::actionButton("lang_es", "Español")
        )
      } else

        if(rv$step == 0.1){
          shiny::tagList(
            shiny::h4("Select working directory"),
            shiny::textInput("wd_path", "Working directory path", value = "",
                             placeholder = "/path/to/directory"),
            shinyFiles::shinyDirButton("choose_wd", "Browse...",
                                       title = "Select working directory"),
            shiny::actionButton("confirm_wd", "OK"),
            shiny::uiOutput("wd_error")
          )
        } else

        if(rv$step == 1){
          shiny::tagList(
            shiny::h4(rv$messages$zenodo_authentication_TITLE),
            shiny::textInput("token", rv$messages$enter_your_zenodo_token),
            shiny::actionButton("submit_token", "OK"),
            shiny::uiOutput("zenodo_wrong_token")
          )
        } else

          if(rv$step == 2){
            shiny::tagList(
              shiny::h4(rv$messages$zenodo_authentication_TITLE),
              shiny::textOutput("zenodo_confirm"),
              shiny::actionButton("yes_token", rv$messages$yes),
              shiny::actionButton("no_token",  rv$messages$no)
            )
          } else

            if(rv$step == 3){
              shiny::tagList(
                shiny::h4(rv$messages$kobotoolbox_authentication_TITLE),
                shiny::textInput("kobo_url",       rv$messages$input_kobo_url),
                shiny::textInput("kobo_user",      rv$messages$kobo_username),
                shiny::passwordInput ("kobo_pwd",  rv$messages$kobo_password),
                shiny::actionButton("kobo_login",  rv$messages$kobo_login),
                shiny::uiOutput("kobo_wrong_credentials")
              )
            } else

              if(rv$step == 4){
                shiny::tagList(
                  shiny::h4(rv$messages$kobotoolbox_authentication_TITLE),
                  shiny::textOutput("kobo_confirm"),
                  shiny::actionButton("yes_kobo", rv$messages$yes),
                  shiny::actionButton("no_kobo",  rv$messages$no)
                )
              } else

                if(rv$step == 5){

                  if(rv$running){
                    shiny::tagList(
                      shiny::h4("Running..."),
                      shiny::verbatimTextOutput("log_output")
                    )
                  } else {
                    shiny::tagList(
                      shiny::actionButton("run_1", rv$messages$download_new_data),
                      shiny::br(), shiny::br(),
                      shiny::actionButton("run_2", rv$messages$collate_new_data),
                      shiny::br(), shiny::br(),
                      shiny::actionButton("run_3", rv$messages$update_data_from_local),
                      shiny::br(), shiny::br(),
                      shiny::actionButton("run_4", rv$messages$delete_processed_instances),
                      shiny::br(), shiny::br(),
                      shiny::actionButton("run_5", rv$messages$estimate_catch),
                      shiny::hr(), shiny::br(),
                      shiny::actionButton("open_artfish_ui_modal", rv$messages$visualize_artfish_results)
                      #shiny::verbatimTextOutput("log_output")
                    )
                  }
                }
    })






    # ---------------------  STEP 0: LANGUAGE  --------------------- #





    shiny::observeEvent(input$lang_en, {
      rv$messages <- interface_messages("en")
      rv$language <- "en"
      if(is.null(wd)) rv$step <- 0.1 else rv$step <- 1
    })

    shiny::observeEvent(input$lang_pt, {
      rv$messages <- interface_messages("pt")
      rv$language <- "pt"
      if(is.null(wd)) rv$step <- 0.1 else rv$step <- 1
    })

    shiny::observeEvent(input$lang_es, {
      rv$messages <- interface_messages("es")
      rv$language <- "es"
      if(is.null(wd)) rv$step <- 0.1 else rv$step <- 1
    })





    # ----------------------  STEP 0.1: WORKING DIR  --------------------- #




    roots <- c(Home = path.expand("~"))

    shinyFiles::shinyDirChoose(input, "choose_wd", roots = roots, session = session)

    shiny::observeEvent(input$choose_wd, {
      shiny::req(is.list(input$choose_wd))   # only fires after real selection
      path <- shinyFiles::parseDirPath(roots, input$choose_wd)
      if(length(path) > 0)
        shiny::updateTextInput(session, "wd_path", value = path)
    })

    shiny::observeEvent(input$confirm_wd, {
      path <- input$wd_path
      if(!dir.exists(path)){
        output$wd_error <- shiny::renderUI(
          shiny::tags$p("Directory does not exist.", style = "color: red; font-size: 18px;")
        )
      } else {
        rv$wd <- path
        setwd(path)
        if(is.null(cache)) rv$cache = "cache"
        if(!dir.exists(rv$cache)) dir.create(rv$cache)
        if(is.null(last_session) &&
          file.exists(file.path(rv$cache, "settings.rds"))){
          rv$last_session <- readRDS(file.path(rv$cache, "settings.rds"))
        }
        rv$step <- 1
      }
    })



    # ----------------------  STEP 1: ZENODO  ---------------------- #


    # Get zenodo link based on zenodo instance
    if(zenodo[["instance"]] == "zenodo")
      zenodo_link <- "https://zenodo.org/api/deposit/depositions"
    if(zenodo[["instance"]] == "sandbox")
      zenodo_link <- "https://sandbox.zenodo.org/api/deposit/depositions"



    # Functions to test whether zenodo credentials are correct
    test_zenodo <- function(token){
      res <- try(httr::GET(zenodo_link,
                           httr::add_headers(Authorization = paste("Bearer", token)),
                           query = list(size = 1)),
                 silent = TRUE)
      !inherits(res, "try-error") && httr::status_code(res) == 200
    }


    # Check for cached file and test it
    shiny::observe({
      shiny::req(rv$step == 1)
      token_path <- file.path(rv$cache, "zenodo.rds")

      if(file.exists(token_path)){

        token <- readRDS(token_path)

        if(test_zenodo(token)){
          rv$token <- token
          rv$step <- 2
        } else {
          file.remove(token_path)
        }
      }
    })


    # Check credentials inputted by user and test them
    shiny::observeEvent(input$submit_token, {

      token_path <- file.path(rv$cache, "zenodo.rds")

      token <- input$token

      if(test_zenodo(token)){
        saveRDS(token, token_path)
        rv$token <- token
        rv$step <- 2
      } else {
        output$zenodo_wrong_token <- shiny::renderUI(
          shiny::tags$p(rv$messages$invalid_token, style = "color: red; font-size: 18px;")
        )
      }
    })


    # Display confirmation panel
    output$zenodo_confirm <- shiny::renderText({
      paste0(rv$messages$using_zenodo_token, " '", rv$token, "'\n", rv$messages$continue)
    })

    shiny::observeEvent(input$yes_token, { rv$step <- 3 })
    shiny::observeEvent(input$no_token, {
      token_path <- file.path(rv$cache, "zenodo.rds")
      if(file.exists(token_path)) file.remove(token_path)
      rv$step <- 1
    })





    # -----------------------  STEP 2: KOBO  ----------------------- #




    # Functions to test whether kobo credentials are correct
    test_kobo <- function(kobo){
      res <- try(httr::GET(paste0("https://", kobo$url, "/api/v2/assets.json"),
                           httr::authenticate(user     = kobo$username,
                                              password = kobo$password)),
                 silent = TRUE)
      !inherits(res, "try-error") && httr::status_code(res) == 200
    }



    # Check for cached credentials and test them

    shiny::observe({
      shiny::req(rv$step == 3)

      kobo_path <- file.path(rv$cache, "kobo.rds")

      if(file.exists(kobo_path)){

        kobo <- readRDS(kobo_path)

        if(!all(c("url","username","password") %in% names(kobo))){
          file.remove(kobo_path)
          return()
        }

        if(test_kobo(kobo)){
          saveRDS(kobo, kobo_path)
          rv$kobo <- kobo
          rv$step <- 4
        } else {
          file.remove(kobo_path)
          rv$step <- 3
        }

      }
    })



    # Check credentials inputted by user and test them
    shiny::observeEvent(input$kobo_login, {

      kobo_path <- file.path(rv$cache, "kobo.rds")

      kobo <- list(
        url      = input$kobo_url,
        username = input$kobo_user,
        password = input$kobo_pwd
      )

      if(test_kobo(kobo)){
        saveRDS(kobo, kobo_path)
        rv$kobo <- kobo
        rv$step <- 4
      } else {
        output$kobo_wrong_credentials <- shiny::renderUI(
          shiny::tags$p(rv$messages$kobo_login_failed, style = "color: red; font-size: 18px;")
        )
      }
    })



    # Display confirmation panel
    output$kobo_confirm <- shiny::renderText({
      paste0(rv$messages$logged_in_kobotoolbox_as, " ", rv$kobo$username, ". \n", rv$messages$continue)
    })

    shiny::observeEvent(input$yes_kobo, { rv$step <- 5 })

    shiny::observeEvent(input$no_kobo, {
      kobo_path <- file.path(rv$cache, "kobo.rds")
      if(file.exists(kobo_path)) file.remove(kobo_path)
      rv$step <- 3
    })





    # ----------------  STEP 3: RUNNING KOBOMANAGER  --------------- #


    ### Function that runs functions with even logger

    buttons_with_logger <- function(fun, args) {

      rv$log      <- character(0)
      rv$log_file <- tempfile()
      rv$running  <- TRUE

      rv$proc <- callr::r_bg(
        func = function(log_file,
                        fun,
                        args) {


          # Redirect cat() output to the log file
          con <- file(log_file, open = "at")
          sink(con, type = "output")
          sink(con, type = "message")

          log <- function(...) {
            cat(..., sep = "")
            flush(con)
          }

          withCallingHandlers({
            do.call(fun, args)
          },
          message = function(m) {
            log(conditionMessage(m))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            log("WARNING:", conditionMessage(w))
            invokeRestart("muffleWarning")
          },
          error = function(e) {
            log("ERROR:", conditionMessage(e))
          })

          sink(type = "output")
          sink(type = "message")
          close(con)
        },
        args = list(
          log_file = rv$log_file,
          fun      = fun,
          args     = args
        )
      )
    }

    ### Button events

    shiny::observeEvent(input$run_1, {
      buttons_with_logger(
        fun = download_new_submissions,
        args = list(kobo                = rv$kobo,
                    repository_name     = repository_name,
                    repository_location = repository_location,
                    forms               = forms,
                    databases           = databases,
                    variables           = variables,
                    config              = config,
                    cleaning_function   = cleaning_function,
                    zenodo              = zenodo,
                    token               = rv$token,
                    language            = rv$language)
      )
    })

    shiny::observeEvent(input$run_2, {
      buttons_with_logger(
        fun = append_and_update,
        args = list(local_data_directory = local_data_directory,
                    variables            = variables,
                    databases            = databases,
                    config               = config,
                    last_session         = rv$last_session,
                    zenodo               = zenodo,
                    token                = rv$token,
                    repository_name      = repository_name,
                    cache                = rv$cache,
                    update_server_data   = FALSE,
                    language             = rv$language)
      )
      })

    shiny::observeEvent(input$run_3, {

      buttons_with_logger(
        fun = append_and_update,
        args = list(local_data_directory = local_data_directory,
                    variables            = variables,
                    databases            = databases,
                    config               = config,
                    last_session         = rv$last_session,
                    zenodo               = zenodo,
                    token                = rv$token,
                    repository_name      = repository_name,
                    cache                = rv$cache,
                    update_server_data   = TRUE,
                    language             = rv$language)
      )
      })

    shiny::observeEvent(input$run_4, {
      buttons_with_logger(
        fun = delete_instances,
        args = list(forms = forms,
                    kobo = rv$kobo,
                    zenodo = zenodo,
                    repository_name = repository_name,
                    token = rv$token,
                    language = rv$language)
      )
      })

    shiny::observeEvent(input$run_5, {
      buttons_with_logger(
        fun = export_artfish,
        args = list(config = config,
                    last_session = rv$last_session,
                    variables = variables,
                    repository_name = repository_name,
                    zenodo = zenodo,
                    token = rv$token,
                    load_data_from = "server",
                    artfish = artfish)
      )

      })

    shiny::observeEvent(input$open_artfish_ui_modal, {

      #read back Artfish report
      estimates = reactive({ readxl::read_excel(file.path("artfish", paste0("catch_and_effort_report.", config[["fileformat"]]))) })

      artfishr::artfish_shiny_overview_server("artfish_overview", lang = rv$language, estimate = estimates, effort_source = "boat_counting", minor_strata = "minor_stratum")

      shiny::showModal(
        shiny::modalDialog(
          title = rv$messages$visualize_artfish_results,
          artfishr::artfish_shiny_overview_ui("artfish_overview"),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l"
        )
      )

    })



    # ----------------  STEP 3: RUNNING KOBOMANAGER  --------------- #


    shiny::observe({

      shiny::req(rv$proc)

      shiny::invalidateLater(100, session)

      if(file.exists(rv$log_file)){
        rv$log <- readLines(rv$log_file, warn = FALSE)
      }

      if(!rv$proc$is_alive()){
        rv$running <- FALSE
        rv$proc <- NULL
      }
    })

    output$log_output <- shiny::renderText({
      paste(rv$log, collapse = "\n")
    })

  }
