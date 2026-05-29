
opa <- function(){
  library(estatisticapesqueiraSTP)
  conf <- get_config_from_local()
  artfish <- list(
    dat = readxl::read_excel(path = file.path("inst", "config", "artfish.xlsx"), sheet = "dat"),
    vars = readxl::read_excel(path = file.path("inst", "config", "artfish.xlsx"),
                              sheet = "vars"))

  config <- conf$config
  load_data_from = "local"
  last_session = conf$last_session
  variables = conf$variables
  repository_name = conf$repository_name
  zenodo = conf$zenodo
  load_data_from = "server"

  export_artfish(config = config,
                 last_session = last_session,
                 variables = variables,
                 repository_name = repository_name,
                 zenodo = zenodo,
                 token = token,
                 load_data_from = "server",
                 artfish = artfish)

}

#' @export
export_artfish <- function(
    config,
    last_session,
    artfish,
    variables,
    load_data_from,
    zenodo,
    token,
    repository_name
){



  ################################################################################
  #                                LOAD DATABASES                                #
  ################################################################################




  if(load_data_from == "local"){
    all_data <- lapply(artfish[["dat"]]$dat, function(datname)
      load_local_data(datname               = datname,
                      local_data_directory  = config[["local_data_directory"]],
                      method                = last_session[["method"]],
                      fileformat            = last_session[["fileformat"]],
                      csv_separator         = last_session[["csv_separator"]],
                      class_list            = lapply(variables[[datname]], function(x) x$class)
      )
    )
    names(all_data) <- artfish[["dat"]]$dat
  }



  if(load_data_from == "server"){
    now <- gsub(" ", "_", gsub("-", "", gsub(":", "", substr(Sys.time(), 3, 19) )))
    dir.create(file.path(tempdir(), now))
    all_data_filepath <- file.path(tempdir(), now, "data.sqlite")
    suppressMessages(
      DOWNLOAD_DEPOSITION_FILE(title = repository_name,
                               token    = token,
                               instance = zenodo[["instance"]],
                               filename = "data.sqlite",
                               filepath = all_data_filepath)
    )


    # Read data in "data.sqlite" (creates file if it does not exist)
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          all_data_filepath)
    all_data <- lapply(DBI::dbListTables(con),
                       function(datname)  DBI::dbReadTable(conn = con,
                                                           name = datname))
    names(all_data) <- DBI::dbListTables(con)

    # Disconnect file
    DBI::dbDisconnect(con)
  }







  ################################################################################
  #                        RENAME VARIABLES AND DATABASES                        #
  ################################################################################




  for(i in 1:nrow(artfish[["dat"]])){
    datname         <- artfish[["dat"]]$dat[i]
    name            <- artfish[["dat"]]$artfish_dat[i]
    dat             <- all_data[[datname]]
    pos             <- which(artfish[["vars"]]$dat == datname &
                               artfish[["vars"]]$variablename %in% colnames(dat))
    varnames        <- artfish[["vars"]]$variablename[pos]
    artfish_varnames<- artfish[["vars"]]$artfishr_variablename[pos]
    pos_not_in_dat  <- which(artfish[["vars"]]$dat == datname &
                               !artfish[["vars"]]$variablename %in% colnames(dat))
    dat             <- dat[,varnames]
    colnames(dat)   <- artfish_varnames
    for(j in pos_not_in_dat){
      dat$temp = artfish[["vars"]]$allocate_value[j]
      colnames(dat)[which(colnames(dat) == "temp")] = artfish[["vars"]]$artfishr_variablename[j]
    }
    varnames        <- artfish[["vars"]]$artfishr_variablename[which(artfish[["vars"]]$dat == datname)]
    dat             <- dat[varnames]
    assign(name, dat)
  }








  ################################################################################
  #                                TRIP DURATION                                 #
  ################################################################################



  # Calculate time conversion factor

  estimation_unit = "day"
  times           = c(1,60,60,24, 365)
  names(times)    = c("sec", "min", "hour", "day", "year")
  for(i in 1:length(times)){
    pos = which(names(times) == estimation_unit)
    f                   = i - pos
    if(f == 0) f = 0 else f = f/abs(f)
    temp = 1
    for(j in c(i:pos)) temp = temp * (times[j+if(pos-j >0) 1 else 0]^f)
    times[i] <- temp
  }


  # Convert trip duration to day
  for(timeunit in names(times)){
    pos = which(tripinfo$effort_fishing_duration_unit == timeunit)
    if(length(pos) > 0)
      tripinfo$effort_fishing_duration[pos] <-
        tripinfo$effort_fishing_duration[pos] * times[which(names(times) == timeunit)]
  }
  tripinfo$effort_fishing_duration_unit = estimation_unit
  tripinfo$effort_fishing_duration[tripinfo$effort_fishing_duration < 0 ] <- NA
  tripinfo$effort_fishing_duration <- ceiling(tripinfo$effort_fishing_duration)




  ################################################################################
  #                                JOIN DATABASES                                #
  ################################################################################



  #  Append 'tripinfo' to 'landings'
  landings <- dplyr::inner_join(x = landings,
                                y = tripinfo,
                                by = c("fishing_trip"))





  ################################################################################
  #                          PROCESS ACTIVE VESSELS DATA                         #
  ################################################################################



  landing_site <- levels(as.factor(c(effort$landing_site,
                                     tripinfo$landing_site)))

  # Convert date to a single numeric value (for date comparison)
  active_vessels$date =
    as.numeric(as.character(active_vessels$year)) +
    (as.numeric(as.character(active_vessels$month)))/100


  # Create a copy of active vessels (observed data)
  active_vessels_obs  <- active_vessels

  # Create skeleton of new active vessels data.frame
  active_vessels <- as.data.frame(expand.grid(year = levels(as.factor(effort$year)),
                                              month = c(1:12),
                                              landing_site  = landing_site,
                                              fishing_unit  = levels(as.factor(effort$fishing_unit))
  ))

  # Convert date to a single numeric value
  active_vessels$date =
    as.numeric(as.character(active_vessels$year)) +
    (as.numeric(as.character(active_vessels$month)))/100

  # Create code of fishing unit per landing site per month per year
  active_vessels$code = paste0(active_vessels$year,
                               active_vessels$month,
                               active_vessels$landing_site,
                               active_vessels$fishing_unit)
  active_vessels_obs$code = paste0(active_vessels_obs$year,
                                   active_vessels_obs$month,
                                   active_vessels_obs$landing_site,
                                   active_vessels_obs$fishing_unit)

  # Join both data frames
  active_vessels <- dplyr::left_join(
    active_vessels,
    dplyr::select(active_vessels_obs, code, fleet_engagement_number),
    by = "code"
  )




  ################################################################################
  #               ESTIMATE MISSING 'FLEET ENGAGEMENT NUMBER' (FEN)               #
  ################################################################################



  # Estimate missing FEN values from last recorded FEN

  active_vessels$fleet_engagement_number <-
    unlist(
      temp <-    lapply(
        c(1:nrow(active_vessels)),
        function(i) {
          FEN <- active_vessels$fleet_engagement_number[i]
          if(is.na(FEN)) {
            pos = which(
              active_vessels_obs$fishing_unit == active_vessels$fishing_unit[i] &
                active_vessels_obs$landing_site == active_vessels$landing_site[i] &
                active_vessels_obs$date < (active_vessels$date[i]) &
                !is.na(active_vessels_obs$fleet_engagement_number)
            )
            temp = active_vessels_obs[pos,]
            if(nrow(temp) > 0) pos = which(temp$date == max(temp$date))
            if(length(pos) == 1) FEN = temp$fleet_engagement_number[pos]
            return(if(is.null(FEN)) NA else FEN)
          } else {
            return(FEN)
            }
        }
      )
    )




  # Estimate missing FEN values from max vessel count

  pos = which(is.na(active_vessels$fleet_engagement_number))
  for(i in pos){
    landing_site = active_vessels$landing_site[i]
    year = active_vessels$year[i]
    fishing_unit = active_vessels$fishing_unit[i]
    pos =     which(effort$landing_site == landing_site &
                      effort$fishing_unit == fishing_unit &
                      effort$year == year)
    if(length(pos) == 0){
      year = as.character(as.integer(year) -1)
      pos  =     which(effort$landing_site == landing_site &
                         effort$fishing_unit == fishing_unit &
                         effort$year == year)
    }
    if(length(pos) == 0) {
      fleet_engagement_number = NA
    } else {
      fleet_engagement_number = max(effort$fleet_engagement_number[pos])
    }

    n_records <- which(tripinfo$year         == year,
                       tripinfo$landing_site == landing_site,
                       tripinfo$fishing_unit == fishing_unit
    )

    if(length(n_records) == 0 &
       !is.na(fleet_engagement_number))
      fleet_engagement_number = 0

    active_vessels$fleet_engagement_number[i] = fleet_engagement_number
  }




  # Attach 'minor stratum' to active vessels

  landing_site <- levels(as.factor(c(effort$landing_site,
                                     tripinfo$landing_site)))
  minor_stratum  <-
    data.frame(
      landing_site  = landing_site,
      minor_stratum =
        unlist(
          lapply(
            landing_site,
            function(site)
              active_vessels_obs$minor_stratum[
                which(
                  active_vessels_obs$landing_site == site
                )][1]
          )
        )
    )

  active_vessels$minor_stratum <-
    unlist(
      lapply(
        active_vessels$landing_site,
        function(site)
          minor_stratum$minor_stratum[which(minor_stratum$landing_site == site)]
      ))





  # Attach "FEN max" to effort

  effort$code = paste0(effort$year,
                       effort$month,
                       effort$landing_site,
                       effort$fishing_unit)
  effort <- dplyr::left_join(
    effort,
    dplyr::select(active_vessels, code, fleet_engagement_max = fleet_engagement_number),
    by = "code"
  )


  effort         <- effort[,!colnames(effort) %in% c("code", "date")]
  active_vessels <- active_vessels[,!colnames(active_vessels) %in% c("code", "date")]


  pos = which(effort$fleet_engagement_number > effort$fleet_engagement_max)
  effort$fleet_engagement_max[pos] <- effort$fleet_engagement_number[pos]






  ################################################################################
  #                         CREATE ACTIVE DAYS DATABASE                          #
  ################################################################################




  active_days <- as.data.frame(expand.grid(year = c(2000:2100)[which(as.character(c(2000:2100)) %in%
                                                                       levels(as.factor(effort$year)))],
                                           month = c(1:12),
                                           minor_stratum = levels(as.factor(effort$minor_stratum)),
                                           landing_site  = levels(as.factor(effort$landing_site)),
                                           fishing_unit  = levels(as.factor(effort$fishing_unit)),
                                           effort_fishable_duration = as.integer(NA)
  )
  )

  active_days$effort_fishable_duration[active_days$month %in% c(9,4,6,11)]  <- 30
  active_days$effort_fishable_duration[!active_days$month %in% c(9,4,6,11)] <- 31
  active_days$effort_fishable_duration[active_days$month == 2]              <- 28
  active_days$effort_fishable_duration[
    active_days$month == 2 &
      active_days$year %in% seq(from = 2000, to = 2100, by = 4)] <- 29












  ################################################################################
  #                                 RUN ART FISH                                 #
  ################################################################################


  effort <- effort[!is.na(effort$fleet_engagement_number) &
                     !is.na(effort$fleet_engagement_number),]

  #--------------------------------------------------------------------
  if(!dir.exists("artfish_inputs")) dir.create("artfish_inputs")
  readr::write_csv(effort, "artfish_inputs/effort.csv")
  readr::write_csv(active_days, "artfish_inputs/active_days.csv")
  readr::write_csv(active_vessels, "artfish_inputs/active_vessels.csv")
  readr::write_csv(landings, "artfish_inputs/landings.csv")
  #--------------------------------------------------------------------

  if(!dir.exists("artfish")) dir.create("artfish")

  #activity coefficient
  activity_coefficient = artfishr::compute_effort_activity_coefficient(
    effort = effort,
    effort_source = "boat_counting",
    minor_strata = "minor_stratum"
  )



  writeDAT(data = activity_coefficient,
           fileformat = config[["fileformat"]],
           filepath = file.path("artfish",
                                paste0("activity_coefficient.",
                                       config[["fileformat"]])
           ),
           csv_separator = config[["csv_separator"]],
           sheetname = "effort_estimate")

  #effort estimate (includes calculation of activity coefficient)
  effort_estimate = artfishr::compute_effort_estimate(
    active_vessels = active_vessels,
    active_vessels_strategy = "closest",
    effort = effort,
    effort_source = "boat_counting",
    active_days = active_days,
    landings= landings,
    minor_strata = "minor_stratum"
  )
  writeDAT(data = effort_estimate,
           fileformat = config[["fileformat"]],
           filepath = file.path("artfish",
                                paste0("effort_estimate.",
                                       config[["fileformat"]])
                                ),
           csv_separator = config[["csv_separator"]],
           sheetname = "effort_estimate")

  #cpue
  cpue = artfishr::compute_cpue(landings, minor_strata = "minor_stratum")
  writeDAT(data = cpue,
           fileformat = config[["fileformat"]],
           filepath = file.path("artfish",
                                paste0("cpue.",
                                       config[["fileformat"]])
           ),
           csv_separator = config[["csv_separator"]],
           sheetname = "cpue")


  # Catch estimate
  catch_estimate = artfishr::compute_catch_estimate(effort_estimate, landings,minor_strata = "minor_stratum")
  writeDAT(data = catch_estimate,
           fileformat = config[["fileformat"]],
           filepath = file.path("artfish",
                                paste0("catch_estimate.",
                                       config[["fileformat"]])
           ),
           csv_separator = config[["csv_separator"]],
           sheetname = "catch_estimate")


  # Catch estimate by species
  catch_estimate_by_species = artfishr::compute_catch_estimates_by_species(landings, catch_estimate,minor_strata = "minor_stratum")
  writeDAT(data = catch_estimate_by_species,
           fileformat = config[["fileformat"]],
           filepath = file.path("artfish",
                                paste0("catch_estimate_by_species.",
                                       config[["fileformat"]])
           ),
           csv_separator = config[["csv_separator"]],
           sheetname = "catch_estimate_by_species")

  # Artfish C&E Report
  catch_and_effort_report = artfishr::compute_report(
    effort = effort,
    effort_source = "boat_counting",
    active_days = active_days,
    active_vessels = active_vessels,
    active_vessels_strategy = "closest",
    landings = landings,
    minor_strata = "minor_stratum"
  )
  catch_and_effort_report = as.data.frame(catch_and_effort_report)
  #Patch to deal with Inf values (due to effort 0)
  for(colname in colnames(catch_and_effort_report)){
    if(any(is.infinite(catch_and_effort_report[,colname]))) catch_and_effort_report[is.infinite(catch_and_effort_report[,colname]),][,colname] <- 0
  }
  catch_and_effort_report$species_label = catch_and_effort_report$species
  catch_and_effort_report$species_scientific = catch_and_effort_report$species
  catch_and_effort_report$fishing_unit_label = catch_and_effort_report$fishing_unit
  catch_and_effort_report$date = lubridate::make_date(catch_and_effort_report$year, catch_and_effort_report$month, 1)

  writeDAT(data = catch_and_effort_report,
           fileformat = config[["fileformat"]],
           filepath = file.path("artfish",
                                paste0("catch_and_effort_report.",
                                       config[["fileformat"]])
           ),
           csv_separator = config[["csv_separator"]],
           sheetname = "catch_and_effort_report")


}
