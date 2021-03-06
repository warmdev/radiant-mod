################################################################################
## functions to set initial values and take information from r_state
## when available
################################################################################

## options to set for debugging
# options(shiny.trace = FALSE)
# options(shiny.trace = TRUE)
# options(shiny.error = recover)
# options(warn = 2)
# options(warn = 0)
## turn off warnings globally
# options(warn=-1)

init_state <- function(r_data) {

  ## initial plot height and width
  r_data$plot_height <- 600
  r_data$plot_width <- 600

  r_data$manual <- FALSE
  r_data$vim_keys <- FALSE

  ## Joe Cheng: "Datasets can change over time (i.e., the .changedata function).
  ## Therefore, the data need to be a reactive value so the other reactive
  ## functions and outputs that depend on these datasets will know when they
  ## are changed."
#   robj <- load(file.path(r_path,"base/data/diamonds.rda"))
#   df <- get(robj)
#   r_data[["diamonds"]] <- df
#   r_data[["diamonds_descr"]] <- attr(df,'description')
#   r_data$datasetlist <- c("diamonds")
#   r_data$url <- NULL
#   r_data
  
  isolate({
    # Get path from url request
    query <- parseQueryString(session$clientData$url_search)
    project_path <- query$project
    # Default path
    if (is.null(project_path)) {
      project_path <- "luxedemo"
    }
    # Remove any "./" characters
    project_path <- gsub("[./]", "", project_path)
    # Construct the actual path
    project_path <- paste0(project_path, "/data")
    project_path <- paste0("miracle/", project_path)
    data_path <- file.path(r_path,project_path)
    # Check if path exists
    if (!file_test(op="-d", data_path)) {
      data_path <- "../miracle/luxedemo/data" 
    }
    # Load files
    data_files <- list.files(data_path)
    # Only display folders that has CSV files
    folderlist <- character()
    folder_list <- list.dirs(data_path)
    for (folder in folder_list) {
      csv_files = list.files(folder, "*.csv")
      if (length(csv_files) != 0) {
        folderlist <- c(folderlist, substring(folder, 12))
      }
    }
    # Construct the folderlist and datasetlist for UI
    r_data[['folderlist']] <- folderlist
    r_data[['datasetlist']] <- character()
    
    for (data_file in data_files) {
      if (!file.info(file.path(data_path, data_file))$isdir) {
        #loadUserData(data_file, file.path(data_path,data_file), 'csv', r_data)
      }
    }
    # sorting files alphabetically
    r_data[['datasetlist']] <- sort(r_data[['datasetlist']])
  })
  
  r_data
}

# if (!r_local) {
if (TRUE) {

  # state_email <- function(body, subject = paste0("From: ", Sys.info()['nodename'])) {
  #   if (!require(sendmailR)) {
  #     install.packages("sendmailR", repos = "http://cran.rstudio.com")
  #     library(sendmailR)
  #   }

  #   from <- '<vincent.nijs@gmail.com>'
  #   to <- '<vincent.nijs@gmail.com>'
  #   body <- paste0(body,collapse="\n")
  #   sendmail(from, to, subject, body,
  #            control=list(smtpServer='ASPMX.L.GOOGLE.COM'))
  # }

  # check_age_and_size <- function() {

  #   ids <- ls(r_sessions)
  #   ages <- list()
  #   for (i in ids) {
  #     session_age <- difftime(Sys.time(), r_sessions[[i]]$timestamp, units = "days")
  #     if (session_age > 1) r_sessions[[i]] <- NULL
  #     ages[i] <- session_age %>% round(3)
  #   }

  #   session_size <- pryr::object_size(r_sessions) %>% as.numeric %>%
  #                     {. / 1048576} %>% round(3)

  #   if (length(r_sessions) > 20 || session_size > 20)
  #     state_email(c("Session size (MB):",session_size,"\nSession ages in days:",ages))
  # }

  ## are there any state files dumped more than 1 minute ago?
  # check_age_and_size()


  remove_session_files <- function(st = Sys.time()) {
    fl <- list.files(normalizePath("~/r_sessions/"), pattern = "*.rds",
                     full.names = TRUE)

    for (f in fl) {
      if (difftime(st, file.mtime(f), units = "days") > 7)
        unlink(f, force = TRUE)
    }
  }

  remove_session_files()
}

## from Joe Cheng's https://github.com/jcheng5/shiny-resume/blob/master/session.R
isolate({
  prevSSUID <- parseQueryString(session$clientData$url_search)[["SSUID"]]
})

most_recent_session_file <- function() {
  fl <- list.files(normalizePath("~/r_sessions/"), pattern = "*.rds",
                   full.names = TRUE)

  if (length(fl) > 0) {
    data.frame(fn = fl, dt = file.mtime(fl)) %>% arrange(desc(dt)) %>%
    slice(1) %>% .[["fn"]] %>% as.character %>% basename %>%
    gsub("r_(.*).rds","\\1",.)
  } else {
    NULL
  }
}

## set the session id
r_ssuid <-
  if (r_local) {
    if (is.null(prevSSUID)) {
      mrsf <- most_recent_session_file()
      paste0("local-",shiny:::createUniqueId(3))
    } else {
      mrsf <- "0000"
      prevSSUID
    }
  } else {
    ifelse (is.null(prevSSUID), shiny:::createUniqueId(5), prevSSUID)
  }

## (re)start the session and push the id into the url
session$sendCustomMessage("session_start", r_ssuid)

## load for previous state if available but look in global memory first
if (exists("r_state") && exists("r_data")) {
  r_data  <- do.call(reactiveValues, r_data)
  r_state <- r_state
  rm(r_data, r_state, envir = .GlobalEnv)
} else if (!is.null(r_sessions[[r_ssuid]]$r_data)) {
  r_data  <- do.call(reactiveValues, r_sessions[[r_ssuid]]$r_data)
  r_state <- r_sessions[[r_ssuid]]$r_state
} else if (file.exists(paste0("~/r_sessions/r_", r_ssuid, ".rds"))) {
  ## read from file if not in global
  fn <- paste0(normalizePath("~/r_sessions"),"/r_", r_ssuid, ".rds")

  rs <- try(readRDS(fn), silent = TRUE)
  if (is(rs, 'try-error')) {
    r_data  <- init_state(reactiveValues())
    r_state <- list()
  } else {
    if (length(rs$r_data) == 0)
      r_data  <- init_state(reactiveValues())
    else
      r_data  <- do.call(reactiveValues, rs$r_data)

    if (length(rs$r_state) == 0)
      r_state <- list()
    else
      r_state <- rs$r_state
  }

  unlink(fn, force = TRUE)
  rm(rs)
} else if (r_local && file.exists(paste0("~/r_sessions/r_", mrsf, ".rds"))) {

  ## restore from local folder but assign new ssuid
  fn <- paste0(normalizePath("~/r_sessions"),"/r_", mrsf, ".rds")

  rs <- try(readRDS(fn), silent = TRUE)
  if (is(rs, 'try-error')) {
    r_data  <- init_state(reactiveValues())
    r_state <- list()
  } else {
    if (length(rs$r_data) == 0)
      r_data  <- init_state(reactiveValues())
    else
      r_data  <- do.call(reactiveValues, rs$r_data)

    if (length(rs$r_state) == 0)
      r_state <- list()
    else
      r_state <- rs$r_state
  }

  ## don't navigate to same tab in case the app locks again
  r_state$nav_radiant <- NULL

  unlink(fn, force = TRUE)
  rm(rs)
} else {
  r_data  <- init_state(reactiveValues())
  r_state <- list()
}

if (r_local) {
  ## reference to radiant environment that can be accessed by exported functions
  ## does *not* make a copy of the data - nice
  ## relevant when you want to access Radiant functions outside of a Shiny app
  # r_env <<- pryr::where("r_data")
  # r_env <- environment()

  ## doesn't work when Radiant is loaded and in the search path
  ## i.e., it is not 'sourced' inside server.R
  # r_env <- environment()
  # ?shiny::getDefaultReactiveDomain

  ## works but puts r_env in the global environment so 'new session' doesn't work properly
  ## when run locally

  # if ("package:radiant" %in% search())
  #   r_env <<- environment()
  # else
  #   r_env <- environment()

    r_env <- environment()

  ## adding any data.frame from the global environment to r_data should not affect
  ## memory usage ... at least until the entry in r_data is changed
  df_list <- sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.data.frame) %>%
    { names(.[.]) }

  for (df in df_list) {
    isolate({
      r_data[[df]] <- get(df, envir = .GlobalEnv)
      r_data[[paste0(df,"_descr")]] <- attr(r_data[[df]],'description') %>%
        { if (is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\n Check the 'Add/edit data description' box on the left of your screen" else . }
      r_data$datasetlist %<>% c(df, .) %>% unique
    })
  }
}

#####################################
## url processing to share results
#####################################

## relevant links
# http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs/25385474#25385474
# https://groups.google.com/forum/#!topic/shiny-discuss/Xgxq08N8HBE
# https://gist.github.com/jcheng5/5427d6f264408abf3049

## try http://127.0.0.1:3174/?url=decide/simulate/&SSUID=local

url_list <-
  list("Data" = list("tabs_data" = list("Manage"    = "data/",
                                        "View"      = "data/view/",
                                        "Visualize" = "data/visualize/",
                                        "Pivot"     = "data/pivot/",
                                        "Explore"   = "data/explore/",
                                        "Transform" = "data/transform/",
                                        "Combine"   = "data/combine/",
                                        "Metadata"  = "data/metadata/")),

       "Sampling"    = "sample/sampling/",
       "Sample size" = "sample/sample-size/",

       "Single mean" = list("tabs_single_mean" = list("Summary" = "base/single-mean/",
                                                      "Plot"    = "base/single-mean/plot/")),

       "Compare means" = list("tabs_compare_means" = list("Summary" = "base/compare-means/",
                                                          "Plot"    = "base/compare-means/plot/")),

       "Single proportion" = list("tabs_single_prop" = list("Summary" = "base/single-prop/",
                                                            "Plot"    = "base/single-prop/plot/")),

       "Compare proportions" = list("tabs_compare_props" = list("Summary" = "base/compare-props/",
                                                                "Plot"    = "base/compare-props/plot/")),

       "Cross-tabs" = list("tabs_cross_tabs" = list("Summary" = "base/cross-tabs/",
                                                     "Plot"    = "base/cross-tabs/plot/")),

       "Correlation" = list("tabs_correlation" = list("Summary" = "regression/correlation/",
                                                      "Plot"    = "regression/correlation/plot/")),

       "Linear (OLS)" = list("tabs_regression" = list("Summary" = "regression/linear/",
                                                      "Predict" = "regression/linear/predict/",
                                                      "Plot"    = "regression/linear/plot/")),

       "GLM" = list("tabs_glm_reg" = list("Summary" = "regression/glm/",
                                          "Predict" = "regression/glm/predict/",
                                          "Plot"    = "regression/glm/plot/")),

       "Decision tree"    = list("tabs_dtree"    = list("Model" = "decide/dtree/",
                                                        "Plot"  = "decide/dtree/plot/")),

       "Simulate"    = list("tabs_simulate"    = list("Model"   = "decide/simulate/",
                                                      "Repeat"  = "decide/simulate/repeat/"))
  )

## generate url patterns for navigation
url_patterns <- list()
for (i in names(url_list)) {
  res <- url_list[[i]]
  if (!is.list(res)) {
    url_patterns[[res]] <- list("nav_radiant" = i)
  } else {
    tabs <- names(res)
    for (j in names(res[[tabs]])) {
      url <- res[[tabs]][[j]]
      url_patterns[[url]] <- setNames(list(i,j), c("nav_radiant",tabs))
    }
  }
}

## try http://127.0.0.1:3174/?url=decide/simulate/&SSUID=local
# unlink("~/gh/radiant/tests/urls/urls.Rmd")
# urls <- grep("/", url_list %>% unlist, value = TRUE)
# for(u in urls) {
#   cat(paste0("http://127.0.0.1:6452/?url=", u, "&SSUID=local<br>\n"),
#       file = "~/gh/radiant/tests/urls/urls.Rmd", append = TRUE)
# }
# knitr::knit2html("~/gh/radiant/tests/urls/urls.Rmd", output = "~/gh/radiant/tests/urls/urls.html")

## environment to results from code run through knitr
# r_knitr <- new.env(parent = emptyenv())
# if (is.null(isolate(r_data$r_knitr))) {
  # isolate({
    # r_data$r_knitr <- if (exists("r_env")) new.env(parent = r_env) else new.env()
  # })
# }


if (!exists("r_knitr")) {
  r_knitr <- if (exists("r_env")) new.env(parent = r_env) else new.env()
}

## parse the url and use updateTabsetPanel to navigate to the desired tab
observe({
  url_query <- parseQueryString(session$clientData$url_search)
  if ("url" %in% names(url_query)) {
    r_data$url <- url_query$url
  } else if (is_empty(r_data$url)) {
    return()
  }

  ## create an observer and suspend when done
  url_observe <- observe({
    if (is.null(input$dataset)) return()
    url <- url_patterns[[r_data$url]]
    if (is.null(url)) {
      ## if pattern not found suspend observer
      url_observe$suspend()
      return()
    }
    ## move through the url
    for (u in names(url)) {
      if (is.null(input[[u]])) return()
      if (input[[u]] != url[[u]])
        updateTabsetPanel(session, u, selected = url[[u]])
      if (names(tail(url,1)) == u) url_observe$suspend()
    }
  })
})

## keeping track of the main tab we are on
observe({
  if (is_empty(input$nav_radiant)) return()
  if (input$nav_radiant != "Stop" && input$nav_radiant != "Refresh")
    r_data$nav_radiant <- input$nav_radiant
})

## Jump to the page you were on
## only goes two layers deep at this point
if (!is.null(r_state$nav_radiant)) {

  ## don't return-to-the-spot if that was quit or stop
  if (r_state$nav_radiant %in% c("Refresh","Stop")) return()

  ## naming the observer so we can suspend it when done
  nav_observe <- observe({
    ## needed to avoid errors when no data is available yet
    if (is.null(input$dataset)) return()
    updateTabsetPanel(session, "nav_radiant", selected = r_state$nav_radiant)

    ## check if shiny set the main tab to the desired value
    if (is.null(input$nav_radiant)) return()
    if (input$nav_radiant != r_state$nav_radiant) return()
    nav_radiant_tab <- url_list[[r_state$nav_radiant]] %>% names

    if (!is.null(nav_radiant_tab) && !is.null(r_state[[nav_radiant_tab]]))
      updateTabsetPanel(session, nav_radiant_tab, selected = r_state[[nav_radiant_tab]])

    ## once you arrive at the desired tab suspend the observer
    nav_observe$suspend()
  })
}

## 'sourcing' radiant's package functions in the server.R environment
if (!"package:radiant" %in% search()) {
  ## for shiny-server
  if (r_path == "..") {
    for (file in list.files("../../R",
        pattern="\\.(r|R)$",
        full.names = TRUE)) {

      source(file, encoding = r_encoding, local = TRUE)
    }
  } else {
    ## for shinyapps.io
    radiant::copy_all(radiant)
    set_class <- radiant::set_class         ## needed but not clear why
    environment(set_class) <- environment() ## needed but not clear why
  }
} else {
  ## for use with launcher
  radiant::copy_all(radiant)
  set_class <- radiant::set_class         ## needed but not clear why
  environment(set_class) <- environment() ## needed but not clear why
}
