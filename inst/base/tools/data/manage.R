descr_out <- function(descr, ret_type = 'html') {
   ## if there is no data description
  if (descr %>% is_empty) return("")

  ## if there is a data description and we want html output
  if (ret_type == 'html')
    descr <- markdown::markdownToHTML(text = descr, stylesheet = "")

  descr
}

#### test
# library(markdown)
# is_empty("## header example")
# is_empty(NULL)
# descr_out(NULL)
# descr_out("## header example", 'html')
# descr_out("## header example", 'md')
#### end test

upload_error_handler <- function(objname, ret, r_data) {
  ## create an empty data.frame and return error message as description
  r_data[[paste0(objname,"_descr")]] <- ret
  r_data[[objname]] <- data.frame(matrix(rep("",12), nrow = 2))
}

loadClipboardData <- function(objname = "copy_and_paste", ret = "", header = TRUE, sep = "\t") {

  dat <- sshhr(try(
         {if (Sys.info()["sysname"] == "Windows") {
            read.table("clipboard", header = header, sep = sep, comment.char = "", fill = TRUE,  as.is = TRUE)
          } else if (Sys.info()["sysname"] == "Darwin") {
            read.table(pipe("pbpaste"), header = header, sep = sep, comment.char = "", fill = TRUE,  as.is = TRUE)
          } else {
            if (!is_empty(input$load_cdata))
              read.table(text = input$load_cdata, header = header, sep = sep, comment.char = "", fill = TRUE,  as.is = TRUE)
          }} %>% as.data.frame(check.names = FALSE), silent = TRUE))

  if (is(dat, 'try-error') || nrow(dat) == 0) {
    if (ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
    upload_error_handler(objname,ret)
  } else {
    ret <- paste0("### Clipboard data\nData copied from clipboard on ", lubridate::now())
    r_data[[objname]] <- dat %>% as.data.frame(check.names = FALSE)
    r_data[[paste0(objname,"_descr")]] <- ret
  }
  r_data[['datasetlist']] <- c(objname,r_data[['datasetlist']]) %>% unique
}

saveClipboardData <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == 'Windows') {
    write.table(.getdata(), "clipboard-10000", sep="\t", row.names=FALSE)
  } else if (os_type == "Darwin") {
    write.table(.getdata(), file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
  } else if (os_type == "Linux") {
    print("### Saving data through the clipboard is currently only supported on Windows and Mac. You can save your data to csv format to use it in a spreadsheet.")
  }
}

# A very rough method for detecting CSV headers based on whether the first row are all strings
detectCSVHeader <- function(csvFile) {
  hasHeader = TRUE
  csv = read.csv(csvFile, header = FALSE, stringsAsFactors = FALSE, nrows = 1)
  for (column in csv[1,]) {
    if (typeof(column) != "character") {
      hasHeader = FALSE
    }
  }
  return(hasHeader)
}

loadUserData <- function(fname, uFile, ext, r_data,
                         header = FALSE,
                         man_str_as_factor = FALSE,
                         sep = ",",
                         dec = ".") {

  filename <- basename(fname)
  ## objname is used as the name of the data.frame
  objname <- sub(paste0(".",ext,"$"),"", filename)

  ## if ext isn't in the filename nothing was replaced and so ...
  if (objname == filename) {
    fext <- tools::file_ext(filename) %>% tolower

    if (fext %in% c("xls","xlsx")) {
      ret <- "### Radiant does not load xls files directly. Please save the data as a csv file and try again."
    } else {
      ret <- paste0("### The filename extension (",fext,") does not match the specified file-type (",ext,"). Please specify the file type you are trying to upload (i.e., csv or rda).")
    }

    upload_error_handler(objname,ret,r_data)
    ext <- "---"
  }

  if (ext == 'rda') {
    ## objname will hold the name of the object(s) inside the R datafile
    robjname <- try(load(uFile), silent = TRUE)
    if (is(robjname, 'try-error')) {
      upload_error_handler(objname, "### There was an error loading the data. Please make sure the data are in either rda or csv format.")
    } else if (length(robjname) > 1) {
      if (sum(robjname %in% c("r_state", "r_data")) == 2) {
        upload_error_handler(objname,"### To restore state from a state-file select 'state' from the 'Load data of type' drowdown before uploading the file")
        rm(r_state, r_data) ## need to remove the local copies of r_state and r_data
      } else {
        upload_error_handler(objname,"### More than one R object contained in the data.")
      }
    } else {
      r_data[[objname]] <- as.data.frame(get(robjname))
      r_data[[paste0(objname,"_descr")]] <- attr(r_data[[objname]], "description")
    }
  }

  if (ext == 'csv') {
    header = detectCSVHeader(uFile)
    r_data[[objname]] <- loadcsv(uFile, header = header, sep = sep, saf = man_str_as_factor) %>%
      {if (is.character(.)) upload_error_handler(objname, mess) else .}
  }
  r_data[['datasetlist']] <<- c(objname, r_data[['datasetlist']]) %>% unique
}
