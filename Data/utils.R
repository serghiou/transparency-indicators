
# Filter XML ------------------------------------------------------------------

filter_css <- function(html_file, search_term){

  html_file %>% html_nodes(search_term) %>% html_text()

}


# Define filters ----------------------------------------------------------

filters <- c("title",        ".rTitleNotIE",
             "subject",      ".rSubject",
             "journal",      ".rJournal",
             "original_doi", ".smallFont:nth-child(5) .rNature",
             "retract_doi",  ".smallFont:nth-child(6) .rNature",
             "notice",       ".wrapCell .rNature",
             "paywall",      ".rPaywalled")

domains <- filters %>%
  matrix(ncol = 2, byrow = T) %>%
  as.data.frame(stringsAsFactors = F) %>%
  setNames(c("Domain", "Filter"))

# Download dataset --------------------------------------------------------

extract_data <- function(file    = "./Data/retraction_watch.html",
                         filters = domains$Filter,
                         domain  = domains$Domain){

  # Extract html using rvest
  page <- read_html(file)

  # Extract data using filters
  data_list <- lapply(filters, function(x) filter_css(page, x)) %>%
               setNames(domain)

  # Correct journal
  data_list$journal <- data_list$journal[seq(1, length(data_list$journal), 2)]

  # Extract html using XML
  parsed_table <- htmlParse(page) %>%
                  readHTMLTable %>%
                  extract("grdRetraction") %>%
                  as.data.frame(stringsAsFactors = F) %>%
                  mutate_all(funs(as.character)) %>%
                  slice(2:n()) %>%
                  select(-contains("V1")) %>%
                  setNames(c("V2", "V3", "V4", "V5", "V6", "V7", "V8"))

  # Length of table
  n <- nrow(parsed_table)

  # Extract manually
  data_list$institute <- lapply(1:n,
                                function(x) strsplit(parsed_table$V2[x],
                                                     data_list$journal[x],
                                                     fixed = T)[[1]][2]) %>%
                         unlist()

  data_list$reason <- parsed_table$V3 %>%
                      gsub('\n', '; ', .) %>%
                      gsub("[+]", " ", .)

  data_list$authors <- parsed_table$V4 %>%
                       gsub('([[:lower:]])([[:upper:]])', '\\1; \\2', .)

  data_list$original_date <- substr(parsed_table$V5, 1, 10)

  data_list$original_pubmed <- lapply(1:n,
                                      function(x) strsplit(parsed_table$V5[x],
                                                           data_list$original_doi[x],
                                                           fixed = T)[[1]][1]) %>%
                               unlist() %>%
                               substring(11)

  data_list$retract_date <- substr(parsed_table$V6, 1, 10)

  data_list$retract_pubmed <- lapply(1:n,
                                     function(x) strsplit(parsed_table$V6[x],
                                                          data_list$retract_doi[x],
                                                          fixed = T)[[1]][1]) %>%
                              unlist() %>%
                              substring(11)

  data_list$article_type <- lapply(1:n,
                                   function(x) strsplit(parsed_table$V7[x],
                                                        data_list$notice[x],
                                                        fixed = T)[[1]][1]) %>%
                            unlist()

  data_list$countries <- lapply(1:n, function(x) strsplit(parsed_table$V8[x],
                                                          data_list$paywall[x],
                                                          fixed = T)[[1]][1]) %>%
                         unlist() %>%
                         gsub('([[:lower:]])([[:upper:]])', '\\1; \\2', .)



  x <- as.data.frame(data_list)
  return(x[c(1, 10, 3, 8, 2, 11, 12, 4, 13, 14, 5, 15, 6, 9, 16, 7)])
}



# Export to Excel ---------------------------------------------------------

export_xlsx <-
  function(object, file = "../Data/retraction_data.xlsx") {

    # Load packages
    require(XLConnect)

    # Export to Excel
    wb <- loadWorkbook(file, create = TRUE)

    # Name the sheet of the workbook
    createSheet(wb, name = "Data")

    # Print the dataframe into the workbook
    # (use rownames = "Row" only when you want to print the names of each row)
    writeWorksheet(wb, object, sheet = "Data", rownames = "Row")

    # Save the workbook
    saveWorkbook(wb)
  }



# Fetch citation count ----------------------------------------------------

getCitations <- function(doi){

  # Packages
  require(rcrossref)

  # Catch error
  citations <- tryCatch(cr_citation_count(doi), error = function(e) e)

  # Return value
  if (inherits(citations, "error")) {
    return(0)
  } else {
    return(citations)
  }
}



# Fetch Altmetric data ----------------------------------------------------

extract_aas <- function(doi, pmid){

  # Do this if given DOI
  if (!missing("doi")) {
    # Catch error
    aas_data <- tryCatch(altmetrics(doi = doi), error = function(e) e)

    # Return value
    if (inherits(aas_data, "error")) {
      return(data.frame(isFound = "No"))
    } else {
      return(altmetric_data(aas_data))
    }
  }

  # Do this if given PMID
  if (!missing("pmid")) {
    # Catch error
    aas_data <- tryCatch(altmetrics(pmid = pmid), error = function(e) e)

    # Return value
    if (inherits(aas_data, "error")) {
      return(data.frame(isFound = "No"))
    } else {
      return(altmetric_data(aas_data))
    }
  }
}
