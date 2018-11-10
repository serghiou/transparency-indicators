
# From DOI to PMID without CrossRef ---------------------------------------

trueDoi <- function(x){

  # Only replace for wrong DOIs
  if(length(grep("^S", x$DOI)) > 0){

    # Extract DOI for each article and replace
    x$DOI <- efetch(x$PMID, db = "pubmed", retmode = "xml")$content %>%
      xmlParse() %>%
      getNodeSet("//PubmedArticle") %>%
      lapply(xpathSApply, ".//ELocationID[@EIdType = \"doi\"]", xmlValue) %>%
      unlist
  }
  return(x)
}


# From DOI to PMID with CrossRef ------------------------------------------

doiToPmid <- function(x){

  # Extract PMID from CrossRef
  a <- id_converter(x, type = "doi")$records

  # Print PMID, if one exists
  if (is.null(a$pmid)){
    "Not on PubMed"
  } else {
    a$pmid
  }
}


# Fetch articles from PubMed ----------------------------------------------

# (NAs imply that PubMed does not have any information about that data point)

fetchArticles <- function(searchTerms){

  require(reutils)
  require(RISmed)
  require(rcrossref)

  # Create the search summary
  res <- EUtilsSummary(searchTerms,
                       type     = "esearch",
                       db       = "pubmed",
                       datetype = 'pdat',
                       retmax   = 5000)     # can take additional options

  # Get the results
  fetch <- EUtilsGet(res)

  if (length(attributes(fetch)$PMID) > 0){

    # Status
    print("Extracting authors")

    # Collect authors
    authors <- Author(fetch) %>%
      lapply(`[`, , 1:2) %>%
      lapply(function(x) paste(x[, 1], x[, 2])) %>%
      lapply(paste, collapse = ", ") %>%
      unlist()

    ## Collect publication type and funding

    # Status
    print("Extracting publication type")

    # Fetch data on publication and funding
    pub.term <- PublicationType(fetch) %>% lapply(as.vector)

    # Identify research support information
    support <- lapply(pub.term, function(x) grep("Research Support", x))

    # Initialize objects
    funder    <- list()
    pub.type  <- list()

    # Separate publication type from funding agency
    for (i in 1:length(pub.term)){
      if (length(support[[i]]) > 0) {
        funder[[i]]   <- pub.term[[i]][  support[[i]]]
        pub.type[[i]] <- pub.term[[i]][- support[[i]]] %>% sort()
      } else {
        funder[[i]]   <- ""
        pub.type[[i]] <- pub.term[[i]] %>% sort()
      }
    }

    # Store publication type and funder
    pub.type <- lapply(pub.type, paste, collapse = "; ") %>% unlist()
    funder   <- lapply(funder, paste, collapse = "; ") %>% unlist()


    ## Collect MeSH terms (neater methods did not work)

    # Status
    print("Extracting MeSH")

    # Initialize objects
    MeSH <- list()
    term <- Mesh(fetch)

    # Extract MeSH
    for(i in 1:length(term)){
      if (any(is.na(term[[i]]))){
        MeSH[[i]] <- NA
      } else {
        MeSH[[i]] <- term[[i]][,"Heading"]
      }
    }

    # Store MeSH terms
    MeSH <- unlist(lapply(MeSH, paste, collapse = "; "))

    ## Build date

    # Status
    print("Building date")

    # Date of registration on PubMed
    # (I prefer date of publication, but specs vary so much between journals!)
    # (some only report month and year)
    day     <- formatC(DayPubmed(fetch), width = 2, format = "d", flag = "0")
    month   <- formatC(MonthPubmed(fetch), width = 2, format = "d", flag = "0")
    year    <- YearPubmed(fetch)
    numdate <- as.Date(paste(year, month, day, sep = "-"))


    ## Build affiliation (neater methods did not work)

    # Status
    print("Building affiliation")

    # Initialize objects
    affiliation <- list()
    term <- Affiliation(fetch)

    # Extract MeSH
    for (i in 1:length(term)) {
      if (length(term[[i]]) == 0) {
        affiliation[[i]] <- NA
      } else {
        affiliation[[i]] <- term[[i]] %>% as.vector
      }
    }

    # Store MeSH terms
    affiliation <- unlist(lapply(affiliation, paste, collapse = "; "))

    # Status
    print("Building dataframe")

    # Create frame of papers
    articles <- data.frame(ID       = 1:length(authors),
                           Title    = ArticleTitle(fetch),
                           Author   = authors,
                           Year     = year,
                           Date     = numdate,
                           Journal  = ISOAbbreviation(fetch),
                           ISSN     = ISSN(fetch), # unique journal identifier
                           Pub_type = pub.type,
                           Funder   = funder,
                           Issue       = Issue(fetch),
                           Volume      = Volume(fetch),
                           Abstract    = AbstractText(fetch),
                           Affiliation = affiliation,
                           Country     = Country(fetch),
                           Language    = Language(fetch),
                           MeSH        = MeSH,
                           PMID        = PMID(fetch),
                           DOI         = ELocationID(fetch), # not always DOI!
                           stringsAsFactors = F
    )

    # Add search date
    articles$Search_date <- Sys.Date()

    # Correct the DOIs
    articles <- trueDoi(articles)

    # Print status
    print("Retrieving missing DOIs")

    # Retrieve DOIs that my function failed to retrieve
    ind <- articles$DOI %>%
      lapply(function(x) grep("/", x)) %>%
      lapply(length) %>%
      is_in(0)

    # Retrieve DOIs
    if (sum(ind) > 0){ # do this b/c I may not have missing DOIs

      # Retrieve DOIs
      dois <- articles$PMID[ind] %>%
        pblapply(function(x) id_converter(x, type = "pmid")$records$doi)

      # Replace missing DOIs
      articles$DOI[ind] <- dois
    }
  } else {
    print("Search identified no records")
  }
  # Return object
  return(articles)
}


# Create search terms -----------------------------------------------------

eTerms <- function(dat, date_interval = 21, with_date = T, with_issue = T){
  # Attach dataframe
  attach(dat)

  journal <- paste0("\"", Journal, "\"", " [Journal]")
  volume  <- paste0(Volume, " [Volume]")
  type.ys <- "\"Journal Article\" [Publication Type]"
  type.no <- "NOT \"Review\" [Publication Type]"
  date    <- ""

  terms <- paste(journal, volume, sep = " AND ")

  if (with_date){
    # Calculate date interval
    lower <- format(Date - date_interval, "%Y/%m/%d")
    upper <- format(Date + date_interval, "%Y/%m/%d")

    # Create terms
    date <- paste0("((\"", lower,"\" [Date - Publication]: ",
                   "\"", upper, "\" [Date - Publication]))")

    # Synthesize
    terms <- paste(date, terms, sep = " AND ")

  }

  if (with_issue){

    # Find issue
    issue <- paste(Issue, "[Issue]")

    # Synthesize
    terms <- paste(terms, issue, sep = " AND ")

  }

  # Publication type
  terms <- paste(terms, "AND", type.ys, type.no)

  # Detach
  detach(dat)

  # Return
  return(terms)
}


# Another function to create search terms ---------------------------------

createTerms <- function(dat, numweeks = 6, with_date = T, with_issue = T, ind = NA){

  # Identify index
  ind <- if (is.na(ind[1])) seq(1, nrow(dat)) else ind

  # Build substrings
  journal <- paste0("\"", dat[ind, ]$Journal, "\"", " [Journal]")
  volume  <- paste0(dat[ind, ]$Volume, " [Volume]")
  type.ys <- "\"Journal Article\" [Publication Type]"
  type.no <- "NOT \"Review\" [Publication Type]"
  date    <- ""

  # Build terms
  terms <- paste(journal, volume, sep = " AND ")

  if (with_date){

    # Calculate date interval
    lower <- format(as.Date(dates[ind]) - numweeks * 7, "%Y/%m/%d")
    upper <- format(as.Date(dates[ind]) + numweeks * 7, "%Y/%m/%d")

    # Create terms
    date <- paste0("((\"", lower,"\" [Date - Create]: ",
                   "\"", upper, "\" [Date - Create]))")

    # Synthesize
    terms <- paste(date, terms, sep = " AND ")

  }

  if (with_issue){

    # Find issue
    issue <- paste(dat[ind, ]$Issue, "[Issue]")

    # Synthesize
    terms <- paste(terms, issue, sep = " AND ")

  }

  # Publication type
  terms <- paste(terms, "AND", type.ys, type.no)

  # Return
  return(terms)
}


# Fetch data from Altmetric -----------------------------------------------

getAltmetric <- function(x){

  # Create URL
  altkey        <- "?key=d8cdb7af57fd685d417482b48b15425a"
  altmetric.API <- "https://api.altmetric.com/v1/doi/"
  # URL.altmetric <- paste(altmetric.API, x$DOI, sep = "")

  # (with the key)
  URL.altmetric <- paste(altmetric.API, x$DOI, altkey, sep = "")

  # Get JSON file from Altmetric
  altmetric.json <- getURL(URL.altmetric)

  if (altmetric.json == "Not Found"){

    # Indicate not found if no Altmetric score available
    data.frame(Not_Found = altmetric.json)

  } else {

    # Extract data from JSON and create a data frame
    fromJSON(altmetric.json, simplifyDataFrame = T) %>%
      unlist() %>%
      t() %>%
      as.data.frame(stringsAsFactors = F)

  }
}


# Fetch citation count ----------------------------------------------------

getCitations <- function(doi){

  # Packages
  require(rcrossref)

  # Catch error
  citations <- tryCatch(cr_citation_count(doi), error = function(e) e)

  # Return value
  if(inherits(citations, "error")){
    return(0)
  } else {
    return(citations)
  }
}


# Full PDF from DOI -------------------------------------------------------

# Package
# library(rcrossref)

# Download PDF
# cr_ft_pdf(post$DOI)


# Fetch references --------------------------------------------------------

# The XML object of each article on PubMed contains their references.
