#' Scrape Pitch F/X Data
#'
#' This function scrapes data from the MLB website.
#'
#' Details go here.
#' 
#' @param start date "yyyy-mm-dd" to commence scraping of pitch F/X data
#' @param end date "yyyy-mm-dd" to terminate scraping pitch F/X data
#' @return Returns a list containing two different data frames. The larger data frame contains data on every pitch thrown (pitch F/X). The smaller one contains data on every atbat.
#' @export
#' @example
#' #' ptm <- proc.time()
#' data <- scrapePitchFX(start = "2011-05-01", end = "2011-05-01")
#' ptm
#'
#' pitches <- data$pitches
#' atbats <- data$atbats
#' pitchFX <- join(pitches, atbats, by = c("num", "url"))
#'
#' unique(pitchFX$pitch_type)
#' pitchFX <- pitchFX[pitchFX$pitch_type == c("SL","CU"), ]

scrapePitchFX <- function(start = "2012-01-01", end = Sys.Date()) { #make fields flexible?
  first.date <- paste("('", start, "')", sep = "")
  if (year(start) < 2005) stop("Not only is pitchFX data not avaliable before 2008, data on each game isn't complete until 2005")
  if (year(start) < 2008) stop("Warning: pitchFX data wasn't recorded consistently until 2008. Please consider a later start date.")
  last.date <- paste("('", end, "')", sep = "")
  if (end > Sys.Date()) stop("Sorry, I can't scrape data on the future!")
  urls <- dbGetQuery(MLB, paste("SELECT url FROM all_games WHERE all_games.date >= ", first.date, "AND all_games.date <=", last.date, sep = ""))
  u <- urls[,"url"]
  p <- c("ax", "ay", "az", "break_angle", "break_length", "break_y", "cc", "des", 
                "end_speed", "id", "mt", "nasty", "on_1b", "on_2b", "on_3b", "pfx_x", "pfx_z", "pitch_type", 
                "px", "pz", "spin_dir", "spin_rate", "start_speed", "sv_id", "sz_bot", "sz_top", 
                "tfs", "tfs_zulu", "type", "type_confidence", "vx0", "vy0", "vz0", 
                "x", "x0", "y", "y0", "z0", "zone")
  ab <- c("away_team_runs", "b", "b_height", "batter", "des", "event", "event2", "event3", 
                "event4", "home_team_runs", "num", "o", "p_throws", "pitcher", "s", "score", "stand", "start_tfs", "start_tfs_zulu")
  data <- urlsToDataFrame(urls = u, p.tags = p, ab.tags = ab)
  #Should I add names to the atbats table before returning it?
  return(data) #Should I write the table to the wd? Would this help with memory issue? 
}

#' URLs to Data Frame
#'
#' This function takes on a list of URLs and returns a data frame
#'
#' @param urls URLs for parsing
#' @param p.tags field names for the pitch F/X table
#' @param ab.tags field names for the atbats table

urlsToDataFrame <- function(urls, p.tags, ab.tags) { #Function that greatly reduces time required to build tables.
  docs <- NULL
  url.vector <- NULL
  for (i in urls) {
    cat(i, "\n")
    doc <- try_default(xmlParse(i), NULL, quiet = TRUE)
    if (!is.null(doc)) {
      docs <- c(docs, doc) #Keep non-empty documents
      url.vector <- c(url.vector, i) #Keep urls that have data
    }
  }
  ab.nodes <- llply(docs, function(x) { 
    getNodeSet(x, "//atbat")
  })
  p.nodes <- llply(docs, function(x) { 
    g <- getNodeSet(x, "//pitch")
  })
  ab.final <- nodesToDataFrame(nodes = ab.nodes, tags = ab.tags)
  p.final <- nodesToDataFrame(nodes = p.nodes, tags = p.tags)
  
  #Create url column to identify which game each record belongs to.
  ab.counts <- llply(ab.nodes, function(x) { length(x) })
  ab.urls <- rep(url.vector, ab.counts)
  p.counts <- llply(p.nodes, function(x) { length(x) })
  p.urls <- rep(url.vector, p.counts)
  ab.final$url <- ab.urls
  p.final$url <- p.urls
  
  #Create atbat_id column for pitches table (used to connect with pitches table)
  p.per.ab <- llply(ab.nodes, function(x) { 
    llply(x, function(y) { 
      sum(as.numeric(names(xmlChildren(y)) == "pitch")) 
    })
  })
  atbat.records <- llply(p.per.ab, function(x) {
    if (length(x) > 0) mapply(rep, 1:length(x), x)
  })
  atbat.id <- unlist(atbat.records, use.names=FALSE)
  p.final$num <- atbat.id
  return(list(pitches = p.final, atbats = ab.final))
}

#' Nodes to Data Frame
#'
#' This function takes on a set of XML nodes and returns a data frame.
#' 
#' Used to avoid redundancy within the urlsToDataFrame function.
#'
#' @param nodes XML nodes to manipulate
#' @param tags field names for a specific table

nodesToDataFrame <- function(nodes, tags) {
  attributes <- llply(nodes, function(x) { 
    if (length(x) > 0) { #Check that each node has at least some info
      llply(x, function(y) { 
        xmlAttrs(y) #Grab all the attributes from each node
      }) 
    }
  })
  data <- llply(attributes, function(x) { 
    llply(x, function(y) { 
      if (length(y) > 0) {
        adjust(y, tags) #Add missing tags and NAs
      }
    }) 
  })
  final <- ldply(data, function(x) {
    ldply(x, function(y) { y }) #Coerce all the data from a list of lists to one big dataframe
  })
  return(final)
}

#' Adjust XML attributes
#'
#' This function adds NAs wherever an XML node is missing information.
#'
#' @param info XML attributes from a specific node
#' @param tags field names for a specific table

adjust <- function(info, tags){ #Adds NAs wherever a tag is missing
  x <- names(info)
  y <- tags
  z <- match(x, y)
  a <- rep(NA, length(tags))
  a[z] <- info
  names(a) <- tags
  return(a)
}
