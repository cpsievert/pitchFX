#' Function for parsing urls and specified nodes into data frames
#' 
#' The primary use for this function is to scrape an "atbats" table and the corresponding "pitch" 
#' (ie, Pitch F/X) table for the specified set of URLs. In fact, this function is used as the core 
#' functionality behind scrapePitchFX. This function provides added flexibility by allowing 
#' one to specify nodes of interest other than "atbat" and "pitch". If the value of the list is NULL,
#' this function will automatically generate the most complete collection of data points.
#' Important: You must have "atbat" AND "pitch" nodes if you want to identify who threw a 
#' particular pitch. Also, if you specify field names for the table, you should be confident that those
#' are the most complete set of fields.
#' 
#' @param urls set of urls for parsing
#' @param tables list of character vectors containing field names for each table. The list names have to correspond to XML nodes of interest within the XML files.
#' @return Returns a data frames if the length of tables is one. Otherwise, it returns a list of data frames.
#' @export
#' @examples
#' #If it isn't currently baseball season, consider changing the dates below:
#' #Also, this is a small scaled example. Visit my website if you would like to see how to 
#' #build a current and complete database.
#' #mini.urls <- getScoreboardURLs(first.date = Sys.Date() - 10, last.date = Sys.Date())
#' #game.urls <- getPitchFxURLs(mini.urls)
#' #data <- urlsToDataFrame(urls = game.urls)
#' #atbats <- data$atbat
#' #pitches <- data$pitch
#' 

urlsToDataFrame <- function(urls, tables = list(atbat = NULL, pitch = NULL)) {
  if (is.null(names(tables))) stop("Please specify at least one XML node of interest.")
  #Order tables alphabetically. This is important because the atbat node must be parsed first if you want an atbat ID for the pitch table 
  orders <- order(names(tables))
  ordered.tables <- llply(orders, function(x) { tables[[x]] })
  names(ordered.tables) <- names(tables)[orders]
  #Start parsing the URLs
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
  #Turn the XML documents into a list of data frames
  ctr <- 1
  frames <- list(NULL)
  length(frames) <- length(names(tables))
  for (j in names(ordered.tables)) {
    fields <- unlist(ordered.tables[[ctr]])
    frame <- docsToDataFrame(docs = docs, node = j, fields = fields, urls = url.vector)
    #Add a date and URL column to games table (note: other nodes can be named "game", but don't have relevant info)
    if (j == "game") { 
      frame <- getPitchFxURLs(frame)
    }
    #Do not return atbat.id if pitch table is not desired
    if (j == "atbat" & !any(names(tables) == "pitch")) { 
      frame <- frame$final
    }
    if (j == "pitch" & any(names(tables) == "atbat")) {
      frame$num <- frames$atbat_id
      frames$atbat_id <- NULL
    }
    if (any(tolower(j) == c("player", "season", "month", "team", "empty", "men_on", "risp", "loaded", "vs_lhp", "vs_rhp", "vs_p"))) {
      names(frame) <- gsub("url", "url_player", names(frame))
      frame$url <- str_replace_all(frame$url_player, "batters/([0-9]+).xml", "inning/inning_all.xml")
      frame$url <- str_replace_all(frame$url_player, "pitchers/([0-9]+).xml", "inning/inning_all.xml")
      split.urls <- str_split(players$url_player, "/")
      frame$type <- gsub("s", "", sapply(split.urls, function(x) { x[11] })) #batter or pitcher?
      frame$id <- gsub(".xml", "", sapply(split.urls, function(x) { x[12] })) #player id
      gids <- sapply(split.urls, function(x) { x[10] })
      frame$date <- sapply(str_split(gids, "_"), function(x) { paste(x[2], x[3], x[4], sep = "/") })
    }
    frames[[ctr]] <- frame
    ctr <- ctr + 1
  }
  if (length(names(tables)) == 1) {
    return(frame)
  } else {
    names(frames) <- names(ordered.tables)
    #names(frames) <- gsub("final.", "", names(frames))
    if (length(frames$atbat) > 1) frames$atbat <- frames$atbat$final
    return(frames)
  }
}

#' Turn XML documents into a Data Frames
#' 
#' This function adds NAs to missing attributes.
#'
#' @param docs XML documents
#' @param node XML node of interest
#' @param fields "Comlpete" set of field names for the data frame
#' @param urls character vector of URLs used to build data frame
#' @return Returns a data frame.

docsToDataFrame <- function(docs, node, fields, urls) {
  nodes <- llply(docs, function(x) { 
    getNodeSet(x, paste("//", node, sep=""))
  })
  attributes <- llply(nodes, function(x) { 
    if (length(x) > 0) { #Check that each node has at least some info
      llply(x, function(y) { 
        xmlAttrs(y) #Grab all the attributes from each node
      }) 
    }
  })
  if (is.null(fields)) { #If field names aren't provided, find them.
    namez <- llply(attributes, function(x) { 
      llply(x, function(y) {
        names(y)
      })
    })
    fields <- unique(unlist(namez))
  }
  data <- llply(attributes, function(x) { 
    llply(x, function(y) { 
      if (length(y) > 0) {
        adjust(y, fields) #Add missing tags and NAs
      }
    }) 
  })
  final <- ldply(data, function(x) {
    ldply(x, function(y) { y }) #Coerce all the data from a list of lists to one big dataframe
  })
  #Create url column to identify where the observation originated.
  counts <- llply(nodes, function(x) { length(x) })
  url.column <- rep(urls, counts)
  final$url <- url.column
  if (node == "atbat") {
    #final <- createInnings(final) 
    atbat.id <- createAtbatID(nodes)
    return(list(final = final, atbat_id = atbat.id))
  } else {
    return(final)
  }
}

#' "Adjust" attributes to match the entire set
#' 
#' This function adds NAs to missing attributes.
#'
#' @param info XML attributes from a particular node.
#' @param tags "complete" set of attribute names.

#Adjust function used inside of UrlsToDataFrame
adjust <- function(info, tags){ #Adds NAs wherever a tag is missing
  x <- names(info)
  y <- tags
  z <- match(x, y)
  w <- z[!is.na(z)] #get rid of elements in info that doesn't match tags (allows fields to be flexible)
  a <- rep(NA, length(tags))
  if (length(w) < length(z)) {
    relevant.info <- info[which(!is.na(z))]
    a[w] <- relevant.info
  } else {
    a[z] <- info
  }
  names(a) <- tags
  return(a)
}

#' Create columns to match an atbat with a inning (and side of that inning)
#'
#' @param data atbat table

createInnings <- function(data) {
  data <- data[order(data[,"url"]),]
  attach(data)
  inning <- tapply(o, INDEX = url, function(x) { swap <- diff(c(x,-1)) #-1 is added to account for walk-offs/quirky endings
                                                 logic <- swap < 0 #Return true whenever we switch sides of an inning
                                                 places <- which(logic) #Which indices are TRUE?
                                                 b <- c(places[1], diff(places)) #Atbats for each side of an inning
                                                 if(length(b)/2 != floor(length(b)/2)) b <- c(b, 0) #Add zero if the bottom inning was not played
                                                 atbatsPerSide <- matrix(b, ncol = 2, byrow = T) #Column1 = "Top", Column2 = "Bottom"
                                                 innings <- dim(atbatsPerSide)[1] #num of innings in the game
                                                 counts <- apply(atbatsPerSide, 1, sum) #num of atbats per inning
                                                 return(rep(1:innings, counts)) #inning ID
  })
  innings <- as.numeric(unlist(inning))
  side.inning <- tapply(o, INDEX = url, function(x) { swap <- diff(c(x,-1)) #-1 is added to account for walk-offs/quirky endings
                                                      logic <- swap < 0 #Return true whenever we switch sides of an inning
                                                      places <- which(logic) #Which indices are TRUE?
                                                      b <- c(places[1], diff(places)) #Atbats for each side of an inning
                                                      if(length(b)/2 != floor(length(b)/2)) b <- c(b, 0) #Add zero if the bottom inning was not played
                                                      atbatsPerSide <- matrix(b, ncol = 2, byrow = T) #Column1 = "Top", Column2 = "Bottom"
                                                      z <- apply(atbatsPerSide, 1, function(x) { rep(c("TOP", "BOTTOM"), x)} )
                                                      return(unlist(z))
  })
  side.innings <- as.character(unlist(side.inning))
  return(cbind(data, inning = innings, side_innings = side.innings))
}
  
#' Assign each pitch an atbat ID
#'
#' @param nodes XML nodes from a set of URLs. These nodes should be from the "atbat" node.

createAtbatID <- function(nodes) {
  p.per.ab <- llply(nodes, function(x) { 
    llply(x, function(y) { 
      sum(as.numeric(names(xmlChildren(y)) == "pitch")) 
    })
  })
  atbat.records <- llply(p.per.ab, function(x) {
    if (length(x) > 0) mapply(rep, 1:length(x), x)
  })
  return(unlist(atbat.records, use.names=FALSE))
}