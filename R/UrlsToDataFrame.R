#' Scraping work horse
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

#Adjust function used inside of UrlsToDataFrame
adjust <- function(info, tags){ #Adds NAs wherever a tag is missing
  x <- names(info)
  y <- tags
  z <- match(x, y)
  a <- rep(NA, length(tags))
  a[z] <- info
  names(a) <- tags
  return(a)
}

masterlist <- function(butes){ #Returns the most complete set of tags from an XML node
  tags <- llply(butes, function(x) { names(x) })
  return(rev(unique(tags))[[1]]) #Most complete list of tags
}