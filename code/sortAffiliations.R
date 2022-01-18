# Function for sorting bibliography entries by regional designation
# Written by Britt Bertolet
# 2020-12-09

sortAffiliations=function(file, n, GNout, GSout, dir="bibs"){
  # Scan in .bib file line by line 
  bib<-scan(paste(dir,file, sep="/"), what = "character", sep = "\n", strip.white = T)
  # Get vector of affiliations
  aff=bib[grepl("affiliation=\\{", bib)]
  # Get vector of bibID
  bibID=bib[grepl("@ARTICLE", bib)]
  # Clean up bibID
  bibID=gsub(",", "", bibID)
  bibID=gsub("@ARTICLE", "", bibID)
  bibID=gsub("\\{", "", bibID)
  
  # Get citation number
  ncites=bib[grepl("note=\\{", bib)]
  ncites=gsub(",", "", ncites)
  ncites=gsub("note=\\{cited By ", "", ncites)
  ncites=as.numeric(gsub("\\}", "", ncites))
  
  # Convert to dataframe with a grouping factor
  aff=data.frame(bibID, aff, citation=1:n, ncites, stringsAsFactors = F)
  
  # Seperate the entries that have multiple affiliations 
  aff=aff %>% mutate(aff = strsplit(aff, ";")) %>%
    unnest(aff)
  # Get just the first affiliation
  aff=aff[grepl("affiliation=\\{", aff$aff),]
  # Seperate affiliations to identify the country
  aff=aff %>% mutate(aff = strsplit(aff, ",")) %>%
    unnest(aff) 
  # Clean up namings 
  ## Get rid of all punctuation
  aff$aff=str_replace_all(aff$aff, "[[:punct:]]", " ")
  ## Trim white space off the front and back
  aff$aff=trimws(aff$aff, which = "both")
  # Clean up United States naming convention
  aff$aff[aff$aff=="USA"]="United States"
  
  # Read in the countries file 
  countries=read.csv("countries.csv", stringsAsFactors = F)
  
  # Get lines from the affiliations that match to country names
  aff=aff[aff$aff%in%countries$country,]
  # Merge with countries list to get global designation
  colnames(aff)=c("bibID","country", "citation", "ncited")
  aff=merge(aff, countries, by="country")
  # Sort by citation number
  aff=aff[order(aff$citation),]
  # Get the rows that mark the end of an entry
  bib=data.frame(bib)
  bib$row=rownames(bib)
  end=data.frame(citation=1:n, 
                 rowbegin=as.numeric(bib$row[grepl("@ARTICLE",bib$bib)]),
                 rowEnd=as.numeric(bib$row[bib$bib=="}"]))
  
  # Merge with list of affilations and designation
  aff=merge(aff, end, by="citation")
  
  # Make two dataframes for north and south designation
  GN=aff[aff$designation=="Global North",]
  GS=aff[aff$designation=="Global South",]
  
  # Write GN bibs to a file
  write("\n",file = paste(dir, GNout, sep="/"))
  cat("- Writing",paste(dir, GNout, sep="/"), "\n")
  for(i in 1:nrow(GN)){
    write(bib$bib[GN$rowbegin[i]:GN$rowEnd[i]], file = paste(dir, GNout, sep="/"), append = TRUE)
  }
  cat("- Number of entries where first-author affiliation is in the global north:", nrow(GN), "\n")
  # Write GS bibs to a file 
  
  write("\n",file = paste(dir, GSout, sep="/"))
  cat("- Writing ",paste(dir, GSout, sep="/"), "\n")
  for(i in 1:nrow(GS)){
    write(bib$bib[GS$rowbegin[i]:GS$rowEnd[i]], file = paste(dir, GSout, sep="/"), append = TRUE)
  }
  cat("- Number of entries where first-author affiliation is in the global south:", nrow(GS), "\n")
  
  # Get rid of extra columns not needed in the aff table 
  aff$rowbegin=NULL
  aff$rowEnd=NULL
  # Reorder columns for export
  aff=aff[,c("citation", "bibID", "ncited", "country", "region", "designation")]
  
  # Return the aff table
  cat("- Returning affilations table\n")
  return(aff)
}

