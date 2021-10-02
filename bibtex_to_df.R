bibtex_to_df<-function(input) {
  # Cleanup: 1 line per citation
  output<-data.frame(Authors=character(),Title=character(),Type=character(),Publisher=character(), year=character(),stringsAsFactors = FALSE)
  count_citations<-0
  cleaned_bib<-data.frame()
  
  for (i in 1:nrow(import)) {
    # If the string is a new citation containing "@", keep it untouched
    if (grepl("@", import[i,1], fixed = TRUE)==TRUE) {
      count_citations<-count_citations+1
      cleaned_bib[count_citations,1]<-import[i,1]
    }
    # If the string is not a new citation, concatenate with previous string
    else {
      cleaned_bib[count_citations,1]<-paste(cleaned_bib[count_citations,1], import[i,1], sep="")
    }
  }
  
  # Extraction function
  extract_str<-function(str_input, search_term) {
    if (grepl(search_term, str_input, fixed = TRUE)==TRUE) {
      pos_start<-stri_locate_all(pattern = search_term, str_input, fixed = TRUE)[[1]][1,2]
      temp<-substring(str_input,pos_start,10000)
      pos_equal<-stri_locate_all(pattern = '=', temp, fixed = TRUE)
      if (nrow(pos_equal[[1]])>1) {
        pos_equal<-pos_equal[[1]][2,1]
      } 
      else {
        pos_equal<-nchar(temp)
      }
      temp<-substring(temp,1,pos_equal)
      pos_open<-stri_locate_all(pattern = '{', temp, fixed = TRUE)[[1]][1,1]
      pos_close<-tail(stri_locate_all(pattern = '}', temp, fixed = TRUE)[[1]],1)[1,1]
      temp<-substring(temp,pos_open,pos_close)
      temp<-str_remove_all(temp,"[{}]")
    }
    else {
      temp<-NA
    }
    return(temp)
  }
  
  # ITeration over every citation
  for (i in 1:count_citations) {
    
    # Remove the last "}"
    pos_tail<-tail(stri_locate_all(pattern = '}', cleaned_bib[i,1], fixed = TRUE)[[1]],1)[1,1]
    cleaned_bib[i,1]<-substring(cleaned_bib[i,1],1,pos_tail-1)
    
    # Extract authors
    output[i,1]<-extract_str(cleaned_bib[i,1],'uthor')
    
    # Extract title
    output[i,2]<-extract_str(cleaned_bib[i,1],'itle')
    
    # Extract type
    pos_start<-stri_locate_all(pattern = '@', cleaned_bib[i,1], fixed = TRUE)[[1]][1,2]
    temp<-substring(cleaned_bib[i,1],pos_start+1,10000)
    pos_open<-stri_locate_all(pattern = '{', temp, fixed = TRUE)[[1]][1,1]
    temp<-substring(temp, 1, pos_open-1)
    output[i,3]<-tolower(temp)
    
    # Extract journal / book publisher / in proceeding / tech report organisation
    if (output[i,3]=='article') {
      output[i,4]<-extract_str(cleaned_bib[i,1],'ournal')
    } 
    
    if (output[i,3]=='book') {
      output[i,4]<-extract_str(cleaned_bib[i,1],'ublisher')
    } 
    
    if (output[i,3]=='inproceedings') {
      output[i,4]<-extract_str(cleaned_bib[i,1],'ooktitle')
    } 
    
    if (output[i,3]=='techreport') {
      output[i,4]<-extract_str(cleaned_bib[i,1],'nstitution')
    }
    else {
      output[i,4]<-extract_str(cleaned_bib[i,1],'rgani')
      if (is.na(output[i,4])) {
        output[i,4]<-extract_str(cleaned_bib[i,1],'ublisher')
        if (is.na(output[i,4])) {
          output[i,4]<-extract_str(cleaned_bib[i,1],'nstitution')
          if (is.na(output[i,4])) {
            output[i,4]<-extract_str(cleaned_bib[i,1],'ournal')
          }
        }
      }
    }
    # Extract year
    output[i,5]<-extract_str(cleaned_bib[i,1],'year')
    if (is.na(output[i,5])) {
      output[i,5]<-extract_str(cleaned_bib[i,1],'Year')
    }
  }
  return(output)
}
