## Helper functions for converting raw chat files to a structured database

# returns the length of a transcript
return_max <- function(df, col_name = "utt_number") {
  max_len <- df[[col_name]] %>% 
    as.numeric() %>% 
    max()
  
  df_new <- df %>% mutate(length_transcript = max_len)
  
  df_new
}


sentenceHandler = function(row){
  #!!! the problem is that there are many sentences where the word nodes do not have all of the metadata, 
  #leading to mismatches in the length of the arrays   
  
  # extract line terminator (i.e., punctuation)
  terminator <- row$X1 %>% 
    str_sub(., start = -1, end = -1) %>% # grab the last character, which should be the line terminator
    str_extract(., pattern = "[\\?\\.\\!]") %>% # check that it matches one of the accepted terminators
    as.character() # convert to character to easily merge with other sentence info later on
  
  temp_glosses = row$Gloss
  
  if (length(grep(reformulations,temp_glosses)) > 0 & is.na(row$mor) == F){ 
    #handle reformulations using the script from Naomi
    original_mor = cleanMOR(row$mor)          	
    scrubbedGloss =  paste(removeAudioTags(strsplit(temp_glosses, ' ')[[1]]), collapse=' ')
    mors = process_backslash_mor(original_gloss = scrubbedGloss, original_mor, reformulations)
    mors = mors[!is.na(mors)]        
  } else {
    mors = cleanMOR(row$mor)   
  }
  
  glosses = cleanGloss(temp_glosses)        
  
  #handle any mismatches in length:
  if (length(glosses) > length(mors)){
    # handle a mismatch in the length of the two cleaned vectors    
    
    #Why not use the gloss to index into the mors, and find the corresponding term for each?
    #b/c there's aproblem with indexing in from the gloss is that there are complex terms 
    #like don't == aux|do~neg|not        
    
    newMor = mat.or.vec(length(mors),1)
    offset = 0 #this is the difference between the index in glosses and the index in mor
    
    for (i in 1:length(glosses)){
      if (length(grep('&=', glosses[i])) > 0){
        newMor[i] = 'NON-LINGUISTIC'       
        offset = offset + 1
      } else if ((length(grep('^&', glosses[i])) > 0) | glosses[i] %in% wordsToExclude){
        newMor[i] = 'EXC'       
        offset = offset + 1
      } else if (glosses[i] %in% noUtt){
        newMor[i] = 'NO_UTT'
        offset = offset + 1
      } else {
        newMor[i] = mors[i-offset]
      }            
    }
    
    if(length(glosses) != length(newMor)){
      print('Glosses: ')
      print(glosses)
      print('Original Mors:')
      print(mors)
      print('Corrected Mors:')
      print(newMor)
      stop('Recovery process for longer gloss failed')
    } else {
      mors = newMor            
    }
  } else if (length(glosses) < length(mors)){         
    #         print('Glosses: ')
    #         print(glosses)
    #         print('Mors:')
    #         print(mors)
    #         print('Row')
    #         print(row)
    #         stop('No recovery process for longer MOR line')
    return(NULL)
  }
  
  splitGlosses = strsplit(glosses,'@') 
  glosses = sapply(splitGlosses, function(x){x[1]})
  atTags = sapply(splitGlosses, function(x){ ifelse(length(x) > 1,x[2],'NA')})    
  
  #print('Gloss')
  #print(glosses)
  #print('Mor')
  #print(mors)
  
  rd = data.frame(Gloss = glosses, mor = mors, atTags, line_terminator = terminator)
  if (nrow(rd) > 0){                        
    row$sentGloss = paste(glosses, collapse = ' ')
    row$sentMor = paste(mors, collapse= ' ')        
    
    if('xgr' %in% names(row)){
      row$xgr = gsub('\\t','',row$xgr)            
    }
    selectRows = names(row)[names(row) %in% metadataRows] #only select those from the desired columns that are present
    return(cbind(rd, row[,selectRows], row.names = NULL)) #returns df, number of words * columns
  } else {
    print(row)
    stop('Zero-length return data')
  }  
}

process_backslash_mor = function(original_gloss, original_mor, reformulations){
  
  original_gloss = gsub('[()]','', original_gloss)
  og_sep = gsub('<', '< ', gsub('>', ' >',original_gloss))
  
  #reverse both and parse from the back
  og = rev(strsplit(og_sep, ' ')[[1]])
  #delete the punctuation, if it exists
  if (nchar(gsub('[[:punct:]]','',og[1])) == 0 ){
    og = og[-1]
  }			
  om = rev(original_mor)
  if (nchar(gsub('[[:punct:]]','',om[1])) == 0 ){
    om = om[-1]
  }			
  
  #then parse both from back to front. > means the beginning of a sequenct, and > the end.
  
  inReformulation = F
  new_mor = list()
  new_mor_index = 0
  old_mor_index = 0
  markerPrevious = T 
  
  for (i in 1:length(og)){
    if(length(grep(reformulations, og[i])) > 0){
      #reformulation marker found
      inReformulation = T			
      markerPrevious = T			
    } else if (inReformulation & og[i] == '>'){
      #beginning marker, don't do anything
      markerPrevious=F
    } else if (inReformulation & og[i] == '<'){
      #end marker; end inReformulation
      inReformulation = F
    } else if (inReformulation & markerPrevious){
      #single content item in the reformulation			
      new_mor_index = new_mor_index+1
      new_mor[new_mor_index] = 'BRK'
      inReformulation = F
      markerPrevious = F 
    } else if (inReformulation ){				
      #content item in the reformulation			
      new_mor_index = new_mor_index+1
      new_mor[new_mor_index] = 'BRK'
    } else {
      new_mor_index = new_mor_index+1
      old_mor_index = old_mor_index+1
      new_mor[new_mor_index] = om[old_mor_index]					
    }	
  }
  
  rev(sapply(new_mor, function(x){x[1]}))
}

removeAudioTags = function(unlistedwords){	
  charLengths = sapply(unlistedwords, nchar)
  numNonNumbers = sapply(unlistedwords, function(x){
    nchar(gsub('[[:digit:]_\\^+U] *','',x)) 		
  })
  
  tagIndices = (charLengths > 5) & (numNonNumbers <= 2) 
  if(any(tagIndices)){
    return(unlistedwords[-which(tagIndices)]) 	
  } else {
    return(unlistedwords) 	
  }	
}

cleanGloss = function(gloss){
  #print('cleaning gloss')
  originalGloss = gloss
  gloss = gsub("[^[:alnum:][:space:]'_@+&=]", '', gloss)  #remove non-apostrophe punctuation  
  gloss = gsub('\342\200\234','\342\200\234 ', gloss)#preceding quotes
  gloss = gsub('\342\200\235',' \342\200\235', gloss)#following quotes
  unlistedwords = unlist(strsplit(gloss, split = " "))
  unlistedwords = unlistedwords[!(unlistedwords ==  '')]
  unlistedwords = gsub("\\n|\\t", " ", unlistedwords)           
  unlistedwords = unlistedwords[sapply(gsub('[[:punct:]]','', unlistedwords), nchar) > 0] #remove puntucation-only words
  #is the last items a number with > 7 digits? this is a tag, don't return it
  if (length(unlistedwords) == 0){
    print(originalGloss)
    print(unlistedwords)
  }
  return(removeAudioTags(unlistedwords))
}


cleanMOR = function(mor){
  #print('cleaning mor')
  onesplit = gsub("cm\\|cm|none\\|cm", "", unlist(strsplit(mor, " ")))
  onesplit = onesplit[grepl("\\|", onesplit)]
  onesplit = gsub("\\n|\\t", " ", onesplit)
  onesplit = unlist(strsplit(onesplit, split = " "))
  onesplit = gsub("[!,?//.]", "", onesplit) #!!! think we probably want to keep this information around
  onesplit = onesplit[!(onesplit %in%  c("","bq|bq","eq|eq"))]        
  return(onesplit[sapply(gsub('[[:punct:]]','', onesplit), nchar) > 0])
}

processClanFile = function(filename, cores = 1){
  library('stringr')
  print(paste('Processing file:', filename))
  df = read.CLAN.file(filename, cores = cores)
  
  if (ncol(df) > 36){ #!!! lower this number if possible
    print(names(df))
    stop(paste(filename, 'has an invalid structure: too many columns found'))
  }
  print(paste('CLANtoR produced dataframe with dimensions:',dim(df)[1], 'by', dim(df)[2]))
  
  # check number of cores
  print(paste("Number of cores is", cores))
  
  processedSentenceList = mclapply(1:nrow(df), function(i){sentenceHandler(df[i,])}, 
                                   mc.cores = cores)
  print('Processed sentences')
  
  allTokens = do.call('rbind.fill', processedSentenceList)    
  
  return(allTokens)
}

processDirectory = function(dirname, cores){    
  fnames = paste(dirname, list.files(dirname, recursive=T, pattern = "\\.cha$"), sep='/')
  print(paste('Processing', length(fnames), 'filenames'))
  
  #!!! multicore this 
  #allFiles = do.call('rbind.fill', lapply(fnames, processClanFile))
  allFiles = do.call('rbind.fill', mclapply(fnames, processClanFile, mc.cores= cores))
  
  names(allFiles) = tolower(names(allFiles))
  allFiles$age = sapply(allFiles$age, ageToDays)
  
  return(allFiles)
}

ageToDays = function(age){
  ageParts = strsplit(age, ';')[[1]]
  return(ceiling((12*30.5*as.numeric(ageParts[1])) + as.numeric(ageParts[2])*30.5))	
}