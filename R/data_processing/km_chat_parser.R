#### Chat processing script

## the workhorse function
## takes a path to a chat file
## returns a tidy text data frame with metadata


process_chat_fun <- function(file_path) {
  # read file and convert to tibble
  df <- readLines(file_path) %>% tibble(line = .)
  
  tmp <- df %>% 
    mutate(line_trim = line %>% str_trim() %>% str_replace_all(pattern = "@", replacement = ""))
    
  # get metadata
  
  # remove unwanted lines
  
  # get sentence gloss
  
  # get tiers
  
  # get participants 
  get_participant_info()
  
  # get line terminator
}


get_participant_info <- function(df) {
  df %>% filter(str_detect(line, "@Participants")) %>% as.character() %>% str_replac
    
}

get_chat_metadata <- function(file_path) {
  
}