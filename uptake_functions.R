#function to allow user to select which samples belong to which condition
  #cond1/2_vec = a string vector of the img file names for a given condition (i.e. "s1.2")
  #cond1/2_title = the name of the condition as a string (i.e. "DMSO")
condition_select <- function(df, cond1_vec, cond2_vec, cond1_title, cond2_title) {

  cond1_vec <- str_c(cond1_vec, collapse = "|") #take list of input samples and add "or" operator between each. For evaluation in str_detect regex
  cond2_vec <- str_c(cond2_vec, collapse = "|")

  #make a new variable 'condition' that assigns the treatment condition based on input strings of sample names
   df %>%
    mutate(condition = case_when(
      str_detect(image_set, pattern = cond1_vec) == TRUE ~ cond1_title,
      str_detect(image_set, pattern = cond2_vec) == TRUE ~ cond2_title
    ))

}

#read-in function for importing all files in a directory and tidying data. Implements the previous method `condition_select`
read_n_clean <- function(folder_path, ...){

  files <- list.files(path = folder_path, pattern = ".xlsx") #list all files in dir
  files <- lapply(files, function(x){paste0(folder_path, "/", x)}) #add the folder path to the front of each file name
  df_list <- lapply(files, read_excel) #perform read_excel on each file
  df <- bind_rows(df_list) %>% #merge the lists into single df
    clean_names(replace=janitor:::mu_to_u) %>%
    select(-c(seq(3,10), seq(15,18))) %>%  #get rid of unnecessary columns for now
    condition_select(...)
  df
}

