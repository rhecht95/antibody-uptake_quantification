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
    #select(-c(seq(3,10), seq(15,18))) %>%  #get rid of unnecessary columns for now
    condition_select(...)
  df
}

# Plotting function for uptake experiments
uptake_plot <- function(df, output_name, condition_label, add_stats = FALSE,...) {
  df_plot <- df %>%
    group_by(tissue, condition, condition_avg, tissue_avg, condition_sd, exp) %>% #trimming the enormous data down to summary level for faster plotting
    summarise() %>%
    ggplot() +
    geom_bar(aes(x = condition, y = condition_avg),color = "black", fill = "white", position = "dodge", stat = "identity")+
    geom_point(aes(x = condition, y = tissue_avg, fill = exp), shape =  21, alpha = 0.75, position = position_jitter(width = 0.1))+
    geom_errorbar(aes(x = condition, ymin = condition_avg + condition_sd, ymax = condition_avg + condition_sd ), position = "dodge", width = 0.2)+
    geom_linerange(aes(x = condition, ymin = condition_avg, ymax = condition_avg + condition_sd))+ #this is a trick to get a 'one-sided' error bar on a bar plot
    scale_y_continuous(expand = expansion(c(0,0.1)))+
    labs(x = condition_label, y = "Average Uptake/Cell", fill = "Experiment")+
    theme_classic()+
    theme(axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"))

  if(add_stats == TRUE){
    df_plot <- df_plot +
      stat_pvalue_manual(...)+
      labs(subtitle = get_test_label(...))
  }

  df_plot
  ggsave(filename = paste0("output/", output_name, ".pdf"), device = "pdf", dpi = 300, height = 7, width = 6, scale = 0.6)
  df_plot
}

# Calculate the average uptake per cell. Applicable on and after the V5 pipeline. Return summary data from the original dataframes
avg_uptake <- function(df){
  df <- df %>%
    mutate(name = str_extract(name, "\\([^)]+\\)")) %>% # simplify the name and remove the 'segment' information
    rename(type = name) # rename name to 'type'

  #get a vector composed of the sum_intensities_3 from each image
  sum_intensities <- df %>%
    group_by(image_set) %>%
    filter(str_detect(type, "Real Uptake")) %>%
    pull(sum_intensities_5) #as of V5.4, the filtered values are stored in a new "Channel 5"

  #get a vector of the count of ON-BCs per image
  num_on_bcs <- df %>%
    group_by(image_set) %>%
    filter(str_detect(type, "ON-BCs")) %>%
    count() %>%
    pull(n)

  #condense df into groups based on image_set and condition
  df <- df %>%
    group_by(image_set, tissue, condition) %>%
    summarise()

  #add the new variables to the df
  df['sum_intensities'] <- sum_intensities #add the sum_intensities vector to the dataframe
  df['on_bc_count'] <- num_on_bcs #add the number of ON-BCs in the image to the df

  #calculate uptake per cell `avg_upc`
  # df <- df %>%
  #   mutate(avg_upc = sum_intensities/on_bc_count) #now calculating this as an average for the whole data set, after merging all experiments into one df

  df
}

# Calculate the average uptake per volume of ON-BC (summed pixel intensities over total ON-BC volume). Applicable for V4.2 pipeline.
avg_uptake_vol <- function(df){
  df <- df %>%
    group_by(image_set) %>%
    mutate(avg_upc = (sum(sum_intensities_5))/(sum(volume_volume_um3)))
}
