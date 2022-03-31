clean_decathlon <- function(x){
  clean_decathlon_data <- rownames_to_column(x, "name") %>% 
    janitor::clean_names()%>% 
    mutate(
      name = str_to_title(name)
    ) %>% 
    arrange(name)
  return(clean_decathlon_data)
  
}

