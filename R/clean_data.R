clean_data <- function(raw_data_file){
  d_raw <- readxl::read_xlsx(raw_data_file)[-1]

  # Clean the names of variables
  names(d_raw)[1:10] <- c(
    "pid",
    "location",
    "age",
    "sex",
    "edu",
    "religious_affiliation",
    "marital_status",
    "occupation",
    "income",
    "rel_deceased")

  # AL102 appears twice in the dataset, I am arbitrarily selecting one here but need to follow up on this
  d_raw <- d_raw %>% 
    group_by(pid) %>% 
    slice(1) %>% 
    ungroup()

  # Separate responses to different instruments, pivot long with respect to item

  ## ritual questions
  d_rituals <- d_raw %>% 
    select( starts_with( c("pid", "Pre Burial", "Rituals", "Post Burial")) ) %>% 
    pivot_longer(-pid, names_to = "item", values_to = "response") %>% 
    mutate(
      # recode so that 1 = never, 2 = closely witnessed, 3 = participated
      response = case_when(
      response == 0 ~ 1,
      response == 1 ~ 2,
      response == 2 ~ 3
    ),
      # separate questions by pre/during/post burial
      subscale = case_when(
        str_starts(item, "Pre") ~ "pre",
        str_starts(item, "Rituals") ~ "during",
        str_starts(item, "Post") ~ "post"
      ),
      # remove the prefix for items now that we have recorded subscale in a separate column
      item = sub(".*?-", "", item),
      item = sub("deceasedâ€™s", "deceased's", item),
      # remove leading white spaces
      item = trimws(item)
    )

  ## prosociality scale
  d_prosoc <- d_raw %>% 
    select( starts_with(c("pid", "Indicate")) ) %>% 
    pivot_longer(-pid, names_to = "item", values_to = "response") %>% 
    mutate(
      # remove the prefix for items now that we have recorded subscale in a separate column
      item = sub(".*?-", "", item),
      # remove leading white spaces
      item = trimws(item),
      item_number = match(item, unique(item))
    )

  ## subjective wellbeing scale
  d_SWB <- d_raw %>% 
    select( starts_with(c("pid", "The following")) ) %>% 
    pivot_longer(-pid, names_to = "item", values_to = "response") %>% 
    mutate(
      # remove the prefix for items now that we have recorded subscale in a separate column
      item = sub(".*?-", "", item),
      # remove leading white spaces
      item = trimws(item),
      item_number = match(item, unique(item))
    )

  # Participant demographics
  d_demographics <- d_raw %>% 
    select(pid, location, age, sex, edu, religious_affiliation, marital_status, occupation,  income, rel_deceased) %>% 
    mutate(income = ifelse(income == "low", "Low", income))

  # Now, export these tables for further analysis
  write_csv(d_demographics, "data/demographics.csv")
  write_csv(d_rituals, "data/mourning_rituals.csv")
  write_csv(d_prosoc, "data/prosociality.csv")
  write_csv(d_SWB, "data/subjective_wellbeing.csv")

  return(list(
    d_demographics = d_demographics,
    d_rituals = d_rituals,
    d_prosoc = d_prosoc,
    d_SWB = d_SWB
  ))
}