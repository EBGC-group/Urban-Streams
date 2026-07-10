here::i_am('code/Untitled.R')

DOfiles_hickory = list.files(here::here('data/DOdata/'), pattern = '.South Hickory.*.txt', ignore.case = TRUE, full.names = TRUE)

x = DOfiles %>% purrr::map(~.x %>% read_clean_DO_files() %>% mutate(site = "Hickory"))
