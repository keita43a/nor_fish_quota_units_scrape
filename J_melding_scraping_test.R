# J melding web scraping
# May 21, 2020

# This is a trial to download the table of quota factor conversion number, which changes a few time in a year.
# each J-meldings includes the table of the quota factors. 

# 1. load package

pacman::p_load(tidyverse,
               rvest,
               lubridate)

# 2. test1: read html of a single page

yr = 2017  #extracting year

list_tables = list()  # make list to store the result

for(i in 1:300){ # j-meldings up to 300 per year? maybe more in other years...
  
 url <- paste0("https://www.fiskeridir.no/Yrkesfiske/Regelverk-og-reguleringer/J-meldinger/Utgaatte-J-meldinger/J-",i,"-",yr)

 ## html <- url %>% possibly(read_html,FALSE)
 delayedAssign("do.next", {next})
 
 # use tryCatch to avoid stopping by error reading 404 page. 
 html <- tryCatch(
   read_html(url),
   error=function(e) {print("URL Not Found, skipping")
     force(do.next)})
 # check if <head> and <body> are properly read.
 
 # read html file from the url
 html <- url %>% read_html
 # print the number of meldings 
 print(paste0("melding ",i," is successfully read"))
 
# 2.1. check the title of melding
 
 # if the read html is not about the cod fishery, skip to next loop
 html_title <- html %>%
   html_node("h1") %>% 
   html_text() %>%
   str_detect(pattern = "Forskrift om regulering av fisket etter torsk, hyse og sei nord for 62°N")
 
 if(!html_title){
   next
 }
 
 # print the j-meldings number
 print(paste0("melding ",i, " is about cod fishery north of 62N"))
 
# 2.2. check date
 
 # get the dates text
 temp <- html %>% 
   html_nodes(css = ".regulations-dates") %>% 
   html_text() %>%
   str_replace_all(pattern = " ", replacement = "") 
 
 # vectorize 
 temp2 = unlist(strsplit(temp,split="\n"))
 # remove empty cells
 temp2 = temp2[temp2 != ""]
 
 # exrtract the dates from the text (+1 means extracting the next cell)
 date_begin = temp2[which(temp2 == "Gyldigfra:") + 1]
 date_end = temp2[which(temp2 == "Gyldigtil:") + 1]


# 2.2. read the table
 
 # get all the tables in the article
 # xpath did not work to specify the table
 table1 <- html %>%
   html_table(fill = TRUE)

 ##length(table1)  # there are 19 tables
 
 # make empty list: 1st component is a data frame to show melding numbera and dates.
 # 2nd component is list of tables (dfs) in an articile. 
 list_tab = list()
 list_tab[[1]] = data.frame("melding" = i, "date_begin" = date_begin, "date_end" = date_end)
 list_tab[[2]] = table1
 
 # merge to the main list: This is list of lists.
 # each list is the list of tables in one melding. 
 # list_table[[melding]][[1:info or 2:list of table]][[each table]]
 
 list_tables[[length(list_tables) + 1]] <- list_tab
 
} # end of for i
 

##### extract the quota units table #######
# there is no way to specify the quota units table as there is no tag to identify. 
# Here I identify the table by the size and the first element.
# 1: The quota units table is either 9 by 4 or 8 by 3, (maybe, and may not be in other years)
# 2: the first element of the quota factor is around 25 (for hj<11m and actual < 11m)
# if the table satisfies these two condotion, it is chosen as quota units table. 
# (I know that this is not a nice way...)

# storage for the results. 
dat_qta_fct_temp = data.frame(X1 = NA, X2=NA,X3=NA,X4=NA,melding=NA,date_begin=NA,date_end=NA)

# this loop is fast.
for(i in 1:length(list_tables)){
  for(j in 1:length(list_tables[[i]][[2]])){
    # quota factor table for cod is either 9x4 or 8x3
    check_9_4 = all(dim(list_tables[[i]][[2]][[j]]) == c(9,4)) # if the table is 9x4?
    check_8_3 = all(dim(list_tables[[i]][[2]][[j]]) == c(8,3)) # if the table is 8x3?
    cell_2_3 <- as.numeric(sub(",",".",list_tables[[i]][[2]][[j]][2,3])) # second row, third column is the quota factor for <11m. It's about 25.
    check_2_3 <- all(cell_2_3 < 30, cell_2_3 > 20) # if the number in the cell between 20 and 30? (should be abour 25)
    
    # if the conditions are satisfied, extract the elements and table and append to the storage
    if((check_9_4 | check_8_3) & check_2_3){
     
      dat_temp = list_tables[[i]][[2]][[j]] %>%
        mutate(melding = list_tables[[i]][[1]]$melding,
               date_begin = list_tables[[i]][[1]]$date_begin,
               date_end = list_tables[[i]][[1]]$date_end)
      
      # append the chosen table to the storage
      dat_qta_fct_temp= bind_rows(dat_qta_fct_temp,dat_temp)
    } # end if
  } # end j
} # end i

dat_qta_fct = dat_qta_fct_temp %>%
  # remove the rows of (old) column names
  filter(X1 != "Hjemmels-lengde") %>%
  # I use sub(",",".",XX) to substitute comma with dot for decimal point
  mutate(ref_length = ifelse(X1 == "", NA, sub(",",".",X1)),
         max_length = ifelse(X2 == "", "Alle lengder",sub(",",".",X2)),
         qta_unit_gr = as.numeric(sub(",",".",X3)),
         qta_unit_max = ifelse(X4 == "", NA, as.numeric(sub(",",".",X4)))) %>%
  # convert date format
  mutate(date_begin = as.Date(date_begin,format = "%d.%m.%Y"),
         date_end = as.Date(date_end,format = "%d.%m.%Y")) %>%
  dplyr::select(-X1, -X2,-X3,-X4) %>%
  tidyr::fill(ref_length)


# plot
ggplot(dat_qta_fct %>% mutate(group = paste0(ref_length,"-",max_length)), 
       aes(x = date_begin,y = qta_unit_gr, col = group, group = group)) + 
  geom_line() + geom_point() + 
  labs(title = "Quota Units for Cod fishing north of 62N in 2017",
       x = "Effective Date", y = "Quota Unit (Guaranteed)",col = "Length Group (Ref.-Max.)") + 
  theme_bw()

# file save

file_name = paste0("quota_units_from_jmeldings_",yr,".csv")

write_csv(dat_qta_fct,file_name)

