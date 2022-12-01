#Code to extract and put order in text and table contents to be used in the VISA html´s
#May 2018
#Adriana Villamor



library(flextable)
library(officer)
library(dplyr)
library(tidyr)
library(data.table)

#setwd("ICES Advice Sheets")
setwd('//GalwayFS03/Fishdata/InformaticsProject/Phase1/Stockbook Handover/Forecasting for 2022/ICES Advice Sheets/')
getwd()

myFiles <- list.files(pattern=".*docx")
myFiles <- myFiles[-grep("~", myFiles)]

for(i in 1:length(myFiles)){
  doc <- officer::read_docx(myFiles[i])
  

## Pull out a data.frame of text (content[]) and tables (both tabs[] and content[])
content <- officer::docx_summary(doc)

table_header_name <- content %>% 
  filter(content_type %in% "table cell",
         row_id == 1) %>%
  select(-dplyr::one_of("content_type", "style_name", "level", 
                        "num_id", "row_id", "is_header", "col_span", "row_span")) %>% 
  spread(cell_id, text) %>% 
  mutate(header_name = case_when(grepl("^[V-v]ariable", `1`) &
                                   grepl("^[V-v]alue", `2`) ~ "catchoptionsbasis",
                                 grepl("^[I-i]ndex", `1`) ~ "catchoptionsbasis",
                                 grepl("^[B-b]asis", `1`) ~ "catchoptions",
                                 grepl("^[A-a]dvice\\sbasis", `1`) ~ "advicebasis",
                                 grepl("^[D-d]escription", `1`) &
                                   grepl("^[V-v]alue", `2`) ~ "ranges",
                                 grepl("^[F-f]ramework", `1`) ~ "referencepoints",
                                 grepl("^ICES\\sstock\\sdata\\scategory", `1`) ~ "assessmentbasis",
                                 grepl("^[Y-y]ear", `1`) &
                                   grepl("^ICES\\sadvice", `2`) ~ "advice",
                                 grepl("^[C-c]atch", `1`) &
                                   grepl("^[W-w]anted\\s[C-c]atch", `2`) ~ "catchdistribution",
                                 grepl("^[C-c]atch", `1`) &
                                   grepl("^[L-l]andings", `2`) ~ "catchdistribution",
                                 grepl("^[Y-year]", `1`) ~ "assessmentsummary",
                                 TRUE ~ "other"),
         caption_index = doc_index - 1) %>% 
  select(doc_index,
         caption_index,
         header_name)

table_header_name$text <- content$text[content$doc_index %in% table_header_name$caption_index]
tab_heads <- table_header_name %>% 
  select(doc_index,
         table_name = header_name, 
         text)

tab_names <- tab_heads %>% 
  select(table_name) %>% 
  filter(!table_name %in% c("stocksummary")) %>% 
  distinct(.keep_all = TRUE)

## Holds all the table information (headers, values, ugly tables... everything)
table_cells <- content %>%
  filter(content_type %in% "table cell") %>%
  group_by(doc_index) %>%
  mutate(is_header = case_when(row_id == 1 ~ TRUE,
                               row_id != 1 ~ FALSE),
         table_name = case_when(is_header == TRUE & grepl("^variable$", tolower(text)) ~ "catchoptionsbasis",
                                is_header == TRUE & grepl("^index\\sa", tolower(text)) ~ "catchoptionsbasis",
                                is_header == TRUE & grepl("^basis$", tolower(text)) ~ "catchoptions",
                                is_header == TRUE & grepl("^advice basis$", tolower(text)) ~ "advicebasis",
                                is_header == TRUE & grepl("^description$", tolower(text)) ~ "ranges",
                                is_header == TRUE & grepl("^framework$", tolower(text)) ~ "referencepoints",
                                is_header == TRUE & grepl("^ices stock data category$", tolower(text)) ~ "assessmentbasis",
                                is_header == TRUE & grepl("^ices advice$", tolower(text)) ~ "advice",
                                is_header == TRUE & grepl("^catch \\(\\d{4}\\)$", tolower(text)) ~ "catchdistribution",
                                ### Add additional for Nephrops cat 3+ and other special cases ###
                                TRUE ~ NA_character_),
         table_name = case_when(is_header == TRUE &  ave(is.na(table_name), doc_index, FUN = all) ~ "REMOVE",
                                TRUE ~ table_name),
         table_name = ave(table_name, doc_index, FUN = function(x) unique(x[!is.na(x)]))) %>%
  left_join(tab_names, by = c("table_name")) %>% 
  ungroup() %>%
  filter(table_name != "REMOVE") %>%
  select(doc_index, table_name, is_header, row_id, cell_id, text)

catchoptions <- table_cells%>% filter(table_name =="catchoptions")
HistoryTAC <- table_cells%>% filter(table_name =="advice")
catchoptionsbasis <- table_cells%>% filter(table_name =="catchoptionsbasis")



#catchoptions
table_body <- catchoptions %>% 
  filter(!is_header) %>%
  ungroup %>% 
  select(-doc_index) %>%
  spread(cell_id, text) %>% 
  select(-table_name,
         -is_header,
         -row_id)
table_header <- catchoptions %>% 
  filter(is_header) %>% 
  ungroup %>% 
  select(-doc_index) %>% 
  spread(cell_id, text) %>% 
  select(-table_name,
         -is_header,
         -row_id)

colnames(table_body) <- table_header[1,]
catchoptions <- table_body  

catchoptions<-catchoptions[complete.cases(catchoptions),]
catchoptions <- data.table(catchoptions)

#headers= rbind(headers, names(catchoptions))

#Not possible to add them all to same file due to different column names
#catchoptions <- cbind(myFiles[i], catchoptions)
#catchoptions_final=rbind(catchoptions_final, catchoptions, fill=TRUE)


#Output each to csv
write.csv(catchoptions, paste0("ForecastOptionsTables/",myFiles[i], ".csv"))


#Other information for catchoptions
table_body <- catchoptionsbasis %>% 
  filter(!is_header) %>%
  ungroup %>% 
  select(-doc_index) %>%
  spread(cell_id, text) %>% 
  select(-table_name,
         -is_header,
         -row_id)
table_header <- catchoptionsbasis %>% 
  filter(is_header) %>% 
  ungroup %>% 
  select(-doc_index) %>% 
  spread(cell_id, text) %>% 
  select(-table_name,
         -is_header,
         -row_id)

colnames(table_body) <- table_header[1,]
catchoptionsV2=table_body
write.csv(catchoptionsV2, paste0("CatchOptionsV2/",myFiles[i], ".csv"))


#HistoryTAC
table_body <- HistoryTAC %>% 
  filter(!is_header) %>%
  ungroup %>% 
  select(-doc_index) %>%
  spread(cell_id, text) %>% 
  select(-table_name,
         -is_header,
         -row_id)
table_header <- HistoryTAC %>% 
  filter(is_header) %>% 
  ungroup %>% 
  select(-doc_index) %>% 
  spread(cell_id, text) %>% 
  select(-table_name,
         -is_header,
         -row_id)

colnames(table_body) <- table_header[1,]
HistoryTAC <- table_body

write.csv(HistoryTAC, paste0("TAC/",myFiles[i], ".csv"))


}

# skipped 4,9,25,28,29 for now

