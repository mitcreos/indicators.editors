library(tidyverse)
library(rvest)
library(magrittr)
library(dplyr)
library(stringr)
scrape_Scitechnol <-function() {
journals <- read.csv(url("https://raw.githubusercontent.com/andreaspacher/academic-publishers/0b7236a0e799377d067d471a1c132a98619b9294/Output/allpublishers-PRELIMINARY-2021-12-09.csv")) %>%
  filter(publisher == "OMICS") %>%
  distinct()

journals %<>%
  mutate(url=stringr::str_replace_all(url,"^NA", ""))

journals <- journals %>%
  mutate(url = if_else(str_detect(url, "^NA"), str_replace_all(url, "^NA", ""), url))

journals <- journals %>%
  mutate(url = str_replace(url, "scitechnol.com/", "scitechnol.com/editorialboard-"))
#try using less urls in journals; or try other scrape files

#Try taking a select few URLs instead -> still no people found
start <- 5
end <- 10
journals <- journals %>%
  slice(start:end)

# prepare the scraping process
EdList <- list()

for (i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  

  # Start scraping with the modified URL
  wholepage <- try(
    rvest::session(
      journals$url[i],
      httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
    ),
    silent = TRUE
  )
  
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print(paste("--- Error scraping page:", journals$url[i]))
    next
  }

journalname <- rvest::html_node(wholepage, css = "h2.font-size-30")
journalname <- rvest::html_text(journalname)
journalname <- stringr::str_remove(journalname, "\\..*")

issn <- rvest::html_node(wholepage, ".font-size-20")
issn <- rvest::html_text(issn)

webpage <- rvest::html_nodes(wholepage, css = ".col-md-9")

people <- rvest::html_nodes(webpage, "h4")
people <- rvest::html_text(people)

affiliations <- rvest::html_nodes(webpage, "p.team-v3-paragraph")
affiliations <- rvest::html_text(affiliations)


if(length(people)==0) {
  warning("No people found", printtext)
  next
}

  EdB <- data.frame(
    "editor" = people,
    "role" = rep("Editorial Board", length(people)),
    "affiliation" = affiliations,
    "journal" = journalname,
    "publisher" = "SciTechnol",
    "issn" = issn,
    "url" = journals$url[i],
    "date" = Sys.Date()
  )
  
  EdList[[i]] <- EdB
  print(paste0("--- Found ", nrow(EdB), " editors for ", journalname))
}

Sys.sleep(8)



combined_DF <- dplyr::bind_rows(EdList)
print("Finished scraping process.")
str(combined_DF)


library(tidyverse)
DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date) %>%
  distinct()


journals2 <- journals[grep("scitechnol.com", journals$url),]
journals2 <- anti_join(journals,journals2)
journals2$url <- paste0("https://www.scitechnol.com/editorialboard-", journals2$url)

EdList2 <- list()

for(i in 1:nrow(journals2)) {
  
  printtext <- paste(i, journals2$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(rvest::session(
    journals2$url[i],
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  ), silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  journalname <- rvest::html_node(wholepage, css = "h2.font-size-30")
  journalname <- rvest::html_text(journalname)
  journalname <- stringr::str_remove(journalname, "\\..*")
  
  issn <- rvest::html_node(wholepage, ".font-size-20")
  issn <- rvest::html_text(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = ".col-md-9")
  
  people <- rvest::html_nodes(webpage, "h4")
  people <- rvest::html_text(people)
  
  affiliations <- rvest::html_nodes(webpage, "p.team-v3-paragraph")
  affiliations <- rvest::html_text(affiliations)
  
  if (length(people)==0) {
    warning("no people", printtext)
    next
  }
  
  EdB <- data.frame(
    "editor" = people,
    "role" = rep("Editorial Board", length(people)),
    "affiliation" = affiliations,
    "journal" = journalname,
    "publisher" = "SciTechnol",
    "issn" = issn,
    "url" = journals2$url[i],
    "date" = Sys.Date()
  )
  
  EdList2[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(8)
  
}

DF2 <- dplyr::bind_rows(EdList2) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date) %>%
  distinct()

DF3 <- rbind(DF, DF2)

DF3 <- DF3 %>%
  filter(!is.na(url)) %>%
  filter(!is.na(editor)) %>%
  mutate(journal = stringr::str_remove(journal, "ISSN: .*$")) %>%
  mutate(issn = stringr::str_remove(issn, "ISSN: "))

write_tsv(DF3, paste0("Output\\2022-Scraping\\SciTechnol-", Sys.Date(), ".tsv"))

}
debug(scrape_Scitechnol)
scrape_Scitechnol()
