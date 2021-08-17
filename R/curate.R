#' This function does the metadata curation. It adds new data to the current metadata dataframe.
#' @import rvest
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import purrr
#' @param repository, the metadata repository
#' @param n, number of datasets to select
#' @export

curate <- function(repository, n = 30) {
  if(repository == "cdph") {
    full_df <- data.frame(matrix(ncol = 12, nrow = 0))
    seq <- 1:119
    for(i in seq) {
      i <- sample(seq, 1)[1]
      url <- paste("https://data.ca.gov/dataset?q=data&page=", i, sep = "")
      pg <- read_html(url)
      links <- paste("https://data.ca.gov", html_attr(html_nodes(read_html(url), ".dataset-heading a"), "href"), sep = "") %>% data.frame() %>% distinct()
      if(nrow(full_df) == n) {
        break
      }
      for(j in links$.) {
        data_link <- j
        tryCatch(expr = {
          full_df <- rbind(full_df, scrape_cdph(data_link))
        }, error = function(err) {
          NULL
        })
        if(nrow(full_df) == n) {
          break
        }
      }
    }
    clean_tag <- str_split(full_df$Tags, "[ ]+") %>%
      paste(sep = ";") %>%
      data.frame()
    clean_tag <- str_remove_all(clean_tag$., c("c[{]", "[)]", "[\"]")) %>%
      data.frame()
  }else if(repository == "datagov") {
    full_df <- data.frame(matrix(ncol = 8, nrow = 0))
    seq <- 1:15391
    for(i in seq) {
      if(nrow(full_df) == n) {
        break
      }
      i <- sample(seq, 1)[1]
      url <- paste("https://catalog.data.gov/dataset?page=", i, sep = "")
      pg <- read_html(url)
      links <- html_attr(html_nodes(pg, ".dataset-heading a"), "href")
      links <- paste("https://catalog.data.gov", links, sep = "")
      links <- links %>% data.frame() %>% distinct()
      links <- links$.
      for(j in links) {
        scraped <- scrape_datagov(j)
        full_df <- rbind(full_df, scraped)
        if(nrow(full_df) == n) {
          break
        }
      }
    }
  }else if(repository == "datahub") {
    url <- "https://datahub.io/search"
    link <- Frost2021Package::links(url, "a")
    link <- link[str_detect(link, "/core/")]
    link <- link %>% data.frame() %>% distinct()
    link <- link$.
    link <- paste("https://datahub.io", link, sep = "")
    full_df <- data.frame(matrix(ncol = 8, nrow = 0))
    name <- c()
    for(i in link) {
      if(nrow(full_df) == n) {
        break
      }
      name <- append(name, checkNull(scrape_rvest(i, "h1") %>% paste(collapse = " ") %>% str_remove_all(" Certified")))
    }
  }else if(repository == "datashare") {
    full_df <- data.frame(matrix(ncol = 11, nrow = 0))
    seq <- 1:172
    for(i in seq) {
      if(nrow(full_df) == n) {
        break
      }
      i <- sample(seq, 1)[1]
      url <- paste("https://datashare.ed.ac.uk/discover?rpp=20&etal=0&group_by=none&page=", i, sep = "")
      link <- Frost2021Package::links(url, ".artifact-description :nth-child(1)")
      link <- link[!is.na(link)]
      link <- paste("https://datashare.ed.ac.uk", link, sep = "")
      for(j in link) {
        if(nrow(full_df) == n) {
          break
        }
        full_df <- rbind(full_df, scrape_datashare(j))
      }
    }
  }else if(repository == "dryad") {
    full_df <- data.frame(matrix(ncol = 10, nrow = 0))
    seq <- 1:41864
    for(i in seq) {
      if(nrow(full_df) == n) {
        break
      }
      i <- sample(seq, 1)[1]
      url <- paste("https://datadryad.org/search?page=", i, "&q=", sep = "")
      pg <- read_html(url)
      links <- html_attr(html_nodes(pg, "#documents a"), "href")
      links <- paste("https://datadryad.org", links, sep = "")
      links <- data.frame(links) %>% distinct()
      links <- links$links
      for(j in links) {
        if(nrow(full_df) == n) {
          break
        }
        tryCatch(expr = {
          full_df <- rbind(full_df, scrape_dryad(j))
        }, error = function(err) {
          NULL
        })
      }
    }
  }else if(repository == "harvard") {
    full_df <- data.frame(matrix(ncol = 13, nrow = 0))
    seq <- 1:11944
    for(i in seq) {
      i <- sample(seq, 1)[1]
      url <- paste("https://dataverse.harvard.edu/dataverse/harvard?q=&types=dataverses%3Adatasets&sort=dateSort&order=desc&page=", i, sep = "")
      links <- Frost2021Package::links(url, ".card-title-icon-block a")
      links <- paste("https://dataverse.harvard.edu", links, sep = "")
      links <- links[str_detect(links, "/dataset")]
      if(nrow(full_df) == n) {
        break
      }
      for(j in links) {
        if(nrow(full_df) == n) {
          break
        }
        tryCatch(expr = {full_df <- rbind(full_df, scrape_harvard(j))}, error = function(error) {
          NULL
        })
      }
    }
  }else if(repository == "re3data") {
    full_df <- data.frame(matrix(ncol = 10, nrow = 0))
    seq <- 1:109
    for(i in seq) {
      if(nrow(full_df) == n) {
        break
      }
      i <- sample(seq, 1)[1]
      link <- links(paste("https://www.re3data.org/search?query=&page=", i, sep = ""), "a")
      link <- link %>% data.frame() %>% distinct()
      link <- link$.
      link <- link[str_detect(link, "/repository") & !is.na(link)]
      link <- paste("https://re3data.org", link, sep = "")
      for(j in link) {
        if(nrow(full_df) == n) {
          break
        }
        tryCatch(expr = {
          full_df <- rbind(full_df, scrape_re3data(j))
        }, error = function(error) {
          NULL
        })
      }
    }
  }else if(repository == "zenodo") {
    full_df <- data.frame(matrix(ncol = 8, nrow = 0))
    for(i in 1:1) {
      url <- paste("https://zenodo.org/search?page=", i, "&size=20#", sep = "")
      url <- "https://zenodo.org"
      pg <- read_html(url)
      links <- html_attr(html_nodes(pg, "h4 a"), "href")
      links <- links[str_detect(links, "/record/")]
      links <- paste("https://zenodo.org", links, sep = "")
      if(nrow(full_df) == n) {
        break
      }
      for(j in links) {
        full_df <- rbind(full_df, scrape_zenodo(j))
        if(nrow(full_df) == n) {
          break
        }
      }
    }
  }
  mdf <- read.csv("./Data/mdf.csv")
  binded <- rbind(mdf, target(full_df, names(mdf)))
  return(binded)
}
