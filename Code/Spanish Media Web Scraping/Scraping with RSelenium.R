```{r EL CONFIDENCIAL, include=FALSE}

link <- "https://www.elconfidencial.com/tags/temas/conflicto-de-ucrania-10136/"

# Function to extract information from each article page

extract_info <- function(link) {
  tryCatch({
    web <- read_html(link)
    fecha_extra <- html_nodes(web, ".articleHeaderBar__sectionDate") %>%
      html_element(".dateTime") %>%
      html_element(".dateTime__created") %>%
      html_text() %>%
      str_extract_all("\\d{2}/\\d{2}/\\d{4}") 
    print(paste0("Ya extrajiste de la pag: ", link))
    if (length(fecha_extra) == 0) {
      fecha_extra <- NA 
    }
    return(fecha_extra)
  }, error = function(e) {
    print(paste0("ERROR en la pag: ", link))
    return(NA)
  })
}

# Configure RSelenium
rD <- rsDriver(browser = "chrome", port = 4568L, chromever = "latest")

remDr <- rD[["client"]]

link <- "https://www.elconfidencial.com/tags/temas/conflicto-de-ucrania-10136/"
remDr$navigate(link)

# Accept cookies
remDr$findElement("css", "#didomi-notice-agree-button")$clickElement()

# Scroll to the bottom of the page to load all articles

tryCatch({
  Sys.sleep(2)
  suppressMessages({
    vermas <- remDr$findElement("css", ".otherArticles__button")
    while(vermas$isElementDisplayed()[[1]]){
      webElem <- remDr$findElement("css", "body")
      webElem$sendKeysToElement(list(key = "end"))
      vermas$sendKeysToElement(list(key = "enter"))
      Sys.sleep(2)
      vermas <- remDr$findElement("css", ".otherArticles__button")
      
    }
  })
}, 
error = function(e) {
  NA_character_
}
)

# Get the source code of the page

code <- remDr$getPageSource()[[1]]

# Extract information from articles

data <- read_html(code) %>%
  html_nodes(".otherArticles__item") %>%
  {cbind(
    titulo = html_node(., ".smallPhoto__title") %>% html_text() %>% str_trim() %>% .[-6],
    autor = html_node(., ".articleInfo__signature") %>% html_text() %>% .[-6],
    corresponsal = NA_character_,
    url = html_node(., ".smallPhoto__titleLink") %>% html_attr("href") %>% .[-6]
  )}

# Extract date using lapply

data <- data.frame(data)

data$fecha <- lapply(data$url,
                extract_info) %>% unlist() %>%
  as.Date("%d/%m/%Y")

# Add additional information
data <- data %>%
  mutate(
    url_general = "https://www.elconfidencial.com/tags/temas/conflicto-de-ucrania-10136/",
    medio = "el confidencial"
  ) %>%
  drop_na(url)

# Close conexion with RSelenium
remDr$close()

# Merge with existing dataframe
dataframe_prensa <- rbind.data.frame(dataframe_prensa, data)

```
