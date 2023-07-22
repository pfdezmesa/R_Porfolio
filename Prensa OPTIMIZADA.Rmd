In this example, we are going to show how to obtain information from some of the main media in Spain about a current issue.

We are going to offer two different examples. Among other packages we will work with "Rvest" and "RSelenium". And finally we will export this data to MySQL with the "RMySQL" package.

---
title: "Media"
output: html_document
date: "2023-01-01"
---

```{r Library, include=FALSE}

library(tibble)
library(dplyr)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyr)
library(RMySQL)
library(purrr)
library(magrittr)

```

```{r Create Dataframe, include=FALSE}

dataframe_prensa <- tibble()

```

```{r EL PAIS, include=FALSE}

# Define function to extract data from a page

extract_data <- function(url_general) {
  tryCatch({
    code <- read_html(url_general)
    
    nodo_general <- ".c_a"
    nodo_titulo <- "h2"
    nodo_autor <- ".c_a_a"
    nodo_corresponsal <- ".c_a_l"
    nodo_fecha <- "#sc_date"
    nodo_url <- "h2 > a"
    
    titulo <- html_nodes(code, nodo_titulo) %>%
      html_text()
    
    autor <- html_nodes(code, nodo_general) %>%
      html_element(nodo_autor) %>%
      html_text()
    
    corresponsal <- html_nodes(code, nodo_general) %>%
      html_element(nodo_corresponsal) %>%
      html_text()
    
    fecha <- html_nodes(code, nodo_fecha) %>%
      html_text() %>%
      unlist() %>%
      str_replace_all(" dic ", "/12/") %>%
      str_replace_all(" nov ", "/11/") %>%
      str_replace_all(" oct ", "/10/") %>%
      str_replace_all(" sep ", "/09/") %>%
      str_replace_all(" ago ", "/08/") %>%
      str_replace_all(" jul ", "/07/") %>%
      str_replace_all(" jun ", "/06/") %>%
      str_replace_all(" may ", "/05/") %>%
      str_replace_all(" abr ", "/04/") %>%
      str_replace_all(" mar ", "/03/") %>%
      str_replace_all(" feb ", "/02/") %>%
      str_replace_all(" ene ", "/01/") %>%
      as.Date(format = "%d/%m/%Y")
    
    url <- html_nodes(code, nodo_url) %>%
      html_attr("href")
    
    datos <- tibble(titulo, autor, corresponsal, fecha, url,
                    url_general = url_general, medio = "el pais")
    
    print(paste0("Ya extrajiste de la pag: ", url_general))
    
    datos
  }, error = function(e) {
    message("Error en página: ", url_general)
    tibble() # Devolver tibble vacío en caso de error
  })
}

# Create list of URLS and extract the data

todos_links <- paste0("https://elpais.com/noticias/rusia/", 0:150)
datos_lista <- map_df(todos_links, extract_data)

# Combine the extracted data with the existing dataframe (dataframe_prensa)
dataframe_prensa <- rbind.data.frame(dataframe_prensa, datos_lista)

```


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

```{r Delete duplicated , include=FALSE}

dataframe_prensa <- dataframe_prensa[!duplicated(dataframe_prensa$url), ]

```

```{r Export to MySQL, include=FALSE, eval=FALSE}

# Connection information

db_user <- "root"
db_password <- *******
db_name <- "prensa"
db_host <- "localhost"  
db_port <- 3306 

# Establish the connection

con <- dbConnect(MySQL(), user = db_user, password = db_password, dbname = db_name, host = db_host, port = db_port, local_infile = TRUE)

# Enable function to import local files.

sql_query <- "SET GLOBAL local_infile = 'ON';"
dbExecute(con, sql_query)

# Write table to MySQL database

nombre_tabla <- "tabla_prensa"

dbWriteTable(con, name = nombre_tabla, value = dataframe_prensa, row.names = FALSE, overwrite = TRUE)

# Create Primary Key and send query to MySQL

sql_query <- "ALTER TABLE prensa.tabla_prensa ADD PRIMARY KEY (url(250));"

dbExecute(con, sql_query)

# We set the date as of type DateTime

sql_query <- "ALTER TABLE `prensa`.`tabla_prensa` 
CHANGE COLUMN `fecha` `fecha` DATETIME NULL DEFAULT NULL ;"

dbExecute(con, sql_query)

#Disabled the function of importing local files for security

sql_query <- "SET GLOBAL local_infile = 'OFF';"
dbExecute(con, sql_query)

#Disconnect with MySQL

dbDisconnect(con)

```







