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
