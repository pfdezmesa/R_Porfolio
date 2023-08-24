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
