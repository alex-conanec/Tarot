# install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "6zNdWEu45"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "tarot",
                 host = "localhost", port = 5432,
                 user = "alex", password = pw)
rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "joueur")
# TRUE

# creates df, a data.frame with the necessary columns
df <- data.frame(nom = "Lee",
                 prenom = "mumu",
                 pseudo = "AlexL",
                 email = "alexandre.lee@agro-bordeaux.fr")

# writes df to the PostgreSQL database "postgres", table "cartable" 
dbWriteTable(con, "joueur", 
             value = df, append = TRUE, row.names = FALSE)

dbSendStatement(con, "DELETE FROM joueur WHERE id=3 OR id=4;")
dbSendStatement(con, "UPDATE joueur SET 
                nom='Conanec'
                WHERE id=1;")
dbSendStatement(con, "INSERT INTO joueur (nom, prenom, pseudo, email) 
                VALUES ('ds','Alexandre', 'AlexL', 'alexandre.lee@agro-bordeaux.fr');")

# query the data from postgreSQL 
df_postgres <- dbGetQuery(con, "SELECT * from joueur")
df_postgres

# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)
