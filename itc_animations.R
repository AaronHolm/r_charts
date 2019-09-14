library(gifski)
library(gganimate)
library(ggplot2)

pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="PG", password="PGAH17", host="data.seia.org", port=5432, dbname="seia")
eia_plus = dbGetQuery(con, "select * from products.eia_plus")
