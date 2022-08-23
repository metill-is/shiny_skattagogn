library(DBI)
library(config)
library(tidyverse)
library(lubridate)
library(janitor)
library(feather)


usr <- config::get("postgres_user")
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = usr$dbname, 
                 host = usr$host,
                 port = usr$port, 
                 user = usr$username, 
                 password = usr$password)


tbl(con, "tiundamork_1997_2021") |> 
    collect() |> 
    write_feather("data/tiundamork_1997_2021.feather")


tbl(con, "skyribreytur_skuldahlutfoll") |> 
    filter(str_detect(skyribreyta, "[0-9]"), hlutf_tegund != "Samtals", name != "Samtals") |> 
    collect() |> 
    write_feather("data/skuldahlutfall.feather")


dbDisconnect(con)

