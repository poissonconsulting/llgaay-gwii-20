source("header.R")

conn <- sbf_open_db(project, exists = FALSE)

sbf_set_sub("tidy")
sbf_load_datas()

DBI::dbGetQuery(conn,
  "CREATE TABLE Species (
                Species TEXT NOT NULL,
                CommonName TEXT NOT NULL,
                PRIMARY KEY (Species),
                UNIQUE(CommonName))")

rws_write(species, conn = conn)

DBI::dbGetQuery(conn,
  "CREATE TABLE Count (
                Species TEXT NOT NULL,
                Count INTEGER NOT NULL,
                CHECK(
                Count >= 0 AND COUNT <= 10
                ),
                FOREIGN KEY (Species) REFERENCES Species (Species))")

rws_write(count, conn = conn)

sbf_close_db(conn)
