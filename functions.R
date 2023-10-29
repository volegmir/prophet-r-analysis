





t_create <- function(con, t1, t2) {
  query1 <- paste0("CREATE TABLE ", t1, " (
    ds DATE,
    y DECIMAL
  );")
  query2 <- paste0("CREATE TABLE ", t2, " (
    ds DATE,
    y DECIMAL
  );")
  dbSendQuery(con, query1)
  dbSendQuery(con, query2)
}

t_drop <- function(con, t1, t2) {
  query1 <- paste0("DROP TABLE ", t1, ";")
  query2 <- paste0("DROP TABLE ", t2, ";")
  dbSendQuery(con, query1)
  dbSendQuery(con, query2)
}


insertData <- function(con, tablename, data) {
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    query <-
      paste0("INSERT INTO ",
             tablename,
             " VALUES ('",
             data$ds[i],
             "',",
             data$y[i],
             ");")
    dbSendQuery(con, query)
  }
}

insertForecast <- function(con, tablename, data, limit) {
  for (i in 1:limit) {
    query <-
      paste0("INSERT INTO ",
             tablename,
             " VALUES ('",
             data$ds[i],
             "',",
             data$y[i],
             ");")
    dbSendQuery(con, query)
  }
}

deleteData <- function(con, tablename) {
  query <- paste0("DELETE FROM ", tablename, ";")
  dbSendQuery(con, query)
}

dataForForecastPlot <- function(con, table, rows) {
  data <-
    dbGetQuery(con,
               paste0("SELECT ds, y FROM ",
                      table,
                      " ORDER BY ds DESC LIMIT ",
                      rows))
}