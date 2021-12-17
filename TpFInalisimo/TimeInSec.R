#Tiempo a partir de los títulos

files <- list.files()
files
ending <- unlist(strsplit(files[1], "_"))[3]
times <- gsub(".jpg", "", ending)
times <- as.integer(times)
times
times %% 10
seconds <- times %% 100
minutes <- (times %% 10000 - seconds) / 100
hours <- (times - minutes * 100 - seconds) / 10000
t <- seconds + minutes * 60 + hours * 3600
t
¨