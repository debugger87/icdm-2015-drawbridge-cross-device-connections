library(readr)
library(xtable)

system("ls -al ../input", intern = TRUE)

train <- read_csv("../input/dev_train_basic.csv")
cat("There are ", nrow(train), " devices in the device train file")

head.table <- function(table) {
  html <- print(xtable(head(table)), type="html", print.results=FALSE)
  cat(paste0("<div style='width:800; overflow:auto; border-width: 2'>", html, "</div>"))
}

head.table(train)

test <- read_csv("../input/dev_test_basic.csv")
cat("There ar ", nrow(test), "devices in the device train file")
head.table(test)

device <- rbind(train, test)
cat("There are", length(unique(device$device_type)), "unique device types")
cat("There are", length(unique(device$device_os)), "unique device operating systems")
cat("There are", length(unique(device$country)), "unique countries")

# graphic
library(dplyr)
library(ggvis)
device %>%
  group_by(device_type) %>%
  summarize(count=length(device_type)) %>%
  ggvis(~device_type, ~count) %>%
  layer_bars(fill:="#20beff")

cookie <- read_csv("../input/cookie_all_basic.csv", n_max=100)
head.table(cookie)

read_bad_csv <- function(file_name, bad_col=3, n_max=-1) {
  f_in <- file(file_name)
  lines <- readLines(f_in, n=n_max)
  close(f_in)
  temp_csv_1 <- tempfile()
  f_out_1 <- file(temp_csv_1, "w")
  writeLines(gsub("\\{|\\}", '"', lines), f_out_1)
  close(f_out_1)
  data <- read_csv(temp_csv_1, col_names=FALSE)
  temp_csv_2 <- tempfile()
  f_out_2 <- file(temp_csv_2, "w")
  for (i in 1:nrow(data)) {
    bad_lines <- strsplit(substr(data[i,bad_col], 2, nchar(data[i,bad_col])-1), "\\),\\(")[[1]]
    if (bad_col==1) {
      lines <- paste(bad_lines,
                     paste0(as.character(data[i,2:ncol(data)]), collapse=","),
                     sep=",")
    } else if (bad_col<ncol(data)) {
      lines <- paste(paste0(as.character(data[i,1:bad_col-1]), collapse=","),
                     bad_lines,
                     paste0(as.character(data[i,bad_col+1:ncol(data)]), collapse=","),
                     sep=",")
    } else {
      lines <- paste(paste0(as.character(data[i,1:ncol(data)-1]), collapse=","),
                     bad_lines,
                     sep=",")
    }
    writeLines(lines, f_out_2)
  }
  close(f_out_2)
  return(read_csv(temp_csv_2))
}

ip <- read_bad_csv("../input/id_all_ip.csv", bad_col=3, n_max=100)
head.table(ip)

property <- read_bad_csv("../input/id_all_property.csv", bad_col=3, n_max=100)
head.table(property)

agg <- read_csv("../input/ipagg_all.csv", n_max=100)
head.table(agg)

property_category <- read_bad_csv("../input/property_category.csv", bad_col=2)
head.table(property_category)
cat("There are", length(unique(property_category$property_id)), "unique properties")
cat("There are", length(unique(property_category$category_id)), "unique categories")

sample_submission <- read_csv("../input/sampleSubmission.csv")
head.table(sample_submission)
write_csv(sample_submission, "sample_submission.csv")
