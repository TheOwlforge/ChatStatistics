library(rjson) # to read json format
library(dplyr) # used to convert to df
library(chron) # date formatting
library(ggplot2) # plotting
library(lubridate) # date formatting
library(scales) # plotting
library(ggstream) # plotting
library(RColorBrewer) # color palettes
library(forcats)

# replace with your own names
filename <- "result.json"
person1 <- "Jane"
person2 <- "Tarzan"
color_p1 <- "#FFC107"
color_p2 <- "#117733"
plot_date_spacing <- "3 month"
agg_unit <- "day" #try week if not working
x_axis_name <- "Date"
y_axis_name1 <- "Number of messages"
y_axis_name2 <- "Text length"
title_name <- "Telegram Chat History"
legend_name1 <- "Sender"
legend_name2 <- "Type"
img_width <- 15.39
img_height <- 10
img_format <- ".png"

dir.create(file.path(paste0("out_", person2)), showWarnings = FALSE)

###################################################################
##################### FUNCTIONS ###################################
###################################################################

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

plot_bar_number <- function(df, unit_string, bar_width){
  df_inv <- df
  df_inv$date <- as.Date(lubridate::floor_date(df_inv$date, unit = unit_string))
  df_inv <- df_inv %>% group_by(date, from) %>% count()
  df_inv[df_inv$from == person2,]$n <- - df_inv[df_inv$from == person2,]$n
  
  ggplot(df_inv, aes(x=date, y=n, fill=from)) +
    geom_bar(stat="identity", width=bar_width) +
    scale_x_date(date_labels = "%b %y", breaks = function(x) seq.Date(from = min(x), to = max(x), by = plot_date_spacing)) +
    labs(x = x_axis_name, y = y_axis_name1, title = paste0(title_name, " (", unit_string, ")"), fill = legend_name1) +
    theme_classic(base_size = 20) +
    theme(panel.grid.major.y = element_line(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_line(), axis.ticks.length = unit(10, "pt")) +
    scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks = c(min(df_inv[df_inv$from == person2,]$n), 0, max(df_inv[df_inv$from == person1,]$n))) +
    scale_fill_manual(values=c(color_p1, color_p2))
  
  ggsave(paste0("out_", person2, "/", "number_", unit_string, img_format), width = img_width, height = img_height)
}

###################################################################
################# PROCESS DATA ####################################
###################################################################

result <- fromJSON(file = filename)

messages <- result$messages

# get column names
infos = c("id")
for (i in 1:length(messages))
{
  infos <- c(infos, names(messages[[i]]))
}
infos <- infos[!duplicated(infos)]

# add missing columns
for (i in 1:length(messages))
{
  for (j in 1:length(infos))
  {
    # add empty data
    if (!infos[j] %in% names(messages[[i]]))
    {
      if (infos[j] == "width" || infos[j] == "height" || infos[j] == "reply_to_message_id" || infos[j] == "duration_seconds")
      {
        messages[[i]][infos[j]] <- 0
      }
      else
      {
        messages[[i]][infos[j]] <- ""
      }
    }
  }
  # remove subinfo of text, move type to text_type column
  if ("type" %in% names(messages[[i]]["text"][[1]][[1]]))
  {
    messages[[i]]["text_type"] <- messages[[i]]["text"][[1]][[1]]$type
    messages[[i]]["text"] <- messages[[i]]["text"][[1]][[1]]$text
  }
  else if (length(messages[[i]]["text"][[1]]) > 1)
  {
    new_type <- ""
    new_text <- ""
    for (k in 1:length(messages[[i]]["text"][[1]]))
    {
      if(length(messages[[i]]["text"][[1]][[k]]) > 1)
      {
        #new_type <- paste(new_type, messages[[i]]["text"][[1]][[k]]$type)
        new_type <- messages[[i]]["text"][[1]][[k]]$type
        new_text <- paste(new_text, messages[[i]]["text"][[1]][[k]]$text)
      }
      else
      {
        #new_type <- paste(new_type, "text")
        new_text <- paste(new_text, messages[[i]]["text"][[1]][[k]])
      }
    }
    messages[[i]]["text"] <- new_text
    messages[[i]]["text_type"] <- new_type
  }
  else
  {
    messages[[i]]["text_type"] <- ""
  }
  #deal with location information
  if ("location_information" %in% names(messages[[i]]))
  {
    messages[[i]]["location_information"] <- ""
  }
}


# convert to data frame
df_raw <- bind_rows(messages)
df <- data.frame(df_raw$id)

# convert date
df$date <- ymd_hms(df_raw$date, tz="CET")
# clean up from data
df$from <- as.factor(df_raw$from)
df$from[df$from == ""] <- df_raw$actor[df$from == ""]
df$from <- droplevels(df$from)
levels(df$from)
levels(df$from) <- c(person1, person2)
levels(df$from)
# clean up types
df$type <- as.factor(df_raw$media_type)
levels(df$type) <- c("text", "gif", "audio", "sticker", "video", "voice msg", "phone call", "image", "pdf", "bold", "contact", "hashtag", "link", "mention")
df$type[df_raw$type == "service"] <- "phone call"
df$type[df_raw$mime_type == "application/pdf"] <- "pdf"
df$type[df_raw$mime_type == "image/jpeg"] <- "image"
df$type[df_raw$mime_type == "image/png"] <- "image"
df$type[df_raw$photo != ""] <- "image"
df$type[df_raw$mime_type == "image/gif"] <- "gif"
df$type[df_raw$mime_type == "audio/ogg"] <- "voice msg"
df$type[df_raw$mime_type == "audio/mpeg"] <- "audio"
df$type[df_raw$mime_type == "video/mp4" && df_raw$media_type != "gif"] <- "video"
df$type[df_raw$text_type == "bold"] <- "bold"
df$type[df_raw$text_type == "email"] <- "contact"
df$type[df_raw$text_type == "hashtag"] <- "hashtag"
df$type[df_raw$text_type == "link"] <- "link"
df$type[df_raw$text_type == "mention"] <- "mention"
df$type[df_raw$text_type == "phone"] <- "contact"
df$type_low <- df$type
df$type_low[df$type_low == "gif"] <- "sticker"
df$type_low[df$type_low == "bold"] <- "text"
df$type_low[df$type_low == "pdf"] <- "audio"
df$type_low[df$type_low == "voice msg"] <- "audio"
df$type_low[df$type_low == "video"] <- "audio"
df$type_low[df$type_low == "phone call"] <- "audio"
df$type_low[df$type_low == "email"] <- "link"
df$type_low[df$type_low == "hashtag"] <- "link"
df$type_low[df$type_low == "mention"] <- "link"
df$type_low[df$type_low == "contact"] <- "link"
df$type_low <- droplevels(df$type_low)
df$type <- droplevels(df$type)
levels(df$type_low) <- c("text", "file/audio", "emoji", "image", "link")
# compute text length
df$text_length <- nchar(df_raw$text)

#########################################################################
######################  PLOTS  ##########################################
#########################################################################

plot_bar_number(df, "weekly", 7)
plot_bar_number(df, "day", 1)

# plot weekly stacked
ggplot(df, aes(x = as.Date(lubridate::floor_date(date, unit = "weekly")), fill=from)) +
  geom_bar(stat = "count", width=7) +
  scale_x_date(date_labels = "%b %y", breaks = function(x) seq.Date(from = min(x), to = max(x), by = plot_date_spacing)) +
  labs(x = x_axis_name, y = y_axis_name1, title = title_name, fill = legend_name1) +
  theme_classic(base_size = 20) +
  theme(panel.grid.major.y = element_line()) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_fill_manual(values=c(color_p1, color_p2))
ggsave(paste0("out_", person2, "/", "number_stacked", img_format), width = img_width, height = img_height)

# plot number as stream
count_table_A <- data.frame(table(data.frame(as.Date(lubridate::floor_date(df$date[df$from == person1], unit = agg_unit)))))
count_table_A$date <- as.Date(count_table_A$Var1)
count_table_A$from <- person1
count_table_A$Var1 <- NULL
count_table_J <- data.frame(table(data.frame(as.Date(lubridate::floor_date(df$date[df$from == person2], unit = agg_unit)))))
count_table_J$date <- as.Date(count_table_J$Var1)
count_table_J$from <- person2
count_table_J$Var1 <- NULL
count_table <- rbind(count_table_A, count_table_J)
ggplot(count_table, aes(x = date, y = Freq, fill=from)) +
  geom_stream() +
  scale_x_date(date_labels = "%b %y", breaks = function(x) seq.Date(from = min(x), to = max(x), by = plot_date_spacing)) +
  labs(x = x_axis_name, y = y_axis_name1, title = title_name, fill = legend_name1) +
  theme_classic(base_size = 20) +
  theme(panel.grid.major.y = element_line()) +
  scale_fill_manual(values=c(color_p1, color_p2))
ggsave(paste0("out_", person2, "/", "number_stream", img_format), width = img_width, height = img_height)

# plot type for both and by person
type_count <- df %>% group_by(type) %>% count() %>% arrange(desc(n))
type_count$type <- factor(type_count$type, levels = as.vector(type_count$type))
type_count_A <- df[df$from == person1,] %>% group_by(type) %>% count() %>% arrange(desc(n))
type_count_A$type <- factor(type_count_A$type, levels = as.vector(type_count_A$type))
type_count_A$from <- person1
type_count_A <- data.frame(type_count_A)
type_count_B <- df[df$from == person2,] %>% group_by(type) %>% count() %>% arrange(desc(n))
type_count_B$type <- factor(type_count_B$type, levels = as.vector(type_count_B$type))
type_count_B$from <- person2
type_count_B <- data.frame(type_count_B)

for (i in 1:length(type_count_B$type)){
  if(length(type_count_A[type_count_A$type == as.character(type_count_B$type[i]),]$type) == 0){
    new_row = data.frame(factor(c(as.character(type_count_B$type[i])), levels=c(levels(type_count_A$type), as.character(type_count_B$type[i]))), as.integer(0), person1)
    names(new_row)=c("type","n", "from")
    type_count_A <- rbind(type_count_A, new_row)
    levels(type_count_A$type) <- c(levels(type_count_A$type), as.character(type_count_B$type[i]))
  }
}
for (i in 1:length(type_count_A$type)){
  if(length(type_count_B[type_count_B$type == as.character(type_count_A$type[i]),]$type) == 0){
    new_row = data.frame(factor(c(as.character(type_count_A$type[i])), levels=c(levels(type_count_B$type), as.character(type_count_A$type[i]))), as.integer(0), person2)
    names(new_row)=c("type","n", "from")
    type_count_B <- rbind(type_count_B, new_row)
    levels(type_count_B$type) <- c(levels(type_count_B$type), as.character(type_count_A$type[i]))
  }
}

# arrange both alphabetically
type_count_A$type <- factor(type_count_A$type, levels=sort(levels(type_count_A$type)))
type_count_A <- type_count_A %>% arrange(type)
type_count_B$type <- factor(type_count_B$type, levels=sort(levels(type_count_B$type)))
type_count_B <- type_count_B %>% arrange(type)

# sort based on sum
type_count_A$sum <- type_count_A$n + type_count_B$n
type_count_A <- type_count_A %>% arrange(desc(sum))
type_count_A$type <- factor(type_count_A$type, levels = as.vector(type_count_A$type))
type_count_B$sum <- type_count_A$n + type_count_B$n
type_count_B <- type_count_B %>% arrange(desc(sum))
type_count_A$type <- factor(type_count_A$type, levels = as.vector(type_count_A$type))

type_count_per_person <- rbind(type_count_A, type_count_B)

ggplot(type_count[type_count$type != "text",], aes(x=type, y=n, fill=type)) +
  geom_bar(stat="identity", color = "black") +
  labs(x = legend_name2, y = y_axis_name1, title = title_name, fill = legend_name2) +
  theme_classic(base_size = 20) +
  theme(panel.grid.major.y = element_line()) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_discrete(labels=addline_format(levels(type_count$type))[2:length(levels(type_count$type))]) +
  scale_fill_brewer(palette = "Set3")
ggsave(paste0("out_", person2, "/", "type_count", img_format), width = img_width, height = img_height)

ggplot(type_count_per_person[type_count_per_person$type != "text",], aes(x=type, y=n, fill=from)) +
  geom_bar(stat="identity", position = "fill") +
  labs(x = legend_name2, y = y_axis_name1, title = title_name, fill = legend_name2) +
  theme_classic(base_size = 20) +
  theme(panel.grid.major.y = element_line()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), labels = scales::percent) +
  scale_x_discrete(labels=addline_format(levels(type_count_per_person$type)[! levels(type_count_per_person$type) %in% c("text")])) +
  scale_fill_manual(values=c(color_p1, color_p2))
ggsave(paste0("out_", person2, "/", "type_count_by_person", img_format), width = img_width, height = img_height)

# plot type as stream
count_table_file <- data.frame(table(data.frame(as.Date(lubridate::floor_date(df$date[df$type_low == "file/audio"], unit = "day")))))
count_table_file$date <- as.Date(count_table_file$Var1)
count_table_file$type <- "file/audio"
count_table_file$Var1 <- NULL

count_table_emoji <- data.frame(table(data.frame(as.Date(lubridate::floor_date(df$date[df$type_low == "emoji"], unit = "day")))))
count_table_emoji$date <- as.Date(count_table_emoji$Var1)
count_table_emoji$type <- "emoji"
count_table_emoji$Var1 <- NULL

count_table_image <- data.frame(table(data.frame(as.Date(lubridate::floor_date(df$date[df$type_low == "image"], unit = "day")))))
count_table_image$date <- as.Date(count_table_image$Var1)
count_table_image$type <- "image"
count_table_image$Var1 <- NULL

count_table_link <- data.frame(table(data.frame(as.Date(lubridate::floor_date(df$date[df$type_low == "link"], unit = "day")))))
count_table_link$date <- as.Date(count_table_link$Var1)
count_table_link$type <- "link"
count_table_link$Var1 <- NULL

type_table <- rbind(count_table_file, count_table_emoji, count_table_image, count_table_link)
ggplot(type_table, aes(x = date, y = Freq, fill=type)) +
  geom_stream() +
  scale_x_date(date_labels = "%b %y", breaks = function(x) seq.Date(from = min(x), to = max(x), by = plot_date_spacing)) +
  labs(x = x_axis_name, y = y_axis_name1, title = title_name, fill = legend_name2) +
  theme_classic(base_size = 20) +
  theme(panel.grid.major.y = element_line()) + 
  scale_fill_brewer(palette = "Set1")
ggsave(paste0("out_", person2, "/", "type_stream", img_format), width = img_width, height = img_height)

# plot text length
msg_A <- df[df$from == person1,]
length_A <- aggregate(msg_A$text_length, by=list(as.Date(msg_A$date)), sum)
length_A$date <- length_A$Group.1
length_A$count <- length_A$x
length_A$from <- person1
length_A$Group.1 <- NULL
length_A$x <- NULL

msg_B <- df[df$from == person2,]
length_B <- aggregate(msg_B$text_length, by=list(as.Date(msg_B$date)), sum)
length_B$date <- length_B$Group.1
length_B$count <- length_B$x
length_B$from <- person2
length_B$Group.1 <- NULL
length_B$x <- NULL

for (i in 1:length(length_B$date)){
  if(length(length_A[length_A$date == length_B$date[i],]$date) == 0){
    new_row = data.frame(length_B$date[i], as.integer(1), person1)
    names(new_row)=c("date","count", "from")
    length_A <- rbind(length_A, new_row)
  }
}

length_A <- length_A[order(as.Date(length_A$date)),]
rownames(length_A) <- 1:length(length_A$date)

for (j in 1:length(length_A$date)){
  if(length(length_B[length_B$date == length_A$date[j],]$date) == 0){
    new_row = data.frame(length_A$date[j], as.integer(1), person2)
    names(new_row)=c("date","count", "from")
    length_B <- rbind(length_B, new_row)
  }
}

length_B <- length_B[order(as.Date(length_B$date)),]
rownames(length_B) <- 1:length(length_B$date)

length_table <- rbind(length_A, length_B)
length_table <- length_table[order(as.Date(length_table$date)),]
rownames(length_table) <- 1:length(length_table$date)

length_table_inv <- length_table
length_table_inv[length_table$from == person2,]$count <- - length_table[length_table$from == person2,]$count
length_table_weekly_A <- length_table[length_table$from == person1,]
length_table_weekly_A$date <- lubridate::floor_date(length_table_weekly_A$date, unit = "weekly")
length_table_weekly_A <- aggregate(length_table_weekly_A$count, by=list(as.Date(length_table_weekly_A$date)), sum)
length_table_weekly_B <- length_table[length_table$from == person2,]
length_table_weekly_B$date <- lubridate::floor_date(length_table_weekly_B$date, unit = "weekly")
length_table_weekly_B <- aggregate(length_table_weekly_B$count, by=list(as.Date(length_table_weekly_B$date)), sum)

length_table_weekly_A[,3] <- person1
length_table_weekly_B[,3] <- person2
length_table_weekly <- rbind(length_table_weekly_A, length_table_weekly_B)
colnames(length_table_weekly) <- c("date", "count", "from")

for (i in 1:length(length_table_weekly_B$Group.1)){
  if(length(length_table_weekly_A[length_table_weekly_A$Group.1 == length_table_weekly_B$Group.1[i],]$Group.1) == 0){
    new_row = data.frame(length_table_weekly_B$Group.1[i], as.integer(0))
    names(new_row)=c("Group.1","x")
    length_table_weekly_A <- rbind(length_table_weekly_A, new_row)
    print(i)
  }
}
for (j in 1:length(length_table_weekly_A$Group.1)){
  if(length(length_table_weekly_B[length_table_weekly_B$Group.1 == length_table_weekly_A$Group.1[j],]$Group.1) == 0){
    new_row = data.frame(length_table_weekly_A$Group.1[j], as.integer(0))
    names(new_row)=c("Group.1","x")
    length_table_weekly_B <- rbind(length_table_weekly_B)
    print(j)
  }
}

ggplot(length_table_inv, aes(x = as.Date(lubridate::floor_date(date, unit = "weekly")), y=count, fill=from)) +
  geom_bar(stat="identity", width=7) +
  scale_x_date(date_labels = "%b %y", breaks = function(x) seq.Date(from = min(x), to = max(x), by = plot_date_spacing)) +
  labs(x = x_axis_name, y = y_axis_name2, title = paste(title_name, "(weekly)"), fill = legend_name1) +
  theme_classic(base_size = 20) +
  theme(panel.grid.major.y = element_line(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_line(), axis.ticks.length = unit(10, "pt")) +
  scale_y_continuous(expand = expansion(mult = c(.1, .1)), breaks = c(-max(length_table_weekly_B$x), 0, max(length_table_weekly_A$x))) +
  scale_fill_manual(values=c(color_p1, color_p2))
ggsave(paste0("out_", person2, "/", "length_weekly", img_format), width = img_width, height = img_height)

ggplot(length_table_inv, aes(x = as.Date(lubridate::floor_date(date, unit = "day")), y=count, fill=from)) +
  geom_bar(stat="identity", width=1) +
  scale_x_date(date_labels = "%b %y", breaks = function(x) seq.Date(from = min(x), to = max(x), by = plot_date_spacing)) +
  labs(x = x_axis_name, y = y_axis_name2, title = paste(title_name, "(daily)"), fill = legend_name1) +
  theme_classic(base_size = 20) +
  theme(panel.grid.major.y = element_line(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_line(), axis.ticks.length = unit(10, "pt")) +
  scale_y_continuous(expand = expansion(mult = c(.05, .05)), breaks = c(-max(length_B$count), 0, max(length_A$count))) +
  scale_fill_manual(values=c(color_p1, color_p2))
ggsave(paste0("out_", person2, "/", "length_daily", img_format), width = img_width, height = img_height)

#try length_table_weekly if not working
ggplot(length_table, aes(x = date, y = count, fill=from)) +
  geom_stream() +
  scale_x_date(date_labels = "%b %y", breaks = function(x) seq.Date(from = min(x), to = max(x), by = plot_date_spacing)) +
  labs(x = x_axis_name, y = y_axis_name2, title = title_name, fill = legend_name1) +
  theme_classic(base_size = 20) +
  theme(panel.grid.major.y = element_line()) +
  scale_y_continuous(expand = expansion(mult = c(0.1, .1))) +
  scale_fill_manual(values=c(color_p1, color_p2))
ggsave(paste0("out_", person2, "/", "length_stream", img_format), width = img_width, height = img_height)
