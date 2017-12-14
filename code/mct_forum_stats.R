##
## Descriptive stats from the McGill Cycling community forum
##

##### PRELIMINARIES #####
library(plyr)
library(ggplot2)
library(reshape2)
library(lubridate) # discovered long after the bulk was written...
# will try to port relying more on lubridate for readability

setwd("~/projects/mct-forum-stats/")

##### LOAD DATA #####
url <- "http://files.j-vk.com/timestamps.dat" # up to "2014-11-13"
dat <- read.csv(url)
str(dat) # what format is this data in
head(dat) # look at the first few entries

##### CLEAN/FORMAT/ORGANIZE #####
dat$date_time <- as.POSIXct(dat$Time, origin = "1970-01-01")
dat$date <- as.Date(dat$date_time)
dat$month <- factor(format(dat$date, "%b"), levels = month.abb)
dat$weekday <- factor(weekdays(dat$date, abbreviate = TRUE),
                      levels = c("Sun","Mon","Tue","Wed",
                                 "Thu","Fri","Sat"))
dat$day <- format(dat$date, '%d')
dat$time <- strftime(dat$date_time, format = "%H:%M")
dat$hr <- as.POSIXlt(dat$date_time)$hour
dat$min <- as.POSIXlt(dat$date_time)$min

# sort posts by date
dat <- ddply(dat,.(Name),function(df) {df[order(df$Time),]} )

# get weekday and hour of day of each post
dat$weekday_hr <- paste(dat$weekday, dat$hr,sep = " ")
# count number of occurences over unique values
weekly_posts <- as.data.frame(table(dat$weekday_hr))
names(weekly_posts) <- c("weekday_hr", "Freq")

# get each day/hour combination for labelling
weekday_hr_labs <- paste(rep(levels(unique(dat$weekday)), each = 24), 0:23)
# structure as a factor (for plotting)
weekly_posts$weekday_hr <- factor(weekly_posts$weekday_hr,
                         levels = weekday_hr_labs)
# repeat 1 for grouping purposes (solves plotting issue)
weekly_posts$g <- rep(1,length(weekly_posts$weekday_hr))
# get the density of posts at each timepoint
weekly_posts$dist <- weekly_posts$Freq / sum(weekly_posts$Freq)  

# get each timepoint
lvls <- levels(weekly_posts$weekday_hr)

my_theme <- theme_bw() +
  theme(axis.text.x        = element_text(face = "italic", size = 12),
        axis.text.y        = element_blank(),
        panel.border       = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "firebrick2"),
        legend.key         = element_blank(),
        axis.ticks         = element_blank(),
        strip.background   = element_blank(),
        plot.title         = element_text(lineheight = 0.8, face = "bold"))

### save individual animation frames
for (i in 1:length(lvls)) {
  if (i %% 10 == 0)
    print(i)
  
  png(filename = paste("./img/tmp/", sprintf("%04d",i), ".png", sep = ""),
      width = 1000, height = 300)
  print(
  ggplot(weekly_posts[weekly_posts$weekday_hr %in% lvls[1:i],],
         aes(x = weekday_hr,y = dist)) +
    labs(
      title = "Aggregated Forum Activity of McGill Cycling from 2006-present",
         y  = "Posts",
         x  = "Time"
      ) + 
    geom_line(aes(group = g), size = 2) +
    scale_x_discrete(breaks = paste(levels(unique(dat$weekday)), 0),
                     labels = levels(unique(dat$weekday)),
                     limits = lvls) +
    scale_y_continuous(limits = c(0, max(weekly_posts$dist))) +
    my_theme
  )
dev.off()
}

### repeat last frame
for (i in 1:250) {
  if (i %% 10 == 0)
    print(i)
  
  png(filename = paste("./img/tmp/", sprintf("%04d", i + length(lvls)), ".png", sep = ""),
      width = 1000, height = 300)
  print(
    ggplot(weekly_posts[weekly_posts$weekday_hr %in% lvls[1:length(lvls)],],
           aes(x = weekday_hr, y = dist)) +
      labs(
        title = "Aggregated Forum Activity of McGill Cycling from 2006-present",
           y  = "Posts",
           x  = "Time"
        ) + 
      geom_line(aes(group = g), size = 2) +
      scale_x_discrete(breaks = paste(levels(unique(dat$weekday)), 0),
                       labels = levels(unique(dat$weekday)),
                       limits = lvls) +
      scale_y_continuous(limits = c(0, max(weekly_posts$dist))) +
      my_theme
  )
  dev.off()
}

### save one copy of the last frame ###
pdf(file = paste0("./img/intraweek.pdf"), width = 10, height = 3)
print(
  ggplot(weekly_posts[weekly_posts$weekday_hr %in% lvls[1:length(lvls)],],
         aes(x = weekday_hr, y = dist)) +
    labs(
      title = "Aggregated Forum Activity of McGill Cycling from 2006-present",
      y     = "Posts",
      x     = "Time") + 
    geom_line(aes(group = g), size = 2) +
    scale_x_discrete(breaks = paste(levels(unique(dat$weekday)), 0),
                     labels = levels(unique(dat$weekday)),
                     limits = lvls) +
    scale_y_continuous(limits = c(0, max(weekly_posts$dist))) +
    my_theme
)
dev.off()

### Stick together into an obnoxious gif
system("convert ./img/tmp/*.png -delay 3 -loop 0 ./gif/intraweek.gif")
file.remove(paste0("./img/tmp/", list.files(path = "./img/tmp/", pattern = ".png")))


##### dist of users throughout day #####
intraday <- as.data.frame(table(dat$Name, dat$hr))
names(intraday) <- c("name","hr","freq")

# get post counts by user
postcount <- as.data.frame(table(dat$Name))
# sort by post count
postcount <- postcount[order(-postcount$Freq),]
# get top n users with the most posts
n <- 36
top_n <- as.character(postcount$Var1[1:n])

# calculate the intraday post ratio by user
# rows are users, columns are hours of the day
# cells are the ratio of total posts by user
intraday <- ddply(intraday, .(name), function(df) {
  df$freq <- df$freq/sum(df$freq)
})

# relabel to hours of the day
names(intraday) <- c("name",0:23)

# reshape to long format for plotting
intraday2 <- melt(intraday)
names(intraday2) <- c("name","hr","dist")

# restructure usernames as factors
intraday$name <- factor(intraday$name, levels = postcount$Var1)

## FINALLY! Lets plot
pdf(file = "./img/intraday.pdf", height = 7, width = 8)
ggplot(intraday2[intraday2$name %in% top_n,], aes(x = hr,y = dist)) +
  labs(x = "Time of Day",y = "Post Distribution") + 
  geom_line(aes(group = 1), size = 1.5) +
  facet_wrap(~ name) +
  theme_bw() +
  theme(strip.text       = element_text(size = 11),
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position  = "none",
        axis.ticks       = element_blank(),
        strip.background = element_blank())
dev.off()


##### posts by month/day/hour of the day #####
# by day nb
dat_c <- as.data.frame(table(dat$hr,dat$day,dat$month))
names(dat_c) <- c("hr","day","month","Freq")

pdf(file = "./img/daily.pdf", height = 8.5, width = 8.5)
ggplot(dat_c, aes(x = day, y = as.factor(hr), fill = Freq)) +
  geom_tile() +
  facet_wrap( ~ month, ncol = 3) +
  xlab("Day") + 
  ylab("Hour") +
  scale_fill_gradientn("Total\nForum\nPosts",
                       limits = c(0, max(dat_c$Freq)),
                       colours = c("darkblue","cyan3","yellow","orange")) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 5), 
        axis.text.y      = element_text(size = 6), 
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.key       = element_blank(),
        axis.ticks       = element_blank(),
        strip.background = element_blank())
dev.off()

# by weekday name
dat_c <- as.data.frame(table(dat$hr,dat$weekday,dat$month))
names(dat_c) <- c("hr","weekday","month","Freq")

pdf(file = "./img/weekdaily.pdf", height = 8.5, width = 7)
ggplot(dat_c, aes(x = weekday, y = as.factor(hr), fill = Freq)) +
  geom_tile() +
  facet_wrap( ~ month, ncol = 3) +
  xlab("Weekday") + 
  ylab("Hour") + 
  scale_fill_gradientn("Total\nForum\nPosts",
                       limits = c(0, max(dat_c$Freq)),
                       colours = c("darkblue","cyan3","yellow","orange")) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 8), 
        axis.text.y      = element_text(size = 6),
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.key       = element_blank(),
        axis.ticks       = element_blank(),
        strip.background = element_blank())
dev.off()



##### some other analytics #####
# we first add postless dates to our data
dateposts_tmp <- table(dat$date)
dateposts <- as.numeric(dateposts_tmp)
names(dateposts) <- names(dateposts_tmp)

alldates <- seq(min(dat$date), max(dat$date), by = "day")
mydates <- as.Date(names(dateposts))

zerodates_idx <- which(!(alldates %in% mydates))
zerodates <- alldates[zerodates_idx]

zeroposts <- rep(0, length(zerodates))
names(zeroposts) <- zerodates

dateposts_all_tmp <- c(dateposts, zeroposts) 
dateposts_all <- dateposts_all_tmp[order(names(dateposts_all_tmp))]
dates_all <- names(dateposts_all)
length(dates_all)

# fix axes so years line up with the first day of the year
jan1st_idx <- which(lubridate::month(dates_all) == 1 & lubridate::day(dates_all) == 1)

pdf("./img/all_posts.pdf", width = 8.5, height = 3.5)
p <- barplot(dateposts_all,
             main = "Number of Posts",
             xlab = "Date",
             ylab = "Posts", 
             xaxt = "n")
axis(side   = 1, 
     at     = p[jan1st_idx],
     labels = year(dates_all[jan1st_idx]), tick = T)
dev.off()

pdf("./img/all_posts_cumul.pdf", width = 8.5, height = 3.5)
p <- barplot(cumsum(dateposts_all),
             main = "Cumulative Posts",
             xlab = "Date",
             ylab = "Posts", 
             xaxt = "n")
axis(side   = 1, 
     at     = p[jan1st_idx],
     labels = year(dates_all[jan1st_idx]), tick = T)
dev.off()

pdf("./img/all_posts_sqrtcumul.pdf", width = 8.5, height = 3.5)
p <- barplot(sqrt(cumsum(dateposts_all)),
             main = expression("Cumulative Posts"),
             xlab = "Date",
             ylab = expression(sqrt("Posts")), 
             xaxt = "n")
axis(side   = 1, 
     at     = p[jan1st_idx],
     labels = year(dates_all[jan1st_idx]), tick = T)
dev.off()

### MONTHS
pdf("./img/month_posts.pdf", width = 8, height = 4.5)
barplot(table(dat$month),
        main = "Number of Posts: Monthly Aggregate",
        xlab = "Month",
        ylab = "Posts")
dev.off()

posts_mo <- table(format(dat$date, "%Y-%m"))
dateposts_mo <- as.Date(paste0(names(posts_mo), "-01"))
jan1st_idx_mo <- which(lubridate::month(dateposts_mo) == 1,
                       lubridate::day(dateposts_mo)   == 1)
pdf("./img/monthly_posts.pdf", width = 8, height = 4.5)
p <- barplot(posts_mo,
             main = "Number of Posts: Monthly",
             xlab = "Date",
             ylab = "Posts",
             xaxt = "n")
axis(side = 1, 
     at = p[jan1st_idx_mo],
     year(dateposts_mo)[jan1st_idx_mo])
dev.off()

pdf("./img/monthly_posts_cumul.pdf", width = 8, height = 4.5)
p <- barplot(cumsum(posts_mo),
             main = "Cumulative Number of Posts: Monthly",
             xlab = "Date",
             ylab = "Posts",
             xaxt = "n")
axis(side = 1, 
     at = p[jan1st_idx_mo],
     year(dateposts_mo)[jan1st_idx_mo])
dev.off()

pdf("./img/monthly_posts_diff.pdf", width = 8, height = 4.5)
p <- barplot(diff(posts_mo),
             main = "Month-Over-Month Number of Posts",
             xlab = "Date",
             ylab = "MOM Posts",
             xaxt = "n")
axis(side = 1, 
     at = p[jan1st_idx_mo],
     year(dateposts_mo)[jan1st_idx_mo])
dev.off()

pdf("./img/monthly_log10posts.pdf", width = 8, height = 4.5)
p <- barplot(log10(posts_mo),
             main = "Number of Posts: Monthly",
             xlab = "Date",
             ylab = "Log10(Posts)",
             xaxt = "n")
axis(side = 1, 
     at = p[jan1st_idx_mo],
     year(dateposts_mo)[jan1st_idx_mo])
dev.off()


pdf("./img/yearly_posts.pdf", width = 8, height = 4.5)
barplot(table(format(dat$date, "%Y")),
        main = "Number of Posts: Yearly",
        xlab = "Year",
        ylab = "Posts")
dev.off()

pdf("./img/yearly_posts_diff.pdf", width = 8, height = 4.5)
barplot(diff(table(format(dat$date, "%Y"))), 
        main = "Number of Posts: Year Over Year",
        xlab = "Year",
        ylab = "YOY Posts")
dev.off()

## cumsum post count
pst_cnt <- table(dat$Name)
srtd_pst_cnt <- pst_cnt[order(-pst_cnt)]
top_n <- names(srtd_pst_cnt[1:8])

all_posts <- as.data.frame(table(dat$date, dat$Name))
names(all_posts) <- c("date", "name", "freq")
top_users <- all_posts[all_posts$name %in% top_n,]

cols <- rainbow(length(top_n))

plot(cumsum(top_users$freq[top_users$name == top_n[1]]),
     type = "l", lwd = 3, col = cols[1],
     main = "The Race to 2000", xlab = "Days", 
     ylab = "Cumulative Posts", ylim = c(0,2250))

legend("topleft", lty = 1, legend = top_n, col = cols, 
       bty = "n", seg.len = 1/2, lwd = 2)
for (i in 2:length(top_n)) {
  lines(cumsum(top_users$freq[top_users$name == top_n[i]]),
        type = "l", lwd = 3, col = cols[i])
}
abline(h = 2000, lty = 3, lwd = 3)










