##=== load some libraries ===##

library(plyr)
library(ggplot2)
library(reshape2)
#library(animation)

##=== load data ===##

#setwd("D:/Dropbox/r/mct/plots")
setwd("~/projects/mct_forum_posts/plots")

url <- "http://files.j-vk.com/timestamps.dat"
dat <- read.csv(url)
str(dat) # what format is this data in
head(dat) # look at the first few entries

##=== clean & organize data ===##

dat$date_time <- as.POSIXct(dat$Time, origin = "1970-01-01")
dat$date <- as.Date(dat$date_time)
dat$month <- factor(format(dat$date, "%b"), levels = month.abb)
dat$weekday <- factor(weekdays(dat$date, abbreviate=TRUE),
                      levels = c("Sun","Mon","Tue","Wed",
                          "Thu","Fri","Sat"))
dat$day <- format(dat$date, '%d')
dat$time <- strftime(dat$date_time, format = "%H:%M")
dat$hr <- as.POSIXlt(dat$date_time)$hour
dat$min <- as.POSIXlt(dat$date_time)$min

# sort posts by date
dat <- ddply(dat,.(Name),function(df) {df[order(df$Time),]} )


# post_lag <- dlply(dat,.(Name),function(df) 
# {difftime(tail(df$date_time,-1),head(df$date_time,-1),units="secs")} 
# )
# head(post_lag)
# post_lag <- lapply(post_lag,as.numeric)
# post_lag <- lapply(post_lag,function(v){c(0,v)})
# dat$post_lag_sec <- unlist(post_lag)

# get weekday and hour of day of each post
dat$weekday_hr <- paste(dat$weekday, dat$hr,sep = " ")
# count number of occurences over unique values
weekly_posts <- as.data.frame(table(dat$weekday_hr))
names(weekly_posts) <- c("weekday_hr", "Freq")

# get each day/hour combination for labelling
weekday_hr_labs <- paste(rep(levels(unique(dat$weekday)),each = 24), 0:23)
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
  theme(axis.text.x=element_text(face="italic"),
        axis.text.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(colour="firebrick2"),
        legend.key=element_blank(),
        axis.ticks=element_blank(),
        strip.background=element_blank(),
        plot.title=element_text(lineheight=.8, face="bold"))

### Create animated plot
for (i in 1:length(lvls)) {
  png(filename = paste(sprintf("%04d",i), ".png", sep = ""),
      width = 1000, height = 400)
  print (
    
  ggplot(weekly_posts[weekly_posts$weekday_hr %in% lvls[1:i],],
         aes(x = weekday_hr,y = dist)) +
    labs(title = "Aggregated Forum Activity of McGill Cycling from 2006-present",
         y = "Proportion of Activity",
         x = "Time") + 
    geom_line(aes(group = g)) +
    scale_x_discrete(breaks = paste(levels(unique(dat$weekday)), 0),
                     labels = levels(unique(dat$weekday)),
                     limits = lvls) +
    scale_y_continuous(limits = c(0, max(weekly_posts$dist))) +
    my_theme
  )
dev.off()
}
### repeat last frame
for (i in 1:300) {
  png(filename = paste(sprintf("%04d", i + length(lvls)), ".png", sep = ""),
      width = 1000, height = 400)
  print (
    
    ggplot(weekly_posts[weekly_posts$weekday_hr %in% lvls[1:length(lvls)],],
           aes(x = weekday_hr, y = dist)) +
      labs(title = "Aggregated Forum Activity of McGill Cycling from 2006-present",
           y = "Proportion of Activity",
           x = "Time") + 
      geom_line(aes(group = g)) +
      scale_x_discrete(breaks = paste(levels(unique(dat$weekday)), 0),
                       labels = levels(unique(dat$weekday)),
                       limits = lvls) +
      scale_y_continuous(limits = c(0, max(weekly_posts$dist))) +
      my_theme
  )
  dev.off()
}
### Stick together into a gif
#shell("\"C:\\Program Files\\ImageMagick\\convert.exe\" *.png -delay 3 -loop 0 mct_posts_weekd.gif")
#shell("\"C:\\Program Files\\ImageMagick\\convert.exe\" -delay 3 mct_posts_weekdayhr.gif mct_week.gif")

# doesn't work? I've yet to find a solution while on a mac
system("convert *.png -delay 3 -loop 0 mct_posts_week.gif")
#file.remove(list.files(pattern = ".png"))



###==== dist of users throughout day ====###
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
intraday2 <- melt(tmp)
names(intraday2) <- c("name","hr","dist")
# restructure usernames as factors
intraday$name <- factor(intraday$name, levels = postcount$Var1)

## FINALLY! Lets plot
ggplot(intraday2[intraday2$name %in% top_n,], aes(x = hr,y = dist)) +
  labs(x = "Time of Day",y = "Post Distribution") + 
  geom_line(aes(group = 1)) +
  facet_wrap(~ name) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        strip.background = element_blank())



##=== posts by month/day/hour of the day ===##

# by day nb
dat_c <- as.data.frame(table(dat$hr,dat$day,dat$month))
names(dat_c) <- c("hr","day","month","Freq")

ggplot(dat_c, aes(x = day, y = as.factor(hr), fill = Freq)) +
  geom_tile() +
  facet_wrap( ~ month, ncol = 3) +
  scale_fill_gradientn("Total\nForum\nPosts",
                       limits = c(0, max(dat_c$Freq)),
                       colours = c("darkblue","cyan3","yellow","orange")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank())


# by weekday name
dat_c <- as.data.frame(table(dat$hr,dat$weekday,dat$month))
names(dat_c) <- c("hr","weekday","month","Freq")

ggplot(dat_c, aes(x = weekday, y = as.factor(hr), fill = Freq)) +
  geom_tile() +
  facet_wrap( ~ month, ncol = 3) +
  scale_fill_gradientn("Total\nForum\nPosts",
                       limits = c(0, max(dat_c$Freq)),
                       colours = c("darkblue","cyan3","yellow","orange")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank())




###==== some other analytics ====###


plot(table(dat$date))


## cumsum post count
pst_cnt <- table(dat$Name)
srtd_pst_cnt <- pst_cnt[order(-pst_cnt)]
top_n <- names(srtd_pst_cnt[1:6])

all_posts <- as.data.frame(table(dat$date, dat$Name))
names(all_posts) <- c("date", "name", "freq")
top_users <- all_posts[all_posts$name %in% top_n,]

cols <- rainbow(length(top_n))

plot(cumsum(top_users$freq[top_users$name == top_n[1]]),
     type = "l", lwd = 3, col = cols[1],
     main = "The Race to 2000", xlab = "Days", 
     ylab = "Cumulative Posts", ylim = c(0,2250))

legend("topleft", lty = 1, legend = top_n, col = cols, 
       bty = "n", seg.len = 1/2)
for (i in 2:length(top_n)) {
  lines(cumsum(top_users$freq[top_users$name == top_n[i]]),
        type = "l", lwd = 3, col = cols[i])
}
abline(h = 2000, lty = 3, lwd = 3)


str(tmp2$Freq[tmp2$Name %in% top_n])







