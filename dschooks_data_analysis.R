packages <- c("plotly","roll","RcppRoll","dataRetrieval","data.table","hydroTSM","tidyverse","chron","lubridate","ggpubr")

# Check to see if each is installed, and install if not.
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

# now, use the lapply function to load the installed libraries in the packages list
lapply(packages,library,character.only=TRUE)

ddata_read <- fread("DData.csv")

head(ddata_read)

# Observe how data were read in by fread
str(ddata_read)

# format date times, convert Well to factor, add date, yday, and year

ddata <- ddata_read %>% 
  mutate(DateTime = lubridate::mdy_hm(DateTime), # add your time zone argument, tz = 
         Well = as.factor(Well),
         date = date(DateTime),
         hour = hour(DateTime),
         yday = yday(date),
         year = year(date)) %>% 
  group_by(Well,year)

summary(ddata$hour)

head(ddata)

# QAQC tip - view the unique second, minute, and hours that are in your dataset.
#  Is this as you expected?
unique(second(ddata$DateTime))
sort(unique(minute(ddata$DateTime)))
sort(unique(hour(ddata$DateTime)))

# How many years with data?
sort(unique((ddata$year)))

# how many wells?
str(ddata$Well)

# Plot scatter plot of DateTimes vs DTM by year with Well as series
pdf("plot.pdf",width=10,height=16)
ggplot(ddata) +
  geom_point(aes(x = DateTime,y = DTW,color = Well),size = 0.5)+
  ylab("DTW")+
  xlab("DateTime")+
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        # strip.text.x = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        text = element_text(size = 24),
        # plot.margin=unit(c(1,1,1,1),"cm"), #trbl
        # panel.spacing = unit(3, "lines"),
        legend.position = "top",
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  facet_wrap(~year ,scales = "free",ncol = 1)
dev.off()

# Gruop by well, date, and hour, get mean DTW for each group
# and create a synthetic datetime with the year, month, day, and hour
ddata_hr <- ddata %>% 
  group_by(Well,date,hour) %>% 
  summarise(DTW_hr = mean(DTW)) %>% 
  mutate(year = year(date)) %>% 
  mutate(DateTime = make_datetime(year,month(date),day(date),hour))

head(ddata_hr)
View(ddata_hr)
# Plot scatter plot of DateTimes vs DTM by year with Well as series
pdf("plot_hourly.pdf",width=10,height=16)
ggplot(ddata_hr) +
  geom_point(aes(x = DateTime,y = DTW_hr,color = Well),size = 0.5)+
  ylab("DTW (mean hourly)")+
  xlab("DateTime")+
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        # strip.text.x = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        text = element_text(size = 24),
        # plot.margin=unit(c(1,1,1,1),"cm"), #trbl
        # panel.spacing = unit(3, "lines"),
        legend.position = "top",
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  facet_wrap(~year ,scales = "free",ncol = 1)
dev.off()

# Summarise daily and plot
head(ddata)
ddata_day <- ddata %>%
  group_by(Well,date) %>% 
  summarise(DTW_day = mean(DTW)) %>% 
  mutate(year = year(date),
         yday = yday(date))
head(ddata_day)

pdf("plot_daily.pdf",width=10,height=16)
ggplot(ddata_day) +
  geom_point(aes(x = date,y = DTW_day,color = Well),size = 0.5)+
  ylab("DTW (mean daily)")+
  xlab("Date")+
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        # strip.text.x = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        text = element_text(size = 24),
        # plot.margin=unit(c(1,1,1,1),"cm"), #trbl
        # panel.spacing = unit(3, "lines"),
        legend.position = "top",
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  facet_wrap(~year ,scales = "free",ncol = 1)
dev.off()


# Look at one site
ddata_day_i <- ddata_day %>% 
  filter(Well == "BD161")

head(ddata_day_i)

ggplot(ddata_day_i) +
  geom_point(aes(x = yday,y = DTW_day,color = as.factor(year)),size = 0.5)+
  ylab("DTW (mean daily)")+
  xlab("Date")+
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        # strip.text.x = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        text = element_text(size = 24),
        # plot.margin=unit(c(1,1,1,1),"cm"), #trbl
        # panel.spacing = unit(3, "lines"),
        legend.position = "top",
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

# Do this for every well in a loop
i = 10
plot_list <- list()
for(i in 1:length(unique(ddata$Well))){
  WellTitle <- paste0("Well_",unique(ddata$Well)[i])
  
  df_i <- ddata_day %>% 
    filter(Well == unique(ddata$Well)[i])
  
  plot_i <- ggplot(df_i) +
    geom_line(aes(x = as.Date(yday-1, "1970-01-01"),
                  y = DTW_day,color = as.factor(year)),size = 1)+
    # geom_point(aes(x = as.Date(yday-1, "1970-01-01"),y = DTW_day,color = as.factor(year)),size = 1)+
    scale_x_date(date_breaks="months", date_labels="%b")+
    ggtitle(WellTitle) +
    ylab("DTW (mean daily)")+
    xlab("YDay")+
    theme(panel.background = element_rect(fill = "white"),
          strip.background = element_blank(),
          # strip.text.x = element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          text = element_text(size = 24),
          # plot.margin=unit(c(1,1,1,1),"cm"), #trbl
          # panel.spacing = unit(3, "lines"),
          legend.position = "right",
          axis.line = element_line(size = 0.5, linetype = "solid",
                                   colour = "black"))
  plot_list[[i]] <- plot_i
  jpeg(filename = paste0(WellTitle,".jpeg"),width = 12,height = 5,
       res = 150,units = "in")
  print(plot_i)
  dev.off()
}



