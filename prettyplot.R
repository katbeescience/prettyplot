# This function queries the user about their two data file names for comparison.
# The code formats the input data, then filters it to include only data where
# the setpoint was 115 or 135.
# Then it creates four line graphs:
#    1) Time and humidity from the first file over the time span of the program.
#    2) Time and humidity from the second file over the time span of the program.
#    3) Time and humidity from both files compared.
#    4) The difference between temperature and humidity in the two files.
# It saves the output plot as a file on the user's computer.

# ==============================================================================

# Function pretty.plot can be executed to generate the plots.

pretty.plot <- function () {
  
# Load up packages.

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)

# Communicate with the user.
  
cat("To get started, please make sure your data file is already saved on your Desktop.\n
    It should follow the usual format output by the machine program. \n
    You don't need to tell me the file ending--I'll just assume it's .dat.")
title1 <- readline(prompt="Type a filename to compare (example: 1234-5678-91011): \n")
filename1 <- file.path("~","Desktop", paste0(title1,".dat"))

while (!file.exists(filename1)) {
  cat("\nTry again. You suck.\n\n")
  title1 <- readline(prompt="Type a filename to compare (example: 1234-5678-91011.dat): \n")
  filename1 <- file.path("~","Desktop",paste0(title1,".dat"))
}

title2 <- readline(prompt="Type a filename to compare (example: 1234-5678-91011): \n")
filename2 <- file.path("~","Desktop",paste0(title2,".dat"))

while (!file.exists(filename2)) {
  cat("\nTry again. You suck.\n\n")
  title2 <- readline(prompt="Type your next filename to compare (example: 1234-5678-91011.dat): \n")
  filename2 <- file.path("~","Desktop",paste0(title2,".dat"))
}  

cat(paste0("Thanks! Your plot will show up on the screen momentarily, and will also automatically save on your Desktop. \n
           Look for something called plot_", title1, "_", title2, ".png."))

# Interpret data:

all_content1 = gsub(":", ",", readLines(filename1))
skip_second1 = all_content1[-1]
data1 = read.delim(textConnection(skip_second1), header = TRUE, stringsAsFactors = FALSE, sep=",")

all_content2 = gsub(":", ",", readLines(filename2))
skip_second2 = all_content2[-1]
data2 = read.delim(textConnection(skip_second2), header = TRUE, stringsAsFactors = FALSE, sep=",")

# Clean it up.
# Change the names of the columns so you can tell which file they both came from.

data1 <- data1 %>%
  select(Hour, Min, Sec, TmpPV, TmpSV, HumPV) %>%
  dplyr::rename(Setpoint1=TmpSV, Temp1=TmpPV, Humidity1=HumPV) %>%
  filter(Setpoint1 == "135" | Setpoint1 == "115") %>%
  mutate(Time1=hms(paste(Hour,Min,Sec))) %>%
  select(Temp1, Setpoint1, Humidity1, Time1)

data1 <- add_column(data1, Row = c(1:nrow(data1)))

data2 <- data2 %>%
  select(Hour, Min, Sec, TmpPV, TmpSV, HumPV) %>%
  dplyr::rename(Setpoint2=TmpSV, Temp2=TmpPV, Humidity2=HumPV) %>%
  filter(Setpoint2 == "135" | Setpoint2 == "115") %>%
  mutate(Time2=hms(paste(Hour,Min,Sec))) %>%
  select(Temp2, Setpoint2, Humidity2, Time2)

data2 <- add_column(data2, Row = c(1:nrow(data2))) 

# Combine the two data frames and add columns that calculate the difference between the two temperatures and humidities:

whole.data <- merge(data1, data2, by="Row")
whole.data$Temp.Diff <- abs(whole.data$Temp1-whole.data$Temp2)
whole.data$Hum.Diff <- abs(whole.data$Humidity1-whole.data$Humidity2)

# Make pretty plots comparing the two columns in four different ways.

compare.plot1 <- ggplot(data=data1, mapping=aes(x=Row)) +
  geom_line(aes(y=Temp1, color="Temperature")) +
  geom_line(aes(y=Humidity1, color="Humidity")) +
  annotate(geom="text",label=paste0("Setpoint = ", data1$Setpoint1[4]), x=(nrow(data1)/2), y=140) +
  scale_color_manual(values=c("red","blue"), name="") +
  labs(title=paste0("Temperature and humidity for ",title1), x="Run time", y=element_blank()) + 
  scale_y_continuous(breaks=seq(0,140, by=20), limits=c(0,140)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

compare.plot2 <- ggplot(data=data2, mapping=aes(x=Row)) +
  geom_line(aes(y=Temp2, color="Temperature")) +
  geom_line(aes(y=Humidity2, color="Humidity")) +
  annotate(geom="text",label=paste0("Setpoint = ", data2$Setpoint2[4]), x=(nrow(data2)/2), y=140) +
  scale_color_manual(values=c("green","orange"), name="") +
  labs(title=paste0("Temperature and humidity for ",title2), x="Run time", y=element_blank()) + 
  scale_x_time() +
  scale_y_continuous(breaks=seq(0,140, by=20), limits=c(0,140)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

labels <- c(paste0("Humidity of ", filename1), paste0("Humidity of ", filename2),
            paste0("Temperature of ",filename1), paste0("Temperature of ", filename2))
all.four.plot <- ggplot(data=whole.data, mapping=aes(x=Row)) +
  geom_line(aes(y=Temp1, color="Temperature 1")) +
  geom_line(aes(y=Humidity1, color="Humidity 1")) +
  geom_line(aes(y=Temp2, color="Temperature 2")) +
  geom_line(aes(y=Humidity2, color="Humidity 2")) +
  annotate(geom="text",label=paste0("Setpoint 1 = ", data1$Setpoint1[4],
                                    ",   Setpoint 2 = ", data2$Setpoint2[4]), x=(nrow(whole.data)/2), y=140) +
  scale_color_manual(values=c("red", "blue", "green","orange"), name="") +
  labs(title=paste0("Temperatures and Humidities of \n", title1, ", and ", title2), x="Run time", y=element_blank()) + 
  scale_x_time() +
  scale_y_continuous(breaks=seq(0,140, by=20), limits=c(0,140)) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

difference.plot <- ggplot(data=whole.data, mapping=aes(x=Row)) +
  geom_line(aes(y=Temp.Diff, color="Temperature Difference")) +
  geom_line(aes(y=Hum.Diff, color = "Humidity Difference")) +
  annotate(geom="text",label=paste0("Setpoint 1 = ", data1$Setpoint1[4],
                                    ",   Setpoint 2 = ", data2$Setpoint2[4]), x=(nrow(whole.data)/2),
           y=max(c(whole.data$Temp.Diff,whole.data$Hum.Diff))) +
  scale_color_manual(values=c("violet", "black"), name="") +
  labs(title=paste0("Differences between ", "\n", title1, ", and ", title2),
       x="Run time", y=element_blank()) +
  scale_x_time() +
  scale_y_continuous(breaks=seq(0,max(c(whole.data$Temp.Diff,whole.data$Hum.Diff)), by=2),
                     limits=c(0,max(c(whole.data$Temp.Diff,whole.data$Hum.Diff)))) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

four.plots <- grid.arrange(compare.plot1, compare.plot2, all.four.plot, difference.plot)
four.plots

ggsave(filename=(paste0("~/Desktop/plot_", title1, "_", title2, ".png")), plot=four.plots, height=9, width=12, units="in")

}

# Do the function using their given answer:

pretty.plot()
