#Please note, this code assumes that the webforum.csv file is in the working directory

#gathering data
rm(list = ls())
set.seed(31486347) # StudentID
webforum <- read.csv("webforum.csv")
webforum <- webforum [sample(nrow(webforum), 20000), ] # 20000 rows

#import all needed libraries
library(ggplot2)
library(dplyr)
library(igraph)
library(igraphdata)
library(gridExtra)

#Renaming row names
webforum[1, ]
webforum["1952",]

rownames(webforum) = NULL

#INVESTIGATING AUTHOR ID -1
author_neg_1 = webforum %>% filter(AuthorID == -1)
View(author_neg_1)

#comparing means for different categories
diff = (colMeans(author_neg_1[5:22]) - colMeans(webforum[5:22])) / colMeans(webforum[5:22]) * 100
diff

#comparing average number of posts vs author -1

#calculate average number of posts per author excluding author -1
author_count = webforum %>% count(AuthorID)
mean(author_count[-1, 2])

#get number of posts by author -1
dim(author_neg_1)[1]


##QUESTION a1

#convert date column to Date type
webforum$Date = as.Date(webforum$Date, "%Y-%m-%d")

#add columns for month and year onto our dataset. Since converting to date reuqires the day, we just add the first day
webforum$yearmonth = format(webforum$Date, "%Y-%m-01")


#count number of posts for each group of month,Year
num_posts = webforum %>%
  group_by(yearmonth) %>%
  summarise(count_posts = n())

#convert yearmonth column to date data type
num_posts$yearmonth = as.Date(num_posts$yearmonth, "%Y-%m-%d")
View(num_posts)

#use ggplot2 to plot the timeseries data
graph = ggplot(num_posts, aes(x=yearmonth, y=count_posts)) + 
  geom_line(size=1.1)+
  xlab("")+
  ylab("Number of Posts")+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y")+
  theme(axis.text.x=element_text(angle=40, hjust=1)) 

graph


##QUESTION a2
#calculate the mean for each linguistic variable for each year,month time period
linguistic_means = aggregate(webforum[, 5:23], list(webforum$yearmonth), mean)

names(linguistic_means)[1] = "yearmonth"

#convert yearmonth column to date data type
linguistic_means$yearmonth = as.Date(linguistic_means$yearmonth, "%Y-%m-%d")

#use ggplot2 to plot the timeseries data

#WordCount
graph = ggplot(linguistic_means, aes(x=yearmonth)) + 
  geom_line(aes(y = WC, color = "WC"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ylim(0, 250)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y")+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black"))
graph




#Analytic, Clout, Authentic, Tone
graph = ggplot(linguistic_means, aes(x=yearmonth)) + 
  geom_line(aes(y = Analytic, color = "Analytic"), size=1.1)+
  geom_line(aes(y = Clout, color = "Clout"), size=1.1)+
  geom_line(aes(y = Authentic, color = "Authentic"), size=1.1)+
  geom_line(aes(y = Tone, color = "Tone"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ylim(0, 80)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y")+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black", "slateblue3", "mediumturquoise", "goldenrod3"))

graph




#ppron, i, we
graph = ggplot(linguistic_means, aes(x=yearmonth)) + 
  geom_line(aes(y = ppron, color = "ppron"), size=1.1)+
  geom_line(aes(y = i, color = "i"), size=1.1)+
  geom_line(aes(y = we, color = "we"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ylim(0, 15)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y")+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black", "mediumturquoise", "goldenrod3"))

graph


#you, shehe, they
graph = ggplot(linguistic_means, aes(x=yearmonth)) + 
  geom_line(aes(y = you, color = "you"), size=1.1)+
  geom_line(aes(y = shehe, color = "shehe"), size=1.1)+
  geom_line(aes(y = they, color = "they"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ylim(0, 4)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y")+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black", "mediumturquoise", "goldenrod3"))

graph



#posemo, negeemo
graph = ggplot(linguistic_means, aes(x=yearmonth)) + 
  geom_line(aes(y = posemo, color = "posemo"), size=1.1)+
  geom_line(aes(y = negemo, color = "negemo"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ylim(0, 15)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y")+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black","mediumturquoise"))

graph


#anx, anger, sad
graph = ggplot(linguistic_means, aes(x=yearmonth)) + 
  geom_line(aes(y = anx, color = "anx"), size=1.1)+
  geom_line(aes(y = anger, color = "anger"), size=1.1)+
  geom_line(aes(y = sad, color = "sad"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ylim(0, 3)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y")+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black","mediumturquoise", "goldenrod3"))

graph



#focuspast, focuspresent, focusfuture
graph = ggplot(linguistic_means, aes(x=yearmonth)) + 
  geom_line(aes(y = focuspast, color = "focuspast"), size=1.1)+
  geom_line(aes(y = focuspresent, color = "focuspresent"), size=1.1)+
  geom_line(aes(y = focusfuture, color = "focusfuture"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ylim(0, 15)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y")+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black","mediumturquoise", "goldenrod3"))

graph


#QUESTION b1
#calculate number of posts for each thread and choose top 2 threads to compare against the whole forum
thread_count = webforum %>% count(ThreadID)
thread_count <- thread_count[order(-thread_count$n),]
head(thread_count)


#get dataframe containing mean posemo, negemo, anger, sad, anx for the whole forum
happy_means = aggregate(webforum[, 16:20], list(webforum$yearmonth), mean)

names(happy_means)[1] = "yearmonth"

#convert yearmonth column to date data type
happy_means$yearmonth = as.Date(happy_means$yearmonth, "%Y-%m-%d")
View(happy_means)

#calculate the same statistics for thread 127115 and thread 283958
#as these threads have the most posts over a large date range so they are more likely to be a better representation
#of the threads

thread_1 = webforum %>% filter(ThreadID == "127115")
thread_1 = aggregate(thread_1[, 16:20], list(thread_1$yearmonth), mean)
names(thread_1)[1] = "yearmonth"
thread_1$yearmonth = as.Date(thread_1$yearmonth, "%Y-%m-%d")
View(thread_1)

thread_2 = webforum %>% filter(ThreadID == "283958")
thread_2 = aggregate(thread_2[, 16:20], list(thread_2$yearmonth), mean)
names(thread_2)[1] = "yearmonth"
thread_2$yearmonth = as.Date(thread_2$yearmonth, "%Y-%m-%d")
View(thread_2)

#plot the variables relating to happiness for the two threads and the forum mean

#posemo
graph1 = ggplot() + 
  geom_line(data=happy_means, aes(x=yearmonth, y=posemo, color = "Forum"), size=1.1)+
  geom_line(data=thread_1, aes(x=yearmonth, y=posemo, color = "Thread 127115"), size=1.1)+
  geom_line(data=thread_2, aes(x=yearmonth, y=posemo, color = "Thread 283958"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ggtitle("Positive Emotions")+
  ylim(0, 10)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y", limits=as.Date(c('2006-07-01','2008-01-01')))+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black","mediumturquoise", "goldenrod3"))
graph1

#negemo
graph2 = ggplot() + 
  geom_line(data=happy_means, aes(x=yearmonth, y=negemo, color = "Forum"), size=1.1)+
  geom_line(data=thread_1, aes(x=yearmonth, y=negemo, color = "Thread 127115"), size=1.1)+
  geom_line(data=thread_2, aes(x=yearmonth, y=negemo, color = "Thread 283958"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ggtitle("Negative Emotions")+
  ylim(0, 10)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y", limits=as.Date(c('2006-07-01','2008-01-01')))+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black","mediumturquoise", "goldenrod3"))
graph2

#anger
graph3 = ggplot() + 
  geom_line(data=happy_means, aes(x=yearmonth, y=anger, color = "Forum"), size=1.1)+
  geom_line(data=thread_1, aes(x=yearmonth, y=anger, color = "Thread 127115"), size=1.1)+
  geom_line(data=thread_2, aes(x=yearmonth, y=anger, color = "Thread 283958"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ggtitle("Anger")+
  ylim(0, 10)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y", limits=as.Date(c('2006-07-01','2008-01-01')))+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black","mediumturquoise", "goldenrod3"))
graph3

#sad
graph4 = ggplot() + 
  geom_line(data=happy_means, aes(x=yearmonth, y=sad, color = "Forum"), size=1.1)+
  geom_line(data=thread_1, aes(x=yearmonth, y=sad, color = "Thread 127115"), size=1.1)+
  geom_line(data=thread_2, aes(x=yearmonth, y=sad, color = "Thread 283958"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ggtitle("Sad")+
  ylim(0, 3)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y", limits=as.Date(c('2006-07-01','2008-01-01')))+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black","mediumturquoise", "goldenrod3"))
graph4

#anxiety
graph5 = ggplot() + 
  geom_line(data=happy_means, aes(x=yearmonth, y=anx, color = "Forum"), size=1.1)+
  geom_line(data=thread_1, aes(x=yearmonth, y=anx, color = "Thread 127115"), size=1.1)+
  geom_line(data=thread_2, aes(x=yearmonth, y=anx, color = "Thread 283958"), size=1.1)+
  xlab("")+
  ylab("Mean")+
  ggtitle("Anxious")+
  ylim(0, 3)+
  scale_x_date(date_break = "6 month", date_labels = "%b %Y", limits=as.Date(c('2006-07-01','2008-01-01')))+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  scale_color_manual(values=c("black","mediumturquoise", "goldenrod3"))
graph5




grid.arrange(graph1, graph2, graph3, graph4, graph5)



#using ttest() to determine if the threads have more positive emotions than the forum
t.test(thread_1$negemo, happy_means$negemo, alternative='greater')[3]
t.test(thread_2$negemo, happy_means$negemo, alternative='greater')[3]
t.test(thread_1$negemo, thread_2$negemo, alternative='greater')[3]

#QUESTION c1
#get 30 authors that have the most posts, skip author id -1
author_top30 = webforum %>% count(AuthorID)
author_top30 = author_top30[order(-author_top30$n),]
author_top30 = author_top30[2:31, ]$AuthorID


#filter data to contain top30 authors during the most active month
network_data = webforum %>%
  filter(AuthorID %in% author_top30)

#get most active month period
toptime = network_data %>%
  count(yearmonth)

toptime = toptime[order(-toptime$n), ]
toptime = toptime[1, 1]


network_data = network_data %>%
  filter(yearmonth == toptime)

network_data = network_data[, 1:2]

#date being used = 2005-12-01


#creating network of authors
#this graph creation was referenced from https://rpubs.com/pjmurphy/317838
g = graph.data.frame(network_data,directed = FALSE)
V(g)$type <- bipartite_mapping(g)$type

bipartite_matrix <- as_incidence_matrix(g)

#Calculate AuthorID adjacency matrix
author_network <- t(bipartite_matrix) %*% bipartite_matrix
diag(author_network) <- 0

#plot network graph
author_network <- graph_from_adjacency_matrix(author_network, 
                                              mode = "undirected", 
                                              weighted = TRUE)


#customise network
V(author_network)$color <- "salmon"
V(author_network)$shape <- "circle"
E(author_network)$color <- "lightgray"
V(author_network)$label.color <- "black"
V(author_network)$label.cex <- 0.6
V(author_network)$frame.color <-  "gray"
V(author_network)$size <- 15

plot(author_network, layout = layout_with_graphopt)

#QUESTION c2
#Identifying most important author based on closeness, betweenness
head(closeness(author_network)[order(closeness(author_network), decreasing = T)], 5)

head(betweenness(author_network)[order(betweenness(author_network), decreasing = T)], 5)

head(evcent(author_network)$vector[order(evcent(author_network)$vector, decreasing = T)], 10)

#most important author is AuthorID 62481
V(author_network)
V(author_network)[5]$color = "yellow"
plot(author_network, layout = layout_with_graphopt)

#getting language used by other others and our important autho
other_authors = webforum %>%
  filter(AuthorID %in% author_top30)

other_authors = other_authors %>%
  filter(yearmonth == toptime)



imp_author = other_authors %>%
  filter(AuthorID == '62481')

other_authors = other_authors %>%
  filter(AuthorID != '62481')



#calculate the mean for each linguistic variable for each year,month time period
imp_author = colMeans(imp_author[, 5:23])
other_authors = colMeans(other_authors[, 5:23])

(imp_author-other_authors)/other_authors * 100

