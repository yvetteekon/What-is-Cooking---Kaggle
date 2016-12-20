# load libraries
packages <- c("graph", "Rgraphviz", "ggplot2", "dplyr", "wordcloud")
lapply(packages, require, character.only = T)

# load data
load("dtms.RData")
load("df.RData")

# term frequency
findFreqTerms(dtms, lowfreq = 10000)

# plot term frequency
head(df, 25)

pdf("Term Frequency.pdf")
subset(df, freq > 10000) %>% ggplot(aes(x = words, y = freq)) +
  geom_bar(stat = "identity", fill = "blue", color = "red", alpha = 0.5, width =  0.8) +
  labs(title = "Ingredients That Occur At Least 10000 Times", x = "Ingredients",
       y = "Count") + theme(plot.title = element_text(lineheight = 0.8, face = "bold",
                                                      hjust = 0.5)) + coord_flip()
dev.off()

# term association
findAssocs(dtms, c("pepper", "salt"), c(0.3, 0.1))

# correlation plot
pdf("Term Association.pdf")
plot(dtms, terms = findFreqTerms(dtms, lowfreq = 10000), corThreshold = 0.2, weighting = T)
dev.off()

# world cloud
pdf("Word Cloud.pdf")
color1 <- brewer.pal(8, "Set1")
color2 <- brewer.pal(8, "Dark2")
color3 <- rainbow(8)

set.seed(123)
wordcloud(df$words, df$freq, min.freq = 10000, rot.per = 0.3, random.order = F,
          color = color1)

set.seed(456)
wordcloud(df$words, df$freq, min.freq = 2500, rot.per = 0.3, random.order = F,
          color = color2)

set.seed(789) # plot 5000 most used ingredients
wordcloud(df$words, df$freq, max.words = 5000, scale = c(6, 0.1), rot.per = 0.3,
          random.order = F, color = color3)
dev.off()
