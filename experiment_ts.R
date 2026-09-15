library(fpp2)
library(dplyr)
data(ausbeer)

head(ausbeer)
ausbeer

class(ausbeer)

time(ausbeer) # returns the time index

cycle(ausbeer) # where in the seasonal cycle is that cell?

frequency(ausbeer) # observations per period (columns per row)

start(ausbeer)
end(ausbeer)

tsp(ausbeer)

window(ausbeer, start=2005) # get a subset

methods(class="ts")

?lag


autoplot(ausbeer) + 
  ggtitle("Quarterly Beer Production in Australia") +
  xlab("Quarter") +
  ylab("Beer produced in Megalitres (ML)") +
  theme_minimal()


p1 <- window(ausbeer, end=1970) %>%
  ggseasonplot(year.labels=FALSE) +
  ggtitle("Quarterly production of beer by season") +
  ylab("Beer produced in Megalitres (ML)")



p2 <- window(ausbeer, start=1970) %>%
  ggseasonplot(year.labels=FALSE) +
  # ggtitle("Quarterly production of beer by season") +
  ylab("Beer produced in Megalitres (ML)")

library(cowplot)

plot_grid(p1, p2, ncol=2, labels=LETTERS[1:2]) + ggtitle("Quarterly production of beer by season")


library(dplyr)

ausbeer %>%
  group_by(year) %>%
  summarise(avg <- mean(value))


time(ausbeer)


df <- data.frame(
  year=floor(time(ausbeer)),
  beer=as.numeric(ausbeer)
)

df %>% 
  group_by(year) %>% 
  summarise(avg_production = mean(beer, na.rm=TRUE)) %>%
  ggplot() +
    geom_line(aes(x=year, y=avg_production))
