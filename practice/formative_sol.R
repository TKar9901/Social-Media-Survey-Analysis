# MTH1004 Solutions to Formative exercise for poster coursework

## 1. Download and read

library(tidyverse)
gas = read_csv('ukgas.csv')
gas

## 2. Calculate median gas consumption to find the highest

gas %>% summarise(m1 = median(Qtr1), 
                  m2 = median(Qtr2),
                  m3 = median(Qtr3),
                  m4 = median(Qtr4))


## 3. Compare gas consumption between 1975 and 1970

gas %>% 
  filter(Year == 1970 | Year == 1975)

# (calculated differences from the output)

## 4. Save time series plot

plt = ggplot(gas) + 
  geom_line(aes(x=Year, y=Qtr1, colour='Quarter 1')) +
  geom_line(aes(x=Year, y=Qtr2, colour='Quarter 2')) +
  geom_line(aes(x=Year, y=Qtr3, colour='Quarter 3')) +
  geom_line(aes(x=Year, y=Qtr4, colour='Quarter 4')) +
  labs(x=NULL, y='UK gas consumption [mio. therms]', colour=NULL)
plt

ggsave('ukgas.png', plt, width=5, height=3, dpi=720)





