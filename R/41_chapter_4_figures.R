#### Chapter 4 FIGURES ####################################################

### Figure 9 - Active and reserved listings since 2018 ####################################################


ggplot()+
  geom_line(data = daily %>% 
              filter(housing, date >= "2017-12-29", status == "R") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status), alpha = 0.4)+
  geom_smooth(data = daily %>% 
                filter(housing, date >= "2017-12-29", status == "R") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F)+
  geom_line(data = daily %>% 
              filter(housing, date >= "2017-12-29", status == "A") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status), alpha = 0.4)+
  geom_smooth(data = daily %>% 
                filter(housing, date >= "2017-12-29", status == "A") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F)+
  ggtitle("Reserved and available listings (Frollmean 7)")


### Figure 10 - Active and reserved listings since 2018 ####################################################

ggplot()+
  geom_line(data = daily %>% 
              filter(housing, date >= "2019-12-29", status == "R") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status))+
  stat_smooth(geom='line', data = daily %>% 
                filter(housing, date >= "2019-12-29", status == "R") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F, alpha = 0.5)+
  geom_line(data = daily %>% 
              filter(housing, date >= "2019-12-29", status == "A") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status))+
  stat_smooth(geom='line', data = daily %>% 
                filter(housing, date >= "2019-12-29", status == "A") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F, alpha = 0.5)+
  ggtitle("Reserved and available listings (2020, Frollmean 7)")

