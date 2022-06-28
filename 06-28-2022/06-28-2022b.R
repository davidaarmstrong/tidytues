library(rvest)
h <- read_html("https://resources.companieshouse.gov.uk/sic/")

tabs <- html_table(h)
shead <- grep("Section", tabs[[1]]$Code)
shead <- c(shead, nrow(tabs[[1]]))
sec_name <- tabs[[1]][shead, "Description"]
snames <- vector(mode="list", length=length(shead)-1)
for(i in 1:length(snames)){
  snames[[i]] <- tabs[[1]]$Code[(shead[i]+1):(shead[(i+1)]-1)]
}

paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')
paygap$sector <- NA
for(i in 1:length(snames)){
  paygap$sector <- ifelse(paygap$sic_codes %in% snames[[i]], 
                          i, paygap$sector)
}
paygap <- paygap %>% 
  mutate(sector= factor(sector, levels=1:21, labels=sec_name$Description))

paygap <- paygap %>% 
  mutate(sector2 = forcats::fct_lump_min(sector, min = 500))


library(ggridges)
paygap %>% 
  filter(!is.na(sector2)) %>% 
  mutate(diff_mean_hourly_percent = case_when(
    diff_mean_hourly_percent > 50 ~ 50, 
    diff_mean_hourly_percent < 0 ~ 0, 
    TRUE ~ diff_mean_hourly_percent
  ), 
  sector2 = reorder(factor(sector2), diff_mean_hourly_percent, mean)) %>% 
ggplot(aes(x=diff_mean_hourly_percent, y=sector2)) + 
  geom_density_ridges(scale=.9) + 
  stat_summary(fun.data=mean_cl_normal, fatten=.25) + 
  labs(x="Average Pay Differential Men vs Women (%)", 
       y="") + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        plot.title = element_text(hjust=.5)) + 
  ggtitle("Average Pay Differential by Employment Sector")
ggsave("uk_dists.png", height=5, width=8, units="in", dpi=300)




