library(tidyverse)
library(ggrepel)
library(ggthemes)
library(ggsci)
library(patchwork)
library(RColorBrewer)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astronauts <- astronauts %>% mutate(occupation = recode(occupation, `Other (Space tourist)` = "Space tourist",
                                          `Flight engineer` = "flight engineer", 
                                          `Pilot` = "pilot"))

names_ordered <- str_split(astronauts$name, ", ", 2, simplify=T)
names_ordered <- as.data.frame(names_ordered) %>% mutate(name_full = paste(V2,V1)) %>% select(name_full)
astronauts_tidy <- astronauts %>% bind_cols(names_ordered) %>% mutate(decade = case_when(
  year_of_mission >=1960 & year_of_mission <1970 ~ "1960s",
  year_of_mission >=1970 & year_of_mission <1980 ~ "1970s",
  year_of_mission >=1980 & year_of_mission <1990 ~ "1980s",
  year_of_mission >=1990 & year_of_mission <2000 ~ "1990s",
  year_of_mission >=2000 & year_of_mission <2010 ~ "2000s",
  year_of_mission >=2010 & year_of_mission <2020 ~ "2010s"
)
                                                                 )
missions <- astronauts %>% group_by(ascend_shuttle, year_of_mission) %>%
  summarise(astronauts=n(), hours = mean(hours_mission)) %>% arrange(year_of_mission)

astro_sum <- astronauts_tidy %>% filter(total_number_of_missions >= 3) %>%
  group_by(name_full, ascend_shuttle, sex,nationality, occupation, year_of_mission, hours_mission, total_hrs_sum, decade) %>% 
  arrange(desc(total_hrs_sum)) %>% group_by(name) %>% mutate(journey=row_number()) %>% ungroup() %>%
  mutate(counter=1)

cols_space <- brewer.pal(10, name="Spectral")

astr_plot <- function(x) {
  ggplot(astro_sum %>% filter(decade==x), aes(y=counter, x=reorder(name_full, journey), fill=factor(year_of_mission), label=ascend_shuttle)) + 
  geom_bar(stat="identity") + 
  coord_polar() +
  theme_dark() +
  theme(legend.position="none",
        plot.background=element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        text=element_text(colour="white", family="Avenir"),
        plot.title=element_text(family="Impact", size=30),
        axis.title.y=element_text(colour="white", size=12),
        axis.text=element_text(colour="white", family="Avenir", size=6)) +
    scale_fill_manual(values=cols_space) +
  labs(title=x, x="", y="Total missions")
}

p1 <- astr_plot("1960s")
p2 <- astr_plot("1970s")
p3 <- astr_plot("1980s")
p4 <- astr_plot("1990s")
p5 <- astr_plot("2000s")
p6 <- astr_plot("2010s")

p1+p2+p3+p4+p5+p6 +plot_annotation(
  title = 'Many small steps',
  subtitle = 'Astronauts with three or more space missions by decade',
  caption = 'Source: Mariya Stavnichuk and Tatsuya Corlett. Position in decade - Red=0 orange=3 yellow=5, green=7, purple=9'
) & theme(plot.title=element_text(family="Avenir", size=35, colour="white"),
          plot.subtitle = element_text(family="Avenir", size=20, colour="white"),
          plot.caption=element_text(family="Avenir", size=12, colour="white"),
          plot.background = element_rect(fill="black", colour="black")) &
  plot_layout(widths=10, heights=10)
ggsave("astro.png", dpi="retina")
