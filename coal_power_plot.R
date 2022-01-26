library(tidyverse)

data <- read_csv("coal_power_plot.csv") %>%
  mutate(scenario=factor(scenario,
                         levels=c("global_15c", "ref", "all_pledges", "15c_aligned"),
                         labels=c("Global 1.5°C budget", "Reference", "All pledges", "1.5°C aligned")
                         ),
         type=factor(type, levels=rev(c("Coal power", "Oil&gas power", "Other energy use"))))

data_segment <- data %>%
  filter(scenario %in% c("Reference","All pledges", "1.5°C aligned")) %>%
  mutate(
    x=as.numeric(scenario),
    value_prev=lag(value),
    x_prev=lag(x)) %>%
  filter(!is.na(value_prev))


ggplot(data, aes(x=scenario,
                 y=value)) +
  geom_bar(aes(fill=scenario),
           stat="identity",
           color="#666666",
           width=0.6,
           show.legend = F) +
  geom_text(aes(label=gsub("er en","er\nen", type), col=scenario),
            position = position_stack(vjust = 0.5),
            size=2.5,
            show.legend = F) +
  geom_segment(data = data_segment,
               color="#666666",
               aes(x = x_prev+0.3, xend = x - 0.3, y = value_prev, yend = value)) +
  theme_light() +
  theme(plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(color = "black")) +
  labs(x=NULL, y="Gt.",
       subtitle="Cumulative CO2 emissions from coal power by scenario (2020-2050)") +
  scale_fill_manual(values=c("white","darkred", "darkblue", "darkgreen")) +
  scale_color_manual(values=c("black","white", "white", "white")) +
  scale_y_continuous(expand = expansion(mult=c(0, 0.1)))

ggsave("coal_power_plot.jpg", width=6, heigh=4)

