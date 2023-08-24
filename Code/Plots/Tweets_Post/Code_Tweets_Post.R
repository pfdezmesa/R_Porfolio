```{r Tweets and Post}

cuentas_unificadas %>% 
  group_by(institucion, red_social) %>% 
  mutate(media = mean(publicaciones)) %>%
  mutate(institucion = case_when(institucion == "local" ~ "Adm. local",
                                 institucion == "autonomico" ~ "Adm. autonómica",
                                 institucion == "comunicacion" ~ "Emp. Comunicación",
                                 institucion == "empresas" ~ "Empresa",
                                 institucion == "bancos" ~ "E. Financiera",
                                 institucion == "estatal" ~ "Adm. Gob. estatal",
                                 institucion == "organizaciones empresariales" ~ "Org. empresarial",
                                 institucion == "ong" ~ "Org. social",
                                 institucion == "partidos politicos" ~ "Partido Político",
                                 institucion == "sindicatos" ~ "Sindicato",
                                 institucion == "universidad" ~ "Universidad"),
         red_social = case_when(red_social == "facebook" ~ "Facebook",
                                red_social == "twitter" ~ "Twitter")) %>%
  ggplot(aes(x= institucion, y= media, fill = red_social)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = round(media, 1)), hjust= -0.2, position = position_dodge(width = 0.9), color = "black", size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, by = 100),
                     position = "right")+
  scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
  scale_fill_manual(values = c("#3b5998", "#00acee")) +
  labs(x = NULL, 
       y = NULL,
       title = "Media de Tweets y Post",
       subtitle = "Datos correspondientes a publicaciones entre el 15.08.2022 y el 15.10.2022",
       caption = "Fuente: Datos recogidos desde Twitter y Facebook",
       fill = NULL) +
  theme(plot.title = element_text(face= "bold",
                                  size = 12),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(face = "italic",
                                    size = 9),
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill="lightblue", 
                                         size=0.5, linetype="solid"))

ggsave(filename, last_plot(), width = 9.02, height = 5.76, dpi = 300)

```
