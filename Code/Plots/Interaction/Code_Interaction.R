```{r Tweets and Post + Interaction}

plot <- cuentas_unificadas %>%
  group_by(institucion, red_social) %>%
  summarise(publicaciones = mean(publicaciones),
            likes = mean(like_publi),
            compartidos = mean(compartido_publi))%>%
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
                                red_social == "twitter" ~ "Twitter"))

plot1 <- plot[, c("institucion", "red_social", "publicaciones")]

plot2 <- plot[, c("institucion", "red_social", "likes", "compartidos")] %>%
  as.data.frame() %>%
  melt(id = c("institucion", "red_social")) %>%
  mutate(variable = case_when((red_social == "Facebook" & variable == "likes") ~ "Likes (Facebook)",
                              (red_social == "Facebook" & variable == "compartidos") ~ "Compartidos (Facebook)",
                              (red_social == "Twitter" & variable == "likes") ~ "Favoritos (Twitter)",
                              (red_social == "Twitter" & variable == "compartidos") ~ "RTweets (Twitter)")) %>%
  drop_na(variable)

ggplot() + 
  geom_bar(data = plot1, aes(x = institucion, y = publicaciones, fill = red_social), position="dodge", stat="identity") +
  scale_fill_manual(values = c("#3b5998", "#00acee")) +
  geom_line(data = plot2, aes(x = institucion, y = value/0.25, group = variable, col = variable), size= 0.8) +
  scale_color_manual(values = c("#E69F00","#CC79A7","#009E73","#F0E442")) +
  scale_y_continuous(limits = c(0, 650),
                     breaks = seq(0, 650, by = 100),
                     name = NULL,
                     sec.axis = sec_axis(trans = ~.*0.25, 
                                         name= NULL,
                                         breaks = seq(0, 163, by = 25)))+
  scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
  labs(x = NULL,
       title = "Media de publicaciones y reacciones al contenido por cada publicación",
       subtitle = "Datos correspondientes a publicaciones entre el 15.08.2022 y el 15.10.2022",
       caption = "Fuente: Datos recogidos desde Twitter y Facebook",
       fill = NULL,
       color= NULL) +
  theme(plot.title = element_text(face= "bold",
                                  size = 12),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(face = "italic",
                                    size = 9),
        axis.text.x = element_text(angle = 20,
                                   size = 9))

ggsave(fillname, last_plot(), width = 9.02, height = 5.76, dpi = 300)

```
