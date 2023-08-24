```{r Frequency}

frecuencias <- publicaciones_unificadas %>%
  group_by(cuenta, institucion, red_social, fecha) %>%
  summarise(n = n(), .groups = 'drop')%>%
  group_by(institucion, red_social, fecha) %>%
  mutate(N = sum(n))

n_institucion <- cuentas_unificadas %>%
  group_by(institucion, red_social) %>%
  mutate(n_institucion = length(cuenta)) %>%
  summarise("institucion" = institucion, "n_institucion" = n_institucion, .groups = 'drop') %>%
  distinct()

frecuencias <- merge(frecuencias, n_institucion, all.x = TRUE)

frecuencias %>%
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
  subset(institucion != "E. Financiera") %>%
  ggplot() +
  geom_line(aes(x=fecha, y=N/n_institucion, color=red_social), size=0.6)+
  scale_y_continuous(limits = c(0, 9),
                     breaks = seq(0, 9, by = 2),
                     name = NULL) +
  scale_colour_manual(values = c("#3b5998", "#00acee")) +
  theme_ipsum() +
  labs(x = NULL, 
       y = NULL,
       title = "Frecuencia media de Tweets y Post diarios",
       subtitle = "Datos correspondientes a los días desde el 15.08.2022 al 15.10.2022",
       caption = "Fuente: Datos recogidos desde Twitter y Facebook",
       color = NULL) +
  theme(plot.title = element_text(face= "bold",
                                  size = 12),
        strip.text.x = element_text(face = "italic",
                                    size = 10),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(face = "italic",
                                    size = 9),
        axis.text.x = element_text(angle = 0,
                                   size = 6),
        axis.text.y = element_text(size= 6),
        legend.title = element_text(face= "bold"),
        legend.position = c(0.6, 0.1)) +
  facet_wrap(~institucion)

ggsave(fillname, last_plot(), width = 9.02, height = 5.76, dpi = 300)

```
