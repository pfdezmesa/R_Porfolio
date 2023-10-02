```{r Boxplot. Followers}

cuentas_unificadas %>%
  drop_na(followers) %>%
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
  ggplot(aes(x= as.factor(institucion), y= followers/1000, colour= red_social)) + 
  geom_boxplot(fill = "#CCFFFF",
               alpha=0.2,
               outlier.colour="red",
               outlier.size=1) +
  scale_y_continuous(limits = c(0, 150),
                     breaks = seq(0, 150, by = 25)) +
  scale_colour_manual(values = c("#3b5998", "#00acee")) +
  labs(x = NULL, 
       y = NULL,
       title = "Followers (en miles) según el tipo de organismo o institución",
       subtitle = "Datos correspondientes al 17.11.2022",
       caption = "Fuente: Datos recogidos desde Twitter y Facebook
       
       Nota: Existen 31 valores atípicos que no aparecen representados en la gráfica por motivo de espacio.",
       colour = NULL) +
  theme(plot.title = element_text(face= "bold",
                                  size = 12),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(face = "italic",
                                    size = 10),
        axis.text.x = element_text(angle = 15,
                                   size = 9)) 

ggsave(fillname, last_plot(), width = 9.02, height = 5.76, dpi = 300)

```
