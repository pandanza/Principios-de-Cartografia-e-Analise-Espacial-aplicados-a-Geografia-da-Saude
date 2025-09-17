#### ATIVIDADE! CALCULAR:
# taxa padronizada para mortalidade infantil neonatal;
# calcule a taxa padronizada para mortalidade infantil pós-neonatal.
# coloque seu nome em Elaborado por: XXXXX.

names(dados_combinados_order)
dados_combinados_order_posneo <- dados_combinados_order |>
  filter(`faixa etária`== "posneo")

E_posneo <- expected(
  population = dados_combinados_order_posneo$pop,
  cases = dados_combinados_order_posneo$obitos, n.strata = 2
)

d_combinado_posneo <- group_by(dados_combinados_order_posneo, distrito_admin_residencia) |> 
  summarise(y=sum(obitos))

d_combinado_posneo$E_posneo <- E_posneo[match(d_combinado_posneo$distrito_admin_residencia,
                                     unique(dados_combinados_order$distrito_admin_residencia))]

d_combinado_posneo$SMR <- d_combinado_posneo$y / d_combinado_posneo$E_posneo

mapa_posneo <- merge(shp, d_combinado_posneo)

plot_mapa_posnatal <- ggplot(mapa_posneo) + 
  geom_sf(aes(fill = SMR)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "grey", high = "red"
  ) +
  ggtitle("Taxa de Mortalidade Infantil Pós-Neonatal Padronizada - \n de 2010 a 2019, São Paulo") +
  labs(caption = "Fonte: Sistema de Informações sobre Mortalidade\n – SIM/PRO-AIM – CEInfo –SMS-SP |\n Elaborado por: Daniela. \n Disciplina: Princípios de Cartografia e Análise Espacial aplicados à Geografia da Saúde (2025)") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.2, 
                              line_width = 0.5, height = unit(0.1,"cm"))
theme_bw()
plot_mapa_posnatal

ggsave("./Mapa padronizado da mortalidade infantil pós-neonatal.jpg", dpi=300, width = 25, height = 15, units = "cm")
