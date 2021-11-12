# different methods of calculating unique beneficiaries
# ultimately, act_test2 was selected as it actually did what it was supposed to do
# code is preserved here for reference

"`r format(Sys.Date(), format='%d %B, %Y')`"

act_test <- act2[0:100,]

act_test %>% 
  group_by(ubicacion, desagregacion) %>% 
  filter(rank(beneficiarios, ties.method = "max") == 1) %>% 
  ungroup() %>% 
  select(mes_reportado, nombre_del_proyecto, organizacion_implementadora, 
         estado, codigo_estado, municipio, codigo_municipio,
         parroquia, codigo_parroquia, ubicacion, latitud, longitud,
         actividad_codigo, actividad_desc, act_covid,
         desagregacion, beneficiarios) %>% 
  view()

act_test1 <- act_test %>% 
  group_by(ubicacion, desagregacion) %>% 
  filter(recurrente_beneficiarios == FALSE) %>% 
  filter(beneficiarios == max(beneficiarios)) %>% 
  ungroup() %>% 
  select(mes_reportado, nombre_del_proyecto, organizacion_implementadora, 
         estado, codigo_estado, municipio, codigo_municipio,
         parroquia, codigo_parroquia, ubicacion, latitud, longitud,
         actividad_codigo, actividad_desc, act_covid,
         desagregacion, beneficiarios) 

act_test2 <- act_test %>% 
  group_by(ubicacion, desagregacion) %>% 
  slice(which.max(beneficiarios)) %>% 
  ungroup() %>% 
  select(mes_reportado, nombre_del_proyecto, organizacion_implementadora, 
         estado, codigo_estado, municipio, codigo_municipio,
         parroquia, codigo_parroquia, ubicacion, latitud, longitud,
         actividad_codigo, actividad_desc, act_covid,
         desagregacion, beneficiarios) 

sum(act_test1$beneficiarios)
sum(act_test2$beneficiarios)

# this code was rejected as the data entry was not as expected:
# recurrent beneficiaries were FALSE for the first entry and TRUE for all 
# subsequent entries. I had thought that they would all be marked TRUE. 
# make a new dataset for beneficiary frequencies
recur_t <- act2 %>% 
  filter(recurrente_beneficiarios == TRUE) %>% 
  group_by(ubicacion, actividad_full, desagregacion) %>% 
  filter(beneficiarios == max(beneficiarios)) %>% 
  ungroup() %>% 
  select(mes_reportado, nombre_del_proyecto, organizacion_implementadora, 
         estado, codigo_estado, municipio, codigo_municipio,
         parroquia, codigo_parroquia, ubicacion, latitud, longitud,
         actividad_codigo, actividad_desc, act_covid,
         desagregacion, beneficiarios)

# you have fixed this problem 
act_ben %>% 
  filter(actividad_codigo == "CA1.05") %>% 
  arrange(desc(beneficiarios)) %>% head()

# pander default table alignment
panderOptions('table.alignment.default',
              function(df) ifelse(sapply(df, is.numeric), 'right', 'left'))

# for filling in missing values
df1 %>% 
  left_join(lookup_df, by = "state_abbrev") %>% 
  mutate(state_name = coalesce(state_name.x, state_name.y)) %>% 
  select(-state_name.x, -state_name.y)

# OR -- computationally faster, but I don't understand the package
library(data.table)
setDT(df1)[setDT(lookup_df), on = "state_abbrev", state_name := i.state_name]
df1

#OR -- one line solution; both datasets must have the same variable names
# not workable for 5W, requires unique values
df1 %>% 
  rows_update(lookup_df, by = "state_abbrev")

# does not work; still don't know why I can't set coltypes in read_csv
act_ben_coltypes <- c("date", "date", "text", "text", 
                      "text", "text", "text", "text", "text", "text",
                      "text", "numeric", "numeric",
                      "text", "text", "logical",
                      "text", "numeric")

u_ben_coltypes <- c("date", "date", "text", "text", "text", "text", "text", "text", 
                    "text", "numeric", "numeric",
                    "text", "numeric")

# function to format number
formatdecimal <- function(x, k) {
  return(format(round(as.numeric(x), k), nsmall=k))
}

# for seeing matching pairs (or non-matching ones)
unique(act_ben[c("actividad_codigo", "actividad_desc")]) %>% 
  arrange(actividad_codigo)

# figure out a better way to find new rows
# I think an anti join works
locations_add <- act1 %>% 
  select(estado, pcode1, municipio, pcode2, parroquia, pcode3, ubicacion) %>% 
  distinct() %>% 
  anti_join(locations, by = "ubicacion")

# you don't really need this anymore
locations <- read_excel("locations_20191111_1600.xlsx", sheet = "uniqueben") %>% 
  clean_names() %>% 
  remove_empty() %>%
  select(estado, pcode1, municipio, pcode2, parroquia, pcode3, 
         comunidad_o_centro, id) %>% 
  mutate(ubicacion = comunidad_o_centro) %>% 
  select(-c(comunidad_o_centro, id)) %>% 
  mutate(estado    = rm_accent(str_to_upper(estado)),
         municipio = rm_accent(str_to_upper(municipio)),
         parroquia = rm_accent(str_to_upper(parroquia)),
         ubicacion = rm_accent(str_to_upper(ubicacion)))

# bind_rows() does not care about column sequence 
  relocate(estado,    .after = organizacion_implementadora) %>% 
  relocate(pcode1,    .after = estado) %>% 
  relocate(municipio, .after = pcode1) %>% 
  relocate(pcode2,    .after = municipio) %>% 
  relocate(parroquia, .after = pcode2) %>% 
  relocate(pcode3,    .after = parroquia) %>% 
  
# checking totals
act2 %>% mutate(total = select(., m_0_3:m_mayores_de_19) %>% rowSums(na.rm = TRUE)) %>% 
    summarise(sum = sum(total)) 
act1 %>% mutate(total = select(., m_0_3:m_mayores_de_19) %>% rowSums(na.rm = TRUE)) %>% 
    summarise(sum = sum(total))

# you're trying to use plotly
mun_points <- all_mun %>% 
  ggplot(aes(x = estado, y = beneficiarios)) +
  geom_point(aes(size = beneficiarios)) +
  scale_y_continuous(trans = "log10", 
                     labels = comma) +
  geom_text(aes(municipio)) +
  xlab("") + ylab("Beneficiarios Alcanzados por Municipio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

ggplotly(mun_points, tooltip = c("text", "y"))

# plotly code
mun_points <- all_mun %>% 
  plot_ly(
    type = "scatter",
    mode = "markers",
    x = ~estado,
    y = ~beneficiarios,
    marker = list(size = ~beneficiarios, sizeref = 200, sizemode = "area", colour = "black"),
    text = ~municipio,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "%{xaxis.title.text}: %{x}<br>",
      "%{yaxis.title.text}: %{y:,.Of}<br>",
      "<extra></extra>"
    )
  )

mun_points <- layout(mun_points, yaxis = list(type = "log"),
                     xaxis = list(title = ""))

mun_points

# we are retiring patchwork in favour of plotly
# this uses patchwork to put both plots side by side             
mun_ben_plot + mun_pop_plot + plot_layout(ncol = 2)

# scale_fill_gradient with reversed log10
scale_fill_gradient(trans = trans_reverser("log10"), labels = comma, na.value = "gray90")

# change of plan, we will retire plotly because it cannot show two legends side by side
mun_ben_plot <- pcode2_shape %>% 
  mutate(pcode2 = ADM2_PCODE) %>% 
  left_join(., all_mun %>% select(municipio, pcode2, beneficiarios)) %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = beneficiarios, text = paste(municipio, pcode2, beneficiarios, sep = "\n"))) +
  geom_sf(size = 0.005) +
  theme_void() +
  ggtitle("Number of beneficiaries reached by municipality") +
  scale_fill_gradient(trans = trans_reverser('log10'), labels = comma, na.value = "gray90") +
  theme(legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.9, 0.1),
        legend.title = element_blank(), 
        plot.title = element_text(size = 12))


mun_pop_plot <- cen_ref %>% 
  select(pcode2, ham_2019_xx_poblacion_pobre_por_parroquia) %>% 
  group_by(pcode2) %>% 
  summarise(poor_pop = sum(ham_2019_xx_poblacion_pobre_por_parroquia)) %>% 
  right_join(., all_mun %>% select(municipio, pcode2, beneficiarios)) %>%
  mutate(percent_reached = beneficiarios/poor_pop*100) %>% 
  select(municipio, pcode2, percent_reached) %>% 
  right_join(pcode2_shape %>% mutate(pcode2 = ADM2_PCODE)) %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = percent_reached, 
             text = paste(municipio, pcode2, round(percent_reached),sep = "\n"))) +
  geom_sf(size = 0.005) +
  theme_void() +
  ggtitle("Percentage of poor persons reached by municipality") +
  scale_fill_gradient(labels = comma, trans = "reverse", na.value = "gray90")+
  theme(legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.9, 0.1),
        legend.title = element_blank(), 
        plot.title = element_text(size = 12))

# maybe you do have to learn shiny after all 
mun_ben_plot <- ggplotly(mun_ben_plot, tooltip = c("text"))
mun_pop_plot <- ggplotly(mun_pop_plot, tooltip = c("text"))


subplot(mun_ben_plot, mun_pop_plot)

# you can add this back in when you figure out plotly
geom_text(aes(y = beneficiarios, label = scales::comma(beneficiarios)), 
          size = 2, position = position_stack(vjust = -0.5)) +

  
# for seeing matching pairs (or non-matching ones)
unique(act_ben2[c("actividad_codigo", "actividad_desc")]) %>% 
  arrange(actividad_codigo)

unique(act_ben2[c("actividad_short", "actividad_desc")]) %>% 
  arrange(actividad_short)

# for mapping pcodes to latlong
geom_sf(data = st_transform(fsc3, 32614)) + # you need to remember this

  
# I think it's just better to have percentage of total below the beneficiaries reached, 
# but I'm lazy now
# REF-for-state-text
mira_dc <- u_ben2 %>% filter(estado == "MIRANDA" | 
                               estado == "DISTRITO CAPITAL")

Beneficiaries in Miranda and Distrito Capital account for  
__`r round((sum(mira_dc$beneficiarios)) / (sum(u_ben2$beneficiarios)) * 100)`%__ of the total of 
__`r format(round(sum(u_ben2$beneficiarios)), big.mark = ",")`__. 