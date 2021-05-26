if(!require(pacman)) install.packages("pacman")

pacman::p_load("rvest", "stringr", "purrr", "dplyr", "magrittr", "ggplot2",
               "forcats", "hrbrthemes")

base_url <- "https://dema.ufc.br/pt/professores/"

profs_url <- base_url %>% 
  read_html() %>%
  html_elements(".post-content li a") %>%
  html_attr("href")

get_research_field <- function(url) {
  url %>%
    read_html() %>%
    html_elements(".post-content p") %>%
    html_text2() %>%
    str_subset("Linha de Pesquisa") %>%
    str_split_n(":", 2) %>%
    str_squish()
}

df_research_field <- tibble(research_field =  map_chr(profs_url, get_research_field)) 

df_research_field %<>%
  group_by(research_field) %>%
  summarise(freq = n()) %>%
  filter(research_field != "") %>%
  mutate(research_field = fct_reorder(research_field, desc(freq)))

df_research_field %>%
  ggplot(aes(x = research_field, y = freq)) +
  geom_col(width = 0.5) +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(y = "Quantidade de professores",
       x = "Linha de pesquisa",
       title = "Linha de pesquisa dos professores do\nDepartamento de Matemática aplicada e Estatística (DEMA)",
       caption = "12 valores faltantes") +
  theme_ipsum()


  