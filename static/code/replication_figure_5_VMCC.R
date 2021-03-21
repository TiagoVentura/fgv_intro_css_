##### Visualização de gráficos ####


# Exemplo 1: Figura 5, Connectivity Efferverscence --------------------------------------------
library(tidyverse)
library(ggthemes)
library(conflicted)
library(lubridate)
library(ggrepel)
library(extrafont)
conflict_prefer("filter", "dplyr")
conflict_prefer("filter", "stats")


####  Abrir dois bancos de dados

# Dados com Comentários
d <- read_csv("comments_facebook_presidental_elections.csv")
d_for_tox <- d %>%
  rowid_to_column(var='unique_id') %>%
  drop_na(comments)

# Dados de Toxicicidade
toxicity <- read_csv("outputs_tox.csv")

# Tidy Data
toxicity <- toxicity %>%
  pivot_longer(cols=-c("text_id", "error"),
               names_to="variables",
               values_to="scores")
# Join

toxicity <- left_join(toxicity, d_for_tox, by=c("text_id"="unique_id"))



## Limpar dados de Toxicidade -------------------------------------------------------------------------------------

toxicity <- toxicity %>%
  mutate(label=str_replace_all(debate, "_", " "),
         label=str_to_title(label),
         label=str_trim(str_remove_all(label, "_|Manual"))) %>%
  mutate(label=str_replace_all(label, "Abc", "ABC"),
         label=str_replace_all(label, "Nbc", "NBC"),
         label=str_replace_all(label, "Fox", "FOX")) %>%
  arrange(label) %>%
  mutate(label=forcats::fct_inorder(label))  %>%
  mutate(Attribute=str_replace_all(str_to_title(variables), "_", " "))

# Remove Some Attributes
types_tox <- unique(toxicity$variables)

types_tox_red <-  c("TOXICITY", "SEVERE_TOXICITY",
                    "INSULT", "THREAT")

tox_red <- toxicity %>%
  dplyr::filter(variables%in%types_tox_red) %>%
  mutate(Attribute=fct_relevel(Attribute, "Toxicity", "Severe toxicity", "Threat", "Insult"))

## Encontre cOmentários com os Candidatos -------------------------------------------------------------------------------------

library(rebus)
library(tidytext)


# Expressões Regulares

biden<- c("bidden.*", "biden.*", "joe", "joseph")
harris <- c("kamal.*", "harris.*")
trump <- c("trump.*", "donald.*")
pence <- c("pence.*")
partisanship <- c("democ.*", "repub*.")

# rename
d_for_tox <- d_for_tox %>%
            rename("text"="comments")

# Mention candidates by condition
colnames(d_for_tox)

d_for_tox <- d_for_tox%>%
  mutate(text=str_squish(text),
         text=str_trim(text),
         text=str_to_lower(text))

# Detectando Comentários com os Candidatos.

tidy_tox_model<- d_for_tox %>%
                unnest_tokens(word, text) %>%
                mutate(biden_mention = str_detect(word,
                                    paste0(biden, collapse = "|")),
                       harris_mention=str_detect(word,
                                   paste0(harris, collapse="|")),
                       trump_mention=str_detect(word,
                                  paste0(trump, collapse = "|")),
                       pence_mention=str_detect(word,
                                  paste0(pence, collapse = "|")))



# Put the models back

tidy_numbers <- tidy_tox_model %>%
                  group_by(unique_id) %>%
                  summarise_if(is_logical,
                                list(sum=~sum(.x)))

# Merge with the rest of the data

d <- left_join(tox_red, tidy_numbers, by=c("text_id" = "unique_id"))


# Converte para Binario
d <- d %>% mutate_at(vars(contains("_sum")),
                     list(binary=~ifelse(.x > 0, 1, 0))) %>%
           mutate(scores_bin=ifelse(scores>.5, 1, 0),
                  n_char=stringi::stri_count_words(comments))



# Modelos ------------------------------------------------------------------
d_tox

model_sum_tox <- d %>%
  group_by(label) %>%
  nest() %>%
  mutate(model=map(data, ~ lm(scores_bin ~ biden_mention_sum_binary +
                                harris_mention_sum_binary +
                                trump_mention_sum_binary+
                                pence_mention_sum_binary + likes + n_char,
                              data=.x)),
         res=map(model, tidy)) %>%
  unnest(res) %>%
  mutate(lb=estimate - 1.96*std.error,
         up= estimate + 1.96*std.error,
         term=str_to_title(str_remove_all(term, "_mention_sum_binary"))) %>%
  dplyr::filter(!(term%in%c("(Intercept)", "Likes", "N_char")))




# Gráfico -----------------------------------------------------------------
pal <- RColorBrewer::brewer.pal(n=9, name="RdBu")

ggplot(model_sum_tox,
       aes(x=term, y=estimate, ymin=lb, ymax=up, fill=term))  +
  geom_pointrange(shape=21, size=1, alpha=.8) +
  coord_flip() +
  facet_wrap(~label) +
  labs(x="", y="Point Estimates",
       title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="red") +
  theme_minimal(base_size=14) +
  scale_fill_manual(values = c(pal[9], pal[7], pal[5], pal[2], pal[1])) +
  scale_color_manual(values = c(pal[9], pal[7], pal[5], pal[2], pal[1])) +
  guides(fill=NULL, color=NULL)

# Salvando.
ggsave(filename="figure_5.png",
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

