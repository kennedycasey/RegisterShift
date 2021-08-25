library(tidyverse)

# get all utterances from providence corpus
cos_sim <- read.csv("data_prep/word2vec/cosine_similarity.csv")  %>%
  pivot_longer(c("overall", "younger", "older"), names_to = "age", values_to = "cosine_similarity") %>%
  mutate(age = factor(age, levels = c("overall", "younger", "older")))
                      
cos_sim_summary <- cos_sim %>%
  group_by(age) %>%
  summarize(mean = mean(cosine_similarity, na.rm = TRUE), 
            se = sd(cosine_similarity, na.rm = TRUE)/sqrt(length(cosine_similarity))) %>%
  mutate(age = factor(age, levels = c("overall", "younger", "older")))

ggplot() +
  geom_jitter(data = cos_sim, aes(x = age, y = cosine_similarity), 
             color = "#F2F2F2", size = 2.5, width = 0.01, alpha=0.8) +
  geom_pointrange(data = cos_sim_summary, aes(x = age, y = mean, ymin = mean-se, ymax = mean+se), 
                  size = 1) +
  geom_hline(yintercept = 0.2646741, linetype = "dashed") +
  labs(y = "Mean semantic similarity\nof CDL and ADL forms", title = "CHILDES") +
  ylim(0, 1) +
  scale_x_discrete(labels = c("Overall", "Younger children\n(<24m)", "Older children\n(24m+)")) +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank())
ggsave("figs/cos_sim.jpg", height = 5, width = 6, dpi = 300)