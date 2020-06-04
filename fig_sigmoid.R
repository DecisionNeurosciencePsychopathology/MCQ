
k <- (-100:-20)/10
df <- as_data_frame(x = c(k))
df$k <- df$value
df <- df %>% select(-value)
df$p <- 1/(1 + exp(-1*(df$k + 5)))
df$p1 <- 1/(1 + exp(-.1*(df$k + 5)))

df <- as_tibble(df)
df <- df %>% pivot_longer(-k)

ggplot(df, aes(k, value, lty = name)) + geom_line()
ggplot(df, aes(k, p1)) + geom_line()

car::Anova(m1)