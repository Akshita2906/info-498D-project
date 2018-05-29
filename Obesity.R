
obesity_df = read.csv("data/obesity_state.csv")
obesity_df$fips = as.numeric(obesity_df$x.state)
state_table$fips = as.numeric(state_table$value)
obesity_df$x.rfbmi5 = round(obesity_df$x.rfbmi5 * 100,0)

obesity_df = obesity_df[obesity_df$x.race.g1==1 ,]
obesity_df <- mutate(obesity_df,type = case_when( x.rfbmi5 > 65~1, x.rfbmi5 <= 65~0))

viz_df<- left_join(obesity_df,state_table, by = "fips")

library(ggplot2)
theme_set(theme_bw())

# Plot
ggplot(viz_df, aes(x = state, y = x.rfbmi5 , label=x.rfbmi5)) + 
  geom_point(stat='identity', aes(col=type), size=8)  +
  scale_fill_gradient(low="#238b45", high="#00441b") +
  geom_text(color="white", size=3, fontface = "bold") +
  labs(title="BMI Levels Across States in the United States of America") + 
  coord_flip()

