
library(ggplot2)

# convex function
x = seq(-1,1, length = 100)
y = x^2

# nonconvex function
x = seq(-1*pi, 1*pi, length = 100)
y = sin(x) * 8 + cos(x*2) * 17

ggplot(data.frame(x, y), aes(x, y)) +
  #ylim(-0.1, 1) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank()) +
  xlab("x") +
  ylab("f(x)") +
  geom_line(size = 1.2, color = "hotpink") +  # Change line thickness and color
  theme(axis.text.x = element_blank(),  # Remove x-axis label
        axis.text.y = element_blank()) +
  geom_text(label = "Global Minimum", size = 5, x = -1.55, y = -15) +
  geom_text(label = "Local Minimum", size = 5, x = 1.55, y = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  annotate("segment", y = min(y), yend = -16, x = -1.56 , xend = -1.56, size = 1) +
  annotate("segment", y = -9, yend = 1, x = 1.57 , xend = 1.57, size = 1)
  





