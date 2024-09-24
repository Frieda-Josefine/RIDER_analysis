
###Experiment 1

library(patchwork)

lines_data1 <- summary_stats_participant_image_pres %>%
  filter(image_presentation %in% c("Img1-Test1", "Img2-Test1"))

lines_data2 <- summary_stats_participant_image_pres %>%
  filter(image_presentation %in% c("Img1-Test2", "Img2-Test2"))

# Calculate means
mean_lines_data1 <- lines_data1 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy))

mean_lines_data2 <- lines_data2 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy))


gap_data2 <- data.frame(
  image_presentation = factor("Gap", levels = c(levels(summary_stats_participant_image_pres$image_presentation), "Gap")),
  participant = NA,  # Assuming type character or factor, adjust as necessary
  N = NA_integer_,   # Use NA_integer_ for integer columns
  accuracy = NA_real_,  # Use NA_real_ for numeric columns
  sd = NA_real_,  # Use NA_real_ for numeric columns
  se = NA_real_,  # Use NA_real_ for numeric columns
  ci = NA_real_  # Use NA_real_ for numeric columns
)


summary_stats_participant_image_pres <- rbind(summary_stats_participant_image_pres, gap_data2)


level_order12 = c('Baseline','Gap','Img1-Test1','Img2-Test1','Img1-Test2','Img2-Test2')

upper_labels <- c("One-Sample","", "Sample 1", "Sample 2", "Sample 1", "Sample 2")
lower_labels <- c( "", "WM Test 1", "WM Test 2")


WM_sample <- ggplot(data = summary_stats_participant_image_pres, aes(x = factor(image_presentation, level = level_order12), y = accuracy, fill = factor(image_presentation))) +
  geom_line(data = summary_stats_participant_image_pres, aes(group = participant, x = factor(image_presentation, level = level_order12), y = accuracy), color = "grey", size = 0.3, alpha = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 1.5, dodge.width = 0.5), alpha = 0.4, shape = 21, color = "black", size = 3) +
  geom_point(data = summary_study_level_image_pres, aes(x = image_presentation, y = accuracy, fill = factor(image_presentation)),  colour = "black", size = 4.4) +
  geom_errorbar(data = summary_study_level_image_pres, aes(x = image_presentation, ymin = accuracy - se, ymax = accuracy + se), width = 0.15, colour = "black", size = 0.8) +
  geom_line(data = mean_lines_data1, aes(x = factor(image_presentation, level = level_order12), y = mean_accuracy, group = 1), colour = "black", size = 2, linetype = "twodash") +
  geom_line(data = mean_lines_data2, aes(x = factor(image_presentation, level = level_order12), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash") +
  scale_x_discrete(labels = upper_labels) +
  scale_y_reverse(breaks = c(0, 30, 60, 90), labels = c("0°", "30°", "60°", "90°")) +  # Set specific y-axis ticks
  theme_minimal() +
  geom_hline(yintercept = 90, color = "darksalmon", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "darksalmon", linetype = "dashed", size = 1) +
  labs(
    title = "WM Performance",
    x = "\n                               WM Test 1               WM Test 2",
    y = "Error (°)\n"
  ) +
  geom_flat_violin(aes(x = factor(image_presentation), y = accuracy, fill = factor(image_presentation)), alpha = 0.3, position = position_nudge(x = 0), width = 1.5) +
  scale_fill_manual(values = c("bisque1",  "#e41658", "#3f8e8e",  "#e41658","#3f8e8e",  "#e41658", "#b41698"),  # Different orangy colors for each trial type
                    name = "Trial Type") +
  theme(
    plot.title = element_text(size = 26, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.background = element_rect(colour = "white", fill = NA, size = 1)
  )





# Add hierarchical labels
upper_grob <- textGrob(label = c("", "Sample 1", "Sample 2", "Sample 1", "Sample 2"), 
                       x = unit(seq(1, length(level_order12), length.out = length(level_order12)), "npc"), y = unit(1, "npc"), just = "center")
lower_grob <- textGrob(label = c("One-Sample", "", "WM Test 1", "WM Test 1", "WM Test 2", "WM Test 2"), 
                       x = unit(seq(1, length(level_order12), length.out = length(level_order12)), "npc"), y = unit(-1.5, "npc"), just = "center")

# Adding grobs to the plot
WM_sample + 
  annotation_custom(upper_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotation_custom(lower_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines")) # Ad



WM_sample <- WM_sample + 
  # Top horizontal line for the third bracket
  annotate("segment", x = 3.5, xend = 5.5, y = -4, yend = -4, colour = "black", size = 0.7) +
  # Corrected vertical lines for the third bracket
  annotate("segment", x = 3.5, xend = 3.5, y = -4, yend = -3, colour = "black", size = 0.7) +
  annotate("segment", x = 5.5, xend = 5.5, y = -4, yend = -3, colour = "black", size = 0.7) +
  # Star on top of the third bracket
  annotate("text", x = 4.5, y = -5, label = "***", size = 12, colour = "black")+
  # NS for primacy
  annotate("text", x = 3.5, y = 11, label = "n.s.", size = 8, colour = "black")+
  annotate("text", x = 5.5, y = 17, label = "***", size = 10, colour = "black")


WM_sample

#LTM

# Filter the data to include only the points you want to calculate means and connect with lines
# Filter the data to include only the points you want to calculate means and connect with lines
lines_data1 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-Test1", "Img2-Test1"))

lines_data2 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-Test2", "Img2-Test2"))

lines_data3 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-NT", "Img2-NT"))

# Calculate means
mean_lines_data1 <- lines_data1 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data2 <- lines_data2 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data3 <- lines_data3 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

scale_fill_manual(values = c("#86be91", "#68af90", "#509e90", "#3d8e8e", "#2a7d8c", "#1d6c8a", "#1f5985", "#27457c"),
                  name = "Trial Type")


gap_data3 <- data.frame(
  image_presentation = factor("Gap3", levels = c(levels(summary_stats_participant_image_pres$image_presentation), "Gap3")),
  participant = NA,  # Assuming type character or factor, adjust as necessary
  N = NA_integer_,   # Use NA_integer_ for integer columns
  accuracy_ltm = NA_real_,  # Use NA_real_ for numeric columns
  sd = NA_real_,  # Use NA_real_ for numeric columns
  se = NA_real_,  # Use NA_real_ for numeric columns
  ci = NA_real_  # Use NA_real_ for numeric columns
)

gap_data2 <- data.frame(
  image_presentation = factor("Gap2", levels = c(levels(summary_stats_participant_image_pres$image_presentation), "Gap2")),
  participant = NA,  # Assuming type character or factor, adjust as necessary
  N = NA_integer_,   # Use NA_integer_ for integer columns
  accuracy_ltm = NA_real_,  # Use NA_real_ for numeric columns
  sd = NA_real_,  # Use NA_real_ for numeric columns
  se = NA_real_,  # Use NA_real_ for numeric columns
  ci = NA_real_  # Use NA_real_ for numeric columns
)

summary_stats_participant_LTM_ip <- rbind(summary_stats_participant_LTM_ip, gap_data3)
summary_stats_participant_LTM_ip <- rbind(summary_stats_participant_LTM_ip, gap_data2)


level_order2 <- c('Baseline','Gap3','Img1-Test1', 'Img2-Test1', 'Img1-Test2','Img2-Test2','Gap2','Img1-NT','Img2-NT')

upper_labels <- c("One-Sample","", "Sample 1", "Sample 2", "Sample 1", "Sample 2","", "Sample 1", "Sample 2")
lower_labels <- c( "", "WM Test 1", "WM Test 2")
# Plot with lines connecting specific means
LTM_sample <- ggplot(data = summary_stats_participant_LTM_ip, aes(x = factor(image_presentation, level = level_order2), y = accuracy_ltm, fill = factor(image_presentation))) +
  geom_line(data = summary_stats_participant_LTM_ip, aes(group = participant, x = factor(image_presentation, level = level_order2), y = accuracy_ltm), color = "grey", size = 0.3, alpha = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 1.5, dodge.width = 0.5), alpha = 0.6, shape = 21, color = "black", size = 3) +
  geom_point(data = summary_study_level_ip, aes(x = image_presentation, y = accuracy_ltm, fill = factor(image_presentation)),  colour = "black", size = 4.4) +
  geom_errorbar(data = summary_study_level_ip, aes(x = image_presentation, ymin = accuracy_ltm - se, ymax = accuracy_ltm + se), width = 0.15, colour = "black", size = 0.8) +
  geom_line(data = mean_lines_data1, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour = "black", size = 2, linetype = "twodash") +
  geom_line(data = mean_lines_data2, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  geom_line(data = mean_lines_data3, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  scale_y_reverse(breaks = c(0, 30, 60, 90, 120), labels = c("0°", "30°", "60°", "90°","120°")) +  # Set specific y-axis ticks
  scale_x_discrete(labels = upper_labels) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "LTM Performance",
    x = "\n          WM Test 1      WM Test 2         Not Probed (NP)",
  ) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Error (°)") +
  xlab("\n        WM Test 1               WM Test 2         Not Probed (NP)") +
  geom_flat_violin(aes(x = factor(image_presentation), y = accuracy_ltm, fill = factor(image_presentation)), alpha = 0.4, position = position_nudge(x = 0), width = 1.5) +
  scale_fill_manual(values = c( "bisque3", "#0072A4" ,"#0072A4","#0072A4", "#5f7e8e",  "#a43658","#0072A4", "#5f7e8e",  "#a43658", "#5f7e8e"),  # Different orangy colors for each trial type
                    name = "Trial Type") +
  geom_hline(yintercept = 90, color = "darksalmon", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "darksalmon", linetype = "dashed", size = 1) +
  # The rest of your ggplot2 code...
  theme_minimal()+
  theme(
    plot.title = element_text(size = 26, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.background = element_rect(colour = "white", fill = NA, size = 1)
  )

LTM_sample
# Add hierarchical labels
upper_grob <- textGrob(label = c("", "Sample 1", "Sample 2", "Sample 1", "Sample 2"), 
                       x = unit(seq(1, length(level_order12), length.out = length(level_order12)), "npc"), y = unit(1, "npc"), just = "center")
lower_grob <- textGrob(label = c("One-Sample", "", "WM Test 1", "WM Test 1", "WM Test 2", "WM Test 2"), 
                       x = unit(seq(1, length(level_order12), length.out = length(level_order12)), "npc"), y = unit(-1.5, "npc"), just = "center")

# Adding grobs to the plot
LTM_sample + 
  annotation_custom(upper_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotation_custom(lower_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines")) # Ad


LTM_sample


LTM_sample <- LTM_sample +
  # Horizontal line
  annotate("text", x = 4.5, y = 10, label = "***", size = 12, colour = "black") +
  # Short horizontal line 2
  annotate("segment", x = 3.5, xend = 5.5, y = 11, yend = 11, colour = "black", size = 0.7) +
  # NS label
  annotate("text", x = 3.5, y = 51, label = "***", size = 10, colour = "black") +
  annotate("text", x = 5.5, y = 46, label = "***", size = 10, colour = "black") +
  annotate("text", x = 8.5, y = 59, label = "***", size = 10, colour = "black") +
  annotate("segment", x = 3.5, xend = 3.5, y = 11, yend =13, colour = "black", size = 0.7) +
  annotate("segment", x = 5.5, xend = 5.5, y = 11, yend =13, colour = "black", size = 0.7) 

LTM_sample


# Create an empty plot to use as a spacer
empty_plot <- ggplot() + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "transparent"))

# Adjust the widths so that WM_sample is smaller and the empty_plot is wider
combined_plot <- (WM_sample | empty_plot) / 
  LTM_sample + 
  plot_layout(heights = c(2, 3), widths = c(5, 1))  # WM_sample gets smaller, empty_plot wider

# Print the combined plot
print(combined_plot)

# Optional additional plot, if needed for layout adjustments
plot_b <- ggplot() +
  theme_void() +
  theme(plot.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank()) +
  annotate("text", x = 0.5, y = 0.5, label = " ") +
  coord_fixed(ratio = 1) +
  xlim(0, 0.9) +
  ylim(0, 1)

# Adjust layout again with specific widths and heights
layout <- (WM_sample + plot_b) / 
  LTM_sample +
  plot_layout(heights = c(2, 3), widths = c(5, 1))  # WM_sample narrower, empty plot wider
layout


tiff("Figure2c.tiff", units="in", width=14, height=14, res=300)
layout
dev.off()

tiff("Figure2db.tiff", units="in", width=14, height=7, res=300)
LTM_accuracies_bias2
dev.off()


#### Supplementary Plot for Exp. 1

# Please see python code in the file "cardinal_bias"




### Experiment 2

lines_data1 <- summary_stats_participant_image_pres %>%
  filter(image_effect %in% c("Image1-attended", "Image2-attended"))

lines_data2 <- summary_stats_participant_image_pres %>%
  filter(image_effect %in% c("Image1-unattended", "Image2-unattended"))

# Calculate means
mean_lines_data1 <- lines_data1 %>%
  group_by(image_effect) %>%
  summarise(mean_accuracy = mean(accuracy_t1))

mean_lines_data2 <- lines_data2 %>%
  group_by(image_effect) %>%
  summarise(mean_accuracy = mean(accuracy_t1))

gap_data2 <- data.frame(
  image_effect = factor("Gap", levels = c(levels(summary_stats_participant_image_pres$image_presentation), "Gap")),
  participant = NA,  # Assuming type character or factor, adjust as necessary
  N = NA_integer_,   # Use NA_integer_ for integer columns
  accuracy_t1 = NA_real_,  # Use NA_real_ for numeric columns
  sd = NA_real_,  # Use NA_real_ for numeric columns
  se = NA_real_,  # Use NA_real_ for numeric columns
  ci = NA_real_  # Use NA_real_ for numeric columns
)

summary_stats_participant_image_pres <- rbind(summary_stats_participant_image_pres, gap_data2)

level_order2 <- c('Image1-attended', 'Image2-attended','Gap','Image1-unattended','Image2-unattended') 

WM_sample <- ggplot(data = summary_stats_participant_image_pres, aes(x = factor(image_effect, level = level_order2), y = accuracy_t1, fill = factor(image_effect))) +
  geom_line(data = summary_stats_participant_image_pres, aes(group = participant, x = factor(image_effect, level = level_order2), y = accuracy_t1), color = "grey", size = 0.3, alpha = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 1.5, dodge.width = 0.5), alpha = 0.6, shape = 21, color = "black", size = 3) +
  geom_point(data = summary_study_level_image_pres, aes(x = image_effect, y = accuracy_t1, fill = factor(image_effect)),  colour = "black", size = 4.4) +
  geom_errorbar(data = summary_study_level_image_pres, aes(x = image_effect, ymin = accuracy_t1 - se, ymax = accuracy_t1 + se), width = 0.15, colour = "black", size = 0.8) +
  geom_line(data = mean_lines_data1, aes(x = factor(image_effect, level = level_order2), y = mean_accuracy, group = 1), colour = "black", size = 2, linetype = "twodash") +
  geom_line(data = mean_lines_data2, aes(x = factor(image_effect, level = level_order2), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  scale_x_discrete(labels = c("Sample 1", 'Sample 2','','Sample 1','Sample 2')) +
  scale_y_reverse(breaks = c(0, 30, 60, 90,120), labels = c("0°", "30°", "60°", "90°","120°")) +  # Set specific y-axis ticks
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "WM Performance",
    x = "\n\nCued                                           Uncued"
  ) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Error (°)") +
  xlab("Cued                                                Uncued") +
  geom_flat_violin(aes(x = factor(image_effect), y = accuracy_t1, fill = factor(image_effect)), alpha = 0.4, position = position_nudge(x = 0), width = 1.5) +
  scale_fill_manual(values = c( "#e41658", "#3f8e8e", "#e41658","#3f8e8e","#e41658"),  # Different orangy colors for each trial type
                    name = "Trial Type") +
  geom_hline(yintercept = 90, color = "darksalmon", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "darksalmon", linetype = "dashed", size = 1) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 26, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.background = element_rect(colour = "white", fill = NA, size = 1)
  )




WM_sample <- WM_sample + 
  # Top horizontal line for the first bracket
  annotate("segment", x = 1.5, xend = 4.5, y = -5, yend = -5, colour = "black", size = 0.7) +
  # Corrected vertical lines for the first bracket
  annotate("segment", x = 1.5, xend = 1.5, y = -5, yend = -4, colour = "black", size = 0.7) +
  annotate("segment", x = 4.5, xend = 4.5, y = -5, yend = -4, colour = "black", size = 0.7) +
  # Star on top of the first bracket
  annotate("text", x = 3, y = -6, label = "**", size = 12, colour = "black")+
  annotate("text", x = 1.5, y = 12, label = "n.s.", size = 8, colour = "black")+
  annotate("text", x = 4.5, y = 18, label = "n.s.", size = 8, colour = "black")

WM_sample


# Filter the data to include only the points you want to calculate means and connect with lines
lines_data1 <- summary_stats_participant_LTM_ip_V3 %>%
  filter(image_effect %in% c("Image1-attended", "Image2-attended"))

lines_data2 <- summary_stats_participant_LTM_ip_V3 %>%
  filter(image_effect %in% c("Image1-unattended", "Image2-unattended"))

lines_data3 <- summary_stats_participant_LTM_ip_V3 %>%
  filter(image_effect %in% c("Image1-attended-never tested", "Image2-attended-never tested"))

lines_data4 <- summary_stats_participant_LTM_ip_V3 %>%
  filter(image_effect %in% c("Image1-unattended-never tested", "Image2-unattended-never tested"))

# Calculate means
mean_lines_data1 <- lines_data1 %>%
  group_by(image_effect) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data2 <- lines_data2 %>%
  group_by(image_effect) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data3 <- lines_data3 %>%
  group_by(image_effect) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data4 <- lines_data4 %>%
  group_by(image_effect) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))


gap_data2 <- data.frame(
  image_effect = factor("Gap2", levels = c(levels(summary_stats_participant_image_pres$image_presentation), "Gap2")),
  participant = NA,  # Assuming type character or factor, adjust as necessary
  N = NA_integer_,   # Use NA_integer_ for integer columns
  accuracy_ltm = NA_real_,  # Use NA_real_ for numeric columns
  sd = NA_real_,  # Use NA_real_ for numeric columns
  se = NA_real_,  # Use NA_real_ for numeric columns
  ci = NA_real_  # Use NA_real_ for numeric columns
)

summary_stats_participant_LTM_ip_V3 <- rbind(summary_stats_participant_LTM_ip_V3, gap_data2)


level_order11 <- c('Image1-attended', 'Image2-attended', 'Image1-unattended','Image2-unattended','Gap2','Image1-attended-never tested','Image2-attended-never tested','Image1-unattended-never tested','Image2-unattended-never tested')

# Plot with lines connecting specific means
LTM_sample <- ggplot(data = summary_stats_participant_LTM_ip_V3, aes(x = factor(image_effect, level = level_order11), y = accuracy_ltm, fill = factor(image_effect))) +
  geom_line(data = summary_stats_participant_LTM_ip_V3, aes(group = participant, x = factor(image_effect, level = level_order11), y = accuracy_ltm), color = "grey", size = 0.3, alpha = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 1.5, dodge.width = 0.5), alpha = 0.6, shape = 21, color = "black", size = 3) +
  geom_point(data = summary_study_level_ip_V3, aes(x = image_effect, y = accuracy_ltm, fill = factor(image_effect)),  colour = "black", size = 4.4) +
  geom_errorbar(data = summary_study_level_ip_V3, aes(x = image_effect, ymin = accuracy_ltm - se, ymax = accuracy_ltm + se), width = 0.15, colour = "black", size = 0.8) +
  geom_line(data = mean_lines_data1, aes(x = factor(image_effect, level = level_order11), y = mean_accuracy, group = 1), colour = "black", size = 2, linetype = "twodash") +
  geom_line(data = mean_lines_data2, aes(x = factor(image_effect, level = level_order11), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  geom_line(data = mean_lines_data3, aes(x = factor(image_effect, level = level_order11), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  geom_line(data = mean_lines_data4, aes(x = factor(image_effect, level = level_order11), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  scale_y_reverse(breaks = c(0, 30, 60, 90,120), labels = c("0°", "30°", "60°", "90°","120°")) +  # Set specific y-axis ticks
  scale_x_discrete(labels = c('Sample 1', 'Sample 2', 'Sample 1','Sample 2','','Sample 1','Sample 2','Sample 1','Sample 2'))+
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Pruned LTM Performance",
    x = "Cued             Uncued           Cued         Uncued      Not Probed"
  ) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Error (°)") +
  xlab("Cued             Uncued           Cued         Uncued      Not Probed") +
  geom_flat_violin(aes(x = factor(image_effect), y = accuracy_ltm, fill = factor(image_effect)), alpha = 0.3, position = position_nudge(x = 0), width = 1.5) +
  scale_fill_manual(values = c( "#0072E2", "#5f7e7e","#0072E2","#a43658","#0072A4", "#5f7e7e","#0072E2","#a43658","#0072A4"),  # Different orangy colors for each trial type
                    name = "Trial Type")+
  geom_hline(yintercept = 90, color = "darksalmon", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "darksalmon", linetype = "dashed", size = 1) +
  theme_minimal()+theme(
    plot.title = element_text(size = 26, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.background = element_rect(colour = "white", fill = NA, size = 1)
  )



LTM_sample

LTM_sample <- LTM_sample + 
  # Top horizontal line for the first bracket
  annotate("segment", x = 3.5, xend = 8.5, y = -2, yend = -2, colour = "black", size = 0.7) +
  # Corrected vertical lines for the first bracket
  annotate("segment", x = 3.5, xend = 3.5, y = -2, yend = -1, colour = "black", size = 0.7) +
  annotate("segment", x = 8.5, xend = 8.5, y = -2, yend = -1, colour = "black", size = 0.7) +
  # Star on top of the first bracket
  annotate("text", x = 6, y = -3, label = "***", size = 12, colour = "black")+
  
  annotate("segment", x = 1.5, xend = 6.5, y = 6, yend = 6, colour = "black", size = 0.7) +
  # Corrected vertical lines for the first bracket
  annotate("segment", x = 1.5, xend = 1.5, y = 6, yend = 7, colour = "black", size = 0.7) +
  annotate("segment", x = 6.5, xend = 6.5, y = 6, yend = 7, colour = "black", size = 0.7) +
  # Star on top of the first bracket
  annotate("text", x = 4, y = 4.5, label = "**", size = 12, colour = "black")+
  annotate("text", x = 8.6, y = 58, label = "***", size = 10, colour = "black")+
  annotate("text", x = 1.6, y = 48, label = "*", size = 10, colour = "black")+
  annotate("text", x = 6.6, y = 55, label = "*", size = 10, colour = "black")


LTM_sample



# Create an empty plot to use as a spacer
empty_plot <- ggplot() + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "transparent"))

plot_layout <- (WM_sample |empty_plot) / 
  LTM_sample +
  plot_layout(heights = c(2, 2))

combined_plot <- (WM_sample) / 
  LTM_sample + 
  plot_layout(heights = c(2, 2), widths = c(4, 4, 1)) # Adjusted widths for first row

# Print the combined plot
print(combined_plot)

plot_b <- ggplot() +
  theme_void() +
  theme(plot.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank()) +
  annotate("text", x = 0.5, y = 0.5, label = " ") +
  coord_fixed(ratio = 1) +
  xlim(0, 0.9) +
  ylim(0, 1)

layout <- (WM_sample + plot_b) / 
  LTM_sample +
  plot_layout(widths = c(2, 2))

tiff("Figure3c.tiff", units="in", width=14, height=14, res=300)
layout
dev.off()

##here I am plotting the difference in pruned performance. 

# Calculate the new values
attended_value <- summary_stats_participant_LTM_p_V3[summary_stats_participant_LTM_p_V3$trial_type == "attended", "accuracy_ltm"]
unattended_value <- summary_stats_participant_LTM_p_V3[summary_stats_participant_LTM_p_V3$trial_type == "unattended", "accuracy_ltm"]
attended_never_tested_value <- summary_stats_participant_LTM_p_V3[summary_stats_participant_LTM_p_V3$trial_type == "attended-never tested", "accuracy_ltm"]
unattended_never_tested_value <- summary_stats_participant_LTM_p_V3[summary_stats_participant_LTM_p_V3$trial_type == "unattended-never tested", "accuracy_ltm"]

difference_1 <- attended_value - attended_never_tested_value
difference_2 <- unattended_value - unattended_never_tested_value

# Create a new data frame with the calculated differences
new_values_df <- data.frame(
  difference_attended_vs_unattended = difference_1,
  difference_attended_never_tested_vs_unattended_never_tested = difference_2
)

differences_per_participant <- summary_stats_participant_LTM_p_V3 %>%
  group_by(participant) %>%
  summarize(
    cued = mean(accuracy_ltm[trial_type == "attended"]) - mean(accuracy_ltm[trial_type == "attended-never tested"]),
    uncued = mean(accuracy_ltm[trial_type == "unattended"]) - mean(accuracy_ltm[trial_type == "unattended-never tested"])
  )

# Calculate overall means and standard errors
mean_values <- differences_per_participant %>%
  summarize(
    mean_cued = mean(cued),
    se_cued = sd(cued) / sqrt(n()),
    mean_uncued = mean(uncued),
    se_uncued = sd(uncued) / sqrt(n())
  ) %>%
  pivot_longer(cols = everything(), names_to = c(".value", "Difference_Type"), names_pattern = "(mean|se)_(.*)")

mean_values$participant <- 1


# Calculate mean differences per participant
differences_per_participant <- summary_stats_participant_LTM_p_V3 %>%
  group_by(participant) %>%
  summarize(
    cued = mean(accuracy_ltm[trial_type == "attended"]) - mean(accuracy_ltm[trial_type == "attended-never tested"]),
    uncued = mean(accuracy_ltm[trial_type == "unattended"]) - mean(accuracy_ltm[trial_type == "unattended-never tested"])
  )

# Calculate overall means and standard errors
mean_values <- differences_per_participant %>%
  summarize(
    mean_cued = mean(cued),
    se_cued = sd(cued) / sqrt(n()),
    mean_uncued = mean(uncued),
    se_uncued = sd(uncued) / sqrt(n())
  )

# Reshape mean_values for ggplot
mean_values_long <- mean_values %>%
  pivot_longer(cols = everything(), names_to = c(".value", "Difference_Type"), names_pattern = "(mean|se)_(.*)")

# Reshape the differences_per_participant data for ggplot
differences_per_participant_long <- differences_per_participant %>%
  pivot_longer(cols = c(cued, uncued), names_to = "Difference_Type", values_to = "Difference_Value")

# Define the colors
fill_colors <- c("cued" = "darkgrey", "uncued" = "bisque3")

mean_values_long$participant <- 1

differfence <- ggplot() +
  geom_line(data = differences_per_participant_long, aes(x = Difference_Type, y = Difference_Value, group = participant, color = "grey"), size = 1, alpha = 0.3) +  # Connect lines per participant
  geom_point(data = differences_per_participant_long, aes(x = Difference_Type, y = Difference_Value, color = "grey"), size = 4, alpha = 0.1) +  # Add points per participant
  geom_bar(data = mean_values_long, aes(x = Difference_Type, y = mean, fill = Difference_Type), width = 0.8, stat = "identity", position = "dodge", alpha = 0.7) +  # Add mean bars with correct fill color and transparency
  geom_point(data = mean_values_long, aes(x = Difference_Type, y = mean), color = "black", size = 5, shape = 19) +  # Add mean points
  geom_errorbar(data = mean_values_long, aes(x = Difference_Type, ymin = mean - se, ymax = mean + se), width = 0.2, color = "black", size = 1) +  # Add error bars
  geom_line(data = mean_values_long, aes(x = Difference_Type, y = mean, group = 1), color = "black", size = 1.5) +  # Connect mean points
  scale_color_manual(values = c("grey" = "grey")) +  # Set color for lines
  scale_fill_manual(values = fill_colors) +  # Set fill colors for mean bars
  theme_minimal() +
  scale_y_reverse() +
  scale_x_discrete(labels = c('Cued', 'Uncued')) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 20),
    plot.background = element_rect(colour = "white", fill = NA, size = 1)
  ) +
  labs(
    title = "LTM benefit (Tested - NP)",
    x = "",
    y = "Error difference (°)"
  )

print(differfence)



differfence <- differfence + 
  annotate("text", x = 1.5, y = -30, label = "*", size = 8, colour = "black")+
  annotate("segment", x = 1, xend = 2, y = -28, yend = -28, colour = "black", size = 0.8) +
  annotate("segment", x = 1, xend = 1, y = -28, yend = -25, colour = "black", size = 0.8) +
  annotate("segment", x = 2, xend = 2, y = -28, yend = -25, colour = "black", size = 0.8) 



differfence







tiff("Figure3d.tiff", units="in", width=5, height=5, res=300)
differfence
dev.off()


### Experiment 3 ehemals 2
summary_stats_participant_image_pres$accuracy <- summary_stats_participant_image_pres$accuracy * 100
summary_study_level_image_pres$accuracy <- summary_study_level_image_pres$accuracy * 100
lines_data1 <- summary_stats_participant_image_pres %>%
  filter(image_presentation %in% c("Img1-Test1", "Img2-Test1"))

lines_data2 <- summary_stats_participant_image_pres %>%
  filter(image_presentation %in% c("Img1-Test2", "Img2-Test2"))

# Calculate means
mean_lines_data1 <- lines_data1 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy))

mean_lines_data2 <- lines_data2 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy))

scale_fill_manual(values = c("bisque1",  "#3f8e8e", "#e41658",  "#3f8e8e","#e41658", "#3f8e8e", "#b41698"),
                  name = "Trial Type")

gap_data2 <- data.frame(
  image_presentation = factor("Gap2", levels = c(levels(summary_stats_participant_image_pres$image_presentation), "Gap2")),
  participant = NA,  # Assuming type character or factor, adjust as necessary
  N = NA_integer_,   # Use NA_integer_ for integer columns
  accuracy = NA_real_,  # Use NA_real_ for numeric columns
  sd = NA_real_,  # Use NA_real_ for numeric columns
  se = NA_real_,  # Use NA_real_ for numeric columns
  ci = NA_real_  # Use NA_real_ for numeric columns
)

summary_stats_participant_image_pres <- rbind(summary_stats_participant_image_pres, gap_data2)


level_order2 <- c('Baseline','Gap2','Img1-Test1', 'Img2-Test1','Img1-Test2','Img2-Test2')

WM_sample <- ggplot(data = summary_stats_participant_image_pres, aes(x = factor(image_presentation, level = level_order2), y = accuracy, fill = factor(image_presentation))) +
  geom_line(data = summary_stats_participant_image_pres, aes(group = participant, x = factor(image_presentation, level = level_order2), y = accuracy), color = "grey", size = 0.3, alpha = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 1, dodge.width = 0.5), alpha = 0.6, shape = 21, color = "black", size = 3) +
  geom_point(data = summary_study_level_image_pres, aes(x = image_presentation, y = accuracy, fill = factor(image_presentation)),  colour = "black", size = 4.4) +
  geom_errorbar(data = summary_study_level_image_pres, aes(x = image_presentation, ymin = accuracy - se, ymax = accuracy + se), width = 0.15, colour = "black", size = 0.8) +
  geom_line(data = mean_lines_data1, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour = "black", size = 2, linetype = "twodash") +
  geom_line(data = mean_lines_data2, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  scale_x_discrete(labels = c("One-Sample","", 'Sample1','Sample2','Sample1','Sample2')) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  # Y-axis breaks
  scale_y_continuous(breaks = c(30, 60, 90)) +
  labs(
    title = "WM Performance",
    x = "\n\nExperimental condition"
  ) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("% Correct") +
  xlab("                                   WM Test 1                          WM Test 2") +
  geom_flat_violin(aes(x = factor(image_presentation), y = accuracy, fill = factor(image_presentation)), alpha = 0.4, position = position_nudge(x = 0), width = 1.5) +
  scale_fill_manual(values = c("bisque1",  "#e41658", "#3f8e8e","#e41658","#3f8e8e","#e41658", "#3f8e8e", "#b41698"),  # Different orangy colors for each trial type
                    name = "Trial Type") +
  geom_hline(yintercept = 100, color = "darksalmon", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 50, color = "darksalmon", linetype = "dashed", size = 1) +
  theme_minimal()+
  theme_minimal()+theme(
    plot.title = element_text(size = 26, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.background = element_rect(colour = "white", fill = NA, size = 1)
  )





WM_sample


##adding stats

WM_sample <- WM_sample + 
  # Top horizontal line for the second bracket
  annotate("segment", x = 3.5, xend = 5.5, y = 90, yend = 90, colour = "black", size =0.7) +
  # Corrected vertical lines for the second bracket
  annotate("segment", x = 3.5, xend = 3.5, y = 90, yend = 89, colour = "black", size = 0.7) +
  annotate("segment", x = 5.5, xend = 5.5, y = 90, yend = 89, colour = "black", size = 0.7) +
  # Star on top of the second bracket
  annotate("text", x = 4.45, y = 93, label = "n.s.", size = 8, colour = "black")



WM_sample



#### LTM Plot 

# Filter the data to include only the points you want to calculate means and connect with lines
lines_data1 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-Test1", "Img2-Test1"))

lines_data2 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-Test2", "Img2-Test2"))

lines_data3 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-NT", "Img2-NT"))

# Calculate means
mean_lines_data1 <- lines_data1 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data2 <- lines_data2 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data3 <- lines_data3 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))


gap_data3 <- data.frame(
  image_presentation = factor("Gap3", levels = c(levels(summary_stats_participant_image_pres$image_presentation), "Gap3")),
  participant = NA,  # Assuming type character or factor, adjust as necessary
  N = NA_integer_,   # Use NA_integer_ for integer columns
  accuracy_ltm = NA_real_,  # Use NA_real_ for numeric columns
  sd = NA_real_,  # Use NA_real_ for numeric columns
  se = NA_real_,  # Use NA_real_ for numeric columns
  ci = NA_real_  # Use NA_real_ for numeric columns
)

summary_stats_participant_LTM_ip <- rbind(summary_stats_participant_LTM_ip, gap_data2,gap_data3)

level_order2 <- c('Baseline','Gap2','Img1-Test1', 'Img2-Test1', 'Img1-Test2','Img2-Test2','Gap3','Img1-NT','Img2-NT')

# Plot with lines connecting specific means
LTM_sample <- ggplot(data = summary_stats_participant_LTM_ip, aes(x = factor(image_presentation, level = level_order2), y = accuracy_ltm, fill = factor(image_presentation))) +
  geom_line(data = summary_stats_participant_LTM_ip, aes(group = participant, x = factor(image_presentation, level = level_order2), y = accuracy_ltm), color = "grey", size = 0.3, alpha = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 1.5, dodge.width = 0.5), alpha = 0.6, shape = 21, color = "black", size = 3) +
  geom_point(data = summary_study_level_ip, aes(x = image_presentation, y = accuracy_ltm, fill = factor(image_presentation)),  colour = "black", size = 4.4) +
  geom_errorbar(data = summary_study_level_ip, aes(x = image_presentation, ymin = accuracy_ltm - se, ymax = accuracy_ltm + se), width = 0.15, colour = "black", size = 0.8) +
  geom_line(data = mean_lines_data1, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour = "black", size = 2, linetype = "twodash") +
  geom_line(data = mean_lines_data2, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  geom_line(data = mean_lines_data3, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  scale_y_reverse() +
  scale_x_discrete(labels = c("One-Sample",'', 'Sample1\n','Sample2\n','Sample1','Sample2','','Sample1','Sample2')) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "LTM Performance",
    x = "\n\nExperimental condition"
  ) +
  ylim(c(115, 0)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Error (°)") +
  xlab("                                                 WM Test 1              WM Test 2                  Not Probed (NP)  ") +
  geom_flat_violin(aes(x = factor(image_presentation), y = accuracy_ltm, fill = factor(image_presentation)), alpha = 0.4, position = position_nudge(x = 0), width = 1.5) +
  scale_fill_manual(values = c("bisque3", "#5f7e7e","#a43658","#0072E2", "#5f7e7e" ,"#a43658","#0072E2", "#5f7e7e" ,"#a43658"),  # Different orangy colors for each trial type
                    name = "Trial Type") +
  geom_hline(yintercept = 90, color = "darksalmon", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "darksalmon", linetype = "dashed", size = 1) +
  theme_minimal()+theme(
    plot.title = element_text(size = 26, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.background = element_rect(colour = "white", fill = NA, size = 1)
  )

LTM_sample <- LTM_sample +
  # Horizontal line
  annotate("text", x = 3.5, y = 70, label = "***", size = 10, colour = "black") +
  annotate("text", x = 5.5, y = 68, label = "***", size = 10, colour = "black") +
  # Short horizontal line
  # Short horizontal line 2
  annotate("segment", x = 3.5, xend = 5.5, y = 11, yend = 11, colour = "black", size = 0.7) +
  # NS label
  annotate("text", x = 4.5, y = 7, label = "n.s.", size = 8, colour = "black") +
  # Short vertical lines at specified positions
  annotate("segment", x = 5.5, xend = 5.5, y = 11, yend = 13, colour = "black", size = 0.7) +
  annotate("segment", x = 3.5, xend = 3.5, y = 11, yend = 13, colour = "black", size = 0.7) 

LTM_sample


# combine the plots

# Create an empty plot to use as a spacer
empty_plot <- ggplot() + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "transparent"))

plot_layout <- ( WM_sample |empty_plot) / 
  LTM_sample +
  plot_layout(heights = c(2, 2))

combined_plot <- (WM_sample) / 
  LTM_sample + 
  plot_layout(heights = c(2, 2), widths = c(4, 4, 1)) # Adjusted widths for first row

# Print the combined plot
print(combined_plot)

plot_b <- ggplot() +
  theme_void() +
  theme(plot.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank()) +
  annotate("text", x = 0.5, y = 0.5, label = " ") +
  coord_fixed(ratio = 1) +
  xlim(0, 0.82) +
  ylim(0, 1)

layout <- (WM_sample + plot_b) / 
  LTM_sample +
  plot_layout(widths = c(2, 2))

tiff("Figure4c.tiff", units="in", width=12, height=12, res=300)
layout
dev.off()


## pruned LTM performance

# Filter the data to include only the points you want to calculate means and connect with lines
lines_data1 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-Test1", "Img2-Test1"))

lines_data2 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-Test2", "Img2-Test2"))

lines_data3 <- summary_stats_participant_LTM_ip %>%
  filter(image_presentation %in% c("Img1-NT", "Img2-NT"))

# Calculate means
mean_lines_data1 <- lines_data1 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data2 <- lines_data2 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))

mean_lines_data3 <- lines_data3 %>%
  group_by(image_presentation) %>%
  summarise(mean_accuracy = mean(accuracy_ltm))


gap_data3 <- data.frame(
  image_presentation = factor("Gap3", levels = c(levels(summary_stats_participant_image_pres$image_presentation), "Gap3")),
  participant = NA,  # Assuming type character or factor, adjust as necessary
  N = NA_integer_,   # Use NA_integer_ for integer columns
  accuracy_ltm = NA_real_,  # Use NA_real_ for numeric columns
  sd = NA_real_,  # Use NA_real_ for numeric columns
  se = NA_real_,  # Use NA_real_ for numeric columns
  ci = NA_real_  # Use NA_real_ for numeric columns
)

summary_stats_participant_LTM_ip <- rbind(summary_stats_participant_LTM_ip, gap_data2,gap_data3)

level_order2 <- c('Baseline','Gap2','Img1-Test1', 'Img2-Test1', 'Img1-Test2','Img2-Test2','Gap3','Img1-NT','Img2-NT')

# Plot with lines connecting specific means
LTM_sample <- ggplot(data = summary_stats_participant_LTM_ip, aes(x = factor(image_presentation, level = level_order2), y = accuracy_ltm, fill = factor(image_presentation))) +
  geom_line(data = summary_stats_participant_LTM_ip, aes(group = participant, x = factor(image_presentation, level = level_order2), y = accuracy_ltm), color = "grey", size = 0.3, alpha = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 1.5, dodge.width = 0.5), alpha = 0.6, shape = 21, color = "black", size = 3) +
  geom_point(data = summary_study_level_ip, aes(x = image_presentation, y = accuracy_ltm, fill = factor(image_presentation)),  colour = "black", size = 4.4) +
  geom_errorbar(data = summary_study_level_ip, aes(x = image_presentation, ymin = accuracy_ltm - se, ymax = accuracy_ltm + se), width = 0.15, colour = "black", size = 0.8) +
  geom_line(data = mean_lines_data1, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour = "black", size = 2, linetype = "twodash") +
  geom_line(data = mean_lines_data2, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  geom_line(data = mean_lines_data3, aes(x = factor(image_presentation, level = level_order2), y = mean_accuracy, group = 1), colour =  "black", size = 2,linetype = "twodash")+
  scale_y_reverse() +
  scale_x_discrete(labels = c("One-Sample",'', 'Sample1\n','Sample2\n','Sample1','Sample2','','Sample1','Sample2')) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "LTM Performance",
    x = "\n\nExperimental condition"
  ) +
  ylim(c(115, 0)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Error (°)") +
  xlab("                                                 WM Test 1              WM Test 2                  Not Probed (NP)  ") +
  geom_flat_violin(aes(x = factor(image_presentation), y = accuracy_ltm, fill = factor(image_presentation)), alpha = 0.4, position = position_nudge(x = 0), width = 1.5) +
  scale_fill_manual(values = c("bisque3", "#5f7e7e","#a43658","#0072E2", "#5f7e7e" ,"#a43658","#0072E2", "#5f7e7e" ,"#a43658"),  # Different orangy colors for each trial type
                    name = "Trial Type") +
  geom_hline(yintercept = 90, color = "darksalmon", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "darksalmon", linetype = "dashed", size = 1) +
  theme_minimal()+theme(
    plot.title = element_text(size = 26, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.background = element_rect(colour = "white", fill = NA, size = 1)
  )



LTM_sample <- LTM_sample +
  # Horizontal line
  annotate("text", x = 1.5, y = 20, label = "* (n.s.)", size = 8, colour = "black") +
  annotate("text", x = 2.5, y = 68, label = "* (n.s.)", size = 8, colour = "black") +
  annotate("text", x = 3.8, y = 11, label = "*", size = 8, colour = "black") +
  annotate("text", x = 4.5, y = 62, label = "* (n.s.)", size = 8, colour = "black") +
  # Short horizontal line
  # Short horizontal line 2
  annotate("segment", x = 3.5, xend = 5.5, y = 11, yend = 11, colour = "black", size = 0.7) +
  # Short vertical lines at specified positions
  annotate("segment", x = 1, xend = 7, y = 11, yend = 11, colour = "black", size = 0.7) +
  annotate("segment", x = 1, xend = 1, y = 11, yend = 13, colour = "black", size = 0.7) +
  annotate("segment", x = 7, xend = 7, y = 11, yend = 13, colour = "black", size = 0.7) +
  annotate("segment", x = 1, xend = 2.5, y = 20, yend = 20, colour = "black", size = 0.7) +
  annotate("segment", x = 1, xend = 1, y = 20, yend = 22, colour = "black", size = 0.7) +
  annotate("segment", x = 2.5, xend = 2.5, y = 20, yend = 22, colour = "black", size = 0.7) 
  

LTM_sample



tiff("pruned_LTMExp3.tiff", units="in", width=12, height=12, res=300)
LTM_sample
dev.off()







