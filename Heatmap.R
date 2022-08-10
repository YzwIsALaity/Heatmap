# Package
library(ggplot2)
library(knitr)
library(viridis)

# Read the first dataset
Dt <- read.csv('Heatmap Plot (Individual).csv')
kable(head(Dt))

# Factor
Dt$Month <- factor(Dt$Month, levels = c('6-month', '12-month', '24-month'))
Dt$Experienced.Symptom <- factor(Dt$Experienced.Symptom, levels = c('No', 'Yes'))

# Extract rows for Fatigue #################################################################################
Fatigue <- Dt[which(Dt$Symptom == 'Fatigue'), ]
# Heatmap ##################################################################################################
p_Fatigue <- 
  ggplot(Fatigue, 
         aes(x = Month, y = reorder(PTID, n.symptoms))) + 
  geom_tile(aes(fill = Experienced.Symptom)) + 
  scale_fill_manual(name = 'Experienced Symptom', 
                    values = c('No' = 'aquamarine2', 'Yes' = 'firebrick2', 'NA' = 'gray45')) + 
  ggtitle('Fatigue') + 
  theme(panel.grid.major = element_blank(),      # remove background and grid
        panel.background = element_blank(), 
        axis.line = element_blank(),             # remove lines in X- and Y-axes
        axis.ticks.x = element_blank(),          # remove ticks in X- and Y-axes
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),          # remove titles in X- and Y-axes
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = 'black', face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15, color = 'black', face = "bold"),
        legend.text = element_text(size = 15, color = 'black', face = "bold"),
        legend.title = element_text(size = 15, face = "bold", color = 'black'),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = 'black'))
p_Fatigue

# First: Fatigue #########################################################################################
Fatigue <- Dt[which(Dt$Symptom == 'Fatigue'), ]
p_Fatigue <- 
  ggplot(Fatigue, 
         aes(x = Month, y = reorder(PTID, n.symptoms))) + 
  geom_tile(aes(fill = Experienced.Symptom)) + 
  scale_fill_manual(name = 'Experienced Symptom', 
                    values = c('No' = 'aquamarine2', 'Yes' = 'firebrick2', 'NA' = 'gray45')) + 
  ggtitle('Fatigue') + 
  theme(panel.grid.major = element_blank(),      # remove background and grid
        panel.background = element_blank(), 
        axis.line = element_blank(),             # remove lines in X- and Y-axes
        axis.ticks.x = element_blank(),          # remove ticks in X- and Y-axes
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),          # remove titles in X- and Y-axes
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = 'black', face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15, color = 'black', face = "bold"),
        legend.position = 'none',                # remove legend
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = 'black'))

# Last: Hair Loss #########################################################################################
Hair_Loss <- Dt[which(Dt$Symptom == 'Hair Loss'), ]
p_Hair_Loss <- 
  ggplot(Hair_Loss, 
         aes(x = Month, y = reorder(PTID, n.symptoms))) + 
  geom_tile(aes(fill = Experienced.Symptom)) + 
  scale_fill_manual(name = 'Experienced Symptom', 
                    values = c('No' = 'aquamarine2', 'Yes' = 'firebrick2', 'NA' = 'gray45')) + 
  ggtitle('Hair Loss') + 
  theme(panel.grid.major = element_blank(),      # remove background and grid
        panel.background = element_blank(), 
        axis.line = element_blank(),             # remove lines in X- and Y-axes
        axis.ticks.x = element_blank(),          # remove ticks in X- and Y-axes
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),          # remove titles in X- and Y-axes
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = 'black', face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 15, color = 'black', face = "bold"),
        legend.title = element_text(size = 15, face = "bold", color = 'black'),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = 'black'))

# Middle: other symptoms  #################################################################################
Var.Middle <- c('Headache', 'Muscle Aches', 'Breathing Difficulties', 'Loss Taste/Smell', 
                'Joint Pain', 'Vertigo', 'Lowering Vision', 'Brain Fog')
Heatmap.Middle <- list()
for(i in 1:length(Var.Middle)){
  Data <- Dt[which(Dt$Symptom == Var.Middle[i]), ]
  p <- 
    ggplot(Data, 
           aes(x = Month, y = reorder(PTID, n.symptoms))) + 
    geom_tile(aes(fill = Experienced.Symptom)) + 
    scale_fill_manual(name = 'Experienced Symptom', 
                      values = c('No' = 'aquamarine2', 'Yes' = 'firebrick2', 'NA' = 'gray45')) + 
    ggtitle(Var.Middle[i]) + 
    theme(panel.grid.major = element_blank(),      # remove background and grid
          panel.background = element_blank(), 
          axis.line = element_blank(),             # remove lines in X- and Y-axes
          axis.ticks.x = element_blank(),          # remove ticks in X- and Y-axes
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),          # remove titles in X- and Y-axes
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 15, color = 'black', face = "bold", angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          legend.position = 'none',                # remove legend
          plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = 'black'))
  Heatmap.Middle[[i]] <- p
}
Layout.Mat <- matrix(c(rep(1:9, each = 8), rep(10, 16)), nrow = 1)
Layout.Mat

# Read the second dataset
Dt <- read.csv('Heatmap Plot (Aggregated).csv')
kable(head(Dt))
# Extract the specific type #################################################################################
Dt1 <- Dt[which(Dt$Comparison == "Acute vs 6-month"), ]
# Plot ###################################################################################################### 
p_acute_6m <- 
  ggplot(Dt1, aes(x = Symptom.PriorTimepoint, y = Symptom.LaterTimepoint)) + 
  geom_tile(aes(fill = Corr)) + 
  ggtitle('Acute vs 6-month') + 
  scale_fill_gradientn(breaks = seq(-1, 1, length.out = 6), 
                       colors = viridis(6), n.breaks = 6, limits = c(-1, 1), na.value = 'gray27') + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = 'black', face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15, color = 'black', face = "bold"),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = 'black'),
        legend.text = element_text(size = 15, hjust = 0.5, color = 'black'),
        legend.title = element_text(size = 15, hjust = 0.5, face = "bold", color = 'black'))
p_acute_6m
# First: Acute vs 6-month ############################################################################
Dt1 <- Dt[which(Dt$Comparison == "Acute vs 6-month"), ]
p_acute_6m <- 
  ggplot(Dt1, aes(x = Symptom.PriorTimepoint, y = Symptom.LaterTimepoint)) + 
  geom_tile(aes(fill = Corr)) + 
  ggtitle('Acute vs 6-month') + 
  scale_fill_gradientn(breaks = seq(-1, 1, length.out = 6), 
                       colors = viridis(6), n.breaks = 6, limits = c(-1, 1), na.value = 'gray27') + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = 'black', face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15, color = 'black', face = "bold"),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = 'black'),
        legend.position = 'none')

# Middle: 6-month vs 12-month #######################################################################
Dt2 <- Dt[which(Dt$Comparison == "6-month vs 12-month"), ]
p_acute_12m <- ggplot(Dt2, aes(x = Symptom.PriorTimepoint, y = Symptom.LaterTimepoint)) + 
  geom_tile(aes(fill = Corr)) + 
  ggtitle('6-month vs 12-month') + 
  scale_fill_gradientn(breaks = seq(-1, 1, length.out = 6), 
                       colors = viridis(6), n.breaks = 6, limits = c(-1, 1), na.value = 'gray27') + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = 'black', face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = 'black'),
        legend.position = 'none')

# Last: 12-month vs 24-month ##########################################################################
Dt3 <- Dt[which(Dt$Comparison == "12-month vs 24-month"), ]
p_acute_24m <- ggplot(Dt3, aes(x = Symptom.PriorTimepoint, y = Symptom.LaterTimepoint)) + 
  geom_tile(aes(fill = Corr)) + 
  ggtitle('12-month vs 24-month') + 
  scale_fill_gradientn(breaks = seq(-1, 1, length.out = 6), 
                       colors = viridis(6), n.breaks = 6, limits = c(-1, 1), na.value = 'gray27') + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = 'black', face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = 'black'),
        legend.text = element_text(size = 15, hjust = 0.5, color = 'black'),
        legend.title = element_text(size = 15, hjust = 0.5, face = "bold", color = 'black'))
# Arrange heatmaps ######################################################################################
Layout.Mat <- matrix(c(rep(1, 14), rep(2, 10), rep(3, 12)), nrow = 1)
grid.arrange(p_acute_6m, 
             p_acute_12m, 
             p_acute_24m, 
             layout_matrix = Layout.Mat)













