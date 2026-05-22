###This is a mix between my code and DSFAIR
##using code from DFAIRhttps://schmidtpaul.github.io/DSFAIR/splitplot_GomezGomez1984.html
##Install and Load pacman package
# packages

pacman::p_load(readxl, tidyverse, # data import and handling
               conflicted, cli,       # handling function conflicts
               lme4, lmerTest,   # linear mixed model 
               emmeans, multcomp, multcompView, ggsci,  # mean comparisons
               cli, here, cowplot) #other packages


# conflicts: identical function names from different packages
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lmer", "lmerTest")


### Load data
data <- read_excel("/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/YLD.xlsx")
## Formatting
dat <- data %>% 
  mutate_at(vars(TRT:REP), as.factor)%>%
  mutate_at(vars(PDWG:MG_total), as.numeric)

dat$ID <- factor(dat$ID,levels = c("C", "BC", "LBC"))


##check the structure 
str(dat)

###Analysis
mod <- lmer(PDWG ~ ID*RT +(1|REP), 
            data=dat)
mod %>% anova(ddf="Kenward-Roger")

C <- mod %>%
  emmeans(specs = ~ RT, 
          lmer.df = "kenward-roger") %>% 
  cld(reversed=T, Letters=LETTERS, adjust="none") # add compact letter display
C
pd <- position_dodge(0.2)
ggplot(C, aes(x=RT, y=emmean)) + 
  geom_point(size = 2,position=pd)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2, position=pd) +
  geom_text(aes(y=upper.CL+2, label = .group))  + 
  theme_bw() + scale_color_aaas() + 
  theme(text=element_text(size=25,  family="serif"))

###Function for LMER
MOD <- function(A){
  mod <- mod <- lmer(A ~ ID*RT +(1|REP), 
                     data=dat)
  mod %>% anova(ddf="Kenward-Roger")
}

###Function for RT
SF.TJRT <-function(A){
  mod <-lmer(A ~ RT*ID +(1|REP), 
             data=dat)
  mod %>% anova(ddf="Kenward-Roger")
  mod
  C <- mod %>%
    emmeans(specs = ~ RT, 
            lmer.df = "kenward-roger") %>% 
    cld(reversed=T, Letters=LETTERS, adjust="none") # add compact letter display
  C
  pd <- position_dodge(0.2)
  ggplot(C, aes(x=RT, y=emmean)) + 
    geom_point(size = 2,position=pd)+
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2, position=pd) +
    geom_text(aes(y=upper.CL+2, label = .group))  + 
    theme_bw() + scale_color_aaas() + 
    theme(text=element_text(size=25,  family="serif"))
}

###Function for ID
SF.TJID <-function(A){
  mod <-lmer(A ~ RT*ID +(1|REP), 
             data=dat)
  mod %>% anova(ddf="Kenward-Roger")
  mod
  C <- mod %>%
    emmeans(specs = ~ ID, 
            lmer.df = "kenward-roger") %>% 
    cld(reversed=T, Letters=LETTERS, adjust="none") # add compact letter display
  C
  pd <- position_dodge(0.2)
  ggplot(C, aes(x=ID, y=emmean)) + 
    geom_point(size = 2,position=pd)+
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2, position=pd) +
    geom_text(aes(y=upper.CL+2, label = .group))  + 
    theme_bw() + scale_color_aaas() + 
    theme(text=element_text(size=25,  family="serif"))
}




DF.TJ <-function(A){
  mod <-lmer(A ~ RT*ID +(1|REP), 
             data=dat)
  mod %>% anova(ddf="Kenward-Roger")
  C <- mod %>%
    emmeans(specs = ~ RT:ID, 
            lmer.df = "kenward-roger") %>% 
    cld(reversed=T, Letters=LETTERS, adjust="none") # add compact letter display
  C
  pd <- position_dodge(0.2)
  ggplot(C, aes(x=RT, y=emmean, color= ID, group=ID)) + 
    geom_point(size = 2,position=pd)+
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2, position=pd) +
    geom_text(aes(y=upper.CL+1, label = .group),position=pd)  + 
    theme_bw() + scale_color_aaas() + 
    theme(text=element_text(size=25,  family="serif"))
}

#PDWG  
MOD(dat$PDWG) #only rate, ID effects
DF.TJ(dat$PDWG)
SF.TJID(dat$PDWG)
SF.TJRT(dat$PDWG)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/PDWG_RT.pdf"
      )
saveRDS(SF.TJRT(dat$PDWG), file = "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/PDWG_RT")

#BFWG
MOD(dat$BFWG) #only rate, ID, interaction effects
DF.TJ(dat$BFWG)
SF.TJID(dat$BFWG)
SF.TJRT(dat$BFWG)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BFWG_RT.pdf"
)


#BDWG  
MOD(dat$BDWG)  #Only rate, ID, interaction effects
DF.TJ(dat$BDWG)
SF.TJID(dat$BDWG)
SF.TJRT(dat$BDWG)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BDWG_RT.pdf"
)
saveRDS(SF.TJRT(dat$BDWG), file = "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/BDWG_RT")


#PNP
MOD(dat$PNP)  #only rate effects
DF.TJ(dat$PNP)
SF.TJRT(dat$PNP)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/PNP_RT.pdf"
)

#PNP_total
MOD(dat$PNP_total) #rate effects
DF.TJ(dat$PNP_total)
SF.TJRT(dat$PNP_total)

#BNP
MOD(dat$BNP) #only rate(BC) effects found
SF.TJID(dat$BNP)
SF.TJRT(dat$BNP)
DF.TJ(dat$BNP)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BNP_DF.pdf"
)

#BNP_total
MOD(dat$BNP_total) #rate effects
SF.TJID(dat$BNP_total)
SF.TJRT(dat$BNP_total)
DF.TJ(dat$BNP_total)

#P_total
MOD((dat$PNP_total + dat$BNP_total))
DF.TJ((dat$PNP_total + dat$BNP_total))
saveRDS(DF.TJ((dat$PNP_total + dat$BNP_total)), file = "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/P_Total_DF")

#P_efficiency
MOD(dat$P_efficiency)

#PNMG
MOD(dat$PNMG) #rate(LBC), ID, ID:RT  effects found 
SF.TJRT(dat$PNMG)
SF.TJID(dat$PNMG)
DF.TJ(dat$PNMG)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/PNMG_DF.pdf"
)

#PNMG_total
MOD(dat$PNMG_total) #rate, ID, ID:RT
SF.TJRT(dat$PNMG_total)
SF.TJID(dat$PNMG_total)
DF.TJ(dat$PNMG_total)

#BNMG
MOD(dat$BNMG) #only ID effects found
SF.TJRT(dat$BNMG)
DF.TJ(dat$BNMG)
SF.TJID(dat$BNMG)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BNMG_ID.pdf"
)

#BNMG_total
MOD(dat$BNMG_total) #rate, ID, ID:RT
SF.TJRT(dat$BNMG_total)
DF.TJ(dat$BNMG_total)
SF.TJID(dat$BNMG_total)

#MG_total
MOD((dat$PNMG_total + dat$BNMG_total))
SF.TJID((dat$PNMG_total + dat$BNMG_total))
SF.TJRT((dat$PNMG_total + dat$BNMG_total))
DF.TJ((dat$PNMG_total + dat$BNMG_total))
saveRDS(DF.TJ((dat$PNMG_total + dat$BNMG_total)), file = "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/PNMG_Total_DF")


#PNCA
MOD(dat$PNCA)  #ID effects, TSP higher
DF.TJ(dat$PNCA)
SF.TJRT(dat$PNCA)
SF.TJID(dat$PNCA)

#PNCA_total
MOD(dat$PNCA_total) #rate effects
DF.TJ(dat$PNCA_total)
SF.TJRT(dat$PNCA_total)
SF.TJID(dat$PNCA_total)

#BNCA
MOD(dat$BNCA) #rate(1x C,LBC) and ID(C,LBC) effects found
SF.TJRT(dat$BNCA)
SF.TJID(dat$BNCA)
DF.TJ(dat$BNCA)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BNCA_DF.pdf"
)

#BNCA_total
MOD(dat$BNCA_total) #ID, rate effects
SF.TJRT(dat$BNCA_total)
SF.TJID(dat$BNCA_total)
DF.TJ(dat$BNCA_total)

#CA_total
MOD((dat$PNCA_total + dat$BNCA_total))
SF.TJID((dat$PNCA_total + dat$BNCA_total))
SF.TJRT((dat$PNCA_total + dat$BNCA_total))
DF.TJ((dat$PNCA_total + dat$BNCA_total))

#PNK flag for review
MOD(dat$PNK) #both rate and ID effects found
DF.TJ(dat$PNK)
SF.TJRT(dat$PNK)
SF.TJID(dat$PNK)

#PNK_total
MOD(dat$PNK_total) #ID:RT effects
DF.TJ(dat$PNK_total)
SF.TJRT(dat$PNK_total)
SF.TJID(dat$PNK_total)

#BNK
MOD(dat$BNK) #ID(1x C,BC):RT(LBC) 
SF.TJRT(dat$BNK)
SF.TJID(dat$BNK)
DF.TJ(dat$BNK)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BNK_DF.pdf"
)

#BNK_total
MOD(dat$BNK_total) #rate effects
DF.TJ(dat$BNK_total)
SF.TJRT(dat$BNK_total)
SF.TJID(dat$BNK_total)

#K_total
MOD((dat$BNK_total + dat$PNK_total))
SF.TJRT((dat$BNK_total + dat$PNK_total))
DF.TJ((dat$BNK_total + dat$PNK_total))

#BNK:MG
MOD(dat$'BNK:MG')
DF.TJ(dat$`BNK:MG`)
SF.TJRT(dat$`BNK:MG`)
SF.TJID(dat$`BNK:MG`)

#PNK:MG
MOD(dat$'PNK:MG')
DF.TJ(dat$`PNK:MG`)
SF.TJRT(dat$`PNK:MG`)
SF.TJID(dat$`PNK:MG`)

#PNZN
MOD(dat$PNZN)  #no effects
DF.TJ(dat$PNZN)
SF.TJRT(dat$PNZN)

#PNZN_total (Ignore total numbers, lots of outliers)
MOD(dat$PNZN_total) #rate effect
SF.TJRT(dat$PNZN_total)
SF.TJID(dat$PNZN_total)


#BNZN
MOD(dat$BNZN) #no effects found
SF.TJID(dat$BNZN)
SF.TJRT(dat$BNZN)
DF.TJ(dat$BNZN)

#BNZN_total
MOD(dat$BNZN_total) #rate effect, ID effect subsig
SF.TJID(dat$BNZN_total)
SF.TJRT(dat$BNZN_total)
DF.TJ(dat$BNZN_total)

#Total Zn uptake 
MOD((dat$PNZN_total + dat$BNZN_total)) #rate and ID effect
SF.TJID((dat$PNZN_total + dat$BNZN_total))
SF.TJRT((dat$PNZN_total + dat$BNZN_total))
DF.TJ((dat$PNZN_total + dat$BNZN_total))


#PNFE
MOD(dat$PNFE) #no  effects found 
SF.TJRT(dat$PNFE)
SF.TJID(dat$PNFE)
DF.TJ(dat$PNFE)

#PNFE_total
MOD(dat$PNFE_total) #no effect
SF.TJRT(dat$PNFE)
SF.TJID(dat$PNFE)
DF.TJ(dat$PNFE)

#BNFE
MOD(dat$BNFE) #rate effects found
DF.TJ(dat$BNFE)
SF.TJID(dat$BNFE)
SF.TJRT(dat$BNFE)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BNFE_RT.pdf"
)

#BNFE_total
MOD(dat$BNFE_total) #rate effect, ID subsig
DF.TJ(dat$BNFE_total)
SF.TJID(dat$BNFE_total)
SF.TJRT(dat$BNFE_total)

#Fe Total
MOD((dat$PNFE_total + dat$BNFE_total)) #no effect
SF.TJID((dat$PNFE_total + dat$BNFE_total))
SF.TJRT((dat$PNFE_total + dat$BNFE_total))
DF.TJ((dat$PNFE_total + dat$BNFE_total))


#PNB
MOD(dat$PNB)  #no effects
DF.TJ(dat$PNB)
SF.TJRT(dat$PNB)
SF.TJID(dat$PNB)

#PNB_total
MOD(dat$PNB_total) #rate effect
DF.TJ(dat$PNB_total)
SF.TJRT(dat$PNB_total)
SF.TJID(dat$PNB_total)


#BNB
MOD(dat$BNB) #no effects found
SF.TJRT(dat$BNB)
SF.TJID(dat$BNB)
DF.TJ(dat$BNB)

#BNB_total
MOD(dat$BNB_total) #rate effect, ID effect
SF.TJRT(dat$BNB_total)
SF.TJID(dat$BNB_total)
DF.TJ(dat$BNB_total)

#B Total
MOD((dat$PNB_total + dat$BNB_total)) #rate effect
SF.TJID((dat$PNB_total + dat$BNB_total))
SF.TJRT((dat$PNB_total + dat$BNB_total))
DF.TJ((dat$PNB_total + dat$BNB_total))


#PNMN 
MOD(dat$PNMN) #No effect
DF.TJ(dat$PNMN)
SF.TJRT(dat$PNMN)
SF.TJID(dat$PNMN)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/PNMN_ID.pdf"
)

#PNMN_total
MOD(dat$PNMN_total) #rate effect
DF.TJ(dat$PNMN_total)
SF.TJRT(dat$PNMN_total)
SF.TJID(dat$PNMN_total)

#BNMN
MOD(dat$BNMN) #rate, ID, Interaction  effect 
SF.TJRT(dat$BNMN)
SF.TJID(dat$BNMN)
DF.TJ(dat$BNMN)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BNMN_DF.pdf"
)

#BNMN_total
MOD(dat$BNMN_total) #rate, ID effect
SF.TJRT(dat$BNMN_total)
SF.TJID(dat$BNMN_total)
DF.TJ(dat$BNMN_total)

#MN Total
MOD((dat$PNMN_total + dat$BNMN_total)) #rate effect
SF.TJID((dat$PNMN_total + dat$BNMN_total))
SF.TJRT((dat$PNMN_total + dat$BNMN_total))
DF.TJ((dat$PNMN_total + dat$BNMN_total))


#PNCU 
MOD(dat$PNCU) #ID, rate, interaction
DF.TJ(dat$PNCU)
SF.TJRT(dat$PNCU)
SF.TJID(dat$PNCU)

#PNCU_total
MOD(dat$PNCU_total) #rate effect
DF.TJ(dat$PNCU_total)
SF.TJRT(dat$PNCU_total)
SF.TJID(dat$PNCU_total)


#BNCU
MOD(dat$BNCU) #rate effect 
SF.TJRT(dat$BNCU)
SF.TJID(dat$BNCU)
DF.TJ(dat$BNCU)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/BNCU_DF.pdf"
)

#BNCU_total
MOD(dat$BNCU_total) #rate, ID effect
SF.TJRT(dat$BNCU_total)
SF.TJID(dat$BNCU_total)
DF.TJ(dat$BNCU_total)

#CU Total
MOD((dat$PNCU_total + dat$BNCU_total)) #no effect
SF.TJID((dat$PNCU_total + dat$BNCU_total))
SF.TJRT((dat$PNCU_total + dat$BNCU_total))
DF.TJ((dat$PNCU_total + dat$BNCU_total))


#Plant height
MOD(dat$PH2)
MOD(dat$PH3)

MOD(dat$PH4) #rate, ID effect found
SF.TJID(dat$PH4)
SF.TJRT(dat$PH4)
DF.TJ(dat$PH4)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/PH4_DF.pdf"
)

MOD(dat$PH5) #rate(LBC, 0x,2x), interaction effect found
SF.TJID(dat$PH5)
SF.TJRT(dat$PH5)
DF.TJ(dat$PH5)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/PH5_DF.pdf"
)

MOD(dat$PH6) #rate(LBC, 0x,2x), interaction effect found
SF.TJID(dat$PH6)
SF.TJRT(dat$PH6)
DF.TJ(dat$PH6)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/PH6_DF.pdf"
)


#Chlorophyll index
MOD(dat$PC2) #rate and ID effects
DF.TJ(dat$PC2)

MOD(dat$PC3)

MOD(dat$PC4) #rate and interaction effects
DF.TJ(dat$PC4)

MOD(dat$PC5) #ID effects found
DF.TJ(dat$PC5)

MOD(dat$PC6) #rate, ID  effect found
#Mg rate is higher in LBC yet chrolophyll index shows same or even lower in some case
#why?
#Because, shorter leaves, height, weight, biomass concentrated chlorophyll
#more chlorophyll bc cant make pi molecules like Rubisco, that need phosphorous and essential for phtosynthesis
SF.TJID(dat$PC6)
SF.TJRT(dat$PC6)
DF.TJ(dat$PC6)
ggsave(file= "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/PC6_DF.pdf"
)
saveRDS(SF.TJRT(dat$PC6), file = "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/PC6_RT")

MOD((dat$PC4 - dat$PC6))
DF.TJ((dat$PC4 - dat$PC6))

#Soil residual P
MOD(dat$SRCEC)
DF.TJ(dat$SRCEC)
SF.TJRT(dat$SRCEC)
SF.TJID(dat$SRCEC)

#Soil residual pH

#PMgMn ratio
MOD(dat$PMgMn)
DF.TJ(dat$PMgMn)
SF.TJID(dat$PMgMn)

MOD(dat$P_efficiency)

cor.test(dat$BNK, dat$BNCA, method = "pearson")

model <- lm(BNCA ~ BNK, data = dat)
summary(model)

library(ggplot2)
ggplot(dat, aes(x = BNK, y = BNCA)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "K vs Ca Concentration in Plant Tissue",
       x = "K Concentration (µg/g)",
       y = "Ca Concentration (µg/g)")

model <- lm(PNCA ~ PNMG + PNK, data = dat)
summary(model)

#Soil residual MG
MOD(dat$SRMg)
DF.TJ(dat$SRMg)


#Soil residual pH
MOD(dat$SRpH - 5.6)
DF.TJ(dat$SRpH - 5.6)


#Figures for supplement

#*Fig 1. chlorophyll indices mean by rate
#The chlorophyll concentrations were significantly higher in plants treated with lower P-rates 
#after 14 (p = 2.028e-02), 28 (p = 3.747e-02), and 42 days (p = 5.086e-03) (insert one PC rate fig).

#*Fig 2. a) plant dry weight b) beans dry weight c) P uptake bean d) P uptake plant by P rates
#plants without phosphorus amendments accumulated significantly lower biomass,  as indicated by 
#plant dry weight (p = 4.983e-07) and bean dry weight measurements (p = 1.46e-06) 
#(insert PDWG, BDWG rate fig, Fig. 6[A,C]). 

#The P uptake increased for both beans( p = 6.825e-04) and the remaining plant tissue (p = 2.125e-04) 
#with increasing rates of P amendment across different treatment types (TSP, BC + TSP, P-LDH/BC)(Insert rate figure). 



#Fig. 3. a) Total Mg uptake b) Total Mn uptake c) Bean Mg uptake d) Bean Mn uptake interaction figure
#The total Mg uptakes were impacted by an interaction of both treatment and rate effects (p = 7.611e-03), 
#with the plants treated with P-LDH/BC at 100.88 kg (P2O5) ha-1 having the highest uptake. (Insert interaction figure). 


#The manganese uptake trend was antagonistic to magnesium uptake trend in beans reported in the previous section 
#(interaction fig in supplement). 

#rate panel:
#A)PC6 42 days
#B) PDWG
#C) BDWG

PC6_RT <-
  readRDS("/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/PC6_RT") +
  labs(
    x = NULL,
    y = "[A] Chlorophyll Index") +
  theme(
    axis.title.y = element_text(size = 34, face = "bold"),
    axis.text = element_text(size = 26, face = "bold"))

# Increase size and make bold
PC6_RT$layers[[3]]$aes_params$size <- 8        # larger text
PC6_RT$layers[[3]]$aes_params$fontface <- "bold" # bold letters
PC6_RT$layers[[1]]$aes_params$size <- 4
PC6_RT$layers[[2]]$aes_params$size <- 2
PC6_RT$layers[[2]]$aes_params$width <- 0.4

PDWG_RT<- 
  readRDS("/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/PDWG_RT") +
  labs(
    x = NULL,
    y = "[B] Plant Dry Weight (g)") +
  theme(
    axis.title.y = element_text(size = 34, face = "bold"),
    axis.text = element_text(size = 26, face = "bold"))
# Increase size and make bold
PDWG_RT$layers[[3]]$aes_params$size <- 8        # larger text
PDWG_RT$layers[[3]]$aes_params$fontface <- "bold" # bold letters
PDWG_RT$layers[[1]]$aes_params$size <- 4
PDWG_RT$layers[[2]]$aes_params$size <- 2
PDWG_RT$layers[[2]]$aes_params$width <- 0.4


BDWG_RT<- 
  readRDS("/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/BDWG_RT") +
  labs(
    x = NULL,
    y = "[C] Bean Dry Weight (g)") +
  theme(
    axis.title.y = element_text(size = 34, face = "bold"),
    axis.text = element_text(size = 26, face = "bold"))
# Increase size and make bold
BDWG_RT$layers[[3]]$aes_params$size <- 8        # larger text
BDWG_RT$layers[[3]]$aes_params$fontface <- "bold" # bold letters
BDWG_RT$layers[[1]]$aes_params$size <- 4
BDWG_RT$layers[[2]]$aes_params$size <- 2
BDWG_RT$layers[[2]]$aes_params$width <- 0.4


combined_rate_plots <- 
  cowplot::plot_grid(
  PC6_RT,
  PDWG_RT,
  BDWG_RT,
  ncol =1,
  align = "hv"
  )+
  theme(plot.margin = margin(t = 5, r = 5, b = 50, l = 12)
        )

final_rate_plot <- 
  ggdraw(combined_rate_plots) +
  draw_label(
    expression(bold("Rate of " * P[2]*O[5] * " ha"^-1)),  # the x-axis label
    x = 0.5,      # centered horizontally
    y = 0.015,     # distance from bottom (adjust as needed)
    vjust = 0.5,  # vertical justification
    angle = 0,    # horizontal text
    size = 28,
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)  # white background
  )

ggsave(
  filename = "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/final_rate_plot.png",
  plot = final_rate_plot,
  width = 12,
  height = 26,
  units = "in",
  device = "png"
)

#interaction panel:
#A) P uptake total interaction
#B)Mg total

P_Total_DF <-
  readRDS("/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/P_Total_DF") +
  labs(
    x = NULL,
    y = expression(bold("[A] Rate of P Uptake (ug g"^-1 *")"))) +
  theme(
    axis.title.y = element_text(size = 34, face = "bold"),
    axis.text = element_text(size = 26, face = "bold"),
    legend.position = "none")

# Increase size and make bold
P_Total_DF$layers[[3]]$aes_params$size <- 8        # larger text
P_Total_DF$layers[[3]]$aes_params$fontface <- "bold" # bold letters
P_Total_DF$layers[[3]]$position <- position_dodge(width = 0.8)
P_Total_DF$layers[[1]]$aes_params$size <- 4
P_Total_DF$layers[[1]]$position <- position_dodge(width = 0.5)
P_Total_DF$layers[[2]]$aes_params$size <- 2
P_Total_DF$layers[[2]]$aes_params$width <- 0.4
P_Total_DF$layers[[2]]$position <- position_dodge(width = 0.5)


PNMG_Total_DF <-
  readRDS("/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/PNMG_Total_DF") +
  labs(
    x = NULL,
    y = expression(bold("[B] Rate of Mg Uptake (ug g"^-1 *")"))) +
  theme(
    axis.title.y = element_text(size = 34, face = "bold"),
    axis.text = element_text(size = 26, face = "bold"),
    legend.position = "none")

# Increase size and make bold
PNMG_Total_DF$layers[[3]]$aes_params$size <- 8        # larger text
PNMG_Total_DF$layers[[3]]$aes_params$fontface <- "bold" # bold letters
PNMG_Total_DF$layers[[3]]$position <- position_dodge(width = 0.8)
PNMG_Total_DF$layers[[3]]$mapping$y <- quote(upper.CL + 2)  # move letters a little higher
PNMG_Total_DF$layers[[1]]$aes_params$size <- 4
PNMG_Total_DF$layers[[1]]$position <- position_dodge(width = 0.5)
PNMG_Total_DF$layers[[2]]$aes_params$size <- 2
PNMG_Total_DF$layers[[2]]$aes_params$width <- 0.4
PNMG_Total_DF$layers[[2]]$position <- position_dodge(width = 0.5)


combined_interaction_plots <- 
  cowplot::plot_grid(
    P_Total_DF,
    PNMG_Total_DF,
    nrow  =1,
    align = "hv"
  )+
  theme(plot.margin = margin(t = 7, r = 7, b = 50, l = 12)
  )

#Modifying the legend title and labels
legend.DF<-
  readRDS("/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/PNMG_Total_DF") +
  labs(color = "Treatment Type") +  # sets legend title
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 34, face = "bold"),
    legend.text  = element_text(size = 32, face = "bold")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size = 4,        # legend points
        linetype = 1,    # solid lines
        stroke = 1       # error bar thickness
      ),
      keywidth = 5
    )
  )

#Extracting the legend
legend.DF <- get_legend(legend.DF)

# Combine the plots with the legend below
combined_with_legend <- plot_grid(
  combined_interaction_plots,   # the 3-panel figure
  legend.DF,                       # the extracted legend
  ncol = 1,                     # stack vertically
  rel_heights = c(1, 0.1)       # adjust space: 0.1 for legend height
)

# Add the shared x-axis label below everything
final_interaction_plot <- ggdraw(combined_with_legend) +
  draw_label(
    expression(bold("Rate of " * P[2]*O[5] * " ha"^-1)),  # shared x-axis label
    x = 0.5,
    y = 0.12,  # adjust depending on legend height
    size = 34
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )


ggsave(
  filename = "/Users/tj/Library/CloudStorage/GoogleDrive-tajindersi227@gmail.com/My Drive/Research/TJ - LDH Project/Analysis/supplementary figure/final_interaction_plot.png",
  plot = final_interaction_plot,
  width = 20,
  height = 12,
  units = "in",
  device = "png"
)
