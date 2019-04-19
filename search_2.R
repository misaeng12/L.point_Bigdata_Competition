library(tidyverse)

final <- read.csv("final.csv")
product <- read.csv("lpoint1.csv")
master <- read.csv("lpoint6.csv")

master_brand <- merge(product, master, by="PD_C") %>%
  select(PD_C, PD_NM, PD_BRA_NM, CLAC1_NM:CLAC3_NM) %>% filter(!duplicated(PD_C))

write.csv(master_brand, "master_brand.csv", row.names=F, fileEncoding="utf-8")



#### brand name으로 분류 ---------------------------------------- ####

mysearch1_2 <- read.csv("mysearch1_2.csv")
mysearch1_4 <- read.csv("mysearch1_4.csv")
master_brand2 <- read.csv("master_brand2.csv")

mysearch1 <- rbind(anti_join(mysearch1_2, mysearch1_4, by=KWD), mysearch1_4)
mysearch1_brand <- filter(mysearch1, myCLAC1_NM=="" & myCLAC2_NM=="" & myCLAC3_NM=="" & myBRD!="")

brand_clac <- group_by(master_brand2, PD_BRA_NM, CLAC1_NM, CLAC2_NM, CLAC3_NM) %>% count
brand_clac_CNT <- group_by(master_brand2, PD_BRA_NM) %>%
  summarise( myCLAC1_CNT = length(unique(CLAC1_NM)),
             myCLAC2_CNT = length(unique(CLAC2_NM)),
             myCLAC3_CNT = length(unique(CLAC3_NM)) )

mysearch1_brabd <- filter(brand_clac_CNT, myCLAC2_CNT==1) %>%
  left_join(brand_clac[!duplicated(brand_clac$PD_BRA_NM),]) %>% select(PD_BRA_NM, CLAC2_NM) %>%
  right_join(mysearch1_brand) %>%
  select(KWD, PD_BRA_NM, myGROUP:myCLAC1_NM, myCLAC2_NM=CLAC2_NM, myCLAC3_NM)

mysearch1_brand <- filter(brand_clac_CNT, myCLAC1_CNT==1) %>%
  left_join(brand_clac[!duplicated(brand_clac$PD_BRA_NM),]) %>% select(PD_BRA_NM, CLAC1_NM) %>%
  right_join(mysearch1_brand) %>%
  select(KWD, PD_BRA_NM, myGROUP:myKWD, myCLAC1_NM=CLAC1_NM, myCLAC2_NM:myCLAC3_NM)

mysearch1 <- rbind(anti_join(mysearch1, mysearch1_brand, by=KWD), mysearch1_brand)



#### clac3 -> clac2로 분류 -------------------------------------- ####

mymaster <- group_by(master_brand, CLAC3_NM, CLAC2_NM, CLAC1_NM) %>% count %>% select(CLAC1_NM:CLAC3_NM)

mysearch1_bi2uni <- read.csv("mysearch1_bi2uni.csv", fileEncoding="utf-8")


mysearch1_a <- filter(mysearch1_bi2uni, myCLAC2_NM != "" | myCLAC3_NM == "")
mysearch1_b <- anti_join(mysearch1_bi2uni, mysearch1_a, by=KWD) %>%
  left_join(mymaster, by=c("myCLAC3_NM"="CLAC3_NM")) %>%
  select(KWD:myKWD, myCLAC1_NM, myCLAC2_NM=CLAC2_NM, myCLAC3_NM)

mysearch1_final <- rbind(mysearch1_a, mysearch1_b) %>%
  right_join(search1, by=c("KWD_NM"="KWD")) %>% select(-myCLAC3_NM)

1 - nrow(filter(mysearch1_final, myCLAC2_NM==""))/nrow(mysearch1_final)   # 분류 비율



#### count -------------------------------------------------- ####

mysearch1 <- mysearch1_final

search1_week <- left_join(mysearch1, final[!duplicated(final$SESS_ID), c("SESS_ID", "myWEEK")]) %>%
  select(SESS_ID, myWEEK, KWD_NM, SEARCH_CNT, myBRD, myCLAC1_NM, myCLAC2_NM)

search1_final <- filter(search1_week, myCLAC2_NM!="") %>% group_by(myCLAC2_NM, myWEEK) %>% count %>%
  select(CLAC2_NM=myCLAC2_NM, myWEEK, mySEARCH1=n) %>%
  filter(complete.cases(search1_final))



##### final search count & plot ---------------------------------------- #####

count <- final %>% group_by(myCLAC_NM, CLAC2_NM, myWEEK) %>% count
write.csv(count, "count.csv", row.names=F, fileEncoding="utf-8")

x <- count$myWEEK[1:26]
df1 <- data.frame(n = filter(count, CLAC2_NM=="여성의류아우터")$n,
                  group = rep(c("fitted", "true"), c(26, 18)), x = x)

ggplot(data=df1, aes(x=x, y=n, group=group)) + geom_line(aes(color=group, linetype=group), size=1.5) +
  theme_light() + theme(axis.text.x = element_text(angle=45, hjust=1), legend.title=element_blank()) +
  scale_color_manual(values=c("#318CE7", "#FF6961"), labels=c("true","fitted")) + 
  scale_linetype_discrete(labels=c("true","fitted")) + labs(x="", y="구매건수") + ylim(500, 5000)

