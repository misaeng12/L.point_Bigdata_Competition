library(tidyverse)

final <- read.csv("final.csv")
final <- filter(final, !is.na(PD_BRA_NM), SESS_DT != 2018-09-30)


# '유아동' 관련 항목 재분류

levels(final$CLAC1_NM) <- c(levels(final$CLAC1_NM), c("유아동가구", "유아동속옷/양말/홈웨어", "유아동스포츠패션"))

for(i in 1:nrow(final)){
  
  if ( final[i, "CLAC2_NM"] %in% c("유아동속옷", "유아동양말류", "유아동화") ) {
    final[i, "CLAC1_NM"] <- "유아동속옷/양말/홈웨어"
  } else if ( final[i, "CLAC2_NM"] %in% c("유아동스포츠화", "유아동일반스포츠의류") ) {
    final[i,"CLAC1_NM"] <- "유아동스포츠패션"
  } else if ( final[i, "CLAC2_NM"] == "유아동가구" ) {
    final[i,"CLAC1_NM"] <- "유아동가구"
  }
  
}

write.csv(final, "final.csv", row.names=F, filenEncoding="utf=8")



#### rebuy ----------------------------------------------- ####

# 6개월 간 재구매율

rebuy <- final %>% distinct(CLNT_ID, CLAC2_NM, SESS_ID) %>% group_by(CLNT_ID, CLAC2_NM) %>% count() %>%
  arrange(CLNT_ID) %>% mutate(obs = 1:n(), repurchase = ifelse(obs==1, n-1, n)) %>%
  group_by(CLAC2_NM) %>% summarise(myREBUY = sum(repurchase, na.rm=T))

mycount <- group_by(final, CLAC2_NM) %>% count
rebuy_p <- left_join(rebuy, mycount) %>% mutate(myREBUY_P = myREBUY/n)

cor(rebuy_p$myREBUY_P, rebuy_p$n)


# 주별 재구매율

rebuy_week <- final %>% distinct(CLNT_ID, CLAC2_NM, SESS_ID, myWEEK) %>%
  group_by(CLNT_ID, CLAC2_NM, myWEEK) %>% count() %>%
  group_by(CLNT_ID, CLAC2_NM) %>% arrange(CLNT_ID, myWEEK) %>%
  mutate(obs = 1:n(), repurchase = ifelse(obs==1, n-1, n)) %>%
  group_by(CLAC2_NM, myWEEK) %>% summarise(myREBUY_WEEK = sum(repurchase, na.rm=T))

rebuy_week_p <- left_join(rebuy_week, select(count, -myCLAC_NM)) %>% mutate(myREBUY_WEEK_P = myREBUY_WEEK/n)
cor(rebuy_week_p$myREBUY_WEEK_P, rebuy_week_p$n)



#### popular brand ----------------------------------------------- ####

final <- read.csv("final(brand_preprocessed).csv", fileEncoding="utf-8")

best <- c()
for(clac_nm in unique(final$CLAC1_NM)){
  sort <- filter(final, CLAC1_NM==clac_nm) %>% select(PD_C, PD_BRA_NM) %>%
    group_by(PD_BRA_NM) %>% count() %>% arrange(desc(n))
  best <- c(best, as.character(sort$PD_BRA_NM[1:as.integer(nrow(sort)*0.05)]))
}

master_brand$myPOP_BR <- 0
master_brand[master_brand$PD_BRA_NM %in% best, "myPOP_BR"] <- 1

write.csv(master_brand, "master_pop_br.csv", row.names=F, fileEncoding ="utf-8")

