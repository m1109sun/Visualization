# data 수정

pres_elec <- data.frame(data.table::fread("presidential_election.csv"))
election_info <- pres_elec[, c(1:6, 20:22)] # 22214 * 9
personal <- pres_elec[, c(7:19)] # 22214 * 13

per_name <- c("문재인", "홍준표", "안철수", "유승민", "심상정", "조원진", "오영국", "장성민", "이재오", "김선동", "이경희", "윤홍식", "김민찬")

dat_combine <- list()
for(i in 1:13) {
  name <- per_name[i]
  dat_combine[[i]] <- data.frame(election_info, p = personal[, i])
  dat_combine[[i]]$rate <- dat_combine[[i]]$p / dat_combine[[i]]$투표수
  colnames(dat_combine[[i]])[colnames(dat_combine[[i]]) == "p"] <- per_name[i]
  dat_combine[[i]] <- reshape2::melt(dat_combine[[i]], id.vars = c(1:9, 11), measure.vars = 10,
                                     variable.name = "name",
                                     value.name = "count")
}

elec <- data.frame(data.table::rbindlist(dat_combine)) # dim : 288782 * 12

# 문재인 후보 데이터만 뽑기
library(dplyr)
moon <- elec %>%
  filter(name == "문재인" & 읍면동명 == "합계") %>%
  select("시도명", "구시군명", "rate") # dim : 250 * 3

# 지도데이터 불러오기
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

korea <- raster::shapefile("TL_SCCO_SIG.shp") # shp 파일을 불러옴
korea <- spTransform(korea, CRS("+proj=longlat")) # 우리가 아는 위도, 경도의 좌표계가 설정되어 있지 않음. 좌표계를 설정해줌
korea <- fortify(korea, region = "SIG_CD") # shapefile 형태를 dataframe 형태로 바꾸는 것

# 행정동 코드 불러옴
region_code <- data.frame(data.table::fread("region_code.csv"))
region_code$행정동코드 <- as.character(region_code$행정동코드)
region_code$행정동코드 <- str_sub(region_code$행정동코드, 1, 5)
region_code <- unique(region_code[, 1:3])

unique(region_code$시도명)
unique(moon$시도명)
setdiff(unique(moon$시도명), region_code$시도명)

region_code$시군구명 <- str_replace_all(region_code$시군구명, fixed(" "), "")
moon$구시군명 <- str_replace_all(moon$구시군명, fixed(" "), "")

unique(moon$구시군명)
setdiff(unique(moon$구시군명), unique(region_code$시군구명)) # sejong 36110

region_code[which(region_code$시도명 == "세종특별자치시"), ]
sejong <- data.frame(행정동코드 = "36110", 시도명 = "세종특별자치시", 시군구명 = "세종특별자치시")
region_code <- rbind(region_code, sejong)

colnames(moon)[2] <- "시군구명"

dat <- merge(moon, region_code, by = c("시도명", "시군구명"))

# korea랑 dat를 합쳐보자
colnames(korea)[6] <- "행정동코드"
intersect(dat$행정동코드, region_code$행정동코드)
map_dat <- merge(korea, dat, by = "행정동코드")

map_plot <- ggplot() + geom_polygon(data = map_dat, aes(x = long, y = lat, group = group, fill = rate), color = "white")

library(viridis)
map_plot + scale_fill_viridis(direction = -1, option = "C")
