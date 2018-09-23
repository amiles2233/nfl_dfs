library(ffanalytics)
library(lpSolve)

### Week 1
scrape <- scrape_data(src = c("ESPN", "FantasyPros", "FantasySharks", "CBS",
                              "NumberFire", "Yahoo", "NFL"),
                      pos=c('QB', 'RB', 'WR', 'TE', 'DST'),
                      season = 2018, 
                      week=2)


#saveRDS(scrape, 'wk1scrape.RDS')

scoring <- list(
  pass = list(
    pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
    pass_int = -1, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
    pass_400_yds = 0
  ),
  rush = list(
    all_pos = TRUE,
    rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
    rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
  rec = list(
    all_pos = TRUE,
    rec = .5, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
    rec_150_yds = 0, rec_200_yds = 0
  ),
  misc = list(
    all_pos = TRUE,
    fumbles_lost = -1, fumbles_total = 0,
    sacks = 0, two_pts = 2
  ),
  ret = list(
    all_pos = TRUE,
    return_tds = 0, return_yds = 0
  ),
  dst = list(
    dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
    dst_blk = 2, dst_ret_yds = 0, dst_pts_allowed = 0
  ),
  pts_bracket = list(
    list(threshold = 0, points = 10),
    list(threshold = 1, points = 7),
    list(threshold = 7, points = 4),
    list(threshold = 14, points = 1),
    list(threshold = 21, points = 0),
    list(threshold = 28, points = -1),
    list(threshold = 35, points = -4)
  )
)


proj <- projections_table(scrape, scoring_rules = scoring) %>%
  add_player_info() %>% add_ecr() %>% add_risk()


#saveRDS(proj, "wk1proj.RDS")


## Make Optimum Lineup
#proj <- readRDS('wk1proj.RDS')

sal <- read_csv('FDSalaries.csv')

pred_sal <- proj %>% 
  filter(avg_type=='robust') %>%
  #mutate(Name = ifelse(pos=="DST", last_name, paste(first_name, last_name))) %>%
  mutate(Name = paste(first_name, last_name)) %>%
  inner_join(sal, by=c("Name"="Nickname")) %>%
  select(Name, team, position, points, Salary) %>%
  filter(!is.na(points), !is.na(Salary)) %>%
  group_by(Name) %>%
  mutate(sal_max=max(Salary)) %>%
  filter(Salary==sal_max)


## Points Model
obj <- pred_sal$points

mat <- rbind(t(model.matrix(~ position + 0,pred_sal)), rep(1, nrow(pred_sal)), pred_sal$Salary)

dir <- c("=","=","<=","=","<=", "=","<=")
rhs <- c(1,1,3,1,4,9,60000)

result <- lp("max", obj, mat, dir, rhs, all.bin = TRUE)	

results <- pred_sal[which(result$solution == 1),]

results

sum(results$points)

arrange(results, position, -points)



