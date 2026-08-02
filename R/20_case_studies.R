# =============================================================================
# 정성 케이스 스터디용 데이터셋 정제 — 이정현(MVP 시즌 선수카드) / 알바노(SK전 버저비터 2경기)
# -----------------------------------------------------------------------------
# 조원이 바로 열어서 정량+정성 분석할 수 있게, 기존 데이터를 종합해 새 CSV로 만든다.
# 산출물(모두 ../docs/case_data/):
#   case_lee_shots.csv            이정현 3시즌 야투 slot-level(레버리지·기대초과·클러치·상대팀)
#   case_lee_season_summary.csv   이정현 시즌 요약카드(슈팅스플릿·클러치기록·리그백분위)
#   case_alvano_events.csv        알바노 2경기 전체 이벤트 시퀀스 + 승리확률(WP) 추이
#   case_alvano_shots.csv         그 2경기의 모든 야투(레버리지 포함) + 버저비터 플래그
#   case_top_moments_2025_26.csv  2025-26 최고 레버리지 슛 top 30 (맥락용)
#   ../docs/img/wp_alvano_*.png   두 경기 WP 추이 그래프(영문·흰배경)
#   ../docs/case_study_guide.md   조원용 가이드라인
# 입력: KBL_pooled_shots_scored.csv, KBL_pooled_wp_model.rds, KBL_<season>_state_table.csv
# =============================================================================
library(dplyr); library(stringr); library(readr); suppressMessages(library(ggplot2))
suppressWarnings(Sys.setlocale("LC_CTYPE","C.UTF-8"))
OUT <- "../docs/case_data"; dir.create(OUT, showWarnings=FALSE, recursive=TRUE)
IMG <- "../docs/img"; dir.create(IMG, showWarnings=FALSE, recursive=TRUE)
norm <- function(x) sub("\\.0$","",trimws(as.character(x)))
SEASONS <- c("2023_24","2024_25","2025_26")
LEE <- "LEE JUNG HYUN"; ALV <- "Ethan Alvano"; ALV_GAMES <- c("S47G01N72","S47G01N97")

mdl <- readRDS("KBL_pooled_wp_model.rds"); sigma<-mdl$sigma; beta<-mdl$beta
wpf <- function(margin, t_eff, poss) pnorm((margin + beta*poss)/(sigma*sqrt(pmax(t_eff,0.5))))

# ── 경기→팀 맵 (상대팀·홈원정 표기용) ───────────────────────────────────────
gmap <- bind_rows(lapply(SEASONS, function(s)
  read_csv(sprintf("KBL_%s_state_table.csv",s), show_col_types=FALSE,
           col_select=c(game_id,home_team,away_team,home_team_code,away_team_code)) %>%
    mutate(hc=norm(home_team_code), ac=norm(away_team_code)) %>%
    distinct(game_id, home_team, away_team, hc, ac)))

sh <- read_csv("KBL_pooled_shots_scored.csv", show_col_types=FALSE) %>% mutate(team=norm(team))

# =============================================================================
# 1) 이정현 선수 카드
# =============================================================================
lee <- sh %>% filter(shooter_en==LEE) %>% left_join(gmap, by="game_id") %>%
  mutate(상대팀 = ifelse(team==hc, away_team, home_team),
         홈원정 = ifelse(team==hc, "홈","원정"),
         팀마진직전 = ifelse(team==hc, margin_before, -margin_before),
         슛 = ifelse(shot_value==3,"3점","2점"))
lee_shots <- lee %>% transmute(season, game_id, game_date, 상대팀, 홈원정,
  period, 남은초=sec_left_period, 팀마진직전, 슛, 성공=made, 클러치=clutch_std,
  레버리지_LI=round(LI,2), 기대초과=round(make_over_exp,3), 기여=round(contribution,3))
write_excel_csv(lee_shots, file.path(OUT,"case_lee_shots.csv"))

# 시즌 요약 + 리그 백분위(그 시즌 클러치≥5 선수 대상)
lg_ps <- sh %>% filter(clutch_std, !is.na(shooter_en)) %>% group_by(season, shooter_en) %>%
  summarise(cl_att=n(), cl_moe=mean(make_over_exp), cl_val=sum(contribution), .groups="drop") %>%
  group_by(season) %>% mutate(vol_pct=round(100*percent_rank(cl_att)),
                              eff_pct=round(100*percent_rank(cl_moe))) %>% ungroup()
lee_sum <- sh %>% filter(shooter_en==LEE) %>% group_by(season) %>%
  summarise(야투시도=n(), FG=round(mean(made),3),
            `2P`=round(mean(made[shot_value==2]),3), `3P`=round(mean(made[shot_value==3]),3),
            `3점비중`=round(mean(shot_value==3),3),
            클러치시도=sum(clutch_std), 클러치FG=round(mean(made[clutch_std]),3),
            클러치_기대초과=round(mean(make_over_exp[clutch_std]),3),
            평균레버리지=round(mean(LI),2), .groups="drop") %>%
  left_join(lg_ps %>% filter(shooter_en==LEE) %>% select(season, 클러치볼륨_백분위=vol_pct, 클러치효율_백분위=eff_pct),
            by="season")
write_excel_csv(lee_sum, file.path(OUT,"case_lee_season_summary.csv"))
message("이정현: 야투 ", nrow(lee_shots), "개 / 시즌요약 ", nrow(lee_sum), "행")

# =============================================================================
# 2) 알바노 SK전 2경기 — 이벤트 시퀀스 + WP 추이
# =============================================================================
lab <- c("201"="2점 성공","202"="2점 실패","205"="3점 성공","206"="3점 실패","207"="2점 성공",
         "203"="자유투 성공","204"="자유투 실패","209"="공격리바","210"="수비리바","211"="어시스트",
         "213"="블록","216"="파울","225"="자유투부여","101"="교체IN","102"="교체OUT",
         "001"="쿼터시작","009"="쿼터종료","003"="타임아웃")
st26 <- read_csv("KBL_2025_26_state_table.csv", show_col_types=FALSE) %>%
  mutate(a=str_pad(norm(a),3,pad="0"), t=norm(t), hc=norm(home_team_code), ac=norm(away_team_code))
ev <- st26 %>% filter(game_id %in% ALV_GAMES) %>%
  arrange(game_id, period, api_row_order) %>%
  mutate(poss=ifelse(is.na(possession_home),0,ifelse(possession_home,1,-1)),
         t_eff=ifelse(period<=4, game_sec_remaining, sec_left_period),
         wp_home=round(wpf(margin_before, t_eff, poss),3),
         경과초=ifelse(period<=4, 2400-game_sec_remaining, 2400+(period-5)*300+(300-sec_left_period)),
         이벤트=ifelse(!is.na(lab[a]), lab[a], paste0("code_",a)),
         선수=p, 팀=ifelse(t==hc, home_team, ifelse(t==ac, away_team, NA)),
         알바노슛=(e==ALV & a %in% c("201","202","205","206","207")),
         버저비터=(e==ALV & a %in% c("205","207","201") & period==4 & sec_left_period<=1))
ev_out <- ev %>% transmute(game_id, home_team, away_team, period, 남은초=sec_left_period, 경과초,
  이벤트, 선수, 팀, 홈마진=margin_before, wp_home, 알바노슛, 버저비터)
write_excel_csv(ev_out, file.path(OUT,"case_alvano_events.csv"))

# 그 2경기의 슛(레버리지 포함) — pooled_shots_scored에서
alv_shots <- sh %>% filter(game_id %in% ALV_GAMES) %>% left_join(gmap, by="game_id") %>%
  transmute(game_id, game_date, 슈터=shooter_kr, 팀마진직전=ifelse(team==hc,margin_before,-margin_before),
    period, 남은초=sec_left_period, 슛=ifelse(shot_value==3,"3점","2점"), 성공=made,
    클러치=clutch_std, 레버리지_LI=round(LI,2), 기대초과=round(make_over_exp,3),
    버저비터=(shooter_en==ALV & shot_value==3 & period==4 & sec_left_period<=1 & made==1)) %>%
  arrange(game_id, desc(레버리지_LI))
write_excel_csv(alv_shots, file.path(OUT,"case_alvano_shots.csv"))

# WP 추이 그래프 (경기별)
th <- theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
  plot.title=element_text(face="bold",size=13), plot.subtitle=element_text(color="grey40",size=9),
  plot.background=element_rect(fill="white",color=NA), panel.background=element_rect(fill="white",color=NA))
for (g in ALV_GAMES) {
  d <- ev %>% filter(game_id==g, !is.na(wp_home))
  buz <- d %>% filter(버저비터)
  info <- d %>% slice(1)
  p <- ggplot(d, aes(경과초, wp_home))+
    geom_hline(yintercept=.5, linetype=3, color="grey70")+
    geom_line(color="#334155", linewidth=.7)+
    geom_point(data=buz, aes(경과초, wp_home), color="#ea580c", size=3)+
    geom_text(data=buz, aes(label="Alvano\nbuzzer 3"), color="#ea580c", hjust=1, nudge_x=-45, vjust=0.2, size=3.5, fontface="bold", lineheight=.9)+
    scale_x_continuous(breaks=c(0,600,1200,1800,2400), labels=c("Q1","Q2","Q3","Q4","End"), expand=expansion(mult=c(.02,.06)))+
    coord_cartesian(ylim=c(0,1))+
    labs(title=sprintf("Win probability — DB vs SK (%s)", info$game_date),
         subtitle="Home (DB) win probability through the game. Orange = Alvano's game-winning buzzer 3.",
         x=NULL, y="DB win probability")+th
  ggsave(file.path(IMG, sprintf("wp_alvano_%s.png", g)), p, width=7.2, height=3.8, dpi=150, bg="white")
}
message("알바노 이벤트 ", nrow(ev_out), "행(2경기) / 슛 ", nrow(alv_shots), "개 / WP 그래프 2종")

# =============================================================================
# 3) 2025-26 최고 레버리지 순간 top 30 (맥락)
# =============================================================================
top <- sh %>% filter(season=="2025_26") %>% left_join(gmap, by="game_id") %>%
  mutate(상대팀=ifelse(team==hc, away_team, home_team), 팀마진직전=ifelse(team==hc,margin_before,-margin_before)) %>%
  arrange(desc(LI)) %>% slice_head(n=30) %>%
  transmute(순위=row_number(), game_id, game_date, 슈터=shooter_kr, 상대팀,
    period, 남은초=sec_left_period, 팀마진직전, 슛=ifelse(shot_value==3,"3점","2점"),
    성공=made, 레버리지_LI=round(LI,2), 기대초과=round(make_over_exp,3))
write_excel_csv(top, file.path(OUT,"case_top_moments_2025_26.csv"))
alv_rank <- which(top$슈터=="이선 알바노" | grepl("알바노", top$슈터))
message("top moments 저장. 알바노 등장 순위: ", paste(alv_rank, collapse=", "))
message("── 케이스 데이터 생성 완료 → ", OUT, " ──")
