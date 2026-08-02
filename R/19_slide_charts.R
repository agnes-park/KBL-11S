# =============================================================================
# 발표(슬라이드) 방법론 시각자료 — WP 캘리브레이션 / 레버리지 상황별 / 클러치 지속성
# 입력: KBL_<season>_state_table.csv, KBL_pooled_wp_model.rds, KBL_pooled_shots_scored.csv
# 출력: ../docs/img/slide_*.png   (영문 라벨; 한글판은 로컬에서 라벨만 교체)
# =============================================================================
library(dplyr); library(stringr); library(readr); suppressMessages(library(ggplot2))
IMG <- "../docs/img"; dir.create(IMG, showWarnings=FALSE, recursive=TRUE)
SEASONS <- c("2023_24","2024_25","2025_26"); norm <- function(x) sub("\\.0$","",trimws(as.character(x)))
th <- theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
  plot.title=element_text(face="bold",size=13), plot.subtitle=element_text(color="grey40",size=9),
  plot.background=element_rect(fill="white",color=NA), panel.background=element_rect(fill="white",color=NA))
OR<-"#ea580c"; GN<-"#16a34a"; SL<-"#334155"

mdl <- readRDS("KBL_pooled_wp_model.rds"); sigma<-mdl$sigma; beta<-mdl$beta

# ── (1) WP 캘리브레이션 ──────────────────────────────────────────────────────
st <- bind_rows(lapply(SEASONS, function(s) read_csv(sprintf("KBL_%s_state_table.csv",s),show_col_types=FALSE) %>% mutate(season=s)))
gw <- st %>% distinct(game_id, official_home_score, official_away_score) %>%
  mutate(home_win = as.integer(official_home_score > official_away_score))
cal <- st %>% left_join(gw %>% select(game_id,home_win), by="game_id") %>%
  mutate(poss=ifelse(is.na(possession_home),0,ifelse(possession_home,1,-1)),
         t_eff=ifelse(period<=4, game_sec_remaining, sec_left_period),
         wp=pnorm((margin_before+beta*poss)/(sigma*sqrt(pmax(t_eff,0.5))))) %>%
  filter(period<=4, t_eff>=1, t_eff<=2400, !is.na(margin_before), !is.na(home_win)) %>%
  mutate(bin=cut(wp, breaks=seq(0,1,0.1), include.lowest=TRUE)) %>%
  group_by(bin) %>% summarise(pred=mean(wp), actual=mean(home_win), n=n(), .groups="drop")
p1 <- ggplot(cal, aes(pred, actual))+
  geom_abline(slope=1, intercept=0, linetype=2, color="grey60")+
  geom_line(color=OR, linewidth=.8)+ geom_point(aes(size=n), color=OR)+
  scale_size(range=c(1.5,5), guide="none")+ coord_equal(xlim=c(0,1), ylim=c(0,1))+
  labs(title="Win-probability model is well calibrated",
       subtitle="Predicted WP vs actual home-win rate (dashed = perfect line).",
       x="Predicted win probability", y="Actual home-win rate")+th
ggsave(file.path(IMG,"slide_wp_calibration.png"), p1, width=5.6, height=4.2, dpi=150, bg="white")

# ── (2) 레버리지 상황별 ─────────────────────────────────────────────────────
sh <- read_csv("KBL_pooled_shots_scored.csv", show_col_types=FALSE)
lev <- sh %>% mutate(situ=case_when(clutch_std ~ "Clutch\n(<=5min, <=5pt)",
                                    abs(margin_before)>=20 ~ "Garbage time\n(>=20pt)",
                                    TRUE ~ "Non-clutch")) %>%
  group_by(situ) %>% summarise(LI=mean(LI, na.rm=TRUE), n=n(), .groups="drop") %>%
  mutate(situ=factor(situ, levels=c("Garbage time\n(>=20pt)","Non-clutch","Clutch\n(<=5min, <=5pt)")))
p2 <- ggplot(lev, aes(situ, LI, fill=situ))+
  geom_col(width=.6)+ geom_hline(yintercept=1, linetype=2, color="grey55")+
  geom_text(aes(label=sprintf("LI = %.2f", LI)), vjust=-0.4, fontface="bold", size=4)+
  scale_fill_manual(values=c("#94a3b8","#fdba74",OR), guide="none")+
  annotate("text", x=0.6, y=1.06, label="league avg = 1", hjust=0, size=3, color="grey45")+
  coord_cartesian(ylim=c(0,3.6))+
  labs(title="Clutch shots swing the game ~3x more than average",
       subtitle="Mean Leverage Index by situation (1 = average shot). Garbage-time shots barely matter.",
       x=NULL, y="Leverage Index (LI)")+th
ggsave(file.path(IMG,"slide_leverage.png"), p2, width=6.4, height=4, dpi=150, bg="white")

# ── (3) 클러치 효율 지속성 산점도 (자유투 대조군과 짝) ───────────────────────
wcor <- function(x,y,w){mx<-weighted.mean(x,w);my<-weighted.mean(y,w)
  sum(w*(x-mx)*(y-my))/sqrt(sum(w*(x-mx)^2)*sum(w*(y-my)^2))}
ps <- sh %>% filter(clutch_std, !is.na(shooter_en)) %>%
  group_by(season, shooter_en) %>% summarise(moe=mean(make_over_exp), n=n(), .groups="drop")
nx <- c("2023_24"="2024_25","2024_25"="2025_26")
pr <- bind_rows(lapply(names(nx), function(s0) inner_join(
  ps %>% filter(season==s0, n>=5) %>% transmute(shooter_en, y0=moe, n0=n),
  ps %>% filter(season==nx[[s0]], n>=5) %>% transmute(shooter_en, y1=moe, n1=n), by="shooter_en")))
r_cl <- wcor(pr$y0, pr$y1, 2/(1/pr$n0+1/pr$n1))
p3 <- ggplot(pr, aes(y0,y1))+
  geom_hline(yintercept=0,color="grey85")+geom_vline(xintercept=0,color="grey85")+
  geom_point(aes(size=pmin(n0,n1)), color=OR, alpha=.5)+
  geom_smooth(method="lm", se=FALSE, color=SL, linewidth=.8)+ scale_size(range=c(1,5),guide="none")+
  annotate("text", x=min(pr$y0), y=max(pr$y1), hjust=0, vjust=1,
           label=sprintf("r = %.2f  (n=%d)\n= essentially zero", r_cl, nrow(pr)), fontface="bold", size=4, color=OR)+
  labs(title="Clutch shooting does NOT repeat season-to-season",
       subtitle="Each point = a player. Clutch make-over-expected: season t vs t+1.",
       x="Clutch make-over-expected (season t)", y="Clutch make-over-expected (season t+1)")+th
ggsave(file.path(IMG,"slide_clutch_scatter.png"), p3, width=6.6, height=4, dpi=150, bg="white")

message(sprintf("저장 완료. WP bins=%d | 레버리지 클러치 LI=%.2f | 클러치 지속성 r=%.2f (쌍 %d)",
                nrow(cal), lev$LI[grepl("Clutch",lev$situ)], r_cl, nrow(pr)))
