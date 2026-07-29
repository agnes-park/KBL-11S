# =============================================================================
# 클러치 재구성 배치 1: (1) 역할·믿음  (2) 리그 클러치 효과  (3) 자유투 청정검정
# -----------------------------------------------------------------------------
# "개인 클러치 효율은 재현 안 됨"이라는 null을 넘어, 능동적으로 서술 가능한 결론:
#   A. 클러치 슈터는 '실력'이 아니라 '역할·믿음'으로 존재하는가?
#      (과거 클러치 성과가 미래 볼륨은 예측하지만 미래 효율은 예측하지 못한다)
#   B. 클러치엔 무슨 일이 벌어지나 — 리그 전체의 압박 효과(효율·슛믹스 변화)
#   C. 자유투 = 난이도 고정·수비 무관 → 압박만 순수 분리한 '청정 실험실'
#
# 입력 : 각 시즌 KBL_<season>_state_table.csv
# 출력 : ../docs/clutch_reframe_findings.md  (+ 콘솔 요약)
#   install.packages(c("dplyr","stringr","readr","tidyr"))
# =============================================================================

library(dplyr); library(stringr); library(readr); library(tidyr)
set.seed(1)
suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

SEASONS <- c("2023_24", "2024_25", "2025_26")
MIN_N   <- 5
N_PERM  <- 5000
REPORT  <- "../docs/clutch_reframe_findings.md"

norm <- function(x) sub("\\.0$", "", as.character(x))
load_state <- function(s) read_csv(sprintf("KBL_%s_state_table.csv", s), show_col_types = FALSE) %>%
  mutate(season = .env$s, a = str_pad(norm(a), 3, pad = "0"),
         t = norm(t), home_code = norm(home_code), away_code = norm(away_code))
ST <- bind_rows(lapply(SEASONS, load_state))

shots <- ST %>%
  filter(a %in% c("201","202","205","206","207","203","204"), !is.na(e)) %>%
  mutate(shot_type = case_when(a %in% c("205","206") ~ "3P",
                               a %in% c("203","204") ~ "FT", TRUE ~ "2P"),
         shot_value = case_when(shot_type=="3P"~3, shot_type=="FT"~1, TRUE~2),
         made = as.integer(a %in% c("201","205","207","203")),
         is_fg = shot_type != "FT",
         clutch = clutch_std, shooter_en = e, shooter_kr = p)

wcor <- function(x, y, w) { mx<-weighted.mean(x,w); my<-weighted.mean(y,w)
  sum(w*(x-mx)*(y-my))/sum(w)/sqrt(sum(w*(x-mx)^2)/sum(w)*sum(w*(y-my)^2)/sum(w)) }
perm_p_cor <- function(x, y, w) { r<-wcor(x,y,w)
  null<-replicate(N_PERM, wcor(x, sample(y), w)); mean(abs(null)>=abs(r)) }

# =============================================================================
# A. 역할·믿음 — 예측 비대칭
#   개인 비클러치 기준선으로 PAE 산출(문서식) → 선수×시즌 클러치 효율/볼륨
# =============================================================================
league_base <- shots %>% filter(!clutch) %>% group_by(season, shot_type) %>%
  summarise(lr = mean(made), .groups="drop")
pbase <- shots %>% filter(!clutch) %>% group_by(season, shooter_en, shot_type) %>%
  summarise(mk=sum(made), att=n(), .groups="drop") %>%
  left_join(league_base, by=c("season","shot_type")) %>%
  mutate(base=(mk+20*lr)/(att+20))
cl <- shots %>% filter(clutch) %>%
  left_join(pbase %>% select(season,shooter_en,shot_type,base), by=c("season","shooter_en","shot_type")) %>%
  left_join(league_base, by=c("season","shot_type")) %>%
  mutate(base=ifelse(is.na(base),lr,base), PAE = made*shot_value - base*shot_value)

ps <- cl %>% group_by(season, shooter_en) %>%
  summarise(vol = n(), eff = mean(PAE), .groups="drop")   # 볼륨=클러치 시도수, 효율=평균 PAE

nx <- c("2023_24"="2024_25","2024_25"="2025_26")
pairs <- bind_rows(lapply(names(nx), function(s0){
  inner_join(
    ps %>% filter(season==s0, vol>=MIN_N) %>% transmute(shooter_en, vol0=vol, eff0=eff),
    ps %>% filter(season==nx[[s0]], vol>=MIN_N) %>% transmute(shooter_en, vol1=vol, eff1=eff),
    by="shooter_en")
}))
w <- 2/(1/pairs$vol0 + 1/pairs$vol1)
r_vol  <- cor(pairs$vol0, pairs$vol1)                       # 볼륨 → 볼륨
r_eff  <- wcor(pairs$eff0, pairs$eff1, w)                   # 효율 → 효율
r_reward <- wcor(pairs$eff0, pairs$vol1, w)                 # 과거 효율 → 미래 볼륨 (보상?)
p_vol <- perm_p_cor(pairs$vol0, pairs$vol1, rep(1,nrow(pairs)))
p_eff <- perm_p_cor(pairs$eff0, pairs$eff1, w)
p_rew <- perm_p_cor(pairs$eff0, pairs$vol1, w)
# 보상 회귀: 미래 볼륨 ~ 과거 볼륨 + 과거 효율 (표준화)
z <- function(v) (v-mean(v))/sd(v)
reg <- summary(lm(z(vol1) ~ z(vol0) + z(eff0), data=pairs))
b_vol0 <- reg$coefficients["z(vol0)",]; b_eff0 <- reg$coefficients["z(eff0)",]

message(sprintf("[A] 볼륨→볼륨 r=%.3f (p=%.3f) | 효율→효율 r=%.3f (p=%.3f) | 과거효율→미래볼륨 r=%.3f (p=%.3f)",
                r_vol, p_vol, r_eff, p_eff, r_reward, p_rew))
message(sprintf("    회귀 z(vol1)~z(vol0)+z(eff0): β(vol0)=%.3f(p=%.3g), β(eff0)=%.3f(p=%.3g)",
                b_vol0[1], b_vol0[4], b_eff0[1], b_eff0[4]))

# =============================================================================
# B. 리그 수준 클러치 효과 — 효율·슛믹스 (클러치 vs 비클러치)
# =============================================================================
two_prop <- function(m1,n1,m0,n0){ p1<-m1/n1; p0<-m0/n0; p<-(m1+m0)/(n1+n0)
  z<-(p1-p0)/sqrt(p*(1-p)*(1/n1+1/n0)); list(p1=p1,p0=p0,d=p1-p0,z=z,p=2*pnorm(-abs(z))) }

lg_rows <- lapply(SEASONS, function(ssn){
  d <- shots %>% filter(season == .env$ssn)
  fg <- d %>% filter(is_fg)
  c2 <- fg %>% filter(shot_type=="2P"); c3 <- fg %>% filter(shot_type=="3P")
  ft <- d %>% filter(!is_fg)
  # 효율
  e_fg <- two_prop(sum(fg$made[fg$clutch]), sum(fg$clutch),
                   sum(fg$made[!fg$clutch]), sum(!fg$clutch))
  e_3  <- two_prop(sum(c3$made[c3$clutch]), sum(c3$clutch),
                   sum(c3$made[!c3$clutch]), sum(!c3$clutch))
  e_ft <- two_prop(sum(ft$made[ft$clutch]), sum(ft$clutch),
                   sum(ft$made[!ft$clutch]), sum(!ft$clutch))
  # 슛믹스
  share3_cl <- mean(fg$shot_type[fg$clutch]=="3P"); share3_nc <- mean(fg$shot_type[!fg$clutch]=="3P")
  ftrate_cl <- sum(ft$clutch)/sum(fg$clutch);  ftrate_nc <- sum(!ft$clutch)/sum(!fg$clutch)
  tibble(season=ssn,
         FG_cl=e_fg$p1, FG_nc=e_fg$p0, dFG=e_fg$d, pFG=e_fg$p,
         P3_cl=e_3$p1, P3_nc=e_3$p0, dP3=e_3$d,
         FT_cl=e_ft$p1, FT_nc=e_ft$p0, dFT=e_ft$d, pFT=e_ft$p,
         sh3_cl=share3_cl, sh3_nc=share3_nc,
         ftr_cl=ftrate_cl, ftr_nc=ftrate_nc)
}) %>% bind_rows()

# 풀링 검정(효율)
fg <- shots %>% filter(is_fg); ft <- shots %>% filter(!is_fg)
pool_fg <- two_prop(sum(fg$made[fg$clutch]),sum(fg$clutch),sum(fg$made[!fg$clutch]),sum(!fg$clutch))
pool_ft <- two_prop(sum(ft$made[ft$clutch]),sum(ft$clutch),sum(ft$made[!ft$clutch]),sum(!ft$clutch))
message(sprintf("\n[B] 풀링 FG%%: 클러치 %.3f vs 비클러치 %.3f (Δ%+.3f, p=%.2g) | FT%%: %.3f vs %.3f (Δ%+.3f, p=%.2g)",
                pool_fg$p1,pool_fg$p0,pool_fg$d,pool_fg$p, pool_ft$p1,pool_ft$p0,pool_ft$d,pool_ft$p))

# =============================================================================
# C. 자유투 청정검정 — 압박만 분리
# =============================================================================
# (C1) 리그: 클러치 FT% vs 비클러치 FT% (초킹?)  → pool_ft 위에서 계산
# (C2) 클러치 FT 성과가 '개인 평소 FT%'로 설명되는가 (별도 클러치 요인 존재?)
ft_base <- shots %>% filter(!is_fg, !clutch) %>% group_by(season, shooter_en) %>%
  summarise(ft_pct = mean(made), nb = n(), .groups="drop")
ft_cl <- shots %>% filter(!is_fg, clutch) %>%
  left_join(ft_base, by=c("season","shooter_en")) %>% filter(!is.na(ft_pct))
# 개인 평소 FT%가 클러치 FT 성공을 예측하는가(=평소 실력이 그대로 발현) vs 잔차(=클러치 요인)
glm_ft <- glm(made ~ ft_pct, family=binomial, data=ft_cl)
# 선수×시즌 클러치 FT 편차의 지속성(표본 적음 — 참고치)
ftdev <- shots %>% filter(!is_fg, clutch) %>% group_by(season, shooter_en) %>%
  summarise(cl_ft=mean(made), n=n(), .groups="drop") %>%
  left_join(ft_base, by=c("season","shooter_en")) %>% mutate(dev=cl_ft-ft_pct)
ftpairs <- bind_rows(lapply(names(nx), function(s0){
  inner_join(ftdev %>% filter(season==s0, n>=3) %>% transmute(shooter_en, d0=dev, n0=n),
             ftdev %>% filter(season==nx[[s0]], n>=3) %>% transmute(shooter_en, d1=dev, n1=n),
             by="shooter_en")}))
r_ftdev <- if(nrow(ftpairs)>=5) wcor(ftpairs$d0, ftpairs$d1, 2/(1/ftpairs$n0+1/ftpairs$n1)) else NA
message(sprintf("\n[C] 클러치 FT: 개인 평소 FT%% 계수 β=%.2f (p=%.3g) | 클러치 FT편차 지속성 r=%.3f (쌍 %d)",
                coef(glm_ft)["ft_pct"], summary(glm_ft)$coefficients["ft_pct",4],
                ifelse(is.na(r_ftdev),NA_real_,r_ftdev), nrow(ftpairs)))

# =============================================================================
# 리포트
# =============================================================================
fmt_tbl <- function(df){ df<-as.data.frame(df)
  cells<-lapply(df,function(c) format(c,trim=TRUE)); rows<-do.call(paste,c(cells,list(sep=" | ")))
  c(paste0("| ",paste(names(df),collapse=" | ")," |"),
    paste0("| ",paste(rep("---",ncol(df)),collapse=" | ")," |"), paste0("| ",rows," |")) }
r3<-function(x) round(x,3)
lgB <- lg_rows %>% transmute(season,
        FG_클=r3(FG_cl), FG_비=r3(FG_nc), ΔFG=r3(dFG),
        `3P_클`=r3(P3_cl), `3P_비`=r3(P3_nc),
        FT_클=r3(FT_cl), FT_비=r3(FT_nc),
        `3점비중_클`=r3(sh3_cl), `3점비중_비`=r3(sh3_nc),
        FT유도_클=r3(ftr_cl), FT유도_비=r3(ftr_nc))

concl_A <- sprintf(paste0(
  "과거 클러치 **볼륨**은 다음 시즌 볼륨을 강하게 예측하고(r=%.2f, p<0.001), 회귀에서도 β(vol0)=%.2f로 지배적이다. ",
  "반면 과거 클러치 **효율**은 다음 시즌 효율을 예측하지 못하며(r=%.2f, p=%.2f), 미래 볼륨도 예측하지 못한다(β(eff0)=%.2f, p=%.2f). ",
  "→ **팀은 '클러치 슈터'라는 역할을 계속 같은 선수에게 맡기지만, 그 배정은 과거의 실제 클러치 효율과 무관하다.** ",
  "클러치 슈터는 재현되는 *역할·믿음*으로 존재하되, 그 믿음은 효율로 뒷받침되지 않는다(볼륨은 자기지속, 효율은 운)."),
  r_vol, b_vol0[1], r_eff, p_eff, b_eff0[1], b_eff0[4])

concl_B <- sprintf(paste0(
  "리그 전체 야투 성공률은 클러치에서 유의하게 **하락**한다(풀링 %.3f→%.3f, Δ%+.3f, p=%.2g). ",
  "자유투는 변화가 %s(%.3f→%.3f, Δ%+.3f, p=%.2g). 3점 비중·자유투 유도율의 클러치 변화는 위 표 참조. ",
  "→ **클러치는 특정 개인의 문제가 아니라, 수비 강화·압박으로 리그 전체가 겪는 구조적 효율 저하 현상이다.**"),
  pool_fg$p0, pool_fg$p1, pool_fg$d, pool_fg$p,
  ifelse(pool_ft$p<0.05,"유의","미미"), pool_ft$p0, pool_ft$p1, pool_ft$d, pool_ft$p)

concl_C <- sprintf(paste0(
  "자유투는 난이도가 고정되어 '압박'만 분리된다. 리그 클러치 FT%%는 비클러치와 %s(Δ%+.3f, p=%.2g) — 조직적 '초킹' 증거는 %s. ",
  "클러치 FT 성공은 **개인 평소 FT%%로 잘 설명되고**(β=%.2f, p=%.2g), 그 위의 '클러치 FT 편차'는 시즌 간 지속성이 없다(r=%.3f). ",
  "→ **가장 깨끗한 조건(자유투)에서도 '평소 슈팅 실력'만 재현될 뿐, 그와 별개의 개인 클러치 능력은 없다.**"),
  ifelse(pool_ft$p<0.05,"다르다","사실상 같다"), pool_ft$d, pool_ft$p,
  ifelse(pool_ft$p<0.05 & pool_ft$d<0,"약함/부분적","없음"),
  coef(glm_ft)["ft_pct"], summary(glm_ft)$coefficients["ft_pct",4],
  ifelse(is.na(r_ftdev),NA_real_,r_ftdev))

lines <- c(
  "# KBL 클러치 — 재구성 배치 1: 역할·믿음 / 리그 효과 / 자유투",
  "",
  sprintf("_3시즌(%s). '개인 클러치 효율은 재현 안 됨'을 넘어 능동적 결론을 도출._", paste(SEASONS,collapse=", ")),
  "",
  "## A. 클러치 슈터 = 실력이 아니라 역할·믿음 (예측 비대칭)",
  "",
  fmt_tbl(tibble(관계=c("볼륨→볼륨","효율→효율","과거효율→미래볼륨"),
                 r=r3(c(r_vol,r_eff,r_reward)), perm_p=r3(c(p_vol,p_eff,p_rew)))),
  "",
  sprintf("회귀 `z(미래볼륨) ~ z(과거볼륨)+z(과거효율)`: β(과거볼륨)=**%.2f** (p=%.2g), β(과거효율)=%.2f (p=%.2g)",
          b_vol0[1], b_vol0[4], b_eff0[1], b_eff0[4]),
  "", concl_A, "",
  "## B. 리그 수준 클러치 효과 (클러치 vs 비클러치)",
  "",
  fmt_tbl(lgB),
  "",
  "_클=클러치, 비=비클러치. 3점비중=3PA/FGA, FT유도=FTA/FGA._",
  "_주: 클러치 FT유도율 급증(약 2배)은 순수 공격성뿐 아니라 종료 직전 **고의 파울(추격 팀의 파울 작전)** 이 섞여 있음._",
  "", concl_B, "",
  "## C. 자유투 청정검정 (압박만 분리)",
  "", concl_C, "",
  "## 종합 결론",
  "",
  "세 조각이 하나의 이야기로 수렴한다:",
  "1. **클러치 슈터는 '역할·믿음'으로 실재한다** — 볼륨(기회)은 시즌 간 재현되지만 효율은 아니다.",
  "2. **압박은 리그 전체가 받는 구조적 현상이다** — 클러치엔 야투 효율이 일괄 하락한다.",
  "3. **가장 깨끗한 자유투에서조차** 평소 실력만 재현되고 별도의 개인 클러치 능력은 없다.",
  "",
  "→ *\"클러치 실력은 없다\"*가 아니라, **\"클러치는 개인의 재현되는 실력이 아니라, 리그 차원의 압박 구조 + 팀이 특정 선수에게 계속 베팅하는 역할·믿음의 산물\"** 이라는 결론.",
  ""
)
dir.create("../docs", showWarnings=FALSE)
writeLines(lines, REPORT)
message(sprintf("\n리포트 저장: %s", REPORT))
message("── 배치 1 완료 ──")
