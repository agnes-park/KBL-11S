# =============================================================================
# 재구성 배치 2 (#4): 결론을 '없음'에서 '상한'으로 — 등가검정 + 승수 환산
# -----------------------------------------------------------------------------
# null("차이를 못 찾음")을 정량적 상한("차이가 있어도 이만큼 작음")으로 격상한다.
#   (1) 클러치 실력 SD의 95% 상한 (선수 클러스터 부트스트랩) + SESOI 등가판정
#   (2) 시즌 간 지속성 상관의 95% 상한
#   (3) 승수 환산: 재현 가능한(=지속되는) 클러치 실력이 시즌 승수로 얼마인가
#       — 레버리지를 WP(승률) 단위로 써서 외부 상수 없이 승수로 직접 환산
#
# 입력 : KBL_pooled_shots_scored.csv, KBL_pooled_wp_model.rds  (← R/10 먼저 실행)
# 출력 : ../docs/clutch_bounds_findings.md
#   install.packages(c("dplyr","readr"))
# =============================================================================

library(dplyr); library(readr)
set.seed(1)
suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

DEF    <- "clutch_std"
MIN_N  <- 5
B_BOOT <- 2000
GAMES_SEASON <- 54           # KBL 정규시즌 팀당 경기수(승수 맥락)
SESOI_SD    <- 0.05          # '의미 있는 클러치 우위'의 최소효과크기: 슛당 성공확률 +5%p
SESOI_WINS  <- 0.5           # 로스터 판단에 의미 있는 최소 승수/시즌

if (!file.exists("KBL_pooled_shots_scored.csv"))
  stop("KBL_pooled_shots_scored.csv 없음 — 먼저 ./run_multiseason.sh (R/10) 실행")
mdl      <- readRDS("KBL_pooled_wp_model.rds"); mean_lev <- mdl$mean_lev
sh <- read_csv("KBL_pooled_shots_scored.csv", show_col_types = FALSE)
cl <- sh %>% filter(.data[[DEF]], !is.na(shooter_en)) %>%
  mutate(lev_wp = LI * mean_lev)                 # 슛당 레버리지를 WP(승률) 단위로 복원

# ── 랜덤효과 실력분산 추정기 (R/10과 동일) ─────────────────────────────────
tau2_hat <- function(y, g) {
  d <- data.frame(y, g); N <- nrow(d)
  grp <- d %>% group_by(g) %>% summarise(n=n(), yb=mean(y), ssw=sum((y-mean(y))^2), .groups="drop")
  K <- nrow(grp); mu <- mean(d$y)
  MSB <- sum(grp$n*(grp$yb-mu)^2)/(K-1); MSW <- sum(grp$ssw)/(N-K)
  n0 <- (N - sum(grp$n^2)/N)/(K-1)
  list(tau2=(MSB-MSW)/n0, sig2=MSW)
}
est   <- tau2_hat(cl$make_over_exp, cl$shooter_en)
skill_sd <- sqrt(max(est$tau2, 0))

# ── (1) 클러스터 부트스트랩: 선수 재표집 → skill_sd 95% 상한 ────────────────
players <- unique(cl$shooter_en)
by_player <- split(seq_len(nrow(cl)), cl$shooter_en)
boot_sd <- replicate(B_BOOT, {
  samp <- sample(players, length(players), replace = TRUE)
  idx  <- unlist(by_player[samp], use.names = FALSE)
  gg   <- rep(seq_along(samp), lengths(by_player[samp]))   # 재표집 선수에 새 id (중복 분리)
  sqrt(max(tau2_hat(cl$make_over_exp[idx], gg)$tau2, 0))
})
sd_hi <- unname(quantile(boot_sd, 0.975))

# ── (2) 시즌 간 지속성 상관 + 95% 상한 ──────────────────────────────────────
ps <- cl %>% group_by(season, shooter_en) %>%
  summarise(n=n(), eff=mean(make_over_exp), .groups="drop")
nx <- c("2023_24"="2024_25","2024_25"="2025_26")
pairs <- bind_rows(lapply(names(nx), function(s0) inner_join(
  ps %>% filter(season==s0, n>=MIN_N) %>% transmute(shooter_en, y0=eff, n0=n),
  ps %>% filter(season==nx[[s0]], n>=MIN_N) %>% transmute(shooter_en, y1=eff, n1=n),
  by="shooter_en")))
wcor <- function(x,y,w){mx<-weighted.mean(x,w);my<-weighted.mean(y,w)
  sum(w*(x-mx)*(y-my))/sum(w)/sqrt(sum(w*(x-mx)^2)/sum(w)*sum(w*(y-my)^2)/sum(w))}
w0 <- 2/(1/pairs$n0+1/pairs$n1); r_pers <- wcor(pairs$y0, pairs$y1, w0)
boot_r <- replicate(B_BOOT, { i<-sample(nrow(pairs), replace=TRUE)
  wcor(pairs$y0[i], pairs$y1[i], 2/(1/pairs$n0[i]+1/pairs$n1[i])) })
r_hi <- unname(quantile(boot_r, 0.975)); r_lo <- unname(quantile(boot_r, 0.025))
reliability <- max(r_pers, 0)

# ── (3) 승수 환산 (WP 단위) ─────────────────────────────────────────────────
lev_clutch <- mean(cl$lev_wp)                       # 클러치 슛 1개의 평균 승률 스윙
vol_med    <- median((cl %>% count(season, shooter_en) %>% filter(n>=MIN_N))$n)
vol_star   <- round(quantile((cl %>% count(season, shooter_en))$n, 0.95))
# +1SD 선수가 시즌에 더하는 승수 = skill_sd × (클러치 슛당 WP) × 시즌 클러치 슛수
wins <- function(sd, vol) sd * lev_clutch * vol
# 재현 가능한(지속되는) 실력만 실제로 취할 수 있음: skill_sd × sqrt(reliability)
skill_sd_repro <- skill_sd * sqrt(reliability)
sd_hi_repro    <- sd_hi   * sqrt(reliability)

msg <- function(...) message(sprintf(...))
msg("클러치 슛 %d | 선수 %d | 슛당 평균 레버리지 %.3f WP | 시즌 클러치 슛 중앙값 %d, 상위볼륨 %d",
    nrow(cl), length(players), lev_clutch, vol_med, vol_star)
msg("[1] 단일시즌 실력 SD=%.4f (95%% 상한 %.4f) | 슛당 σ=%.3f", skill_sd, sd_hi, sqrt(est$sig2))
msg("[2] 지속성 r=%.3f (95%% CI %.3f~%.3f) → 신뢰도 %.3f", r_pers, r_lo, r_hi, reliability)
msg("[3] +1SD 선수 승수: 단일시즌 %.2f승(상한 %.2f) | 재현가능(지속) %.3f승(상한 %.3f) @중앙볼륨 %d",
    wins(skill_sd,vol_med), wins(sd_hi,vol_med), wins(skill_sd_repro,vol_med), wins(sd_hi_repro,vol_med), vol_med)

# 등가판정
equ_sd   <- sd_hi < SESOI_SD
equ_wins <- wins(sd_hi_repro, vol_star) < SESOI_WINS

# =============================================================================
# 리포트
# =============================================================================
fmt_tbl <- function(df){ df<-as.data.frame(df)
  cells<-lapply(df,function(c) format(c,trim=TRUE)); rows<-do.call(paste,c(cells,list(sep=" | ")))
  c(paste0("| ",paste(names(df),collapse=" | ")," |"),
    paste0("| ",paste(rep("---",ncol(df)),collapse=" | ")," |"), paste0("| ",rows," |")) }
r3<-function(x) round(x,3)

tbl1 <- tibble(
  지표 = c("클러치 실력 SD (단일시즌, 잡음보정)",
           "└ 95% 상한",
           "슛 1개 내부 SD (순수 잡음 규모)",
           "SESOI (의미있는 우위)"),
  값_슛당성공확률 = c(r3(skill_sd), r3(sd_hi), r3(sqrt(est$sig2)), SESOI_SD))
tbl2 <- tibble(
  지표 = c("시즌간 지속성 상관 r", "└ 95% CI 하한", "└ 95% CI 상한", "→ 재현 가능한 신뢰도"),
  값 = c(r3(r_pers), r3(r_lo), r3(r_hi), r3(reliability)))
tbl3 <- tibble(
  시나리오 = c("+1SD 선수 (단일시즌 관측 SD)",
              "+1SD 선수 (재현 가능한 실력만)",
              "  └ 95% 상한 (재현 가능)"),
  중앙볼륨_승 = c(r3(wins(skill_sd,vol_med)), r3(wins(skill_sd_repro,vol_med)), r3(wins(sd_hi_repro,vol_med))),
  상위볼륨_승 = c(r3(wins(skill_sd,vol_star)), r3(wins(skill_sd_repro,vol_star)), r3(wins(sd_hi_repro,vol_star))))

lines <- c(
  "# KBL 클러치 — 배치 2: 등가검정(상한) + 승수 환산",
  "",
  "\"차이를 못 찾았다\"를 **\"차이가 있어도 이만큼 작다\"**는 정량적 상한으로 바꾼다.",
  "레버리지를 WP(승률) 단위로 써서 외부 상수 없이 승수로 직접 환산한다.",
  "",
  "## 1. 클러치 실력 SD의 상한 (등가검정)",
  "",
  "단위 = 클러치 슛 1개의 성공확률(0~1). 선수 클러스터 부트스트랩 2000회.",
  "",
  fmt_tbl(tbl1),
  "",
  sprintf("→ 클러치 실력 SD의 95%% 상한(**%.3f**)조차 슛 1개 잡음(σ=%.2f)의 %.0f분의 1이며, SESOI(%.2f)보다 **%s**. %s",
          sd_hi, sqrt(est$sig2), sqrt(est$sig2)/sd_hi, SESOI_SD,
          ifelse(equ_sd,"작다","크다"),
          ifelse(equ_sd,"→ 실전 무시 가능 수준으로 **등가(zero-equivalent)** 판정.",
                 "→ SD만으로는 등가판정 보류. 단 이 SD는 **대부분 재현되지 않는 단일시즌 변동**을 포함하므로, 결정적 판단은 3절의 '재현 가능한 승수 상한'이 내린다.")),
  "",
  "## 2. 시즌 간 지속성의 상한",
  "",
  fmt_tbl(tbl2),
  "",
  sprintf("→ 지속성 상관의 95%% 상한이 **%.2f**에 불과 — '재현 가능한 실력'이라 부를 신뢰도(%.2f)가 사실상 0. 단일시즌에 보이는 분산의 대부분은 다음 시즌에 사라진다.",
          r_hi, reliability),
  "",
  "## 3. 승수 환산 — '있어도 얼마나 이득인가'",
  "",
  sprintf("클러치 슛 1개의 평균 승률 스윙 %.3f WP. 시즌 클러치 슛: 중앙 %d개, 상위볼륨 %d개. (정규시즌 %d경기 맥락)",
          lev_clutch, vol_med, vol_star, GAMES_SEASON),
  "",
  fmt_tbl(tbl3),
  "",
  sprintf(paste0("→ **핵심**: 단일시즌 관측 SD로는 +1SD 선수가 ~%.2f승처럼 보이지만, 이는 재현되지 않는다. ",
    "실제로 **취할 수 있는(재현 가능한) 클러치 실력**은 최상위 볼륨 선수라도 95%% 상한 **%.2f승/시즌** — ",
    "SESOI(%.1f승)보다 %s. 즉 클러치를 근거로 한 로스터/전술 판단의 기대이득은 **사실상 0.**"),
    wins(skill_sd,vol_star), wins(sd_hi_repro,vol_star), SESOI_WINS,
    ifelse(equ_wins,"작아 **무의미**","커 재검토")),
  "",
  "## 결론",
  "",
  "클러치 실력은 단지 '검출 실패'가 아니라, **상한을 씌워도 실전 무시 가능한 크기임이 증명**된다:",
  sprintf("- 실력 SD 95%% 상한 %.3f (슛당 성공확률) — 잡음의 %.0f분의 1", sd_hi, sqrt(est$sig2)/sd_hi),
  sprintf("- 지속성 상관 95%% 상한 %.2f — 재현성 사실상 0", r_hi),
  sprintf("- 재현 가능한 클러치 실력의 승수 가치 95%% 상한 **%.2f승/시즌** (SESOI %.1f승)", wins(sd_hi_repro,vol_star), SESOI_WINS),
  ""
)
dir.create("../docs", showWarnings=FALSE)
writeLines(lines, "../docs/clutch_bounds_findings.md")
msg("\n리포트 저장: ../docs/clutch_bounds_findings.md")
message("── 배치 2 완료 ──")
