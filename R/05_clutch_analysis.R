# =============================================================================
# WP 모델 4단계: 클러치 실력 존재 검정 + 선수별 축소 추정
# -----------------------------------------------------------------------------
# 질문: "클러치 슈터는 존재하는가?" = 선수 간 클러치 실력 분산 τ² > 0 인가?
# 방법: 분산분해(method of moments) + 순열검정 (MCMC 불필요, 이식 쉬움)
#   - moe(make over expected) = made - xg  를 선수별로 집계
#   - τ² = 관측 선수간분산 - 표본노이즈 기대  (음수면 실력차 증거 없음)
#   - 순열검정: 슛을 선수에 무작위 재배정한 귀무분포와 비교
#   - 축소 추정: θ̂_i = τ²/(τ²+σ²/n_i) · ybar_i  (표본 적은 선수는 평균으로)
#
# 입력 : KBL_<season>_shots_scored.csv  (4단계 산출물: make_over_exp, LI, contribution)
# 출력 : *_clutch_players.csv (선수별 n·ybar·축소추정·레버리지가중값)
#
# 파이썬 검증(2024-25, 표준정의): τ²<0, 순열 p≈0.43 → 클러치 실력 증거 없음
#   install.packages(c("dplyr","readr"))
# =============================================================================

library(dplyr); library(readr)
set.seed(1)

SEASON_LABEL <- Sys.getenv("SEASON_LABEL", "2024_25")
CLUTCH_DEF   <- Sys.getenv("CLUTCH_DEF", "clutch_std")   # clutch_std / clutch_2poss / clutch_strict
N_PERM       <- 2000

sh <- read_csv(sprintf("KBL_%s_shots_scored.csv", SEASON_LABEL), show_col_types = FALSE)

cl <- sh %>% filter(.data[[CLUTCH_DEF]], !is.na(shooter_en))
message(sprintf("클러치 슛 %d개 | 선수 %d명 | moe 평균 %+.4f",
                nrow(cl), n_distinct(cl$shooter_en), mean(cl$make_over_exp)))

grp <- cl %>% group_by(shooter_en, shooter_kr) %>%
  summarise(n = n(), ybar = mean(make_over_exp),
            value = sum(contribution), .groups = "drop")   # 레버리지 가중 클러치 가치
message(sprintf("선수당 클러치 슛: 중앙값 %.0f | 평균 %.1f | 최대 %.0f",
                median(grp$n), mean(grp$n), max(grp$n)))

# ── 분산분해 ────────────────────────────────────────────────────────────────
mu       <- weighted.mean(grp$ybar, grp$n)
sig2     <- var(cl$make_over_exp)                 # 슛 1개 분산
between  <- weighted.mean((grp$ybar - mu)^2, grp$n)
sampling <- mean(sig2 / grp$n)
tau2     <- between - sampling                    # 진짜 선수간 실력분산

# ── 순열검정 ────────────────────────────────────────────────────────────────
vals <- cl$make_over_exp; Nvec <- grp$n
perm_between <- function() {
  v <- sample(vals); idx <- 1; tot <- 0
  for (nn in Nvec) { tot <- tot + nn * (mean(v[idx:(idx + nn - 1)]) - mu)^2; idx <- idx + nn }
  tot / sum(Nvec)
}
null <- replicate(N_PERM, perm_between())
pval <- mean(null >= between)

# ── 축소 추정 ────────────────────────────────────────────────────────────────
grp <- grp %>% mutate(shrunk = if (tau2 > 0) tau2 / (tau2 + sig2 / n) * ybar else 0)
write_excel_csv(grp %>% arrange(desc(value)),
                sprintf("KBL_%s_clutch_players.csv", SEASON_LABEL))

# ── 리포트 ──────────────────────────────────────────────────────────────────
message("──────── 클러치 실력 존재 검정 ────────")
message(sprintf("슛 1개 분산 σ²=%.4f", sig2))
message(sprintf("관측 선수간분산=%.4f | 표본노이즈 기대=%.4f", between, sampling))
message(sprintf("추정 실력분산 τ²=%.5f → 실력 SD=%.4f", tau2, sqrt(max(tau2, 0))))
message(sprintf("순열검정 p=%.3f → %s", pval,
                ifelse(pval < 0.05, "선수간 유의미한 차이 있음",
                       "랜덤과 구분 안 됨 (클러치=대부분 운)")))
message("(주의: 한 시즌·coarse xG 기준. 세 시즌 통합·수동 xG로 검정력 향상 필요)")
