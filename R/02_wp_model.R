# =============================================================================
# WP 모델 1단계: 확산(diffusion) 베이스라인
# -----------------------------------------------------------------------------
# WP_home = Φ( (마진 + β·공격권) / (σ·√잔여시간) )
#   - σ: 단위시간당 점수 변동성  (파라미터 1개)
#   - β: 공격권 가치(점)         (파라미터 1개)
#   두 파라미터를 전체 (상태, 홈승패) 쌍의 우도최대화로 추정.
#
# 입력 : KBL_<season>_state_table.csv  (고친 전처리 산출물)
# 출력 : *_state_wp.csv (wp_home 컬럼 추가), KBL_wp_model.rds (σ,β 저장)
#
# 파이썬 검증 결과(2024-25): σ≈0.355, β≈0.22, Brier≈0.176, 캘리브레이션 양호
#   install.packages(c("dplyr","readr"))
# =============================================================================

library(dplyr); library(readr)

SEASON_LABEL <- "2024_25"
INPUT   <- sprintf("KBL_%s_state_table.csv", SEASON_LABEL)
OUTPUT  <- sprintf("KBL_%s_state_wp.csv", SEASON_LABEL)

st <- read_csv(INPUT, show_col_types = FALSE)

# ── 홈 승패 라벨 ────────────────────────────────────────────────────────────
gw <- st %>% distinct(game_id, official_home_score, official_away_score) %>%
  mutate(home_win = as.integer(official_home_score > official_away_score))
st <- st %>% left_join(gw %>% select(game_id, home_win), by = "game_id")

# ── 공격권 → 부호(+1 홈볼 / -1 원정볼 / 0 불명) ─────────────────────────────
st <- st %>% mutate(
  poss  = ifelse(is.na(possession_home), 0, ifelse(possession_home, 1, -1)),
  t_eff = ifelse(period <= 4, game_sec_remaining, sec_left_period)   # OT는 해당 OT 잔여
)

# ── σ,β 우도최대화 (정규시간 유효 이벤트로 적합) ────────────────────────────
fitset <- st %>% filter(period <= 4, t_eff >= 1, t_eff <= 2400, !is.na(margin_before))
m <- fitset$margin_before; t <- fitset$t_eff; ps <- fitset$poss; y <- fitset$home_win

nll <- function(par) {
  sig <- par[1]; beta <- par[2]
  z  <- (m + beta * ps) / (sig * sqrt(t) + 1e-9)
  pr <- pmin(pmax(pnorm(z), 1e-6), 1 - 1e-6)
  -mean(y * log(pr) + (1 - y) * log(1 - pr))
}
fit   <- optim(c(0.3, 1.0), nll, method = "Nelder-Mead")
sigma <- fit$par[1]; beta <- fit$par[2]

# ── 모든 이벤트에 WP 부여 ───────────────────────────────────────────────────
wp_home <- function(margin, t_rem, poss, sigma, beta)
  pnorm((margin + beta * poss) / (sigma * sqrt(pmax(t_rem, 0.5))))

st <- st %>% mutate(wp_home = wp_home(margin_before, t_eff, poss, sigma, beta))

# ── 저장 ────────────────────────────────────────────────────────────────────
saveRDS(list(sigma = sigma, beta = beta), "KBL_wp_model.rds")
write_excel_csv(st, OUTPUT)

# ── 리포트: 파라미터 · Brier · 캘리브레이션 · 상식체크 ──────────────────────
d <- st %>% filter(period <= 4, t_eff >= 1, t_eff <= 2400, !is.na(margin_before))
brier <- mean((d$wp_home - d$home_win)^2)
message(sprintf("σ=%.4f  β=%.3f  | 시작 마진 SD≈%.1f점  | Brier=%.4f",
                sigma, beta, sigma * sqrt(2400), brier))

cal <- d %>%
  mutate(bin = cut(wp_home, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(pred = mean(wp_home), actual = mean(home_win), n = n(), .groups = "drop")
message("── 캘리브레이션 (예측WP vs 실제승률) ──")
print(cal)

message("── 상식 체크 ──")
chk <- data.frame(
  desc   = c("동점 시작", "홈+10 2분 홈볼", "홈-3 30초 원정볼", "홈+5 5분 홈볼"),
  margin = c(0, 10, -3, 5), t = c(2400, 120, 30, 300), poss = c(0, 1, -1, 1))
chk$WP <- with(chk, round(wp_home(margin, t, poss, sigma, beta), 3))
print(chk)
