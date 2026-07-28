# =============================================================================
# WP 모델 2단계: 레버리지(Leverage Index)
# -----------------------------------------------------------------------------
# 각 슛의 레버리지 = |WP(성공했다면) - WP(실패했다면)|
#   성공: 마진 ± 슛값(홈부호), 공격권 → 상대
#   실패: 마진 그대로, 공격리바(같은팀) or 수비리바(상대) 를 리그 OReb%로 가중
# LI = 레버리지 / 리그평균레버리지  (1 = 평균, 3 = 평균의 3배)
#
# 입력 : KBL_<season>_state_table.csv, KBL_wp_model.rds
# 출력 : *_shots_leverage.csv
#
# 파이썬 검증(2024-25): OReb=0.306, 클러치 LI 3.12 vs 비클러치 0.89, 가비지 0.08
#   install.packages(c("dplyr","readr","stringr"))
# =============================================================================

library(dplyr); library(readr); library(stringr)

SEASON_LABEL <- Sys.getenv("SEASON_LABEL", "2024_25")
st  <- read_csv(sprintf("KBL_%s_state_table.csv", SEASON_LABEL), show_col_types = FALSE)
mdl <- readRDS("KBL_wp_model.rds"); sigma <- mdl$sigma; beta <- mdl$beta

# ── 타입 정규화 (CSV 재로드 시 코드/팀이 숫자로 읽히는 것 방지) ─────────────
norm <- function(x) sub("\\.0$", "", as.character(x))
st <- st %>% mutate(
  a = str_pad(norm(a), 3, pad = "0"),
  t = norm(t), home_code = norm(home_code), away_code = norm(away_code)
)

# ── 확산 WP 함수 ────────────────────────────────────────────────────────────
wp <- function(margin, t_rem, poss) pnorm((margin + beta * poss) / (sigma * sqrt(pmax(t_rem, 0.5))))

# ── 리그 평균 공격리바운드 확률 ─────────────────────────────────────────────
st <- st %>% arrange(game_id, period, api_row_order) %>%
  group_by(game_id) %>% mutate(next_a = lead(a)) %>% ungroup()
reb <- st %>% filter(a %in% c("202","206"), next_a %in% c("209","210"))
oreb_rate <- mean(reb$next_a == "209")

# ── 슛별 레버리지 ───────────────────────────────────────────────────────────
sh <- st %>%
  filter(a %in% c("201","202","205","206","207")) %>%
  mutate(
    t_rem = game_sec_remaining,
    val   = ifelse(a %in% c("205","206"), 3, 2),
    sign  = ifelse(t == home_code, 1, -1),                 # 홈 슛 +, 원정 슛 -
    poss  = ifelse(is.na(possession_home), 0, ifelse(possession_home, 1, -1)),
    wp_make = wp(margin_before + sign * val, t_rem, -sign),  # 성공 → 공격권 상대로
    wp_miss = oreb_rate      * wp(margin_before, t_rem,  sign) +   # 공격리바: 같은 팀
              (1 - oreb_rate) * wp(margin_before, t_rem, -sign),   # 수비리바: 상대
    leverage = abs(wp_make - wp_miss)
  )
mean_lev <- mean(sh$leverage, na.rm = TRUE)
sh <- sh %>% mutate(LI = leverage / mean_lev)

# ── 저장 ────────────────────────────────────────────────────────────────────
out <- sh %>% transmute(
  game_id, game_date, home_team, away_team,
  period, sec_left_period, margin_before,
  shooter_id = player_id, shooter_en = e, shooter_kr = p, team = t,
  shot_value = val, made = ifelse(a %in% c("201","205","207"), 1L, 0L),
  wp_make, wp_miss, leverage, LI,
  clutch_std, clutch_2poss, clutch_strict
)
write_excel_csv(out, sprintf("KBL_%s_shots_leverage.csv", SEASON_LABEL))

# ── 리포트 ──────────────────────────────────────────────────────────────────
message(sprintf("공격리바 확률=%.3f | 평균 레버리지=%.4f | 슛 %d개",
                oreb_rate, mean_lev, nrow(sh)))
message(sprintf("LI 분포: 중앙값 %.2f | 90p %.2f | 99p %.2f | 최대 %.1f",
                median(sh$LI, na.rm = TRUE), quantile(sh$LI, .9, na.rm = TRUE),
                quantile(sh$LI, .99, na.rm = TRUE), max(sh$LI, na.rm = TRUE)))
message(sprintf("클러치 평균 LI %.2f vs 비클러치 %.2f | 가비지(20+) %.3f",
                mean(sh$LI[sh$clutch_std], na.rm = TRUE),
                mean(sh$LI[!sh$clutch_std], na.rm = TRUE),
                mean(sh$LI[abs(sh$margin_before) >= 20], na.rm = TRUE)))
