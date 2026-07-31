# =============================================================================
# KBL PBP 전처리: 원본 PBP → 분석용 상태 테이블 / 슛 테이블 / 선수 사전
# -----------------------------------------------------------------------------
# [수정] 러닝스코어 계산의 NA 전파 버그 픽스:
#   R의 ifelse(t==code,...)는 t가 NA인 이벤트(피리어드 시작/종료 등)에서 NA를
#   반환하고, cumsum이 그 뒤 전부를 NA로 오염시킴 → margin/클러치가 전부 NA.
#   해결: ifelse(!is.na(t) & t==code, points, 0L) 로 NA를 0점 처리.
#
# 입력 : KBL_<season>_regular_season_full_pbp.csv
# 출력 : *_state_table.csv / *_shots.csv / *_player_dim.csv
#   install.packages(c("dplyr","stringr","readr","tidyr"))
# =============================================================================

library(dplyr); library(stringr); library(readr); library(tidyr)

SEASON_LABEL <- Sys.getenv("SEASON_LABEL", "2024_25")   # ← 환경변수 또는 여기서 변경
PBP_KIND     <- Sys.getenv("PBP_KIND", "regular_season") # regular_season | playoff
INPUT        <- sprintf("KBL_%s_%s_full_pbp.csv", SEASON_LABEL, PBP_KIND)

# 산출물 접두: 정규시즌은 기존 이름 유지(하위호환), 그 외는 KIND 태그 부착
OUT_PREFIX  <- if (PBP_KIND == "regular_season") sprintf("KBL_%s", SEASON_LABEL) else
               sprintf("KBL_%s_%s", SEASON_LABEL, PBP_KIND)
STATE_FILE  <- sprintf("%s_state_table.csv", OUT_PREFIX)
SHOTS_FILE  <- sprintf("%s_shots.csv",       OUT_PREFIX)
PLAYER_FILE <- sprintf("%s_player_dim.csv",  OUT_PREFIX)

clean_code <- function(x) trimws(sub("\\.0$", "", as.character(x)))

# ── 로드 & 코드 표준화 ──────────────────────────────────────────────────────
pbp <- read_csv(INPUT, show_col_types = FALSE) %>%
  mutate(
    a         = str_pad(clean_code(a), 3, pad = "0"),
    t         = clean_code(t),
    home_code = clean_code(home_team_code),
    away_code = clean_code(away_team_code)
  )

# ── 이벤트 라벨 (확정 코드만; 미확정은 code_XXX로 보존) ──────────────────────
label_map <- c(
  "201" = "made_2",  "202" = "miss_2",  "205" = "made_3",  "206" = "miss_3",
  "203" = "made_ft", "204" = "miss_ft", "207" = "made_2",
  "209" = "oreb",    "210" = "dreb",    "211" = "assist",  "213" = "block",
  "216" = "foul",    "225" = "ft_awarded",
  "101" = "sub_in",  "102" = "sub_out",
  "001" = "period_start", "009" = "period_end", "003" = "timeout"
)
pbp$event <- unname(label_map[pbp$a])
pbp$event[is.na(pbp$event)] <- paste0("code_", pbp$a[is.na(pbp$event)])

# ── 시간 / 피리어드 (m,s 컬럼 사용 → 결측 없음) ─────────────────────────────
pnum <- function(q) {
  q <- as.character(q)
  ifelse(startsWith(q, "Q"), suppressWarnings(as.integer(substr(q, 2, 3))),
  ifelse(startsWith(q, "X"), 4L + suppressWarnings(as.integer(substr(q, 2, 3))),
         NA_integer_))
}
pbp <- pbp %>%
  mutate(
    period          = pnum(q),
    sec_left_period = suppressWarnings(as.integer(m)) * 60 + suppressWarnings(as.integer(s)),
    is_overtime     = period >= 5,
    game_sec_remaining = ifelse(period <= 4,
                                sec_left_period + 600 * (4 - period),
                                sec_left_period)
  )

# ── 정렬 & 러닝스코어(홈 기준) — NA-safe ───────────────────────────────────
pbp <- pbp %>%
  arrange(game_id, period, api_row_order) %>%
  group_by(game_id) %>%
  mutate(
    .hp           = ifelse(!is.na(t) & t == home_code, points, 0L),   # [FIX] NA팀 → 0점
    .ap           = ifelse(!is.na(t) & t == away_code, points, 0L),   # [FIX]
    home_after    = cumsum(.hp),
    away_after    = cumsum(.ap),
    home_before   = home_after - .hp,
    away_before   = away_after - .ap,
    margin_before = home_before - away_before,
    margin_after  = home_after - away_after
  ) %>%
  ungroup()

# ── 공격권 추론 (이벤트 유형 → 공격 팀, 그 뒤 forward-fill) ──────────────────
off_is_t   <- c("201","202","203","204","205","206","207","209","211")
off_is_opp <- c("210","213","216")
pbp <- pbp %>%
  mutate(offense_team = case_when(
    a %in% off_is_t   ~ t,
    a %in% off_is_opp ~ ifelse(t == home_code, away_code, home_code),
    TRUE              ~ NA_character_
  )) %>%
  group_by(game_id) %>%
  fill(offense_team, .direction = "down") %>%
  ungroup() %>%
  mutate(possession_home = offense_team == home_code)

# ── 클러치 플래그 (직전 마진 기준) ──────────────────────────────────────────
pbp <- pbp %>%
  mutate(
    clutch_std    = ((period == 4 & sec_left_period <= 300) | (period >= 5)) & abs(margin_before) <= 5,
    clutch_2poss  = ((period == 4 & sec_left_period <= 300) | (period >= 5)) & abs(margin_before) <= 6,
    clutch_strict = ((period == 4 & sec_left_period <= 120) | (period >= 5)) & abs(margin_before) <= 3
  )

# ── 선수 표준 ID (영문명 기준으로 동명이인 분리) ────────────────────────────
player_dim <- pbp %>%
  filter(!is.na(e)) %>%
  distinct(e, p) %>%
  group_by(e) %>%
  summarise(name_kr = first(p), .groups = "drop") %>%
  arrange(e) %>%
  mutate(player_id = sprintf("P%03d", row_number())) %>%
  rename(name_en = e) %>%
  select(player_id, name_en, name_kr)

pbp <- pbp %>%
  left_join(player_dim %>% select(player_id, name_en), by = c("e" = "name_en"))

# 주: 어시스트(211)는 슛과 인접/시간정합/링크(c)가 모두 성립하지 않아
#     특정 슛에 신뢰성 있게 귀속할 수 없으므로 assisted 컬럼을 두지 않는다.
#     (KBL 규칙상 어시스트는 파울 얻은 슛·자유투 실패 등에도 붙어, 성공 슛
#      인접으로는 판정 불가. 레전드/추가 필드 확보 시 재도입.)

# ── 상태 테이블 저장 ────────────────────────────────────────────────────────
state_table <- pbp %>% select(-.hp, -.ap)
write_excel_csv(state_table, STATE_FILE)

# ── 슛 테이블 (야투 시도만) ─────────────────────────────────────────────────
shots <- pbp %>%
  filter(a %in% c("201","202","205","206","207")) %>%
  transmute(
    game_id, game_date, home_team, away_team, home_code, away_code,
    period, sec_left_period, game_sec_remaining, is_overtime,
    margin_before, possession_home,
    shooter_id = player_id, shooter_en = e, shooter_kr = p, team = t,
    shot_value = ifelse(a %in% c("205","206"), 3L, 2L),
    made       = ifelse(a %in% c("201","205","207"), 1L, 0L),
    clutch_std, clutch_2poss, clutch_strict
  )
write_excel_csv(shots, SHOTS_FILE)
write_excel_csv(player_dim, PLAYER_FILE)

# ── 요약 리포트 ─────────────────────────────────────────────────────────────
message("── 전처리 완료 ──")
message("상태 테이블 행: ", nrow(state_table), " (", STATE_FILE, ")")
message("야투 시도: ", nrow(shots),
        " | 클러치(표준): ", sum(shots$clutch_std,    na.rm = TRUE),
        " | 클러치(엄격): ", sum(shots$clutch_strict, na.rm = TRUE), " (", SHOTS_FILE, ")")
message("선수 수: ", nrow(player_dim), " (", PLAYER_FILE, ")")

chk <- state_table %>%
  group_by(game_id) %>%
  summarise(hf = max(home_after), af = max(away_after),
            oh = first(official_home_score), oa = first(official_away_score),
            .groups = "drop") %>%
  summarise(match_rate = mean(hf == oh & af == oa, na.rm = TRUE))
message("러닝스코어 무결성: ", round(chk$match_rate * 100, 1), "% 경기 일치")
