# =============================================================================
# 클러치 슛 워크리스트 생성기
# -----------------------------------------------------------------------------
# 크롤러 산출물(full_pbp.csv)에서 클러치 야투 시도만 뽑아,
#   - 무작위로 섞고
#   - 성공여부(made)는 별도 '정답키'로 분리 (블라인드 코딩 → 결과 편향 방지)
# 하여 주석 도구에 넣을 워크리스트를 만든다.
#
# 출력:
#   *_clutch_worklist.csv   주석자가 채울 파일 (made 없음)
#   *_clutch_answerkey.csv  shot_uid ↔ made (주석 끝난 뒤 조인용, 따로 보관)
#
#   install.packages(c("dplyr","stringr","readr"))
# =============================================================================

library(dplyr); library(stringr); library(readr)

SEASON_LABEL <- "2025_26"
DEFINITION   <- "strict"   # "strict"(≤2:00·≤3점,MVP) / "std"(≤5:00·≤5) / "2poss"(≤5:00·≤6)

INPUT          <- sprintf("KBL_%s_regular_season_full_pbp.csv", SEASON_LABEL)
WORKLIST_FILE  <- sprintf("KBL_%s_clutch_worklist_%s.csv",  SEASON_LABEL, DEFINITION)
ANSWERKEY_FILE <- sprintf("KBL_%s_clutch_answerkey_%s.csv", SEASON_LABEL, DEFINITION)

clean_code <- function(x) trimws(sub("\\.0$", "", as.character(x)))
pnum <- function(q) { q <- as.character(q)
  ifelse(startsWith(q, "Q"), suppressWarnings(as.integer(substr(q, 2, 3))),
  ifelse(startsWith(q, "X"), 4L + suppressWarnings(as.integer(substr(q, 2, 3))), NA_integer_)) }

pbp <- read_csv(INPUT, show_col_types = FALSE) %>%
  mutate(
    a = str_pad(clean_code(a), 3, pad = "0"), t = clean_code(t),
    home_code = clean_code(home_team_code), away_code = clean_code(away_team_code),
    period = pnum(q),
    .mm = suppressWarnings(as.integer(str_extract(time_remaining, "^[0-9]+"))),
    .ss = suppressWarnings(as.integer(str_extract(time_remaining, "[0-9]+$"))),
    sec_left_period = .mm * 60 + .ss
  ) %>%
  arrange(game_id, period, api_row_order) %>%
  group_by(game_id) %>%
  mutate(.hp = ifelse(t == home_code, points, 0L),
         .ap = ifelse(t == away_code, points, 0L),
         margin_before = (cumsum(.hp) - .hp) - (cumsum(.ap) - .ap)) %>%
  ungroup()

late5 <- (pbp$period == 4 & pbp$sec_left_period <= 300) | (pbp$period >= 5)
late2 <- (pbp$period == 4 & pbp$sec_left_period <= 120) | (pbp$period >= 5)
pbp$clutch <- switch(DEFINITION,
  "std"    = late5 & abs(pbp$margin_before) <= 5,
  "2poss"  = late5 & abs(pbp$margin_before) <= 6,
  "strict" = late2 & abs(pbp$margin_before) <= 3)

shots <- pbp %>%
  filter(a %in% c("201","202","205","206","207"), clutch) %>%
  mutate(shot_uid   = paste(game_id, period, api_row_order, sep = "_"),
         shot_value = ifelse(a %in% c("205","206"), 3L, 2L),
         made       = ifelse(a %in% c("201","205","207"), 1L, 0L))

set.seed(42)
shots <- shots[sample(nrow(shots)), ]      # 무작위 셔플 (피로·학습 편향 차단)

# 워크리스트: made 없음. 주석자가 채울 빈 칸을 미리 둠.
worklist <- shots %>% transmute(
  shot_uid, game_id, game_date, home_team, away_team,
  quarter = q, clock = time_remaining, margin_before,
  shooter_en = e, shooter_kr = p, team = t, shot_value,
  loc_x = NA_real_, loc_y = NA_real_, distance_ft = NA_real_, angle_deg = NA_real_,
  zone = NA_character_, contest = NA_integer_, shot_type = NA_character_,
  shot_clock_bin = NA_character_, video_url = NA_character_, note = NA_character_)

answerkey <- shots %>% transmute(shot_uid, made)   # 따로 보관 (코딩 끝난 뒤 조인)

write_excel_csv(worklist,  WORKLIST_FILE)
write_excel_csv(answerkey, ANSWERKEY_FILE)

message("정의: ", DEFINITION, " | 클러치 슛: ", nrow(worklist), "개")
message("워크리스트(주석용): ", WORKLIST_FILE)
message("정답키(분리 보관): ",  ANSWERKEY_FILE)
message("→ 주석 완료 후: read_csv(annotations) %>% left_join(read_csv(answerkey), by='shot_uid')")
