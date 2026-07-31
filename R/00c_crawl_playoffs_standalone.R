# =============================================================================
# KBL 플레이오프 수집 — 자립형(standalone) 스크립트
# -----------------------------------------------------------------------------
# 복붙해서 그대로 실행 가능 (source 의존성 없음). R 콘솔/RStudio/Rscript 모두 OK.
# ⚠ api.kbl.or.kr 접근 가능한 로컬에서 실행할 것.
#
#   1) 아래 CONFIG의 DATA_DIR을 CSV 저장 폴더로 바꾼다(정규시즌 CSV가 있는 data/ 권장).
#   2) 통째로 실행. → KBL_<season>_playoff_full_pbp.csv / _playoff_schedule.csv /
#      _playoff_score_validation.csv 생성 + 스코어 일치 요약 출력.
# =============================================================================

# ── CONFIG ───────────────────────────────────────────────────────────────────
DATA_DIR <- "."                                   # 예: "~/kbl_clutch_project/data"
SEASONS  <- c("2023_24", "2024_25", "2025_26")    # 과거시즌 추가 시 PLAYOFF_MONTHS에도 추가
PLAYOFF_MONTHS <- list(                           # KBL 포스트시즌 대략 3~5월(여유 3~6월)
  "2023_24" = list(c(2024,3), c(2024,4), c(2024,5), c(2024,6)),
  "2024_25" = list(c(2025,3), c(2025,4), c(2025,5), c(2025,6)),
  "2025_26" = list(c(2026,3), c(2026,4), c(2026,5), c(2026,6))
  # "2022_23" = list(c(2023,3), c(2023,4), c(2023,5)),   # 과거시즌 예시
)
CATEGORY_QUERY <- ""   # 서버가 카테고리 필터 요구 시에만: 예) "P"

# ── 패키지 ──────────────────────────────────────────────────────────────────
options(repos = c(CRAN = "https://cloud.r-project.org"))
.pkgs <- c("httr2","jsonlite","dplyr","stringr","readr","tibble")
.new  <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.new)) install.packages(.new)
suppressMessages(invisible(lapply(.pkgs, library, character.only = TRUE)))

if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
setwd(DATA_DIR)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── 상수 ────────────────────────────────────────────────────────────────────
HEADERS <- c(accept = "application/json, text/plain, */*", channel = "WEB", lang = "ko",
             teamcode = "XX", origin = "https://kbl.or.kr", referer = "https://kbl.or.kr/",
             `x-requested-with` = "XMLHttpRequest")
QUARTERS  <- c("Q1","Q2","Q3","Q4","X1","X2","X3","X4","X5")
POINT_MAP <- c("201" = 2L, "203" = 1L, "205" = 3L, "207" = 2L)
CACHE_DIR <- "raw_playoff"

# ── 유틸 ────────────────────────────────────────────────────────────────────
clean_code <- function(x) { x <- as.character(x); x <- sub("\\.0$", "", x); trimws(x) }
month_range <- function(year, month) {
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  last  <- seq(first, by = "month", length.out = 2)[2] - 1
  c(format(first, "%Y%m%d"), format(last, "%Y%m%d"))
}
fetch_raw <- function(url, params) {
  req <- request(url) |> req_headers(!!!as.list(HEADERS)) |> req_url_query(!!!params) |>
    req_timeout(30) |>
    req_retry(max_tries = 4, retry_on_failure = TRUE,
              is_transient = function(resp) resp_status(resp) %in% c(429,500,502,503,504),
              backoff = function(i) 2^i * 0.5 + runif(1, 0, 0.3))
  resp_body_string(req_perform(req))
}
parse_pbp_json <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  data <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(data)) return(NULL)
  if (is.data.frame(data)) { if (nrow(data) == 0) return(NULL); return(as_tibble(data)) }
  if (is.list(data) && length(data) == 0) return(NULL)
  as_tibble(data)
}
get_game_pbp <- function(game_id, cache_dir = CACHE_DIR) {
  url <- sprintf("https://api.kbl.or.kr/match/%s/text-cast", game_id)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  frames <- list()
  for (quarter in QUARTERS) {
    cache_file <- file.path(cache_dir, sprintf("%s_%s.json", game_id, quarter))
    if (file.exists(cache_file)) {
      txt <- read_file(cache_file)
    } else {
      txt <- tryCatch(fetch_raw(url, list(quarterList = quarter)), error = function(e) NA_character_)
      if (!is.na(txt)) write_file(txt, cache_file)
      Sys.sleep(0.15 + runif(1, 0, 0.1))
    }
    q <- if (is.na(txt)) NULL else parse_pbp_json(txt)
    if (!is.null(q)) {
      q$quarter_requested <- quarter; q$api_row_order <- seq_len(nrow(q))
      frames[[length(frames) + 1]] <- q
    } else if (startsWith(quarter, "X")) break
  }
  if (length(frames) == 0) stop(sprintf("%s: PBP 데이터 없음", game_id))
  pbp <- bind_rows(frames)
  for (col in c("a","t","m","s","n","p","e","q","c","f")) if (!col %in% names(pbp)) pbp[[col]] <- NA
  pbp$a <- str_pad(clean_code(pbp$a), 3, pad = "0"); pbp$t <- clean_code(pbp$t)
  pbp$n <- suppressWarnings(as.numeric(pbp$n))
  pbp$m <- suppressWarnings(as.numeric(pbp$m)); pbp$s <- suppressWarnings(as.numeric(pbp$s))
  pbp$time_remaining <- ifelse(is.na(pbp$m) | is.na(pbp$s), NA_character_,
                               sprintf("%d:%02d", as.integer(pbp$m), as.integer(pbp$s)))
  pts <- unname(POINT_MAP[pbp$a]); pts[is.na(pts)] <- 0L
  pbp$points <- pts; pbp$game_id <- as.character(game_id)
  pbp
}
collect_season_pbp <- function(schedule_df, cache_dir = CACHE_DIR) {
  game_ids <- unique(as.character(schedule_df$game_id)); total <- length(game_ids)
  collected <- vector("list", total); failures <- list()
  for (i in seq_along(game_ids)) {
    gid <- game_ids[i]
    g <- tryCatch(get_game_pbp(gid, cache_dir), error = function(e) e)
    if (inherits(g, "error")) {
      failures[[length(failures) + 1]] <- tibble(game_id = gid, error = conditionMessage(g))
      message(sprintf("[%d/%d] %s 실패: %s", i, total, gid, conditionMessage(g)))
    } else { collected[[i]] <- g; message(sprintf("[%d/%d] %s 완료 (%d행)", i, total, gid, nrow(g))) }
    Sys.sleep(0.25)
  }
  collected <- collected[!vapply(collected, is.null, logical(1))]
  if (length(collected) == 0) stop("수집 성공 경기가 없습니다.")
  list(pbp = bind_rows(collected), failures = if (length(failures)) bind_rows(failures) else tibble())
}
validate_scores <- function(pbp_df, schedule_df) {
  meta <- distinct(schedule_df, game_id, .keep_all = TRUE)
  meta$game_id <- as.character(meta$game_id); meta$tcodeH <- clean_code(meta$tcodeH); meta$tcodeA <- clean_code(meta$tcodeA)
  pbp <- pbp_df; pbp$a <- str_pad(clean_code(pbp$a), 3, pad = "0"); pbp$t <- clean_code(pbp$t)
  pts <- unname(POINT_MAP[pbp$a]); pts[is.na(pts)] <- 0L; pbp$pts <- pts
  team_pts <- pbp |> filter(pts > 0, !t %in% c("","0","nan","NA","<NA>")) |>
    group_by(game_id, t) |> summarise(pts = sum(pts), .groups = "drop")
  rows <- lapply(split(meta, meta$game_id), function(m) {
    m <- m[1, ]; gid <- m$game_id; tp <- team_pts[team_pts$game_id == gid, ]
    rh <- sum(tp$pts[tp$t == m$tcodeH]); ra <- sum(tp$pts[tp$t == m$tcodeA])
    oh <- as.integer(as.numeric(m$scoreH)); oa <- as.integer(as.numeric(m$scoreA))
    tibble(game_id = gid, game_date = m$gameDate, home_team = m$tnameH, official_home_score = oh,
           reconstructed_home_score = rh, home_difference = oh - rh, away_team = m$tnameA,
           official_away_score = oa, reconstructed_away_score = ra, away_difference = oa - ra,
           score_match = (oh == rh && oa == ra))
  })
  bind_rows(rows) |> arrange(game_date, game_id)
}
repair_games <- function(pbp, schedule_df, rounds = 2, cache_dir = CACHE_DIR) {
  for (r in seq_len(rounds)) {
    val <- validate_scores(pbp, schedule_df); bad <- val$game_id[!val$score_match]
    if (length(bad) == 0) { message("모든 경기 스코어 일치 ✔"); break }
    message(sprintf("[복구 %d회차] 재수집 대상: %s", r, paste(bad, collapse = ", ")))
    for (gid in bad) { old <- list.files(cache_dir, pattern = paste0("^", gid, "_"), full.names = TRUE); if (length(old)) file.remove(old) }
    fixed <- list()
    for (gid in bad) { g <- tryCatch(get_game_pbp(gid, cache_dir), error = function(e) e)
      if (!inherits(g, "error")) fixed[[gid]] <- g else message(sprintf("%s 재수집 실패: %s", gid, conditionMessage(g))) }
    if (length(fixed)) pbp <- bind_rows(pbp[!pbp$game_id %in% bad, ], bind_rows(fixed))
  }
  pbp
}
attach_metadata <- function(pbp, schedule_df, season_label) {
  meta <- schedule_df |> distinct(game_id, .keep_all = TRUE) |> transmute(
    game_id = as.character(game_id), season = season_label, game_date = gameDate,
    home_team = tnameH, away_team = tnameA, home_team_code = clean_code(tcodeH),
    away_team_code = clean_code(tcodeA), official_home_score = as.integer(as.numeric(scoreH)),
    official_away_score = as.integer(as.numeric(scoreA)))
  pbp |> mutate(game_id = as.character(game_id)) |> left_join(meta, by = "game_id")
}
attach_round <- function(pbp, schedule_df) {
  if (!"seasonCategoryName" %in% names(schedule_df)) return(pbp)
  rd <- schedule_df |> distinct(game_id, .keep_all = TRUE) |>
    transmute(game_id = as.character(game_id), round = as.character(seasonCategoryName))
  left_join(pbp, rd, by = "game_id")
}
# ── 플레이오프 일정: 정규('정규') 제외 = 포스트시즌만 ───────────────────────
collect_playoff_schedule <- function(months, category_query = "") {
  url <- "https://api.kbl.or.kr/match/list"; frames <- list()
  for (ym in months) {
    rng <- month_range(ym[1], ym[2])
    params <- list(fromDate = rng[1], toDate = rng[2], tcodeList = "all", seasonGrade = 1)
    if (nzchar(category_query)) params$seasonCategory <- category_query
    txt <- tryCatch(fetch_raw(url, params), error = function(e) {
      message(sprintf("%d-%02d 일정 실패: %s", ym[1], ym[2], conditionMessage(e))); NULL })
    if (!is.null(txt)) {
      data <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
      if (is.data.frame(data) && nrow(data) > 0) {
        frames[[length(frames) + 1]] <- as_tibble(data)
        message(sprintf("%d-%02d: %d건", ym[1], ym[2], nrow(data)))
      }
    }
    Sys.sleep(0.3)
  }
  if (length(frames) == 0) stop("수집된 일정이 없습니다.")
  schedule <- bind_rows(frames)
  schedule$game_id <- as.character(schedule$gmkey %||% schedule$game_id %||% stop("gmkey/game_id 없음"))
  if (all(c("seasonCategory","seasonCategoryName") %in% names(schedule))) {
    message("── 수신 카테고리 분포 (정규 제외 전) ──"); print(count(schedule, seasonCategory, seasonCategoryName))
  }
  if ("seasonCategoryName" %in% names(schedule)) schedule <- filter(schedule, !grepl("정규", seasonCategoryName))
  else if ("seasonCategory" %in% names(schedule)) schedule <- filter(schedule, as.character(seasonCategory) != "R")
  if ("seasonGrade" %in% names(schedule)) schedule <- filter(schedule, suppressWarnings(as.numeric(seasonGrade)) == 1)
  if ("isEnded" %in% names(schedule))
    schedule <- filter(schedule, suppressWarnings(as.numeric(isEnded)) == 1 |
                                 (!is.na(scoreH) & suppressWarnings(as.numeric(scoreH)) > 0))
  schedule <- schedule[!duplicated(schedule$game_id, fromLast = TRUE), ]
  sc <- intersect(c("gameDate","gameStart","game_id"), names(schedule))
  if (length(sc)) schedule <- schedule[do.call(order, schedule[sc]), ]
  as_tibble(schedule)
}

# ── 실행: 시즌별 수집 ────────────────────────────────────────────────────────
run_one <- function(season_key) {
  months <- PLAYOFF_MONTHS[[season_key]]
  if (is.null(months)) { message("PLAYOFF_MONTHS에 없는 시즌 건너뜀: ", season_key); return(invisible()) }
  message("\n==================== 플레이오프 수집: ", season_key, " ====================")
  schedule <- collect_playoff_schedule(months, CATEGORY_QUERY)
  message("플레이오프 경기 수: ", n_distinct(schedule$game_id))
  if (n_distinct(schedule$game_id) == 0) { message("경기 0개 — 월/카테고리 확인. 건너뜀."); return(invisible()) }
  res <- collect_season_pbp(schedule)
  pbp <- repair_games(res$pbp, schedule, rounds = 2)
  validation <- validate_scores(pbp, schedule)
  message(sprintf("스코어 일치: %d/%d 경기", sum(validation$score_match), nrow(validation)))
  bad <- validation[!validation$score_match, ]; if (nrow(bad)) print(bad)
  pbp <- attach_round(attach_metadata(pbp, schedule, season_key), schedule)
  write_excel_csv(pbp,        sprintf("KBL_%s_playoff_full_pbp.csv", season_key))
  write_excel_csv(schedule,   sprintf("KBL_%s_playoff_schedule.csv", season_key))
  write_excel_csv(validation, sprintf("KBL_%s_playoff_score_validation.csv", season_key))
  if (nrow(res$failures)) write_excel_csv(res$failures, sprintf("KBL_%s_playoff_collection_failures.csv", season_key))
  message("저장 완료: KBL_", season_key, "_playoff_full_pbp.csv 등")
}

for (S in SEASONS) run_one(S)
message("\n── 전체 완료. CSV 위치: ", normalizePath(getwd()), " ──")
