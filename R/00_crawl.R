# =============================================================================
# KBL 정규시즌 PBP 수집 스크립트 (R 버전)
# -----------------------------------------------------------------------------
# 원본 Colab 노트북을 R로 포팅하고 다음 보완점을 반영했습니다.
#   1. 재시도 + 지수 백오프 (httr2::req_retry) — 순간적 오류로 경기를 잃지 않음
#   2. 쿼터 단위 내결함성 — 한 쿼터가 실패해도 나머지 쿼터는 살림
#   3. 자동 복구 루프 (repair_games) — 스코어 불일치/실패 경기를 재수집
#   4. 연장 조기 종료 — 빈 X 쿼터를 만나면 이후 연장 요청을 중단(헛요청 제거)
#   5. 원본 JSON 캐싱 — 재파싱 시 재크롤링 불필요, 중단 후 재개 가능
#   6. clean_code 정규식 수정 — 끝자리 '.0'만 제거
#   7. time_remaining 결측은 NA로 (0:00 위장 방지)
#
# 사용법: SEASON_KEY만 바꾸고 스크립트를 통째로 실행하세요.
#   install.packages(c("httr2","jsonlite","dplyr","stringr","readr","tibble"))
#
# ── 추가로 확인하면 좋은 것 (프로젝트 가치가 큼) ────────────────────────────
#   • 이벤트 코드 레전드: 브라우저 개발자도구 > 네트워크 탭에서 문자중계를 열 때
#     코드→한글설명 매핑을 주는 응답/엔드포인트가 있는지 확인 → 턴오버·스틸·파울
#     서브타입 코드의 불확실성을 제거할 수 있음.
#   • 슛 좌표(샷차트) 엔드포인트: 경기 페이지에서 샷차트를 띄울 때 호출되는 API가
#     따로 있는지 확인. x/y 좌표나 존 정보가 있으면 xG 모델이 크게 개선됨.
#   • POINT_MAP 완전성: 레전드로 특수 득점(앤드원 등) 코드가 더 있는지 점검.
#     (코드 208이 49건·0점 처리 중 — 혹시 득점 이벤트인지 확인 권장)
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(tibble)

# ── 시즌 설정 ───────────────────────────────────────────────────────────────
SEASON_CONFIG <- list(
  "2023_24" = list(label = "2023_24",
    months = list(c(2023,10), c(2023,11), c(2023,12), c(2024,1), c(2024,2), c(2024,3))),
  "2024_25" = list(label = "2024_25",
    months = list(c(2024,10), c(2024,11), c(2024,12), c(2025,1), c(2025,2), c(2025,3), c(2025,4))),
  "2025_26" = list(label = "2025_26",
    months = list(c(2025,10), c(2025,11), c(2025,12), c(2026,1), c(2026,2), c(2026,3), c(2026,4)))
)

SEASON_KEY   <- "2024_25"   # ← 여기만 바꾸기
CONFIG       <- SEASON_CONFIG[[SEASON_KEY]]
SEASON_LABEL <- CONFIG$label
MONTHS       <- CONFIG$months
message("선택 시즌: ", SEASON_KEY)

# ── 상수 ────────────────────────────────────────────────────────────────────
HEADERS <- c(
  accept             = "application/json, text/plain, */*",
  channel            = "WEB",
  lang               = "ko",
  teamcode           = "XX",
  origin             = "https://kbl.or.kr",
  referer            = "https://kbl.or.kr/",
  `x-requested-with` = "XMLHttpRequest"
)

QUARTERS  <- c("Q1","Q2","Q3","Q4","X1","X2","X3","X4","X5")
POINT_MAP <- c("201" = 2L, "203" = 1L, "205" = 3L, "207" = 2L)

CACHE_DIR <- "raw"   # 원본 JSON 캐시 폴더

# ── 유틸 ────────────────────────────────────────────────────────────────────
clean_code <- function(x) {
  x <- as.character(x)
  x <- sub("\\.0$", "", x)          # 끝자리 '.0'만 제거 (개선점)
  trimws(x)
}

month_range <- function(year, month) {
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  last  <- seq(first, by = "month", length.out = 2)[2] - 1
  c(format(first, "%Y%m%d"), format(last, "%Y%m%d"))
}

# 재시도 + 지수 백오프가 붙은 GET → 응답 본문(문자열) 반환
fetch_raw <- function(url, params) {
  req <- request(url) |>
    req_headers(!!!as.list(HEADERS)) |>
    req_url_query(!!!params) |>
    req_timeout(30) |>
    req_retry(
      max_tries        = 4,
      retry_on_failure = TRUE,   # 연결 오류도 재시도
      is_transient     = function(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504),
      backoff          = function(i) 2^i * 0.5 + runif(1, 0, 0.3)
    )
  resp_body_string(req_perform(req))
}

# PBP JSON 텍스트 → tibble (비어 있으면 NULL)
parse_pbp_json <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  data <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE),
                   error = function(e) NULL)
  if (is.null(data)) return(NULL)
  if (is.data.frame(data)) {
    if (nrow(data) == 0) return(NULL)
    return(as_tibble(data))
  }
  if (is.list(data) && length(data) == 0) return(NULL)
  as_tibble(data)
}

# ── 일정 수집 ───────────────────────────────────────────────────────────────
collect_regular_schedule <- function(months) {
  url <- "https://api.kbl.or.kr/match/list"
  frames <- list()
  for (ym in months) {
    year <- ym[1]; month <- ym[2]
    rng <- month_range(year, month)
    params <- list(fromDate = rng[1], toDate = rng[2],
                   seasonCategory = "R", tcodeList = "all", seasonGrade = 1)
    txt <- tryCatch(fetch_raw(url, params), error = function(e) {
      message(sprintf("%d-%02d 일정 수집 실패: %s", year, month, conditionMessage(e))); NULL
    })
    if (!is.null(txt)) {
      data <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
      if (is.data.frame(data) && nrow(data) > 0) {
        frames[[length(frames) + 1]] <- as_tibble(data)
        message(sprintf("%d-%02d: %d건", year, month, nrow(data)))
      }
    }
    Sys.sleep(0.3)
  }
  if (length(frames) == 0) stop("수집된 일정이 없습니다.")

  schedule <- bind_rows(frames)

  if ("gmkey" %in% names(schedule))        schedule$game_id <- as.character(schedule$gmkey)
  else if ("game_id" %in% names(schedule)) schedule$game_id <- as.character(schedule$game_id)
  else stop("gmkey 또는 game_id 열이 없습니다.")

  if ("seasonCategory" %in% names(schedule))
    schedule <- filter(schedule, as.character(seasonCategory) == "R")
  if ("seasonGrade" %in% names(schedule))
    schedule <- filter(schedule, suppressWarnings(as.numeric(seasonGrade)) == 1)
  if ("seasonCategoryName" %in% names(schedule))
    schedule <- filter(schedule, grepl("정규", seasonCategoryName))

  schedule <- schedule[!duplicated(schedule$game_id, fromLast = TRUE), ]
  sort_cols <- intersect(c("gameDate", "gameStart", "game_id"), names(schedule))
  if (length(sort_cols)) schedule <- schedule[do.call(order, schedule[sort_cols]), ]
  as_tibble(schedule)
}

# ── 경기별 PBP 수집 (캐싱 + 쿼터 내결함성 + 연장 조기 종료) ──────────────────
get_game_pbp <- function(game_id, cache_dir = CACHE_DIR) {
  url <- sprintf("https://api.kbl.or.kr/match/%s/text-cast", game_id)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  frames <- list()
  for (quarter in QUARTERS) {
    cache_file <- file.path(cache_dir, sprintf("%s_%s.json", game_id, quarter))

    if (file.exists(cache_file)) {
      txt <- read_file(cache_file)                       # 캐시 재사용 → 재크롤링 불필요
    } else {
      txt <- tryCatch(fetch_raw(url, list(quarterList = quarter)),
                      error = function(e) NA_character_)  # 쿼터 실패해도 경기 유지
      if (!is.na(txt)) write_file(txt, cache_file)
      Sys.sleep(0.15 + runif(1, 0, 0.1))
    }

    q <- if (is.na(txt)) NULL else parse_pbp_json(txt)
    if (!is.null(q)) {
      q$quarter_requested <- quarter
      q$api_row_order     <- seq_len(nrow(q))
      frames[[length(frames) + 1]] <- q
    } else if (startsWith(quarter, "X")) {
      break                                              # 빈 연장 이후는 요청 안 함
    }
  }

  if (length(frames) == 0) stop(sprintf("%s: PBP 데이터 없음", game_id))
  pbp <- bind_rows(frames)

  for (col in c("a","t","m","s","n","p","e","q","c","f"))
    if (!col %in% names(pbp)) pbp[[col]] <- NA

  pbp$a <- str_pad(clean_code(pbp$a), 3, pad = "0")
  pbp$t <- clean_code(pbp$t)
  pbp$n <- suppressWarnings(as.numeric(pbp$n))
  pbp$m <- suppressWarnings(as.numeric(pbp$m))
  pbp$s <- suppressWarnings(as.numeric(pbp$s))

  # 시간 결측은 NA로 (0:00 위장 방지)
  pbp$time_remaining <- ifelse(
    is.na(pbp$m) | is.na(pbp$s), NA_character_,
    sprintf("%d:%02d", as.integer(pbp$m), as.integer(pbp$s))
  )

  pts <- unname(POINT_MAP[pbp$a]); pts[is.na(pts)] <- 0L
  pbp$points  <- pts
  pbp$game_id <- as.character(game_id)
  pbp
}

# ── 시즌 전체 수집 ──────────────────────────────────────────────────────────
collect_season_pbp <- function(schedule_df, cache_dir = CACHE_DIR) {
  game_ids <- unique(as.character(schedule_df$game_id))
  total <- length(game_ids)
  collected <- vector("list", total)
  failures  <- list()

  for (i in seq_along(game_ids)) {
    gid <- game_ids[i]
    g <- tryCatch(get_game_pbp(gid, cache_dir), error = function(e) e)
    if (inherits(g, "error")) {
      failures[[length(failures) + 1]] <- tibble(game_id = gid, error = conditionMessage(g))
      message(sprintf("[%d/%d] %s 실패: %s", i, total, gid, conditionMessage(g)))
    } else {
      collected[[i]] <- g
      message(sprintf("[%d/%d] %s 완료 (%d행)", i, total, gid, nrow(g)))
    }
    Sys.sleep(0.25)
  }

  collected <- collected[!vapply(collected, is.null, logical(1))]
  if (length(collected) == 0) stop("수집 성공 경기가 없습니다.")
  list(
    pbp      = bind_rows(collected),
    failures = if (length(failures)) bind_rows(failures) else tibble()
  )
}

# ── 스코어 검증 ─────────────────────────────────────────────────────────────
validate_scores <- function(pbp_df, schedule_df) {
  meta <- distinct(schedule_df, game_id, .keep_all = TRUE)
  meta$game_id <- as.character(meta$game_id)
  meta$tcodeH  <- clean_code(meta$tcodeH)
  meta$tcodeA  <- clean_code(meta$tcodeA)

  pbp <- pbp_df
  pbp$a <- str_pad(clean_code(pbp$a), 3, pad = "0")
  pbp$t <- clean_code(pbp$t)
  pts <- unname(POINT_MAP[pbp$a]); pts[is.na(pts)] <- 0L
  pbp$pts <- pts

  team_pts <- pbp |>
    filter(pts > 0, !t %in% c("", "0", "nan", "NA", "<NA>")) |>
    group_by(game_id, t) |>
    summarise(pts = sum(pts), .groups = "drop")

  rows <- lapply(split(meta, meta$game_id), function(m) {
    m <- m[1, ]
    gid <- m$game_id
    tp  <- team_pts[team_pts$game_id == gid, ]
    rh  <- sum(tp$pts[tp$t == m$tcodeH])
    ra  <- sum(tp$pts[tp$t == m$tcodeA])
    oh  <- as.integer(as.numeric(m$scoreH))
    oa  <- as.integer(as.numeric(m$scoreA))
    tibble(
      game_id = gid, game_date = m$gameDate,
      home_team = m$tnameH, official_home_score = oh,
      reconstructed_home_score = rh, home_difference = oh - rh,
      away_team = m$tnameA, official_away_score = oa,
      reconstructed_away_score = ra, away_difference = oa - ra,
      score_match = (oh == rh && oa == ra)
    )
  })
  bind_rows(rows) |> arrange(game_date, game_id)
}

# ── 자동 복구: 불일치 경기 캐시 삭제 후 재수집 ──────────────────────────────
repair_games <- function(pbp, schedule_df, rounds = 2, cache_dir = CACHE_DIR) {
  for (r in seq_len(rounds)) {
    val <- validate_scores(pbp, schedule_df)
    bad <- val$game_id[!val$score_match]
    if (length(bad) == 0) { message("모든 경기 스코어 일치 ✔"); break }
    message(sprintf("[복구 %d회차] 재수집 대상: %s", r, paste(bad, collapse = ", ")))

    for (gid in bad) {                                   # 캐시 무시하고 강제 재수집
      old <- list.files(cache_dir, pattern = paste0("^", gid, "_"), full.names = TRUE)
      if (length(old)) file.remove(old)
    }

    fixed <- list()
    for (gid in bad) {
      g <- tryCatch(get_game_pbp(gid, cache_dir), error = function(e) e)
      if (!inherits(g, "error")) fixed[[gid]] <- g
      else message(sprintf("%s 재수집 실패: %s", gid, conditionMessage(g)))
    }
    if (length(fixed))
      pbp <- bind_rows(pbp[!pbp$game_id %in% bad, ], bind_rows(fixed))
  }
  pbp
}

# ── 일정 메타데이터를 PBP에 결합 (원본 CSV와 동일한 컬럼 구성) ──────────────
attach_metadata <- function(pbp, schedule_df, season_label) {
  meta <- schedule_df |>
    distinct(game_id, .keep_all = TRUE) |>
    transmute(
      game_id             = as.character(game_id),
      season              = season_label,
      game_date           = gameDate,
      home_team           = tnameH,
      away_team           = tnameA,
      home_team_code      = clean_code(tcodeH),
      away_team_code      = clean_code(tcodeA),
      official_home_score = as.integer(as.numeric(scoreH)),
      official_away_score = as.integer(as.numeric(scoreA))
    )
  pbp |> mutate(game_id = as.character(game_id)) |> left_join(meta, by = "game_id")
}

# =============================================================================
# 실행
# =============================================================================
PBP_FILE        <- sprintf("KBL_%s_regular_season_full_pbp.csv", SEASON_LABEL)
SCHEDULE_FILE   <- sprintf("KBL_%s_regular_season_schedule.csv", SEASON_LABEL)
VALIDATION_FILE <- sprintf("KBL_%s_score_validation.csv", SEASON_LABEL)
FAILURE_FILE    <- sprintf("KBL_%s_collection_failures.csv", SEASON_LABEL)

run_crawler <- function() {
  schedule <- collect_regular_schedule(MONTHS)
  message("정규시즌 경기 수: ", n_distinct(schedule$game_id))
  if (n_distinct(schedule$game_id) != 270) message("주의: 경기 수가 270이 아닙니다.")

  res        <- collect_season_pbp(schedule)
  season_pbp <- res$pbp
  failures   <- res$failures
  message("PBP 경기 수: ", n_distinct(season_pbp$game_id),
          " | 전체 이벤트 수: ", nrow(season_pbp),
          " | 실패 경기 수: ", nrow(failures))

  season_pbp <- repair_games(season_pbp, schedule, rounds = 2)   # 불일치 자동 복구

  validation <- validate_scores(season_pbp, schedule)
  print(table(validation$score_match, useNA = "ifany"))
  bad <- validation[!validation$score_match, ]
  if (nrow(bad)) print(bad)   # 재수집 후에도 남으면 박스스코어로 수동 보정 대상

  season_pbp <- attach_metadata(season_pbp, schedule, SEASON_LABEL)

  write_excel_csv(season_pbp, PBP_FILE)          # UTF-8 BOM (엑셀/한글 안전)
  write_excel_csv(schedule,   SCHEDULE_FILE)
  write_excel_csv(validation, VALIDATION_FILE)
  if (nrow(failures)) write_excel_csv(failures, FAILURE_FILE)

  message("저장 완료:\n  ", PBP_FILE, "\n  ", SCHEDULE_FILE, "\n  ", VALIDATION_FILE)
  invisible(list(pbp = season_pbp, schedule = schedule,
                 validation = validation, failures = failures))
}

# 스크립트를 통째로 실행하면 자동으로 크롤링을 시작합니다.
# 함수만 불러오고 싶으면 아래 줄을 주석 처리하세요.
result <- run_crawler()
