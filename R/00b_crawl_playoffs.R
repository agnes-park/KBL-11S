# =============================================================================
# KBL 플레이오프 PBP 수집 스크립트
# -----------------------------------------------------------------------------
# 정규시즌 크롤러(00_crawl.R)의 검증된 헬퍼(fetch/parse/validate/repair)를 재사용하고,
# 스케줄 수집만 '플레이오프'용으로 바꾼다. 산출물 스키마는 정규시즌과 동일 +
# 라운드(round=seasonCategoryName) 컬럼 추가(레버리지 가중·시리즈 분석용).
#
# ⚠ 실행 환경: 이 스크립트는 api.kbl.or.kr 에 접근 가능한 곳에서 실행해야 한다.
#    (관리형 원격 세션은 egress 정책으로 kbl.or.kr 이 차단될 수 있음 — 로컬 실행 권장.)
#
# 사용법 (data/ 에서):
#   SEASON_KEY=2024_25 Rscript ../R/00b_crawl_playoffs.R
#   → KBL_2024_25_playoff_full_pbp.csv / _playoff_schedule.csv / _playoff_score_validation.csv
#   수집 후 무결성 검증:  PBP_KIND=playoff Rscript ../R/15_data_audit.R
#
# 첫 실행 시 콘솔에 찍히는 "카테고리 분포" 표를 반드시 확인할 것 —
#   플레이오프 게임이 제대로 잡혔는지(정규시즌 제외가 맞는지) 눈으로 검증.
#   install.packages(c("httr2","jsonlite","dplyr","stringr","readr","tibble"))
# =============================================================================

# ── 00_crawl.R 의 헬퍼를 자동실행 없이 로드 ─────────────────────────────────
get_script_dir <- function() {
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else "."
}
.orig_noautorun <- Sys.getenv("KBL_CRAWL_NOAUTORUN", "")
Sys.setenv(KBL_CRAWL_NOAUTORUN = "1")                       # 00_crawl.R 자동실행 억제
source(file.path(get_script_dir(), "00_crawl.R"), local = FALSE)
Sys.setenv(KBL_CRAWL_NOAUTORUN = .orig_noautorun)           # 원복 → 00b 자동실행 정상화
# 재사용: fetch_raw, parse_pbp_json, get_game_pbp, collect_season_pbp,
#         validate_scores, repair_games, attach_metadata, clean_code, month_range,
#         HEADERS, QUARTERS, POINT_MAP, CACHE_DIR

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── 플레이오프 월 설정 (KBL 포스트시즌은 대략 3~5월; 여유 있게 3~6월) ────────
PLAYOFF_MONTHS <- list(
  "2023_24" = list(c(2024,3), c(2024,4), c(2024,5), c(2024,6)),
  "2024_25" = list(c(2025,3), c(2025,4), c(2025,5), c(2025,6)),
  "2025_26" = list(c(2026,3), c(2026,4), c(2026,5), c(2026,6))
  # ── 과거 시즌(‘큰 무대에 강한 선수’ 지속성 검정 검정력용) 추가 예시 ──
  # "2022_23" = list(c(2023,3), c(2023,4), c(2023,5)),
  # "2021_22" = list(c(2022,3), c(2022,4), c(2022,5)),
)

SEASON_KEY <- Sys.getenv("SEASON_KEY", "2024_25")
MONTHS     <- PLAYOFF_MONTHS[[SEASON_KEY]]
if (is.null(MONTHS)) stop("PLAYOFF_MONTHS 에 없는 시즌: ", SEASON_KEY)
# 서버가 seasonCategory 필터를 요구하면 후보 코드를 넣어 시도(비우면 전체 수신 후 클라이언트 필터)
CATEGORY_QUERY <- Sys.getenv("KBL_PO_CATEGORY", "")   # 예: "P" 로 강제하고 싶을 때
CACHE_DIR      <- "raw_playoff"
message("선택 시즌(플레이오프): ", SEASON_KEY)

# ── 플레이오프 일정 수집: 정규시즌(‘정규’) 제외 = 포스트시즌만 ──────────────
collect_playoff_schedule <- function(months, category_query = "") {
  url <- "https://api.kbl.or.kr/match/list"
  frames <- list()
  for (ym in months) {
    rng <- month_range(ym[1], ym[2])
    params <- list(fromDate = rng[1], toDate = rng[2], tcodeList = "all", seasonGrade = 1)
    if (nzchar(category_query)) params$seasonCategory <- category_query
    txt <- tryCatch(fetch_raw(url, params), error = function(e) {
      message(sprintf("%d-%02d 일정 수집 실패: %s", ym[1], ym[2], conditionMessage(e))); NULL })
    if (!is.null(txt)) {
      data <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
      if (is.data.frame(data) && nrow(data) > 0) {
        frames[[length(frames) + 1]] <- tibble::as_tibble(data)
        message(sprintf("%d-%02d: %d건", ym[1], ym[2], nrow(data)))
      }
    }
    Sys.sleep(0.3)
  }
  if (length(frames) == 0) stop("수집된 일정이 없습니다. (플레이오프 기간/카테고리 확인)")
  schedule <- dplyr::bind_rows(frames)

  schedule$game_id <- as.character(schedule$gmkey %||% schedule$game_id %||%
                                     stop("gmkey/game_id 열 없음"))

  # ── 카테고리 분포 로그 (첫 실행 시 눈으로 확인!) ──
  if (all(c("seasonCategory","seasonCategoryName") %in% names(schedule))) {
    message("── 수신 카테고리 분포 (정규 제외 전) ──")
    print(dplyr::count(schedule, seasonCategory, seasonCategoryName))
  }

  # 포스트시즌만 남김: '정규' 이름 제외 (코드 미상 대비 이름 기반이 가장 안전)
  if ("seasonCategoryName" %in% names(schedule))
    schedule <- dplyr::filter(schedule, !grepl("정규", seasonCategoryName))
  else if ("seasonCategory" %in% names(schedule))
    schedule <- dplyr::filter(schedule, as.character(seasonCategory) != "R")
  if ("seasonGrade" %in% names(schedule))
    schedule <- dplyr::filter(schedule, suppressWarnings(as.numeric(seasonGrade)) == 1)
  # 실제 치러진 경기만(점수 존재)
  if ("isEnded" %in% names(schedule))
    schedule <- dplyr::filter(schedule, suppressWarnings(as.numeric(isEnded)) == 1 |
                                        (!is.na(scoreH) & suppressWarnings(as.numeric(scoreH)) > 0))

  schedule <- schedule[!duplicated(schedule$game_id, fromLast = TRUE), ]
  sc <- intersect(c("gameDate","gameStart","game_id"), names(schedule))
  if (length(sc)) schedule <- schedule[do.call(order, schedule[sc]), ]
  tibble::as_tibble(schedule)
}

# 라운드(seasonCategoryName)를 PBP에 추가로 결합
attach_round <- function(pbp, schedule_df) {
  if (!"seasonCategoryName" %in% names(schedule_df)) return(pbp)
  rd <- schedule_df |>
    dplyr::distinct(game_id, .keep_all = TRUE) |>
    dplyr::transmute(game_id = as.character(game_id), round = as.character(seasonCategoryName))
  dplyr::left_join(pbp, rd, by = "game_id")
}

# ── 실행 ────────────────────────────────────────────────────────────────────
run_playoff_crawler <- function() {
  schedule <- collect_playoff_schedule(MONTHS, CATEGORY_QUERY)
  message("플레이오프 경기 수: ", dplyr::n_distinct(schedule$game_id))
  if (dplyr::n_distinct(schedule$game_id) == 0) stop("플레이오프 경기가 0개 — 월/카테고리 설정 확인.")

  res <- collect_season_pbp(schedule, cache_dir = CACHE_DIR)
  pbp <- res$pbp; failures <- res$failures
  message("PBP 경기 수: ", dplyr::n_distinct(pbp$game_id),
          " | 이벤트 수: ", nrow(pbp), " | 실패: ", nrow(failures))

  pbp <- repair_games(pbp, schedule, rounds = 2, cache_dir = CACHE_DIR)

  validation <- validate_scores(pbp, schedule)
  print(table(validation$score_match, useNA = "ifany"))
  bad <- validation[!validation$score_match, ]; if (nrow(bad)) print(bad)

  pbp <- attach_metadata(pbp, schedule, SEASON_KEY)
  pbp <- attach_round(pbp, schedule)

  readr::write_excel_csv(pbp,        sprintf("KBL_%s_playoff_full_pbp.csv",  SEASON_KEY))
  readr::write_excel_csv(schedule,   sprintf("KBL_%s_playoff_schedule.csv",  SEASON_KEY))
  readr::write_excel_csv(validation, sprintf("KBL_%s_playoff_score_validation.csv", SEASON_KEY))
  if (nrow(failures)) readr::write_excel_csv(failures, sprintf("KBL_%s_playoff_collection_failures.csv", SEASON_KEY))

  message("저장 완료 (플레이오프): KBL_", SEASON_KEY, "_playoff_full_pbp.csv 등")
  message("다음: PBP_KIND=playoff Rscript ../R/15_data_audit.R  로 무결성 검증")
  invisible(list(pbp = pbp, schedule = schedule, validation = validation, failures = failures))
}

if (Sys.getenv("KBL_CRAWL_NOAUTORUN", "") == "") result <- run_playoff_crawler()
