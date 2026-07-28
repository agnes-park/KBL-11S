# =============================================================================
# 세 시즌 통합 (three-season integration): 검정력 향상
# -----------------------------------------------------------------------------
# CLAUDE.md "다음 단계 1"의 구현. 설계 원칙:
#   • 리그 기준선(WP σ·β, OReb%, xG 2/3점)  → 세 시즌 **풀링**(pooled)
#   • 선수 클러치 단위                        → **선수 × 시즌** 유지
#       (시즌 간 지속성 검정이 "클러치 실력 존재"의 진짜 핵심)
#   • 통합 전 필수 검증: 핵심 코드(득점·리바운드)가 세 시즌 동일하게 작동하는가
#       (러닝스코어 무결성 / 2·3점 성공률 / OReb same-team 비율 / 226·227 등장)
#
# 입력 : 각 시즌 KBL_<season>_state_table.csv  (01_preprocess.R 산출물)
#          → 먼저 각 시즌에 대해 01을 실행할 것 (run_multiseason.sh 참고)
# 출력 :
#   KBL_pooled_wp_model.rds              풀링 WP 파라미터(σ,β,oreb_rate,base2,base3)
#   KBL_pooled_shots_scored.csv          전 시즌 슛 + leverage/LI/xg/moe/contribution
#   KBL_multiseason_playerseason.csv     선수×시즌 클러치 집계(축소추정 포함)
#   KBL_multiseason_players_pooled.csv   선수(3시즌 풀링) 클러치 집계
#   ../docs/multiseason_findings.md      검증표 + 존재검정 + 지속성 리포트
#
#   install.packages(c("dplyr","stringr","readr","tidyr"))
# =============================================================================

library(dplyr); library(stringr); library(readr); library(tidyr)
set.seed(1)

SEASONS  <- c("2023_24", "2024_25", "2025_26")
DEFS     <- c("clutch_std", "clutch_2poss", "clutch_strict")
PRIMARY  <- "clutch_std"     # 선수×시즌 산출물의 기준 정의
MIN_N    <- 5                # 지속성 분석: 시즌당 최소 클러치 슛 수
N_PERM   <- 5000             # 순열검정 반복
REPORT   <- "../docs/multiseason_findings.md"

norm <- function(x) sub("\\.0$", "", as.character(x))

# ── 로드 & 시즌 태깅 & 타입 정규화 ──────────────────────────────────────────
load_state <- function(s) {
  f <- sprintf("KBL_%s_state_table.csv", s)
  if (!file.exists(f)) stop(sprintf("없음: %s — 먼저 SEASON_LABEL=%s Rscript ../R/01_preprocess.R 실행", f, s))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(
      season    = .env$s,   # 주의: PBP에 컬럼 s(초)가 있어 data-masking됨 → .env$s로 고정
      a         = str_pad(norm(a), 3, pad = "0"),
      t         = norm(t),
      home_code = norm(home_code),
      away_code = norm(away_code)
    )
}
ST <- bind_rows(lapply(SEASONS, load_state))
message(sprintf("풀링 상태테이블: %d행 / %d시즌 / %d경기",
                nrow(ST), n_distinct(ST$season), n_distinct(ST$game_id)))

SHOT_CODES <- c("201", "202", "205", "206", "207")
MAKE_CODES <- c("201", "205", "207")

# =============================================================================
# STAGE A — 통합 전 크로스-시즌 일관성 검증 (하드윈: 코드가 시즌마다 같은가)
# =============================================================================
val <- lapply(SEASONS, function(s) {
  d  <- ST %>% filter(season == .env$s)
  # 러닝스코어 무결성
  sc <- d %>% group_by(game_id) %>%
    summarise(hf = max(home_after), af = max(away_after),
              oh = first(official_home_score), oa = first(official_away_score),
              .groups = "drop")
  match_rate <- mean(sc$hf == sc$oh & sc$af == sc$oa, na.rm = TRUE)
  # 슛 성공률
  sh <- d %>% filter(a %in% SHOT_CODES) %>%
    mutate(val = ifelse(a %in% c("205", "206"), 3L, 2L),
           made = as.integer(a %in% MAKE_CODES))
  # OReb same-team 비율 (미스 202/206 직후 209/210)
  rb <- d %>% arrange(game_id, period, api_row_order) %>%
    group_by(game_id) %>% mutate(next_a = lead(a)) %>% ungroup() %>%
    filter(a %in% c("202", "206"), next_a %in% c("209", "210"))
  tibble(
    season       = s,
    games        = n_distinct(d$game_id),
    score_match  = match_rate,
    shots        = nrow(sh),
    pct_2        = mean(sh$made[sh$val == 2]),
    pct_3        = mean(sh$made[sh$val == 3]),
    oreb_rate    = mean(rb$next_a == "209"),
    n_226        = sum(d$a == "226"),
    n_227        = sum(d$a == "227")
  )
}) %>% bind_rows()

message("\n── 크로스-시즌 일관성 검증 ──")
print(as.data.frame(val), row.names = FALSE, digits = 4)
# 자동 플래그: 시즌 간 편차가 상식 범위를 벗어나면 경고
flag <- c()
if (diff(range(val$pct_2)) > 0.03) flag <- c(flag, "2점 성공률 시즌편차>3%p")
if (diff(range(val$pct_3)) > 0.03) flag <- c(flag, "3점 성공률 시즌편차>3%p")
if (diff(range(val$oreb_rate)) > 0.04) flag <- c(flag, "OReb 비율 시즌편차>4%p")
if (any(val$score_match < 0.98)) flag <- c(flag, "러닝스코어 무결성<98%")
if (length(flag)) message("⚠ 플래그: ", paste(flag, collapse = " | ")) else message("✓ 세 시즌 핵심 코드 일관 — 풀링 진행")

# =============================================================================
# STAGE B — 풀링 리그 기준선: WP(σ,β), OReb%, xG(base2/base3)
# =============================================================================
# WP 확산모델 우도최대화 (정규시간 유효 이벤트, 세 시즌 풀링)
gw <- ST %>% distinct(game_id, official_home_score, official_away_score) %>%
  mutate(home_win = as.integer(official_home_score > official_away_score))
fit <- ST %>%
  left_join(gw %>% select(game_id, home_win), by = "game_id") %>%
  mutate(poss  = ifelse(is.na(possession_home), 0, ifelse(possession_home, 1, -1)),
         t_eff = ifelse(period <= 4, game_sec_remaining, sec_left_period)) %>%
  filter(period <= 4, t_eff >= 1, t_eff <= 2400, !is.na(margin_before))
mv <- fit$margin_before; tv <- fit$t_eff; pv <- fit$poss; yv <- fit$home_win
nll <- function(par) {
  sig <- par[1]; beta <- par[2]
  z  <- (mv + beta * pv) / (sig * sqrt(tv) + 1e-9)
  pr <- pmin(pmax(pnorm(z), 1e-6), 1 - 1e-6)
  -mean(yv * log(pr) + (1 - yv) * log(1 - pr))
}
opt   <- optim(c(0.3, 1.0), nll, method = "Nelder-Mead")
sigma <- opt$par[1]; beta <- opt$par[2]

# 풀링 OReb 확률
reb <- ST %>% arrange(game_id, period, api_row_order) %>%
  group_by(game_id) %>% mutate(next_a = lead(a)) %>% ungroup() %>%
  filter(a %in% c("202", "206"), next_a %in% c("209", "210"))
oreb_rate <- mean(reb$next_a == "209")

wp <- function(margin, t_rem, poss) pnorm((margin + beta * poss) / (sigma * sqrt(pmax(t_rem, 0.5))))

# =============================================================================
# STAGE C — 전 시즌 슛에 leverage/LI/xG/moe/contribution (풀링 기준선 사용)
# =============================================================================
sh <- ST %>%
  filter(a %in% SHOT_CODES) %>%
  mutate(
    shot_value = ifelse(a %in% c("205", "206"), 3L, 2L),
    made       = as.integer(a %in% MAKE_CODES),
    t_rem      = game_sec_remaining,
    sign       = ifelse(t == home_code, 1, -1),
    poss       = ifelse(is.na(possession_home), 0, ifelse(possession_home, 1, -1)),
    wp_make    = wp(margin_before + sign * shot_value, t_rem, -sign),
    wp_miss    = oreb_rate * wp(margin_before, t_rem, sign) +
                 (1 - oreb_rate) * wp(margin_before, t_rem, -sign),
    leverage   = abs(wp_make - wp_miss)
  )
mean_lev <- mean(sh$leverage, na.rm = TRUE)

# 풀링 xG 리그 기준선 (상황 중립, 슛값만)
base2 <- mean(sh$made[sh$shot_value == 2])
base3 <- mean(sh$made[sh$shot_value == 3])

sh <- sh %>% mutate(
  LI            = leverage / mean_lev,
  xg            = ifelse(shot_value == 3, base3, base2),
  make_over_exp = made - xg,
  pts_scored    = made * shot_value,
  xpts          = xg * shot_value,
  pts_over_exp  = pts_scored - xpts,
  contribution  = LI * pts_over_exp
) %>%
  transmute(
    season, game_id, game_date, period, sec_left_period, margin_before,
    shooter_id = player_id, shooter_en = e, shooter_kr = p, team = t,
    shot_value, made, LI, xg, make_over_exp, pts_over_exp, contribution,
    clutch_std, clutch_2poss, clutch_strict
  )

saveRDS(list(sigma = sigma, beta = beta, oreb_rate = oreb_rate,
             base2 = base2, base3 = base3, mean_lev = mean_lev, seasons = SEASONS),
        "KBL_pooled_wp_model.rds")
write_excel_csv(sh, "KBL_pooled_shots_scored.csv")

message(sprintf("\n풀링 기준선: σ=%.4f β=%.3f | OReb=%.3f | 2점 %.3f 3점 %.3f | 평균lev=%.4f",
                sigma, beta, oreb_rate, base2, base3, mean_lev))

# =============================================================================
# STAGE D — 클러치 실력 존재검정 (풀링, 선수 단위 = 최대 검정력)
#   분산분해 τ² + 순열검정.  정의(std/2poss/strict)별로 반복.
# =============================================================================
# 표준 불균형 일원 랜덤효과 분산성분 추정(Searle) + 정확 순열검정.
#   [수정] 원본 05는 between(=SSB/N, 슛수 가중)과 sampling(=mean(σ²/nᵢ), 비가중)을
#   서로 다른 스케일로 비교해 τ²와 순열 p가 어긋난다(단일 시즌엔 둘 다 null이라
#   가려졌으나 풀링에서 노출됨). 여기서는 두 지표를 동일한 SSB 스케일로 일치시킨다.
#     SSB=Σnᵢ(ȳᵢ-μ)², MSB=SSB/(K-1);  SSW=ΣΣ(y-ȳᵢ)², MSW=SSW/(N-K)=σ̂²
#     τ̂²=(MSB-MSW)/n₀,  n₀=(N-Σnᵢ²/N)/(K-1)   ← 순열검정과 동일 통계량
existence_test <- function(cl) {
  grp <- cl %>% group_by(shooter_en) %>%
    summarise(n = n(),
              ybar = mean(make_over_exp),
              ssw  = sum((make_over_exp - mean(make_over_exp))^2), .groups = "drop")
  N <- nrow(cl); K <- nrow(grp)
  mu   <- mean(cl$make_over_exp)                    # = weighted.mean(ybar, n) (grand mean)
  SSB  <- sum(grp$n * (grp$ybar - mu)^2); MSB <- SSB / (K - 1)
  SSW  <- sum(grp$ssw);                   MSW <- SSW / (N - K)   # 슛 1개 내부분산 σ²
  n0   <- (N - sum(grp$n^2) / N) / (K - 1)          # 유효 그룹크기
  tau2 <- (MSB - MSW) / n0                          # 진짜 선수간 실력분산
  # 정확 순열검정: 슛을 선수에 무작위 재배정한 SSB 귀무분포
  vals <- cl$make_over_exp; Nvec <- grp$n
  perm_SSB <- function() {
    v <- sample(vals); idx <- 1L; tot <- 0
    for (nn in Nvec) { tot <- tot + nn * (mean(v[idx:(idx + nn - 1L)]) - mu)^2; idx <- idx + nn }
    tot
  }
  null <- replicate(N_PERM, perm_SSB())
  list(n_shots = N, n_players = K, med_n = median(grp$n),
       sig2 = MSW, tau2 = tau2, skill_sd = sqrt(pmax(tau2, 0)),
       icc = tau2 / (tau2 + MSW), p = mean(null >= SSB))
}

ex_rows <- lapply(DEFS, function(dn) {
  cl <- sh %>% filter(.data[[dn]], !is.na(shooter_en))
  r  <- existence_test(cl)
  tibble(def = dn, shots = r$n_shots, players = r$n_players, med_n = r$med_n,
         sig2 = r$sig2, tau2 = r$tau2, skill_sd = r$skill_sd,
         icc = r$icc, perm_p = r$p)
}) %>% bind_rows()

message("\n── 존재검정 (3시즌 풀링, 선수 단위) ──")
print(as.data.frame(ex_rows), row.names = FALSE, digits = 4)

# =============================================================================
# STAGE E — 시즌 간 지속성 (persistence): 클러치의 진짜 시금석
#   선수×시즌 moe가 다음 시즌 moe를 예측하는가? (연속 시즌 가중 상관)
#   + 스플릿-하프 신뢰도(스피어만-브라운)로 신호/잡음 측정.
# =============================================================================
# 선수×시즌 집계 (PRIMARY 정의)
ps <- sh %>% filter(.data[[PRIMARY]], !is.na(shooter_en)) %>%
  group_by(shooter_en, shooter_kr, season) %>%
  summarise(n = n(), ybar = mean(make_over_exp),
            value = sum(contribution), .groups = "drop")

# --- (E1) 연속 시즌 year-to-year 상관 (가중) ---
season_next <- c("2023_24" = "2024_25", "2024_25" = "2025_26")
pairs <- lapply(names(season_next), function(s0) {
  s1 <- season_next[[s0]]
  a <- ps %>% filter(season == s0, n >= MIN_N) %>% select(shooter_en, y0 = ybar, n0 = n)
  b <- ps %>% filter(season == s1, n >= MIN_N) %>% select(shooter_en, y1 = ybar, n1 = n)
  inner_join(a, b, by = "shooter_en") %>% mutate(pair = paste0(s0, "->", s1))
}) %>% bind_rows()

wcor <- function(x, y, w) {
  mx <- weighted.mean(x, w); my <- weighted.mean(y, w)
  cov <- sum(w * (x - mx) * (y - my)) / sum(w)
  sx  <- sqrt(sum(w * (x - mx)^2) / sum(w)); sy <- sqrt(sum(w * (y - my)^2) / sum(w))
  cov / (sx * sy)
}
if (nrow(pairs) >= 3) {
  w <- 2 / (1 / pairs$n0 + 1 / pairs$n1)        # 조화평균 표본 가중
  r_y2y <- wcor(pairs$y0, pairs$y1, w)
  # 순열 귀무분포 (y1 라벨 셔플)
  null_r <- replicate(N_PERM, wcor(pairs$y0, sample(pairs$y1), w))
  p_y2y  <- mean(abs(null_r) >= abs(r_y2y))
} else { r_y2y <- NA; p_y2y <- NA }

# --- (E2) 스플릿-하프 신뢰도 (풀링, PRIMARY) ---
clP <- sh %>% filter(.data[[PRIMARY]], !is.na(shooter_en))
split_half <- function() {
  d <- clP %>% mutate(half = sample(rep(c(1L, 2L), length.out = n()))) %>%
    group_by(shooter_en, half) %>% summarise(m = mean(make_over_exp), k = n(), .groups = "drop")
  w1 <- d %>% filter(half == 1) %>% select(shooter_en, m1 = m, k1 = k)
  w2 <- d %>% filter(half == 2) %>% select(shooter_en, m2 = m, k2 = k)
  j <- inner_join(w1, w2, by = "shooter_en") %>% filter(k1 >= 3, k2 >= 3)
  if (nrow(j) < 5) return(NA_real_)
  wcor(j$m1, j$m2, 2 / (1 / j$k1 + 1 / j$k2))
}
sh_r  <- replicate(200, split_half())
r_half <- mean(sh_r, na.rm = TRUE)
r_sb   <- 2 * r_half / (1 + r_half)               # 스피어만-브라운 보정

message("\n── 시즌 간 지속성 ──")
message(sprintf("연속시즌 year-to-year 가중상관 r=%.3f (쌍 %d, 순열 p=%.3f)",
                ifelse(is.na(r_y2y), NA, r_y2y), nrow(pairs), ifelse(is.na(p_y2y), NA, p_y2y)))
message(sprintf("스플릿-하프 r=%.3f → 스피어만-브라운 신뢰도=%.3f", r_half, r_sb))

# =============================================================================
# STAGE F — 산출물 저장 (선수×시즌 · 선수풀링) + 축소추정
# =============================================================================
# 선수풀링 존재검정의 τ²로 축소(shrinkage) — PRIMARY 정의 기준
clPrimary <- sh %>% filter(.data[[PRIMARY]], !is.na(shooter_en))
rP <- existence_test(clPrimary)
players_pooled <- clPrimary %>%
  group_by(shooter_en, shooter_kr) %>%
  summarise(seasons = n_distinct(season), n = n(),
            ybar = mean(make_over_exp), value = sum(contribution), .groups = "drop") %>%
  mutate(shrunk = if (rP$tau2 > 0) rP$tau2 / (rP$tau2 + rP$sig2 / n) * ybar else 0) %>%
  arrange(desc(value))
write_excel_csv(players_pooled, "KBL_multiseason_players_pooled.csv")
write_excel_csv(ps %>% arrange(season, desc(value)), "KBL_multiseason_playerseason.csv")

# ── 리포트 (markdown) ────────────────────────────────────────────────────────
fmt_tbl <- function(df) {
  df <- as.data.frame(df)
  hdr <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  body <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(hdr, sep, body)
}
val_disp <- val %>% mutate(across(where(is.numeric), ~round(., 4)))
ex_disp  <- ex_rows %>% mutate(across(where(is.numeric), ~round(., 4)))

# ── 결론 생성 (데이터 기반, 두 결과를 구분해 정직하게) ──────────────────────
n_sig       <- sum(ex_rows$perm_p < 0.05)
max_skillsd <- max(ex_rows$skill_sd)
persist_null <- is.na(p_y2y) || p_y2y >= 0.05
if (n_sig > 0 && persist_null) {
  concl <- paste0(
    sprintf("**풀링 존재검정은 %d/%d 정의에서 한계적 유의(순열 p<0.05)를 보이나, 시즌 간 지속성은 없다.** ", n_sig, length(DEFS)),
    sprintf("3시즌을 합치면 선수간 클러치 분산이 순수 교환가능성 귀무가설을 근소하게 벗어나지만(추정 실력 SD≈%.3f, 슛 1개 σ≈%.2f 대비 매우 작음), ", max_skillsd, sqrt(mean(ex_rows$sig2))),
    sprintf("한 시즌 클러치 성과는 다음 시즌을 사실상 예측하지 못한다(year-to-year r=%.3f, p=%.3f). ", r_y2y, p_y2y),
    "지속되지 않고 재현성이 낮은 선수간 차이는 **안정적 '클러치 실력'의 증거가 아니다** — 시즌 내 변동(뜨거운 손·표본 잡음)과 구분되지 않는다. ",
    "결론: 검정력을 3배로 키운 뒤에도 **반복 가능한 클러치 실력의 증거는 없다.** 한계적 풀링 신호는 지속성 검정을 통과하지 못한다.")
} else if (n_sig == 0 && persist_null) {
  concl <- "**세 시즌을 통합해도 클러치 실력의 통계적 증거는 없다.** τ²는 ≤0이고 순열검정도 유의하지 않으며, 시즌 간 지속성도 0과 구분되지 않는다 — '클러치는 대부분 운'을 강하게 뒷받침한다."
} else {
  concl <- sprintf("존재검정(%d/%d 유의)과 시즌 간 지속성(r=%.3f, p=%.3f)이 함께 신호를 보인다 — 다중검정·표본을 감안해 신중히 해석하되, 후속(수동 xG 격상)으로 확인할 가치가 있다.",
                   n_sig, length(DEFS), r_y2y, p_y2y)
}

lines <- c(
  "# KBL 클러치 — 세 시즌 통합 결과",
  "",
  sprintf("_생성: 3시즌 풀링 (%s). 리그 기준선은 풀링, 선수 단위는 선수×시즌._", paste(SEASONS, collapse = ", ")),
  "",
  "## 1. 통합 전 크로스-시즌 일관성 검증",
  "",
  "핵심 코드(득점·리바운드)가 세 시즌 동일하게 작동하는지부터 확인한다.",
  "",
  fmt_tbl(val_disp),
  "",
  if (length(flag)) paste0("> ⚠ 플래그: ", paste(flag, collapse = " | ")) else "> ✓ 세 시즌 핵심 코드 일관 — 풀링 타당.",
  sprintf("> 226 코드는 2025-26에만(%d건), 227도 2025-26에만(%d건) 등장 — 비득점 코드라 슛/러닝스코어에 무영향.",
          val$n_226[val$season == "2025_26"], val$n_227[val$season == "2025_26"]),
  "",
  "## 2. 풀링 리그 기준선",
  "",
  sprintf("- WP 확산모델: **σ=%.4f, β=%.3f** (시작 마진 SD≈%.1f점)", sigma, beta, sigma * sqrt(2400)),
  sprintf("- 공격리바운드 확률(OReb): **%.3f**", oreb_rate),
  sprintf("- xG 리그 기준선: **2점 %.3f / 3점 %.3f**", base2, base3),
  sprintf("- 평균 레버리지(LI=1 기준): %.4f", mean_lev),
  "",
  "## 3. 클러치 실력 존재검정 (풀링, 선수 단위)",
  "",
  "표준 불균형 일원 랜덤효과 분산성분: `τ̂²=(MSB−MSW)/n₀`. τ²>0면 선수간 실력분산 존재. ",
  "`sig2`=슛 1개 내부분산(σ²), `skill_sd`=√τ², `icc`=τ²/(τ²+σ²), `perm_p`=슛을 선수에 무작위 재배정한 SSB 귀무분포 대비 정확검정.",
  "",
  fmt_tbl(ex_disp),
  "",
  "> **주의(원본 방법 대비 수정):** 원본 `05`는 `between`(슛수 가중)과 `sampling`(비가중 mean(σ²/nᵢ))을 다른 스케일로 비교해 τ²와 순열 p가 어긋난다. 단일 시즌에선 둘 다 null이라 가려졌으나 풀링에서 노출되어, 여기서는 동일 SSB 스케일의 표준 추정량으로 일치시켰다.",
  "",
  "## 4. 시즌 간 지속성 (핵심 시금석)",
  "",
  "안정적 '실력'이라면 한 시즌 클러치 성과가 다음 시즌을 예측해야 한다. 존재검정이 한계적 신호를 보여도, 지속성이 없으면 그것은 시즌 내 변동(뜨거운 손·잡음)이지 실력이 아니다.",
  "",
  sprintf("- 연속시즌 year-to-year 가중상관: **r=%.3f** (선수쌍 %d, 순열 p=%.3f, 시즌당 최소 %d슛)",
          ifelse(is.na(r_y2y), NA_real_, r_y2y), nrow(pairs), ifelse(is.na(p_y2y), NA_real_, p_y2y), MIN_N),
  sprintf("- 스플릿-하프 신뢰도(스피어만-브라운): **%.3f** (0이면 순수 잡음, 1이면 완전 재현)", r_sb),
  "",
  "## 5. 결론",
  "",
  concl,
  "",
  "## 산출물",
  "",
  "- `KBL_pooled_shots_scored.csv` — 전 시즌 슛 + 풀링 기준선 기반 LI/xG/moe/contribution",
  "- `KBL_multiseason_playerseason.csv` — 선수×시즌 클러치 집계(지속성 분석 단위)",
  "- `KBL_multiseason_players_pooled.csv` — 선수(3시즌 풀링) 클러치 가치·축소추정",
  "- `KBL_pooled_wp_model.rds` — 풀링 σ·β·OReb·xG 기준선",
  ""
)
dir.create("../docs", showWarnings = FALSE)
writeLines(lines, REPORT)
message(sprintf("\n리포트 저장: %s", REPORT))
message("── 세 시즌 통합 완료 ──")
