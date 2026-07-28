# =============================================================================
# Clutch Score (5조 문서 지표) 구현 + 존재·지속성 검정
# -----------------------------------------------------------------------------
# 문서(11기 방중 5조)의 지표를 충실히 구현하고, 문서에 없는 "그래서 반복 가능한
# 실력인가"를 검정해 의미 있는 결론까지 낸다.
#
#   CS = PAE × TimeWeight × MarginWeight × StateChangeWeight
#     • PAE = 실제득점 − 기대득점,  기대득점 = 선수 개인 '비클러치' 성공률 × 슛값
#       (2P/3P/FT 구분). 개인 기준선은 표본잡음 완화 위해 시즌·유형 리그평균으로 축소.
#     • TimeWeight   : 종료 5분(=1.00) → 0초(=2.00) 연속.  TW = 2 − min(t,300)/300
#     • MarginWeight : |점수차| 5(=1.00) → 동점(=2.00) 연속. MW = 2 − min(|m|,5)/5
#     • StateChange  : LeadExt/Chase 1.00, Lead-Taking 1.05, Game-Tying 1.10, Go-Ahead 1.20
#       (성공 가정 시 점수상태로 분류; 성공·실패 모두 동일 가중)
#     • 자유투 포함. 클러치 집합 = 종료 5분 이내 & |점수차|≤5 (= clutch_std).
#
# 입력 : 각 시즌 KBL_<season>_state_table.csv  (01_preprocess.R 산출물)
# 출력 :
#   KBL_clutchscore_playerseason.csv   선수×시즌 CS(총/시도당/유형별) + 4분면
#   KBL_clutchscore_players_pooled.csv 선수(3시즌 풀링) 랭킹
#   ../docs/clutch_score_findings.md   랭킹 + 존재검정 + 지속성 + 자유투 대조군
#
#   install.packages(c("dplyr","stringr","readr","tidyr"))
# =============================================================================

library(dplyr); library(stringr); library(readr); library(tidyr)
set.seed(1)
suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))   # 한글 이름이 <U+..>로 이스케이프되지 않게

SEASONS  <- c("2023_24", "2024_25", "2025_26")
SHRINK_K <- 20      # 개인 비클러치 기준선 축소 강도(시도-등가). 리그평균 쪽으로 당김
MIN_N    <- 5       # 지속성: 시즌당 최소 클러치 시도 수
N_PERM   <- 5000
REPORT   <- "../docs/clutch_score_findings.md"

norm <- function(x) sub("\\.0$", "", as.character(x))
load_state <- function(s) {
  f <- sprintf("KBL_%s_state_table.csv", s)
  if (!file.exists(f)) stop(sprintf("없음: %s — 먼저 01_preprocess.R를 시즌별로 실행", f))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(season = .env$s, a = str_pad(norm(a), 3, pad = "0"),
           t = norm(t), home_code = norm(home_code), away_code = norm(away_code))
}
ST <- bind_rows(lapply(SEASONS, load_state))

# ── 슛(야투+자유투) 추출 & 유형/상태 ────────────────────────────────────────
shots <- ST %>%
  filter(a %in% c("201","202","205","206","207","203","204"), !is.na(e)) %>%
  mutate(
    shot_type  = case_when(a %in% c("205","206") ~ "3P",
                           a %in% c("203","204") ~ "FT",
                           TRUE                    ~ "2P"),
    shot_value = case_when(shot_type == "3P" ~ 3, shot_type == "FT" ~ 1, TRUE ~ 2),
    made       = as.integer(a %in% c("201","205","207","203")),
    mb         = ifelse(t == home_code, margin_before, -margin_before),   # 슈터팀 관점 점수차
    t_left     = ifelse(period <= 4, game_sec_remaining, sec_left_period),
    clutch     = clutch_std,
    shooter_en = e, shooter_kr = p
  )

# ── 개인 '비클러치' 기준선 성공률 (시즌·선수·유형) + 리그평균 축소 ───────────
league_base <- shots %>% filter(!clutch) %>%
  group_by(season, shot_type) %>% summarise(lr = mean(made), .groups = "drop")
player_base <- shots %>% filter(!clutch) %>%
  group_by(season, shooter_en, shot_type) %>%
  summarise(mk = sum(made), att = n(), .groups = "drop") %>%
  left_join(league_base, by = c("season", "shot_type")) %>%
  mutate(base_rate = (mk + SHRINK_K * lr) / (att + SHRINK_K),   # empirical-Bayes 축소
         base_raw  = mk / att)

# ── 클러치 시도에 CS 부여 ───────────────────────────────────────────────────
cl <- shots %>% filter(clutch) %>%
  left_join(player_base %>% select(season, shooter_en, shot_type, base_rate),
            by = c("season", "shooter_en", "shot_type")) %>%
  left_join(league_base, by = c("season", "shot_type")) %>%
  mutate(
    base_rate  = ifelse(is.na(base_rate), lr, base_rate),   # 비클러치 표본 없으면 리그평균
    exp_pts    = base_rate * shot_value,
    act_pts    = made * shot_value,
    PAE        = act_pts - exp_pts,
    TW         = pmax(1, pmin(2, 2 - pmin(t_left, 300) / 300)),
    MW         = pmax(1, pmin(2, 2 - pmin(abs(mb), 5) / 5)),
    make_mb    = mb + shot_value,
    state      = case_when(mb  > 0                 ~ "LeadExtension",
                           mb == 0                 ~ "LeadTaking",
                           mb  < 0 & make_mb > 0   ~ "GoAhead",
                           mb  < 0 & make_mb == 0  ~ "GameTying",
                           TRUE                    ~ "Chase"),
    SCW        = recode(state, LeadExtension = 1.00, Chase = 1.00,
                        LeadTaking = 1.05, GameTying = 1.10, GoAhead = 1.20),
    CS         = PAE * TW * MW * SCW
  )
message(sprintf("클러치 시도 %d (2P %d / 3P %d / FT %d) | 선수 %d",
                nrow(cl), sum(cl$shot_type=="2P"), sum(cl$shot_type=="3P"),
                sum(cl$shot_type=="FT"), n_distinct(cl$shooter_en)))

# ── 선수×시즌 집계 + 4분면 ──────────────────────────────────────────────────
ps <- cl %>% group_by(season, shooter_en, shooter_kr) %>%
  summarise(att = n(), total_CS = sum(CS), CS_per = mean(CS), mean_PAE = mean(PAE),
            att_2P = sum(shot_type=="2P"), att_3P = sum(shot_type=="3P"), att_FT = sum(shot_type=="FT"),
            .groups = "drop")
med_tot <- median(ps$total_CS); med_per <- median(ps$CS_per)
ps <- ps %>% mutate(quadrant = case_when(
  total_CS >= med_tot & CS_per >= med_per ~ "고효율·고볼륨",
  total_CS <  med_tot & CS_per >= med_per ~ "고효율·저볼륨",
  total_CS >= med_tot & CS_per <  med_per ~ "저효율·고볼륨",
  TRUE                                     ~ "저효율·저볼륨"))

players_pooled <- cl %>% group_by(shooter_en, shooter_kr) %>%
  summarise(seasons = n_distinct(season), att = n(),
            total_CS = sum(CS), CS_per = mean(CS), mean_PAE = mean(PAE), .groups = "drop") %>%
  arrange(desc(total_CS))
write_excel_csv(ps %>% arrange(season, desc(total_CS)), "KBL_clutchscore_playerseason.csv")
write_excel_csv(players_pooled, "KBL_clutchscore_players_pooled.csv")

# =============================================================================
# 존재검정: 선수간 (per-attempt) 분산이 순수 운을 넘는가
#   표준 불균형 일원 랜덤효과 τ̂²=(MSB−MSW)/n₀ + 정확 순열검정(SSB)
# =============================================================================
existence_test <- function(df, val) {
  d <- df %>% transmute(g = shooter_en, y = .data[[val]])
  grp <- d %>% group_by(g) %>%
    summarise(n = n(), ybar = mean(y), ssw = sum((y - mean(y))^2), .groups = "drop")
  N <- nrow(d); K <- nrow(grp); mu <- mean(d$y)
  SSB <- sum(grp$n * (grp$ybar - mu)^2); MSB <- SSB / (K - 1)
  MSW <- sum(grp$ssw) / (N - K)
  n0  <- (N - sum(grp$n^2) / N) / (K - 1)
  tau2 <- (MSB - MSW) / n0
  vals <- d$y; Nvec <- grp$n
  perm <- function() { v <- sample(vals); idx <- 1L; tot <- 0
    for (nn in Nvec) { tot <- tot + nn * (mean(v[idx:(idx+nn-1L)]) - mu)^2; idx <- idx + nn }; tot }
  null <- replicate(N_PERM, perm())
  tibble(metric = val, shots = N, players = K, med_n = median(grp$n),
         sig2 = MSW, tau2 = tau2, skill_sd = sqrt(pmax(tau2, 0)),
         icc = tau2 / (tau2 + MSW), perm_p = mean(null >= SSB))
}
ex <- bind_rows(existence_test(cl, "PAE"), existence_test(cl, "CS"))
message("\n── 존재검정 (선수 단위, per-attempt) ──")
print(as.data.frame(ex), row.names = FALSE, digits = 4)

# =============================================================================
# 시즌 간 지속성 (핵심 시금석): 한 시즌 성과가 다음 시즌을 예측하는가
# =============================================================================
wcor <- function(x, y, w) {
  mx <- weighted.mean(x, w); my <- weighted.mean(y, w)
  sxy <- sum(w*(x-mx)*(y-my))/sum(w)
  sx <- sqrt(sum(w*(x-mx)^2)/sum(w)); sy <- sqrt(sum(w*(y-my)^2)/sum(w)); sxy/(sx*sy)
}
season_next <- c("2023_24" = "2024_25", "2024_25" = "2025_26")
persistence <- function(tab, valcol, ncol) {
  pr <- lapply(names(season_next), function(s0) {
    s1 <- season_next[[s0]]
    a <- tab %>% filter(season == s0, .data[[ncol]] >= MIN_N) %>%
      transmute(shooter_en, y0 = .data[[valcol]], n0 = .data[[ncol]])
    b <- tab %>% filter(season == s1, .data[[ncol]] >= MIN_N) %>%
      transmute(shooter_en, y1 = .data[[valcol]], n1 = .data[[ncol]])
    inner_join(a, b, by = "shooter_en")
  }) %>% bind_rows()
  if (nrow(pr) < 3) return(tibble(metric = valcol, pairs = nrow(pr), r = NA, p = NA))
  w <- 2 / (1/pr$n0 + 1/pr$n1); r <- wcor(pr$y0, pr$y1, w)
  null <- replicate(N_PERM, wcor(pr$y0, sample(pr$y1), w))
  tibble(metric = valcol, pairs = nrow(pr), r = r, p = mean(abs(null) >= abs(r)))
}
pers <- bind_rows(persistence(ps, "CS_per", "att"), persistence(ps, "mean_PAE", "att"))
message("\n── 시즌 간 지속성 (클러치 지표) ──")
print(as.data.frame(pers), row.names = FALSE, digits = 4)

# =============================================================================
# 양성 대조군: 비클러치 자유투% — 재현 가능한 실력으로 알려짐.
#   같은 방법(선수×시즌 year-to-year)으로 지속성이 '검출되면' 파이프라인 정상.
# =============================================================================
ft_base <- player_base %>% filter(shot_type == "FT", att >= 10) %>%
  transmute(season, shooter_en, ft_pct = base_raw, n = att)
ft_pers <- persistence(ft_base, "ft_pct", "n")
message("\n── 양성 대조군: 비클러치 FT% 지속성 ──")
print(as.data.frame(ft_pers), row.names = FALSE, digits = 4)

# =============================================================================
# 리포트
# =============================================================================
fmt_tbl <- function(df) {
  df <- as.data.frame(df)
  cells <- lapply(df, function(col) format(col, trim = TRUE))   # UTF-8 보존(apply/as.matrix 회피)
  rows  <- do.call(paste, c(cells, list(sep = " | ")))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |"),
    paste0("| ", rows, " |"))
}
top10 <- players_pooled %>% slice_head(n = 10) %>%
  transmute(shooter_kr, shooter_en, seasons, att,
            total_CS = round(total_CS,2), CS_per = round(CS_per,3), mean_PAE = round(mean_PAE,3))
quad_tab <- ps %>% count(quadrant) %>% arrange(desc(n))
ex_d   <- ex   %>% mutate(across(where(is.numeric), ~round(., 4)))
pers_d <- pers %>% mutate(across(where(is.numeric), ~round(., 4)))
ftp_d  <- ft_pers %>% mutate(across(where(is.numeric), ~round(., 4)))

# 결론 (데이터 기반)
clutch_persists <- any(pers$p < 0.05 & pers$r > 0, na.rm = TRUE)
ctrl_ok         <- !is.na(ft_pers$p) && ft_pers$p < 0.05 && ft_pers$r > 0
n_sig_exist     <- sum(ex$perm_p < 0.05)
concl <- if (ctrl_ok && !clutch_persists) {
  sprintf(paste0("**Clutch Score는 선수를 잘 서열화하지만, 그 서열은 시즌 간 반복되지 않는다.** ",
    "같은 방법으로 비클러치 자유투%%는 시즌 간 지속성이 뚜렷이 잡히는데(양성 대조군 r=%.2f, p=%.3f — 방법이 '진짜 실력'은 검출함), ",
    "클러치 지표(CS/시도당, PAE)의 지속성은 0과 구분되지 않는다(r=%.3f, p=%.3f). ",
    "즉 CS 랭킹 상위는 대부분 '기회(볼륨)와 시즌 내 변동'의 산물이며, **반복 가능한 클러치 실력의 증거는 아니다.** ",
    "존재검정에서 %d/2 지표가 한계적 유의를 보여도(풀링 표본 효과), 지속성 검정을 통과하지 못한다."),
    ft_pers$r, ft_pers$p, pers$r[pers$metric=="CS_per"], pers$p[pers$metric=="CS_per"], n_sig_exist)
} else if (clutch_persists) {
  "**일부 클러치 지표가 시즌 간 유의한 지속성을 보인다** — 다중검정·표본을 감안하되 실제 신호일 수 있어 후속(수동 xG) 확인 가치가 있다. 아래 표 참조."
} else {
  "클러치 지표는 지속성이 없고, 대조군(FT%) 지속성도 약하다 — 표본/방법 검토가 필요하다. 아래 표 참조."
}

lines <- c(
  "# KBL 클러치 — Clutch Score(5조 지표) 구현 + 실력 검정",
  "",
  sprintf("_3시즌(%s). 문서의 CS를 그대로 구현하고, 문서에 없는 존재·지속성 검정을 붙여 결론까지 도출._", paste(SEASONS, collapse=", ")),
  "",
  "## 지표 정의 (문서 충실 구현)",
  "",
  "`CS = PAE × TimeWeight × MarginWeight × StateChangeWeight`",
  sprintf("- 기대득점 = **개인 비클러치 성공률**(2P/3P/FT) × 슛값 — 표본잡음 완화 위해 시즌·유형 리그평균으로 축소(k=%d).", SHRINK_K),
  "- TimeWeight = 2 − min(t,300)/300 (5분 1.00 → 0초 2.00), MarginWeight = 2 − min(|점수차|,5)/5 (5점 1.00 → 동점 2.00).",
  "- StateChange: LeadExt/Chase 1.00, Lead-Taking 1.05, Game-Tying 1.10, Go-Ahead 1.20 (성공 가정 분류).",
  "- 자유투 포함. 클러치 = 종료 5분 이내 & |점수차|≤5.",
  "",
  "## 1. Clutch Score 랭킹 (3시즌 누적 상위 10)",
  "",
  fmt_tbl(top10),
  "",
  "### 4분면 (총 CS × 시도당 CS, 선수×시즌)",
  "",
  fmt_tbl(quad_tab),
  "",
  "## 2. 존재검정 — 선수간 (시도당) 성과 분산이 운을 넘는가",
  "",
  "`τ̂²=(MSB−MSW)/n₀` (순열검정과 동일 스케일). `PAE`=가중 전 기대초과, `CS`=가중 후.",
  "",
  fmt_tbl(ex_d),
  "",
  "## 3. 시즌 간 지속성 — **핵심 시금석**",
  "",
  "안정적 실력이라면 한 시즌 클러치 성과가 다음 시즌을 예측해야 한다.",
  "",
  fmt_tbl(pers_d),
  "",
  "## 4. 양성 대조군 — 비클러치 자유투% 지속성",
  "",
  "재현 가능한 실력으로 알려진 FT%를 **같은 방법**으로 검정. 여기서 지속성이 잡히면 방법은 정상 작동.",
  "",
  fmt_tbl(ftp_d),
  "",
  "## 5. 결론",
  "",
  concl,
  "",
  "## 산출물",
  "- `KBL_clutchscore_playerseason.csv` — 선수×시즌 CS(총/시도당/유형별)+4분면",
  "- `KBL_clutchscore_players_pooled.csv` — 선수 3시즌 풀링 랭킹",
  ""
)
dir.create("../docs", showWarnings = FALSE)
writeLines(lines, REPORT)
message(sprintf("\n리포트 저장: %s", REPORT))
message("── Clutch Score 분석 완료 ──")
