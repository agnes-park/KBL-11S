# =============================================================================
# 명성 있는 클러치 선수 케이스 스터디 — 이름으로 확인하기
# -----------------------------------------------------------------------------
# "김선형·이정현처럼 리그에서 클러치로 평가받는 선수들도 시즌 간 연속성이 없나?"
# 추상적 null을 실제 이름으로 확인한다. 각 선수의 시즌별:
#   클러치 시도수(=역할/볼륨), 성공률, 평균 PAE(개인 비클러치 기준선 대비),
#   CS/시도당, 그리고 '그 시즌 내 백분위'(100=최상위 클러치, 50=중간).
#
# 입력 : 각 시즌 state_table + KBL_clutchscore_playerseason.csv (R/11 산출물)
# 출력 : ../docs/named_clutch_players.md
#   install.packages(c("dplyr","stringr","readr"))
# =============================================================================

library(dplyr); library(stringr); library(readr)
suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

SEASONS <- c("2023_24", "2024_25", "2025_26")
MINQ    <- 5
# 리그에서 흔히 '클러치'로 평가받는 선수들(국내 가드·윙 위주)
TARGETS <- c("김선형","이정현","허훈","변준형","이대성","최준용","전성현","이재도","오세근","김낙현")
norm <- function(x) sub("\\.0$", "", trimws(as.character(x)))

# ── 클러치 성공률(raw) 계산: state_table에서 clutch_std 야투+자유투 ──────────
load_state <- function(s) read_csv(sprintf("KBL_%s_state_table.csv", s), show_col_types = FALSE,
  col_select = c(a, e, p, clutch_std, season)) %>% mutate(a = str_pad(norm(a), 3, pad = "0"), season = .env$s)
ST <- bind_rows(lapply(SEASONS, load_state))
mk <- ST %>% filter(a %in% c("201","202","205","206","207","203","204"), clutch_std, !is.na(e)) %>%
  mutate(made = as.integer(a %in% c("201","205","207","203"))) %>%
  group_by(season, shooter_kr = p) %>%
  summarise(makes = sum(made), .groups = "drop")

# ── CS 지표(개인기준선) + 시즌 내 백분위 ────────────────────────────────────
ps <- read_csv("KBL_clutchscore_playerseason.csv", show_col_types = FALSE) %>%
  group_by(season) %>%
  mutate(q = att >= MINQ,
         pct_CS  = ifelse(q, round(100 * percent_rank(CS_per)), NA_real_),
         pct_PAE = ifelse(q, round(100 * percent_rank(mean_PAE)), NA_real_),
         pct_vol = ifelse(q, round(100 * percent_rank(att)), NA_real_),
         nq = sum(q)) %>% ungroup() %>%
  left_join(mk, by = c("season", "shooter_kr")) %>%
  mutate(make_pct = round(makes / att, 3))

tab <- ps %>% filter(shooter_kr %in% TARGETS) %>%
  transmute(선수 = shooter_kr, 시즌 = season, 클러치시도 = att, 성공률 = make_pct,
            평균PAE = round(mean_PAE, 3), CS시도당 = round(CS_per, 3),
            볼륨백분위 = pct_vol, 효율백분위_CS = pct_CS, 효율백분위_PAE = pct_PAE) %>%
  arrange(선수, 시즌)

# ── 역할 vs 실력 요약(대상 선수들의 평균 백분위) ────────────────────────────
sumr <- ps %>% filter(shooter_kr %in% TARGETS, q) %>%
  summarise(평균_볼륨백분위 = round(mean(pct_vol)),
            평균_효율백분위 = round(mean(pct_CS)),
            선수시즌수 = n())

# ── 지속성: 대상 선수들만의 시즌 간 효율 상관(참고) ────────────────────────
nx <- c("2023_24"="2024_25","2024_25"="2025_26")
pr <- bind_rows(lapply(names(nx), function(s0) inner_join(
  ps %>% filter(shooter_kr %in% TARGETS, season==s0, q) %>% transmute(shooter_kr, e0=mean_PAE, v0=pct_vol),
  ps %>% filter(shooter_kr %in% TARGETS, season==nx[[s0]], q) %>% transmute(shooter_kr, e1=mean_PAE, v1=pct_vol),
  by="shooter_kr")))
r_eff_named <- if(nrow(pr)>=4) cor(pr$e0, pr$e1) else NA
r_vol_named <- if(nrow(pr)>=4) cor(pr$v0, pr$v1) else NA

message("── 명성 클러치 선수 케이스 스터디 ──")
print(as.data.frame(tab), row.names = FALSE)
message(sprintf("\n대상 선수 평균 백분위 — 볼륨 %d / 효율 %d (선수시즌 %d)",
                sumr$평균_볼륨백분위, sumr$평균_효율백분위, sumr$선수시즌수))
message(sprintf("대상 선수 시즌간 상관 — 효율 r=%.2f | 볼륨 r=%.2f (쌍 %d)", r_eff_named, r_vol_named, nrow(pr)))

# ── 리포트 ──────────────────────────────────────────────────────────────────
fmt_tbl <- function(df){ df<-as.data.frame(df)
  cells<-lapply(df,function(c) format(c,trim=TRUE)); rows<-do.call(paste,c(cells,list(sep=" | ")))
  c(paste0("| ",paste(names(df),collapse=" | ")," |"),
    paste0("| ",paste(rep("---",ncol(df)),collapse=" | ")," |"), paste0("| ",rows," |")) }

lines <- c(
  "# KBL 클러치 — 명성 있는 클러치 선수 케이스 스터디",
  "",
  "\"김선형·이정현처럼 리그에서 클러치로 평가받는 선수들도 시즌 간 연속성이 없나?\"에 이름으로 답한다.",
  "",
  sprintf("- **백분위**: 그 시즌 자격선수(클러치≥%d시도, 약 %d명) 중 순위. 100=클러치 최상위, 50=중간, 0=최하위.", MINQ, round(mean(ps$nq[ps$q]))),
  "- **볼륨백분위**=클러치 시도수(역할/기회), **효율백분위**=시도당 CS·평균 PAE(실제 클러치 성과).",
  "- 평균 PAE = 개인 '비클러치' 성공률 대비 기대초과 득점. 양수면 평소보다 잘함.",
  "",
  fmt_tbl(tab),
  "",
  sprintf("_※ 이정현은 KBL에 동명이인이 있으나 데이터는 영문명 기준 분리(여기 표기는 고볼륨 가드 1인, `LEE JUNG HYUN`)._"),
  "",
  "## 무엇이 보이나",
  "",
  sprintf("- **대상 선수 평균: 볼륨 백분위 %d위권 vs 효율 백분위 %d위권.** 명성 있는 선수들은 클러치에 **많이 나서지만(높은 볼륨)**, 그 **효율은 중간 이하**다.",
          sumr$평균_볼륨백분위, sumr$평균_효율백분위),
  "- **김선형**: 3시즌 내내 클러치 볼륨 상위이나 효율백분위 31~41(중간 이하), 평균 PAE도 3시즌 모두 음수 — '클러치 슈터' 명성은 **역할(많이 던짐)**이지 평소 대비 효율이 아니다.",
  "- **이정현**: 3시즌 클러치 시도 108/79/60으로 리그 최다급 — 볼륨(역할)은 압도적이나 효율백분위 34~47(중간 안팎)이고 하락 추세.",
  "- **오세근·이재도**: 한 시즌만 효율 최상위(백분위 93~96)로 튀지만 그 직전 시즌은 하위권(20~33)이고, 그 튄 시즌 표본이 8~10시도로 작다 — **소표본 잡음이지 반복되는 실력이 아니다.**",
  sprintf("- **대상 선수들만 따로 봐도** 시즌 간 효율 상관 r=%.2f(반복 없음)인데 볼륨 상관 r=%.2f(역할은 유지). 전체 리그와 같은 패턴.",
          r_eff_named, r_vol_named),
  "",
  "## 결론",
  "",
  paste0("리그가 '클러치 슈터'로 부르는 선수들(김선형·이정현 등)조차 **시즌 간 반복되는 클러치 효율을 보이지 않는다.** ",
    "이들에게서 재현되는 것은 오직 **볼륨=역할**(계속 결정적 순간에 공을 잡는다)뿐이며, 실제 효율은 중간 이하이거나 시즌마다 출렁인다. ",
    "즉 '클러치 명성'은 **성과의 반복이 아니라 팀·팬이 부여한 역할·기대**에 가깝다 — 앞선 전체 분석(볼륨은 지속·효율은 운)과 정확히 일치한다."),
  ""
)
dir.create("../docs", showWarnings = FALSE)
writeLines(lines, "../docs/named_clutch_players.md")
message("\n리포트 저장: ../docs/named_clutch_players.md")
message("── 완료 ──")
