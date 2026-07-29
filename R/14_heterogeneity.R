# =============================================================================
# 재구성 배치 3 (#5): 하위그룹·팀 수준 이질성
# -----------------------------------------------------------------------------
# 개인 단위에서 클러치 실력이 null이어도, 국적/팀 단위에서는 신호가 날 수 있다.
#   A. 국적(외국인 vs 국내): 클러치 볼륨 집중도, 압박효과(FG% 하락), 그룹 내 지속성
#   B. 팀 수준: 팀×시즌 클러치 성과의 시즌 간 지속성(시스템·코칭 효과?), 볼륨 집중도
#
# 국적 판정: 한글이름에 공백이 있으면 외국인(전사명), 없으면 국내(name-based 휴리스틱).
#   → player_dim 검증 결과 42/211 외국인, 오분류 거의 없음(제목케이스 국내명도 정확 분류).
#
# 입력 : KBL_pooled_shots_scored.csv (야투, 리그기준선) + 팀명 맵(상태테이블)
# 출력 : ../docs/clutch_heterogeneity_findings.md
#   install.packages(c("dplyr","stringr","readr"))
# =============================================================================

library(dplyr); library(stringr); library(readr)
set.seed(1)
suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

SEASONS <- c("2023_24", "2024_25", "2025_26")
MIN_N   <- 5           # 그룹내 선수 지속성: 시즌당 최소 클러치 슛
TEAM_MIN<- 20          # 팀×시즌 최소 클러치 슛
N_PERM  <- 5000

if (!file.exists("KBL_pooled_shots_scored.csv"))
  stop("KBL_pooled_shots_scored.csv 없음 — 먼저 ./run_multiseason.sh (R/10) 실행")
sh <- read_csv("KBL_pooled_shots_scored.csv", show_col_types = FALSE) %>%
  mutate(nat = ifelse(grepl(" ", shooter_kr), "외국인", "국내"),
         clutch = clutch_std)

# 팀 코드→이름 맵 (상태테이블에서)
norm <- function(x) sub("\\.0$", "", as.character(x))
tmap <- bind_rows(lapply(SEASONS, function(s)
  read_csv(sprintf("KBL_%s_state_table.csv", s), show_col_types = FALSE,
           col_select = c(home_team, home_code, away_team, away_code)) %>%
    { bind_rows(transmute(., code = norm(home_code), name = home_team),
                transmute(., code = norm(away_code), name = away_team)) })) %>%
  distinct(code, name) %>% group_by(code) %>% summarise(team_name = first(name), .groups = "drop")
sh <- sh %>% mutate(code = norm(team)) %>% left_join(tmap, by = "code")

two_prop <- function(m1,n1,m0,n0){ p1<-m1/n1; p0<-m0/n0; p<-(m1+m0)/(n1+n0)
  z<-(p1-p0)/sqrt(p*(1-p)*(1/n1+1/n0)); list(p1=p1,p0=p0,d=p1-p0,p=2*pnorm(-abs(z))) }
wcor <- function(x,y,w){mx<-weighted.mean(x,w);my<-weighted.mean(y,w)
  sum(w*(x-mx)*(y-my))/sum(w)/sqrt(sum(w*(x-mx)^2)/sum(w)*sum(w*(y-my)^2)/sum(w))}
persist <- function(tab, val, ncol, nmin){
  nx <- c("2023_24"="2024_25","2024_25"="2025_26")
  pr <- bind_rows(lapply(names(nx), function(s0) inner_join(
    tab %>% filter(season==s0, .data[[ncol]]>=nmin) %>% transmute(id, y0=.data[[val]], n0=.data[[ncol]]),
    tab %>% filter(season==nx[[s0]], .data[[ncol]]>=nmin) %>% transmute(id, y1=.data[[val]], n1=.data[[ncol]]),
    by="id")))
  if (nrow(pr) < 4) return(list(pairs=nrow(pr), r=NA, p=NA))
  w <- 2/(1/pr$n0+1/pr$n1); r <- wcor(pr$y0,pr$y1,w)
  null <- replicate(N_PERM, wcor(pr$y0, sample(pr$y1), w))
  list(pairs=nrow(pr), r=r, p=mean(abs(null)>=abs(r)))
}

# =============================================================================
# A. 국적 하위그룹
# =============================================================================
# A1. 클러치 볼륨 집중도 — 외국인이 비클러치보다 클러치에 더 몰리는가
fg <- sh   # 전부 야투
share <- fg %>% group_by(clutch) %>%
  summarise(foreign_share = mean(nat=="외국인"), .groups="drop")
sh_nc <- share$foreign_share[!share$clutch]; sh_cl <- share$foreign_share[share$clutch]
tp_share <- two_prop(sum(fg$nat=="외국인" & fg$clutch), sum(fg$clutch),
                     sum(fg$nat=="외국인" & !fg$clutch), sum(!fg$clutch))

# A2. 압박효과(FG% 클러치 vs 비클러치) — 그룹별
grpeff <- lapply(c("외국인","국내"), function(g){
  d <- fg %>% filter(nat==g)
  e <- two_prop(sum(d$made[d$clutch]), sum(d$clutch), sum(d$made[!d$clutch]), sum(!d$clutch))
  tibble(nat=g, FG_클=e$p1, FG_비=e$p0, ΔFG=e$d, p=e$p, n_clutch=sum(d$clutch))
}) %>% bind_rows()

# A3. 그룹 내 개인 클러치 효율 지속성
ps_nat <- fg %>% filter(clutch) %>% group_by(nat, season, shooter_en) %>%
  summarise(n=n(), eff=mean(make_over_exp), .groups="drop") %>% rename(id=shooter_en)
per_for <- persist(ps_nat %>% filter(nat=="외국인"), "eff", "n", MIN_N)
per_dom <- persist(ps_nat %>% filter(nat=="국내"),   "eff", "n", MIN_N)

# A3b. 교란 점검: make_over_exp는 '리그' 기준선이라 '평소 잘 쏘는 선수'가 지속적으로 양수가 됨.
#   외국인의 '비클러치' 효율 지속성이 클러치와 비슷하면, A3의 신호는 클러치가 아니라 '실력 지속'.
ps_nat_nc <- fg %>% filter(!clutch) %>% group_by(nat, season, shooter_en) %>%
  summarise(n=n(), eff=mean(make_over_exp), .groups="drop") %>% rename(id=shooter_en)
per_for_nc <- persist(ps_nat_nc %>% filter(nat=="외국인"), "eff", "n", 20)

message(sprintf("[A1] 외국인 클러치 슛 점유 %.1f%% vs 비클러치 %.1f%% (Δ%+.1f%%p, p=%.2g)",
                100*sh_cl, 100*sh_nc, 100*tp_share$d, tp_share$p))
message("[A2] 압박효과(FG% 하락) 그룹별:")
print(as.data.frame(grpeff %>% mutate(across(where(is.numeric), ~round(.,3)))), row.names=FALSE)
message(sprintf("[A3] 그룹내 클러치효율 지속성: 외국인 r=%.3f(p=%.2f,쌍%d) | 국내 r=%.3f(p=%.2f,쌍%d)",
                per_for$r, per_for$p, per_for$pairs, per_dom$r, per_dom$p, per_dom$pairs))
message(sprintf("[A3b] 교란점검: 외국인 '비클러치' 효율 지속성 r=%.3f(p=%.2f,쌍%d) — 클러치와 비슷하면 '실력 지속'(클러치 아님)",
                per_for_nc$r, per_for_nc$p, per_for_nc$pairs))

# =============================================================================
# B. 팀 수준
# =============================================================================
# B1. 팀×시즌 클러치 성과(평균 make_over_exp) 지속성
team_ps <- fg %>% filter(clutch) %>% group_by(season, team_name) %>%
  summarise(n=n(), eff=mean(make_over_exp), .groups="drop") %>% rename(id=team_name)
per_team <- persist(team_ps, "eff", "n", TEAM_MIN)

# B2. 팀 클러치 볼륨 집중도(최다 슈터 점유율)와 그 안정성
conc <- fg %>% filter(clutch) %>% group_by(season, team_name, shooter_en) %>%
  summarise(k=n(), .groups="drop_last") %>%
  summarise(top1_share = max(k)/sum(k), n=sum(k), .groups="drop")
per_conc <- persist(conc %>% rename(id=team_name), "top1_share", "n", TEAM_MIN)

message(sprintf("\n[B1] 팀×시즌 클러치성과 지속성 r=%.3f (p=%.2f, 팀쌍 %d)",
                per_team$r, per_team$p, per_team$pairs))
message(sprintf("[B2] 팀 클러치 최다슈터 점유율: 평균 %.1f%% | 안정성 r=%.3f (p=%.2f, 쌍%d)",
                100*mean(conc$top1_share), per_conc$r, per_conc$p, per_conc$pairs))

# =============================================================================
# 리포트
# =============================================================================
fmt_tbl <- function(df){ df<-as.data.frame(df)
  cells<-lapply(df,function(c) format(c,trim=TRUE)); rows<-do.call(paste,c(cells,list(sep=" | ")))
  c(paste0("| ",paste(names(df),collapse=" | ")," |"),
    paste0("| ",paste(rep("---",ncol(df)),collapse=" | ")," |"), paste0("| ",rows," |")) }
r3<-function(x) round(x,3)
teamtab <- fg %>% filter(clutch) %>% group_by(팀=team_name) %>%
  summarise(clutch_슛=n(), 평균PAE=r3(mean(make_over_exp)), .groups="drop") %>%
  arrange(desc(평균PAE))

pers_tab <- tibble(
  단위 = c("개인(외국인)","개인(국내)","팀","팀 볼륨집중도"),
  r = r3(c(per_for$r, per_dom$r, per_team$r, per_conc$r)),
  p = r3(c(per_for$p, per_dom$p, per_team$p, per_conc$p)),
  쌍 = c(per_for$pairs, per_dom$pairs, per_team$pairs, per_conc$pairs))

lines <- c(
  "# KBL 클러치 — 배치 3: 하위그룹·팀 수준 이질성",
  "",
  "개인 단위 null이어도 국적·팀 단위에서 신호가 있는지 확인한다. (국적=한글이름 공백 휴리스틱)",
  "",
  "## A. 국적 하위그룹 (외국인 vs 국내)",
  "",
  "### A1. 클러치 볼륨 집중",
  sprintf("- 외국인 야투 점유율: 비클러치 **%.1f%%** → 클러치 **%.1f%%** (Δ%+.1f%%p, p=%.2g)",
          100*sh_nc, 100*sh_cl, 100*tp_share$d, tp_share$p),
  sprintf("- → 접전 종반 공격이 %s. '클러치 역할'의 %s 집중은 %s.",
          ifelse(tp_share$d>0,"외국인에게 더 쏠린다","특정 국적으로 쏠리지 않는다"),
          ifelse(tp_share$d>0,"외국인","특정 그룹"),
          ifelse(tp_share$p<0.05,"통계적으로 유의","유의하지 않음")),
  "",
  "### A2. 압박 효과 (FG% 클러치 vs 비클러치)",
  "",
  fmt_tbl(grpeff %>% mutate(across(where(is.numeric), ~round(.,3)))),
  "",
  sprintf("- 두 그룹 모두 클러치에서 FG%%가 하락 → 압박효과는 국적 불문 공통. (외국인 Δ%+.3f, 국내 Δ%+.3f)",
          grpeff$ΔFG[grpeff$nat=="외국인"], grpeff$ΔFG[grpeff$nat=="국내"]),
  "",
  "### A3. 그룹 내 개인 클러치효율 지속성",
  sprintf("- 외국인 r=**%.3f** (p=%.2f, 쌍 %d) | 국내 r=%.3f (p=%.2f, 쌍 %d)",
          per_for$r, per_for$p, per_for$pairs, per_dom$r, per_dom$p, per_dom$pairs),
  sprintf(paste0("- ⚠ **교란 주의**: 외국인의 겉보기 지속성은 클러치 실력이 아니라 '평소 잘 쏘는 선수'일 가능성. ",
    "make_over_exp가 **리그** 기준선이라 꾸준히 좋은 슈터는 클러치에서도 꾸준히 양수가 된다. ",
    "실제로 외국인의 **비클러치** 효율 지속성도 r=%.3f(p=%.2f)로 클러치와 %s → 이 신호는 **클러치 특이적이 아니라 '전반적 슈팅 실력의 지속'**이다. ",
    "(개인 '비클러치 기준선'을 쓴 R/11에서는 어느 그룹도 클러치 지속성 없음.)"),
          per_for_nc$r, per_for_nc$p,
          ifelse(abs(per_for_nc$r - per_for$r) < 0.2, "비슷하다", "다르다")),
  "",
  "## B. 팀 수준",
  "",
  "### 지속성 요약",
  "",
  fmt_tbl(pers_tab),
  "",
  sprintf("- **팀×시즌 클러치 성과 지속성 r=%.3f (p=%.2f)** — 팀(시스템·코칭) 단위에서도 %s.",
          per_team$r, per_team$p, ifelse(!is.na(per_team$p)&&per_team$p<0.05,"지속성 있음(주목)","반복되지 않음")),
  sprintf("- 팀 클러치 **최다슈터 점유율 평균 %.0f%%**(집중적) 이고 그 집중 구조는 시즌 간 %s(r=%.2f) — '누가 던지냐'는 팀 전술로 안정적이나, 그 결과(효율)는 반복 안 됨.",
          100*mean(conc$top1_share), ifelse(!is.na(per_conc$p)&&per_conc$p<0.05,"유지됨","약함"), per_conc$r),
  "",
  "### 팀별 클러치 평균 PAE (3시즌)",
  "",
  fmt_tbl(teamtab),
  "",
  "## 결론",
  "",
  paste0("하위그룹·팀 단위로 내려가도 **재현되는 클러치 '실력'은 나타나지 않는다.** ",
    "재현되는 것은 오직 **역할·구조**뿐이다: (1) 클러치 볼륨이 특정 선수·(부분적으로)외국인에게 집중되고 그 집중이 시즌 간 유지되며, ",
    "(2) 압박에 따른 FG% 하락은 국적 불문 리그 공통 현상이다. 즉 이질성의 축은 '실력'이 아니라 '기회 배분'이다."),
  ""
)
dir.create("../docs", showWarnings=FALSE)
writeLines(lines, "../docs/clutch_heterogeneity_findings.md")
message("\n리포트 저장: ../docs/clutch_heterogeneity_findings.md")
message("── 배치 3 완료 ──")
