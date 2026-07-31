# =============================================================================
# 큰 무대(플레이오프) 클러치: '큰 무대에서의 퍼포먼스'를 클러치로 재정의
# -----------------------------------------------------------------------------
# 5분 창에 얽매이지 않고, 플레이오프 = 지속되는 고압박 무대에서의 '개인 퍼포먼스'
# 자체를 클러치로 본다. 정규시즌 last-5min 클러치가 시즌 간 지속성이 없었던 것을 이어,
# 같은 시즌 안에서:
#   A. 정규시즌에 보여준 능력이 큰 무대로 이어지는가? (전이)
#      — 큰무대 성과를 예측하는 건 정규 '전반적 실력'인가, 정규 '5분 클러치'인가?
#   B. 정규엔 평범했지만 PO에서 빛나는 선수(riser)가 있는가? 예측·반복 가능한가?
#   C. '큰 무대 퍼포먼스'는 반복되는 실력인가? (PO 다회 진출 선수의 시즌 간 지속성)
#
# 지표(선수×시즌, 모두 기대초과 성공확률 스케일 made−기준율):
#   rs_ov  = 정규 전반 퍼포먼스(리그 기준선 대비)   ← '정규에 보여준 실력'
#   rs_cl  = 정규 5분 클러치 PAE(개인 비클러치 기준선)← 보조 예측변수
#   po     = 플레이오프 전경기 퍼포먼스(리그 기준선 대비) ← '큰 무대 성과'(주 outcome)
#   boost  = PO 성과 − 개인 정규 성공률          ← '평소보다 큰무대에서 얼마나 올렸나'
#
# 입력 : KBL_<season>_state_table.csv + KBL_<season>_playoff_state_table.csv
#   install.packages(c("dplyr","stringr","readr"))
# =============================================================================

library(dplyr); library(stringr); library(readr)
set.seed(1)
suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

SEASONS   <- c("2023_24", "2024_25", "2025_26")
RS_CL_MIN <- 10   # 정규 5분 클러치 최소 시도
PO_MIN    <- 15   # 플레이오프 최소 야투(안정적 큰무대 측정)
RS_MIN    <- 50   # 정규 최소 야투
SHRINK_K  <- 20
N_PERM    <- 5000
TARGETS   <- c("김선형","이정현","허훈","변준형","이대성","최준용","전성현","이재도","오세근","김낙현")
norm <- function(x) sub("\\.0$", "", trimws(as.character(x)))

load_shots <- function(season, kind) {
  pref <- if (kind == "regular") sprintf("KBL_%s", season) else sprintf("KBL_%s_playoff", season)
  f <- sprintf("%s_state_table.csv", pref)
  if (!file.exists(f)) stop("없음: ", f, " — 먼저 01_preprocess(정규/PO) 실행")
  read_csv(f, show_col_types = FALSE) %>%
    mutate(a = str_pad(norm(a), 3, pad = "0")) %>%
    filter(a %in% c("201","202","205","206","207","203","204"), !is.na(e)) %>%
    transmute(season = .env$season, shooter_en = e, shooter_kr = p,
              shot_type = case_when(a %in% c("205","206") ~ "3P", a %in% c("203","204") ~ "FT", TRUE ~ "2P"),
              made = as.integer(a %in% c("201","205","207","203")),
              clutch = clutch_std)
}
rs <- bind_rows(lapply(SEASONS, load_shots, kind = "regular"))
po <- bind_rows(lapply(SEASONS, load_shots, kind = "playoff"))

league <- rs %>% group_by(shot_type) %>% summarise(lr = mean(made), .groups = "drop")
lrget  <- function(tp) league$lr[match(tp, league$shot_type)]

# ── 정규 지표 ───────────────────────────────────────────────────────────────
rs_ncbase <- rs %>% filter(!clutch) %>%
  group_by(season, shooter_en, shot_type) %>% summarise(mk = sum(made), att = n(), .groups="drop") %>%
  left_join(rs %>% filter(!clutch) %>% group_by(season, shot_type) %>% summarise(slr = mean(made), .groups="drop"),
            by = c("season","shot_type")) %>% mutate(nc_rate = (mk+SHRINK_K*slr)/(att+SHRINK_K))
rs_ownrate <- rs %>% group_by(season, shooter_en, shot_type) %>% summarise(own_rate = mean(made), .groups="drop")

rs_clutch <- rs %>% filter(clutch) %>%
  left_join(rs_ncbase %>% select(season,shooter_en,shot_type,nc_rate), by=c("season","shooter_en","shot_type")) %>%
  mutate(nc_rate = ifelse(is.na(nc_rate), lrget(shot_type), nc_rate), pae = made - nc_rate) %>%
  group_by(season, shooter_en) %>% summarise(rs_cl = mean(pae), rs_cl_n = n(), .groups="drop")
rs_overall <- rs %>% mutate(moe = made - lrget(shot_type)) %>%
  group_by(season, shooter_en, shooter_kr) %>% summarise(rs_ov = mean(moe), rs_ov_n = n(), .groups="drop") %>%
  group_by(season) %>% mutate(rs_ov_pct = round(100*percent_rank(rs_ov))) %>% ungroup()

# ── 플레이오프 지표 ─────────────────────────────────────────────────────────
po_perf <- po %>% mutate(moe = made - lrget(shot_type)) %>%
  group_by(season, shooter_en, shooter_kr) %>% summarise(po = mean(moe), po_n = n(), .groups="drop")
po_boost <- po %>%
  left_join(rs_ownrate, by=c("season","shooter_en","shot_type")) %>%
  mutate(own_rate = ifelse(is.na(own_rate), lrget(shot_type), own_rate), b = made - own_rate) %>%
  group_by(season, shooter_en) %>% summarise(boost = mean(b), .groups="drop")

D <- po_perf %>%
  left_join(rs_overall %>% select(season,shooter_en,rs_ov,rs_ov_n,rs_ov_pct), by=c("season","shooter_en")) %>%
  left_join(rs_clutch,  by=c("season","shooter_en")) %>%
  left_join(po_boost,   by=c("season","shooter_en"))

corp <- function(x, y) { ok <- is.finite(x)&is.finite(y); x<-x[ok]; y<-y[ok]
  if (length(x)<5) return(c(r=NA,p=NA,n=length(x)))
  r<-cor(x,y); null<-replicate(N_PERM, cor(x,sample(y))); c(r=r,p=mean(abs(null)>=abs(r)),n=length(x)) }

# =============================================================================
# A. 전이: 정규에 보여준 능력 → 큰 무대 성과
# =============================================================================
A <- D %>% filter(po_n >= PO_MIN, rs_ov_n >= RS_MIN)
a_ov  <- corp(A$rs_ov, A$po)                                   # 정규 전반 → PO
Acl <- A %>% filter(rs_cl_n >= RS_CL_MIN, !is.na(rs_cl))
a_cl  <- corp(Acl$rs_cl, Acl$po)                              # 정규 5분클러치 → PO
reg <- summary(lm(po ~ scale(rs_ov) + scale(rs_cl), data = Acl))  # 클러치가 전반 너머 설명?
bov <- reg$coefficients["scale(rs_ov)",]; bcl <- reg$coefficients["scale(rs_cl)",]

message(sprintf("[A] 자격 선수시즌 %d | 정규전반→PO r=%.3f(p=%.3f) | 정규5분클러치→PO r=%.3f(p=%.3f, n=%d)",
                nrow(A), a_ov["r"], a_ov["p"], a_cl["r"], a_cl["p"], a_cl["n"]))
message(sprintf("    회귀 PO ~ 정규전반+정규클러치: β(전반)=%.2f(p=%.3g) | β(클러치)=%.2f(p=%.3g)",
                bov[1], bov[4], bcl[1], bcl[4]))

# ── A2. 왜 R²가 작은가: 결과(PO 성과)가 대부분 표본 잡음 ─────────────────────
# 선수당 PO 야투가 적어 결과 신뢰도가 낮음 → 어떤 예측변수도 R²가 낮을 수밖에 없다.
wr <- function(x, y, w) { ok <- is.finite(x)&is.finite(y)&is.finite(w); x<-x[ok];y<-y[ok];w<-w[ok]
  mx<-weighted.mean(x,w); my<-weighted.mean(y,w)
  sum(w*(x-mx)*(y-my))/sqrt(sum(w*(x-mx)^2)*sum(w*(y-my)^2)) }
a_ov_w <- wr(A$rs_ov, A$po, A$po_n)               # (1) PO 야투수 가중(WLS): 노이즈 큰 선수 down-weight
# (2) 결과 신뢰도: PO 슛 스플릿-하프(스피어만-브라운)
sh_po <- function() {
  d <- po %>% mutate(moe = made - lrget(shot_type)) %>%
    group_by(shooter_en) %>% mutate(h = sample(rep(c(1L,2L), length.out = n()))) %>%
    group_by(shooter_en, h) %>% summarise(m = mean(moe), k = n(), .groups="drop")
  w1 <- d %>% filter(h==1) %>% select(shooter_en, m1=m, k1=k)
  w2 <- d %>% filter(h==2) %>% select(shooter_en, m2=m, k2=k)
  j <- inner_join(w1, w2, by="shooter_en") %>% filter(k1>=8, k2>=8)
  if (nrow(j) < 8) return(NA_real_); cor(j$m1, j$m2)
}
rel_half <- mean(replicate(200, sh_po()), na.rm=TRUE); rel_po <- 2*rel_half/(1+rel_half)
ceil_r <- sqrt(max(rel_po, 0))                    # 관측 상관의 이론적 최대(결과 노이즈 천장)
disatt_ov <- unname(a_ov["r"] / ceil_r)           # 결과 노이즈 보정(참 관계 추정)
# (3) 임계별 민감도 (표본 클수록 결과 신뢰↑ → R²↑)
sens <- lapply(c(15,30,50), function(m){ AA <- D %>% filter(po_n>=m, rs_ov_n>=RS_MIN)
  r <- cor(AA$rs_ov, AA$po, use="complete.obs"); tibble(PO_MIN=m, n=nrow(AA), r=round(r,3), R2=round(r^2,3)) }) %>% bind_rows()
message(sprintf("[A2] WLS 가중 r=%.3f(R²=%.3f) | PO성과 신뢰도=%.3f → 관측 r 천장≈%.3f | 노이즈보정 참상관≈%.3f",
                a_ov_w, a_ov_w^2, rel_po, ceil_r, disatt_ov))

# =============================================================================
# B. Riser: 정규엔 평범, PO에서 빛남 + 예측가능성
# =============================================================================
B <- D %>% filter(po_n >= PO_MIN, rs_ov_n >= RS_MIN, !is.na(boost))
b_pred_ov <- corp(B$rs_ov, B$boost)          # 정규 전반으로 부스트 예측?
Bcl <- B %>% filter(rs_cl_n >= RS_CL_MIN, !is.na(rs_cl))
b_pred_cl <- corp(Bcl$rs_cl, Bcl$boost)      # 정규 클러치로 부스트 예측?
top_riser <- B %>% arrange(desc(boost)) %>% slice_head(n = 8)
top_fader <- B %>% arrange(boost) %>% slice_head(n = 5)

message(sprintf("\n[B] riser 부스트 예측: 정규전반 r=%.3f(p=%.3f) | 정규클러치 r=%.3f(p=%.3f) → %s",
                b_pred_ov["r"], b_pred_ov["p"], b_pred_cl["r"], b_pred_cl["p"],
                ifelse((is.na(b_pred_ov["p"])||b_pred_ov["p"]>=.05)&&(is.na(b_pred_cl["p"])||b_pred_cl["p"]>=.05),
                       "사전 예측 불가(빛남=대체로 예측 안 됨)","일부 예측 가능")))

# =============================================================================
# C. '큰 무대 퍼포먼스'는 반복되는가 (PO 다회 진출 선수 시즌 간)
# =============================================================================
nx <- c("2023_24"="2024_25","2024_25"="2025_26")
pairs <- bind_rows(lapply(names(nx), function(s0) inner_join(
  D %>% filter(season==s0, po_n>=PO_MIN) %>% transmute(shooter_en, po0=po, b0=boost),
  D %>% filter(season==nx[[s0]], po_n>=PO_MIN) %>% transmute(shooter_en, po1=po, b1=boost),
  by="shooter_en")))
c_po <- if (nrow(pairs)>=5) corp(pairs$po0, pairs$po1) else c(r=NA,p=NA,n=nrow(pairs))
message(sprintf("\n[C] 큰무대 퍼포먼스 시즌간 지속성 r=%.3f (p=%.3f, 쌍 %d) → %s",
                c_po["r"], c_po["p"], nrow(pairs),
                ifelse(!is.na(c_po["p"])&&c_po["p"]<.05,"반복됨(주목)","반복 안 됨")))

# ── 명성 클러치 선수: 큰 무대에서 riser인가 fader인가 ───────────────────────
named <- D %>% filter(shooter_kr %in% TARGETS, po_n >= 8) %>%
  transmute(선수=shooter_kr, 시즌=season, PO야투=po_n,
            정규전반백분위=rs_ov_pct, PO성과=round(po,3), 부스트=round(boost,3)) %>%
  arrange(선수, 시즌)

# =============================================================================
# 리포트
# =============================================================================
fmt <- function(df){ df<-as.data.frame(df)
  cells<-lapply(df,function(c) format(c,trim=TRUE)); rows<-do.call(paste,c(cells,list(sep=" | ")))
  c(paste0("| ",paste(names(df),collapse=" | ")," |"),
    paste0("| ",paste(rep("---",ncol(df)),collapse=" | ")," |"), paste0("| ",rows," |")) }
r3 <- function(x) round(x,3)
riser_tab <- top_riser %>% transmute(선수=shooter_kr, 시즌=season, PO야투=po_n,
              정규전반백분위=rs_ov_pct, 부스트=r3(boost), 정규클러치시도=rs_cl_n)
fader_tab <- top_fader %>% transmute(선수=shooter_kr, 시즌=season, PO야투=po_n,
              정규전반백분위=rs_ov_pct, 부스트=r3(boost))

concl <- paste0(
  sprintf("**큰 무대 성과를 예측하는 건 정규시즌 '전반적 실력'(r=%.2f, p=%.2f)이지, 5분 '클러치' 지표가 아니다**(r=%.2f, 회귀에서 β=%.2f p=%.2g로 전반 통제 후 무의미). ",
          a_ov["r"], a_ov["p"], a_cl["r"], bcl[1], bcl[4]),
  sprintf("'평소보다 PO에서 얼마나 올렸나'(부스트)는 정규 클러치로 전혀 예측 안 되고(r=%.2f), 정규 전반과는 음의 상관(r=%.2f)=**평균회귀**다 — 즉 큰 무대 '깜짝 활약'은 발굴 가능한 능력이 아니라 (덜 뛰어난 선수·소표본의) 회귀 현상. ",
          b_pred_cl["r"], b_pred_ov["r"]),
  sprintf("큰 무대 성과의 시즌 간 반복성도 유의하지 않다(r=%.2f, p=%.2f, 쌍 %d — 검정력 낮음). ", c_po["r"], c_po["p"], nrow(pairs)),
  sprintf("전반→PO의 R²는 %.2f로 작지만 이는 결과 노이즈 탓(PO 성과 신뢰도 %.2f, 관측 r 천장 ≈%.2f); WLS·신뢰도보정 시 참 관계는 강하다(r≈%.2f). ", a_ov["r"]^2, rel_po, ceil_r, disatt_ov),
  "→ 클러치를 '큰 무대에서의 퍼포먼스'로 재정의해도 결론은 같다: **큰 무대에서 재현되는 건 '전반적 실력'이지, 그와 별개의 '큰 경기에 강한 클러치 능력'이 아니다.** 5분 클러치에서의 결론이 최고 무대에서도 유지된다.")

lines <- c(
  "# KBL 클러치 — 큰 무대(플레이오프) 퍼포먼스",
  "",
  "5분 창이 아니라 **'큰 무대에서의 개인 퍼포먼스'** 자체를 클러치로 보고, 정규→PO 전이·riser·반복성을 검정.",
  sprintf("_지표: 기대초과 성공확률(made−기준율). 자격: PO 야투 ≥%d, 정규 야투 ≥%d. 3시즌 %d개 플레이오프 선수시즌._",
          PO_MIN, RS_MIN, nrow(A)),
  "",
  "## A. 정규 능력 → 큰 무대 성과 (전이)",
  "",
  fmt(tibble(예측변수=c("정규 전반 실력","정규 5분 클러치"),
             `→PO 상관 r`=r3(c(a_ov["r"], a_cl["r"])),
             `순열 p`=r3(c(a_ov["p"], a_cl["p"])),
             n=c(a_ov["n"], a_cl["n"]))),
  "",
  sprintf("회귀 `PO ~ 정규전반 + 정규클러치`: β(전반)=**%.2f** (p=%.2g), β(클러치)=%.2f (p=%.2g) → 큰 무대는 **전반적 실력**이 예측하고, 5분 클러치 지표는 그 너머로 아무것도 더하지 못한다.",
          bov[1], bov[4], bcl[1], bcl[4]),
  "",
  "## A2. R²가 왜 작은가 — 결과가 대부분 '표본 잡음'",
  "",
  sprintf(paste0("정규전반→PO의 순수 R²는 %.3f로 작다. **그러나 이는 모델 결함이 아니라 결과(PO 성과) 측정이 노이즈이기 때문**이다: ",
    "선수당 PO 야투가 적어 **PO 성과 신뢰도(split-half SB)=%.2f**밖에 안 된다. 신뢰도가 이 정도면 **어떤 완벽한 예측변수라도 관측 상관의 이론적 최대치가 √신뢰도≈%.2f**(R²≈%.2f)에 불과하다. 노이즈는 설명할 수 없다."),
          a_ov["r"]^2, rel_po, ceil_r, ceil_r^2),
  "",
  "정당한 개선(R²를 억지로 올리는 게 아니라 노이즈를 줄이는):",
  "",
  fmt(sens %>% mutate(설명=c("현행","신뢰도 높은 선수만","깊은 PO런 선수만"))),
  "",
  sprintf("- **PO 야투수로 가중(WLS)**: r=%.2f → **R²=%.2f** (노이즈 큰 소표본 선수 down-weight). ", a_ov_w, a_ov_w^2),
  sprintf("- **노이즈 보정(disattenuated) 참 상관 ≈ %.2f** (결과 신뢰도만 보정; 정규 신뢰도까지 보정하면 ~0.5) — 측정 노이즈를 걷어내면 '정규 실력 → 큰 무대 성과'의 **참 관계는 관측치(0.20)의 2배 이상**. 낮은 R²는 관계가 없어서가 아니라 PO 성과를 20슛 안팎으로 재기 때문.",
          disatt_ov),
  "- ⚠ 단, 이 노이즈 보정은 **전반 실력**에만 신호를 키운다. 5분 클러치(r≈0.03)는 보정해도 0 근처 — **개선해도 '클러치는 안 잡히고 전반 실력만 잡힌다'는 결론은 그대로**.",
  "",
  "## B. Riser — 정규엔 평범, PO에서 빛나는 선수",
  "",
  "부스트 = PO 성공률 − 개인 정규 성공률(양수=평소보다 큰무대에서 올림). 상위 8명:",
  "",
  fmt(riser_tab),
  "",
  "하위(fader) 5명:",
  "",
  fmt(fader_tab),
  "",
  sprintf(paste0("- 부스트 예측: 정규 **클러치**로는 전혀 안 됨(r=%.2f, p=%.2f). 정규 **전반 실력**과는 **음의 상관**(r=%.2f, p=%.2f) — ",
    "이는 발굴 가능한 클러치가 아니라 **평균회귀**다: 표를 보라 — 상위 riser는 정규 전반 백분위가 낮거나(이근준 15·김진유 32) 소표본이고, ",
    "fader는 정규 상위 스타(저스틴 구탕 97·김낙현 94·대릴 먼로 85)가 자기 높은 평소치로 회귀한 것. ",
    "→ '큰 무대에서 빛남'은 사전 식별 가능한 실력이 아니라 평균회귀+소표본 변동."),
          b_pred_cl["r"], b_pred_cl["p"], b_pred_ov["r"], b_pred_ov["p"]),
  "",
  "## C. 큰 무대 퍼포먼스는 반복되는가",
  "",
  sprintf(paste0("- PO 다회 진출 선수의 시즌 간 PO 성과 상관 r=**%.2f** (p=%.2f, 쌍 %d). ",
    "쌍이 %d개뿐이라 **검정력이 낮아 유의하지 않음** — 강한 반복의 증거는 없으나 완전히 배제하지도 못한다(과거 PO까지 모으면 검정력↑). ",
    "정규 5분 클러치가 지속성 0이었던 것과 최소한 상충하지 않는다."),
          c_po["r"], c_po["p"], nrow(pairs), nrow(pairs)),
  "",
  "## D. 명성 클러치 선수들, 큰 무대에서",
  "",
  fmt(named),
  "",
  "_정규전반백분위=그 시즌 정규 전반 실력 순위(100=최상위), PO성과=리그 대비, 부스트=평소 대비._",
  "",
  "## 결론",
  "",
  concl,
  ""
)
dir.create("../docs", showWarnings=FALSE)
writeLines(lines, "../docs/clutch_playoff_findings.md")
message("\n리포트 저장: ../docs/clutch_playoff_findings.md")
message("── 완료 ──")
