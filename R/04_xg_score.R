# =============================================================================
# WP 모델 3단계: 기대 성공률(coarse xG) + 슛별 클러치 기여
# -----------------------------------------------------------------------------
# xG = 리그 기준선 성공률 (상황 중립, 현재 데이터로는 슛값 2/3만으로 구성)
#   ※ 리그 기준선을 쓰는 이유: 선수 개인 평균으로 만들면 made-xG가 항상 0이 되어
#     클러치 능력이 사라짐. "리그 평균 대비 얼마나 잘했나"를 봐야 함.
#   ※ 어시스트 여부는 성공 슛에만 관측되어(선택 편향) xG에서 제외.
#   ※ 클러치 효과는 xG에 넣지 않음 — 최종 점수에서 드러나야 하므로.
#
# 슛 1개의 클러치 기여 = LI × 기대초과득점(points over expected)
#   → 4단계에서 선수별로 합산 + 베이지안 축소
#
# 입력 : KBL_<season>_shots_leverage.csv
# 출력 : *_shots_scored.csv
#
# 파이썬 검증(2024-25): 2점 0.513 / 3점 0.315, xG총합=실제성공, 리그평균 0
#   install.packages(c("dplyr","readr"))
# =============================================================================

library(dplyr); library(readr)

SEASON_LABEL <- "2024_25"
sh <- read_csv(sprintf("KBL_%s_shots_leverage.csv", SEASON_LABEL), show_col_types = FALSE)

# ── 리그 기준선 성공률 (상황 중립, 슛값만) ──────────────────────────────────
base2 <- mean(sh$made[sh$shot_value == 2])
base3 <- mean(sh$made[sh$shot_value == 3])

sh <- sh %>% mutate(
  xg            = ifelse(shot_value == 3, base3, base2),
  make_over_exp = made - xg,                     # 기대초과 성공확률
  pts_scored    = made * shot_value,
  xpts          = xg * shot_value,
  pts_over_exp  = pts_scored - xpts,             # 기대초과 득점 (3점을 더 크게 반영)
  contribution  = LI * pts_over_exp              # 슛 1개의 클러치 기여
)

write_excel_csv(sh, sprintf("KBL_%s_shots_scored.csv", SEASON_LABEL))

# ── 리포트 ──────────────────────────────────────────────────────────────────
message(sprintf("리그 기준선: 2점 %.3f | 3점 %.3f", base2, base3))
message(sprintf("xG 총합 %.0f vs 실제 성공 %d  (리그 전체 일치해야 정상)",
                sum(sh$xg), sum(sh$made)))
message(sprintf("pts_over_exp 리그 평균 %.4f  (0에 수렴)", mean(sh$pts_over_exp)))
message(sprintf("기여값 예시 — 2점성공 %+.2f / 2점실패 %+.2f / 3점성공 %+.2f / 3점실패 %+.2f (LI=1 기준)",
                (1 - base2) * 2, (0 - base2) * 2, (1 - base3) * 3, (0 - base3) * 3))
