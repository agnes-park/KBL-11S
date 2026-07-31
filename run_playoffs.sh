#!/usr/bin/env bash
# =============================================================================
# 플레이오프 수집 + 무결성 검증 드라이버 (로컬 전용)
# -----------------------------------------------------------------------------
# ⚠ api.kbl.or.kr 에 접근 가능한 곳(로컬)에서 실행할 것.
#   관리형 원격 세션은 egress 정책으로 kbl.or.kr 이 차단될 수 있음.
#
# 사전 준비(최초 1회):
#   install.packages(c("httr2","jsonlite","dplyr","stringr","readr","tibble","tidyr"))
#
# 실행:  ./run_playoffs.sh
#   1) 각 시즌 플레이오프 PBP 수집 → KBL_<season>_playoff_full_pbp.csv 등
#   2) 삼중 소스 무결성 검증 → docs/data_audit_findings.md (PBP_KIND=playoff)
#
# 과거 시즌을 추가하려면 아래 SEASONS 와 R/00b_crawl_playoffs.R 의
# PLAYOFF_MONTHS 에 시즌을 함께 추가하면 됩니다.
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")/data"

SEASONS=("2023_24" "2024_25" "2025_26")

for S in "${SEASONS[@]}"; do
  echo "==================== 플레이오프 수집: $S ===================="
  SEASON_KEY="$S" Rscript ../R/00b_crawl_playoffs.R
done

echo "==================== 무결성 검증 (삼중 소스) ===================="
PBP_KIND=playoff Rscript ../R/15_data_audit.R

echo "완료. 산출물: data/KBL_<season>_playoff_full_pbp.csv | 리포트: docs/data_audit_findings.md"
echo "첫 실행 시 각 시즌의 '수신 카테고리 분포' 로그를 확인해 플레이오프가 제대로 잡혔는지 검증하세요."
