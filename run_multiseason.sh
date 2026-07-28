#!/usr/bin/env bash
# 세 시즌 통합 파이프라인 드라이버.
#   1) 각 시즌 전처리(01) → *_state_table.csv / *_shots.csv
#   2) 통합 분석(10)      → 풀링 기준선 + 존재검정 + 시즌간 지속성 + 리포트
# R 스크립트는 작업 디렉터리 기준으로 CSV를 읽고 쓰므로 data/ 에서 실행한다.
set -euo pipefail
cd "$(dirname "$0")/data"

for S in 2023_24 2024_25 2025_26; do
  echo "==================== 전처리 SEASON $S ===================="
  SEASON_LABEL="$S" Rscript ../R/01_preprocess.R
done

echo "==================== 세 시즌 통합 (10_multiseason.R) ===================="
Rscript ../R/10_multiseason.R

echo "완료. 리포트: docs/multiseason_findings.md"
