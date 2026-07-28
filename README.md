# KBL 클러치 슈터 분석

"클러치 슈터는 진짜 존재하는가?"를 KBL play-by-play 데이터로 검증하는 프로젝트.

## Claude Code로 시작하기

1. 이 폴더를 로컬에 풀고 터미널에서 폴더로 이동한다.
2. `claude` 를 실행한다. (세션이 `CLAUDE.md`를 읽어 프로젝트 맥락을 파악한다.)
3. 예: "CLAUDE.md 읽고, 다음 단계인 세 시즌 통합부터 시작하자"

## 폴더 구조

```
CLAUDE.md          ← 프로젝트 맥락·발견·주의사항·다음단계 (먼저 읽기)
R/                 ← 파이프라인 (00 수집 → 05 클러치검정)
data/              ← 3시즌 원본 CSV (pbp·schedule·validation)
annotation/        ← 수동 슛 위치·컨테스트 코딩 도구
```

## 파이프라인 실행 (한 시즌)

R 스크립트는 작업 디렉터리 기준으로 CSV를 읽고 쓴다. `data/`에서 실행한다.

```bash
cd data
Rscript ../R/01_preprocess.R      # state_table, shots, player_dim
Rscript ../R/02_wp_model.R        # WP 모델 + KBL_wp_model.rds
Rscript ../R/03_leverage.R        # 슛별 레버리지
Rscript ../R/04_xg_score.R        # coarse xG + 기여값
Rscript ../R/05_clutch_analysis.R # 클러치 존재검정 + 선수 순위
```

시즌 변경: 각 스크립트 상단 `SEASON_LABEL`을 `"2023_24"` / `"2025_26"`로.

필요 패키지: `install.packages(c("dplyr","stringr","readr","tidyr"))`
(수집 스크립트는 `httr2","jsonlite","tibble"` 추가)

## 현재 상태

한 시즌(2024-25)·coarse xG 기준 파이프라인 완주. 클러치 실력의 통계적 증거는
아직 없음(선수당 클러치 슛 중앙값 7개로 표본 부족). 다음 단계는 세 시즌 통합과
수동 xG 격상 — 자세한 건 `CLAUDE.md` 참고.
