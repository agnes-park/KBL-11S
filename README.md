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

## 세 시즌 통합 (구현 완료)

```bash
./run_multiseason.sh        # 3시즌 전처리(01) 후 통합 분석(10) 실행
# 리포트: docs/multiseason_findings.md
```

리그 기준선(WP σ·β, OReb, xG)은 세 시즌 **풀링**, 선수 단위는 **선수×시즌**으로 유지해
시즌 간 지속성을 검정한다. 각 스크립트는 `SEASON_LABEL` 환경변수로 시즌을 바꿀 수 있다
(예: `SEASON_LABEL=2025_26 Rscript ../R/01_preprocess.R`).

## 플레이오프 수집 (로컬 실행)

포스트시즌은 "정규시즌 더"와 질적으로 다른 무대(고압박·out-of-sample)라, RS→PO 이월
검정 등에 유효하다. 크롤러는 정규 크롤러(`00`)의 검증된 헬퍼를 재사용한다.

```bash
# 사전: install.packages(c("httr2","jsonlite","dplyr","stringr","readr","tibble","tidyr"))
./run_playoffs.sh            # 3시즌 PO 수집 → 삼중소스 무결성 검증
# 또는 시즌별로:
cd data
SEASON_KEY=2024_25 Rscript ../R/00b_crawl_playoffs.R
PBP_KIND=playoff  Rscript ../R/15_data_audit.R
```

⚠ **`api.kbl.or.kr` 에 접근 가능한 로컬에서 실행**할 것(관리형 원격 세션은 egress 정책으로
차단될 수 있음). 첫 실행 시 콘솔의 "수신 카테고리 분포"로 플레이오프가 제대로 잡혔는지 확인.
과거 시즌 추가는 `R/00b_crawl_playoffs.R`의 `PLAYOFF_MONTHS` + `run_playoffs.sh`의 `SEASONS`에.

## 현재 상태

세 시즌(2023-24·2024-25·2025-26)·coarse xG 통합 완료. **반복 가능한 클러치 실력의
통계적 증거는 없음** — 풀링 존재검정은 한계적(순열 p≈0.03)이나 시즌 간 지속성이 없다
(year-to-year r≈0.02). 검정력을 3배로 키운 뒤에도 신호가 지속성 검정을 통과하지 못한다.
다음 지렛대는 표본이 아니라 **수동 xG(슛 난이도 보정)** — 자세한 건 `CLAUDE.md` 참고.
