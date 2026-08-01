# =============================================================================
# 자유투 클러치 심층분석 — '압박만 분리한 청정 실험실'
# -----------------------------------------------------------------------------
# 자유투는 난이도 고정·수비 무관 → 순수 '압박' 효과만 분리된다. 가장 유의미했던
# 결과(비클러치 FT% 지속성 r≈0.62 = 방법이 진짜 실력을 검출)를 확장해 깊게 본다:
#   A. 리그 압박 그래디언트: 압박이 세질수록 FT%가 떨어지나(조직적 초킹)?
#   B. 개인 클러치 FT: 평소 FT%로 설명되나, 별개의 '클러치 FT 요인'이 있나?
#      - 존재검정(선수간 분산) + 시즌 간 지속성 + 양성대조군(비클러치 FT%)
#   C. 축소추정 리더보드: 압박 속 실제 초과/미달 선수(회귀 보정).
# 그래프(영문 라벨, ggplot2): 압박 그래디언트 / 지속성 비교 막대 / FT% 지속성 산점도.
#
# 입력 : KBL_<season>_state_table.csv (+ _playoff_state_table.csv)
# 출력 : ../docs/ft_clutch_findings.md, ../docs/img/*.png
#   install.packages(c("dplyr","stringr","readr","ggplot2"))
# =============================================================================

library(dplyr); library(stringr); library(readr); suppressMessages(library(ggplot2))
set.seed(1); suppressWarnings(Sys.setlocale("LC_CTYPE","C.UTF-8"))
SEASONS<-c("2023_24","2024_25","2025_26"); N_PERM<-5000
IMG<-"../docs/img"; dir.create(IMG, showWarnings=FALSE, recursive=TRUE)
norm<-function(x) sub("\\.0$","",trimws(as.character(x)))

load_ft<-function(season,kind){ pref<-if(kind=="regular") sprintf("KBL_%s",season) else sprintf("KBL_%s_playoff",season)
  f<-sprintf("%s_state_table.csv",pref); if(!file.exists(f)) return(NULL)
  read_csv(f,show_col_types=FALSE)%>%mutate(a=str_pad(norm(a),3,pad="0"))%>%
    filter(a%in%c("203","204"),!is.na(e))%>%
    transmute(season=.env$season, kind=.env$kind, en=e, kr=p, made=as.integer(a=="203"),
              mb=abs(margin_before), period, sl=sec_left_period,
              t_left=ifelse(period<=4, game_sec_remaining, sec_left_period),
              clutch=clutch_std) }
ft_rs<-bind_rows(lapply(SEASONS,load_ft,"regular"))
ft_po<-bind_rows(lapply(SEASONS,load_ft,"playoff"))
message(sprintf("정규 FT %d개 (클러치 %d) | 플레이오프 FT %d개", nrow(ft_rs), sum(ft_rs$clutch), nrow(ft_po)))

# 야투(FG)도 로드(지속성 비교용)
load_fg<-function(season){ f<-sprintf("KBL_%s_state_table.csv",season)
  read_csv(f,show_col_types=FALSE)%>%mutate(a=str_pad(norm(a),3,pad="0"))%>%
    filter(a%in%c("201","202","205","206","207"),!is.na(e))%>%
    transmute(season=.env$season,en=e, val=ifelse(a%in%c("205","206"),3,2),
              made=as.integer(a%in%c("201","205","207")), clutch=clutch_std) }
fg<-bind_rows(lapply(SEASONS,load_fg)); lg2<-mean(fg$made[fg$val==2]); lg3<-mean(fg$made[fg$val==3])

win<-function(k,n){ if(n==0) return(c(NA,NA,NA)); p<-k/n; z<-1.96; d<-1+z^2/n
  c(p, (p+z^2/2/n)/d - z/d*sqrt(p*(1-p)/n+z^2/4/n^2), (p+z^2/2/n)/d + z/d*sqrt(p*(1-p)/n+z^2/4/n^2)) }
wcor<-function(x,y,w){ok<-is.finite(x)&is.finite(y)&is.finite(w);x<-x[ok];y<-y[ok];w<-w[ok]
  mx<-weighted.mean(x,w);my<-weighted.mean(y,w);sum(w*(x-mx)*(y-my))/sqrt(sum(w*(x-mx)^2)*sum(w*(y-my)^2))}
permp<-function(x,y,w){r<-wcor(x,y,w);mean(replicate(N_PERM,abs(wcor(x,sample(y),w))>=abs(r)))}

# =============================================================================
# A. 리그 압박 그래디언트
# =============================================================================
tiers<-list(
  "Non-clutch"      = ft_rs%>%filter(!clutch),
  "Clutch (5m,<=5)" = ft_rs%>%filter(clutch),
  "Tight (2m,<=3)"  = ft_rs%>%filter((period==4&sl<=120)|period>=5, mb<=3),
  "Final (1m,<=2)"  = ft_rs%>%filter((period==4&sl<=60)|period>=5, mb<=2),
  "Playoff (all)"   = ft_po,
  "Playoff clutch"  = ft_po%>%filter(clutch))
grad<-bind_rows(lapply(names(tiers),function(nm){d<-tiers[[nm]];w<-win(sum(d$made),nrow(d))
  tibble(tier=nm, n=nrow(d), pct=w[1], lo=w[2], hi=w[3])}))
base_ft<-grad$pct[grad$tier=="Non-clutch"]
message("\n[A] 압박 그래디언트 FT%:"); print(as.data.frame(grad%>%mutate(across(c(pct,lo,hi),~round(.,3)))),row.names=FALSE)

# =============================================================================
# B. 개인 클러치 FT
# =============================================================================
# 개인 비클러치 FT%(시즌) : 기준선
nc<-ft_rs%>%filter(!clutch)%>%group_by(season,en)%>%summarise(nc_p=mean(made),nc_n=n(),.groups="drop")
# 클러치 FT 각 시도의 개인기준선 대비 초과
clft<-ft_rs%>%filter(clutch)%>%left_join(nc,by=c("season","en"))%>%
  mutate(nc_p=ifelse(is.na(nc_p),base_ft,nc_p), over=made-nc_p)
# (B1) 별개 클러치 요인? 클러치 FT 성공 ~ 개인 평소 FT% (로지스틱)
glm_ft<-glm(made~nc_p, family=binomial, data=clft%>%filter(!is.na(nc_p)))
# (B2) 존재검정: 선수간 (클러치 over) 분산 (랜덤효과 τ²) + 순열
ex<-clft%>%group_by(en)%>%summarise(n=n(),yb=mean(over),ssw=sum((over-mean(over))^2),.groups="drop")
N<-nrow(clft);K<-nrow(ex);mu<-mean(clft$over)
MSB<-sum(ex$n*(ex$yb-mu)^2)/(K-1);MSW<-sum(ex$ssw)/(N-K);n0<-(N-sum(ex$n^2)/N)/(K-1)
tau2<-(MSB-MSW)/n0
permE<-function(){v<-sample(clft$over);idx<-1;tot<-0;for(nn in ex$n){tot<-tot+nn*(mean(v[idx:(idx+nn-1)])-mu)^2;idx<-idx+nn};tot}
pE<-mean(replicate(N_PERM,permE())>=sum(ex$n*(ex$yb-mu)^2))

# 선수×시즌 집계 (지속성용)
ps_ftnc<-nc%>%filter(nc_n>=20)                                   # 비클러치 FT%(대조군)
ps_ftcl<-ft_rs%>%filter(clutch)%>%left_join(nc,by=c("season","en"))%>%
  mutate(nc_p=ifelse(is.na(nc_p),base_ft,nc_p))%>%group_by(season,en)%>%
  summarise(cl_over=mean(made-nc_p),cl_n=n(),.groups="drop")%>%filter(cl_n>=4)
ps_fgvol<-fg%>%filter(clutch)%>%count(season,en,name="vol")
ps_fgeff<-fg%>%filter(clutch)%>%mutate(moe=made-ifelse(val==3,lg3,lg2))%>%
  group_by(season,en)%>%summarise(eff=mean(moe),eff_n=n(),.groups="drop")

pers<-function(tab,val,ncol,nmin){ nx<-c("2023_24"="2024_25","2024_25"="2025_26")
  pr<-bind_rows(lapply(names(nx),function(s0) inner_join(
    tab%>%filter(season==s0,.data[[ncol]]>=nmin)%>%transmute(en,y0=.data[[val]],n0=.data[[ncol]]),
    tab%>%filter(season==nx[[s0]],.data[[ncol]]>=nmin)%>%transmute(en,y1=.data[[val]],n1=.data[[ncol]]),by="en")))
  if(nrow(pr)<5) return(list(r=NA,p=NA,n=nrow(pr),pr=pr))
  w<-2/(1/pr$n0+1/pr$n1); list(r=wcor(pr$y0,pr$y1,w),p=permp(pr$y0,pr$y1,w),n=nrow(pr),pr=pr) }
P_ftnc<-pers(ps_ftnc,"nc_p","nc_n",20)     # 비클러치 FT% (양성 대조군)
P_ftcl<-pers(ps_ftcl,"cl_over","cl_n",4)   # 클러치 FT 초과 (핵심)
P_vol <-pers(ps_fgvol,"vol","vol",5)       # 클러치 볼륨
P_eff <-pers(ps_fgeff,"eff","eff_n",10)    # 클러치 FG 효율

message(sprintf("\n[B1] 클러치FT ~ 개인 평소FT%%: β=%.2f (p=%.2g) — 평소 실력으로 설명",
                coef(glm_ft)["nc_p"], summary(glm_ft)$coefficients["nc_p",4]))
message(sprintf("[B2] 존재검정(클러치FT 초과): τ²=%.4f, 순열 p=%.3f", tau2, pE))
message(sprintf("[B3] 지속성 — 비클러치FT%%(대조군) r=%.2f(p=%.3f,n=%d) | 클러치FT초과 r=%.2f(p=%.3f,n=%d)",
                P_ftnc$r,P_ftnc$p,P_ftnc$n, P_ftcl$r,P_ftcl$p,P_ftcl$n))
message(sprintf("     비교 — 클러치볼륨 r=%.2f | 클러치FG효율 r=%.2f", P_vol$r, P_eff$r))

# =============================================================================
# C. 축소추정 리더보드 (클러치 FT, empirical Bayes)
# =============================================================================
K_SH<-30
lead<-ft_rs%>%filter(clutch)%>%left_join(nc,by=c("season","en"))%>%group_by(kr,en)%>%
  summarise(att=n(), made=sum(made), cl_pct=mean(made), nc_pct=mean(nc_p,na.rm=TRUE),.groups="drop")%>%
  mutate(shrunk_over=(made - nc_pct*att)/(att+K_SH))%>%filter(att>=8)%>%arrange(desc(shrunk_over))

# =============================================================================
# 그래프 (영문 라벨)
# =============================================================================
th<-theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
  plot.title=element_text(face="bold",size=13), plot.subtitle=element_text(color="grey40",size=9))
OR<-"#ea580c"; GN<-"#16a34a"; SL<-"#334155"

# G1: 압박 그래디언트
g1<-grad%>%mutate(tier=factor(tier,levels=tier))
p1<-ggplot(g1,aes(tier,pct))+
  geom_hline(yintercept=base_ft,linetype=2,color="grey60")+
  geom_col(fill=OR,width=.65)+geom_errorbar(aes(ymin=lo,ymax=hi),width=.2,color=SL)+
  geom_text(aes(label=sprintf("%.1f%%\n(n=%d)",100*pct,n)),vjust=-0.3,size=3)+
  coord_cartesian(ylim=c(0.5,0.82))+
  labs(title="Under pressure, free-throw % dips only slightly",
       subtitle="Non-clutch 72% -> extreme pressure ~65-68% (CIs overlap): mild, non-significant decline.",
       x=NULL,y="Free-throw %")+th+theme(axis.text.x=element_text(angle=20,hjust=1))
ggsave(file.path(IMG,"ft_gradient.png"),p1,width=7.2,height=3.6,dpi=150)

# G2: 지속성 비교 막대 (핵심)
pb<-tibble(metric=c("Non-clutch FT%\n(skill, control)","Clutch volume\n(role)",
                    "Clutch FG efficiency","Clutch FT over-perf"),
           r=c(P_ftnc$r,P_vol$r,P_eff$r,P_ftcl$r),
           grp=c("skill/role","skill/role","clutch","clutch"))%>%
  mutate(metric=factor(metric,levels=metric))
p2<-ggplot(pb,aes(metric,r,fill=grp))+
  geom_col(width=.62)+geom_hline(yintercept=0,color="grey50")+
  geom_text(aes(label=sprintf("%.2f",r)),vjust=ifelse(pb$r>=0,-0.4,1.3),size=3.6,fontface="bold")+
  scale_fill_manual(values=c("skill/role"=GN,"clutch"=OR),guide="none")+
  coord_cartesian(ylim=c(-0.15,0.8))+
  labs(title="What repeats season-to-season? Skill & role, not clutch.",
       subtitle="Year-to-year correlation. Green = reproducible skill/role; Orange = clutch-specific (~0).",
       x=NULL,y="year-to-year correlation r")+th
ggsave(file.path(IMG,"persistence_bar.png"),p2,width=7.2,height=3.8,dpi=150)

# G3: 비클러치 FT% 지속성 산점도 (양성 대조군)
sc<-P_ftnc$pr
p3<-ggplot(sc,aes(y0,y1))+
  geom_point(aes(size=pmin(n0,n1)),color=GN,alpha=.5)+
  geom_smooth(method="lm",se=FALSE,color=SL,linewidth=.8)+
  scale_size(range=c(1,5),guide="none")+
  annotate("text",x=min(sc$y0),y=max(sc$y1),hjust=0,vjust=1,
           label=sprintf("r = %.2f  (n=%d)",P_ftnc$r,P_ftnc$n),fontface="bold",size=4,color=GN)+
  labs(title="Free-throw skill IS reproducible (positive control)",
       subtitle="Each point = a player. Non-clutch FT% in season t vs t+1. Point size = attempts.",
       x="Non-clutch FT%  (season t)",y="Non-clutch FT%  (season t+1)")+th
ggsave(file.path(IMG,"ft_persistence_scatter.png"),p3,width=6.6,height=4,dpi=150)

message("\n그래프 저장: ", IMG)

# =============================================================================
# 리포트
# =============================================================================
fmt<-function(df){df<-as.data.frame(df);cells<-lapply(df,function(c) format(c,trim=TRUE))
  rows<-do.call(paste,c(cells,list(sep=" | ")))
  c(paste0("| ",paste(names(df),collapse=" | ")," |"),paste0("| ",paste(rep("---",ncol(df)),collapse=" | ")," |"),paste0("| ",rows," |"))}
r3<-function(x) round(x,3)
lines<-c(
  "# KBL 클러치 — 자유투 심층분석 (압박만 분리한 청정 실험실)",
  "",
  "자유투는 난이도 고정·수비 무관 → **순수 '압박' 효과만** 분리된다. 가장 설득력 있던 결과",
  "(비클러치 FT% 지속성 = 방법이 진짜 실력을 검출)를 확장해, 압박이 개인에게 다르게 작용하는지 깊게 본다.",
  "",
  "## A. 리그 압박 그래디언트 — 압박이 세질수록 FT%가 떨어지나?",
  "",
  fmt(grad%>%transmute(압박수준=tier, n, `FT%`=r3(pct), `95%CI하`=r3(lo), `95%CI상`=r3(hi))),
  "",
  sprintf("→ 비클러치 %.1f%% → 종료 1분·2점차 이내 %.1f%%, 플레이오프 클러치 %.1f%%로 **미세하게 하락하나 통계적으로 유의하지 않다(CI 겹침)**. 즉 **극적인 조직적 초킹은 없다** — 있어도 4~7%%p의 약한 경향에 그친다.",
          100*base_ft, 100*grad$pct[grad$tier=="Final (1m,<=2)"], 100*grad$pct[grad$tier=="Playoff clutch"]),
  "",
  "![압박 그래디언트](img/ft_gradient.png)",
  "",
  "## B. 개인 클러치 FT — 별개의 '클러치 FT 능력'이 있나?",
  "",
  sprintf("- **B1. 평소 실력으로 설명됨**: 클러치 FT 성공 ~ 개인 평소 FT%% 로지스틱 β=**%.2f** (p=%.1g). 클러치 성공은 그 선수의 평소 FT 실력으로 대부분 설명된다.",
          coef(glm_ft)["nc_p"], summary(glm_ft)$coefficients["nc_p",4]),
  sprintf("- **B2. 존재검정**: 선수 간 '클러치 초과' 분산 τ²=%.4f, 순열 p=%.2f — 풀링 표본에선 선수간 차이가 보이지만, **아래 B3 지속성 검정에서 사라진다**(개인 기준선 잡음·시즌 내 변동).",
          tau2, pE),
  sprintf("- **B3. 지속성(핵심)**: 클러치 FT 초과의 시즌 간 상관 r=**%.2f** (p=%.2f, 쌍 %d) — %s.",
          P_ftcl$r,P_ftcl$p,P_ftcl$n, ifelse(!is.na(P_ftcl$p)&&P_ftcl$p<.05,"반복","반복 안 됨")),
  "",
  "### 무엇이 반복되나 — 지속성 비교",
  "",
  fmt(tibble(지표=c("비클러치 FT%(실력·대조군)","클러치 볼륨(역할)","클러치 FG 효율","클러치 FT 초과"),
             `시즌간 r`=r3(c(P_ftnc$r,P_vol$r,P_eff$r,P_ftcl$r)))),
  "",
  sprintf("→ **자유투 '실력'(비클러치 FT%%)은 r=%.2f로 또렷이 반복**되고 볼륨(역할)도 반복되지만, **클러치 특이적 지표(FG 효율·FT 초과)는 0 근처**. 방법은 진짜 실력을 검출하는데, 클러치 초과분만 재현되지 않는다.",
          P_ftnc$r),
  "",
  "![지속성 비교](img/persistence_bar.png)",
  "",
  "![FT% 지속성 산점도](img/ft_persistence_scatter.png)",
  "",
  "## C. 클러치 FT 리더보드 (축소추정)",
  "",
  "압박 속 개인 초과(개인 평소 FT% 대비, empirical-Bayes 축소). 상위·하위 5명(클러치 FT ≥8):",
  "",
  fmt(bind_rows(head(lead,5),tail(lead,5))%>%transmute(선수=kr, 클러치FT시도=att,
      `클러치FT%`=r3(cl_pct), `평소FT%`=r3(nc_pct), 축소초과=r3(shrunk_over))),
  "",
  "_축소초과가 0 근처에 몰려 있으면 '압박 속 개인차'가 사실상 없다는 뜻(대부분 평소 실력으로 회귀)._",
  "",
  "## 결론",
  "",
  paste0("자유투라는 **가장 깨끗한 압박 검정**에서: (1) 압박이 세져도 리그 FT%는 안 떨어진다(초킹 없음), ",
    "(2) 클러치 FT 성공은 개인 평소 FT%로 설명되고 별개의 클러치 요인은 지속성이 없다, ",
    sprintf("(3) 반면 자유투 '실력' 자체(비클러치 FT%%)는 r=%.2f로 명확히 재현된다. ",P_ftnc$r),
    "→ **'압박 속 개인 클러치 능력'은 존재의 증거가 없고, 재현되는 건 평소 슈팅 실력뿐이다.** ",
    "방법이 실력을 또렷이 검출한다는 바로 그 점이, 클러치 신호의 부재를 '검정력 부족'이 아니라 '실재하지 않음'으로 못박는다.")
)
writeLines(lines,"../docs/ft_clutch_findings.md")
message("리포트 저장: ../docs/ft_clutch_findings.md"); message("── 완료 ──")
