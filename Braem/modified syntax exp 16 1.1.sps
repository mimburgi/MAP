* Encoding: UTF-8.

*** making extra useful variables

COMPUTE trialtest = trial - 12.
EXECUTE.
COMPUTE blocktrial=MOD(trialtest,80).
EXECUTE.
if blocktrial = 0 blocktrial = 80.
EXECUTE.

if corr = 1 and rew = 1 score = 1.
if corr = 1 and rew = 2 score = 10.
EXECUTE.

if task = lag(task,1) taskseq = 0.
if task ~= lag(task,1) taskseq = 1.
EXECUTE.

*** GROUP variable: ODD subjects were assigned to the reward switching group, EVEN subjects to reward repetition group

COMPUTE group=MOD(subj,2).
EXECUTE.

*** outlier criteria for RTs

DO IF ( rt > 150 & block > 0 & lag(corr,1) = 1 & corr = 1 & expphase = lag(expphase,1) ).
RECODE
  rt
  (ELSE=Copy)  INTO  rtcor .
END IF .
EXECUTE .


AGGREGATE
  /OUTFILE=
  MODE=ADDVARIABLES
  /BREAK= subj expphase 
  /rt_sd = SD(rtcor) /rt_mean = MEAN(rtcor).

COMPUTE outlierlow = rt_mean - (rt_sd * 2.5) .
EXECUTE .
COMPUTE outlierup = rt_mean + (rt_sd * 2.5) .
EXECUTE .

DO IF ( rt>outlierlow & rt<outlierup ) .
RECODE
  rtcor
  (ELSE=Copy)  INTO  rtcorr .
END IF .
EXECUTE .

*** making task-specific RT variables fo = forced = cued; fr = free = voluntary

if expphase = 1 and task = lag(task,1) and con = 0 forepcon = rtcorr.
if expphase = 1 and task = lag(task,1) and con = 1 forepincon = rtcorr.
if expphase = 1 and task ~= lag(task,1) and con = 0 foswicon = rtcorr.
if expphase = 1 and task ~= lag(task,1) and con = 1 foswiincon = rtcorr.
if expphase = 2 and task = lag(task,1) and con = 0 frrepcon = rtcorr.
if expphase = 2 and task = lag(task,1) and con = 1 frrepincon = rtcorr.
if expphase = 2 and task ~= lag(task,1) and con = 0 frswicon = rtcorr.
if expphase = 2 and task ~= lag(task,1) and con = 1 frswiincon = rtcorr.
EXECUTE.


*** outlier criteria error rates

DO IF (expphase > 0 & expphase = lag(expphase,1)) .
RECODE
  corr
  (ELSE=Copy)  INTO  corrcl .
END IF .
EXECUTE .

if corrcl = 1 and lag(corr,1) = 1 err = 0.
if corrcl = 0 and lag(corr,1) = 1 err = 1.
EXECUTE.



if expphase = 1 and r > 2 and corr = 1 and task = 0 taskone = 0.
if expphase = 1 and r < 3 and corr = 1 and task = 0 taskone = 1.
if expphase = 1 and r > 2 and corr = 1 and task = 1 tasktwo = 0.
if expphase = 1 and r < 3 and corr = 1 and task = 1 tasktwo = 1.
EXECUTE.


AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=subj
  /taskone_mean=MEAN(taskone) 
  /tasktwo_mean=MEAN(tasktwo).



*** making task-specific error and task choice variables

if expphase = 2 and r > 2 and lag(corr,1) = 1 and corr = 1 taskch = 1.
if expphase = 2 and r < 3 and lag(corr,1) = 1 and corr = 1 taskch = 0.
if expphase = 2 and taskone_mean = 0 and r > 2 and corr = 1 taskchoice = 1.
if expphase = 2 and taskone_mean = 0 and r < 3 and corr = 1 taskchoice = 0.
if expphase = 2 and taskone_mean = 1 and r > 2 and corr = 1 taskchoice = 0.
if expphase = 2 and taskone_mean = 1 and r < 3 and corr = 1 taskchoice = 1.
EXECUTE.

if expphase = 1 and task = lag(task,1) and con = 0 errforepcon = err.
if expphase = 1 and task = lag(task,1) and con = 1 errforepincon = err.
if expphase = 1 and task ~= lag(task,1) and con = 0 errfoswicon = err.
if expphase = 1 and task ~= lag(task,1) and con = 1 errfoswiincon = err.
if expphase = 2 and task = lag(task,1) and con = 0 errfrrepcon = err.
if expphase = 2 and task = lag(task,1) and con = 1 errfrrepincon = err.
if expphase = 2 and task ~= lag(task,1) and con = 0 errfrswicon = err.
if expphase = 2 and task ~= lag(task,1) and con = 1 errfrswiincon = err.
if expphase = 2 and lag(expphase,1) = 2 and taskch = lag(taskch,1) taskchswi = 0.
if expphase = 2 and lag(expphase,1) = 2 and taskch ~= lag(taskch,1) taskchswi = 1.
if expphase = 2 and lag(expphase,1) = 2 and block = 1 and taskch = lag(taskch,1) fibtaskchswi = 0.
if expphase = 2 and lag(expphase,1) = 2 and block = 1 and taskch ~= lag(taskch,1) fibtaskchswi = 1.
if expphase = 2 and lag(expphase,1) = 2 and block = 2 and taskch = lag(taskch,1) sebtaskchswi = 0.
if expphase = 2 and lag(expphase,1) = 2 and block = 2 and taskch ~= lag(taskch,1) sebtaskchswi = 1.
if expphase = 2 and lag(expphase,1) = 2 and block = 3 and taskch = lag(taskch,1) thbtaskchswi = 0.
if expphase = 2 and lag(expphase,1) = 2 and block = 3 and taskch ~= lag(taskch,1) thbtaskchswi = 1.
if expphase = 2 and lag(expphase,1) = 2 and block = 4 and taskch = lag(taskch,1) fobtaskchswi = 0.
if expphase = 2 and lag(expphase,1) = 2 and block = 4 and taskch ~= lag(taskch,1) fobtaskchswi = 1.
EXECUTE.


AGGREGATE
  /OUTFILE='C:\Users\mimburgi\Desktop\Possible_Dissert\Braem\data exp 16 1.1 '+
    'aggr.sav'
  /BREAK= subj group
  /corr_mean=MEAN(corr) 
  /corrcl_mean=MEAN(corrcl) 
  /taskchoice_mean=MEAN(taskchoice) 
  /taskchswi_mean=MEAN(taskchswi) 
  /rtcor_mean=MEAN(rtcor) 
  /rtcorr_mean=MEAN(rtcorr) 
  /forepcon_mean=MEAN(forepcon) 
  /forepincon_mean=MEAN(forepincon) 
  /foswicon_mean=MEAN(foswicon) 
  /foswiincon_mean=MEAN(foswiincon) 
  /frrepcon_mean=MEAN(frrepcon) 
  /frrepincon_mean=MEAN(frrepincon) 
  /frswicon_mean=MEAN(frswicon) 
  /frswiincon_mean=MEAN(frswiincon) 
  /err_mean=MEAN(err) 
  /errforepcon_mean=MEAN(errforepcon) 
  /errforepincon_mean=MEAN(errforepincon) 
  /errfoswicon_mean=MEAN(errfoswicon) 
  /errfoswiincon_mean=MEAN(errfoswiincon) 
  /errfrrepcon_mean=MEAN(errfrrepcon) 
  /errfrrepincon_mean=MEAN(errfrrepincon) 
  /errfrswicon_mean=MEAN(errfrswicon) 
  /errfrswiincon_mean=MEAN(errfrswiincon) 
  /fibtaskchswi_mean=MEAN(fibtaskchswi) 
  /sebtaskchswi_mean=MEAN(sebtaskchswi) 
  /thbtaskchswi_mean=MEAN(thbtaskchswi) 
  /fobtaskchswi_mean=MEAN(fobtaskchswi).


GET
  FILE='C:\Users\mimburgi\Desktop\Possible_Dissert\Braem\data exp 16 1.1 aggr.sav'.
DATASET NAME DataSet31 WINDOW=FRONT.


COMPUTE errfoconbyswi=((errfoswiincon_mean - errfoswicon_mean))  -  ((errforepincon_mean - errforepcon_mean)).
EXECUTE.

COMPUTE baltaskswimean = (fibtaskchswi_mean + sebtaskchswi_mean + thbtaskchswi_mean + fobtaskchswi_mean)/4.
EXECUTE.

if subj = 1  BIS = 23.
if subj = 2  BIS = 13.
if subj = 3  BIS = 24.
if subj = 4  BIS = 24.
if subj = 5  BIS = 20.
if subj = 6  BIS = 22.
if subj = 7  BIS = 20.
if subj = 8  BIS = 22.
if subj = 9  BIS = 20.
if subj = 10 BIS = 26.
if subj = 11 BIS = 25.
if subj = 12 BIS = 19.
if subj = 13 BIS = 21.
if subj = 14 BIS = 23.
if subj = 15 BIS = 25.
if subj = 16 BIS = 26.
if subj = 17 BIS = 22.
if subj = 18 BIS = 16.
if subj = 19 BIS = 26.
if subj = 20 BIS = 26.
if subj = 21 BIS = 16.
if subj = 22 BIS = 22.
if subj = 23 BIS = 20.
if subj = 24 BIS = 18.
if subj = 25 BIS = 25.
if subj = 26 BIS = 25.
if subj = 27 BIS = 28.
if subj = 28 BIS = 20.
if subj = 29 BIS = 24.
if subj = 30 BIS = 23.
if subj = 31 BIS = 16.
if subj = 32 BIS = 25.
if subj = 33 BIS = 14.
if subj = 34 BIS = 26.
if subj = 35 BIS = 19.
if subj = 36 BIS = 23.
if subj = 37 BIS = 25.
if subj = 38 BIS = 24.
if subj = 39 BIS = 25.
if subj = 40 BIS = 25.
if subj = 41 BIS = 21.
if subj = 42 BIS = 27.
if subj = 43 BIS = 25.
if subj = 44 BIS = 20.
if subj = 45 BIS = 16.
if subj = 46 BIS = 16.
if subj = 47 BIS = 16.
if subj = 48 BIS = 15.
if subj = 49 BIS = 16.
if subj = 1  BAS = 49.
if subj = 2  BAS = 42.
if subj = 3  BAS = 44.
if subj = 4  BAS = 43.
if subj = 5  BAS = 49.
if subj = 6  BAS = 38.
if subj = 7  BAS = 49.
if subj = 8  BAS = 41.
if subj = 9  BAS = 37.
if subj = 10 BAS = 37.
if subj = 11 BAS = 46.
if subj = 12 BAS = 37.
if subj = 13 BAS = 43.
if subj = 14 BAS = 40.
if subj = 15 BAS = 40.
if subj = 16 BAS = 48.
if subj = 17 BAS = 51.
if subj = 18 BAS = 46.
if subj = 19 BAS = 44.
if subj = 20 BAS = 37.
if subj = 21 BAS = 37.
if subj = 22 BAS = 42.
if subj = 23 BAS = 43.
if subj = 24 BAS = 41.
if subj = 25 BAS = 42.
if subj = 26 BAS = 40.
if subj = 27 BAS = 42.
if subj = 28 BAS = 42.
if subj = 29 BAS = 38.
if subj = 30 BAS = 38.
if subj = 31 BAS = 38.
if subj = 32 BAS = 44.
if subj = 33 BAS = 40.
if subj = 34 BAS = 46.
if subj = 35 BAS = 45.
if subj = 36 BAS = 44.
if subj = 37 BAS = 36.
if subj = 38 BAS = 40.
if subj = 39 BAS = 48.
if subj = 40 BAS = 38.
if subj = 41 BAS = 50.
if subj = 42 BAS = 32.
if subj = 43 BAS = 43.
if subj = 44 BAS = 45.
if subj = 45 BAS = 49.
if subj = 46 BAS = 40.
if subj = 47 BAS = 41.
if subj = 48 BAS = 43.
if subj = 49 BAS = 39.
if subj = 1  BASr = 19.
if subj = 2  BASr = 15.
if subj = 3  BASr = 18.
if subj = 4  BASr = 16.
if subj = 5  BASr = 18.
if subj = 6  BASr = 15.
if subj = 7  BASr = 18.
if subj = 8  BASr = 16.
if subj = 9  BASr = 13.
if subj = 10 BASr = 15.
if subj = 11 BASr = 17.
if subj = 12 BASr = 17.
if subj = 13 BASr = 17.
if subj = 14 BASr = 17.
if subj = 15 BASr = 16.
if subj = 16 BASr = 18.
if subj = 17 BASr = 19.
if subj = 18 BASr = 18.
if subj = 19 BASr = 16.
if subj = 20 BASr = 17.
if subj = 21 BASr = 16.
if subj = 22 BASr = 17.
if subj = 23 BASr = 17.
if subj = 24 BASr = 16.
if subj = 25 BASr = 17.
if subj = 26 BASr = 17.
if subj = 27 BASr = 18.
if subj = 28 BASr = 15.
if subj = 29 BASr = 16.
if subj = 30 BASr = 15.
if subj = 31 BASr = 16.
if subj = 32 BASr = 19.
if subj = 33 BASr = 14.
if subj = 34 BASr = 16.
if subj = 35 BASr = 16.
if subj = 36 BASr = 17.
if subj = 37 BASr = 15.
if subj = 38 BASr = 18.
if subj = 39 BASr = 19.
if subj = 40 BASr = 17.
if subj = 41 BASr = 20.
if subj = 42 BASr = 15.
if subj = 43 BASr = 17.
if subj = 44 BASr = 17.
if subj = 45 BASr = 18.
if subj = 46 BASr = 15.
if subj = 47 BASr = 15.
if subj = 48 BASr = 17.
if subj = 49 BASr = 16.
if subj = 1  BASd = 14.
if subj = 2  BASd = 13.
if subj = 3  BASd = 12.
if subj = 4  BASd = 12.
if subj = 5  BASd = 15.
if subj = 6  BASd = 13.
if subj = 7  BASd = 16.
if subj = 8  BASd = 12.
if subj = 9  BASd = 12.
if subj = 10 BASd = 11.
if subj = 11 BASd = 16.
if subj = 12 BASd = 10.
if subj = 13 BASd = 12.
if subj = 14 BASd = 12.
if subj = 15 BASd = 12.
if subj = 16 BASd = 16.
if subj = 17 BASd = 16.
if subj = 18 BASd = 14.
if subj = 19 BASd = 14.
if subj = 20 BASd = 9.
if subj = 21 BASd = 12.
if subj = 22 BASd = 15.
if subj = 23 BASd = 13.
if subj = 24 BASd = 15.
if subj = 25 BASd = 13.
if subj = 26 BASd = 13.
if subj = 27 BASd = 13.
if subj = 28 BASd = 14.
if subj = 29 BASd = 12.
if subj = 30 BASd = 13.
if subj = 31 BASd = 11.
if subj = 32 BASd = 12.
if subj = 33 BASd = 11.
if subj = 34 BASd = 15.
if subj = 35 BASd = 14.
if subj = 36 BASd = 12.
if subj = 37 BASd = 13.
if subj = 38 BASd = 10.
if subj = 39 BASd = 14.
if subj = 40 BASd = 12.
if subj = 41 BASd = 15.
if subj = 42 BASd = 10.
if subj = 43 BASd = 13.
if subj = 44 BASd = 15.
if subj = 45 BASd = 15.
if subj = 46 BASd = 11.
if subj = 47 BASd = 12.
if subj = 48 BASd = 11.
if subj = 49 BASd = 13.
if subj = 1  BASf = 16.
if subj = 2  BASf = 14.
if subj = 3  BASf = 14.
if subj = 4  BASf = 15.
if subj = 5  BASf = 16.
if subj = 6  BASf = 10.
if subj = 7  BASf = 15.
if subj = 8  BASf = 13.
if subj = 9  BASf = 12.
if subj = 10 BASf = 11.
if subj = 11 BASf = 13.
if subj = 12 BASf = 10.
if subj = 13 BASf = 14.
if subj = 14 BASf = 11.
if subj = 15 BASf = 12.
if subj = 16 BASf = 14.
if subj = 17 BASf = 16.
if subj = 18 BASf = 14.
if subj = 19 BASf = 14.
if subj = 20 BASf = 11.
if subj = 21 BASf = 9 .
if subj = 22 BASf = 10.
if subj = 23 BASf = 13.
if subj = 24 BASf = 10.
if subj = 25 BASf = 12.
if subj = 26 BASf = 10.
if subj = 27 BASf = 11.
if subj = 28 BASf = 13.
if subj = 29 BASf = 10.
if subj = 30 BASf = 10.
if subj = 31 BASf = 11.
if subj = 32 BASf = 13.
if subj = 33 BASf = 15.
if subj = 34 BASf = 15.
if subj = 35 BASf = 15.
if subj = 36 BASf = 15.
if subj = 37 BASf = 8 .
if subj = 38 BASf = 12.
if subj = 39 BASf = 15.
if subj = 40 BASf = 9 .
if subj = 41 BASf = 15.
if subj = 42 BASf = 7 .
if subj = 43 BASf = 13.
if subj = 44 BASf = 13.
if subj = 45 BASf = 16.
if subj = 46 BASf = 14.
if subj = 47 BASf = 14.
if subj = 48 BASf = 15.
if subj = 49 BASf = 10.
EXECUTE.


USE ALL.
COMPUTE filter_$=(corrcl_mean > .768 & taskchswi_mean > .1).
VARIABLE LABELS filter_$ 'subj ~= 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.



T-TEST GROUPS=group(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=BASr
  /CRITERIA=CI(.95).

T-TEST GROUPS=group(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=taskchoice_mean
  /CRITERIA=CI(.95).

T-TEST
  /TESTVAL=0.5
  /MISSING=ANALYSIS
  /VARIABLES=taskchoice_mean taskchswi_mean 
  /CRITERIA=CI(.95).




T-TEST GROUPS=group(0 1)
  /MISSING=ANALYSIS
  /VARIABLES= fibtaskchswi_mean sebtaskchswi_mean thbtaskchswi_mean fobtaskchswi_mean baltaskswimean
  /CRITERIA=CI(.95).


GLM fibtaskchswi_mean sebtaskchswi_mean thbtaskchswi_mean fobtaskchswi_mean BY group
  /WSFACTOR=block 4 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT = ETASQ
  /PLOT=PROFILE(block*group)
  /EMMEANS=TABLES(block*group)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=block 
  /DESIGN=group.




GLM frrepcon_mean frrepincon_mean frswicon_mean frswiincon_mean BY group
  /WSFACTOR=taskseq 2 Polynomial con 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(group*taskseq group*con taskseq*con*group)
  /EMMEANS=TABLES(taskseq)
  /EMMEANS=TABLES(con)
  /EMMEANS=TABLES(group*con)
  /EMMEANS=TABLES(group*taskseq*con)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=taskseq con taskseq*con
  /DESIGN=group.

GLM errfrrepcon_mean errfrrepincon_mean errfrswicon_mean errfrswiincon_mean BY group
  /WSFACTOR=taskseq 2 Polynomial con 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(group*taskseq group*con taskseq*con*group)
  /EMMEANS=TABLES(taskseq)
  /EMMEANS=TABLES(con)
  /EMMEANS=TABLES(group*taskseq*con)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=taskseq con taskseq*con
  /DESIGN=group.




GLM forepcon_mean forepincon_mean foswicon_mean foswiincon_mean BY group
  /WSFACTOR=taskseq 2 Polynomial con 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(group*taskseq group*con taskseq*con*group)
  /EMMEANS=TABLES(taskseq)
  /EMMEANS=TABLES(con)
  /EMMEANS=TABLES(group*taskseq*con)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=taskseq con taskseq*con
  /DESIGN=group.

GLM errforepcon_mean errforepincon_mean errfoswicon_mean errfoswiincon_mean BY group
  /WSFACTOR=taskseq 2 Polynomial con 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(group*taskseq group*con taskseq*con*group)
  /EMMEANS=TABLES(taskseq)
  /EMMEANS=TABLES(con)
  /EMMEANS=TABLES(group*taskseq*con)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=taskseq con taskseq*con
  /DESIGN=group.




SORT CASES  BY group.
SPLIT FILE SEPARATE BY group.


GLM frrepcon_mean frrepincon_mean frswicon_mean frswiincon_mean
  /WSFACTOR=taskseq 2 Polynomial con 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(con)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=taskseq con taskseq*con.

GLM errforepcon_mean errforepincon_mean errfoswicon_mean errfoswiincon_mean
  /WSFACTOR=taskseq 2 Polynomial con 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(taskseq*con)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=taskseq con taskseq*con.

CORRELATIONS
  /VARIABLES=  BASr errfoconbyswi  
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

T-TEST PAIRS=errforepcon_mean errfoswicon_mean WITH errforepincon_mean errfoswiincon_mean (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST
  /TESTVAL=0.5
  /MISSING=ANALYSIS
  /VARIABLES=taskchoice_mean baltaskswimean
  /CRITERIA=CI(.95).

SPLIT FILE OFF.



 
NPTESTS 
  /INDEPENDENT TEST (taskchswi_mean fibtaskchswi_mean sebtaskchswi_mean thbtaskchswi_mean fobtaskchswi_mean) GROUP (group) 
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE
  /CRITERIA ALPHA=0.05  CILEVEL=95.


