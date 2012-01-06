C MEMBER ESETA
C  (from old member EESETA)
C
      SUBROUTINE ESETA(NYRS,PESP,MPESP,MA)
C
C   THIS SUBROUTINE SETS UP THE ACCUMULATOR ARRAY .
C
C   THIS SUBROUTINE WAS WRITTEN BY GERALD N DAY.
C
      INCLUDE 'common/eswtch'
      INCLUDE 'common/esprun'
      INCLUDE 'common/where'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/eacc'
      INCLUDE 'common/etime'
      INCLUDE 'common/evar'
      INCLUDE 'common/espseg'
C
      DIMENSION SBNAME(2),OLDOPN(2),PESP(1),TSIND(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eseta.f,v $
     . $',                                                             '
     .$Id: eseta.f,v 1.1 1995/09/17 19:19:01 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HESET,4HA   /,OBS/4HOBS /,SIM/4HSIM /
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** ESETA ENTERED)
C
C   CALCULATE NUMBER OF PLACES NEEDED IN ACCUMULATOR ARRAY FOR
C   A SINGLE VALUED SIMULATED VARIABLE.
C
      LENSIM=NYRS*(1+JHSS+JASS)
C
C   CALCULATE NUMBER OF PLACES NEEDED IN ACCUMULATOR ARRAY FOR
C   A SINGLE VALUED OBSERVED TS
C
      LENOBS=NYRS
      IF(JBPS.EQ.0) GO TO 80
      IOBSDF=IHYR-IBHYR
      IF(IOBSDF.LE.0) GO TO 60
      LENOBS=LENOBS+IOBSDF
   60 IF(LBHYR.LE.LHYR) GO TO 80
      LENOBS=LENOBS+(LBHYR-LHYR)
   80 CONTINUE
C
C   CALCULATE THE BEGINNING OF EACH VARIABLE IN THE ACCUMULATOR
C   ARRAY
C
      LP=0
      LA=1
   85 NVAR=PESP(LP+1)
      IF(NVAR.EQ.0) GO TO 999
      NEXVAR=PESP(LP+2)
      TSIND(1)=PESP(LP+17)
      TSIND(2)=PESP(LP+26)
      NVAL=NVPV(NVAR)
      LEN=0
      DO 100 I=1,2
      IF(TSIND(I).NE.SIM) GO TO 90
      LEN=LEN+LENSIM
      GO TO 100
   90 IF(TSIND(I).NE.OBS) GO TO 100
      LEN=LEN+LENOBS
  100 CONTINUE
      LEN=LEN*NVAL
      NWIND=0
      DO 150 J=1,NUMWIN
      IF(JVAR(J,NVAR).NE.0) GO TO 150
      NWIND=NWIND+1
  150 CONTINUE
      LEN=LEN*NWIND
      LENA=LA+LEN-1
      IF(LENA.LE.MA) GO TO 200
      WRITE(IPR,600) MA,LENA
  600 FORMAT(1H0,10X,45H**ERROR** THE ACCUMULATOR ARRAY IS NOT LARGE ,
     1 13HENOUGH, MA = ,I5,2X,6HLENA =,I5)
      LENA=LA-1
      CALL ERROR
      GO TO 999
C
  200 PESP(LP+7)=LA+.01
      LA=LA+LEN
      IF(NEXVAR.GT.LPESP) GO TO 999
      LP=NEXVAR-1
      GO TO 85
C
  999 CONTINUE
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
      RETURN
      END
