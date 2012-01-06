C MEMBER ESPEX
C  (from old member EESPEX)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/22/94.09:41:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
C   THIS IS THE DRIVING PROGRAM FOR THE ESP EXECUTION PROGRAM.
C
C   THIS SUBROUTINE WAS WRITTEN BY GERALD N. DAY .
C
      SUBROUTINE ESPEX
      INCLUDE 'common/fc'
      INCLUDE 'common/fd'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/ep'
      INCLUDE 'common/esp'
      INCLUDE 'common/ets'
      INCLUDE 'common/ea'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/killcd'
      INCLUDE 'common/fctime'
      INCLUDE 'common/etime'
      INCLUDE 'common/fnopr'
      DIMENSION SBNAME(2),OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/espex.f,v $
     . $',                                                             '
     .$Id: espex.f,v 1.1 1995/09/17 19:19:06 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SBNAME/4HESPE,4HX   /,DEBUG/4HPRON/,DEBUG2/4HEMAX/
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H ,16H** ESPEX ENTERED)
C
      IBUG=IFBUG(DEBUG)
      JBUG=IFBUG(DEBUG2)
C
      IF(IBUG.EQ.1) NOPROT=0
C
      IF(JBUG.EQ.1) WRITE(IODBUG,910) MC,MD,MP,MT,MPESP,MSPESP,MTSESP,
     1 MA
  910 FORMAT(10X,34HMC,MD,MP,MT,MPESP,MSPESP,MTSESP,MA,5X,8I6)
C
C   READ FORECAST AND ESP FILES TO FILL COMMON BLOCKS
C
      CALL EFAZE0
C
      FLOCAL=LOCAL+.01
      LOCAL=0
C
C   SET UP VALUES FOR RUN - ORDER OF SEGMENTS AND RUN
C   DATES.
C
      CALL EFAZE1(MD,D)
C
C   EXECUTE SEGMENTS ONE AT A TIME
C
      CALL EFAZE2(MC,C,MD,D,MP,P,MT,T,MTSESP,TSESP,MPESP,PESP,
     1 MSPESP,SPESP,MA,A)
C
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
C
      END
