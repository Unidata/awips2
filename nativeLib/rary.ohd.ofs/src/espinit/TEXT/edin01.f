C MEMBER EDIN01
C  (from old member EEDIN01)
C
      SUBROUTINE EDIN01(DSP,LEFTD,NVAR,IERR)
C ................................................................
C
C THIS IS THE INPUT SUBROUTINE FOR DISPLAY 1
C NO INFO IS REALLY INPUT AT CURRENT TIME EXCEPT PUT DISPLAY # (1)
C IN DSP(1) AND # OF PIECES OF INFO(2) IN DSP(2).
C
C ORIGINALLY BY ED VANBLARGAN - HRL - JUNE,1981
C .................................................................
C
      DIMENSION DSP(1),SBNAME(2),OLDOPN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/edin01.f,v $
     . $',                                                             '
     .$Id: edin01.f,v 1.1 1995/09/17 18:46:19 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HEDIN,4H01  /
C
C SET ERROR TRACES IN CB/WHERE/
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
10    OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,100)
100   FORMAT(1H0,14HEDIN01 ENTERED)
C
C
      IERR=0
      IF(LEFTD.LT.2) GO TO 999
      DSP(1)=1.01
      DSP(2)=2.01
      GO TO 1000
  999 WRITE(IPR,600)
  600 FORMAT(1H0,10X,43H**ERROR** DISPLAY IGNORED. NOT ENOUGH SPACE,
     1 45H IN THE ESP PARAMETER ARRAY FOR DISPLAY INFO.)
      IERR=1
      CALL ERROR
C
C RESET ERROR TRACES IN CB/WHERE/
C
1000  IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
