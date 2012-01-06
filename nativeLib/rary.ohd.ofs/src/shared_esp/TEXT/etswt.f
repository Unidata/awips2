C MEMBER ETSWT
C  (from old member EETSWT)
C
      SUBROUTINE ETSWT(TSESP,MTSESP,D,MD,KNTYR,kntmo,NYRS,ihzero,
     + ljdcon,IERR)
C
C   THIS SUBROUTINE IS THE DRIVER SUBROUTINE FOR WRITING OUTPUT
C   TIME SERIES.
C
C   THIS SUBROUTINE WAS ORIGINALLY WRITTEN BY GERALD N. DAY .
C
      LOGICAL LBUG
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/espseg'
C
      DIMENSION D(1),TSESP(1),SBNAME(2),OLDOPN(2),TSID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/etswt.f,v $
     . $',                                                             '
     .$Id: etswt.f,v 1.2 1997/06/25 11:29:14 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HETSW,4HT   /,ESP/4HESP /,DEBUG/4HWTOF/
      DATA DEBUG2/4HETSW/
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** ETSWT ENTERED)
C
      LBUG=.FALSE.
      IF(IFBUG(DEBUG2).EQ.0) GO TO 12
      LBUG=.TRUE.
   12 CONTINUE
C
      IBUG=IFBUG(DEBUG)
C
      IF(IBUG.EQ.1) GO TO 999
C
      IERR=0
C
C   SEARCH TS ARRAY FOR UPDATE AND OUTPUT TIME SERIES
C
      LOC=0
   15 ITYPE=TSESP(LOC+1)
      IF(ITYPE.EQ.0) GO TO 999
      NXLOC=TSESP(LOC+2)
      IF(ITYPE.NE.2.AND.ITYPE.NE.3) GO TO 200
C
C   FOUND AN UPDATE / OUTPUT TIME SERIES, GET GENERAL TS INFO
C
      TSID(1)=TSESP(LOC+3)
      TSID(2)=TSESP(LOC+4)
      DTYPE=TSESP(LOC+5)
      IDT=TSESP(LOC+6)
      NVPDT=TSESP(LOC+7)
      LD=TSESP(LOC+8)
      FILEID=TSESP(LOC+10)
      LEXT=LOC+13
      IF(ITYPE.NE.2) GO TO 20
C
C   UPDATE TS
C
      NV=TSESP(LOC+12)
      NADD=TSESP(LOC+13+NV)
      LESP=LOC+15+NV+NADD
      FILEID=TSESP(LESP)
      LEXT=LESP+2
   20 IF(FILEID.EQ.ESP) GO TO 100
      WRITE(IPR,600) FILEID
  600 FORMAT(1H0,10X,10H**ERROR** ,A4,2X,22HIS NOT A VALID OUTPUT ,
     1 9HFILE TYPE)
      IERR=1
      CALL ERROR
      GO TO 999
C
  100 CONTINUE
C
      IF(LBUG) WRITE(IODBUG,910) TSID,DTYPE,IDT
  910 FORMAT(1H0,10X,17HWRITE TIME SERIES,10X,2A4,2X,A4,I5)
C
      CALL EWTDFL(D,LD,TSESP(LEXT),TSID,DTYPE,IDT,NVPDT,
     1 KNTYR,kntmo,NYRS,ihzero,ljdcon,IERR)
      IF(IERR.NE.0) GO TO 999
C
  200 IF(NXLOC.GT.LTSESP) GO TO 999
      LOC=NXLOC-1
      GO TO 15
C
  999 CONTINUE
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
      RETURN
      END
