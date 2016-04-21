C MEMBER EFNDSG
C  (from old member EEFNDSG)
C
      SUBROUTINE EFNDSG(IDSEG,ISGREC,IER)
C
C   THIS SUBROUTINE CALLS FGETSG TO READ THE SEGMENT STATUS FILE IN
C   ORDER TO FILL CB/FCSEGN/ WITH A VALUE FOR IEREC.
C
C   THIS SUBROUTINE WAS WRITTEN BY GERALD N DAY.
C
      INCLUDE 'common/espseg'
      INCLUDE 'common/espfle'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
C
      DIMENSION SBNAME(2),OLDOPN(2),IDSEG(2),DUM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/efndsg.f,v $
     . $',                                                             '
     .$Id: efndsg.f,v 1.1 1995/09/17 19:18:39 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HEFND,4HSG  /
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H ,17H** EFNDSG ENTERED)
C
      IER=0
C
C   FIND THE LOCATION OF THE SEGMENT ON FILE FCSEGSTS
C
      CALL FLOCSG(IDSEG,IRSEG)
C
C   FILL COMMON BLOCK FCSEGN FOR SEGMENT
C
      CALL FGETSG(DUM,IRSEG,MP,P,MT,T,MTS,TS,1,0,IERR)
      IF(IERR.EQ.0) GO TO 100
      IER=1
      GO TO 999
C
  100 ISGREC=IEREC
      IF(IEREC.EQ.0) IER=1
C
  999 CONTINUE
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
      RETURN
      END
