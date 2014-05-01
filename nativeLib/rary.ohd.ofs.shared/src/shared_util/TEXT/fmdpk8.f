C MEMBER FMDPK8
C  (from old member FCFMDPK)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/09/94.15:36:56 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE FMDPK8(BUF,IS,LEN,CMND)
      LOGICAL*1 BUF(1),CMND(1),BLNK
      DIMENSION OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fmdpk8.f,v $
     . $',                                                             '
     .$Id: fmdpk8.f,v 1.1 1995/09/17 19:23:46 dws Exp $
     . $' /
C    ===================================================================
C
      DATA BLNK/1H /
C
      CALL FSTWHR('FMDPK8  ',0,OLDOPN,IOLDOP)
      IPK=8
C
      DO 10 I=1,IPK
   10 CMND(I)=BLNK
C
      IF(LEN.LT.1)GO TO 999
C
      ISTRT=(IS-1)*4+1
      LENGTH=LEN
      IF(LEN.GT.IPK)LENGTH=IPK
C
      DO 20 I=1,LENGTH
   20 CMND(I)=BUF(ISTRT+(I-1)*4)
  999 CALL FSTWHR(OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
      END
