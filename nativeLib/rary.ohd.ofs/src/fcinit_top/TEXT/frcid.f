C MEMBER FRCID
C  (from old member FCFRCID)
C***********************************************************************
C
      SUBROUTINE FRCID(NF,IFOUND)
C
C SUBROUTINE TO SIMPLY CHECK IF A FIELD IS 'ID' (OR I OR IDENTIFIER)
C OR 'END'.  THIS IS PART OF DEFRC.
C AUTHOR - ED VANBLARGAN - 6/83
C
C ARGUMENT LIST:
C NF - INPUT - FIELD NUMBER OF INTEREST
C IFOUND - OUTPUT - =0 FIELD IS NOT 'ID' OR 'END'
C                   =1 FIELD IS 'ID'
C                   =2 FIELD IS 'END'
C
      DIMENSION IWORK(13)
      INCLUDE 'ufreei'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/frcid.f,v $
     . $',                                                             '
     .$Id: frcid.f,v 1.1 1995/09/17 18:55:02 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IWORK/1HI,1HD,1HE,1HN,1HT,1HI,1HF,1HI,1HE,1HR,
     $ 1HE,1HN,1HD/
C
C INITIALIZE
      IFOUND=0
      INIT=IFSTRT(NF)
      IFIN=IFSTOP(NF)
C CHECK FOR ID
      N=0
      DO 200 I=INIT,IFIN
      N=N+1
      IF (IBUF(I).EQ.IWORK(N)) GO TO 200
      GO TO 300
200   CONTINUE
      IFOUND=1
      GO TO 999
C CHECK FOR END
300   N=10
      IF (IFIN-INIT .NE. 2) GO TO 999
      DO 400 I=INIT,IFIN
      N=N+1
      IF (IBUF(I).EQ.IWORK(N)) GO TO 400
      GO TO 999
400   CONTINUE
      IFOUND=2
C
999   RETURN
      END
