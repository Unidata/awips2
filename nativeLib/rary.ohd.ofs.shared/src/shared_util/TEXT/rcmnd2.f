C MEMBER RCMND2
C (from old member UTIL6)
C-----------------------------------------------------------------------
C
      SUBROUTINE RCMND2 (LSTCMD,NUMCMD,INDXC)
C
C  9/9/88 SRS ADD RCMND3 TO MEMBER
C          SUBROUTINE:  RCMND2
C
C             VERSION:  1.0.0
C
C              AUTHOR:  JONATHAN D. GERSHAN
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO READ A CARD AND LOOK FOR AN 8-LETTER COMMAND
C    THE LIST OF COMMANDS ARE IN LSTCMD
C    CARD IS READ INTO IBUF BUT NOT PRINTED ON LISTING
C    INDXC=INDEX OF COMMAND, =0 FOR NO COMMAND
C             OR -1 FOR EOF FOUND.
C    IBUF IS ANALYZED BY UFREE AND INFO IS IN UFREEI
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       LSTCMD    I      I  NUMCMD  LIST OF COMMANDS
C       NUMCMD    I      I    1     NUMBER OF COMMANDS IN LIST
C       INDXC     I      O    1     SUBSCRIPT OF COMMAND IF FOUND
C                                    0 IF NOT IN LIST
C                                     -1 IF EOF
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hdatas'
      INCLUDE 'ufreei'
      INCLUDE 'udsi'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION LSTCMD(2,6),ICMD(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/rcmnd2.f,v $
     . $',                                                             '
     .$Id: rcmnd2.f,v 1.1 1995/09/17 19:24:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C
C
C
C***********************************************************************
C
C
       IF (NOBUG.EQ.1) WRITE(LPD,1999)
 1999  FORMAT (' *** ENTER RCMND2')
C
      INDXC=-1
C
C  READ CARD
      CALL RPCARD (IBUF,ISTAT)
C
C  CHECK FOR END OF FILE
      IF (ISTAT.NE.1) GO TO 5
         INDXC=-1
         GO TO 999
C
C  PRINT CARD
5     CALL WPCARD (IBUF)
      IF (ISTAT.NE.0) GO TO 999
C
C  PARSE CARD
      IBEG=1
      IEND=72
      CALL UFREE(IBEG,IEND)
      K=IFSTRT(1)
      N=IFSTOP(1)-K+1
      IF (N.GT.8) N=8
C
      CALL UMEMST (IBLNK,ICMD,2)
      CALL UPACK1 (IBUF(K),ICMD(1),N)
C
      DO 20 I=1,NUMCMD
         IF (ICMD(1).EQ.LSTCMD(1,I).AND.ICMD(2).EQ.LSTCMD(2,I))
     1      GO TO 100
20       CONTINUE
C
C  MATCH NOT FOUND
50    INDXC=0
      GO TO 999
C
C  MATCH FOUND
100    INDXC=I
C
999   IF (NOBUG.GT.0) WRITE(LPD,2001) ISTAT,INDXC
2001  FORMAT (' *** EXIT RCMND2 - STATUS=',I4,' INDEX=',I4)
C
      RETURN
C
      END
