C MEMBER RCOMND
C (from old member UTIL2)
C******************************************************************
C
       SUBROUTINE RCOMND(LSTCMD,NUMCMD,INDXC)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  RCOMND                                         *
C                                                                      *
C  **VERSION 1.0.1 6-24-81 CHANGE MEANING OF INDXC AND REMOVE
C                           EXCLAMATION POINT
C  **VERSION 1.0.2 1-20-84 CHECK FOR $ PRINT COMMENT CARD.
C             VERSION:  1.0.0                                          *
C                                                                      *
C              AUTHOR:  SONJA R. SIEGEL                                *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    ROUTINE TO READ A CARD AND LOOK FOR A COMMAND                     *
C    THE LIST OF COMMANDS ARE IN LSTCMD
C    CARD IS READ INTO IBUF BUT NOT PRINTED ON LISTING                 *
C    INDXC=INDEX OF COMMAND, =0 FOR NO COMMAND                    *
C             OR -1 FOR EOF FOUND.
C    IBUF IS ANALYZED BY UFREE AND INFO IS IN UFREEI
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       LSTCMD    I      I  NUMCMD  LIST OF COMMANDS                   *
C       NUMCMD    I      I    1     NUMBER OF COMMANDS IN LIST         *
C       INDXC     I      O    1     SUBSCRIPT OF COMMAND IF FOUND      *
C                                    0 IF NOT IN LIST
C                                    -1 FOR EOF
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'udsi'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      DIMENSION LSTCMD(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/rcomnd.f,v $
     . $',                                                             '
     .$Id: rcomnd.f,v 1.1 1995/09/17 19:24:30 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C
C
C                                                                      *
C                                                                      *
C***********************************************************************
C
C
      IF(NOBUG.EQ.2) WRITE(LPD,2000) (LSTCMD(I),I=1,NUMCMD)
2000  FORMAT(' LOOKING FOR COMMANDS: ',10(2X,A4))
10    CONTINUE
      INDXC=-1
      CALL RPCARD(IBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
C
C SEARCH FOR COMMAND
C
      IBEG=1
      IEND=72
      CALL UFREE(IBEG,IEND)
C
C  SEE IF THIS CARD IS A COMMENT
C
      NUM = IBUF(IFSTRT(1))
      IF(NUM.NE.IDOLR) GO TO 15
      CALL WPCARD(IBUF)
      GO TO 10
   15 CONTINUE
      K=IFSTRT(1)
      N=IFSTOP(1)-K+1
      IF(N.GT.4) N=4
      CALL UPACK1(IBUF(K),ICMD,N)
      DO 20 I=1,NUMCMD
      IF(ICMD.EQ.LSTCMD(I)) GO TO 100
20    CONTINUE
C
C COMMAND NOT FOUND
C
C
50    INDXC=0
      GO TO 999
100   INDXC=I
999   IF(NOBUG.EQ.2) WRITE(LPD,2001) ISTAT,INDXC
2001  FORMAT(' ***SUBROUTINE RCOMND STATUS=',I4,' INDEX=',I4)
      RETURN
      END
