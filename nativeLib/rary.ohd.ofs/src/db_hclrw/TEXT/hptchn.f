C MEMBER HPTCHN
C  (from old member HCLCNGPR)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/04/95.09:39:21 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HPTCHN (IDELAR,IDPOS,IADDAR,IADD,IADPOS,IOLD,NUMCDS,
     1           INEW,MAXNEW,INPOS,ISTAT)
C***********************************************************************
C
C          ROUTINE:  HPTCHN
C
C             VERSION:  1.0.0
C
C                DATE:  1-13-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE COMBINES OLD,INSERTED,AND DELETED COMMANDS
C    FOR THE CHANGE PROC COMMAND
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IDELAR     I     I     ?    ARRAY CONTAINING DELETED LINE #S
C       IDPOS      I     I     1    POINTER TO LAST LINE IN IDELAR
C       IADDAR     I     I     ?    ARRAY HOLDING NEW COMMANDS
C       IADD       I     I     ?    ARRAY OF POINTERS FOR IADDAR
C       IADPOS     I     I     1    LAST WORD USED IN IADD
C       IOLD       I     I     ?    ARRAY OF OLD COMMANDS
C       NUMCDS     I    I/O    1    NUMBER OF COMMANDS
C       INEW       I     O     ?    RESTRUCTURED COMMANDS ARRAY
C       MAXNEW     I     I     1    NO. ELEMENTS IN INEW ARRAY
C       INPOS      I     O     1    NUMBER OF WORDS USED IN INEW
C       ISTAT      I     O     1    STATUS INDICATOR
C                                   0=OK  1=BUFFER FULL
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IDELAR(1),IADDAR(1),IADD(1),INEW(1),IOLD(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hptchn.f,v $
     . $',                                                             '
     .$Id: hptchn.f,v 1.1 1995/09/17 18:42:53 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IHCLDB.EQ.3) WRITE (IOGDB,10)
10    FORMAT (' HPTCHN ENTERED')
C
C        INITIALIZE
C
      ISTAT=0
      INPOS=1
      IOPOS=1
      IAPOS=1
      JJ=IADPOS/2
      KK=IDPOS
      J=1
      K=1
      I=1
      NADD=0
20    CONTINUE
C
C        IS IT AN INSERT
C
      IF (JJ.EQ.0) GO TO 70
      IF (IADD(J).NE.I-1) GO TO 70
C
C        DO AN INSERT
C
      NUM=IADD(J+1)
      DO 60 L=1,NUM
          IF (IHCLDB.EQ.3) WRITE (IOGDB,30)
30        FORMAT (' DOING AN INSERT')
          NWDS=IADDAR(IAPOS)+1
          IF (INPOS+NWDS.GT.MAXNEW) GO TO 150
          CALL UMEMOV (IADDAR(IAPOS),INEW(INPOS),NWDS)
          IAPOS=IAPOS+NWDS
          INPOS=INPOS+NWDS
          IF (IHCLDB.EQ.3) WRITE (IOGDB,40) (INEW(LL),LL=1,INPOS)
40        FORMAT (' INEW='/(1X,20I4))
          IF (IHCLDB.EQ.3) WRITE (IOGDB,50)
50        FORMAT (' INSERT DONE')
60        CONTINUE
      J=J+2
      JJ=JJ-1
      NADD=NADD+NUM
70    CONTINUE
C
C        IS IT A DELETE
C
      IF (KK.EQ.0) GO TO 110
      IF (IDELAR(K).NE.I) GO TO 110
C
C        DO A DELETE
C
      IF (IHCLDB.EQ.3) WRITE (IOGDB,80)
80    FORMAT (' DOING A DELET')
      IF (IHCLDB.EQ.3) WRITE (IOGDB,90) IOPOS,IOLD(IOPOS),IOGDB
90    FORMAT (' IOPOS,VALUE,IOGDB ',3(1X,I6))
      NWDS=IOLD(IOPOS)+1
      IOPOS=IOPOS+NWDS
      I=I+1
      K=K+1
      KK=KK-1
      IF (IHCLDB.EQ.3) WRITE (IOGDB,100)
100   FORMAT (' DELETE DONE')
      GO TO 20
110   CONTINUE
C
C        MOVE ORIGINAL CARD
C
      IF (I.GT.NUMCDS) GO TO 140
      IF (IHCLDB.EQ.3) WRITE (IOGDB,120)
120   FORMAT (' MOVING OLD COMMAND IN')
      NWDS=IOLD(IOPOS)+1
      IF (INPOS+NWDS.GT.MAXNEW) GO TO 150
      CALL UMEMOV (IOLD(IOPOS),INEW(INPOS),NWDS)
      IOPOS=IOPOS+NWDS
      INPOS=INPOS+NWDS
      I=I+1
      NADD=NADD+1
      IF (IHCLDB.EQ.3) WRITE (IOGDB,130)
130   FORMAT (' OLD MOVED')
      GO TO 20
140   CONTINUE
C
C       SEE IF FINISHED
C
      IF (JJ.NE.0.OR.KK.NE.0) GO TO 20
      INPOS=INPOS-1
      GO TO 170
150   CONTINUE
C
C       BUFFER FULL
C
      CALL ULINE (LP,2)
      WRITE (LP,160)
160   FORMAT ('0**ERROR** PROCEDURE BUFFER IS FULL.')
      ISTAT=1
170   CONTINUE
      IF (IHCLDB.EQ.3) WRITE (IOGDB,180) INPOS,NADD,(INEW(L),L=1,INPOS)
180   FORMAT (' INPOS=',I4,' NADD=',I3,' INEW='/(1X,20I4))
      NUMCDS=NADD
C
      RETURN
C
      END
