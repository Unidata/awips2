C MEMBER HCKCNG
C  (from old member HCLCNGPR)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/04/95.09:39:21 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCKCNG (NAME,ITYPE,NUMCMD,ICMDAR,ISTAT)
C***********************************************************************
C
C          ROUTINE:  HCKCNG
C
C             VERSION:  1.0.0
C
C                DATE:  3-19-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE SETS UP AND WRITES THE LDR RECORD FOR THE CHANGE
C    PROC COMMAND. THE BLANK SUPRESSED COMMAND STRINGS ARE EXPANDED
C    AND CHECKED FOR GLOBAL REFERENCES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NAME       A8    I     2    NAME OF PROC
C       ITYPE      I     I     1    TYPE OF RECORD
C       NUMCMD     I     I     1    NUMBER OF COMMAND STRINGS
C       ICMDAR     I     I     ?    ARRAY CONTAINING COMMANDS
C       ISTAT      I     O     1    STATUS INDICATOR
C                                   0=OK
C                                   1=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER LDRREC(50),ICMDAR(1),TBUF(20),NAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hckcng.f,v $
     . $',                                                             '
     .$Id: hckcng.f,v 1.1 1995/09/17 18:41:39 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA MAXJ/50/,MAXT/20/
C
C***********************************************************************
C
C
      J=1
      IPOS=1
      ISTAT=0
C
C        LOOP THROUGH COMPACTED STRINGS FINDING GLOBAL REFERENCES
C
      DO 30 I=1,NUMCMD
         NWDS=ICMDAR(IPOS)
         CALL HGTSTR(MAXT,ICMDAR(IPOS+1),TBUF,LENGTH,ISTAT)
         IF (ISTAT.NE.0) GO TO 40
C GET EXPANDED STRING INTO IBUF IN 80A1 AND PARSE
         NUM=(LENGTH+3)/4
         CALL UNPAKS(TBUF,IBUF,NUM,80,ISTAT)
         IF (ISTAT.NE.0) GO TO 40
         CALL UFREE(1,80)
C SET UP LDR ARRAY
         CALL HFEXCP(1,LDRREC(J),1,ISTAT)
         IF (ISTAT.NE.0) GO TO 40
         IF (LDRREC(J).NE.0) J=J+1
         IF (J.LE.MAXJ) GO TO 20
         CALL ULINE (LP,2)
         WRITE (LP,10)
10    FORMAT ('0**ERROR** LOCAL DEFINITION REFERENCE BUFFER IS FULL.')
         ISTAT=1
         GO TO 60
20       CONTINUE
         IPOS=IPOS+NWDS+1
30       CONTINUE
C
C     WRITE THE LDR RECORD
C
      J=J-1
      CALL HDWLDR(NAME,ITYPE,LDRREC,J,ISTAT)
      IF (ISTAT.EQ.0) GO TO 60
      ISTAT=1
      GO TO 60
40    CONTINUE
C
C        SYSTEM ERROR
C
      CALL ULINE (LP,2)
      WRITE (LP,50)
50    FORMAT ('0**ERROR** SYSTEM ERROR IN HCKCNG')
      ISTAT=1
60    CONTINUE
      IF (ISTAT.NE.0) CALL ULINE (LP,2)
      IF (ISTAT.NE.0) WRITE (LP,70)
70    FORMAT ('0**ERROR** LOCAL DEFINITION REFERENCE RECORD NOT ',
     *   'WRITTEN.')
C
      RETURN
C
      END
