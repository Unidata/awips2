C MEMBER HCKTYP
C  (from old member HCLCHECK)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCKTYP (IFIELD,ITYPE,ISTAT)
C
C          ROUTINE:  HCKTYP
C
C             VERSION:  1.0.0
C
C                DATE:  8-7-81
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C   THIS ROUTINE WILL CHECK A FIELD FOR T=X OR TYPE=X AND SAVE
C    THE TYPE IN ITYPE
C
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IFIELD      I     I     1     THE FIELD NUMBER IN IBUF
C       ITYPE       I    O     1     THE TYPE FOUND 1=INT,2=REAL,
C                                        3=ALPHA, 4=LOGICAL,5=DATE
C       ISTAT       I    O     1     STATUS 0=OK, NOT 0=ERROR
C
C
C
C
C***********************************************************************
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hwords'
      DIMENSION LEFT(2),IRIGHT(2)
      DIMENSION LETRAY(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hcktyp.f,v $
     . $',                                                             '
     .$Id: hcktyp.f,v 1.1 1995/09/17 18:41:44 dws Exp $
     . $' /
C    ===================================================================
C
      DATA LETRAY/1HI,1HR,1HA,1HL,1HD/
      DATA LETT/4HT   /,LTYPE/4HTYPE/
C
C CHECK FIELD FOR T= OR TYPE=
C
      ISTAT=0
      CALL HFEQLS (IFIELD,LEFT,IRIGHT,ISTRGT)
      IF (LEFT(1).EQ.LETT.OR.LEFT(1).EQ.LTYPE) GO TO 30
10    WRITE (LP,20) IFIELD
20    FORMAT ('0**ERROR** INVALID TYPE SPECIFICATION IN FIELD',I3)
      ISTAT=1
      GO TO 70
C
C NOW DO TYPE
C
30    DO 40 I=1,5
      IF (IBUF(ISTRGT).EQ.LETRAY(I)) GO TO 60
40    CONTINUE
      WRITE (LP,50) LETRAY
50    FORMAT ('0**ERROR** TYPE MUST BE ',4(A1,', '),A1)
      GO TO 10
60    ITYPE=I
      IF (ITYPE.NE.3) GO TO 70
C
C DO MAX FOR A
C
      ISTRGT=ISTRGT+1
      IERR=0
      CALL UINTFX (MAX,ISTRGT,IFSTOP(IFIELD),IERR)
      IF (IERR.NE.0) GO TO 10
      ITYPE=-MAX
70    CONTINUE
      IF (IHCLDB.EQ.3) WRITE (IOGDB,80) IFIELD,ITYPE,ISTAT
80    FORMAT (' IN HCKTYP - IFIELD=',I2,' ITYPE=',I5,' ISTAT=',I2)
C
      RETURN
C
      END
