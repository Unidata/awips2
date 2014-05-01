C MODULE HPPRCG
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT PROCEDURE DEFINITIONS.
C
      SUBROUTINE HPPRCG (KBUF)
C
C  ARGUMENT LIST:
C
C     NAME     TYPE   I/O   DIM   DESCRIPTION
C     ------   ----   ---   ---   ------------
C     KBUF       I     I     ?    ARRAY CONTAINING PROCEDURE RECORD
C
      CHARACTER*4 PROCPASS
      CHARACTER*8 RTNNAM,RTNOLD
      CHARACTER*8 PROCNAME,PROCTYPE,DFLTTYPE
      CHARACTER*8 PARMNAME
      CHARACTER*88 IDBUF2
C
      DIMENSION KBUF(32)
      PARAMETER (LIDBUFL=2000,LIDBUF=2000)
      DIMENSION IDBUFL(LIDBUFL),IDBUF(LIDBUF)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'hclcommon/hword2'
      INCLUDE 'hclcommon/hunits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hpprcg.f,v $
     . $',                                                             '
     .$Id: hpprcg.f,v 1.5 2002/02/11 20:30:23 dws Exp $
     . $' /
C    ===================================================================
C
C   
      RTNNAM='HPPRCG'
C
      IF (IHCLTR.GT.1) WRITE (IOGDB,*) 'ENTER ',RTNNAM
C
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,RTNOLD,IOLDOP)
C
      LIDBUF2=LEN(IDBUF2)/4
      LDF=0
C
C  GET PROCEDURE NAME
      CALL UMEMOV (KBUF(4),PROCNAME,2)
C
C  GET PROCEDURE TYPE
      LGFLAG=KBUF(3)
      CALL UMEMOV (LLOCAL,PROCTYPE,2)
      IF (LGFLAG.LT.0) CALL UMEMOV (LGLOBL,PROCTYPE,2)
C
C  GET TECHNIQUE PASSWORD
      CALL UMEMOV (KBUF(6),PROCPASS,1)
      IF (KBUF(6).EQ.IBLNK) CALL UMEMOV (LNONE,PROCPASS,1)
C
C  PRINT THE FIRST LINES
      CALL ULINE (LP,2)
      WRITE (LP,90)
      CALL ULINE (LP,2)
      WRITE (LP,100) PROCNAME,PROCTYPE,PROCPASS
C
C  CHECK IF ANY PARAMETERS
      NUM=KBUF(9)
      IF (NUM.EQ.0) GO TO 40
C
C  GET THE DEFAULT
      IREC=KBUF(7)
      IUNIT=KDEFNL
      IF (LGFLAG.GT.0) GO TO 10
         IREC=KBUF(8)
         IUNIT=KDEFNG
C
10    IF (IREC.LE.0) GO TO 60
      CALL HGTRDN (IUNIT,IREC,IDBUF,LIDBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 60
      IF (LGFLAG.EQ.1.OR.KBUF(1).LT.0) GO TO 20
      IREC=KBUF(7)
      IF (IREC.LE.0) GO TO 60
      CALL HGTRDN (KLDFGD,IREC,IDBUFL,LIDBUFL,ISTAT)
      IF (ISTAT.NE.0) GO TO 60
      IF (IDBUFL(1).EQ.0) GO TO 20
      CALL UNAMCP (IDBUFL(4),NAME,IMATCH)
      IF (IMATCH.NE.0) GO TO 60
      LDF=1
C
20    CALL ULINE (LP,3)
      WRITE (LP,110)
C
C  PRINT THE DEFAULTS
      KPOS=10
      IPOS=8
      DO 30 I=1,NUM
          CALL UMEMOV (KBUF(KPOS),PARMNAME,2)
          KPOS=KPOS+2
          CALL HPPLDF (LDF,LGFLAG,IDBUF(IPOS),IDBUFL,DFLTTYPE)
          CALL HGTSTR (LIDBUF2,IDBUF(IPOS+2),IDBUF2,NIDBUF2,ISTAT)
          IF (ISTAT.NE.0) GO TO 60
          NWDS=IDBUF(IPOS+1)
          IPOS=IPOS+NWDS+2
          CALL ULINE (LP,1)
          WRITE (LP,120) PARMNAME,DFLTTYPE,IDBUF2(1:NIDBUF2)
30        CONTINUE
C
C  PRINT THE COMMANDS
40    NUM=KBUF(KPOS)
      IF (NUM.EQ.0) GO TO 70
C
      CALL ULINE (LP,3)
      WRITE (LP,130)
C
C  PRINT THE COMMANDS
      KPOS=KPOS+2
      DO 50 I=1,NUM
          NWDS=KBUF(KPOS-1)
          CALL HGTSTR (LIDBUF2,KBUF(KPOS),IDBUF2,NIDBUF2,ISTAT)
          IF (ISTAT.NE.0) GO TO 60
          KPOS=KPOS+NWDS+1
          CALL ULINE (LP,1)
          WRITE (LP,140) I,IDBUF2(1:NIDBUF2)
50        CONTINUE
      GO TO 70
C
60    CALL ULINE (LP,2)
      WRITE (LP,150)
      CALL ERROR      
C
      IF (IHCLDB.GT.1) THEN
         WRITE (IOGDB,160) KBUF
         WRITE (IOGDB,170) IDBUF
         ENDIF
C
70    CALL FSTWHR (RTNOLD,IOLDOP,RTNOLD,IOLDOP)
C
      IF (IHCLTR.GT.1) WRITE (IOGDB,*) 'EXIT ',RTNNAM
C
      RETURN
C
C-----------------------------------------------------------------------
C
90    FORMAT ('0',105('-'))
100   FORMAT ('0',
     *   'PROCEDURE = ',A,3X,
     *   'TYPE = ',A,3X,
     *   'PASSWORD = ',A)
110   FORMAT ('0',15X,'PARAMETER',5X,'DEFAULT TYPE',5X,'DEFAULT' /
     *   16X,9('-'),5X,12('-'),5X,7('-'))
120   FORMAT (16X,A,1X,5X,A,9X,A)
130   FORMAT ('0',15X,'LINE NUMBER  COMMANDS' /
     *   16X,11('-'),2X,8('-'))
140   FORMAT (19X,I3,7X,A)
150   FORMAT ('0**ERROR** SYSTEM ERROR ENCOUNTERED IN ROUTINE HPPRCG.')
160   FORMAT (1X,3I4,3A4,3I6 / (1X,20I5))
170   FORMAT (1X,3I4,2A4,2I5 / (1X,20I5))
C
      END
