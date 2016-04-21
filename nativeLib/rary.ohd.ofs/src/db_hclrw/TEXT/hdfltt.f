C MODULE HDFLTT
C-----------------------------------------------------------------------
C
      SUBROUTINE HDFLTT (IGL,IF,NAME,IPASS,NNCARD)
C
C          ROUTINE:  HDFLTT
C          VERSION:  1.0.0
C             DATE:  4-12-82
C           AUTHOR:  JIM ERLANDSON
C                    DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE SETS DEFAULTS FOR TECHNIQUE DEFINITIONS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL        I     I     1    TYPE INDICATOR
C                                     1=SET LOCAL DEFAULTS
C                                    -1=SET GLOBAL DEFAULTS
C       IF         I    I/O    1    FIELD POSITION
C       NAME       A8    I     1    NAME OF TECHNIQUE
C       IPASS      A4    I     1    SUPPLIED PASSWORD
C       NNCARD     I     I     1    LAST INPUT CARD READ
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hcomnd'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C      
      PARAMETER (LMAX=2000)
      INTEGER TBUF(LMAX),DBUF(LMAX)
      PARAMETER (LANS=20)
      INTEGER ANS(LANS)
      INTEGER NAME(2),ANAME(2),RIGHT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdfltt.f,v $
     . $',                                                             '
     .$Id: hdfltt.f,v 1.2 1998/04/07 12:54:27 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LQUOTE/4H4H   /
C
C***********************************************************************
C
C
      IF (IHCLTR.GT.1) WRITE (IOGDB,5)
5     FORMAT (' ENTER HDFLTT')
C
      LDFFLG=0
      IERR=0
C
C  GET THE TECHNIQUE RECORD
      ITYPE=3
      CALL HGTRCD (ITYPE,NAME,LMAX,TBUF,ISTAT)
      IF (ISTAT.EQ.0) GO TO 10
         IERR=1
         GO TO 290
C
C  CHECK THE PASSWORD
10    IF (ITYPE.LT.0.AND.IGL.EQ.1) GO TO 30
      IF (IPASS.EQ.TBUF(6)) GO TO 30
         CALL ULINE (LP,2)
         WRITE (LP,20) NAME
20    FORMAT ('0**ERROR** INVALID PASSWORD FOR TECHNIQUE ',2A4,'.')
         IERR=1
C
C  CHECK THE TYPE
30    IF (IGL.NE.-1.OR.ITYPE.NE.3) GO TO 50
         CALL ULINE (LP,2)
         WRITE (LP,40) NAME
40    FORMAT ('0**ERROR** TECHNIQUE ',2A4,' IS A LOCAL TECHNIQUE - ',
     *   '@SETGDFLT IS NOT VALID.')
         IERR=1
         GO TO 290
C
C  GET THE DEFAULT RECORD
50    IF (TBUF(10).EQ.0) GO TO 370
      IDFREC=TBUF(8)
      IDFUNT=KDEFNL
      IF (ITYPE.LT.0) IDFUNT=KLDFGD
      IF (IGL.GT.0) GO TO 60
         IDFREC=TBUF(9)
         IDFUNT=KDEFNG
60    IF (IDFUNT.EQ.KLDFGD) LDFFLG=1
      CALL HGTRDN (IDFUNT,IDFREC,DBUF,LMAX,ISTAT)
      IF (ISTAT.NE.0) GO TO 390
      IF (DBUF(1).NE.0) GO TO 70
C
C  NO PREVIOUS LOCAL DEFAULT FOR GLOBAL
      CALL UMEMST (0,DBUF,LMAX)
      DBUF(2)=1
      DBUF(3)=5
      CALL UMEMOV (TBUF(4),DBUF(4),2)
      DBUF(6)=-3
C
C  READ ANOTHER CARD
70    IPOINT=7
      IF (IF.LE.NFIELD) GO TO 90
80       NNCARD=NNCARD+1
         IF (NNCARD.GT.NCARD) GO TO 290
         CALL HCARDR (NNCARD,ISTAT)
         IF (ISTAT.NE.0) GO TO 390
         IF=1
90    IF (IF.GT.NFIELD) GO TO 80
C
C  GET ARGUMENT NAME AND FIND IT IN TECHNIQUE RECORD
      IPOINT=IPOINT+4
      CALL HFEQLS (IF,ANAME,RIGHT,ISTRT)
      IF (ISTRT.EQ.IFSTRT(IF)) GO TO 120
      IPOINT=11
      NUM=TBUF(10)
      DO 100 I=1,NUM
         CALL UNAMCP (TBUF(IPOINT),ANAME,ISTAT)
         IF (ISTAT.EQ.0) GO TO 120
         IPOINT=IPOINT+4
100      CONTINUE
C
C  ARGUMENT NAME NOT FOUND
      CALL ULINE (LP,2)
      WRITE (LP,110) ANAME,NAME
110   FORMAT ('0**ERROR** ARGUMENT ',2A4,' NOT FOUND IN TECHNIQUE ',2A4,
     *   '.')
      IERR=1
      IF=IF+1
      GO TO 90
C
C  GET ARGUMENT VALUE
120   CALL HCKARG (ISTRT,IFSTOP(IF),ITYPEA,ANS,LANS,NANS,ISTAT)
      IF (ISTAT.EQ.0) GO TO 130
         IERR=1
         IF=IF+1
         GO TO 90
C
C  CHECK ARGUMENT TYPE
130   IF (ITYPEA.EQ.TBUF(IPOINT+2)) GO TO 190
      IF (ITYPEA.EQ.3.AND.TBUF(IPOINT+2).LT.0) GO TO 190
      IF (ITYPEA.NE.3.AND.TBUF(IPOINT+2).LT.0) GO TO 160
      IF (ITYPEA.EQ.1.AND.TBUF(IPOINT+2).EQ.5) GO TO 180
140   CALL ULINE (LP,2)
      WRITE (LP,150) IF
150   FORMAT ('0**ERROR** ARGUMENT IN FIELD ',I2,' IS WRONG TYPE.')
      IERR=1
      IF=IF+1
      GO TO 90
C
C  FIX STRING VALUE
160   IF (IBUF(ISTRT).NE.LQUOTE) GO TO 170
         ISTRT=ISTRT+1
170   NANS=IFSTOP(IF)-ISTRT+1
      CALL UMEMST (IBLNK,ANS,LANS)
      CALL HPTSTR (IBUF(ISTRT),NANS,ANS,ISTAT)
      ITYPEA=3
      IF (ISTAT.EQ.0) GO TO 190
         IERR=1
          IF=IF+1
        GO TO 90
C
C  FIX DATE VALUE
180   CALL HCKDAT (ISTRT,IFSTOP(IF),ANS,ISTAT)
      NANS=7
      IF (ISTAT.NE.0) GO TO 140
C
C FIND WHERE TO PUT THE VALUE
190   IPOS=8
      NUM=(IPOINT-7)/4-1
      IF (NUM.EQ.0) GO TO 210
      IF (NUM.LT.TBUF(10)) GO TO 210
         CALL ULINE (LP,2)
         WRITE (LP,200) IF
200   FORMAT ('0**ERROR** BAD POSITIONAL ASSIGNMENT FOR FIELD ',I2,'.')
         IERR=1
         IF=IF+1
         GO TO 90
C
C  FIND POSITION FOR DEFAULT
210   IF (LDFFLG.EQ.1) GO TO 230
      IF (NUM.EQ.0) GO TO 260
      DO 220 I=1,NUM
         IPOS=IPOS+DBUF(IPOS+1)+2
220      CONTINUE
      GO TO 260
C
C  USING LOCAL DEFAULT FOR GLOBAL DEFINITION
230   ITNUM=TBUF(IPOINT+3)
      NUMDEF=DBUF(7)
      IF (NUMDEF.EQ.0) GO TO 250
      DO 240 I=1,NUMDEF
         IF (ITNUM.EQ.DBUF(IPOS)) GO TO 260
         IPOS=IPOS+DBUF(IPOS+1)+2
240      CONTINUE
C
C  COULD NOT FIND IT
250   DBUF(7)=DBUF(7)+1
      DBUF(IPOS)=ITNUM
      DBUF(IPOS+1)=NANS
      IF (TBUF(IPOINT+2).LT.0) DBUF(IPOS+1)=(-TBUF(IPOINT+2)+8)/4
C
C  CHECK THE SIZE
260   IF (DBUF(IPOS+1).GE.NANS) GO TO 280
         CALL ULINE (LP,2)
         WRITE (LP,270) IF
270   FORMAT ('0**ERROR** STRING IN FIELD ',I2,' IS TOO LARGE FOR ',
     *   'RESERVED SPACE.')
         IERR=1
         IF=IF+1
         GO TO 90
C
C  MOVE THE DEFAULT IN
280   CALL UMEMST (IBLNK,DBUF(IPOS+2),DBUF(IPOS+1))
      CALL UMEMOV (ANS,DBUF(IPOS+2),NANS)
      IF=IF+1
      GO TO 90
C
290   IF (IERR.EQ.0) GO TO 310
         CALL ULINE (LP,2)
         WRITE (LP,300) NAME
300   FORMAT ('0**ERROR** DEFAULTS NOT SET FOR TECHNIQUE ',2A4,'.')
         GO TO 410
C  
310   IF (LDFFLG.EQ.0) GO TO 330
C
C  CALCULATE NUMBER OF RECORDS FOR DEFAULT
      NWDS=7
      NUMDEF=DBUF(7)
      DO 320 I=1,NUMDEF
         NWDS=NWDS+DBUF(NWDS+2)+2
320      CONTINUE
      DBUF(1)=(NWDS+15)/16
C
C  WRITE THE RECORD
330   CALL WVLRCD (IDFUNT,IDFREC,DBUF(1),DBUF,16,ISTAT)
      IF (ISTAT.EQ.0) GO TO 350
         CALL ULINE (LP,2)
         WRITE (LP,340)
340   FORMAT ('0**ERROR** ERROR ENCOUNTERED WRITING DEFAULT RECORD.')
         IERR=1
         GO TO 290
C
C  PRINT IT
350   CALL ULINE (LP,2)
      WRITE (LP,360) NAME
360   FORMAT ('0**NOTE** NEW DEFAULTS SET FOR TECHNIQUE ',2A4,'.')
      IF (IGL.LT.0) TBUF(1)=-TBUF(1)
      CALL HPTECG (TBUF)
      GO TO 410
C
C  ERROR - TECHNIQUE HAS NO ARGUMENTS
C
370   CALL ULINE (LP,2)
      WRITE (LP,380) NAME
380   FORMAT ('0**ERROR** THERE ARE NO DEFAULTS FOR TECHNIQUE ',2A4,'.')
      IERR=1
      GO TO 290
C
C  SYSTEM ERROR
390   CALL ULINE (LP,2)
      WRITE (LP,400)
400   FORMAT ('0**ERROR** SYSTEM ERROR.')
      IERR=1
      GO TO 290
C
410   IF (IHCLTR.GT.1) WRITE (IOGDB,420) NAME,IERR
420   FORMAT (' EXIT HDFLTT - NAME=',2A4,'IERR=',I2)
C
      RETURN
C
      END
