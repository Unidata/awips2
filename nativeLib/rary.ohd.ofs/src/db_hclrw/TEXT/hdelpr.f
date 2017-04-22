C MODULE HDELPR
C-----------------------------------------------------------------------
C
      SUBROUTINE HDELPR (IGL,NAME,IPASS,NUMCAR)
C
C
C          ROUTINE:  HDELPR
C          VERSION:  1.0.0
C             DATE:  12-29-81
C           AUTHOR:  JIM ERLANDSON
C                    DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO DELETE PROCEDURE DEFINITION RECORDS.
C    CHANGES NAME TO DELETED IN INDEX AND SETS INTERNAL POINTER
C    IN RECORD TO 0.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL       I      I    1     DELG/DELL INDICATO
C       NAME      A8     I    2     NAME OF PROC
C       IPASS     A4     I    1     INPUT PASSWORD
C       NUMCAR    I      I    1     NUMBER OF CHARACTERS IN NAME
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/where'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hdatas'
      INCLUDE 'hclcommon/hindx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LKBUF=2000,LIPROC=2000)
      DIMENSION KBUF(LKBUF),IPROC(LIPROC)
      PARAMETER (LISTRNG=20)
      DIMENSION ISTRNG(LISTRNG)
      INTEGER NAME(2),IXBUF(4),IDELET(2)
      INTEGER IATEXC(2),IUPATC(8),IUPSTR(80)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdelpr.f,v $
     . $',                                                             '
     .$Id: hdelpr.f,v 1.2 1998/04/07 12:53:55 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IDELET(1)/4HDELE/,IDELET(2)/4HTED /
      DATA IATEXC(1)/4H@EXE/,IATEXC(2)/4HC   /
C
C***********************************************************************
C
C
C
C  FIND NAME IN INDEX AND GET RECORD
      IERR=0
      ITYPE=1
      ISUB=1
      IUNIT=KDEFNL
      IXUNIT=KINDXL
      CALL HFNDDF (NAME,IREC,ITYPE,IXREC)
      IF (IREC.NE.0) GO TO 20
         WRITE (LP,10) NAME
10    FORMAT ('0**ERROR** PROCEDURE ',2A4,' NOT DEFINED.')
         IERR=1
         GO TO 190
C
C  CHECK FOR DELG AND DELL
20    IF (ITYPE.EQ.IGL) GO TO 60
      IERR=1
      IF (IGL.GT.0) GO TO 40
         WRITE (LP,30)
30    FORMAT ('0**ERROR** @DELG OR @DELETEG COMMAND NOT VALID FOR ',
     1   'LOCAL PROCS.')
         GO TO 190
40    WRITE (LP,50)
50    FORMAT ('0**ERROR** @DELL OR @DELETEL COMMAND NOT VALID FOR ',
     1   'GLOBAL PROCS.')
      GO TO 190
C
C  CHECK IF ANY LOCAL PROCS USE IT IF GLOBAL
60    IF (ITYPE.GT.0) GO TO 70
         ISUB=5
         IUNIT=KDEFNG
         IXUNIT=KINDXG
         CALL UREADT (IXUNIT,IXREC,IXBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 170
         IPOINT=IXBUF(4)+500
         CALL HSRLDR (NAME,IPOINT,3,IDUM,ISTAT)
         IF (ISTAT.NE.0) IERR=1
70    CALL HGTRDN (IUNIT,IREC,KBUF,LKBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
C
C  CHECK PASSWORD
      IF (IPASS.EQ.KBUF(6)) GO TO 90
         WRITE (LP,80) NAME
80    FORMAT ('0**ERROR** INVALID PASSWORD FOR PROCEDURE ',2A4,'.')
         IERR=1
         GO TO 190

C  CHECK IF PROC IS IN ANY PROCS
90    IS=HINDEX(2,ISUB)
      IE=HINDEX(3,ISUB)
      IF (IE.LT.IS) GO TO 150
      DO 140 I=IS,IE
          CALL UREADT (IXUNIT,I,IXBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 170
C     CHECK IF PROC IS DELETED
          CALL UNAMCP (IDELET,IXBUF(1),ISTAT)
          IF (ISTAT.EQ.0) GO TO 140
C     GET THE PROC
          IF (IXBUF(3).LE.0) GO TO 170
          CALL HGTRDN (IUNIT,IXBUF(3),IPROC,LIPROC,ISTAT)
          IF (ISTAT.NE.0) GO TO 170
C     CHECK IF PROC IN PROC
          IPOS=12+2*IPROC(9)
          NUMCOM=IPROC(IPOS - 2)
          IF (NUMCOM.EQ.0) GO TO 140
          DO 110 II=1,NUMCOM
             CALL HGTSTR (LISTRNG,IPROC(IPOS),ISTRNG,LENGTH,ISTAT)
             IF (ISTAT.NE.0) GO TO 170
             NWDS=IPROC(IPOS - 1)
             IPOS=IPOS+NWDS+1
C     CHECK FOR @EXEC IN STRING
             CALL UFDSTR (IATEXC,1,IUPATC,8,2,5,ISTRNG,1,
     1          IUPSTR,80,NWDS,INDEX,ISTAT)
             IF (ISTAT.NE.0) GO TO 100
             IF (INDEX.EQ.0) GO TO 110
C     CHECK FOR PROCEDURE NAME IN STRING
             CALL UFDSTR (NAME,1,IUPATC,8,2,NUMCAR,ISTRNG,1,
     1          IUPSTR,80,NWDS,INDEX2,ISTAT)
             IF (ISTAT.NE.0) GO TO 100
             IF (INDEX2.EQ.0) GO TO 110
C     MAKE SURE ABOUT NAME
             IF (INDEX2.LT.INDEX+6) GO TO 110
             IF (IUPSTR(INDEX2-1).NE.IBLNK.AND.
     1           IUPSTR(INDEX2-1).NE.COMMA) GO TO 110
             IF (IUPSTR(INDEX2+NUMCAR).NE.IBLNK.AND.
     1           IUPSTR(INDEX2+NUMCAR).NE.COMMA) GO TO 110
C        FOUND IT
             GO TO 120
100          CONTINUE
             IERR=1
110          CONTINUE
C     TRY NEXT PROC
          GO TO 140
120       CONTINUE
          WRITE (LP,130) NAME,IPROC(4),IPROC(5)
130    FORMAT ('0**ERROR** PROCEDURE ',2A4,' USED BY PROCEDURE ',2A4,
     *   '.')
          IERR=1
140       CONTINUE
C
150   IF (IERR.EQ.1) GO TO 190
C
C  DELETE IN INDEX
      CALL UREADT (IXUNIT,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      CALL UMEMOV (IDELET,IXBUF(1),2)
      CALL UWRITT (IXUNIT,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
C      
C  DELETE IN RECORD
      KBUF(2)=0
      CALL WVLRCD (IUNIT,IREC,KBUF(1),KBUF,LRECLH,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      WRITE (LP,160) NAME
160   FORMAT ('0**NOTE** PROCEDURE ',2A4,' DELETED.')
      IF (ITYPE.GT.0) CALL HDLLDR (NAME,ITYPE,ISTAT)
      GO TO 190
C
C  SYSTEM ERROR
170   WRITE (LP,180)
180   FORMAT ('0**ERROR** SYSTEM ERROR.')
      IERR=1
C      
190   IF (IERR.EQ.1) WRITE (LP,210) NAME
210   FORMAT ('0**NOTE** PROCEDURE ',2A4,' NOT DELETED.')
C
      IF (IHCLDB.EQ.3) WRITE (IOGDB,200)
200   FORMAT (' EXIT HDELPR')
C
      RETURN
C
      END
