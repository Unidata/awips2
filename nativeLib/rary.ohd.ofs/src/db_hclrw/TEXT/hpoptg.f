C MODULE HPOPTG
C-----------------------------------------------------------------------
C          ROUTINE:  HPOPTG
C             VERSION:  1.0.0
C                DATE:  9-3-81
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C***********************************************************************
C          DESCRIPTION:
C
C    GENERAL PRINT ROUTINE FOR NAMED OPTION RECORDS
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       KBUF       I     I     ?    INPUT BUFFER CONTAINING RECORD
C***********************************************************************
      SUBROUTINE HPOPTG (KBUF,IWORK,LWORK)
C
      INCLUDE 'udebug'
      INCLUDE 'uio'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'hclcommon/hword2'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hunits'
C
      DIMENSION KBUF(*)
      DIMENSION IWORK(LWORK)
      DIMENSION NONAME(2),NOTNAM(2),INDBUF(4)
      DIMENSION IFPRCP(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hpoptg.f,v $
     . $',                                                             '
     .$Id: hpoptg.f,v 1.2 1998/07/06 11:33:46 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA IFPRCP/4HFUTP,4HRECP/
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,140)
C
      CALL UMEMOV (KBUF(4),NONAME,2)
      NOPASS=KBUF(6)
      IF (NOPASS.EQ.IBLNK) NOPASS=LNONE
      NONUM=KBUF(2)
C
      CALL ULINE (LP,2)
      WRITE (LP,150)
      CALL ULINE (LP,2)
      WRITE (LP,160) NONAME,NOPASS,NONUM
C
      J=0
C
10    NOWRDS=KBUF(7+J)
      IF (NOWRDS.EQ.0) GO TO 120
      IF (NOWRDS.GT.0) GO TO 20
      IREC=HINDEX(2,4)-NOWRDS-1
      IF (IREC.LT.3005.OR.IREC.GT.4004) GO TO 110
         CALL UREADT (KINDXL,IREC,INDBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 110
         CALL UMEMOV (INDBUF,NOTNAM,2)
         CALL ULINE (LP,2)
         WRITE (LP,170) LINCLD,NOTNAM
         J=J+1
         GO TO 100
C
C  WRITE THE TECHNIQUE
20    NKIND=KBUF(8+J)
      IF (NKIND.EQ.0) GO TO 90
C
C  GET TECH NAME FROM APPROPRIATE INDEX
      ISUB=3
      IXUNIT=KINDXL
      IF (NKIND.GT.0) GO TO 30
         ISUB=7
         IXUNIT=KINDXG
         NKIND=-NKIND
30    IREC=HINDEX(2,ISUB)+NKIND-1
      IF (IREC.LT.HINDEX(2,ISUB).OR.IREC.GT.HINDEX(1,ISUB)) GO TO 110
         CALL UREADT (IXUNIT,IREC,INDBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 110
         CALL UMEMOV (INDBUF,NOTNAM,2)
C
C  SET VALUE OF GROUP IDENTIFIER IF THERE ARE IDS
      IDFLAG=0
      IF (KBUF(10+J).NE.0) IDFLAG=1
C
C  SET DEFAULT VALUE OF TECHNIQUE
      NOTVAL=KBUF(9+J)
      CALL UCMPAR (NOTNAM,IFPRCP,2,IMATCH)
      IF (IMATCH.EQ.0) THEN
         IF (NOTVAL.EQ.-1) THEN
            NOTVAL=LYES
            GO TO 40
            ENDIF
         ENDIF
      IF (NOTVAL.LT.0.OR.NOTVAL.GT.1) GO TO 50
         IF (NOTVAL.EQ.0) NOTVAL=LNO
         IF (NOTVAL.EQ.1) NOTVAL=LYES
40    CALL ULINE (LP,2)
      WRITE (LP,180) NOTNAM,NOTVAL
      GO TO 60
50    CALL ULINE (LP,2)
      WRITE (LP,190) NOTNAM,NOTVAL
C
C  WRITE ANY IDENTIFIERS
60    IF (IDFLAG.EQ.0) GO TO 70
      CALL HPRIDS (KBUF(10+J),IPOINT,1)
      IPOINT=IPOINT+9+J
      GO TO 80
C
70    IPOINT=11+J
C
C  WRITE ANY ARGUMENTS
80    NONARS=KBUF(IPOINT)
      J=J+NOWRDS+1
      IF (NONARS.EQ.0) GO TO 100
         ITYPE=3
         IF (IXUNIT.EQ.KINDXG) ITYPE=-3
         CALL HPRARS (KBUF(IPOINT),INDBUF(3),ITYPE,IWORK,LWORK)
         GO TO 100
C
C  WRITE ANY MODS
90    CALL HPRMOD (KBUF(9+J))
C
      J=J+NOWRDS+1
C
100   GO TO 10
C
110   CALL ULINE (LP,2)
      WRITE (LP,200) IREC
      GO TO 130
C
120   IF (IHCLDB.GT.0) WRITE (IOGDB,210)
C
130   RETURN
C
C-----------------------------------------------------------------------
C
140   FORMAT (' ENTER HPOPTG')
150   FORMAT ('0',132('-'))
160   FORMAT ('0',
     *   'NAMED OPTION = ',2A4,3X,
     *   'PASSWORD = ',A4,3X,
     *   'INTERNAL NAMED OPTION NUMBER = ',I4)
170   FORMAT ('0',13X,4A4)
180   FORMAT ('0',13X,'TECHNIQUE = ',2A4,'  VALUE = ',A4)
190   FORMAT ('0',13X,'TECHNIQUE = ',2A4,'  VALUE = ',I7)
200   FORMAT ('0**ERROR** IN HPOPTG - SYSTEM ERROR. IREC=',I6)
210   FORMAT (' EXIT HPOPTG')
C
      END
