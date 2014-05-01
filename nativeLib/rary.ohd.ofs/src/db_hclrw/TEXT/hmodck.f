C MODULE HMODCK
C-----------------------------------------------------------------------
C
      SUBROUTINE HMODCK (MODCMD,MAXMOD,IMGMOD,NMOD,ISTATS)
C
C  THIS ROUTINE RETURNS ANY MOD COMMAND CARD IMAGES FOR THE SPECIFIED
C  MOD COMMAND AND ALL SUBSEQUENT MOD CARDS.
C
C  ARGUMENT LIST:
C
C    NAME    TYPE  I/O   DIM       DESCRIPTION
C    ------  ----  ---   ---       -----------
C    MODCMD   A8     I    2        MODCOMMAND
C    MAXMOD   I      I    1        MAXIMUM NUMBER OF CARD IMAGES TO RE
C    IMGMOD   I      O (20,MAXMOD) MOD ARRAY
C    NMOD     I      O    1        NUMBER OF CARDS RETURND
C    ISTAT    I      O    1        STATUS:
C                                    0=OKAY
C                                    1=MAXMOD EXCEEDED
C                                    2=UNEXPECTED END OF RUN OPTIONS
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hseg1'
      INCLUDE 'hclcommon/hcurfc'
      INCLUDE 'common/where'
C
      LOGICAL*4 CONTIN
      DIMENSION IMGMOD(20,MAXMOD),TMPMOD(20)
      DIMENSION OPNOLD(2)
      DIMENSION MODCMD(2),ICMND(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hmodck.f,v $
     . $',                                                             '
     .$Id: hmodck.f,v 1.2 2000/03/14 12:52:41 page Exp $
     . $' /
C    ===================================================================
C
      DATA LDOT/4H.   /,LAMP/4H&   /
C
C
      CALL UMEMOV (OPNAME,OPNOLD,2)
      CALL UMEMOV ('HMODCK  ',OPNAME,2)
C
      ISTATS=0
      NUMRED=0
      NCDS=0
      NMOD=0
C
C  SET POINTER TO NEXT OPTION RECORD
10    NXOPT=LRECOP
      CALL HINCNX
      IF (ISTAT.NE.0) GO TO 170
C
      NXREC=IOPTRC(1)+NUMRED-1
      IF (IOPTRC(2).NE.0.AND.IOPTRC(2).NE.IHCFUN) GO TO 140
      NXOPT=2
C
C  GET NEXT WORD IN OPTION WHICH IS COUNT
20    CALL HINCNX
      IF (ISTAT.NE.0) GO TO 170
C
C  CHECK IF DONE
30    IF (IOPTRC(NXOPT).EQ.0) GO TO 10
C
C  COMPUTE START OF NEXT PART OF OPTION
      NEXT=IOPTRC(NXOPT)+NXOPT+1
      NRECS=(NEXT-1)/LRECOP
      NRSAVE=NUMRED+NRECS
C
      CALL HINCNX
      IF (ISTAT.NE.0) GO TO 170
C
C  CHECK IF OPTION IS A MOD
      IF (IOPTRC(NXOPT).NE.0) GO TO 120
C
C  MOD FOUND - READ COMMANDS LOOKING FOR A MATCH
      NUMCDS=0
      CALL HINCNX
C
C  READ ONE MORE TO SKIP OVER 0 IDS
      CALL HINCNX
      IF (ISTAT.NE.0) GO TO 170
      NUMCDS=IOPTRC(NXOPT)
      IF (NUMCDS.EQ.0) GO TO 120
      IWANT=0
C
C  PROCESS EACH MOD CARD
      CONTIN=.FALSE.
      DO 110 I=1,NUMCDS
         CALL HUPMOD (TMPMOD,NMOD,NCDS,IRANGE,IRECSV,IOPSAV)
         IF (ISTAT.NE.0) GO TO 150
C     CHECK FOR A COMMAND
         IF (CONTIN) GO TO 50
         IF (IBUF(IFSTRT(1)).NE.LDOT) GO TO 50
         ICMND(2)=IBLNK
         N=IFSTOP(1)-IFSTRT(1)
         IF (N.GT.8) N=8
         CALL UPACK1(IBUF(IFSTRT(1)+1),ICMND,N)
         CALL UCMPAR(ICMND,MODCMD,2,IST)
         IF (IST.EQ.0) GO TO 40
C     DO NOT WANT
         IWANT=0
         GO TO 80
40       IWANT=1
50       IF (IWANT.EQ.1) THEN
C        CHECK IF TOO MANY MOD CARDS
            IF (NMOD.GT.MAXMOD) THEN
               NMOD=MAXMOD
               ISTATS=1
               GO TO 170
               ENDIF
            CALL UMEMOV (TMPMOD,IMGMOD(1,NMOD),20)
            GO TO 90
            ENDIF
80        NMOD=NMOD-1
90        CONTIN=.FALSE.
          DO 100 IFLD=1,NFIELD
             IF (IFSTOP(IFLD)-IFSTRT(IFLD).GT.0) GO TO 100
             IF (IBUF(IFSTRT(IFLD)).NE.LAMP) GO TO 100
             CONTIN=.TRUE.
             GO TO 110
100          CONTINUE
110       CONTINUE
      GO TO 20
C
C  SKIP TO NEXT OPTION IN RECORD
120   IF (NRSAVE.LE.NUMRED) GO TO 130
C
C  NEED TO READ SOME RECORDS
      NXOPT=LRECOP
      CALL HINCNX
      IF (ISTAT.NE.0) GO TO 170
      GO TO 120
C
130   NXOPT=NEXT-NRECS*LRECOP
      GO TO 30
C
C  DO NOT WANT OPTION AT ALL
140   IF (NXREC.EQ.NUMRED) GO TO 10
      NUMRED=NXREC
      GO TO 10
C
150   WRITE (LP,160)
160   FORMAT ('0**ERROR** IN HMODCK - UNEXPECTED END OF RUN OPTIONS.')
      ISTATS=2
      CALL ERROR
C
170   IF (IHCLTR.GT.1) WRITE (IOGDB,180) NMOD,MODCMD
180   FORMAT (' EXIT HMODCK : NMOD=',I4,' MODCMD=',2A4)
C
      CALL UMEMOV (OPNOLD,OPNAME,2)
C
      RETURN
C
      END
