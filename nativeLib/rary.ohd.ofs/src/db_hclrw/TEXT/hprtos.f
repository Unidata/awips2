C MEMBER HPRTOS
C  (from old HCLPRALL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/28/95.07:58:34 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HPRTOS
C
C          ROUTINE:  HPRTOS
C
C
C             VERSION:  1.0.0
C
C                DATE:  9-22-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO PRINT ALL RUN-TIME OPTIONS FOR A SPECIFIED FUNCTION
C    OR ALL RUN-TIME OPTIONS
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        NONE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hword2'
      INCLUDE 'hclcommon/hseg1'
      INCLUDE 'hclcommon/hunits'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*8 FUNNAM/' '/
C
      DIMENSION IFUNRC(128),IRTORC(2000),IWORK(400)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hprtos.f,v $
     . $',                                                             '
     .$Id: hprtos.f,v 1.1 1995/09/17 18:42:52 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      LFUNRC=128
      MOPTRC=2000
      LWORK=400
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,70)
C
      IREC=1
      ITYPE=2
      NUMOPT=1
      FUNNAM='ALL'
C
C  GET CARD
      NNCARD=1
      CALL HCARDR (NNCARD,ISTAT)
      IF (ISTAT.NE.0) GO TO 40
      IF (NFIELD.EQ.1) GO TO 30
C
C  GET FUNCTION NAME
      NFLD=2
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NUM.GT.8) THEN
         CALL ULINE (LP,2)
         WRITE (LP,80)
         GO TO 60
         ENDIF
      ISTRT=IFSTRT(NFLD)
      CALL UPACK1 (IBUF(ISTRT),FUNNAM,NUM)
C
C  CHECK FOR 'ALL'
      IF (FUNNAM.EQ.'ALL') GO TO 30
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT ALL RUN-TIME OPTIONS FOR NAMED FUNCTION
      CALL HGTRCD (ITYPE,FUNNAM,LFUNRC,IFUNRC,ISTAT)
      IF (ISTAT.NE.0) GO TO 40
      IFNUM=IFUNRC(2)
C
10    IF (IREC.GT.MAXOPT) GO TO 50
C
      CALL HGTRDN (KHDFLT,IREC,IRTORC,MOPTRC,ISTAT)
      IF (ISTAT.NE.0) GO TO 40
      IF (IRTORC(2).NE.0.AND.IRTORC(2).NE.IFNUM) GO TO 20
      CALL HPRTOG (IRTORC,IWORK,LWORK)
      NUMOPT=NUMOPT+1
C
20    IREC=IREC+IRTORC(1)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT ALL RUN-TIME OPTIONS
C
30    IF (IREC.GT.MAXOPT) GO TO 50
C
      CALL HGTRDN (KHDFLT,IREC,IRTORC,MOPTRC,ISTAT)
      IF (ISTAT.NE.0) GO TO 40
      CALL HPRTOG (IRTORC,IWORK,LWORK)
      IREC=IREC+IRTORC(1)
      NUMOPT=NUMOPT+1
      GO TO 30
C
40    CALL ULINE (LP,2)
      WRITE (LP,90)
C
50    IF (NUMOPT.EQ.1) THEN
         IF (FUNNAM.EQ.'ALL') THEN
            CALL ULINE (LP,2)
            WRITE (LP,100)
            ELSE
               CALL ULINE (LP,2)
               WRITE (LP,110) FUNNAM(1:LENSTR(FUNNAM))
            ENDIF
         GO TO 60
         ENDIF
C
60    IF (IHCLTR.GT.0) WRITE (IOGDB,120)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' ENTER HPRTOS')
80    FORMAT ('0**ERROR** IN HPRTOS - INVALID KEYWORD.')
90    FORMAT ('0**ERROR** IN HPRTOS - SYSTEM ERROR.')
100   FORMAT ('0**NOTE** NO RUN-TIME OPTIONS FOUND.')
110   FORMAT ('0**NOTE** NO RUN-TIME OPTIONS FOUND ',
     *   'FOR FUNCTION ',A,'.')
120   FORMAT (' EXIT HPRTOS')
C
      END
