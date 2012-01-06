C MEMBER PRTPSR
C  (from old member PDPRTOBS)
C-----------------------------------------------------------------------
C
      SUBROUTINE PRTPSR (XDTYPE,JULBEG,JULEND,LIDATA,IDATA,IUNTYP)
C
C          ROUTINE:  PRTPSR
C
C             VERSION:  1.0.0
C
C                DATE:  9-8-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE PRINTS THE PPSR OBSERVATION DATA FOR THE
C    SPECIFIED DATES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       XDTYPE     A     I     1    DATA TYPE
C       JULBEG      I     I     1    START DATE (JULIAN DAY)
C       JULEND     I     I     1    END DATE (JULIAN DAY)
C       IUNTYP     I     I     1    UNITS TYPE
C                                     0=ENGLISH
C                                     1=METRIC
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LPNTRS=32)
C
      CHARACTER*(*) XDTYPE
      CHARACTER*4 XDUNIT
C
      INTEGER*2 IDATA(LIDATA)
      INTEGER*2 IPNTRS(LPNTRS)
      INTEGER*2 MSNG
C
      DIMENSION X(3),Y(3),FLON(3),FLAT(3)
      DIMENSION VAL(3),IH(3),IMO(3),IDAY(3),IYR(3),IHR(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/prtpsr.f,v $
     . $',                                                             '
     .$Id: prtpsr.f,v 1.3 1996/07/11 21:19:12 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) '*** ENTER PRTPSR'
C
      LDATES=1
C
      JULNXT=JULBEG
      IRETRN=0
      IDSPLY=0
C
      IPRPAG=1
      IPRHDR=1
C
C  FIND TYPE
      IDX=IPDCKD(XDTYPE)
      IF (IDDTDR(7,IDX).LT.1) GO TO 160
C
C  FIND WHERE THIS RECORD BEGINS
      IF (IDDTDR(17,IDX).EQ.0) GO TO 180
C
C  CHECK IF ANY DATA IN DATA BASE
      CALL UMEMOV (IDDTDR(8,IDX),IFDATE,1)
      CALL UMEMOV (IDDTDR(11,IDX),ILDATE,1)
      IF (IPDDB.GT.0) WRITE (LP,*) 'IFDATE=',IFDATE,
     *   ' ILDATE=',ILDATE
      IF (IFDATE.EQ.0) GO TO 160
C
C  CHECK IF DATE WITHIN RANGE
10    IF (JULNXT.GE.IFDATE.AND.JULNXT.LE.ILDATE) GO TO 20
         IHOUR=24
         CALL MDYH2 (JULNXT,IHOUR,IMO(1),IDAY(1),IYR(1),IHR(1),
     *      ITZ,IDSAV,TIME(3))
         CALL MDYH2 (IFDATE,IHOUR,IMO(2),IDAY(2),IYR(2),IHR(2),
     *      ITZ,IDSAV,TIME(3))
         CALL MDYH2 (ILDATE,IHOUR,IMO(3),IDAY(3),IYR(3),IHR(3),
     *      ITZ,IDSAV,TIME(3))
         WRITE (LP,250)
     *      IMO(1),IDAY(1),IYR(1),IHR(1),TIME(3),
     *      XDTYPE(1:LENSTR(XDTYPE)),
     *      IMO(2),IDAY(2),IYR(2),IHR(2),TIME(3),
     *      IMO(3),IDAY(3),IYR(3),IHR(3),TIME(3)
         GO TO 80
C
C  READ DATA      
20    CALL RPDDLY (XDTYPE,JULNXT,IRETRN,LPNTRS,IPNTRS,LPFILL,
     *   LIDATA,IDATA,LDFILL,NUMSTA,MSNG,LDATES,IDATES,IERR)
      IF (NUMSTA.EQ.0) GO TO 80
      IF (IERR.EQ.0) GO TO 30
      GO TO (100,100,100,120,140,220),IERR
C
C  GET UNITS
30    CALL PDFUNT (XDTYPE,IUNTYP,XDUNIT)
C
C  CHECK IF NEED TO PRINT HEADER
      IF (IPRPAG.EQ.1) THEN
         CALL UPAGE (LP)
         IPRPAG=0
         ENDIF
      NLINEL=1
      CALL ULINEL (LP,NLINEL,IRETRN)
      IF (IPRHDR.EQ.0.AND.IRETRN.EQ.0) GO TO 40
      IPRHDR=0
      CALL ULINE (LP,7)
      WRITE (LP,260) XDTYPE,XDUNIT,TIME(3)
C
40    NST=NUMSTA
      NUMOV=3
      IPOS=3
      J=1
C
50    IF (NST.LT.3) NUMOV=NST
C
C  MOVE LAT/LON AND VALUES FOR CONVERSION
      DO 60 K=1,NUMOV
         Y(K)=IDATA(J)
         Y(K)=Y(K)/10
         J=J+1
         X(K)=IDATA(J)
         X(K)=X(K)/10
         J=J+2
60       CONTINUE
C
C  CONVERT COORDINATES
      CALL SBLLGD (FLON,FLAT,NUMOV,X,Y,0,IERR)
      IF (IERR.NE.0) GO TO 200
C
C  GET DATA VALUES
      DO 70 KT=1,NUMOV
         JULDA=JULNXT
         CALL PDGTPP (IDATA(IPOS),VAL(KT),IH(KT),IVV)
         IF (IH(KT).NE.0) JULDA=JULNXT-1
         CALL MDYH2 (JULDA,IH(KT),IMO(KT),IDAY(KT),IYR(KT),IHR(KT),
     *      ITZ,IDSAV,TIME(3))
         IYR(KT)=MOD(IYR(KT),100)
         IPOS=IPOS+3
70       CONTINUE
C
C  PRINT REPORT FOR ONE DAY
      NL=(NUMOV+2)/3
      CALL ULINE (LP,NL)
      WRITE (LP,270) (IMO(I),IDAY(I),IYR(I),IHR(I),FLAT(I),FLON(I),
     *   VAL(I),I=1,NUMOV)
      IDSPLY=1
C
C  CHECK IF MORE DATA TO PRINT FOR THIS DAY
      NST=NST-NUMOV
      IF (NST.GT.0) GO TO 50
C
C  INCREMENT TO NEXT DAY
80    JULNXT=JULNXT+1
      IF (JULNXT.LE.JULEND) GO TO 10
C
90    IF (IDSPLY.EQ.0) WRITE (LP,280)
      GO TO 240
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   WRITE (LP,110)
110   FORMAT ('0**ERROR** WORK ARRAYS TOO SMALL.')
      GO TO 240
C
120   WRITE (LP,130)
130   FORMAT ('0**WARNING** DATE OUT OF RANGE.')
      GO TO 80
C
140   WRITE (LP,150) XDTYPE(1:LENSTR(XDTYPE))
150   FORMAT ('0**ERROR** DATA TYPE ',A,' NOT FOUND.')
      GO TO 240
C
160   WRITE (LP,170)
170   FORMAT ('0**ERROR** NO STRANGER STATION DATA FOUND.')
      GO TO 240
C
180   WRITE (LP,190) XDTYPE(1:LENSTR(XDTYPE))
190   FORMAT ('0**NOTE** NO STATIONS DEFINED WITH DATA TYPE ',A,'.')
      GO TO 240
C
200   WRITE (LP,210) IERR
210   FORMAT ('0**ERROR** CONVERING LATITUDE/LONGITUDE VALUES. ',
     *   'SBLLGD STATUS CODE=',I2)
      GO TO 240
C
220   WRITE (LP,230) IERR
230   FORMAT ('0**ERROR** IN PRTPSR - SYSTEM ERROR - IERR=',I2)
      GO TO 240
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
240   IF (IPDTR.GT.0) WRITE (IOGDB,*) '*** EXIT PRTPSR'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
250   FORMAT (' **WARNING** ',
     *   'DATE ',
     *   I2.2,'/',I2.2,'/',I4.4,1X,I2.2,A4,
     *   ' IS NOT IN RANGE OF DATES FOR ',A,' DATA (',
     *   I2.2,'/',I2.2,'/',I4.4,1X,I2.2,A4,
     *   ' THRU ',
     *   I2.2,'/',I2.2,'/',I4.4,1X,I2.2,A4,
     *   ').')
260   FORMAT ('0*** DAILY DATA TYPE ',A,' ***',5X,
     *      'DATA UNITS = ',A,5X,
     *      'TIME ZONE = ',A4 /
     *   '0',2X,3('DATE',3X,'HR',6X,'LAT/LON',6X,'VALUE',10X) /
     *   1X,3(8('-'),1X,2('-'),3X,13('-'),2X,7('-'),7X))
270   FORMAT (1X,
     *   3(I2.2,'/',I2.2,'/',I2.2,1X,I2.2,3X,F5.2,2X,F6.2,2X,F7.2,7X))
280   FORMAT ('0**WARNING** NO PPSR DATA PRINTED.')
C
      END
