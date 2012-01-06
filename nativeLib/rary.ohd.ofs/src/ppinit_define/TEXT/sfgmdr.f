C MEMBER SFGMDR
C-----------------------------------------------------------------------
C
C DESC ROUTINE FOR DEFINING GMDR PARAMETER RECORD FOR THE MARO FUNCTION
C DESC PPINIT COMMAND :  @DEFINE MDRGRID
C
      SUBROUTINE SFGMDR (LARRAY,ARRAY,IERR)
C
C
      REAL XGMDR/4HGMDR/
      REAL XNEW/4HNEW /
      REAL XOLD/4HOLD /
      INTEGER*2 NMDRGP(42,89)
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
      INCLUDE 'scommon/sgboxx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfgmdr.f,v $
     . $',                                                             '
     .$Id: sfgmdr.f,v 1.1 1995/09/17 19:11:02 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,180)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(XGMDR)
C
      ISTAT=0
C
C  SET VALUE FOR MISSING PARAMETER
      UNSD=-999.
C
      NUMERR=0
      NUMWRN=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
C  PRINT CARD
      CALL SUPCRD
C
C  PRINT HEADER LINE
      WRITE (LP,190)
      CALL SULINE (LP,2)
      WRITE (LP,200)
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF PREPROCESSOR PARAMETRIC DATA BASE ALLOCATED
20    IDPPP=1
      CALL SUDALC (0,0,0,0,IDPPP,0,0,0,0,0,NUMERR,IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (LP,210)
         CALL SUERRS (LP,2,NUMERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF GENERAL USER PARAMETER COMMON BLOCK FILLED
C
30    IF (IUGFIL.GT.0) GO TO 50
C
C  READ USER DEFINED DEFAULTS
      CALL SUGTUG (LARRAY,ARRAY,IERR)
      IF (IERR.NE.0) GO TO 40
         IUGFIL=1
         GO TO 50
40    WRITE (LP,220)
      CALL SUERRS (LP,2,NUMERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF GRID BOX COMMON BLOCK FILLED
C
50    IF (IBXFIL.GT.0) GO TO 60
C
C  READ GRID BOX PARAMETER ARRAYS
      CALL SUGTBX (LARRAY,ARRAY,IERR)
C
C  CHECK IF ANY BOXES DEFINED
60    NGBOX=0
      DO 70 IBOX=1,99
         IF (IGBOX(IBOX).EQ.0) GO TO 70
            LAT=GBOXLT(IBOX)
            LON=GBOXLN(IBOX)
            IF (LDEBUG.GT.0) WRITE (IOSDBG,230) IBOX,LAT,LON,
     *         IGBOXM(IBOX)
            IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
            NGBOX=NGBOX+1
70       CONTINUE
C
      IF (NGBOX.GT.0) GO TO 80
         WRITE (LP,240)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF GMDR PARAMETERS ALREADY DEFINED
80    DISP=XOLD
      IPRERR=0
      INCLUDE 'scommon/callsrgmdr'
      IF (IERR.EQ.0) GO TO 90
         DISP=XNEW
C
C  SET BEGINNING AND ENDING ROW AND COLUMN OF MDR GRID SUBSET
90    ICOL=MDRSUB(1)
      NCOL=MDRSUB(2)
      LCOL=ICOL+NCOL-1
      IROW=MDRSUB(3)
      NROW=MDRSUB(4)
      LROW=IROW+NROW-1
      DO 10 I=1,42
         DO 10 J=1,89
            NMDRGP(I,J)=0
10          CONTINUE
      IF (LDEBUG.GT.0)
     *   CALL SUGMDR (ICOL,NCOL,LCOL,IROW,NROW,LROW,NMDRGP,IOSDBG)
C
      NPAIR=1
      ILLGD=0
C
C  START WITH SOUTHWEST MDR BOX AND PROCEED BY COLUMNS TO EAST
C  AND THEN BY ROWS TO NORTH
      DO 130 IR=IROW,LROW
         DO 120 IC=ICOL,LCOL
C        COMPUTE HRAP COORDINATES OF SOUTHWEST CORNER OF MDR BOX
            IHRAPX=(IC-1)*10+1
            IHRAPY=(IR-1)*10+1
C        COMPUTE HRAP COORDINATES OF CENTROID OF MDR BOX
            IHRAPX=IHRAPX+5
            IHRAPY=IHRAPY+5
C        CONVERT HRAP COORDINATES OF CENTRIOD TO LAT/LON
            HRAPX=FLOAT(IHRAPX)
            HRAPY=FLOAT(IHRAPY)
            CALL SBLLGD (XLON,XLAT,NPAIR,HRAPX,HRAPY,ILLGD,IERR)
            IF (LDEBUG.GT.0) WRITE (IOSDBG,260) IC,IHRAPX,XLON,
     *         IR,IHRAPY,XLAT
            IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
            XLAT=XLAT+.05
            XLON=XLON+.05
            ILAT=XLAT
            ILON=XLON
            IF (LDEBUG.GT.0) WRITE (IOSDBG,270) XLAT,XLON,ILAT,ILON
            IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C        SEARCH GRID BOXES USING LAT/LON TO WHOLE DEGREES
            DO 110 IBOX=1,99
               IF (IGBOX(IBOX).EQ.0) GO TO 110
                  LAT=GBOXLT(IBOX)
                  LON=GBOXLN(IBOX)
                  IF (ILAT.EQ.LAT.AND.ILON.EQ.LON) GO TO 100
                     GO TO 110
C              MATCH FOUND - COMPUTE GRID POINT ADDRESS
100               IFRACN=(XLAT-FLOAT(ILAT))*10.
                  IFRACW=(XLON-FLOAT(ILON))*10.
                  NGPA=IBOX*100+IFRACN*10+IFRACW
C              CHECK IF MDR PCPN ESTIMATES ALWAYS TO BE USED
                  IF (IGBOXM(IBOX).EQ.1) NGPA=-NGPA
                  NIC=IC-ICOL+1
                  NMDRGP(NIC,IR)=NGPA
                  IF (LDEBUG.GT.0) WRITE (IOSDBG,280) IBOX,IFRACN,
     *               IFRACW,NIC,IR,NMDRGP(NIC,IR)
                  IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
                  GO TO 120
110            CONTINUE
120         CONTINUE
130      CONTINUE
C
C  CHECK IF ERRORS ENCOUNTERED
140   IF (NUMERR.EQ.0) GO TO 145
         WRITE (1290) NUMERR
         CALL SULINE (LP,2)
         ISTAT=1
         GO TO 170
C
145   IF (LDEBUG.GT.0)
     *   CALL SUGMDR (ICOL,NCOL,LCOL,IROW,NROW,LROW,NMDRGP,IOSDBG)
C
C  CHECK IF RUNCHECK OPTION SPECIFIED
150   IF (IRUNCK.EQ.1) GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  WRITE GMDR PARAMETERS TO FILE
      IVGMDR=1
      MDRWCL=ICOL
      MDRNCL=NCOL
      MDRSRW=IROW
      MDRNRW=NROW
      WDISP=DISP
      CALL SWGMDR (IVGMDR,MDRWCL,MDRNCL,MDRSRW,MDRNRW,NMDRGP,
     *   UNSD,LARRAY,ARRAY,IPTR,WDISP,IERR)
      IF (IERR.EQ.0) GO TO 160
         ISTAT=1
         GO TO 170
C
C  READ GMDR PARAMETERS
160   IPTR=0
      IPRERR=1
      INCLUDE 'scommon/callsrgmdr'
      IF (IERR.GT.0) GO TO 170
C
C  PRINT GMDR PARAMETERS
      INCLUDE 'scommon/callspgmdr'
C
170   IF (ISTRCE.GT.0) WRITE (IOSDBG,300)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT (' *** ENTER SFGMDR')
190   FORMAT (1H )
200   FORMAT ('0*--> DEFINE GMDR PARAMETERS')
210   FORMAT ('0*** ERROR - PREPROCESSOR PARAMETRIC DATA BASE FILES ',
     *   'ARE NOT ALLOCATED. INPUT DATA WILL BE CHECKED FOR ERRORS.')
220   FORMAT ('0*** ERROR - UGNL PARAMETER NOT SUCCESSFULLY READ.')
230   FORMAT (' IBOX=',I2,3X,'LAT=',I3,3X,'LON=',I3,3X,
     *   'IGBOXM=',I1)
240   FORMAT ('0*** ERROR - NO GRID BOXES ARE DEFINED.')
260   FORMAT (' IC=',I3,3X,'IHRAPY=',I3,3X,'XLON=',F6.2,3X,
     *   'IR=',I3,3X,'IHRAPX=',I3,3X,'XLAT=',F6.2)
270   FORMAT (' XLAT+.05=',F6.2,3X,'XLON+.05=',F6.2,3X,
     *   'ILAT=',I3,3X,'ILON=',I3)
280   FORMAT (' IBOX=',I2,3X,'IFRACN=',I2,3X,'IFRACW=',I2,3X,
     *   'NIC=',I3,3X,'IR=',I3,3X,'NMDRGP(NIC,IR)=',I5)
290   FORMAT ('0*** NOTE - GMDR PARAMETERS NOT WRITTEN BECAUSE ',I2,
     *   ' ERRORS ENCOUNTERED.')
300   FORMAT (' *** EXIT SFGMDR')
C
      END
