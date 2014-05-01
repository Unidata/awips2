C MODULE URIPRD
C-----------------------------------------------------------------------
C
      SUBROUTINE URIPRD (IINIT)
C
C  THIS ROUTINE WILL INITIALIZE THE NEW PROCESSED DATA
C  FILES BY COPYING THE CONTROL RECORDS FROM THE OLD FILES AND ZEROING
C  SPECIFIC CONTROL VARIABLES; OR BY ONLY ZEROING SPECIFIC CONTROL
C  VARIABLES.
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'urcommon/urtscl'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/urmaxm'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urprd'
C
      DIMENSION ARR(16),IURXRC(4)
      DIMENSION KUNITS(5)
C
      EQUIVALENCE (KUNITS(1),KUMAPT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_reorder/RCS/uriprd.f,v $
     . $',                                                             '
     .$Id: uriprd.f,v 1.3 2000/12/18 21:39:41 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'ENTER URIPRD'
         ENDIF
C
      IF (IINIT.EQ.1) THEN
         CALL SULINE (LP,2)
         WRITE (LP,175)
         ENDIF
      IF (IINIT.EQ.-1) THEN
         CALL SULINE (LP,2)
         WRITE (LP,177)
         ENDIF
C
C  COPY OLD COMMON INTO NEW COMMON
      IF (IINIT.EQ.-1) CALL UMEMOV (USERPR,INAMRF,60)
C
C  SET NUMBER OF TIME SERIES DEFINED
      NMTIMS=0
C
C  COPY DATA TYPE DIRECTORY FROM OLD FILES
      DO 10 I=1,NUMDTP
         IF (IINIT.EQ.-1) CALL UMEMOV (DATFIL(1,I),IDATFL(1,I),18)
         IDATFL(8,I)=0
         IDATFL(9,I)=0
         IDATFL(10,I)=0
         IDATFL(15,I)=0
10       CONTINUE
      IF (IPRDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,180) (IDATFL(J,1),J=1,15)
         ENDIF
C
C  INITIALIZE CONTROL RECORD FOR NEW FILES
      IF (IINIT.EQ.-1) CALL UMEMOV (TSCNTR,ITSCNT,80)
C
C  RESET NEXT AVAILABLE RECORD FOR EACH FILE TO INITIALIZED STATE
      DO 20 I=1,5
         ITSCNT(3,I)=2
20       CONTINUE
C
C  FILL IN NEW COMMON WITH MAXIMUM NUMBER OF RECORDS FOR EACH FILE
      DO 30 I=1,5
         MXRECS(I)=ITSCNT(2,I)
30       CONTINUE
C
C  WRITE CONTROL RECORDS TO FILES
      IAMORD=1
      CALL WPDBCO (ISTAT)
      IF (ISTAT.NE.0) GO TO 90
C
C  INITIALIZE INDEX
      CALL UMEMST (0,IURXRC,4)
      NUMREC=MXTIME*2
      DO 80 IREC=1,NUMREC
         CALL UWRITT (KUPRIX,IREC,IURXRC,ISTAT)
         IF (ISTAT.NE.0) GO TO 130
80       CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,190) NUMREC,KUPRIX
C
C  INITIALIZE TIME SERIES FILES
      NTSFIL=5
      DO 40 I=1,LRECT
         ARR(I)=ZAPPR
40       CONTINUE
      DO 60 I=1,NTSFIL
         IUNIT=KUNITS(I)
         LSTREC=MXRECS(I)
         NUMREC=LSTREC-2+1
         DO 50 IREC=2,LSTREC
            CALL UWRITT (IUNIT,IREC,ARR,ISTAT)
            IF (ISTAT.NE.0) GO TO 130
50          CONTINUE
         CALL SULINE (LP,2)
         WRITE (LP,190) NUMREC,IUNIT
60       CONTINUE
C
      CALL SULINE (LP,2)
      WRITE (LP,210)
      GO TO 160
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    WRITE (LP,100)
      CALL SUERRS (LP,2,-1)
100   FORMAT ('0*** ERROR - IN URIPRD - TRYING TO WRITE CONTROL ',
     *   'RECORDS FOR TIME SERIES FILES.')
      GO TO 150
C
130   WRITE (LP,140)
      CALL SUERRS (LP,2,-1)
140   FORMAT ('0*** ERROR - IN URIPRD - TRYING TO WRITE TIME SERIES ',
     *   'RECORDS FOR NEW FILE INITIALIZATION.')
C
C  SET ERROR FLAG
150   IWURFL=1
C
160   IF (IPRTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'EXIT URIPRD'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
175   FORMAT ('0*** BEGIN TO INITIALIZE PROCESSED DATA ',
     *   'BASE NEW FILES.')
177   FORMAT ('0*** BEGIN TO INITIALIZE PROCESSED DATA ',
     *   'BASE NEW FILES BY COPYING CONTROLS FROM OLD FILES.')
180   FORMAT (' IDATFL(J,1)',A4,2X,14I4)
190   FORMAT ('0*** NOTE - ',I6,' RECORDS INITIALIZED FOR ',
     *   'UNIT ',I2,'.')
210   FORMAT ('0*** NOTE - THE PROCESSED DATA BASE NEW FILES ',
     *   'HAVE BEEN INITIALIZED.')
C
      END
