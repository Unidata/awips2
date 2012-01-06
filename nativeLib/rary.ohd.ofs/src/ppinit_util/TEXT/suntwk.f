C MODULE SUNTWK
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT CONTENTS OF NETWORK COMMON BLOCK.
C
      SUBROUTINE SUNTWK (IUNIT)
C
      CHARACTER*20 DESCRP
      DIMENSION STDSNW(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sntwkx'
C
      EQUIVALENCE (STDSNW(1),STIDNW(1,1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suntwk.f,v $
     . $',                                                             '
     .$Id: suntwk.f,v 1.2 1998/07/06 12:19:06 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IUNIT.EQ.LP.AND.ISLEFT(10).GT.0) CALL SUPAGE
C
      WRITE (IUNIT,40)
      CALL SULINE (IUNIT,2)
      WRITE (LP,50) INWFIL,MAXSNW,FLATMX,FLATMN,INWTYP,INWSRT
      CALL SULINE (IUNIT,2)
      IF (INWTYP.LT.3) THEN
         WRITE (IUNIT,60)
         CALL SULINE (IUNIT,2)
         ENDIF
      IF (INWTYP.GE.3) THEN
         WRITE (IUNIT,80)
         CALL SULINE (IUNIT,2)
         ENDIF
C
      DO 30 I=1,INWFIL
         IF (INWTYP.LE.2) THEN
            IF (ISNWPG(IUNIT).EQ.1) THEN
               WRITE (IUNIT,60)
               CALL SULINE (IUNIT,1)
               ENDIF
            WRITE (IUNIT,70) I,(STIDNW(J,I),J=1,2),
     *         (CORDNW(J,I),J=1,2),SFLGNW(I),
     *         PP24NW(I),PPVRNW(I),PCHRNW(I),
     *         TA24NW(I),TAINNW(I),TF24NW(I),ELEVNW(I),
     *         EA24NW(I),
     *         GENLNW(I),GPANW(I)
            CALL SULINE (IUNIT,1)
            GO TO 30
            ENDIF
         IF (ISNWPG(IUNIT).EQ.1) THEN
            WRITE (IUNIT,80)
            CALL SULINE (IUNIT,1)
            ENDIF
         IF (INWTYP.EQ.3) THEN
            WRITE (IUNIT,90) I,(STIDNW(J,I),J=1,2),STATNW(I),
     *         PP24NW(I),PPVRNW(I),TA24NW(I),EA24NW(I),
     *         GENLNW(I),GPANW(I)
            CALL SULINE (IUNIT,1)
            GO TO 30
            ENDIF
         IF (INWTYP.EQ.4) THEN
            IPOS=(I-1)*5+1
            CALL SUBSTR (STDSNW(IPOS),1,20,DESCRP,1)
            WRITE (IUNIT,100) I,STATNW(I),
     *         DESCRP,
     *         PP24NW(I),PPVRNW(I),TA24NW(I),EA24NW(I),
     *         GENLNW(I),GPANW(I)
            CALL SULINE (IUNIT,1)
            GO TO 30
            ENDIF
30       CONTINUE
C
      WRITE (IUNIT,110) NPP24,NPPVR,NTA24,NTAIN,NTF24,NEA24
      CALL SULINE (IUNIT,2)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('0CONTENTS OF NETWORK COMMON BLOCK')
50    FORMAT ('0INWFIL=',I4,3X,'MAXSNW=',I4,3X,'FLATMX=',F6.2,3X,
     *   'FLATMN=',F6.2,3X,'INWTYP=',I2,3X,'INWSRT=',I2)
60    FORMAT ('0',4X,1X,'STIDNW  ',4X,'CORDNW',12X,
     *   'SFLGNW',3X,
     *   'PP24NW',3X,'PPVRNW',3X,'PCHRNW',3X,
     *   'TA24NW',3X,'TAINNW',3X,'TF24NW',3X,'ELEVNW',3X,
     *   'EA24NW',3X,
     *   'GENLNW',3X,'GPANW')
70    FORMAT (' ',I4,1X,2A4,13(3X,I6))
80    FORMAT (1H0,4X,1X,'STIDNW  ',3X,'STATNW',3X,
     *   'DESCRP',17X,
     *   'PP24NW',3X,'PPVRNW',3X,'TA24NW',3X,'EA24NW',3X,
     *   'GENLNW',3X,'GPANW')
90    FORMAT (' ',I4,1X,2A4,3X,A4,5X,20X,6(3X,I6))
100   FORMAT (' ',I4,1X,8X,3X,A4,5X,A,6(3X,I6))
110   FORMAT ('0NPP24=',I4,3X,'NPPVR=',I4,3X,'NTA24=',I4,3X,
     *   'NTAIN=',I4,3X,'NTF24=',I4,3X,'NEA24=',I4)
C
      END
