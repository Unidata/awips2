C MODULE SBTABL
C-----------------------------------------------------------------------
C
      SUBROUTINE SBTABL (FLON,FLAT,NBPTS,ISTAT)
C
C  THIS ROUTINE PRINTS A TABLE OF THE LATITUDE AND LONGITUDE VALUES
C  USED TO DEFINE THE BASIN BOUNDARY.
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION FLON(NBPTS),FLAT(NBPTS)
      CHARACTER*132 PLINE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbtabl.f,v $
     . $',                                                             '
     .$Id: sbtabl.f,v 1.3 2003/03/14 18:56:41 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBTABL'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      WRITE (LP,30)
      CALL SULINE (LP,1)
      WRITE (LP,40)
      CALL SULINE (LP,1)
      WRITE (LP,50)
      CALL SULINE (LP,1)
C
      LLP=1
      PLINE=' '
C
      IBSMAP=1
      ICOL=80
      DO 10 I=1,NBPTS
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,40)
            CALL SULINE (LP,1)
            WRITE (LP,50)
            CALL SULINE (LP,1)
            ENDIF
         CALL SBLETR (IBSMAP,ICOL,I,PLINE,LLP,ISTAT)
         WRITE (LP,60) I,PLINE(1:2),FLAT(I),FLON(I)
         CALL SULINE (LP,1)
10       CONTINUE
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBTABL'
         CALL SULINE (LP,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' ')
40    FORMAT (' ',
     *   'POINT',3X,
     *   'SYMBOL',3X,
     *   'LATITUDE',3X,
     *   'LONGITUDE')
50    FORMAT (' ',
     *   5('-'),3X,
     *   6('-'),3X,
     *   8('-'),3X,
     *   9('-'))
60    FORMAT (' ',
     *   I5,3X,
     *   2X,A,2X,3X,
     *   F8.4,3X,
     *   F9.4)
C
      END
