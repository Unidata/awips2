C MODULE FDMPA
C-----------------------------------------------------------------------
C
      SUBROUTINE FDMPA (ANAME,A,MA)
C
C  THIS ROUTINE PRINTS THE CONTENTS OF AN ARRAY.
C
C     SUBROUTINE INITIALLY WRITTEN BY - ERIC ANDERSON - HRL 9/1979
C
      CHARACTER*4 ANAME
      CHARACTER*4 FMT(2)/'F8.2','I8'/
      DIMENSION A(MA)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/flarys'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fdmpa.f,v $
     . $',                                                             '
     .$Id: fdmpa.f,v 1.4 2001/06/13 09:51:02 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.3) WRITE (IODBUG,*) 'ENTER FDMPA'
C
C  SET LENGTH OF THE ARRAY BEING USED
      L=0
      J=1
      IF (ANAME.EQ.'TS') THEN
         L=LTS
         GO TO 10
         ENDIF
      IF (ANAME.EQ.'P') THEN
         L=LP
         GO TO 10
         ENDIF
      IF (ANAME.EQ.'C') THEN
         L=LC
         GO TO 10
         ENDIF
      IF (ANAME.EQ.'T') THEN
         L=LT
         J=2
         GO TO 10
         ENDIF
      IF (ANAME.EQ.'D') THEN
         L=LD
         GO TO 10
         ENDIF
C
C  CHECK IF LENGTH IS GREATER THAN ZERO
10    IF (L.EQ.0) THEN
         WRITE (IODBUG,20) ANAME(1:LENSTR(ANAME))
20    FORMAT (' THE ',A,' ARRAY DOES NOT CONTAIN ANY VALUES')
         GO TO 130
         ENDIF
C
C  PRINT THE CONTENTS OF THE ARRAY
      WRITE (IODBUG,30) ANAME(1:LENSTR(ANAME)),FMT(J),MA,L
30    FORMAT (' CONTENTS OF THE ',A,' ARRAY:',3X,'FORMAT=15',A4,
     *      3X,'DIMENSIONED LENGTH=',I6,
     *      3X,'PORTION USED=',I5 /
     *   1X,'POSITION')
      I1=1
40    I2=I1+14
      IF (I2.GT.L) I2=L
      IF (J.EQ.1) WRITE (IODBUG,50) I1,(A(I),I=I1,I2)
50    FORMAT (' ',I5,2X,15F8.2)
      IF (J.EQ.2) WRITE (IODBUG,70) I1,(A(I),I=I1,I2)
70    FORMAT (' ',I5,15I8)
      IF (I2.LT.L) THEN
         I1=I2+1
         GO TO 40
         ENDIF
C
C  PRINT CONTENTS IN ALPHANUMERIC FORMAT FOR TS, P AND C ARRAYS
      IF (ANAME.EQ.'TS'.OR.ANAME.EQ.'P'.OR.ANAME.EQ.'C') THEN
         WRITE (IODBUG,100) ANAME(1:LENSTR(ANAME))
100   FORMAT (' CONTENTS OF THE ',A,' ARRAY:',3X,'FORMAT=15(4X,A4)' /
     *   1X,'POSITION')
         I1=1
110      I2=I1+14
         IF (I2.GT.L) I2=L
         WRITE (IODBUG,120) I1,(A(I),I=I1,I2)
120   FORMAT (' ',I5,2X,15(4X,A4))
         IF (I2.LT.L) THEN
            I1=I2+1
            GO TO 110
            ENDIF
         ENDIF
C
130   RETURN
C
      END
