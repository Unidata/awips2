C MODULE FCDMP1
C-----------------------------------------------------------------------
C
C  PRINT CONTENTS OF COMMON BLOCK FCSEGN (ICOM=0) OR FCSGNN (ICOM=1).
C
      SUBROUTINE FCDMP1 (ICOM,IARRAY)
C
      DIMENSION IARRAY(*)
      CHARACTER*6 COMNAM
      EQUIVALENCE (XLAT,LAT),(XLONG,LONG)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fcdmp1.f,v $
     . $',                                                             '
     .$Id: fcdmp1.f,v 1.3 2001/06/13 10:57:19 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.2) WRITE (IODBUG,*) 'ENTER FCDMP1'
C
      IBUG=IFBUG('SEGD')
C
      IF (IBUG.EQ.1) THEN
         COMNAM=' '
         IF (ICOM.EQ.0) COMNAM='FCSEGN'
         IF (ICOM.EQ.1) COMNAM='FCSGNN'
         WRITE (IODBUG,10) IARRAY(2),IARRAY(3),COMNAM
10    FORMAT (' SEGMENT=',2A4,' COMNAM=',A)
         ENDIF
C
C  PRINT SEGMENT DESCRIPTION
      WRITE (IPR,30) (IARRAY(I),I=24,28)
30    FORMAT ('0',14X,22('*') /
     *   ' ',14X,'*',5A4,'*' /
     *   ' ',14X,22('*'))
C
C  SEGMENT CONNECTIVITY
      WRITE (IPR,40) (IARRAY(I),I=2,17)
40    FORMAT ('0',9('-'),2X,49('-'),1X,21('-') /
     *  ' ','-SEGMENT-',2X,'-',10X,'  UPSTREAM SEGMENTS',18X,
     *  ' ','- -DOWNSTREAM SEGMENTS-' /
     *  ' ',9('-'),2X,49('-'),1X,21('-') /
     *  ' ',1X,2A4,1X,7(2X,2A4))
C
C  RECORD LOCATIONS
      WRITE (IPR,50) IARRAY(1)
50    FORMAT ('0DEFINED AT RECORD ',I5,' IN FILE FCSEGSTS')
      WRITE (IPR,60) IARRAY(18)
60    FORMAT (' DEFINED AT RECORD ',I5,' IN FILE FCPARAM')
      WRITE (IPR,70) IARRAY(19)
70    FORMAT (' DEFINED AT WORD OFFSET ',I6,' IN FILE FCCARRY')
C
C  ARRAY SIZES
      WRITE (IPR,80) (IARRAY(I),I=37,41)
80    FORMAT ('0ARRAY SIZES: ',
     *  'C=',I6,3X,'D=',I6,3X,'T=',I6,3X,'TS=',I6,3X,'P=',I6)
      WRITE (IPR,90) IARRAY(65)
90    FORMAT ('0NEXT UNUSED POSITION IN D ARRAY = ',I5)
      WRITE (IPR,100) IARRAY(42)
100   FORMAT ('0NUMBER OF OPERATIONS THAT SAVE CARRYOVER = ',I3)
C
      WRITE (IPR,110) IARRAY(22),IARRAY(23)
110   FORMAT ('0CARRYOVER GROUP = ',2A4,' (BLANK IF NONE)')
      WRITE (IPR,120) IARRAY(20),IARRAY(21)
120   FORMAT ('0FORECAST  GROUP = ',2A4,' (BLANK IF NONE)')
C
      LAT=IARRAY(35)
      LONG=IARRAY(36)
      WRITE (IPR,130) XLAT,XLONG
130   FORMAT ('0LATITUDE = ',F6.2,3X,'LONGITUDE = ',F7.2)
      WRITE (IPR,140) IARRAY(34)
140   FORMAT ('0MINIMUM TIME STEP SEGMENT CAN BE RUN = ',I2,' HOURS')
C
      RETURN
C
      END
