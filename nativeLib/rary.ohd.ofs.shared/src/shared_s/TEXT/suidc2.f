C MODULE SUIDC2
C-----------------------------------------------------------------------
C
      SUBROUTINE SUIDC2 (CTYPE,IFOUND,ISTAT)
C
      CHARACTER*4 CTYPE
C
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/stypax'
      INCLUDE 'scommon/stypsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/suidc2.f,v $
     . $',                                                             '
     .$Id: suidc2.f,v 1.2 2001/06/13 14:00:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('SYS ','        ','SUIDC2  ',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,40) CTYPE,MATYPE,MSTYPE
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK FOR AREA KEYWORD
      DO 10 I=1,MATYPE
         IF (CTYPE.NE.ATYPE(I)) GO TO 10
            IFOUND=1
            ISTAT=2
            GO TO 30
10       CONTINUE
C
C  CHECK FOR STATION KEYWORD
      DO 20 I=1,MSTYPE
         IF (CTYPE.NE.STYPE(I)) GO TO 20
            IFOUND=1
            ISTAT=2
            GO TO 30
20       CONTINUE
C
30    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,50) IFOUND,ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' ENTER SUIDC2 : ',
     *   'CTYPE=',A4,3X,'MATYPE=',I2,3X,'MSTYPE=',I2)
50    FORMAT (' EXIT SUIDC2 : ',3X,
     *   'IFOUND=',I2,3X,'ISTAT=',I2)
C
      END
