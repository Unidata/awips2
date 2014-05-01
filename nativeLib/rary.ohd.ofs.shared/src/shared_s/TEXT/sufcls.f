C MODULE SUFCLS
C-----------------------------------------------------------------------
C
C  ROUTINE TO CLOSE ALL OPEN FILES.
C
      SUBROUTINE SUFCLS
C
      INCLUDE 'uiox'
      INCLUDE 'ufiles'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sufcls.f,v $
     . $',                                                             '
     .$Id: sufcls.f,v 1.2 2001/06/13 13:33:11 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUFCLS'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('UTIL')
C
      NFILES=0
C
C  CLOSE ALL FILES TO WRITE FROM BUFFER TO DATA FILE
      DO 10 NUNIT=1,MFILES
         IF (IFILES(NUNIT).EQ.1) THEN
            CALL UCLOST (NUNIT)
            NFILES=NFILES+1
            ENDIF
10       CONTINUE
C
      IF (NFILES.EQ.0) THEN
         WRITE (LP,30)
         CALL SULINE (LP,2)
         ENDIF
      IF (NFILES.GT.0) THEN
         WRITE (LP,40) NFILES
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUFCLS'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT ('0*** NOTE - NO FILES CLOSED BECAUSE NO FILES ARE ',
     *   'CURRENTLY OPEN.')
40    FORMAT ('0*** NOTE - ',I2,' FILES SUCCESSFULLY CLOSED.')
C
      END
