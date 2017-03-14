C MODULE SUPCLS
C-----------------------------------------------------------------------
C
C  ROUTINE TO CLOSE THE FILE IN WHICH A PAMAMETER TYPE IS LOCATED.
C
      SUBROUTINE SUPCLS (NTYPES,TYPES,ISTAT)
C
      DIMENSION TYPES(NTYPES)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/supcls.f,v $
     . $',                                                             '
     .$Id: supcls.f,v 1.2 2001/06/13 14:06:53 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,*) 'ENTER SUPCLS'
         CALL SULINE (IOSDB,1)
         ENDIF
C
      LDEBUG=ISBUG(4HUTIL)
C
      ISTAT=0
C
      IF (NTYPES.EQ.0) GO TO 30
C
      DO 20 I=1,NTYPES
         INDEX=IPCKDT(TYPES(I))
         IF (INDEX.GT.0) GO TO 10
            ISTAT=1
            GO TO 20
10       IUNIT=KPPRMU(IPDTDR(2,INDEX))
         CALL UCLOST (IUNIT)
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'IUNIT=',IUNIT
             CALL SULINE (LP,1)
             ENDIF
20       CONTINUE
C
30    IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,*) 'EXIT SUPCLS'
         CALL SULINE (IOSDBG,2)
         ENDIF
C
      RETURN
C
      END
