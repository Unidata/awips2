C MODULE UCNCAT
C-----------------------------------------------------------------------
C
      SUBROUTINE UCNCAT (STROL,STRAD,ISTAT)
C
C  ROUTINE UCNCAT CONCATENATES TWO CHARACTER STRINGS.
C
C  INPUT VARIABLES -
C     STROL - CHARACTER STRING TO BE CONCATENATED TO
C     STRAD - CHARACTER STRING TO BE CONCATENATED TO STROL
C
C  OUTPUT VARIABLES -
C     ISTAT  - STATUS CODE
C              0=NORMAL RETURN
C              1=STROL NOT LARGE ENOUGH
C
C
      CHARACTER*(*) STROL,STRAD
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ucncat.f,v $
     . $',                                                             '
     .$Id: ucncat.f,v 1.2 2001/06/13 10:42:50 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UCNCAT'
         ENDIF
C
      ISTAT=0
C
C  FIND LENGTH OF CHARACTER STRINGS
      CALL ULENTH (STROL,LEN(STROL),LSTROL)
      CALL ULENTH (STRAD,LEN(STRAD),LSTRAD)
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'LSTROL=',LSTROL,' STROL=',STROL
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'LSTRAD=',LSTRAD,' STRAD=',STRAD
         ENDIF
C
      IF (LSTRAD.EQ.0) GO TO 40
C
      IBEG=LSTROL+1
      IEND=IBEG+LSTRAD-1
C
C  CHECK IF STROL LARGE ENOUGH TO HOLD STRAD CHARACTERS
      MSTROL=LEN(STROL)
      IF (IEND.GT.MSTROL) THEN
         ISTAT=1
         GO TO 40
         ENDIF
C
      STROL(IBEG:IEND)=STRAD
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'IBEG=',IBEG,' IEND=',IEND,' STROL=',STROL
         ENDIF
C
40    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UCNCAT -',
     *      ' ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
      END
