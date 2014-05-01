C MODULE MODS1
C-----------------------------------------------------------------------
C
C
C
      SUBROUTINE MODS1 (NCARDS,ICMND,MODNAM,MODCRD,ITYPE)
C
      INCLUDE 'common/fmodft'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/fctime'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/modctl'
      INCLUDE 'ufreex'
C
      DIMENSION MODCRD(20,NCARDS)
C
      CHARACTER*8 MODNAM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mods1.f,v $
     . $',                                                             '
     .$Id: mods1.f,v 1.3 1998/07/02 20:45:48 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITYPE.EQ.1) WRITE(IPR,3001) MODNAM
      IF (ITYPE.EQ.2) WRITE(IPR,4001) MODNAM
      IF (ITYPE.EQ.3) WRITE(IPR,4501) MODNAM
      IF (ITYPE.EQ.4) WRITE(IPR,5001) MODNAM
      IF (ITYPE.EQ.5) WRITE(IPR,5501) MODNAM
      IF (ITYPE.EQ.6) WRITE(IPR,6001) MODNAM
      IF ((ITYPE.GE.1).AND.(ITYPE.LE.6)) THEN
         WRITE(IPR,3002) (MODCRD(I,NRDCRD),I=1,20)
         CALL WARN
      ENDIF
C
 3001 FORMAT(1H0,10X,'**WARNING** THE ',A8,' MOD WILL NOT ',
     *'BE ACTIVATED BECAUSE TEMPERATURE CHANGES ARE FOR ',
     */23X,'DATES AFTER LASTCMPDATE ON THE MOD ',
     *'CARD AND LASTCMPDATE IS NOT EQUAL TO LSTCMPDY ',
     *'FOR RUN.')
C
 3002 FORMAT(23X,'THE MOD CARD IMAGE IS: ',20A4)
C
 4001 FORMAT(1H0,10X,'**WARNING** THE ',A8,' MOD WILL NOT BE ',
     *'ACTIVATED BECAUSE THE DATE ON THE MOD CARD IS GREATER ',
     *'THAN LSTCMPDY VALUE.')
C
 4501 FORMAT(1H0,10X,'**WARNING** THE ',A8,' MOD WILL NOT BE ',
     *'ACTIVATED BECAUSE THE DATE ON THE MOD CARD IS NOT ',
     *'EQUAL TO THE LSTCMPDY VALUE.')
C
 5001 FORMAT(1H0,10X,'**WARNING** THE ',A8,' MOD WILL NOT BE ',
     *'ACTIVATED BECAUSE THE EFFECTIVE DATE ON THE MOD CARD ',
     *'IS NOT '/23X,'EQUAL LSTCMPDY VALUE.')
C
 5501 FORMAT(1H0,10X,'**WARNING** THE ',A8,' MOD WILL NOT BE ',
     *'ACTIVATED BECAUSE DATE ON MOD CARD IS NOT THE SAME DATE AT',
     *' START OF RUN.')
C
 6001 FORMAT(1H0,10X,'**WARNING** THE ',A8,' MOD WILL NOT BE ',
     *'ACTIVATED BECAUSE THE START DATE ON THE MOD CARD ',
     *'IS AFTER VALIDDATE')
C
      RETURN
      END
