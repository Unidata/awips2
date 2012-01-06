C MODULE UPRCRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT THE INPUT CARD FROM THE FREE FORMAT COMMON BLOCK.
C
      SUBROUTINE UPRCRD (NPUNIT)
C
      INCLUDE 'ucmdbx'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/uprcrd.f,v $
     . $',                                                             '
     .$Id: uprcrd.f,v 1.2 1998/07/06 13:22:00 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER UPRCRD -',
     *      ' NPUNIT=',NPUNIT,
     *      ' IPRTCD=',IPRTCD,
     *      ' ICDSPC=',ICDSPC,
     *      ' NRDCRD=',NRDCRD
         ENDIF
C
C  CHECK IF CARD HAS ALREADY BEEN PRINTED
      IF (IPRTCD.EQ.0) THEN
         NSPACE=ICDSPC
         IFORM=1
         CALL UPRCR2 (NSPACE,IFORM,NPUNIT,ICDBUF,NRDCRD)
         IPRTCD=1
         ENDIF
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT  UPRCRD'
         ENDIF
C
      RETURN
C
      END
