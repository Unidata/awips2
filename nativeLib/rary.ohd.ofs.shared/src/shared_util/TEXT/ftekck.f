C MODULE FTEKCK
C-----------------------------------------------------------------------
C
      SUBROUTINE FTEKCK (IVALUE,TECHNM,IDEFLT,IVALIN,ILOWER,IUPPER)
C
C  THIS ROUTINE CHECKS IF THE VALUE OF A TECHNIQUE IS WITHIN THE 
C  SPECIFIED LIMITS.
C
C  ARGUMENT LIST:
C
C    VARIABLE     I/O     DEFINITION
C    --------     ---     ----------
C    IVALUE        O      VALUE RETURNED FOR VARIABLE
C    TECHNM        I      8-CHARACTER TECHNIQUE NAME
C    IDEFLT        I      DEFAULT VALUE FOR TECHNIQUE
C    IVALIN        I      TECHNIQUE VALUE FROM HCL
C    ILOWER        I      LOWER LIMIT OF VALID RANGE FOR TECHNIQUE
C    IUPPER        I      UPPER LIMIT OF VALID RANGE FOR TECHNIQUE
C
      CHARACTER*8 TECHNM
C
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/ftekck.f,v $
     . $',                                                             '
     .$Id: ftekck.f,v 1.2 2001/06/13 13:38:54 dws Exp $
     . $' /
C    ===================================================================
C
C
      IVALUE=IVALIN
      IOIFPR=IVALUE
C
      IF (IVALIN.LT.ILOWER.OR.IVALIN.GT.IUPPER) THEN
C     VALUE IS NOT WITHIN VALID RANGE
         IVALUE=IDEFLT
         IF (TECHNM.EQ.'FUTPRECP') THEN
            IF (IOIFPR.LT.ILOWER) IVALUE=ILOWER
            IF (IOIFPR.GT.IUPPER) IVALUE=IUPPER
            ENDIF
         WRITE (IPR,10) TECHNM,ILOWER,IUPPER,IOIFPR,IVALUE
10    FORMAT ('0**WARNING** THE VALUE OF TECHNIQUE ',A,
     *  ' IS OUTSIDE ITS VALID RANGE OF ',I5,' TO ',I5,'.' /
     *  12X,'THE VALUE ENTERED IS ',I5,
     *  '. A DEFAULT VALUE OF ',I5,' HAS BEEN USED INSTEAD.')
         CALL WARN
         ENDIF
C
      RETURN
C
      END
