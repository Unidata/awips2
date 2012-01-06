C MODULE FARGCK
C-----------------------------------------------------------------------
C
      SUBROUTINE FARGCK (IVALUE,TECHNM,ARGNM,IDEFLT,IVALIN,ILOWER,
     *   IUPPER)
C
C  THIS ROUTINE CHECKS IF THE VALUE OF AN INTEGER ARGUMENT IS WITHIN 
C  THE SPECIFIED LIMITS.
C
C  ARGUMENT LIST:
C
C    VARIABLE     I/O     DEFINITION
C    --------     ---     ----------
C    IVALUE        O      VALUE RETURNED FOR VARIABLE
C    TECHNM        I      8-CHARACTER TECHNIQUE NAME
C    ARGNM         I      8-CHARACTER ARGUMENT NAME
C    IDEFLT        I      DEFAULT VALUE FOR ARGUMENT
C    IVALIN        I      ARGUMENT VALUE FROM HCL
C    ILOWER        I      LOWER LIMIT OF VALID RANGE FOR ARGUMENT
C    IUPPER        I      UPPER LIMIT OF VALID RANGE FOR ARGUMENT
C
      CHARACTER*8 TECHNM,ARGNM
C
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/fargck.f,v $
     . $',                                                             '
     .$Id: fargck.f,v 1.2 2001/06/13 13:39:51 dws Exp $
     . $' /
C    ===================================================================
C
C
      IVALUE=IVALIN
C
      IF (IVALIN.LT.ILOWER.OR.IVALIN.GT.IUPPER) THEN
C     VALUE IS NOT WITHIN VALID RANGE
         IVALUE=IDEFLT
         WRITE (IPR,10) ARGNM,TECHNM,ILOWER,IUPPER,IVALIN,IVALUE
10    FORMAT ('0**WARNING** THE VALUE OF ARGUMENT ',A,
     *  ' FOR TECHNIQUE ',A,
     *  ' IS OUTSIDE ITS VALID RANGE OF ',I5,' TO ',I5,'.' /
     *  12X,'THE VALUE ENTERED IS ',I5,
     *      '. A DEFAULT VALUE OF ',I5,' HAS BEEN USED INSTEAD.')
         CALL WARN
         ENDIF
C
      RETURN
C
      END
