C MEMBER RANDU
C  (from old member OARSCH)
C*    ******************************************************************
      SUBROUTINE RANDU(IX,IY,Z)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/randu.f,v $
     . $',                                                             '
     .$Id: randu.f,v 1.1 1996/07/11 21:01:12 dws Exp $
     . $' /
C    ===================================================================
C
C*    ******************************************************************
C*
C*    *** SUBROUTINE GENERATES UNIFORMLY DISTRIBUTED RANDOM NUMBERS
C*        BETWEEN 0.0 AND 1.0
C*
C*        PARAMETERS:
C*           INPUT: IX - FOR THE FIRST ENTRY IT MUST CONTAIN ANY ODD
C*                       INTEGER NUMBER WITH NINE OR LESS DIGITS. AFTER
C*                       THE FIRST ENTRY, IX SHOULD BE THE PREVIOUS
C*                       VALUE OF IY COMPUTED BY THIS SUBROUTINE
C*           OUTPUT:IY - A RESULTANT INTEGER RANDOM NUMBER REQUIRED FOR
C*                       THE NEXT ENTRY TO THIS SUBROUTINE
C*                  Z  - THE RESULTANT REAL RANDOM NUMBER
C*
C*        REMARKS: FOR REFERANCE SEE IBM/SSP VERSION III
C*
      IY=IX*65539
      IF(IY)10,10,20
   10 IY=IY+2147483647+1
   20 Z=IY
      Z=Z*0.4656613E-9
      RETURN
      END
