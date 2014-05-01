      SUBROUTINE IERX(KFILDO,IP1,IOS,RUTINE,STATMT) 
C 
C        JANUARY 1994   GLAHN   TDL  MOS-2000
C        APRIL   2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                    CONFORM TO FORTRAN 90 STANDARDS
C                                    ON THE IBM SP
C 
C        PURPOSE 
C            TO HANDLE AN ERROR INDICATED BY A SYSTEM SUBROUTINE, USUALLY 
C            AN I/O OPERATION.  DIAGNOSTIC IS WRITTEN TO UNIT NUMBER KFILDO.
C            WHEN IP1 NE KFILDO, THE DIAGNOSTIS IS ALSO WRITTEN TO UNIT
C            NUMBER IP1.
C 
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C            IP1    - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)   
C 
C        VARIABLES 
C 
C          INPUT 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                 IP1 = UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C                 IOS = INPUT/OUTPUT STATUS.
C              RUTINE = NAME OF CALLING SUBROUTINE. 
C                       MAXIMUM OF 6 CHARACTERS PRINTED.
C                       (CHARACTER*(*))
C              STATMT = STATEMENT NUMBER IN CALLING PROGRAM NEAR CALL 
C                       TO IERX.  MAXIMUM OF 4 CHARACTERS PRINTED.
C                       (CHARACTER*(*))
C 
C        NONSYSTEM SUBROUINES USED 
C            NONE. 
C 
      CHARACTER*(*) RUTINE,STATMT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/ierx.f,v $
     . $',                                                             '
     .$Id: ierx.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C 
      WRITE(KFILDO,100)IOS,RUTINE,STATMT 
      IF(IP1.NE.KFILDO)WRITE(IP1,100)IOS,RUTINE,STATMT 
 100  FORMAT(/,' ****SYSTEM STATUS CODE ERROR =',I5, 
     1         ' IN ROUTINE ',A6,' NEAR ',A4) 
      RETURN
      END 
