C MODULE QISTFT
C-----------------------------------------------------------------------
C
C  ROUTINE QISTFT DETERMINES THE HOUR OF THE FIRST FUTURE OBSERVATION.
C
C  ORIGINALLY CODED BY DEBBIE VAN DEMARK - 9/21/84
C
C-----------------------------------------------------------------------
C
C  INPUT ARGUMENTS
C
C  OUTPUT ARGUMNENTS
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QISTFT (STAID,DTYPE,OBS,IOBS,NVLPOB,NCOUNT,LSTHR,
     *   ISTFUT)
C
      DIMENSION STAID(2),OBS(1),IOBS(1)
      DIMENSION OLDOPN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
C  USE INPTZC FROM FCTIM2 FOR TIME ZONE CODE
C
      INCLUDE 'common/pptime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rrs/RCS/qistft.f,v $
     . $',                                                             '
     .$Id: qistft.f,v 1.3 1999/07/06 15:56:02 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPTRCE.GT.1) WRITE (IOPDBG,30)
C      
      IOPNUM=-3
      CALL FSTWHR ('QISTFT  ',IOPNUM,OLDOPN,IOLDOP)
C
C  CHECK DEBUG CODES
      IBUG=IPBUG(4HQFUT)
C
C  ADD THE DATA BASE OFFSET TO LSTHR
      LSTHR=LSTHR+NHOPDB
      DO 10 I=1,NCOUNT,NVLPOB
         IF (IOBS(I).LE.LSTHR) GO TO 10
            ISTFUT=IOBS(I)
            GO TO 20
10       CONTINUE
C
20    IF (IBUG.GT.0) WRITE (IOPDBG,40) STAID,DTYPE,ISTFUT
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
      IF (IPTRCE.GT.1) WRITE (IOPDBG,50)
C      
      RETURN
C
30    FORMAT('0*** ENTER QISTFT')
40    FORMAT(' THE HOUR OF THE FIRST FUTURE OBS FOR STATION ',2A4,
     $ ' AND DATA TYPE ',A4,' IS ',I8)
50    FORMAT('0*** EXIT - QISTFT')
C
      END
