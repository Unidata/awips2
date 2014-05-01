C MODULE FCTBL22
C
       SUBROUTINE PRTD22 (IODBUG,IIMO,IIDA,IIYR,IIHR,
CKPG 12/2/97 ADDED PREDSD IN THE ARGUMENT LIST FOR FORECAST ST DEV
     +  MAP,PE,PREDD,PREDSD,OBSD,ESTD)
C
C***J.C.,890810
       COMMON/PFLAGS/ IFIRST, NCTIM
C
CKPG 12/2/97 ADDED PREDSD
       REAL MAP, PE, PREDD, PREDSD OBSD, ESTD
       INTEGER IFIRST
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/prtd22.f,v $
     . $',                                                             '
     .$Id: prtd22.f,v 1.4 2002/05/15 13:55:27 hank Exp $
     . $' /
C    ===================================================================
C
C
       IF (IFIRST .NE. 1) GOTO 801
           IFIRST = 0
           WRITE (IODBUG,111)
111        FORMAT(T46,'|----- DISCHARGE IN MM/DT -----|')
            WRITE (IODBUG,11)
11         FORMAT('DATE',T15,'HOUR',T25,'MAP',T36,'ET',
CKPG 12/2/97 ADDED LABEL FOR PREDSD
     +      T47,'PRED D',T58,'PRED STD',T69,'OBS D',T80,'EST D',/)
           RETURN
801    CONTINUE
C
CKPG 12/2/97 ADDED PREDSD AND CHANGED FORMAT
       WRITE (IODBUG,10) IIMO,IIDA,IIYR,IIHR,MAP,PE,PREDD,PREDSD,OBSD,
     + ESTD
10     FORMAT(I2.2,'/',I2.2,'/',I4,T15,I2.2,':00',
     +  T24,E10.3,T35,E10.3,T46,
     +  E10.3,T57,E10.3,T68,E10.3,T79,E10.3)
C
       RETURN
       END
