C MODULE IPDFDT
C-----------------------------------------------------------------------
C
      FUNCTION IPDFDT (ISIREC,IDATYP)
C
C  ROUTINE TO FIND THE POINTER OR RECORD NUMBER OF A DATA TYPE IN A
C  STATION INFORMATION RECORD.
C
      INTEGER*2 ISIREC(*)
C
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/ipdfdt.f,v $
     . $',                                                             '
     .$Id: ipdfdt.f,v 1.2 2002/02/11 19:53:29 dws Exp $
     . $' /
C    ===================================================================
C
      DATA LPP24/4HPP24/
      DATA LTM24/4HTM24/
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) 'ENTER IPDFDT'
C
      IX=0
      IF (IDATYP.EQ.LPP24) IX=ISIREC(8)
      IF (IDATYP.EQ.LTM24) IX=ISIREC(9)
      IF (IX.NE.0) GO TO 40
C
C  CHECK IF ANY ADDITIONAL DATA TYPES DEFINED
      IF (ISIREC(10).EQ.0) GO TO 40
C
      N=ISIREC(10)
      J=11
      DO 20 I=1,N
         CALL UCMPAR (ISIREC(J),IDATYP,1,IMATCH)
         IF (IMATCH.EQ.0) GO TO 30
         J=J+3
20       CONTINUE
      GO TO 40
C
30    IX=ISIREC(J+2)
C
40    IPDFDT=IX
C
      IF (IPDDB.GT.0) WRITE (IOGDB,50) IDATYP,IPDFDT
50    FORMAT (' EXIT IPDFDT - IDATYP=',A4,' IPDFDT=',I5)
C
      RETURN
C
      END
