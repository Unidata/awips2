C MEMBER SMARE2
C  (from old member SMAREA)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/25/95.12:23:50 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SMARE2 (LOPTN,OPTN,NMAP,NFMAP,NMAT,NMAPE,NMARO,NMAPX)
C
C  ROUTINE TO PRINT STATISTICS FOR AREAS PROCESSED BY DUMP AREA
C
      DIMENSION OPTN(*)
C
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smare2.f,v $
     . $',                                                             '
     .$Id: smare2.f,v 1.1 1995/09/17 19:12:34 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (LOPTN.EQ.0) THEN
         IF (NMAP.GT.0) THEN
            WRITE (LP,20) NMAP,OPTN(2)
            CALL SULINE (LP,2)
            ENDIF
         IF (NFMAP.GT.0) THEN
            WRITE (LP,20) NFMAP,OPTN(3)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMAT.GT.0) THEN
            WRITE (LP,20) NMAT,OPTN(4)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMAPE.GT.0) THEN
            WRITE (LP,20) NMAPE,OPTN(5)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMARO.GT.0) THEN
            WRITE (LP,20) NMARO,OPTN(6)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMAPX.GT.0) THEN
            WRITE (LP,20) NMAPX,OPTN(7)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMAP.EQ.0) THEN
            WRITE (LP,30) OPTN(2)
            CALL SULINE (LP,2)
            ENDIF
         IF (NFMAP.EQ.0) THEN
            WRITE (LP,30) OPTN(3)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMAT.EQ.0) THEN
            WRITE (LP,30) OPTN(4)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMAPE.EQ.0) THEN
            WRITE (LP,30) OPTN(5)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMARO.EQ.0) THEN
            WRITE (LP,30) OPTN(6)
            CALL SULINE (LP,2)
            ENDIF
         IF (NMAPX.EQ.0) THEN
            WRITE (LP,30) OPTN(7)
            CALL SULINE (LP,2)
            ENDIF
         GO TO 10
         ENDIF
C
      IF (LOPTN.EQ.2.AND.NMAP.GT.0) THEN
         WRITE (LP,20) NMAP,OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.3.AND.NFMAP.GT.0) THEN
         WRITE (LP,20) NFMAP,OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.4.AND.NMAT.GT.0) THEN
         WRITE (LP,20) NMAT,OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.5.AND.NMAPE.GT.0) THEN
          WRITE (LP,20) NMAPE,OPTN(LOPTN)
          CALL SULINE (LP,2)
          ENDIF
      IF (LOPTN.EQ.6.AND.NMARO.GT.0) THEN
         WRITE (LP,20) NMARO,OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.7.AND.NMAPX.GT.0) THEN
         WRITE (LP,20) NMAPX,OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.2.AND.NMAP.EQ.0) THEN
         WRITE (LP,30) OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.3.AND.NFMAP.EQ.0) THEN
         WRITE (LP,30) OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.4.AND.NMAT.EQ.0) THEN
         WRITE (LP,30) OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.5.AND.NMAPE.EQ.0) THEN
         WRITE (LP,30) OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.6.AND.NMARO.EQ.0) THEN
         WRITE (LP,30) OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.7.AND.NMAPX.EQ.0) THEN
         WRITE (LP,30) OPTN(LOPTN)
         CALL SULINE (LP,2)
         ENDIF
C
10    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT ('0*** NOTE - ',I4,' AREAS WITH ',A,' PARAMETERS ',
     *   'DEFINED WERE PROCESSED.')
30    FORMAT ('0*** NOTE - NO   AREAS WITH ',A,' PARAMETERS ',
     *   'WERE PROCESSED.')
C
      END
