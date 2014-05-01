C MEMBER HPRIDS
C  (from old member HCLPROPT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/10/95.12:41:55 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HPRIDS (KBUF,IPOINT,ITYPE)
C
C
C          ROUTINE:  HPRIDS
C
C             VERSION:  1.0.0
C
C                DATE:  9-3-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO PRINT THE IDENTIFIERS FOR OPTION RECORDS
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       KBUF       I     I     ?    BUFFER CONTAINING IDENTIFIERS
C
C       IPOINT     I     O     1    POINTER TO POSITION IN KBUF
C
C       ITYPE      I     I     1    TYPE OF PRINT
C                                     1=START IN COLUMN 26
C                                     2=START IN COLUMN 18
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION KBUF(*),ITBUF(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hprids.f,v $
     . $',                                                             '
     .$Id: hprids.f,v 1.1 1995/09/17 18:42:48 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA ILC/4H<   /,IRC/4H>   /
C
C***********************************************************************
C
C
      IGPID=KBUF(2)
      CALL HGTGID (IGPID)
      IPOINT=3
      IPOS=IPOINT+1
      NUMIDS=KBUF(1)
      IF (ITYPE.EQ.1) THEN
         CALL ULINE (LP,1)
         WRITE (LP,10)
         ENDIF
      IF (ITYPE.EQ.2) THEN
         CALL ULINE (LP,1)
         WRITE (LP,20)
         ENDIF
10    FORMAT (24X,'IDENTIFIERS:')
20    FORMAT (16X,'IDENTIFIERS:')
C
      DO 170 I=1,NUMIDS
         IDKEY=KBUF(IPOINT)
         N=IABS(IDKEY)-1
         IF (IDKEY.EQ.0) GO TO 200
         IF (IDKEY.EQ.1) GO TO 70
         IF (IDKEY.GT.1) GO TO 100
         IF (IDKEY.EQ.-1) GO TO 130
C     HAVE A NUMBER OF IDS TO EXCLUDE
         IPOSS=IPOS
         K=1
         J=0
         DO 40 M=1,N
            IF (J.NE.0) GO TO 30
               ITBUF(K)=ILC
               K=K+1
30            CALL UMEMOV (KBUF(IPOSS),ITBUF(K),2)
              IPOSS=IPOSS+2
              K=K+2
              J=J+1
              IF (J.NE.5) GO TO 40
              ITBUF(K)=IRC
              IF (ITYPE.EQ.1) CALL ULINE (LP,1)
              IF (ITYPE.EQ.1) WRITE (LP,50) (ITBUF(MM),MM=1,K)
              IF (ITYPE.EQ.2) CALL ULINE (LP,1)
              IF (ITYPE.EQ.2) WRITE (LP,60) (ITBUF(MM),MM=1,K)
              K=1
              J=0
40            CONTINUE
          IF (J.EQ.0) GO TO 160
          ITBUF(K)=IRC
         IF (ITYPE.EQ.1) CALL ULINE (LP,1)
         IF (ITYPE.EQ.1) WRITE (LP,50) (ITBUF(MM),MM=1,K)
         IF (ITYPE.EQ.2) CALL ULINE (LP,1)
         IF (ITYPE.EQ.2) WRITE (LP,60) (ITBUF(MM),MM=1,K)
50       FORMAT (26X,A1,6(2A4,1X))
60       FORMAT (18X,A1,6(2A4,1X))
         GO TO 160
C     HAVE A RANGE TO INCLUDE
70       IPOINT=IPOINT+4
         IF (ITYPE.EQ.1) CALL ULINE (LP,1)
         IF (ITYPE.EQ.1) WRITE (LP,80) (KBUF(K),K=IPOS,IPOINT)
         IF (ITYPE.EQ.2) CALL ULINE (LP,1)
         IF (ITYPE.EQ.2) WRITE (LP,90) (KBUF(K),K=IPOS,IPOINT)
80       FORMAT (26X,2A4,'-',2A4)
90       FORMAT (18X,2A4,'-',2A4)
         IPOINT=IPOINT+1
         GO TO 160
C     HAVE A NUMBER OF IDS TO INCLUDE
100      IPOINT=IPOINT+2*N
         IF (ITYPE.EQ.1) CALL ULINE (LP,1)
         IF (ITYPE.EQ.1) WRITE (LP,110) (KBUF(K),K=IPOS,IPOINT)
         IF (ITYPE.EQ.2) CALL ULINE (LP,1)
         IF (ITYPE.EQ.2) WRITE (LP,120) (KBUF(K),K=IPOS,IPOINT)
110      FORMAT ((26X,5(2A4,1X)))
120      FORMAT ((18X,5(2A4,1X)))
         GO TO 160
C     HAVE A RANGE OF IDS TO EXCLUDE
130      IPOINT=IPOINT+4
         IF (ITYPE.EQ.1) CALL ULINE (LP,1)
         IF (ITYPE.EQ.1) WRITE (LP,140) (KBUF(K),K=IPOS,IPOINT)
         IF (ITYPE.EQ.2) CALL ULINE (LP,1)
         IF (ITYPE.EQ.2) WRITE (LP,150) (KBUF(K),K=IPOS,IPOINT)
140      FORMAT (26X,'<',2A4,'-',2A4,'>')
150      FORMAT (18X,'<',2A4,'-',2A4,'>')
         IPOINT=IPOINT+1
160      IPOINT=IPOINT+1
         IPOS=IPOINT+1
170      CONTINUE
C
      IF (ITYPE.EQ.1) CALL ULINE (LP,1)
      IF (ITYPE.EQ.1) WRITE (LP,180) IGPID
      IF (ITYPE.EQ.2) CALL ULINE (LP,1)
      IF (ITYPE.EQ.2) WRITE (LP,190) IGPID
180   FORMAT (24X,'GROUP IDENTIFIER=',A1)
190   FORMAT (16X,'GROUP IDENTIFIER=',A1)
C
200   IF (IHCLDB.GT.0) WRITE (IOGDB,210) IPOINT
210   FORMAT (' IN HPRIDS - IPOINT=',I3)
C
      RETURN
C
      END
