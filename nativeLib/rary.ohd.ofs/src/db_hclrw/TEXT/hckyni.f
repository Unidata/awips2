C MEMBER HCKYNI
C  (from old member HCLCK2)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/02/95.14:08:16 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCKYNI (ILP,IFIELD,IOPTRC,NXOPT,ISTAT)
C
C ROUTINE TO CHECK FOR A YES, NO OR INTEGER AFTER THE TECHNIQUE NAME
C IN A DEFINE OPTION OR OPTION COMMAND.
C
C            ARG     TYPE   IO    DIM   DESC
C
C           IFIELD     I    IO      1     FIELD TO CHECK FOR Y N OR INT
C                                          INCREMENTED IF FOUND
C           IOPTRC      I   IO      ?     OPTION RECORD TO ENTER VALUE
C           NXOPT       I   IO      1     LAST USED WORD IN IOPTRC
C           ISTAT       I    O      1     STATUS, 0=OK, 1=ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'hclcommon/hwords'
C
      DIMENSION IOPTRC(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hckyni.f,v $
     . $',                                                             '
     .$Id: hckyni.f,v 1.2 1995/11/14 19:20:47 erb Exp $
     . $' /
C    ===================================================================
C
C
C
      ISET=1
C
C GET THE LENGTH OF FIELD
      J=ILP+1
      N=IFSTOP(IFIELD)-J+1
      IF (N.GT.1) GO TO 10
      IF (IBUF(J).EQ.LETY) GO TO 40
      IF (IBUF(J).NE.LETN) GO TO 10
      ISET=0
      GO TO 40
C
C  CHECK IF INTEGER
10    IERR=0
      CALL UCKINT (IBUF,J,IFSTOP(IFIELD),IERR)
      IF (IERR.EQ.0) GO TO 30
      WRITE (LP,20) IFIELD
20    FORMAT ('0**ERROR** INVALID TECHNIQUE VALUE IN FIELD ',I3,'.')
      ISTAT=1
      GO TO 40
C
30    CALL UNUMIC (IBUF,J,IFSTOP(IFIELD),ISET)
C
40    IFIELD=IFIELD+1
      NXOPT=NXOPT+1
      IOPTRC(NXOPT)=ISET
C
      IF (IHCLDB.GT.0) WRITE (LPD,60) IFIELD,ISET,NXOPT
60    FORMAT (' IN HCKYNI - IFIELD=',I3,' ISET=',I8,' NXOPT=',I3)
C
      RETURN
C
      END
