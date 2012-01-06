C MEMBER FRCOPT
C  (from old member FCFRCOPT)
C **********************************************************************
C
      SUBROUTINE FRCOPT(INIT,ENGUNT,OPTN)
C
C SUBROUTINE TO CHECK A FIELD FOR DEFRC TO SEE WHAT UNITS
C   OPTION IS SELECTED; CAN BE 'ENGL' OR 'METR' OR
C   'E' OR 'M' FOR ENGLISH OR METRIC.
C ORIGINALLY BY ED VANBLARGAN - HRL - JULY,1983
C
C ARGUMENT LIST:
C INIT  - INPUT  - START LOCATION OF FIELD IN IBUF ARRAY
C ENGUNT- OUTPUT - UNITS USED (LOGICAL VARIABLE)
C                    =TRUE FOR ENGL   =FALSE FOR METR
C OPTN  - OUTPUT - UNITS USED (ALPHA VARIABLE)
C                    =ENGL  OR = METR
C
      LOGICAL ENGUNT
      INCLUDE 'ufreei'
      INCLUDE 'common/frcers'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/frcopt.f,v $
     . $',                                                             '
     .$Id: frcopt.f,v 1.1 1995/09/17 18:55:03 dws Exp $
     . $' /
C    ===================================================================
C
      DATA ENGL,CMET,IE,M/4HENGL,4HMETR,1HE,1HM/
C
      ENGUNT=.FALSE.
      GO TO (720,710,710,750),NCHAR
C
C ERROR
C
710   CALL FRCERR(2)
      GO TO 800
C
C NCHAR IS = 1; THEREFORE CHECK FOR 'E' OR 'M'
C
720   IF (IBUF(INIT).NE.IE) GO TO 730
      ENGUNT=.TRUE.
      GO TO 800
C..METRIC
730   IF (IBUF(INIT).NE.M) CALL FRCERR(3)
      GO TO 800
C
C NCHAR=4;THEREFORE CHECK FOR ENGL OR METR
C
750   CALL UPACK1(IBUF(INIT),OPTN,NCHAR)
      IF (OPTN.EQ.CMET) GO TO 800
      IF (OPTN.NE.ENGL) CALL FRCERR(3)
      ENGUNT=.TRUE.
C
800   OPTN=CMET
      IF (ENGUNT) OPTN=ENGL
C
      RETURN
      END
