C  MEMBER FCBLOC07
C ----------------------------------------
      BLOCK DATA FCBLOC07
C.......................................................................
C
C     COMMON BLOCK FATLGK DETERMINES WHICH OF THE TWO METHODS IS USED.
C     IF IATL=0, THE MCP2.0 METHOD IS USED.
C     IF IATL=1, THE ATLANTA RFC METHOD IS USED.
C       IF THE ATLANTA RFC METHOD IS USED TWO ADDITIONAL VALUES ARE
C       REQUIRED TO SPECIFY THE DELTA O INCREMENTS USED IN COMPUTING
C       THE 2*S/DT+O VS O TABLE.  THESE VALUES ARE C1 AND C2.
C
C     IATL,C1,AND C2 ARE SET IN BLOCK DATA ROUTINES FOR
C       MCP 3.0, OPT 3.0, ESP 3.0, AND VERSION 5.0 OF
C       THE NWSRFS OPERATIONAL PROGRAM.
C.......................................................................
C
      COMMON /FATLGK/ IATL,C1,C2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSFCBLOC07      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/fcbloc07.f,v $
     . $',                                                             '
     .$Id: fcbloc07.f,v 1.1 1997/06/24 15:56:52 page Exp $
     . $' /
C    ===================================================================
C

      DATA  IATL, C1, C2 / 1, 12.0, 100.0 /

      END
