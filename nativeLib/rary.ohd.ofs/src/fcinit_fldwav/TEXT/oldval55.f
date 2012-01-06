      SUBROUTINE OLDVAL55(NVAL,IVAL,NPV,NN,NP)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
      DIMENSION    NVAL(NPV),IVAL(NPV),NN(NP)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/oldval55.f,v $
     . $',                                                             '
     .$Id: oldval55.f,v 1.2 2000/09/27 15:12:04 page Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'OLDVAL55' /

      CALL FPRBUG(SNAME, 1, 55, IBUG)

      DO 20 K=1,NPV
        DO 10 I=1,NP
          IF(NVAL(K).EQ.NN(I)) THEN
            IVAL(K)=I
            GO TO 20
          ENDIF
   10   CONTINUE
   20 CONTINUE

      RETURN
      END
