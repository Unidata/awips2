      SUBROUTINE MATHQ55(YQ1,TPG,RHO,GAMA,YQI,K1)
C
C  THIS SUBROUTINE SETS UP MATHEMATICAL DISCHARGE OR STAGE
C  AT UPSTREAM END
C
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
C
      DIMENSION TPG(K1),RHO(K1),GAMA(K1),YQI(K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/mathq55.f,v $
     . $',                                                             '
     .$Id: mathq55.f,v 1.1 1999/04/23 18:08:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HMATH,4HQ55 /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      J=JJ
      ALPH=1./(GAMA(J)-1.)
      YQ1=YQI(J)*(1.+(RHO(J)-1.)*(TT/TPG(J))**ALPH*
     &EXP(ALPH*(1.-TT/TPG(J))))
      RETURN
      END

