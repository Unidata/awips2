C MODULE PRSTOP
C-----------------------------------------------------------------------
C
C  THIS IS THE PRINT SUBROUTINE FOR THE STOP OPERATION.
C
      SUBROUTINE PRSTOP (MP,MC,MT,MTS,MD,MINDT)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/flarys'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/prstop.f,v $
     . $',                                                             '
     .$Id: prstop.f,v 1.5 2002/02/11 19:03:47 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER PRSTOP'

      WRITE (IPR,20)
20    FORMAT ('0',60('*'))
      WRITE (IPR,30)
30    FORMAT ('0THIS IS THE END OF THE OPERATIONS TABLE FOR THIS ',
     *   'SEGMENT.')
      WRITE (IPR,20)
C
      WRITE (IPR,40) MINDT
40    FORMAT ('0THE MINIMUM TIME INTERVAL FOR WHICH THIS SEGMENT ',
     *   'CAN BE EXECUTED IS ',I2,' HOURS.')
C
      WRITE (IPR,50)
50    FORMAT ('0ACTUAL SPACE USED FOR THE P, C, T, TS AND D ARRAYS ',
     *   'FOR THIS SEGMENT:')
      WRITE (IPR,60) 'P ',LP,MP
60    FORMAT ('0',5X,A,' ARRAY ',I6,' OUT OF ',I6,' SPACES')
      WRITE (IPR,60) 'C ',LC,MC
      WRITE (IPR,60) 'T ',LT,MT
      WRITE (IPR,60) 'TS',LTS,MTS
      WRITE (IPR,60) 'D ',LD,MD
      WRITE (IPR,70)
70    FORMAT ('0(PRECEEDING MESSAGES WILL INDICATE IF MORE SPACE WAS ',
     *   'NEEDED FOR THE P, C AND T ARRAYS)')
C
      RETURN
C
      END
