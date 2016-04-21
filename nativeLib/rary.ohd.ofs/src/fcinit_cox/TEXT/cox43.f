C     MODULE  FCCOX43     VERSION 1      
c  =====================================================================
c  pgm:  cox43 (pold,cold,ponew,conew)
c
c   in: pold   .... old parameter array
c   in: cold   .... old carryover array
c  i/o: ponew  .... new parameter array
c  i/o: conew  .... new carryover array
c  =====================================================================
      SUBROUTINE COX43(POLD,COLD,PONEW,CONEW)

C#######################################################################

C  THIS ROUTINE TRANSFERS OLD CARRYOVER VALUES FROM THE COLD ARRAY
C  INTO THE NEW CARRYOVER ARRAY CONEW WHEN A SEGMENT IS REDEFINED.
C  BEFORE THIS TRANSFER TAKES PLACE, HOWEVER, A CHECK IS MADE TO SEE
C  IF EITHER THE COMPUTATIONAL TIME STEP INTERVAL AND/OR THE NEW STORM
C  WINDOW HAS CHANGED.  IF SO, ADJUSTMENTS ARE MADE TO THE RNSP ARRAY.

C#######################################################################

C  FOR DEFINITION OF VARIABLES STORED IN THE CO AND PO ARRAYS,
C  SEE THE PIN43 ROUTINE.

C#######################################################################
c  Initially written by
c     Ken Mack    NERFC                             8/10/95
c     Tim Sweeney  HRL                              October 1995
c.......................................................................
c
      include 'common/fdbug'
      include 'common/ionum'
c
c      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
c      COMMON /IONUM/ IN,IPR,IPU
c
      DIMENSION POLD(*),COLD(*),PONEW(*),CONEW(*),SUBNAM(2)
      DIMENSION RNSP(24)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox43.f,v $
     . $',                                                             '
     .$Id: cox43.f,v 1.1 1996/03/21 16:00:39 page Exp $
     . $' /
C    ===================================================================
C
c
      DATA SUBNAM/4HCOX4,4H3   /,NOP/43/

C  CALL DEBUG CHECK ROUTINE.

      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)

C  EXTRACT NEEDED VARIABLES FROM OLD AND NEW PARAMETER ARRAYS.

      IDELTO = POLD(24)
      NSWO   = POLD(25)
      NSPERO = POLD(26)
      VERS   = PONEW(1)
      IDELTN = PONEW(24)
      NSWN   = PONEW(25)
      NSPERN = PONEW(26)
      IF(IFDEB)50,50,4050

C  READ IN VARIABLES FROM OLD CARRYOVER FILE.

50    TAPI  = COLD(1)
      TATI  = COLD(2)
      TRI   = COLD(3)
      SAPI  = COLD(4)
      SATI  = COLD(5)
      SRI   = COLD(6)
      SRAIM = COLD(7)
      SRO   = COLD(8)
      DRAIM = COLD(9)
      DRO   = COLD(10)
      DO 100 I=1,NSPERO
100   RNSP(I) = COLD(10+I)
      if(ifdeb) 150,150,4170

C  CHECK TO SEE IF THE COMPUTATIONAL TIME STEP INTERVAL AND THE
C  NEW STORM WINDOW HAVE BEEN CHANGED.  IF NOT, STORE THE
C  OLD CARRYOVER VALUES IN THE NEW CARRYOVER ARRAY.  OTHERWISE,
C  MAKE ADJUSTMENTS TO THE RNSP ARRAY.

150   IDELTD=IDELTN-IDELTO
      IDELW=NSWN-NSWO
      IF((IDELTD.EQ.0).AND.(IDELW.EQ.0))GO TO 900

C  IF THE COMPUTATIONAL TIME STEP INTERVAL HAS CHANGED, SET ALL
C  VALUES IN THE RNSP ARRAY TO ZERO AND PRINT A WARNING MESSAGE
C  TO THIS EFFECT.

300   IF(IDELTD)500,310,500

C  IF THE NSW HAS INCREASED, FILL THE NEEDED EXTRA SPACES IN
C  THE RNSP ARRAY TO ZERO.

310   IF(IDELW)410,900,320
320   I1=NSPERO+1
      DO 330 I=I1,NSPERN
330   RNSP(I)=0.0
      GO TO 5330

C  IF THE NSW HAS DECREASED, SET EXTRANEOUS NON-ZERO VALUES
C  IN THE RNSP ARRAY TO ZERO.

410   I1=NSPERN+1
      DO 430 I=I1,NSPERO
430   RNSP(I)=0.0
      GO TO 5430

C  IF THERE ARE ANY CHANGES IN THE COMPUTATIONAL TIME STEP INTERVAL,
C  REGARDLESS OF ANY CHANGES WHICH MAY HAVE OCCURRED IN THE
C  NEW STORM WINDOW, SET ALL VALUES IN THE RNSP ARRAY TO ZERO.
C  THEN PRINT OUT A WARNING MESSAGE TO THIS EFFECT.

500   DO 530 I=1,NSPERN
530   RNSP(I)=0.0
      GO TO 5530

C  NOW PLACE VARIABLES INTO NEW CARRYOVER ARRAY.

900   CONEW(1)=TAPI
      CONEW(2)=TATI
      CONEW(3)=TRI
      CONEW(4)=SAPI
      CONEW(5)=SATI
      CONEW(6)=SRI
      CONEW(7)=SRAIM
      CONEW(8)=SRO
      CONEW(9)=DRAIM
      CONEW(10)=DRO
      DO 930 I=1,NSPERN
930   CONEW(10+I)=RNSP(I)
      IF(IFDEB)1000,1000,4200
1000  RETURN

C#######################################################################

C  DEBUG WRITE STATEMENTS.
C
C#######################################################################

4050  WRITE(IODBUG,8050)SUBNAM,VERS
      WRITE(IODBUG,8060)
      WRITE(IODBUG,8151)(POLD(I),I=2,11)
      WRITE(IODBUG,8152)(POLD(I),I=12,18)
      WRITE(IODBUG,8153)(POLD(I),I=22,27)
      WRITE(IODBUG,8160)
      WRITE(IODBUG,8151)(PONEW(I),I=2,11)
      WRITE(IODBUG,8152)(PONEW(I),I=12,18)
      WRITE(IODBUG,8153)(PONEW(I),I=22,27)
      goto 50
4170  WRITE(IODBUG,8170)
      WRITE(IODBUG,8180)(COLD(I),I=1,10)
      WRITE(IODBUG,8185)
      WRITE(IODBUG,8190) (I,RNSP(I),I=1,NSPERO)
      GOTO 150
4200  WRITE(IODBUG,8200)
      WRITE(IODBUG,8180)(CONEW(I),I=1,10)
      WRITE(IODBUG,8185)
      WRITE(IODBUG,8190) (I,RNSP(I),I=1,NSPERN)
      GO TO 1000

C#######################################################################

C  WARNING WRITE STATEMENTS.

C#######################################################################

5330  WRITE(IPR,9330)
      CALL WARN
      GO TO 900
5430  WRITE(IPR,9430)
      CALL WARN
      GO TO 900
5530  WRITE(IPR,9530)
      CALL WARN
      GO TO 900

C#######################################################################
C
C  DEBUG FORMAT STATEMENTS.
C
C#######################################################################

8050  FORMAT(/5X,2A4,' DEBUG OUTPUT.',5X,'VERSION: ',f5.2)
8060  FORMAT(/27X,'CONTENTS OF THE POLD ARRAY')
8151  FORMAT(/29X,'RID   = ',2A4,/29X,'RNAME = ',5A4,
     1      /29X,'IRNUM = ',I4,/29X,'RLAT  = ',F5.2,
     2      /29X,'RLNG  = ',F5.2)
8152  FORMAT(/29X,'RFCTR = ',F5.3,/29X,'R24   = ',F5.3,
     1      /29X,'PMAX  = ',F5.3,/29X,'ULIMW = ',F5.1,
     2      /29X,'BLIMW = ',F5.1,/29X,'ULIMS = ',F5.1,
     3      /29X,'BLIMS = ',F5.1)
8153  FORMAT(/29X,'NREL  = ',I4,/29X,'IDELTA= ',I4,
     1      /29X,'NSW   = ',I4,/29X,'NSPER = ',I4,
     2      /29X,'IUSEC = ',I4,/29X,'IOFAAA= ',I4)
8160  FORMAT(/27X,'CONTENTS OF THE PONEW ARRAY')
8170  FORMAT(/27X,'CONTENTS OF THE COLD ARRAY')
8180  FORMAT(/33X,'TAPI  = ',F5.2,/33X,'TATI  = ',F4.1,
     1      /33X,'TRI   = ',F5.2,/33X,'SAPI  = ',F5.2,
     2      /33X,'SATI  = ',F4.1,/33X,'SRI   = ',F5.2,
     3      /33X,'SRAIM = ',F5.2,/33X,'SRO   = ',F5.2,
     4      /33X,'DRAIM = ',F5.2,/33X,'DRO   = ',F5.2)
8185  FORMAT(/34X,'I',4X,'RNSP(I)')
8190  FORMAT(24(32X,I4,4X,F5.2,/))
8200  FORMAT(/27X,'CONTENTS OF THE CONEW ARRAY')

C#######################################################################

C  WARNING FORMAT STATEMENTS.

C#######################################################################

9330  FORMAT(/10X,'** WARNING ** THE NUMBER OF HOURS IN THE NEW STORM ',
     1      'WINDOW',/15X,'HAS BEEN INCREASED DURING SEGMENT ',
     2      'REDEFINITION, BUT THE COMPUTATIONAL TIME STEP INTERVAL',
     3      /15X,'HAS REMAINED CONSTANT.  THE NUMBER OF RAIN/MELT ',
     4      'VALUES WITHIN THE NEW STORM WINDOW',/15X,
     5      'OF THE NEW CARRYOVER ARRAY HAS BEEN EXPANDED WITH ZEROS ',
     6      'TO ALLOW FOR THIS INCREASED WINDOW SIZE.',/15X,
     7      'THE API OPERATION SHOULD NOW BE EXECUTED STARTING WITH ',
     8      'THE OLDEST AVAILABLE DAY OF CARRYOVER',/15X,'TO AVOID ',
     9      'INCONSISTENCIES IN RUNOFF CALCULATIONS.')
9430  FORMAT(/10X,'** WARNING ** THE NUMBER OF HOURS IN THE NEW STORM ',
     1      'WINDOW',/15X,'HAS BEEN DECREASED DURING SEGMENT ',
     2      'REDEFINITION, BUT THE COMPUTATIONAL TIME STEP INTERVAL ',
     3      /15X,'HAS REMAINED CONSTANT.  THE NUMBER OF RAIN/MELT ',
     4      'VALUES WITHIN THE NEW STORM WINDOW OF THE',/15X,'NEW ',
     5      'CARRYOVER ARRAY HAS BEEN TRUNCATED TO COMPENSATE FOR ',
     6      'THIS DECREASED WINDOW SIZE.',/15X,'NO ERRORS SHOULD ',
     7      'OCCUR IN RUNOFF COMPUTATIONS AS A RESULT OF THIS ACTION.')
9530  FORMAT(/10X,'** WARNING ** THE COMPUTATIONAL TIME STEP INTERVAL ',
     1      'HAS BEEN CHANGED',/15X,'DURING SEGMENT REDEFINITION.  ',
     2      'RAIN/MELT VALUES WITHIN THE NEW STORM WINDOW STORED IN ',
     3      /15X,'THE NEW CARRYOVER ARRAY HAVE BEEN SET TO ZERO TO ',
     4      'COMPENSATE FOR THESE CHANGES.',/15X,'THE API OPERATION ',
     5      'MUST NOW BE EXECUTED STARTING WITH THE OLDEST DAY OF ',
     6      'AVAILABLE',/15X,'CARRYOVER TO AVOID ERRORS IN RUNOFF ',
     7      'CALCULATIONS.')
      END
