c  =====================================================================
c  pgm:  dutext (po,iout)
c
c   in: po     .... parameter array
c   in: iout   .... output device
c  =====================================================================
c
      subroutine dutext (po,iout)
c
c.......................................................................
c
c  This routine dumps parameters that define text (data type TEXT)
c  to a file in the same format that can be used to define the text.
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL - Aug 1992
c.......................................................................
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/txtpar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/dutext.f,v $
     . $',                                                             '
     .$Id: dutext.f,v 1.2 2004/01/30 17:56:02 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('dutext',1,1,ibug)
c
c  get parameters
      call gettxt (po)
c
      write (iout,10) txtyp,txtid
10    format (a4,1x,2a4)
c
c  add 'ENDTEXT' to end
      k = nlines*18
      atext(k+1) = 'ENDT'
      atext(k+2) = 'EXT '
      jb = k + 3
      je = k + 18
c
c  fill rest of row with blanks
      do 20 i=jb,je
         atext(i) = ' '
20       continue
c
      write (iout,30) (atext(i),i=1,je)
30    format (18a4)
c
      return
c
      end
