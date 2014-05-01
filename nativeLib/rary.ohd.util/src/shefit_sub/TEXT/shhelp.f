C  =====================================================================
C  pgm: SHHELP .. Output to stdout help instructions for this program
C
C  use:     CALL SHHELP()
C  =====================================================================
      SUBROUTINE SHHELP()

      INTEGER      IUN,IERR

      DATA         IUN / 6 /

      WRITE(IUN,101,IOSTAT=IERR)
      WRITE(IUN,102,IOSTAT=IERR)

  101 FORMAT(/,
     $ /,' Shefit is a UNIX filter program to decode shef messages.',
     $ /,' It can be used by piping messages into it and redirecting',
     $ /,' output to a file; or by giving it filename arguments for',
     $ /,' input, output, and errors.',
     $ /,'',
     $ /,' One of the following options may be given as the first',
     $ /,' argument:',
     $ /,'',
     $ /,'  -1 .. Text output as one long line, full quotes (default)',
     $ /,'  -2 .. Text output as two lines, quotes limited to 66 chars',
     $ /,'  -b .. Binary output if output is to a file',
     $ /,'  -h .. Output a help message on how to run shefit (stdout)')
  102 FORMAT(
     $ /,' General forms:',
     $ /,'',
     $ /,'  shefit  [option]  input_file  [output_file]  [error_file]',
     $ /,'  cmd-mesg  |  shefit  [option]  [> out_file]  [2> err_file]',
     $ /,'',
     $ /,' Examples:',
     $ /,'',
     $ /,'  shefit  shef_messg_in',
     $ /,'  shefit  -b  shef_messg_in  decode_out  err_out',
     $ /,'  shefit  -2  shef_messg_in  >  decode_out',
     $ /,'  print ".A STA 971205 Z DH08/HG 2"  |  shefit  2>&-',
     $ /,'  cat  shef_messg_in  |  shefit  >  decode_out  2>  err_out',
     $ /)

      RETURN
      END
