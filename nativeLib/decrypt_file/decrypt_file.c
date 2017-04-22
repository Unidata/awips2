/* -----------------------------------------------------------------------------

File name:
    decrypt_file.c

Compilation:
    cc -D_GNU_SOURCE -o decrypt_file decrypt_file.c

Requirements:
    - cruft must be installed
    - the decryption key must be in ~/.key

File description:
    This program attempts to decrypt a stream of data that has been encrypted
    with cruft.

    This program reads data from stdin.  It assumes that there is some kind of
    header before the start of the cruft header that must be stripped, though
    it will still work if there is no header.

    If the cruft header is not found within the first 128 bytes, then the input
    data is simply copied to the output file since decryption is not needed.  If
    a cruft header is found, then the data is piped to cruft for decryption and
    is written to the file name matching the template specified on the command line.

    This program takes one argument: the output file name template that is used
    by the mkstemp system call to open a file with a unique file name.  The
    string (".XXXXXX") is appended by this program as required by mkstemp.

    Any program looking for files to process that are output by this program
    should look at the file permissions before attempting a read.  When this
    program has finished writing the output file, it will change permissions to
    0666.

Command line:
    decrypt_file <output_file_name>

Author:
    Brian M. Rapp        03/19/2009

----------------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#define MAX_BUF_SIZE        4096        // This must not be greater than PIPE_BUF
                                         // as defined in the kernel header file
                                         // /usr/src/linux/include/linux/limits.h

#define HDR_DEPTH           128         // The cruft header must occur within 128
                                         // bytes of the beginning of the file.

#define ENCRYPT_PROGRAM     "cruft"     // The code may require modification if
                                         // a different decryption tool is used.

#define INSERT_PROGRAM      "pqinsert"  // LDM tool to insert data into the product
                                        // queue

#define CRUFT_HDR_SIZE      9           // Cruft header length (see cruft_hdr below)
#define SBN_HDR_SIZE        24          // Length of the SBN header
#define WMO_HDR_SIZE        21          // Length of the WMO header (TTAAii CCCC DDHHMM\r\r\n)
#define OPEN_FILE_PERMS     (S_IRUSR | S_IWUSR)
#define DONE_FILE_PERMS     (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#define DIR_PERMS           0777        // the permissions for the directories created
#define DECRYPT_FLAG_OFFSET		 5		 // Offset to NWSTG CCB encryption flag

char    *progname;
char    cruft_hdr[]         = {253, 253, 253, 253, 253, 253, 253, 253, 253};

/* -------------------------------------------------------------------------- */

void usage (char *exename) {
    //fprintf (stderr, "Usage: %s output_file_path feed_type\n", exename); // no feed_type
    fprintf (stderr, "Usage: %s output_file_path \n", exename);
}

/* -------------------------------------------------------------------------- */

int main (int argc, char **argv) {

    char            *out_template;          // Input as the file name template from the command
                                            // line. This is modified by mkstemp to the output
                                            // file name.

    int             out_fd;                 // Output file descriptor returned by call to mkstemp
    FILE            *out_pipe;              // File pointer returned by call to popen for decryption
    size_t          len;                    // Length of buffers read from STDIN
    size_t          wlen;                   // Length of the write buffer
    size_t          count;                  // Number of items written by calls to fwrite
    char            file_buf[MAX_BUF_SIZE]; // Input buffer from STDIN
    char            *hdr_ptr;               // Pointer to start of cruft header within file_buf
    char            command_line[128];      // Command line for starting cruft via popen
    char            pqinsert_line[128];     // PQInsert Line
    char		    *sbn_hdr_ptr;           // Pointer to the start of the 24-byte SBN header in each product
    char            *slash_ptr;             // pointer for the slashes in the path
    progname    = argv[0];                  // Get the program name

    if (argc != 2) {    // Only 1 parameter - the template for the output file name.
        usage (argv[0]);
        exit (1);
    }

    len = (size_t) strlen (argv[1]) + 7 + 1;    // Allocate 7 extra bytes for the ".XXXXXX"
                                                // plus 1 for the terminating nul
    if ((out_template = malloc (len)) == NULL) {    // Try to malloc memory for output file name
        fprintf (stderr, "%s: Error \"%s\" while allocating memory for output template -- exiting\n",
                progname, strerror (errno));
        exit (errno);
    }

    sprintf (out_template, "%s.XXXXXX", argv[1]);   // Output file name template

    // the reasons the DIR_PERMS are 777 is because of NFS permission issues with mkdir
    // make sure all of the intervening directories in out_template actually exist
    slash_ptr = strchr ( out_template, '/' );
    while ( slash_ptr != NULL )
    {
        *slash_ptr = '\0'; // truncate at the slash
        if ( strlen( out_template ) > 0 ) // corner case with a leading slash
        {
            if ( mkdir ( out_template, DIR_PERMS ) == - 1 && errno != EEXIST )
            {
                // it failed but not because the directory already exists
                fprintf ( stderr, "mkdir errno %d\n", errno );
                if ( errno == EPERM )
                {
                    fprintf ( stderr, "mkdir permission error\n" );
                }
                exit (errno);
            }
        }
        *slash_ptr = '/'; // put the slash back
        ++slash_ptr;
        slash_ptr = strchr ( slash_ptr, '/' ); // find the next slash
    }

    if ((out_fd = mkstemp (out_template)) == -1) {  // Create and open a unique temporary file for output
        fprintf (stderr, "%s: Error \"%s\" creating temporary file for template %s -- exiting\n",
                progname, strerror (errno), out_template);
        exit (errno);
    }

    // Try to read some data from STDIN
    if ((len = read (STDIN_FILENO, file_buf, MAX_BUF_SIZE)) < 0) { // Error if read return negative value -- clean up
        fprintf (stderr, "%s: Error \"%s\" reading from STDIN -- exiting\n",
                progname, strerror (errno));
        close (out_fd);
        unlink (out_template);
        exit (errno);
    } else if (len == 0) {  // Input data stream is empty -- clean up
        fprintf (stderr, "%s: File is empty -- ignoring\n", progname);
        close (out_fd);
        unlink (out_template);
        exit (0);
    }

    // If we're here, then we read some bytes from STDIN.
    // If the input buffer is less than 128 bytes, then only search through the number of bytes read
    if ((hdr_ptr = memmem (file_buf, (len < HDR_DEPTH) ? len : HDR_DEPTH, cruft_hdr, CRUFT_HDR_SIZE)) == NULL) {
        // then there is no cruft header within the first HDR_DEPTH bytes

        if ((wlen = write (out_fd, file_buf, len)) != len) {		 // Try to write the input buffer to the output file
            fprintf (stderr, "%s: Error \"%s\" writing to %s -- exiting\n", progname, strerror (errno), out_template);
            close (out_fd);
            unlink (out_template);
            exit (errno);
        }

        // Read the rest of the input data and write it to the output file, exit on error
        while ((len = read (STDIN_FILENO, file_buf, MAX_BUF_SIZE)) > 0) {
            if ((wlen = write (out_fd, file_buf, len)) != len) {
                fprintf (stderr, "%s: Error \"%s\" writing to %s -- exiting\n", progname, strerror (errno), out_template);
                close (out_fd);
                unlink (out_template);
                exit (errno);
            }
        }

        if (len < 0) {  // Uh oh, there was an error reading from STDIN
            fprintf (stderr, "%s: Error \"%s\" reading from STDIN -- exiting\n",
                    progname, strerror (errno));
            close (out_fd);
            unlink (out_template);
            exit (errno);
        }

        close (out_fd); 		 // Close the output file
    } else {    // Found a cruft header
		 // Write the 24-byte SBN header to the output file before closing it.  Cruft will append to this file.
		 sbn_hdr_ptr = hdr_ptr - (WMO_HDR_SIZE + SBN_HDR_SIZE);

		 // Change compression flag from 'E' to 'U'
		 if (file_buf[0] == 0x40) {
		     if (file_buf[DECRYPT_FLAG_OFFSET] == 'E') {
		 		 file_buf[DECRYPT_FLAG_OFFSET] = 'U';
		     }
		 }

		 if ((wlen = write (out_fd, sbn_hdr_ptr, SBN_HDR_SIZE)) != SBN_HDR_SIZE) {
		     fprintf (stderr, "%s: Error \"%s\" writing SBN header to %s -- exiting\n", progname, strerror (errno), out_template);
		     close (out_fd);
		     unlink (out_template);
		     exit (errno);
		 }

        close (out_fd);

        if (chmod (out_template, OPEN_FILE_PERMS) < 0) {		 // Set file perms to 0600 so cruft can write to it.
            fprintf (stderr, "%s: Error \"%s\" changing file permissions on %s to %od\n",
                    progname, strerror (errno), out_template, DONE_FILE_PERMS);
        }

        sprintf (command_line, "%s >> %s", ENCRYPT_PROGRAM, out_template);		 // Create pipe command line
        if ((out_pipe = popen (command_line, "w")) == NULL) { // Try to open pipe for output to cruft
            fprintf (stderr, "%s: Error \"%s\" while attempting to open pipe to %s -- exiting\n",
                    progname, strerror (errno), command_line);
            unlink (out_template);      // Clean up mess
            exit (errno);
        }

        wlen = file_buf + len - hdr_ptr;    // Calculate the length of the output buffer from the start of the cruft header
        if ((count = fwrite (hdr_ptr, wlen, 1, out_pipe)) != 1) {		 // Try to pipe the output buffer to cruft
            fprintf (stderr, "%s: Error \"%s\" writing to output pipe -- only wrote %d items exiting\n",
                    progname, strerror (errno), (int) count);
            fclose (out_pipe);
            unlink (out_template);		 		 // Clean up mess
            exit (errno);
        }

        while ((len = read (STDIN_FILENO, file_buf, MAX_BUF_SIZE)) > 0) {
            if ((count = fwrite (file_buf, len, 1, out_pipe)) != 1) {
                fprintf (stderr, "%s: Error \"%s\" writing to output pipe -- only wrote %d items exiting\n",
                        progname, strerror (errno), (int) count);
                fclose (out_pipe);
                unlink (out_template);		 		 // Clean up mess
                exit (errno);
            }
        }

        if (len < 0) {		 		 // Uh oh, there was an error reading from STDIN
            fprintf (stderr, "%s: Error \"%s\" reading from STDIN -- exiting\n", progname, strerror (errno));
            fclose (out_pipe);
            unlink (out_template);		 // Clean up the mess
            exit (errno);
        }

        fclose (out_pipe);

        sprintf(pqinsert_line, "%s %s", INSERT_PROGRAM, out_template);
        if((system(pqinsert_line)) > 0) {
            fprintf (stderr, "%s: Error \"%s\" running pqinsert -- file not inserted into data stream\n", progname, strerror (errno));
        }

    }

    // Change file permissions on output file to 0666 as a signal to anyone watching that the file is available for access now
    if (chmod (out_template, DONE_FILE_PERMS) < 0) {
        fprintf (stderr, "%s: Error \"%s\" changing file permissions on %s to %od\n", progname, strerror (errno), out_template, DONE_FILE_PERMS);
    }

    exit (0);
}
