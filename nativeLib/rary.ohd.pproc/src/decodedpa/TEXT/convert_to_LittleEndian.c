/****************************************************************************************/
/*  Big Endian to Little Endian conversion routine for DPA products                     */
/*  source code in this routine is based on code from ABRFC                             */
/*                                                                                      */
/*  calling routine:  main_decodedpa                                                    */
/*                                                                                      */
/*  this routine creates a new file in Little Endian format with the filename           */
/*    of the original DPA product with ".LE" appended                                   */
/*  at the end of the decoding process, both files are deleted                          */        
/*                                                                                      */
/*  Input:  BigEfn = filename of original raw DPA product (Big Endian format)           */
/*                                                                                      */
/*  Output: LittleEfname = filename of converted raw DPA product (Little Endian format) */
/*                                                                                      */
/****************************************************************************************/

#include "decodedpa.h"

int convert_to_LittleEndian (char *BigEfn, char *LittleEfname)

{

FILE *infile, *outfile;
unsigned char head [146], data[2], data2[2], asc;
int	i, j, count, num_bytes_read, num_bytes_writ;
char	BigEfname[256], ch, prevchar, char_chunk[200];

  /*-------------------------------------------------------*/
  /*   open original raw DPA file (Big Endian format)      */   
  /*-------------------------------------------------------*/

  strcpy(BigEfname,BigEfn);
  if ((infile = fopen(BigEfname, "rb")) == NULL)
  {
     printf("error opening raw DPA file -- %s\n",BigEfname);
     printf("  -- product not decoded\n");
     return 6;
  }

  /*----------------------------------------------------------------------*/
  /*   open file for converted DPA product (Little Endian format)         */   
  /*   filename is same as original raw DPA filename with ".LE" appended  */
  /*----------------------------------------------------------------------*/

  sprintf(LittleEfname,"%s.LE",BigEfname);

  if ((outfile = fopen(LittleEfname, "wb")) == NULL)
  {
     printf("error opening file for Little Endian product -- %s.LE\n",BigEfname);
     printf("  -- product not converted, product not decoded\n");
     fclose(infile);
     return 5;
  }

  /*---------------------------------------------------------*/
  /* position to beginning of product                        */
  /* from AWIPS has header already stripped off              */
  /* if this is 00 hex, then it already has header stripped  */
  /*---------------------------------------------------------*/

  fread(&asc, sizeof(unsigned char), 1, infile);
  fseek(infile, 0, SEEK_SET);
  if (asc != 0)
  {
     /* read in extra info at top  30 bytes (from ftp site) */
     for (i = 0; i < 30; i++)
        fread (&asc, sizeof(unsigned char), 1, infile);
  }

/*-----------------------------------------------------------------------------*/
/* DPA product is made up of 16-bit words, i.e. 2 unsigned chars per word.     */
/*     Grab, byte-swap, and output the first 73 words,                         */
/*     which correspond to pages one and two of the DPA format guide document. */
/*-----------------------------------------------------------------------------*/

     fread (&head[0], sizeof (unsigned char), 146, infile);

     /* Output the first 73 words, reversing the byte order for each variable read. */

     fwrite (&head[1], sizeof (unsigned char), 1, outfile); /* Begin Msg code */
     fwrite (&head[0], sizeof (unsigned char), 1, outfile);
     fwrite (&head[3], sizeof (unsigned char), 1, outfile); /* Begin Msg Julian date */
     fwrite (&head[2], sizeof (unsigned char), 1, outfile);
     fwrite (&head[7], sizeof (unsigned char), 1, outfile); /* Begin Message time */
     fwrite (&head[6], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[5], sizeof (unsigned char), 1, outfile);
     fwrite (&head[4], sizeof (unsigned char), 1, outfile);
     fwrite (&head[11], sizeof (unsigned char), 1, outfile); /* Begin Product length */
     fwrite (&head[10], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[9], sizeof (unsigned char), 1, outfile);
     fwrite (&head[8], sizeof (unsigned char), 1, outfile);
     fwrite (&head[13], sizeof (unsigned char), 1, outfile); /* Begin RPG ID */
     fwrite (&head[12], sizeof (unsigned char), 1, outfile);
     fwrite (&head[15], sizeof (unsigned char), 1, outfile); /* Begin Dest. ID */
     fwrite (&head[14], sizeof (unsigned char), 1, outfile);
     fwrite (&head[17], sizeof (unsigned char), 1, outfile); /* Begin Num. Blocks */
     fwrite (&head[16], sizeof (unsigned char), 1, outfile);
     fwrite (&head[19], sizeof (unsigned char), 1, outfile); /* Begin Divider */
     fwrite (&head[18], sizeof (unsigned char), 1, outfile);
     fwrite (&head[23], sizeof (unsigned char), 1, outfile); /* Begin Latitude */
     fwrite (&head[22], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[21], sizeof (unsigned char), 1, outfile);
     fwrite (&head[20], sizeof (unsigned char), 1, outfile);
     fwrite (&head[27], sizeof (unsigned char), 1, outfile); /* Begin Longitude */
     fwrite (&head[26], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[25], sizeof (unsigned char), 1, outfile);
     fwrite (&head[24], sizeof (unsigned char), 1, outfile);
     fwrite (&head[29], sizeof (unsigned char), 1, outfile); /* Begin Height MSL */
     fwrite (&head[28], sizeof (unsigned char), 1, outfile);
     fwrite (&head[31], sizeof (unsigned char), 1, outfile); /* Begin Prod. Code */
     fwrite (&head[30], sizeof (unsigned char), 1, outfile);
     fwrite (&head[33], sizeof (unsigned char), 1, outfile); /* Begin Op Mode */
     fwrite (&head[32], sizeof (unsigned char), 1, outfile);
     fwrite (&head[35], sizeof (unsigned char), 1, outfile); /* Begin VCP */
     fwrite (&head[34], sizeof (unsigned char), 1, outfile);
     fwrite (&head[37], sizeof (unsigned char), 1, outfile); /* Begin Seq. Number */
     fwrite (&head[36], sizeof (unsigned char), 1, outfile);
     fwrite (&head[39], sizeof (unsigned char), 1, outfile); /* Begin Scan Number */
     fwrite (&head[38], sizeof (unsigned char), 1, outfile);
     fwrite (&head[41], sizeof (unsigned char), 1, outfile); /* Begin Scan Julian date */
     fwrite (&head[40], sizeof (unsigned char), 1, outfile);
     fwrite (&head[45], sizeof (unsigned char), 1, outfile); /* Begin Scan time */
     fwrite (&head[44], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[43], sizeof (unsigned char), 1, outfile);
     fwrite (&head[42], sizeof (unsigned char), 1, outfile);
     fwrite (&head[47], sizeof (unsigned char), 1, outfile); /* Begin Gen Julian date */
     fwrite (&head[46], sizeof (unsigned char), 1, outfile);
     fwrite (&head[51], sizeof (unsigned char), 1, outfile); /* Begin Gen time */
     fwrite (&head[50], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[49], sizeof (unsigned char), 1, outfile);
     fwrite (&head[48], sizeof (unsigned char), 1, outfile);
     fwrite (&head[53], sizeof (unsigned char), 1, outfile); /* Begin 4 unused words */
     fwrite (&head[52], sizeof (unsigned char), 1, outfile); /* (just flip per word) */
     fwrite (&head[55], sizeof (unsigned char), 1, outfile);
     fwrite (&head[54], sizeof (unsigned char), 1, outfile);
     fwrite (&head[57], sizeof (unsigned char), 1, outfile);
     fwrite (&head[56], sizeof (unsigned char), 1, outfile);
     fwrite (&head[59], sizeof (unsigned char), 1, outfile);
     fwrite (&head[58], sizeof (unsigned char), 1, outfile);
     fwrite (&head[61], sizeof (unsigned char), 1, outfile); /* Begin Min data level */
     fwrite (&head[60], sizeof (unsigned char), 1, outfile);
     fwrite (&head[63], sizeof (unsigned char), 1, outfile); /* Begin data level incr. */
     fwrite (&head[62], sizeof (unsigned char), 1, outfile);
     fwrite (&head[65], sizeof (unsigned char), 1, outfile); /* Begin num. of data levels */
     fwrite (&head[64], sizeof (unsigned char), 1, outfile);
     fwrite (&head[67], sizeof (unsigned char), 1, outfile); /* Begin 13 unused words */
     fwrite (&head[66], sizeof (unsigned char), 1, outfile); /* (just flip per word) */
     fwrite (&head[69], sizeof (unsigned char), 1, outfile);
     fwrite (&head[68], sizeof (unsigned char), 1, outfile);
     fwrite (&head[71], sizeof (unsigned char), 1, outfile);
     fwrite (&head[70], sizeof (unsigned char), 1, outfile);
     fwrite (&head[73], sizeof (unsigned char), 1, outfile);
     fwrite (&head[72], sizeof (unsigned char), 1, outfile);
     fwrite (&head[75], sizeof (unsigned char), 1, outfile);
     fwrite (&head[74], sizeof (unsigned char), 1, outfile);
     fwrite (&head[77], sizeof (unsigned char), 1, outfile);
     fwrite (&head[76], sizeof (unsigned char), 1, outfile);
     fwrite (&head[79], sizeof (unsigned char), 1, outfile);
     fwrite (&head[78], sizeof (unsigned char), 1, outfile);
     fwrite (&head[81], sizeof (unsigned char), 1, outfile);
     fwrite (&head[80], sizeof (unsigned char), 1, outfile);
     fwrite (&head[83], sizeof (unsigned char), 1, outfile);
     fwrite (&head[82], sizeof (unsigned char), 1, outfile);
     fwrite (&head[85], sizeof (unsigned char), 1, outfile);
     fwrite (&head[84], sizeof (unsigned char), 1, outfile);
     fwrite (&head[87], sizeof (unsigned char), 1, outfile);
     fwrite (&head[86], sizeof (unsigned char), 1, outfile);
     fwrite (&head[89], sizeof (unsigned char), 1, outfile);
     fwrite (&head[88], sizeof (unsigned char), 1, outfile);
     fwrite (&head[91], sizeof (unsigned char), 1, outfile);
     fwrite (&head[90], sizeof (unsigned char), 1, outfile);
     fwrite (&head[93], sizeof (unsigned char), 1, outfile); /* Begin Max data value */
     fwrite (&head[92], sizeof (unsigned char), 1, outfile);
     fwrite (&head[95], sizeof (unsigned char), 1, outfile); /* Begin Bias */
     fwrite (&head[94], sizeof (unsigned char), 1, outfile);
     fwrite (&head[97], sizeof (unsigned char), 1, outfile); /* Begin Error Variance */
     fwrite (&head[96], sizeof (unsigned char), 1, outfile);
     fwrite (&head[99], sizeof (unsigned char), 1, outfile); /* Begin Julain Date */
     fwrite (&head[98], sizeof (unsigned char), 1, outfile);
     fwrite (&head[101], sizeof (unsigned char), 1, outfile); /* Begin End time */
     fwrite (&head[100], sizeof (unsigned char), 1, outfile); /* (one word only this time) */
     fwrite (&head[103], sizeof (unsigned char), 1, outfile); /* Begin 2 unused words */
     fwrite (&head[102], sizeof (unsigned char), 1, outfile); /* (just flip per word) */
     fwrite (&head[105], sizeof (unsigned char), 1, outfile);
     fwrite (&head[104], sizeof (unsigned char), 1, outfile);
     fwrite (&head[107], sizeof (unsigned char), 1, outfile); /* Begin Number of Maps */
     fwrite (&head[106], sizeof (unsigned char), 1, outfile);
     fwrite (&head[111], sizeof (unsigned char), 1, outfile); /* Begin Prod Sym Offset */
     fwrite (&head[110], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[109], sizeof (unsigned char), 1, outfile);
     fwrite (&head[108], sizeof (unsigned char), 1, outfile);
     fwrite (&head[115], sizeof (unsigned char), 1, outfile); /* Begin Graph Att Offset */
     fwrite (&head[114], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[113], sizeof (unsigned char), 1, outfile);
     fwrite (&head[112], sizeof (unsigned char), 1, outfile);
     fwrite (&head[119], sizeof (unsigned char), 1, outfile); /* Begin Graph Alpha Offset */
     fwrite (&head[118], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[117], sizeof (unsigned char), 1, outfile);
     fwrite (&head[116], sizeof (unsigned char), 1, outfile);
     fwrite (&head[121], sizeof (unsigned char), 1, outfile); /* Begin Divider */
     fwrite (&head[120], sizeof (unsigned char), 1, outfile);
     fwrite (&head[123], sizeof (unsigned char), 1, outfile); /* Begin Block ID */
     fwrite (&head[122], sizeof (unsigned char), 1, outfile);
     fwrite (&head[127], sizeof (unsigned char), 1, outfile); /* Begin Prod Sym Length */
     fwrite (&head[126], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[125], sizeof (unsigned char), 1, outfile);
     fwrite (&head[124], sizeof (unsigned char), 1, outfile);
     fwrite (&head[129], sizeof (unsigned char), 1, outfile); /* Begin Num Layers */
     fwrite (&head[128], sizeof (unsigned char), 1, outfile);
     fwrite (&head[131], sizeof (unsigned char), 1, outfile); /* Begin Divider */
     fwrite (&head[130], sizeof (unsigned char), 1, outfile);
     fwrite (&head[135], sizeof (unsigned char), 1, outfile); /* Begin Layer Length */
     fwrite (&head[134], sizeof (unsigned char), 1, outfile); /* (two words) */
     fwrite (&head[133], sizeof (unsigned char), 1, outfile);
     fwrite (&head[132], sizeof (unsigned char), 1, outfile);
     fwrite (&head[137], sizeof (unsigned char), 1, outfile); /* Begin Packet Code */
     fwrite (&head[136], sizeof (unsigned char), 1, outfile);
     fwrite (&head[141], sizeof (unsigned char), 1, outfile); /* Begin 2 unused words */
     fwrite (&head[140], sizeof (unsigned char), 1, outfile); /* (just flip per word) */
     fwrite (&head[139], sizeof (unsigned char), 1, outfile);
     fwrite (&head[138], sizeof (unsigned char), 1, outfile);
     fwrite (&head[143], sizeof (unsigned char), 1, outfile); /* Begin Num LFM Boxes */
     fwrite (&head[142], sizeof (unsigned char), 1, outfile);
     fwrite (&head[145], sizeof (unsigned char), 1, outfile); /* Begin Num Rows */
     fwrite (&head[144], sizeof (unsigned char), 1, outfile);

/*-----------------------------------------------------------------------------------*/

     /*--------------------------------------*/
     /*  Read data portion of product        */   
     /*--------------------------------------*/

     if ((int)head[145] != 131)
     {
        printf("error in BigE to LittleE conversion - number of rows ne 131\n");
        fclose(outfile);
        fclose(infile);
        return 1;
     }

     for (i = 0; i < (int)head[145]; i++)
     {
        count = 0;
        fread(&data, sizeof (unsigned char), 2, infile);

        /* number of bytes in row, 2 x number of runs in row */
        fwrite(&data[1], sizeof (unsigned char), 1, outfile);

        fwrite(&data[0], sizeof (unsigned char), 1, outfile);

        /* read in run data */
        for (j = 0; j < ((int)data[1])/2; j++)
        {
           fread(&data2, sizeof (unsigned char), 2, infile);  /* Begin run data (MSB = number of grids) */
           fwrite(&data2[0], sizeof (unsigned char), 1, outfile);
           fwrite(&data2[1], sizeof (unsigned char), 1, outfile);
           count = count + (int)data2[0];
        }

        if (count != 131)
        {
           printf("error in BigE to LittleE conversion - number of counted rows ne 131\n");
           fclose(outfile);
           fclose(infile);
           return 2;
        }
      }

/*--------------------------------------------------------------------------------*/

      /*----------------------------------------------------------------------------------*/
      /*   Read Adaptation Data, Bias Table and other ascii data                          */
      /*   rate scans are not used by decoder and therefore are not written to ".LE" file */
      /*----------------------------------------------------------------------------------*/

      /*-------------------------------------------------------------*/
      /* read past rate scans until finding adaptation data header   */
      /* adaptation data header = "ADAP(..)"                         */
      /*-------------------------------------------------------------*/

      prevchar = ' ';
      ch = ' ';

      for (;;)
      {
         num_bytes_read = fread(&ch, sizeof (char), 1, infile);
         if(feof(infile) != 0)
         {
            printf("error in BigE to LittleE conversion - adaptation data header not found\n");
            fclose(outfile);
            fclose(infile);
            return 3;
         }

         if (ch == 'D' && prevchar == 'A')
         {
            fseek(infile, -2, SEEK_CUR);  /*  move file pointer back two spaces  */
            break;
         }

         prevchar=ch;
      }

      /*-------------------------------------------------*/
      /*  read and write remainder of product until EOF  */
      /*-------------------------------------------------*/

      for (;;)
      {
         num_bytes_read = fread(&char_chunk, 1, sizeof (char_chunk), infile);

         if(feof(infile) != 0)
         {
            num_bytes_writ = fwrite(&char_chunk, 1, num_bytes_read, outfile);
            break;
         }

         num_bytes_writ = fwrite(&char_chunk, 1, num_bytes_read, outfile);

         if(num_bytes_read != num_bytes_writ)
         {
            printf("error in BigE to LittleE conversion writing to .LE file\n");
            fclose(outfile);
            fclose(infile);
            return 4;
         }
         
      }

/*--------------------------------------------------------------------------------*/

    fclose(outfile);
    fclose(infile);
    return 0;
}
