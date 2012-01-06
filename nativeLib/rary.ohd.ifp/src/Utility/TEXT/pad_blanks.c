/*
 * utility routine pad_blanks - used to fill a character array with
 *  blanks up to a specified limit
 * written by George Smith - HRL - Nov 10, 1992
 *
 * Argument list:
 *
 *  array:   I/O   array to be right padded with blanks to "fillto"
 *                 characters
 *  fillto:   I    total number of characters in "array"
 */
int pad_blanks(array, fillto)

char * array;
int  * fillto;
{
 int    i, begin;

 begin = strlen(array);
 if (begin < *fillto)
   {
    for (i = begin; i < *fillto; i++)
      {
       array[i] = ' ';
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/pad_blanks.c,v $";
 static char rcs_id2[] = "$Id: pad_blanks.c,v 1.1 1995/09/08 14:59:38 page Exp $";}
/*  ===================================================  */

}
