#include "decodedpa.h"

void get_radid_from_product(char *arg)
{

/*
   function to parse radar id from product header
   radar id follows "DPA" string

   calling function: main_decodedpa
*/

 int n;
 char ch, prevchar, pprevchar;

 /*----------------------------------------------*/
 /*   open product                               */
 /*----------------------------------------------*/

 if((dpafile = fopen(arg,"rb")) == NULL)
 {
   printf("error opening product -- product not deocded\n");
   exit(3);
 }

 /*----------------------------------------------*/
 /*   initialization                             */
 /*----------------------------------------------*/

 radid[3]='\0';
 prevchar = ' ';
 pprevchar = ' ';

 /*-------------------------------------------------------------*/
 /*   search product character by character for string = "DPA"  */
 /*   if not found before EOF, then exit program                */
 /*-------------------------------------------------------------*/

 for (;;)
 {
    n = fscanf(dpafile,"%c",&ch);
    if (n==EOF) 
    {
      printf("Error: EOF encountered before finding radar identifier");
      printf(" in %s\n",arg);
      fclose(dpafile);
      exit(2);
    }

    if (ch == 'A' && prevchar == 'P' && pprevchar == 'D') 
    {
      fgets(radid, 4, dpafile);
      break;
    }
    else
    {
       pprevchar = prevchar;
       prevchar = ch;
    }
 }

 fclose(dpafile);

}
