/*******************************************************************************
* FILENAME:            qksort.c
* NUMBER OF MODULES:  
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       May 2, 2005
* ORGANIZATION:        OHD-11
* MACHINE:             Linux 
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*        
********************************************************************************
*/

/* Include files. */
#include <math.h>
#include <string.h>

#include "empe_fieldgen.h"

/* Constant definitions. */
const int hpe_fieldgen_m = 7; /* God only knows what this does. */
const int hpe_fieldgen_nstack = 1000; /* And this*/
const double hpe_fieldgen_fm = 7875.0; /* And this*/
const double hpe_fieldgen_fa = 211.0; /* */
const double hpe_fieldgen_fc = 1663.0; /* */
const double hpe_fieldgen_fmi = .00012698413; /* */

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

void qksort4 ( gage_radar_pair_table_struct * pGageRadarTable )
{
   double fx = 0.0;
   double zzg;
   double zzr;

   int i;
   int iq;
   int istack [ hpe_fieldgen_nstack ];
   int j;
   int jju;
   int jjv;
   int jstack = 0;
   int l = 0;
   int ir = pGageRadarTable->pairNum - 1 ;

   memset ( istack, 0, hpe_fieldgen_nstack* sizeof ( int ) );

   while ( 1 )  /* Goto 10 */
   {
      if ( ( ir - l ) < hpe_fieldgen_m )
      {
         for ( j = l ; j <= ir; ++j )
         {
            jju = pGageRadarTable->ptrGageRadarPair[j].hrap_x;
            jjv = pGageRadarTable->ptrGageRadarPair[j].hrap_y;
            zzg = pGageRadarTable->ptrGageRadarPair[j].gageValue;
            zzr = pGageRadarTable->ptrGageRadarPair[j].radarValue;

            for ( i = j - 1; i >= 0; --i )
            {
               if ( pGageRadarTable->ptrGageRadarPair[i].hrap_x <= jju )
               {
                  break;
               }
            
               pGageRadarTable->ptrGageRadarPair[ i + 1 ].hrap_x = 
                             pGageRadarTable->ptrGageRadarPair[ i ].hrap_x; 
               pGageRadarTable->ptrGageRadarPair[ i + 1 ].hrap_y = 
                             pGageRadarTable->ptrGageRadarPair[ i ].hrap_y;
               pGageRadarTable->ptrGageRadarPair[ i + 1 ].gageValue = 
                             pGageRadarTable->ptrGageRadarPair[ i ].gageValue;
               pGageRadarTable->ptrGageRadarPair[ i + 1 ].radarValue =
                             pGageRadarTable->ptrGageRadarPair[ i ].radarValue;
            }

            /* i = 0 */
            pGageRadarTable->ptrGageRadarPair[ i + 1 ].hrap_x = jju;
            pGageRadarTable->ptrGageRadarPair[ i + 1 ].hrap_y = jjv;
            pGageRadarTable->ptrGageRadarPair[ i + 1 ].gageValue = zzg;
            pGageRadarTable->ptrGageRadarPair[ i + 1 ].radarValue = zzr;
         }

/* cause "Beyond stack read error" from pure version
 * qksort32 and fortran version use jstack == 0
 * guoxian 08/19/2005
         if ( jstack < 0 ) return;
*/
         if ( jstack == 0 )
         {
             return;
         }

         ir = istack [ jstack ];
         l = istack [ jstack - 1 ]; 
         jstack -= 2;
      }
      else
      {
         i = l;
         j = ir;
         fx = fmodf ( ( fx * hpe_fieldgen_fa + hpe_fieldgen_fc ), hpe_fieldgen_fm);
         iq = l + ( ir - l + 1 ) * ( fx * hpe_fieldgen_fmi );

         jju = pGageRadarTable->ptrGageRadarPair [ iq ].hrap_x;
         jjv = pGageRadarTable->ptrGageRadarPair [ iq ].hrap_y;
         zzg = pGageRadarTable->ptrGageRadarPair [ iq ].gageValue;
         zzr = pGageRadarTable->ptrGageRadarPair [ iq ].radarValue;

         pGageRadarTable->ptrGageRadarPair [ iq ].hrap_x = 
               pGageRadarTable->ptrGageRadarPair [ l ].hrap_x;
         pGageRadarTable->ptrGageRadarPair [ iq ].hrap_y =
               pGageRadarTable->ptrGageRadarPair [ l ].hrap_y;   
         pGageRadarTable->ptrGageRadarPair [ iq ].gageValue =
               pGageRadarTable->ptrGageRadarPair [ l ].gageValue;
         pGageRadarTable->ptrGageRadarPair [ iq ].radarValue =
               pGageRadarTable->ptrGageRadarPair [ l ].radarValue;

         while ( 1 )  /* goto 20 */
         {
            for ( ; ( j > 0 ) && 
                 ( jju < pGageRadarTable->ptrGageRadarPair [ j ].hrap_x );
                 --j );

            if ( j <= i )
            {
               pGageRadarTable->ptrGageRadarPair [ i ].hrap_x = jju;
               pGageRadarTable->ptrGageRadarPair [ i ].hrap_y = jjv;
               pGageRadarTable->ptrGageRadarPair [ i ].gageValue = zzg;
               pGageRadarTable->ptrGageRadarPair [ i ].radarValue = zzr;
               break; /* Break out of the 20 loop. */
            }

            pGageRadarTable->ptrGageRadarPair [ i ].hrap_x = 
                  pGageRadarTable->ptrGageRadarPair [ j ].hrap_x;
            pGageRadarTable->ptrGageRadarPair [ i ].hrap_y =
                  pGageRadarTable->ptrGageRadarPair [ j ].hrap_y;   
            pGageRadarTable->ptrGageRadarPair [ i ].gageValue =
                  pGageRadarTable->ptrGageRadarPair [ j ].gageValue;
            pGageRadarTable->ptrGageRadarPair [ i ].radarValue =
                  pGageRadarTable->ptrGageRadarPair [ j ].radarValue;

            ++i;
       
            for ( ; ( i <= pGageRadarTable->pairNum ) && 
                    ( jju > pGageRadarTable->ptrGageRadarPair [ i ].hrap_x );
                    ++i );

            if ( j <= i )
            {
               pGageRadarTable->ptrGageRadarPair [ j ].hrap_x = jju;
               pGageRadarTable->ptrGageRadarPair [ j ].hrap_y = jjv;
               pGageRadarTable->ptrGageRadarPair [ j ].gageValue = zzg;
               pGageRadarTable->ptrGageRadarPair [ j ].radarValue = zzr;
               i = j;
               break;
            }

            pGageRadarTable->ptrGageRadarPair [ j ].hrap_x = 
                  pGageRadarTable->ptrGageRadarPair [ i ].hrap_x;
            pGageRadarTable->ptrGageRadarPair [ j ].hrap_y =
                  pGageRadarTable->ptrGageRadarPair [ i ].hrap_y;   
            pGageRadarTable->ptrGageRadarPair [ j ].gageValue =
                  pGageRadarTable->ptrGageRadarPair [ i ].gageValue;
            pGageRadarTable->ptrGageRadarPair [ j ].radarValue =
                  pGageRadarTable->ptrGageRadarPair [ i ].radarValue;
            --j;
         }

         /* 30 goes to here. */
         jstack += 2; 

         if ( jstack > hpe_fieldgen_nstack )
         {
            sprintf ( message, "nstack must be made larger...stop\n" );
            hpe_fieldgen_printMessage( message); 
            return;
         }

         if ( ( ir - i ) > ( i - l ) )
         {
            istack [ jstack ] = ir;
            istack [ jstack - 1 ] = i + 1;    
            ir = i - 1;
         }
         else
         {
            istack [ jstack ] = i - 1;
            istack [ jstack - 1 ] = 1; 
            l = i + 1;
         }
      }
   }

   return ;
} 

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void qksort22 ( int n, float * dd, short * ii )
{
   float dis;
   int i;
   int iii;
   int iq;
   int istack [ hpe_fieldgen_nstack ];
   int j;
   int jstack = 0;
   int l = 0;
   int ir = n - 1;
   float fx = 0.0;

   /* Initialize the istack array to 0. */
   memset ( istack, 0, hpe_fieldgen_nstack * sizeof ( int ) );

   while ( 1 )
   {
      if ( ( ir - l ) < hpe_fieldgen_m )
      {
         for ( j = l; j <= ir; ++j )  
         {
            dis=dd [ j ];
            iii=ii [ j ];

            for ( i = j - 1; i >= 0; --i )
            {
              if ( dd[i] <= dis )
              {
                  break;
              } 

              dd[i+1]=dd[i];
              ii[i+1]=ii[i];
            }

            dd [ i + 1 ] = dis;
            ii [ i + 1 ] = iii;
         }

/* cause "Beyond stack read error" from pure version
 * qksort32 and fortran version use jstack == 0
 * guoxian 08/19/2005
         if ( jstack < 0 ) return;
*/
         if ( jstack == 0 )
         {
             return;
         }

         ir = istack [ jstack ];
         l = istack [ jstack-1 ];
         jstack -= 2;
      }
      else
      {
         i=l;
         j=ir;
         fx=fmodf ( fx * hpe_fieldgen_fa + hpe_fieldgen_fc, hpe_fieldgen_fm );
         iq=l+(ir-l+1)*(fx*hpe_fieldgen_fmi);
         dis=dd[iq];
         iii=ii[iq];
         dd[iq]=dd[l];
         ii[iq]=ii[l];

         while ( 1 ) /* do 20 loop */
         {
            while ( ( j > 0 ) && ( dis < dd [ j ] ) )
            {
               --j;
            }

            if ( j <= i )
            {
               dd[i]=dis;
               ii[i]=iii;
               break;
            }

            dd[i]=dd[j];
            ii[i]=ii[j];
            ++i; 

            while ( ( i < n ) && ( dis > dd [ i ]  ) )
            {
                ++i; 
            }
 
            if ( j <= i )
            {
               dd [ j ] = dis;
               ii [ j ] = iii;
               i = j;
               break;
             }
 
             dd [ j ] = dd [ i ];
             ii [ j ] = ii [ i ];
             --j; 
         }

         jstack +=2 ;

         if ( jstack > hpe_fieldgen_nstack )
         {
            sprintf ( message, "nstack must be made larger...stop" );         
            hpe_fieldgen_printMessage( message);
            return;
         }

         if ( ( ir - i ) >= ( i - l ) )
         {
             istack[jstack]=ir;
             istack[jstack-1]=i+1;
             ir=i-1;
         }
         else
         {
             istack[jstack]=i-1;
             istack[jstack-1]=l;
             l=i+1;
         }
      }
   }
}

/*****************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
******************************************************************************
*/

void qksort32 (int n, double * dd, short * ii, short * jj)
{
   double dis;
   int i;
   int iii;
   int iq;
   int istack [ hpe_fieldgen_nstack ];
   int j;
   int jjj;
   int jstack = 0;
   int l = 1;
   int ir = n;
   double fx = 0.0;

   while ( 1 )
   {
      if ( ir-l < hpe_fieldgen_m )
      {
         for ( j=l + 1; j<=ir; ++j)
         {
            dis=dd[j];
            iii=ii[j];
            jjj=jj[j];

            for ( i=j-1; i >= 1; --i )
            {
               if(dd[i]<=dis)
               {
                   break;
               }

               dd[i+1]=dd[i];
               ii[i+1]=ii[i];
               jj[i+1]=jj[i];
            }

            if ( i < 1 )
            {
                i = 0;
            }

            dd[i+1]=dis;
            ii[i+1]=iii;
            jj[i+1]=jjj;
         }

         /* Break out of the loop here. */
         if(jstack==0)
         {
             return;
         }

         ir=istack[jstack];
         l=istack[jstack-1];
         jstack=jstack-2;
      }
      else
      {
         i=l;
         j=ir;
         fx = fmodf ( fx * hpe_fieldgen_fa + hpe_fieldgen_fc, hpe_fieldgen_fm );
         iq=l+(ir-l+1)*(fx*hpe_fieldgen_fmi);
         dis=dd[iq];
         iii=ii[iq];
         jjj=jj[iq];
         dd[iq]=dd[l];
         ii[iq]=ii[l];
         jj[iq]=jj[l];
         while ( 1 )
         {
            if(j > 0 )
            {
               if(dis < dd[j])
               {
                  j=j-1;
                  continue;
               }
            }

            if(j <=i )
            {
               dd[i]=dis;
               ii[i]=iii;
               jj[i]=jjj;
               break;
            }

            dd[i]=dd[j];
            ii[i]=ii[j];
            jj[i]=jj[j];
            i=i+1;

            while ( i < n )
            {
               if ( dis <= dd [ i ] )
               {
                   break;
               }

               i=i+1;
            }
    
            if ( j <= i )
            {
               dd[j]=dis;
               ii[j]=iii;
               jj[j]=jjj;
               i=j;
               break;
            }

            dd[j]=dd[i];
            ii[j]=ii[i];
            jj[j]=jj[i];
            j=j-1;
         }

         jstack=jstack+2;

         if ( jstack > hpe_fieldgen_nstack )
         {
            sprintf ( message, "nstack must be made larger...stop" );
            hpe_fieldgen_printMessage( message);
            return;
         }

         if(ir-i >= i-l)
         {
           istack[jstack]=ir;
           istack[jstack-1]=i+1;
           ir=i-1;
         }
         else
         {
           istack[jstack]=i-1;
           istack[jstack-1]=l;
           l=i+1;
         }
      }
  }

  return;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void qksorti22 ( int n, short * dd, short * ii )
{
   int dis;
   int i;
   int iii;
   int iq;
   int istack [ hpe_fieldgen_nstack ];
   int j;
   int jstack = 0;
   int l = 0;
   int ir = n - 1;
   float fx = 0.0;

   /* Initialize the istack array to 0. */
   memset ( istack, 0, hpe_fieldgen_nstack * sizeof ( int ) );

   while ( 1 )
   {
      if ( ( ir - l ) < hpe_fieldgen_m )
      {
         for ( j = l; j <=ir; ++j )  
         {
            dis=dd [ j ];
            iii=ii [ j ];

            for ( i = j - 1; i >= 0; --i )
            {
              if ( dd[i] <= dis )
              {
                  break;
              } 

              dd[i+1]=dd[i];
              ii[i+1]=ii[i];
            }

            dd [ i + 1 ] = dis;
            ii [ i + 1 ] = iii;
         }

/* cause "Beyond stack read error" from pure version
 * qksort32 and fortran version use jstack == 0
 * guoxian 08/19/2005
         if ( jstack < 0 ) return;
*/
         if ( jstack == 0 )
         {
             return;
         }

         ir = istack [ jstack ];
         l = istack [ jstack-1 ];
         jstack -= 2;
      }
      else
      {
         i=l;
         j=ir;
         fx=fmodf ( fx * hpe_fieldgen_fa + hpe_fieldgen_fc, hpe_fieldgen_fm );
         iq=l+(ir-l+1)*(fx*hpe_fieldgen_fmi);
         dis=dd[iq];
         iii=ii[iq];
         dd[iq]=dd[l];
         ii[iq]=ii[l];

         while ( 1 ) /* do 20 loop */
         {
            while ( ( j > 0 ) && ( dis < dd [ j ] ) )
            {
               --j;
            }

            if ( j <= i )
            {
               dd[i]=dis;
               ii[i]=iii;
               break;
            }

            dd[i]=dd[j];
            ii[i]=ii[j];
            ++i; 

            while ( ( i < n ) && ( dis > dd [ i ]  ) )
            {
                ++i; 
            }
 
            if ( j <= i )
            {
               dd [ j ] = dis;
               ii [ j ] = iii;
               i = j;
               break;
             }
 
             dd [ j ] = dd [ i ];
             ii [ j ] = ii [ i ];
             --j; 
         }

         jstack +=2 ;

         if ( jstack > hpe_fieldgen_nstack )
         {
            sprintf ( message, "nstack must be made larger...stop" );         
            hpe_fieldgen_printMessage( message);
            return;
         }
    
         if ( ( ir - i ) >= ( i - l ) )
         {
             istack[jstack]=ir;
             istack[jstack-1]=i+1;
             ir=i-1;
         }
         else
         {
             istack[jstack]=i-1;
             istack[jstack-1]=l;
             l=i+1;
         }
      }
   }
}
