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

#include "mpe_fieldgen.h"

/* Constant definitions. */
const int m = 7; /* God only knows what this does. */
const int nstack = 1000; /* And this*/
const double fm = 7875.0; /* And this*/
const double fa = 211.0; /* */
const double fc = 1663.0; /* */
const double fmi = .00012698413; /* */

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
static void qksort_gage_radar_pairs ( gage_radar_pair_struct * ptrGageRadarPair,
                                      int left,
                                      int right )
{
   double gage_value;
   double radar_value;
   int hrap_y;
   int pivot;
   int l_hold;
   int r_hold;

   l_hold = left;
   r_hold = right;
   pivot = ptrGageRadarPair[left].hrap_x;
   hrap_y = ptrGageRadarPair[left].hrap_y;
   gage_value = ptrGageRadarPair[left].gageValue;
   radar_value = ptrGageRadarPair[left].radarValue;

   while ( left < right )
   {
      while (( ptrGageRadarPair[right].hrap_x >= pivot ) && ( left < right ))
      {
        right--;
      }

      if ( left != right )
      {
         ptrGageRadarPair[left].hrap_x = ptrGageRadarPair[right].hrap_x;
         ptrGageRadarPair[left].hrap_y = ptrGageRadarPair[right].hrap_y;
         ptrGageRadarPair[left].gageValue = ptrGageRadarPair[right].gageValue;
         ptrGageRadarPair[left].radarValue = ptrGageRadarPair[right].radarValue;
         left++;
      }
      while ((  ptrGageRadarPair[left].hrap_x <= pivot ) && ( left < right ))
      {
        left++;
      }

      if ( left != right )
      {
         ptrGageRadarPair[right].hrap_x = ptrGageRadarPair[left].hrap_x;
         ptrGageRadarPair[right].hrap_y = ptrGageRadarPair[left].hrap_y;
         ptrGageRadarPair[right].gageValue = ptrGageRadarPair[left].gageValue;
         ptrGageRadarPair[right].radarValue = ptrGageRadarPair[left].radarValue;
         right--;
      }
   }

   ptrGageRadarPair[left].hrap_x = pivot;
   ptrGageRadarPair[left].hrap_y = hrap_y;
   ptrGageRadarPair[left].gageValue = gage_value;
   ptrGageRadarPair[left].radarValue = radar_value;

   pivot = left;
   left = l_hold;
   right = r_hold;

   if ( left < pivot )
   {
      qksort_gage_radar_pairs ( ptrGageRadarPair,
                                left,
                                pivot - 1);
   }

   if ( right > pivot )
   {
      qksort_gage_radar_pairs ( ptrGageRadarPair,
                                pivot + 1,
                                right );
   }
}

void sort_gage_radar_table ( gage_radar_pair_table_struct * pGageRadarTable )
{
   qksort_gage_radar_pairs ( pGageRadarTable->ptrGageRadarPair ,
                             0,
                             pGageRadarTable->pairNum - 1 );
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
static void qksort_float_short ( float * dd, short *ii, int left, int right )
{
   float pivot;
   int l_hold;
   int r_hold;
   short int short_value;

   l_hold = left;
   r_hold = right;
   pivot = dd[left];
   short_value = ii[left];

   while ( left < right )
   {
      while (( dd[right] >= pivot ) && ( left < right ))
      {
        right--;
      }

      if ( left != right )
      {
         dd[left] = dd[right];
         ii[left] = ii[right];
         left++;
      }

      while ((  dd[left] <= pivot ) && ( left < right ))
      {
        left++;
      }

      if ( left != right )
      {
         dd[right] = dd[left];
         ii[right] = ii[left];
         right--;
      }
   }

   dd[left] = pivot;
   ii[left] = short_value;

   pivot = left;
   left = l_hold;
   right = r_hold;

   if ( left < pivot )
   {
      qksort_float_short ( dd,
                           ii,
                           left,
                           pivot - 1);
   }

   if ( right > pivot )
   {
      qksort_float_short ( dd,
                           ii, 
                           pivot + 1,
                           right );
   }
}

void sort_float_short ( int n, float * dd, short * ii )
{
   qksort_float_short (dd, ii,  0 , n - 1 );
}

/* Defunct ... Superceded by sort_float_short. */
void MPEFieldGen_qksort22 ( int n, float * dd, short * ii )
{
   float dis;
   int i;
   int iii;
   int iq;
   int istack [ nstack ];
   int j;
   int jstack = 0;
   int l = 0;
   int ir = n - 1;
   float fx = 0.0;

   /* Initialize the istack array to 0. */
   memset ( istack, 0, nstack * sizeof ( int ) );

   while ( 1 )
   {
      if ( ( ir - l ) < m )
      {
         for ( j = l; j <= ir; ++j )  
         {
            dis=dd [ j ];
            iii=ii [ j ];

            for ( i = j - 1; i >= 0; --i )
            {
              if ( dd[i] <= dis ) break; 
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
         if ( jstack == 0 ) return;

         ir = istack [ jstack ];
         l = istack [ jstack-1 ];
         jstack -= 2;
      }
      else
      {
         i=l;
         j=ir;
         fx=fmodf ( fx * fa + fc, fm );
         iq=l+(ir-l+1)*(fx*fmi);
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

         if ( jstack > nstack )
         {
            sprintf ( message, "nstack must be made larger...stop" );         
            printMessage ( message, logFile );
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
static void qksort_double_short_short ( double * dd, short * ii, short * jj,
                                        int left, int right )
{
   double pivot;
   int l_hold;
   int r_hold;
   short int short_value_ii;
   short int short_value_jj;

   l_hold = left;
   r_hold = right;
   pivot = dd[left];
   short_value_ii = ii[left];
   short_value_jj = jj[left];

   while ( left < right )
   {
      while (( dd[right] >= pivot ) && ( left < right ))
      {
        right--;
      }

      if ( left != right )
      {
         dd[left] = dd[right];
         ii[left] = ii[right];
         jj[left] = jj[right];
         left++;
      }

      while ((  dd[left] <= pivot ) && ( left < right ))
      {
        left++;
      }

      if ( left != right )
      {
         dd[right] = dd[left];
         ii[right] = ii[left];
         jj[right] = jj[left];
         right--;
      }
   }

   dd[left] = pivot;
   ii[left] = short_value_ii;
   jj[left] = short_value_jj;

   pivot = left;
   left = l_hold;
   right = r_hold;

   if ( left < pivot )
   {
      qksort_double_short_short ( dd,
                                  ii,
                                  jj,
                                  left,
                                  pivot - 1);
   }

   if ( right > pivot )
   {
      qksort_double_short_short ( dd,
                                  ii, 
                                  jj,
                                  pivot + 1,
                                  right );
   }
}

void sort_double_short_short ( int n, double * dd, short * ii,
                               short * jj )
{
   qksort_double_short_short (dd, ii, jj, 0 , n - 1 );
}


/* Defunct ... replaced by routine sort_double_short_short. */
                                   
void MPEFieldGen_qksort32 (int n, double * dd, short * ii, short * jj)

{
   double dis;
   int i;
   int iii;
   int iq;
   int istack [ nstack ];
   int j;
   int jjj;
   int jstack = 0;
   int l = 1;
   int ir = n;
   double fx = 0.0;

   while ( 1 )
   {
      if ( ir-l < m )
      {
         for ( j=l + 1; j<=ir; ++j)
         {
            dis=dd[j];
            iii=ii[j];
            jjj=jj[j];

            for ( i=j-1; i >= 1; --i )
            {
               if(dd[i]<=dis) break;

               dd[i+1]=dd[i];
               ii[i+1]=ii[i];
               jj[i+1]=jj[i];
            }

            if ( i < 1 ) i = 0;
            dd[i+1]=dis;
            ii[i+1]=iii;
            jj[i+1]=jjj;
         }

         /* Break out of the loop here. */
         if(jstack==0) return;
         ir=istack[jstack];
         l=istack[jstack-1];
         jstack=jstack-2;
      }
      else
      {
         i=l;
         j=ir;
         fx = fmodf ( fx * fa + fc, fm );
         iq=l+(ir-l+1)*(fx*fmi);
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
               if ( dis <= dd [ i ] ) break;
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

         if ( jstack > nstack )
         {
            sprintf ( message, "nstack must be made larger...stop" );
            printMessage ( message, logFile );
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
static void qksort_short_short (short * dd, short * ii, int left, int right )
{
   short pivot;
   int l_hold;
   int r_hold;
   short int short_value;

   l_hold = left;
   r_hold = right;
   pivot = dd[left];
   short_value = ii[left];

   while ( left < right )
   {
      while (( dd[right] >= pivot ) && ( left < right ))
      {
        right--;
      }

      if ( left != right )
      {
         dd[left] = dd[right];
         ii[left] = ii[right];
         left++;
      }

      while ((  dd[left] <= pivot ) && ( left < right ))
      {
        left++;
      }

      if ( left != right )
      {
         dd[right] = dd[left];
         ii[right] = ii[left];
         right--;
      }
   }

   dd[left] = pivot;
   ii[left] = short_value;

   pivot = left;
   left = l_hold;
   right = r_hold;

   if ( left < pivot )
   {
      qksort_short_short ( dd,
                           ii,
                           left,
                           pivot - 1);
   }

   if ( right > pivot )
   {
      qksort_short_short ( dd,
                           ii, 
                           pivot + 1,
                           right );
   }
}

void sort_short_short ( int n, short * dd, short * ii )
{
    qksort_short_short ( dd, ii, 0, n - 1);
}

void MPEFieldGen_qksorti22 ( int n, short * dd, short * ii )
{
   int dis;
   int i;
   int iii;
   int iq;
   int istack [ nstack ];
   int j;
   int jstack = 0;
   int l = 0;
   int ir = n - 1;
   float fx = 0.0;

   /* Initialize the istack array to 0. */
   memset ( istack, 0, nstack * sizeof ( int ) );

   while ( 1 )
   {
      if ( ( ir - l ) < m )
      {
         for ( j = l; j <=ir; ++j )  
         {
            dis=dd [ j ];
            iii=ii [ j ];

            for ( i = j - 1; i >= 0; --i )
            {
              if ( dd[i] <= dis ) break; 
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
         if ( jstack == 0 ) return;

         ir = istack [ jstack ];
         l = istack [ jstack-1 ];
         jstack -= 2;
      }
      else
      {
         i=l;
         j=ir;
         fx=fmodf ( fx * fa + fc, fm );
         iq=l+(ir-l+1)*(fx*fmi);
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

         if ( jstack > nstack )
         {
            sprintf ( message, "nstack must be made larger...stop" );         
            printMessage ( message, logFile );
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

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/qksort.c,v $";
 static char rcs_id2[] = "$Id: qksort.c,v 1.1 2007/10/15 12:19:11 dsa Exp $";}
/*  ===================================================  */

}
