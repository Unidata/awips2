package ohd.hseb.prism;

import java.io.*;
import ohd.hseb.util.*;

/**
 * @author Bryon Lawrence
 * March 8, 2006
 * 
 * This program crops a national PRISM file to the size of an office's MPE area.
 * It stores this cropped PRISM product as an XMRG file which can be read and 
 * displayed by MPE Editor.  This program replaces the create_prism program.
 * 
 * The arguments to this program are:
 * 
 * "prefix_of_input_prism_file" "prefix_of_output_prism_file" <t|p>
 * 
 * The input prism files are assumed to have a suffix of _MM where MM is the
 * two digit month number.
 * 
 * 01	January	
 * 02	February
 * 03	March
 * 04	April
 * 05	May
 * 06	June	
 * 07	July	
 * 08	August
 * 09	September
 * 10	October
 * 11	November
 * 12	December
 * 
 * A suffix of _MMM is placed on each output XMRG PRISM file.  The MMM is the 3
 * character month abbreviation:
 * 
 * jan	January
 * feb	February
 * mar  March
 * apr	April
 * may  May
 * jun  June
 * jul  July
 * aug  August
 * sep  September
 * oct  October
 * nov  November
 * dec  December
 * 
 * An optional third argument indicates whether a precipitation or temperature
 * prism file is being.  "p" indicates a precipitation file. "t" indicates a
 * temperature file.  If this argument is omitted, then a precipitation file is
 * assumed.
 * 
 * The tokens input_prism_dir and output_prism_dir define the directories
 * of the input PRISM files and the output cropped XMRG files.  If these are not
 * set, then this program will look for the prism files in the current working
 * directory.
 * 
 * This program expects there to be 13 raw prism files in the directory specified by 
 * input_prism_dir.  These include 12 monthly files and 1 annual prism file.
 * This program will produce 13 XMRG files corresponding to the 13 input files.
 * 
 * Example of usage:
 * 
 * export prism_input_dir=/fs/hseb/whfs/site_data/mpe_data/PRISM
 * export prism_output_dir=/awips/hydroapps/precip_proc/local/data/app/mpe/prism
 * 
 * java MPEClimo us_tmin prism_min_temp_ounx t
 * 
 * This will look for raw prism files in the /fs/hseb/whfs/site_data/mpe_data/PRISM
 * directory for the following files:
 * 
 * us_tmin_01		us_tmin_04		us_tmin_07		us_tmin_10	us_tmin_14
 * us_tmin_02		us_tmin_05		us_tmin_08		us_tmin_11
 * us_tmin_03		us_tmin_06		us_tmin_09		us_tmin_12
 * 
 * It will create the following output files in the
 * /awips/hydroapps/precip_proc/local/data/app/mpe/PRISM directory:
 * 
 * prism_min_temp_ounx_jan		prism_min_temp_ounx_aug
 * prism_min_temp_ounx_feb		prism_min_temp_ounx_sep
 * prism_min_temp_ounx_mar		prism_min_temp_ounx_oct
 * prism_min_temp_ounx_apr		prism_min_temp_ounx_nov
 * prism_min_temp_ounx_may		prism_min_temp_ounx_dec
 * prism_min_temp_ounx_jun		prism_min_temp_ounx_ann
 * prism_min_temp_ounx_jul
 * 
 * The "t" argument specifies this is by default
 */

public class MPEClimo implements MPEConstants
{
   
   static private boolean temperature_prism_flag = false;
   static private double scale_factor = 1.0;
   
   static public double[][] cropPrismToHrapGrid ( Prism prism,
                                                  int southwest_column,
                                                  int southwest_row,
                                                  int number_columns,
                                                  int number_rows )
   {
      double latitude_upper_left;
      double col_longitude;
      double row_latitude ; 
      double cellsize = prism.getCellSize ( );
      double latitude_lower_left = prism.getSouthWestLat ( );
      double longitude_lower_left = prism.getSouthWestLong ( );
      double precip[][] = new double [ number_columns ] [ number_rows ]; 
      Converter.HrapPoint hrap_point;
      int count [][] = new int [ number_columns ] [ number_rows ];
      int hrap_row;
      int hrap_col;
      int num_prism_cols = prism.getNumColumns ( );
      int num_prism_rows = prism.getNumRows ( );
      int prism_data [ ][ ] = prism.getPrism ( );
      int prism_nodata = prism.getNoDataVal ( ); 
      
      // Initialize the count and precipitation arrays to 0.
      for ( int col = 0; col < number_columns; ++ col )
      {
         for ( int row =0; row < number_rows; ++ row ) 
         {
            count [ col ] [ row ] = 0;
            precip [ col ] [ row ] = 0.0;
         }
      }

      // Get the latitude of the upperleft corner.
      latitude_upper_left =  latitude_lower_left + cellsize * num_prism_rows;
      
      // Process data row by row.
      for ( int row = 0; row < num_prism_rows; ++row )
      {
         row_latitude = latitude_upper_left - ( ( row - 0.5 ) * cellsize );

         for ( int col = 0; col < num_prism_cols; ++col )
         {
            if ( prism_data [ row ][ col ] != prism_nodata )
            {
               col_longitude = longitude_lower_left + 
                               ( ( col - 0.5 ) * cellsize ); 
               col_longitude = - col_longitude;

               hrap_point = Converter.convertLatLonToHrap ( col_longitude,
                                                            row_latitude );
               hrap_row = (int)hrap_point.getHrapRow ( );
               hrap_col = (int)hrap_point.getHrapCol ( );

               hrap_row -= southwest_row;
               hrap_col -= southwest_column;

               if ( ( hrap_row >= 0 ) &&
                    ( hrap_row < number_rows ) &&
                    ( hrap_col >= 0 ) &&
                    ( hrap_col < number_columns ) )
               {
                  count [ hrap_col ] [ hrap_row ] ++;
                  precip [ hrap_col ][ hrap_row ] += prism_data [ row ] [ col ];
               }
            }
         }
      }

      /* Compute the average. */
      for ( int row = 0; row < number_rows; ++ row )
      {
         for ( int col = 0; col < number_columns; ++ col )
         {
            if ( count [ col ][ row ] == 0 )
            {
               precip [ col ] [ row ] = MISSING;
            }
            else
            {
               precip [ col ] [ row ] = precip [ col ] [ row ]/
                                        count [ col ] [ row ];
            }
         }
      } 

      // Fill in any holes in the data.
      double sum;
      int col_begin = 0;
      int col_end = 0;
      int num = 0;
      int row_begin = 0;
      int row_end = 0;

      for ( int row = 0; row < number_rows; ++ row )
      {
         row_begin = row - 1 ;

         if ( row_begin < 0 ) row_begin = 0;

         row_end = row + 1;

         if ( row_end == number_rows ) row_end = row;

         for ( int col = 0; col < number_columns; ++ col )
         {
            if ( count [ col ] [ row ] == 0 )
            {
               col_begin = col - 1;
               if ( col_begin < 0 ) col_begin = 0;

               col_end = col + 1;

               if ( col_end == number_columns ) col_end = col;

               num = 0;
               sum = 0.0;

               for ( int row_fill = row_begin; row_fill <= row_end; 
                     ++row_fill )
               {
                  for ( int col_fill = col_begin; col_fill <= col_end;
                        ++ col_fill )
                  {
                     if ( count [ col_fill ] [ row_fill ] > 0 )
                     {
                        ++num;
                        sum += precip [ col_fill ] [ row_fill ];
                     }
                  }
               }

               if ( num == 0 )
               {
                precip [ col ] [ row ] = MISSING;
               }
               else
               {
                  precip [ col ] [ row ] = sum / num;
               }
            }
         }
        
      }

      return  precip;
   }

   static void appendPrismToHrapGrid ( Prism prism,
                                       int southwest_column,
                                       int southwest_row,
                                       int number_columns,
                                       int number_rows,
                                       double precip[][])
   {
       double latitude_upper_left;
       double col_longitude;
       double row_latitude ; 
       double cellsize = prism.getCellSize ( );
       double latitude_lower_left = prism.getSouthWestLat ( );
       double longitude_lower_left = prism.getSouthWestLong ( );
       Converter.HrapPoint hrap_point;
       int count [][] = new int [ number_columns ] [ number_rows ];
       int hrap_row;
       int hrap_col;
       int num_prism_cols = prism.getNumColumns ( );
       int num_prism_rows = prism.getNumRows ( );
       int prism_data [ ][ ] = prism.getPrism ( );
       int prism_nodata = prism.getNoDataVal ( );
       
//     Initialize the count array to 0.
       for ( int col = 0; col < number_columns; ++ col )
       {
           for ( int row =0; row < number_rows; ++ row ) 
           {
               count [ col ] [ row ] = 0;
           }
       }

//     Get the latitude of the upperleft corner.
       latitude_upper_left =  latitude_lower_left + cellsize * num_prism_rows;

//     Process data row by row.
       for ( int row = 0; row < num_prism_rows; ++row )
       {
           row_latitude = latitude_upper_left - ( ( row - 0.5 ) * cellsize );

           for ( int col = 0; col < num_prism_cols; ++col )
           {
               if ( prism_data [ row ][ col ] != prism_nodata  ) 
               {
                   col_longitude = longitude_lower_left + 
                   ( ( col - 0.5 ) * cellsize ); 
                   col_longitude = - col_longitude;

                   hrap_point = Converter.convertLatLonToHrap ( col_longitude,
                           row_latitude );
                   hrap_row = (int)hrap_point.getHrapRow ( );
                   hrap_col = (int)hrap_point.getHrapCol ( );

                   hrap_row -= southwest_row;
                   hrap_col -= southwest_column;

                   if ( ( precip [hrap_col][hrap_row] == MISSING) && 
                        ( hrap_row >= 0 ) &&
                        ( hrap_row < number_rows ) &&
                        ( hrap_col >= 0 ) &&
                        ( hrap_col < number_columns ) )
                   {
                       count [ hrap_col ] [ hrap_row ] ++;
                       precip [ hrap_col ][ hrap_row ] += prism_data [ row ] [ col ];
                   }
               }
           }
       }

       /* Compute the average. */
       for ( int row = 0; row < number_rows; ++ row )
       {
           for ( int col = 0; col < number_columns; ++ col )
           {
               if ( count [ col ][ row ] > 0 )
               {
                   precip [ col ] [ row ] = precip [ col ] [ row ]/
                   count [ col ] [ row ];
               }
           }
       } 

//     Fill in any holes in the data.
       double sum;
       int col_begin = 0;
       int col_end = 0;
       int num = 0;
       int row_begin = 0;
       int row_end = 0;

       for ( int row = 0; row < number_rows; ++ row )
       {
           row_begin = row - 1 ;

           if ( row_begin < 0 ) row_begin = 0;

           row_end = row + 1;

           if ( row_end == number_rows ) row_end = row;

           for ( int col = 0; col < number_columns; ++ col )
           {
               if ( count [ col ] [ row ] == 0 )
               {
                   col_begin = col - 1;
                   if ( col_begin < 0 ) col_begin = 0;

                   col_end = col + 1;

                   if ( col_end == number_columns ) col_end = col;

                   num = 0;
                   sum = 0.0;

                   for ( int row_fill = row_begin; row_fill <= row_end; 
                   ++row_fill )
                   {
                       for ( int col_fill = col_begin; col_fill <= col_end;
                       ++ col_fill )
                       {
                           if ( count [ col_fill ] [ row_fill ] > 0 )
                           {
                               ++num;
                               sum += precip [ col_fill ] [ row_fill ];
                           }
                       }
                   }

                   if ( num > 0 )
                   {
                       precip [ col ] [ row ] = sum / num;
                   }
               }
           }

       }

   }

   static void convertPrismData ( int number_columns,
                                  int number_rows,
                                  double precip[][] )
   {
       // If processing temperature data, convert them from Celcius to 
       // Fahrenheit.
       if ( temperature_prism_flag )
       {
           for ( int row = 0; row < number_rows; ++ row )
           {
               for ( int col = 0; col < number_columns; ++ col )
               {
                   if ( precip [ col ] [ row ] != MISSING )
                   {
                       precip [ col ] [ row ] /= 100.0 ;

//                     Convert from Celcius to Fahrenheit
                       precip [col][row] = ( precip [col][row] * 1.8 ) + 32;
                   }
                   else
                   {
                	   precip [ col ] [ row ] = MISSING_SAVE; 
                   }
               } 
           }
       }
       else
       {
           for ( int row = 0; row < number_rows; ++ row )
           {
               for ( int col = 0; col < number_columns; ++ col )
               {
                   if ( precip [ col ] [ row ] != MISSING )
                   {
                       precip [ col ] [ row ] /= 100.0 ;
                   }
                   else
                   {
                	   precip [ col ] [ row ] = MISSING_SAVE; 
                   }
 
               } 
           }
       }
   }
   
   /*
    * The Main Routine for MPE Climo.
    */
   static public void main ( String args [ ] )
   {
      double precip [ ][ ];
      GeoData geo_data;
      final String month_array [ ] = new String [ ] { "01", "02", "03", "04",
                                           "05", "06", "07", "08", "09", "10", 
                                           "11", "12", "14" };
      final String month_name_array [ ] = new String [ ] {"jan", "feb", "mar",
    		                                              "apr", "may", "jun",
    		                                              "jul", "aug", "sep",
    		                                              "oct", "nov", "dec",
    		                                              "ann" };
      int southwest_column;
      int southwest_row;
      int number_columns;
      int number_rows;
            
      Prism prism;
      String prism_output_path = Prism.getOutputFilePath ( );
      String file;
      
      // The user must supply the prefix of the national prism file(s).  The
      // national prism file(s) is assumed to end with the month number.  For example
      // <file_prefix>01 like us_tmin_01
      // The user must also supply the file name prefix of the cropped, output
      // Prism file.  The user must also supply a single character flag indicating 
      // whether the file contains temperature ('T') or precipitation ('P') data.
      
      // The number of args must be at least 3.  Any higher number indicates that the user
      // has supplied multiple PRISM input files which will be mosaicked to provide
      // information for the office's MPE area.
      if (args.length < MINIMUM_COMMAND_LINE_ARGS )
      {
    	  System.out.println( "Must provide the prefix of the input prism file(s)");
    	  System.out.println( "and the cropped, output prism file.");
    	  System.out.print ( "Usage: MPEclimo input_prism_file_prefix [input_prism_file_prefix2 ...] ");
    	  System.out.println ("output_prism_file_prefix <t|p>");
    	  System.exit(1);
      }
      
      int numInputFiles = (args.length - MINIMUM_COMMAND_LINE_ARGS) + 1; 
      
      // The user must supply a flag indicating whether temperature or
      // precipitation PRISM xmrg files are to be created.
      
      if ( args [ args.length - 1 ].charAt(0) == 't' || args [ args.length - 1 ].charAt(0) == 'T')
      {
         temperature_prism_flag = true;
             
         // The scale factor for precipitation data is 1.0.
         // The scale factor for temperature data is 10.0. 
         scale_factor = 10.0;
      }

      // Read the coordinates of the MPE forecast area the
      // PRISM data will be cropped to.
      try
      {
         geo_data = new GeoData ( );
         southwest_column = geo_data.getSouthwestColumn ( );
         southwest_row = geo_data.getSouthwestRow ( );
         number_columns = geo_data.getNumColumns ( );
         number_rows = geo_data.getNumRows ( );

         System.out.println ( "South West Column: " + southwest_column );
         System.out.println ( "South West Row: " + southwest_row );
         System.out.println ( "Number of Columns: " + number_columns );
         System.out.println ( "Number of Rows: " + number_rows );

         prism = new Prism ( );

         /* Start a timer.  This will show how much time is spent
          * reading the prism data.
          */
         CodeTimer totalTime = new CodeTimer ();
         totalTime.start();
         
         for ( int month = 0; month < month_array.length; ++ month )
         {
        
        	// Construct the input PRISM file name
		    file = args[0]+ "_" + month_array [ month ];
            prism.readPrism (file);
            System.out.println ( "Processing file: " + Prism.getInputFilePath ()
                                 + "/" + file );
            System.out.println ( prism );

            // Crop the PRISM data to the forecast area. 
            precip = cropPrismToHrapGrid ( prism, southwest_column, 
                                           southwest_row,
                                           number_columns, 
                                           number_rows );
            
            // Merge the PRISM data for any other source files
            // into the precip array.
            
            for ( int i = 1; i < numInputFiles; ++i )
            {
               //Construct the input PRISM file name
               file = args[i]+ "_" + month_array [ month ];
               prism.readPrism (file);
               System.out.println ( "Processing file: " + Prism.getInputFilePath ()
                                    + "/" + file );
               System.out.println ( prism );
               appendPrismToHrapGrid ( prism,
                                       southwest_column,
                                       southwest_row,
                                       number_columns,
                                       number_rows,
                                       precip );
            }
            
            convertPrismData ( number_columns,
                               number_rows,
                               precip );
            
            // Write the prism data to an XMRG file.
            file = prism_output_path + "/" + args[args.length-2] + "_"+ month_name_array [ month ];
            System.out.println( "Writing PRISM data to " + file );
            
            XMRG.writeXMRG ( file,
                             "lawrence",
                             "HP",
                             "PRISM",
                             "xxxxxx",
                             "xxxxxx",
                             precip,
                             scale_factor,
                             southwest_column,
                             southwest_row,
                             number_columns,
                             number_rows,
                             9998,
                             1,
                             false );
         }
         
         totalTime.stop("Total read time for Scanner ");
         System.exit ( 0 );
      }
      catch ( IOException e )
      {
         System.out.println ( e.toString ( ) );
         System.exit ( 1 );
      }
   }
}
