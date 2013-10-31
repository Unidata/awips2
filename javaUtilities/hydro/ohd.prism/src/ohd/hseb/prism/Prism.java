/*
 * Created on August 25, 2005
 */

/**
 * @ author Bryon Lawrence
 * 
 * This is the Java version of the create_prism
 * program.
 */
package ohd.hseb.prism;

import java.io.*;
import java.util.*;
import ohd.hseb.util.*;

public class Prism
{
        static private String _prism_input_path;
        static private String _prism_output_path;

        private double _cellsize;
        private double _latitude_southwest_corner;
        private double _longitude_southwest_corner;
        private int _nodata_value;
        private int _num_rows;
        private int _num_cols;
        private int[][] _prism_data;
        
        // Static initializer for prism input path and
        // prism output path.
        static
        {
           AppsDefaults apps_defaults = new AppsDefaults ( ); 
           _prism_input_path = apps_defaults.getToken ( "prism_input_dir" );
           _prism_output_path = apps_defaults.getToken ( "prism_output_dir" );
        }

        public void readPrism ( String prism_filename ) throws IOException
        {
           
           String prism_filepath;
           String token;

           // Build the input filepath.
           prism_filepath = _prism_input_path + "/" + prism_filename;
           FileInputStream fileInputStream = new FileInputStream(prism_filepath);
           BufferedInputStream bufferedInputStream = new BufferedInputStream ( fileInputStream);
          
           Scanner scanner = new Scanner ( bufferedInputStream );
           
           //Do we need to read the string?
           token = scanner.next();
           _num_cols = scanner.nextInt ( );

           token = scanner.next();
           _num_rows = scanner.nextInt ( );

           token = scanner.next();
           _longitude_southwest_corner = scanner.nextDouble();

           token = scanner.next();
 	       _latitude_southwest_corner = scanner.nextDouble();

	       token = scanner.next();
	       _cellsize = scanner.nextDouble();

	       token = scanner.next();
	       _nodata_value = scanner.nextInt();

           _prism_data = new int [ _num_rows ] [ _num_cols ];  

           CodeTimer outerTimer = new CodeTimer ( );
           CodeTimer innerTimer = new CodeTimer ( );
           outerTimer.start();
                     
          for ( int row = 0; row < _num_rows; ++row )
          {
    	     for ( int col = 0; col < _num_cols; ++col )
    	     {
    		    innerTimer.restart();
                _prism_data [ row ] [ col ] = scanner.nextInt ();
                innerTimer.stop();
             }
    	   
          }

          outerTimer.stop( "The nested for loops took ");
       
          System.out.println( "Parse Token Elapsed Time = " + innerTimer.getElapsedTime());
          scanner.close();
            
        }

        /* This method is not used.  It represents a faster way of reading
         * and Processing PRISM data.  But the code is trickier.  Code readability
         * has been chosen over performance.
         */
        public void readPrismOld ( String prism_filename ) throws IOException
        {
           String line;
           String prism_filepath;
           String token;
           StringTokenizer token_string;

           // Build the input filepath.
           prism_filepath = _prism_input_path + "/" + prism_filename;

           FileInputStream fileInputStream = new FileInputStream(prism_filepath);
           InputStreamReader inputStreamReader = new InputStreamReader (fileInputStream); 
           BufferedReader reader = new BufferedReader(inputStreamReader);
           
          //DataInputStream reader = new DataInputStream (new BufferedReader ( 
          //                              new FileReader ( _prism_filepath ) ));


           // Read the number of columns
           line = reader.readLine ( );
                  
           token_string = new StringTokenizer ( line );
           token = token_string.nextToken( );
           token = token_string.nextToken( );
           
           //Do we need to read the string?
           _num_cols = Integer.parseInt ( token );

           // Read the number of rows
           line = reader.readLine ( );
           token_string = new StringTokenizer ( line );
           token = token_string.nextToken ( );
           token = token_string.nextToken( );
           _num_rows = Integer.parseInt ( token );

           // Read the longitude of the lowerleft corner
           line = reader.readLine ( );
           token_string = new StringTokenizer ( line );
           token = token_string.nextToken ( );
           token = token_string.nextToken ( );
           _longitude_southwest_corner = Double.parseDouble ( token );

	   // Read the latitude of the lowerleft corner
	   line = reader.readLine ( );
	   token_string = new StringTokenizer ( line );
	   token = token_string.nextToken ( );
	   token = token_string.nextToken ( );
	   _latitude_southwest_corner = Double.parseDouble (token);

	   // Read the cell size
	   line = reader.readLine ( );
	   token_string = new StringTokenizer ( line );
	   token = token_string.nextToken ( );
	   token = token_string.nextToken ( );
	   _cellsize = Double.parseDouble (token);

	   // Read the nodata value.
	   line = reader.readLine ( );
	   token_string = new StringTokenizer ( line );
	   token = token_string.nextToken ( );
	   token = token_string.nextToken ( );
	   _nodata_value = Integer.parseInt (token);

       _prism_data = new int [ _num_rows ] [ _num_cols ];  

       CodeTimer outerTimer = new CodeTimer ( );
       CodeTimer innerTimer = new CodeTimer ( );
       outerTimer.start();
       
       for ( int row = 0; row < _num_rows; ++row )
       {
          line = reader.readLine ( );
	      token_string = new StringTokenizer ( line );

          for ( int col = 0; col < _num_cols; ++col )
          {
        	  innerTimer.restart();
	          token = token_string.nextToken ( );
              _prism_data [ row ][ col ] = Integer.parseInt(token);
              innerTimer.stop();
          }
       }
       
       outerTimer.stop( "The nested for loops took ");
       
       System.out.println( "Parse Token Elapsed Time = " + innerTimer.getElapsedTime());
       reader.close();
            
     }

        public int getNumColumns ( )
        {
           return _num_cols;
        }
 
        public int getNumRows ( )
        {
           return _num_rows;
        }

        public double getSouthWestLong ( )
        {
           return _longitude_southwest_corner;
        }

        public double getSouthWestLat ( )
        {
           return _latitude_southwest_corner;
        }

        public double getCellSize ( )
        {
           return _cellsize;
        }

        public int getNoDataVal ( )
        {
           return _nodata_value;
        }

        public int[][] getPrism ( )
        {
           return _prism_data;
        }

        public String toString ( )
        {
           return new String ( "num_rows: " + _num_rows + " num_cols: " +
                               _num_cols + " southwest longitude: " +
                               _longitude_southwest_corner  +
                               " southwest latitude: " +
                               _latitude_southwest_corner );
        }

        static public String getInputFilePath ( )
        {
           return _prism_input_path;
        }

        static public String getOutputFilePath ( )
        {
           return _prism_output_path;
        }
}
