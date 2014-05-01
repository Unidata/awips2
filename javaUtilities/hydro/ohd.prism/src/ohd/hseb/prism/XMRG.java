package ohd.hseb.prism;

import java.io.*;


public class XMRG
{
    static public void writeXMRG ( String filePath,
                            String user,
                            String OS,
                            String process,
                            String datetimes,
                            String datetimev,
                            double [ ] [ ] data_matrix,
                            double conversion_factor,
                            int southwest_column,
                            int southwest_row,
                            int number_columns,
                            int number_rows,
                            int maxval,
                            int version_number,
                            boolean replace_missing ) throws IOException 
    {
       byte [ ] byte_array;
       byte [ ] boperuser = new byte [ 10 ];
       byte [ ] bdatev = new byte [ 20 ];
       byte [ ] bdates = new byte [ 20 ];
       byte [ ] bproc_flag = new byte [ 8 ];

       DataOutputStream file;
       int record_length;
       short mosaic [][] = new short [ number_columns ] [ number_rows ];

       // This method writes XMRG files.
       String operuser = OS + user;
       byte_array = operuser.getBytes ( );

       for ( int i = 0; i < byte_array.length; ++i ) 
       {
          boperuser [ i ] = byte_array [ i ];
       }
 
       byte_array = process.getBytes ( );

       for ( int i = 0; i < byte_array.length; ++i ) 
       {
          bproc_flag [ i ] = byte_array [ i ];
       }

       byte_array = datetimes.getBytes ( );

       for ( int i = 0; i < byte_array.length; ++i ) 
       {
          bdates [ i ] = byte_array [ i ];
       }

       byte_array = datetimev.getBytes ( );

       for ( int i = 0; i < byte_array.length; ++i ) 
       {
          bdatev [ i ] = byte_array [ i ];
       }
 

       for ( int col = 0; col < number_columns; ++ col )
       {
          for ( int row = 0; row < number_rows; ++ row )
          {
             mosaic [ col ][ row ] = ( short ) ( data_matrix [ col ] [ row ] *
                                               conversion_factor ); 
          }
       }


       // Open the output file.
       // Write the xmrg header information.
       file = new DataOutputStream ( new FileOutputStream ( 
                                     new File ( filePath ) ) ); 

       // Write the first record of the header.
       file.writeInt ( 16 );
       file.writeInt ( southwest_column );
       file.writeInt ( southwest_row );
       file.writeInt ( number_columns );
       file.writeInt ( number_rows );      
       file.writeInt ( 16 );

       // Write the second record of the header.
       file.writeInt ( 66 );
       file.write ( boperuser, 0, boperuser.length );
       file.write ( bdates, 0, bdates.length );
       file.write ( bproc_flag, 0, bproc_flag.length ); 
       file.write ( bdatev, 0, bdatev.length ); 
       file.writeInt ( maxval );
       file.writeInt ( version_number );
       file.writeInt ( 66 );
       
       // Compute the data record length in bytes.
       record_length = number_columns * 2;

       // Write the data records.
       for ( int row = 0; row < number_rows; ++ row )
       {
          file.writeInt ( record_length );

          for ( int col = 0; col < number_columns; ++ col )
          {
             file.writeShort ( mosaic [ col ] [ row ] );
          }

          file.writeInt ( record_length );
       }
    }
}
