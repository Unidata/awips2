/*
 * Created on August 25, 2005
 * 
 */

/**
 * @author Bryon Lawrence
 * 
 * This retrieves the contents of the coord_xxxx.dat file.
 * For a RFC, xxxx is the name of the office.  For a WFO,
 * xxxx is 'host'.
 */
package ohd.hseb.prism;

import java.io.*;
import ohd.hseb.util.*;

public class GeoData {
	
	static private String _filepath;
	private int _southwest_column = -1;
	private int _southwest_row = -1;
	private int _number_of_columns = -1;
	private int _number_of_rows = -1;

        // Static initializer
        static
        {
           build_filepath ( );
        }
	
	static private void build_filepath ( )
	{
		String office_name = "";
		String geo_data_dir = "";
		AppsDefaults apps_defaults = new AppsDefaults ( );
		
		office_name = apps_defaults.getToken ( "st3_rfc" );
		geo_data_dir = apps_defaults.getToken ( "geo_data" );
		
		_filepath = geo_data_dir + "/" + office_name +"/ascii/coord_" +
		            office_name + ".dat";
	}
	
	private void read_geo_data ( ) throws IOException
	{
		BufferedReader _reader = new BufferedReader ( 
                                         new FileReader ( _filepath ) );
		_southwest_column = Integer.parseInt( _reader.readLine ( ) );
		_southwest_row = Integer.parseInt ( _reader.readLine ( ) );
		_number_of_columns = Integer.parseInt ( _reader.readLine ( ) );
		_number_of_rows = Integer.parseInt ( _reader.readLine ( ) );
					
	}
	
	public int getSouthwestColumn ( )
	{
		return _southwest_column;
	}
	
	public int getSouthwestRow ( )
	{
		return _southwest_row;
	}
	
	public int getNumColumns ( )
	{
		return _number_of_columns;
	}
	
	public int getNumRows ( )
	{
		return _number_of_rows;
	}

        static public String getFilePath ( )
        {
                 return _filepath;
        }
	
	GeoData ( ) throws IOException
	{
		read_geo_data ( );
	}
}
