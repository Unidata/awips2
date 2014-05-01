/*
 * Created on Feb 26, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : March 16, 2004
 * Revised:
 * Chip Gobs March 16, 2005 (exactly 1 year later!)
 * added variables _paramsValidTime and _timeSeriesBasisTime
 * now use them instead of using _extractionTime directly for SacSmaParams validTime and
 * PE and Runoff time series basistime.
 * The _timeSeriesBasisTime variable is set to _extractionTime, but _paramsValidTime is the
 * 00:00:00 of the same date as _extractionTime
 */

package ohd.hseb.sshp.messaging;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.StringParser;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

public class SacXMLEncoder
{
    private String _basinId = null;
	private String _extractFileName = null;
	private Document _doc = null;
    
    private BufferedReader _reader = null;
    
    private final long HOUR_IN_MILLIS = 1 * 60 * 60 * 1000;
    private final long MILLIS_PER_DAY = 24 * HOUR_IN_MILLIS; 
    private final String SOURCE = "RFC"; 
    private long _extractionTime = 0;
    private long _paramsValidTime = 0;
    private long _timeSeriesBasisTime = 0;
   // private String _executionTimeString = null;
	private FileLogger _logger = null;
    
    // ---------------------------------------------------------------------
    
	public SacXMLEncoder( String basinId, String extractFileName, Document outputXMLDocument, FileLogger logger )
	{
        _basinId = basinId;
		_extractFileName = extractFileName;
		_doc = outputXMLDocument;
		_logger = logger;
		_logger.log( "" );
		//_executionTimeString = DbTimeHelper.getDateTimeStringFromLongTime( System.currentTimeMillis() );
	}

    // ---------------------------------------------------------------------
  
	public void parse() throws OfsFileParserException
	{
//		writeXmlToFile( _doc, "D:/data/xmlencoderoutput.txt" );
        
        try
        {
            _reader = new BufferedReader( new FileReader( _extractFileName ) );
            
            Element rootElement = _doc.getRootElement();
            Element runoffTimeSeriesListElement = rootElement.getChild( "RunoffTimeSeriesList" );
            Element peTimeSeriesListElement = rootElement.getChild( "PeTimeSeriesList" );
            Element sacParamsListElement = rootElement.getChild( "SacParamsList" );
            Element sacStateListElement = rootElement.getChild( "SacStateList" );
            Element monthlyValuesListElement = rootElement.getChild( "MonthlyValuesList" );
            
			String line = findLine( "Extracted on" );
 
			if ( line == null ) 
			{
				throw new OfsFileParserException( "line empty" );
			}
			
			int index = getNextCharIndex(line,"Extracted on");
			if ( index >= 0 )
			{
				_extractionTime = parseTimeString( line.substring( index, line.length() ) );
				
				//set to the same as the extraction time, since we don't have anything better
				_timeSeriesBasisTime = _extractionTime;
				
				//round back to the beginning of the same day,
				//so that whatever parameters we have for that day will be usable with
				//the latest states of that day
				_paramsValidTime = _extractionTime /= MILLIS_PER_DAY;
				_paramsValidTime *= MILLIS_PER_DAY;
				
				
			}
			
			_logger.log( "Parsing " + _extractFileName );
			_logger.log( "-----" );
            line = _reader.readLine();
			
			if ( line == null )
			{
				throw new OfsFileParserException( "line empty" );
			}

			parseRunoffTimeSeries( runoffTimeSeriesListElement );
			parsePeTimeSeries( peTimeSeriesListElement );
			parseMonthlyValues( monthlyValuesListElement );
			parseSacParams( sacParamsListElement );
			parseSacState( sacStateListElement );
        }
	    
	    catch ( OfsFileParserException e )
	    {
	    	_logger.log( _extractFileName + " is not a complete extracted file" );
	    	throw new OfsFileParserException( "line empty" );
	    }
	    
        catch ( FileNotFoundException e )
        {
        	_logger.log( _extractFileName + " not found." );
            e.printStackTrace( _logger.getPrintWriter() );  //we will add this to everything later
        }
        catch ( IOException e )
        {
        	_logger.log( "Unable to read from " + _extractFileName );
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        finally
        {
        	try
        	{
        		_reader.close();
        	}
        	catch ( IOException e )
        	{
        		_logger.log( "Unable to close " + _extractFileName );
        	}
        }
        

	}

    //  ---------------------------------------------------------------------

	private long parseTimeString( String timeString )
	{
     	// example: String format = "yyyy-MM-dd HH:mm:ss"
		String formatString = "MM/dd/yyyy HHmm";
		SimpleDateFormat sdf = new SimpleDateFormat( formatString );
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));    
		long time = 0;
		
		try
		{
			time = sdf.parse( timeString ).getTime();
		}
		catch ( ParseException e )
		{
			e.printStackTrace();
		}
		 
		return time;
	}

	//  ---------------------------------------------------------------------
    private int getNextCharIndex(String line, String searchString)
    {
        int index = line.indexOf(searchString);
        if (index > -1)
        {
           index += searchString.length();    
        }
        
        return index;
    }
    // ---------------------------------------------------------------------
    private void parseRunoffTimeSeries( Element runoffTimeSeriesListElement )
    {
		String line = null;
		int duration = 0;
        
		Element roTimeSeriesElement = new Element( "RunoffTimeSeries" );
		Element measurementListElement = new Element( "MeasurementList" );

		findLine( "RO Time Series Begin" );  // Searches the input file for the string passed in
		
		try
		{
			//read next line
			line = _reader.readLine();
			if ( line.startsWith( "No RO time series" ) )
			{
				roTimeSeriesElement.addContent(getElement("basinId", _basinId));
				roTimeSeriesElement.addContent( getElement( "exists", "FALSE" ) );
			}
			else // there is a Runoff time series
			{
				_logger.log( "Extracting RO Timeseries" );
				roTimeSeriesElement.addContent(getElement("exists", "TRUE"));
				roTimeSeriesElement.addContent(getElement("basinId", _basinId));
				String timeString = DbTimeHelper.getDateTimeStringFromLongTime( _timeSeriesBasisTime );
				roTimeSeriesElement.addContent(getElement("basisTime", timeString));

				int index = getNextCharIndex( line, "Step=" );
				
				if ( index > -1 )
				{
					String durationString = line.substring( index, index + 2 ).trim();
					duration = Integer.parseInt( durationString );
					roTimeSeriesElement.addContent( getElement( "duration", getDuration( durationString ) ) ); 
				}

				index = getNextCharIndex( line,"Data Units=" );

				if ( index > -1 )
				{
					String unitsString = line.substring(index).toLowerCase().trim();
					roTimeSeriesElement.addContent(getElement("units", unitsString));
				}

                
				roTimeSeriesElement.addContent( measurementListElement );

				line = _reader.readLine();
				// parses the start and end time for the measurements
				String measurementTimeString = null;
				int startIndex = getNextCharIndex( line, "Time=" );
				int endIndex = getNextCharIndex( line, "Z" );
				measurementTimeString = line.substring( startIndex, endIndex - 1 ).trim();
				long measurementTimeLong = parseTimeString( measurementTimeString + "00" );
				
				startIndex = getNextCharIndex( line, "Count=" );
				endIndex = startIndex + 2;
				
				line = _reader.readLine();
				index = getNextCharIndex( line, "Values=" );
				line = line.substring( index, line.length() );				
				
				while ( ! line.startsWith( "RO Time Series End" ) )
				{
					String[] valueStringArray = StringParser.tokenize( line );
					
					for ( int i = 0; i < valueStringArray.length; i++ ) 
					{
						Element measurementElement = new Element( "Measurement" );
						measurementElement.addContent( getElement( "time", DbTimeHelper.getDateTimeStringFromLongTime( measurementTimeLong ) ) );
						measurementTimeLong += duration * HOUR_IN_MILLIS;
						measurementElement.addContent( getElement( "value", valueStringArray[ i ] ) );
						measurementListElement.addContent( measurementElement );
					}
					line = _reader.readLine();
				}
		   }  // close bracket for else statement
    	   runoffTimeSeriesListElement.addContent( roTimeSeriesElement );
		}
		catch ( IOException e )
		{
			e.printStackTrace();
		}
    }
    
    
    // ---------------------------------------------------------------------
    private void parsePeTimeSeries( Element peTimeSeriesListElement )
    {
        String line = null;
        int duration = 0;
        
        Element peTimeSeriesElement = new Element( "PeTimeSeries" );
        Element measurementListElement = new Element( "MeasurementList" );

		findLine( "PE Time Series Begin" );  // Searches the input file for the string passed in
		
        try
        {
            //read next line
            line = _reader.readLine();
            if ( line.startsWith( "No PE Time Series" ) )
            {
				peTimeSeriesElement.addContent(getElement("basinId", _basinId));
                peTimeSeriesElement.addContent(getElement("isUsed", "FALSE"));
            }
            else // there is a PE time series
            {
            	_logger.log( "Extracting PE Timeseries" );
                peTimeSeriesElement.addContent(getElement("isUsed", "TRUE"));
                peTimeSeriesElement.addContent(getElement("basinId", _basinId));
				String timeString = DbTimeHelper.getDateTimeStringFromLongTime( _timeSeriesBasisTime );
				peTimeSeriesElement.addContent(getElement("basisTime", timeString));

				int index = getNextCharIndex( line, "Step=" );
				
				if ( index > -1 )
				{
					String durationString = line.substring( index, index + 2 ).trim();
					duration = Integer.parseInt( durationString );
					peTimeSeriesElement.addContent( getElement( "duration", getDuration( durationString ) ) ); 
				}

				index = getNextCharIndex( line,"Data Units=" );

				if ( index > -1 )
				{
					String unitsString = line.substring(index).toLowerCase().trim();
					peTimeSeriesElement.addContent(getElement("units", unitsString));
				}

                
				peTimeSeriesElement.addContent( measurementListElement );

				line = _reader.readLine();
				// parses the start and end time for the measurements
				String measurementTimeString = null;
				int startIndex = getNextCharIndex( line, "Time=" );
				int endIndex = getNextCharIndex( line, "Z" );
				measurementTimeString = line.substring( startIndex, endIndex - 1 ).trim();
				long measurementTimeLong = parseTimeString( measurementTimeString + "00" );
				
				startIndex = getNextCharIndex( line, "Count=" );
				endIndex = startIndex + 2;
				
				line = _reader.readLine();
				index = getNextCharIndex( line, "Values=" );
				line = line.substring( index, line.length() );				
				
				while ( ! line.startsWith( "PE Time Series End" ) )
				{
					String[] valueStringArray = StringParser.tokenize( line );
					
					for ( int i = 0; i < valueStringArray.length; i++ ) 
					{
						Element measurementElement = new Element( "Measurement" );
						measurementElement.addContent( getElement( "time", DbTimeHelper.getDateTimeStringFromLongTime( measurementTimeLong ) ) );
						measurementTimeLong += duration * HOUR_IN_MILLIS;
						measurementElement.addContent( getElement( "value", valueStringArray[ i ] ) );
						measurementListElement.addContent( measurementElement );
					}
					line = _reader.readLine();
				}
				
           }
		   peTimeSeriesListElement.addContent(peTimeSeriesElement);
        }
		catch ( IOException e )
		{
			e.printStackTrace();
		}
    } 
    
    //  ---------------------------------------------------------------------
    
    private Element getElement(String elementName, String elementText)
    {
        Element element = new Element(elementName);
        element.setText(elementText);
        
        return element;
    }

    // ---------------------------------------------------------------------
    
	private void parseMonthlyValues( Element monthlyValuesListElement )
	{
          // this is not done yet - work on this Gautam 
		String line = null;
		boolean done = false;
		Element monthlyValuesElement = new Element( "MonthlyValues" );
		String[] valuesStringArray = null;
		
		findLine( "Monthly Values Begin" );

		try
		{
			line = _reader.readLine();
			if ( line != null )
			{
				if ( line.startsWith( "No Monthly Values" ) )
				{
					done = true;
				}
				else // there are monthly Values
				{
					_logger.log( "Extracting MonthlyValues" );
					monthlyValuesElement.addContent( getElement( "locationId", _basinId ) );
					int index = getNextCharIndex( line, "Type=" );
					String isAdjustmentString = line.substring( index, line.length() );
					if ( isAdjustmentString.startsWith( "PE Adjustment" ) )
					{
						monthlyValuesElement.addContent( getElement( "isAdjustment", "TRUE" ) );
					}
					else
					{
						monthlyValuesElement.addContent( getElement( "isAdjustment", "FALSE" ) );
					}
						
					line = _reader.readLine();
					valuesStringArray = StringParser.tokenize( line );

					monthlyValuesElement.addContent( getElement( "janValue", valuesStringArray[ 0 ] ) );
					monthlyValuesElement.addContent( getElement( "febValue", valuesStringArray[ 1 ] ) );
					monthlyValuesElement.addContent( getElement( "marValue", valuesStringArray[ 2 ] ) );
					monthlyValuesElement.addContent( getElement( "aprValue", valuesStringArray[ 3 ] ) );
					monthlyValuesElement.addContent( getElement( "mayValue", valuesStringArray[ 4 ] ) );
					monthlyValuesElement.addContent( getElement( "junValue", valuesStringArray[ 5 ] ) );
					monthlyValuesElement.addContent( getElement( "julValue", valuesStringArray[ 6 ] ) );
					monthlyValuesElement.addContent( getElement( "augValue", valuesStringArray[ 7 ] ) );
					monthlyValuesElement.addContent( getElement( "sepValue", valuesStringArray[ 8 ] ) );
					monthlyValuesElement.addContent( getElement( "octValue", valuesStringArray[ 9 ] ) );
					monthlyValuesElement.addContent( getElement( "novValue", valuesStringArray[ 10 ] ) );
					monthlyValuesElement.addContent( getElement( "decValue", valuesStringArray[ 11 ] ) );
					done = true;
				}
			}
			if ( done )
			{
				monthlyValuesListElement.addContent( monthlyValuesElement );
			}
		}
		catch ( IOException e )
		{
			e.printStackTrace();
		}
    }
    
    // ---------------------------------------------------------------------
    
    private void parseSacParams(Element sacParamsListElement)
    {
		String line = null;
		boolean done = false;
		Element sacParamsElement = new Element( "SacParams" );

		findLine( "SAC Params Begin" );

		try
		{
			line = _reader.readLine();
			if ( line != null )
			{
				_logger.log( "Extracting SacSmaParameters" );
				sacParamsElement.addContent( getElement( "basinId", _basinId ) );
				sacParamsElement.addContent( getElement( "source", SOURCE ) );
				sacParamsElement.addContent( getElement( "validTime", DbTimeHelper.getDateTimeStringFromLongTime( _paramsValidTime ) ) );
				
				if ( ! line.startsWith( "No SAC Params" ) )
				{
					while ( ! line.startsWith( "SAC Params End" ) )
					{
						int index = getNextCharIndex( line, "=" );
						String paramString = line.substring( 0, index - 1 ).trim().toLowerCase();
						String valueString = line.substring( index, line.length() ).trim();
						sacParamsElement.addContent( getElement( paramString, valueString ) );
						line = _reader.readLine();
					}
					done = true;
				}
			}
			if ( done )
			{
				sacParamsListElement.addContent( sacParamsElement );
			}
		}
		catch ( IOException e )
		{
			e.printStackTrace();
		}
      
    }
    
    // ---------------------------------------------------------------------
    private void parseSacState( Element sacStateListElement )
    {
		String line = null;
		boolean done = false;
		Element sacStateElement = new Element( "SacState" );
		long validTime = 0;
				
		findLine( "SAC State Begin" );
		try
		{
			line = _reader.readLine();
			if ( line != null )
			{
				_logger.log( "Extracting SacSmaState" );
				sacStateElement.addContent( getElement( "basinId", _basinId ) );
				sacStateElement.addContent( getElement( "source", SOURCE ) );
				int dateIndex = getNextCharIndex( line, "date=" );
				String dateString = line.substring( dateIndex, line.length() ).trim();
				dateString = dateString.substring( 0, dateString.length() - 1 );
				long nonRoundedCarryOverDateTimeLong = parseTimeString( dateString + "00" );
				
				
				long carryOverDateTimeLong = getRoundedValidTime( nonRoundedCarryOverDateTimeLong );
				if ( carryOverDateTimeLong != nonRoundedCarryOverDateTimeLong )
				{
					_logger.log( "Original valid time was NOT 12Z!" );
				}
				sacStateElement.addContent( getElement( "validTime", DbTimeHelper.getDateTimeStringFromLongTime( carryOverDateTimeLong ) ) );
				sacStateElement.addContent( getElement( "basisTime", DbTimeHelper.getDateTimeStringFromLongTime( carryOverDateTimeLong ) ) );
				
				line = _reader.readLine();
				
				if ( ! line.startsWith( "No SAC States" ) )
				{
					while ( ! line.startsWith( "SAC State End" ) )
					{
						int index = getNextCharIndex( line, "=" );
						String stateString = line.substring( 0, index - 1 ).trim().toLowerCase();
						String valueString = line.substring( index, line.length() ).trim();
						sacStateElement.addContent( getElement( stateString, valueString ) );
						line = _reader.readLine();
					}
					done = true;
				}
			}
			if ( done )
			{
				sacStateListElement.addContent( sacStateElement );
			}
		}
		catch ( IOException e )
		{
			e.printStackTrace();
		}
    }
    
    private long getRoundedValidTime( long extractionTime )
    {
    	long validTime = extractionTime;
    	
		validTime /= MILLIS_PER_DAY;
		validTime *= MILLIS_PER_DAY;
		validTime += ( MILLIS_PER_DAY / 2 );
    	if ( validTime > extractionTime )
    	{
    		validTime -= MILLIS_PER_DAY;
    	}
    	return validTime;
    }
 
    // ---------------------------------------------------------------------
  
    private void writeXmlToFile(Document outputXmlDoc, String xmlFileName)
    {
    	Format format = Format.getRawFormat();
    	format.setIndent("   ");
        XMLOutputter _xmlOutputter = new XMLOutputter(format);
  
        try
        {
           FileWriter writer = new FileWriter(xmlFileName);
           _xmlOutputter.output(outputXmlDoc, writer);
           writer.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }
   
    // ---------------------------------------------------------------------
 

    private static Document getBaseDocument()
    {
        //this is used only for testing
        
        Document doc = new Document();
        
        Element rootElement = new Element("SacModelMessage");

        doc.setRootElement(rootElement);
        
        Element sacParamsListElement = new Element("SacParamsList");
        Element sacStateListElement = new Element("SacStateList");
        Element peTimeSeriesListElement = new Element("PeTimeSeriesList");
        Element runoffTimeSeriesElement = new Element("RunoffTimeSeriesList");
        Element monthlyValuesListElement = new Element("MonthlyValuesList");
        
        rootElement.addContent(sacParamsListElement);
        rootElement.addContent(sacStateListElement);
        rootElement.addContent(peTimeSeriesListElement);
        rootElement.addContent(runoffTimeSeriesElement);
        rootElement.addContent(monthlyValuesListElement);
    
        return doc;
    }
	
	// ---------------------------------------------------------------------

	private String findLine( String searchString )
	{
		String line = null;
		try
		{
			line = _reader.readLine();
			while ( ! line.startsWith( searchString ) )
			{
				line = _reader.readLine();
			}
		}
		catch ( IOException e )
		{
			e.printStackTrace();
			line = null;
		}
		return line;
	}
	
	// ---------------------------------------------------------------------

	private String getDuration( String durationString )
	{
		int duration = Integer.parseInt( durationString );
		duration += 1000;
		return ( "" + duration );
	}
	
    // ---------------------------------------------------------------------

	public static void main(String[] args)
	{
        String extractedFileName = null;
        String processedFileName = null;
        SacXMLEncoder encoder = null;
        
        String basinId = null;
        
        Document xmlOutputDoc = getBaseDocument(); 
        
        // file1
		extractedFileName = "D:/Data/SacEncoderInput/BLAS.txt";
//		extractedFileName = args[ 0 ];
        basinId = "BLASE";
        encoder = new SacXMLEncoder( basinId, extractedFileName, xmlOutputDoc, new FileLogger( "D:/data/log/SacXMLEncoder.log", true, true ) );
        try
        {
        	encoder.parse();
        }
        catch( OfsFileParserException e )
        {
        }
        
          
        // write out the output
        processedFileName = "D:/Data/XMLFiles/BLASE.xml";
//		processedFileName = "/fs/home/gsood/processed_SJU.xml";
        encoder.writeXmlToFile( xmlOutputDoc, processedFileName );

	}
    
    // ---------------------------------------------------------------------
  
}
