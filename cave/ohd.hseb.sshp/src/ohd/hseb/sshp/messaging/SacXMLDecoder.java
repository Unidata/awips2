/*
 * Created on Sep 11, 2003
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : March 16, 2004
 * Purpose:  Decodes XML input files and posts the data to the database.
 */
 
package ohd.hseb.sshp.messaging;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.model.MonthlyValues;
import ohd.hseb.model.sacsma.SacSmaParameters;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.sshp.FcstTsDescriptor;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;


public class SacXMLDecoder
{
	private String _inputDir 		  = null;
	private String _archiveDir 		  = null;
	private String _logFullPath		  = null;
	private long _sleepTime 		  = 0;
	private Document _doc 			  = null;
	private String backupInputFilePath = null;
	private boolean _isTest 		  = true;
	
	private static final String PE_TIMESERIES_PE_CODE = "EA";
	private static final String RUNOFF_TIMESERIES_PE_CODE = "QB";
	private static final String TS = "FZ";
	private static final String EXTREMUM = "Z";
	private static final float PROBABILITY = -1.0f;
	private static final String SHEF_QUAL_CODE = "Z";
	private static final int GOOD_QC = 1879048191;
	private static final short REVISION = 0;
	private static final String PRODUCT_ID = "SSHP_XFER";
	
	private static final String MONTHLY_VALUES_TS = "FZ";
	private static final String MONTHLY_VALUES_EXTREMUM = "Z";
	private static final String MONTHLY_VALUES_PE = "EA";
	private static final short MONTHLY_VALUES_DUR = 2001;
	private static final MeasuringUnit DB_MEASURING_UNIT = MeasuringUnit.inches;
	private static final int INTERVAL_IN_HOURS = 1;
	
	private static Map _unitsMap = new HashMap();
	
	private FileLogger _logger = null;
	
	private String _connectionString = null;
	
	private DecoderDataMgr _dataMgr = null;
	
	public SacXMLDecoder( String connectionString )
	{
		_connectionString = connectionString;
	}
	
	private void execute()
	{
		boolean executeProgram  = true;
		File localDirFile     	= null;
		File[] localFileArray 	= null;
		File backupInputFile 	= null;
		localDirFile 		 	= new File( _inputDir );
		localFileArray 			= localDirFile.listFiles();
		SAXBuilder builder 		= new SAXBuilder();
		String fileName 		= null;
		File inputFile 			= null;
		Date time				= new Date();
		int badFileCounter      = 0;
		boolean keepFileOpen 	= true;
		boolean appendDateTime 	= true;
		CodeTimer timer1 		= null;

		_logger = new FileLogger( _logFullPath, keepFileOpen, appendDateTime );
		timer1 = new CodeTimer( _logger );

		connect();
		
		initUnitsMap();
		
		while ( executeProgram )
		{
			localFileArray = getFileList( localDirFile );
			if ( ( localFileArray.length - badFileCounter ) > 0 )
			{
				for ( int i = 0; i < localFileArray.length; i++ )
				{
					try
					{
						builder.setExpandEntities( true );
						fileName = localFileArray[ i ].getAbsolutePath();
						_logger.log( "Processing " + localFileArray[ i ].getName() );
						timer1.start();
						
						_doc = builder.build( fileName );
						parseXMLFiles();
						backupInputFile = new File( _archiveDir + "/" + localFileArray[ i ].getName() + "_" + time.getTime() );
					
						inputFile = new File( fileName );
						inputFile.renameTo( backupInputFile );
						timer1.stop( "Finished processing " + localFileArray[ i ].getName() + " in " );
					}
					catch ( JDOMException e )
					{
						badFileCounter++;
						_logger.log( "     " + fileName + " is not a valid XML file." );
					}

					catch ( IOException e )
					{
						e.printStackTrace();
					}	
				}
			}
			else  // if localFileArray.length <= 0  (No files found)  
			{
				
				if ( _isTest )
				{
                    _logger.log( "No more valid XML Files found.  Moving back to sleep mode." );
					sleep();
				}
				else
				{
                    _logger.log( "No more valid XML Files found.  Processing Complete." );
					executeProgram = false;
				}
			}			
		} // end While loop
	}
	
	private File[] getFileList( File localDirFile )
	{
		File[] returnFileArray = null;
		File[] currentFileArray = null;
		List tempFileList = new ArrayList();
		
		currentFileArray = localDirFile.listFiles();
		
		
		for ( int i = 0; i < currentFileArray.length; i++ )
		{
			if ( ! currentFileArray[ i ].isDirectory() )
			{
				tempFileList.add( currentFileArray[ i ] );
			}
		}
		
		returnFileArray = (File[]) tempFileList.toArray( new File[ tempFileList.size() ] );
		
		return returnFileArray;
	}
	
	private void initUnitsMap()
	{
		_unitsMap.put( new String( "mm" ), MeasuringUnit.mm );
		_unitsMap.put( new String( "cm" ), MeasuringUnit.cm );
		_unitsMap.put( new String( "m" ), MeasuringUnit.meters );
		_unitsMap.put( new String( "in" ), MeasuringUnit.inches );
		_unitsMap.put( new String( "ft" ), MeasuringUnit.feet );
	}
	
	private void sleep()
	{
		try
		{
//			System.out.println( "Sleeping for " + _sleepTime/1000 + " seconds..." );
			Thread.sleep( _sleepTime );
		}
		catch ( InterruptedException e ) 
		{
			e.printStackTrace();
		}
	}
	
	
	private void parseXMLFiles()
	{
		Element sacParamsListElement			= _doc.getRootElement().getChild( "SacParamsList" );
		Element sacStateListElement           	= _doc.getRootElement().getChild( "SacStateList" );
		Element peTimeSeriesListElement       	= _doc.getRootElement().getChild( "PeTimeSeriesList" );
		Element runoffTimeSeriesListElement 	= _doc.getRootElement().getChild( "RunoffTimeSeriesList" );
		Element monthlyValuesListElement 		= _doc.getRootElement().getChild( "MonthlyValuesList" );
		
		Element sacParamsElement     	        = null;
		Element sacStateElement         	    = null;
		Element peTimeSeriesElement				= null;
		Element runoffTimeSeriesElement        	= null;
		Element monthlyValuesElement			= null;
		
		SacSmaParameters sacSmaParams 			= new SacSmaParameters();
		SacSmaState sacSmaState 				= new SacSmaState();
		MonthlyValues monthlyValues 			= new MonthlyValues();
		
		List elementList 					  	= null;
		
//		--------- Processing Sac Parameters

		elementList = sacParamsListElement.getChildren( "SacParams" );
		for ( int i = 0; i < elementList.size(); i++ )
		{
			SacSmaParameters latestParams = null;
			String basinId = null;
			String source = null;
			
			sacParamsElement = (Element) elementList.get( i );
			sacSmaParams = retrieveSacSmaParams( sacParamsElement );
			basinId = sacSmaParams.getBasinId();
			source = sacSmaParams.getSource();
				
			latestParams = _dataMgr.loadNewestSacSmaParams(basinId, source );
			if ( latestParams == null )
			{
				_dataMgr.saveParams( sacSmaParams );
			}
			else if ( ! latestParams.equalsValues( sacSmaParams ) )
			{
				_dataMgr.saveParams( sacSmaParams );
			}
		}
		
//		--------- Processing Sac States

		elementList = sacStateListElement.getChildren( "SacState" );
		for ( int i = 0; i < elementList.size(); i++ )
		{
			sacStateElement = (Element) elementList.get( i );
			sacSmaState = retrieveSacSmaState( sacStateElement );
			_dataMgr.saveState( sacSmaState );
		}
		
// --------- Processing Pe Timeseries

		elementList = peTimeSeriesListElement.getChildren( "PeTimeSeries" );
		for ( int i = 0; i < elementList.size(); i++ )
		{
			peTimeSeriesElement = (Element) elementList.get( i );
			processPeTimeSeries( peTimeSeriesElement );
		}
// --------- Processing Runoff Timeseries
		
		elementList = runoffTimeSeriesListElement.getChildren( "RunoffTimeSeries" );
		for ( int i = 0; i < elementList.size(); i++ )
		{
			runoffTimeSeriesElement = (Element) elementList.get( i );
			processRunoffTimeSeries( runoffTimeSeriesElement );
		}

//		--------- Processing MonthlyValues
		
		elementList = monthlyValuesListElement.getChildren( "MonthlyValues" );
		for ( int i = 0; i < elementList.size(); i++ )
		{
			monthlyValuesElement = (Element) elementList.get( i );
			monthlyValues = retrieveMonthlyValues( monthlyValuesElement );
			_dataMgr.saveMonthlyValues( monthlyValues );
		}
	}

//	-----------------------------------------------------------------------------     
	
	private SacSmaParameters retrieveSacSmaParams( Element e )
	{
		SacSmaParameters currentSacSmaParams = new SacSmaParameters();
		
		currentSacSmaParams.setBasinId( getElementStringValue( e, "basinId" ) );
		currentSacSmaParams.setSource( getElementStringValue( e, "source" ) );
		currentSacSmaParams.setValidTime( getElementLongTimeValue( e, "validTime" ) );
		currentSacSmaParams.setPostingTime( System.currentTimeMillis() );
		currentSacSmaParams.setUztwm( getElementDoubleValue( e, "uztwm" ) );	 
		currentSacSmaParams.setUzfwm( getElementDoubleValue( e, "uzfwm" ) );
		currentSacSmaParams.setUzk( getElementDoubleValue( e, "uzk" ) );
		currentSacSmaParams.setPctim( getElementDoubleValue( e, "pctim" ) );
		currentSacSmaParams.setAdimp( getElementDoubleValue( e, "adimp" ) );
		currentSacSmaParams.setRiva( getElementDoubleValue( e, "riva" ) );
		currentSacSmaParams.setZperc( getElementDoubleValue( e, "zperc" ) );
		currentSacSmaParams.setRexp( getElementDoubleValue( e, "rexp" ) );
		currentSacSmaParams.setLztwm( getElementDoubleValue( e, "lztwm" ) );
		currentSacSmaParams.setLzfsm( getElementDoubleValue( e, "lzfsm" ) );
		currentSacSmaParams.setLzfpm( getElementDoubleValue( e, "lzfpm" ) );
		currentSacSmaParams.setLzsk( getElementDoubleValue( e, "lzsk" ) );
		currentSacSmaParams.setLzpk( getElementDoubleValue( e, "lzpk" ) );
		currentSacSmaParams.setPfree( getElementDoubleValue( e, "pfree" ) );
		currentSacSmaParams.setRserv( getElementDoubleValue( e, "rserv" ) );
		currentSacSmaParams.setSide( getElementDoubleValue( e, "side" ) );
		currentSacSmaParams.setPxadj( getElementDoubleValue( e, "pxadj" ) );
		currentSacSmaParams.setPeadj( getElementDoubleValue( e, "peadj" ) );
		currentSacSmaParams.setEfc( getElementDoubleValue( e, "efc" ) );
		
		
		return currentSacSmaParams;
	}
	
//	-----------------------------------------------------------------------------     

	private SacSmaState retrieveSacSmaState( Element e )
	{
		SacSmaState currentSacSmaState = new SacSmaState();

		currentSacSmaState.setBasinId( getElementStringValue( e, "basinId") );
		currentSacSmaState.setSource( getElementStringValue( e, "source") );
		currentSacSmaState.setValidTime( getElementLongTimeValue( e, "validTime" ) );
		currentSacSmaState.setBasisTime( getElementLongTimeValue( e, "basisTime" ) );
		currentSacSmaState.setPostingTime( System.currentTimeMillis() );
		currentSacSmaState.setUztwc( getElementDoubleValue( e, "uztwc" ) );
		currentSacSmaState.setUzfwc( getElementDoubleValue( e, "uzfwc" ) );
		currentSacSmaState.setLztwc( getElementDoubleValue( e, "lztwc" ) );
		currentSacSmaState.setLzfsc( getElementDoubleValue( e, "lzfsc" ) );
		currentSacSmaState.setLzfpc( getElementDoubleValue( e, "lzfpc" ) );
		currentSacSmaState.setAdimc( getElementDoubleValue( e, "adimc" ) );
		
		return currentSacSmaState;
	}
	
//	-----------------------------------------------------------------------------     

	private MonthlyValues retrieveMonthlyValues( Element e )
	{
		MonthlyValues currentMonthlyValues = new MonthlyValues();
		double[] valueArray = new double[ 12 ];

		currentMonthlyValues.setBasinId( getElementStringValue( e, "locationId" ) );
		currentMonthlyValues.setPe( MONTHLY_VALUES_PE );
		currentMonthlyValues.setDur(MONTHLY_VALUES_DUR );
		currentMonthlyValues.setTs( MONTHLY_VALUES_TS );
		currentMonthlyValues.setExtremum( MONTHLY_VALUES_EXTREMUM );
		if ( getElementStringValue( e, "isAdjustment" ).equalsIgnoreCase( "TRUE" ) )
		{
			currentMonthlyValues.setAdjustment( true );
		}
		else
		{
			currentMonthlyValues.setAdjustment( false );
		}
		currentMonthlyValues.setPostingTime( System.currentTimeMillis() );

		valueArray[ 0 ] = getElementDoubleValue( e, "janValue" );
		valueArray[ 1 ] = getElementDoubleValue( e, "febValue" );
		valueArray[ 2 ] = getElementDoubleValue( e, "marValue" );
		valueArray[ 3 ] = getElementDoubleValue( e, "aprValue" );
		valueArray[ 4 ] = getElementDoubleValue( e, "mayValue" );
		valueArray[ 5 ] = getElementDoubleValue( e, "junValue" );
		valueArray[ 6 ] = getElementDoubleValue( e, "julValue" );
		valueArray[ 7 ] = getElementDoubleValue( e, "augValue" );
		valueArray[ 8 ] = getElementDoubleValue( e, "sepValue" );
		valueArray[ 9 ] = getElementDoubleValue( e, "octValue" );
		valueArray[ 10 ] = getElementDoubleValue( e, "novValue" );
		valueArray[ 11 ] = getElementDoubleValue( e, "decValue" );
		currentMonthlyValues.setValues( valueArray );
		
		return currentMonthlyValues;
	}
	
//	-----------------------------------------------------------------------------     

	private void processPeTimeSeries( Element e )
	{
		String usedString = getElementStringValue( e, "isUsed" );
		boolean process = false;  // Determines whether or not to process the TS
		RegularTimeSeries peTimeSeries = null;
		FcstTsDescriptor descriptor = new FcstTsDescriptor( "FcstOther" );

		if ( usedString.trim().equalsIgnoreCase( "true" ) )
		{
			process = true;
		}
		else
		{
			process = false;
		}
		
		if ( process )
		{
			String xmlUnitString = getElementStringValue( e, "units" );
			Element measurementListElement = e.getChild( "MeasurementList" );
			Element measurementElement = null;
			List elementList = new ArrayList();
			Measurement measurement = null;
			MeasuringUnit xmlMeasuringUnit = (MeasuringUnit) _unitsMap.get( xmlUnitString );
			
			descriptor.setLid( getElementStringValue( e, "basinId" ) );
			descriptor.setPe( PE_TIMESERIES_PE_CODE );
			descriptor.setDur( getElementShortValue( e, "duration" ) );
			descriptor.setTs( TS );
			descriptor.setExtremum( EXTREMUM );
			descriptor.setProbability( PROBABILITY );
			descriptor.setBasistime( getElementLongTimeValue( e, "basisTime" ) );
			descriptor.setShef_qual_code( SHEF_QUAL_CODE );
			descriptor.setQuality_code( GOOD_QC );
			descriptor.setRevision( REVISION );
			descriptor.setProduct_id( PRODUCT_ID );
			descriptor.setProducttime( getElementLongTimeValue( e, "basisTime" ) );
			descriptor.setPostingtime( System.currentTimeMillis() );
			
			elementList = measurementListElement.getChildren( "Measurement" );
			
			for ( int i = 0; i < elementList.size(); i++ )
			{
				measurementElement = (Element) elementList.get( i );
				double xmlValue = getElementDoubleValue( measurementElement, "value" );
				measurement = new Measurement( xmlValue, xmlMeasuringUnit );
				long time = getElementLongTimeValue( measurementElement, "time" );

				if ( i == 0 )
				{
					peTimeSeries = new RegularTimeSeries( time, time, INTERVAL_IN_HOURS, 
					                                      DB_MEASURING_UNIT );
				}
				peTimeSeries.setMeasurementByTime( measurement, time );
			}
			_dataMgr.saveFcstTimeSeries( descriptor, peTimeSeries );
			
		} // end if (process)
	}

//	-----------------------------------------------------------------------------     
	
	private void processRunoffTimeSeries( Element e )
	{
		String processString = getElementStringValue( e, "exists" );
		boolean process = false;  // Determines whether or not to process the TS
		RegularTimeSeries runoffTimeSeries = null;
		FcstTsDescriptor descriptor = new FcstTsDescriptor( "FcstDischarge" );
		
		if ( processString.trim().equalsIgnoreCase( "true" ) )
		{
			process = true;
		}
		else
		{
			process = false;
		}
		
		if ( process )
		{
			String xmlUnitString = getElementStringValue( e, "units" );
			Element measurementListElement = e.getChild( "MeasurementList" );
			Element measurementElement = null;
			List elementList = new ArrayList();
			Measurement measurement = null;
			MeasuringUnit xmlMeasuringUnit = (MeasuringUnit) _unitsMap.get( xmlUnitString );
	
			descriptor.setLid( getElementStringValue( e, "basinId" ) );
			descriptor.setPe( RUNOFF_TIMESERIES_PE_CODE );
			descriptor.setDur( getElementShortValue( e, "duration" ) );
			descriptor.setTs( TS );
			descriptor.setExtremum( EXTREMUM );
			descriptor.setProbability( PROBABILITY );
			descriptor.setBasistime( getElementLongTimeValue( e, "basisTime" ) );
			descriptor.setShef_qual_code( SHEF_QUAL_CODE );
			descriptor.setQuality_code( GOOD_QC );
			descriptor.setRevision( REVISION );
			descriptor.setProduct_id( PRODUCT_ID );
			descriptor.setProducttime( getElementLongTimeValue( e, "basisTime" ) );
			descriptor.setPostingtime( System.currentTimeMillis() );

			elementList = measurementListElement.getChildren( "Measurement" );
			
			for ( int i = 0; i < elementList.size(); i++ )
			{
				measurementElement = (Element) elementList.get( i );
				double xmlValue = getElementDoubleValue( measurementElement, "value" );
				measurement = new Measurement( xmlValue, xmlMeasuringUnit );
				long time = getElementLongTimeValue( measurementElement, "time" );
				
				if ( i == 0 )
				{
					runoffTimeSeries = new RegularTimeSeries( time, time, INTERVAL_IN_HOURS, 
															  DB_MEASURING_UNIT );
				}
				runoffTimeSeries.setMeasurementByTime( measurement, time );
			}
			_dataMgr.saveFcstTimeSeries( descriptor, runoffTimeSeries );
		}
	}

//	-----------------------------------------------------------------------------     
	
	private String getElementStringValue( Element e, String subElementName )
	{
		return e.getChildTextTrim( subElementName );
	}
	
//	-----------------------------------------------------------------------------     

	private double getElementDoubleValue( Element e, String subElementName )
	{
		String attributeValueString = e.getChildTextTrim( subElementName );
		return Double.parseDouble( attributeValueString.trim() );
	}
	
//	-----------------------------------------------------------------------------     

	private int getElementIntValue( Element e, String subElementName )
	{
		String attributeValueString = e.getChildTextTrim( subElementName );
		return Integer.parseInt( attributeValueString.trim() );
	}
	
//	-----------------------------------------------------------------------------     
	private short getElementShortValue( Element e, String subElementName )
	{
		String attributeValueString = e.getChildTextTrim( subElementName );
		return Short.parseShort( attributeValueString.trim() );
	}
	
//	-----------------------------------------------------------------------------     

	private long getElementLongTimeValue( Element e, String subElementName )
	{
		String attributeValueString = e.getChildTextTrim( subElementName );
		return DbTimeHelper.getLongTimeFromDateTimeString( attributeValueString );
	}
	
//	-----------------------------------------------------------------------------     

	public String toString()
	{
		StringWriter stringWriter = new StringWriter();
		Format format = Format.getPrettyFormat();
		format = format.setIndent("	");
		XMLOutputter outputter = new XMLOutputter(format);
		String returnValue = null;
		
		try
		{
			outputter.output( _doc, stringWriter );
			returnValue = stringWriter.toString();
		}
		catch ( IOException e )
		{
			e.printStackTrace();
		}
		
		return returnValue;
	}

//	-----------------------------------------------------------------------------     
	
	private void connect()
	{
		_dataMgr = new DecoderDataMgr( _connectionString, _logger );
	}
	
	private boolean validateInputParameters( String[] args )
	{
		String inputDir 	   = null;
		String archiveDir      = null;
		String logDir		   = null;
		String sleepTimeString = null;
		long sleepTimeLong 	   = 0;
		File directory   	   = null;
		boolean returnValue    = true;

		if ( args.length < 3 )
		{
			System.out.println( "SacXMLDecoder <DBConnection String> <input directory> <archive directory> <log directory> [ <sleep time (ms)> ] " );
			returnValue = false;
		}
		else
		{
			_connectionString = args[ 0 ];

			inputDir = args[ 1 ];
		
			directory = new File( inputDir );
			if ( directory.exists() )
			{
				_inputDir = inputDir;
			}
			else
			{
				System.err.println( "You must specify a valid input directory" );
				returnValue = false;
			}
			
			archiveDir = args[ 2 ];
			
			directory = new File( archiveDir );
			if ( directory.exists() )
			{
				_archiveDir = archiveDir;
			}
			else
			{
				System.err.println( "You must specify a valid archive directory" );
				returnValue = false;
			}
			
			logDir = args[ 3 ];
			directory = new File( logDir );
			if ( directory.exists() )
			{
				_logFullPath = logDir + "/SacXMLDecoder.log";
			}
			else
			{
				System.err.println( "You must specify a valid log directory" );
				returnValue = false;
			}

			
			if ( args.length > 4 )
			{
				sleepTimeString = args[ 4 ];
				try
				{
					sleepTimeLong = Long.parseLong( sleepTimeString );
					_sleepTime = sleepTimeLong;
					_isTest = true;
				}
				catch ( NumberFormatException e )
				{
					System.err.println( "Invalid sleep time specified.  Defaulting to 60 seconds" );
					_sleepTime = 60000;
				}
			}
			else
			{
				_isTest = false;
			}
		} // end else
			
		return returnValue;
	}


	
	public static void main( String[] args )
	{
		SacXMLDecoder sacDecoder 		 = new SacXMLDecoder( args[ 0 ] );
		boolean validInputParameters = false;
		
		validInputParameters = sacDecoder.validateInputParameters( args );
		
		if ( validInputParameters )
		{
			sacDecoder.execute();
		}

		sacDecoder._dataMgr.disconnect();
		  
//		System.exit( 0 );
	}
}