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
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.model.MonthlyValues;
import ohd.hseb.model.sacsma.SacSmaParameters;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.TimeHelper;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format.TextMode;
import org.jdom.output.XMLOutputter;
import org.jdom.output.Format;

public class SacXMLTester
{
	private String _inputDir 		  = null;
	private String _archiveDir 		  = null;
	private String _logFullPath		  = null;
	private long _sleepTime 		  = 0;
	private Document _inputXMLDoc 	  = null;
    private Document _outputXMLDoc    = null;
	private String backupInputFilePath = null;
    private String _inputXMLFileName = null;
	private boolean _isTest 		  = true;
    private String _basinID           = null;
	
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

	private DecoderDataMgr _dataMgr = null;

    
    
    
    
	public SacXMLTester( String inputXMLFileName, Document xmlOutputDoc, String logFileName )
	{
        _inputXMLFileName = inputXMLFileName;
        _outputXMLDoc = xmlOutputDoc;
        _logFullPath = logFileName;
	}
	
	private void execute()
	{
		File backupInputFile 	= null;
		SAXBuilder builder 		= new SAXBuilder();
		File inputFile 			= null;
		Date time				= new Date();
		boolean keepFileOpen 	= true;
		boolean appendDateTime 	= true;
		CodeTimer timer1 		= null;

		_logger = new FileLogger( _logFullPath, keepFileOpen, appendDateTime );
		timer1 = new CodeTimer( _logger );

		initUnitsMap();

		try
		{
		    builder.setExpandEntities( true );
		    _logger.log( "Processing " + _inputXMLFileName );
		    timer1.start();
		    
		    _inputXMLDoc = builder.build( _inputXMLFileName );
		    parseXMLFiles();
		    backupInputFile = new File( _archiveDir + "/" + _inputXMLFileName );
		    if ( backupInputFile.isFile() )
		    {
		        backupInputFile = new File( backupInputFilePath + _inputXMLFileName + "_" + time.getTime() );
		    }
		    inputFile = new File( _inputXMLFileName );
		    inputFile.renameTo( backupInputFile );
		    timer1.stop( "Finished processing " + _inputXMLFileName + " in " );
		}
		catch ( JDOMException e )
		{
		    _logger.log( "     " + _inputXMLFileName + " is not a valid XML file." );
		}
		
		catch ( IOException e )
		{
		    e.printStackTrace();
		}	
	}
	
	private void initUnitsMap()
	{
		_unitsMap.put( new String( "mm" ), MeasuringUnit.mm );
		_unitsMap.put( new String( "cm" ), MeasuringUnit.cm );
		_unitsMap.put( new String( "m" ), MeasuringUnit.meters );
		_unitsMap.put( new String( "in" ), MeasuringUnit.inches );
		_unitsMap.put( new String( "ft" ), MeasuringUnit.feet );
	}
	
	private void parseXMLFiles()
	{
		Element inputSacParamsListElement		= _inputXMLDoc.getRootElement().getChild( "SacParamsList" );
		Element inputSacStateListElement        = _inputXMLDoc.getRootElement().getChild( "SacStateList" );
		Element monthlyValuesListElement 		= _inputXMLDoc.getRootElement().getChild( "MonthlyValuesList" );

        Element outputRootElement               = _outputXMLDoc.getRootElement();
        Element outputSacParamsListElement      = outputRootElement.getChild( "SacParamsList" );
        Element outputSacStateListElement       = outputRootElement.getChild( "SacStateList" );
        Element outputMonthlyValuesListElement  = outputRootElement.getChild( "MonthlyValuesList" );
        Element outputPETimeSeriesListElement   = outputRootElement.getChild( "PeTimeSeriesList" );
        Element outputRunoffTimeSeriesElement   = outputRootElement.getChild( "RunoffTimeSeriesList" );
        
		Element sacParamsElement     	        = null;
		Element sacStateElement         	    = null;
		Element peTimeSeriesElement				= null;
		Element runoffTimeSeriesElement        	= null;
        Element monthlyValuesElement            = null;
		
		SacSmaParameters sacSmaParamsToBeCopied	= new SacSmaParameters();
        SacSmaState sacSmaStateToBeCopied       = new SacSmaState();
        RegularTimeSeries peTimeSeries          = null;
        RegularTimeSeries roTimeSeries          = null;
		
        Random generator                        = new Random();
        
		List elementList 					  	= null;
		
        

//		--------- Processing Sac Parameters

		elementList = inputSacParamsListElement.getChildren( "SacParams" );
        
        Element outputSacParamsElement = null;
        for ( int i = 0; i < elementList.size(); i++ )
		{
			sacParamsElement = (Element) elementList.get( i );
            
            sacSmaParamsToBeCopied = retrieveSacSmaParams( sacParamsElement );
            outputSacParamsElement = convertSacSmaParamsToSacSmaParamsElement( sacSmaParamsToBeCopied );
            outputSacParamsListElement.addContent( outputSacParamsElement );
		}
		
        _basinID = sacSmaParamsToBeCopied.getBasinId();
//		--------- Processing Sac States
		elementList = inputSacStateListElement.getChildren( "SacState" );
        Element outputSacStateElement = null;
        
		for ( int i = 0; i < elementList.size(); i++ )
		{
			sacStateElement = (Element) elementList.get( i );
            
            sacSmaStateToBeCopied = retrieveSacSmaState( sacStateElement );
            outputSacStateElement = convertSacSmaStateToSacSmaStateElement( sacSmaStateToBeCopied );
            
            outputSacStateListElement.addContent( outputSacStateElement );
		}

//      --------- Processing MonthlyValues
        
        elementList = monthlyValuesListElement.getChildren( "MonthlyValues" );
        Element outputMonthlyValuesElement = null;
        
        for ( int i = 0; i < elementList.size(); i++ )
        {
            monthlyValuesElement = (Element) elementList.get( i );
            
            outputMonthlyValuesElement = (Element) monthlyValuesElement.clone();
            
            outputMonthlyValuesListElement.addContent( outputMonthlyValuesElement );
        }

// --------- Processing Pe Timeseries

        long endTime = TimeHelper.roundTimeInMillisToNearestHour( System.currentTimeMillis() );
        long startTime = endTime - ( 100 * TimeHelper.MILLIS_PER_HOUR ); 
        long validTime = TimeHelper.roundTimeInMillisToNearestHour( System.currentTimeMillis() );
        Element measurementListElement = new Element( "MeasurementList" );

        String validTimeString = DbTimeHelper.getDateTimeStringFromLongTime( validTime );

        peTimeSeries = new RegularTimeSeries(startTime, endTime, INTERVAL_IN_HOURS, DB_MEASURING_UNIT );
        
        peTimeSeriesElement = new Element( "PeTimeSeries" );
        peTimeSeriesElement.addContent( getElement( "isUsed", "TRUE" ) );
        peTimeSeriesElement.addContent( getElement( "basinId", _basinID ) );
        peTimeSeriesElement.addContent( getElement( "basisTime", validTimeString ) );
        peTimeSeriesElement.addContent( getElement( "duration", "1024" ) );
        peTimeSeriesElement.addContent( getElement( "units", "mm" ) );
        peTimeSeriesElement.addContent( measurementListElement );
        
        for ( long timeIndex = startTime; timeIndex <= endTime; timeIndex += TimeHelper.MILLIS_PER_HOUR ) 
        {
            double randomNumber = Math.random() * 3;
            double value = ( Math.round ( randomNumber * 1000.0 ) ) / 1000.0;
            
            Element measurementElement = new Element( "Measurement" );

            measurementElement.addContent( getElement ( "time", DbTimeHelper.getDateTimeStringFromLongTime( timeIndex ) ) );
            measurementElement.addContent( getElement( "value", Double.toString( value ) ) );
            measurementListElement.addContent( measurementElement );
        }
        
        outputPETimeSeriesListElement.addContent( peTimeSeriesElement );
        

// --------- Processing Runoff Timeseries
      	
        measurementListElement = new Element( "MeasurementList" );

        roTimeSeries = new RegularTimeSeries(startTime, endTime, INTERVAL_IN_HOURS, DB_MEASURING_UNIT );
        
        runoffTimeSeriesElement = new Element( "RunoffTimeSeries" );
        runoffTimeSeriesElement.addContent( getElement( "exists", "TRUE" ) );
        runoffTimeSeriesElement.addContent( getElement( "basinId", _basinID ) );
        runoffTimeSeriesElement.addContent( getElement( "basisTime", validTimeString ) );
        runoffTimeSeriesElement.addContent( getElement( "duration", "1024" ) );
        runoffTimeSeriesElement.addContent( getElement( "units", "mm" ) );
        runoffTimeSeriesElement.addContent( measurementListElement );
        
        for ( long timeIndex = startTime; timeIndex <= endTime; timeIndex += TimeHelper.MILLIS_PER_HOUR ) 
        {
            double randomNumber = Math.random() * 3;
            double value = ( Math.round ( randomNumber * 1000.0 ) ) / 1000.0;
            
            Element measurementElement = new Element( "Measurement" );

            measurementElement.addContent( getElement ( "time", DbTimeHelper.getDateTimeStringFromLongTime( timeIndex ) ) );
            measurementElement.addContent( getElement( "value", Double.toString( value ) ) );
            measurementListElement.addContent( measurementElement );
        }
        
        outputRunoffTimeSeriesElement.addContent( runoffTimeSeriesElement );

	}

//	-----------------------------------------------------------------------------     

    private Element convertSacSmaParamsToSacSmaParamsElement( SacSmaParameters params )
    {
        Element element = new Element ( "SacParams" );
        
        long validTime = TimeHelper.roundTimeInMillisToNearestHour( System.currentTimeMillis() );
        String validTimeString = DbTimeHelper.getDateTimeStringFromLongTime( validTime );
        
        
        element.addContent( getElement( "basinId", params.getBasinId() ) );
        element.addContent( getElement( "source", params.getSource() ) );
        element.addContent( getElement( "validTime", validTimeString ) );
        element.addContent( getElement( "pxadj", Double.toString( params.getPxadj() ) ) );
        element.addContent( getElement( "peadj", Double.toString( params.getPeadj() ) ) );
        element.addContent( getElement( "uztwm", Double.toString( params.getUztwm() ) ) );
        element.addContent( getElement( "uzfwm", Double.toString( params.getUzfwm() ) ) );
        element.addContent( getElement( "uzk", Double.toString( params.getUzk() ) ) );
        element.addContent( getElement( "pctim", Double.toString( params.getPctim() ) ) );
        element.addContent( getElement( "adimp", Double.toString( params.getAdimp() ) ) );
        element.addContent( getElement( "riva", Double.toString( params.getRiva() ) ) );
        element.addContent( getElement( "zperc", Double.toString( params.getZperc() ) ) );
        element.addContent( getElement( "rexp", Double.toString( params.getRexp() ) ) );
        element.addContent( getElement( "lztwm", Double.toString( params.getLztwm() ) ) );
        element.addContent( getElement( "lzfsm", Double.toString( params.getLzfsm() ) ) );
        element.addContent( getElement( "lzfpm", Double.toString( params.getLzfpm() ) ) );
        element.addContent( getElement( "lzsk", Double.toString( params.getLzsk() ) ) );
        element.addContent( getElement( "lzpk", Double.toString( params.getLzpk() ) ) );
        element.addContent( getElement( "pfree", Double.toString( params.getPfree() ) ) );
        element.addContent( getElement( "rserv", Double.toString( params.getRserv() ) ) );
        element.addContent( getElement( "side", Double.toString( params.getSide() ) ) );
//        element.addContent( getElement( "ioptet", Double.toString( params.get() ) ) );
        element.addContent( getElement( "efc", Double.toString( params.getEfc() ) ) );
        
        return element;
    }
    
    private Element convertSacSmaStateToSacSmaStateElement( SacSmaState state )
    {
        Element element = new Element ( "SacState" );
        
        long validTime = TimeHelper.roundTimeInMillisToNearestHour( System.currentTimeMillis() );
        String validTimeString = DbTimeHelper.getDateTimeStringFromLongTime( validTime );

        element.addContent( getElement( "basinId", state.getBasinId() ) );
        element.addContent( getElement( "source", state.getSource() ) );
        element.addContent( getElement( "validTime", validTimeString ) );
        element.addContent( getElement( "basisTime", validTimeString ) );
        element.addContent( getElement( "uztwc", Double.toString( state.getUztwc() ) ) );
        element.addContent( getElement( "uzfwc", Double.toString( state.getUzfwc() ) ) );
        element.addContent( getElement( "lztwc", Double.toString( state.getLztwc() ) ) );
        element.addContent( getElement( "lzfsc", Double.toString( state.getLzfsc() ) ) );
        element.addContent( getElement( "lzfpc", Double.toString( state.getLzfpc() ) ) );
        element.addContent( getElement( "adimc", Double.toString( state.getAdimc() ) ) );
        
        return element;
    }

    private Element getElement(String elementName, String elementText)
    {
        Element element = new Element(elementName);
        element.setText(elementText);
        
        return element;
    }
    
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
			outputter.output( _inputXMLDoc, stringWriter );
			returnValue = stringWriter.toString();
		}
		catch ( IOException e )
		{
			e.printStackTrace();
		}
		
		return returnValue;
	}

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
//	-----------------------------------------------------------------------------     
	
	public static void main( String[] args )
	{
        String inputFileName = args[ 0 ];
        String outputFileName = args[ 1 ];
        String logFileName = args [ 2 ];
        
        Document xmlOutputDoc = getBaseDocument(); 
        
        SacXMLTester sacDecoder = new SacXMLTester( inputFileName, xmlOutputDoc, logFileName );
        
		sacDecoder.execute();

        sacDecoder.writeXmlToFile( xmlOutputDoc, outputFileName );
//        System.exit( 0 );
	}
}