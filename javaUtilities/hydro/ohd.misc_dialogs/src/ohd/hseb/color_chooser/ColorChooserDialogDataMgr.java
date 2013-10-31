package ohd.hseb.color_chooser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;

import ohd.hseb.db.Database;
import ohd.hseb.ihfsdb.generated.ColorNameRecord;
import ohd.hseb.ihfsdb.generated.ColorNameTable;
import ohd.hseb.ihfsdb.generated.ColorValueRecord;
import ohd.hseb.ihfsdb.generated.ColorValueTable;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.Logger;
import ohd.hseb.util.StringParser;

public class ColorChooserDialogDataMgr 
{
	private Database _db = null;
	private Logger _logger = null;
    private String _applicationName = null;
	private List _defaultColorScaleValueSetList = new ArrayList();
    private List _colorValueTableColorScaleValueSetList = new ArrayList();
	private HashMap _rgbColorMap = new HashMap();
    private HashMap _hardCodedColorUseToDatabaseColorUseMap = new HashMap();
    private String _userID = null;
    private String _defaultColorFilePath = null;
    private String _rgbColorFilePath = null;
	
	
    public ColorChooserDialogDataMgr( String baseConnectionString, Logger logger, String applicationName, 
                                      String defaultColorFilePath, String rgbColorFilePath ) 
    {		
    	super();

    	_db = new Database();
    	_logger = logger;
        _applicationName = applicationName;
        _defaultColorFilePath = defaultColorFilePath;
        _rgbColorFilePath = rgbColorFilePath;

    	_db.connectWithDriverSearch( baseConnectionString );

        readRGBTextFile();
    	readHydroViewDefaultColorFile();
        readColorValuesFromDatabase();
//        testRgbTextVersusColorNameTable();
    }
    public String getHardCodedColorUse( String colorUse )
    {
        return (String) _hardCodedColorUseToDatabaseColorUseMap.get( colorUse );
    }

    public void disconnect()
    {
        _db.disconnect();   
    }
    
        //  ------------------------------------------------------------------------
    public void finalize()
    {
        disconnect();   
    }
    
    public void readColorValuesFromDatabase()
    {
        ColorValueTable table = new ColorValueTable( _db );
        ColorValue colorValue = new ColorValue();
        int i = 0;
/*        
        String whereClause = "WHERE application_name='" + _applicationName + "' AND userid != 'default' " + 
                             "AND userid != 'factory_default'  ORDER BY color_use_name, userid, duration, " + 
                             "threshold_value";
*/
        String whereClause = "WHERE application_name='" + _applicationName + "' AND userid != 'factory_default' " +
                             "ORDER BY color_use_name, userid, duration, threshold_value";

        List colorValueRecordsList = null;
        ColorScaleValueSet colorScaleValueSet = new ColorScaleValueSet();
        ColorSet colorSet = null;
//        Color colorObject = null;
        ScaleValueSet scaleValueSet = null;
//        double scaleValue = 0;
        String currentIndexString = null, prevIndexString = null;
        CodeTimer timer = new CodeTimer();
        timer.start();
        CodeTimer timer2 = new CodeTimer();
        String colorUseString = null;
        
        try
        {
            colorValueRecordsList = table.select( whereClause );
        }
        catch ( SQLException e )
        {
            e.printStackTrace();
        }
        
        timer.stop( "Finished reading the ColorValue table in " );
        timer2.start();
        
        for ( i = 0; i < colorValueRecordsList.size(); i++ )
        {
            colorValue = getColorValueFromColorValueRecord( (ColorValueRecord) colorValueRecordsList.get( i ) );
            if ( i == 0 )
            {
                prevIndexString = getColorValueKeyString( colorValue );
                scaleValueSet = new ScaleValueSet();
                colorSet = new ColorSet();
                scaleValueSet.addScaleValue( colorValue.getThresholdValue() );
                colorSet.addNamedColor( getNamedColorByName( colorValue.getColorName() ) );
                
                colorUseString = (String) _hardCodedColorUseToDatabaseColorUseMap.get( colorValue.getColorUseName() );
                if ( colorUseString != null )
                {
                    colorScaleValueSet.setColorUseString( colorUseString );
                }
                else
                {
                    colorScaleValueSet.setColorUseString( colorValue.getColorUseName() );
                }

                colorScaleValueSet.setDuration( colorValue.getDuration() );
                colorScaleValueSet.setUserID( colorValue.getUserID() );
            }
            else if ( i == ( colorValueRecordsList.size() - 1 ) )  // Last entry in the records list
            {
                scaleValueSet.addScaleValue( colorValue.getThresholdValue() );
                colorSet.addNamedColor( getNamedColorByName( colorValue.getColorName() ) );
                colorScaleValueSet.setColorSet( colorSet );
                colorScaleValueSet.setScaleValueSet( scaleValueSet );
                _colorValueTableColorScaleValueSetList.add( colorScaleValueSet );
            }
            else
            {
                currentIndexString = getColorValueKeyString( colorValue );
                if ( currentIndexString.equalsIgnoreCase( prevIndexString ) )  // same index
                {
                    scaleValueSet.addScaleValue( colorValue.getThresholdValue() );
                    colorSet.addNamedColor( getNamedColorByName( colorValue.getColorName() ) );
                }
                else   // new index
                {
                    colorScaleValueSet.setColorSet( colorSet );
                    colorScaleValueSet.setScaleValueSet( scaleValueSet );
                    _colorValueTableColorScaleValueSetList.add( colorScaleValueSet );
                    prevIndexString = currentIndexString;
                    scaleValueSet = new ScaleValueSet();
                    colorSet = new ColorSet();
                    colorScaleValueSet = new ColorScaleValueSet();
                    scaleValueSet.addScaleValue( colorValue.getThresholdValue() );
                    colorSet.addNamedColor( getNamedColorByName( colorValue.getColorName() ) );
                    colorUseString = (String) _hardCodedColorUseToDatabaseColorUseMap.get( colorValue.getColorUseName() );
                    if ( colorUseString != null )
                    {
                        colorScaleValueSet.setColorUseString( colorUseString );
                    }
                    else
                    {
                        colorScaleValueSet.setColorUseString( colorValue.getColorUseName() );
                    }
                    colorScaleValueSet.setDuration( colorValue.getDuration() );
                    colorScaleValueSet.setUserID( colorValue.getUserID() );
                }
            }
        }
        timer2.stop( "Finished processing the ColorValue table in " );
    }
    
    
    private NamedColor getNamedColorByName( String colorName )
    {
        NamedColor namedColor = (NamedColor) getRGBColorMap().get( colorName );
        
        return namedColor;
    }
    
    private String getColorValueKeyString( ColorValue colorValue )
    {
        return colorValue.getUserID() + "|" + colorValue.getColorUseName() + "|" + colorValue.getDuration();
    }
    
    private ColorValue getColorValueFromColorValueRecord( ColorValueRecord record )
    {
        ColorValue colorValue = new ColorValue();
        
        colorValue.setApplicationName( record.getApplication_name() );
        colorValue.setColorName( record.getColor_name() );
        colorValue.setColorUseName( record.getColor_use_name() );
        colorValue.setDuration( (double) record.getDuration() / (double) 3600 );
        colorValue.setThresholdUnit( record.getThreshold_unit() );
        colorValue.setThresholdValue( record.getThreshold_value() );
        colorValue.setUserID( record.getUserid() );
        
        return colorValue;
    }
    
    private void saveColorScaleValueSet( List colorValueRecordList, String userID, String colorUseString, double duration )
    {
        ColorValueTable table = new ColorValueTable( _db );

        int durationSecondsInt = (int) ( duration*3600 );
        
        String whereClause = "WHERE userID='" + userID + "' AND application_name='" + _applicationName + "' AND " +
        "color_use_name='" + colorUseString + "' AND duration=" + durationSecondsInt; 
        
        System.out.println( whereClause );
        
        try 
        {
            table.delete( whereClause );
            for ( int i = 0; i < colorValueRecordList.size(); i++ )
            {
                ColorValueRecord record = (ColorValueRecord) colorValueRecordList.get( i );
                table.insert( record );
            }
        } 
        catch ( SQLException e ) 
        {
            e.printStackTrace();
        }
        
        resetHvDefaultColorList();
        resetColorValueTableColorScaleValueSetList();
    }
    
    public void saveAsOffice( ColorScaleValueSet colorScaleValueSet )
    {
        List colorValueRecordList = getColorValueRecordListFromColorScaleValueSet( colorScaleValueSet, "default" );
        
        String colorUseString = (String) _hardCodedColorUseToDatabaseColorUseMap.get( colorScaleValueSet.getColorUseString() );
        if ( colorUseString == null )  //Checking to see if the color Use is in the hash map.  If not, use the original name.
        {
            colorUseString = colorScaleValueSet.getColorUseString();
        }

        saveColorScaleValueSet( colorValueRecordList, "default", colorUseString, colorScaleValueSet.getDuration() );
    }

    public void saveAsUser( ColorScaleValueSet colorScaleValueSet, double duration )
    {
        List colorValueRecordList = getColorValueRecordListFromColorScaleValueSet(
                colorScaleValueSet, _userID);
        
        String colorUseString = (String) _hardCodedColorUseToDatabaseColorUseMap.get( colorScaleValueSet.getColorUseString() );
        if ( colorUseString == null )  //Checking to see if the color Use is in the hash map.  If not, use the original name.
        {
            colorUseString = colorScaleValueSet.getColorUseString();
        }
         
        saveColorScaleValueSet( colorValueRecordList, _userID, colorUseString, duration );
    }
    
    public void deleteAsUser( ColorScaleValueSet colorScaleValueSet )
    {
        List colorValueRecordList = getColorValueRecordListFromColorScaleValueSet( colorScaleValueSet, _userID );

        String colorUseString = (String) _hardCodedColorUseToDatabaseColorUseMap.get( colorScaleValueSet.getColorUseString() );
        
        if ( colorUseString == null )
        {
            colorUseString = colorScaleValueSet.getColorUseString();
        }
        
        deleteColorScaleValueSet( colorValueRecordList, _userID, colorUseString, colorScaleValueSet.getDuration() );
    }

    public void deleteAsOffice( ColorScaleValueSet colorScaleValueSet )
    {
        List colorValueRecordList = getColorValueRecordListFromColorScaleValueSet( colorScaleValueSet, "default" );

        String colorUseString = (String) _hardCodedColorUseToDatabaseColorUseMap.get( colorScaleValueSet.getColorUseString() );
        
        if ( colorUseString == null )
        {
            colorUseString = colorScaleValueSet.getColorUseString();
        }
        
        deleteColorScaleValueSet( colorValueRecordList, "default", colorUseString, colorScaleValueSet.getDuration() );
    }

    private void deleteColorScaleValueSet( List colorValueRecordList, String userID, String colorUseString, double duration )
    {
        ColorValueTable table = new ColorValueTable( _db );

        String whereClause = "WHERE userID='" + userID + "' AND application_name='" + _applicationName + "' AND " +
        "color_use_name='" + colorUseString + "' AND duration=" + duration*3600; 
        
        try 
        {
            table.delete( whereClause );
        } 
        catch ( SQLException e ) 
        {
            e.printStackTrace();
        }
        
        resetHvDefaultColorList();
        resetColorValueTableColorScaleValueSetList();
    }
    
    private List getColorValueRecordListFromColorScaleValueSet( ColorScaleValueSet colorScaleValueSet, String userID )
    {
        List colorValueRecordList = new ArrayList();
        
        ColorValueRecord record = new ColorValueRecord();
        ColorSet colorSet = colorScaleValueSet.getColorSet();
        List colorSetList = colorSet.getNamedColorList();
        List scaleValueSetList = colorScaleValueSet.getScaleValueSet().getScaleValueSet();
        String colorUseString = null;
        
        record.setApplication_name( _applicationName );
        colorUseString = (String) _hardCodedColorUseToDatabaseColorUseMap.get( colorScaleValueSet.getColorUseString() );
        if ( colorUseString == null )  //Checking to see if the color Use is in the hash map.  If not, use the original name.
        {
            colorUseString = colorScaleValueSet.getColorUseString();
        }

        record.setColor_use_name( colorUseString );
        record.setDuration( (int) ( colorScaleValueSet.getDuration() * 3600 ) );
        record.setUserid( userID );
        record.setThreshold_unit( "E" );
        
        for ( int i = 0; i < scaleValueSetList.size(); i++ )
        {
            double scaleValue = (Double) scaleValueSetList.get( i );
            NamedColor namedColor = (NamedColor) colorSetList.get( i );
            
            record.setThreshold_value( scaleValue );
            record.setColor_name( namedColor.getName() );
            
            colorValueRecordList.add( record );
            record = new ColorValueRecord( record );
        }
        return colorValueRecordList;
    }
    
    
    public void setUserID( String userID )
    {
        _userID = userID;
    }
    
    private Set getColorNameTableSet()
    {
        Set colorNameSet = new HashSet();
        ColorNameTable table = new ColorNameTable( _db );
        
        try
        {
            List <ColorNameRecord> recordList = table.select( "" );
            for ( ColorNameRecord record : recordList ) 
            {
                colorNameSet.add( record.getColor_name() );
            }
        }
        catch ( SQLException e )
        {
            e.printStackTrace();
        }
        
        
        return colorNameSet;
    }
    
    public void readRGBTextFile()
    {
    	File rgbColorFile = null;
    	BufferedReader reader = null;
    	String line = null;
    	String[] tokenizedString = null;
        String colorName = null;
        Set nameFromColorNameTableSet = getColorNameTableSet();
    	rgbColorFile = new File( _rgbColorFilePath );

    	try
    	{
    		reader = new BufferedReader( new FileReader( rgbColorFile ) );
    		line = reader.readLine(); //discard the first line
    		line = reader.readLine();
    		while ( line != null )
    		{
    			tokenizedString = StringParser.tokenize( line );
                colorName = getRGBColorName( tokenizedString );

//              if the read in RGB color name exists in the ColorName table, then add to the _rgbColorMap
                if ( nameFromColorNameTableSet.contains( colorName.toUpperCase() ) ) 
                {
                    NamedColor namedColor = new NamedColor( colorName, Integer.parseInt( tokenizedString[ 0 ] ), 
                            Integer.parseInt( tokenizedString[ 1 ] ), 
                            Integer.parseInt( tokenizedString[ 2 ] ) );


                    _rgbColorMap.put( colorName.toUpperCase(), namedColor );
                }
    			line = reader.readLine();
    		}
    	}
        catch( IOException e )
    	{
        	e.printStackTrace();
    	}
    }
 
    
    private String getRGBColorName( String[] tokenizedName )
    {
        StringBuffer rgbColorName = new StringBuffer();
        
        for ( int i = 3; i < tokenizedName.length;i++ )
        {
            rgbColorName.append( tokenizedName[ i ] );

            if ( i != ( tokenizedName.length - 1 ) )
            {
                rgbColorName.append( " " );
            }
        }
        
        return rgbColorName.toString();
    }
    
    public void readHydroViewDefaultColorFile()
    {
//  	String hydroviewDefaultColorFileName = null;
    	AppsDefaults appsDefaults = new AppsDefaults();
    	File hydroviewDefaultColorFile = null;
    	BufferedReader reader = null;
    	String line = null;
    	String colorSetString = null;
        String colorUseString = null;
    	String colorString = null;
        String databaseColorUseString = null;
    	float scaleValue = 0;
    	NamedColor namedColor = null;
    	String[] tokenizedString = null;
        
        ColorScaleValueSet colorScaleValueSet = null;
        ColorSet colorSet = new ColorSet();
        ScaleValueSet scaleValueSet = new ScaleValueSet();
        
    	hydroviewDefaultColorFile = new File( _defaultColorFilePath );
    	
    	try
    	{
    		reader = new BufferedReader( new FileReader( hydroviewDefaultColorFile ) );
    		line = reader.readLine();
    		while ( line != null )
    		{
    			if ( line.trim().length() > 0 ) // not an empty line
    			{
    				tokenizedString = StringParser.tokenize( line );
    				if ( line.indexOf( "|" ) == -1 )
    				{
        				colorString = tokenizedString [ 0 ];
        				namedColor =  getNamedColorByName( colorString );
        				scaleValue = Float.valueOf( tokenizedString [ 1 ] );
                        colorSet.addNamedColor( namedColor );
                        scaleValueSet.addScaleValue( scaleValue );
                        
    				}
    				else //line read in is the color set name (first line of each set)
    				{
                        colorScaleValueSet = new ColorScaleValueSet();
                        
                        StringTokenizer tokenizer = new StringTokenizer( line, "|" );

                        colorUseString = tokenizer.nextToken();
                        databaseColorUseString = tokenizer.nextToken();
                        
                        insertColorUseStringIntoHashMap( colorUseString, databaseColorUseString );
                        
                        colorScaleValueSet.setColorUseString( colorUseString );
    				}
    			}
    			else //empty line
    			{
                    colorScaleValueSet.setColorSet( colorSet );
                    colorScaleValueSet.setScaleValueSet( scaleValueSet );
                    colorScaleValueSet.setUserID( "HARDCODED" );
                    colorScaleValueSet.setDuration( 0 );
                    _defaultColorScaleValueSetList.add( colorScaleValueSet );
                    colorSet = new ColorSet();
                    scaleValueSet = new ScaleValueSet();
                }
    			line = reader.readLine();
    		}
    	}
        catch( IOException e )
    	{
        	e.printStackTrace();
    	}
    }

    private void testRgbTextVersusColorNameTable()
    {
        ColorNameTable table = new ColorNameTable( _db );
        List extraCrapList = new ArrayList();
        Set rgbHashSet = new TreeSet( _rgbColorMap.keySet() );
        List rgbList = new ArrayList( _rgbColorMap.keySet() );
        String colorName = null;
        
        try 
        {
            List recordList = table.select( "" );
            Set recordSet = new TreeSet();
            
            for ( int i = 0; i < recordList.size(); i++ )
            {
                ColorNameRecord record = (ColorNameRecord) recordList.get( i );
                colorName = record.getColor_name();
                recordSet.add( getStringWithoutSpaces( colorName ) );
                if ( ! rgbHashSet.contains( colorName ) )
                {
                    extraCrapList.add( colorName );
                    System.out.println( colorName + " is not in the RGB text file but is in the database" );
                }
            }


            for ( int i = 0; i < rgbList.size(); i++ )
            {
                String rgbColorName = getStringWithoutSpaces( (String) rgbList.get( i ) );
                if ( ! recordSet.contains( rgbColorName ) )
                {
                    extraCrapList.add( rgbColorName );
                    System.out.println( rgbColorName + " is not in the database but is in the RGB text file" );
                }
            }

            
            System.out.println( "DONE" );
            
        } 
        catch (SQLException e) 
        {
            e.printStackTrace();
        }
    }
    
    private String getStringWithoutSpaces( String colorName )
    {
        StringBuffer buffer = new StringBuffer();
        
        for (int i = 0; i < colorName.length() ; i++)
        {
            char c = colorName.charAt(i);
            if (c != ' ')
            {
                buffer.append(c);
            }
        }
        
        return buffer.toString();
    }
    private void insertColorUseStringIntoHashMap( String colorUseString, String databaseColorUseString )
    {
        _hardCodedColorUseToDatabaseColorUseMap.put( colorUseString, databaseColorUseString );
        _hardCodedColorUseToDatabaseColorUseMap.put( databaseColorUseString, colorUseString );
    }
    
	public List getDefaultColorScaleValueSetList() 
	{
		return _defaultColorScaleValueSetList;
	}

    public void setApplicationName(String _applicationName) {
        this._applicationName = _applicationName;
    }

    public String getApplicationName() {
        return _applicationName;
    }

    public List getColorValueTableColorScaleValueSetList() 
    {
        return _colorValueTableColorScaleValueSetList;
    }
    
    public void addColorScaleValueSetToColorValueTableList( ColorScaleValueSet colorScaleValueSet )
    {
        _colorValueTableColorScaleValueSetList.add( colorScaleValueSet );
    }
    
    public void resetColorValueTableColorScaleValueSetList()
    {
        _colorValueTableColorScaleValueSetList = new ArrayList();
        readColorValuesFromDatabase();
    }

    public void resetHvDefaultColorList()
    {
        _defaultColorScaleValueSetList = new ArrayList();
        readHydroViewDefaultColorFile();
    }
    
    public HashMap getRGBColorMap() 
    {
        return _rgbColorMap;
    }
}
