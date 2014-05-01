package ohd.hseb.util;

import java.text.DecimalFormat;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;

public class StringDataConverter
{
    private static final String latlonFormatString = "0.00";
    private static final DecimalFormat _latlonFormatter = new DecimalFormat(latlonFormatString);

    public StringDataConverter()
    {
    }
    
    public String getFormattedDoubleValue( double doubleValue )
    {
        String returnString = "";
        if ( doubleValue != DbTable.getNullDouble() )
        {
            returnString = _latlonFormatter.format( doubleValue );
        }

        return ( returnString );
    }
    
    public double getDoubleValue( String valueString )
    {
        double value = DbTable.getNullDouble();
        
        if ( ! valueString.equalsIgnoreCase( "" ) )
        {
            value = Double.parseDouble( valueString );
        }
        
        return value;
    }
    
    public long getLongDateValue( String dateString )
    {
        long value = DbTable.getNullLong();
        
        if ( ! dateString.equalsIgnoreCase( "" ) )
        {
            value = DbTimeHelper.getLongTimeFromDateString( dateString );
        }
        
        return value;
    }
    
    public long getLongDateTimeValue( String dateTimeString )
    {
        long value = DbTable.getNullLong();
        
        if ( ! dateTimeString.equalsIgnoreCase( "" ) )
        {
            value = DbTimeHelper.getLongTimeFromDateTimeString( dateTimeString );
        }
        
        return value;
    }
    
    public long getLongValue( String valueString )
    {
        long value = DbTable.getNullLong();
        
        if ( ! valueString.equalsIgnoreCase( "" ) )
        {
            value = Long.parseLong( valueString );
        }
        return value;
    }
    
    public String getFormattedFloatValue( float floatValue )
    {
        String returnString = "";
        if ( floatValue != DbTable.getNullFloat() )
        {
            returnString = _latlonFormatter.format( floatValue );
        }

        return ( returnString );
    }
    
    public float getFloatValue( String valueString )
    {
        float value = DbTable.getNullFloat();
        
        if ( ! valueString.equalsIgnoreCase( "" ) )
        {
            value = Float.parseFloat( valueString );
        }
        return value;
    }
    
    public int getIntValue( String valueString )
    {
        int value = DbTable.getNullInt();
        
        if ( ! valueString.equalsIgnoreCase( "" ) )
        {
            value = Integer.parseInt( valueString );
        }
        
        return value;
    }

    public short getShortValue( String valueString )
    {
        short value = DbTable.getNullShort();
        
        if ( ! valueString.equalsIgnoreCase( "" ) )
        {
            value = Short.parseShort( valueString );
        }
        
        return value;
    }

    public String getStringFromDouble( double value )
    {
        String valueString = "";
        
        if ( value != DbTable.getNullDouble() )
        {
            valueString = Double.toString( value );
        }
        
        return valueString;
    }
    
    public String getStringFromInt( int value )
    {
        String valueString = "";
        
        if ( value != DbTable.getNullInt() )
        {
            valueString = Integer.toString( value );
        }
        
        return valueString;
    }
    
    public String getStringFromLong( long value )
    {
        String valueString = "";
        
        if ( value != DbTable.getNullLong() )
        {
            valueString = Long.toString( value );
        }
        
        return valueString;
    }

    public String getStringFromShort( short value )
    {
        String valueString = "";
        
        if ( value != DbTable.getNullShort() )
        {
            valueString = Short.toString( value );
        }
        
        return valueString;
    }
    
    public String getDateStringFromDateLong( long dateLong )
    {
        String dateString = "";
        
        if ( dateLong != DbTable.getNullLong() )
        {
            dateString = DbTimeHelper.getDateStringFromLongTime( dateLong );
        }
        
        return dateString;
    }
    
    public String getString( String string )
    {
        String returnString = null;
        
        if ( ! string.equalsIgnoreCase( "" ) )
        {
            returnString = string;
        }
        
        return returnString;
    }
    
    public String getDateTimeStringFromDateTimeLong( long dateTimeLong )
    {
        String dateString = "";
        
        if ( dateTimeLong != DbTable.getNullLong() )
        {
            dateString = DbTimeHelper.getDateTimeStringFromLongTime( dateTimeLong );
        }
        
        return dateString;
    }
}
