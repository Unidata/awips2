package ohd.hseb.raxbase.util;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;

public class StringPadder
{
    private int _frontPaddingLength;
    private int _endPaddingLength;
    
    public StringPadder( int frontPaddingLength, int endPaddingLength )
    {
        _frontPaddingLength = frontPaddingLength;
        _endPaddingLength = endPaddingLength;
    }
    
    
    public String getFrontPaddedString( String str )
    {
        String blankString = "";
        int strLength = 0;
        
        if ( str == null )
        {
            strLength = 4;
        }
        else
        {
            strLength = str.length();
        }
        
        for ( int i = 0; i < ( _frontPaddingLength - strLength); i++ )
        {
            blankString += " ";
        }
        
        return ( str + blankString );
    }
    
    public String getFrontPaddedString( double doubleValue )
    {
        if ( doubleValue == DbTable.getNullDouble() )
        {
            return ( getFrontPaddedString( null ) );
        }
        else
        {
            return ( getFrontPaddedString( doubleValue + "" ) );
        }
    }
    
    public String getFrontPaddedString( short shortValue )
    {
        if ( shortValue == DbTable.getNullShort() )
        {
            return ( getFrontPaddedString( null ) );
        }
        else
        {
            return ( getFrontPaddedString( shortValue + "" ) );
        }
    }

    public String getFrontPaddedString( int intValue )
    {
        if ( intValue == DbTable.getNullInt() )
        {
            return ( getFrontPaddedString( null ) );
        }
        else
        {
            return ( getFrontPaddedString( intValue + "" ) );
        }
    }

    public String getFrontPaddedString( long longValue )
    {
        if ( longValue == DbTable.getNullLong() )
        {
            return ( getFrontPaddedString( null ) );
        }
        else
        {
            return ( getFrontPaddedString( longValue + "" ) );
        }
    }

    public String getFrontPaddedDateString( long dateValue )
    {
        if ( dateValue == DbTable.getNullDate() )
        {
            return ( getFrontPaddedString( null ) );
        }
        else
        {
            String dateString = DbTimeHelper.getDateStringFromLongTime( dateValue );
            return ( getFrontPaddedString( dateString ) );
        }
    }
    
    public String getFrontPaddedDateTimeString( long dateTimeValue )
    {
        if ( dateTimeValue == DbTable.getNullDate() )
        {
            return ( getFrontPaddedString( null ) );
        }
        else
        {
            String dateString = DbTimeHelper.getDateTimeStringFromLongTime( dateTimeValue );
            return ( getFrontPaddedString( dateString ) );
        }
    }


    public String getEndPaddedString( String colName, boolean booleanValue )
    {
        String blankString = "";
        int strLength = 0;
        
        if ( booleanValue )
        {
            strLength = 4 + colName.length();
        }
        else
        {
            strLength = 5 + colName.length();
        }
        
        for ( int i = 0; i < ( _endPaddingLength - strLength); i++ )
        {
            blankString += " ";
        }
        
        return ( blankString + booleanValue );
    }

    public String getEndPaddedString( String colName, String str )
    {
        String blankString = "";
        int strLength = 0;
        
        if ( str == null )
        {
            strLength = 4 + colName.length();
        }
        else
        {
            strLength = str.length() + colName.length();
        }
        
        for ( int i = 0; i < ( _endPaddingLength - strLength); i++ )
        {
            blankString += " ";
        }
        
        return ( blankString + str );
    }
    
    public String getEndPaddedString( String colName, double doubleValue )
    {
        if ( doubleValue == DbTable.getNullDouble() )
        {
            return ( getEndPaddedString( colName, null ) );
        }
        else
        {
            return ( getEndPaddedString( colName, doubleValue + "" ) );
        }
    }
    
    public String getEndPaddedString( String colName, short shortValue )
    {
        if ( shortValue == DbTable.getNullShort() )
        {
            return ( getEndPaddedString( colName, null ) );
        }
        else
        {
            return ( getEndPaddedString( colName, shortValue + "" ) );
        }
    }
    
    public String getEndPaddedString( String colName, int intValue )
    {
        if ( intValue == DbTable.getNullInt() )
        {
            return ( getEndPaddedString( colName, null ) );
        }
        else
        {
            return ( getEndPaddedString( colName, intValue + "" ) );
        }
    }

    public String getEndPaddedString( String colName, long longValue )
    {
        if ( longValue == DbTable.getNullLong() )
        {
            return ( getEndPaddedString( colName, null ) );
        }
        else
        {
            return ( getEndPaddedString( colName, longValue + "" ) );
        }
    }

    public String getEndPaddedDateString( String colName, long dateValue )
    {
        if ( dateValue == DbTable.getNullDate() )
        {
            return ( getEndPaddedString( colName, null ) );
        }
        else
        {
            String dateString = DbTimeHelper.getDateStringFromLongTime( dateValue );
            return ( getEndPaddedString( colName, dateString ) );
        }
    }

    public String getEndPaddedDateTimeString( String colName, long dateTimeValue )
    {
        if ( dateTimeValue == DbTable.getNullDate() )
        {
            return ( getEndPaddedString( colName, null ) );
        }
        else
        {
            String dateString = DbTimeHelper.getDateTimeStringFromLongTime( dateTimeValue );
            return ( getEndPaddedString( colName, dateString ) );
        }
    }

}