package ohd.hseb.util.gui.jtable;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

import ohd.hseb.db.DbTimeHelper;

public class AbstractJTableRowData implements JTableRowData 
{
    private Map _cellMap = new HashMap();

    protected String _missingRepresentation = "MISSING"; 

    public void setMissingRepresentation(String missingRepresentation)
    {
        _missingRepresentation = missingRepresentation;
    }

    public void resetCellMap()
    {
        _cellMap.clear();
    }
    
    public String getMissingRepresentation()
    {
        return _missingRepresentation;
    }

    public Color getCellBackgroundColor(String columnName) 
    {
        return null;
    }

    public Color getCellForegroundColor(String columnName) 
    {
        return null;
    }


    public int compareBoolean( boolean value1, boolean value2 )
    {
        int returnValue = 0;

        if ( value1 == value2 )
        {
            returnValue = 0;
        }
        else if ( value1 == false )
        {
            returnValue = -1;
        }
        else  
        {
            returnValue = 1;
        }

        return returnValue;
    }

    public int compareStrings(String string1, String string2)
    {
        int result = 0;

        if ( (string1 != null) && (string2 != null) )
        {
            result = string1.compareTo(string2);
        }
        else if ( (string1 == null) && (string2 == null))
        {
            result = 0;
        }
        else if (string1 == null)
        {
            result = -1;
        }
        else // string2 == null
        {
            result = 1;	
        }
        return result;
    }

    public int compareNumbers( int value1, int value2 )
    {
        int returnValue = 0;

        if ( value1 < value2 )
        {
            returnValue = -1;
        }
        else if ( value1 == value2 )
        {
            returnValue = 0;
        }
        else  
        {
            returnValue = 1;
        }

        return returnValue;
    }

    public int compareNumbers( short value1, short value2 )
    {
        int returnValue = 0;

        if ( value1 < value2 )
        {
            returnValue = -1;
        }
        else if ( value1 == value2 )
        {
            returnValue = 0;
        }
        else  
        {
            returnValue = 1;
        }

        return returnValue;
    }

    public int compareNumbers( double value1, double value2 )
    {
        int returnValue = 0;

        if ( value1 < value2 )
        {
            returnValue = -1;
        }
        else if ( value1 == value2 )
        {
            returnValue = 0;
        }
        else  
        {
            returnValue = 1;
        }

        return returnValue;
    }

    public int compareNumbers( float value1, float value2 )
    {
        int returnValue = 0;

        if ( value1 < value2 )
        {
            returnValue = -1;
        }
        else if ( value1 == value2 )
        {
            returnValue = 0;
        }
        else  
        {
            returnValue = 1;
        }

        return returnValue;
    }

    public int compareNumbers( long value1, long value2 )
    {
        int returnValue = 0;

        if ( value1 < value2 )
        {
            returnValue = -1;
        }
        else if ( value1 == value2 )
        {
            returnValue = 0;
        }
        else  
        {
            returnValue = 1;
        }

        return returnValue;
    }

    public int compareColors( Color value1, Color value2 )
    {
        int returnValue = 0;

        if(value1 == value2)
        {
            returnValue = 0;
        }
        else if(value1 == Color.YELLOW && value2 == Color.RED)
        {
            returnValue = -1;
        }
        else if(value1 == Color.RED && value2 == Color.YELLOW)
        {
            returnValue = 1;
        }
        else if(value1 == Color.WHITE && value2 != Color.WHITE)
        {
            returnValue = -1;
        }
        else if(value1 != Color.WHITE && value2 == Color.WHITE)
        {
            returnValue = 1;
        }

        return returnValue;
    }

    public String getStringValue(boolean value)
    {
        String result = String.valueOf(value);
        return result;
    }

    public String getStringValue(double value)
    {
        String result = _missingRepresentation;
        if ( value != 0 )
        {
            result = String.valueOf(value);
        }
        return result;
    }

    public String getStringValue(double value, double missingValue)
    {
        String result = _missingRepresentation;
        if ( value != missingValue )
        {
            result = String.valueOf(value);
        }
        return result;
    }

    public String getStringValue(float value, float missingValue)
    {
        String result = _missingRepresentation;
        if ( value != missingValue )
        {
            result = String.valueOf(value);
        }
        return result;
    }

    public String getStringValue(int value)
    {
        String result = _missingRepresentation;
        if ( value != 0 )
        {
            result = String.valueOf(value);
        }
        return result;
    }

    public String getStringValue(int value, int missingValue)
    {
        String result = _missingRepresentation;
        if ( value != missingValue )
        {
            result = String.valueOf(value);
        }
        return result;
    }

    public String getStringValue(short value)
    {
        String result = _missingRepresentation;
        if ( value != 0 )
        {
            result = String.valueOf(value);
        }
        return result;
    }

    public String getStringValue(short value, short missingValue)
    {
        String result = _missingRepresentation;
        if ( value != missingValue )
        {
            result = String.valueOf(value);
        }
        return result;
    }

    public String getStringValue(long value, long missingValue)
    {
        String result = _missingRepresentation;
        if ( value != missingValue )
        {
            result = String.valueOf(DbTimeHelper.getDateTimeStringFromLongTime
                (value));
        }
        return result;
    }

    public String getStringValue(String value)
    {
        String result = _missingRepresentation;
        if ( value != null )
        {
            result = String.valueOf(value);
        }
        return result;
    }

    public Map getCellMap()
    {
        return _cellMap;
    }

    public void addCell(BaseTableCell cell)
    {
        _cellMap.put(cell.getColumnName(), cell);
    }

    // -----------------------------------------------------------------------------------

    public BaseTableCell getCell(String columnName)
    {
        BaseTableCell cell = (BaseTableCell) _cellMap.get(columnName);

        return cell;
    }
//  -----------------------------------------------------------------------------------

    public String getDataValue(String columnName)
    {
        //     _timer.restart();

        //RiverMonitorCell cell = (RiverMonitorCell) getCell(columnName);
        BaseTableCell cell = (BaseTableCell) getCell(columnName);

        String displayString = cell.getDisplayString();
        //    _timer.stop("Get Data Value for column:"+ columnName + " Time elapsed:");
        return displayString;

    }
    // -----------------------------------------------------------------------------------
    public int compare(String columnName, JTableRowData rowData)
    {
        int compareValue = 0;
        //String header = "RiverMonitorJTableRowData2.compare(): ";

        /*        RiverMonitorJTableRowData otherRiverMonitorRowData = (RiverMonitorJTableRowData) rowData;

        RiverMonitorCell cell = getCell(columnName);
        RiverMonitorCell otherCell = otherRiverMonitorRowData.getCell(columnName);*/

        BaseTableCell cell = getCell(columnName);
        BaseTableCell otherCell = rowData.getCell(columnName);


        if (( cell != null ) || (otherCell != null) )
        {
            compareValue = cell.compare(otherCell);
        }

        return compareValue;

    }



}
