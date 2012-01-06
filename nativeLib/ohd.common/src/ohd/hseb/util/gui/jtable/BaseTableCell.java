package ohd.hseb.util.gui.jtable;

import java.awt.Color;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import ohd.hseb.db.DbTable;
import ohd.hseb.util.MathHelper;

public class BaseTableCell implements TableCellInterface
{
    protected static final double _missingDouble = DbTable.getNullDouble();
    protected static final float _missingFloat = DbTable.getNullFloat();
    protected static final long _missingLong = DbTable.getNullLong();
    protected static final int _missingInt = DbTable.getNullInt();
    protected static final short _missingShort = DbTable.getNullShort();
    protected static final long _missingDateTime = DbTable.getNullLong(); //stored as longs
    protected static final long _missingDate = DbTable.getNullLong(); //stored as longs

    private int _decimalPointsForDisplay = 2;

    private static final String _defaultDateTimeFormatString = "yyyy-MM-dd HH:mm:ss";
    private static final String _defaultDateFormatString = "yyyy-MM-dd";
  
    private String _dateFormatString = null;

    private Object _value = 0;
    private String _columnName = null;
    private CellType _cellType = null;
    private Color _backgroundColor = Color.WHITE;
    private Color _foregroundColor = Color.BLACK;

    private String _missingRepresentation =  null;

    // ---------------------------------------------------------------------------

    public BaseTableCell(String columnName, CellType cellType, Object value, String missingRepresentation)
    {
        setValue(value);
        setColumnName(columnName);
        setCellType(cellType);     
        setMissingRepresentation(missingRepresentation);
    }

    // ---------------------------------------------------------------------------

    public BaseTableCell(String columnName, CellType cellType, Object value, String missingRepresentation,
            int decimalPointsForDisplay)
    {
        this( columnName,  cellType,  value,  missingRepresentation);
        setDecimalPointsForDisplay(decimalPointsForDisplay);
    }

 // ---------------------------------------------------------------------------

    public BaseTableCell(String columnName, CellType cellType, Object value, Color cellBackgroundColor, String missingRepresentation,
            int decimalPointsForDisplay)
    {
        this( columnName,  cellType,  value,  missingRepresentation);
        setDecimalPointsForDisplay(decimalPointsForDisplay);
        setCellBackgroundColor(cellBackgroundColor);
    }

    // ---------------------------------------------------------------------------
    public BaseTableCell(String columnName, CellType cellType, Object value, String missingRepresentation,
            String dateFormatString)
    {
        this( columnName,  cellType,  value,  missingRepresentation);
        setDateFormatString(dateFormatString);
    }

//  ---------------------------------------------------------------------------

    public BaseTableCell(String columnName, CellType cellType, Object value, String missingRepresentation,
            int decimalPointsForDisplay, String dateFormatString)
    {
        this( columnName,  cellType,  value,  missingRepresentation, dateFormatString);
        setDecimalPointsForDisplay(decimalPointsForDisplay);
    }

//  ---------------------------------------------------------------------------

    public void setColumnName(String columnName)
    {
        this._columnName = columnName;
    }

    public String getColumnName()
    {
        return _columnName;
    }

    private void setCellType(CellType cellType)
    {
        _cellType = cellType;
    }

    protected CellType getCellType()
    {
        return _cellType;
    }

    public Object getValue()
    {
        return _value;
    }

    public void setValue(Object value)
    {
        _value = value;
    }

    public void setMissingRepresentation(String missingRepresentation)
    {
        _missingRepresentation = missingRepresentation;
    }

    public String getMissingRepresentation()
    {
        return _missingRepresentation;
    }

    public void setDecimalPointsForDisplay(int decimalPointsForDisplay)
    {
        _decimalPointsForDisplay = decimalPointsForDisplay;
    }

    public int getDecimalPointsForDisplay()
    {
        return _decimalPointsForDisplay;
    }

    public void setDateFormatString(String dateFormatString)
    {
        _dateFormatString = dateFormatString;
    }

    public String getDateFormatString()
    {
        return _dateFormatString;
    }

    //-------------------------------------------------------------------------------------------------------------------
   
    public int compare(TableCellInterface otherCell)
    {
        int result = 0;

        if (_cellType == CellType.STRING)
        {
            result = compareCells((String) _value, (String)otherCell.getValue() );
        }
        else if (_cellType == CellType.DOUBLE)
        {
            Double value1 = (Double)getValue();
            Double value2 = (Double)otherCell.getValue();
            result = compareCells(value1, value2);
        }
        else if (_cellType == CellType.FLOAT)
        {
            Float value1 = (Float)getValue();
            Float value2 = (Float)otherCell.getValue();        
            result = compareCells(value1, value2);
        }
        else if (_cellType == CellType.LONG) 
        {
            Long value1 = (Long)getValue();
            Long value2 = (Long)otherCell.getValue();
            result = compareCells(value1, value2);
        }
        else if (_cellType == CellType.INTEGER)
        {
            Integer value1 = (Integer)getValue();
            Integer value2 = (Integer)otherCell.getValue();
            result = compareCells(value1, value2);
        }
        else if (_cellType == CellType.SHORT)
        {
            Short value1 = (Short)getValue();
            Short value2 = (Short)otherCell.getValue();
            result = compareCells(value1, value2);
        }
        else if (_cellType == CellType.BOOLEAN)
        {
            Boolean value1 = (Boolean)getValue();
            Boolean value2 = (Boolean)otherCell.getValue();
            result = compareCells(value1, value2);
        }
        else if (_cellType == CellType.DATE_TIME  
                || _cellType == CellType.DATE) 
        {
            //date/times are stored as longs
            Long value1 = (Long)getValue();
            Long value2 = (Long)otherCell.getValue();
            result = compareCells(value1, value2);
        }
        
        return result;
    }

    public Color getCellBackgroundColor()
    {   
        return _backgroundColor;
    }

    public void setCellBackgroundColor(Color color)
    {   
        _backgroundColor = color;
        return;
    }

    public Color getCellForegroundColor()
    {
        return _foregroundColor;
    }

    public void setCellForegroundColor(Color color)
    {   
        _foregroundColor = color;
        return;
    }

  
    public String getDisplayString()
    {
        String displayString = null;

        if (_cellType == CellType.STRING)
        {
            displayString = getStringValue((String) getValue());
        }
        else if (_cellType == CellType.BOOLEAN)
        {
            Boolean value = (Boolean) getValue();
            displayString = getStringValue(value.booleanValue());

        }
        else if (_cellType == CellType.DOUBLE)
        {
            Double value = (Double) getValue();
            displayString = getStringValue(value.doubleValue(), _missingDouble, getDecimalPointsForDisplay());

        }
        else if (_cellType == CellType.FLOAT)
        {
            Float value = (Float) getValue();
            displayString = getStringValue(value.floatValue(), _missingFloat, getDecimalPointsForDisplay());
        }
        else if (_cellType == CellType.LONG) 
        {
            Long value = (Long) getValue();
            displayString = getStringValue(value.longValue(), _missingLong);
        }
        else if (_cellType == CellType.SHORT) 
        {
            Short value = (Short) getValue();
            displayString = getStringValue(value.shortValue(), _missingShort);
        }
        else if (_cellType == CellType.INTEGER)
        {
            Integer value = (Integer) getValue();
            displayString = getStringValue(value.intValue(), _missingInt);
        }
        else if (_cellType == CellType.DATE_TIME) 
        {
            Long value = (Long) getValue();

            //date/times are stored as longs

            displayString = getDateTimeStringValue(value.longValue(), _missingDateTime, _dateFormatString);

        }
        else if (_cellType == CellType.DATE) 
        {
            Long value = (Long) getValue();

            //date are stored as longs

            displayString = getDateStringValue(value.longValue(), _missingDate, _dateFormatString);

        }
        else
        {
            displayString = "UNKNOWN_TYPE";
        }

        return displayString;
    }

    public int compareCells(Comparable value1, Comparable value2)
    {
        int result;
        if( value1 == value2 ) //either both are null, or both are the same object
        {
            result = 0;   
        }
        else if(value1 == null)
        {
            result = -1;
        }
        else if(value2 == null)
        {
            result = 1;
        }
        else
        {
            result = value1.compareTo(value2);
        }
        return result;
    }

    //----------------------------------------------------------------------------------------------
    public String getStringValue(boolean value)
    {
        String result = _missingRepresentation;
        result = String.valueOf(value);
        return result;
    }

    public String getStringValue(double value, double missingValue, int decimalPoints)
    {
        String result = _missingRepresentation;
        if ( value != missingValue )
        {
            value =  MathHelper.roundToNDecimalPlaces(value, decimalPoints);
            result = String.format("%.2f", value);
        }

        return result;
    }

    public String getStringValue(float value, float missingValue, int decimalPoints)
    {
        String result = _missingRepresentation;
        if ( value != missingValue )
        {
            value =  (float) MathHelper.roundToNDecimalPlaces(value, decimalPoints);
            result = String.format("%.2f", value);
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
            result = String.valueOf(value);
        }
        return result;
    }

    private static String getStringFromLongTime(long time, String dateFormat)
    {
        String timeString  = null;

        SimpleDateFormat utcSdf2 = new SimpleDateFormat(dateFormat);
        utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
        timeString = utcSdf2.format(new java.util.Date(time));

        return timeString;
    }

    public String getDateTimeStringValue(long time, long missingValue, String dateFormatString)
    {
        String result = _missingRepresentation;
        String dateTimeFormatStringToUse = dateFormatString;
        if (dateTimeFormatStringToUse == null)
        {
            dateTimeFormatStringToUse = _defaultDateTimeFormatString;
        }

        if ( time != missingValue )
        {
            result = getStringFromLongTime(time, dateTimeFormatStringToUse );

        }
        return result;
    }

    public String getDateStringValue(long time, long missingValue, String dateFormatString)
    {
        String result = _missingRepresentation;
        String dateFormatStringToUse = dateFormatString;
        if (dateFormatStringToUse == null)
        {
            dateFormatStringToUse = _defaultDateFormatString;
        }

        if ( time != missingValue )
        {
            result = getStringFromLongTime(time, dateFormatStringToUse );

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

  
}
