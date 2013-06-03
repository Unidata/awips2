package ohd.hseb.monitor;

import ohd.hseb.util.gui.jtable.TableCellInterface;
import java.awt.Color;

import ohd.hseb.monitor.ThreatLevel;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class MonitorCell extends BaseTableCell
{
    private ThreatLevel _monitorThreatLevel = ThreatLevel.NO_THREAT;

    private static int defaultDecimalPointsForDisplay = 2;

    // -------------------------------------------------------------------------------------------------------

    public MonitorCell (String columnName, CellType cellType, 
            Object value, ThreatLevel threatLevel, 
            String missingRepresentation)
    {
        super(columnName, cellType, value, missingRepresentation, defaultDecimalPointsForDisplay);
        this.setThreatLevel(threatLevel);
    }

    public MonitorCell (String columnName, CellType cellType,
            Object value, ThreatLevel threatLevel, Color cellBackgroundColor,
            String missingRepresentation,
            int decimalPointsForDisplay)
    {
        super(columnName, cellType, value, cellBackgroundColor, missingRepresentation,decimalPointsForDisplay);
        this.setThreatLevel(threatLevel);
    }

    public MonitorCell (String columnName, CellType cellType,
            Object value, ThreatLevel threatLevel,
            String missingRepresentation,
            int decimalPointsForDisplay)
    {
        super(columnName, cellType, value, missingRepresentation,decimalPointsForDisplay);
        this.setThreatLevel(threatLevel);
    }

    public MonitorCell (String columnName, CellType cellType, 
            Object value, ThreatLevel threatLevel,
            String missingRepresentation,                              
            String dateFormatString)
    {
        super(columnName, cellType, value, missingRepresentation, dateFormatString);
        this.setThreatLevel(threatLevel);
    }

    public ThreatLevel getThreatLevel()
    {
        return _monitorThreatLevel;
    }

    public void setThreatLevel(ThreatLevel rivermonitorThreatLevel)
    { 
        _monitorThreatLevel = rivermonitorThreatLevel;
    }

    public Color getCellBackgroundColor()
    {
        String header = "MonitorCell.getCellBackgroundColor(): ";
        Color color = super.getCellBackgroundColor();

       
        if (color == null)
        {
            System.out.println(header + "color = " + color);
            System.out.println(header + "Color was null for column name = " + getColumnName() + ". Setting to Color.WHITE.");
            color = Color.WHITE; //bug fix for occasional, mysterious nullness
        }
        
        if( color == Color.WHITE)
        {
           ThreatLevel threatLevel = getThreatLevel();

            switch(threatLevel)
            {
                case MISSING_DATA: 
                    color = Color.gray;
                    break;

                case AGED_DATA:
                    color = Color.gray;
                    break;

                case CAUTION:
                    color = Color.yellow;
                    break;

                case ALERT:
                    color = Color.red;
                    break;

                default:
                    color = Color.white;
            }
        }
         return color;
    }

    public Color getCellForegroundColor()
    {
        String header = "MonitorCell.getCellForegroundColor(): ";
        Color color = Color.black;

        Color backgroundColor = getCellBackgroundColor();
     //   System.out.println(header + "backgroundColor = " + backgroundColor);
        
        
        if (backgroundColor.equals(Color.red) || (backgroundColor.equals(Color.gray) ) )
        {
            color = Color.yellow;
        }

        return color;
    }

    public int compare(TableCellInterface otherCell)
    {
        int result = 0;

        if (getCellType() != CellType.EXTENSION)
        {
            result = super.compare(otherCell);
        }
        else
        {   
            result = compareThreat(otherCell);
        }
        return result;
    }

    private int compareThreat(TableCellInterface otherCell)
    {
        MonitorCell otherThreatCell = (MonitorCell) otherCell;
        int result = 0;
        result = getThreatLevel().compareTo(otherThreatCell.getThreatLevel());
        if(result == 0)
        {
            result = compareCells((String) getValue(), (String)otherCell.getValue());
            result *= -1;
        }

        return result;
    }

    public String getDisplayString()
    {
        String displayString = null;

        if (getCellType() != CellType.EXTENSION)
        {
            displayString = super.getDisplayString();
        }
        else
        {
            displayString = getStringValue((String) getValue());
        }
        return displayString;
    }

}
