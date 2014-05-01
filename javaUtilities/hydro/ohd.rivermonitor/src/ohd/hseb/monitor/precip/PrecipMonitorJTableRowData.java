package ohd.hseb.monitor.precip;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbTable;
import ohd.hseb.monitor.LocationInfoColumns;
import ohd.hseb.monitor.MonitorCell;
import ohd.hseb.monitor.ThreatLevel;
import ohd.hseb.monitor.derivedcolumns.DerivedColumn;
import ohd.hseb.monitor.precip.PrecipData;
import ohd.hseb.monitor.precip.settings.PrecipColumnDataSettings;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.CellType;

public class PrecipMonitorJTableRowData extends AbstractJTableRowData
{
    private int _defaultDecimalPoints = 2;

    private static Map _derivedReturnTypeToCellTypeMap = new HashMap();

    private String _groupName;
    private String _lid;
    private String _name;
    private String _county;
    private String _state;
    private String _hsa;
    private String _riverBasin;
    private String _groupId;
    private int _groupOrdinal;
    private int _locationOrdinal;
    private double _obsFcstMax;
    private SessionLogger _logger;

    protected PrecipMonitorDataManager _precipMonitorDataManager;

    private String _toolTipTextForPrecipThreatSummary;
    private List _derivedColumnsDetailsList;

    private PrecipData _precipData;  
    private PrecipColumnDataSettings _precipSettings;
    
    static
    {
        _derivedReturnTypeToCellTypeMap.put("boolean", CellType.BOOLEAN);
        _derivedReturnTypeToCellTypeMap.put("double", CellType.DOUBLE);
        _derivedReturnTypeToCellTypeMap.put("float", CellType.FLOAT);
        _derivedReturnTypeToCellTypeMap.put("long", CellType.LONG);
        _derivedReturnTypeToCellTypeMap.put("int", CellType.INTEGER);
        _derivedReturnTypeToCellTypeMap.put("string", CellType.STRING);

    }

    public PrecipMonitorJTableRowData(String missingRepresentation, PrecipMonitorDataManager precipMonitorDataManager, 
                                       SessionLogger logger)
    {
        setMissingRepresentation(missingRepresentation);
        _logger = logger;
        _precipMonitorDataManager = precipMonitorDataManager;
    }
    
    public String getLid()
    {
        return _lid;
    }

    public void setLid(String lid)
    {
        this._lid = lid ;
    }

    public String getName()
    {
        return _name;
    }

    public void setName(String name)
    {
        this._name = name ;
    }

    public String getCounty()
    {
        return _county;
    }

    public void setCounty(String county)
    {
        this._county = county ;
    }

    public String getState()
    {
        return _state;
    }

    public void setState(String state)
    {
        this._state = state ;
    }

    public String getHsa()
    {
        return _hsa;
    }

    public void setHsa(String hsa)
    {
        this._hsa = hsa ;
    }

    public String getGroupName()
    {
        return _groupName;
    }

    public void setGroupName(String groupName)
    {
        this._groupName = groupName ;
    }

    public String getGroupId()
    {
        return _groupId;
    }

    public void setGroupId(String groupId)
    {
        this._groupId = groupId ;
    }

    public int getGroupOrdinal()
    {
        return _groupOrdinal;
    }

    public void setGroupOrdinal(int groupOrdinal)
    {
        this._groupOrdinal = groupOrdinal ;
    }

    public int getLocationOrdinal()
    {
        return _locationOrdinal;
    }

    public void setLocationOrdinal(int locationOrdinal)
    {
        this._locationOrdinal = locationOrdinal ;
    }

    public void setObsFcstMax(double obsFcstMax)
    {
        this._obsFcstMax = obsFcstMax;
    }

    public double getObsFcstMax()
    {
        return _obsFcstMax;
    }

    public PrecipData getPrecipData()
    {
        return _precipData;
    }

    public void setPrecipData(PrecipData precipData)
    {
        _precipData = precipData;
    }
    
    public void setDerivedColumnsDetailsList(List list)
    {
        _derivedColumnsDetailsList = list; 
    }

    public List getDerivedColumnsDetailsList()
    {
        return _derivedColumnsDetailsList;
    }


    public String getStringValue(long value, long missingValue)
    {
        String result = super.getStringValue(value, missingValue);
        if(result != super.getMissingRepresentation())
        {
            String tempStr = result.substring(5, result.length()-3);
            tempStr = tempStr.replace('-','/');
            tempStr = tempStr.replace(' ', '-');
            result = tempStr;
        }
        return result;
    }

    public String getToolTipTextForPrecipThreatSummary()
    {
        return _toolTipTextForPrecipThreatSummary;
    }

    public void setToolTipTextForPrecipThreatSummary(String toolTipText)
    {
        _toolTipTextForPrecipThreatSummary = toolTipText;
    }

    public void resetCellMap()
    {
        super.resetCellMap();
    }

    public void addAllCellsToMap()
    {
        ThreatLevel defaultThreatLevel = ThreatLevel.NO_THREAT;
        String header = "RiverMonitorJTableRowData2.addAllCellsToMap(): ";

        String dateTimeFormatString = "MM/dd - HH:mm";
        
        _precipSettings = _precipMonitorDataManager.getPrecipSettings();

        //static data

        addToCellMap(LocationInfoColumns.GROUP_NAME, CellType.STRING, getGroupName() );  
        addToCellMap(LocationInfoColumns.GROUP_ID, CellType.STRING, getGroupId() );
        addToCellMap(LocationInfoColumns.GROUP_ORDINAL, CellType.INTEGER, getGroupOrdinal());

        addToCellMap(LocationInfoColumns.LOCATION_ID, CellType.STRING, getLid() );
        addToCellMap(LocationInfoColumns.LOCATION_NAME, CellType.STRING, getName() );
        addToCellMap(LocationInfoColumns.LOCATION_ORDINAL, CellType.INTEGER, getLocationOrdinal());

        addToCellMap(LocationInfoColumns.COUNTY, CellType.STRING, getCounty() );
        addToCellMap(LocationInfoColumns.STATE, CellType.STRING, getState() );
        
        addToCellMap(LocationInfoColumns.HSA,    CellType.STRING, getHsa() );
 
        addToCellMap(LocationInfoColumns.RIVER_BASIN, CellType.STRING, getRiverBasin());
                
        addToCellMap(PrecipColumns.LATEST_PRECIP_PARAM_CODE,  CellType.STRING, getPrecipData().getLatestPrecipParamCodeString() );
        
        addToCellMap(PrecipColumns.LATEST_30MIN, CellType.DOUBLE, getPrecipData().getLatestPrecip30Min()); 
        addToCellMap(PrecipColumns.LATEST_1HR, CellType.DOUBLE, getPrecipData().getLatestPrecip1Hr(), getThreatLevelForPrecipColumn("LATEST", 1));
        addToCellMap(PrecipColumns.LATEST_3HR, CellType.DOUBLE, getPrecipData().getLatestPrecip3Hr(), getThreatLevelForPrecipColumn("LATEST", 3));
        addToCellMap(PrecipColumns.LATEST_6HR, CellType.DOUBLE, getPrecipData().getLatestPrecip6Hr(), getThreatLevelForPrecipColumn("LATEST", 6));
        addToCellMap(PrecipColumns.LATEST_12HR, CellType.DOUBLE, getPrecipData().getLatestPrecip12Hr());
        addToCellMap(PrecipColumns.LATEST_18HR, CellType.DOUBLE, getPrecipData().getLatestPrecip18Hr());
        addToCellMap(PrecipColumns.LATEST_24HR, CellType.DOUBLE, getPrecipData().getLatestPrecip24Hr());

        addToCellMap(PrecipColumns.TOH_PRECIP_1HR_PARAM_CODE,  CellType.STRING, getPrecipData().getTohPrecip1HrParamCodeString() );
        addToCellMap(PrecipColumns.TOH_PRECIP_1HR, CellType.DOUBLE, getPrecipData().getTohPrecip1Hr(), getThreatLevelForPrecipColumn("TOH", 1));
   
        addToCellMap(PrecipColumns.FFG_1HR, CellType.DOUBLE, getPrecipData().getFFG1Hr());
        addToCellMap(PrecipColumns.FFG_3HR, CellType.DOUBLE, getPrecipData().getFFG3Hr());
        addToCellMap(PrecipColumns.FFG_6HR, CellType.DOUBLE, getPrecipData().getFFG6Hr());

        addToCellMap(PrecipColumns.DIFF_1HR, CellType.DOUBLE, getPrecipData().getDiff1Hr(), getThreatLevelForPrecipColumn("DIFF", 1));
        addToCellMap(PrecipColumns.DIFF_3HR, CellType.DOUBLE, getPrecipData().getDiff3Hr(), getThreatLevelForPrecipColumn("DIFF", 3));
        addToCellMap(PrecipColumns.DIFF_6HR, CellType.DOUBLE, getPrecipData().getDiff6Hr(), getThreatLevelForPrecipColumn("DIFF", 6));

        addToCellMap(PrecipColumns.RATIO_1HR, CellType.INTEGER, getPrecipData().getRatio1Hr(), getThreatLevelForPrecipColumn("RATIO", 1));
        addToCellMap(PrecipColumns.RATIO_3HR, CellType.INTEGER, getPrecipData().getRatio3Hr(), getThreatLevelForPrecipColumn("RATIO", 3));
        addToCellMap(PrecipColumns.RATIO_6HR, CellType.INTEGER, getPrecipData().getRatio6Hr(), getThreatLevelForPrecipColumn("RATIO", 6));

        ThreatLevel threatLevel = getThreatLevelForPrecipThreatSummary();    
        addToCellMap(PrecipColumns.PRECIP_THREAT, CellType.EXTENSION,
            getPrecipThreatSummary(), threatLevel,
            getMissingRepresentation());

    } //end addAllCellsToMap

    // -----------------------------------------------------------------------------------

    public double round(double value, int decimalPlacesToMaintain)
    {
        return MathHelper.roundToNDecimalPlaces(value, decimalPlacesToMaintain);
    }

    private MonitorCell addToCellMap(String columnName, CellType cellType,
            Object value)
    {
        MonitorCell cell = addToCellMap(columnName,  cellType, 
            value,  ThreatLevel.NO_THREAT, getMissingRepresentation());       

        return cell;
    }
    // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value, ThreatLevel threatLevel)
    {

        MonitorCell cell =  addToCellMap(columnName,  cellType,
            value, threatLevel, getMissingRepresentation(), _defaultDecimalPoints);   

        return cell;
    }
    
    // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value, Color cellBackgroundColor)
    {

        MonitorCell cell =  addToCellMap(columnName,  cellType,
            value, ThreatLevel.NO_THREAT, cellBackgroundColor, getMissingRepresentation(), _defaultDecimalPoints);   

        return cell;
    }
//  -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value, ThreatLevel threatLevel,
            String missingRepresentation)
    {

        MonitorCell cell =  addToCellMap(columnName,  cellType,
            value, threatLevel, missingRepresentation, _defaultDecimalPoints);   

        return cell;
    }
    // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value,  ThreatLevel threatLevel,
            String missingRepresentation,
            int decimalPointsForDisplay)
    {
        MonitorCell cell = new MonitorCell (columnName,  cellType,
            value, threatLevel,
            missingRepresentation, decimalPointsForDisplay);
        addCell(cell);

        return cell;
    }

 // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value,  ThreatLevel threatLevel,Color cellBackgroundColor,
            String missingRepresentation,
            int decimalPointsForDisplay)
    {
        MonitorCell cell = new MonitorCell (columnName,  cellType,
            value, threatLevel, cellBackgroundColor,
            missingRepresentation, decimalPointsForDisplay);
        addCell(cell);

        return cell;
    }

    // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value, ThreatLevel threatLevel,
            String missingRepresentation,
            String dateFormatString)
    {        

        MonitorCell cell = new MonitorCell (columnName,  cellType, value, threatLevel,
            missingRepresentation, dateFormatString);


        // addToCellMap(cell);
        addCell(cell);


        return cell;
    }

    // -----------------------------------------------------------------------------------
    
    protected void addDerivedDataToCellMap()
    {
        //   add all derived columns as well        
        List derivedColumnsDetailsList = this.getDerivedColumnsDetailsList();
        for(int i=0 ; i < derivedColumnsDetailsList.size(); i++)
        {
            DerivedColumn desc1 = (DerivedColumn) derivedColumnsDetailsList.get(i);

            CellType cellType = getCellTypeFromReturnType(desc1.getReturnType());

            addToCellMap(desc1.getColumnName(), cellType, desc1.getValue(), desc1.getCellBackgroundColor());       
        }
    }
    // -----------------------------------------------------------------------------------
    
    private CellType getCellTypeFromReturnType(String derivedColumnReturnType)
    {
        CellType cellType = null;

        cellType = (CellType) _derivedReturnTypeToCellTypeMap.get(derivedColumnReturnType.toLowerCase());

        return cellType;
    }
    

    public Color getCellBackgroundColor(String columnName)
    {
        Color color = Color.white;

        MonitorCell cell = (MonitorCell) getCell(columnName);

        if (cell != null)
        {   
            color = cell.getCellBackgroundColor();
        }

        return color;
    }

    // -----------------------------------------------------------------------------------

    public Color getCellForegroundColor(String columnName)
    {
        Color color = Color.black;

        MonitorCell cell = (MonitorCell) getCell(columnName);

        if (cell != null)
        {
            color = cell.getCellForegroundColor();
        }
        return color;
    }

    // -----------------------------------------------------------------------------------
  
    public ThreatLevel getPrecipMonitorThreatLevel()
    {
        //Find the max threat levels for all cells in this row
        ThreatLevel maxCellThreatLevel = ThreatLevel.NO_THREAT;
  
        List cellList = new ArrayList( getCellMap().values());
        maxCellThreatLevel = getMaxRowThreatLevel(cellList);
        
        ThreatLevel rowThreatLevel = maxCellThreatLevel;
        
        //If the rowThreatLevel == missing, we want to make sure that ALL of the important columns have missing data.
        //If there is any data in these columns, then we want to use NO_THREAT
        if (rowThreatLevel == ThreatLevel.MISSING_DATA)
        {
            if (_precipData.isDataAvailable())
            {
                rowThreatLevel = ThreatLevel.NO_THREAT;    
            }
        }

        return rowThreatLevel;   
    }
    // helper methods

    // -----------------------------------------------------------------------------------
    public Color getCriticalColorForThisRow()
    {
        Color color = Color.WHITE;
        ThreatLevel level = getPrecipMonitorThreatLevel();
        if(level == ThreatLevel.ALERT)
        {
            color = Color.RED;
        }
        else if(level == ThreatLevel.CAUTION)
        {
            color = Color.YELLOW;
        }
        else if(level == ThreatLevel.AGED_DATA || 
                level == ThreatLevel.MISSING_DATA)
        {
            color = Color.GRAY;
        }
        
        return color;    
    }

    //  -----------------------------------------------------------------------------------
    public ThreatLevel getThreatLevelForPrecipThreatSummary()
    {
        ThreatLevel level = getPrecipThreatLevelForPrecipThreatSummary();

        //Threat cell should be color red/yellow/white. So skip threat aged / missing threat level 
        if(level == ThreatLevel.AGED_DATA ||
                level == ThreatLevel.MISSING_DATA)
        {
            level = ThreatLevel.NO_THREAT;
        }

        return level;
    }

//  -----------------------------------------------------------------------------------
    public ThreatLevel getMaxRowThreatLevel(List cellList)
    {
        //this method assumes that the threat column has not yet been added to the cell map, when this
        // method is invoked
        ThreatLevel level = ThreatLevel.NO_THREAT;
        ThreatLevel maxLevel = ThreatLevel.ALERT;

        //make sure the threat cell's threat level does not get used to calculate row's threat level, so clear it to zero
        ThreatLevel cellThreatLevel = ThreatLevel.NO_THREAT;

        //iterate through each cell, and set the level to the max threat level found
        for (int i = 0; i < cellList.size(); i++)
        {

            MonitorCell cell = (MonitorCell) cellList.get(i);

            if (cell == null)
            {
                continue;
            }
            else
            {
                cellThreatLevel = cell.getThreatLevel();

                if (cellThreatLevel.isGreater(level))
                {
                    level = cellThreatLevel;
                    if (level == maxLevel)
                    {
                        break; //quit looking, because we have determined it is maxed out
                    }
                }
            }
        }
        return level;
    }
    // -----------------------------------------------------------------------------------


    public List getPrecipCells()
    {
        List cellList = new ArrayList();

        addCell(cellList, PrecipColumns.LATEST_30MIN);
        addCell(cellList, PrecipColumns.LATEST_1HR);
        addCell(cellList, PrecipColumns.LATEST_3HR );
        addCell(cellList, PrecipColumns.LATEST_6HR );
        
        addCell(cellList, PrecipColumns.TOH_PRECIP_1HR );
        
        addCell(cellList, PrecipColumns.DIFF_1HR );
        addCell(cellList, PrecipColumns.DIFF_3HR );
        addCell(cellList, PrecipColumns.DIFF_6HR );
        
        addCell(cellList, PrecipColumns.RATIO_1HR );
        addCell(cellList, PrecipColumns.RATIO_3HR );
        addCell(cellList, PrecipColumns.RATIO_6HR );
 
        return cellList;
    }
    
    // -----------------------------------------------------------------------------------

    private void  addCell(List cellList, String columnName )
    {
        MonitorCell cell = null;
        
        cell = (MonitorCell) getCellMap().get(columnName);
        if(cell != null)
        {
            cellList.add(cell);
        }
        
        return;
    }
    
    // -----------------------------------------------------------------------------------

    public ThreatLevel getPrecipThreatLevelForPrecipThreatSummary()
    {
        ThreatLevel cellThreatLevel = ThreatLevel.NO_THREAT;

        List cellList = getPrecipCells();
        cellThreatLevel = getMaxRowThreatLevel(cellList);

        return cellThreatLevel;   
    }
    //--------------------------------------------------------------------------------------
    /*
     * Returns true if the data is aged, by checking the time with earliestAcceptableTime
     */
    protected boolean isDataAged(long  time, long earliestAcceptableTime)
    {
        boolean result = false;

        if (time < earliestAcceptableTime)
        {
            result = true;
        }

        return result;
    }

    private ThreatLevel getPrecipThreatLevel(double value, double alert, double caution)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        
        if(! DbTable.isNull(value))
        {
            if(value >= alert)
                level = ThreatLevel.ALERT;
            else if(value >= caution)
                level = ThreatLevel.CAUTION;
        }
        else //value isNull()
        {
            level = ThreatLevel.MISSING_DATA;
        }

        return level;
    }
//  -------------------------------------------------------------------------------------  
    protected ThreatLevel getPrecipValueThreatLevelForHrMentioned(int hour, PrecipColumnDataSettings precipSettings, String type)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        double missing = DbTable.getNullDouble();
        double value;
        switch(hour)
        {

            case 1:
                if(type.compareTo("LATEST") == 0)
                    value = _precipData.getLatestPrecip1Hr();
                else
                    value = _precipData.getTohPrecip1Hr();
                
                level = getPrecipThreatLevel( value,  precipSettings.getPrecipAlert1Hr(),  precipSettings.getPrecipCaution1Hr());
                break;
                
            case 3:
                if(type.compareTo("LATEST") == 0)
                    value = _precipData.getLatestPrecip3Hr();
                else
                   // value = _precipData.getTohPrecip3Hr();
                    value = missing;
                
                level = getPrecipThreatLevel( value,  precipSettings.getPrecipAlert3Hr(),  precipSettings.getPrecipCaution3Hr());
                break;
            case 6:
                if(type.compareTo("LATEST") == 0)
                    value = _precipData.getLatestPrecip6Hr();
                else
                    //value = _precipData.getTohPrecip6Hr();
                    value = missing;

                level = getPrecipThreatLevel( value,  precipSettings.getPrecipAlert6Hr(),  precipSettings.getPrecipCaution6Hr());
                break;

        }
        return level;

    }
//  -------------------------------------------------------------------------------------  
    protected ThreatLevel getPrecipRatioThreatLevelForHrMentioned(int hour, PrecipColumnDataSettings precipSettings)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        int value;
        switch(hour)
        {

            case 1:
                value = _precipData.getRatio1Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getRatioAlert1Hr(),  precipSettings.getRatioCaution1Hr());
                break;
            case 3:
                value = _precipData.getRatio3Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getRatioAlert3Hr(),  precipSettings.getRatioCaution6Hr());
                break;
            case 6:
                value = _precipData.getRatio6Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getRatioAlert6Hr(),  precipSettings.getRatioCaution6Hr());
                break;

        }
        return level;

    }
//  -------------------------------------------------------------------------------------  
    protected ThreatLevel getPrecipDiffThreatLevelForHrMentioned(int hour, PrecipColumnDataSettings precipSettings)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        double value;

        switch(hour)
        {
            case 1:
                value = _precipData.getDiff1Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getDiffAlert1Hr(),  precipSettings.getDiffCaution1Hr());
                break;
            case 3:
                value = _precipData.getDiff3Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getDiffAlert3Hr(),  precipSettings.getDiffCaution3Hr());
                break;
            case 6:
                value = _precipData.getDiff6Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getDiffAlert6Hr(),  precipSettings.getDiffCaution6Hr());
                break;
        }
        return level;
    }
   
//  -------------------------------------------------------------------------------------

    protected ThreatLevel getThreatLevelForPrecipColumn(String type, int hour)
    {

        ThreatLevel level = ThreatLevel.NO_THREAT;

        if(type.compareTo("LATEST") == 0) // LATEST precip
        {
            level = getPrecipValueThreatLevelForHrMentioned(hour, _precipSettings, "LATEST");
        }
        else if(type.compareTo("TOH") == 0) // toh precip
        {
            level = getPrecipValueThreatLevelForHrMentioned(hour, _precipSettings, "TOH");
        }
        else if(type.compareTo("RATIO") == 0)
        {
            level = getPrecipRatioThreatLevelForHrMentioned(hour, _precipSettings);
        }
        else //type is DIFF
        {
            level = getPrecipDiffThreatLevelForHrMentioned(hour, _precipSettings);
        }
        return level;
    }


//  -------------------------------------------------------------------------------------

    public ThreatLevel getCellThreatLevelForColumn(String columnName)
    {
       // String header = "PrecipMonitorJTableRowData.getCellThreatLevelForColumn(): ";
        ThreatLevel level;

        MonitorCell cell = (MonitorCell) getCellMap().get(columnName);

       // System.out.println(header + "columnName = " + columnName);
        
        level = cell.getThreatLevel();

        return level;
    }

//  -------------------------------------------------------------------------------------


    public String getPrecipThreatSummary()
    {
        String summary = "";
        StringBuffer summaryStringBuffer = new StringBuffer("");
        StringBuffer toolTipStringBuffer = new StringBuffer("<HTML><BODY>");

        ThreatLevel level;
        ThreatLevel level1Hr = getCellThreatLevelForColumn(PrecipColumns.LATEST_1HR);
        ThreatLevel level3Hr = getCellThreatLevelForColumn(PrecipColumns.LATEST_3HR);
        ThreatLevel level6Hr = getCellThreatLevelForColumn(PrecipColumns.LATEST_6HR);
        if( ((level1Hr == ThreatLevel.ALERT || level1Hr == ThreatLevel.CAUTION)) ||
                ((level3Hr == ThreatLevel.ALERT || level3Hr == ThreatLevel.CAUTION)) ||
                ((level6Hr == ThreatLevel.ALERT || level6Hr == ThreatLevel.CAUTION)) )
        {
            summaryStringBuffer = summaryStringBuffer.append("L");
            toolTipStringBuffer.append("L: Latest Precip Exceeded the threshold<BR>");
        }

        level1Hr = getCellThreatLevelForColumn(PrecipColumns.TOH_PRECIP_1HR);
      //  level3Hr = getCellThreatLevelForColumn(PrecipColumns.TOH_PRECIP_3HR);
     //   level6Hr = getCellThreatLevelForColumn(PrecipColumns.TOH_PRECIP_6HR);
        if ( 
                ((level1Hr == ThreatLevel.ALERT || level1Hr == ThreatLevel.CAUTION)) 
               /* ||
                ((level3Hr == ThreatLevel.ALERT || level3Hr == ThreatLevel.CAUTION)) ||
                ((level6Hr == ThreatLevel.ALERT || level6Hr == ThreatLevel.CAUTION)) 
                */
           )
        {
            summaryStringBuffer = summaryStringBuffer.append("T");
            toolTipStringBuffer.append("T: Top Of The Hour Precip Exceeded the threshold<BR>");
        }

        level1Hr = getCellThreatLevelForColumn(PrecipColumns.DIFF_1HR);
        level3Hr = getCellThreatLevelForColumn(PrecipColumns.DIFF_3HR);
        level6Hr = getCellThreatLevelForColumn(PrecipColumns.DIFF_6HR);
        if( ((level1Hr == ThreatLevel.ALERT || level1Hr == ThreatLevel.CAUTION)) ||
                ((level3Hr == ThreatLevel.ALERT || level3Hr == ThreatLevel.CAUTION)) ||
                ((level6Hr == ThreatLevel.ALERT || level6Hr == ThreatLevel.CAUTION)) )
        {
            summaryStringBuffer = summaryStringBuffer.append("D");
            toolTipStringBuffer.append("D: Latest Precip - FFG  Difference Exceeded the threshold<BR>");
        }

        level1Hr = getCellThreatLevelForColumn(PrecipColumns.RATIO_1HR);
        level3Hr = getCellThreatLevelForColumn(PrecipColumns.RATIO_3HR);
        level6Hr = getCellThreatLevelForColumn(PrecipColumns.RATIO_6HR);
        if( ((level1Hr == ThreatLevel.ALERT || level1Hr == ThreatLevel.CAUTION)) ||
                ((level3Hr == ThreatLevel.ALERT || level3Hr == ThreatLevel.CAUTION)) ||
                ((level6Hr == ThreatLevel.ALERT || level6Hr == ThreatLevel.CAUTION)) )
        {
            summaryStringBuffer = summaryStringBuffer.append("R");
            toolTipStringBuffer.append("R: Latest Precip / FFG Ratio Exceeded the threshold<BR>");
        }

        summary = summaryStringBuffer.toString();
        _toolTipTextForPrecipThreatSummary = toolTipStringBuffer.toString();    
        return summary;
    }
//  -------------------------------------------------------------------------------------

    public void setRiverBasin(String riverBasin)
    {
        _riverBasin = riverBasin;
    }

    public String getRiverBasin()
    {
        return _riverBasin;
    }

} //end class 

