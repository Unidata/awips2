package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseCountiesJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _county = null;
    private String _state = null;
    private String _countryFips = null;
    private String _countyFips = null;
    private String _wfo = null;
    private String _zon = null;
        
    public ArcBaseCountiesJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "County = " + _county +
               " | State = " + _state + 
               " | CountryFips = " + _countryFips +
               " | CountyFips = " + _countyFips +
               " | Wfo = " + _wfo +
               " | Zon = " + _zon;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "County", CellType.STRING, getCounty(), _missingRepresentation ) );
        addCell( new BaseTableCell( "State", CellType.STRING, getState(), _missingRepresentation ) );
        addCell( new BaseTableCell( "CountryFips", CellType.STRING, getCountryFips(), _missingRepresentation ) );
        addCell( new BaseTableCell( "CountyFips", CellType.STRING, getCountyFips(), _missingRepresentation ) );
        addCell( new BaseTableCell( "WFO", CellType.STRING, getWfo(), _missingRepresentation ) );
        addCell( new BaseTableCell( "ZON", CellType.STRING, getZon(), _missingRepresentation ) );
    }
    
    public String getState()
    {
        return _state;
    }

    public void setState(String state)
    {
        _state = state ;
    }

    public String getCountryFips()
    {
        return _countryFips;
    }

    public void setCountryFips(String countryfips)
    {
        _countryFips = countryfips ;
    }

    public String getCountyFips()
    {
        return _countyFips;
    }

    public void setCountyFips(String countyfips)
    {
        _countyFips = countyfips ;
    }

    public String getWfo()
    {
        return _wfo;
    }

    public void setWfo(String wfo)
    {
        _wfo = wfo ;
    }

    public String getZon()
    {
        return _zon;
    }

    public void setZon(String zon)
    {
        _zon = zon ;
    }

    public void setCounty( String county )
    {
        _county = county;
    }

    public String getCounty()
    {
        return _county;
    }
}
