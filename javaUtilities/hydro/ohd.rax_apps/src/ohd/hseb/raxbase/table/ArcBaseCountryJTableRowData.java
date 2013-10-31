package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseCountryJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _country = null;
    private String _countryFips = null;
        
    public ArcBaseCountryJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Country = " + _country + " CountryFips = " + _countryFips;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Country", CellType.STRING, getCountry(), _missingRepresentation ) );
        addCell( new BaseTableCell( "CountryFips", CellType.STRING, getCountryFips(), _missingRepresentation ) );
    }

    public void setCountry( String country )
    {
        _country = country;
    }

    public String getCountry()
    {
        return _country;
    }

    public void setCountryFips( String countryFips )
    {
        _countryFips = countryFips;
    }

    public String getCountryFips()
    {
        return _countryFips;
    }
}
