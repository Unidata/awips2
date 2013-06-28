package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseStateJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _state = null;
    private String _countryFips = null;
    private String _name = null;
    private String _ncdc = null;
    private String _stateFips = null;
        
    public ArcBaseStateJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "State = " + _state + 
               " | CountryFips = " + _countryFips +
               " | Name = " + _name +
               " | Ncdc = " + _ncdc +
               " | StateFips = " + _stateFips;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "State", CellType.STRING, getState(), _missingRepresentation ) );
        addCell( new BaseTableCell( "CountryFips", CellType.STRING, getCountryFips(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Name", CellType.STRING, getName(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Ncdc", CellType.STRING, getNcdc(), _missingRepresentation ) );
        addCell( new BaseTableCell( "StateFips", CellType.STRING, getStateFips(), _missingRepresentation ) );
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

    public String getName()
    {
        return _name;
    }

    public void setName(String name)
    {
        _name = name ;
    }

    public String getNcdc()
    {
        return _ncdc;
    }

    public void setNcdc(String ncdc)
    {
        _ncdc = ncdc ;
    }

    public void setStateFips( String stateFips )
    {
        _stateFips = stateFips;
    }
    
    public String getStateFips()
    {
        return _stateFips;
    }
}
