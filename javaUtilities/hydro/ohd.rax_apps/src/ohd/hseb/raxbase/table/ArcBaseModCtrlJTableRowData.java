package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseModCtrlJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _modName = null;
    private boolean _load = false;
    private boolean _fetchOper = false;
    private boolean _fetchSpin = false;
        
    public ArcBaseModCtrlJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "ModName = " + _modName + " Load = " + _load + " FetchOper = " + _fetchOper + " FetchSpin = " + _fetchSpin;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "ModName", CellType.STRING, getModName(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Load", CellType.BOOLEAN, isLoad(), _missingRepresentation ) );
        addCell( new BaseTableCell( "FetchOper", CellType.BOOLEAN, isFetchOper(), _missingRepresentation ) );
        addCell( new BaseTableCell( "FetchSpin", CellType.BOOLEAN, isFetchSpin(), _missingRepresentation ) );
    }

    public void setModName( String modName )
    {
        _modName = modName;
    }

    public String getModName()
    {
        return _modName;
    }

    public void setLoad( boolean load )
    {
        _load = load;
    }

    public boolean isLoad()
    {
        return _load;
    }

    public void setFetchOper( boolean fetchOper )
    {
        _fetchOper = fetchOper;
    }

    public boolean isFetchOper()
    {
        return _fetchOper;
    }

    public void setFetchSpin( boolean fetchSpin )
    {
        _fetchSpin = fetchSpin;
    }

    public boolean isFetchSpin()
    {
        return _fetchSpin;
    }

}
