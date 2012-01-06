package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseProdJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private static final short MISSING = -9999;
    
    private String _id = null;
    private int _pmax = MISSING;
    private int _pap = MISSING;
    private int _err = MISSING;
    private String _gra = null;
    private String _pr1 = null;
    private String _net = null;
    
    public ArcBaseProdJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Id = " + _id +
               " | Pmax = " + _pmax +
               " | Pap = " + _pap +
               " | Err = " + _err + 
               " | Gra = " + _gra +
               " | Pr1 = " + _pr1 +
               " | Net = " + _net;
    }

    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Id", CellType.STRING, getId(), _missingRepresentation ) );
        addCell( new BaseTableCell( "PMax", CellType.INTEGER, getPmax(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Pap", CellType.INTEGER, getPap(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Err", CellType.INTEGER, getErr(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Gra", CellType.STRING, getGra(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Pr1", CellType.STRING, getPr1(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Net", CellType.STRING, getNet(), _missingRepresentation ) );
    }
    
    public String getId()
    {
        return _id;
    }

    public void setId(String id)
    {
        _id = id ;
    }

    public int getPmax()
    {
        return _pmax;
    }

    public void setPmax(int pmax)
    {
        _pmax = pmax ;
    }

    public int getPap()
    {
        return _pap;
    }

    public void setPap(int pap)
    {
        _pap = pap ;
    }

    public int getErr()
    {
        return _err;
    }

    public void setErr(int err)
    {
        _err = err ;
    }

    public String getGra()
    {
        return _gra;
    }

    public void setGra(String gra)
    {
        _gra = gra ;
    }

    public String getPr1()
    {
        return _pr1;
    }

    public void setPr1(String pr1)
    {
        _pr1 = pr1 ;
    }

    public String getNet()
    {
        return _net;
    }

    public void setNet(String net)
    {
        _net = net ;
    }
}
