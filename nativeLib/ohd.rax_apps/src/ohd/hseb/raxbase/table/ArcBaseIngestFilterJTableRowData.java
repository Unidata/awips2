package ohd.hseb.raxbase.table;

import ohd.hseb.raxbase.model.RaxIngestFilter;
import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class ArcBaseIngestFilterJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
 
    private RaxIngestFilter _raxIngestFilter = null;

    public ArcBaseIngestFilterJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Lid = " + getRaxIngestFilter().getLid() + " PE = " + getRaxIngestFilter().getPe() + " dur = " + getRaxIngestFilter().getIdur() + " TS = " + getRaxIngestFilter().getTs() + " Extremum = " + getRaxIngestFilter().getExtremum();
    }

    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Location", CellType.STRING, _raxIngestFilter.getLid(), _missingRepresentation ) );
        addCell( new BaseTableCell( "PE", CellType.STRING, _raxIngestFilter.getPe(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Dur", CellType.STRING, _raxIngestFilter.getIdur() + "/" + _raxIngestFilter.getDur(), _missingRepresentation ) );
        addCell( new BaseTableCell( "TypeSrc", CellType.STRING, _raxIngestFilter.getTs(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Ext", CellType.STRING, _raxIngestFilter.getExtremum(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Rank", CellType.SHORT, _raxIngestFilter.getTsRank(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Master", CellType.BOOLEAN, _raxIngestFilter.isIngest(), _missingRepresentation ) );
        addCell( new BaseTableCell( "OFS", CellType.BOOLEAN, _raxIngestFilter.isOfsInput(), _missingRepresentation ) );
        addCell( new BaseTableCell( "MPE", CellType.BOOLEAN, _raxIngestFilter.isMpeInput(), _missingRepresentation ) );
    }

    
    public String getStringValue(short value)
    {
        return String.valueOf(value);
    }

    public void setRaxIngestFilter( RaxIngestFilter raxIngestFilter )
    {
        _raxIngestFilter = raxIngestFilter;
    }

    public RaxIngestFilter getRaxIngestFilter()
    {
        return _raxIngestFilter;
    }

}
