package ohd.hseb.alertalarm;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class AlertAlarmJTableRowData extends AbstractJTableRowData 
{
    private String _lid;           
    private String _pe;           
    private short    _dur;          
    private String _ts;          
    private String _extremum;      
    private float  _probability;   
    private long   _validTime;    
    private long   _basisTime;    
    private double _value;          
    private double _supplValue;    
    private String _shefQualCode; 
    private String    _qualityCode;   
    private String    _revision;       
    private String _productId;     
    private long   _productTime;    
    private long   _postingTime;    
    private long   _actionTime;    
    private String _aaCateg;      
    private String _aaCheck;

    public AlertAlarmJTableRowData(String missingRepresentation)
    {
        setMissingRepresentation(missingRepresentation);
    }

    public String getAaCateg() 
    {
        return _aaCateg;
    }
    public void setAaCateg(String aaCateg) 
    {
        _aaCateg = aaCateg;
    }
    public String getAaCheck() 
    {
        return _aaCheck;
    }
    public void setAaCheck(String aaCheck) 
    {
        _aaCheck = aaCheck;
    }
    public long getActionTime() 
    {
        return _actionTime;
    }
    public void setActionTime(long actionTime) 
    {
        _actionTime = actionTime;
    }
    public long getBasisTime() 
    {
        return _basisTime;
    }
    public void setBasisTime(long basisTime) 
    {
        _basisTime = basisTime;
    }
    public short getDur() 
    {
        return _dur;
    }
    public void setDur(short dur) 
    {
        this._dur = dur;
    }
    public String getExtremum() 
    {
        return _extremum;
    }
    public void setExtremum(String extremum) 
    {
        this._extremum = extremum;
    }
    public String getLid() 
    {
        return _lid;
    }
    public void setLid(String lid) 
    {
        this._lid = lid;
    }
    public String getPe() 
    {
        return _pe;
    }
    public void setPe(String pe) 
    {
        this._pe = pe;
    }
    public long getPostingTime() 
    {
        return _postingTime;
    }
    public void setPostingTime(long postingTime) 
    {
        _postingTime = postingTime;
    }
    public float getProbability() 
    {
        return _probability;
    }
    public void setProbability(float probability) 
    {
        this._probability = probability;
    }
    public String getProductId() 
    {
        return _productId;
    }
    public void setProductId(String productId) 
    {
        _productId = productId;
    }
    public long getProductTime() 
    {
        return _productTime;
    }
    public void setProductTime(long productTime) 
    {
        _productTime = productTime;
    }
    public String getQualityCode() 
    {
        return _qualityCode;
    }
    public void setQualityCode(String qualityCode) 
    {
        _qualityCode = qualityCode;
    }
    public String getRevision() 
    {
        return _revision;
    }
    public void setRevision(String revision) 
    {
        this._revision = revision;
    }
    public String getShefQualCode() 
    {
        return _shefQualCode;
    }
    public void setShefQualCode(String shefQualCode) 
    {
        _shefQualCode = shefQualCode;
    }
    public double getSupplValue() 
    {
        return _supplValue;
    }
    public void setSupplValue(double supplValue) 
    {
        _supplValue = supplValue;
    }
    public String getTs() 
    {
        return _ts;
    }
    public void setTs(String ts) 
    {
        this._ts = ts;
    }
    public long getValidTime() 
    {
        return _validTime;
    }
    public void setValidTime(long validTime) 
    {
        _validTime = validTime;
    }
    public double getValue() 
    {
        return _value;
    }
    public void setValue(double value) 
    {
        this._value = value;
    }  

    // -----------------------------------------------------------------------------------

    private void addToCellMap(String columnName, CellType cellType, 
            Object value)
    {        
        BaseTableCell cell = new BaseTableCell (columnName, cellType, value, getMissingRepresentation(), "MM/dd - HH:mm");
        addCell(cell);

     }

    public void addAllCellsToMap()
    {
        addToCellMap(AlertAlarmColumns.LID, CellType.STRING, getLid());
        addToCellMap(AlertAlarmColumns.PE, CellType.STRING, getPe());
        addToCellMap(AlertAlarmColumns.DUR, CellType.SHORT, getDur());
        addToCellMap(AlertAlarmColumns.TS, CellType.STRING, getTs());
        addToCellMap(AlertAlarmColumns.EXT, CellType.STRING, getExtremum());
        addToCellMap(AlertAlarmColumns.PROB, CellType.FLOAT,getProbability());
        addToCellMap(AlertAlarmColumns.VALID_TIME,CellType.DATE_TIME, getValidTime());
        addToCellMap(AlertAlarmColumns.BASIS_TIME,CellType.DATE_TIME,getBasisTime());
        addToCellMap(AlertAlarmColumns.VALUE,CellType.DOUBLE, getValue());
        addToCellMap(AlertAlarmColumns.SUP_VALUE, CellType.DOUBLE, getSupplValue());
        addToCellMap(AlertAlarmColumns.SHEF_QUAL_CODE, CellType.STRING, getShefQualCode());
        addToCellMap(AlertAlarmColumns.QUAL_CODE,CellType.STRING, getQualityCode());
        addToCellMap(AlertAlarmColumns.REVISION, CellType.STRING, getRevision());
        addToCellMap(AlertAlarmColumns.PRODUCT_ID, CellType.STRING, getProductId());
        addToCellMap(AlertAlarmColumns.PROD_TIME, CellType.DATE_TIME, getProductTime());
        addToCellMap(AlertAlarmColumns.POSTING_TIME, CellType.DATE_TIME, getPostingTime());
        addToCellMap(AlertAlarmColumns.ACTION_TIME, CellType.DATE_TIME, getActionTime());
        addToCellMap(AlertAlarmColumns.DESC, CellType.STRING, getAaCateg());
        addToCellMap(AlertAlarmColumns.THREAT, CellType.STRING,getAaCheck());

    }
   
}
