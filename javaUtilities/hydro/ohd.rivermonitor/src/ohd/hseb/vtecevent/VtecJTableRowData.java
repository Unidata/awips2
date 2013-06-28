package ohd.hseb.vtecevent;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class VtecJTableRowData extends AbstractJTableRowData 
{
	private String  _geoId;
	private String  _productId;
	private long    _productTime;
	private long    _ugcExpireTime;
	private String  _productMode;
	private String  _action;
	private String  _officeId;
	private String  _phenom;
	private String  _signif;
	private short     _etn;  
	private long    _beginTime;
	private long    _endTime;
	private String _severity;
	private String _immedCause;
	private long   _riseTime;
	private long   _crestTime;
	private long   _fallTime;
	private String _record;
	private String _risets;
	private String _crests;
	private String _fallts;
	private String _active;
	private double _crestValue;
	
	public VtecJTableRowData(String missingRepresentation)
	{
		setMissingRepresentation(missingRepresentation);
	}

	public String getAction() 
	{
		return _action;
	}

	public void setAction(String action) 
	{
		this._action = action;
	}

	public long getBeginTime() 
	{
		return _beginTime;
	}

	public void setBeginTime(long beginTime) 
	{
		_beginTime = beginTime;
	}

	public String getCrests() 
	{
		return _crests;
	}

	public void setCrests(String crests) 
	{
		this._crests = crests;
	}

	public long getCrestTime() 
	{
		return _crestTime;
	}

	public void setCrestTime(long crestTime) 
	{
		_crestTime = crestTime;
	}

	public long getEndTime() 
	{
		return _endTime;
	}

	public void setEndTime(long endTime) 
	{
		_endTime = endTime;
	}

	public short getEtn() 
	{
		return _etn;
	}

	public void setEtn(short etn) 
	{
		this._etn = etn;
	}

	public long getFallTime() 
	{
		return _fallTime;
	}

	public void setFallTime(long fallTime) 
	{
		_fallTime = fallTime;
	}

	public String getFallts() 
	{
		return _fallts;
	}

	public void setFallts(String fallts) 
	{
		this._fallts = fallts;
	}

	public String getGeoId() 
	{
		return _geoId;
	}

	public void setGeoId(String geoId) 
	{
		_geoId = geoId;
	}

	public String getImmedCause() 
	{
		return _immedCause;
	}

	public void setImmedCause(String immedCause) 
	{
		_immedCause = immedCause;
	}

	public String getOfficeId() 
	{
		return _officeId;
	}

	public void setOfficeId(String officeId) 
	{
		_officeId = officeId;
	}

	public String getPhenom() 
	{
		return _phenom;
	}

	public void setPhenom(String phenom) 
	{
		this._phenom = phenom;
	}

	public String getProductId() 
	{
		return _productId;
	}

	public void setProductId(String productId) 
	{
		_productId = productId;
	}

	public String getProductMode() 
	{
		return _productMode;
	}

	public void setProductMode(String productMode) 
	{
		_productMode = productMode;
	}

	public long getProductTime() 
	{
		return _productTime;
	}

	public void setProductTime(long productTime) 
	{
		_productTime = productTime;
	}

	public long getUGCExpireTime() 
	{
		return _ugcExpireTime;
	}

	public void setUGCExpireTime(long ugcExpireTime) 
	{
		_ugcExpireTime = ugcExpireTime;
	}
	
	public String getRecord() 
	{
		return _record;
	}

	public void setRecord(String record) 
	{
		this._record = record;
	}

	public long getRiseTime() 
	{
		return _riseTime;
	}

	public void setRiseTime(long riseTime) 
	{
		_riseTime = riseTime;
	}

	public String getRisets() 
	{
		return _risets;
	}

	public void setRisets(String risets) 
	{
		this._risets = risets;
	}

	public String getSeverity() 
	{
		return _severity;
	}

	public void setSeverity(String severity) 
	{
		this._severity = severity;
	}

	public String getSignif() 
	{
		return _signif;
	}

	public void setSignif(String signif) 
	{
		this._signif = signif;
	}

	public String getActive() 
	{
		return _active;
	}

	public void setActive(String active) 
	{
		this._active = active;
	}
	
	public double getCrestValue()
	{
		return _crestValue;
	}

	public void setCrestValue(double crestValue) 
	{
		_crestValue = crestValue;
	}
	
	public String toString()
	{
	  String str = null;
	  return str;
	}
	
	private void addToCellMap(String columnName, CellType cellType, 
	        Object value)
	{        
	    BaseTableCell cell = new BaseTableCell (columnName, cellType, value, getMissingRepresentation(), "MM/dd - HH:mm");
	    addCell(cell);

	}

	public void addAllCellsToMap()
	{
	    addToCellMap(VtecEventColumns.OFFICE_ID, CellType.STRING, getOfficeId());
	    addToCellMap(VtecEventColumns.LID, CellType.STRING, getGeoId());
	    addToCellMap(VtecEventColumns.PRODUCT_MODE, CellType.STRING, getProductMode());
	    addToCellMap(VtecEventColumns.ACTION, CellType.STRING, getAction());
	    addToCellMap(VtecEventColumns.PHENOMENON, CellType.STRING, getPhenom());
	    addToCellMap(VtecEventColumns.SIGNIFICANCE, CellType.STRING,getSignif());
	    addToCellMap(VtecEventColumns.EVENT_TRACKING_NUMBER,CellType.SHORT, getEtn());
	    addToCellMap(VtecEventColumns.BEGIN_TIME,CellType.DATE_TIME, getBeginTime());
	    addToCellMap(VtecEventColumns.END_TIME,CellType.DATE_TIME, getEndTime());
	    addToCellMap(VtecEventColumns.ACTIVE, CellType.STRING, getActive());
	    addToCellMap(VtecEventColumns.SEVERITY, CellType.STRING, getSeverity());
	    addToCellMap(VtecEventColumns.IMMEDIATE_CAUSE,CellType.STRING, getImmedCause());
	    addToCellMap(VtecEventColumns.RISE_TIME, CellType.DATE_TIME, getRiseTime());
	    addToCellMap(VtecEventColumns.CREST_TIME, CellType.DATE_TIME, getCrestTime());
	    addToCellMap(VtecEventColumns.FALL_TIME, CellType.DATE_TIME, getFallTime());
	    addToCellMap(VtecEventColumns.PRODUCT_ID, CellType.STRING, getProductId());
	    addToCellMap(VtecEventColumns.PRODUCT_TIME, CellType.DATE_TIME, getProductTime());
	    addToCellMap(VtecEventColumns.UGC_EXPIRE_TIME, CellType.DATE_TIME, getUGCExpireTime());
	    addToCellMap(VtecEventColumns.RECORD, CellType.STRING, getRecord());
	    addToCellMap(VtecEventColumns.RISE_TS, CellType.STRING, getRisets());
	    addToCellMap(VtecEventColumns.CREST_TS, CellType.STRING, getCrests());
	    addToCellMap(VtecEventColumns.FALL_TS, CellType.STRING, getFallts());
	    addToCellMap(VtecEventColumns.CREST_VALUE, CellType.DOUBLE,getCrestValue());
	}

}
