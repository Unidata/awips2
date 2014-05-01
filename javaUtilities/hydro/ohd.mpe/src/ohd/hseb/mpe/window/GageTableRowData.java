package ohd.hseb.mpe.window;

import java.util.Map;
import java.util.Set;

import ohd.hseb.db.DbTable;
import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class GageTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "M";
    
    // These are the parts of the Gage Table record
    // which always present.
    private String _gageId;
    private String _radarId;
    private float _value;  
    private Float _editValue;
    private Double _diffValue;
    private float _orginalValue;
    private Map<String, Double> _productValueMap;
    
    public GageTableRowData( String gageId, float value, Float editValue, Double diffValue, String radarId, Map<String, Double> productValueMap)
    {
    	
        setGageId(gageId);
        setValue(value);
        setEditValue(editValue);
        setDiffValue(diffValue);
        setRadarId(radarId);
        setOrginalValue(value);
        setProductValueMap(productValueMap);
    }
    
    public void setGageId(String gageId)
    {
        _gageId = gageId;
    }

    public String getGageId()
    {
        return _gageId;
    }

    public void setValue(float value)
    {
        _value = value;
    }

    public float getValue()
    {
        return _value;
    }
    
    public void addAllCellsToMap()
    {
        Set <Map.Entry<String, Double>> mapEntrySet = null;
        
        addCell ( new BaseTableCell("LID", CellType.STRING, getGageId(), _missingRepresentation));
        addCell ( new BaseTableCell("Gage Value", CellType.FLOAT, getValue(), _missingRepresentation));     
        addCell ( new BaseTableCell("Edit Gage Value", CellType.FLOAT, getEditValue(), ""));
        addCell (new BaseTableCell("Diff (Gage-Grid)", CellType.DOUBLE, getDiffValue(), _missingRepresentation));
        addCell ( new BaseTableCell("Radar ID", CellType.STRING, getRadarId(), _missingRepresentation));
        
        mapEntrySet = getProductValueMap().entrySet();
        
        for ( Map.Entry<String, Double> mapEntry : mapEntrySet )
        {
            addCell ( new BaseTableCell(mapEntry.getKey(), CellType.DOUBLE, mapEntry.getValue().doubleValue(), _missingRepresentation ));
        }
    }

    public void setProductValueMap(Map<String, Double> productValueMap)
    {
        _productValueMap = productValueMap;
    }

    public Map<String, Double> getProductValueMap()
    {
        return _productValueMap;
    }

    public void setOrginalValue(float orginalValue)
    {
        this._orginalValue = orginalValue;
    }

    public float getOrginalValue()
    {
        return _orginalValue;
    }

    public void setRadarId(String radarId)
    {
        _radarId = radarId;
    }

    public String getRadarId()
    {
        return _radarId;
    }	
    
    public Float getEditValue() {
		return _editValue;
	}

	public void setEditValue(Float _editValue) {
		this._editValue = _editValue;
	}
	
	public Double getDiffValue() {
		return _diffValue;
	}

	public void setDiffValue(Double _diffValue) {
		this._diffValue = _diffValue;
	}
    
}
