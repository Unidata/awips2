package ohd.hseb.monitor.derivedcolumns;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

public class DerivedColumn 
{
	private String _columnName;
	private Color _color;
	private String _equationForCellColor;
	private String _returnType;
	private String _equationForCellValue;
	private Object _value;
	private static Map _colorMap = new HashMap();
	
	static 
	{
	  _colorMap.put("black", Color.BLACK);
	  _colorMap.put("blue", Color.BLUE);
	  _colorMap.put("cyan", Color.CYAN);
	  _colorMap.put("darkgray", Color.DARK_GRAY);
	  _colorMap.put("gray", Color.GRAY);
	  _colorMap.put("green", Color.GREEN);
	  _colorMap.put("lightgray", Color.LIGHT_GRAY);
	  _colorMap.put("magenta", Color.MAGENTA);
	  _colorMap.put("orange", Color.ORANGE);
	  _colorMap.put("pink", Color.PINK);
	  _colorMap.put("red", Color.RED);
	  _colorMap.put("white", Color.WHITE);
	  _colorMap.put("yellow", Color.YELLOW);
	}
	
	public String getColumnName() 
	{
		return _columnName;
	}
	
	public void setColumnName(String columnName) 
	{
		_columnName = columnName;
	}
	
	public String getEquationForCellValue() 
	{
		return _equationForCellValue;
	}
	
	public void setEquationForCellValue(String valueEquation) 
	{
		this._equationForCellValue = valueEquation;
	}
	
	public String getEquationForCellColor() 
	{
		return _equationForCellColor;
	}
	
	public void setEquationForCellColor(String cellEquation) 
	{
		this._equationForCellColor = cellEquation;
	}
	
	public String getReturnType() 
	{
		return _returnType;
	}
	
	public void setReturnType(String returnType) 
	{
		_returnType = returnType;
	}
	
	public Object getValue() 
	{
		return _value;
	}
	
	public void setvalue(Object value) 
	{
		_value = value;
	}
	
	public void setCellBackgroundColor(Color color)
	{
		_color = color;
	}
	
	public Color getCellBackgroundColor()
	{
		return _color;
	}
	
	public void setCellBackgroundColor(String colorName)
	{
		 Color color = (Color) _colorMap.get(colorName.toLowerCase());
	
		 if (color != null)
		 {
			setCellBackgroundColor(color); 
		 }
	}
}
