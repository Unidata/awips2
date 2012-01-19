/*
 * Created on May 24, 2004
 *
 * 
 */
package ohd.hseb.grid;


import java.util.HashMap;
import java.util.Map;

import ohd.hseb.db.DbTimeHelper;

/**
 * @author GobsC
 *
 *
 */
public class XmrgGrid
{
    public static final double OFF_GRID_VALUE = -8888.0;
    private double _missingValue = -999.0;
    
    private long _time = 0;
    
// old way of storing grid
    private Map _valueMap  = null;
   // private List _valueList = null;
   

//new way of storing grid
    private double[][] _gridValuesArray = null;

    private boolean _isValid = true;

    
    private int _rowCount = 0;
    private int _colCount = 0;

    private int _baseRow = 0; //southern row
    private int _baseCol = 0; // western col


  //  private boolean _newWay = true;
 
    
//  -----------------------------------------------------------------------------------
    private XmrgGrid()
    {
        _isValid = true;
    }

//  -----------------------------------------------------------------------------------
    public static XmrgGrid getInvalidGrid(long time)
    {
        XmrgGrid grid = new XmrgGrid(time);

        grid.setIsValid(false);

        return grid;
    }


//  -----------------------------------------------------------------------------------

    public XmrgGrid(long time)
    {
        _time = time;
        _valueMap = new HashMap();
       // _valueList = new ArrayList();
        
    }
    
 //  -----------------------------------------------------------------------------------
   
    public XmrgGrid(long time, int  southernRowIndex,  int westernColIndex,
                   int rowCount, int colCount)
    {
                
          _time = time;
        
          // new way
          _rowCount = rowCount;
          _colCount = colCount;

          _baseRow = southernRowIndex;
          _baseCol = westernColIndex;

          _gridValuesArray = new double[_rowCount][_colCount];

          return;
    }

//  -----------------------------------------------------------------------------------
 
    public void setValue(int hrapRow, int hrapCol, double value)
    {
        int rowIndex = hrapRow - _baseRow;
        int colIndex = hrapCol - _baseCol;

        _gridValuesArray[rowIndex][colIndex] = value;

        return;
    }
//  -----------------------------------------------------------------------------------
 
    public double[][] getValuesArray()
    {
        return _gridValuesArray;
    }
    
//  -----------------------------------------------------------------------------------
 
    public double getValue(int hrapRow, int hrapCol)
    {
        int rowIndex = hrapRow - _baseRow;
        int colIndex = hrapCol - _baseCol;

        double value = OFF_GRID_VALUE;
        
        //Chip CHANGE 8/23/05  
        if (
             (rowIndex > -1) && (rowIndex < _rowCount)  && 
             (colIndex > -1) && (colIndex < _colCount)
           )
        {             
            value = _gridValuesArray[rowIndex][colIndex];
        }
        else
        {
           
           // System.out.println("XmrgGrid.getValue(): attempting to access " +
           //         "rowIndex = " + rowIndex + "  colIndex = " + colIndex);

           // System.out.print("  Row Values should be >= 0 < " + _rowCount);
           // System.out.println("  and column values should be >= 0 < " + _colCount);   

        }
        return value;
    }
 //  -----------------------------------------------------------------------------------
   
	public void setTime(long time)
	{
		_time = time;
	}
//  -----------------------------------------------------------------------------------

	public long getTime()
	{
		return _time;
	}
    
//  -----------------------------------------------------------------------------------
    private Object getKey(int hrapRow, int hrapCol)
    {
        Long key = new Long(hrapRow*10000 + hrapCol);
        
        return key;    
    }
    
//  -----------------------------------------------------------------------------------
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
    //    String outString = null;
        double value = 0;
        
         
        buffer.append("time = " + DbTimeHelper.getDateTimeStringFromLongTime(_time) + "\n");
        /*   
        for (int i = 0; i < _valueList.size(); i++)
        {
            String keyValue = (String) _valueList.get(i);
            StringTokenizer tokenizer = new StringTokenizer(keyValue, "|");
    
           
            while (tokenizer.hasMoreTokens())
            {
                 String hrapRow = tokenizer.nextToken();
                 String hrapCol = tokenizer.nextToken();
                 String valueString = tokenizer.nextToken();
                 value = Double.parseDouble(valueString);
                 
                 if (value > 0.0)
                 {
                    buffer.append("hrapRow = " + hrapRow + " hrapCol = " + hrapCol + " value = " + value + "\n");
                 }
            
            }
            
        }
        */ 
        return buffer.toString();
        
    }
//  -----------------------------------------------------------------------------------

    public void setIsValid(boolean isValid)
    {
	    _isValid = isValid;
    }
//  -----------------------------------------------------------------------------------

    public boolean isValid()
    {
	    return _isValid;
    }

    private void setBaseRow(int baseRow)
    {
        _baseRow = baseRow;
    }

    public int getBaseRow()
    {
        return _baseRow;
    }

    private void setBaseCol(int baseCol)
    {
        _baseCol = baseCol;
    }

    public int getBaseCol()
    {
        return _baseCol;
    }

    public int getRowCount()
    {
        return _rowCount;
    }

    public int getColCount()
    {
        return _colCount;
    }

//  -----------------------------------------------------------------------------------

}
