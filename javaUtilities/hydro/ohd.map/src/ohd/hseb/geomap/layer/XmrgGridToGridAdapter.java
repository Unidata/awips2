package ohd.hseb.geomap.layer;

import ohd.hseb.grid.XmrgGrid;
import ohd.hseb.geomap.contour.ContourGrid;

public class XmrgGridToGridAdapter implements ContourGrid
{
    private XmrgGrid _xmrgGrid = null;
    
    // ---------------------------------------------------------------------------------------------
    public XmrgGridToGridAdapter(XmrgGrid xmrgGrid)
    {
        _xmrgGrid = xmrgGrid;
    }
    // ---------------------------------------------------------------------------------------------
    
    public double getValue(int row, int col)
    {
        int hrapRow = row + _xmrgGrid.getBaseRow();
        int hrapCol = col + _xmrgGrid.getBaseCol();
         
        return  _xmrgGrid.getValue(hrapRow, hrapCol);
    }
    // ---------------------------------------------------------------------------------------------
    public void setValue(int row, int col, double value)
    {
        int hrapRow = row + _xmrgGrid.getBaseRow();
        int hrapCol = col + _xmrgGrid.getBaseCol();
        
        _xmrgGrid.setValue(hrapRow, hrapCol, value);
        
        return;
    }
    // ---------------------------------------------------------------------------------------------

    
    public int getColCount()
    {
        
        return _xmrgGrid.getColCount();
    }

    // ---------------------------------------------------------------------------------------------
   
    public int getRowCount()
    {
        return _xmrgGrid.getRowCount();
    }
    
    // ---------------------------------------------------------------------------------------------

    
    public double[][] getValuesArray()
    {
        return _xmrgGrid.getValuesArray();
    }

    // ---------------------------------------------------------------------------------------------

} //end XmrgGridToGridAdapter
