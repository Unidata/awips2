package ohd.hseb.geomap.model;

public class RowColumnPoint
{
    private double _row = 0;
    private double _column = 0;
    
    // -----------------------------------------------------------------------------------------
    public RowColumnPoint(double row, double column)
    {
        setRow(row);
        setColumn(column);
    }
    
    // -----------------------------------------------------------------------------------------
     
    public void setRow(double row)
    {
        _row = row;
    }
    
    // -----------------------------------------------------------------------------------------
    
    public double getRow()
    {
        return _row;
    }
    
    // -----------------------------------------------------------------------------------------
    
    public void setColumn(double column)
    {
        _column = column;
    }
    
    // -----------------------------------------------------------------------------------------
    
    public double getCol()
    {
        return _column;
    }

    // -----------------------------------------------------------------------------------------
    
    public String toString()
    {
        return "row = " + getRow() + " col = " + getCol();
    }
    // -----------------------------------------------------------------------------------------

}
