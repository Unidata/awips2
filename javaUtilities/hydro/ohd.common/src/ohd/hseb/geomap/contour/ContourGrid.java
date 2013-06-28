package ohd.hseb.geomap.contour;

/**
 * 
 * @author cgobs
 * This is an interface for a 2-D (row * col) non-jagged array that contains doubles.
 */
public interface ContourGrid
{
    
     double getValue(int row, int col);
     void   setValue(int row, int col, double value);
     
     double[][] getValuesArray();
     
     int getRowCount();
     int getColCount();

}
