package ohd.hseb.db;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

public class DbArrayColumn implements java.sql.Array
{
    
    private String _arrayString = null;
    private double[][] _array = null;
    
    public DbArrayColumn(String arrayString)
    {
        _arrayString = arrayString;
        double[][] newArray = { {1.0, 2.0 }, {2.0, 8.0 }, {3.0, 27.0 } };
        _array = newArray;
    }

    public Object getArray() throws SQLException
    {
     
        return _array ;
  
    }

    public Object getArray(Map<String, Class<?>> arg0) throws SQLException
    {
         return _array;
    }

    public Object getArray(long index, int count) throws SQLException
    {
         return _array[(int)index][0];
    }

    public Object getArray(long arg0, int arg1, Map<String, Class<?>> arg2) throws SQLException
    {
        return _array;
     
    }

    public int getBaseType() throws SQLException
    {
   
        return java.sql.Types.DOUBLE;
    }

    public String getBaseTypeName() throws SQLException
    {  
        return "double[]";
    }

    public String toString()
    {
        return _arrayString;
    }
    
    public ResultSet getResultSet() throws SQLException
    {
        // TODO Auto-generated method stub
        return null;
    }

    public ResultSet getResultSet(Map<String, Class<?>> arg0) throws SQLException
    {
        // TODO Auto-generated method stub
        return null;
    }

    public ResultSet getResultSet(long index, int count) throws SQLException
    {
        // TODO Auto-generated method stub
        return null;
    }

    public ResultSet getResultSet(long arg0, int arg1, Map<String, Class<?>> arg2) throws SQLException
    {
        // TODO Auto-generated method stub
        return null;
    }

	@Override
	public void free() throws SQLException {
		// TODO Auto-generated method stub
		
	}
    
}
