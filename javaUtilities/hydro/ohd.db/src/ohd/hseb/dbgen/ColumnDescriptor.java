/*
 * Created on Aug 13, 2003
 *
 * 
 */
package ohd.hseb.dbgen;

/**
 * @author Chip Gobs
 *
 * This class encapsulates information about table columns, for the purpose of 
 * code generation.
 */
public class ColumnDescriptor
{
	private String _name = null;
	
	private int _sqlTypeInt = -1;
	private String _sqlTypeString = null;
	private int  _size = 0; //useful for char types
	
	private boolean _keyColumn = false;
	private boolean _nullable = true;

	public void setName(String name) 
	{
		_name = name;
	}

	public String getName() 
	{
		return _name;
	}

	
	public void setSqlTypeString(String sqlTypeString)
	{
		_sqlTypeString = sqlTypeString;
	}
	/*

	public String getSqlTypeString()
	{
		return _sqlTypeString;
	}
	*/
/*
	public void setResultSetType(String resultSetType)
	{
		_resultSetType = resultSetType;
	}

	public String getResultSetType()
	{
		return _resultSetType;
	}

	public void setJavaType(String javaType)
	{
		_javaType = javaType;
	}

	public String getJavaType()
	{
		return _javaType;
	}
*/
	public void setKeyColumn(boolean keyColumn)
	{
		_keyColumn = keyColumn;
	}

	public boolean isKeyColumn()
	{
		return _keyColumn;
	}

    /**
     * @param sqlTypeInt The sqlTypeInt to set.
     */
    public void setSqlTypeInt(int sqlTypeInt)
    {
        _sqlTypeInt = sqlTypeInt;
    }

    /**
     * @return Returns the sqlTypeInt.
     */
    public int getSqlTypeInt()
    {
        return _sqlTypeInt;
    }
    
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
        
        buffer.append("Name = " + _name  + "\n");
        buffer.append("SQL Type (Int) = " + _sqlTypeInt + "\n");
        buffer.append("SQL Type (String) = " + _sqlTypeString + "\n");
        buffer.append("size = " + _size + "\n");
        buffer.append("Is Key Column? = " + _keyColumn + "\n");
        
        
        return buffer.toString();
    }

    /**
     * @param size The size to set.
     */
    public void setSize(int size)
    {
        _size = size;
    }

    /**
     * @return Returns the size.
     */
    public int getSize()
    {
        return _size;
    }

    /**
     * @param nullable The nullable to set.
     */
    public void setNullable(boolean nullable)
    {
        _nullable = nullable;
    }

    /**
     * @return Returns the nullable.
     */
    public boolean isNullable()
    {
        return _nullable;
    }

  
    
	
} //end class ColumnDescriptor
