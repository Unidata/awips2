/*
 * Created on Aug 13, 2003
 *
 * This class encapsulates information about a table, including a list
 * of column descriptors.
 */
package ohd.hseb.dbgen;

import java.util.*;


/**
 * 
 */
public class TableDescriptor
{
    private String _name = null;
    private String _originalCaseTableName = null;
    private boolean _isView = false;
    private boolean _hasPrimaryKey = false;
    
    
    private List _columnDescList = new ArrayList();
    private List _keyColumnDescList = null;
    
    // ---------------------------------------------------------------------------------------------
    
    public TableDescriptor()
    {
        
    }
    
    // ---------------------------------------------------------------------------------------------
    
	public void setName(String name) 
	{
		this._name = name;
	}
	
	// ---------------------------------------------------------------------------------------------
	   
	public String getName()
	{
		return _name;
	}
	// ---------------------------------------------------------------------------------------------
	   
	/**
     * @param originalTableName The originalTableName to set.
     */
    public void setOriginalCaseTableName(String originalTableName)
    {
        _originalCaseTableName = originalTableName;
    }
    // ---------------------------------------------------------------------------------------------
    
    /**
     * @return Returns the originalTableName.
     */
    public String getOriginalCaseTableName()
    {
        return _originalCaseTableName;
    }
    
    // ---------------------------------------------------------------------------------------------
    
	public void setView(boolean isView)
	{
		this._isView = isView;
	}
   
	// ---------------------------------------------------------------------------------------------
	   
	public boolean isView()
	{
		return _isView;
	}
	
	//	 ---------------------------------------------------------------------------------------------
		
	public void setHasPrimaryKey(boolean hasPrimaryKey)
	{
		_hasPrimaryKey = hasPrimaryKey;
	}

	//	 ---------------------------------------------------------------------------------------------
	
	public boolean hasPrimaryKey()
	{
		return _hasPrimaryKey;
	}
	
	//	 ---------------------------------------------------------------------------------------------
	
	
	public List getColumnDescriptorList()
	{
	   return _columnDescList;	
	}
	
	//	 ---------------------------------------------------------------------------------------------
	
	public List getKeyColumnDescriptorList()
	{
	    
        if (_keyColumnDescList == null)
        {
            _keyColumnDescList = new ArrayList();
            
           // for each key column, check if it is a key column, and, if so, then insert it into the list
	        for (int i = 0 ; i < _columnDescList.size() ; i++)
	        {
	            ColumnDescriptor colDesc = (ColumnDescriptor) _columnDescList.get(i);
	            
	            if (colDesc.isKeyColumn())
	            {
	                _keyColumnDescList.add(colDesc);
	            }
	        }
        }
   
	    return _keyColumnDescList;
	}

	//	 ---------------------------------------------------------------------------------------------
	
	public String toString()
    {
        StringBuffer buffer = new StringBuffer();

        buffer.append("Name = " + _name);
        
        buffer.append(" Original Case Table name = " + _originalCaseTableName + "\n");
        buffer.append(" Mixed Case Table name = " + _name  + "\n");
        buffer.append(" Is View? = " + _isView + "\n");

        buffer.append("Columns:\n");
        for (int i = 0; i < _columnDescList.size(); i++)
        {
            ColumnDescriptor descriptor = (ColumnDescriptor) _columnDescList
                    .get(i);

            buffer.append("Column " + i + "\n" +
                    descriptor.toString()  + "\n");
            
        }


        return buffer.toString();
    }
  
	//	 ---------------------------------------------------------------------------------------------
	
} //end class TableDescriptor
