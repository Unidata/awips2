package ohd.hseb.util.gui.jtable;

import javax.swing.table.DefaultTableModel;

public class JTableModel  extends DefaultTableModel 
{
	private String[] _columnNames = null;

	private Object[][] _data = null;
	private int[] _columnWidths = null;
	private String[] _columnAlignments = null;
	
		
	public JTableModel( String[] colNames, String[][] dataStringArray,
			           int[] columnWidth, String[] columnAlignment)
	{
		_columnNames = colNames;
       _data = dataStringArray;
		_columnWidths = columnWidth;
		_columnAlignments = columnAlignment;
	}
	
	public void setChangedColumnNames(String colNames[])
	{
		_columnNames = colNames;
	}
	
	public void setChangedColumnWidths(int columnWidths[])
	{
		_columnWidths = columnWidths;
	}
	
	public void setChangedColumnAlignments(String columnAlignments[])
	{
		_columnAlignments = columnAlignments;
	}
	
	public void updateData( String[][] dataStringArray )
	{
		_data = dataStringArray;
	}
	
	public int getColumnCount() 
	{
		return _columnNames.length;
	}

	public int getRowCount() 
	{
		if ( _data != null )
		{
			return _data.length;
		}
		else
		{
		    return 0;
		}
	}

	public String getColumnName(int col) 
	{
		return _columnNames[col];
	}
	
	public int getColumnNumber(String columnName)
	{
	  int index = -1;
	  for(int i=0; i < _columnNames.length; i++)
	  {
		  if(_columnNames[i].equals(columnName))
		  {
			  index =i;
			  break;
		  }
	  }
	 return index;
	}
	
	public String getColumnAlignment(String columnName)
	{
		   String alignment = null;
		   int index = -1;
		   for(int i=0; i <_columnNames.length; i++)
		   {
			   if(_columnNames[i].equals(columnName))
			   {
				   index =i;
				   break;
			   }
		   }
		   
		   alignment = _columnAlignments[index];
		   return alignment;
	}
	
	public int getColumnWidth(String columnName)
	{
	   int width = -1;
	   int index = -1;
	   for(int i=0; i <_columnNames.length; i++)
	   {
		   if(_columnNames[i].equals(columnName))
		   {
			   index =i;
			   break;
		   }
	   }
	   
	   width = _columnWidths[index];
	   return width;
	}
	
	public Object getValueAt(int row, int col) 
	{
		return _data[row][col];
	}
		
	public boolean isCellEditable(int row, int col) 
	{
		return false;
	}
}

