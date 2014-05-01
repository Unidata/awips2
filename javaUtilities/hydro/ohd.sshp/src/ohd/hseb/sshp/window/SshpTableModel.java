/*
 * Created on Feb 12, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Feb 12, 2004
 *  
 */

package ohd.hseb.sshp.window;

import javax.swing.table.DefaultTableModel;

public class SshpTableModel extends DefaultTableModel 
{
	private String[] _columnNames = null;

	private Object[][] _data = null;
		
	public SshpTableModel( String[] colNames, String[][] dataStringArray )
	{
		_columnNames = colNames;
		_data = dataStringArray;
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

	public Object getValueAt(int row, int col) 
	{
		return _data[row][col];
	}
		
	public boolean isCellEditable(int row, int col) 
	{
		return false;
	}
}
