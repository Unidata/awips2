/*
 * Created on Feb 24, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Feb 24, 2004
 *  
 */
package ohd.hseb.util.gui;

import java.util.LinkedList;

public class TableHelper
{
	private String[] _dbColumnNameArray = null;
	private LinkedList _sortColumnOrderList = new LinkedList();
	private boolean[] _sortOrderArray = null;
	private int _columnCount = 0;
	

	public TableHelper( String[] dbColumnNameArray )
	{
		_dbColumnNameArray = dbColumnNameArray;
		_columnCount = _dbColumnNameArray.length;
		_sortOrderArray = new boolean[ _columnCount ];
		initialize();
	}
	
	private void initialize()
	{
		initLinkedListAndOrderArray();
	}

	public String getOrderByClause()
	{
		StringBuffer clauseBuffer = new StringBuffer();
		
		clauseBuffer.append( " ORDER BY" );
		
		for( int i = 0; i < _columnCount; i++ )
		{
			String orderString = null;

			int column = Integer.parseInt( _sortColumnOrderList.get( i ).toString() );

			if ( _sortOrderArray[ column ] == true )
			{
				orderString = " asc";
			}
			else
			{
				orderString = " desc";
			}
			clauseBuffer.append( " " + _dbColumnNameArray[ column ] + orderString + "," );
		}
		clauseBuffer.replace( clauseBuffer.length() - 1, clauseBuffer.length(), " " );  /* Replaces the last comma
																						   with a space */
		return clauseBuffer.toString();
	}
	
	public void setOrderByClause( int column )
	{
		// If true, sets it false and vice versa
		setColumnOrderList( column );
//		System.out.println( _sortColumnOrderList );
		_sortOrderArray[ column ] = !( _sortOrderArray[ column ] );
	}
	
	private void setColumnOrderList( int column )
	{
		int index = -1;
		
		for( int i = 0; i < _sortColumnOrderList.size(); i++ )
		{
			Integer valueInteger = (Integer) _sortColumnOrderList.get( i );
			int value = valueInteger.intValue();
			if ( column == value )
			{
				index = i;
			}
		}
		if ( index != -1 )
		{
			Object columnValueObject = _sortColumnOrderList.remove( index );
			_sortColumnOrderList.addFirst( columnValueObject );
		}
	}
	
	private void initLinkedListAndOrderArray()
	{
		for ( int i = 0; i < _columnCount; i++ )
		{
			_sortOrderArray[ i ] = false;
			_sortColumnOrderList.add( i, new Integer( i ) );
		}
	}

	public static void main( String args[] )
	{
		String[] columnNames = { "Basin ID", "PE", "DUR", "TS", "Extremum", "Adjustment" };
		TableHelper tbhelper = new TableHelper( columnNames );
		System.out.println( tbhelper.getOrderByClause() );
		tbhelper.setOrderByClause( 4 );
		System.out.println( tbhelper.getOrderByClause() );
		tbhelper.setOrderByClause( 2 );
		System.out.println( tbhelper.getOrderByClause() );
	}
}
