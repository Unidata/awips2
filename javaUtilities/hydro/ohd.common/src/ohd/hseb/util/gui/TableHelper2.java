/*
 * Created on June 14, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : June 14, 2004
 *  
 */
package ohd.hseb.util.gui;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;


public class TableHelper2
{
	private String[] _columnNameArray = null;
	private LinkedList _sortColumnOrderList = new LinkedList();
	private boolean[] _sortOrderArray = null;
	private int _columnCount = 0;
	private TableColumnComparator _listComparator = null;
	private int _lastSortedColumn = 0;
	
	
	public TableHelper2( TableColumnComparator listComparator, String[] columnNameArray )
	{
		_columnNameArray = columnNameArray;
		_columnCount = _columnNameArray.length;
		_sortOrderArray = new boolean[ _columnCount ];
		_listComparator = listComparator;
		initialize();
	}
	
	private void initialize()
	{
		initLinkedListAndOrderArray();
	}

	public void sortList( List listToSort, int column, boolean flipOrder )
	{
		String columnName = _columnNameArray[ column ];
		boolean sortAscOrder = _sortOrderArray[ column ];
		
		_listComparator.initColumnInfo( columnName, sortAscOrder );
		Collections.sort( listToSort, _listComparator );
		
		if (flipOrder)
		{
		    _sortOrderArray[ column ] = ! _sortOrderArray[ column ];
		}
		
		_lastSortedColumn = column;
	}
	
	
	public void sortWithoutMemory( List listToSort, int column, boolean isAscendingOrder )
	{
		String columnName = _columnNameArray[ column ];
		boolean sortAscOrder = isAscendingOrder;
		
		_listComparator.initColumnInfo( columnName, sortAscOrder );
		Collections.sort( listToSort, _listComparator );
	}
	
	/*
	public void resortList( List listToSort, int column )
	{
		_sortOrderArray[ column ] = true;
		sortList( listToSort, column );
		sortList( listToSort, column );
	}
	*/
	
	public void resortList( List listToSort)
	{
		sortList( listToSort, _lastSortedColumn, false);
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
//		TableHelper2 tbhelper = new TableHelper2( columnNames );
	}
}
