package ohd.hseb.util.gui.jtable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class JTableSortHelper
{
    private String[] _columnNameArray = null;
    private List _sortColumnOrderList = new ArrayList();
    private boolean[] _isSortAscOrDescArray = null;
    private int _columnCount = 0;
    private JTableColumnComparator _columnComparator = new JTableColumnComparator();

    public JTableSortHelper(String[] columnNameArray )
    {
        _columnNameArray = columnNameArray;
        _columnCount = _columnNameArray.length;
        _isSortAscOrDescArray = new boolean[ _columnCount ];
        initialize();
    }

    public void setChangedColumns(String[] columnNameArray)
    {
        _columnNameArray = columnNameArray;
        _columnCount = _columnNameArray.length;
    }

    public void setChangedSortOrderArray(boolean sortOrderArray[])
    {
        _isSortAscOrDescArray = sortOrderArray;	
    }

    public boolean[] getsortOrderArray()
    {
        return _isSortAscOrDescArray;	
    }

    private void initialize()
    {
        initLinkedListAndOrderArray();
    }

    public void sortList( List listToSort, String columnName, boolean isAUserClick , boolean isAscending)
    {
        int index = -1;
        for(int i=0; i < _columnNameArray.length; i++)
        {
            if(_columnNameArray[i].compareTo(columnName) == 0 )
            {
                index = i;
                break;
            }
        }
        _columnComparator.initColumnInfo( columnName, isAscending );

        Collections.sort( listToSort, _columnComparator );

        if (isAUserClick)
        {
            _isSortAscOrDescArray[ index ] = ! isAscending;
        }

    }

    public boolean determineAscOrDescForColumnName(String columnName)
    {
        boolean result;
        int matchIndex = findIndexOfColumnNameInColumnNamesArray(columnName);
        if(matchIndex != -1)
            result = _isSortAscOrDescArray[matchIndex];
        else
            result = false; //default is desc

        return result;
    }

    public boolean isColumnAsc(String columnName)
    {
        boolean result = false;//default is desc
        int matchIndex = findIndexOfColumnNameInColumnNamesArray(columnName);
        if(matchIndex != -1)
            result = _isSortAscOrDescArray[matchIndex];

        return result;
    }

    private int findIndexOfColumnNameInColumnNamesArray(String columnName)
    {
        int matchIndex = -1;
        for(int i=0; i < _columnNameArray.length; i++)
        {
            if(_columnNameArray[i].equals(columnName))
            {
                matchIndex = i;
                break;
            }
        }
        return matchIndex;
    }

    public void sortWithoutMemory( List listToSort, int column, boolean isAscendingOrder )
    {
        String columnName = _columnNameArray[ column ];
        boolean sortAscOrder = isAscendingOrder;

        _columnComparator.initColumnInfo( columnName, sortAscOrder );
        Collections.sort( listToSort, _columnComparator );
    }

    private void initLinkedListAndOrderArray()
    {
        for ( int i = 0; i < _columnCount; i++ )
        {
            _isSortAscOrDescArray[ i ] = true;
            _sortColumnOrderList.add( i, new Integer( i ) );
        }
    }

}
