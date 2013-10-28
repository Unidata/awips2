package ohd.hseb.util.gui.jtable;

import java.util.Comparator;

public class JTableColumnComparator implements Comparator
{
		protected String _columnName = null;
		protected boolean _isAscending;
		
		public int compare(Object obj1, Object obj2)
		{
			JTableRowData rowData1, rowData2;
			
			rowData1 = (JTableRowData) obj1;
			rowData2 = (JTableRowData) obj2;
			int ret = rowData1.compare(_columnName, rowData2);
			if (! _isAscending)
			{
				ret *= -1;
			}
			return ret;
		}

		public void initColumnInfo( String columnName, boolean isAscending )
		{
			_columnName = columnName;
			_isAscending = isAscending;
		}

}


