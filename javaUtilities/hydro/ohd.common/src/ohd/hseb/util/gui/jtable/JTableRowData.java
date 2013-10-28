package ohd.hseb.util.gui.jtable;

import java.awt.Color;

public interface JTableRowData 
{
   int compare(String columnName, JTableRowData object);
   
   String getDataValue(String ColumnName);
   
   Color getCellBackgroundColor(String columnName);
   
   Color getCellForegroundColor(String columnName);
   
   void addCell(BaseTableCell cell);
   
   BaseTableCell getCell(String columnName);
}
