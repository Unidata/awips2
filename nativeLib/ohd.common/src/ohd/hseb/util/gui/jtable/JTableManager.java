package ohd.hseb.util.gui.jtable;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.util.EventListener;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;

public interface JTableManager
{
	
   void sort(String columnName, boolean isAUserClick, boolean isAscending); 
   
   void sort(boolean isAUserClick);
   
   String[][] getDisplayRowDataArray();
   
   JTableRowData getRowData(int i);
   
   String[] getColumnNamesThatAreCurrentlyDisplayed();
  
   Color getCellBackgroundColor(int row, int column);
   
   Color getCellForegroundColor(int row, int column);
   
   Dimension getSizeForJScrollPane();
   
   void swapColumns(String columnName1, String columnName2);
   
   int[] getWidthOfDisplayedColumns();
   
   boolean[] getIsEditableArray();
   
   String[] getAlignmentOfDisplayedColumns();
   
   void setWidthOfDisplayedColumns(int columnWidths[]);
   
   void setAlignmentOfDisplayedColumns(String columnAlignments[]);
   
   void setColumnsForDisplay(String columnNamesSelected[]);
   
   void setDisplayableColumns(String columnNamesSelected[], 
		                      boolean changeSortOrder, boolean firstDraw);
   
   void selectColumnForSorting(String columnName);
   
   String[] getColumnNamesForSortToBeDone();
   
   public int[] getSortOrderForSortedColumnNames();
   
   public void setCellEditors(JTable table);
   
   void setChangedAllRowDataList(List allRowDataList);
   
   void resetSortHelper();
   
   void setSortInfoNotBasedOnUserClicks(String columnNamesToSort[], boolean columnSortOrder[], int columnSortNumber[]);
   
   void clearSort();
   
   JTableColumnDisplaySettings getTableSettings();
   
   public void addTableListener(EventListener eventListener);
   
   public void removeTableListener(EventListener eventListener);
   
   public JScrollPane getJScrollPane();
   
   public void setPreferredSizeForJScrollPane(Dimension dimension);
   
   public int[] getSelectedRowIndices();
   
   public List getSelectedRowsData();
   
   public int getMousePointedRowIndex(Point p);
   
   public int getMousePointedColumnIndex(Point p);
   
   public String getColumnNameAtIndex(int columnIndex) ;
   
   public JTableRowData getSelectedRowData(int rowIndex);
   
   public void setRowToolTipText(String str);
   
   public void refreshDisplay();
   
   public List getAllRowDataList();
   
   public String[] getAllowedColumnNamesArray();
   
   /**
    * Find the index of the first row containing the value
    * @param uniqueValueForRow
    * @return index 
    */
   public int getTheRowIndexToHighLightBasedOnValue(String uniqueValueForRow);

   /**
    * Find the index of the first row containing the value 
    * in the columnName specified 
    * @param uniqueValueForRow
    * @param columnName
    * @return index
    */
   public int getTheRowIndexToHighLightBasedOnValue(String uniqueValueForRow, String columnName);

   public void selectRows(int initialRowToSelect, int finalRowToSelect);
   
   public void selectRows(int rowIndices[]);
   
   public void deselectRows(int rowIndices[]);
   
   public void deselectRows(int initialRowToRemoveSelection, int finalRowToRemoveSelection);
   
   public void 	setVerifyInputWhenFocusTarget(boolean verify);
   
   public String getAlignment(String columnName);
   
   public String getColumnHeaderToolTipText(String columnName);
   
   public boolean isColumnAsc(String columnName);
   
   public void invokeColumnSelector(JFrame frame);
   
 }
