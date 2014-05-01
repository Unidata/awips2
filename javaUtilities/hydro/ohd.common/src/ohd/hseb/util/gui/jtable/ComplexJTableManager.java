package ohd.hseb.util.gui.jtable;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.EventListener;
import java.util.List;

import javax.swing.DefaultCellEditor;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;

import ohd.hseb.dbgen.ColumnDescriptor;
import ohd.hseb.util.StringHelper;
import ohd.hseb.util.gui.ItemsSelectionDialog;

public class ComplexJTableManager implements JTableManager
{
    private String _rowDataForCurrentDisplayedRows[][];
    private Color _cellBackgroundColorsForCurrentDisplayedRows[][];
    private Color _cellForegroundColorsForCurrentDisplayedRows[][];
    private List<JTableRowData> _allRowDataList = null;
    private JTableSortHelper _sortHelper;
    private JTableColumnDescriptorManager _columnDescriptorManager;
    private ScrollableJTable _scrollableJTable = null;
    
    private Dimension _columnHeaderPreferredSize = null;

    
    public ComplexJTableManager(List columnDescriptorList, List rowDataList, Dimension columnHeaderPreferredSize)
    {
        
        _columnHeaderPreferredSize = columnHeaderPreferredSize;
        
        _allRowDataList = rowDataList;
        _columnDescriptorManager = new JTableColumnDescriptorManager(
            columnDescriptorList);
        _scrollableJTable = new ScrollableJTable(this, _columnHeaderPreferredSize);
        String columnNamesToDisplay[] = getColumnNamesThatAreCurrentlyDisplayed();
        if(columnNamesToDisplay != null)
        {
            setDisplayableColumns(getColumnNamesThatAreCurrentlyDisplayed(), true, true);
        }
        
    }
    
    public ComplexJTableManager(List columnDescriptorList, List rowDataList)
    {
        this (columnDescriptorList, rowDataList, null);
    }

      
    public ComplexJTableManager(List columnDescriptorList, List rowDataList, int tableRowSelectionMode)
    {
        _allRowDataList = rowDataList;
        _columnDescriptorManager = new JTableColumnDescriptorManager(
            columnDescriptorList);
        _scrollableJTable = new ScrollableJTable(this, tableRowSelectionMode);
    }
    
    public void setColumnHeaderPreferredSize(Dimension columnHeaderPreferredSize)
    {
        _columnHeaderPreferredSize = columnHeaderPreferredSize;
        if (_scrollableJTable != null)
        {
            _scrollableJTable.setColumnHeaderPreferredSize(columnHeaderPreferredSize);
        }
    }

    public void resetSortHelper()
    {
        _sortHelper = null;
    }

    public void setChangedAllRowDataList(List allRowDataList)
    {
        _allRowDataList = allRowDataList;
    }

    public List getAllRowDataList()
    {
        return _allRowDataList;
    }

    public void setColumnsForDisplay(String columnNamesSelected[])
    {
        _columnDescriptorManager.setDisplayableColumns(columnNamesSelected);
    }

    public void setCellEditors(JTable table)
    {
        String[] columnNames = getColumnNamesThatAreCurrentlyDisplayed();
        int columnCount = columnNames.length;
        JTableColumnDescriptorManager mgr = _columnDescriptorManager;
            
        for (int i = 0; i < columnCount ; i++)
        {
            String columnName = columnNames[i];
            JTableColumnDescriptor columnDescriptor = mgr.getColumnDescriptorByName(columnName);
            //for each displayed column, set the cell editor
            //get the cell editor out of the descriptorlist
            
            if (columnDescriptor.isEditable())
            {
                setEditor(table, i, columnDescriptor.getCellEditor());
            }
        }
        
        return;
    }
    
    private void setEditor(JTable table, int columnIndex, TableCellEditor cellEditor)
    {
        //set the CellEditor
        table.getColumnModel().getColumn(columnIndex).setCellEditor(cellEditor);
        
        //set the toggle value to true within JTableModel
        JTableModel model = (JTableModel) table.getModel();
        model.setIsCellColumnEditable(true, columnIndex);
    }
    
    //------------------------------------------------------------------------------
 
    
    public void setDisplayableColumns(String columnNamesSelected[], 
            boolean changeSortOrder,boolean firstDraw)
    {
        _columnDescriptorManager.setDisplayableColumns(columnNamesSelected);

        boolean columnSortOrderArray[] = null;

        if(changeSortOrder)
        {
            columnSortOrderArray = _columnDescriptorManager.getSortOrderOfAllColumns();
        }

        String[] allColumnNamesInColumnDescriptorManager = _columnDescriptorManager.getAllColumnNamesInColumnDescriptorManager(); 
        _sortHelper = new JTableSortHelper(allColumnNamesInColumnDescriptorManager); 
        if(columnSortOrderArray != null)
        {
            _sortHelper.setChangedSortOrderArray(columnSortOrderArray);
        }
        
        _scrollableJTable.createTable(firstDraw);
    }

    public String[] getColumnNamesThatAreCurrentlyDisplayed()
    {
        String columnNames[] = _columnDescriptorManager.
        getColumnNamesThatAreCurrentlyDisplayed();
        return columnNames;
    }

    public void swapColumns(String columnName1, String columnName2)
    {
        _columnDescriptorManager.swapColumnsInList(columnName1, columnName2);
        
    }

    public String[][] getDisplayRowDataArray()
    {
        //   CodeTimer timer = new CodeTimer();
        // timer.start();
        List columnDescriptorList = _columnDescriptorManager.getColumnDescriptorList();
        String colNamesSelected[] = _columnDescriptorManager.getColumnNamesThatAreCurrentlyDisplayed();

        _rowDataForCurrentDisplayedRows = new String[_allRowDataList.size()]
                                                     [colNamesSelected.length];
        _cellBackgroundColorsForCurrentDisplayedRows = new Color[_allRowDataList.size()]
                                                                 [colNamesSelected.length];
        _cellForegroundColorsForCurrentDisplayedRows = new Color[_allRowDataList.size()]
                                                                 [colNamesSelected.length];
        int col = 0;

        JTableColumnDescriptor columnDescription = new JTableColumnDescriptor();
        for(int row=0; row < _allRowDataList.size(); row++)
        {
            JTableRowData infoRecord = (JTableRowData) _allRowDataList.get(row);
            col = 0;
            for(int j =0; j <columnDescriptorList.size(); j++)
            {
                columnDescription = (JTableColumnDescriptor) columnDescriptorList.get(j);
                if(columnDescription.getDisplay() == true)
                {
                    String colName = columnDescription.getColumnName();
                    _cellBackgroundColorsForCurrentDisplayedRows[row][col] = infoRecord.getCellBackgroundColor(colName);
                    _cellForegroundColorsForCurrentDisplayedRows[row][col] = infoRecord.getCellForegroundColor(colName);
                    _rowDataForCurrentDisplayedRows[row][col] = infoRecord.getDataValue(colName);
                    col++;
                }
            }
        }
        //   timer.stop("Time elapsed in getDisplayRowDataArray():");
        return _rowDataForCurrentDisplayedRows;
    }

    public void sort(boolean isAUserClick)
    {
        // do sorting on columns that need to be sort.....
        // hope this is called when we do a refresh (when the applicaiton is running)
        // and also when the application is loaded from the settings file
        String columnNamesForSortingToBeDone[] = getColumnNamesForSortToBeDone();
        if(columnNamesForSortingToBeDone != null)
        {
            for(int i=columnNamesForSortingToBeDone.length-1; i >=0; i--)
            {
                boolean isAscending = _columnDescriptorManager.getSortDirectionForColumn(columnNamesForSortingToBeDone[i]);
                //      System.out.println("ComplexJTableManager.sort(): "+ columnNamesForSortingToBeDone[i]+"|"+ isAscending);
                sort(columnNamesForSortingToBeDone[i], isAUserClick, isAscending);
            }   
        }
    }

    public void sort(String columnName, boolean isAUserClick, boolean isAscending) 
    {
      //  System.out.println("ComplexJTableManager.sort(,,):" + columnName);
        _sortHelper.sortList(_allRowDataList, columnName, isAUserClick, isAscending); 
    }

    public boolean isColumnAsc(String columnName)
    {
        return _sortHelper.isColumnAsc(columnName);
    }

    public JTableRowData getRowData(int rowIndex)
    {
        JTableRowData rowData = (JTableRowData) _allRowDataList.get(rowIndex);
        return rowData;
    }

    public Color getCellBackgroundColor(int row, int column) 
    {
        Color newBackgroundColor = _cellBackgroundColorsForCurrentDisplayedRows[row][column];
        return newBackgroundColor;
    }

    public Color getCellForegroundColor(int row, int column) 
    {
        Color newForegroundColor = _cellForegroundColorsForCurrentDisplayedRows[row][column];
        return newForegroundColor;
    }

    public void setSortInfoNotBasedOnUserClicks(String columnNamesToSort[], boolean columnSortOrder[], int columnSortNumber[])
    {
        _columnDescriptorManager.setSortInfoNotBasedOnUserClicks(columnNamesToSort, columnSortOrder, columnSortNumber);
        _scrollableJTable.sort();
    }
    
    public void clearSort()
    {
        _columnDescriptorManager.clearSort();
        _scrollableJTable.sort();
    }

    public String[] getColumnNamesForSortToBeDone()
    {
        String columnNamesForSorting[] = _columnDescriptorManager.getColumnNamesForSortToBeDone();

        return columnNamesForSorting;
    }

    public int[] getSortOrderForSortedColumnNames()
    {
        int sortOrderForSortedColumns[] = _columnDescriptorManager.getSortOrderForSortedColumnNames();

        return sortOrderForSortedColumns;
    }

    public int[] getWidthOfDisplayedColumns()
    {
        int columnWidths[] = _columnDescriptorManager.getWidthOfDisplayedColumns();
        return columnWidths;
    }

    public String[] getAlignmentOfDisplayedColumns()
    {
        String columnAligments[] = _columnDescriptorManager.getAlignmentOfDisplayedColumns();
        return columnAligments;
    }
    
    public boolean[] getIsEditableArray()
    {
        boolean[] isEditableArray = _columnDescriptorManager.getIsEditableArray();
        return isEditableArray;
    }

    public void setWidthOfDisplayedColumns(int columnWidths[])
    {
        _columnDescriptorManager.setWidthOfDisplayedColumns(columnWidths);
    }

    public void setAlignmentOfDisplayedColumns(String columnAlignments[])
    {
        _columnDescriptorManager.setAlignmentOfDisplayedColumns(columnAlignments);
    }

    public void selectColumnForSorting(String columnName)
    {
        boolean isAscending = _columnDescriptorManager.getSortDirectionForColumn(columnName);
        _columnDescriptorManager.selectColumnForSorting(columnName, isAscending, -1, true); 
    }

    public JTableColumnDisplaySettings getTableSettings() 
    {
        JTableColumnDisplaySettings columnDisplaySettings = _columnDescriptorManager.getTableColumnSettings();
        return columnDisplaySettings;
    }

    public void addTableListener(EventListener eventListener)
    {
        _scrollableJTable.addTableListeners(eventListener);
    }

    public void removeTableListener(EventListener eventListener)
    {
        _scrollableJTable.removeTableListener(eventListener);	
    }

    public JScrollPane getJScrollPane()
    {
        return(_scrollableJTable.getJScrollPane());
    }

    public void setPreferredSizeForJScrollPane(Dimension dimension) 
    {
        _scrollableJTable.setPreferredSizeForJScrollPane(dimension);	
    }

    public int[] getSelectedRowIndices()
    {
        int selectedRowIndices[] =_scrollableJTable.getSelectedRowIndices(); 

        return selectedRowIndices;
    }

    public List getSelectedRowsData()
    {
        int selectedRowIndices[] = getSelectedRowIndices();
        List selectedRowDataList = new ArrayList();
        for(int loopCnt=0; loopCnt<selectedRowIndices.length; loopCnt++)
        {
            int selectedRowIndex = selectedRowIndices[loopCnt];
            for(int row=0; row < _allRowDataList.size(); row++)
            {
                if(row == selectedRowIndex)
                {
                    JTableRowData infoRecord = (JTableRowData) _allRowDataList.get(row);
                    selectedRowDataList.add(infoRecord);
                }
            }
        }
        return selectedRowDataList;
    }

    public int getMousePointedRowIndex(Point p) 
    {
        int rowIndex = _scrollableJTable.getMousePointedRowIndex(p);
        return rowIndex;
    }

    public int getMousePointedColumnIndex(Point p) 
    {
        int columnIndex = _scrollableJTable.getMousePointedColumnIndex(p);
        return columnIndex;
    }

    public String getColumnNameAtIndex(int columnIndex) 
    {
        String columnName = _scrollableJTable.getColumnNameAtIndex(columnIndex);
        return columnName;
    }

    public JTableRowData getSelectedRowData(int rowIndex) 
    {
        JTableRowData selectedRowData = null;
        for(int row=0; row < _allRowDataList.size(); row++)
        {
            if(row == rowIndex)
            {
                JTableRowData infoRecord = (JTableRowData) _allRowDataList.get(row);
                selectedRowData = infoRecord;
                break;
            }
        }

        return selectedRowData;
    }

    public void setRowToolTipText(String str) 
    {
        _scrollableJTable.setRowToolTip(str);	
    }

    public void refreshDisplay() 
    {
        String colNamesSelected[] = getColumnNamesThatAreCurrentlyDisplayed();
        Dimension size = _scrollableJTable.getSizeForJScrollPane();
 
        _scrollableJTable.setPreferredSizeForJScrollPane(size);
        setDisplayableColumns(colNamesSelected, true, false);
    }		

    public Dimension getSizeForJScrollPane()
    {
        return _scrollableJTable.getSizeForJScrollPane();
    }

    public String[] getAllowedColumnNamesArray()
    {
        String[] allowedColNames = _columnDescriptorManager.getAllowedColumnNamesArray();
        return allowedColNames; 
    }

    public int getTheRowIndexToHighLightBasedOnValue(String uniqueValueForRow, String columnName)
    {
        int rowToHighlight =-1;
        int rowCount = 0;
        for(JTableRowData rowData: _allRowDataList)
        {
            String value = rowData.getDataValue(columnName);
            if(value.equals(uniqueValueForRow))
            {
                rowToHighlight = rowCount;
                break;
            }
            rowCount ++;
        }
        return rowToHighlight;
    }

    public int getTheRowIndexToHighLightBasedOnValue(String uniqueValueForRow)
    {
        int col = 0;
        List columnDescriptorList = _columnDescriptorManager.getColumnDescriptorList();

        int rowToHighlight =-1;
        boolean rowFound = false;
        int rowCount = 0;
        for(JTableRowData rowData: _allRowDataList)
        {
            col = 0;
            JTableColumnDescriptor columnDescription = null;
            for(int j =0; j <columnDescriptorList.size(); j++)
            {
                columnDescription = (JTableColumnDescriptor) columnDescriptorList.get(j);
                if(columnDescription.getDisplay() == true)
                {
                    String colName = columnDescription.getColumnName();
                    String value = rowData.getDataValue(colName);
                    if(value.equals(uniqueValueForRow))
                    {
                        rowToHighlight = rowCount;
                        rowFound = true;
                        break;
                    }
                    col++;
                }
            }
            if(rowFound)
            {
                break;
            }
            rowCount ++;
        }
        return rowToHighlight;
    }

    public void selectRows(int initialRowToHighLight, int finalRowToHighLight)
    {
        _scrollableJTable.selectRows(initialRowToHighLight, finalRowToHighLight);			
    }

    public void selectRows(int rowIndices[])
    {
        _scrollableJTable.selectRows(rowIndices);	
    }

    public void deselectRows(int[] rowIndices) 
    {
        _scrollableJTable.deselectRows(rowIndices);
    }

    public void deselectRows(int initialRowToRemoveSelection, int finalRowToRemoveSelection) 
    {
        _scrollableJTable.deselectRows(initialRowToRemoveSelection, finalRowToRemoveSelection);		
    }

    public void setVerifyInputWhenFocusTarget(boolean verify)
    {
        _scrollableJTable.setVerifyInputWhenFocusTarget(verify);	
    }

    public String getAlignment(String columnName)
    {
        List columnDescriptorList = _columnDescriptorManager.getColumnDescriptorList();
        String align = null;

        JTableColumnDescriptor columnDescription = null;
        for(int j =0; j <columnDescriptorList.size(); j++)
        {
            columnDescription = (JTableColumnDescriptor) columnDescriptorList.get(j);
            String name = columnDescription.getColumnName();
            if(name.equals(columnName))
            {
                align = columnDescription.getAlignment();
                break;
            }
        }
        return align;
    }

    public String getColumnHeaderToolTipText(String columnName)
    {
        List columnDescriptorList = _columnDescriptorManager.getColumnDescriptorList();
        String toolTip = null;
        JTableColumnDescriptor columnDescription = null;

        for(int j =0; j <columnDescriptorList.size(); j++)
        {
            columnDescription = (JTableColumnDescriptor) columnDescriptorList.get(j);
            String name = columnDescription.getColumnName();
            if(name.equals(columnName))
            {
                toolTip = columnDescription.getToolTipText();
                break;
            }
        }
        return toolTip;

    }

    public void invokeColumnSelector(JFrame frame)
    {
        String colNamesAllowed[] = getAllowedColumnNamesArray();

        String currentlyDisplayedColumns[] = getColumnNamesThatAreCurrentlyDisplayed();

        ItemsSelectionDialog columnSelectionDialog = new 
        ItemsSelectionDialog(frame, "Column Selection", colNamesAllowed,
            currentlyDisplayedColumns);

        String colNamesSelected[] = columnSelectionDialog.getSelectedItems();

        if(colNamesSelected == null)
        {
            JOptionPane.showMessageDialog(frame, "No columns are selected","Application",JOptionPane.PLAIN_MESSAGE);
        }
        else
        {
            if ( StringHelper.areArraysEqual(colNamesSelected, currentlyDisplayedColumns) != 0)
            {
                resetSortHelper();
                setDisplayableColumns(colNamesSelected, true, false);
            }
        }

    }
    
    public void setTableCellEditor(Class<?> columnClass, DefaultCellEditor defaultCellEditor)
    {
        _scrollableJTable.setTableCellEditor(columnClass, defaultCellEditor);
    }

}
