package ohd.hseb.util.gui.jtable;

import java.awt.event.*;
import java.util.ArrayList;
import java.util.EventListener;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.awt.*;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableColumn;

/**
 * @author RajaramV
 *
 */

public class ScrollableJTable 
{
    private JTable _table;
    private JTableManager _tableManager;
    private JTableModel _tableModel;
    private JTableCellRenderer _cellRenderer = null;
    private JScrollPane _tableScrollPane = null;
    private boolean _sortFlag = false;
    private boolean _moveFlag = false;
    private boolean _firstDraw = false;

    private Dimension _columnHeaderPreferredSize = null;

    private Set _listenerSet = new HashSet();
    private int _tableRowSelectionMode = ListSelectionModel.MULTIPLE_INTERVAL_SELECTION;

    /*
    public ScrollableJTable(JTableManager tableManager)
    {
        _tableManager = tableManager;
        _cellRenderer = new JTableCellRenderer(); 
        _cellRenderer.setTableManager(_tableManager);
    }
   */
    
    public ScrollableJTable(JTableManager tableManager, Dimension columnHeaderPreferredSize)
    {
        _columnHeaderPreferredSize = columnHeaderPreferredSize;
        
        _tableManager = tableManager;
        _cellRenderer = new JTableCellRenderer(); 
        _cellRenderer.setTableManager(_tableManager);
    }
    
    public ScrollableJTable(JTableManager tableManager, int tableRowSelectionMode)
    {
        _tableManager = tableManager;
        _cellRenderer = new JTableCellRenderer(); 
        _cellRenderer.setTableManager(_tableManager);
        _tableRowSelectionMode = tableRowSelectionMode;
    }


    public JScrollPane getJScrollPane()
    {
        return _tableScrollPane;
    }

    protected void setPreferredSizeForJScrollPane(Dimension dimension)
    {
        _tableScrollPane.setPreferredSize(dimension);
    }

    protected Dimension getSizeForJScrollPane()
    {
        return _tableScrollPane.getSize();
    }

    protected void createTable(boolean firstDraw)
    {
        _firstDraw = firstDraw;
        _tableModel = null;
        Container scrollPaneContainer = null;
        JScrollPane oldTableScrollPane = _tableScrollPane;
        Dimension oldPreferredSizeForTableScrollPane = null;
        Point oldPosition = null;
        if(_tableScrollPane != null)
        {
            oldPreferredSizeForTableScrollPane = _tableScrollPane.getPreferredSize();
            oldPosition = _tableScrollPane.getViewport().getViewPosition();
        }

        String columnNamesToBeDisplayed[] = _tableManager.getColumnNamesThatAreCurrentlyDisplayed();

        String rowData[][] = _tableManager.getDisplayRowDataArray();

        int widthOfColumnsDisplayed[] = _tableManager.getWidthOfDisplayedColumns();

        String[] alignmentOfColumnsDisplayed = _tableManager.getAlignmentOfDisplayedColumns();

        _tableModel = new JTableModel(columnNamesToBeDisplayed, rowData,
            widthOfColumnsDisplayed, alignmentOfColumnsDisplayed);

        _table = new JTable(_tableModel);
        _table.setSelectionMode(_tableRowSelectionMode);
        
        // make the user's edits stored without need for type "return" key or click
        //on JTable
        _table.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);


        
        int cnt=0;
        Iterator it = _listenerSet.iterator();
        while(it.hasNext() && cnt++ < _listenerSet.size())
        {
            addTableListeners((EventListener)it.next());
        }

        _table.setOpaque(false);
        _table.setDefaultRenderer(Object.class, _cellRenderer);

        _table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        setColumnListeners();
        
        _tableManager.setCellEditors(_table);

        _tableScrollPane = new JScrollPane(_table);

        if(oldTableScrollPane !=null)
        {
            scrollPaneContainer = oldTableScrollPane.getParent();
        }

        if (scrollPaneContainer != null)
        {
            scrollPaneContainer.setVisible(false);
            scrollPaneContainer.remove(oldTableScrollPane);
            _tableScrollPane.setPreferredSize(oldPreferredSizeForTableScrollPane);
            _tableScrollPane.getViewport().setViewPosition(oldPosition);
            scrollPaneContainer.add(_tableScrollPane);
            scrollPaneContainer.setVisible(true);
        }

        sort();

        for(int i=0; i < columnNamesToBeDisplayed.length; i++)
        {
            setWidth(columnNamesToBeDisplayed[i], widthOfColumnsDisplayed[i]);
        }
    }
    //------------------------------------------------------------------------------
      protected void sort()
    {
        _tableManager.sort(true);
        refreshAfterSort();
    }
    //------------------------------------------------------------------------------

    protected void 	setVerifyInputWhenFocusTarget(boolean verify)
    {
        _table.setVerifyInputWhenFocusTarget(verify);
    }

    //------------------------------------------------------------------------------   
    /**
     * Select the rows that are passed in as argument and 
     * set the view port to the first selected row if setViewPort is true 
     */
    protected void selectRows(int rowIndices[], boolean setViewPort)
    {
        Color highLightBackGroundColor = Color.BLUE;
        Color highLightForeGroundColor = Color.YELLOW;
        for(int i=0; i < rowIndices.length; i++)
        {
            _table.getSelectionModel().addSelectionInterval(rowIndices[i], rowIndices[i]);
            _table.setSelectionBackground(highLightBackGroundColor);
            _table.setSelectionForeground(highLightForeGroundColor);
            if(setViewPort)
            {
                Rectangle rect = _table.getCellRect(rowIndices[0], 0, true);
                _tableScrollPane.getViewport().setViewPosition(rect.getLocation());
            }
        }
    }

    //------------------------------------------------------------------------------
    /**
     * Select the rows that are passed in as argument and set the view port to the first selected row
     */
    protected void selectRows(int rowIndices[])
    {
        selectRows(rowIndices, true);
    }

    //------------------------------------------------------------------------------
    /**
     * Deselect the rows that are passed in as argument 
     */
    protected void deselectRows(int rowIndices[])
    {
        for(int i=0; i < rowIndices.length; i++)
        {
            _table.getSelectionModel().removeSelectionInterval(rowIndices[i], rowIndices[i]);
        }
    }

    //------------------------------------------------------------------------------
    /**
     * Deselect the previously selected rows 
     */
    protected void deselectRows(int initialRowToRemoveSelection, int finalRowToRemoveSelection)
    {
        _table.getSelectionModel().removeSelectionInterval(initialRowToRemoveSelection, finalRowToRemoveSelection);
    }

    //------------------------------------------------------------------------------
    /**
     * Select the rows from initialRowToSelect till finalRowToSelect 
     * that are passed in as argument and set the view port to the first selected row
     */
    protected void selectRows(int initialRowToSelect, int finalRowToSelect)
    {
        selectRows( initialRowToSelect,  finalRowToSelect,  true);
    }

    //------------------------------------------------------------------------------
    /**
     * Select the rows from initialRowToSelect till finalRowToSelect 
     * that are passed in as argument and set the view port to the first selected row if setViewPort is true
     */
    protected void selectRows(int initialRowToSelect, int finalRowToSelect, boolean setViewPort)
    {
        Color highLightBackGroundColor = Color.BLUE;
        Color highLightForeGroundColor = Color.YELLOW;
        _table.setRowSelectionInterval(initialRowToSelect, finalRowToSelect);
        _table.setSelectionBackground(highLightBackGroundColor);
        _table.setSelectionForeground(highLightForeGroundColor);
        if(setViewPort)
        {
            Rectangle rect = _table.getCellRect(initialRowToSelect, 0, true);
            _tableScrollPane.getViewport().setViewPosition(rect.getLocation());
        }
    }
   //------------------------------------------------------------------------------
   private void setColumnWidths()
    {
       //String header = "ScrollableJTable.setColumnWidths(); ";
       //System.out.println(header + "called." );
       
        if(_table!=null)
        {
            int numOfCols = _table.getColumnCount();
            for(int i=0; i < numOfCols; i++)
            {
                String columnName = _tableModel.getColumnName(i);
                int columnWidth = _tableModel.getColumnWidth(columnName);
                int columnIndex = _tableModel.getColumnNumber(columnName);

                TableColumn column = _table.getColumnModel().getColumn(columnIndex);
                int prevWidth = column.getWidth();
                if(prevWidth != columnWidth)
                {
                    column.setWidth(columnWidth);
                }
            }
        }
    }

    private void setWidth(String columnName, int columnWidth)
    {
        int columnIndex = _tableModel.getColumnNumber(columnName);
        TableColumn column = _table.getColumnModel().getColumn(columnIndex);
        _table.getColumnModel().getColumn(columnIndex).setHeaderRenderer(new JTableHeaderRenderer(_tableManager, _columnHeaderPreferredSize));
        column.setWidth(columnWidth);
        column.setPreferredWidth(columnWidth);
    }
    
    public void setColumnHeaderPreferredSize(Dimension columnHeaderPreferredSize)
    {
        _columnHeaderPreferredSize = columnHeaderPreferredSize;
    }

    protected void addTableListeners(EventListener eventListener)
    {
        if(eventListener != null)
        {
            _listenerSet.add(eventListener);
            Class[] classArray = eventListener.getClass().getInterfaces();

            for(int i=0; i < classArray.length; i++)
            {
                String splitStr[] = classArray[i].toString().split(" ");
                String className = splitStr[1];
                if(className.equals("javax.swing.event.ListSelectionListener"))
                {
                    _table.getSelectionModel().addListSelectionListener((ListSelectionListener)eventListener);
                }
                else if(className.equals("java.awt.event.MouseListener"))
                {
                    _table.addMouseListener((MouseListener)eventListener);
                }
                else if(className.equals("java.awt.event.MouseMotionListener"))
                {
                    _table.addMouseMotionListener((MouseMotionListener)eventListener);
                }
                else if(className.equals("javax.swing.event.TableModelListener"))
                {
                    _tableModel.addTableModelListener((TableModelListener)eventListener);
                }
                else
                {
                    System.out.println ("I don't know what " + className + " is.");
                }
            }
        }
    }

    protected void removeTableListener(EventListener eventListener)
    {
        if(eventListener != null)
        {
            Class[] classArray = eventListener.getClass().getInterfaces();
            for(int i=0; i < classArray.length; i++)
            {
                String splitStr[] = classArray[i].toString().split(" ");
                String className = splitStr[1];
                if(className.equals("javax.swing.event.ListSelectionListener"))
                {
                    _table.getSelectionModel().removeListSelectionListener((ListSelectionListener)eventListener);
                }
                else if(className.equals("java.awt.event.MouseListener"))
                {
                    _table.removeMouseListener((MouseListener)eventListener);
                }
                else if(className.equals("java.awt.event.MouseMotionListener"))
                {
                    _table.removeMouseMotionListener((MouseMotionListener)eventListener);
                }
            }
        }
    }

    protected int[] getSelectedRowIndices()
    {
        java.util.List rowsSelectedList = new ArrayList();
        ListSelectionModel jtableListSelectionModel = _table.getSelectionModel();
        int firstIndex = jtableListSelectionModel.getMinSelectionIndex();
        int lastIndex = jtableListSelectionModel.getMaxSelectionIndex();
        for(int i=firstIndex ; i<=lastIndex ; i++)
        {
            if(jtableListSelectionModel.isSelectedIndex(i))
            {
                rowsSelectedList.add(new Integer(i).toString());
            }
        }
        int rowsSelected[] = new int[rowsSelectedList.size()];
        for(int i=0; i <rowsSelected.length; i++)
        {
            rowsSelected[i]= Integer.parseInt(rowsSelectedList.get(i).toString());
        }
        return rowsSelected;
    }

    protected int getMousePointedRowIndex(Point p)
    {
        int rowIndex = -1;
        if(p != null)
        {
            rowIndex = _table.rowAtPoint(p);
        }
        return rowIndex;
    }

    protected int getMousePointedColumnIndex(Point p)
    {
        int columnIndex = -1;
        if(p != null)
        {
            columnIndex = _table.columnAtPoint(p);
        }
        return columnIndex;
    }

    protected String getColumnNameAtIndex(int columnIndex)
    {
        String columnName = null;
        if(columnIndex != -1)
        {
            columnName = _table.getColumnName(columnIndex);
        }
        return columnName;
    }

    protected void setRowToolTip(String str)
    {
        _table.setToolTipText(str);	
    }

    private void setColumnListeners()
    {
        TableColumnHeaderMouseListener columnHeaderListener = new TableColumnHeaderMouseListener();
        _table.getTableHeader().addMouseListener(columnHeaderListener);

        TableColumnChangeListener columnChangeListener = new TableColumnChangeListener();
        _table.getColumnModel().addColumnModelListener(columnChangeListener);

        TableColumnHeaderMouseMotionListener columnHeaderMouseMotionListener = new TableColumnHeaderMouseMotionListener();
        _table.getTableHeader().addMouseMotionListener(columnHeaderMouseMotionListener);

    }

    private void refresh(String rowData[][])
    {
        _tableModel.updateData( rowData);
        if(_sortFlag)
            _tableModel.fireTableChanged( null );
        else
            _tableModel.fireTableDataChanged();
              
    }

    private void refreshAfterSort()
    {
        String rowData[][] = _tableManager.getDisplayRowDataArray();
        _sortFlag = true;
        refresh(rowData);
        
        setColumnWidths();
    }
    
    /*
     * Added by Bryon L. on May 20, 2008.  This allows the
     * cell editor to be set for the entire table.  Columns which 
     * are editable will use this editor.
     */
    public void setTableCellEditor(Class<?> columnClass, DefaultCellEditor defaultCellEditor)
    {
        _table.setDefaultEditor(columnClass, defaultCellEditor);
    }

    class TableColumnHeaderMouseMotionListener extends MouseMotionAdapter
    {
        TableColumn _currentColumn; 
        public void mouseMoved(MouseEvent e) 
        {
            int columnIndexPointedByMouse = _table.getColumnModel().getColumnIndexAtX(e.getX());
            if(columnIndexPointedByMouse != -1)
            {
                String columnNamePointedByMouse = _table.getColumnName(columnIndexPointedByMouse); 
                String headerToolTip = _tableManager.getColumnHeaderToolTipText(columnNamePointedByMouse);
                TableColumn pointedColumn = _table.getColumnModel().getColumn(columnIndexPointedByMouse);

                if(_currentColumn != pointedColumn)
                {
                    if(headerToolTip != null)
                    {
                        _table.getTableHeader().setToolTipText(headerToolTip);
                    }
                    _currentColumn = pointedColumn;
                }
            }
        }
    }

    class TableColumnHeaderMouseListener extends MouseAdapter 
    {
        public void mouseClicked(MouseEvent e) 
        {
            Point point = e.getPoint();

            int column;
            String columnNames[] = _tableManager.getColumnNamesThatAreCurrentlyDisplayed();
            _tableModel.setChangedColumnNames(columnNames);

            int columnWidths[] = _tableManager.getWidthOfDisplayedColumns();
            _tableModel.setChangedColumnWidths(columnWidths);

            String columnAlignments[] = _tableManager.getAlignmentOfDisplayedColumns();
            _tableModel.setChangedColumnAlignments(columnAlignments);

            column = _table.getTableHeader().columnAtPoint( point );

            if(column != -1)
            {
                String columnName = _tableModel.getColumnName(column);

                //System.out.println("Setting column for sorting:"+ columnName);
                _tableManager.selectColumnForSorting(columnName);
                sort();
                // OLD _tableManager.sort(_tableModel.getColumnName(column));


                int columnWidth = _tableModel.getColumnWidth(columnName);
                setWidth(columnName, columnWidth);

                //  _tableManager.setColumnSortInformation(columnName); OLD

                setColumnWidths();
            }
        }

        public void mousePressed(MouseEvent e) 
        {
        }

        public void mouseReleased(MouseEvent e) 
        {
            if(_moveFlag)
            {
                Point point = e.getPoint();
                int column = _table.getTableHeader().columnAtPoint( point );

                String rowData[][] = _tableManager.getDisplayRowDataArray();
                _sortFlag = true;
                refresh(rowData);
                if(column != -1)
                {

                    String columnName = _tableModel.getColumnName(column);
                    int columnWidth = _tableModel.getColumnWidth(columnName);
                    setWidth(columnName, columnWidth);

                    //_tableManager.setColumnSortInformation(columnName);
                }
                    setColumnWidths();
                
            }
        }

    }

    class TableColumnChangeListener implements TableColumnModelListener
    {
        String rowData[][];
        boolean marginChanged = false;

        public void columnMoved(TableColumnModelEvent e) 
        {

            if(e.getFromIndex() != e.getToIndex())
            {
                String columnNames[] = _tableManager.
                getColumnNamesThatAreCurrentlyDisplayed();

                _tableModel.setChangedColumnNames(columnNames);

                String columnName1 = _tableModel.getColumnName(e.getFromIndex());
                String columnName2 = _tableModel.getColumnName(e.getToIndex());

                _tableManager.swapColumns(columnName1, columnName2);

                columnNames = _tableManager.
                getColumnNamesThatAreCurrentlyDisplayed();
                _tableModel.setChangedColumnNames(columnNames);

                int columnWidths[] = _tableManager.getWidthOfDisplayedColumns();
                _tableModel.setChangedColumnWidths(columnWidths);

                String columnAlignments[] = _tableManager.getAlignmentOfDisplayedColumns();
                _tableModel.setChangedColumnAlignments(columnAlignments);
                
                boolean isEditableArray[] = _tableManager.getIsEditableArray();
                _tableModel.setChangedIsEditableArray(isEditableArray);

                _moveFlag = true;
            }
        }

        public void columnMarginChanged(ChangeEvent e) 
        {
            marginChanged = true;
            int numOfCols = _tableModel.getColumnCount();
            int columnWidthsMatchingTheOrderOfColumnNames[] = new int[numOfCols];
            String columnAlignmentsMatchingTheOrderOfColumnNames[] = new String[numOfCols];

            for(int i=0; i < numOfCols; i++)
            {
                String columnName;
                columnName = _tableModel.getColumnName(i);
                int columnWidth;
                String columnAlignment;
                if( _sortFlag || _moveFlag || _firstDraw)
                {
                    columnWidth = _tableModel.getColumnWidth(columnName);
                }
                else
                {
                    columnWidth = _table.getColumnModel().getColumn(i).getWidth();

                }
                columnAlignment = _tableManager.getAlignment(columnName);
                columnWidthsMatchingTheOrderOfColumnNames[i] = columnWidth;
                columnAlignmentsMatchingTheOrderOfColumnNames[i] = columnAlignment;
                setWidth(columnName, columnWidth);	
            }
            _sortFlag = false;
            _moveFlag = false;
            _firstDraw = false;

            _tableModel.setChangedColumnWidths(
                columnWidthsMatchingTheOrderOfColumnNames);
            _tableManager.setWidthOfDisplayedColumns(
                columnWidthsMatchingTheOrderOfColumnNames);

            _tableModel.setChangedColumnAlignments(columnAlignmentsMatchingTheOrderOfColumnNames);
            _tableManager.setAlignmentOfDisplayedColumns(columnAlignmentsMatchingTheOrderOfColumnNames);
        } 

        public void columnSelectionChanged(ListSelectionEvent e) 
        {
            if(marginChanged == true)
            {
                String columnNames[] = _tableManager.
                getColumnNamesThatAreCurrentlyDisplayed();

                _tableModel.setChangedColumnNames(columnNames);

                rowData = _tableManager.getDisplayRowDataArray();
                refresh(rowData);
                marginChanged = false;
            }
        }

        public void columnAdded(TableColumnModelEvent e) 
        {
        }

        public void columnRemoved(TableColumnModelEvent e) 
        {
        }
        
    }
}
