/*
 * Created on Aug 19, 2004
 *
 * 
 */
package ohd.hseb.sshp.gui;

import java.util.ArrayList;
import java.util.List;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;

import ohd.hseb.sshp.window.SshpTableModel;
import ohd.hseb.util.gui.TableColumnComparator;
import ohd.hseb.util.gui.TableHelper2;

/**
 * @author Chip Gobs
 *
 * The purpose of this class is to wrap-up the usual way we use JTable, with all of its
 * associated classes.
 */
abstract public class TableAdapter
{
    private JTable _table = null;
    private List _dataList = null;
    
    private SshpTableModel _tableModel = null;
    private ListSelectionModel _listSelectionModel = null;
    private JScrollPane _scrollPane = null;
    private TableHelper2 _tableHelper = null;
    
    private String[]   _columnNameArray = null;
    private String[][] _dataStringArray = null;
    
    abstract public  String[][] getDataStringArrayFromDataList(List dataList);
    // --------------------------------------------------------------------------------
    
    //	 --------------------------------------------------------------------------------

    public TableAdapter(TableColumnComparator comparator,
                        String[] columnNameArray,
                        List dataList)
    {   
        setColumnNameArray(columnNameArray); 
        
         String[][] dataStringArray  = getDataStringArrayFromDataList(dataList);
        //setDataStringArray(dataStringArray);
        
        _dataList = dataList;
        
        _tableHelper = new TableHelper2(comparator, _columnNameArray);
        
        _tableModel = new SshpTableModel( _columnNameArray, dataStringArray );
        _table = new JTable(_tableModel);
    	_table.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
    	_table.setPreferredScrollableViewportSize( new Dimension( 300, 100 ) );
		
    	
    	 int index = 0;

    	 Rectangle rect = _table.getCellRect(index, 0, true);
    	 _scrollPane = new JScrollPane(_table);
    	 
    	 // a suggestion I read somewhere, said to try the following instead of
    	 // _scrollPane = new JScrollPane(_table);
     	
    //	 _scrollPane = new JScrollPane();
    //	 _scrollPane.setViewportView(_table);
    //	 _scrollPane.revalidate();
    	 
    	 
      	 _scrollPane.getViewport().setViewPosition(rect.getLocation());
    	   
        _listSelectionModel = _table.getSelectionModel();
        _listSelectionModel.setSelectionInterval( 0, 0 );
    	
         
        addListeners();
    }
    //  --------------------------------------------------------------------------------
    
    
    protected int[] getSelectedRowIndexArray()
    {
         //returns =1 if nothing selected
        List selectedRowIndexList = new ArrayList();
       
        for (int i = 0; i < selectedRowIndexList.size(); i++)
        {
            if (_listSelectionModel.isSelectedIndex(i))
            {
                selectedRowIndexList.add(new Integer(i));
            }
            
        }
        
        int[] indexArray = new int[selectedRowIndexList.size()];
        
        for (int i = 0; i < indexArray.length; i++ )
        {
            Integer integer = (Integer)selectedRowIndexList.get(i);
            indexArray[i] = integer.intValue();
        }
  
        return indexArray;        
    }
 
    public void setAllowMultipleSelection(boolean allowMultipleSelection)
    {
        if (allowMultipleSelection)
        {
            _table.setSelectionMode( ListSelectionModel.MULTIPLE_INTERVAL_SELECTION ); 
        }
        else
        {
            _table.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );   
        }
            
        return;
    }
    // --------------------------------------------------------------------------------
  
    public void setColumnNameArray(String[] columnNameArray)
    {
        _columnNameArray = new String[columnNameArray.length];
        for (int i = 0; i < _columnNameArray.length; i++)
        {
            _columnNameArray[i] = columnNameArray[i];
        }
    }
    // --------------------------------------------------------------------------------
    public String[] getColumnNameArray()
    {
        return _columnNameArray;
    }
     
//  --------------------------------------------------------------------------------
    private void addListeners()
    {
		//_listSelectionModel.addListSelectionListener( new TableUpdateListener() );
		_table.getTableHeader().addMouseListener( new TableSortingListener() ); 
		
        return;
    }
/**
     * @param table The table to set.
     */
    public void setTable(JTable table)
    {
        _table = table;
    }
    /**
     * @return Returns the table.
     */
    public JTable getTable()
    {
        return _table;
    }
    /**
     * @param scrollPane The scrollPane to set.
     */
    private void setScrollPane(JScrollPane scrollPane)
    {
        _scrollPane = scrollPane;
    }
    /**
     * @return Returns the scrollPane.
     */
    public JScrollPane getScrollPane()
    {
        return _scrollPane;
    }
    //  --------------------------------------------------------------------------------
 	public void refreshTableModel(List dataList)
	{
 //	   	int index = 0;
		      
		
//		int[] indexArray = getSelectedRowIndexArray();

        System.out.println("TableAdapter.refreshTableModel(): - should be calling same method in subclass ");
		//_tableModel.updateData( _dataStringArray );
		//_tableModel.fireTableChanged( null );
	
		
		// reselect all the items that 
//		_listSelectionModel.setSelectionInterval()

		//make sure that the item is visible
//		Rectangle rect = _table.getCellRect(index, 0, true);
//		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		
		//load up the data entry fields
//		loadTextFields();

		//redraw the canvas
       //_canvasMgr.refreshCanvas(_uhgList);
     
       return;
	}

	//	 --------------------------------------------------------------------------------

    /**
     * @param dataList The dataList to set.
     */
    public void setDataList(List dataList)
    {
        _dataList = dataList;
    }
 
    /**
     * @return Returns the dataList.
     */
    public List getDataList()
    {
        return _dataList;
    }

    /**
     * @param listSelectionModel The listSelectionModel to set.
     */
    protected void setListSelectionModel(ListSelectionModel listSelectionModel)
    {
        _listSelectionModel = listSelectionModel;
    }
    /**
     * @return Returns the listSelectionModel.
     */
    public ListSelectionModel getListSelectionModel()
    {
        return _listSelectionModel;
    }

    /**
     * @param tableModel The tableModel to set.
     */
    public void setTableModel(SshpTableModel tableModel)
    {
        _tableModel = tableModel;
    }
    /**
     * @return Returns the tableModel.
     */
    public SshpTableModel getTableModel()
    {
        return _tableModel;
    }

    //------------------------------------------------------------------------------
    public int getFirstSelectedIndex()
    {
        int index = -1;
        
        index = _table.getSelectedRow();
        
        return index;
        
    }
    //------------------------------------------------------------------------------
	private class TableSortingListener extends MouseAdapter
	{
		public void mouseClicked( MouseEvent e )
		{
			Point point = e.getPoint();
			int column = _table.getTableHeader().columnAtPoint( point );
			
			_tableHelper.sortList( _dataList, column, true );
			refreshTableModel(_dataList);
		}
	}
//	------------------------------------------------------------------------------
}
