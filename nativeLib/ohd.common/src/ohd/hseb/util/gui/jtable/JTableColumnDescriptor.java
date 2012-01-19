package ohd.hseb.util.gui.jtable;

import javax.swing.CellEditor;
import javax.swing.table.TableCellEditor;

/**
 * @author RajaramV
 *
 */
public class JTableColumnDescriptor 
{
	private String _name;
	private boolean _display;
    private static final int _defaultWidth = 65;
	private int _width;
	private boolean _isAscending;
	private int _sortOrder;
	private String _alignment = null;
	private static final String _defaultAlignment = "left";
    private boolean _defaultDisplay = false;
	private String _toolTipText = null; // column header tool tip
	
	
    private boolean _isEditable;
    private TableCellEditor _cellEditor;
	
	
	public JTableColumnDescriptor()
	{

	}

    /**
     * Constructor
     * @param columnName - name of the column
     * @param display - true ----column should be displayed , false ---- column should not be displayed
     * Assumes column's
     * width = 65,  sortOrder = -1, isAscending = false, alignment = left
     */
	public JTableColumnDescriptor(String columnName, 
			boolean display)
	{
		setColumnDetails(columnName, display);
	}
    
    /**
     * Constructor
     * @param columnName- name of the column
     * @param columnWidth- width of the column
     * Assumes column's
     * sortOrder = -1, isAscending = false, alignment = left, display = false
     */
    public JTableColumnDescriptor(String columnName, 
            int columnWidth)
    {
        int colWidth = columnWidth;
        int sortOrder = -1;
        boolean isAscending = false;
        String alignment = _defaultAlignment;
        boolean display = _defaultDisplay;
        setColumnDetails(columnName, colWidth, display, isAscending, sortOrder, alignment);
    }

    /**
     * Constructor
     * @param columnName- name of the column
     * Assumes column's
     * width = 65, , sortOrder = -1, isAscending = false, alignment = left, display = true
     */
	public JTableColumnDescriptor(String columnName)
	{
		this(columnName, true);
	}

     /**
     * Constructor
     * @param columnName- name of the column
     * @param alignment- alignment(left/center/right) of the column
     * By default column's
     * width = 65, , sortOrder = -1, isAscending = false, display = false
     */
	public JTableColumnDescriptor(String columnName, String alignment)
	{
		setColumnDetails(columnName, _defaultDisplay, alignment);
	}

    /**
    * Constructor
    * @param columnName- name of the column
    * @param alignment- alignment(left/center/right) of the column
    * @param columnHeaderToolTipText- tooltip of column header
    * By default column's
    * width = 65, sortOrder = -1, isAscending = false, display = false
    */
	public JTableColumnDescriptor(String columnName, String alignment, String columnHeaderToolTipText)
	{
		setColumnDetails(columnName, _defaultDisplay, alignment, columnHeaderToolTipText);
	}
	
      /**
        * Constructor
        * @param columnName- name of the column
        * @param display - true ----column should be displayed , false ---- column should not be displayed
        * @param isAscending - true (ascending order sort) , false (descending order sort)
        * @param sortOrder - order that says which column to be sorted first, second up untill fourth
        * @param alignment- alignment(left/center/right) of the column
        * @param columnHeaderToolTipText- tooltip of column header
        * @param width- width of the column
        */
	public JTableColumnDescriptor(String columnName, int width,
			boolean display, boolean isAscending,
			int sortOrder, String alignment, String columnHeaderToolTipText)
	{
		setColumnDetails(columnName, width, display, isAscending, sortOrder, alignment, columnHeaderToolTipText);
	}

      /**
        * Constructor
        * @param columnName- name of the column
        * @param display- true ----column should be displayed , false ---- column should not be displayed
        * @param colWidth- width of the column
        * By default column's
        * sortOrder = -1, isAscending = false, display = false, alignment = left
        */
    public JTableColumnDescriptor(String colName, 
            boolean display, int colWidth)
    {
        int sortOrder = -1;
        boolean isAscending = false;
        String alignment = _defaultAlignment;
        setColumnDetails(colName, colWidth, display, isAscending, sortOrder, alignment);
    }
    
    /**
     * Constructor
     * @param columnName- name of the column
     * @param display- true ----column should be displayed , false ---- column should not be displayed
     * @param colWidth- width of the column
     * @param alignment- alignment(left/center/right) of the column
     * By default column's
     * sortOrder = -1, isAscending = false 
     */
    public JTableColumnDescriptor(String colName, 
            boolean display, int colWidth, String alignment)
    {
        int sortOrder = -1;
        boolean isAscending = false;
        setColumnDetails(colName, colWidth, display, isAscending, sortOrder, alignment);
    }
    
    /**
     * Constructor
     * @param columnName- name of the column
     * @param display - true ----column should be displayed , false ---- column should not be displayed
     * @param width- width of the column
     * @param alignment- alignment(left/center/right) of the column
     * @param sortOrder - order that says which column to be sorted first, second up untill fourth
     * By default column's
     * sortOrder = -1, isAscending = false 
     */
    public JTableColumnDescriptor(String colName, 
            boolean display, int colWidth, String alignment, String columnHeaderToolTipText)
    {
        int sortOrder = -1;
        boolean isAscending = false;
        setColumnDetails(colName, colWidth, display, isAscending, sortOrder, alignment, columnHeaderToolTipText);
    }

    /**
     * Constructor
     * @param columnName- name of the column
     * @param colWidth- width of the column
     * @param alignment- alignment(left/center/right) of the column
     * @param columnHeaderToolTipText- tooltip of column header
     * By default column's
     * sortOrder = -1, isAscending = false , display = false
     */
    public JTableColumnDescriptor(String colName, 
            int colWidth, String alignment, String columnHeaderToolTipText)
    {
        int sortOrder = -1;
        boolean isAscending = false;
        boolean display = true;
        setColumnDetails(colName, colWidth, display, isAscending, sortOrder, alignment, columnHeaderToolTipText);
    }
    

	public JTableColumnDescriptor(JTableColumnDescriptor origRecord)
	{
		this(origRecord.getColumnName(), 
				origRecord.getDisplay());
	}

    /**
     * Set the column details for specified column
     * @param colName- name of the column
     * By default column's
     * width = 65, sortOrder = -1, isAscending = false, display = false, alignment = left 
     */
	public void setColumnDetails(String colName)
	{
		boolean display = _defaultDisplay;
		setColumnDetails(colName,display);
	}

     /**
     * Set the column details for specified column
     * @param colName- name of the column
     * @param display - true ----column should be displayed , false ---- column should not be displayed
     * By default column's
     * width = 65, sortOrder = -1, isAscending = false, display = false, alignment = left 
     */
	public void setColumnDetails(String colName, 
			boolean display)
	{
		int colWidth = _defaultWidth;
		int sortOrder = -1;
		boolean isAscending = false;
		String alignment = _defaultAlignment;
		setColumnDetails(colName, colWidth, display, isAscending, sortOrder, alignment);
	}

     /**
     * Set the column details for specified column
     * @param colName- name of the column
     * @param display - true ----column should be displayed , false ---- column should not be displayed
     * @param alignment- alignment(left/center/right) of the column
     * @param columnHeaderToolTipText- tooltip of column header
     * By default column's
     * width = 65, sortOrder = -1, isAscending = false 
     */
	public void setColumnDetails(String colName, 
			boolean display, String alignment, String columnHeaderToolTipText)
	{
		int colWidth = _defaultWidth;
		int sortOrder = -1;
		boolean isAscending = false;
		setColumnDetails(colName, colWidth, display, isAscending, sortOrder, alignment, columnHeaderToolTipText);
	}
	
    /**
     * Set the column details for specified column
     * @param colName- name of the column
     * @param display - true ----column should be displayed , false ---- column should not be displayed
     * @param alignment- alignment(left/center/right) of the column
     * By default column's
     * width = 65, sortOrder = -1, isAscending = false 
     */
	public void setColumnDetails(String colName, 
			boolean display, String alignment)
	{
		int colWidth = _defaultWidth;
		int sortOrder = -1;
		boolean isAscending = false;
		setColumnDetails(colName, colWidth, display, isAscending, sortOrder, alignment);
	}

    /**
     * Set the column details for specified column to default
     * width = 65, sortOrder = -1, isAscending = false , display = false, alignment = left
     */
     
	public void setDefaultColumnDetails()
	{
		_display = _defaultDisplay;
		_isAscending = false;
		_sortOrder = -1;
		_width = _defaultWidth;
        _alignment = _defaultAlignment;
        _isEditable = false;
	}
	
	
	public void setColumnDetails(String colName,
            int colWidth, boolean display,
            boolean isAscending,
            int sortOrder, String alignment, String columnHeaderToolTipText, boolean isEditable)
    {
        _name = colName;
        _display = display;
        _width = colWidth;
        _isAscending = isAscending;
        _sortOrder = sortOrder;
        _alignment = alignment;
        _toolTipText = columnHeaderToolTipText;
        _isEditable = isEditable;
    }
    

	public void setColumnDetails(String colName,
			int colWidth, boolean display,
			boolean isAscending,
			int sortOrder, String alignment, String columnHeaderToolTipText)
	{
		_name = colName;
		_display = display;
		_width = colWidth;
		_isAscending = isAscending;
		_sortOrder = sortOrder;
		_alignment = alignment;
		_toolTipText = columnHeaderToolTipText;
	}
	
	public void setColumnDetails(String colName,
			int colWidth, boolean display,
			boolean isAscending,
			int sortOrder, String alignment)
	{
		_name = colName;
		_display = display;
		_width = colWidth;
		_isAscending = isAscending;
		_sortOrder = sortOrder;
		_alignment = alignment;
		_toolTipText = null;
	}
	
	public void turnOnCellEditing(TableCellEditor cellEditor)
	{
	    setCellEditor(cellEditor);
	    setIsEditable(true);
	}
	
	public void turnOffCellEditing(TableCellEditor cellEditor)
    {
        setCellEditor(null);
        setIsEditable(false);
    }
	
	public String getColumnName()
	{
		String columnName = _name;
		return columnName;
	}

	public void setDisplay(boolean display) 
	{
		_display = display;
	}

	public boolean getDisplay() 
	{
		return _display;
	}

	public void setWidth(int width)
	{
		_width = width;
	}

	public int getWidth()
	{
		return _width;
	}

	public void setSortAsc(boolean isAscending)
	{
		_isAscending = isAscending;
	}

	public boolean getSortAsc()
	{
		return _isAscending;
	}

	public void setSortOrder(int sortOrder)
	{
		_sortOrder = sortOrder;
	}

	public int getSortOrder()
	{
		return _sortOrder;
	}
	
	public String getAlignment()
	{
		return _alignment;
	}
	
	public void setAlignment(String alignment)
	{
		_alignment = alignment;
	}
	
	public String getToolTipText()
	{
		return _toolTipText;
	}
	
	public void setToolTip(String columnHeaderToolTipText)
	{
		_toolTipText = columnHeaderToolTipText;
	}

    public void setIsEditable(boolean isEditable)
    {
        _isEditable = isEditable;
    }

    public boolean isEditable()
    {
        return _isEditable;
    }

    public void setCellEditor(TableCellEditor cellEditor)
    {
        _cellEditor = cellEditor;
    }

    public TableCellEditor getCellEditor()
    {
        return _cellEditor;
    }
	
}
