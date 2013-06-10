/*
 * Created on June 08, 2004
 *
 * Filename : UnitHydrographEditor.java
 * Author   : Gautam Sood
 * Modified by: Chip Gobs (addition of graphics and the table to control them).
 * Purpose: Display GUI for Unit Hydrograph.
 * Last Revision Date : October 8, 2004
 * 
 */

package ohd.hseb.sshp.window;

import javax.swing.*;
import javax.swing.event.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import ohd.hseb.model.DurationCode;
import ohd.hseb.model.RainfallRunoffModelType;
import ohd.hseb.model.UnitHydrographDescriptor;
import ohd.hseb.model.UnitHydrographEntry;


import ohd.hseb.sshp.DataMgr;

import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.ExtensionFileFilter;
import ohd.hseb.util.gui.OptionSelectionDialog;
import ohd.hseb.util.gui.TableHelper2;
import ohd.hseb.util.StringParser;

import java.text.DecimalFormat;
import java.util.List;
import java.util.*;

public class UnitHydrographEditor extends JDialog
{
	private Container _frameContentPane = 			getContentPane();
	
	private JPanel _uhgListPanel =	 				new JPanel();
	private JPanel _uhgTextPanel = 					new JPanel();
	private JPanel _listButtonPanel = 				new JPanel();
	private JPanel _dialogButtonPanel = 			new JPanel();
	
	private Font _labelFont = 						new Font( "Helvetica", Font.BOLD, 13 );
	private Font _textFieldFont = 					new Font( "Helvetica", Font.PLAIN, 12 );
	
	private List _uhgList	= 						null;
	private List _uhgEntryAddModifyDeleteList =     new ArrayList(); 
	
	private DataMgr _dataMgr = 						null;
	private UnitHydrographEntry _unitHydrographEntry = null;
	
	private String _locationId = 					null;

    private String _connectionString =              null;
    private FileLogger _logger =                    null;
    
	private static final int DEFAULT_WIDTH =		1200;
	private static final int DEFAULT_HEIGHT = 		800; //909
	
	private static final int MAX_WIDTH = 			1200;
	private static final int MAX_HEIGHT =			1000;
	
	private static final int MIN_WIDTH = 			545;
	private static final int MIN_HEIGHT = 			500;
	
	//used for JFormattedTextFields
	private static final String _decimalFormatString = "0.0";
	private DecimalFormat _decimalFormat = 			new DecimalFormat( _decimalFormatString );

	private JLabel _areaIdLabel = 					getJLabel( "Area ID" );
	private JLabel _durationLabel = 				getJLabel( "Duration" );
	private JLabel _ordLabel = 						getJLabel( "Ordinal" );
	private JLabel _dischargeLabel = 				getJLabel( "Discharge" );
	
	private JLabel _modelLabel = 					getJLabel( "Model" );

	
	private static final String[]  _modelStringArray =	{"ANY", "API-MKC", "SAC-SMA" };
	private JComboBox _modelComboBox = 				new JComboBox( _modelStringArray );
	
	private static final String[] _durationStringArray = { "1", "6" };
	private JComboBox _durationComboBox = 			new JComboBox (_durationStringArray);
	
	private JTextField _areaIdTextField = 			getJTextField( 13 );
	private JTextField _ordTextField = 				getJTextField( 13 );
	private JTextField _dischargeTextField = 		getJTextField( 13 );

	
	private JButton _saveButton = 					new JButton( "Save" );
	private JButton _deleteButton = 				new JButton( "Delete" );
	private JButton _closeButton = 					new JButton( "Close" );
	private JButton _addButton = 					new JButton( "Add" );
	private JButton _modifyButton = 				new JButton( "Modify" );
	private JButton _removeButton = 				new JButton( "Remove" );
	private JButton _importButton = 				new JButton( "Import" );	
	private JButton _exportButton = 				new JButton( "Export" );
	
	
	//used for custom notification
	private List _applyListenerList =				new ArrayList();

	
	//used for the row and master Tables
	public static final String AREA_ID = 			"Area ID";
	public static final String MODEL = 				"Model";
	public static final String DURATION = 			"Duration";
	
	//used for the row table
	public static final String ORDINAL = 			"Ordinal";
	public static final String DISCHARGE = 			"Discharge";
	
	public static final String DISPLAY = 			"Display";

	
	private String[] _rowTableColumnNames = 				{ AREA_ID, MODEL, DURATION, ORDINAL, DISCHARGE };
	private int _lastColumnSorted = 0;
	
	private String[][] _unitHydrographStringArray = null;
		
	private JTable _uhgRowJTable = 					null;
	private JScrollPane _scrollPane = 				null;
	private SshpTableModel _rowTableModel =    		null;
	private ListSelectionModel _rowListSelectionModel = null;
	
	private boolean _needsSaving = false;
	
	private String _importDirString = null;
	private String _exportDirString = null;
	private String _fileChooserDirectory = null;

	//used for sorting the Table
	private UnitHydrographEntryComparator _comparator = new UnitHydrographEntryComparator();

	private TableHelper2 _tableHelper = 				new TableHelper2( _comparator, _rowTableColumnNames );
    
    
    // ---- picture related objects -----------------------------------------------
	//canvas Panel
	private UnitHydrographCanvasPanel _canvasPanel =	null;
		
    // -------------------------------------------------------------------------------------

    public UnitHydrographEditor()
    {
    }
    
	public UnitHydrographEditor( Frame owner, DataMgr dataMgr, String locationId, boolean isModal )
	/********************
	Purpose: Constructor 
	*********************/
	{
		super( owner, isModal );
		_locationId = locationId;
		this.setTitle( "Unit Hydrograph - " + _locationId );
		_dataMgr = dataMgr;
		
		AppsDefaults ad = new AppsDefaults();
		_importDirString = ad.getToken("whfs_import_dir");
		
		initGui();
	}
    // -------------------------------------------------------------------------------------
   	
	private void initGui()
	/********************
	Purpose: Initializes the GUI by calling the appropriate methods 
	*********************/
	{
        
        // init the rest of the variables
		initJTable();
		
	    // initialize the drawing area (canvas) panel
        _canvasPanel  = new UnitHydrographCanvasPanel(_uhgList);
  
	
		initUnitHydrographJTableComponents();
		initUnitHydrographDataPanelComponents();
		initListButtons();
		initDialogButtons();
		initFrameComponents();
		
		addListeners();

		setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );
		
		//set the initial sort to the "model" column	
		initializeTableSort();
	
		//let the sort take visual effect
		refreshUnitHydrographTable();
		
	
		return;
	}
    // -------------------------------------------------------------------------------------
	private int getColumnIndex(String columnName)
	{
	    //this does NOT keep track of whether the column has been moved graphically
	    int index = -1;
	    
	    for (int i = 0; i < _rowTableColumnNames.length; i++)
	    {
	        if (_rowTableColumnNames[i].equalsIgnoreCase(columnName))
	        {
	            index = i;
	            break;
	    	}
	    }
	    
	    return index;
	}
    // -------------------------------------------------------------------------------------
	private List loadUhgList()
	{
	    
		List list = _dataMgr.loadUnitHydrographEntryList( _locationId, "ORDER BY ordinal" );   
	
		return list;
	}
	
    // -------------------------------------------------------------------------------------

	private void initJTable()
	{
		int index = 0;
		final String header = "UnitHydrographEditor.setupTables(): "; 
		_uhgList = loadUhgList();
		_unitHydrographStringArray = convertListToArray();
		if ((_uhgList != null) && ( _uhgList.size() != 0 ))
		{
			_unitHydrographEntry = (UnitHydrographEntry) _uhgList.get( 0 );
		}

		_rowTableModel = new SshpTableModel( _rowTableColumnNames, _unitHydrographStringArray );
		_uhgRowJTable = new JTable( _rowTableModel );
		_uhgRowJTable.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
		_uhgRowJTable.setPreferredScrollableViewportSize( new Dimension( 300, 100 ) );
		
		_rowListSelectionModel = _uhgRowJTable.getSelectionModel();
		_rowListSelectionModel.setSelectionInterval( index, index);
		
		Rectangle rect = _uhgRowJTable.getCellRect(index, 0, true);
		_scrollPane = new JScrollPane( _uhgRowJTable );
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();

	}

    // -------------------------------------------------------------------------------------
  
	
	// -------------------------------------------------------------------------------------
	   
	private void refreshUnitHydrographTable()
	{
		int index = 0;
		      
		
		//find index of matching entry
		if ( _unitHydrographEntry != null )
		{
			for ( int i = 0; i < _uhgList.size(); i++ )
			{
				UnitHydrographEntry uhgEntry = (UnitHydrographEntry) _uhgList.get( i );
				
				if ( _unitHydrographEntry.hasEqualKey( uhgEntry ) )
				{
					index = i;
					break;
				}
			}
		}

		//update the Jtable with the just-loaded data from the DB
        _unitHydrographStringArray = convertListToArray();
		_rowTableModel.updateData( _unitHydrographStringArray );
		_rowTableModel.fireTableChanged( null );
		
		//have the one item be selected
		_rowListSelectionModel.setSelectionInterval( index, index );

		//make sure that the item is visible
		Rectangle rect = _uhgRowJTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		
		//load up the data entry fields
		loadTextFields();

		//give the canvas the latest data - it will then redraw
		_canvasPanel.refreshData(_uhgList);
		
        return;
	}

   // -------------------------------------------------------------------------------------
	private String[][] convertMasterListToArray()
	{
		String[][] dataArray = new String[ _uhgList.size() ][ _rowTableColumnNames.length ];

		for( int i = 0; i < dataArray.length; i++ )
		{
			UnitHydrographEntry unitHydrographEntry = (UnitHydrographEntry) _uhgList.get( i );
			
			dataArray[ i ][ 0 ] = unitHydrographEntry.getAreaId();
			dataArray[ i ][ 1 ] = "" + unitHydrographEntry.getModel();
			dataArray[ i ][ 2 ] = "" + unitHydrographEntry.getDur();
			dataArray[ i ][ 3 ] = "" + unitHydrographEntry.getOrdinal();
			dataArray[ i ][ 4 ] = "" + unitHydrographEntry.getDischarge();
		}
		return dataArray;
	}        
   
   
	private String[][] convertListToArray()
	{
		String[][] dataArray = new String[ _uhgList.size() ][ _rowTableColumnNames.length ];

		for( int i = 0; i < dataArray.length; i++ )
		{
			UnitHydrographEntry unitHydrographEntry = (UnitHydrographEntry) _uhgList.get( i );
			
			dataArray[ i ][ 0 ] = unitHydrographEntry.getAreaId();
			dataArray[ i ][ 1 ] = "" + unitHydrographEntry.getModel();
			dataArray[ i ][ 2 ] = "" + DurationCode.getHoursFromCode(unitHydrographEntry.getDur());
			dataArray[ i ][ 3 ] = "" + unitHydrographEntry.getOrdinal();
			dataArray[ i ][ 4 ] = "" + unitHydrographEntry.getDischarge();
		}
		return dataArray;
	}
    // -------------------------------------------------------------------------------------
   
	private void initFrameComponents()
	/********************
	Purpose: Add's the panels to the Frame 
	*********************/
	{
		JPanel horizontalSeparatorPanel = new JPanel();
		JPanel horizontalSeparatorPanel2 = new JPanel();
		JPanel horizontalSeparatorPanel3 = new JPanel();
		JPanel horizontalSeparatorPanel4 = new JPanel();
		JPanel verticalSeparatorPanel = new JPanel();

		_frameContentPane.setLayout( new GridBagLayout() );

//		_dataScrollPane.setPreferredSize( new Dimension( 150, 50 ) );

		horizontalSeparatorPanel.setPreferredSize( new Dimension( 300, 10 ) );
		horizontalSeparatorPanel2.setPreferredSize( new Dimension( 300, 30 ) );
		horizontalSeparatorPanel3.setPreferredSize( new Dimension( 300, 30 ) );
		horizontalSeparatorPanel4.setPreferredSize( new Dimension( 300, 30 ) );
		
	
		JSeparator buttonSeparator = new JSeparator();
		
		_uhgListPanel.setMinimumSize(new Dimension(400, 500)); 
		//_uhgListPanel.setMaximumSize(new Dimension(500, 500)); 
		
//																							X,   Y,  #Col,  #Row
		ComponentHelper.addFrameComponent( _frameContentPane, _uhgListPanel,  				0,   0,   25,    5, 1, 1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel,     0,   5,   25,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _uhgTextPanel,                0,   7,   25,    1, 1, 0, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel2,    0,   8,   25,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _listButtonPanel, 			0,  10,   25,    1, GridBagConstraints.NONE );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel3,    0,  11,   25,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, buttonSeparator		   ,    0,  13,   25,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel4,    0,  15,   25,    2, GridBagConstraints.BOTH );
		
		ComponentHelper.addFrameComponent( _frameContentPane, _dialogButtonPanel,           0,  17,   25,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, verticalSeparatorPanel,       25,  0,   1,    15, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _canvasPanel,                 26,  0,   15,   15, 1, 1, GridBagConstraints.BOTH );
	    
		return;
	}
    // -------------------------------------------------------------------------------------
   	
	private void initUnitHydrographJTableComponents()
	/********************
	Purpose: Add's Components to the JList Scroll Pane Panel 
	*********************/
	{
		_uhgListPanel.setLayout( new GridBagLayout() );
																			 //          	X,  Y, #Col,  #Row
		ComponentHelper.addPanelComponent( _uhgListPanel, _scrollPane,   					0,  0,   5,    25, 1, 1, GridBagConstraints.BOTH );

	}
 
//	 -------------------------------------------------------------------------------------
	   
	private void initUnitHydrographDataPanelComponents()
	/********************
	Purpose: Add's Label/TextField componenets to the uhgDataPanel
	*********************/
	{
		JPanel verticalSeparatorPanel = new JPanel();
		JPanel verticalSeparatorPanel2 = new JPanel();
		JPanel verticalSeparatorPanel3 = new JPanel();
		JPanel horizontalSeparatorPanel = new JPanel();
		
		_uhgTextPanel.setLayout( new GridBagLayout() );

		verticalSeparatorPanel.setPreferredSize( new Dimension( 10, 20 ) );
		verticalSeparatorPanel2.setPreferredSize( new Dimension( 10, 20 ) );
		verticalSeparatorPanel3.setPreferredSize( new Dimension( 10, 20 ) );
		
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.insets = new Insets(5, 5, 5, 5);

		//_areaIdTextField.setPreferredSize(new Dimension(100, 40));
		_areaIdTextField.setMinimumSize(new Dimension(80, 20));
		_dischargeTextField.setMinimumSize(new Dimension(80, 20));
        
        _modelComboBox.setEditable(true);

//          																	         X,    Y, #Col,  #Row
		ComponentHelper.addPanelComponent( _uhgTextPanel, _areaIdLabel,   gbc,          0,    0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _uhgTextPanel, _areaIdTextField,  gbc,       0,    1,   1,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( _uhgTextPanel, _modelLabel,	 gbc,	         2,    0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _uhgTextPanel, _modelComboBox,  gbc,          2,    1,   1,     1, GridBagConstraints.BOTH );
			
		ComponentHelper.addPanelComponent( _uhgTextPanel, _durationLabel,  gbc,          4,    0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _uhgTextPanel, _durationComboBox,  gbc,       4,    1,   1,     1, GridBagConstraints.BOTH );
			
		ComponentHelper.addPanelComponent( _uhgTextPanel, _ordLabel,       gbc,          6,    0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _uhgTextPanel, _ordTextField,   gbc,          6,    1,   1,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( _uhgTextPanel, _dischargeLabel,     gbc,      8,    0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _uhgTextPanel, _dischargeTextField,   gbc,    8,    1,   1,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( _uhgTextPanel, verticalSeparatorPanel, gbc,   1,    0,   1,     2, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _uhgTextPanel, verticalSeparatorPanel2, gbc,  3,    0,   1,     2, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _uhgTextPanel, verticalSeparatorPanel3, gbc,  5,    0,   1,     2, GridBagConstraints.BOTH ); 
	}
    // -------------------------------------------------------------------------------------
   	
	private void initListButtons()
	/********************
	Purpose: adds the buttons to the list button panel 
	*********************/
	{
		JPanel verticalSeparatorPanel = new JPanel();
		JPanel verticalSeparatorPanel2 = new JPanel();
		
		_listButtonPanel.setLayout( new GridBagLayout() );
//									
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.insets = new Insets(5, 5, 5, 5);
																								      //X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( _listButtonPanel, (Component)_addButton,         gbc,       0,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _listButtonPanel, verticalSeparatorPanel,        gbc,       2,   0,   1,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _listButtonPanel, (Component) _modifyButton,      gbc,      3,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _listButtonPanel, verticalSeparatorPanel2,       gbc,       5,   0,   1,     2, GridBagConstraints.BOTH );  
		ComponentHelper.addPanelComponent( _listButtonPanel, (Component) _removeButton,      gbc,      6,   0,   2,     2, GridBagConstraints.BOTH );
		
		/*
		if ( _uhgList.size() > 0 )
		{
			_importButton.setEnabled( false );
		}
		*/

	}
    // -------------------------------------------------------------------------------------
   	
	private void initDialogButtons()
	/********************
	Purpose: adds the buttons to the dialog button panel 
	*********************/
	{
		JPanel verticalSeparatorPanel = new JPanel();
		JPanel verticalSeparatorPanel2 = new JPanel();
		JPanel verticalSeparatorPanel3 = new JPanel();
		JPanel verticalSeparatorPanel4 = new JPanel();
		JPanel horizontalSeparatorPanel = new JPanel();

		verticalSeparatorPanel.setPreferredSize( new Dimension( 50, 20 ) );
		verticalSeparatorPanel2.setPreferredSize( new Dimension( 50, 20 ) );
		verticalSeparatorPanel3.setPreferredSize( new Dimension( 50, 20 ) );
		verticalSeparatorPanel4.setPreferredSize( new Dimension( 50, 20 ) );
		horizontalSeparatorPanel.setPreferredSize( new Dimension( 300, 10 ) );

		_dialogButtonPanel.setLayout( new GridBagLayout() );
//																						  X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( _dialogButtonPanel, _saveButton,               6,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, verticalSeparatorPanel,    8,   0,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, _deleteButton,            12,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, verticalSeparatorPanel2,  14,   0,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, _importButton,            18,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, verticalSeparatorPanel3,  20,   0,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, _exportButton,            24,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, verticalSeparatorPanel4,  26,   0,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, _closeButton,             30,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dialogButtonPanel, horizontalSeparatorPanel,  0,   2,  20,     2, GridBagConstraints.BOTH );  
	}
    // -------------------------------------------------------------------------------------
   	
	
	private void loadTextFields()
	/********************
	Purpose: Load's the appropriate values for the Text Fields 
	*********************/
	{
		if ( _unitHydrographEntry != null )
		{
			_areaIdTextField.setText( _unitHydrographEntry.getAreaId());
			//_modelComboBox.setSelectedIndex( getModelIndex(_unitHydrographEntry.getModel()));
			
			
			ensureModelIsInModelComboBox(_modelComboBox, _unitHydrographEntry.getModel());
			_modelComboBox.setSelectedItem( _unitHydrographEntry.getModel());
			
			_durationComboBox.setSelectedItem( "" + DurationCode.getHoursFromCode(_unitHydrographEntry.getDur()));
			
			//_durationTextField.setText( "" + _unitHydrographEntry.getDur() );
			_ordTextField.setText( "" + _unitHydrographEntry.getOrdinal() );
			_dischargeTextField.setText( "" + _unitHydrographEntry.getDischarge() );
		}
		else
		{
			_areaIdTextField.setText( "" );
			_modelComboBox.setSelectedIndex(0);
			_durationComboBox.setSelectedIndex(0);
			
			//_durationTextField.setText( "" );
			_ordTextField.setText( "" );
			_dischargeTextField.setText( "" );
		}
	}
	
	// ------------------------------------------------------------------------------------- 
	private void  ensureModelIsInModelComboBox(JComboBox comboBox, String modelString)
	
	{
	    boolean isInComboBox = false;
	    
	    for (int i=0 ; i < comboBox.getItemCount(); i++)
	    {
	        String item = (String) comboBox.getItemAt(i);
	        if (item.equalsIgnoreCase(modelString))
	        {
	            isInComboBox = true;
	        }
	    }
	    
	    if (! isInComboBox)
	    {
	       comboBox.addItem(modelString);    
	    }
	    
	    return;
	    
	}
	
    // -------------------------------------------------------------------------------------
    private int getModelIndex(String model)
    {
        int index = -1;
        for (int i = 0 ; i < _modelStringArray.length; i++)
        {
            if (_modelStringArray[i].equalsIgnoreCase(model))
            {
                index = i;
                break;
            }
        }
   
        return index;
    }
	
    // -------------------------------------------------------------------------------------
	  
	private void addListeners()
	/********************
	Purpose: Add's listeners to the buttons and TextFields
	*********************/
	{
		WindowCloserListener windowCloser = new WindowCloserListener();
		_addButton.addActionListener( new AddModifyButtonListener() );
		_modifyButton.addActionListener( new AddModifyButtonListener() );
		_removeButton.addActionListener( new RemoveButtonListener() );
		_importButton.addActionListener( new ImportButtonListener() );
		_exportButton.addActionListener( new ExportButtonListener() );
		_saveButton.addActionListener( new SaveButtonListener() );
		_closeButton.addActionListener( windowCloser );
		_deleteButton.addActionListener( new DeleteButtonListener() );
		addWindowListener( windowCloser );
		addComponentListener( new WindowResizeComponentListener() );
		_rowListSelectionModel.addListSelectionListener( new TableUpdateListener() );
		_uhgRowJTable.getTableHeader().addMouseListener( new TableSortingListener() ); 

		//_apiMkcCheckBox.addActionListener(new UnitgraphDrawingListener("API-MKC"));
		//_sacSmaCheckBox.addActionListener(new UnitgraphDrawingListener("SAC-SMA"));
	}

	   // -------------------------------------------------------------------------------------

	private void saveChanges()
	{
	    for( int i = 0; i < _uhgEntryAddModifyDeleteList.size(); i++ )
		{
			UnitHydrographEntryCommand entry = (UnitHydrographEntryCommand) _uhgEntryAddModifyDeleteList.get( i );
			
			if ( entry.AddToDatabase() )
			{
				_dataMgr.saveUnitHydrographEntry( entry.getUhgEntry() );
			}
			else if ( ! entry.AddToDatabase() )
			{
				_dataMgr.removeUnitHydrographEntry( entry.getUhgEntry() );
			}
		}
		_uhgEntryAddModifyDeleteList.clear();    
		
		setNeedsSaving(false);
	    
	}
	
    // -------------------------------------------------------------------------------------
   	private void setNeedsSaving(boolean value)
   	{
   	    _needsSaving = value;
   	    
   	    if (_needsSaving)
   	    {
   	        _saveButton.setText("* Save!!! *");
   	    }
   	    else
   	    {
   	        _saveButton.setText("Save");
   	    }
   	    
   	}
    // -------------------------------------------------------------------------------------
	private class TableSortingListener extends MouseAdapter
	{
		public void mouseClicked( MouseEvent e )
		{
			Point point = e.getPoint();
			_lastColumnSorted = _uhgRowJTable.getTableHeader().columnAtPoint( point );
			//System.out.println("TableSortingListener(): mouseClicked()");
			_tableHelper.sortList( _uhgList, _lastColumnSorted, true );
			refreshUnitHydrographTable();
		}
	}
    // -------------------------------------------------------------------------------------
   
	private class TableUpdateListener implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent arg0)
		{
			int index = _uhgRowJTable.getSelectedRow();
			if ( ( index >= 0 ) && ( ! _uhgList.isEmpty() ) )
			{
				_unitHydrographEntry = (UnitHydrographEntry) _uhgList.get( index );
			}
			else
			{
				_unitHydrographEntry = null;
			}
			loadTextFields();
		}
	}
    // -------------------------------------------------------------------------------------
   
	private class SaveButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			saveChanges();
		}
	}
    // -------------------------------------------------------------------------------------
   	
	private class DeleteButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			boolean confirm = false;
			
			String basinId = _areaIdTextField.getText();
			String model = (String) _modelComboBox.getSelectedItem();
		
			String durationString = (String) _durationComboBox.getSelectedItem();
			int	durationCode = DurationCode.getCodeFromHoursString(durationString);
			
			confirm = DialogHelper.displayConfirmDialog(null,
						"Are you sure you want to delete the unit hydrograph for \n "  +
						_locationId + " for the basin = " + basinId + "\n" +
						" and the model = " + model + " AND " +
					    " duration = "+  durationCode + "?",  "Delete Unit Hydrograph");
			
			
			
			
			if ( confirm )
			{
				_dataMgr.deleteUnitHydrograph( _locationId, basinId, model, durationCode );
				_uhgEntryAddModifyDeleteList.clear();  //Clears the queue of all updates/deletions
				
				_uhgList = loadUhgList();
						
				resortUsingTableSort();
				refreshUnitHydrographTable();
				
			}
		}
	}
    // -------------------------------------------------------------------------------------
		 
	private boolean isAnyFieldEmpty()
	{
		boolean isEmpty = false;
		
		if ( 
		   (  isEmptyString( _areaIdTextField.getText() ) ) ||
		   (  isEmptyString((String) _modelComboBox.getSelectedItem() ) ) ||
		   (  isEmptyString((String) _durationComboBox.getSelectedItem() ) ) ||
		   (  isEmptyString( _ordTextField.getText() ) ) ||
		   (  isEmptyString( _dischargeTextField.getText() ) ) 
		   )  // Checks to make sure the text fields do not contain
			  // empty strings
		{
			isEmpty = true;
		}
		return isEmpty;
	}
    // -------------------------------------------------------------------------------------
   
	private UnitHydrographEntry getUnitHydrographEntryFromFields()
	{
		UnitHydrographEntry newUHGEntry = new UnitHydrographEntry();
		
		try
		{
		
		newUHGEntry.setAreaId( _areaIdTextField.getText().trim() );
		newUHGEntry.setModel((String) _modelComboBox.getSelectedItem());
		newUHGEntry.setDur( DurationCode.getCodeFromHoursString( (String)_durationComboBox.getSelectedItem()) );
		newUHGEntry.setOrdinal( Integer.parseInt( _ordTextField.getText().trim() ) );
		newUHGEntry.setDischarge( Double.parseDouble( _dischargeTextField.getText().trim() ) );
		newUHGEntry.setLocationId( _locationId );
		}
		
		catch (Exception e)
		{
		    DialogHelper.displayErrorDialog(this, "Error parsing an input field, please examine.", "UnitHydrographEditor Data Entry Error" );
		    newUHGEntry = null;
		    
		}
		
		return newUHGEntry;
	}
    // -------------------------------------------------------------------------------------
   	
	private class AddModifyButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{

			UnitHydrographEntry newUHGEntry = null;
			
			if ( ! isAnyFieldEmpty() )
			{
				boolean addToDatabase = true;
				boolean duplicateEntry = false;
			//	int column = _lastSortedColumn;
				
				newUHGEntry = getUnitHydrographEntryFromFields();
				
				
				if (newUHGEntry != null)
				{
				
				    duplicateEntry = validateNewUnitHydrographEntry(newUHGEntry);

                    if (e.getSource() == _addButton)
                    {
                        if (!duplicateEntry)
                        {
                            UnitHydrographEntryCommand addEntry = new UnitHydrographEntryCommand(
                                    newUHGEntry, addToDatabase);
                            _uhgEntryAddModifyDeleteList.add(addEntry);
                            _unitHydrographEntry = newUHGEntry;
                            _uhgList.add(newUHGEntry);
                        }
                        else
                        {
                            DialogHelper
                                    .displayErrorDialog(
                                            UnitHydrographEditor.this,
                                            "Duplicate Ordinal values may not be entered.",
                                            "Error Message");
                        }

                    }
                    else if (e.getSource() == _modifyButton)
                    {
                        if (duplicateEntry)
                        {
                            UnitHydrographEntryCommand changedEntry = new UnitHydrographEntryCommand(
                                    newUHGEntry, addToDatabase);
                            _uhgEntryAddModifyDeleteList.add(changedEntry);
                            _uhgList.remove(_unitHydrographEntry);
                            _uhgList.add(newUHGEntry);

                            _unitHydrographEntry = newUHGEntry;
                        }
                    }
				
                   // System.out.println("AddModifyButtonListener(): actionPerformed()");

                    //sort by ordinal so that it looks good
                    _tableHelper.sortWithoutMemory(_uhgList,
                            getColumnIndex(ORDINAL), true);

                    // sort by whatever is the last requested sort order
                    _tableHelper.resortList(_uhgList);

                    refreshUnitHydrographTable();

                    notifyAllApplyActionListeners(); // notifies all the listeners

                    setNeedsSaving(true);
				
				} //end newUHGEntry
				
			}
			
			
		}
        // -------------------------------------------------------------------------------------
   		
		private boolean validateNewUnitHydrographEntry( UnitHydrographEntry newUHGEntry )
		{
			boolean foundEntry = false;
			
			for( int i = 0; i < _uhgList.size(); i++ )
			{
				UnitHydrographEntry entry = (UnitHydrographEntry) _uhgList.get( i );
				
				if ( entry.hasEqualKey( newUHGEntry ) )
				{
					_unitHydrographEntry = entry;
					foundEntry = true;
				}
			}
			return foundEntry;
		}
	}
    // -------------------------------------------------------------------------------------
   	
	private class ExportButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			UnitHydrographEditor.this.exportUnitHydrographToFile();
		}
	}
    // -------------------------------------------------------------------------------------
   	
	private void exportUnitHydrographToFile()
	{
	    File fileName = null;
	    OutputStream outputStream = null;
	    PrintWriter writer = null;
	    boolean confirm = false;
	    boolean fileIsOpen = false;
	    
	    
	    // get confirmation of desire to Export the unit hydrograph	
	    String basinId = _areaIdTextField.getText();
	    String model = (String) _modelComboBox.getSelectedItem();
	    
	    String durationString = (String) _durationComboBox.getSelectedItem();
	    int	durationCode = DurationCode.getCodeFromHoursString(durationString);
	    
	    
	    //use this to ensure only 1 unithydrograph is exported
	    UnitHydrographDescriptor selectedDescriptor = 
	                    new UnitHydrographDescriptor(_locationId, basinId, model, durationCode );    
	    
	    String message = "Are you sure you want to export the unit hydrograph for \n "  +
        _locationId + " for the basin = " + basinId + "\n" +
        " and the model = " + model + " AND " +
        " durationCode = " +  durationCode + "?\n" +
        "Note: the export file will be overwritten if it already exists\n";
	    
	    
	    confirm = DialogHelper.displayConfirmDialog(null, message, "Export Unit Hydrograph");
	    
	    if ( confirm )
	    {        
		    fileName = displayExportFileChooser();
		    
	        if ( ! fileIsOpen )
	        {
	            try
	            {
	                if ( fileName != null )
	                {
	                    outputStream = new FileOutputStream(fileName, false);
	                    fileIsOpen = true;
	                }
	                else //fileName is null
	                {
	                    outputStream = System.out;
	                }
	                
	                writer = new PrintWriter( outputStream );
	            }
	            catch (java.io.IOException e)
	            {
	                String errorMessage = "Error exporting file: " + e.getMessage();
	                DialogHelper.displayErrorDialog(null, message, "Export Error");
	                //e.printStackTrace();
	            }
	        }
	        for ( int i = 0; i < _uhgList.size(); i++ )
	        {
	            UnitHydrographEntry entry = (UnitHydrographEntry) _uhgList.get( i );
	            
	            //only write out the unithydrograph that matches the one selected to be
	            //exported
	            if (selectedDescriptor.isMatchingEntry(entry) )         
	            {
	            
	                writer.println( entry.getAreaId() + " " + entry.getModel() + " " +
	                    entry.getDur() + " "  +
	                    entry.getOrdinal() + " " + entry.getDischarge() );
	                writer.flush();
	            
	            }
	        }
	        writer.close();
	        
	    }
	    
	    
	}
	
    // -------------------------------------------------------------------------------------
   	
	private class ImportButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			File importFile = UnitHydrographEditor.this.displayImportFileChooser();
			if ( importFile != null )
			{
				boolean result = UnitHydrographEditor.this.importUnitHydrographFromFile( importFile );
				
				if (result)
				{
				    saveChanges();
				}
			}
			
		}
	}
    // -------------------------------------------------------------------------------------
   
	public void addApplyActionListener( ActionListener listener )
	{
		_applyListenerList.add(listener);
	}
    // -------------------------------------------------------------------------------------
   
	private void notifyAllApplyActionListeners()
	{
		for (int i = 0; i < _applyListenerList.size(); i++)
		{
			ActionListener listener = (ActionListener) _applyListenerList.get(i);
			ActionEvent event = new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "APPLY action");
			listener.actionPerformed(event);
		}
	}
    // -------------------------------------------------------------------------------------
   
	private class RemoveButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
            boolean confirm = false;
            
			if ( _unitHydrographEntry != null )
			{
                confirm = DialogHelper.displayConfirmDialog(null,
                                                           "Are you sure you want to delete this Entry?", 
                                                           "Delete Entry");
				if ( confirm )  // User selected YES
				{
					boolean addToDatabase = false;
					
					UnitHydrographEntryCommand deleteEntry = new UnitHydrographEntryCommand( _unitHydrographEntry, addToDatabase );
					
					_uhgEntryAddModifyDeleteList.add( deleteEntry );
					_uhgList.remove( _unitHydrographEntry );
					_unitHydrographEntry = null;
					
					resortUsingTableSort();
					refreshUnitHydrographTable();
		
				}
			}
			
			setNeedsSaving(true);
		}
	}
    // -------------------------------------------------------------------------------------
   
	private class WindowCloserListener extends WindowAdapter implements ActionListener
	{
		public void windowClosing ( WindowEvent e )
		{
			closeWindow();
		}
		
		public void actionPerformed( ActionEvent e )
		{
			closeWindow();
		}
	}
    // -------------------------------------------------------------------------------------
   	
	private class WindowResizeComponentListener extends ComponentAdapter
	{
		public void componentResized( ComponentEvent e )
		{
			int height = UnitHydrographEditor.this.getHeight();
			int width = UnitHydrographEditor.this.getWidth();
		//    System.out.println( "Width/Height = [" + width + ", " + height + "]" );

			if ( width < MIN_WIDTH ) 
			{ 
				UnitHydrographEditor.this.setSize( new Dimension( MIN_WIDTH, height ) );
			}
			if ( width > MAX_WIDTH )
			{
				UnitHydrographEditor.this.setSize( new Dimension( MAX_WIDTH, height ) );
			}
			if ( height < MIN_HEIGHT )
			{
				UnitHydrographEditor.this.setSize( new Dimension( width, MIN_HEIGHT ) );
			}
			if ( height > MAX_HEIGHT )
			{
				UnitHydrographEditor.this.setSize( new Dimension( width, MAX_HEIGHT ) );
			}
		
		}
	}
	//-------------------------------------------------------------------------------------

	private boolean importUnitHydrographFromFile( File importFile )
	{
	    boolean result = false;
		boolean addToDatabase = true;
		final int newFormatTokenCount = 5;
		final int oldFormatTokenCount = 4;
		boolean needToAddModelType = false;
		String defaultModelType = null;
		
		try
		{
			BufferedReader _reader = new BufferedReader( new FileReader( importFile ) );
			String line = _reader.readLine();
			if ( _uhgList == null )
			{
				_uhgList = new ArrayList();
				_uhgEntryAddModifyDeleteList = new ArrayList();
			}
		
			
			//determine if need to do something special to translate from original format UHG files to
			// new format
			
			String[] initialTokenizedLine = StringParser.tokenize( line );
			int tokenCount = initialTokenizedLine.length;
			if ( tokenCount == oldFormatTokenCount)
			{
			    //need to translate == true;
			    needToAddModelType = true;
		        defaultModelType = getModelTypeFromDialog();
			}
			else if (tokenCount == newFormatTokenCount)
			{
			    //things are good   
			}
			else
			{
			    String message = "Invalid file format. There are " + tokenCount + 
			                     " fields in the line instead of " + 
			                     oldFormatTokenCount + " or " + newFormatTokenCount + " fields.";
			    throw new Exception(message);
			}
			
			
			//read in each line and add it to the JTable
			while( line != null )
			{	    
				//String[] tokenizedLine = StringParser.tokenize( line );
			    String[] tokenizedLine = getTokenizedLine(line, needToAddModelType, defaultModelType);
               
				UnitHydrographEntry entry = new UnitHydrographEntry();
				
				entry.setLocationId( _locationId );
				entry.setAreaId( tokenizedLine[ 0 ] );
				
				//special model type name processing, tries
				// to correct any misspellings or wrong case
				String model = tokenizedLine[ 1 ];	
				model = correctModelName(model);
				entry.setModel(model);
                	
				entry.setDur( Integer.parseInt( tokenizedLine[ 2 ] ) );
				entry.setOrdinal( Integer.parseInt( tokenizedLine[ 3 ] ) );
				entry.setDischarge( Double.parseDouble( tokenizedLine[ 4 ] ) );

				UnitHydrographEntryCommand entryCommand = new UnitHydrographEntryCommand( entry, addToDatabase ); 
				
				
				//add the entry if it is not already present
				if(!_uhgList.contains(entry))
				{
				    _uhgList.add( entry );
					_uhgEntryAddModifyDeleteList.add( entryCommand );
				}	
				line = _reader.readLine();
			}
			
			_reader.close();
			
			// everything worked, update the display
		    result = true;	
		   
		    resortUsingTableSort();
			refreshUnitHydrographTable();
			
		}
		catch( FileNotFoundException e )
		{
		    String errorMessage = e.getMessage();
		    String title = "Import Error - File Not Found";
		    DialogHelper.displayErrorDialog(this, "Error Importing File: " + errorMessage, title);
		}
		catch( IOException e )
		{
		    String errorMessage = e.getMessage();
		    String title = "Import Error - I/O Error";
		    DialogHelper.displayErrorDialog(this, "Error Importing File: " + errorMessage, title);
		}
		catch( ArrayIndexOutOfBoundsException e )
		{
		    String errorMessage = e.getMessage();
		    String title = "Import Error - I/O Error";
		    DialogHelper.displayErrorDialog(this, "Error Importing File: " + errorMessage, title);
		    
			System.out.println( "Invalid file format: " + importFile.getName() );
		}
		catch (Exception e)
		{
		    String errorMessage = e.getMessage();
		    if (errorMessage == null)
		    {
		       errorMessage = "- Check for extra or incomplete lines at end of file."; 
		    }
		    String title = "Import File Formatting Error";
		    DialogHelper.displayErrorDialog(this, "Error Importing File: " + errorMessage, title);
		    
			System.out.println( "Invalid file format: " + importFile.getName() );
		}
		
		// the import failed, so rollback any changes
		//reload the uhgList and refresh the display
		if (result == false)
		{
		    _uhgEntryAddModifyDeleteList.clear();    	    
		    _uhgList = loadUhgList();
		    
		    resortUsingTableSort();
		    
		    refreshUnitHydrographTable();
		  
		}
		
		return result;

	}

	
//	 -------------------------------------------------------------------------------------
	private void initializeTableSort()
	{
	      
	    //set the initial sort to the "model" column	
	    _lastColumnSorted = getColumnIndex("model");
	    
	    _tableHelper.sortList(_uhgList, _lastColumnSorted, false);
	    
	}
	// -------------------------------------------------------------------------------------
	private void resortUsingTableSort()
	{    
	    _tableHelper.sortList(_uhgList, _lastColumnSorted, false);
	    
	}
	// -------------------------------------------------------------------------------------
    private String getModelTypeFromDialog()
    {
        String modelTypeString = null;
    
        String title = "Rainfall-Runoff Model Selection";
        String message = "Please enter the rainfall-runoff model\n" +
                         " for which this unit hydrograph is appropriate.\n" +
                         "                                           " ;
        String comboBoxLabelString = "Rainfall-Runoff Model:";
        String[] optionArray = { "ANY ",
                				    "API-MKC",
                				    "SAC-SMA" };
        
        OptionSelectionDialog dialog = new OptionSelectionDialog();
        
        dialog.requestItemByDialog(title, message, comboBoxLabelString, optionArray );
        
        
        modelTypeString = dialog.getItemString();
        
        return modelTypeString;
        
    }
    
    // -------------------------------------------------------------------------------------

	private String[] getTokenizedLine(String line, boolean needToAddModelType, String defaultModelType)
    { 
	    String header = "UnitHydrographEditor.getTokenizedLine(): ";
	    
	    if (needToAddModelType)
	    {
	        StringBuffer buffer = new StringBuffer(line);
	        
	        int spacePos = buffer.indexOf(" ");
	        buffer.insert(spacePos, " " + defaultModelType);
	            
	       // System.out.println(header + "old line = :" + line + ":" );
	        
	        line  =  buffer.toString();
	        
	      //  System.out.println(header + "new line = :" + line + ":" );
	    }
	    
	    String[] tokenizedLine = StringParser.tokenize( line );
	    
	    
        return tokenizedLine;
    }
    
    // -------------------------------------------------------------------------------------
	private String correctModelName(String origModelName) throws Exception
    {
        String newModelName = null;
        
        String apiName = RainfallRunoffModelType.API_MKC.getName();
        String sacSmaName = RainfallRunoffModelType.SAC_SMA.getName();
        String anyName = "ANY";
        
        String[] incorrectApiMkcNameArray = { "API_MKC", "APIMKC", "API MKC",  "MKCAPI", "KCAPI", "KC_API", "KC-API" };

        String[] incorrectSacNameArray = { "SAC_SMA", "SACSMA", "SAC SMA" };
        
        String[] incorrectAnyNameArray = {"default", "none" };

        //do case correction
        if (origModelName.equalsIgnoreCase(apiName))
        {
            newModelName = apiName;
        }
        else if (origModelName.equalsIgnoreCase(sacSmaName))
        {
            newModelName = sacSmaName;
        }
        else if (origModelName.equalsIgnoreCase(anyName))
        {
            newModelName = anyName;   
        }
        else //no match so far
        {
            boolean found = false;
            
            //look for common errors in typing of the model names
            for (int i = 0; i < incorrectApiMkcNameArray.length && !found; i++)
            {
                if (origModelName.equalsIgnoreCase(incorrectApiMkcNameArray[i]))
                {
                    newModelName = apiName;
                    found = true;
                }
            }
            
            for (int i = 0; i < incorrectSacNameArray.length && !found; i++)
            {
                if (origModelName.equalsIgnoreCase(incorrectSacNameArray[i]))
                {
                    newModelName = sacSmaName;
                    found = true;
                }
            }
            
            for (int i = 0; i < incorrectAnyNameArray.length && !found; i++)
            {
                if (origModelName.equalsIgnoreCase(incorrectAnyNameArray[i]))
                {
                    newModelName = anyName;
                    found = true;
                }
            }
            
            if (!found)
            {
                newModelName = origModelName;
                //String message = "Invalid modelName - " + origModelName;
                // throw new Exception(message);
            }
            
        }


        return newModelName;

    }
	    // -------------------------------------------------------------------------------------
	    
	private File displayImportFileChooser()
	{
	  
	    AppsDefaults ad = new AppsDefaults();
	    _importDirString = ad.getToken("whfs_import_dir");
	 
	    // reset the import directory to the last one imported from, if it exists
	    if (_fileChooserDirectory != null)
	    {      
	        _importDirString = _fileChooserDirectory;    
	    }      
	   
		JFileChooser _importFileChooser = new JFileChooser( _importDirString );
		File file = null;
		List filterList = new ArrayList();
		
		filterList.add( "UHG" );
		
		_importFileChooser.addChoosableFileFilter( new ExtensionFileFilter( filterList, "Unit Hydrograph Files (UHG)" ) );

		int returnVal = _importFileChooser.showOpenDialog( this );
		
		if (returnVal == JFileChooser.APPROVE_OPTION)
		{
			file =  _importFileChooser.getSelectedFile();
			_fileChooserDirectory = file.getParentFile().getPath().toString();
		} 
		else
		{
			DialogHelper.displayMessageDialog(UnitHydrographEditor.this, "Unit Hydrograph file not imported.");
		}
		return file;
	}
    
    // -------------------------------------------------------------------------------------
   	
	private File displayExportFileChooser()
	{   
	    
	    if (_fileChooserDirectory != null)
	    {
	        _exportDirString =  _fileChooserDirectory;  
	    }
	    else
	    {
	        AppsDefaults ad = new AppsDefaults();
	        _exportDirString = ad.getToken("whfs_import_dir"); //shares with import directory 
	    }
	    
	    
		JFileChooser _exportFileChooser = new JFileChooser( _exportDirString );
		File file = null;
		List filterList = new ArrayList();
		
		filterList.add( "UHG" );
		
		_exportFileChooser.addChoosableFileFilter( new ExtensionFileFilter( filterList, "Unit Hydrograph Files (UHG)" ) );

		int returnVal = _exportFileChooser.showSaveDialog( this );
		
		if (returnVal == JFileChooser.APPROVE_OPTION)
		{
			file =  _exportFileChooser.getSelectedFile();
			_fileChooserDirectory = file.getParentFile().getPath().toString();
		} 
		else
		{
			DialogHelper.displayMessageDialog(UnitHydrographEditor.this, "Unit Hydrograph not exported.");
		}
		
		String extension = ExtensionFileFilter.getExtension( file );
		
		if ( file != null )
		{
			if ( ( extension == null ) || ( ! isExtensionInExtensionFilterList( extension, filterList ) ) )
			{
				file = new File( file.getAbsoluteFile() + "." + (String) filterList.get( 0 ) );
			}
		}
		
		return file;
	}
	
    // -------------------------------------------------------------------------------------
   
    
	private boolean isExtensionInExtensionFilterList( String extension, List filterList )
	{
		boolean extensionExists = false;
		
		for( int i = 0; i < filterList.size(); i++ )
		{
			String ext = (String) filterList.get( i );
			if ( extension.equalsIgnoreCase( ext ) )
			{
				extensionExists = true;
                break;
			}
		}
		return extensionExists;
	}

    // -------------------------------------------------------------------------------------
   
	
	private class UnitHydrographEntryCommand
	{
		private UnitHydrographEntry _uhgEntry = null;
		private boolean _addToDatabase = false;
		
		public UnitHydrographEntryCommand( UnitHydrographEntry entry, boolean addToDatabase )
		{
			_uhgEntry = entry;
			_addToDatabase = addToDatabase;
		}

		public void setUhgEntry( UnitHydrographEntry uhgEntry )
		{
			_uhgEntry = uhgEntry;
		}

		public UnitHydrographEntry getUhgEntry()
		{
			return _uhgEntry;
		}

		public void setAddToDatabase( boolean addToDatabase )
		{
			_addToDatabase = addToDatabase;
		}

		public boolean AddToDatabase()
		{
			return _addToDatabase;
		}
	}

    // -------------------------------------------------------------------------------------
   	
	private void closeWindow()
	/********************
	Purpose: Exit's the program gracefully 
	*********************/
	{
		this.dispose();
	}

    // -------------------------------------------------------------------------------------
   
	
	private boolean isEmptyString( String testingString )
	/********************
	Purpose: Check's whether the string passed in is an empty string 
	*********************/
	{
		boolean returnValue = false;
		
		if ( testingString.trim().equals( "" ) )
		{
			returnValue = true;
		}
		return returnValue;
	}
    // -------------------------------------------------------------------------------------
   
	private JLabel getJLabel( String label )
	{
		JLabel returnJLabel = new JLabel( label );
		returnJLabel.setFont( _labelFont );
		return returnJLabel;
	}
   
    // -------------------------------------------------------------------------------------
   	
	private JTextField getJTextField( int size )
	{
		JTextField returnJTextField = new JTextField( size );
		returnJTextField.setFont( _textFieldFont );
		
		StringBuffer buffer = new StringBuffer();
		for (int i = 0 ; i < size; i++)
		{
		    buffer.append("W");
		}
		
		returnJTextField.setText(buffer.toString());
		return returnJTextField;
	}

    // -------------------------------------------------------------------------------------
   

	private JFormattedTextField getJFormattedTextField( String formatString )
	{
		JFormattedTextField returnJFormattedTextField = new JFormattedTextField( formatString );
		returnJFormattedTextField.setFont( _textFieldFont );
		returnJFormattedTextField.setColumns( 17 );
		return returnJFormattedTextField;
	}
    
    // -------------------------------------------------------------------------------------
   
	private String getFormattedValue( double valueString )
	{
		return _decimalFormat.format( valueString );
	}

    // -------------------------------------------------------------------------------------

    public void setConnectionString( String connectionString ) 
    {
        _connectionString = connectionString;
    }

    // -------------------------------------------------------------------------------------

    public String getConnectionString() 
    {
        return _connectionString;
    }

    // -------------------------------------------------------------------------------------

    public void setLogger( FileLogger logger ) 
    {
        _logger = logger;
    }

    // -------------------------------------------------------------------------------------

    public FileLogger getLogger() 
    {
        return _logger;
    }

    // -------------------------------------------------------------------------------------

    public void setLocationId( String locationId ) 
    {
        _locationId = locationId;
    }

    // -------------------------------------------------------------------------------------

    public String getLocationId() 
    {
        return _locationId;
    }

    private boolean validateInputParameters( String[] args )
    {
        String logDir          = null;
        File directory         = null;
        boolean returnValue    = true;

        if ( args.length < 3 )
        {
            System.out.println( "UnitHydrographEditor <DBConnection String> <basin ID> <log directory> " );
            returnValue = false;
        }
        else
        {
            _connectionString = args[ 0 ];

            _locationId = args[ 1 ]; 

            _logger = new FileLogger( args[ 2 ] );
        } // end else
        return returnValue;
    }


   	public static void main(String[] args) 
	{
        String basinId = null;
        DataMgr dataMgr = null;
        UnitHydrographEditor editor = new UnitHydrographEditor();
        
        boolean validInputParameters = false;
        
        validInputParameters = editor.validateInputParameters( args );
        
        if ( validInputParameters )
        {
            dataMgr = new DataMgr( editor.getConnectionString(), editor.getLogger() );
            
            editor = new UnitHydrographEditor( new Frame(), dataMgr, editor.getLocationId(), true );
            editor.show();
        }

        System.exit( 0 );
	}
}
