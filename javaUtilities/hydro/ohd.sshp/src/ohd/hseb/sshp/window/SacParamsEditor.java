/*
 * Created on Oct 21, 2003
 *
 * Filename : SacParamsEditor.java
 * Author   : Gautam Sood
 * Last Revision Date : Oct 21, 2003
 * Purpose: Display GUI for the SacSMA Parameters.
 */

package ohd.hseb.sshp.window;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.TableColumn;

import java.awt.*;
import java.awt.event.*;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.model.sacsma.SacSmaParameters;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.TableHelper;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.util.gui.*;

import java.text.DecimalFormat;
import java.util.List;
import java.util.*;

public class SacParamsEditor extends JDialog
{
	private Container _frameContentPane = 			getContentPane();
	
	private JPanel _sacParamsListPanel =	 		new JPanel();
	private JPanel _sacParamsTextPanel = 			new JPanel();
	private JPanel _buttonPanel = 					new JPanel();
	
	private Font _labelFont = 						new Font( "Helvetica", Font.BOLD, 14 );
	private Font _textFieldFont = 					new Font( "Helvetica", Font.PLAIN, 12 );
	
	private List _sacParamsList	= 					null;
	
	private DataMgr _dataMgr = 						null;
	private SacSmaParameters _sacSmaParameters =	null;
	
	private String _basinId = 						null;
	
	private static final int DEFAULT_WIDTH =		340;
	private static final int DEFAULT_HEIGHT = 		654;
	
	private static final int MAX_WIDTH = 			500;
	private static final int MAX_HEIGHT =			5000;
	
	private static final int MIN_WIDTH = 			340;
	private static final int MIN_HEIGHT = 			500;
	
	//used for JFormattedTextFields
	private static final String _threeDecimalFormatString = 	"0.000";
	private DecimalFormat _decimalFormat = 			new DecimalFormat( _threeDecimalFormatString );

	private JLabel _basinIDLabel = 					getJLabel( "Basin ID" );
	private JLabel _sourceLabel = 					getJLabel( "Source" );
	private JLabel _validTimeLabel = 				getJLabel( "Valid Time" );
	private JLabel _uztwmLabel = 					getJLabel( "UZTWM" );
	private JLabel _uzfwmLabel = 					getJLabel( "UZFWM" );
	private JLabel _uzkLabel =	 					getJLabel( "UZK" );
	private JLabel _pctimLabel =	 				getJLabel( "PCTIM" );
	private JLabel _adimpLabel = 					getJLabel( "ADIMP" );
	private JLabel _rivaLabel = 					getJLabel( "RIVA" );
	private JLabel _zpercLabel = 					getJLabel( "ZPERC" );
	private JLabel _rexpLabel = 					getJLabel( "REXP" );
	private JLabel _lztwmLabel =	 				getJLabel( "LZTWM" );
	private JLabel _lzfsmLabel = 					getJLabel( "LZFSM" );
	private JLabel _lzfpmLabel = 					getJLabel( "LZFPM" );
	private JLabel _lzskLabel = 					getJLabel( "LZSK" );
	private JLabel _lzpkLabel = 					getJLabel( "LZPK" );
	private JLabel _pfreeLabel = 					getJLabel( "PFREE" );
	private JLabel _rservLabel = 					getJLabel( "RSERV" );
	private JLabel _sideLabel = 					getJLabel( "SIDE" );
	private JLabel _peAdjustLabel = 				getJLabel( "PEADJ" );
	private JLabel _pxAdjustLabel = 				getJLabel( "PXADJ" );
	private JLabel _efcLabel = 						getJLabel( "EFC" );
	
	private JTextField _basinIDTextField = 			getJTextField( 17 );
	private JTextField _sourceTextField = 			getJTextField( 17 );
	private JTextField _validTimeTextField = 		getJTextField( 17 );
	private JFormattedTextField _uztwmTextField =	 		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _uzfwmTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _uzkTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _pctimTextField =	 		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _adimpTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _rivaTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _zpercTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _rexpTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _lztwmTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _lzfsmTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _lzfpmTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _lzskTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _lzpkTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _pfreeTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _rservTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _sideTextField = 			getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _peAdjustTextField = 		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _pxAdjustTextField = 		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _efcTextField = 			getJFormattedTextField( _threeDecimalFormatString );

	private JScrollPane _dataScrollPane =			new JScrollPane( _sacParamsTextPanel );

	private JButton _saveButton = 					new JButton( "Save" );
	private JButton _deleteButton = 				new JButton( "Delete" );
	private JButton _exitButton = 					new JButton( "Close" );
	private JButton _clearButton = 					new JButton( "Clear" );
	
	//used for custom notification
	private List _applyListenerList =				new ArrayList();

	//used for the Table
	private String[] _columnNames = 				{ "Basin ID", "Source", "Valid Time" };
	private String[][] _sacParamsStringArray = 		null;
	private JTable _sacSmaParamsTable = 			null;
	private JScrollPane _scrollPane = 				null;
	private SshpTableModel _tableModel =    		null;
	private ListSelectionModel _listSelectionModel = null;

	//used for sorting the Table
	private String[] _dbColumnNames =				{ "basin_id", "source", "validtime" };
	private TableHelper _tableHelper = 				new TableHelper( _dbColumnNames );

	public SacParamsEditor( Frame owner, DataMgr dataMgr )
	/********************
	Purpose: Constructor 
	*********************/
	{
		this( owner, dataMgr, null, false );
	}
	
	
	public SacParamsEditor( Frame owner, DataMgr dataMgr, String basinId )
	/********************
	Purpose: Constructor 
	*********************/
	{
		this( owner, dataMgr, basinId, false );
	}
	
	public SacParamsEditor( Frame owner, DataMgr dataMgr, String basinId, boolean isModal )
	/********************
	Purpose: Constructor 
	*********************/
	{
		super( owner, isModal );
		this.setTitle( "SacSma Parameters" );
		_basinId = basinId;
		_dataMgr = dataMgr;
		initialize();
	}
	
	private void initialize()
	/********************
	Purpose: Initializes the GUI by calling the appropriate methods 
	*********************/
	{
		setupTable();		
		
		AddSacParamsListPanelComponents();
		AddSacParamsDataPanelComponents();
		AddButtons();
		AddFrameComponents();
		AddListeners();
		
		setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );
	}

	private void setupTable()
	{
		int index = -1;
		boolean foundBasinId= false;
		
		_tableHelper.setOrderByClause( 0 );

		_sacParamsList = _dataMgr.loadSacSmaParamsList( _tableHelper.getOrderByClause() );
		_sacParamsStringArray = convertListToArray();

		_tableModel = new SshpTableModel( _columnNames, _sacParamsStringArray );
		_sacSmaParamsTable = new JTable( _tableModel );
		_sacSmaParamsTable.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
		_sacSmaParamsTable.setPreferredScrollableViewportSize( new Dimension( 300, 100 ) );
		_listSelectionModel = _sacSmaParamsTable.getSelectionModel();
		_scrollPane = new JScrollPane( _sacSmaParamsTable );
		if ( _basinId != null )
		{
			for ( int i = 0; i < _sacParamsList.size(); i++ )
			{
				SacSmaParameters sacSmaParams = (SacSmaParameters) _sacParamsList.get( i );
	
				if ( _basinId.equalsIgnoreCase( sacSmaParams.getBasinId().trim() ) )
				{
					index = i;
					foundBasinId = true;
					_sacSmaParameters = sacSmaParams;
					break;
				}
			}
		}
		
		if ( ! foundBasinId )  // If no basin ID found in the Params list, selects the first one by default.
		{
		    if (_sacParamsList.size() > 0)
		    {
		        _sacSmaParameters = (SacSmaParameters) _sacParamsList.get( 0 );
		    }
		}
		
		for( int i = 0; i < _columnNames.length; i++ )
		{
			TableColumn column = _sacSmaParamsTable.getColumnModel().getColumn( i );
			if ( i == 2 )
			{
				column.setPreferredWidth( 150 );
			}
			else
			{
				column.setPreferredWidth( 75 );
			}
		}
		
		if ( foundBasinId )  // Checks if a Basin Id was found
		{
			_listSelectionModel.setSelectionInterval( index, index );
		}
		else
		{
			_listSelectionModel.setSelectionInterval( 0, 0 );
		}
		Rectangle rect = _sacSmaParamsTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();
	}

	private void refreshSacSmaParamsTable()
	{
		int index = 0;
		
		_sacParamsList = _dataMgr.loadSacSmaParamsList( _tableHelper.getOrderByClause() );

		_sacParamsStringArray = convertListToArray();
		if ( _sacSmaParameters != null )
		{
			for ( int i = 0; i < _sacParamsList.size(); i++ )
			{
				SacSmaParameters sacSmaParams = (SacSmaParameters) _sacParamsList.get( i );
				
				if ( _sacSmaParameters.equals( sacSmaParams ) )
				{
					index = i;
					break;
				}
			}
		}
		_tableModel.updateData( _sacParamsStringArray );
		_tableModel.fireTableChanged( null );
		
		for( int i = 0; i < _columnNames.length; i++ )
		{
			TableColumn column = _sacSmaParamsTable.getColumnModel().getColumn( i );
			if ( i == 2 )
			{
				column.setPreferredWidth( 150 );
			}
			else
			{
				column.setPreferredWidth( 75 );
			}
		}

		_listSelectionModel.setSelectionInterval( index, index );

		Rectangle rect = _sacSmaParamsTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();

	}

	private String[][] convertListToArray()
	{
		String[][] dataArray = new String[ _sacParamsList.size() ][ _columnNames.length ];
		
		for( int i = 0; i < dataArray.length; i++ )
		{
			SacSmaParameters sacParams = (SacSmaParameters) _sacParamsList.get( i );
			
			dataArray[ i ][ 0 ] = sacParams.getBasinId();
			dataArray[ i ][ 1 ] = sacParams.getSource();
			dataArray[ i ][ 2 ] = "" + DbTimeHelper.getDateTimeStringFromLongTime( sacParams.getValidTime() );
		}
		return dataArray;
	}

	private void AddFrameComponents()
	/********************
	Purpose: Add's the panels to the Frame 
	*********************/
	{
		JPanel horizontalSeparatorPanel = new JPanel();
		JPanel horizontalSeparatorPanel2 = new JPanel();
		_frameContentPane.setLayout( new GridBagLayout() );

		_dataScrollPane.setPreferredSize( new Dimension( 150, 300 ) );

		horizontalSeparatorPanel.setPreferredSize( new Dimension( 300, 10 ) );
		horizontalSeparatorPanel2.setPreferredSize( new Dimension( 300, 30 ) );
//																							X,   Y,  #Col,  #Row
		ComponentHelper.addFrameComponent( _frameContentPane, _sacParamsListPanel,  		0,   0,   10,   15, 1, .5, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel,     0,  15,    4,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _dataScrollPane,              0,  17,   20,   15, 0, .8, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel2,    0,  32,    4,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel, 			    0,  34,   20,   20, GridBagConstraints.BOTH );		
	}
	
	private void AddSacParamsListPanelComponents()
	/********************
	Purpose: Add's Components to the JList Scroll Pane Panel 
	*********************/
	{
		_sacParamsListPanel.setLayout( new GridBagLayout() );
																			 //          	X,  Y, #Col,  #Row
		ComponentHelper.addPanelComponent( _sacParamsListPanel, _scrollPane,   				0,  0,   5,    25, 1, 1, GridBagConstraints.BOTH );

	}
	
	
	private void AddSacParamsDataPanelComponents()
	/********************
	Purpose: Add's Label/TextField componenets to the sacParamsTextPanel 
	*********************/
	{
		_sacParamsTextPanel.setLayout( new GridBagLayout() );
		
		java.awt.Color backgroundColor = _validTimeTextField.getBackground();
		_validTimeTextField.setEditable( false );
		_validTimeTextField.setBackground(backgroundColor);
		
		
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _basinIDLabel,         0,    0,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _basinIDTextField,    10,    0,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _sourceLabel,          0,   10,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _sourceTextField,     10,   10,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _validTimeLabel,       0,   20,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _validTimeTextField,  10,   20,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _uztwmLabel,           0,   30,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _uztwmTextField,      10,   30,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _uzfwmLabel,           0,   40,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _uzfwmTextField,      10,   40,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _uzkLabel,             0,   50,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _uzkTextField,        10,   50,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _pctimLabel,           0,   60,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _pctimTextField,      10,   60,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _adimpLabel,           0,   70,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _adimpTextField,      10,   70,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _rivaLabel,            0,   80,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _rivaTextField,       10,   80,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _zpercLabel,           0,   90,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _zpercTextField,      10,   90,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _rexpLabel,            0,  100,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _rexpTextField,       10,  100,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lztwmLabel,           0,  110,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lztwmTextField,      10,  110,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lzfsmLabel,           0,  120,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lzfsmTextField,      10,  120,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lzfpmLabel,           0,  130,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lzfpmTextField,      10,  130,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lzskLabel,            0,  140,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lzskTextField,       10,  140,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lzpkLabel,            0,  150,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _lzpkTextField,       10,  150,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _pfreeLabel,           0,  160,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _pfreeTextField,      10,  160,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _rservLabel,           0,  170,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _rservTextField,      10,  170,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _sideLabel,            0,  180,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _sideTextField,       10,  180,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _peAdjustLabel,        0,  190,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _peAdjustTextField,   10,  190,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _pxAdjustLabel,        0,  200,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _pxAdjustTextField,   10,  200,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _efcLabel,             0,  210,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacParamsTextPanel, _efcTextField,        10,  210,  5,     1, GridBagConstraints.BOTH );
	}
	
	private void AddButtons()
	/********************
	Purpose: add's the buttons to the button panel 
	*********************/
	{
		JPanel verticalSeparatorPanel = new JPanel();
		JPanel verticalSeparatorPanel2 = new JPanel();
		JPanel verticalSeparatorPanel3 = new JPanel();
		JPanel horizontalSeparatorPanel = new JPanel();

		verticalSeparatorPanel.setPreferredSize( new Dimension( 50, 20 ) );
		verticalSeparatorPanel2.setPreferredSize( new Dimension( 50, 20 ) );
		verticalSeparatorPanel3.setPreferredSize( new Dimension( 50, 20 ) );
		horizontalSeparatorPanel.setPreferredSize( new Dimension( 300, 10 ) );

		_buttonPanel.setLayout( new GridBagLayout() );
//																					X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,               0,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, verticalSeparatorPanel,    4,   0,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,             8,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, verticalSeparatorPanel2,  10,   0,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, _clearButton,             14,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, verticalSeparatorPanel3,  16,   0,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, _exitButton,              20,   0,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, horizontalSeparatorPanel,  0,   2,  22,     2, GridBagConstraints.BOTH );  
	}
	
	private void loadTextFields()
	/********************
	Purpose: Load's the appropriate values for the Text Fields 
	*********************/
	{
		if ( _sacSmaParameters != null )
		{
			_basinIDTextField.setText( _sacSmaParameters.getBasinId() );
			_sourceTextField.setText( _sacSmaParameters.getSource() );
			_validTimeTextField.setText( "" + DbTimeHelper.getDateTimeStringFromLongTime( _sacSmaParameters.getValidTime() ) );

			_uztwmTextField.setText( getFormattedValue( _sacSmaParameters.getUztwm() ) );
			_uzfwmTextField.setText( getFormattedValue( _sacSmaParameters.getUzfwm() ) );
			_uzkTextField.setText( getFormattedValue( _sacSmaParameters.getUzk() ) );
			_pctimTextField.setText( getFormattedValue( _sacSmaParameters.getPctim() ) );
			_adimpTextField.setText( getFormattedValue( _sacSmaParameters.getAdimp() ) );
			_rivaTextField.setText( getFormattedValue( _sacSmaParameters.getRiva() ) );
			_zpercTextField.setText( getFormattedValue( _sacSmaParameters.getZperc() ) );
			_rexpTextField.setText( getFormattedValue( _sacSmaParameters.getRexp() ) );
			_lztwmTextField.setText( getFormattedValue( _sacSmaParameters.getLztwm() ) );
			_lzfsmTextField.setText( getFormattedValue( _sacSmaParameters.getLzfsm() ) );
			_lzfpmTextField.setText( getFormattedValue( _sacSmaParameters.getLzfpm() ) );
			_lzskTextField.setText( getFormattedValue( _sacSmaParameters.getLzsk() ) );
			_lzpkTextField.setText( getFormattedValue( _sacSmaParameters.getLzpk() ) );
			_pfreeTextField.setText( getFormattedValue( _sacSmaParameters.getPfree() ) );
			_rservTextField.setText( getFormattedValue( _sacSmaParameters.getRserv() ) );
			_sideTextField.setText( getFormattedValue( _sacSmaParameters.getSide() ) );
			_peAdjustTextField.setText( getFormattedValue( _sacSmaParameters.getPeadj() ) );
			_pxAdjustTextField.setText( getFormattedValue( _sacSmaParameters.getPxadj() ) );
			_efcTextField.setText( getFormattedValue( _sacSmaParameters.getEfc() ) );
		}
		else
		{
			_basinIDTextField.setText( "" );
			_sourceTextField.setText( "" );
			_validTimeTextField.setText( "yyyy-mm-dd hh:mm:ss" );
			_uztwmTextField.setText( "" );		
			_uzfwmTextField.setText( "" );
			_uzkTextField.setText( "" );
			_pctimTextField.setText( "" );
			_adimpTextField.setText( "" );
			_rivaTextField.setText( "" );
			_zpercTextField.setText( "" );
			_rexpTextField.setText( "" );
			_lztwmTextField.setText( "" );
			_lzfsmTextField.setText( "" );
			_lzfpmTextField.setText( "" );
			_lzskTextField.setText( "" );
			_lzpkTextField.setText( "" );
			_pfreeTextField.setText( "" );
			_rservTextField.setText( "" );
			_sideTextField.setText( "" );
			_peAdjustTextField.setText( "" );
			_pxAdjustTextField.setText( "" );
			_efcTextField.setText( "" );
		}
	}
	
	private void AddListeners()
	/********************
	Purpose: Add's listeners to the buttons and TextFields
	*********************/
	{
		_clearButton.addActionListener( new ClearButtonListener() );
		_saveButton.addActionListener( new SaveButtonListener() );
		_deleteButton.addActionListener( new DeleteButtonListener() );
		_validTimeTextField.addMouseListener( new TimeTextFieldListener() );
		WindowCloserListener windowCloser = new WindowCloserListener();
		_exitButton.addActionListener( windowCloser );
		addWindowListener( windowCloser );
		addComponentListener( new WindowResizeComponentListener() );
		_listSelectionModel.addListSelectionListener( new TableUpdateListener() ); 
		_sacSmaParamsTable.getTableHeader().addMouseListener( new TableSortingListener() );
	}
	
	private class TableSortingListener extends MouseAdapter
	{
		public void mouseClicked( MouseEvent e )
		{
			Point point = e.getPoint();
			int column = _sacSmaParamsTable.getTableHeader().columnAtPoint( point );
			
			_tableHelper.setOrderByClause( column );
			refreshSacSmaParamsTable();
		}
	}

	private class TableUpdateListener implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent arg0)
		{
			int index = _sacSmaParamsTable.getSelectedRow();
			if ( ( index >= 0 ) && ( ! _sacParamsList.isEmpty() ) )
			{
				_sacSmaParameters = (SacSmaParameters) _sacParamsList.get( index );
			}
			else
			{
				_sacSmaParameters = null;
			}
			loadTextFields();
		}
	}
	
	private class ClearButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			_sacSmaParameters = null;
			loadTextFields();
		}
	}
	
	private class SaveButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			SacSmaParameters params = new SacSmaParameters();
			
			if ( 
			   ( ! isEmptyString( _basinIDTextField.getText() ) ) &&
			   ( ! isEmptyString( _sourceTextField.getText() ) ) &&
			   ( ! _validTimeTextField.getText().trim().equals( "yyyy-mm-dd hh:mm:ss" ) )  
			   )  // Checks to make sure the basinID and source
			      // are not empty strings
			{
				try
				{
					params.setBasinId( _basinIDTextField.getText().trim() );
					params.setSource( _sourceTextField.getText().trim() );
					params.setValidTime( DbTimeHelper.getLongTimeFromDateTimeString( _validTimeTextField.getText() ) );
					params.setPostingTime( getCurrentTime() );
        			params.setUztwm( Double.parseDouble( _uztwmTextField.getText() ) );
					params.setUzfwm( Double.parseDouble( _uzfwmTextField.getText() ) );
					params.setUzk( Double.parseDouble( _uzkTextField.getText() ) );
					params.setPctim( Double.parseDouble( _pctimTextField.getText() ) );
					params.setAdimp( Double.parseDouble( _adimpTextField.getText() ) );
					params.setRiva( Double.parseDouble( _rivaTextField.getText() ) );
					params.setZperc( Double.parseDouble( _zpercTextField.getText() ) );
					params.setRexp( Double.parseDouble( _rexpTextField.getText() ) );
					params.setLztwm( Double.parseDouble( _lztwmTextField.getText() ) );
					params.setLzfsm( Double.parseDouble( _lzfsmTextField.getText() ) );
					params.setLzfpm( Double.parseDouble( _lzfpmTextField.getText() ) );
					params.setLzsk( Double.parseDouble( _lzskTextField.getText() ) );
					params.setLzpk( Double.parseDouble( _lzpkTextField.getText() ) );
					params.setPfree( Double.parseDouble( _pfreeTextField.getText() ) );
					params.setRserv( Double.parseDouble( _rservTextField.getText() ) );
					params.setSide( Double.parseDouble( _sideTextField.getText() ) );
					params.setPeadj( Double.parseDouble( _peAdjustTextField.getText() ) );
					params.setPxadj( Double.parseDouble( _pxAdjustTextField.getText() ) );
					params.setEfc( Double.parseDouble( _efcTextField.getText() ) );

					_dataMgr.saveParams( params );
					_sacSmaParameters = params;
					refreshSacSmaParamsTable();

					notifyAllApplyActionListeners();  // notifies all the listeners
				}
				catch ( NumberFormatException exception )
				{
                    DialogHelper.displayErrorDialog(null, 
                                                    "There is a parsing error in your input", 
                                                    "Error Message");
                                                    
                 /*                                   
					JOptionPane.showMessageDialog( null, "There is a parsing error in your input", 
												   "Error Message", JOptionPane.ERROR_MESSAGE );
				 */
                }
			}
			else
			{
                DialogHelper.displayErrorDialog(null, "Empty Strings are not allowed", 
                                                "Error Message");
				//JOptionPane.showMessageDialog( null, "Empty Strings are not allowed", 
				//							   "Error Message", JOptionPane.ERROR_MESSAGE );
			}
		}
		
		private long getCurrentTime()
		{
			// Get's the current system time truncated to the nearest second.
			long time = System.currentTimeMillis();
			
			time /= 1000;
			time *= 1000;
			return time;
		}
		
	}

	public void addApplyActionListener(ActionListener listener)
	{
		_applyListenerList.add(listener);
	}

	private void notifyAllApplyActionListeners()
	{
		for (int i = 0; i < _applyListenerList.size(); i++)
		{
			ActionListener listener = (ActionListener) _applyListenerList.get(i);
			ActionEvent event = new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "APPLY action");
			listener.actionPerformed(event);
		}
	}

	private class DeleteButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
            boolean confirm = false;
            
			if ( _sacSmaParameters != null )
			{
                confirm = DialogHelper.displayConfirmDialog(null,
                                                           "Are you sure you want to delete this Parameter?", 
                                                           "Delete Parameter");
                
				if ( confirm )
				/* User selected YES */
				{
					_dataMgr.deleteParams( _sacSmaParameters );
					_sacSmaParameters = null;
					refreshSacSmaParamsTable();
				}
			}
		}
	}
	
	private class DateTimeListener implements DateTimeEventListener
	{
		private JTextField _timeTextField = null;
		
		public DateTimeListener( JTextField timeTextField )
		{
			_timeTextField = timeTextField;
		}
		
		public void dateTimeEntered( DateTimeEvent dateTimeEvent )
		{
			if ( dateTimeEvent.isGoodDate() )
			{
				String timeString = DbTimeHelper.getDateTimeStringFromLongTime( dateTimeEvent.getDateTimeLong() );
				_timeTextField.setText( timeString );
			}
		}
	}
	
	private class TimeTextFieldListener extends MouseAdapter
	{
		public void mousePressed( MouseEvent e )
		{
			long dateTime = 0;
			JTextField textField = (JTextField) e.getSource();
			
			if ( ! textField.getText().equals( "yyyy-mm-dd hh:mm:ss" ) )
			{
				dateTime = DbTimeHelper.getLongTimeFromDateTimeString( textField.getText() );
			}
			else
			{
				dateTime = new Date().getTime();
			}
			DateTimeDialog dateTimeGUI = new DateTimeDialog( SacParamsEditor.this, dateTime, "Valid Time" );
			dateTimeGUI.addDateTimeEventListener( new DateTimeListener( textField ) );
			dateTimeGUI.show();
		}
	}
		
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
	
	private class WindowResizeComponentListener extends ComponentAdapter
	{
		public void componentResized( ComponentEvent e )
		{
			int height = SacParamsEditor.this.getHeight();
			int width = SacParamsEditor.this.getWidth();
//			System.out.println( "Width/Height = [" + width + ", " + height + "]" );

			if ( width < MIN_WIDTH ) 
			{ 
				SacParamsEditor.this.setSize( new Dimension( MIN_WIDTH, height ) );
			}
			if ( width > MAX_WIDTH )
			{
				SacParamsEditor.this.setSize( new Dimension( MAX_WIDTH, height ) );
			}
			if ( height < MIN_HEIGHT )
			{
				SacParamsEditor.this.setSize( new Dimension( width, MIN_HEIGHT ) );
			}
			if ( height > MAX_HEIGHT )
			{
				SacParamsEditor.this.setSize( new Dimension( width, MAX_HEIGHT ) );
			}
		}
	}
	
	
	private void closeWindow()
	/********************
	Purpose: Exit's the program gracefully 
	*********************/
	{
		this.dispose();
	}
	
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
	
	private JLabel getJLabel( String label )
	{
		JLabel returnJLabel = new JLabel( label );
		returnJLabel.setFont( _labelFont );
		return returnJLabel;
	}
	
	private JTextField getJTextField( int size )
	{
		JTextField returnJTextField = new JTextField( size );
		returnJTextField.setFont( _textFieldFont );
		return returnJTextField;
	}

	private JFormattedTextField getJFormattedTextField( String formatString )
	{
		JFormattedTextField returnJFormattedTextField = new JFormattedTextField( formatString );
		returnJFormattedTextField.setFont( _textFieldFont );
		returnJFormattedTextField.setColumns( 17 );
		return returnJFormattedTextField;
	}

	private String getFormattedValue( double valueString )
	{
		return _decimalFormat.format( valueString );
	}

	public static void main(String[] args) 
	{
		SacParamsEditor sacParamsGUI = new SacParamsEditor( new Frame(), new DataMgr(), "BLUZ2" );
		sacParamsGUI.show();
	}
}
