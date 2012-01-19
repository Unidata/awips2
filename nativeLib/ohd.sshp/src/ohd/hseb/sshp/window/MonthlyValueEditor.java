/*
 * Created on Jan 22, 2004
 *
 * Filename : MonthlyValueEditor.java
 * Author   : Gautam Sood
 * Last Revision Date : Jan 22, 2004
 *  
 */
package ohd.hseb.sshp.window;

import ohd.hseb.sshp.DataMgr;
import ohd.hseb.model.MonthlyValues;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.TableHelper;
import ohd.hseb.db.DbTimeHelper;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.List;
import java.util.*;

public class MonthlyValueEditor extends JDialog
{
	private Container _frameContentPane = getContentPane();
	
	private JPanel _monthlyValueListPanel =	 		new JPanel();
	private JPanel _monthlyValueTextPanel = 		new JPanel();
	private JPanel _buttonPanel = 					new JPanel();

	private Font _labelFont = 						new Font( "Helvetica", Font.BOLD, 14 );
	private Font _textFieldFont = 					new Font( "Helvetica", Font.PLAIN, 12 );

	private DataMgr _dataMgr = 						null;
	private MonthlyValues _monthlyValues =			null;
	
	private List _monthlyValuesList	= 				null;
	
	private static final int DEFAULT_WIDTH =		450;
	private static final int DEFAULT_HEIGHT = 		654;
	
	private static final int MAX_WIDTH = 			500;
	private static final int MAX_HEIGHT =			5000;
	
	private static final int MIN_WIDTH = 			430;
	private static final int MIN_HEIGHT = 			500;
	
	private static final int NUMBER_OF_MONTHS = 	12;
	
	private String _pe = 							"EA";
	
	private String[] _durStringArray = 				{ "1 hour", "3 hours", "6 hours", "12 hours", "1 day" };
	private int[] _durDbValueIntArray = 		    { 1001, 1003, 1006, 1012, 2001 };
	private Map _durationMap = 						new HashMap();
		
	private String _basinId = 						null;
	
	private JFrame _owner = 					    null;

	private JLabel _basinIdLabel = 					getJLabel( "Basin ID" );
	private JLabel _peLabel = 						getJLabel( "PE" );
	private JLabel _durLabel = 						getJLabel( "Duration" );
	private JLabel _tsLabel = 						getJLabel( "TS" );
	private JLabel _extremumLabel = 				getJLabel( "Extremum" );
	private JLabel _postingTimeLabel = 				getJLabel( "Posting Time" );
	private JLabel _adjustmentLabel = 				getJLabel( "Adjustment?" );
	private JLabel _janLabel = 						getJLabel( "January" );
	private JLabel _febLabel = 						getJLabel( "February" );
	private JLabel _marLabel = 						getJLabel( "March" );
	private JLabel _aprLabel = 						getJLabel( "April" );
	private JLabel _mayLabel = 						getJLabel( "May" );
	private JLabel _junLabel =						getJLabel( "June" );
	private JLabel _julLabel = 						getJLabel( "July" );
	private JLabel _augLabel = 						getJLabel( "August" );
	private JLabel _sepLabel = 						getJLabel( "September" );
	private JLabel _octLabel = 						getJLabel( "October" );
	private JLabel _novLabel = 						getJLabel( "November" );
	private JLabel _decLabel = 						getJLabel( "December" );
	
	private static final int TEXTFIELD_LENGTH = 	17;
	private JTextField _basinIdTextField = 			getJTextField( TEXTFIELD_LENGTH );
	private JTextField _peTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _tsTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _extremumTextField = 		getJTextField( TEXTFIELD_LENGTH );
	private JTextField _postingTimeTextField = 		getJTextField( TEXTFIELD_LENGTH );
	private JTextField _janTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _febTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _marTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _aprTextField =				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _mayTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _junTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _julTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _augTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _sepTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _octTextField = 				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _novTextField =				getJTextField( TEXTFIELD_LENGTH );
	private JTextField _decTextField = 				getJTextField( TEXTFIELD_LENGTH );
	
	private JCheckBox _adjustmentCheckBox =			new JCheckBox();
	
	private JComboBox _durComboBox = 				new JComboBox( _durStringArray );

	private JScrollPane _dataScrollPane = 			new JScrollPane( _monthlyValueTextPanel );
	
	private JButton _saveButton = 					new JButton( "Save" );
	private JButton _deleteButton = 				new JButton( "Delete" );
	private JButton _exitButton = 					new JButton( "Close" );
	private JButton _clearButton = 					new JButton( "Clear" );

	//used for the Table
	private String[] _columnNames = 				{ "Basin ID", "PE", "DUR", "TS", "Extremum", "Adjustment" };
	private String[] _dbColumnNames = 				{ "lid", "pe", "dur", "ts", "extremum", "adjustment" };
	private String[][] _monthlyValuesStringArray = 	null;
	private JTable _monthlyValuesTable = 			null;
	private JScrollPane _scrollPane = 				null;
	private SshpTableModel _tableModel =    		null;
	private ListSelectionModel _listSelectionModel =null;
	private TableHelper _tableHelper = 				new TableHelper( _dbColumnNames );


	private MonthlyValueEditor( JFrame owner, DataMgr dataMgr )
	{
		this( owner, dataMgr, "", "EA", true );
	}

	private MonthlyValueEditor( JFrame owner, DataMgr dataMgr, String basinId )
	{
		this( owner, dataMgr, basinId, "EA", true );
	}

	public MonthlyValueEditor( JFrame owner, DataMgr dataMgr, String basinId, String pe, boolean isModal )
	{
		super( owner, isModal );
		this.setTitle( "Monthly Value Editor" );
		
		_owner = owner;
		_basinId = basinId;
		_dataMgr = dataMgr;
		_pe = pe;
		initialize();
	}

	private void initialize()
	{
	    
	    long currentTime = System.currentTimeMillis(); 
	     
		setupDurationHashMap();
		setupTable();
				
		AddMonthlyValueListPanelComponents();
		AddMonthlyValueDataPanelComponents();
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

		String whereString = " WHERE pe='" + _pe + "' ";

		whereString += _tableHelper.getOrderByClause();
		_monthlyValuesList = _dataMgr.loadMonthlyValuesList( whereString );

		_monthlyValuesStringArray = convertListToArray();

		_tableModel = new SshpTableModel( _columnNames, _monthlyValuesStringArray );
		_monthlyValuesTable = new JTable( _tableModel );
		_monthlyValuesTable.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
		_monthlyValuesTable.setPreferredScrollableViewportSize( new Dimension( 380, 100 ) );
		_listSelectionModel = _monthlyValuesTable.getSelectionModel();
		_scrollPane = new JScrollPane( _monthlyValuesTable );
		if ( _basinId != null )
		{
			for ( int i = 0; i < _monthlyValuesList.size(); i++ )
			{
				MonthlyValues monthlyValues = (MonthlyValues) _monthlyValuesList.get( i );
	
				if ( _basinId.equalsIgnoreCase( monthlyValues.getBasinId().trim() ) )
				{
					index = i;
					foundBasinId = true;
					_monthlyValues = monthlyValues;
					break;
				}
			}
		}
		
		if ( ! foundBasinId )  // If no basin ID found in the Params list, selects the first one by default.
		{
		    if (_monthlyValuesList.size() > 0)
		    {
			    _monthlyValues = (MonthlyValues) _monthlyValuesList.get( 0 );
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
		Rectangle rect = _monthlyValuesTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();
	}
	
	private void setupDurationHashMap()
	{
		for ( int i = 0; i < _durStringArray.length; i++ )
		{
			Integer key = new Integer( _durDbValueIntArray[ i ] );
			String value = _durStringArray[ i ] ;
			_durationMap.put( key, value );
		}
	}

	private String[][] convertListToArray()
	{
		String[][] dataArray = new String[ _monthlyValuesList.size() ][ _columnNames.length ];
		
		for( int i = 0; i < dataArray.length; i++ )
		{
			Integer durationInteger = null;
			
			MonthlyValues monthlyValues = (MonthlyValues) _monthlyValuesList.get( i );

			int duration = monthlyValues.getDur();
			durationInteger = new Integer( duration );
			String durationString = (String) _durationMap.get( durationInteger );

			dataArray[ i ][ 0 ] = monthlyValues.getBasinId();
			dataArray[ i ][ 1 ] = monthlyValues.getPe();
			dataArray[ i ][ 2 ] = durationString;
			dataArray[ i ][ 3 ] = monthlyValues.getTs();
			dataArray[ i ][ 4 ] = monthlyValues.getExtremum();
			dataArray[ i ][ 5 ] = "" + monthlyValues.isAdjustment();
		}
		return dataArray;
	}
	
	private void refreshMonthlyValuesTable()
	{
		int index = 0;
		
		String whereString = " WHERE pe='" + _pe + "' ";

		whereString += _tableHelper.getOrderByClause();

		_monthlyValuesList = _dataMgr.loadMonthlyValuesList( whereString );

		_monthlyValuesStringArray = convertListToArray();
		if ( _monthlyValues != null )
		{
			for ( int i = 0; i < _monthlyValuesList.size(); i++ )
			{
				MonthlyValues monthlyValues = (MonthlyValues) _monthlyValuesList.get( i );
				
				if ( _monthlyValues.equals( monthlyValues ) )
				{
					index = i;
					break;
				}
			}
		}
		_tableModel.updateData( _monthlyValuesStringArray );
		_tableModel.fireTableChanged( null );
		
		_listSelectionModel.setSelectionInterval( index, index );

		Rectangle rect = _monthlyValuesTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();
	}

	private void AddMonthlyValueListPanelComponents()
	/********************
	Purpose: Add's Components to the JTable Scroll Pane Panel 
	*********************/
	{
		_monthlyValueListPanel.setLayout( new GridBagLayout() );
																			 //          		X,  Y, #Col,  #Row
		ComponentHelper.addPanelComponent( _monthlyValueListPanel, _scrollPane,					0,  0,   5,    25, 0, 1, GridBagConstraints.BOTH );
	}
	
	private void AddMonthlyValueDataPanelComponents()
	/********************
	Purpose: Add's Label/TextField componenets to the _monthlyValueTextPanel 
	*********************/
	{
		_monthlyValueTextPanel.setLayout( new GridBagLayout() );
		
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _basinIdLabel,          0,    0,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _basinIdTextField,     10,    0,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _peLabel,               0,   10,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _peTextField,          10,   10,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _durLabel,              0,   20,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _durComboBox,          10,   20,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _tsLabel,               0,   30,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _tsTextField,          10,   30,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _extremumLabel,         0,   40,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _extremumTextField,    10,   40,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _postingTimeLabel,      0,   50,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _postingTimeTextField, 10,   50,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _adjustmentLabel,       0,   60,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _adjustmentCheckBox,   10,   60,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _janLabel,          	   0,   70,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _janTextField,         10,   70,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _febLabel,          	   0,   80,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _febTextField,         10,   80,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _marLabel,              0,   90,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _marTextField,  	      10,   90,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _aprLabel,       	   0,  100,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _aprTextField,         10,  100,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _mayLabel,         	   0,  110,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _mayTextField,         10,  110,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _junLabel,         	   0,  120,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _junTextField,         10,  120,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _julLabel,         	   0,  130,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _julTextField,         10,  130,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _augLabel,         	   0,  140,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _augTextField,         10,  140,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _sepLabel,          	   0,  150,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _sepTextField,         10,  150,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _octLabel,         	   0,  160,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _octTextField,         10,  160,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _novLabel,         	   0,  170,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _novTextField,         10,  170,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _decLabel,         	   0,  180,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _monthlyValueTextPanel, _decTextField,         10,  180,  5,     1, GridBagConstraints.BOTH );
		
		_postingTimeTextField.setEditable( false );
		_peTextField.setEditable( false );
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
		ComponentHelper.addPanelComponent( _buttonPanel, horizontalSeparatorPanel,  0,   3,  22,     2, GridBagConstraints.BOTH );  
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
		ComponentHelper.addFrameComponent( _frameContentPane, _monthlyValueListPanel,  		0,   0,   10,   15, 1, 1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel,     0,  15,    4,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _dataScrollPane,              0,  17,   20,   15, 0, .5, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel2,    0,  32,    4,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel, 			    0,  34,   20,   20, GridBagConstraints.BOTH );		
	}

	private void loadTextFields()
	/********************
	Purpose: Load's the appropriate values for the Text Fields 
	*********************/
	{
		
		if ( _monthlyValues != null )
		{
			double[] valueArray = _monthlyValues.getValueArray();

			int duration = 0;
			
			_basinIdTextField.setText( _monthlyValues.getBasinId() );
			_postingTimeTextField.setText( "" + DbTimeHelper.getDateTimeStringFromLongTime( _monthlyValues.getPostingTime() ) );
			_peTextField.setText( _monthlyValues.getPe() );
			
			duration = _monthlyValues.getDur();  // Returns either 1001, 1003, 1006, or 1012
			
			_durComboBox.setSelectedIndex( getDurationValueIndex( duration ) );
			 
			_tsTextField.setText( _monthlyValues.getTs() );
			_extremumTextField.setText( _monthlyValues.getExtremum() );
			_adjustmentCheckBox.setSelected( _monthlyValues.isAdjustment() );
			_janTextField.setText( Double.toString( valueArray[ 0 ] ) );
			_febTextField.setText( Double.toString( valueArray[ 1 ] ) );		
			_marTextField.setText( Double.toString( valueArray[ 2 ] ) );
			_aprTextField.setText( Double.toString( valueArray[ 3 ] ) );
			_mayTextField.setText( Double.toString( valueArray[ 4 ] ) );
			_junTextField.setText( Double.toString( valueArray[ 5 ] ) );
			_julTextField.setText( Double.toString( valueArray[ 6 ] ) );
			_augTextField.setText( Double.toString( valueArray[ 7 ] ) );
			_sepTextField.setText( Double.toString( valueArray[ 8 ] ) );
			_octTextField.setText( Double.toString( valueArray[ 9 ] ) );
			_novTextField.setText( Double.toString( valueArray[ 10 ] ) );
			_decTextField.setText( Double.toString( valueArray[ 11 ] ) );
		}
		else
		{
			_basinIdTextField.setText( "" );
			_postingTimeTextField.setText( "Not Posted Yet" );
			_peTextField.setText( _pe );
			_durComboBox.setSelectedIndex( 0 );
			_tsTextField.setText( "" );
			_extremumTextField.setText( "" );
			_adjustmentCheckBox.setSelected( false );
			_janTextField.setText( "" );		
			_febTextField.setText( "" );
			_marTextField.setText( "" );
			_aprTextField.setText( "" );
			_mayTextField.setText( "" );
			_junTextField.setText( "" );
			_julTextField.setText( "" );
			_augTextField.setText( "" );
			_sepTextField.setText( "" );
			_octTextField.setText( "" );
			_novTextField.setText( "" );
			_decTextField.setText( "" );
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
		WindowCloserListener windowCloser = new WindowCloserListener();
		_exitButton.addActionListener( windowCloser );
		addWindowListener( windowCloser );
		addComponentListener( new WindowResizeComponentListener() );
		_listSelectionModel.addListSelectionListener( new TableUpdateListener() ); 
		_monthlyValuesTable.getTableHeader().addMouseListener( new TableSortingListener() );
	}
	
	private class TableSortingListener extends MouseAdapter
	{
		public void mouseClicked( MouseEvent e )
		{
			Point point = e.getPoint();
			int column = _monthlyValuesTable.getTableHeader().columnAtPoint( point );
			
			_tableHelper.setOrderByClause( column );
			refreshMonthlyValuesTable();
		}
	}
	
	private class TableUpdateListener implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent arg0)
		{
			int index = _monthlyValuesTable.getSelectedRow();
			if ( ( index >= 0 ) && ( ! _monthlyValuesList.isEmpty() ) )
			{
				_monthlyValues = (MonthlyValues) _monthlyValuesList.get( index );
			}
			else
			{
				_monthlyValues = null;
			}
			loadTextFields();
		}
	}

	private class SaveButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			
			MonthlyValues monthlyValues = new MonthlyValues();
			double[] valueArray = new double[ NUMBER_OF_MONTHS ];
			
			if ( 
			   ( ! isEmptyString( _basinIdTextField.getText() ) ) &&
			   ( ! isEmptyString( _peTextField.getText() ) ) &&
			   ( ! isEmptyString( _tsTextField.getText() ) ) &&
			   ( ! isEmptyString( _extremumTextField.getText() ) )
			   )  // Checks to make sure the basinID and source
				  // are not empty strings
			{
				try
				{
					int index = 0;
					
					monthlyValues.setBasinId( _basinIdTextField.getText().trim() );
					monthlyValues.setPe( _pe );
					
					index = _durComboBox.getSelectedIndex();
					monthlyValues.setDur( (short) _durDbValueIntArray[ index ] );

					monthlyValues.setTs( _tsTextField.getText().trim() );
					monthlyValues.setExtremum( _extremumTextField.getText().trim() );
					monthlyValues.setAdjustment( _adjustmentCheckBox.isSelected() );
					valueArray[ 0 ] = Double.parseDouble( _janTextField.getText().trim() );
					valueArray[ 1 ] = Double.parseDouble( _febTextField.getText().trim() );
					valueArray[ 2 ] = Double.parseDouble( _marTextField.getText().trim() );
					valueArray[ 3 ] = Double.parseDouble( _aprTextField.getText().trim() );
					valueArray[ 4 ] = Double.parseDouble( _mayTextField.getText().trim() );
					valueArray[ 5 ] = Double.parseDouble( _junTextField.getText().trim() );
					valueArray[ 6 ] = Double.parseDouble( _julTextField.getText().trim() );
					valueArray[ 7 ] = Double.parseDouble( _augTextField.getText().trim() );
					valueArray[ 8 ] = Double.parseDouble( _sepTextField.getText().trim() );
					valueArray[ 9 ] = Double.parseDouble( _octTextField.getText().trim() );
					valueArray[ 10 ] = Double.parseDouble( _novTextField.getText().trim() );
					valueArray[ 11 ] = Double.parseDouble( _decTextField.getText().trim() );
					
					monthlyValues.setValues( valueArray );
					
					_dataMgr.saveMonthlyValues( monthlyValues );
					_monthlyValues = monthlyValues;
					refreshMonthlyValuesTable();			
				}
				catch ( NumberFormatException exception )
				{
                    
                    DialogHelper.displayMessageDialog(null,
                                                      "There is a parsing error in your input", 
                                                      "Error Message" );
                    
					//JOptionPane.showMessageDialog( null, "There is a parsing error in your input", 
					//							   "Error Message", JOptionPane.ERROR_MESSAGE );
				}
			}
			else
			{
                DialogHelper.displayMessageDialog(null,
                                                  "Empty Strings are not allowed", 
                                                  "Error Message" );
                                                  
				//JOptionPane.showMessageDialog( null, "Empty Strings are not allowed", 
				//							   "Error Message", JOptionPane.ERROR_MESSAGE );
			}
		}
	}

	private class ClearButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			_monthlyValues = null;
			loadTextFields();
		}
	}

	private class DeleteButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			boolean confirm = false;
			if ( _monthlyValues != null )
			{
                confirm = DialogHelper.displayConfirmDialog(null, "Are you sure you want to delete this Monthly Value?",
                                                            "Delete Monthly Value");
                /*
				choice = JOptionPane.showConfirmDialog( null, "Are you sure you want to delete this Monthly Value?", 
														"Delete Monthly Value", JOptionPane.YES_NO_OPTION, 
														JOptionPane.WARNING_MESSAGE );
				*/
                //if ( choice == JOptionPane.YES_OPTION )
				/* User selected YES */
                
                if (confirm)
				{
					_dataMgr.deleteMonthlyValues( _monthlyValues );
					_monthlyValues = null;
					refreshMonthlyValuesTable();
				}
			}
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
			int height = MonthlyValueEditor.this.getHeight();
			int width = MonthlyValueEditor.this.getWidth();
//			System.out.println( "Width/Height = [" + width + ", " + height + "]" );

			if ( width < MIN_WIDTH ) 
			{ 
				MonthlyValueEditor.this.setSize( new Dimension( MIN_WIDTH, height ) );
			}
			if ( width > MAX_WIDTH )
			{
				MonthlyValueEditor.this.setSize( new Dimension( MAX_WIDTH, height ) );
			}
			if ( height < MIN_HEIGHT )
			{
				MonthlyValueEditor.this.setSize( new Dimension( width, MIN_HEIGHT ) );
			}
			if ( height > MAX_HEIGHT )
			{
				MonthlyValueEditor.this.setSize( new Dimension( width, MAX_HEIGHT ) );
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
	
	private int getDurationValueIndex( int duration )
	{
		int index = 0;
		
		for( int i = 0; i < _durDbValueIntArray.length; i++ )
		{
			if ( _durDbValueIntArray [ i ] == duration )
			{
				index = i;
			}

		}
		return index;		
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

	public static void main( String[] args )
	{
		String basin_Id = "BLUO2";
		JFrame frame = new JFrame();
		DataMgr dataMgr = new DataMgr();
		
		MonthlyValueEditor editor = new MonthlyValueEditor( frame, dataMgr, basin_Id, "EA", true );
		editor.setVisible(true);
	}
}