/*
 * Created on Oct 23, 2003
 *
 * Filename : SacStateEditor.java
 * Author   : Gautam Sood
 * Last Revision Date : Oct 23, 2003
 * 
 *  
 */
package ohd.hseb.sshp.window;


import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.TableColumn;

import java.awt.*;
import java.awt.event.*;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.gui.*;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.sshp.DataMgr;

import java.text.DecimalFormat;
import java.util.List;
import java.util.*;

public class SacStateEditor extends JDialog
{
	private Container _frameContentPane = 		getContentPane();
	
	private JPanel _sacStateListPanel = 		new JPanel();
	private JPanel _sacStateTextPanel = 		new JPanel();
	private JPanel _buttonPanel = 				new JPanel();
	
	private Font _labelFont = 					new Font( "Helvetica", Font.BOLD, 14 );
	private Font _textFieldFont = 				new Font( "Helvetica", Font.PLAIN, 12 );

	private List _sacStateList = 				null;
	
	private DataMgr _dataMgr =					null;
	private SacSmaState _sacSmaState = 			null;
	
	private static final int DEFAULT_WIDTH =	340;
	private static final int DEFAULT_HEIGHT = 	470;
	
	private static final int MAX_WIDTH = 		500;
	private static final int MAX_HEIGHT =		5000;

	private static final int MIN_WIDTH = 		340;
	private static final int MIN_HEIGHT = 		450;

	
	//JFrame owner
	private JFrame _owner = null;
	
	//used for JFormattedTextFields
	private static final String _threeDecimalFormatString = 	"0.000";
	private DecimalFormat _decimalFormat = 			new DecimalFormat( _threeDecimalFormatString );

	private String _basinId = 					null;
	
	private JLabel _basinIDLabel = 				getJLabel( "Basin ID" );
	private JLabel _sourceLabel = 				getJLabel( "Source" );
	private JLabel _validTimeLabel = 			getJLabel( "Valid Time" );
	private JLabel _basisTimeLabel = 			getJLabel( "Basis Time" );
	private JLabel _uztwcLabel =				getJLabel( "UZTWC" );
	private JLabel _uzfwcLabel = 				getJLabel( "UZFWC" );
	private JLabel _lztwcLabel = 				getJLabel( "LZTWC" );
	private JLabel _lzfscLabel = 				getJLabel( "LZFSC" );
	private JLabel _lzfpcLabel = 				getJLabel( "LZFPC" );
	private JLabel _adimcLabel = 				getJLabel( "ADIMC" );	

	private JTextField _basinIDTextField = 		getJTextField( 17 );
	private JTextField _sourceTextField = 		getJTextField( 17 );
	
//	private JTextField _validTimeTextField = 	getJTextField( 17 );
//	private JTextField _basisTimeTextField =    getJTextField( 17 );
	
	private DateTimeTextField _validTimeTextField = null;
	private DateTimeTextField _basisTimeTextField = null;
	
	private JFormattedTextField _uztwcTextField =		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _uzfwcTextField = 		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _lztwcTextField = 		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _lzfscTextField = 		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _lzfpcTextField = 		getJFormattedTextField( _threeDecimalFormatString );
	private JFormattedTextField _adimcTextField = 		getJFormattedTextField( _threeDecimalFormatString );

	private JButton _saveButton =				new JButton( "Save" );
	private JButton _deleteButton = 			new JButton( "Delete" );
	private JButton _exitButton = 				new JButton( "Close" );
	private JButton _clearButton = 				new JButton( "Clear" );
	
	//used for custom notification
	private List _applyListenerList =			new ArrayList();

	//used for the Table
	private String[] _columnNames = 			{ "Basin ID", "Source", "Valid Time" };
	private String[][] _sacStateStringArray = 	null;
	private JTable _sacSmaStateTable = 			null;
	private JScrollPane _scrollPane = 			null;
	private SshpTableModel _tableModel =    	null;
	private ListSelectionModel _listSelectionModel = 		null;

	//used for sorting purposes
	private String[] _dbColumnNames =			{ "basin_id", "source", "validtime" };
	private TableHelper _tableHelper = 			new TableHelper( _dbColumnNames );

	public SacStateEditor( JFrame owner, DataMgr dataMgr )
	/********************
	Purpose: Constructor 
	*********************/
	{
		this( owner, dataMgr, null, false );
	}

	public SacStateEditor( JFrame owner, DataMgr dataMgr, String basinId )
	/********************
	Purpose: Constructor 
	*********************/
	{
		this( owner, dataMgr, basinId, false );
	}
	
	public SacStateEditor( JFrame owner, DataMgr dataMgr, String basinId, boolean isModal )
	/********************
	Purpose: Constructor 
	*********************/
	{
		super( owner, isModal );
		this.setTitle( "SacSma State" );
		
		_owner = owner;
		_basinId = basinId;
		_dataMgr = dataMgr;
		initialize();
	}

	private void initialize()
	/********************
	Purpose: Initializes the GUI by calling the appropriate methods  
	*********************/
	{
	    
	    long latestHourTime = TimeHelper.truncateTimeInMillisToNearestHour(System.currentTimeMillis(),1);
	    
	    _validTimeTextField  = new DateTimeTextField(latestHourTime, _owner, "Valid Time", 17);
	    _basisTimeTextField  = new DateTimeTextField(latestHourTime, _owner, "Basis Time", 17);
	      	
	        
		setupTable();
		
		addSacStateListPanelComponents();
		addSacStateDataPanelComponents();
		addButtons();
		addFrameComponents();	
		addListeners();

		setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );
	}

	private void setupTable()
	{
		int index = -1;
		boolean foundBasinId= false;
		
		_tableHelper.setOrderByClause( 0 );
		_sacStateList = _dataMgr.loadSacSmaStateList( _tableHelper.getOrderByClause() );
		_sacStateStringArray = convertListToArray();

		_tableModel = new SshpTableModel( _columnNames, _sacStateStringArray );
		_sacSmaStateTable = new JTable( _tableModel );
		_sacSmaStateTable.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
		_sacSmaStateTable.setPreferredScrollableViewportSize( new Dimension( 300, 100 ) );
		_listSelectionModel = _sacSmaStateTable.getSelectionModel();
		_scrollPane = new JScrollPane( _sacSmaStateTable );
		if ( _basinId != null )
		{
			for ( int i = 0; i < _sacStateList.size(); i++ )
			{
				SacSmaState sacSmaState = (SacSmaState) _sacStateList.get( i );
	
				if ( _basinId.equalsIgnoreCase( sacSmaState.getBasinId().trim() ) )
				{
					index = i;
					foundBasinId = true;
					_sacSmaState = sacSmaState;
					break;
				}
			}
		}

		if ( ! foundBasinId )  // If no basin ID found in the Params list, selects the first one by default.
		{
		    if (_sacStateList.size() > 0)
		    {
			    _sacSmaState = (SacSmaState) _sacStateList.get( 0 );
		    }
		}

		for( int i = 0; i < _columnNames.length; i++ )
		{
			TableColumn column = _sacSmaStateTable.getColumnModel().getColumn( i );
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
		Rectangle rect = _sacSmaStateTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();

	}

	private void refreshSacSmaStateTable()
	{
		int index = 0;

		_sacStateList = _dataMgr.loadSacSmaStateList( _tableHelper.getOrderByClause() );

		_sacStateStringArray = convertListToArray();
		
		if ( _sacSmaState != null )
		{
			for ( int i = 0; i < _sacStateList.size(); i++ )
			{
				SacSmaState sacSmaState = (SacSmaState) _sacStateList.get( i );
				
				if ( _sacSmaState.equals( sacSmaState ) )
				{
					index = i;
					break;
				}
			}
		}

		_tableModel.updateData( _sacStateStringArray );
		_tableModel.fireTableChanged( null );

		for( int i = 0; i < _columnNames.length; i++ )
		{
			TableColumn column = _sacSmaStateTable.getColumnModel().getColumn( i );
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

		Rectangle rect = _sacSmaStateTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();
	}
	
	private void addFrameComponents()
	/********************
	Purpose: Add's the panels to the Frame 
	*********************/
	{
		JPanel horizontalSeparatorPanel = new JPanel();
		JPanel horizontalSeparatorPanel2 = new JPanel();
		_frameContentPane.setLayout( new GridBagLayout() );

		horizontalSeparatorPanel.setPreferredSize( new Dimension( 300, 10 ) );
		horizontalSeparatorPanel2.setPreferredSize( new Dimension( 300, 30 ) );

//																						    X,   Y,  #Col,  #Row
		ComponentHelper.addFrameComponent( _frameContentPane, _sacStateListPanel,  			0,   0,   10,   15, 1, 1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel,     0,  15,    4,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _sacStateTextPanel,           0,  17,   20,   15, 0, .4, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel2,    0,  32,    4,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel, 			    0,  34,   20,   20, GridBagConstraints.BOTH );		
	}
	
	private String[][] convertListToArray()
	{
		String[][] dataArray = new String[ _sacStateList.size() ][ _columnNames.length ];
		
		for( int i = 0; i < dataArray.length; i++ )
		{
			SacSmaState sacState = (SacSmaState) _sacStateList.get( i );
			
			dataArray[ i ][ 0 ] = sacState.getBasinId();
			dataArray[ i ][ 1 ] = sacState.getSource();
			dataArray[ i ][ 2 ] = "" + DbTimeHelper.getDateTimeStringFromLongTime( sacState.getValidTime() );
		}
		return dataArray;
	}
	
	
	private void addSacStateListPanelComponents()
	/********************
	Purpose: Add's Components to the JTable Scroll Pane Panel 
	*********************/
	{
		_sacStateListPanel.setLayout( new GridBagLayout() );
		
		ComponentHelper.addPanelComponent( _sacStateListPanel, _scrollPane, 			 0,   0,    5,   25, 1, 1, GridBagConstraints.BOTH );

	}

	
	private void addSacStateDataPanelComponents()
	/********************
	Purpose: Add's Label/TextField componenets to the sacStateTextPanel 
	*********************/
	{
		_sacStateTextPanel.setLayout( new GridBagLayout() );

		_validTimeTextField.setEditable( false );
		_basisTimeTextField.setEditable( false );
//																				      X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _basinIDLabel,         0,    0,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _basinIDTextField,    10,    0,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _sourceLabel,          0,   10,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _sourceTextField,     10,   10,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _validTimeLabel,       0,   20,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _validTimeTextField,  10,   20,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _basisTimeLabel,       0,   30,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _basisTimeTextField,  10,   30,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _uztwcLabel,           0,   40,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _uztwcTextField,      10,   40,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _uzfwcLabel,           0,   50,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _uzfwcTextField,      10,   50,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _lztwcLabel,           0,   60,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _lztwcTextField,      10,   60,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _lzfscLabel,           0,   70,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _lzfscTextField,      10,   70,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _lzfpcLabel,           0,   80,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _lzfpcTextField,      10,   80,  5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _adimcLabel,           0,   90,  5,     1, GridBagConstraints.BOTH ); 
		ComponentHelper.addPanelComponent( _sacStateTextPanel, _adimcTextField,      10,   90,  5,     1, GridBagConstraints.BOTH );
	}
	
	private void addButtons()
	/********************
	Purpose: add's the buttons to the button panel 
	*********************/
	{
		JPanel verticalSeparatorPanel = new JPanel();
		JPanel verticalSeparatorPanel2 = new JPanel();
		JPanel verticalSeparatorPanel3 = new JPanel();
		JPanel horizontalSeparatorPanel = new JPanel();

		_buttonPanel.setLayout( new GridBagLayout() );
		verticalSeparatorPanel.setPreferredSize( new Dimension( 50, 20 ) );
		verticalSeparatorPanel2.setPreferredSize( new Dimension( 50, 20 ) );
		verticalSeparatorPanel3.setPreferredSize( new Dimension( 50, 20 ) );
		horizontalSeparatorPanel.setPreferredSize( new Dimension( 300, 20 ) );

//																					X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,               2,   2,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, verticalSeparatorPanel,    4,   3,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,             8,   2,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, verticalSeparatorPanel2,  10,   3,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, _clearButton,             14,   2,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, verticalSeparatorPanel3,  16,   3,   4,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, _exitButton,              20,   2,   2,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, horizontalSeparatorPanel,  0,   4,  22,     2, GridBagConstraints.BOTH );
	}
	
	
	private void loadTextFields()
	/********************
	Purpose: Load's the appropriate values for the Text Fields 
	*********************/
	{
		if ( _sacSmaState != null )
		{
			_basinIDTextField.setText( _sacSmaState.getBasinId() );
			_sourceTextField.setText( _sacSmaState.getSource() );
			_validTimeTextField.setText( "" + DbTimeHelper.getDateTimeStringFromLongTime( _sacSmaState.getValidTime() ) );
			_basisTimeTextField.setText( "" + DbTimeHelper.getDateTimeStringFromLongTime( _sacSmaState.getBasisTime() ) );
			_uztwcTextField.setText( getFormattedValue( _sacSmaState.getUztwc() ) );
			_uzfwcTextField.setText( getFormattedValue( _sacSmaState.getUzfwc() ) );
			_lztwcTextField.setText( getFormattedValue( _sacSmaState.getLztwc() ) );
			_lzfscTextField.setText( getFormattedValue( _sacSmaState.getLzfsc() ) );
			_lzfpcTextField.setText( getFormattedValue( _sacSmaState.getLzfpc() ) );
			_adimcTextField.setText( getFormattedValue( _sacSmaState.getAdimc() ) );
		}
		else
		{
			_basinIDTextField.setText( "" );
			_sourceTextField.setText( "" );
			_validTimeTextField.setText( "yyyy-mm-dd hh:mm:ss" );
			_basisTimeTextField.setText( "yyyy-mm-dd hh:mm:ss" );
			_uztwcTextField.setText( "" );
			_uzfwcTextField.setText( "" );
			_lztwcTextField.setText( "" );
			_lzfscTextField.setText( "" );
			_lzfpcTextField.setText( "" );
			_adimcTextField.setText( "" );
		}
	}
	
	private void addListeners()
	/********************
	Purpose: Add's listeners to the buttons and TextFields 
	*********************/
	{
		_clearButton.addActionListener( new ClearButtonListener() );
		_saveButton.addActionListener( new SaveButtonListener() );
		_deleteButton.addActionListener( new DeleteButtonListener() );
	//	_basisTimeTextField.addMouseListener( new TimeTextFieldListener() );
	//	_validTimeTextField.addMouseListener( new TimeTextFieldListener() );
		WindowCloserListener windowCloser = new WindowCloserListener();
		_exitButton.addActionListener( windowCloser ); 
		addWindowListener( windowCloser );
		addComponentListener( new WindowResizeComponentListener() );
		_listSelectionModel.addListSelectionListener( new TableUpdateListener() ); 
		_sacSmaStateTable.getTableHeader().addMouseListener( new TableSortingListener() );
	}
	
	private class TableUpdateListener implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent arg0)
		{
			int index = _sacSmaStateTable.getSelectedRow();
			if ( ( index >= 0 ) && ( ! _sacStateList.isEmpty() ) )
			{
				_sacSmaState = (SacSmaState) _sacStateList.get( index );
			}
			else
			{
				_sacSmaState = null;
			}
			loadTextFields();
		}
	}
	
	private class TableSortingListener extends MouseAdapter
	{
		public void mouseClicked( MouseEvent e )
		{
			Point point = e.getPoint();
			int column = _sacSmaStateTable.getTableHeader().columnAtPoint( point );
			
			_tableHelper.setOrderByClause( column );
			refreshSacSmaStateTable();
		}
	}


	private class SaveButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			SacSmaState state = new SacSmaState();
			
			if ( 
			   ( ! isEmptyString( _basinIDTextField.getText() ) ) &&
			   ( ! isEmptyString( _sourceTextField.getText() ) ) &&
			   ( ! _validTimeTextField.getText().trim().equals( "yyyy-mm-dd hh:mm:ss" ) ) &&
			   ( ! _basisTimeTextField.getText().trim().equals( "yyyy-mm-dd hh:mm:ss" ) ) 
			   )
			{
				try
				{
					state.setBasinId( _basinIDTextField.getText().trim() );
					state.setSource( _sourceTextField.getText().trim() );
					state.setValidTime( DbTimeHelper.getLongTimeFromDateTimeString( 
					                        _validTimeTextField.getText().trim() ) );
					state.setBasisTime( DbTimeHelper.getLongTimeFromDateTimeString(
											_basisTimeTextField.getText().trim() ) );
					state.setPostingTime( getCurrentTime() );
					state.setUztwc( Double.parseDouble( _uztwcTextField.getText() ) );
					state.setUzfwc( Double.parseDouble( _uzfwcTextField.getText() ) );
					state.setLztwc( Double.parseDouble( _lztwcTextField.getText() ) );
					state.setLzfsc( Double.parseDouble( _lzfscTextField.getText() ) );
					state.setLzfpc( Double.parseDouble( _lzfpcTextField.getText() ) );
					state.setAdimc( Double.parseDouble( _adimcTextField.getText() ) );
					_dataMgr.saveState( state );	
					_sacSmaState = state;
					refreshSacSmaStateTable();
					notifyAllApplyActionListeners();  // notifies all the listeners

				}
				catch ( NumberFormatException exception )
				{
                    DialogHelper.displayMessageDialog(null,
                                                      "There is a parsing error in your input", 
                                                      "Error Message");
					//JOptionPane.showMessageDialog( null, "There is a parsing error in your input", 
					//							   "Error Message", JOptionPane.ERROR_MESSAGE );
				}
			}
			else
			{
                DialogHelper.displayMessageDialog(null,
                                                 "Empty Strings/Dates are not allowed", 
                                                 "Error Message");
                                                 
				//JOptionPane.showMessageDialog( null, "Empty Strings/Dates are not allowed", 
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
			if ( _sacSmaState != null )
			{
				//choice = JOptionPane.showConfirmDialog( null, "Are you sure you want to delete this State?", 
                 //                                       "Delete State", JOptionPane.YES_NO_OPTION, 
                //                                        JOptionPane.WARNING_MESSAGE );
				confirm = DialogHelper.displayConfirmDialog(null,
                                                 "Are you sure you want to delete this State?", 
                                                 "Delete State" );
                
                if ( confirm )
				{
					_dataMgr.deleteState( _sacSmaState );
					_sacSmaState = null;
					refreshSacSmaStateTable();
				}
			}
		}
	}
	
	private class ClearButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			_sacSmaState = null;
			loadTextFields();
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

	/*
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
			DateTimeDialog dateTimeGUI = null;
			
			if ( textField == _basisTimeTextField )
			{
				dateTimeGUI = new DateTimeDialog( SacStateEditor.this, dateTime, "Basis Time" );
			}
			else
			{
				dateTimeGUI = new DateTimeDialog( SacStateEditor.this, dateTime, "Valid Time" );
			}
			dateTimeGUI.addDateTimeEventListener( new DateTimeListener( textField ) );
			dateTimeGUI.show();
		}
	}
	*/

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
			int height = SacStateEditor.this.getHeight();
			int width = SacStateEditor.this.getWidth();

//			System.err.println( width + "\n" + height );			
			if ( width < MIN_WIDTH ) 
			{ 
				SacStateEditor.this.setSize( new Dimension( MIN_WIDTH, height ) );
			}
			if ( width > MAX_WIDTH )
			{
				SacStateEditor.this.setSize( new Dimension( MAX_WIDTH, height ) );
			}
			if ( height < MIN_HEIGHT )
			{
				SacStateEditor.this.setSize( new Dimension( width, MIN_HEIGHT ) );
			}
			if ( height > MAX_HEIGHT )
			{
				SacStateEditor.this.setSize( new Dimension( width, MAX_HEIGHT ) );
			}
		}
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

	private void closeWindow()
	/********************
	Purpose: closes the window 
	*********************/
	{
		this.dispose();
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
		SacStateEditor sacStateGUI = new SacStateEditor( new JFrame(), new DataMgr(), "BLUO2" );
		sacStateGUI.show();
	}
}
