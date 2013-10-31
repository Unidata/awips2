/*
 * Created on Feb 4, 2004
 *
 * Filename : SSHPConfigDialog.java 
 * Author   : Gautam Sood
 * Last Revision Date : Feb 4, 2004
 *  
 */

package ohd.hseb.sshp.window;

import ohd.hseb.model.ForecastInterpolationMethod;
import ohd.hseb.model.RainfallRunoffModelType;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.sshp.SSHPConfig;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.TableHelper;
import ohd.hseb.db.DbTimeHelper;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.*;
import javax.swing.table.TableColumn;

import java.util.List;

public class SSHPConfigDialog extends JDialog
{
    private static final int MAX_BLENDING_HOURS = 1000;
    
	private Container _frameContentPane = getContentPane();
	
	private JPanel _sshpConfigListPanel =	 		new JPanel();
	private JPanel _sshpConfigTextPanel = 			new JPanel();
	private JPanel _buttonPanel = 					new JPanel();
	
	private Font _labelFont = 						new Font( "Helvetica", Font.BOLD, 12 );
	private Font _textFieldFont = 					new Font( "Helvetica", Font.PLAIN, 12 );

	private DataMgr _dataMgr = 						null;
	private SSHPConfig _sshpConfig =			    null;

	private List _sshpConfigList = 					null;
	
	private static final int DEFAULT_WIDTH =		850;
	private static final int DEFAULT_HEIGHT = 		654;
	
	private static final int MAX_WIDTH = 			850;
	private static final int MAX_HEIGHT =			5000;
	
	private static final int MIN_WIDTH = 			750;
	private static final int MIN_HEIGHT = 			500;

	private String[] _sourcePrefStringArray =       { "LOCAL", "RFC" };
	private String[] _modelPrefStringArray =        { RainfallRunoffModelType.API_MKC.getName(),
                                                      RainfallRunoffModelType.SAC_SMA.getName() };
	private String[] _blendMethodStringArray = 		{ "DIFF", "RATIO" };

	private String _lid = 							null;
//  JComponents

//  JLabels for the Text Panel		
	private JLabel _lidLabel = 						getJLabel( "LID" );
	private JLabel _basinIdLabel = 					getJLabel( "Basin ID" );
	private JLabel _postingTimeLabel = 				getJLabel( "Posting Time" );
	private JLabel _modelPrefLabel = 				getJLabel( "Model Preference" );
	
	// the subpanel for the SAC-SMA settings
	private Border _panelBorder = BorderFactory.createLineBorder(Color.black);  
	private JPanel _sacSettingsPanel = 				new JPanel();
	private JLabel _sacSettingsPanelLabel = 		getJLabel("SAC-SMA settings");
	
	private JLabel _autoProcessLabel = 				getJLabel( "Update States Locally?" );
	private JLabel _sourcePrefLabel = 				getJLabel( "Data Source Preference" );
	private JLabel _useStaticEvapLabel = 			getJLabel( "Use ET Demand Curve?" );
	
	private JLabel _blendHoursLabel = 				getJLabel("Blending Hours");
	private JLabel _useBlendLabel	=				getJLabel("Use Adjustment?");
	private JLabel _blendMethodLabel	=			getJLabel("Adjustment Method");

// JTextFields for the Data Panel
	private JTextField _lidTextField = 				getJTextField( 17 );
	private JTextField _basinIdTextField = 			getJTextField( 17 );
	private JTextField _postingTimeTextField = 		getJTextField( 17 );
	
	private JSpinner _blendHoursSpinner =		    null;

//  JCheckboxes for the Data Panel
	private JCheckBox _autoProcessCheckBox = 		new JCheckBox();
	private JCheckBox _useStaticEvapCheckBox = 		new JCheckBox();
	private JCheckBox _useBlendCheckBox = 			new JCheckBox();

//	JComboboxes for the Data Panel
	private JComboBox _sourcePrefComboBox = 		new JComboBox( _sourcePrefStringArray );
	private JComboBox _modelPrefComboBox = 			new JComboBox( _modelPrefStringArray );
	private JComboBox _blendMethodComboBox	=		new JComboBox( _blendMethodStringArray );
	
	private JScrollPane _dataScrollPane = 			new JScrollPane( _sshpConfigTextPanel );
	
	private JButton _saveButton = 					new JButton( "Save" );
	private JButton _deleteButton = 				new JButton( "Delete" );
	private JButton _exitButton = 					new JButton( "Close" );
	private JButton _clearButton = 					new JButton( "Clear" );

	//used for the Table
	private String[] _columnNames = 				{ "LID", "Basin ID", "Model Pref", 
	        										  "Update States Locally?", "Source Pref", 
	        										  "ET-Demand Curve?", "Use Adjust?",
	        										  "Adjust Method", "Blend Hours"};
	
	private String[][] _sshpConfigStringArray = 	null;
	private JTable _sshpConfigTable = 				null;
	private JScrollPane _scrollPane = 				null;
	private SshpTableModel _tableModel =    		null;
	private ListSelectionModel _listSelectionModel =null;

	//used for sorting the Table
	private String[] _dbColumnNames =				{ "lid", "basin_id", "model_pref", "auto_process", "source_pref", "use_static_evap" };
	private TableHelper _tableHelper = 				new TableHelper( _dbColumnNames );

	public SSHPConfigDialog( Frame owner, DataMgr dataMgr )
	{
		this( owner, dataMgr, null, true );
	}
	
	public SSHPConfigDialog( Frame owner, DataMgr dataMgr, String lid )
	{
		this( owner, dataMgr, lid, true );
	}
	
	public SSHPConfigDialog( Frame owner, DataMgr dataMgr, String lid, boolean isModal )
	{
		super( owner, isModal );
		this.setTitle( "SSHP Config" );
		_dataMgr = dataMgr;
		_lid = lid;
		
		
		//set up the spinner control for time
		int initialValue = 0;
	    int min = 0;
        int max = MAX_BLENDING_HOURS;
        int step = 6;
  
        SpinnerModel model = new SpinnerNumberModel(initialValue, min, max, step);
        _blendHoursSpinner = new JSpinner(model);

		
		initialize();
	}
	
	private void initialize()
	{
	    
		setupTable();
		
		
		addSSHPConfigListPanelComponents();
		addSSHPConfigDataPanelComponents();
		addButtons();
		addFrameComponents();
		addListeners();
		
		refreshSSHPConfigTable();
		
		setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );
	}
	
	private void setupTable()
	/********************
	Purpose: Sets up the JTable 
	*********************/
	{
		int index = -1;
		boolean foundLid= false;

		_tableHelper.setOrderByClause( 0 );

		_sshpConfigList = _dataMgr.loadSSHPConfigList( _tableHelper.getOrderByClause() );
		_sshpConfigStringArray = convertListToArray();

		_tableModel = new SshpTableModel( _columnNames, _sshpConfigStringArray );
		_sshpConfigTable = new JTable( _tableModel );
		_sshpConfigTable.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
		_sshpConfigTable.setPreferredScrollableViewportSize( new Dimension( 600, 100 ) );
		_listSelectionModel = _sshpConfigTable.getSelectionModel();
		_scrollPane = new JScrollPane( _sshpConfigTable );
		if ( _lid != null )
		{
			for ( int i = 0; i < _sshpConfigList.size(); i++ )
			{
				SSHPConfig sshpConfig = (SSHPConfig) _sshpConfigList.get( i );
	
				if ( _lid.equalsIgnoreCase( sshpConfig.getLid().trim() ) )
				{
					index = i;
					foundLid = true;
					_sshpConfig = sshpConfig;
					break;
				}
			}
		}
		
		adjustColumnWidths();
		
	

		
		if ( foundLid )  // Checks if a Lid was found
		{
			_listSelectionModel.setSelectionInterval( index, index );
		}
		else
		{
			_listSelectionModel.setSelectionInterval( 0, 0 );
		}
		Rectangle rect = _sshpConfigTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();
	}
	
	private void adjustColumnWidths()
	{
		for( int i = 0; i < _columnNames.length; i++ )
		{
			TableColumn column = _sshpConfigTable.getColumnModel().getColumn( i );
			if ( ( i == 3 ) || ( i == 5 ) )
			{
				column.setPreferredWidth( 125 );
			}
			else
			{
				column.setPreferredWidth( 75 );
			}
		}    
	    
	}
	
	private void refreshSSHPConfigTable()
	{
		int index = 0;
		
		_sshpConfigList = _dataMgr.loadSSHPConfigList( _tableHelper.getOrderByClause() );

		_sshpConfigStringArray = convertListToArray();
		if ( _sshpConfig != null )
		{
			for ( int i = 0; i < _sshpConfigList.size(); i++ )
			{
				SSHPConfig sshpConfig = (SSHPConfig) _sshpConfigList.get( i );
				
				if ( _sshpConfig.equals( sshpConfig ) )
				{
					index = i;
					break;
				}
			}
		}
		
		_tableModel.updateData( _sshpConfigStringArray );
		_tableModel.fireTableChanged( null );

		adjustColumnWidths();
	
		
		_listSelectionModel.setSelectionInterval( index, index );

		Rectangle rect = _sshpConfigTable.getCellRect(index, 0, true);
		_scrollPane.getViewport().setViewPosition(rect.getLocation());
		loadTextFields();

	}

	private String[][] convertListToArray()
	{
		String[][] dataArray = new String[ _sshpConfigList.size() ][ _columnNames.length ];
		
		for( int i = 0; i < dataArray.length; i++ )
		{
			SSHPConfig sshpConfig = (SSHPConfig) _sshpConfigList.get( i );
			
			dataArray[ i ][ 0 ] = sshpConfig.getLid();
			dataArray[ i ][ 1 ] = sshpConfig.getBasinId();
			dataArray[ i ][ 2 ] = sshpConfig.getModelPref();
			dataArray[ i ][ 3 ] = "" + sshpConfig.isAutoProcess();
			dataArray[ i ][ 4 ] = sshpConfig.getSourcePref();
			dataArray[ i ][ 5 ] = "" + sshpConfig.useStaticEvap();
			dataArray[ i ][ 6 ] = "" + sshpConfig.useBlend();
			dataArray[ i ][ 7 ] = sshpConfig.getBlendMethod().getName();
			dataArray[ i ][ 8 ] = "" + sshpConfig.getBlendHours();
		}
		return dataArray;
	}

	
	private void addSSHPConfigListPanelComponents()
	/********************
	Purpose: Add's Components to the JList Scroll Pane Panel 
	*********************/
	{
		_sshpConfigListPanel.setLayout( new GridBagLayout() );
		
		ComponentHelper.addPanelComponent( _sshpConfigListPanel, _scrollPane,               0,  0,  2,    12, 1, 1, GridBagConstraints.BOTH );           		
	}
	
	private void addSSHPConfigDataPanelComponents()
	/********************
	Purpose: Add's Label/TextField componenets to the _sshpConfigTextPanel 
	*********************/
	{
		_sshpConfigTextPanel.setLayout( new GridBagLayout() );
	
//	                                                                      			   	XCol, YRow,  ColCount, RowCount		
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _lidLabel,       	 	 0,    0,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _lidTextField,     	 1,    0,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _basinIdLabel,          0,    1,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _basinIdTextField,      1,    1,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _modelPrefLabel,        0,    2,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _modelPrefComboBox,     1,    2,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _useBlendLabel,         0,   3,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _useBlendCheckBox,      1,   3,  1,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _blendMethodLabel,      0,   4,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _blendMethodComboBox,   1,   4,  1,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _blendHoursLabel,       0,   5,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _blendHoursSpinner,   1,   5,  1,     1, GridBagConstraints.BOTH );
	
		
		
		// inside the SAC Settings sub panel
		_sacSettingsPanel.setLayout(new GridBagLayout());
		_sacSettingsPanel.setBorder(_panelBorder);
			
		ComponentHelper.addPanelComponent( _sacSettingsPanel, _autoProcessLabel,      0,   0,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacSettingsPanel, _autoProcessCheckBox,   1,   0,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacSettingsPanel, _sourcePrefLabel,       0,   1,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacSettingsPanel, _sourcePrefComboBox,    1,   1,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacSettingsPanel, _useStaticEvapLabel,    0,   2,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sacSettingsPanel, _useStaticEvapCheckBox, 1,   2,  1,     1, GridBagConstraints.BOTH );
		
		
		
		
		
		//add the subpanel to the _sshpConfigTextPanel
		JPanel separatorPanel1 = new JPanel();
		JPanel separatorPanel2 = new JPanel();
		
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, separatorPanel1,      0,   6,   1,  1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _sacSettingsPanelLabel,0,  7,   1,  1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _sacSettingsPanel,     0,  8,   4, 5, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, separatorPanel2,      0,   14,   1, 1, GridBagConstraints.BOTH );
	
		
		
		//continue with more of the _sshpConfigTextPanel
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _postingTimeLabel,      0,   15,  1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _sshpConfigTextPanel, _postingTimeTextField,  1,   15,  1,     1, GridBagConstraints.BOTH );
		
		_postingTimeTextField.setEditable( false );
		_postingTimeTextField.setToolTipText("Indicates the last time that this record was saved");
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
	
	private void addFrameComponents()
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
		ComponentHelper.addFrameComponent( _frameContentPane, _sshpConfigListPanel,  		0,   0,   10,   15, 1, 1, GridBagConstraints.BOTH );
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
		
		if ( _sshpConfig != null )
		{
			_lidTextField.setText( _sshpConfig.getLid() );
			_basinIdTextField.setText( _sshpConfig.getBasinId() );
			_postingTimeTextField.setText( "" + DbTimeHelper.getDateTimeStringFromLongTime( _sshpConfig.getPostingTime() ) );
			_modelPrefComboBox.setSelectedItem( _sshpConfig.getModelPref().trim() );
			_autoProcessCheckBox.setSelected( _sshpConfig.isAutoProcess() );
			_sourcePrefComboBox.setSelectedItem( _sshpConfig.getSourcePref().trim() );
			_useStaticEvapCheckBox.setSelected( _sshpConfig.useStaticEvap() );
			_useBlendCheckBox.setSelected(_sshpConfig.useBlend());
			_blendMethodComboBox.setSelectedItem( _sshpConfig.getBlendMethod());
			_blendHoursSpinner.setValue(new Integer( _sshpConfig.getBlendHours()));
		}
		else
		{
			_lidTextField.setText( "" );
			_basinIdTextField.setText( "" );
			_postingTimeTextField.setText( "Not Posted Yet" );
			_modelPrefComboBox.setSelectedIndex( 0 );
			_autoProcessCheckBox.setSelected( false );
			_sourcePrefComboBox.setSelectedIndex( 0 );
			_useStaticEvapCheckBox.setSelected( false );
			_useBlendCheckBox.setSelected(false);
			_blendMethodComboBox.setSelectedIndex(0);
			_blendHoursSpinner.setValue(new Integer(999));
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
		WindowCloserListener windowCloser = new WindowCloserListener();
		_exitButton.addActionListener( windowCloser );
		addWindowListener( windowCloser );
		addComponentListener( new WindowResizeComponentListener() );
		_listSelectionModel.addListSelectionListener( new TableUpdateListener() ); 
		_sshpConfigTable.getTableHeader().addMouseListener( new TableSortingListener() );

	}
	
	private class TableUpdateListener implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent arg0)
		{
			int index = _sshpConfigTable.getSelectedRow();
			if ( ( index >= 0 ) && ( ! _sshpConfigList.isEmpty() ) )
			{
				_sshpConfig = (SSHPConfig) _sshpConfigList.get( index );
			}
			else
			{
				_sshpConfig = null;
			}
			loadTextFields();
		}
	}
	
	private class TableSortingListener extends MouseAdapter
	{
		public void mouseClicked( MouseEvent e )
		{
			Point point = e.getPoint();
			int column = _sshpConfigTable.getTableHeader().columnAtPoint( point );
			
			_tableHelper.setOrderByClause( column );
			refreshSSHPConfigTable();
		}
	}

	private class SaveButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			SSHPConfig sshpConfig = new SSHPConfig();

			if (
			   ( ! isEmptyString( _lidTextField.getText() ) ) &&  
			   ( ! isEmptyString( _basinIdTextField.getText() ) )
			   )  // Checks to make sure the basinID and source
				  // are not empty strings
			{
				try
				{
					sshpConfig.setLid( _lidTextField.getText().trim() );
					sshpConfig.setBasinId( _basinIdTextField.getText().trim() );
					sshpConfig.setModelPref( (String) _modelPrefComboBox.getSelectedItem() );
					sshpConfig.setAutoProcess( _autoProcessCheckBox.isSelected() );
					sshpConfig.setSourcePref( (String) _sourcePrefComboBox.getSelectedItem() );
					sshpConfig.setUseStaticEvap( _useStaticEvapCheckBox.isSelected() );
					sshpConfig.setUseBlend(_useBlendCheckBox.isSelected());
					
					
					//get the method name and translate that to an actual ForecastBlendMethod object
					String methodName = (String) _blendMethodComboBox.getSelectedItem();
					ForecastInterpolationMethod method = ForecastInterpolationMethod.getMethodByName(methodName);
					sshpConfig.setBlendMethod ( method );
					
					// get and parse the string from the blend hours text field
					Integer hoursInteger = (Integer) _blendHoursSpinner.getValue();
					int hours = 0;
					try
					{
					    hours = hoursInteger.intValue();
					}
					catch (NumberFormatException formatException)
					{
					    hours = 999;
					}
					sshpConfig.setBlendHours(hours);
					
					
					// save the configuration to the database and reload the JTable
					_dataMgr.saveSSHPConfig( sshpConfig );
					_sshpConfig = sshpConfig;
					refreshSSHPConfigTable();			
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
                                                 "Empty Strings are not allowed", 
                                                  "Error Message");
                                                  
				//JOptionPane.showMessageDialog( null, "Empty Strings are not allowed", 
				//							   "Error Message", JOptionPane.ERROR_MESSAGE );
			}
		}
	}
	
	private class DeleteButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			boolean confirm = false;
			if ( _sshpConfig != null )
			{
    			//	choice = JOptionPane.showConfirmDialog( null, "Are you sre you want to delete this Config?", 
    			//											"Delete SSHP Config", JOptionPane.YES_NO_OPTION, 
    			//								JOptionPane.WARNING_MESSAGE );
    			
                confirm = DialogHelper.displayConfirmDialog(null, "Are you sre you want to delete this Config?", 
                                                           "Delete SSHP Config");
           
                if ( confirm )
    			/* User selected YES */
    			{
    				_dataMgr.deleteSSHPConfig( _sshpConfig );
    				_sshpConfig = null;
    				refreshSSHPConfigTable();
    			}
            }
		}
	}
	
	private class ClearButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			_sshpConfig = null;
			loadTextFields();
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
			int height = SSHPConfigDialog.this.getHeight();
			int width = SSHPConfigDialog.this.getWidth();
			
//			System.err.println( "Width: " + width + " Height: " + height );
			if ( width < MIN_WIDTH ) 
			{ 
				SSHPConfigDialog.this.setSize( new Dimension( MIN_WIDTH, height ) );
			}
			if ( width > MAX_WIDTH )
			{
				SSHPConfigDialog.this.setSize( new Dimension( MAX_WIDTH, height ) );
			}
			if ( height < MIN_HEIGHT )
			{
				SSHPConfigDialog.this.setSize( new Dimension( width, MIN_HEIGHT ) );
			}
			if ( height > MAX_HEIGHT )
			{
				SSHPConfigDialog.this.setSize( new Dimension( width, MAX_HEIGHT ) );
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
	/********************
	Purpose: Returns a JLabel set to the string passed in
	*********************/
	{
		JLabel returnJLabel = new JLabel( label );
		returnJLabel.setFont( _labelFont );
		return returnJLabel;
	}
	
	private JTextField getJTextField( int size )
	/********************
	Purpose: Returns a JTextField using the size passed in
	*********************/
	{
		JTextField returnJTextField = new JTextField( size );
		returnJTextField.setFont( _textFieldFont );
		return returnJTextField;
	}
	
	public static void main( String[] args )
	{
		SSHPConfigDialog dialog = new SSHPConfigDialog( new Frame(), new DataMgr(), "BLUO2" );
		dialog.show();
	}
}
