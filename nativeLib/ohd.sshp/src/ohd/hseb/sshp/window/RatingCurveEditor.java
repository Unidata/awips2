/*
 * Created on Jun 22, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 22, 2004
 *  
 */
 
/**
 * @author SoodG
*/

package ohd.hseb.sshp.window;

import javax.swing.*;
import javax.swing.event.*;

import java.awt.*;
import java.awt.event.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import java.text.DecimalFormat;

import java.util.List;
import java.util.*;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.RatingPoint;
import ohd.hseb.model.RatingShift;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.util.StringParser;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DateTimeDialog;
import ohd.hseb.util.gui.DateTimeEvent;
import ohd.hseb.util.gui.DateTimeEventListener;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.ExtensionFileFilter;
import ohd.hseb.util.gui.TableHelper2;
import ohd.hseb.util.gui.drawing.DataPointCanvas;


//------------------------------------------------------------------------------------

public class RatingCurveEditor extends JDialog
{
	private Container _frameContentPane = 					getContentPane();

	private JPanel _ratingShiftPanel = 						new JPanel();
	private JPanel _ratingCurveDataPanel = 					new JPanel();
	private JPanel _ratingCurveButtonPanel = 				new JPanel();
	private JPanel _ratingCurveDisplayPanel =				new JPanel();
	private JPanel _buttonPanel = 							new JPanel();

	private Font _labelFont = 								new Font( "Helvetica", Font.BOLD, 14 );
	private Font _textFieldFont = 							new Font( "Helvetica", Font.PLAIN, 12 );
	
	private DataMgr _dataMgr = 								null;
	
	private String _locationId = 							null;
	
	private RatingShift _ratingShift = 						null;
	private RatingCurve _ratingCurve = 						null;
	private RatingPoint _ratingPoint = 						null;
	
	//private double _shiftAmount = 							0.0;
	
	private static final int DEFAULT_WIDTH =				1200;
	private static final int DEFAULT_HEIGHT = 				900;
	
	private static final int MAX_WIDTH = 					5000;
	private static final int MAX_HEIGHT =					5000;
	
	private static final int MIN_WIDTH =		 			1075;
	private static final int MIN_HEIGHT = 					860;
	
	private List _ratingShiftList = 						null;
//	private List _ratingPointList = 						new ArrayList();
	
	//used for JFormattedTextFields
	private static final String _decimalFormatString 		= "0.000";
	private DecimalFormat _decimalFormat = 					new DecimalFormat( _decimalFormatString );
	
	//used for the rating shift entries
	private JLabel _shiftDateLabel = 						getJLabel( "Shift Date" );
	private JLabel _shiftValueLabel = 						getJLabel( "Value" );
	private JLabel _shiftActiveLabel = 						getJLabel( "Active?" );
	
	private JTextField _shiftDateTextField = 				getJTextField( 15 );
	private JTextField _shiftValueTextField = 				getJTextField( 15 );
	private JCheckBox _shiftActiveCheckBox = 				new JCheckBox();
	
	private JButton _removeShiftButton =					new JButton( "Remove Shift" );
	private JButton _saveShiftButton = 						new JButton( "Save Shift" );
	
	// used for the rating curve entries
	private JLabel _stageLabel = 							getJLabel( "Stage" );
	private JLabel _dischargeLabel = 						getJLabel( "Discharge" );
	private JLabel _tableHeaderLabel = 						getJLabel( "Active Rating Curve" );
	private JLabel _dateOfRatingLabel = 					null;
	private JLabel _USGSRatingNumberLabel = 				null;
	
	private JTextField _stageTextField = 					getJTextField( 11 );
	private JTextField _dischargeTextField = 				getJTextField( 11 );
	
	private JButton _importCurveButton = 					new JButton( "Import Curve" );
	private JButton _exportCurveButton = 					new JButton( "Export Curve" );
	private JButton _clearAllCurveButton = 					new JButton( "Clear All" );
	private JButton _removePointButton = 					new JButton( "Remove Point" );
	private JButton _updateInsertPointButton = 				new JButton( "Update/Insert" );
    private JButton _saveRatingCurveButton =                new JButton( "Save Rating" );

	// used for the buttonPanel
	private JButton _closeButton = 							new JButton( "Close" );

	//used for the ratingShift JTable
	public static final String SHIFT_DATE =                "Shift Date";
	public static final String VALUE = 						"Value";
	public static final String ACTIVE = 					"Active?";
	private String[] _shiftColumnNames =					{ SHIFT_DATE, VALUE, ACTIVE };
	
	private String[][] _ratingShiftStringArray = 			null;
	private JTable _ratingShiftJTable = 					null;
	private JScrollPane _shiftScrollPane = 					null;
	private SshpTableModel _shiftTableModel =    			null;
	private ListSelectionModel _shiftListSelectionModel =	null;
	private RatingShiftComparator _shiftComparator = 		new RatingShiftComparator();		
	private TableHelper2 _shiftTableHelper = 				new TableHelper2( _shiftComparator, _shiftColumnNames );
	
	//used for the ratingCurve JTable
	public static final String STAGE = 						"Stage";
	public static final String STAGE_WITH_SHIFT = 			"Stage w/ Shift";
	public static final String DISCHARGE = 					"Discharge";
	private String[] _ratingCurveColumnNames =				{ STAGE, STAGE_WITH_SHIFT, DISCHARGE };
	
	private String[][] _ratingCurveStringArray = 			null;
	private JTable _ratingCurveJTable = 					null;
	private JScrollPane _curveScrollPane = 					null;
	private SshpTableModel _curveTableModel =    			null;
	private ListSelectionModel _curveListSelectionModel =	null;
//	private RatingPointComparator _pointComparator = 		new RatingPointComparator();
//	private TableHelper2 _curveTableHelper = 				new TableHelper2( _pointComparator, _ratingCurveColumnNames );

	//	 ----------------------------------------------------------------------------------------------
	//drawing variables
	
	RatingCurveDataPointCanvasAdapter _canvasMgr = null;
	DataPointCanvas _canvas = null;
	// ----------------------------------------------------------------------------------------------
	

	public RatingCurveEditor( Frame owner, DataMgr dataMgr, String locationId, boolean isModal )
	{
		super( owner, isModal );
		_locationId = locationId;
		this.setTitle( "Rating Curve - " + _locationId );
		_dataMgr = dataMgr;
		initialize();
	}
//	------------------------------------------------------------------------------------
	
	private void initialize()
	{	
	 
	    // initialize the drawing area (canvas)
        _canvasMgr = new RatingCurveDataPointCanvasAdapter();
        _canvas = _canvasMgr.initCanvas();
	    
        setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );
		 
        
		setupRatingShiftTable();		
		setupRatingCurveTable();
		
		addRatingShiftPanelComponents();
		addRatingCurvePanelComponents();
		addRatingCurveButtonPanelComponents();
		addButtonPanelComponents();
		addFrameComponents();
		
		
		

		addListeners();
		
	//	setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );
		
	
	}
//	------------------------------------------------------------------------------------
	
	
	
	private void addRatingShiftPanelComponents()
	{
		_ratingShiftPanel.setLayout( new GridBagLayout() );
		
		JPanel horizontalSeparatorPanel = new JPanel();
		JPanel horizontalSeparatorPanel2 = new JPanel();
		JPanel horizontalSeparatorPanel3 = new JPanel();
		JPanel verticalSeparatorPanel = new JPanel();

		horizontalSeparatorPanel3.setPreferredSize( new Dimension( 35, 40 ) );

//																				          	X,  Y, #Col,  #Row
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _shiftScrollPane,   			0,  0,   3,    5, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, horizontalSeparatorPanel,     0,  5,   3,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _shiftDateLabel,              0,  6,   1,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _shiftValueLabel,             1,  6,   1,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _shiftActiveLabel,            2,  6,   1,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _shiftDateTextField,          0,  7,   1,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _shiftValueTextField,         1,  7,   1,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _shiftActiveCheckBox,         2,  7,   1,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, verticalSeparatorPanel,       3,  0,   1,    8, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, horizontalSeparatorPanel3,    4,  0,   2,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _removeShiftButton,           4,  1,   2,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, horizontalSeparatorPanel2,    4,  2,   2,    1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingShiftPanel, _saveShiftButton,     		4,  3,   2,    1, GridBagConstraints.BOTH );
		
		java.awt.Color background = _shiftDateTextField.getBackground(); //store the editable color
		_shiftDateTextField.setEditable( false );
		_shiftDateTextField.setBackground(background); //make it appear editable
	}
//	------------------------------------------------------------------------------------
	
	private void addButtonPanelComponents()
	{
		_buttonPanel.setLayout( new GridBagLayout() );
		
//																							X,  Y, #Col,  #Row
		ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,			   			0,  0,   1,    1, GridBagConstraints.BOTH );
		

	}
//	------------------------------------------------------------------------------------
	
	private void addRatingCurvePanelComponents()
	{
		JPanel horizontalSeparatorPanel = new JPanel();
		_ratingCurveDataPanel.setLayout( new GridBagLayout() );

//																							 X,   Y, #Col,  #Row
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _tableHeaderLabel,         0,   0,   5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _dateOfRatingLabel,        0,   1,   5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _USGSRatingNumberLabel,    0,   2,   5,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _curveScrollPane,          0,   3,   5,    10, 1, 1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, horizontalSeparatorPanel,  0,  13,   5,     2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _importCurveButton,        0,  15,   1,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _exportCurveButton,        1,  15,   1,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _saveRatingCurveButton,               2,  15,   1,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _clearAllCurveButton,      3,  15,   1,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _ratingCurveDataPanel, _removePointButton,        4,  15,   1,     1, GridBagConstraints.NONE );
        
        if ( _ratingCurve.getRatingPointCount() > 0 )
        {
            _importCurveButton.setEnabled( false );
        }

	}
//	------------------------------------------------------------------------------------
	
	private void addRatingCurveButtonPanelComponents()
	{
		_ratingCurveButtonPanel.setLayout( new GridBagLayout() );
		JPanel horizontalSeparatorPanel = new JPanel();
		JPanel verticalSeparatorPanel = new JPanel();
		
//																							   X,   Y, #Col,  #Row
		ComponentHelper.addPanelComponent( _ratingCurveButtonPanel, _updateInsertPointButton,  0,   1,   1,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _ratingCurveButtonPanel, verticalSeparatorPanel,    1,   0,   1,     6, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveButtonPanel, _stageLabel,               2,   0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveButtonPanel, _dischargeLabel,           3,   0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveButtonPanel, _stageTextField,           2,   1,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveButtonPanel, _dischargeTextField,       3,   1,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _ratingCurveButtonPanel, horizontalSeparatorPanel,  0,   2,   4,     4, 1, 1, GridBagConstraints.BOTH );
	}
		
//	------------------------------------------------------------------------------------
	
	
	private void addFrameComponents()
	{
		_frameContentPane.setLayout( new GridBagLayout() );

		JPanel horizontalSeparatorPanel = new JPanel();
		JPanel horizontalSeparatorPanel2 = new JPanel();
		JPanel horizontalSeparatorPanel3 = new JPanel();
		JPanel verticalSeparatorPanel = new JPanel();

/*
 		verticalSeparatorPanel.setBackground( Color.YELLOW );
		horizontalSeparatorPanel.setBackground( Color.YELLOW );
		horizontalSeparatorPanel2.setBackground( Color.YELLOW );
		horizontalSeparatorPanel3.setBackground( Color.YELLOW );
*/		
		_canvas.setPreferredSize( new Dimension( 300, 500 ) );
		
//																				      		 X,   Y, #Col,  #Row
		ComponentHelper.addFrameComponent( _frameContentPane, _canvas,                		 0,   0,  10,     8, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, verticalSeparatorPanel,  		10,   0,   2,    22, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel, 	 0,   8,  10,     1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _ratingShiftPanel,      		 0,  17,   8,     5, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _ratingCurveDataPanel,   		12,   0,   9,    17, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel3,    12,  17,   9,     2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _ratingCurveButtonPanel, 		12,  19,   9,     5, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel2,     0,  24,  21,     3, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel,  				 0,  27,  21,     1, GridBagConstraints.BOTH );
		
	}
//	------------------------------------------------------------------------------------
		
	private void setupRatingShiftTable()
	{
		int index = 0;
		
		_ratingShiftList = _dataMgr.getRatingShift( _locationId );
		if (_ratingShiftList == null)
		{
		    _ratingShiftList = new ArrayList();
		}
		
		_ratingShiftStringArray = convertShiftListToArray();
		
		if (_ratingShiftList != null && _ratingShiftList.size() != 0 )
		{
			_ratingShift = (RatingShift) _ratingShiftList.get( 0 );
		}
		
		_shiftTableModel = new SshpTableModel( _shiftColumnNames, _ratingShiftStringArray );
		_ratingShiftJTable = new JTable( _shiftTableModel );
		_ratingShiftJTable.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
		_ratingShiftJTable.setPreferredScrollableViewportSize( new Dimension( 300, 100 ) );
		_shiftListSelectionModel = _ratingShiftJTable.getSelectionModel();
		_shiftScrollPane = new JScrollPane( _ratingShiftJTable );
		
		_shiftListSelectionModel.setSelectionInterval( 0, 0 );
		
		Rectangle rect = _ratingShiftJTable.getCellRect( index, 0, true );
		_shiftScrollPane.getViewport().setViewPosition( rect.getLocation() );

		loadShiftTextFields();
	}
//	------------------------------------------------------------------------------------
	
	private String[][] convertShiftListToArray()
	{
	    int ratingShiftListSize = 0;
	    if (_ratingShiftList != null)
	    {
	        ratingShiftListSize = _ratingShiftList.size();
	    }
	    
		String[][] dataArray = new String[ ratingShiftListSize ][ _shiftColumnNames.length ];

		for( int i = 0; i < dataArray.length; i++ )
		{
			RatingShift entry = (RatingShift) _ratingShiftList.get( i );
			
			dataArray[ i ][ 0 ] = DbTimeHelper.getDateStringFromLongTime( entry.getDate() );
			dataArray[ i ][ 1 ] = "" + entry.getShiftAmount();
			dataArray[ i ][ 2 ] = "" + entry.isActive();
		}
		return dataArray;
	}
//	------------------------------------------------------------------------------------

	private void refreshShiftListJTable()
	{
		int index = 0;
		
		_ratingShiftStringArray = convertShiftListToArray();
		
		if ( _ratingShift != null )
		{
			if ( _ratingShiftList.size() != 0 )
			{
				for ( int i = 0; i < _ratingShiftList.size(); i++ )
				{
					RatingShift shift = (RatingShift) _ratingShiftList.get( i );
		
					if ( shift.equals( _ratingShift ) )
					{
						index = i;
						break;
					}
				}
			}
		}

		_shiftTableModel.updateData( _ratingShiftStringArray );
		_shiftTableModel.fireTableChanged( null );
		
		_shiftListSelectionModel.setSelectionInterval( index, index );
		
		Rectangle rect = _ratingShiftJTable.getCellRect(index, 0, true);
		_shiftScrollPane.getViewport().setViewPosition(rect.getLocation());
		loadShiftTextFields();
	}
//	------------------------------------------------------------------------------------

	private void setupRatingCurveTable()
	{
		int index = 0;
		
		_ratingCurve = _dataMgr.loadRatingCurve( _locationId );
		_ratingCurveStringArray = convertCurveListToArray();

/*
		for ( int i = 0; i < _ratingCurve.getRatingPointCount(); i++ )
		{
			_ratingPointList.add( _ratingCurve.getRatingPoint( i ) );
		}
*/
		
		if ( _ratingCurve.getRatingPointCount() != 0 )
		{
			_ratingPoint = _ratingCurve.getFirstRatingPoint();
		}
		
		_curveTableModel = new SshpTableModel( _ratingCurveColumnNames, _ratingCurveStringArray );
		_ratingCurveJTable = new JTable( _curveTableModel );
		_ratingCurveJTable.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
		_ratingCurveJTable.setPreferredScrollableViewportSize( new Dimension( 300, 400 ) );
		_curveListSelectionModel = _ratingCurveJTable.getSelectionModel();
		_curveScrollPane = new JScrollPane( _ratingCurveJTable );
		
		_curveListSelectionModel.setSelectionInterval( 0, 0 );
		
		Rectangle rect = _ratingCurveJTable.getCellRect( index, 0, true );
		_curveScrollPane.getViewport().setViewPosition( rect.getLocation() );

		_dateOfRatingLabel = getJLabel( "Date of Rating: " + DbTimeHelper.getDateStringFromLongTime( _ratingCurve.getRatingDate() ) );
		_USGSRatingNumberLabel = getJLabel ( "USGS Rating No.: " + _ratingCurve.getUsgsRatingNumber() );
		loadCurveTextFields();
		
		List ratingPointList = _ratingCurve.getRatingPointList();
		_canvasMgr.refreshCanvas(ratingPointList);
	}
	
//	------------------------------------------------------------------------------------


	private void refreshRatingCurveListJTable(boolean shouldReloadData)
	{
		int index = 0;
		
		if (shouldReloadData)
		{
		    _ratingCurve = _dataMgr.loadRatingCurve( _locationId );
		}
		_ratingCurveStringArray = convertCurveListToArray();
		
		if ( ( _ratingCurve != null ) && ( _ratingPoint != null ) )
		{
			if ( _ratingCurve.getRatingPointCount() > 0 )
			{
				for ( int i = 0; i < _ratingCurve.getRatingPointCount(); i++ )
				{
					RatingPoint point = (RatingPoint) _ratingCurve.getRatingPoint( i );
		
					if ( point.equals( _ratingPoint ) )
					{
						index = i;
						break;
					}
				}
			}
		}

		_curveTableModel.updateData( _ratingCurveStringArray );
		_curveTableModel.fireTableChanged( null );
		
		_curveListSelectionModel.setSelectionInterval( index, index );
		
		Rectangle rect = _ratingCurveJTable.getCellRect(index, 0, true);
		_curveScrollPane.getViewport().setViewPosition(rect.getLocation());
		loadCurveTextFields();
		
		_canvasMgr.refreshCanvas(_ratingCurve.getRatingPointList());
	}

//	------------------------------------------------------------------------------------
	
	private String[][] convertCurveListToArray()
	{
		String[][] dataArray = new String[ _ratingCurve.getRatingPointCount() ][ _ratingCurveColumnNames.length ];

		for( int i = 0; i < dataArray.length; i++ )
		{
			RatingPoint point = _ratingCurve.getRatingPoint( i );
			
			dataArray[ i ][ 0 ] = "" + _decimalFormat.format( point.getUnshiftedStage() );
			dataArray[ i ][ 1 ] = "" + _decimalFormat.format( point.getShiftedStage() );
			dataArray[ i ][ 2 ] = "" + _decimalFormat.format( point.getDischarge() );
		}
		return dataArray;
	}
//	------------------------------------------------------------------------------------

	private void loadCurveTextFields()
	/********************
	Purpose: Load's the appropriate values for the Text Fields 
	*********************/
	{
		if ( _ratingPoint != null )
		{
			_stageTextField.setText( "" + _ratingPoint.getUnshiftedStage() );
			_dischargeTextField.setText( "" + _ratingPoint.getDischarge() );
		}
		else
		{
			_stageTextField.setText( "" );
			_dischargeTextField.setText( "" );
		}
	}
//	------------------------------------------------------------------------------------

	private void loadShiftTextFields()
	/********************
	Purpose: Load's the appropriate values for the Text Fields 
	*********************/
	{
		if ( _ratingShift != null )
		{
			_shiftDateTextField.setText( DbTimeHelper.getDateStringFromLongTime( _ratingShift.getDate() ) );
			_shiftValueTextField.setText( "" + _ratingShift.getShiftAmount() );
			_shiftActiveCheckBox.setSelected( _ratingShift.isActive() );
		}
		else
		{
			_shiftDateTextField.setText( "" );
			_shiftValueTextField.setText( "" );
			_shiftActiveCheckBox.setSelected( false );
		}
	}
//	------------------------------------------------------------------------------------

	private void addListeners()
	/********************
	Purpose: Add's listeners to the buttons and TextFields
	*********************/
	{
		WindowCloserListener windowCloser = new WindowCloserListener();
		_closeButton.addActionListener( windowCloser );
		_removeShiftButton.addActionListener( new RemoveShiftButtonListener() );
		_saveShiftButton.addActionListener( new SaveShiftButtonListener() );
		_shiftDateTextField.addMouseListener( new TimeTextFieldListener() );
		addWindowListener( windowCloser );
		addComponentListener( new WindowResizeComponentListener() );
		_shiftListSelectionModel.addListSelectionListener( new RatingShiftTableUpdateListener() );

		_updateInsertPointButton.addActionListener( new UpdateInsertPointButtonListener() );
		_removePointButton.addActionListener( new RemovePointButtonListener() );
        _clearAllCurveButton.addActionListener( new ClearAllCurveButtonListener() );
        _saveRatingCurveButton.addActionListener( new SaveRatingCurveButtonListener() );
        _importCurveButton.addActionListener( new ImportRatingCurveButtonListener() );
		
		_shiftListSelectionModel.addListSelectionListener( new RatingShiftTableUpdateListener() );
		_curveListSelectionModel.addListSelectionListener( new RatingCurveTableUpdateListener() );
		_ratingShiftJTable.getTableHeader().addMouseListener( new TableSortingListener( _ratingShiftJTable, true ) );
		_ratingCurveJTable.getTableHeader().addMouseListener( new TableSortingListener( _ratingCurveJTable, false ) );
	
	}

//	------------------------------------------------------------------------------------
	private boolean isNewEntry( RatingShift newShift )
	{
		boolean isNew = true;
		
		if (_ratingShiftList != null)
        {
            for (int i = 0; i < _ratingShiftList.size(); i++)
            {
                RatingShift shift = (RatingShift) _ratingShiftList.get(i);

                if (shift.equals(newShift))
                {
                    isNew = false;
                }
            }
        }
		return isNew;
	}

// --------------------------------------------------------------------------------------
	
    private void importRatingCurveFromFile( File importFile )
    {
        boolean addToDatabase = true;
        
        try
        {
            BufferedReader _reader = new BufferedReader( new FileReader( importFile ) );
            String line = _reader.readLine();
            
 //           if ( ( _ratingCurve.getRatingPointCount() == 0)  && ( _ratingPointList.size() == 0 ) )
            if ( ( _ratingCurve.getRatingPointCount() == 0))
            {
                while( line != null )
                {
                    while( line.startsWith( "x" ) )
                    {
                        line = _reader.readLine();
                    }
                    String[] tokenizedLine = StringParser.tokenize( line );
                    
                    RatingPoint point = new RatingPoint();
                    
                    point.setUnshiftedStage( Double.parseDouble( tokenizedLine[ 0 ] ) );
                    point.setDischarge( Double.parseDouble( tokenizedLine[ 1 ] ) );

                    _ratingCurve.addRatingPoint( point );
              //      _ratingPointList.add( point );
                    
                    line = _reader.readLine();
                }
            }
            _importCurveButton.setEnabled( false );
            
            refreshRatingCurveListJTable(true);
        }
        catch( FileNotFoundException e )
        {
        }
        catch( IOException e )
        {
        }
        catch( ArrayIndexOutOfBoundsException e )
        {
            System.out.println( "Invalid file format: " + importFile.getName() );
        }

    }
//  --------------------------------------------------------------------------------------

    private File displayImportFileChooser()
    {
        JFileChooser _importFileChooser = new JFileChooser( "d:/data/rating_curve" );
        File file = null;
        List filterList = new ArrayList();
        
        filterList.add( "rating" );
        
        _importFileChooser.addChoosableFileFilter( new ExtensionFileFilter( filterList, "Rating Curve Files (RATING)" ) );

        int returnVal = _importFileChooser.showOpenDialog( this );
        
        if (returnVal == JFileChooser.APPROVE_OPTION)
        {
            file =  _importFileChooser.getSelectedFile();
        } 
        else
        {
            DialogHelper.displayMessageDialog( RatingCurveEditor.this, "Rating Curve file not imported." );
        }
        return file;
    }
//  --------------------------------------------------------------------------------------
    private void addOrReplacePointInRatingPointList(RatingPoint pointToInsert)
    {
        
        _ratingCurve.addRatingPoint(pointToInsert);
        
    }
    
//  --------------------------------------------------------------------------------------

	private class RemoveShiftButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			boolean confirm = false;
			
			confirm = DialogHelper.displayConfirmDialog(null,
													   "Are you sure you want to delete the rating shift for " 
													   + _locationId + "?",  "Delete Rating Shift");
			
			if ( confirm )
			{
				_dataMgr.deleteRatingShift( _ratingShift );
				_ratingShiftList.remove( _ratingShift );
				_ratingShift = null;
				refreshShiftListJTable();
				refreshRatingCurveListJTable(true);
			}

		}
		
	}
//	 --------------------------------------------------------------------------------------

	private class RatingCurveTableUpdateListener implements ListSelectionListener
	{
		public void valueChanged( ListSelectionEvent arg0 )
		{
			int index = _ratingCurveJTable.getSelectedRow();
			if ( ( index >= 0 ) && ( _ratingCurve.getRatingPointCount() >= 1 ) )
			{
				_ratingPoint = _ratingCurve.getRatingPoint( index ); 
			}
			else
			{
				_ratingPoint = null;
			}
			loadCurveTextFields();
		}
	}
//	 --------------------------------------------------------------------------------------

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
//	 --------------------------------------------------------------------------------------
	
	private class WindowResizeComponentListener extends ComponentAdapter
	{
		public void componentResized( ComponentEvent e )
		{
			int height = RatingCurveEditor.this.getHeight();
			int width = RatingCurveEditor.this.getWidth();
		//	System.out.println( "Width/Height = [" + width + ", " + height + "]" );

			if ( width < MIN_WIDTH ) 
			{ 
				RatingCurveEditor.this.setSize( new Dimension( MIN_WIDTH, height ) );
			}
			if ( width > MAX_WIDTH )
			{
				RatingCurveEditor.this.setSize( new Dimension( MAX_WIDTH, height ) );
			}
			if ( height < MIN_HEIGHT )
			{
				RatingCurveEditor.this.setSize( new Dimension( width, MIN_HEIGHT ) );
			}
			if ( height > MAX_HEIGHT )
			{
				RatingCurveEditor.this.setSize( new Dimension( width, MAX_HEIGHT ) );
			}
		}
	}
//	 --------------------------------------------------------------------------------------

	private void closeWindow()
	/********************
	Purpose: Exit's the program gracefully 
	*********************/
	{
        this.dispose();
	}
//	 --------------------------------------------------------------------------------------
	
/*
	private boolean isEmptyString( String testingString )
	// 
	//Purpose: Check's whether the string passed in is an empty string 
	//
	{
		boolean returnValue = false;
		
		if ( testingString.trim().equals( "" ) )
		{
			returnValue = true;
		}
		return returnValue;
	}
*/
//	 --------------------------------------------------------------------------------------

	private JLabel getJLabel( String label )
	{
		JLabel returnJLabel = new JLabel( "<HTML><CENTER> " + label + "</CENTER></HTML>" );
		returnJLabel.setFont( _labelFont );
		return returnJLabel;
	}
//	 --------------------------------------------------------------------------------------
	
	private JTextField getJTextField( int size )
	{
		JTextField returnJTextField = new JTextField( size );
		returnJTextField.setFont( _textFieldFont );
		return returnJTextField;
	}
//	 --------------------------------------------------------------------------------------

	private JFormattedTextField getJFormattedTextField( String formatString )
	{
		JFormattedTextField returnJFormattedTextField = new JFormattedTextField( formatString );
		returnJFormattedTextField.setFont( _textFieldFont );
		returnJFormattedTextField.setColumns( 17 );
		return returnJFormattedTextField;
	}
//	 --------------------------------------------------------------------------------------

	private String getFormattedValue( double valueString )
	{
		return _decimalFormat.format( valueString );
	}
	
	
	//	------------------------------------------------------------------------------------

	private class TableSortingListener extends MouseAdapter
	{
		JTable __table = null;
		boolean __isRatingShiftJTable;
		
		public TableSortingListener( JTable table, boolean ratingShiftJTable )
		{
			__table = table;
			__isRatingShiftJTable = ratingShiftJTable;
		}		

		public void mouseClicked( MouseEvent e )
		{
			Point point = e.getPoint();
			int column = __table.getTableHeader().columnAtPoint( point );
			
			if ( __isRatingShiftJTable )
			{
				_shiftTableHelper.sortList( _ratingShiftList, column, true );
				refreshShiftListJTable();
			}
			else
			{
				
			}
		}
	}
//	------------------------------------------------------------------------------------

	private class RatingShiftTableUpdateListener implements ListSelectionListener
	{
		public void valueChanged( ListSelectionEvent arg0 )
		{
			int index = _ratingShiftJTable.getSelectedRow();
			if ( ( index >= 0 ) && ( ! _ratingShiftList.isEmpty() ) )
			{
				_ratingShift = (RatingShift) _ratingShiftList.get( index );
			}
			else
			{
				_ratingShift = null;
			}
			loadShiftTextFields();
		}
	}
//	------------------------------------------------------------------------------------
	
	private class TimeTextFieldListener extends MouseAdapter
	{
		public void mousePressed( MouseEvent e )
		{
            long dateTime = 0;
            
            if ( _shiftDateTextField.getText().trim().equals( "" ) )
            {
                dateTime = System.currentTimeMillis(); 
            }
            else
            {
                dateTime = DbTimeHelper.getLongTimeFromDateString( _shiftDateTextField.getText() );
            }
            
			JTextField textField = (JTextField) e.getSource();
			
			DateTimeDialog dateTimeGUI = new DateTimeDialog( RatingCurveEditor.this, dateTime, "Rating Shift Date" );
			dateTimeGUI.setDateOnly();
			dateTimeGUI.addDateTimeEventListener( new DateTimeListener( textField ) );
			dateTimeGUI.show();
		}
	}
//	------------------------------------------------------------------------------------

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
				String timeString = DbTimeHelper.getDateStringFromLongTime( dateTimeEvent.getDateTimeLong() );
				_timeTextField.setText( timeString );
			}
		}
	}
//	------------------------------------------------------------------------------------

	private class UpdateInsertPointButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			RatingPoint point = new RatingPoint();
			
			point.setDischarge( Double.parseDouble( _dischargeTextField.getText() ) );
			point.setUnshiftedStage( Double.parseDouble( _stageTextField.getText() ) );
			point.setShiftAmount( _ratingCurve.getShiftAmount() );
			
			
			_ratingCurve.addRatingPoint( point );
			
			addOrReplacePointInRatingPointList(point);
          	
            if ( _importCurveButton.isEnabled() )
            {
                _importCurveButton.setEnabled( false );
            }
			refreshRatingCurveListJTable(false);
		}
	}
//	------------------------------------------------------------------------------------
	
	private class RemovePointButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			boolean confirm = false;
			
			confirm = DialogHelper.displayConfirmDialog(null,
													   "Are you sure you want to remove the rating point from the rating curve?",
													   "Remove Rating Point");
			if ( confirm )
			{
              //  _ratingPointList.remove( _ratingPoint );
                _ratingCurve.removeRatingPoint( _ratingPoint );
				_ratingPoint = null;

                if ( _ratingCurve.getRatingPointCount() == 0 )
                {
                    _importCurveButton.setEnabled( true );
                }
                else
                {
                    _importCurveButton.setEnabled( false );
                }
				refreshRatingCurveListJTable(false);
			}
		}
	}
//	------------------------------------------------------------------------------------
	
    private class ClearAllCurveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            boolean confirm = false;
            
            confirm = DialogHelper.displayConfirmDialog(null,
                       "Are you sure you want to remove all of the rating points?",
                       "Clear Rating Curve");
            
            if ( confirm ) 
            {
                int ratingPointCount = _ratingCurve.getRatingPointCount();
                
                for ( int i = 0; i < ratingPointCount; i++ )
                {
                    _ratingCurve.removeRatingPoint( _ratingCurve.getFirstRatingPoint() );
                }
                
             //   _ratingPointList.clear();
                
                refreshRatingCurveListJTable(false);
                
                _importCurveButton.setEnabled( true );
            }
        }
    }
//	------------------------------------------------------------------------------------
	
	private class SaveShiftButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			boolean newEntry = false;
			boolean validEntry = true;
			
			RatingShift newShift = new RatingShift();
			try
			{
				
				newShift.setLid( _locationId );
				newShift.setDate( DbTimeHelper.getLongTimeFromDateString( _shiftDateTextField.getText().trim() ) );
				newShift.setShiftAmount( Double.parseDouble( _shiftValueTextField.getText().trim() ) );
				newShift.setActive( _shiftActiveCheckBox.isSelected() );
			}
			catch( NumberFormatException error )
			{
				DialogHelper.displayErrorDialog( RatingCurveEditor.this, "Illegal characters in the text fields", "Parsing Error" );
				validEntry = false;
			}
			
			if ( validEntry )
			{
				_dataMgr.saveRatingShift( newShift );
				
				newEntry = isNewEntry( newShift );
				
				if( newEntry )
				{
					_ratingShift = newShift;
					_ratingShiftList.add( _ratingShift );
				}
				else
				{
					_ratingShiftList.remove( _ratingShift );
					_ratingShiftList.add( newShift );
				}
				
				_ratingCurve = _dataMgr.loadRatingCurve( _locationId );
				
				refreshShiftListJTable();
				refreshRatingCurveListJTable(true);
			}
		}
	}
//	------------------------------------------------------------------------------------
	
    private class SaveRatingCurveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e ) 
        {
            _dataMgr.saveRatingCurve( _ratingCurve );
            
            DialogHelper.displayMessageDialog( null,
                                               "Successfully saved Rating Curve",
                                               "Save Successful" );
        }
    }
    
//	------------------------------------------------------------------------------------
	
	private class ImportRatingCurveButtonListener implements ActionListener
    {
	    public void actionPerformed( ActionEvent e )
        {
            File importFile = RatingCurveEditor.this.displayImportFileChooser();
            if ( importFile != null )
            {
                RatingCurveEditor.this.importRatingCurveFromFile( importFile );
            }
        }
    }
//	------------------------------------------------------------------------------------


	public static void main( String args[] )
	{
		RatingCurveEditor editor = new RatingCurveEditor( new Frame(), new DataMgr(), "BLUO2", true );
		editor.show();
	}
//	 --------------------------------------------------------------------------------------
	
}
