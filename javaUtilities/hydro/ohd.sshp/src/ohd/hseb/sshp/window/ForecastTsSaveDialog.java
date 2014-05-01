/*
 * Created on Dec 8, 2003
 *
 * Filename : ForecastTsSaveDialog.java 
 * Author   : Gautam Sood
 * Last Revision Date : Dec 8, 2003
 *  
 */
package ohd.hseb.sshp.window;

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DateTimeTextField;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.sshp.FcstTsDescriptor;
import ohd.hseb.measurement.*;
import ohd.hseb.util.gui.*;

public class ForecastTsSaveDialog extends JDialog
{
	private static final long HOUR_IN_MILLIS = 60 * 60 * 1000;
	private static final int GOOD_QC=1879048191;	
	private RegularTimeSeriesHolder _timeSeriesHolder = null;
	private RegularTimeSeries _timeSeries = null;
	private DataMgr _dataMgr = null;

	private static final int DEFAULT_WIDTH =	390;
   
	private static final int DEFAULT_HEIGHT = 	390;
	
	private static final int MAX_WIDTH = 		DEFAULT_WIDTH;
	private static final int MAX_HEIGHT =		DEFAULT_HEIGHT;

	private static final int MIN_WIDTH = 		266;
	private static final int MIN_HEIGHT = 		296;
	
	private static final int SAVE_HOURS_STEPS = 6;

	private String _extremum = "Z";
	private float _probability = -1.0f;
	private int _revision = 0;
	private String _physicalElement = null;
	
	private String[] _physicalElementStringArray = null;
	
	private String _lid = null;
	private String _productId = "";
	private String _basisProductTime = null;
	private String _tableName = null;
	private String[] _typeSourceStringArray = { "FE", "FF", "FG", "FM", "FZ" };
	private String _sshpForecastTsToken = null;
	private String[] _shefQualifierStringArray = { "Z", "E" };
	private long _productTime = 0;
	private long _basisTime = 0;
	
	
	
	//used for custom notification
	private List _applyListenerList = new ArrayList();
	
//               GUI Components                         
	private Container _frameContentPane = getContentPane();
	private JLabel _physicalElementLabel = new JLabel( "Physical Element: " );
	private JLabel _pElementValueLabel = new JLabel();
	private JComboBox _physicalElementComboBox = null;
	
	private JLabel _typeSourceLabel = new JLabel( "Type Source: " );
	private JLabel _shefQualifierLabel = new JLabel( "SHEF Qualifier: " );
	private JLabel _productIDLabel = new JLabel( "Product ID: " );
	private JLabel _basisTimeLabel = new JLabel( "Basis Time: " );
	private JLabel _productTimeLabel = new JLabel( "Product Time: " );
	private JTextField _productIDTextField = new JTextField(10);

	private DateTimeTextField _basisTimeTextField = null;
	private DateTimeTextField _productTimeTextField = null;

	private JComboBox _typeSourceComboBox = new JComboBox( _typeSourceStringArray );
	private JComboBox _shefQualifierComboBox = new JComboBox( _shefQualifierStringArray );
	private JPanel _dataPanel = new JPanel();
	private JPanel _buttonPanel = new JPanel();
	
	private JLabel _hoursToSaveLabel = new JLabel("Hours to Save:");
	private JSpinner _hoursToSaveSpinner = null;
	
	//private JButton _saveButton = new JButton( "<HTML><CENTER>Save to<BR>Database</CENTER></HTML>" ); 
	private JButton _saveButton = new JButton( "Save to Database" ); 
	private JButton _closeButton = new JButton( "Close" );

/*  Empty Constructor
 *  Sets the basis and product time to the current system time
 */
 
	public ForecastTsSaveDialog( JFrame owner, String lid, String title, 
	                             RegularTimeSeriesHolder timeSeriesHolder, DataMgr dataMgr, 
	 							 String tableName,
	 							 String[] physicalElementStringArray, 
	 							 String preferredPhysicalElement )
	{
        
		this( owner, lid, title, timeSeriesHolder,
              System.currentTimeMillis(), System.currentTimeMillis(),
              dataMgr, tableName, 
              physicalElementStringArray,
              preferredPhysicalElement, true ); 
	}

	public ForecastTsSaveDialog(JFrame owner, String lid, String title,
	                             RegularTimeSeriesHolder timeSeriesHolder,
                                 long productTime, long basisTime,
                                 DataMgr dataMgr, String tableName,
                                 String[] physicalElementStringArray,
                                 String preferredPhysicalElement )
	{
		this( owner, lid, 
		      title, timeSeriesHolder,
		      productTime, basisTime, 
		      dataMgr, tableName, 
		      physicalElementStringArray, preferredPhysicalElement,
		      true );
	}

	public ForecastTsSaveDialog(JFrame owner, String lid, String title,
	                             RegularTimeSeriesHolder timeSeriesHolder,
								 long productTime, long basisTime,
								 DataMgr dataMgr, String tableName, 
								 String[] physicalElementStringArray, 
								 String preferredPhysicalElement, 
								 boolean isModal )
	{
		super( owner, isModal );
		this.setTitle(title + ": " + lid );
		_timeSeriesHolder = timeSeriesHolder;
		_timeSeries = _timeSeriesHolder.getTimeSeries(); 
		_basisTime = basisTime;
		_productTime = productTime;
		_lid = lid;
		_dataMgr = dataMgr;
		_tableName = tableName;
		
		
		long currentTime = System.currentTimeMillis();
	    long latestHourTime = TimeHelper.truncateTimeInMillisToNearestHour(currentTime,1);                                                                                                              
		_basisTimeTextField =  new DateTimeTextField(latestHourTime, owner, "Basis Time", 19);
		
		_productTimeTextField =  new DateTimeTextField(latestHourTime, owner, "Product Time", 19);
		
		
        _physicalElementComboBox = new JComboBox(physicalElementStringArray);
		_physicalElementComboBox.setSelectedItem(preferredPhysicalElement);
		_physicalElement = preferredPhysicalElement;
        
		_pElementValueLabel.setText( _physicalElement );
		
		//set up the spinner control for time
		int initialValue = timeSeriesHolder.getTimeSeries().getMeasurementCount();
	    int min = 1;
        int max = initialValue;
        int step = SAVE_HOURS_STEPS;
  
        SpinnerModel model = new SpinnerNumberModel(initialValue, min, max, step);
        _hoursToSaveSpinner = new JSpinner(model);
		
		// load this token and set the initially selected type/source to this default value
		AppsDefaults ad = new AppsDefaults();
		
		_sshpForecastTsToken = ad.getToken("sshp_fcst_ts");
        if (_sshpForecastTsToken == null)
        {
            _sshpForecastTsToken = ad.getToken("SSHP_FCST_TS");
        }
      //  System.out.println("ForecastTsSaveDialog(): _sshpForecastTsToken = " + _sshpForecastTsToken);
		initialize();
	}
	
	private void initialize()
	{	   
	    
		addDataPanelComponents();
		addButtonPanelComponents();
		addFrameComponents();
		
		selectDefaultTypeSource(_sshpForecastTsToken);
		
		addListeners();
		
		setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );
	}
	
	private void selectDefaultTypeSource(String typeSource)
	{
	    
	  //  System.out.println("ForecastTsSaveDialog.selectDefaultTypeSource() typeSource=" + typeSource);
	    if (typeSource == null)
	    {
	        //System.out.println("ForecastTsSaveDialog.selectDefaultTypeSource() since typeSource token was null, use value of FZ");
	        typeSource = "FZ";
	    }
	    
	    _typeSourceComboBox.setSelectedItem(typeSource);
	   
	}
	
	private void addDataPanelComponents()
	{
		_dataPanel.setLayout( new GridBagLayout() );
		JPanel hSeparator[] = new JPanel[ 7 ];
		
		for ( int i = 0; i < hSeparator.length; i++ )
		{
			hSeparator[ i ] = new JPanel();
			hSeparator[ i ].setPreferredSize( new Dimension( 5, 15 ) );
		}
		
		_basisTimeTextField.setText( DbTimeHelper.getDateTimeStringFromLongTime( _basisTime ) );
		_productTimeTextField.setText( DbTimeHelper.getDateTimeStringFromLongTime( _productTime ) );

//																				X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( _dataPanel, _physicalElementLabel, 	0,   0,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _physicalElementComboBox,3,   0,    1,     1, GridBagConstraints.NONE ); 
		ComponentHelper.addPanelComponent( _dataPanel, hSeparator[ 0 ],         0,   1,    4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _typeSourceLabel,        0,   2,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _typeSourceComboBox,     3,   2,    2,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _dataPanel, hSeparator[ 1 ],         0,   3,    4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _shefQualifierLabel,     0,   4,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _shefQualifierComboBox,  3,   4,    2,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _dataPanel, hSeparator[ 2 ],         0,   5,    4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _productIDLabel,         0,   6,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _productIDTextField,     3,   6,    1,     1, GridBagConstraints.BOTH );

		ComponentHelper.addPanelComponent( _dataPanel, hSeparator[ 3 ],         0,   7,    4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _basisTimeLabel, 		0,   8,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _basisTimeTextField, 	3,   8,    1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, hSeparator[ 4 ],         0,   9,    4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _productTimeLabel,       0,  10,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _productTimeTextField,   3,  10,    1,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( _dataPanel, hSeparator[ 5 ],         0,  11,    4,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( _dataPanel, _hoursToSaveLabel,       0,  12,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _dataPanel, _hoursToSaveSpinner,     3,  12,    1,     1, GridBagConstraints.BOTH );
		
		
		ComponentHelper.addPanelComponent( _dataPanel, hSeparator[ 6 ],         0,  13,    4,     1, GridBagConstraints.BOTH );
		
		_basisTimeTextField.setEditable( false );
		_productTimeTextField.setEditable( false );
	}
	
	private void addButtonPanelComponents()
	{
		_buttonPanel.setLayout( new GridBagLayout() );
		
		JPanel horizSeparatorPanel = new JPanel();
		horizSeparatorPanel.setPreferredSize(new Dimension(30, 30));
		_saveButton.setPreferredSize(new Dimension(180, 30));
		_closeButton.setPreferredSize(new Dimension(80, 30));
		
		
//																				X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           0,   0,  1,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _buttonPanel, horizSeparatorPanel,   1,   0,  1,     1, GridBagConstraints.NONE );
		ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          2,   0,  1,     1, GridBagConstraints.BOTH );
	}
	
	
	
	private void addFrameComponents()
	{
		JPanel hLine = new JPanel();
		JPanel hSeparator = new JPanel();
		
		_frameContentPane.setLayout( new GridBagLayout() );
		hLine.setPreferredSize( new Dimension( 300, 1 ) );
		hSeparator.setPreferredSize( new Dimension( 300, 10 ) );
		hLine.setBackground( Color.BLACK );
		
//																				X,   Y,  #Col,  #Row
		ComponentHelper.addFrameComponent( _frameContentPane, _dataPanel, 		0,   0,    4,    13, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, hLine,            0,   13,   4,     1, GridBagConstraints.NONE );
		ComponentHelper.addFrameComponent( _frameContentPane, hSeparator,       0,   14,   4,     1, GridBagConstraints.NONE );
		ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel,     0,   15,   4,     1, GridBagConstraints.BOTH );
	}
	
	private FcstTsDescriptor getFcstTsDescriptor( String tableName )
	{
		FcstTsDescriptor fcstTs = new FcstTsDescriptor( tableName );

		fcstTs.setLid( _lid );
		fcstTs.setTableName( tableName );
		fcstTs.setPe((String) _physicalElementComboBox.getSelectedItem() );
		fcstTs.setTs( (String ) _typeSourceComboBox.getSelectedItem() );

		fcstTs.setExtremum( _extremum );
		fcstTs.setShef_qual_code( (String) _shefQualifierComboBox.getSelectedItem() );
		fcstTs.setProduct_id( _productIDTextField.getText().trim() );

		fcstTs.setProbability( _probability );
		fcstTs.setQuality_code( GOOD_QC );
		fcstTs.setRevision( (short) _revision );
        
        if  ( (tableName.equalsIgnoreCase("FcstHeight")) ||
              (tableName.equalsIgnoreCase("FcstDischarge"))
            )          
        {
            fcstTs.setDur((short)0);
        } 
        else  // evapotranspiration
        {
            int dur = (1000 + _timeSeries.getIntervalInHours());
            fcstTs.setDur((short)dur);
        }    
		
    	fcstTs.setProducttime( _productTimeTextField.getTime() );
		fcstTs.setBasistime( _basisTimeTextField.getTime() );
		
		return fcstTs;
	}
	
	private void addListeners()
	{
		_saveButton.addActionListener( new SaveButtonListener() );
		WindowCloserListener windowCloser = new WindowCloserListener();
		addWindowListener( windowCloser );
		_closeButton.addActionListener( windowCloser );
	//	_basisTimeTextField.addMouseListener( new TimeTextFieldListener() );
	//	_productTimeTextField.addMouseListener( new TimeTextFieldListener() );
		addComponentListener( new WindowResizeComponentListener() );
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
				dateTimeGUI = new DateTimeDialog( ForecastTsSaveDialog.this, dateTime, "Basis Time" );
      
			}
			else
			{
				dateTimeGUI = new DateTimeDialog( ForecastTsSaveDialog.this, dateTime, "Product Time" );
			}
			
			dateTimeGUI.addDateTimeEventListener( new DateTimeListener( textField ) );
			dateTimeGUI.show();
			_basisTime = DbTimeHelper.getLongTimeFromDateTimeString( _basisTimeTextField.getText().trim() );
			_productTime = DbTimeHelper.getLongTimeFromDateTimeString( _productTimeTextField.getText().trim() );
		}
	}
*/	
	/*
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
*/
	private class SaveButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			boolean success = false;
			
			FcstTsDescriptor fcstTs = new FcstTsDescriptor( _tableName );
			
			fcstTs = getFcstTsDescriptor( _tableName );
			
			Integer hoursToSaveInteger = (Integer) _hoursToSaveSpinner.getValue();
			int hoursToSave = hoursToSaveInteger.intValue();
			
			success = _dataMgr.saveFcstTimeSeries( fcstTs, _timeSeries, hoursToSave );
			
			if ( success )
			{
				DialogHelper.displayMessageDialog( ForecastTsSaveDialog.this, "Insert Successful!" );
			}
			else
			{
				DialogHelper.displayMessageDialog( ForecastTsSaveDialog.this, "Insert Failed!" );
			}
			notifyAllApplyActionListeners();
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

	private void closeWindow()
	{
		this.dispose();
	}
	
	private class WindowResizeComponentListener extends ComponentAdapter
	{
		public void componentResized( ComponentEvent e )
		{
			int height = ForecastTsSaveDialog.this.getHeight();
			int width = ForecastTsSaveDialog.this.getWidth();

//			System.err.println( width + "\n" + height );			
			if ( width < MIN_WIDTH ) 
			{ 
				ForecastTsSaveDialog.this.setSize( new Dimension( MIN_WIDTH, height ) );
			}
			if ( width > MAX_WIDTH )
			{
				ForecastTsSaveDialog.this.setSize( new Dimension( MAX_WIDTH, height ) );
			}
			if ( height < MIN_HEIGHT )
			{
				ForecastTsSaveDialog.this.setSize( new Dimension( width, MIN_HEIGHT ) );
			}
			if ( height > MAX_HEIGHT )
			{
				ForecastTsSaveDialog.this.setSize( new Dimension( width, MAX_HEIGHT ) );
			}
		}
	}
	
	private static RegularTimeSeries getTestTimeSeries()
	{
		RegularTimeSeries testTimeSeries = null;
		
		Date date = new Date();
		long currentTime = date.getTime();
		
		currentTime = TimeHelper.truncateTimeInMillisToNearestHour( currentTime, 1 );
		long startTime = currentTime - ( 24*HOUR_IN_MILLIS );
		long endTime = startTime + ( 10*HOUR_IN_MILLIS );
		testTimeSeries = new RegularTimeSeries( startTime, endTime, 1, MeasuringUnit.inches );
		for ( int i = 0; i < testTimeSeries.getMeasurementCount(); i++ )
		{
			Measurement m = new Measurement( i, MeasuringUnit.inches );
			long time = startTime + (i * HOUR_IN_MILLIS);
			testTimeSeries.setMeasurementByTime( m, time );
		}
		return testTimeSeries;
	}

	
	public static void main(String[] args)
	{
		RegularTimeSeries timeSeries = ForecastTsSaveDialog.getTestTimeSeries();
		DataMgr dataMgr = new DataMgr();
		RegularTimeSeriesHolder holder = new RegularTimeSeriesHolder();
		holder.setTimeSeries( timeSeries );
		
		String[] physicalElementStringArray = {"HG", "HP", "HT"};

		ForecastTsSaveDialog saveForecast = 
		       new ForecastTsSaveDialog( new JFrame(), "BLUO2",
		                                 "Save Fcst Time Series Dialog ", 
		                                 holder, dataMgr, 
		                                 "FcstHeight", physicalElementStringArray, "HG" );
		saveForecast.setVisible(true);
	}
}
