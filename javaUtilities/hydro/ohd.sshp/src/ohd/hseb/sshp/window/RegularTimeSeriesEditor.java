/*
 * Created on Nov 26, 2003
 *
 * Filename : RegularTimeSeriesEditor.java
 * Author   : Gautam Sood
 * Last Revision Date : Nov 26, 2003
 *  
 */

package ohd.hseb.sshp.window;

import ohd.hseb.measurement.*;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.UnitValueMapper;
import ohd.hseb.util.ValueMapper;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;

import java.util.*;
import java.util.List;
import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;

public class RegularTimeSeriesEditor extends JDialog
{
	private RegularTimeSeriesHolder _timeSeriesHolder = 				new RegularTimeSeriesHolder();

	private TimeSeriesListener _timeSeriesListener =                    null;

	private List _absTimeMeasurementAndFieldsList = 					new ArrayList();
	private AbsTimeMeasurementAndFields _absTimeMeasurementAndFields =	null;
	
	//nice initial scrolling members
	private AbsTimeMeasurementAndFields _searchAbsTimeMeasurementAndFields = null;
	private JPanel _searchMeasurementAndFieldsPanel = null;
	
	
	private boolean _isEditable = 										true;
	private long _startTime = 											0;
	private long _endTime = 											0;
	private long _intervalInHours = 									0;
	private long _intervalInMillis = 									0;
	private long _searchTime = 											0;
	private static final long HOUR_IN_MILLIS = 							60 * 60 * 1000;
	private DecimalFormat _decimalFormat = 								null;
	private static final String _defaultFormatString = 					"0.00";					
	private String _formatString =										_defaultFormatString;
	private ValueMapper _valueMapper = 									null;
	private String _mappedValueLabelString = 							null;
	private double _missingValue = 										0.0;
	private boolean _displayMappedValue = 								false;
	private boolean _wasWindowActivated = 								false;
	private double _minValue = 											0.0;
	private double _maxValue = 											0.0;
	
	private static final int DEFAULT_WIDTH =							550;
	private static final int DEFAULT_HEIGHT = 							450;
	
	private static final int MAX_WIDTH = 								600;
	private static final int MAX_HEIGHT =								5000;

	private static final int MIN_WIDTH = 								450;
	private static final int MIN_HEIGHT = 								312;
	
	private Container _frameContentPane = 								getContentPane();
	private JButton _applyButton = 										new JButton( "Apply" );
	private JButton _closeButton = 										new JButton( "Close" );
	private JLabel _timeLabel = 										new JLabel( "Time (GMT) " );
	private JLabel _valueLabel = 										null;
	private JLabel _mappedValueLabel = 									null;
	private JLabel _missingValueLabel = 								new JLabel( "<HTML><CENTER>Missing<BR>Value?</CENTER></HTML>" );
	private JPanel _dataPanel = 										new JPanel();
	private JPanel _buttonPanel = 										new JPanel();
	private JPanel _labelPanel = 										new JPanel();
	private JScrollPane _dataScrollPane = 								new JScrollPane( _dataPanel );
	
	//used for custom notification
	//private List _applyListenerList =								    new ArrayList();
	
	// ------------------------------------------------------------------------------------------

	public RegularTimeSeriesEditor( RegularTimeSeriesEditorDescriptor descriptor )
	{
		super( descriptor.getOwner(), descriptor.isModal() );
		this.setTitle( descriptor.getTitleLabelString() );
		_valueLabel = new JLabel( descriptor.getValueLabelString() );
		_timeSeriesHolder = descriptor.getRegularTimeSeriesHolder();
		_isEditable = descriptor.isEditable();
		
		_valueMapper = descriptor.getValueMapper();
		_mappedValueLabelString = descriptor.getMappedValueLabelString();
		
		if ( _valueMapper != null )
		{
			_mappedValueLabel = new JLabel( _mappedValueLabelString );
			_displayMappedValue = true;
		}
		_searchTime = descriptor.getInitialSearchTime();
		
		String searchTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_searchTime);
		
		_formatString = descriptor.getTextFieldFormatString();
		_missingValue = descriptor.getMissingValue();
		_decimalFormat = new DecimalFormat( _formatString );
		_minValue = descriptor.getMinValue();
		_maxValue = descriptor.getMaxValue();
			
		initialize();
	}
	
//	 ------------------------------------------------------------------------------------------


	private void initialize()
	{
		_startTime = getTimeSeries().getStartTime();
		_endTime = getTimeSeries().getEndTime();
		_intervalInHours = getTimeSeries().getIntervalInHours();
		_intervalInMillis = _intervalInHours * HOUR_IN_MILLIS;

		addLabelPanelComponents();
		addDataPanelComponents();
		addButtonPanelComponents();
		addFrameComponents();
		addListeners();
		
		refreshDataPanel();
		
		setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );
	}
	
//	 ------------------------------------------------------------------------------------------
  
    private void initializeScrollAmount()
    {
        
        int fieldHeight = 50;
        if (_absTimeMeasurementAndFieldsList.size() > 0)
        {
               AbsTimeMeasurementAndFields measurementAndFields = 
                             (AbsTimeMeasurementAndFields) _absTimeMeasurementAndFieldsList.get(0);
               fieldHeight = measurementAndFields._valueTextField.getHeight();
        }
      
        // set the amount to scroll when clicking on the slider bar
        _dataScrollPane.getVerticalScrollBar().setUnitIncrement( 2* fieldHeight );
        
        return;
    }
//  ------------------------------------------------------------------------------------------
  
    private void initializeScrollPosition()
    {
//      initialize the location of the slider bar
        if ( ( _searchTime != 0 ) && ( _searchAbsTimeMeasurementAndFields != null ) )
        {
             // rect is set to the dimensions of one of the value Text Fields
             // so that we can use the y value
             Rectangle rect = _searchAbsTimeMeasurementAndFields._valueTextField.getBounds();
             
             _searchAbsTimeMeasurementAndFields._valueTextField.requestFocus( true );
             
             Rectangle parentPanelRect = _searchMeasurementAndFieldsPanel.getBounds();
               
             // Sets the y value of rectangle to the middle of the visible screen.
     
             int newY = rect.y + parentPanelRect.y;
             if (newY < 0)
             {
                 newY = 0;    
             }
             int newX = rect.x;
            
             Point viewLocation = new Point (newX, newY);
      
           //  String header = "RegularTimeSeriesEditor.initializeScrollPosition()";
           //  System.out.println(header + "trying to set viewPosition to  " + viewLocation);
             
         //    System.out.println(header + "before setting, getViewPosition() = " +  _dataScrollPane.getViewport().getViewPosition());
             _dataScrollPane.getViewport().setViewPosition(viewLocation);
             
            
         //    System.out.println(header + "after setting, getViewPosition() = " +  _dataScrollPane.getViewport().getViewPosition());
        }
        
    }
//  ------------------------------------------------------------------------------------------
  
	private void initializeScrolling()
	{
        // Sets the scroll bar increment to 2 text fields per click
        initializeScrollAmount();
        
        initializeScrollPosition();
        
        return;
	}
//	 ------------------------------------------------------------------------------------------
	
	private void addFrameComponents()
	{
		_frameContentPane.setLayout( new GridBagLayout() );

		JPanel hSeparatorPanel = 	new JPanel();
		JPanel hSeparatorPanel2 = 	new JPanel();
		
		hSeparatorPanel.setPreferredSize( new Dimension( 300, 5 ) );
		hSeparatorPanel2.setPreferredSize( new Dimension( 300, 20 ) );
	
//																	   		    X,  Y,  #Col, #Row
		ComponentHelper.addFrameComponent( _frameContentPane, _labelPanel,      0,  0,   11,    1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, hSeparatorPanel,  0,  1,   11,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _dataScrollPane,  0,  3,   11,   20, 1, 1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, hSeparatorPanel2, 0, 23,   11,    2, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel,     0, 25,   11,    2, GridBagConstraints.BOTH );
	}
//	 ------------------------------------------------------------------------------------------

	private void addLabelPanelComponents()
	{
		JPanel verticalSeparatorPanel = 	new JPanel();
		JPanel verticalSeparatorPanel2 = 	new JPanel();
		JPanel verticalSeparatorPanel3 = 	new JPanel();
		
		_labelPanel.setLayout( new GridBagLayout() );
		verticalSeparatorPanel.setPreferredSize( new Dimension( 100, 15 ) );
		verticalSeparatorPanel2.setPreferredSize( new Dimension( 65, 15 ) );
		verticalSeparatorPanel3.setPreferredSize( new Dimension( 65, 15 ) );

//																				  X,   Y,  #Col, #Row
		ComponentHelper.addFrameComponent( _labelPanel, _timeLabel, 		      0,   0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _labelPanel, verticalSeparatorPanel,   1,   0,   4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _labelPanel, _valueLabel, 		      5,   0,   1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _labelPanel, verticalSeparatorPanel2,  6,   0,   4,     1, GridBagConstraints.BOTH );
		if ( _mappedValueLabel != null )
		{
			ComponentHelper.addFrameComponent( _labelPanel, _mappedValueLabel,       10,   0,   1,     1, GridBagConstraints.BOTH );
			ComponentHelper.addFrameComponent( _labelPanel, verticalSeparatorPanel3, 11,   0,   4,     1, GridBagConstraints.BOTH );
			ComponentHelper.addFrameComponent( _labelPanel, _missingValueLabel,      15,  0,   1,     1, GridBagConstraints.BOTH );
		}
		else
		{
			ComponentHelper.addFrameComponent( _labelPanel, _missingValueLabel,      10,  0,   1,     1, GridBagConstraints.BOTH );
		}			
	}
//	 ------------------------------------------------------------------------------------------

	private void addDataPanelComponents()
	{
	    //String header = "RegularTimeSeriesEditor.addDataPanelComponents():";
		int counter = 0;
		
		int yCoor = 1;
		int yPanelCounter = 0;

		_dataPanel.setLayout( new GridBagLayout() );

		JPanel subDataPanel = new JPanel();
		
		for ( long time = _startTime; time <= _endTime; time += _intervalInMillis )
		{
			if ( ( counter % 400 ) == 0 ) 
			{
				JPanel verticalSeparatorPanel = 	new JPanel();
				JPanel verticalSeparatorPanel2 = 	new JPanel();
				JPanel verticalSeparatorPanel3 = 	new JPanel();
				verticalSeparatorPanel3.setPreferredSize( new Dimension( 40, 20 ) );
				verticalSeparatorPanel2.setPreferredSize( new Dimension( 60, 20 ) );
				verticalSeparatorPanel.setPreferredSize( new Dimension( 20, 25 ) );

				subDataPanel = new JPanel();
				yCoor = 0;
				counter = 0;
				subDataPanel.setLayout( new GridBagLayout() );
				ComponentHelper.addPanelComponent( subDataPanel, verticalSeparatorPanel2, 2, 0, 2, 20, GridBagConstraints.BOTH );
				ComponentHelper.addPanelComponent( subDataPanel, verticalSeparatorPanel, 6, 0, 1, 20, GridBagConstraints.BOTH );
				ComponentHelper.addPanelComponent( subDataPanel, verticalSeparatorPanel3, 8, 0, 1, 20, GridBagConstraints.BOTH );

//																			 X,       Y,     #Col, #Row
				ComponentHelper.addPanelComponent( _dataPanel, subDataPanel, 0, yPanelCounter, 1,   1, GridBagConstraints.BOTH );
				yPanelCounter++;
			}
			
			Measurement measurement = getTimeSeries().getMeasurementByTime( time );
			double value = measurement.getValue();

			_absTimeMeasurementAndFields = new AbsTimeMeasurementAndFields( time, value );
			_absTimeMeasurementAndFieldsList.add( _absTimeMeasurementAndFields );
			
			if ( time == _searchTime )
			{
				_searchAbsTimeMeasurementAndFields = _absTimeMeasurementAndFields;
				String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
				
				_searchMeasurementAndFieldsPanel = subDataPanel;
			//	System.out.println(header + "timeString = " + timeString);
			//	System.out.println(header + "yPanelCounter = " + yPanelCounter); 
			}
//																									           X,   Y,  #Col, #Row
			ComponentHelper.addPanelComponent( subDataPanel, _absTimeMeasurementAndFields._valueDateTimeLabel,  0, yCoor, 1,    1, GridBagConstraints.BOTH );
			ComponentHelper.addPanelComponent( subDataPanel, _absTimeMeasurementAndFields._valueTextField,      4, yCoor, 1,    1, GridBagConstraints.BOTH );
			if ( _displayMappedValue )
			{
				ComponentHelper.addPanelComponent( subDataPanel, _absTimeMeasurementAndFields._mappedValueTextField, 7, yCoor, 1,    1, GridBagConstraints.BOTH );
				ComponentHelper.addPanelComponent( subDataPanel, _absTimeMeasurementAndFields._missingValueCheckBox, 10, yCoor, 1,    1, GridBagConstraints.BOTH );
			}
			else
			{
				ComponentHelper.addPanelComponent( subDataPanel, _absTimeMeasurementAndFields._missingValueCheckBox, 7, yCoor, 1,    1, GridBagConstraints.BOTH );
			}
			yCoor++;
			counter++;
		}
		_dataScrollPane.setPreferredSize( new Dimension( 375, 200 ) );
	}
//	 ------------------------------------------------------------------------------------------

	private void addButtonPanelComponents()
	{
		JPanel verticalSeparatorPanel = new JPanel();
		
		_buttonPanel.setLayout( new GridBagLayout() );
		verticalSeparatorPanel.setPreferredSize( new Dimension( 80, 20 ) );

//																				 X, Y, #Col, #Row
		ComponentHelper.addPanelComponent( _buttonPanel, _applyButton, 			 0, 0,   2,    2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, verticalSeparatorPanel, 2, 0,   3,    2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, _closeButton, 			 5, 0,   2,    2, GridBagConstraints.BOTH );
		if ( ! _isEditable )
		{
			_applyButton.setVisible( false );
		}
	}
//	 ------------------------------------------------------------------------------------------

	private RegularTimeSeries getTimeSeries()
	{
		return _timeSeriesHolder.getTimeSeries();
	}
//	 ------------------------------------------------------------------------------------------

	private void addListeners()
	{
        _timeSeriesListener = new UpdateTimeSeriesListener();
        _timeSeriesHolder.addListener(_timeSeriesListener);
        
		_applyButton.addActionListener( new ApplyButtonListener() );
		
		
		
		
		
		WindowEventManager windowEventManager = new WindowEventManager();
		_closeButton.addActionListener( windowEventManager );
		addWindowListener( windowEventManager );
		addComponentListener( new WindowResizeComponentListener() );
	}
//	 ------------------------------------------------------------------------------------------

	private class ApplyButtonListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			updateTimeSeries();
			refreshDataPanel();

			//notification occurs in updateTimeSeries(
    	}
	}

//	 ------------------------------------------------------------------------------------------

	private class ValueChangedFocusListener implements FocusListener
    {
           
           public ValueChangedFocusListener ()
           {
             
           }
        
           public void focusGained(FocusEvent event)
           {

           }

           public void focusLost(FocusEvent event)
           {
               updateTimeSeries();
   			   refreshDataPanel();

           } //end focusLost       
        
    }
//	 ------------------------------------------------------------------------------------------
/*
	public void addApplyActionListener(ActionListener listener)
	{
		_applyListenerList.add(listener);
	}
*/
//	 ------------------------------------------------------------------------------------------
/*
	private void notifyAllApplyActionListeners()
	{
		for (int i = 0; i < _applyListenerList.size(); i++)
		{
			ActionListener listener = (ActionListener) _applyListenerList.get(i);
			ActionEvent event = new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "APPLY action");
			listener.actionPerformed(event);
		}
	}
*/
//	 ------------------------------------------------------------------------------------------
	
	private void updateTimeSeries()
	{
		int index = 0;
		RegularTimeSeries timeSeries = new RegularTimeSeries(getTimeSeries().getStartTime(),
		                                                     getTimeSeries().getEndTime(),
		                                                     getTimeSeries().getIntervalInHours(),
		                                                     getTimeSeries().getMeasuringUnit() );
		boolean success = true;
		
		for ( long time = _startTime; time <= _endTime ; time += _intervalInMillis )
		{
			AbsTimeMeasurementAndFields absTimeMeasurementAndFields = null;
				
			absTimeMeasurementAndFields =
			        (AbsTimeMeasurementAndFields) _absTimeMeasurementAndFieldsList.get( index );
			try
			{
				String textFieldString = absTimeMeasurementAndFields._valueTextField.getText().trim();
				double value = Double.parseDouble( textFieldString );
				Measurement measurement = new Measurement( value , timeSeries.getMeasuringUnit() );
    		   	timeSeries.setMeasurementByTime( measurement, time );
            
			}
			catch ( NumberFormatException e )
			{
                DialogHelper.displayMessageDialog(null,
                                                  "Non-numerical values are not allowed!", 
                                                  "Error Message" );
                success = false;
                                                  
				//JOptionPane.showMessageDialog( null, "Non-numerical values are not allowed!", 
				//							   "Error Message", JOptionPane.ERROR_MESSAGE );
			}
			index++;
		}
		
		if ( success )
		{
			_timeSeriesHolder.setTimeSeries( timeSeries );
            
            TimeSeriesEvent event =  new TimeSeriesEvent("RegularTimeSeriesEditor Update",
                                                          RegularTimeSeriesEditor.this);
            _timeSeriesHolder.forwardEvent(event);
		}
	}

	private class WindowEventManager extends WindowAdapter implements ActionListener
	{
		public void windowClosing ( WindowEvent e )
		{
        	closeWindow();
		}
		
		public void actionPerformed( ActionEvent e )
		{
			closeWindow();
		}
		
		public void windowActivated( WindowEvent e )
		{
			if ( ! _wasWindowActivated )
			{
				initializeScrolling();
				repaint();
				_wasWindowActivated = true;
			}
		}
	}
//	 ------------------------------------------------------------------------------------------

	private class WindowResizeComponentListener extends ComponentAdapter
	{
		public void componentResized( ComponentEvent e )
		{
			int height = RegularTimeSeriesEditor.this.getHeight();
			int width = RegularTimeSeriesEditor.this.getWidth();

//			System.err.println( width + "\n" + height );			
			if ( width <= MIN_WIDTH ) 
			{ 
				RegularTimeSeriesEditor.this.setSize( new Dimension( MIN_WIDTH, height ) );
				width = MIN_WIDTH;
			}
			if ( width >= MAX_WIDTH )
			{
				RegularTimeSeriesEditor.this.setSize( new Dimension( MAX_WIDTH, height ) );
				width = MAX_WIDTH;
			}
			if ( height <= MIN_HEIGHT )
			{
				RegularTimeSeriesEditor.this.setSize( new Dimension( width, MIN_HEIGHT ) );
				height = MIN_HEIGHT;
			}
			if ( height >= MAX_HEIGHT )
			{
				RegularTimeSeriesEditor.this.setSize( new Dimension( width, MAX_HEIGHT ) );
				height = MAX_HEIGHT;
			}
		}
	}
//	 ------------------------------------------------------------------------------------------

	private void closeWindow()
	{
        //don't want to get messages when this window is gone
        _timeSeriesHolder.removeListener(_timeSeriesListener);
        
        //make it go away
		this.dispose();
	}
//	 ------------------------------------------------------------------------------------------
	
	private String getFormattedValue( double value )
	{
		return _decimalFormat.format( value );
	}
//	 ------------------------------------------------------------------------------------------
	
	private void refreshDataPanel()
	{
	   // String header = "RegularTimeSeriesEditor.refreshDataPanel():";
		int index = 0;

		_startTime = getTimeSeries().getStartTime();
		_endTime = getTimeSeries().getEndTime();
		
		//System.out.println(header + "called." );
		
		for ( long time = _startTime; time <= _endTime; time += _intervalInMillis )
		{
			Measurement measurement = getTimeSeries().getMeasurementByTime( time );
			double value = measurement.getValue();
			_absTimeMeasurementAndFields = (AbsTimeMeasurementAndFields) _absTimeMeasurementAndFieldsList.get( index );
			_absTimeMeasurementAndFields.refresh( time, value );
			index++;
		}
     // System.out.println(header + "getViewPosition() = " +  _dataScrollPane.getViewport().getViewPosition());
	}
//	 ------------------------------------------------------------------------------------------

	private static RegularTimeSeries getTestTimeSeries()
	{
		RegularTimeSeries testTimeSeries = null;
		
		Date date = new Date();
		long currentTime = date.getTime();
		
		currentTime = TimeHelper.truncateTimeInMillisToNearestHour( currentTime, 1 );
		long startTime = currentTime - ( 24*HOUR_IN_MILLIS );
		long endTime = startTime + ( 100*HOUR_IN_MILLIS );
		testTimeSeries = new RegularTimeSeries( startTime, endTime, 1, MeasuringUnit.inches );
		for ( int i = 0; i < testTimeSeries.getMeasurementCount(); i++ )
		{
		    long time = startTime + (i * HOUR_IN_MILLIS);
			Measurement m = new Measurement( i, MeasuringUnit.inches );
			testTimeSeries.setMeasurementByTime( m, time );
		}
		return testTimeSeries;
	}
//	 ------------------------------------------------------------------------------------------

	private class AbsTimeMeasurementAndFields
	{
		private JLabel _valueDateTimeLabel = 				new JLabel();
		private JFormattedTextField _valueTextField = 		null;
		private JFormattedTextField _mappedValueTextField = null;
		private JCheckBox _missingValueCheckBox =			new JCheckBox();
		private boolean _isMissingValue = 					false;
		private long _time = 								0;
		
		public AbsTimeMeasurementAndFields( long time, double value )
		{
			_time = time;
			
			_valueTextField = new JFormattedTextField( _decimalFormat );
			_mappedValueTextField = new JFormattedTextField( _decimalFormat );
			_valueTextField.setColumns( 8 );
			_mappedValueTextField.setColumns( 8 );
			_mappedValueTextField.setEditable( false );
			_mappedValueTextField.setFocusable( false );
			
			if ( _isEditable )
			{
				_valueTextField.setEditable( true );
			}
			else
			{
				_valueTextField.setEditable( false );
				_valueTextField.setFocusable( false );
				_missingValueCheckBox.setEnabled( false );
			}
			String timeString = DbTimeHelper.getDateTimeStringFromLongTime( time );
			_valueDateTimeLabel.setText( timeString );
			_valueTextField.setText( getFormattedValue( value ) );
			if ( _displayMappedValue )
			{
				_mappedValueTextField.setText( getMappedValueString( value ) );
			}
			_missingValueCheckBox.setSelected( false );
			addListeners();
		}
		
		public void refresh( long time, double value )
		{
			_time = time;
			_valueTextField.setText( getFormattedValue( value ) );
			if ( _displayMappedValue )
			{
				if ( ! _isMissingValue )
				{
					_mappedValueTextField.setText( getMappedValueString( value ) );
				}
			}				
		}
		
		private String getMappedValueString( double value )
		{
			double mappedValue = 0;
			String mappedValueString = null;
			
			if ( value != _missingValue )
			{
				Measurement measurement = new Measurement( value, getTimeSeries().getMeasuringUnit() );
				Measurement mappedMeasurement = _valueMapper.getResultMeasurement( measurement );
				mappedValue = mappedMeasurement.getValue();
			
			}
			else
			{
				mappedValue = _missingValue;
			}

			mappedValueString = getFormattedValue( mappedValue );
			return mappedValueString;
		}

		private void addListeners()
		{
			_missingValueCheckBox.addActionListener( new MissingValueCheckBoxListener() );
			_valueTextField.addKeyListener( new PrecipTextFieldListener() );
			_valueTextField.addFocusListener( new FocusLostFocusListener() );
			
			//_valueTextField.addFocusListener(RegularTimeSeriesEditor.this._focusListener);
		}
		
		private class MissingValueCheckBoxListener implements ActionListener
		{
			public void actionPerformed( ActionEvent e )
			{
				_isMissingValue = _missingValueCheckBox.isSelected();
				if ( _isMissingValue )
				{
					_valueTextField.setText( "" + _missingValue );
					if ( _displayMappedValue )
					{
						_mappedValueTextField.setText( "" + _missingValue );
					}				
				}
				else 
				{
					double value = getTimeSeries().getMeasurementValueByTime( _time, getTimeSeries().getMeasuringUnit() );
					_valueTextField.setText( getFormattedValue( value ) ); 
					if ( _displayMappedValue )
					{
						_mappedValueTextField.setText( getMappedValueString( value ) );
					}				
				}					
			}
		}
		
		private class PrecipTextFieldListener implements KeyListener
		{
			public void keyPressed(KeyEvent arg0)
			{
				_missingValueCheckBox.setSelected( false );
				_isMissingValue = _missingValueCheckBox.isSelected();
			}

			public void keyReleased(KeyEvent arg0)
			{
			}

			public void keyTyped(KeyEvent arg0)
			{
			}
		}
		
		private class FocusLostFocusListener implements FocusListener
		{
            private double parseDouble(String valueString)
            {
                
                double value = 0.0;
                
                try
                {     
                    value = Double.parseDouble( valueString );
                }
                catch (java.lang.NumberFormatException e) 
                {
                    value = 0.0;
                    _valueTextField.setText(getFormattedValue(value));
                }
                
                return value;
            }
            
            
			public void focusGained(FocusEvent arg0)
			{
			}

			public void focusLost(FocusEvent arg0)
			{
				String valueString = _valueTextField.getText().trim();
				double value = parseDouble( valueString );
				
				if ( valueString.length() == 0 )
				{
					_valueTextField.setText( getFormattedValue(value));
				}
				
				if ( ! _missingValueCheckBox.isSelected() )
				{
					if ( value < _minValue )
					{
						_valueTextField.setText( "" + _minValue );
					}
					
					if ( value > _maxValue )
					{
						_valueTextField.setText( "" + _maxValue );
					}
					
					valueString = _valueTextField.getText().trim();
					value = parseDouble( valueString );
				}

				if ( _displayMappedValue )
				{
					_mappedValueTextField.setText( getMappedValueString( value ) );
				}
				
				//Allows for an immediate update of precip once values have been changed.
		        updateTimeSeries();
	   			//refreshDataPanel();

			}
		}
		
	}
//	 ------------------------------------------------------------------------------------------
 
    private class UpdateTimeSeriesListener implements TimeSeriesListener
    {
        public Object getReceiver()
        {
            return RegularTimeSeriesEditor.this;    
        }
        
        public void handleTimeSeriesEvent(TimeSeriesEvent event)
        {
            if (event.getSource() != RegularTimeSeriesEditor.this)
            {
               // System.out.println("UpdateTimeSeriesListener event  = " + event.toString());
                refreshDataPanel();
            }   
        }
        
    }
//  ------------------------------------------------------------------------------------------

	public static void main( String args[] )
	{
		String valueLabel = null;
		String titleLabel = null;
		
		RegularTimeSeries timeSeries = RegularTimeSeriesEditor.getTestTimeSeries();
		RegularTimeSeriesHolder holder = new RegularTimeSeriesHolder();
		holder.setTimeSeries( timeSeries );
		
		valueLabel = "MAP(" + timeSeries.getMeasuringUnit() + ")";
		titleLabel = "Forecast Stage Editor";
		ValueMapper mapper = new UnitValueMapper( MeasuringUnit.feet, MeasuringUnit.mm );
		
		RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor( new Frame(), holder, valueLabel );
		descriptor.setValueMapper( mapper );
		descriptor.setMappedValueLabelString( "cfs" );
		descriptor.setTitleLabelString( titleLabel );
		descriptor.setInitialSearchTime( System.currentTimeMillis() );
		descriptor.setMissingValue( -9999.0 );
		descriptor.setMinValue( 0.0 );
		descriptor.setMaxValue( 20.0 );
		
		RegularTimeSeriesEditor editor = new RegularTimeSeriesEditor( descriptor );
		editor.show();
	}
//	 ------------------------------------------------------------------------------------------

}


