/*
 * Created on Oct 28, 2003
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Oct 28, 2003
 *  
 */
package ohd.hseb.util.gui;

import ohd.hseb.db.DbTimeHelper;

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;


public class TimePanel extends JPanel
{
	private DatePanel _datePanel =				null; 	
	
	private int _hours = 						0;
	private int _minutes = 						0;
	private int _seconds = 						0;
	private long _dateTimeLong = 				0;
	
	private JButton _upHourButton = 			new JButton();
	private JButton _upMinuteButton =			new JButton();
	private JButton _upSecondButton = 			new JButton();
	
	private JButton _downHourButton = 			new JButton();
	private JButton _downMinuteButton =			new JButton();
	private JButton _downSecondButton =			new JButton();
	
	private JTextField _hoursTextField = 		new JTextField( 3 );
	private JTextField _minutesTextField = 		new JTextField( 3 );
	private JTextField _secondsTextField =		new JTextField( 3 );
    
    private boolean _allowOnlyEvenHours = false; 
	
	public TimePanel()
	/********************
	Purpose: Empty Constructor
	*********************/
	{
		this( 0 );
	}

	public TimePanel( long dateTimeLong )
	/********************
	Purpose: Constructor with a date/time long value
	*********************/
	{
		int hours = 	0,
			minutes = 	0,
			seconds = 	0;

		String time = DbTimeHelper.getDateTimeStringFromLongTime( dateTimeLong );
		
		hours = Integer.parseInt( time.substring( 11, 13 ) );
		minutes = Integer.parseInt( time.substring( 14, 16 ) );
		seconds = Integer.parseInt( time.substring( 17, 19 ) );

		initTime( hours, minutes, seconds );
		
		initialize();
	}
    
    public void setAllowOnlyEvenHours(boolean allowOnlyEvenHours)
    {
        _allowOnlyEvenHours = allowOnlyEvenHours;  
        
        if (_allowOnlyEvenHours)
        {
            setEnableMinutesAndSeconds(false);
        }
    }

    private void setEnableMinutesAndSeconds(boolean enable)
    {

        _minutesTextField.setEditable(enable);
        _secondsTextField.setEditable(enable); 
   
        _upMinuteButton.setEnabled(enable);
        _downMinuteButton.setEnabled(enable);
   
        _upSecondButton.setEnabled(enable);
        _downSecondButton.setEnabled(enable);
        
        return;   
    }

	
	private void initialize()
	/********************
	Purpose: Initializes the layout and adds the listeners
	*********************/
	{
		this.setLayout( new GridBagLayout() );

		initTimeDisplay();
		
		addListeners();
		
		setSize( 240, 150 );
	}
	
	private void initTimeDisplay()
	/********************
	Purpose: Initializes the Panel
	*********************/
	{
		_upHourButton.setIcon( getUpIcon() );
		_upMinuteButton.setIcon( getUpIcon() );
		_upSecondButton.setIcon( getUpIcon() );
		_downHourButton.setIcon( getDownIcon() );
		_downMinuteButton.setIcon( getDownIcon() );
		_downSecondButton.setIcon( getDownIcon() );

		_upHourButton.setBackground( Color.WHITE );
		_upMinuteButton.setBackground( Color.WHITE );
		_upSecondButton.setBackground( Color.WHITE );
		_downHourButton.setBackground( Color.WHITE );
		_downMinuteButton.setBackground( Color.WHITE );
		_downSecondButton.setBackground( Color.WHITE );
		
							//										      X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( this, _upHourButton,     0,   0,    1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _upMinuteButton,   10,   0,    1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _upSecondButton,     20,   0,    1,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( this, _hoursTextField,    0,  10,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _minutesTextField,  10,  10,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _secondsTextField,    20,  10,    5,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( this, _downHourButton,   0,  20,    1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _downMinuteButton, 10,  20,    1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _downSecondButton,   20,  20,    1,     1, GridBagConstraints.BOTH );
	}
	
	
	private void addListeners()
	/********************
	Purpose: Adds the listeners to the buttons and text fields
	*********************/
	{
		// Listeners for the 6 buttons for the date
		_upHourButton.addActionListener( new UpHourListener() );
		_upMinuteButton.addActionListener( new UpMinuteListener() );
		_upSecondButton.addActionListener( new UpSecondListener() );
		_downHourButton.addActionListener( new DownHourListener() );
		_downMinuteButton.addActionListener( new DownMinuteListener() );
		_downSecondButton.addActionListener( new DownSecondListener() );
		
		// Listeners for the 3 date Text Fields
        _hoursTextField.addFocusListener( new HoursTextFieldFocusListener() );
        _minutesTextField.addFocusListener( new MinutesTextFieldFocusListener() );
        _secondsTextField.addFocusListener( new SecondsTextFieldFocusListener() );
		_hoursTextField.addActionListener( new HoursTextFieldListener() );
		_minutesTextField.addActionListener( new MinutesTextFieldListener() );
		_secondsTextField.addActionListener( new SecondsTextFieldListener() );
	}
	
	private void updateHoursTextField()
    {
        int inputHours = -1;
        
        try
        {
            inputHours = Integer.parseInt( _hoursTextField.getText() );
        }
        catch ( NumberFormatException exception )
        {
        }

        if ( ( inputHours >= 0 ) && ( inputHours <= 23 ) )
        {
            _hours = inputHours;
        } 
        
        updateTime();
    }
    
    private void updateMinutesTextField()
    {
        int inputMinutes = -1;
        
        try
        {
            inputMinutes = Integer.parseInt( _minutesTextField.getText() );
        }
        catch ( NumberFormatException exception )
        {
        }
        
        if ( ( inputMinutes >= 0 ) && ( inputMinutes < 60 ) )
        {
            _minutes = inputMinutes;
        }
        
        updateTime();
    }
    
    private void updateSecondsTextField()
    {
        int inputSeconds = -1;
        
        try
        {
            inputSeconds = Integer.parseInt( _secondsTextField.getText() );
        }
        catch ( NumberFormatException exception )
        {
        }
        
        if ( ( inputSeconds >= 0 ) && ( inputSeconds < 60 ) )
        {
            _seconds = inputSeconds;
        }
        
        updateTime();
    }
    
    private class HoursTextFieldFocusListener implements FocusListener
    {
        public void focusGained(FocusEvent arg0) { }

        public void focusLost(FocusEvent arg0) 
        {
            updateHoursTextField();
        }
    }
    
    private class MinutesTextFieldFocusListener implements FocusListener
    {
        public void focusGained(FocusEvent arg0) { }

        public void focusLost(FocusEvent arg0) 
        {
            updateMinutesTextField();
        }
    }
    
    private class SecondsTextFieldFocusListener implements FocusListener
    {
        public void focusGained(FocusEvent arg0) { }

        public void focusLost(FocusEvent arg0) 
        {
            updateSecondsTextField();
        }
    }
	private class HoursTextFieldListener implements ActionListener
	/********************
	Purpose: Hours text field listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
            updateHoursTextField();
		}
	}

	private class MinutesTextFieldListener implements ActionListener
	/********************
	Purpose: Minutes text field listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
            updateMinutesTextField();
		}
	}

	private class SecondsTextFieldListener implements ActionListener
	/********************
	Purpose: Seconds text field listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
            updateSecondsTextField();
		}
	}

	private class UpHourListener implements ActionListener
	/********************
	Purpose: Up hour button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			if ( _hours < 23 )
			{
				_hours++;
			}
			else
			{
				_hours = 0;
			}
			
			updateTime();
		}
	}
	
	private class UpMinuteListener implements ActionListener
	/********************
	Purpose: Up minute button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			if ( _minutes < 59 )
			{
				_minutes++;
			}
			else
			{
				_minutes = 0;
			}
			
			updateTime();
		}
	}

	private class UpSecondListener implements ActionListener
	/********************
	Purpose: Up second button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			if ( _seconds < 59 )
			{
				_seconds++;
			}
			else
			{
				_seconds = 0;
			}
			
			updateTime();
		}
	}
	
	private class DownHourListener implements ActionListener
	/********************
	Purpose: Down hour button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			if ( _hours > 0 )
			{
				_hours--;
			}
			else
			{
				_hours = 23;
			}
			updateTime();
		}
	}
	
	private class DownMinuteListener implements ActionListener
	/********************
	Purpose: Down minute button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			if ( _minutes > 0 )
			{
				_minutes--;
			}
			else
			{
				_minutes = 59;
			}
			updateTime();
			
		}
	}

	private class DownSecondListener implements ActionListener
	/********************
	Purpose: Down second button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			if ( _seconds > 0 )
			{
				_seconds--;
			}
			else
			{
				_seconds = 59;
			}
			updateTime();
		}
	}
	

	private Icon getUpIcon()
	/********************
	Purpose: Returns the up Icon
	*********************/
	{
		return( StandardImageIcons.getImageIcon( StandardImageIcons.UP_ARROW, Color.BLUE, Color.WHITE, new Dimension( 25, 25 ) ) );
	}
	
	private Icon getDownIcon()
	/********************
	Purpose: Returns the down Icon
	*********************/
	{
		return( StandardImageIcons.getImageIcon( StandardImageIcons.DOWN_ARROW, Color.BLUE, Color.WHITE, new Dimension( 25, 25 ) ) );
	}


	private void updateTime()
	/********************
	Purpose: Updates the time/Panel
	*********************/
	{
		initTime( _hours, _minutes, _seconds );
		if ( _datePanel != null )
		{
			setTime( _dateTimeLong );
		}
	}
	
	public long getDateTimeLong() 
	/********************
	Purpose: Return a long value for the date/time
	*********************/
	{
		long dateTimeLong = 0;
		long dateLong = 0;
		StringBuffer dateTimeBuffer = new StringBuffer();
		String dateTimeString = null;
		
		dateLong = _datePanel.getDateLong();
		
 		dateTimeBuffer.append( DbTimeHelper.getDateStringFromLongTime( dateLong ) + " " );
		dateTimeBuffer.append( _hours + ":" + _minutes + ":" + _seconds );
		dateTimeString = dateTimeBuffer.toString();
		dateTimeLong = DbTimeHelper.getLongTimeFromDateTimeString( dateTimeString );

		return dateTimeLong;
	}
	
	public String getTimeString()
	{
		return _hours + ":" + _minutes + ":" + _seconds;
	}
	

	public DatePanel getDatePanel()
	/********************
	Purpose: Returns the datePanel object
	*********************/
	{
		return _datePanel;
	}
	
	public void linkDateTime( DatePanel datePanel )
	/********************
	Purpose: links the date/time objects
	*********************/
	{
		_datePanel = datePanel;
		if ( _datePanel.getTimePanel() != this )
		{
			_datePanel.linkDateTime( this );
		}
	}

	public void setTime( long dateTimeLong, boolean recurse )
	/********************
	Purpose:
	*********************/
	{
		int hours = 	0,
			minutes = 	0,
			seconds = 	0;

		String time = DbTimeHelper.getDateTimeStringFromLongTime( dateTimeLong );
		
		hours = Integer.parseInt( time.substring( 11, 13 ) );
		minutes = Integer.parseInt( time.substring( 14, 16 ) );
		seconds = Integer.parseInt( time.substring( 17, 19 ) );

		initTime( hours, minutes, seconds );
		
		if ( ( _datePanel != null ) && ( recurse ) )
		{
			_datePanel.setDate( dateTimeLong, false );
		}
	}

	public void setTime( long dateTimeLong )
	/********************
	Purpose:
	*********************/
	{
		setTime( dateTimeLong, true );
	}	
	
	private void initTime( int hours, int minutes, int seconds )
	/********************
	Purpose: Initializes the time values/Textfields
	*********************/
	{
		String dateTimeString = null;

		_hoursTextField.setText( "" + hours );
		_minutesTextField.setText( "" + minutes );
		_secondsTextField.setText( "" + seconds );
		_hours = hours;
		_minutes = minutes;
		_seconds = seconds;
		
		if ( _datePanel != null )
		{
			dateTimeString = _datePanel.getDateString();
			
			dateTimeString += " " + _hours + ":" + _minutes + ":" + _seconds;
						
			_dateTimeLong = DbTimeHelper.getLongTimeFromDateTimeString( dateTimeString );
		}
		
//		setTime( _dateTimeLong );
	}

	public static void main(String[] args) 
	{
		Date date = new Date();
		TimePanel TimePanel = new TimePanel( date.getTime() );
	}
}
