/*
 * Created on Oct 23, 2003
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Oct 30, 2003
 *  
 */
package ohd.hseb.util.gui;

import ohd.hseb.db.DbTimeHelper;

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class DatePanel extends JPanel
{
	private TimePanel _timePanel =				null;
	
	private int _month = 						0;
	private int _day = 							0;
	private int _year = 						0;
	private long _dateTimeLong = 				0;
	private boolean _leapYear = 				false;

	private JButton _upMonthButton = 			new JButton();
	private JButton _upDayButton = 				new JButton();
	private JButton _upYearButton = 			new JButton();

	private JButton _downMonthButton = 			new JButton();
	private JButton _downDayButton = 			new JButton();
	private JButton _downYearButton = 			new JButton();

	private JTextField _monthTextField = 		new JTextField( 6 );
	private JTextField _dayTextField = 			new JTextField( 3 );
	private JTextField _yearTextField =			new JTextField( 4 );

	private static Map _monthNamesMap = 		new HashMap();
	private static Map _dayMaxMap = 			new HashMap();

	public DatePanel()
	/********************
	Purpose: Empty Constructor 
	*********************/
	{
		this( new Date().getTime() );
	}
	
	public DatePanel( long dateLong )
	/********************
	Purpose: Constructor with a long date value
	*********************/
	{
		int month, day, year;

		_dateTimeLong = dateLong;
		
		setupMonthNamesMap();

		String date = DbTimeHelper.getDateStringFromLongTime( dateLong );
		
		year = Integer.parseInt( date.substring( 0, 4 ) );
		month = Integer.parseInt( date.substring( 5, 7 ) );
		day = Integer.parseInt( date.substring( 8, 10 ) );
		
		initDate( month, day, year );
		execute();
	}
	
	private void execute()
	/********************
	Purpose: Main execution of the program
	*********************/
	{		
		this.setLayout( new GridBagLayout() );
		
		setupDayMaxMap();
		
		initDateDisplay();
		
		addListeners();
		setSize( 240, 150 );
	}
	
	private void initDateDisplay()
	/********************
	Purpose: Initializes the GUI 
	*********************/
	{
		_upYearButton.setIcon( getUpIcon() );
		_upMonthButton.setIcon( getUpIcon() );
		_upDayButton.setIcon( getUpIcon() );
		_downYearButton.setIcon( getDownIcon() );
		_downMonthButton.setIcon( getDownIcon() );
		_downDayButton.setIcon( getDownIcon() );



		_upYearButton.setBackground( Color.WHITE );
		_upMonthButton.setBackground( Color.WHITE );
		_upDayButton.setBackground( Color.WHITE );
		_downYearButton.setBackground( Color.WHITE );
		_downMonthButton.setBackground( Color.WHITE );
		_downDayButton.setBackground( Color.WHITE );

							//										      X,   Y,  #Col,  #Row
		ComponentHelper.addPanelComponent( this, _upYearButton,     0,   0,    1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _upMonthButton,   10,   0,    4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _upDayButton,     20,   0,    1,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( this, _yearTextField,    0,  10,    3,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _monthTextField,  10,  10,    4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _dayTextField,    20,  10,    5,     1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( this, _downYearButton,   0,  20,    1,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _downMonthButton, 10,  20,    4,     1, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( this, _downDayButton,   20,  20,    1,     1, GridBagConstraints.BOTH );
	}

	
	private void initDate( int month, int day, int year )
	/********************
	Purpose: Initializes the date text fields and the month, day, year member variables
	*********************/
	{
		String dateTimeString = null;
		
		_monthTextField.setText( "" + _monthNamesMap.get( new Integer( month ) ) );
		_dayTextField.setText( "" + day );
		_yearTextField.setText( "" + year );
		_month = month;
		_year = year;
		_day = day;
		if ( _timePanel != null )
		{
			dateTimeString = _year + "-" + _month + "-" + _day + " " + _timePanel.getTimeString();
			_dateTimeLong = DbTimeHelper.getLongTimeFromDateTimeString( dateTimeString );
		}
	}
	
	public String getDateString()
	{
		return _year + "-" + _month + "-" + _day;
	}
	
	
	private void updateDate()
	/********************
	Purpose: Updates the date/display by calling initDate  
	*********************/
	{
		initDate( _month, _day, _year );
		if ( _timePanel != null )
		{
			setDate( _dateTimeLong );
		}
	}

	
	private ImageIcon getUpIcon()
	/********************
	Purpose: Returns the Up Arrow icon
	*********************/
	{
		return( StandardImageIcons.getImageIcon( StandardImageIcons.UP_ARROW, Color.BLUE, Color.WHITE, new Dimension( 25, 25 ) ) );
	}
	
	private ImageIcon getDownIcon()
	/********************
	Purpose: Returns the Down Arrow icon 
	*********************/
	{
		return( StandardImageIcons.getImageIcon( StandardImageIcons.DOWN_ARROW, Color.BLUE, Color.WHITE, new Dimension( 25, 25 ) ) );
	}
	
	private void addListeners()
	/********************
	Purpose: Adds all of the listeners 
	*********************/
	{
		// Listeners for the 6 buttons for the date
		_upYearButton.addActionListener( new UpYearListener() );
		_upMonthButton.addActionListener( new UpMonthListener() );
		_upDayButton.addActionListener( new UpDayListener() );
		_downYearButton.addActionListener( new DownYearListener() );
		_downMonthButton.addActionListener( new DownMonthListener() );
		_downDayButton.addActionListener( new DownDayListener() );
		
		// Listeners for the 3 date Text Fields
        _yearTextField.addFocusListener( new YearTextFieldFocusListener() );
        _monthTextField.addFocusListener( new MonthTextFieldFocusListener() );
        _dayTextField.addFocusListener( new DayTextFieldFocusListener() );
        _yearTextField.addActionListener( new YearTextFieldListener() );
		_monthTextField.addActionListener( new MonthTextFieldListener() );
		_dayTextField.addActionListener( new DayTextFieldListener() );
	}
	
    private void updateDayTextField()
    {
        int inputDay = 0; 
        int maxDay = getDayMax();
        
        validateLeapYear();
        
        try
        {
            inputDay = Integer.parseInt( _dayTextField.getText() );
        }
        catch ( NumberFormatException exception )
        {
        }
        
        if ( ( inputDay <= maxDay ) && ( inputDay > 0 ) )
        {
            _day = inputDay;
        }
        updateDate();
    }
    
    private void updateYearTextField()
    {
        try
        {
            _year = Integer.parseInt( _yearTextField.getText() );
        }
        catch ( NumberFormatException exception )
        {
        }
        validateLeapYear();  // Sets _leapYear to true if it's a leapyear           
        validateDayValue();  
        updateDate();
    }
    
    private void updateMonthTextField()
    {
        int inputMonth = 0; 
        
        try
        {
            inputMonth = Integer.parseInt( _monthTextField.getText() );
        }
        catch ( NumberFormatException exception )
        {
        }

        if ( ( inputMonth >= 1 ) && ( inputMonth <= 12 ) )
        {
            _month = inputMonth;
            validateLeapYear();
            validateDayValue();
        }
        updateDate();
    }

    private class DayTextFieldFocusListener implements FocusListener
    {
        public void focusGained(FocusEvent arg0) { } 
        public void focusLost(FocusEvent arg0) 
        {
            updateDayTextField();
        }
    }
    
	private class YearTextFieldFocusListener implements FocusListener
    {
        public void focusGained(FocusEvent arg0) { }

        public void focusLost(FocusEvent arg0) 
        {
            updateYearTextField();
        }
    }
    
    private class MonthTextFieldFocusListener implements FocusListener
    {
        public void focusGained(FocusEvent arg0) { }

        public void focusLost(FocusEvent arg0) 
        {
            updateMonthTextField();
        }
    
    }
	private class YearTextFieldListener implements ActionListener
	/********************
	Purpose: Year text field listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
            updateYearTextField();
		}
	}
	
	private class MonthTextFieldListener implements ActionListener
	/********************
	Purpose: Month text field listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
            updateMonthTextField();
		}
	}
	
	private class DayTextFieldListener implements ActionListener
	/********************
	Purpose: Day text field listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
            updateDayTextField();
		}
	}
	



	private class UpYearListener implements ActionListener
	/********************
	Purpose: Up year button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			_year++;

			validateLeapYear();  // Sets _leapYear to true if it's a leapyear			
			validateDayValue();  
			updateDate();
		}
	}
	
	private class UpMonthListener implements ActionListener
	/********************
	Purpose: Up month button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			validateLeapYear();

			if ( _month < 12 )
			{
				_month++;
			}
			else
			{
				_month = 1;
			}
			validateDayValue();
			updateDate();
		}
	}
	
	private class UpDayListener implements ActionListener
	/********************
	Purpose: Up day button listener 
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			validateLeapYear();
			
			if ( _day < getDayMax() )
			{
				_day++;
			}
			else
			{
				_day = 1;
			}
			updateDate();
		}
	}
	
	private class DownYearListener implements ActionListener
	/********************
	Purpose: Down year button listener 
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			// Need to make sure the year is still A.D.
			if ( _year >= 1 )
			{
				_year--;
			}
			validateLeapYear();
			validateDayValue();
			updateDate();
		}
	}
	
	private class DownMonthListener implements ActionListener
	/********************
	Purpose: Down month button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			validateLeapYear();

			if ( _month > 1 )    // Checks to see if the month is greater than 1
			{
				_month--;
			}
			else
			{
				_month = 12;
			}
			validateDayValue();
			updateDate();
		}
	}
	
	private class DownDayListener implements ActionListener
	/********************
	Purpose: Down day button listener
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			validateLeapYear();
			
			if ( _day > 1 )
			{
				_day--;
			}
			else
			{
				_day = getDayMax();
			}
			updateDate();
		}
	}
	
	private int getDayMax()
	/********************
	Purpose: returns the maximum # of days for the particular month/year
	*********************/
	{
		String dayMaxString = null;
		int dayMax = 0;

		dayMaxString = "" + _dayMaxMap.get( new Integer( _month ) );
		dayMax = Integer.parseInt( dayMaxString );

		if ( _month == 2 )
		{
			if ( _leapYear )
			{
				dayMax++;   // Adds 1 to the max day which is 28
			}
		}			
		return dayMax;
	}
	
	private void validateDayValue()
	/********************
	Purpose: Sets a valid day value when the month is changed 
	*********************/
	{
		int dayMax = getDayMax();
		
		if ( _day > dayMax )
		{
			_day = dayMax;
		}
	}
	
	private void validateLeapYear()
	/********************
	Purpose: Determines if _year is a leap year
	*********************/
	{
		if ( _year % 4 == 0 ) // Checks if the year is divisible by 4
		{
			_leapYear = true;
			if ( _year % 400 == 0 ) // Checks if the year is divisible by 400
			{
				_leapYear = true;
			}
			if ( ( _year % 100 == 0 ) && ( _year % 400 != 0 ) ) // Checks if the year is divisible by 100 
			{											        // and not by 400.  If true, then it is NOT a leap year
				_leapYear = false;
			}
		}
		else
		{
			_leapYear = false;
		}
	}
	
	private void setupMonthNamesMap()
	/********************
	Purpose: Initialize _monthNamesMap
	*********************/
	{
		String months[] = { "January", "February", "March", "April", "May", "June", "July", "August",
						    "September", "October", "November", "December" };

		for ( int i = 1; i <= 12; i++ )
		{
			_monthNamesMap.put( new Integer( i ), months[ i - 1 ] );
		}
	}

	private void setupDayMaxMap()
	/********************
	Purpose: Initialize _dayMaxMap
	*********************/
	{
		int days[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

		for ( int i = 1; i <= 12; i++ )
		{
			_dayMaxMap.put( new Integer( i ), new Integer( days[ i - 1 ] ) );
		}
	}

	protected long getDateLong() 
	/********************
	Purpose: Return a long value for the date
	*********************/
	{
		long dateTimeLong = 0;
		String dateString = null;
		String dateTimeString = null;

		dateString = _year + "-" + _month + "-" + _day;

		dateTimeString = dateString;

		dateTimeLong = DbTimeHelper.getLongTimeFromDateString( dateTimeString );

		return dateTimeLong;
	}
	
	public long getDateTimeLong()
	/********************
	Purpose: Returns a long value for the date/time by calling getDateTimeLong from TimePanel
	*********************/
	{
		return _dateTimeLong;		
	}
	
	private long getInternalDateTimeLong()
	{
		return _dateTimeLong;
	}
	

	public void linkDateTime( TimePanel timePanel )
	/********************
	Purpose: Links the Date object with the Time object
	*********************/
	{
		_timePanel = timePanel;
		if ( _timePanel.getDatePanel() != this )
		{
			_timePanel.setTime( _dateTimeLong );
			_timePanel.linkDateTime( this );
		}
	}

	public TimePanel getTimePanel()
	/********************
	Purpose: returns the Time object
	*********************/
	{
		return _timePanel;
	}

	public void setDate( long dateTimeLong, boolean recurse )
	{
		int month, day, year;

		_dateTimeLong = dateTimeLong;
		
		setupMonthNamesMap();

		String date = DbTimeHelper.getDateStringFromLongTime( dateTimeLong );
		
		year = Integer.parseInt( date.substring( 0, 4 ) );
		month = Integer.parseInt( date.substring( 5, 7 ) );
		day = Integer.parseInt( date.substring( 8, 10 ) );
		
		initDate( month, day, year );
		
		if ( ( _timePanel != null ) && ( recurse ) )
		{
			_timePanel.setTime( dateTimeLong );
		}
	}
	
	public void setDate( long dateTimeLong )
	{
		setDate( dateTimeLong, true );
	}
	
	public static void main( String[] args ) 
	{
		Date date = new Date();
		DatePanel datetimeGUI = new DatePanel( date.getTime() );
	}
}
