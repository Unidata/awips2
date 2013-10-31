/*
 * Created on Apr 27, 2004
 *
 * 
 */
package ohd.hseb.util.gui;

import javax.swing.*;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Date;
import ohd.hseb.db.DbTimeHelper;

/**
 * @author GobsC
 *
 * 
 */
public class DateTimeTextField extends JTextField
{   
    private JFrame _parent = null;
    private String _dialogTitle = null;
    private boolean _allowOnlyEvenHours = false;
    private boolean _limitTimeToMinutes = false; //defaults to seconds
    
    // ----------------------------------------------------------------------------------------
    public DateTimeTextField(long initialTime, JFrame parent, String dialogTitle, int columns) 
    {
       this(initialTime, parent, dialogTitle, columns, false);
    }
    
    public DateTimeTextField(long initialTime, JFrame parent, String dialogTitle, int columns, boolean limitTimeToMinutes) 
    {
        super(columns);
       // this.setColumns(columns);
       _parent = parent;
       _dialogTitle = dialogTitle;   
       
       java.awt.Color backgroundColor = getBackground();
       
       setEditable(false);
       
       setBackground(backgroundColor);
       
       setLimitTimeToMinutes(limitTimeToMinutes);
       
       setTime(initialTime);
       
       addListeners();
    }
    
    public void setLimitTimeToMinutes(boolean limitTimeToMinutes)
    {
        _limitTimeToMinutes = limitTimeToMinutes;   
    }
    //  ----------------------------------------------------------------------------------------
    
    public long getTime()
    {
    	long time = 0;
    	
    	if (_limitTimeToMinutes)
    	{
    		time = DbTimeHelper.getLongTimeFromDateTimeToMinutesString(this.getText() );
    	}
    	else
    	{
    		time = DbTimeHelper.getLongTimeFromDateTimeString( this.getText() );
    	}
        return time;
    }
    //  ----------------------------------------------------------------------------------------
     
    public void setTime(long time)
    {
    	String timeString = null;
    	
    	if (_limitTimeToMinutes) //date time string limited to minutes
    	{
    		timeString = DbTimeHelper.getDateTimeToMinutesStringFromLongTime(time );
    	}
    	else //normal date time string
    	{
    		timeString = DbTimeHelper.getDateTimeStringFromLongTime(time );
    	}
    	this.setText(timeString);
    }
    
    public void setAllowOnlyEvenHours(boolean allowOnlyEvenHours)
    {
        _allowOnlyEvenHours = allowOnlyEvenHours;
        
    }
   
    
    //  ----------------------------------------------------------------------------------------
    private void addListeners()
    {
        this.addMouseListener(new TimeTextFieldListener());
        
    }
    
    // ----------------------------------------------------------------------------------------

    private class TimeTextFieldListener extends MouseAdapter
    {
        public void mousePressed( MouseEvent e )
        {
            long dateTime = 0;
            JTextField textField = (JTextField) e.getSource();
    
            String text = textField.getText();
            
            if ( ! text.equals( "yyyy-mm-dd hh:mm:ss" ) )
            {
                if ( _limitTimeToMinutes)
                {
                    dateTime = DbTimeHelper.getLongTimeFromDateTimeToMinutesString(text);
                }
                else
                {
                    dateTime = DbTimeHelper.getLongTimeFromDateTimeString(text);
                }
            }
            else
            {
                dateTime = new Date().getTime();
            }
           
            DateTimeDialog dateTimeGUI = new DateTimeDialog( _parent, dateTime, _dialogTitle );
            dateTimeGUI.setAllowOnlyEvenHours(_allowOnlyEvenHours);
        
            dateTimeGUI.addDateTimeEventListener( new DateTimeListener( textField ) );
            dateTimeGUI.setVisible(true);
        }
    }

    // ----------------------------------------------------------------------------------------
   
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
                    String timeString = null;
                    
                    if (_limitTimeToMinutes)
                    {
                    	timeString = DbTimeHelper.getDateTimeToMinutesStringFromLongTime(dateTimeEvent.getDateTimeLong() );        
                    }
                    else //normal, full Year to seconds string
                    {
                    	timeString = DbTimeHelper.getDateTimeStringFromLongTime( dateTimeEvent.getDateTimeLong() );
                        
                    }
                    
                    _timeTextField.setText( timeString );
                }
            }
    }
    //  ----------------------------------------------------------------------------------------
    

}
