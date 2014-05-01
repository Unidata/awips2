/*
 * Created on Nov 06, 2006
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
 * @author VarmaR
 *
 * 
 */
public class DateTextField extends JTextField
{   
    private JFrame _parent = null;
    private String _dialogTitle = null;
    private boolean _allowOnlyEvenHours = false;
    private String _missingRepresentation = null;
    
    // ----------------------------------------------------------------------------------------
    public DateTextField(long initialTime, JFrame parent, String dialogTitle, int columns, String missingRepresentation) 
    {
        super(columns);
       // this.setColumns(columns);
       _parent = parent;
       _dialogTitle = dialogTitle;

       setTime(initialTime);
    	   
       java.awt.Color backgroundColor = getBackground();
       
       setEditable(false);
       
       setBackground(backgroundColor);
       
       addListeners();
       
       _missingRepresentation = missingRepresentation;
    }
    //  ----------------------------------------------------------------------------------------
    
    public long getTime()
    {
        long time = DbTimeHelper.getLongTimeFromDateTimeString( this.getText() );
        return time;
    }
    //  ----------------------------------------------------------------------------------------
     
    public void setTime(long time)
    {
        if(time != 0)
        {
        	String timeString = DbTimeHelper.getDateStringFromLongTime(time );
        	this.setText(timeString);
        }
        else
        {
        	this.setText("yyyy-mm-dd");
        }
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
    
            if (_missingRepresentation != null &&  ! _missingRepresentation.equals("") && !textField.getText().equals( "yyyy-mm-dd" ) && !textField.getText().equals( _missingRepresentation ))
            {
                dateTime = DbTimeHelper.getLongTimeFromDateString( textField.getText() );
            }
            else
            {
                dateTime = new Date().getTime();
            }
           
            DateTimeDialog dateTimeGUI = new DateTimeDialog( _parent, dateTime, _dialogTitle );
            dateTimeGUI.setDateOnly();
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
                    String timeString = DbTimeHelper.getDateStringFromLongTime( dateTimeEvent.getDateTimeLong() );
                    _timeTextField.setText( timeString );
                }
            }
    }
    //  ----------------------------------------------------------------------------------------
    

}
