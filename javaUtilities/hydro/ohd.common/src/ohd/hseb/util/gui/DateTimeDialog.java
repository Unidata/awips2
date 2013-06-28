/*
 * Created on Oct 29, 2003
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Oct 29, 2003
 *  
 */
package ohd.hseb.util.gui;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;
import java.awt.event.*;

public class DateTimeDialog extends JDialog
{
	private Container _frameContentPane = 	getContentPane();
	private JButton _setTimeButton = new JButton( "Apply" );
	private JButton _cancelButton = new JButton( "Cancel" );
	private TimePanel _timePanel = null;
	private DatePanel _datePanel = null;
	private List _dateTimeListenerList = new ArrayList();
	private JPanel _buttonPanel = new JPanel();
    
    private boolean _allowOnlyEvenHours = false;
    
    private boolean _dateOnly = false;
    

	public DateTimeDialog( JDialog owner, String frameTitle )
	{
		this( owner, new Date().getTime(), frameTitle );
	}
	
	public DateTimeDialog( JDialog owner )
	{
		this( owner, new Date().getTime(), "" );
	}
    
    
	public DateTimeDialog( JDialog owner, long dateTimeLong, String frameTitle )
	{
		super( owner, true );
		this.setTitle( "Date/Time Display - " + frameTitle );
		_datePanel = new DatePanel();
		_timePanel = new TimePanel();
		setupGUI();
		_datePanel.setDate( dateTimeLong );
	}
	
	public DateTimeDialog( Frame owner, String frameTitle )
	{
		this( owner, new Date().getTime(), frameTitle );
	}
	
	public DateTimeDialog( Frame owner )
	{
		this( owner, new Date().getTime(), "" );
	}

	public DateTimeDialog( Frame owner, long dateTimeLong, String frameTitle )
	{
		super( owner, true );
		this.setTitle( "Date/Time Display - " + frameTitle );
		_datePanel = new DatePanel();
		_timePanel = new TimePanel();
		setupGUI();
		_datePanel.setDate( dateTimeLong );
	}
    
    public void setAllowOnlyEvenHours(boolean allowOnlyEvenHours)
    {
        _allowOnlyEvenHours = allowOnlyEvenHours;
       _timePanel.setAllowOnlyEvenHours(_allowOnlyEvenHours);
    }
    
    public void setDateOnly()
    {
    	_timePanel.setVisible( false );
    }
    
	private void setupGUI()
	{
		JPanel horizontalSeparatorPanel = 	new JPanel();

		_frameContentPane.setLayout( new GridBagLayout() );
		_buttonPanel.setLayout( new GridBagLayout() );
		
		_datePanel.linkDateTime( _timePanel );

//																						X,   Y,  #Col, #Row
		ComponentHelper.addFrameComponent( _frameContentPane, _datePanel, 				0, 	 0,     1,    1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, horizontalSeparatorPanel, 0,   1,     1,    1, GridBagConstraints.BOTH );
		ComponentHelper.addFrameComponent( _frameContentPane, _timePanel, 				0,   2,     1,    1, GridBagConstraints.BOTH );
		
		ComponentHelper.addPanelComponent( _buttonPanel, _setTimeButton, 				0,   0,     2,    2, GridBagConstraints.BOTH );
		ComponentHelper.addPanelComponent( _buttonPanel, _cancelButton, 				2, 	 0, 	2, 	  2, GridBagConstraints.BOTH );
		
		ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel,				0, 	 3,     4,    2, GridBagConstraints.BOTH );

		setSize( 270, 250 );

		addListener();
	}
	
	public void addDateTimeEventListener( DateTimeEventListener dateTimeEventListener )
	{
		_dateTimeListenerList.add( dateTimeEventListener );
	}
	
	private void notifyListeners( DateTimeEvent dateTimeEvent )
	{
		for ( int i = 0; i < _dateTimeListenerList.size(); i++ )
		{
			DateTimeEventListener listener = (DateTimeEventListener) _dateTimeListenerList.get( i );
			listener.dateTimeEntered( dateTimeEvent ); 
		}
	}
	
	
	private void addListener() 
	{
		_setTimeButton.addActionListener( new SetTimeListener() );
		_cancelButton.addActionListener( new CancelListener() );
	}
	
	private class SetTimeListener implements ActionListener
	/********************
	Purpose: 
	*********************/
	{
		public void actionPerformed( ActionEvent e )
		{
			long dateTime = _datePanel.getDateTimeLong();
			
			DateTimeEvent dateTimeEvent = new DateTimeEvent( dateTime, true );
			notifyListeners( dateTimeEvent );
			close();		
		}
	}

	private class CancelListener implements ActionListener
	{
		public void actionPerformed( ActionEvent e )
		{
			long dateTime = _datePanel.getDateTimeLong();
			
			DateTimeEvent dateTimeEvent = new DateTimeEvent( dateTime, false );			
			notifyListeners( dateTimeEvent );
			close();
		}
	}
	
	private void close()
	{
		this.dispose();
	}
	

	public static void main(String[] args) 
	{
		DateTimeDialog dateTimeGUI = new DateTimeDialog( new Frame(), new Date().getTime(), "Testing" );
		dateTimeGUI.show();
	}
}
