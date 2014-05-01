package ohd.hseb.util;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;


public class TimerHelper
{

    private ActionListener _applicationListener = null;
    
    private Timer _refreshDisplayTimer = null;
    private int _delayInMillis = 60 * 1 * 1000 ; 
    //  --------------------------------------------------------------------------------------------------
    public TimerHelper(ActionListener applicationListener, int delayInMillis)
    {
        _applicationListener = applicationListener;
        _delayInMillis = delayInMillis;
        
        startTimer(_delayInMillis);
    }
    
    private class RefreshTimerListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            refresh();
        }
    }
    
    //  --------------------------------------------------------------------------------------------------
    
    private void refresh()
    {     
        String header = "TimerHelper.refresh(): ";
        
        System.out.println(header + "Refreshing");
        
        _applicationListener.actionPerformed(null);
        
        startTimer( _delayInMillis);
    }
    
    //  --------------------------------------------------------------------------------------------------
    
    protected void startTimer(int delayMilliSecs)
    {
        if(_refreshDisplayTimer == null)
        {
            RefreshTimerListener refreshDisplayTimerListener = new RefreshTimerListener();
            _refreshDisplayTimer = new Timer(delayMilliSecs, refreshDisplayTimerListener);
        }
        else
        {
            _refreshDisplayTimer.stop();
            //This timer doesn't repeat, hence it always uses setInitialDelay and not setDelay
            //which is used only if the timer is set to repeat
            _refreshDisplayTimer.setInitialDelay(delayMilliSecs); 
        }
        _refreshDisplayTimer.start();
    }
   
}
