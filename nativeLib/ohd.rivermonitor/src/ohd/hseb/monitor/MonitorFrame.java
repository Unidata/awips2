package ohd.hseb.monitor;

import java.awt.Container;
import java.awt.Cursor;
import javax.swing.JFrame;

public class MonitorFrame extends JFrame
{
    public MonitorFrame()
    {
        super();
    }
    
    public void setWaitCursor()
    {    
        Container container = this.getContentPane();  // get the window's content pane
        container.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    }

    public void setDefaultCursor()
    {
        Container container = this.getContentPane();  // get the window's content pane
        container.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
    
        
}
