package ohd.hseb.raxbase.util;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


public final class CursorController
{
    public final static Cursor busyCursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
    public final static Cursor defaultCursor = Cursor.getDefaultCursor();
    
    private CursorController(){}

    public static ActionListener createListener( final Component component, final ActionListener mainActionListener) 
    {
        ActionListener actionListener = new ActionListener() 
        {
            public void actionPerformed(ActionEvent ae) 
            {
                try 
                {
                    component.setCursor(busyCursor);
                    mainActionListener.actionPerformed(ae);
                } 
                finally 
                {
                    component.setCursor(defaultCursor);
                }
            }
        };
        return actionListener;
    }
}