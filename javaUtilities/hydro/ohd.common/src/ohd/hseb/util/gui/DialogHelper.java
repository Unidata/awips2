/*
 * Created on Mar 16, 2004
 *
 * 
 */
package ohd.hseb.util.gui;



import java.awt.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

/**
 * @author GobsC
 *
 * This is a simple class with static methods to make it
 * easy to display error and message dialogs.
 */
public class DialogHelper
{
    
   
    // ----------------------------------------------------------------------------------------- 
 
    
    public static boolean displayConfirmDialog(Window parent, String message, String dialogTitle)
    {
        int choice = 0;
        boolean confirm = false;
        
        choice = JOptionPane.showConfirmDialog( parent, message, dialogTitle,
                                               JOptionPane.YES_NO_OPTION, 
                                               JOptionPane.WARNING_MESSAGE );    
        
        if ( choice == JOptionPane.YES_OPTION )
        {
            confirm = true;    
        }
        
        return confirm;
    }
    
    // ----------------------------------------------------------------------------------------- 
    
    public static void displayErrorDialog(Window parent, String message, String dialogTitle)
    {
                    
        int dialogType = JOptionPane.ERROR_MESSAGE;
         
        JOptionPane.showMessageDialog(parent, message, dialogTitle, dialogType);    
    }
 
    // ----------------------------------------------------------------------------------------- 
 
    public static void displayMessageDialog(Window parent, String message, String dialogTitle)
    {
                 
        int dialogType = JOptionPane.INFORMATION_MESSAGE;
         
        JOptionPane.showMessageDialog(parent, message, dialogTitle, dialogType);    
    }
 
    // ----------------------------------------------------------------------------------------- 
  
    public static void displayMessageDialog(Window parent, String message)
    {
        
        JOptionPane.showMessageDialog(parent, message);     
    }
 
    // ----------------------------------------------------------------------------------------- 
 
    public static void setWaitCursor(JFrame frame)
    {
        Container c = frame.getContentPane();  // get the window's content pane
        setWaitCursor(c);
        //c.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    
    }
    
    public static void setDefaultCursor(JFrame frame)
    {
        Container c = frame.getContentPane();  // get the window's content pane
        setDefaultCursor(c);
       // c.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    
    }
    
    public static void setWaitCursor(Container container)
    {
        container.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    
    }
    
    public static void setDefaultCursor(Container container)
    { 
        container.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
    
    
} //end class DialogHelper
