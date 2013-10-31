package ohd.hseb.util.gui;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;


public class FrameCloseWindowListener extends WindowAdapter
{

    
     
        private JFrame _frame = null;
        
        public FrameCloseWindowListener(JFrame frame)
        {
            _frame = frame;
        }
        
        public void windowClosing(WindowEvent evt)
        {
            _frame.dispose();
        }
        
        public static  void addFrameCloseWindowListener(JFrame frame)
        {
            FrameCloseWindowListener listener = new FrameCloseWindowListener(frame);
          
            frame.addWindowListener(listener);
        }

}
