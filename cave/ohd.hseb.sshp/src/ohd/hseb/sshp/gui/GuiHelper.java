/*
 * Created on Jul 23, 2004
 *
 *
 */
package ohd.hseb.sshp.gui;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;

/**
 * @author GobsC
 *
 * 
 */
public class GuiHelper
{
    private static final Color _comboBoxColor = new Color(194, 141,237);
    private static final Color _buttonColor = new Color(158,244,252);

    private GuiHelper()
    {
        //does nothing.  No one is allowed to call this.
    }

    public static JComboBox getJComboBox(String[] contentArray)
    {
        String header = "AnalysisWindow.getJButton(): ";
        
           
        JComboBox comboBox = new JComboBox(contentArray);
       
       // Color color = new Color(97, 166, 96);
        
       // comboBox.setBackground(_comboBoxColor);
    
        return comboBox;

    }
    //  ------------------------------------------------------------
    public static JComboBox getJComboBox()
    {
        String header = "AnalysisWindow.getJButton(): ";
        
           
        JComboBox comboBox = new JComboBox();
       
      //  Color color = new Color(97, 166, 96);
        
      //  comboBox.setBackground(_comboBoxColor);
    
        return comboBox;

    }
    //  ------------------------------------------------------------
 
    
    public static JButton getJButton(String text)
    {
        String header = "AnalysisWindow.getJButton(): ";
        
        int width = 150;
        int height = 35;
        
        JButton button = new JButton(text);
            
        //button.setFont(_buttonFont);
       
     //   Color color = new Color(158, 244, 252);
        
   //     button.setBackground(_buttonColor);
        
        button.setSize(new Dimension(width, height));
    
        return button;

    }

    // ------------------------------------------------------------

    public static JButton getJButton(ImageIcon imageIcon)
    {   
        JButton button = new JButton(imageIcon);
        
        //button.setBackground(Color.GRAY);
     
        return button;

    }
}
