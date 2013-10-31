/*
 * Created on Nov 12, 2003
 *
 * 
 */
package ohd.hseb.sshp.gui;



import javax.swing.*;

/**
 * @author GobsC
 *
 * This group of controls is used for an idiom in which
 * a text field shows the value and allows the user to change it,
 * a label tells the user what the value represents, 
 * and a buttom allows the user to reset the value 
 * to one retrieved from the database value.
 */
public class SettingPanel
{
    
    private JLabel _label = null;
    private JTextField _textField = null;
    private JButton _button = null;
    
    
    public SettingPanel(String labelString, int textFieldLength,  String buttonString, String toolTipString)
    {
        initGui(labelString, textFieldLength, buttonString, toolTipString);
        
    } //end SettingPanel()

   // -----------------------------------------------------------------------------
    private void initGui(String labelString, int textFieldLength,  String buttonString, String toolTipString)
    {
        
   
    
        _label = new JLabel(labelString);
        _textField = new JTextField(textFieldLength);
        //_textField.setColumns(textFieldLength);
            
        _button = GuiHelper.getJButton(buttonString); 
        _button.setToolTipText(toolTipString);
   
        return;
 
    } //end initGui();
 
   // ---------------------------------------------------------
    
    
   // ---------------------------------------------------------
    
             
    public JLabel getLabel()
    {
        return _label;
    }

    // ---------------------------------------------------------

    public JTextField getTextField()
    {
        return _textField;
    }

    // ---------------------------------------------------------

    public JButton getButton()
    {
        return _button;
    }    
    // ---------------------------------------------------------
 
} //end class SettingPanel
