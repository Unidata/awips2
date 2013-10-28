package ohd.hseb.mpe.window;

import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

import javax.swing.DefaultCellEditor;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

public class GageTableCellEditor extends DefaultCellEditor
{
    public GageTableCellEditor()
    {
        super (new JTextField());
        
        getComponent().addFocusListener(new gageTableFocusListener());
        
        
        // Set the number of mouse clicks necessary to start editing.
        setClickCountToStart(1);
        
       
        
    }
    
    public boolean stopCellEditing ()
    {
        String newCellValue = (String) getCellEditorValue();
        
        // The cell must contain a real number or 'm' or 'M' or ""
        if ( newCellValue.compareToIgnoreCase("m") != 0 &&
        		newCellValue.compareTo("") != 0)
        {
           try
           {
               new Float(newCellValue).floatValue();
           } 
           catch ( NumberFormatException e)
           {
              JOptionPane.showMessageDialog(null, "Invalid Value");
              return false;    
           }
        }
        
        return super.stopCellEditing();
    }
    
    
    private class gageTableFocusListener extends FocusAdapter
    {
       public void focusGained ( FocusEvent e)
       {
          JTextField textField = (JTextField) e.getComponent();
          
          int stringLength = textField.getText().length();
          
          if ( stringLength > 0 )
          {
              textField.setSelectionStart(0);
              textField.setSelectionEnd(stringLength);
          }
       }
    }
}
