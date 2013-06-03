/*
 * Created on Aug 10, 2005
 *
 * 
 */
package ohd.hseb.util.gui;

import javax.swing.*;


/**
 * @author Chip Gobs
 *
 * 
 */
public class OptionSelectionDialog
{
  
   private String _itemString = null;
   
   //---------------------------------------------------
   
   public OptionSelectionDialog()
   {
   	
   }
   //---------------------------------------------------
   
   private void setItemString(String itemString)
   {
       this._itemString = itemString;
   }
   
   public String getItemString()
   {
       return _itemString;
   }

  // ---------------------------------------------------
   
    public boolean requestItemByDialog(String dialogTitleString,
                                       String textMessage,
                                       String comboBoxLabelString,
                                       String[] optionArray)
    {
   	    String password = null;
   	    JComboBox comboBox = new JComboBox();
   	    
   	    
   	    for (int i = 0; i < optionArray.length; i++)
   	    {
   	        comboBox.addItem(optionArray[i]);
   	    }
       
        Object[] messageObjectArray = 
                 { textMessage, 
                   comboBoxLabelString, comboBox,
        	     };
        
        //_userName = JOptionPane.showInputDialog(null, messageObjectArray);
        
   	
		JOptionPane optionPane = new JOptionPane();

	    optionPane.setMessage( messageObjectArray );

		optionPane.setMessageType(	JOptionPane.QUESTION_MESSAGE );
					
		JDialog dialog = optionPane.createDialog( null, dialogTitleString);
		
		dialog.show();
		
		dialog.dispose();
						
		
		_itemString =  (String) comboBox.getSelectedItem();
	 	
   		
   	    return true;
   }
   
   
   
   //---------------------------------------------------
 
   public static void main(String[] args)
   {
   	
       OptionSelectionDialog dialog = new OptionSelectionDialog();
       
       String title = "Rainfall-Runoff Model Selection";
       String message = "Please enter the rainfall-runoff model\n" +
                        " for which this unit hydrograph is appropriate.\n" +
                        "                                           " ;
       String comboBoxLabelString = "Rainfall-Runoff Model:";
       String[] optionArray = { "ANY ",
               				    "API-MKC",
               				    "SAC-SMA" };
       
       dialog.requestItemByDialog(title, message, comboBoxLabelString, optionArray );
       
	   System.out.println("model = " + dialog.getItemString());      	
     
   }
   //---------------------------------------------------
}
