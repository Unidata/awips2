/*
 * Created on Aug 29, 2003
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package ohd.hseb.dimensions_file_uploader;

import javax.swing.*;


/**
 * @author Chip Gobs
 *
 * This class lets the user request a password and obscures it
 * as it is being typed in.
 */
public class PasswordDialog
{
  
   private String _userName = null;
   private String _password = null;
   
   //---------------------------------------------------
   
   public PasswordDialog()
   {
   	
   }
   //---------------------------------------------------
  
    public void setUserName(String userName)
    {
	    _userName = userName;
    }

    public String getUserName()
    {
	    return _userName;
    }

    public void setPassword(String password)
    {
	    _password = password;
    }

    public String getPassword()
    {
	    return _password;
    }  
  
	//---------------------------------------------------
  
  
    public boolean requestPasswordByDialog(String textMessage)
    {
   	    String password = null;
   	    JTextField userNameField = new JTextField("");
        JPasswordField passwordField = new JPasswordField("");	
        
        Object[] messageObjectArray = 
                 { textMessage, 
				   "User Name:", userNameField,
        	       "Password:", passwordField
        	     };
        
        //_userName = JOptionPane.showInputDialog(null, messageObjectArray);
        
   	
		JOptionPane optionPane = new JOptionPane();

	    optionPane.setMessage( messageObjectArray );

		optionPane.setMessageType(	JOptionPane.QUESTION_MESSAGE );
					
		JDialog dialog = optionPane.createDialog( null, "Password" );
		
		dialog.show();
		
		dialog.dispose();
						
		
		_userName = new String(userNameField.getText());
	    _password = new String(passwordField.getPassword());
   	
   		
   		
   	    return true;
   }
   
   
   
   //---------------------------------------------------
   public static void main(String[] args)
   {
   	
       PasswordDialog dialog = new PasswordDialog();
       
       dialog.requestPasswordByDialog("Please enter your username and password, dude!");
       
	   System.out.println("username = " + dialog.getUserName());      	
       System.out.println("password = " + dialog.getPassword());      	
   	
   }


  // ---------------------------------------------------
   
}
