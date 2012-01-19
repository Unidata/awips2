package ohd.hseb.util.gui.jchecktree;

import javax.swing.*;

public class JCheckTreeManager 
{
	CheckTreeManager check; //NEW
	
	public JCheckTreeManager(JTree tree)
	{
		//ORG CheckTreeManager check = new CheckTreeManager(tree);
		check = new CheckTreeManager(tree); //NEW
	}
	
	
	//NEW
	public CheckTreeSelectionModel getSelectionModel()
	{
		return(check.getSelectionModel());
	}
	//NEW
}
