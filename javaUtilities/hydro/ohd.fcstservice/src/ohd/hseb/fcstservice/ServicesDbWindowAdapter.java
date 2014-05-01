package ohd.hseb.fcstservice;   

import java.awt.event.*;
import javax.swing.*;

public class ServicesDbWindowAdapter extends WindowAdapter 
{
	private JFrame jf = null;
	private DataServicesGlobals dataServicesGlobals = null;
	
	public ServicesDbWindowAdapter()
	{
		super();
	}
	
	public void windowClosing(WindowEvent e)
	{
		jf = (JFrame) e.getSource();
		operationCloseWindow();
	}
	
	private void operationCloseWindow()
	{	
		dataServicesGlobals = DataServicesGlobals.getSingleInstanceOfDataServicesGlobals();
		if(dataServicesGlobals.getCount() <= 1)
		{
			System.exit(0);
		}
		else
		{	
			dataServicesGlobals.decrementCount();
			jf.dispose();
		}
	}
}
