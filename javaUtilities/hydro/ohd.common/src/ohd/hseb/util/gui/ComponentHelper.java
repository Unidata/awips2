/*
 * Created on Oct 21, 2003
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Oct 21, 2003
 *  
 */
package ohd.hseb.util.gui;

import java.awt.*;
import javax.swing.*;

public class ComponentHelper 
{
	
	public static void addPanelComponent( JPanel panel, Component component, int column, int row, int width, int height, int fill )
	{
		addComponent( panel, component, column, row, width, height, 0, 0, fill );
	}

	public static void addPanelComponent( JPanel panel, Component component, int column, int row, int width, int height, double weightx, double weighty, int fill )
	{
		addComponent( panel, component, column, row, width, height, weightx, weighty, fill );
	}
	
	public static void addPanelComponent( JPanel panel, Component component, GridBagConstraints gbc, int column, int row, int width, int height, int fill )
	{
		addComponent( panel, component, gbc, column, row, width, height, fill );
	}

	public static void addFrameComponent( Container frameContentPane, Component component, int column, int row, int width, int height, int fill )
	{
		addComponent( frameContentPane, component, column, row, width, height, 0, 0, fill );
	}

	public static void addFrameComponent( Container frameContentPane, Component component, int column, int row, int width, int height, double weightx, double weighty, int fill )
	{
		addComponent( frameContentPane, component, column, row, width, height, weightx, weighty, fill );
	}

	private static void addComponent( Container container, Component component, int column, int row, int width, int height, double weightx, double weighty, int fill)
	{
		GridBagConstraints gbConstraints = new GridBagConstraints();
		gbConstraints.fill = fill;

		setConstraints( gbConstraints, column, row, width, height, weightx, weighty );
		
		GridBagLayout layoutMgr = (GridBagLayout) container.getLayout();
		layoutMgr.setConstraints( component, gbConstraints );
		
		container.add( component );
	}
	
	private static void addComponent( Container container, Component component, GridBagConstraints constraints, int column, int row, int width, int height, int fill)
	{
		GridBagConstraints gbConstraints = constraints;
		gbConstraints.fill = fill;

		double weightx = gbConstraints.weightx;
		double weighty = gbConstraints.weighty;
		setConstraints( gbConstraints, column, row, width, height, weightx, weighty);
		
		GridBagLayout layoutMgr = (GridBagLayout) container.getLayout();
		layoutMgr.setConstraints( component, gbConstraints );
		
		container.add( component );
	}

	private static void setConstraints( GridBagConstraints gbConstraints, int column, int row, int width, int height, double weightx, double weighty )
	{
		gbConstraints.gridx = column;
		gbConstraints.gridy = row;
		gbConstraints.gridwidth = width;
		gbConstraints.gridheight = height;
		gbConstraints.weightx = weightx;
		gbConstraints.weighty = weighty;
	}
}
