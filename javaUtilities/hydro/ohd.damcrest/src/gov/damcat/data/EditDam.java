package gov.damcat.data;

import java.lang.reflect.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import gov.dambreak.util.*;

/**
 * This class allows the user to view and edit all the data stored in DAMCAT for a specific dam.
 */
 
class EditDam extends JFrame implements ActionListener, FocusListener, DocumentListener, TableModelListener {

class EditDamCellRenderer extends DefaultTableCellRenderer {
	private Component component;
	Color background = new Color( 230, 230, 230 );
     
	public Component getTableCellRendererComponent( JTable table,
		Object value, boolean isSelected, boolean hasFocus, 
			int row, int col)	
	{
		component = super.getTableCellRendererComponent(table, value,isSelected, hasFocus, row, col);
	    component.setBackground(background);
		return component;
	}
}

class NewDamDownKeys extends JFrame implements ActionListener
{
	private JTextField downName;
	private JTextField distanceText;
	private JTextField distance;
	private JButton okButton;
    private JButton cancelButton;
   
    public void actionPerformed(ActionEvent evt)
    {
		boolean _tableChanged = false;

	
	    if (evt.getSource() == cancelButton)
		{
			setVisible(false);
		}
		if (evt.getSource() == okButton)
		{
			downName.grabFocus();
			DownstreamEntryInfo downEntry = new DownstreamEntryInfo();
			String damID = damInfo.getNidid();
			String downKey = downName.getText().trim();
			String distanceKey = distanceText.getText().trim();
			float distanceNumKey = 0.0f;
			
			if(downKey.equals(""))
			{
				downName.grabFocus();
				JOptionPane.showMessageDialog(this, "Downstream name is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}

			if(distanceKey.equals(""))
			{
				distanceText.grabFocus();
				JOptionPane.showMessageDialog(this, "Distance from dam must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}

			try
			{
				distanceNumKey = Float.parseFloat(distanceKey);
				
			} catch (NumberFormatException e)
			{
				distanceText.grabFocus();
				JOptionPane.showMessageDialog(this, "Distance from dam must be numeric","Error", JOptionPane.ERROR_MESSAGE);
				return;
			} 

			
			downEntry.down_name = downKey;
			downName.setText("");

			downEntry.distance_from_dam = new Float(distanceNumKey);
			distanceText.setText("");

			boolean bError = dbAccess.insertDownstream(damID, downEntry);
			
			if(bError == true)
			{	
				damInfo.insertDown(downEntry);
				
				refresh();
			
			}
			setVisible(false);	
		}		
    }
	public NewDamDownKeys()
	{
		setTitle("Insert Downstream Table Keys");
		setSize(350, 280);					// was 		setSize(350, 150);
		setLocation(400, 200);
		setResizable(false);
		GridBagLayout gbl = new GridBagLayout();
		GridBagConstraints constr = new GridBagConstraints();
		
		Container  c = getContentPane();
		c.setLayout(gbl);
		JLabel lb1 = new JLabel("Downstream Point Name: ");
        lb1.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 1;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb1, constr);
  
        c.add(lb1);
        
		downName = new JTextField(20);

		constr.gridx = 0;
        constr.gridy = 2;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(downName, constr);
        
		c.add(downName);
		downName.addActionListener(this);
		
		// additions start from here
		JLabel lb2 = new JLabel("Distance From Dam: ");
        lb2.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 3;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb2, constr);

        c.add(lb2);

        distanceText = new JTextField(20);
		constr.gridx = 0;
        constr.gridy = 4;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(distanceText, constr);
        
		c.add(distanceText);
		distanceText.addActionListener(this);

		
		// to here	

		okButton = new JButton("    OK    ");
        okButton.setForeground(Color.black);
        
        cancelButton = new JButton("Cancel");
        cancelButton.setForeground(Color.black);
        
        constr.gridx = 0;
        constr.gridy = 5;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
       // constr.insets = new Insets(20,0,0,0);
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(okButton, constr); 
        c.add(okButton);
        okButton.addActionListener(this);
       
        constr.gridx = 1;
        constr.gridy = 5;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
      //  constr.insets = new Insets(20,0,0,0);
        constr.anchor = GridBagConstraints.CENTER;
        gbl.setConstraints(cancelButton, constr);
        c.add(cancelButton);
        cancelButton.addActionListener(this);
        
//		show();
        this.setVisible( true );
		
	}
	
}

class NewDamPairKeys extends JFrame implements ActionListener
{
	private JTextField downName;
	private JTextField xsecType;
	private JTextField pairNum;
	private JButton okButton;
    private JButton cancelButton;
    
    public void actionPerformed(ActionEvent evt)
    {
		if (evt.getSource() == cancelButton)
		{
			setVisible(false);
		}
		if (evt.getSource() == okButton)
		{
			
			downName.grabFocus();
			CrossSectionEntryInfo pairEntry = new CrossSectionEntryInfo();
			String damID = damInfo.getNidid();
			String downNameKey = downName.getText().trim();
			if(downNameKey.equals(""))
			{
				JOptionPane.showMessageDialog(this, "Downstream name is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			pairEntry.down_name = downNameKey;
			String xsecTypeKey = xsecType.getText().trim().toUpperCase();
			if(xsecTypeKey.equals(""))
			{
				xsecType.grabFocus();
				JOptionPane.showMessageDialog(this, "Cross-section type is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			pairEntry.xsec_type = xsecTypeKey;
			try
			{
				int pairNumKey = Integer.parseInt(pairNum.getText().trim());
				pairEntry.pair_num = new Integer(pairNumKey);
			} catch (NumberFormatException e)

			{
				pairNum.grabFocus();
				JOptionPane.showMessageDialog(this, "Pair number is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;		
			}			
			downName.setText("");
			xsecType.setText("");
			pairNum.setText("");
			
			boolean compareFlag = damInfo.insertPair(damInfo,pairEntry);
			if(compareFlag == true)
			{	
				boolean bError = dbAccess.insertSectionPair(damID, pairEntry);
			
				if(bError == true)
				{	
					refresh();
				}
			}
			setVisible(false);		
		}
    }
    public NewDamPairKeys()
	{
    	setTitle("Insert Cross-Section Table Keys");
		setSize(350, 350);
		setLocation(400, 200);
		setResizable(false);
		GridBagLayout gbl = new GridBagLayout();
		GridBagConstraints constr = new GridBagConstraints();
		
		Container  c = getContentPane();
		c.setLayout(gbl);
		JLabel lb1 = new JLabel("Downstream Point Name: ");
        lb1.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 1;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb1, constr);

        c.add(lb1);
        
		downName = new JTextField(20);

		constr.gridx = 0;
        constr.gridy = 2;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(downName, constr);
        
		c.add(downName);
		downName.addActionListener(this);

		JLabel lb2 = new JLabel("Cross-Section Type: ");
        lb2.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 3;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb2, constr);

        c.add(lb2);

        xsecType = new JTextField(20);
		constr.gridx = 0;
        constr.gridy = 4;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(xsecType, constr);
        
		c.add(xsecType);
		xsecType.addActionListener(this);

		JLabel lb3 = new JLabel("Pair Number: ");
        lb3.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 5;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb3, constr);

        c.add(lb3);

        pairNum = new JTextField(20);
		constr.gridx = 0;
        constr.gridy = 6;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(pairNum, constr);
        
		c.add(pairNum);
		pairNum.addActionListener(this);
		

		okButton = new JButton("    OK    ");
        okButton.setForeground(Color.black);
        
        cancelButton = new JButton("Cancel");
        cancelButton.setForeground(Color.black);
        
        constr.gridx = 0;
        constr.gridy = 7;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
       // constr.insets = new Insets(20,0,0,0);
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(okButton, constr);
        c.add(okButton);
        okButton.addActionListener(this);
       
        constr.gridx = 1;
        constr.gridy = 7;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
      //  constr.insets = new Insets(20,0,0,0);
        constr.anchor = GridBagConstraints.CENTER;
        gbl.setConstraints(cancelButton, constr);
        c.add(cancelButton);
        cancelButton.addActionListener(this);
        
//		show();
        this.setVisible( true );
		
	}
}

class NewDamInKeys extends JFrame implements ActionListener
{
	private JTextField src;
	private JTextField scenario;
	private JButton okButton;
    private JButton cancelButton;

    public void actionPerformed(ActionEvent evt)
    {
		if (evt.getSource() == cancelButton)
		{
			setVisible(false);
		}
		if (evt.getSource() == okButton)
		{
			src.grabFocus();
			InputEntryInfo inEntry = new InputEntryInfo();
			String damID = damInfo.getNidid();
			String srcKey = src.getText().trim();
			if(srcKey.equals(""))
			{
				JOptionPane.showMessageDialog(this, "Source is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			inEntry.src = srcKey;
			String scenarioKey = scenario.getText().trim();
			if(scenarioKey.equals(""))
			{
				scenario.grabFocus();
				JOptionPane.showMessageDialog(this, "Scenario is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			inEntry.scenario = scenarioKey;
			src.setText("");
			scenario.setText("");

			boolean bError = dbAccess.insertSdbIn(damID,inEntry);
			
			if(bError == true)
			{	
				damInfo.insertInput(inEntry);
			
				refresh();
			
			}
			// System.out.println("nidid in insertDamcatIn " + damID);

			setVisible(false);		
		}
	}
    public NewDamInKeys()
	{
    	setTitle("Insert Input Table Keys");
		setSize(350, 280);
		setLocation(400, 200);
		setResizable(false);
		GridBagLayout gbl = new GridBagLayout();
		GridBagConstraints constr = new GridBagConstraints();
		
		Container  c = getContentPane();
		c.setLayout(gbl);
		JLabel lb1 = new JLabel("Source: ");
        lb1.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 1;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb1, constr);

        c.add(lb1);
        
		src = new JTextField(20);

		constr.gridx = 0;
        constr.gridy = 2;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(src, constr);
        
		c.add(src);
		src.addActionListener(this);

		JLabel lb2 = new JLabel("Scenario: ");
        lb2.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 3;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb2, constr);

        c.add(lb2);

        scenario = new JTextField(20);
		constr.gridx = 0;
        constr.gridy = 4;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(scenario, constr);
        
		c.add(scenario);
		scenario.addActionListener(this);

		okButton = new JButton("    OK    ");
        okButton.setForeground(Color.black);
        
        cancelButton = new JButton("Cancel");
        cancelButton.setForeground(Color.black);
        
        constr.gridx = 0;
        constr.gridy = 5;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        //constr.insets = new Insets(20,0,0,0);
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(okButton, constr);
        c.add(okButton);
        okButton.addActionListener(this);
       
        constr.gridx = 1;
        constr.gridy = 5;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,70,0,0);
        constr.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(cancelButton, constr);
        c.add(cancelButton);
        cancelButton.addActionListener(this);
        
//		show();
        this.setVisible( true );
		
	}
}

class NewDamOutKeys extends JFrame implements ActionListener
{
	private JTextField src;
	private JTextField scenario;
	private JTextField downName;
	private JButton okButton;
    private JButton cancelButton;

    public void actionPerformed(ActionEvent evt)
    {
		if (evt.getSource() == cancelButton)
		{
			setVisible(false);
		}
		if (evt.getSource() == okButton)
		{
			src.grabFocus();
			OutputEntryInfo outEntry = new OutputEntryInfo();
			String damID = damInfo.getNidid();
			String srcKey = src.getText().trim();
			if(srcKey.equals(""))
			{
				JOptionPane.showMessageDialog(this, "Source is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			outEntry.src = srcKey;
			String scenarioKey = scenario.getText().trim();
			if(scenarioKey.equals(""))
			{
				scenario.grabFocus();
				JOptionPane.showMessageDialog(this, "Scenario is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			outEntry.scenario = scenarioKey;
			String downNameKey = downName.getText().trim();
			if(downNameKey.equals(""))
			{
				downName.grabFocus();
				JOptionPane.showMessageDialog(this, "Downstream name is a part of the key, must be supplied","Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			outEntry.down_name = downNameKey;
			src.setText("");
			scenario.setText("");
			downName.setText("");

			boolean compareFlag = damInfo.insertOutput(damInfo,outEntry);
			if(compareFlag == true)
			{	
				boolean bError = dbAccess.insertSdbOut(damID, outEntry);
				
				if(bError == true)
				{	
					refresh();
				}
			}
			/*boolean bError = dbAccess.insertDamcatOut(damID,outEntry);
			
			if(bError == true)
			{	
				damInfo.insertOutput(damInfo, outEntry);
			
				initialize();
			}*/
			System.out.println("nidid in insertDamcatOut " + damID);

			setVisible(false);
		}		
	}
    public NewDamOutKeys()
	{
    	setTitle("Insert Output Table Keys");
		setSize(350, 350);
		setLocation(400, 200);
		setResizable(false);
		GridBagLayout gbl = new GridBagLayout();
		GridBagConstraints constr = new GridBagConstraints();
		
		Container  c = getContentPane();
		c.setLayout(gbl);
		JLabel lb1 = new JLabel("Source: ");
        lb1.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 1;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb1, constr);

        c.add(lb1);
        
		src = new JTextField(20);

		constr.gridx = 0;
        constr.gridy = 2;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(src, constr);

        c.add(src);
		src.addActionListener(this);

		JLabel lb2 = new JLabel("Scenario: ");
        lb2.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 3;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb2, constr);

        c.add(lb2);

        scenario = new JTextField(20);
		constr.gridx = 0;
        constr.gridy = 4;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(scenario, constr);
        
		c.add(scenario);
		scenario.addActionListener(this);

		JLabel lb3 = new JLabel("Downstream Point Name: ");
        lb3.setForeground(Color.black);

        constr.gridx = 0;
        constr.gridy = 5;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(lb3, constr);

        c.add(lb3);
        
		downName = new JTextField(20);

		constr.gridx = 0;
        constr.gridy = 6;
        constr.gridwidth = 3;
        constr.gridheight = 1;
        constr.weightx = 0;
        constr.weighty = 0;
        constr.insets = new Insets(20,0,0,0);
        gbl.setConstraints(downName, constr);
        
		c.add(downName);
		downName.addActionListener(this);

		okButton = new JButton("    OK    ");
        okButton.setForeground(Color.black);
        
        cancelButton = new JButton("Cancel");
        cancelButton.setForeground(Color.black);
        
        constr.gridx = 0;
        constr.gridy = 7;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
       // constr.insets = new Insets(20,0,0,0);
        constr.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(okButton, constr);
        c.add(okButton);
        okButton.addActionListener(this);
       
        constr.gridx = 1;
        constr.gridy = 7;
        constr.gridwidth = 1;
        constr.gridheight = 1;
		constr.weightx = 0;
        constr.weighty = 0;
      //  constr.insets = new Insets(20,0,0,0);
        constr.anchor = GridBagConstraints.CENTER;
        gbl.setConstraints(cancelButton, constr);
        c.add(cancelButton);
        cancelButton.addActionListener(this);
        
//		show();
        this.setVisible( true );
		
	}
}
	private JPanel ivjJFrameContentPane = null;
	private DamInfo damInfo;
	private InputEntryInfo inInfo;
	private JPanel ivjPanelConstruction = null;
	private JPanel ivjPanelDownstream = null;
	private JPanel ivjPanelInputs = null;
	private JPanel ivjPanelOutput = null;
	private JPanel ivjPanelPairs = null;
	private JPanel ivjPanelRegulatory = null;
	private JLabel ivjJLabel1 = null;
	private JLabel ivjJLabel10 = null;
	private JLabel ivjJLabel11 = null;
	private JLabel ivjJLabel12 = null;
	private JLabel ivjJLabel13 = null;
	private JLabel ivjJLabel14 = null;
	private JLabel ivjJLabel15 = null;
	private JLabel ivjJLabel16 = null;
	private JLabel ivjJLabel17 = null;
	private JLabel ivjJLabel18 = null;
	private JLabel ivjJLabel2 = null;
	private JLabel ivjJLabel3 = null;
	private JLabel ivjJLabel4 = null;
	private JLabel ivjJLabel5 = null;
	private JLabel ivjJLabel6 = null;
	private JLabel ivjJLabel7 = null;
	private JLabel ivjJLabel8 = null;
	private JLabel ivjJLabel9 = null;
	private JPanel ivjPanelGeneral = null;
	private JTextField ivjTextComments = null;
	private JTextField ivjTextCounty = null;
	private JTextField ivjTextDamName = null;
	private JTextField ivjTextDrainageArea = null;
	private JTextField ivjTextElevation = null;
	private JTextField ivjTextFormerDamName = null;
	private JTextField ivjTextHSA = null;
	private JTextField ivjTextLatitude = null;
	private JTextField ivjTextLongitude = null;
	private JTextField ivjTextNIDID = null;
	private JTextField ivjTextOtherDamName = null;
	private JTextField ivjTextPrebreakAvailable = null;
	private JTextField ivjTextReturnFlowRegion = null;
	private JTextField ivjTextRFC = null;
	private JTextField ivjTextRiver = null;
	private JTextField ivjTextStateID = null;
	private JTextField ivjTextTopoMap = null;
	private JTextField ivjTextUpdated = null;
	private JLabel ivjJLabel19 = null;
	private JLabel ivjJLabel191 = null;
	private JLabel ivjJLabel20 = null;
	private JLabel ivjJLabel201 = null;
	private JLabel ivjJLabel21 = null;
	private JLabel ivjJLabel211 = null;
	private JLabel ivjJLabel22 = null;
	private JLabel ivjJLabel221 = null;
	private JLabel ivjJLabel23 = null;
	private JLabel ivjJLabel231 = null;
	private JLabel ivjJLabel24 = null;
	private JLabel ivjJLabel241 = null;
	private JLabel ivjJLabel25 = null;
	private JLabel ivjJLabel251 = null;
	private JLabel ivjJLabel26 = null;
	private JLabel ivjJLabel261 = null;
	private JLabel ivjJLabel27 = null;
	private JLabel ivjJLabel271 = null;
	private JLabel ivjJLabel272 = null;
	private JLabel ivjJLabel273 = null;
	private JLabel ivjJLabel28 = null;
	private JLabel ivjJLabel29 = null;
	private JLabel ivjJLabel30 = null;
	private JLabel ivjJLabel31 = null;
	private JLabel ivjJLabel32 = null;
	private JLabel ivjJLabel33 = null;
	private JLabel ivjJLabel34 = null;
	private JLabel ivjJLabel35 = null;
	private JLabel ivjJLabel36 = null;
	private JLabel ivjJLabel37 = null;
	private JLabel ivjJLabel38 = null;
	private JLabel ivjJLabel39 = null;
	private JLabel ivjJLabel40 = null;
	private JLabel ivjJLabel41 = null;
	private JLabel ivjJLabel42 = null;
	private JLabel ivjJLabel43 = null;
	private JLabel ivjJLabel44 = null;
	private JLabel ivjJLabel45 = null;
	private JLabel ivjJLabel46 = null;
	private JLabel ivjJLabel47 = null;
	private JLabel ivjJLabel48 = null;
	private JLabel ivjJLabel49 = null;
	private JLabel ivjJLabel50 = null;
	private JTextField ivjTextCore = null;
	private JTextField ivjTextDamDesigner = null;
	private JTextField ivjTextDamHeight = null;
	private JTextField ivjTextDamLength = null;
	private JTextField ivjTextDamType = null;
	private JTextField ivjTextDownstreamHazard = null;
	private JTextField ivjTextEmergencyAction = null;
	private JTextField ivjTextFederalConstruction = null;
	private JTextField ivjTextFederalDesign = null;
	private JTextField ivjTextFederalFunding = null;
	private JTextField ivjTextFederalInspection = null;
	private JTextField ivjTextFederalOperation = null;
	private JTextField ivjTextFederalOther = null;
	private JTextField ivjTextFederalOwner = null;
	private JTextField ivjTextFederalRegulatoryAgency = null;
	private JTextField ivjTextFoundation = null;
	private JTextField ivjTextHydraulicHeight = null;
	private JTextField ivjTextInspectionDate = null;
	private JTextField ivjTextInspectionFrequency = null;
	private JTextField ivjTextLengthOfLocks = null;
	private JTextField ivjTextMaxDischarge = null;
	private JTextField ivjTextMaxStorage = null;
	private JTextField ivjTextNIDHeight = null;
	private JTextField ivjTextNIDStorage = null;
	private JTextField ivjTextNormalStorage = null;
	private JTextField ivjTextNumberOfLocks = null;
	private JTextField ivjTextOutletGates = null;
	private JTextField ivjTextOwnerName = null;
	private JTextField ivjTextOwnerType = null;
	private JTextField ivjTextPrivateOnFederalLand = null;
	private JTextField ivjTextPurposes = null;
	private JTextField ivjTextSectionTR = null;
	private JTextField ivjTextSourceAgency = null;
	private JTextField ivjTextSpillwayType = null;
	private JTextField ivjTextSpillwayWidth = null;
	private JTextField ivjTextStateRegulated = null;
	private JTextField ivjTextStateRegulatoryAgency = null;
	private JTextField ivjTextStructuralHeight = null;
	private JTextField ivjTextSurfaceArea = null;
	private JTextField ivjTextVolumeMaterial = null;
	private JTextField ivjTextWidthOfLocks = null;
	private JTextField ivjTextYearCompleted = null;
	private JTextField ivjTextYearModified = null;
	private JScrollPane ivjScrollDownstream = null;
	private TableColumn ivjTableColumn1 = null;
	private TableColumn ivjTableColumn2 = null;
	private TableColumn ivjTableColumn3 = null;
	private TableColumn ivjTableColumn4 = null;
	private TableColumn ivjTableColumn5 = null;
	private TableColumn ivjTableColumn6 = null;
	private TableColumn ivjTableColumn7 = null;
	private TableColumn ivjTableColumn8 = null;
	private TableColumn ivjTableColumn9 = null;
	private TableColumn ivjTableColumn10 = null;
	private TableColumn ivjTableColumn11 = null;
	private TableColumn ivjTableColumn12 = null;
	private JTable ivjTableDownstream = null;
	private JScrollPane ivjScrollCrossSection = null;
	private TableColumn ivjTableColumn15 = null;
	private TableColumn ivjTableColumn16 = null;
	private TableColumn ivjTableColumn17 = null;
	private TableColumn ivjTableColumn18 = null;
	private TableColumn ivjTableColumn19 = null;
	private TableColumn ivjTableColumn20 = null;
	private TableColumn ivjTableColumn21 = null;
	private TableColumn ivjTableColumn22 = null;
	private JTable ivjTableCrossSection = null;
	private JScrollPane ivjJScrollPane1 = null;
	private TableColumn ivjTableColumn24 = null;
	private TableColumn ivjTableColumn25 = null;
	private TableColumn ivjTableColumn26 = null;
	private TableColumn ivjTableColumn28 = null;
	private TableColumn ivjTableColumn30 = null;
	private TableColumn ivjTableColumn32 = null;
	private TableColumn ivjTableColumn33 = null;
	private TableColumn ivjTableColumn34 = null;
	private TableColumn ivjTableColumn35 = null;
	private TableColumn ivjTableColumn36 = null;
	private TableColumn ivjTableColumn37 = null;
	private TableColumn ivjTableColumn38 = null;
	private HashMap dbFieldRelations;
	private UneditableJTableModel modelCrossSections,modelInput,modelOutput,modelDownstream;
	private JTable ivjTableInput = null;
	private JScrollPane ivjJScrollPane2 = null;
	private TableColumn ivjTableColumn27 = null;
	private TableColumn ivjTableColumn29 = null;
	private TableColumn ivjTableColumn31 = null;
	private TableColumn ivjTableColumn39 = null;
	private TableColumn ivjTableColumn40 = null;
	private TableColumn ivjTableColumn41 = null;
	private TableColumn ivjTableColumn42 = null;
	private TableColumn ivjTableColumn43 = null;
	private TableColumn ivjTableColumn44 = null;
	private TableColumn ivjTableColumn45 = null;
	private TableColumn ivjTableColumn46 = null;
	private JTable ivjTableOutput = null;
	private JButton ivjButtonExport = null;
	private JTabbedPane ivjTabData = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private JButton ivjButtonClose = null;
	private boolean bUpdateMaster,bUpdateFeatures,bUpdateDown,bUpdatePair,bUpdateIn,bUpdateOut;
	private JButton ivjButtonSave = null;
	private DBAccess dbAccess;
	private javax.swing.JComboBox scenarioCombo;
	private JPanel ivjPanelButtons = null;
	private boolean[] _tableRowChangedArray = null;
	private boolean[] _tableDownRowChangedArray = null;
	private boolean[] _tableInRowChangedArray = null;
	private boolean[] _tableOutRowChangedArray = null;
	private boolean[] _tablePairRowChangedArray = null;
	private JButton ivjButtonDeleteDam = null;
	private int _indexTab;
	private JFrame insertDownKeys = null;
	private JFrame insertPairKeys = null;
	private JFrame insertInKeys = null;
	private JFrame insertOutKeys = null;
	private JButton ivjButtonNewDam = null;
	private int _searchSelection = -1;
	int [] xsColFrom = {0, 7};
	int [] xsColTo = {2, 7};
	int [] inColFrom = {0, 11};
	int [] inColTo = {1, 11};
	int [] outColFrom = {0, 10};
	int [] outColTo = {2, 10};
	int [] downColFrom = {0, 11};
	int [] downColTo = {0, 11};
	private Search searchScreen = null;

class IvjEventHandler implements java.awt.event.ActionListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == EditDam.this.getButtonExport()) 
				connEtoC1();
			if (e.getSource() == EditDam.this.getButtonSave()) 
				connEtoC3();
			if (e.getSource() == EditDam.this.getButtonClose()) 
				connEtoC2();
			if (e.getSource() == EditDam.this.getButtonNewDam()) 
				connEtoC5();
			if (e.getSource() == EditDam.this.getButtonNewDam()) 
				connEtoC4();
			if (e.getSource() == EditDam.this.getButtonNewDam()) 
				connEtoC6();
			if (e.getSource() == EditDam.this.getButtonNewDam()) 
				connEtoC7();
			if (e.getSource() == EditDam.this.getButtonDeleteDam()) 
				connEtoC8();
		};
	};
/**
 * Constructor
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
public EditDam() {
	super();
	initialize();
}
public EditDam(DamInfo _damInfo, DBAccess _dbAccess) {
	super();

	damInfo = _damInfo;

	dbAccess = _dbAccess;
	
	initialize();
}
/**
 * Insert the method's description here.
 * Creation date: (2/18/2004 5:13:14 PM)
 * @param _damInfo gov.damcat.data.DamInfo
 * @param _dbAccess gov.damcat.data.DBAccess
 * @param _searchScreen gov.damcat.data.Search
 */
public EditDam(DamInfo _damInfo, DBAccess _dbAccess, Search _searchScreen) 
{
	super();

	damInfo = _damInfo;

	dbAccess = _dbAccess;

	searchScreen = _searchScreen;
	
	initialize();	
}
	/**
	 * Invoked when an action occurs.
	 */
public void actionPerformed(ActionEvent e) {}
/**
 * Insert the method's description here.
 * Creation date: (7/18/2003 8:36:21 AM)
 * @param e javax.swing.event.DocumentEvent
 */
public void changedUpdate(DocumentEvent e) {}
/**
 * Insert the method's description here.
 * Creation date: (2/17/2004 1:45:34 PM)
 * @param changedArray boolean[]
 */
public void clearRowChanged(boolean[] changedArray) 
{
	for (int i = 0; i < changedArray.length; i++)
    {
		changedArray[i] = false;
        
	}	
}
/**
 * connEtoC1:  (ButtonExport.action. --> DAMCAT_EditDam.handleExportToSMPDBK()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1() {
	try {
		// user code begin {1}
		// user code end
		
		this.handleExportToSMPDBK();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC2:  (ButtonClose.action. --> DAMCAT_EditDam.handleClose()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC2() {
	try {
		// user code begin {1}
		// user code end
		this.handleClose();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC3:  (ButtonSave.action. --> DAMCAT_EditDam.handleSave()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC3() {
	try {
		// user code begin {1}
		// user code end
		this.handleSave();
		// user code begin {2}	
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC4:  (ButtonNewDam.action. --> EditDam.handleNewDamInKeys()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC4() {
	try {
		// user code begin {1}
		// user code end
		this.handleNewDamInKeys();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC5:  (ButtonNewDam.action. --> EditDam.handleNewDamDownKeys()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC5() {
	try {
		// user code begin {1}
		// user code end
		this.handleNewDamDownKeys();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC6:  (ButtonNewDam.action. --> EditDam.handleNewDamOutKeys()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC6() {
	try {
		// user code begin {1}
		// user code end
		this.handleNewDamOutKeys();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC7:  (ButtonNewDam.action. --> EditDam.handleNewDamPairKeys()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC7() {
	try {
		// user code begin {1}
		// user code end
		this.handleNewDamPairKeys();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC8:  (ButtonDeleteDam.action. --> EditDam.handleDeleteDamcatDam()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC8() {
	try {
		// user code begin {1}
		// user code end
		this.handleDeleteDamcatDam();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (7/15/2003 4:55:25 PM)
 */
private void fillGUI() {
	Iterator it = dbFieldRelations.entrySet().iterator();
	int size;

	try {
		// Fill text fields
		while (it.hasNext()) {
			Map.Entry entry = (Map.Entry)it.next();
			JTextField guiField = (JTextField)(entry.getKey());
			
			if (((Field)entry.getValue()).get(damInfo) == null) continue;
			
			if (((Field)entry.getValue()).getType().equals(Float.class))
				guiField.setText(DamInfo.formatIfPossible((Float)(((Field)entry.getValue()).get(damInfo))));
			else
				guiField.setText(((Field)entry.getValue()).get(damInfo).toString());
		}

		// Fill cross section table
		modelCrossSections = new UneditableJTableModel(0,9,xsColFrom,xsColTo); 
		if (damInfo._crossSectionEntries != null) {

			 size = damInfo._crossSectionEntries.size();
        	_tablePairRowChangedArray = new boolean[size];
        	clearRowChanged(_tablePairRowChangedArray);
        	
			for (int i=0; i<damInfo._crossSectionEntries.size(); i++) {
				CrossSectionEntryInfo xs = (CrossSectionEntryInfo)damInfo._crossSectionEntries.get(i);
				modelCrossSections.addRow(new Object[] { xs.down_name,xs.xsec_type,xs.pair_num,DamInfo.formatIfPossible(xs.elev),DamInfo.formatIfPossible(xs.tw),DamInfo.formatIfPossible(xs.inactive_width),DamInfo.formatIfPossible(xs.mann_n),xs.updated});
			}
		}
		modelCrossSections.addTableModelListener(this);
		getTableCrossSection().setModel(modelCrossSections);

		// Fill input table
		modelInput = new UneditableJTableModel(0,15,inColFrom,inColTo);
		if (damInfo._inputEntries != null) {

			 size = damInfo._inputEntries.size();
        	_tableInRowChangedArray = new boolean[size];
        	clearRowChanged(_tableInRowChangedArray);
        	
			for (int i=0; i<damInfo._inputEntries.size(); i++) {
				InputEntryInfo in = (InputEntryInfo)damInfo._inputEntries.get(i);
				modelInput.addRow(new Object[] { in.src,in.scenario,DamInfo.formatIfPossible(in.hde),DamInfo.formatIfPossible(in.bme),DamInfo.formatIfPossible(in.vol),DamInfo.formatIfPossible(in.sa),DamInfo.formatIfPossible(in.tfm),DamInfo.formatIfPossible(in.qo),DamInfo.formatIfPossible(in.bw),in.idam.toString(),in.comments, in.updated }); //,java.text.DateFormat.getDateInstance().format(in.updated) });
			}
		}
		modelInput.addTableModelListener(this);
		getTableInput().setModel(modelInput);

		// Fill output table
		modelOutput = new UneditableJTableModel(0,12,outColFrom,outColTo);
		if (damInfo._outputEntries != null) {
			
			 size = damInfo._outputEntries.size();
        	_tableOutRowChangedArray = new boolean[size];
        	clearRowChanged(_tableOutRowChangedArray);
        	
			for (int i=0; i<damInfo._outputEntries.size(); i++) {
				OutputEntryInfo out = (OutputEntryInfo)damInfo._outputEntries.get(i);
				modelOutput.addRow(new Object[] { out.src,out.scenario,out.down_name,DamInfo.formatIfPossible(out.slope),DamInfo.formatIfPossible(out.max_flow),DamInfo.formatIfPossible(out.max_depth),DamInfo.formatIfPossible(out.time_max_depth),DamInfo.formatIfPossible(out.time_flood),DamInfo.formatIfPossible(out.time_deflood),out.comments,out.updated });
			}
		}
		modelOutput.addTableModelListener(this);
		getTableOutput().setModel(modelOutput);

		// Fill downstream table
		modelDownstream = new UneditableJTableModel(0,14,downColFrom,downColTo);
		if (damInfo._downstreamEntries != null) {
			
			 size = damInfo._downstreamEntries.size();
        	_tableDownRowChangedArray = new boolean[size];
        	clearRowChanged(_tableDownRowChangedArray);
      
			for (int i=0; i<damInfo._downstreamEntries.size(); i++) {
				DownstreamEntryInfo down = (DownstreamEntryInfo)damInfo._downstreamEntries.get(i);
				
				modelDownstream.addRow(new Object[] { down.down_name,down.xsec_best_type,DamInfo.formatIfPossible(down.distance_from_dam),DamInfo.formatIfPossible(down.latitude),DamInfo.formatIfPossible(down.longitude),DamInfo.formatIfPossible(down.elevation),DamInfo.formatIfPossible(down.flood_flow),DamInfo.formatIfPossible(down.flood_depth),DamInfo.formatIfPossible(down.flood_width),DamInfo.formatIfPossible(down.mann_oc),down.comments,down.updated });
			}
		}
		modelDownstream.addTableModelListener(this);
		getTableDownstream().setModel(modelDownstream);
		
		//getTableDownstream().addFocusListener(this);
		//TableModel tm = getTableDownstream().getModel();
		
	} catch (Throwable e) {
		JOptionPane.showMessageDialog(null,"Error: Could not fill GUI with data from the database.");
		e.printStackTrace();
	}
}
/**
 * Insert the method's description here.
 * Creation date: (2/2/2004 6:15:41 PM)
 * @param fe java.awt.event.FocusEvent
 */
public void focusGained(FocusEvent fe) 
{
 	getButtonSave().setEnabled(true);

 	JTable downTable = (JTable) fe.getSource();

 	TableModel tm = downTable.getModel();

 	int rows, cols;

 	rows = tm.getRowCount();
 	cols = tm.getColumnCount();


 	String sourceString = downTable.toString();

 	System.out.println("focus Gained ");
 	System.out.println("row count = " + rows + " column count = " + cols);

	 Object val = tm.getValueAt(0, 3);
 	String valString = val.toString();

 	System.out.println("Val String = " + valString);	
}
/**
 * Insert the method's description here.
 * Creation date: (2/2/2004 6:16:50 PM)
 * @param fe java.awt.event.FocusEvent
 */
public void focusLost(FocusEvent fe) 
{
	bUpdateMaster = true;
 	bUpdateDown = true;
 	getButtonSave().setEnabled(true);

 	JTable downTable = (JTable) fe.getSource();

 	TableModel tm = downTable.getModel();

 	int rows, cols;

 	rows = tm.getRowCount();
 	cols = tm.getColumnCount();


 	String sourceString = downTable.toString();

 	System.out.println("focus Lost ");
 	System.out.println("row count = " + rows + " column count = " + cols);

 	Object val = tm.getValueAt(0, 3);
 	String valString = val.toString();

 	System.out.println("Val String = " + valString);


}
/**
 * Insert the method's description here.
 * Creation date: (1/22/2004 5:20:45 PM)
 * @return int
 */
public int get_indexTab() {
	return _indexTab;
}
/**
 * Insert the method's description here.
 * Creation date: (2/18/2004 6:12:30 PM)
 * @return int
 */
public int get_searchSelection() {
	return _searchSelection;
}
/**
 * Return the ButtonDiscard property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getButtonClose() {
	if (ivjButtonClose == null) {
		try {
			ivjButtonClose = new javax.swing.JButton();
			ivjButtonClose.setName("ButtonClose");
			ivjButtonClose.setMnemonic('c');
			ivjButtonClose.setText("Close");
			ivjButtonClose.setMaximumSize(new java.awt.Dimension(47, 25));
			ivjButtonClose.setBorderPainted(true);
			ivjButtonClose.setPreferredSize(new java.awt.Dimension(80, 25));
			ivjButtonClose.setEnabled(true);
			ivjButtonClose.setMinimumSize(new java.awt.Dimension(47, 25));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjButtonClose;
}
/**
 * Return the ButtonInsert property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getButtonDeleteDam() {
	if (ivjButtonDeleteDam == null) {
		try {
			ivjButtonDeleteDam = new javax.swing.JButton();
			ivjButtonDeleteDam.setName("ButtonDeleteDam");
			ivjButtonDeleteDam.setText("Delete");
			ivjButtonDeleteDam.setActionCommand("Delete Dam");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjButtonDeleteDam;
}
/**
 * Return the ButtonExport property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getButtonExport() {
	if (ivjButtonExport == null) {
		try {
			ivjButtonExport = new javax.swing.JButton();
			ivjButtonExport.setName("ButtonExport");
			ivjButtonExport.setMnemonic('e');
			ivjButtonExport.setText("View / Edit Dam Failure Scenarios");
			ivjButtonExport.setEnabled(true);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjButtonExport;
}
/**
 * Return the ButtonNewDam property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getButtonNewDam() {
	if (ivjButtonNewDam == null) {
		try {
			ivjButtonNewDam = new javax.swing.JButton();
			ivjButtonNewDam.setName("ButtonNewDam");
			ivjButtonNewDam.setText("New Dam");
			ivjButtonNewDam.setEnabled(false);
			ivjButtonNewDam.setActionCommand("New Dam");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjButtonNewDam;
}
/**
 * Return the ButtonUpdate property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getButtonSave() {
	if (ivjButtonSave == null) {
		try {
			ivjButtonSave = new javax.swing.JButton();
			ivjButtonSave.setName("ButtonSave");
			ivjButtonSave.setMnemonic('s');
			ivjButtonSave.setText("Save Changes");
			ivjButtonSave.setBorderPainted(true);
			ivjButtonSave.setPreferredSize(new java.awt.Dimension(139, 25));
			ivjButtonSave.setEnabled(false);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjButtonSave;
}
/**
 * Return the JFrameContentPane property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJFrameContentPane() {
	if (ivjJFrameContentPane == null) {
		try {
			ivjJFrameContentPane = new javax.swing.JPanel();
			ivjJFrameContentPane.setName("JFrameContentPane");
			ivjJFrameContentPane.setLayout(new java.awt.BorderLayout());
			getJFrameContentPane().add(getPanelButtons(), "South");
			getJFrameContentPane().add(getTabData(), "North");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJFrameContentPane;
}
/**
 * Return the JLabel1 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel1() {
	if (ivjJLabel1 == null) {
		try {
			ivjJLabel1 = new javax.swing.JLabel();
			ivjJLabel1.setName("JLabel1");
			ivjJLabel1.setText("NID ID:");
			ivjJLabel1.setBounds(15, 10, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel1;
}
/**
 * Return the JLabel10 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel10() {
	if (ivjJLabel10 == null) {
		try {
			ivjJLabel10 = new javax.swing.JLabel();
			ivjJLabel10.setName("JLabel10");
			ivjJLabel10.setText("Elevation (ft MSL):");
			ivjJLabel10.setBounds(15, 217, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel10;
}
/**
 * Return the JLabel11 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel11() {
	if (ivjJLabel11 == null) {
		try {
			ivjJLabel11 = new javax.swing.JLabel();
			ivjJLabel11.setName("JLabel11");
			ivjJLabel11.setText("Topo Map:");
			ivjJLabel11.setBounds(15, 240, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel11;
}
/**
 * Return the JLabel12 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel12() {
	if (ivjJLabel12 == null) {
		try {
			ivjJLabel12 = new javax.swing.JLabel();
			ivjJLabel12.setName("JLabel12");
			ivjJLabel12.setText("Return Flow Region:");
			ivjJLabel12.setBounds(15, 263, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel12;
}
/**
 * Return the JLabel13 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel13() {
	if (ivjJLabel13 == null) {
		try {
			ivjJLabel13 = new javax.swing.JLabel();
			ivjJLabel13.setName("JLabel13");
			ivjJLabel13.setText("Drainage Area (mi2):");
			ivjJLabel13.setBounds(15, 286, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel13;
}
/**
 * Return the JLabel14 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel14() {
	if (ivjJLabel14 == null) {
		try {
			ivjJLabel14 = new javax.swing.JLabel();
			ivjJLabel14.setName("JLabel14");
			ivjJLabel14.setText("Prebreak Available:");
			ivjJLabel14.setBounds(15, 309, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel14;
}
/**
 * Return the JLabel15 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel15() {
	if (ivjJLabel15 == null) {
		try {
			ivjJLabel15 = new javax.swing.JLabel();
			ivjJLabel15.setName("JLabel15");
			ivjJLabel15.setText("Comments:");
			ivjJLabel15.setBounds(15, 332, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel15;
}
/**
 * Return the JLabel16 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel16() {
	if (ivjJLabel16 == null) {
		try {
			ivjJLabel16 = new javax.swing.JLabel();
			ivjJLabel16.setName("JLabel16");
			ivjJLabel16.setText("Updated:");
			ivjJLabel16.setBounds(15, 355, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel16;
}
/**
 * Return the JLabel17 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel17() {
	if (ivjJLabel17 == null) {
		try {
			ivjJLabel17 = new javax.swing.JLabel();
			ivjJLabel17.setName("JLabel17");
			ivjJLabel17.setText("HSA:");
			ivjJLabel17.setBounds(15, 378, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel17;
}
/**
 * Return the JLabel18 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel18() {
	if (ivjJLabel18 == null) {
		try {
			ivjJLabel18 = new javax.swing.JLabel();
			ivjJLabel18.setName("JLabel18");
			ivjJLabel18.setText("RFC:");
			ivjJLabel18.setBounds(15, 401, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel18;
}
/**
 * Return the JLabel19 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel19() {
	if (ivjJLabel19 == null) {
		try {
			ivjJLabel19 = new javax.swing.JLabel();
			ivjJLabel19.setName("JLabel19");
			ivjJLabel19.setText("Dam Type:");
			ivjJLabel19.setBounds(9, 21, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel19;
}
/**
 * Return the JLabel191 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel191() {
	if (ivjJLabel191 == null) {
		try {
			ivjJLabel191 = new javax.swing.JLabel();
			ivjJLabel191.setName("JLabel191");
			ivjJLabel191.setText("Volume Material (yd3):");
			ivjJLabel191.setBounds(318, 65, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel191;
}
/**
 * Return the JLabel2 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel2() {
	if (ivjJLabel2 == null) {
		try {
			ivjJLabel2 = new javax.swing.JLabel();
			ivjJLabel2.setName("JLabel2");
			ivjJLabel2.setText("Dam Name:");
			ivjJLabel2.setBounds(15, 33, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel2;
}
/**
 * Return the JLabel20 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel20() {
	if (ivjJLabel20 == null) {
		try {
			ivjJLabel20 = new javax.swing.JLabel();
			ivjJLabel20.setName("JLabel20");
			ivjJLabel20.setText("Dam Height (ft):");
			ivjJLabel20.setBounds(9, 62, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel20;
}
/**
 * Return the JLabel201 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel201() {
	if (ivjJLabel201 == null) {
		try {
			ivjJLabel201 = new javax.swing.JLabel();
			ivjJLabel201.setName("JLabel201");
			ivjJLabel201.setText("Core:");
			ivjJLabel201.setBounds(318, 106, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel201;
}
/**
 * Return the JLabel21 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel21() {
	if (ivjJLabel21 == null) {
		try {
			ivjJLabel21 = new javax.swing.JLabel();
			ivjJLabel21.setName("JLabel21");
			ivjJLabel21.setText("Structural Height (ft):");
			ivjJLabel21.setBounds(9, 103, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel21;
}
/**
 * Return the JLabel211 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel211() {
	if (ivjJLabel211 == null) {
		try {
			ivjJLabel211 = new javax.swing.JLabel();
			ivjJLabel211.setName("JLabel211");
			ivjJLabel211.setText("Foundation:");
			ivjJLabel211.setBounds(318, 147, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel211;
}
/**
 * Return the JLabel22 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel22() {
	if (ivjJLabel22 == null) {
		try {
			ivjJLabel22 = new javax.swing.JLabel();
			ivjJLabel22.setName("JLabel22");
			ivjJLabel22.setText("Hydraulic Height (ft):");
			ivjJLabel22.setBounds(9, 144, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel22;
}
/**
 * Return the JLabel221 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel221() {
	if (ivjJLabel221 == null) {
		try {
			ivjJLabel221 = new javax.swing.JLabel();
			ivjJLabel221.setName("JLabel221");
			ivjJLabel221.setText("Spillway Type:");
			ivjJLabel221.setBounds(318, 188, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel221;
}
/**
 * Return the JLabel23 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel23() {
	if (ivjJLabel23 == null) {
		try {
			ivjJLabel23 = new javax.swing.JLabel();
			ivjJLabel23.setName("JLabel23");
			ivjJLabel23.setText("NID Storage (acre-ft):");
			ivjJLabel23.setBounds(9, 226, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel23;
}
/**
 * Return the JLabel231 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel231() {
	if (ivjJLabel231 == null) {
		try {
			ivjJLabel231 = new javax.swing.JLabel();
			ivjJLabel231.setName("JLabel231");
			ivjJLabel231.setText("Spillway Width (ft):");
			ivjJLabel231.setBounds(318, 229, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel231;
}
/**
 * Return the JLabel24 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel24() {
	if (ivjJLabel24 == null) {
		try {
			ivjJLabel24 = new javax.swing.JLabel();
			ivjJLabel24.setName("JLabel24");
			ivjJLabel24.setText("Normal Storage (acre-ft):");
			ivjJLabel24.setBounds(9, 308, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel24;
}
/**
 * Return the JLabel241 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel241() {
	if (ivjJLabel241 == null) {
		try {
			ivjJLabel241 = new javax.swing.JLabel();
			ivjJLabel241.setName("JLabel241");
			ivjJLabel241.setText("Outlet Gates:");
			ivjJLabel241.setBounds(318, 270, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel241;
}
/**
 * Return the JLabel25 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel25() {
	if (ivjJLabel25 == null) {
		try {
			ivjJLabel25 = new javax.swing.JLabel();
			ivjJLabel25.setName("JLabel25");
			ivjJLabel25.setText("Surface Area (acre):");
			ivjJLabel25.setBounds(9, 390, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel25;
}
/**
 * Return the JLabel251 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel251() {
	if (ivjJLabel251 == null) {
		try {
			ivjJLabel251 = new javax.swing.JLabel();
			ivjJLabel251.setName("JLabel251");
			ivjJLabel251.setText("Number Of Locks:");
			ivjJLabel251.setBounds(318, 311, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel251;
}
/**
 * Return the JLabel26 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel26() {
	if (ivjJLabel26 == null) {
		try {
			ivjJLabel26 = new javax.swing.JLabel();
			ivjJLabel26.setName("JLabel26");
			ivjJLabel26.setText("Max Discharge (cfs):");
			ivjJLabel26.setBounds(318, 24, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel26;
}
/**
 * Return the JLabel261 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel261() {
	if (ivjJLabel261 == null) {
		try {
			ivjJLabel261 = new javax.swing.JLabel();
			ivjJLabel261.setName("JLabel261");
			ivjJLabel261.setText("Length of Locks (ft):");
			ivjJLabel261.setBounds(318, 352, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel261;
}
/**
 * Return the JLabel27 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel27() {
	if (ivjJLabel27 == null) {
		try {
			ivjJLabel27 = new javax.swing.JLabel();
			ivjJLabel27.setName("JLabel27");
			ivjJLabel27.setText("Width of Locks (ft):");
			ivjJLabel27.setBounds(318, 390, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel27;
}
/**
 * Return the JLabel271 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel271() {
	if (ivjJLabel271 == null) {
		try {
			ivjJLabel271 = new javax.swing.JLabel();
			ivjJLabel271.setName("JLabel271");
			ivjJLabel271.setText("Dam Length (ft):");
			ivjJLabel271.setBounds(9, 349, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel271;
}
/**
 * Return the JLabel272 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel272() {
	if (ivjJLabel272 == null) {
		try {
			ivjJLabel272 = new javax.swing.JLabel();
			ivjJLabel272.setName("JLabel272");
			ivjJLabel272.setText("NID Height (ft):");
			ivjJLabel272.setBounds(9, 185, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel272;
}
/**
 * Return the JLabel273 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel273() {
	if (ivjJLabel273 == null) {
		try {
			ivjJLabel273 = new javax.swing.JLabel();
			ivjJLabel273.setName("JLabel273");
			ivjJLabel273.setText("Max Storage (acre-ft):");
			ivjJLabel273.setBounds(9, 267, 153, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel273;
}
/**
 * Return the JLabel28 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel28() {
	if (ivjJLabel28 == null) {
		try {
			ivjJLabel28 = new javax.swing.JLabel();
			ivjJLabel28.setName("JLabel28");
			ivjJLabel28.setText("Owner Name:");
			ivjJLabel28.setBounds(12, 20, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel28;
}
/**
 * Return the JLabel29 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel29() {
	if (ivjJLabel29 == null) {
		try {
			ivjJLabel29 = new javax.swing.JLabel();
			ivjJLabel29.setName("JLabel29");
			ivjJLabel29.setText("Owner Type:");
			ivjJLabel29.setBounds(12, 54, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel29;
}
/**
 * Return the JLabel3 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel3() {
	if (ivjJLabel3 == null) {
		try {
			ivjJLabel3 = new javax.swing.JLabel();
			ivjJLabel3.setName("JLabel3");
			ivjJLabel3.setText("Other Dam Name:");
			ivjJLabel3.setBounds(15, 56, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel3;
}
/**
 * Return the JLabel30 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel30() {
	if (ivjJLabel30 == null) {
		try {
			ivjJLabel30 = new javax.swing.JLabel();
			ivjJLabel30.setName("JLabel30");
			ivjJLabel30.setText("Year Completed:");
			ivjJLabel30.setBounds(12, 88, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel30;
}
/**
 * Return the JLabel31 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel31() {
	if (ivjJLabel31 == null) {
		try {
			ivjJLabel31 = new javax.swing.JLabel();
			ivjJLabel31.setName("JLabel31");
			ivjJLabel31.setText("Year Modified:");
			ivjJLabel31.setBounds(12, 122, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel31;
}
/**
 * Return the JLabel32 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel32() {
	if (ivjJLabel32 == null) {
		try {
			ivjJLabel32 = new javax.swing.JLabel();
			ivjJLabel32.setName("JLabel32");
			ivjJLabel32.setText("Purposes:");
			ivjJLabel32.setBounds(12, 156, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel32;
}
/**
 * Return the JLabel33 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel33() {
	if (ivjJLabel33 == null) {
		try {
			ivjJLabel33 = new javax.swing.JLabel();
			ivjJLabel33.setName("JLabel33");
			ivjJLabel33.setText("Dam Designer:");
			ivjJLabel33.setBounds(12, 190, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel33;
}
/**
 * Return the JLabel34 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel34() {
	if (ivjJLabel34 == null) {
		try {
			ivjJLabel34 = new javax.swing.JLabel();
			ivjJLabel34.setName("JLabel34");
			ivjJLabel34.setText("Private on Federal Land:");
			ivjJLabel34.setBounds(12, 224, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel34;
}
/**
 * Return the JLabel35 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel35() {
	if (ivjJLabel35 == null) {
		try {
			ivjJLabel35 = new javax.swing.JLabel();
			ivjJLabel35.setName("JLabel35");
			ivjJLabel35.setText("Downstream Hazard:");
			ivjJLabel35.setBounds(12, 258, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel35;
}
/**
 * Return the JLabel36 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel36() {
	if (ivjJLabel36 == null) {
		try {
			ivjJLabel36 = new javax.swing.JLabel();
			ivjJLabel36.setName("JLabel36");
			ivjJLabel36.setText("Emergency Action:");
			ivjJLabel36.setBounds(12, 292, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel36;
}
/**
 * Return the JLabel37 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel37() {
	if (ivjJLabel37 == null) {
		try {
			ivjJLabel37 = new javax.swing.JLabel();
			ivjJLabel37.setName("JLabel37");
			ivjJLabel37.setText("Inspection Date:");
			ivjJLabel37.setBounds(12, 326, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel37;
}
/**
 * Return the JLabel38 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel38() {
	if (ivjJLabel38 == null) {
		try {
			ivjJLabel38 = new javax.swing.JLabel();
			ivjJLabel38.setName("JLabel38");
			ivjJLabel38.setText("Inspection Frequency:");
			ivjJLabel38.setBounds(12, 360, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel38;
}
/**
 * Return the JLabel39 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel39() {
	if (ivjJLabel39 == null) {
		try {
			ivjJLabel39 = new javax.swing.JLabel();
			ivjJLabel39.setName("JLabel39");
			ivjJLabel39.setText("State Regulated:");
			ivjJLabel39.setBounds(12, 394, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel39;
}
/**
 * Return the JLabel4 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel4() {
	if (ivjJLabel4 == null) {
		try {
			ivjJLabel4 = new javax.swing.JLabel();
			ivjJLabel4.setName("JLabel4");
			ivjJLabel4.setText("Former Dam Name:");
			ivjJLabel4.setBounds(15, 79, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel4;
}
/**
 * Return the JLabel40 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel40() {
	if (ivjJLabel40 == null) {
		try {
			ivjJLabel40 = new javax.swing.JLabel();
			ivjJLabel40.setName("JLabel40");
			ivjJLabel40.setText("State Regulatory Agency:");
			ivjJLabel40.setBounds(315, 20, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel40;
}
/**
 * Return the JLabel41 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel41() {
	if (ivjJLabel41 == null) {
		try {
			ivjJLabel41 = new javax.swing.JLabel();
			ivjJLabel41.setName("JLabel41");
			ivjJLabel41.setText("Federal Funding:");
			ivjJLabel41.setBounds(315, 54, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel41;
}
/**
 * Return the JLabel42 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel42() {
	if (ivjJLabel42 == null) {
		try {
			ivjJLabel42 = new javax.swing.JLabel();
			ivjJLabel42.setName("JLabel42");
			ivjJLabel42.setText("Federal Design:");
			ivjJLabel42.setBounds(315, 88, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel42;
}
/**
 * Return the JLabel43 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel43() {
	if (ivjJLabel43 == null) {
		try {
			ivjJLabel43 = new javax.swing.JLabel();
			ivjJLabel43.setName("JLabel43");
			ivjJLabel43.setText("Federal Construction:");
			ivjJLabel43.setBounds(315, 122, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel43;
}
/**
 * Return the JLabel44 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel44() {
	if (ivjJLabel44 == null) {
		try {
			ivjJLabel44 = new javax.swing.JLabel();
			ivjJLabel44.setName("JLabel44");
			ivjJLabel44.setText("Federal Regulatory Agency:");
			ivjJLabel44.setBounds(315, 156, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel44;
}
/**
 * Return the JLabel45 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel45() {
	if (ivjJLabel45 == null) {
		try {
			ivjJLabel45 = new javax.swing.JLabel();
			ivjJLabel45.setName("JLabel45");
			ivjJLabel45.setText("Federal Inspection:");
			ivjJLabel45.setBounds(315, 190, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel45;
}
/**
 * Return the JLabel46 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel46() {
	if (ivjJLabel46 == null) {
		try {
			ivjJLabel46 = new javax.swing.JLabel();
			ivjJLabel46.setName("JLabel46");
			ivjJLabel46.setText("Federal Operation:");
			ivjJLabel46.setBounds(315, 224, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel46;
}
/**
 * Return the JLabel47 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel47() {
	if (ivjJLabel47 == null) {
		try {
			ivjJLabel47 = new javax.swing.JLabel();
			ivjJLabel47.setName("JLabel47");
			ivjJLabel47.setText("Federal Owner:");
			ivjJLabel47.setBounds(315, 258, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel47;
}
/**
 * Return the JLabel48 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel48() {
	if (ivjJLabel48 == null) {
		try {
			ivjJLabel48 = new javax.swing.JLabel();
			ivjJLabel48.setName("JLabel48");
			ivjJLabel48.setText("Federal Other:");
			ivjJLabel48.setBounds(315, 292, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel48;
}
/**
 * Return the JLabel49 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel49() {
	if (ivjJLabel49 == null) {
		try {
			ivjJLabel49 = new javax.swing.JLabel();
			ivjJLabel49.setName("JLabel49");
			ivjJLabel49.setText("Source Agency:");
			ivjJLabel49.setBounds(315, 326, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel49;
}
/**
 * Return the JLabel5 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel5() {
	if (ivjJLabel5 == null) {
		try {
			ivjJLabel5 = new javax.swing.JLabel();
			ivjJLabel5.setName("JLabel5");
			ivjJLabel5.setText("State ID:");
			ivjJLabel5.setBounds(15, 102, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel5;
}
/**
 * Return the JLabel50 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel50() {
	if (ivjJLabel50 == null) {
		try {
			ivjJLabel50 = new javax.swing.JLabel();
			ivjJLabel50.setName("JLabel50");
			ivjJLabel50.setText("Section T R:");
			ivjJLabel50.setBounds(315, 360, 161, 14);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel50;
}
/**
 * Return the JLabel6 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel6() {
	if (ivjJLabel6 == null) {
		try {
			ivjJLabel6 = new javax.swing.JLabel();
			ivjJLabel6.setName("JLabel6");
			ivjJLabel6.setText("River:");
			ivjJLabel6.setBounds(15, 125, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel6;
}
/**
 * Return the JLabel7 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel7() {
	if (ivjJLabel7 == null) {
		try {
			ivjJLabel7 = new javax.swing.JLabel();
			ivjJLabel7.setName("JLabel7");
			ivjJLabel7.setText("County:");
			ivjJLabel7.setBounds(15, 148, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel7;
}
/**
 * Return the JLabel8 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel8() {
	if (ivjJLabel8 == null) {
		try {
			ivjJLabel8 = new javax.swing.JLabel();
			ivjJLabel8.setName("JLabel8");
			ivjJLabel8.setText("Latitude:");
			ivjJLabel8.setBounds(15, 171, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel8;
}
/**
 * Return the JLabel9 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel9() {
	if (ivjJLabel9 == null) {
		try {
			ivjJLabel9 = new javax.swing.JLabel();
			ivjJLabel9.setName("JLabel9");
			ivjJLabel9.setText("Longitude:");
			ivjJLabel9.setBounds(15, 194, 118, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel9;
}
/**
 * Return the JScrollPane1 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane1() {
	if (ivjJScrollPane1 == null) {
		try {
			ivjJScrollPane1 = new javax.swing.JScrollPane();
			ivjJScrollPane1.setName("JScrollPane1");
			ivjJScrollPane1.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
			ivjJScrollPane1.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			getJScrollPane1().setViewportView(getTableInput());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane1;
}
/**
 * Return the JScrollPane2 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane2() {
	if (ivjJScrollPane2 == null) {
		try {
			ivjJScrollPane2 = new javax.swing.JScrollPane();
			ivjJScrollPane2.setName("JScrollPane2");
			ivjJScrollPane2.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
			ivjJScrollPane2.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			getJScrollPane2().setViewportView(getTableOutput());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane2;
}
/**
 * Return the JPanel1 property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getPanelButtons() {
	if (ivjPanelButtons == null) {
		try {
			ivjPanelButtons = new javax.swing.JPanel();
			ivjPanelButtons.setName("PanelButtons");
			ivjPanelButtons.setPreferredSize(new java.awt.Dimension(210, 40));
			ivjPanelButtons.setBorder(new javax.swing.border.EtchedBorder());
			ivjPanelButtons.setLayout(new java.awt.FlowLayout());
			getPanelButtons().add(getButtonNewDam(), getButtonNewDam().getName());
			getPanelButtons().add(getButtonExport(), getButtonExport().getName());
			getPanelButtons().add(getButtonSave(), getButtonSave().getName());
			getPanelButtons().add(getButtonDeleteDam(), getButtonDeleteDam().getName());
			getPanelButtons().add(getButtonClose(), getButtonClose().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPanelButtons;
}
/**
 * Return the PanelConstruction property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getPanelConstruction() {
	if (ivjPanelConstruction == null) {
		try {
			ivjPanelConstruction = new javax.swing.JPanel();
			ivjPanelConstruction.setName("PanelConstruction");
			ivjPanelConstruction.setLayout(null);
			getPanelConstruction().add(getJLabel19(), getJLabel19().getName());
			getPanelConstruction().add(getJLabel20(), getJLabel20().getName());
			getPanelConstruction().add(getJLabel21(), getJLabel21().getName());
			getPanelConstruction().add(getJLabel22(), getJLabel22().getName());
			getPanelConstruction().add(getJLabel23(), getJLabel23().getName());
			getPanelConstruction().add(getJLabel24(), getJLabel24().getName());
			getPanelConstruction().add(getJLabel25(), getJLabel25().getName());
			getPanelConstruction().add(getJLabel271(), getJLabel271().getName());
			getPanelConstruction().add(getJLabel272(), getJLabel272().getName());
			getPanelConstruction().add(getJLabel273(), getJLabel273().getName());
			getPanelConstruction().add(getTextDamType(), getTextDamType().getName());
			getPanelConstruction().add(getTextDamHeight(), getTextDamHeight().getName());
			getPanelConstruction().add(getTextStructuralHeight(), getTextStructuralHeight().getName());
			getPanelConstruction().add(getTextHydraulicHeight(), getTextHydraulicHeight().getName());
			getPanelConstruction().add(getTextNIDHeight(), getTextNIDHeight().getName());
			getPanelConstruction().add(getTextNIDStorage(), getTextNIDStorage().getName());
			getPanelConstruction().add(getTextMaxStorage(), getTextMaxStorage().getName());
			getPanelConstruction().add(getTextNormalStorage(), getTextNormalStorage().getName());
			getPanelConstruction().add(getTextDamLength(), getTextDamLength().getName());
			getPanelConstruction().add(getTextSurfaceArea(), getTextSurfaceArea().getName());
			getPanelConstruction().add(getJLabel26(), getJLabel26().getName());
			getPanelConstruction().add(getJLabel191(), getJLabel191().getName());
			getPanelConstruction().add(getJLabel201(), getJLabel201().getName());
			getPanelConstruction().add(getJLabel211(), getJLabel211().getName());
			getPanelConstruction().add(getJLabel221(), getJLabel221().getName());
			getPanelConstruction().add(getJLabel231(), getJLabel231().getName());
			getPanelConstruction().add(getJLabel241(), getJLabel241().getName());
			getPanelConstruction().add(getJLabel251(), getJLabel251().getName());
			getPanelConstruction().add(getJLabel261(), getJLabel261().getName());
			getPanelConstruction().add(getJLabel27(), getJLabel27().getName());
			getPanelConstruction().add(getTextWidthOfLocks(), getTextWidthOfLocks().getName());
			getPanelConstruction().add(getTextLengthOfLocks(), getTextLengthOfLocks().getName());
			getPanelConstruction().add(getTextNumberOfLocks(), getTextNumberOfLocks().getName());
			getPanelConstruction().add(getTextOutletGates(), getTextOutletGates().getName());
			getPanelConstruction().add(getTextSpillwayWidth(), getTextSpillwayWidth().getName());
			getPanelConstruction().add(getTextSpillwayType(), getTextSpillwayType().getName());
			getPanelConstruction().add(getTextFoundation(), getTextFoundation().getName());
			getPanelConstruction().add(getTextCore(), getTextCore().getName());
			getPanelConstruction().add(getTextVolumeMaterial(), getTextVolumeMaterial().getName());
			getPanelConstruction().add(getTextMaxDischarge(), getTextMaxDischarge().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPanelConstruction;
}
/**
 * Return the PanelDownstream property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getPanelDownstream() {
	if (ivjPanelDownstream == null) {
		try {
			ivjPanelDownstream = new javax.swing.JPanel();
			ivjPanelDownstream.setName("PanelDownstream");
			ivjPanelDownstream.setLayout(new java.awt.BorderLayout());
			getPanelDownstream().add(getScrollDownstream(), "North");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPanelDownstream;
}
/**
 * Return the Page property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getPanelGeneral() {
	if (ivjPanelGeneral == null) {
		try {
			ivjPanelGeneral = new javax.swing.JPanel();
			ivjPanelGeneral.setName("PanelGeneral");
			ivjPanelGeneral.setLayout(null);
			getPanelGeneral().add(getJLabel1(), getJLabel1().getName());
			getPanelGeneral().add(getJLabel2(), getJLabel2().getName());
			getPanelGeneral().add(getJLabel3(), getJLabel3().getName());
			getPanelGeneral().add(getJLabel4(), getJLabel4().getName());
			getPanelGeneral().add(getJLabel5(), getJLabel5().getName());
			getPanelGeneral().add(getJLabel6(), getJLabel6().getName());
			getPanelGeneral().add(getJLabel7(), getJLabel7().getName());
			getPanelGeneral().add(getJLabel8(), getJLabel8().getName());
			getPanelGeneral().add(getJLabel9(), getJLabel9().getName());
			getPanelGeneral().add(getJLabel10(), getJLabel10().getName());
			getPanelGeneral().add(getJLabel11(), getJLabel11().getName());
			getPanelGeneral().add(getJLabel12(), getJLabel12().getName());
			getPanelGeneral().add(getJLabel13(), getJLabel13().getName());
			getPanelGeneral().add(getJLabel14(), getJLabel14().getName());
			getPanelGeneral().add(getJLabel15(), getJLabel15().getName());
			getPanelGeneral().add(getJLabel16(), getJLabel16().getName());
			getPanelGeneral().add(getJLabel17(), getJLabel17().getName());
			getPanelGeneral().add(getJLabel18(), getJLabel18().getName());
			getPanelGeneral().add(getTextNIDID(), getTextNIDID().getName());
			getPanelGeneral().add(getTextDamName(), getTextDamName().getName());
			getPanelGeneral().add(getTextOtherDamName(), getTextOtherDamName().getName());
			getPanelGeneral().add(getTextFormerDamName(), getTextFormerDamName().getName());
			getPanelGeneral().add(getTextStateID(), getTextStateID().getName());
			getPanelGeneral().add(getTextRiver(), getTextRiver().getName());
			getPanelGeneral().add(getTextCounty(), getTextCounty().getName());
			getPanelGeneral().add(getTextLatitude(), getTextLatitude().getName());
			getPanelGeneral().add(getTextLongitude(), getTextLongitude().getName());
			getPanelGeneral().add(getTextElevation(), getTextElevation().getName());
			getPanelGeneral().add(getTextTopoMap(), getTextTopoMap().getName());
			getPanelGeneral().add(getTextReturnFlowRegion(), getTextReturnFlowRegion().getName());
			getPanelGeneral().add(getTextDrainageArea(), getTextDrainageArea().getName());
			getPanelGeneral().add(getTextPrebreakAvailable(), getTextPrebreakAvailable().getName());
			getPanelGeneral().add(getTextComments(), getTextComments().getName());
			getPanelGeneral().add(getTextUpdated(), getTextUpdated().getName());
			getPanelGeneral().add(getTextHSA(), getTextHSA().getName());
			getPanelGeneral().add(getTextRFC(), getTextRFC().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPanelGeneral;
}
/**
 * Return the PanelInputs property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getPanelInputs() {
	if (ivjPanelInputs == null) {
		try {
			ivjPanelInputs = new javax.swing.JPanel();
			ivjPanelInputs.setName("PanelInputs");
			ivjPanelInputs.setLayout(new java.awt.BorderLayout());
			getPanelInputs().add(getJScrollPane1(), "North");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPanelInputs;
}
/**
 * Return the PanelOutput property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getPanelOutput() {
	if (ivjPanelOutput == null) {
		try {
			ivjPanelOutput = new javax.swing.JPanel();
			ivjPanelOutput.setName("PanelOutput");
			ivjPanelOutput.setLayout(new java.awt.BorderLayout());
			getPanelOutput().add(getJScrollPane2(), "North");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPanelOutput;
}
/**
 * Return the PanelPairs property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getPanelPairs() {
	if (ivjPanelPairs == null) {
		try {
			ivjPanelPairs = new javax.swing.JPanel();
			ivjPanelPairs.setName("PanelPairs");
			ivjPanelPairs.setLayout(new java.awt.BorderLayout());
			getPanelPairs().add(getScrollCrossSection(), "North");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPanelPairs;
}
/**
 * Return the PanelRegulatory property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getPanelRegulatory() {
	if (ivjPanelRegulatory == null) {
		try {
			ivjPanelRegulatory = new javax.swing.JPanel();
			ivjPanelRegulatory.setName("PanelRegulatory");
			ivjPanelRegulatory.setLayout(null);
			getPanelRegulatory().add(getJLabel28(), getJLabel28().getName());
			getPanelRegulatory().add(getJLabel29(), getJLabel29().getName());
			getPanelRegulatory().add(getJLabel30(), getJLabel30().getName());
			getPanelRegulatory().add(getJLabel31(), getJLabel31().getName());
			getPanelRegulatory().add(getJLabel32(), getJLabel32().getName());
			getPanelRegulatory().add(getJLabel33(), getJLabel33().getName());
			getPanelRegulatory().add(getJLabel34(), getJLabel34().getName());
			getPanelRegulatory().add(getJLabel35(), getJLabel35().getName());
			getPanelRegulatory().add(getJLabel36(), getJLabel36().getName());
			getPanelRegulatory().add(getJLabel37(), getJLabel37().getName());
			getPanelRegulatory().add(getJLabel38(), getJLabel38().getName());
			getPanelRegulatory().add(getJLabel39(), getJLabel39().getName());
			getPanelRegulatory().add(getJLabel40(), getJLabel40().getName());
			getPanelRegulatory().add(getJLabel41(), getJLabel41().getName());
			getPanelRegulatory().add(getJLabel42(), getJLabel42().getName());
			getPanelRegulatory().add(getJLabel43(), getJLabel43().getName());
			getPanelRegulatory().add(getJLabel44(), getJLabel44().getName());
			getPanelRegulatory().add(getJLabel45(), getJLabel45().getName());
			getPanelRegulatory().add(getJLabel46(), getJLabel46().getName());
			getPanelRegulatory().add(getJLabel47(), getJLabel47().getName());
			getPanelRegulatory().add(getJLabel48(), getJLabel48().getName());
			getPanelRegulatory().add(getJLabel49(), getJLabel49().getName());
			getPanelRegulatory().add(getJLabel50(), getJLabel50().getName());
			getPanelRegulatory().add(getTextOwnerName(), getTextOwnerName().getName());
			getPanelRegulatory().add(getTextOwnerType(), getTextOwnerType().getName());
			getPanelRegulatory().add(getTextYearCompleted(), getTextYearCompleted().getName());
			getPanelRegulatory().add(getTextYearModified(), getTextYearModified().getName());
			getPanelRegulatory().add(getTextPurposes(), getTextPurposes().getName());
			getPanelRegulatory().add(getTextDamDesigner(), getTextDamDesigner().getName());
			getPanelRegulatory().add(getTextPrivateOnFederalLand(), getTextPrivateOnFederalLand().getName());
			getPanelRegulatory().add(getTextDownstreamHazard(), getTextDownstreamHazard().getName());
			getPanelRegulatory().add(getTextEmergencyAction(), getTextEmergencyAction().getName());
			getPanelRegulatory().add(getTextInspectionDate(), getTextInspectionDate().getName());
			getPanelRegulatory().add(getTextInspectionFrequency(), getTextInspectionFrequency().getName());
			getPanelRegulatory().add(getTextStateRegulated(), getTextStateRegulated().getName());
			getPanelRegulatory().add(getTextStateRegulatoryAgency(), getTextStateRegulatoryAgency().getName());
			getPanelRegulatory().add(getTextFederalFunding(), getTextFederalFunding().getName());
			getPanelRegulatory().add(getTextFederalDesign(), getTextFederalDesign().getName());
			getPanelRegulatory().add(getTextFederalConstruction(), getTextFederalConstruction().getName());
			getPanelRegulatory().add(getTextFederalRegulatoryAgency(), getTextFederalRegulatoryAgency().getName());
			getPanelRegulatory().add(getTextFederalInspection(), getTextFederalInspection().getName());
			getPanelRegulatory().add(getTextFederalOperation(), getTextFederalOperation().getName());
			getPanelRegulatory().add(getTextFederalOwner(), getTextFederalOwner().getName());
			getPanelRegulatory().add(getTextFederalOther(), getTextFederalOther().getName());
			getPanelRegulatory().add(getTextSourceAgency(), getTextSourceAgency().getName());
			getPanelRegulatory().add(getTextSectionTR(), getTextSectionTR().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPanelRegulatory;
}
/**
 * Return the ScrollCrossSection property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getScrollCrossSection() {
	if (ivjScrollCrossSection == null) {
		try {
			ivjScrollCrossSection = new javax.swing.JScrollPane();
			ivjScrollCrossSection.setName("ScrollCrossSection");
			ivjScrollCrossSection.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
			ivjScrollCrossSection.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			getScrollCrossSection().setViewportView(getTableCrossSection());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjScrollCrossSection;
}
/**
 * Return the JScrollPane1 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getScrollDownstream() {
	if (ivjScrollDownstream == null) {
		try {
			ivjScrollDownstream = new javax.swing.JScrollPane();
			ivjScrollDownstream.setName("ScrollDownstream");
			ivjScrollDownstream.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
			ivjScrollDownstream.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			getScrollDownstream().setViewportView(getTableDownstream());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjScrollDownstream;
}
/**
 * Insert the method's description here.
 * Creation date: (2/18/2004 3:18:23 PM)
 * @return gov.damcat.data.Search
 */
public Search getSearchScreen() {
	return searchScreen;
}
/**
 * Return the JTabbedPane1 property value.
 * @return javax.swing.JTabbedPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTabbedPane getTabData() {
	if (ivjTabData == null) {
		try {
			ivjTabData = new javax.swing.JTabbedPane();
			ivjTabData.setName("TabData");
			ivjTabData.insertTab("General", null, getPanelGeneral(), null, 0);
			ivjTabData.insertTab("Construction", null, getPanelConstruction(), null, 1);
			ivjTabData.insertTab("Regulatory", null, getPanelRegulatory(), null, 2);
			ivjTabData.insertTab("Downstream", null, getPanelDownstream(), null, 3);
			ivjTabData.insertTab("Input", null, getPanelInputs(), null, 4);
			ivjTabData.insertTab("Output", null, getPanelOutput(), null, 5);
			ivjTabData.insertTab("Cross Sections", null, getPanelPairs(), null, 6);
			// user code begin {1}
			// changes could not be made via Visual Composition Editor, user's code overrides
			// initial settings to change tabs order in the tabbed pane
			ivjTabData.insertTab("Cross Sections", null, getPanelPairs(), null, 4);
			ivjTabData.insertTab("Input", null, getPanelInputs(), null, 5);
			ivjTabData.insertTab("Output", null, getPanelOutput(), null, 6);

			ivjTabData.addChangeListener(new ChangeListener()
			{
					public void stateChanged(ChangeEvent ev)
					{
						
						int n = ivjTabData.getSelectedIndex();
						set_indexTab(n);
					
						/*if((n == 3) || (n == 4) || (n == 5) || (n == 6))
						{
							getButtonNewDam().setEnabled(true);
						}
						if((n ==1) || (n == 2))
						{
							getButtonDeleteDam().setEnabled(false);
						}
						if((n ==0) || (n == 3) || ( n == 4) || (n == 5) || (n == 6))
						{
							getButtonDeleteDam().setEnabled(true);
						}	*/
						if(n == 0)
						{
							getButtonNewDam().setEnabled(false);
							getButtonNewDam().setText("New Dam");
							getButtonDeleteDam().setEnabled(true);
							
						}
						if(n == 1)
						{
							getButtonNewDam().setEnabled(false);
							getButtonNewDam().setText("New Dam");
							getButtonDeleteDam().setEnabled(false);
							
						}
						else if(n == 2)
						{
							getButtonNewDam().setEnabled(false);
							getButtonNewDam().setText("New Dam");
							getButtonDeleteDam().setEnabled(false);
							
						}
						else if(n == 3)
						{
							getButtonNewDam().setEnabled(true);
							getButtonNewDam().setText("New D/S Point");
							getButtonDeleteDam().setEnabled(true);
							
						}
						else if(n == 4)
						{
							getButtonNewDam().setEnabled(true);
							getButtonNewDam().setText("New XSection");
							getButtonDeleteDam().setEnabled(true);
							
						}
						else if(n == 5)
						{
							getButtonNewDam().setEnabled(true);
							getButtonNewDam().setText("New Input");
							getButtonDeleteDam().setEnabled(true);
							
						}
						else if(n == 6)
						{
							getButtonNewDam().setEnabled(true);
							getButtonNewDam().setText("New Output");
							getButtonDeleteDam().setEnabled(true);
							
						}
					}
			
			});		
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTabData;
}
/**
 * Return the TableColumn1 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn1() {
	if (ivjTableColumn1 == null) {
		try {
			ivjTableColumn1 = new javax.swing.table.TableColumn();
			ivjTableColumn1.setHeaderValue("D/S Point Name");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn1.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn1;
}
/**
 * Return the TableColumn10 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn10() {
	if (ivjTableColumn10 == null) {
		try {
			ivjTableColumn10 = new javax.swing.table.TableColumn();
			ivjTableColumn10.setModelIndex(9);
			ivjTableColumn10.setHeaderValue("Off Channel Mannings N");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn10;
}
/**
 * Return the TableColumn11 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn11() {
	if (ivjTableColumn11 == null) {
		try {
			ivjTableColumn11 = new javax.swing.table.TableColumn();
			ivjTableColumn11.setModelIndex(10);
			ivjTableColumn11.setHeaderValue("Comments");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn11;
}
/**
 * Return the TableColumn12 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn12() {
	if (ivjTableColumn12 == null) {
		try {
			ivjTableColumn12 = new javax.swing.table.TableColumn();
			ivjTableColumn12.setModelIndex(11);
			ivjTableColumn12.setHeaderValue("Updated");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn12.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn12;
}
/**
 * Return the TableColumn13 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
/*private javax.swing.table.TableColumn getTableColumn13() {
	if (ivjTableColumn13 == null) {
		try {
			ivjTableColumn13 = new javax.swing.table.TableColumn();
			ivjTableColumn13.setModelIndex(12);
			ivjTableColumn13.setHeaderValue("Comments");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn13;
}*/
/**
 * Return the TableColumn14 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
/*private javax.swing.table.TableColumn getTableColumn14() {
	if (ivjTableColumn14 == null) {
		try {
			ivjTableColumn14 = new javax.swing.table.TableColumn();
			ivjTableColumn14.setModelIndex(13);
			ivjTableColumn14.setHeaderValue("Updated");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn14;
}*/
/**
 * Return the TableColumn15 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn15() {
	if (ivjTableColumn15 == null) {
		try {
			ivjTableColumn15 = new javax.swing.table.TableColumn();
			ivjTableColumn15.setModelIndex(2);
			ivjTableColumn15.setHeaderValue("Pair Number");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn15;
}
/**
 * Return the TableColumn16 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn16() {
	if (ivjTableColumn16 == null) {
		try {
			ivjTableColumn16 = new javax.swing.table.TableColumn();
			ivjTableColumn16.setModelIndex(1);
			ivjTableColumn16.setHeaderValue("Cross Section Type");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn16.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn16;
}
/**
 * Return the TableColumn17 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn17() {
	if (ivjTableColumn17 == null) {
		try {
			ivjTableColumn17 = new javax.swing.table.TableColumn();
			ivjTableColumn17.setModelIndex(0);
			ivjTableColumn17.setHeaderValue("D/S Point Name");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn17.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn17;
}
/**
 * Return the TableColumn18 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn18() {
	if (ivjTableColumn18 == null) {
		try {
			ivjTableColumn18 = new javax.swing.table.TableColumn();
			ivjTableColumn18.setModelIndex(3);
			ivjTableColumn18.setHeaderValue("Gage Elevation (ft MSL)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn18;
}
/**
 * Return the TableColumn19 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn19() {
	if (ivjTableColumn19 == null) {
		try {
			ivjTableColumn19 = new javax.swing.table.TableColumn();
			ivjTableColumn19.setModelIndex(4);
			ivjTableColumn19.setHeaderValue("Top Width (ft)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn19;
}
/**
 * Return the TableColumn2 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn2() {
	if (ivjTableColumn2 == null) {
		try {
			ivjTableColumn2 = new javax.swing.table.TableColumn();
			ivjTableColumn2.setModelIndex(1);
			ivjTableColumn2.setHeaderValue("Best CrossSection Type");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn2;
}
/**
 * Return the TableColumn20 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn20() {
	if (ivjTableColumn20 == null) {
		try {
			ivjTableColumn20 = new javax.swing.table.TableColumn();
			ivjTableColumn20.setModelIndex(6);
			ivjTableColumn20.setHeaderValue("Mannings N");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn20;
}
/**
 * Return the TableColumn21 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn21() {
	if (ivjTableColumn21 == null) {
		try {
			ivjTableColumn21 = new javax.swing.table.TableColumn();
			ivjTableColumn21.setModelIndex(5);
			ivjTableColumn21.setHeaderValue("Inactive Width (ft)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn21;
}
/**
 * Return the TableColumn22 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn22() {
	if (ivjTableColumn22 == null) {
		try {
			ivjTableColumn22 = new javax.swing.table.TableColumn();
			ivjTableColumn22.setModelIndex(7);
			ivjTableColumn22.setHeaderValue("Updated");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn22.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn22;
}
/**
 * Return the TableColumn24 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn24() {
	if (ivjTableColumn24 == null) {
		try {
			ivjTableColumn24 = new javax.swing.table.TableColumn();
			ivjTableColumn24.setWidth(25);
			ivjTableColumn24.setHeaderValue("Source");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn24.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn24;
}
/**
 * Return the TableColumn25 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn25() {
	if (ivjTableColumn25 == null) {
		try {
			ivjTableColumn25 = new javax.swing.table.TableColumn();
			ivjTableColumn25.setModelIndex(1);
			ivjTableColumn25.setWidth(25);
			ivjTableColumn25.setHeaderValue("Scenario");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn25.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn25;
}
/**
 * Return the TableColumn26 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn26() {
	if (ivjTableColumn26 == null) {
		try {
			ivjTableColumn26 = new javax.swing.table.TableColumn();
			ivjTableColumn26.setModelIndex(2);
			ivjTableColumn26.setWidth(25);
			ivjTableColumn26.setHeaderValue("Starting Water Surface (ft MSL)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn26;
}
/**
 * Return the TableColumn27 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn27() {
	if (ivjTableColumn27 == null) {
		try {
			ivjTableColumn27 = new javax.swing.table.TableColumn();
			ivjTableColumn27.setWidth(25);
			ivjTableColumn27.setHeaderValue("Source");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn27.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn27;
}
/**
 * Return the TableColumn28 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn28() {
	if (ivjTableColumn28 == null) {
		try {
			ivjTableColumn28 = new javax.swing.table.TableColumn();
			ivjTableColumn28.setModelIndex(3);
			ivjTableColumn28.setWidth(25);
			ivjTableColumn28.setHeaderValue("Bottom of Breach Width (ft MSL)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn28;
}
/**
 * Return the TableColumn29 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn29() {
	if (ivjTableColumn29 == null) {
		try {
			ivjTableColumn29 = new javax.swing.table.TableColumn();
			ivjTableColumn29.setModelIndex(1);
			ivjTableColumn29.setWidth(25);
			ivjTableColumn29.setHeaderValue("Scenario");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn29.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn29;
}
/**
 * Return the TableColumn3 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn3() {
	if (ivjTableColumn3 == null) {
		try {
			ivjTableColumn3 = new javax.swing.table.TableColumn();
			ivjTableColumn3.setModelIndex(2);
			ivjTableColumn3.setHeaderValue("Distance from Dam (mi)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn3;
}
/**
 * Return the TableColumn30 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn30() {
	if (ivjTableColumn30 == null) {
		try {
			ivjTableColumn30 = new javax.swing.table.TableColumn();
			ivjTableColumn30.setModelIndex(4);
			ivjTableColumn30.setWidth(25);
			ivjTableColumn30.setHeaderValue("Starting Volume (acre-ft)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn30;
}
/**
 * Return the TableColumn31 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn31() {
	if (ivjTableColumn31 == null) {
		try {
			ivjTableColumn31 = new javax.swing.table.TableColumn();
			ivjTableColumn31.setModelIndex(2);
			ivjTableColumn31.setWidth(25);
			ivjTableColumn31.setHeaderValue("D / S Point Name");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn31.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn31;
}
/**
 * Return the TableColumn32 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn32() {
	if (ivjTableColumn32 == null) {
		try {
			ivjTableColumn32 = new javax.swing.table.TableColumn();
			ivjTableColumn32.setModelIndex(5);
			ivjTableColumn32.setWidth(25);
			ivjTableColumn32.setHeaderValue("Starting Surface Area (acre)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn32;
}
/**
 * Return the TableColumn33 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn33() {
	if (ivjTableColumn33 == null) {
		try {
			ivjTableColumn33 = new javax.swing.table.TableColumn();
			ivjTableColumn33.setModelIndex(6);
			ivjTableColumn33.setWidth(25);
			ivjTableColumn33.setHeaderValue("Time Of Failure (min)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn33;
}
/**
 * Return the TableColumn34 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn34() {
	if (ivjTableColumn34 == null) {
		try {
			ivjTableColumn34 = new javax.swing.table.TableColumn();
			ivjTableColumn34.setModelIndex(7);
			ivjTableColumn34.setWidth(25);
			ivjTableColumn34.setHeaderValue("Additional Flow to Add (cfs)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn34;
}
/**
 * Return the TableColumn35 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn35() {
	if (ivjTableColumn35 == null) {
		try {
			ivjTableColumn35 = new javax.swing.table.TableColumn();
			ivjTableColumn35.setModelIndex(8);
			ivjTableColumn35.setHeaderValue("Final Breach Width (ft)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn35;
}
/**
 * Return the TableColumn36 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn36() {
	if (ivjTableColumn36 == null) {
		try {
			ivjTableColumn36 = new javax.swing.table.TableColumn();
			ivjTableColumn36.setModelIndex(9);
			ivjTableColumn36.setHeaderValue("Dam Type Code");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn36;
}
/**
 * Return the TableColumn37 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn37() {
	if (ivjTableColumn37 == null) {
		try {
			ivjTableColumn37 = new javax.swing.table.TableColumn();
			ivjTableColumn37.setModelIndex(10);
			ivjTableColumn37.setHeaderValue("Comments");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn37;
}
/**
 * Return the TableColumn38 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn38() {
	if (ivjTableColumn38 == null) {
		try {
			ivjTableColumn38 = new javax.swing.table.TableColumn();
			ivjTableColumn38.setModelIndex(11);
			ivjTableColumn38.setHeaderValue("Updated");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn38.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn38;
}
/**
 * Return the TableColumn39 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn39() {
	if (ivjTableColumn39 == null) {
		try {
			ivjTableColumn39 = new javax.swing.table.TableColumn();
			ivjTableColumn39.setModelIndex(3);
			ivjTableColumn39.setWidth(25);
			ivjTableColumn39.setHeaderValue("Slope (ft/mi)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn39;
}
/**
 * Return the TableColumn4 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn4() {
	if (ivjTableColumn4 == null) {
		try {
			ivjTableColumn4 = new javax.swing.table.TableColumn();
			ivjTableColumn4.setModelIndex(3);
			ivjTableColumn4.setHeaderValue("Latitude");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn4;
}
/**
 * Return the TableColumn40 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn40() {
	if (ivjTableColumn40 == null) {
		try {
			ivjTableColumn40 = new javax.swing.table.TableColumn();
			ivjTableColumn40.setModelIndex(4);
			ivjTableColumn40.setWidth(25);
			ivjTableColumn40.setHeaderValue("Max Flow (cfs)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn40;
}
/**
 * Return the TableColumn41 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn41() {
	if (ivjTableColumn41 == null) {
		try {
			ivjTableColumn41 = new javax.swing.table.TableColumn();
			ivjTableColumn41.setModelIndex(5);
			ivjTableColumn41.setWidth(25);
			ivjTableColumn41.setHeaderValue("Max Elevation (Depth) (ft MSL)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn41;
}
/**
 * Return the TableColumn42 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn42() {
	if (ivjTableColumn42 == null) {
		try {
			ivjTableColumn42 = new javax.swing.table.TableColumn();
			ivjTableColumn42.setModelIndex(6);
			ivjTableColumn42.setWidth(25);
			ivjTableColumn42.setHeaderValue("Time to Max Elevation (hr)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn42;
}
/**
 * Return the TableColumn43 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn43() {
	if (ivjTableColumn43 == null) {
		try {
			ivjTableColumn43 = new javax.swing.table.TableColumn();
			ivjTableColumn43.setModelIndex(7);
			ivjTableColumn43.setWidth(25);
			ivjTableColumn43.setHeaderValue("Time to Flood (hr)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn43;
}
/**
 * Return the TableColumn44 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn44() {
	if (ivjTableColumn44 == null) {
		try {
			ivjTableColumn44 = new javax.swing.table.TableColumn();
			ivjTableColumn44.setModelIndex(8);
			ivjTableColumn44.setHeaderValue("Time to De-flood (hr)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn44;
}
/**
 * Return the TableColumn45 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn45() {
	if (ivjTableColumn45 == null) {
		try {
			ivjTableColumn45 = new javax.swing.table.TableColumn();
			ivjTableColumn45.setModelIndex(9);
			ivjTableColumn45.setHeaderValue("Comments");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn45;
}
/**
 * Return the TableColumn46 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn46() {
	if (ivjTableColumn46 == null) {
		try {
			ivjTableColumn46 = new javax.swing.table.TableColumn();
			ivjTableColumn46.setModelIndex(10);
			ivjTableColumn46.setHeaderValue("Updated");
			// user code begin {1}
			EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableColumn46.setCellRenderer(editDamCellRenderer);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn46;
}
/**
 * Return the TableColumn5 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn5() {
	if (ivjTableColumn5 == null) {
		try {
			ivjTableColumn5 = new javax.swing.table.TableColumn();
			ivjTableColumn5.setModelIndex(4);
			ivjTableColumn5.setHeaderValue("Longitude");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn5;
}
/**
 * Return the TableColumn6 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn6() {
	if (ivjTableColumn6 == null) {
		try {
			ivjTableColumn6 = new javax.swing.table.TableColumn();
			ivjTableColumn6.setModelIndex(5);
			ivjTableColumn6.setHeaderValue("Gage Elevation (ft MSL)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn6;
}
/**
 * Return the TableColumn7 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn7() {
	if (ivjTableColumn7 == null) {
		try {
			ivjTableColumn7 = new javax.swing.table.TableColumn();
			ivjTableColumn7.setModelIndex(6);
			ivjTableColumn7.setHeaderValue("Flood Flow (cfs)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn7;
}
/**
 * Return the TableColumn8 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn8() {
	if (ivjTableColumn8 == null) {
		try {
			ivjTableColumn8 = new javax.swing.table.TableColumn();
			ivjTableColumn8.setModelIndex(7);
			ivjTableColumn8.setHeaderValue("Flood Elevation (Depth) (ft MSL)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn8;
}
/**
 * Return the TableColumn9 property value.
 * @return javax.swing.table.TableColumn
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.table.TableColumn getTableColumn9() {
	if (ivjTableColumn9 == null) {
		try {
			ivjTableColumn9 = new javax.swing.table.TableColumn();
			ivjTableColumn9.setModelIndex(8);
			ivjTableColumn9.setHeaderValue("Flood Width (ft)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableColumn9;
}
/**
 * Return the TableCrossSection property value.
 * @return javax.swing.JTable
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTable getTableCrossSection() {
	if (ivjTableCrossSection == null) {
		try {
			ivjTableCrossSection = new javax.swing.JTable();
			ivjTableCrossSection.setName("TableCrossSection");
			getScrollCrossSection().setColumnHeaderView(ivjTableCrossSection.getTableHeader());
			getScrollCrossSection().getViewport().setBackingStoreEnabled(true);
			ivjTableCrossSection.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
			ivjTableCrossSection.setBounds(0, 0, 200, 200);
			ivjTableCrossSection.setAutoCreateColumnsFromModel(false);
			ivjTableCrossSection.addColumn(getTableColumn17());
			ivjTableCrossSection.addColumn(getTableColumn16());
			ivjTableCrossSection.addColumn(getTableColumn15());
			ivjTableCrossSection.addColumn(getTableColumn18());
			ivjTableCrossSection.addColumn(getTableColumn19());
			ivjTableCrossSection.addColumn(getTableColumn21());
			ivjTableCrossSection.addColumn(getTableColumn20());
			ivjTableCrossSection.addColumn(getTableColumn22());
			// user code begin {1}
			
			
			/*EditDamCellRenderer editDamCellRenderer = new EditDamCellRenderer();
			ivjTableCrossSection.getColumn(ivjTableColumn17).setCellRenderer(editDamCellRenderer);*/
			
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableCrossSection;
}
/**
 * Return the ScrollPaneTable property value.
 * @return javax.swing.JTable
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTable getTableDownstream() {
	if (ivjTableDownstream == null) {
		try {
			ivjTableDownstream = new javax.swing.JTable();
			ivjTableDownstream.setName("TableDownstream");
			getScrollDownstream().setColumnHeaderView(ivjTableDownstream.getTableHeader());
			getScrollDownstream().getViewport().setBackingStoreEnabled(true);
			ivjTableDownstream.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
			ivjTableDownstream.setComponentOrientation(java.awt.ComponentOrientation.LEFT_TO_RIGHT);
			ivjTableDownstream.setBounds(0, 0, 200, 200);
			ivjTableDownstream.setAutoCreateColumnsFromModel(false);
			ivjTableDownstream.addColumn(getTableColumn1());
			ivjTableDownstream.addColumn(getTableColumn2());
			ivjTableDownstream.addColumn(getTableColumn3());
			ivjTableDownstream.addColumn(getTableColumn4());
			ivjTableDownstream.addColumn(getTableColumn5());
			ivjTableDownstream.addColumn(getTableColumn6());
			ivjTableDownstream.addColumn(getTableColumn7());
			ivjTableDownstream.addColumn(getTableColumn8());
			ivjTableDownstream.addColumn(getTableColumn9());
			ivjTableDownstream.addColumn(getTableColumn10());
			ivjTableDownstream.addColumn(getTableColumn11());
			ivjTableDownstream.addColumn(getTableColumn12());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableDownstream;
}
/**
 * Return the ScrollPaneTable property value.
 * @return javax.swing.JTable
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTable getTableInput() {
	if (ivjTableInput == null) {
		try {
			ivjTableInput = new javax.swing.JTable();
			ivjTableInput.setName("TableInput");
			getJScrollPane1().setColumnHeaderView(ivjTableInput.getTableHeader());
			getJScrollPane1().getViewport().setBackingStoreEnabled(true);
			ivjTableInput.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
			ivjTableInput.setBounds(0, 0, 200, 200);
			ivjTableInput.setAutoCreateColumnsFromModel(false);
			ivjTableInput.addColumn(getTableColumn24());
			ivjTableInput.addColumn(getTableColumn25());
			ivjTableInput.addColumn(getTableColumn26());
			ivjTableInput.addColumn(getTableColumn28());
			ivjTableInput.addColumn(getTableColumn30());
			ivjTableInput.addColumn(getTableColumn32());
			ivjTableInput.addColumn(getTableColumn33());
			ivjTableInput.addColumn(getTableColumn34());
			ivjTableInput.addColumn(getTableColumn35());
			ivjTableInput.addColumn(getTableColumn36());
			ivjTableInput.addColumn(getTableColumn37());
			ivjTableInput.addColumn(getTableColumn38());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableInput;
}
/**
 * Return the TableOutput property value.
 * @return javax.swing.JTable
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTable getTableOutput() {
	if (ivjTableOutput == null) {
		try {
			ivjTableOutput = new javax.swing.JTable();
			ivjTableOutput.setName("TableOutput");
			getJScrollPane2().setColumnHeaderView(ivjTableOutput.getTableHeader());
			getJScrollPane2().getViewport().setBackingStoreEnabled(true);
			ivjTableOutput.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
			ivjTableOutput.setBounds(0, 0, 200, 200);
			ivjTableOutput.setAutoCreateColumnsFromModel(false);
			ivjTableOutput.addColumn(getTableColumn27());
			ivjTableOutput.addColumn(getTableColumn29());
			ivjTableOutput.addColumn(getTableColumn31());
			ivjTableOutput.addColumn(getTableColumn39());
			ivjTableOutput.addColumn(getTableColumn40());
			ivjTableOutput.addColumn(getTableColumn41());
			ivjTableOutput.addColumn(getTableColumn42());
			ivjTableOutput.addColumn(getTableColumn43());
			ivjTableOutput.addColumn(getTableColumn44());
			ivjTableOutput.addColumn(getTableColumn45());
			ivjTableOutput.addColumn(getTableColumn46());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTableOutput;
}
/**
 * Return the TextComments property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextComments() {
	if (ivjTextComments == null) {
		try {
			ivjTextComments = new javax.swing.JTextField();
			ivjTextComments.setName("TextComments");
			ivjTextComments.setBounds(213, 333, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextComments;
}
/**
 * Return the TextCore property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextCore() {
	if (ivjTextCore == null) {
		try {
			ivjTextCore = new javax.swing.JTextField();
			ivjTextCore.setName("TextCore");
			ivjTextCore.setText("");
			ivjTextCore.setBounds(482, 103, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextCore;
}
/**
 * Return the TextCounty property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextCounty() {
	if (ivjTextCounty == null) {
		try {
			ivjTextCounty = new javax.swing.JTextField();
			ivjTextCounty.setName("TextCounty");
			ivjTextCounty.setBounds(213, 149, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextCounty;
}
/**
 * Return the TextDamDesigner property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextDamDesigner() {
	if (ivjTextDamDesigner == null) {
		try {
			ivjTextDamDesigner = new javax.swing.JTextField();
			ivjTextDamDesigner.setName("TextDamDesigner");
			ivjTextDamDesigner.setBounds(158, 188, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextDamDesigner;
}
/**
 * Return the TextDamHeight property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextDamHeight() {
	if (ivjTextDamHeight == null) {
		try {
			ivjTextDamHeight = new javax.swing.JTextField();
			ivjTextDamHeight.setName("TextDamHeight");
			ivjTextDamHeight.setText("");
			ivjTextDamHeight.setBounds(162, 62, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextDamHeight;
}
/**
 * Return the TextDamLength property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextDamLength() {
	if (ivjTextDamLength == null) {
		try {
			ivjTextDamLength = new javax.swing.JTextField();
			ivjTextDamLength.setName("TextDamLength");
			ivjTextDamLength.setText("");
			ivjTextDamLength.setBounds(162, 349, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextDamLength;
}
/**
 * Return the TextDamName property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextDamName() {
	if (ivjTextDamName == null) {
		try {
			ivjTextDamName = new javax.swing.JTextField();
			ivjTextDamName.setName("TextDamName");
			ivjTextDamName.setBounds(213, 34, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextDamName;
}
/**
 * Return the TextDamType property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextDamType() {
	if (ivjTextDamType == null) {
		try {
			ivjTextDamType = new javax.swing.JTextField();
			ivjTextDamType.setName("TextDamType");
			ivjTextDamType.setText("");
			ivjTextDamType.setBounds(162, 21, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextDamType;
}
/**
 * Return the TextDownstreamHazard property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextDownstreamHazard() {
	if (ivjTextDownstreamHazard == null) {
		try {
			ivjTextDownstreamHazard = new javax.swing.JTextField();
			ivjTextDownstreamHazard.setName("TextDownstreamHazard");
			ivjTextDownstreamHazard.setBounds(158, 256, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextDownstreamHazard;
}
/**
 * Return the TextDrainageArea property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextDrainageArea() {
	if (ivjTextDrainageArea == null) {
		try {
			ivjTextDrainageArea = new javax.swing.JTextField();
			ivjTextDrainageArea.setName("TextDrainageArea");
			ivjTextDrainageArea.setBounds(213, 287, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextDrainageArea;
}
/**
 * Return the TextElevation property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextElevation() {
	if (ivjTextElevation == null) {
		try {
			ivjTextElevation = new javax.swing.JTextField();
			ivjTextElevation.setName("TextElevation");
			ivjTextElevation.setBounds(213, 218, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextElevation;
}
/**
 * Return the TextEmergencyAction property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextEmergencyAction() {
	if (ivjTextEmergencyAction == null) {
		try {
			ivjTextEmergencyAction = new javax.swing.JTextField();
			ivjTextEmergencyAction.setName("TextEmergencyAction");
			ivjTextEmergencyAction.setBounds(158, 290, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextEmergencyAction;
}
/**
 * Return the TextFederalConstruction property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFederalConstruction() {
	if (ivjTextFederalConstruction == null) {
		try {
			ivjTextFederalConstruction = new javax.swing.JTextField();
			ivjTextFederalConstruction.setName("TextFederalConstruction");
			ivjTextFederalConstruction.setBounds(476, 120, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFederalConstruction;
}
/**
 * Return the TextFederalDesign property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFederalDesign() {
	if (ivjTextFederalDesign == null) {
		try {
			ivjTextFederalDesign = new javax.swing.JTextField();
			ivjTextFederalDesign.setName("TextFederalDesign");
			ivjTextFederalDesign.setBounds(476, 86, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFederalDesign;
}
/**
 * Return the TextFederalFunding property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFederalFunding() {
	if (ivjTextFederalFunding == null) {
		try {
			ivjTextFederalFunding = new javax.swing.JTextField();
			ivjTextFederalFunding.setName("TextFederalFunding");
			ivjTextFederalFunding.setBounds(476, 52, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFederalFunding;
}
/**
 * Return the TextFederalInspection property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFederalInspection() {
	if (ivjTextFederalInspection == null) {
		try {
			ivjTextFederalInspection = new javax.swing.JTextField();
			ivjTextFederalInspection.setName("TextFederalInspection");
			ivjTextFederalInspection.setBounds(476, 188, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFederalInspection;
}
/**
 * Return the TextFederalOperation property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFederalOperation() {
	if (ivjTextFederalOperation == null) {
		try {
			ivjTextFederalOperation = new javax.swing.JTextField();
			ivjTextFederalOperation.setName("TextFederalOperation");
			ivjTextFederalOperation.setBounds(476, 222, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFederalOperation;
}
/**
 * Return the TextFederalOther property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFederalOther() {
	if (ivjTextFederalOther == null) {
		try {
			ivjTextFederalOther = new javax.swing.JTextField();
			ivjTextFederalOther.setName("TextFederalOther");
			ivjTextFederalOther.setBounds(476, 290, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFederalOther;
}
/**
 * Return the TextFederalOwner property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFederalOwner() {
	if (ivjTextFederalOwner == null) {
		try {
			ivjTextFederalOwner = new javax.swing.JTextField();
			ivjTextFederalOwner.setName("TextFederalOwner");
			ivjTextFederalOwner.setBounds(476, 256, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFederalOwner;
}
/**
 * Return the TextFederalRegulatoryAgency property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFederalRegulatoryAgency() {
	if (ivjTextFederalRegulatoryAgency == null) {
		try {
			ivjTextFederalRegulatoryAgency = new javax.swing.JTextField();
			ivjTextFederalRegulatoryAgency.setName("TextFederalRegulatoryAgency");
			ivjTextFederalRegulatoryAgency.setBounds(476, 154, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFederalRegulatoryAgency;
}
/**
 * Return the TextFormerDamName property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFormerDamName() {
	if (ivjTextFormerDamName == null) {
		try {
			ivjTextFormerDamName = new javax.swing.JTextField();
			ivjTextFormerDamName.setName("TextFormerDamName");
			ivjTextFormerDamName.setBounds(213, 80, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFormerDamName;
}
/**
 * Return the TextFoundation property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextFoundation() {
	if (ivjTextFoundation == null) {
		try {
			ivjTextFoundation = new javax.swing.JTextField();
			ivjTextFoundation.setName("TextFoundation");
			ivjTextFoundation.setText("");
			ivjTextFoundation.setBounds(482, 144, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextFoundation;
}
/**
 * Return the TextHSA property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextHSA() {
	if (ivjTextHSA == null) {
		try {
			ivjTextHSA = new javax.swing.JTextField();
			ivjTextHSA.setName("TextHSA");
			ivjTextHSA.setBounds(213, 379, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextHSA;
}
/**
 * Return the TextHydraulicHeight property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextHydraulicHeight() {
	if (ivjTextHydraulicHeight == null) {
		try {
			ivjTextHydraulicHeight = new javax.swing.JTextField();
			ivjTextHydraulicHeight.setName("TextHydraulicHeight");
			ivjTextHydraulicHeight.setText("");
			ivjTextHydraulicHeight.setBounds(162, 144, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextHydraulicHeight;
}
/**
 * Return the TextInspectionDate property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextInspectionDate() {
	if (ivjTextInspectionDate == null) {
		try {
			ivjTextInspectionDate = new javax.swing.JTextField();
			ivjTextInspectionDate.setName("TextInspectionDate");
			ivjTextInspectionDate.setBounds(158, 324, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextInspectionDate;
}
/**
 * Return the TextInspectionFrequency property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextInspectionFrequency() {
	if (ivjTextInspectionFrequency == null) {
		try {
			ivjTextInspectionFrequency = new javax.swing.JTextField();
			ivjTextInspectionFrequency.setName("TextInspectionFrequency");
			ivjTextInspectionFrequency.setBounds(158, 358, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextInspectionFrequency;
}
/**
 * Return the TextLatitude property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextLatitude() {
	if (ivjTextLatitude == null) {
		try {
			ivjTextLatitude = new javax.swing.JTextField();
			ivjTextLatitude.setName("TextLatitude");
			ivjTextLatitude.setBounds(213, 172, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextLatitude;
}
/**
 * Return the TextLengthOfLocks property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextLengthOfLocks() {
	if (ivjTextLengthOfLocks == null) {
		try {
			ivjTextLengthOfLocks = new javax.swing.JTextField();
			ivjTextLengthOfLocks.setName("TextLengthOfLocks");
			ivjTextLengthOfLocks.setText("");
			ivjTextLengthOfLocks.setBounds(482, 349, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextLengthOfLocks;
}
/**
 * Return the TextLongitude property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextLongitude() {
	if (ivjTextLongitude == null) {
		try {
			ivjTextLongitude = new javax.swing.JTextField();
			ivjTextLongitude.setName("TextLongitude");
			ivjTextLongitude.setBounds(213, 195, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextLongitude;
}
/**
 * Return the TextMaxDischarge property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextMaxDischarge() {
	if (ivjTextMaxDischarge == null) {
		try {
			ivjTextMaxDischarge = new javax.swing.JTextField();
			ivjTextMaxDischarge.setName("TextMaxDischarge");
			ivjTextMaxDischarge.setText("");
			ivjTextMaxDischarge.setBounds(482, 21, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextMaxDischarge;
}
/**
 * Return the TextMaxStorage property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextMaxStorage() {
	if (ivjTextMaxStorage == null) {
		try {
			ivjTextMaxStorage = new javax.swing.JTextField();
			ivjTextMaxStorage.setName("TextMaxStorage");
			ivjTextMaxStorage.setText("");
			ivjTextMaxStorage.setBounds(162, 267, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextMaxStorage;
}
/**
 * Return the TextNIDHeight property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextNIDHeight() {
	if (ivjTextNIDHeight == null) {
		try {
			ivjTextNIDHeight = new javax.swing.JTextField();
			ivjTextNIDHeight.setName("TextNIDHeight");
			ivjTextNIDHeight.setText("");
			ivjTextNIDHeight.setBounds(162, 185, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextNIDHeight;
}
/**
 * Return the JTextField1 property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextNIDID() {
	if (ivjTextNIDID == null) {
		try {
			ivjTextNIDID = new javax.swing.JTextField();
			ivjTextNIDID.setName("TextNIDID");
			ivjTextNIDID.setBounds(213, 11, 408, 20);
			ivjTextNIDID.setEnabled(false);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextNIDID;
}
/**
 * Return the TextNIDStorage property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextNIDStorage() {
	if (ivjTextNIDStorage == null) {
		try {
			ivjTextNIDStorage = new javax.swing.JTextField();
			ivjTextNIDStorage.setName("TextNIDStorage");
			ivjTextNIDStorage.setText("");
			ivjTextNIDStorage.setBounds(162, 226, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextNIDStorage;
}
/**
 * Return the TextNormalStorage property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextNormalStorage() {
	if (ivjTextNormalStorage == null) {
		try {
			ivjTextNormalStorage = new javax.swing.JTextField();
			ivjTextNormalStorage.setName("TextNormalStorage");
			ivjTextNormalStorage.setText("");
			ivjTextNormalStorage.setBounds(162, 308, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextNormalStorage;
}
/**
 * Return the TextNumberOfLocks property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextNumberOfLocks() {
	if (ivjTextNumberOfLocks == null) {
		try {
			ivjTextNumberOfLocks = new javax.swing.JTextField();
			ivjTextNumberOfLocks.setName("TextNumberOfLocks");
			ivjTextNumberOfLocks.setText("");
			ivjTextNumberOfLocks.setBounds(482, 308, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextNumberOfLocks;
}
/**
 * Return the TextOtherDamName property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextOtherDamName() {
	if (ivjTextOtherDamName == null) {
		try {
			ivjTextOtherDamName = new javax.swing.JTextField();
			ivjTextOtherDamName.setName("TextOtherDamName");
			ivjTextOtherDamName.setBounds(213, 57, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextOtherDamName;
}
/**
 * Return the TextOutletGates property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextOutletGates() {
	if (ivjTextOutletGates == null) {
		try {
			ivjTextOutletGates = new javax.swing.JTextField();
			ivjTextOutletGates.setName("TextOutletGates");
			ivjTextOutletGates.setText("");
			ivjTextOutletGates.setBounds(482, 267, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextOutletGates;
}
/**
 * Return the TextOwnerName property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextOwnerName() {
	if (ivjTextOwnerName == null) {
		try {
			ivjTextOwnerName = new javax.swing.JTextField();
			ivjTextOwnerName.setName("TextOwnerName");
			ivjTextOwnerName.setBounds(158, 18, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextOwnerName;
}
/**
 * Return the TextOwnerType property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextOwnerType() {
	if (ivjTextOwnerType == null) {
		try {
			ivjTextOwnerType = new javax.swing.JTextField();
			ivjTextOwnerType.setName("TextOwnerType");
			ivjTextOwnerType.setBounds(158, 52, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextOwnerType;
}
/**
 * Return the TextPrebreakAvailable property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextPrebreakAvailable() {
	if (ivjTextPrebreakAvailable == null) {
		try {
			ivjTextPrebreakAvailable = new javax.swing.JTextField();
			ivjTextPrebreakAvailable.setName("TextPrebreakAvailable");
			ivjTextPrebreakAvailable.setBounds(213, 310, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextPrebreakAvailable;
}
/**
 * Return the TextPrivateOnFederalLand property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextPrivateOnFederalLand() {
	if (ivjTextPrivateOnFederalLand == null) {
		try {
			ivjTextPrivateOnFederalLand = new javax.swing.JTextField();
			ivjTextPrivateOnFederalLand.setName("TextPrivateOnFederalLand");
			ivjTextPrivateOnFederalLand.setBounds(158, 222, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextPrivateOnFederalLand;
}
/**
 * Return the TextPurposes property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextPurposes() {
	if (ivjTextPurposes == null) {
		try {
			ivjTextPurposes = new javax.swing.JTextField();
			ivjTextPurposes.setName("TextPurposes");
			ivjTextPurposes.setBounds(158, 154, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextPurposes;
}
/**
 * Return the TextReturnFlowRegion property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextReturnFlowRegion() {
	if (ivjTextReturnFlowRegion == null) {
		try {
			ivjTextReturnFlowRegion = new javax.swing.JTextField();
			ivjTextReturnFlowRegion.setName("TextReturnFlowRegion");
			ivjTextReturnFlowRegion.setBounds(213, 264, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextReturnFlowRegion;
}
/**
 * Return the TextRFC property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextRFC() {
	if (ivjTextRFC == null) {
		try {
			ivjTextRFC = new javax.swing.JTextField();
			ivjTextRFC.setName("TextRFC");
			ivjTextRFC.setBounds(213, 402, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextRFC;
}
/**
 * Return the TextRiver property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextRiver() {
	if (ivjTextRiver == null) {
		try {
			ivjTextRiver = new javax.swing.JTextField();
			ivjTextRiver.setName("TextRiver");
			ivjTextRiver.setBounds(213, 126, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextRiver;
}
/**
 * Return the TextSectionTR property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextSectionTR() {
	if (ivjTextSectionTR == null) {
		try {
			ivjTextSectionTR = new javax.swing.JTextField();
			ivjTextSectionTR.setName("TextSectionTR");
			ivjTextSectionTR.setBounds(476, 358, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextSectionTR;
}
/**
 * Return the TextSourceAgency property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextSourceAgency() {
	if (ivjTextSourceAgency == null) {
		try {
			ivjTextSourceAgency = new javax.swing.JTextField();
			ivjTextSourceAgency.setName("TextSourceAgency");
			ivjTextSourceAgency.setBounds(476, 324, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextSourceAgency;
}
/**
 * Return the TextSpillwayType property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextSpillwayType() {
	if (ivjTextSpillwayType == null) {
		try {
			ivjTextSpillwayType = new javax.swing.JTextField();
			ivjTextSpillwayType.setName("TextSpillwayType");
			ivjTextSpillwayType.setText("");
			ivjTextSpillwayType.setBounds(482, 185, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextSpillwayType;
}
/**
 * Return the TextSpillwayWidth property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextSpillwayWidth() {
	if (ivjTextSpillwayWidth == null) {
		try {
			ivjTextSpillwayWidth = new javax.swing.JTextField();
			ivjTextSpillwayWidth.setName("TextSpillwayWidth");
			ivjTextSpillwayWidth.setText("");
			ivjTextSpillwayWidth.setBounds(482, 226, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextSpillwayWidth;
}
/**
 * Return the TextStateID property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextStateID() {
	if (ivjTextStateID == null) {
		try {
			ivjTextStateID = new javax.swing.JTextField();
			ivjTextStateID.setName("TextStateID");
			ivjTextStateID.setBounds(213, 103, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextStateID;
}
/**
 * Return the TextStateRegulated property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextStateRegulated() {
	if (ivjTextStateRegulated == null) {
		try {
			ivjTextStateRegulated = new javax.swing.JTextField();
			ivjTextStateRegulated.setName("TextStateRegulated");
			ivjTextStateRegulated.setBounds(158, 392, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextStateRegulated;
}
/**
 * Return the TextStateRegulatoryAgency property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextStateRegulatoryAgency() {
	if (ivjTextStateRegulatoryAgency == null) {
		try {
			ivjTextStateRegulatoryAgency = new javax.swing.JTextField();
			ivjTextStateRegulatoryAgency.setName("TextStateRegulatoryAgency");
			ivjTextStateRegulatoryAgency.setBounds(476, 18, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextStateRegulatoryAgency;
}
/**
 * Return the TextStructuralHeight property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextStructuralHeight() {
	if (ivjTextStructuralHeight == null) {
		try {
			ivjTextStructuralHeight = new javax.swing.JTextField();
			ivjTextStructuralHeight.setName("TextStructuralHeight");
			ivjTextStructuralHeight.setText("");
			ivjTextStructuralHeight.setBounds(162, 103, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextStructuralHeight;
}
/**
 * Return the TextSurfaceArea property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextSurfaceArea() {
	if (ivjTextSurfaceArea == null) {
		try {
			ivjTextSurfaceArea = new javax.swing.JTextField();
			ivjTextSurfaceArea.setName("TextSurfaceArea");
			ivjTextSurfaceArea.setText("");
			ivjTextSurfaceArea.setBounds(162, 390, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextSurfaceArea;
}
/**
 * Return the TextTopoMap property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextTopoMap() {
	if (ivjTextTopoMap == null) {
		try {
			ivjTextTopoMap = new javax.swing.JTextField();
			ivjTextTopoMap.setName("TextTopoMap");
			ivjTextTopoMap.setBounds(213, 241, 408, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextTopoMap;
}
/**
 * Return the TextUpdated property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextUpdated() {
	if (ivjTextUpdated == null) {
		try {
			ivjTextUpdated = new javax.swing.JTextField();
			ivjTextUpdated.setName("TextUpdated");
			ivjTextUpdated.setBounds(213, 356, 408, 20);
			ivjTextUpdated.setEnabled(false);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextUpdated;
}
/**
 * Return the TextVolumeMaterial property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextVolumeMaterial() {
	if (ivjTextVolumeMaterial == null) {
		try {
			ivjTextVolumeMaterial = new javax.swing.JTextField();
			ivjTextVolumeMaterial.setName("TextVolumeMaterial");
			ivjTextVolumeMaterial.setText("");
			ivjTextVolumeMaterial.setBounds(482, 62, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextVolumeMaterial;
}
/**
 * Return the TextWidthOfLocks property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextWidthOfLocks() {
	if (ivjTextWidthOfLocks == null) {
		try {
			ivjTextWidthOfLocks = new javax.swing.JTextField();
			ivjTextWidthOfLocks.setName("TextWidthOfLocks");
			ivjTextWidthOfLocks.setText("");
			ivjTextWidthOfLocks.setBounds(482, 390, 124, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextWidthOfLocks;
}
/**
 * Return the TextYearCompleted property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextYearCompleted() {
	if (ivjTextYearCompleted == null) {
		try {
			ivjTextYearCompleted = new javax.swing.JTextField();
			ivjTextYearCompleted.setName("TextYearCompleted");
			ivjTextYearCompleted.setBounds(158, 86, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextYearCompleted;
}
/**
 * Return the TextYearModified property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getTextYearModified() {
	if (ivjTextYearModified == null) {
		try {
			ivjTextYearModified = new javax.swing.JTextField();
			ivjTextYearModified.setName("TextYearModified");
			ivjTextYearModified.setBounds(158, 120, 123, 20);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextYearModified;
}
/**
 * Called when "Close" is clicked
 */
private void handleClose() {

	// if the data has been edited, ask the user if they would like to save before closing
	if (bUpdateMaster || bUpdateFeatures || bUpdateDown || bUpdateIn || bUpdateOut || bUpdatePair) {
		int confirmOption;
		confirmOption = JOptionPane.showConfirmDialog(this, "Do you want to save the changes?", "Confirm", JOptionPane.YES_NO_CANCEL_OPTION);
		if (confirmOption == JOptionPane.YES_OPTION)
			handleSave();
		else if (confirmOption == JOptionPane.CANCEL_OPTION)
			return;
	}
	
	dispose();
	return;
}
/**
 * Insert the method's description here.
 * Creation date: (2/5/2004 6:03:38 PM)
 */
public void handleDeleteDamcatDam() 
{
	int confirmOption;
	String[] buttons = {"OK", "Cancel"};
	String damID = damInfo.getNidid().trim();
	String damName = damInfo.getDam_name();
	int count = 0;  //to count rows of the same scenario and source as selected for deleting - output table
	
	//String xsecType = pairEntry.getXsec_type(); 
	//int pairNum = pairEntry.getPair_num();
    try{
	switch (_indexTab) {
		case 0:	
			
			//confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete the dam from database?", "Confirm Delete", JOptionPane.WARNING_MESSAGE,0,null,buttons,buttons[1]);
			confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete the dam\n" + damID + " from the database?", 
				     "Confirm Delete", JOptionPane.OK_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,null,buttons, buttons[1]);
			
			if (confirmOption == 0)
			{
				JOptionPane.showMessageDialog(null,"The dam " + damID + " was deleted");
				dbAccess.deleteSelectedDam(damID);
				
				//System.out.println("Selection in editDam: " + get_searchSelection());
				getSearchScreen().clearDeletedSearch(get_searchSelection());		
				
				dispose();
				
			}
			else if (confirmOption == 1)
				return;
			break;
		case 1:
			break;
		case 2:
			break;	
		case 3:
				
			//confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete selected downstream point\n from the database?", "Confirm Delete", JOptionPane.WARNING_MESSAGE,0,null,buttons,buttons[1]);
			confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete selected downstream point(s)\n from the database?", "Confirm Delete", JOptionPane.OK_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,
    		null,buttons, buttons[1]);
			if (confirmOption == 0)
			{
				// changes  made on 11/29/2004
				
				/* int selectedDown = getTableDownstream().getSelectedRow();
				int rowCount = getTableDownstream().getRowCount();
				
				if (selectedDown != -1)
				{
					JOptionPane.showMessageDialog(null,"Selected downstream point was deleted.");
					String downName = (String)getTableDownstream().getModel().getValueAt(selectedDown,0);
					dbAccess.deleteSelectedPoints(damID,downName);
					dispose(); 	
				} */

				int [] selectedDown;
				selectedDown = getTableDownstream().getSelectedRows();
				
				if (selectedDown.length > 0) 
     			{
        			if (selectedDown.length == 1)
        			{
	     				JOptionPane.showMessageDialog(null,"Selected downstream point was deleted");
        			}
        			else
        			{
	        			JOptionPane.showMessageDialog(null,"Selected downstream points were deleted");
        			}
	     			for (int i= 0; i < selectedDown.length; i++) 
        			{
          				// get Downstream table data
          				String downName = (String)getTableDownstream().getModel().getValueAt(selectedDown[i],0);
						dbAccess.deleteSelectedPoints(damID,downName);
         		 	}
        			dispose();
     			}
     			
				else
					JOptionPane.showMessageDialog(null,"No rows was selected to delete.");
			}
			else if (confirmOption == 1)
				return;
			break;
		case 4:
			
			//confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete selected cross-section\n from the database?", "Confirm Delete", JOptionPane.WARNING_MESSAGE,0,null,buttons,buttons[1]);
			confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete selected cross-section(s)\n from the database?", "Confirm Delete", JOptionPane.OK_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,
    		null,buttons, buttons[1]);
			if (confirmOption == 0)
			{
				/*int selectedCross = getTableCrossSection().getSelectedRow();
				int rowCount = getTableCrossSection().getRowCount();
				if (selectedCross != -1)
				{
					JOptionPane.showMessageDialog(null,"Selected cross-section will be deleted");
					String downName = (String)getTableCrossSection().getModel().getValueAt(selectedCross,0);
					String xsType = (String)getTableCrossSection().getModel().getValueAt(selectedCross,1);
					Integer pairInteger = (Integer)getTableCrossSection().getModel().getValueAt(selectedCross,2);
					int pairNum = pairInteger.intValue(); 
					// int pairNum = Integer.parseInt(pairString.trim()); 
					dbAccess.deleteSelectedPairs(damID,downName, xsType,pairNum);
					dispose();
				}*/

				int [] selectedCross;
				selectedCross = getTableCrossSection().getSelectedRows();

     			if (selectedCross.length > 0) 
     			{
        			if (selectedCross.length == 1)
        			{
	     				JOptionPane.showMessageDialog(null,"Selected cross-section was deleted");
        			}
        			else
        			{
	        			JOptionPane.showMessageDialog(null,"Selected cross-sections were deleted");
        			}
	     			for (int i= 0; i < selectedCross.length; i++) 
        			{
          				// get Cross-Section table data
          				String downName = (String)getTableCrossSection().getModel().getValueAt(selectedCross[i],0);
						String xsType = (String)getTableCrossSection().getModel().getValueAt(selectedCross[i],1);
						Integer pairInteger = (Integer)getTableCrossSection().getModel().getValueAt(selectedCross[i],2);
						int pairNum = pairInteger.intValue();
						dbAccess.deleteSelectedPairs(damID,downName, xsType,pairNum);
         		 	}
        			dispose();
        		}

				else
					JOptionPane.showMessageDialog(null,"No rows was selected to delete.");
					
			}
			else if (confirmOption == 1)
				return;
			break;
		case 5:
			
			//confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete the input table record\n from the database?", "Confirm Delete", JOptionPane.WARNING_MESSAGE,0,null,buttons,buttons[1]);
			confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete the input table record\n from the database?", "Confirm Delete", JOptionPane.OK_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,
    		null,buttons, buttons[1]);
			if (confirmOption == 0)
			{
				int selectedIn = getTableInput().getSelectedRow();
				if (selectedIn != -1)
				{
					JOptionPane.showMessageDialog(null,"The input table record was deleted");
					String src = (String)getTableInput().getModel().getValueAt(selectedIn,0);
					String scenario = (String)getTableInput().getModel().getValueAt(selectedIn,1); 
					dbAccess.deleteSelectedInputs(damID,src,scenario);
					dispose();
				}
				else
					JOptionPane.showMessageDialog(null,"No rows was selected to delete.");
					
			}
			else if (confirmOption == 1)
				return;
			break;
		case 6:
	
			//confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete the output table record from database?", "Confirm Delete", JOptionPane.WARNING_MESSAGE,0,null,buttons,buttons[1]);
			confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete the output table record\n from the database?", "Confirm Delete", JOptionPane.OK_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,
    		null,buttons, buttons[1]);
			if (confirmOption == 0)
			{
				int selectedOut = getTableOutput().getSelectedRow();
				if (selectedOut != -1)
				{
					JOptionPane.showMessageDialog(null,"The dam output table record was deleted");
					String src = (String)getTableOutput().getModel().getValueAt(selectedOut,0);
					String scenario = (String)getTableOutput().getModel().getValueAt(selectedOut,1);
					String downName = (String)getTableOutput().getModel().getValueAt(selectedOut,2);

					
					dbAccess.deleteSelectedOutputs(damID,src,scenario,downName);
					
					if(src.startsWith("#"))
					{
						// System.out.println("In EditDam.handleDeleteDamcatDam, src starts with #");
					
						for (int i = 0; i < getTableOutput().getRowCount(); i++)
						{
							String srcOut = (String)getTableOutput().getModel().getValueAt(i,0);
							String scenarioOut = (String)getTableOutput().getModel().getValueAt(i,1);
				
							if(src.equals(srcOut) && scenario.equals(scenarioOut))
							{
								// System.out.println("Equal: " + src + " = " + srcOut + " " + scenario + " = " +scenarioOut);
								count++;
							}
						}
						if(count < 2)
						{
							// System.out.println("The row in input table was deleted");
							dbAccess.deleteSelectedInputs(damID,src,scenario);
						}
					}
					dispose();
				}
				else
					JOptionPane.showMessageDialog(null,"No rows was selected to delete.");
			}
			else if (confirmOption == 1)
				return;
			break;
		default:
			System.out.println("indexTab is not set correctly");
	}
    } catch (Exception e){
	    
	    
    }
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	// System.out.println("--------- UNCAUGHT EXCEPTION ---------");
	// exception.printStackTrace(System.out);
}
/**
 * Called when "Export to SMPDBK" is clicked
 */
private void handleExportToSMPDBK() {

	gov.dambreak.smpdbk.ModelGUI smpdbkInputEditor = new gov.dambreak.smpdbk.ModelGUI(true,damInfo.nidid,damInfo.getAnalysisData(),dbAccess);

    return;
}
/**
 * Called when the user clicks "New Dam"
 */

private void handleNewDamDownKeys()
{
	int i = get_indexTab();
	
	saveUpdatesOnInsert();   // call was added on 11/30/2004
	
	if(i == 3)
	
		if(insertDownKeys == null)
		{
			insertDownKeys = new NewDamDownKeys();
		}
		else
		{
			insertDownKeys.setVisible(true);
		}
		
	return;
	
}
private void handleNewDamInKeys()
{
	int i = get_indexTab();

	saveUpdatesOnInsert();    // call was added on 11/30/2004
	
	if(i == 5)
	{
		if(insertInKeys == null)
		{
			insertInKeys = new NewDamInKeys();
		}
		else
		{
			insertInKeys.setVisible(true);
		}
	}
		
	return;
	
}
private void handleNewDamOutKeys()
{
	int i = get_indexTab();

	saveUpdatesOnInsert();    // call was added on 11/30/2004
	
	if(i == 6)
	{
		if(insertOutKeys == null)
		{
			insertOutKeys = new NewDamOutKeys();
		}
		else
		{
			insertOutKeys.setVisible(true);
		}
	}
	return;
	
}
private void handleNewDamPairKeys()
{
	int i = get_indexTab();

	saveUpdatesOnInsert();     // call was added on 11/30/2004
	
	if(i == 4)
		if(insertPairKeys == null)
		{
			insertPairKeys = new NewDamPairKeys();
		}
		else
		{
			insertPairKeys.setVisible(true);
		}

	return;
	
}
/**
 * Called when the user clicks "Save"
 */
private boolean handleSave() {
	boolean _tableChanged = false;
	
	if (!saveGUI())
		return false;

	if (bUpdateMaster)
	{
		dbAccess.updateDamMaster(damInfo); // update the DamMaster table with data from the gui
	}

	if (bUpdateFeatures)
	{
		dbAccess.updateDamFeatures(damInfo); // update the DamFeatures table with data from the gui
	}
	if (bUpdateDown)
	{
		dbAccess.updateDownstream(damInfo, _tableDownRowChangedArray); // update the Downstream table with data from the gui
		clearRowChanged(_tableDownRowChangedArray);
	}	
	if (bUpdateIn)
	{
		dbAccess.updateSdbIn(damInfo, _tableInRowChangedArray); // update the SdbIn table with data from the gui
		clearRowChanged(_tableInRowChangedArray);
	}
	if (bUpdateOut)
	{
		dbAccess.updateSdbOut(damInfo, _tableOutRowChangedArray); // update the SdbOut table with data from the gui
		clearRowChanged(_tableOutRowChangedArray);
	}
	if (bUpdatePair)
	{
		dbAccess.updateSectionPair(damInfo, _tablePairRowChangedArray); // update the SectionPair table with data from the gui
		clearRowChanged(_tablePairRowChangedArray);
	}

	if ((bUpdateMaster) || (bUpdateFeatures))
		getButtonNewDam().setEnabled(false);
	else
		getButtonNewDam().setEnabled(true);
		
	bUpdateMaster = bUpdateFeatures = bUpdateDown = bUpdateIn = bUpdateOut = bUpdatePair = false;
	
	getButtonSave().setEnabled(false);
	
	return true;
}
/**
 * Initializes connections
 * @exception java.lang.Exception The exception description.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	getButtonExport().addActionListener(ivjEventHandler);
	getButtonSave().addActionListener(ivjEventHandler);
	getButtonClose().addActionListener(ivjEventHandler);
	getButtonNewDam().addActionListener(ivjEventHandler);
	getButtonDeleteDam().addActionListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("DAMCAT_EditDam");
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		setResizable(true);
		setSize(724, 553);
		setTitle("DAMCAT - Dam View");
		setContentPane(getJFrameContentPane());
		initConnections();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	setTitle(getTitle()+" - "+damInfo.nidid);
	initializeDBFieldRelations();
	fillGUI();
	bUpdateMaster = bUpdateFeatures = bUpdateDown = bUpdateIn = bUpdateOut = bUpdatePair = false;

	/**
	 * Add listeners to `JTextField`s
	 */
	for (int i=0; i<getPanelGeneral().getComponentCount(); i++) {
		if (getPanelGeneral().getComponent(i).getClass() == JTextField.class)
			((JTextField)(getPanelGeneral().getComponent(i))).getDocument().addDocumentListener(this);
	}
	for (int i=0; i<getPanelConstruction().getComponentCount(); i++) {
		if (getPanelConstruction().getComponent(i).getClass() == JTextField.class)
			((JTextField)(getPanelConstruction().getComponent(i))).getDocument().addDocumentListener(this);
	}
	for (int i=0; i<getPanelRegulatory().getComponentCount(); i++) {
		if (getPanelRegulatory().getComponent(i).getClass() == JTextField.class)
			((JTextField)(getPanelRegulatory().getComponent(i))).getDocument().addDocumentListener(this);
	}

//	JPanel outputPanel = new JPanel(new BorderLayout());
//	outputPanel.add(new smpdbk.DisplayGraphicsPanel(damInfo.getSMPDBKOutput(0)), BorderLayout.CENTER);
//	getTabData().addTab("Output Viewer", new smpdbk.DisplayGraphicsPanel(damInfo.getSMPDBKOutput(0)));

	// user code end
}
/**
 * Insert the method's description here.
 * Creation date: (7/16/2003 3:20:18 PM)
 */
private void initializeDBFieldRelations() {
	dbFieldRelations = new HashMap();
	try {
		dbFieldRelations.put(ivjTextNIDID,DamInfo.class.getField("nidid"));
		dbFieldRelations.put(ivjTextDamName,DamInfo.class.getField("dam_name"));
		dbFieldRelations.put(ivjTextOtherDamName,DamInfo.class.getField("other_dam_name"));
		dbFieldRelations.put(ivjTextFormerDamName,DamInfo.class.getField("dam_former_name"));
		dbFieldRelations.put(ivjTextStateID,DamInfo.class.getField("stateid"));
		dbFieldRelations.put(ivjTextRiver,DamInfo.class.getField("river"));
		dbFieldRelations.put(ivjTextCounty,DamInfo.class.getField("county"));
		dbFieldRelations.put(ivjTextLatitude,DamInfo.class.getField("latitude_dam"));
		dbFieldRelations.put(ivjTextLongitude,DamInfo.class.getField("longitude_dam"));
	     dbFieldRelations.put(ivjTextElevation,DamInfo.class.getField("elev"));
		dbFieldRelations.put(ivjTextTopoMap,DamInfo.class.getField("topo_map"));
		dbFieldRelations.put(ivjTextReturnFlowRegion,DamInfo.class.getField("return_flow_region"));
		dbFieldRelations.put(ivjTextDrainageArea,DamInfo.class.getField("drainage_area"));
		dbFieldRelations.put(ivjTextPrebreakAvailable,DamInfo.class.getField("prebreak_avail"));
		dbFieldRelations.put(ivjTextComments,DamInfo.class.getField("comments"));
		dbFieldRelations.put(ivjTextUpdated,DamInfo.class.getField("updated"));
		dbFieldRelations.put(ivjTextHSA,DamInfo.class.getField("hsa"));
		dbFieldRelations.put(ivjTextRFC,DamInfo.class.getField("rfc"));

		dbFieldRelations.put(ivjTextDamType,DamInfo.class.getField("dam_type"));
		dbFieldRelations.put(ivjTextDamHeight,DamInfo.class.getField("dam_height"));
		dbFieldRelations.put(ivjTextStructuralHeight,DamInfo.class.getField("structural_height"));
		dbFieldRelations.put(ivjTextHydraulicHeight,DamInfo.class.getField("hydraulic_height"));
		dbFieldRelations.put(ivjTextNIDHeight,DamInfo.class.getField("nid_height"));
		dbFieldRelations.put(ivjTextNIDStorage,DamInfo.class.getField("nid_storage"));
		dbFieldRelations.put(ivjTextMaxStorage,DamInfo.class.getField("max_storage"));
		dbFieldRelations.put(ivjTextNormalStorage,DamInfo.class.getField("normal_storage"));
		dbFieldRelations.put(ivjTextDamLength,DamInfo.class.getField("dam_length"));
		dbFieldRelations.put(ivjTextSurfaceArea,DamInfo.class.getField("surface_area"));
		dbFieldRelations.put(ivjTextMaxDischarge,DamInfo.class.getField("max_discharge"));
		dbFieldRelations.put(ivjTextVolumeMaterial,DamInfo.class.getField("volume_dam"));
		dbFieldRelations.put(ivjTextCore,DamInfo.class.getField("core"));
		dbFieldRelations.put(ivjTextFoundation,DamInfo.class.getField("foundation"));
		dbFieldRelations.put(ivjTextSpillwayType,DamInfo.class.getField("spillway_type"));
		dbFieldRelations.put(ivjTextSpillwayWidth,DamInfo.class.getField("spillway_width"));
		dbFieldRelations.put(ivjTextOutletGates,DamInfo.class.getField("outlet_gates"));
		dbFieldRelations.put(ivjTextNumberOfLocks,DamInfo.class.getField("number_locks"));
		dbFieldRelations.put(ivjTextLengthOfLocks,DamInfo.class.getField("length_locks"));
		dbFieldRelations.put(ivjTextWidthOfLocks,DamInfo.class.getField("width_locks"));

		dbFieldRelations.put(ivjTextOwnerName,DamInfo.class.getField("owner_name"));
		dbFieldRelations.put(ivjTextOwnerType,DamInfo.class.getField("owner_type"));
		dbFieldRelations.put(ivjTextYearCompleted,DamInfo.class.getField("year_completed"));
		dbFieldRelations.put(ivjTextYearModified,DamInfo.class.getField("year_modified"));
		dbFieldRelations.put(ivjTextPurposes,DamInfo.class.getField("purposes"));
		dbFieldRelations.put(ivjTextDamDesigner,DamInfo.class.getField("dam_designer"));
		dbFieldRelations.put(ivjTextPrivateOnFederalLand,DamInfo.class.getField("private_on_federal"));
		dbFieldRelations.put(ivjTextDownstreamHazard,DamInfo.class.getField("downstream_hazard"));
		dbFieldRelations.put(ivjTextEmergencyAction,DamInfo.class.getField("emerg_action_plan"));
		dbFieldRelations.put(ivjTextInspectionDate,DamInfo.class.getField("inspection_date"));
		dbFieldRelations.put(ivjTextInspectionFrequency,DamInfo.class.getField("inspection_freq"));
		dbFieldRelations.put(ivjTextStateRegulated,DamInfo.class.getField("st_reg_dam"));
		dbFieldRelations.put(ivjTextStateRegulatoryAgency,DamInfo.class.getField("st_reg_agency"));
		dbFieldRelations.put(ivjTextFederalFunding,DamInfo.class.getField("fed_funding"));
		dbFieldRelations.put(ivjTextFederalDesign,DamInfo.class.getField("fed_design"));
		dbFieldRelations.put(ivjTextFederalConstruction,DamInfo.class.getField("fed_construction"));
		dbFieldRelations.put(ivjTextFederalRegulatoryAgency,DamInfo.class.getField("fed_regulatory"));
		dbFieldRelations.put(ivjTextFederalInspection,DamInfo.class.getField("fed_inspection"));
		dbFieldRelations.put(ivjTextFederalOperation,DamInfo.class.getField("fed_operation"));
		dbFieldRelations.put(ivjTextFederalOwner,DamInfo.class.getField("fed_owner"));
		dbFieldRelations.put(ivjTextFederalOther,DamInfo.class.getField("fed_other"));
		dbFieldRelations.put(ivjTextSourceAgency,DamInfo.class.getField("source_agency"));
		dbFieldRelations.put(ivjTextSectionTR,DamInfo.class.getField("section_t_r"));
	} catch (NoSuchFieldException e) {
		JOptionPane.showMessageDialog(this,"Error: Could not associate database fields with GUI elements.");
	}
}
public void insertUpdate(DocumentEvent e) {
	bUpdateMaster = true;
	bUpdateFeatures = true;
	
	getButtonSave().setEnabled(true);
	getButtonNewDam().setEnabled(false);
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	
	try {
		EditDam aEditDam;
		aEditDam = new EditDam();
		aEditDam.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
//		aEditDam.show();
        aEditDam.setVisible( true );
		java.awt.Insets insets = aEditDam.getInsets();
		aEditDam.setSize(aEditDam.getWidth() + insets.left + insets.right, aEditDam.getHeight() + insets.top + insets.bottom);
		aEditDam.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JFrame");
		exception.printStackTrace(System.out);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (8/3/2004 2:05:05 PM)
 */
public void refresh() {
	
	//setTitle(getTitle()+" - "+damInfo.nidid);
	//initializeDBFieldRelations();
	fillGUI();
	bUpdateMaster = bUpdateFeatures = bUpdateDown = bUpdateIn = bUpdateOut = bUpdatePair = false;
	
}
/**
 * Insert the method's description here.
 * Creation date: (7/18/2003 8:36:57 AM)
 * @param e javax.swing.event.DocumentEvent
 */
public void removeUpdate(DocumentEvent e) {
	bUpdateMaster = true;
	bUpdateFeatures = true;
	getButtonSave().setEnabled(true);
}
/**
 * Insert the method's description here.
 * Creation date: (7/21/2003 7:52:13 AM)
 */
private boolean saveGUI() {
	Iterator it = dbFieldRelations.entrySet().iterator();

	try {
		if ((bUpdateMaster) || (bUpdateFeatures)) {
			while (it.hasNext()) {
				Map.Entry entry = (Map.Entry)it.next();
				JTextField guiField = (JTextField)(entry.getKey());
				Field field = (Field)entry.getValue();
				
				if (field.getType().equals(String.class))
					field.set(damInfo, guiField.getText());
				else if (field.getType().equals(Float.class))
				{
     				String textValue = guiField.getText();
     				if (textValue == null)
     				{
      					field.set(damInfo, new Float(0.0f));
     				} 
     				else
     				{
						field.set(damInfo, Float.valueOf(guiField.getText()));
     				}
				}
				else if (field.getType().equals(Integer.class))
				{
     				String textValue = guiField.getText();
     				if (textValue == null)
     				{
      					field.set(damInfo, new Integer(0));
     				} 
     				else
     				{
						field.set(damInfo, Integer.valueOf(guiField.getText()));
     				}
				}
	//			else if (field.getType().equals(java.sql.Date.class))
	//				damFields[i].set(ret,rs.getDate(damFields[i].getName()));
			}
		}

		if (bUpdatePair) {	
			// Save cross section table
			for (int i=0; i<damInfo._crossSectionEntries.size(); i++) {
				CrossSectionEntryInfo xs = (CrossSectionEntryInfo)damInfo._crossSectionEntries.get(i);
				xs.elev = DamInfo.safeParseFloat((String)modelCrossSections.getValueAt(i,3));
				xs.tw = DamInfo.safeParseFloat((String)modelCrossSections.getValueAt(i,4));
				xs.inactive_width = DamInfo.safeParseFloat((String)modelCrossSections.getValueAt(i,5));
				xs.mann_n = DamInfo.safeParseFloat((String)modelCrossSections.getValueAt(i,6));
			}
		}

		if (bUpdateIn) {
			// Save input table
			for (int i=0; i<damInfo._inputEntries.size(); i++) {
				InputEntryInfo in = (InputEntryInfo)damInfo._inputEntries.get(i);
				in.hde = DamInfo.safeParseFloat((String)modelInput.getValueAt(i,2));
				in.bme = DamInfo.safeParseFloat((String)modelInput.getValueAt(i,3));
				in.vol = DamInfo.safeParseFloat((String)modelInput.getValueAt(i,4));
				in.sa = DamInfo.safeParseFloat((String)modelInput.getValueAt(i,5));
				in.tfm = DamInfo.safeParseFloat((String)modelInput.getValueAt(i,6));
				in.qo = DamInfo.safeParseFloat((String)modelInput.getValueAt(i,7));
				in.bw = DamInfo.safeParseFloat((String)modelInput.getValueAt(i,8));
				in.idam = DamInfo.safeParseInteger((String)modelInput.getValueAt(i,9));
				in.comments = (String)modelInput.getValueAt(i,10);
				in.updated = (String)modelInput.getValueAt(i,11);
			}
		}

		if (bUpdateOut) {
			// Save output table
			for (int i=0; i<damInfo._outputEntries.size(); i++) {
				OutputEntryInfo out = (OutputEntryInfo)damInfo._outputEntries.get(i);
				out.slope = DamInfo.safeParseFloat((String)modelOutput.getValueAt(i,3));
				out.max_flow = DamInfo.safeParseFloat((String)modelOutput.getValueAt(i,4));
				out.max_depth = DamInfo.safeParseFloat((String)modelOutput.getValueAt(i,5));
				out.time_max_depth = DamInfo.safeParseFloat((String)modelOutput.getValueAt(i,6));
				out.time_flood = DamInfo.safeParseFloat((String)modelOutput.getValueAt(i,7));
				out.time_deflood = DamInfo.safeParseFloat((String)modelOutput.getValueAt(i,8));
				out.comments = (String)modelOutput.getValueAt(i,9);
				// field "updated" is not saved
			}
		}

		if (bUpdateDown) {
			// Save downstream table
			for (int i=0; i<damInfo._downstreamEntries.size(); i++) {
				DownstreamEntryInfo down = (DownstreamEntryInfo)damInfo._downstreamEntries.get(i);
				
				//down.xsec_best_type = ((String)modelDownstream.getValueAt(i,1));
				down.xsec_best_type = modelDownstream.getValueAt(i,1).toString().toUpperCase();
				modelDownstream.setValueAt(down.xsec_best_type,i,1); 
				
				down.distance_from_dam = DamInfo.safeParseFloat((String)modelDownstream.getValueAt(i,2));
				down.latitude = DamInfo.safeParseFloat((String)modelDownstream.getValueAt(i,3));
				down.longitude = DamInfo.safeParseFloat((String)modelDownstream.getValueAt(i,4));
				down.elevation = DamInfo.safeParseFloat((String)modelDownstream.getValueAt(i,5));
				down.flood_flow = DamInfo.safeParseFloat((String)modelDownstream.getValueAt(i,6));
				down.flood_depth = DamInfo.safeParseFloat((String)modelDownstream.getValueAt(i,7));
				down.flood_width = DamInfo.safeParseFloat((String)modelDownstream.getValueAt(i,8));
				down.mann_oc = DamInfo.safeParseFloat((String)modelDownstream.getValueAt(i,9));
				down.comments = (String)modelDownstream.getValueAt(i,10);
				// updated is not saved
			}
		}

	} catch (Throwable e) {
		JOptionPane.showMessageDialog(null,"Error: Could not save data in GUI.");
		e.printStackTrace();
		return false;
	}
	return true;
}
/**
 * Called when the user clicks "Save" when previous tabs were updated.
 * Creation date: (11/30/2004 12:51:01 PM)
 */
public boolean saveUpdatesOnInsert() {

	boolean _tableChanged = false;
	
	if (!saveGUI())
		return false;

	if (bUpdateMaster)
	{
		dbAccess.updateDamMaster(damInfo); // update the DamMaster table with data from the gui
	}

	if (bUpdateFeatures)
	{
		dbAccess.updateDamFeatures(damInfo); // update the DamFeatures table with data from the gui
	}
	if (bUpdateDown)
	{
		dbAccess.updateDownstream(damInfo, _tableDownRowChangedArray); // update the Downstream table with data from the gui
		clearRowChanged(_tableDownRowChangedArray);
	}	
	if (bUpdateIn)
	{
		dbAccess.updateSdbIn(damInfo, _tableInRowChangedArray); // update the SdbIn table with data from the gui
		clearRowChanged(_tableInRowChangedArray);
	}
	if (bUpdateOut)
	{
		dbAccess.updateSdbOut(damInfo, _tableOutRowChangedArray); // update the SdbOut table with data from the gui
		clearRowChanged(_tableOutRowChangedArray);
	}
	if (bUpdatePair)
	{
		dbAccess.updateSectionPair(damInfo, _tablePairRowChangedArray); // update the SectionPair table with data from the gui
		clearRowChanged(_tablePairRowChangedArray);
	}

	bUpdateMaster = bUpdateFeatures = bUpdateDown = bUpdateIn = bUpdateOut = bUpdatePair = false;
	
	return true;
}
/**
 * Insert the method's description here.
 * Creation date: (1/22/2004 5:20:45 PM)
 * @param new_indexTab int
 */
public void set_indexTab(int new_indexTab) {
	_indexTab = new_indexTab;
}
/**
 * Insert the method's description here.
 * Creation date: (2/18/2004 6:12:30 PM)
 * @param new_searchSelection int
 */
public void set_searchSelection(int new_searchSelection) {
	_searchSelection = new_searchSelection;
}
/**
 * Insert the method's description here.
 * Creation date: (2/18/2004 3:18:23 PM)
 * @param newSearchScreen gov.damcat.data.Search
 */
public void setSearchScreen(Search newSearchScreen) {
	searchScreen = newSearchScreen;
}
/**
 * Insert the method's description here.
 * Creation date: (7/21/2003 9:19:11 AM)
 */
public void tableChanged(TableModelEvent e) {
	
	String header = "EditDam.tableChanged(): ";
	int size, firstRow, lastRow;
   
	if (e.getSource() == getTableInput().getModel())
	{

        /*size = damInfo._inputEntries.size();
        
        _tableRowChangedArray = new boolean[size];
        
        for (int i = 0; i < size; i++)
        {
            _tableRowChangedArray[i] = false;
        
        }*/
		firstRow = e.getFirstRow();
		lastRow = e.getLastRow();
        
		/* System.out.println(header + "firstRow = " + firstRow + 
                                   " lastRow =" + lastRow);*/
        
		for(int i = firstRow; i <= lastRow; i++)	
		{
			_tableInRowChangedArray[i] = true;
		}
		
		bUpdateIn = true;
	}
	else if (e.getSource() == getTableDownstream().getModel())
		//bUpdateDown = true;
	{

       /* size = damInfo._downstreamEntries.size();
        _tableRowChangedArray = new boolean[size];
        
        for (int i = 0; i < size; i++)
        {
            _tableRowChangedArray[i] = false;
        }*/
		
		firstRow = e.getFirstRow();
		lastRow = e.getLastRow();

		/* System.out.println(header + "firstRow = " + firstRow + 
                                   " lastRow =" + lastRow);*/
		
		for(int i = firstRow; i <= lastRow; i++)	
		{
			_tableDownRowChangedArray[i] = true;
		}	
		bUpdateDown = true;
	}
	else if (e.getSource() == getTableOutput().getModel())
	{

       /* size = damInfo._outputEntries.size();			// is this size correct ????  
        _tableRowChangedArray = new boolean[size];
        
        for (int i = 0; i < size; i++)
        {
            _tableRowChangedArray[i] = false;
        }*/
		
		firstRow = e.getFirstRow();
		lastRow = e.getLastRow();

		System.out.println(header + "firstRow = " + firstRow + 
                                   " lastRow =" + lastRow);
		
		for(int i = firstRow; i <= lastRow; i++)	
		{
			_tableOutRowChangedArray[i] = true;
		}	
		bUpdateOut = true;
	}
	
	else if (e.getSource()  == getTableCrossSection().getModel())
	{

       /* size = damInfo._crossSectionEntries.size(); 
        _tableRowChangedArray = new boolean[size];
        
        for (int i = 0; i < size; i++)
        {
            _tableRowChangedArray[i] = false;
        }*/
		
		firstRow = e.getFirstRow();
		lastRow = e.getLastRow();

		System.out.println(header + "firstRow = " + firstRow + 
                                   " lastRow =" + lastRow);
		for(int i = firstRow; i <= lastRow; i++)	
		{
			_tablePairRowChangedArray[i] = true;
		}	
		bUpdatePair = true;
	}
	
	getButtonSave().setEnabled(true);
}
 /** 
   * Called whenever the value of the selection changes.
   * @param e the event that characterizes the change.
   */
public void valueChanged(ListSelectionEvent e) {}
}
