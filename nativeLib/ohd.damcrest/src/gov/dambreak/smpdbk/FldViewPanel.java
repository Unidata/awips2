package gov.dambreak.smpdbk;

import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import gov.dambreak.util.*;

/**
 * This panel is used as a tab in the output viewer.  It allows the user to
 * choose a range of cross sections to output to FLDVIEW files (.xy, .fcs). It
 * also contains the code to produce the files.
 */ 
class FldViewPanel extends JPanel implements ActionListener, ListCellRenderer {
	
	private ModelOutput outputData;
	private JLabel xsLabel, xsLabel2; 
	private JList xsChooser;
	private JScrollPane xsChooserScroll;
	private JButton generateFilesButton;
	private String outputFilename;
	
	public FldViewPanel(ModelOutput _outputData) {
		super();
		outputData = _outputData;
		
		try {
			setName("FldViewPanel");
			setLayout(new BorderLayout());
			int width = getWidth(), height = getHeight();
			
			JPanel panelTop = new JPanel(new BorderLayout());
			xsLabel = new JLabel("  Select the cross sections to chart:");
			xsLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
			panelTop.add(xsLabel,BorderLayout.NORTH);
			
			xsLabel2 = new JLabel("  (hold down Ctrl/Shift to select multiple cross sections):");
			panelTop.add(xsLabel2,BorderLayout.SOUTH);
			
			add(panelTop, BorderLayout.NORTH);
			
			// create list box
			int nSections = outputData.inputDownstream.size();
			String[] items = new String[nSections];
			java.text.DecimalFormat df;
			df = new java.text.DecimalFormat("#########0.0000");
			for (int i=0; i<nSections; i++) {
				DownstreamPoint dp = (DownstreamPoint)outputData.inputDownstream.get(i);
				items[i] =  "Name: " + dp.name + 
							", Distance: " + df.format(dp.distanceToSection) + 
							", Latitude: " + df.format(dp.latitude) + 
							", Longitude: " + df.format(dp.longitude);
			}
			xsChooser = new JList(items);
			xsChooser.setSelectionInterval(0,items.length-1);
			xsChooser.setCellRenderer(this);
			xsChooserScroll = new JScrollPane(xsChooser);
			xsChooserScroll.setBorder(BorderFactory.createBevelBorder(1));
			add(xsChooserScroll, BorderLayout.CENTER);
			
			JPanel panelBottom = new JPanel(new FlowLayout());
			generateFilesButton = new JButton("<html><b>Generate FLDVIEW Files (.xy, .fcs)</b></html>");
			generateFilesButton.addActionListener(this);
			panelBottom.add(generateFilesButton);
			add(panelBottom, BorderLayout.SOUTH);
		} catch (Throwable e) {
			JOptionPane.showMessageDialog(this, "Exception caught: " + e.getMessage());
		}
	}
	public void actionPerformed(ActionEvent evt) {
		String osName = System.getProperty("os.name");
		String lowerCaseName = osName.toLowerCase();
		String tempPath = "";
		
		// get selected cross sections
		int[] selections = xsChooser.getSelectedIndices();
		if (selections.length == 0) {
			JOptionPane.showMessageDialog(this, "No cross sections selected.", "Error", JOptionPane.ERROR_MESSAGE);	
			return;
		}

		// verify that the endpoints have lat / lon coordinates

		double epsilon  = 0.001;
		int minSel = xsChooser.getMinSelectionIndex(),
			maxSel = xsChooser.getMaxSelectionIndex();
			
		/*			
		if (    (inputFile.sectionData[maxSel].getLatitude() < epsilon)
			 || (inputFile.sectionData[maxSel].getLongitude() < epsilon)
			 || (inputFile.sectionData[minSel].getLatitude() < epsilon)
			 || (inputFile.sectionData[minSel].getLongitude() < epsilon))
		*/
		DownstreamPoint maxPoint = (DownstreamPoint) outputData.inputDownstream.get(maxSel);
		DownstreamPoint minPoint = (DownstreamPoint) outputData.inputDownstream.get(minSel);		

		if (    (Math.abs(maxPoint.latitude) < epsilon)
			 || (Math.abs(maxPoint.longitude)  < epsilon)
			 || (Math.abs(minPoint.latitude) < epsilon)
			 || (Math.abs(minPoint.longitude) < epsilon))  
			 {
			 
			JOptionPane.showMessageDialog(this, "The cross sections at the endpoints of the selection range must have non-zero latitude and longitude.", "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}


		// write .xy and .fcs files
		float[] XSWidth = new float[50];

		int nSections = outputData.inputDownstream.size(), nPairs = AnalysisData.getNumPairs(outputData.inputDownstream);
		// System.out.println("Number of Sections = " + nSections + " Number of Pairs = " + nPairs);
		// calculate XSWidth
		for (int i=0; i<nSections; i++) {
			// System.out.println( "iiiiiiii = " + i); 
			DownstreamPoint dp = (DownstreamPoint)outputData.inputDownstream.get(i);
			int j=0;
			float target = outputData.maxElevation[i];
			
			while (j < nPairs && dp.getBestSection().getElevationData(j,0) < target  )
				j++;
			// System.out.println("Jay = " + j);
			if (j == 0)							// can not interpolate, use the first
				XSWidth[i] = dp.getBestSection().getElevationData(0,1);
			else if (j == nPairs)				// can not interpolate, use the last
				XSWidth[i] = dp.getBestSection().getElevationData(j-1,1);
			else {
				float x1 = dp.getBestSection().getElevationData(j-1,0);
				float x2 = dp.getBestSection().getElevationData(j,0);
				float y1 = dp.getBestSection().getElevationData(j-1,1);
				float y2 = dp.getBestSection().getElevationData(j,1);
				float diffx = x2-x1;
				float diffy = y2-y1;
				XSWidth[i] = (target - x1) * diffy / diffx + y1;
			}
		}

		// generate .xy file

		// TEMPPATH variable is renamed on the Linux side to DAMCREST_DATA_DIR
		// for better readability
		if(lowerCaseName.indexOf("windows") > -1)	
			tempPath = PropertyReader.getProperty("TEMPPATH");
		else if(lowerCaseName.indexOf("linux") > -1)
			tempPath = PropertyReader.getProperty("DAMCREST_DATA_DIR");
		
		if (!tempPath.endsWith(System.getProperty("file.separator")))
			tempPath = tempPath.concat(System.getProperty("file.separator"));
			

		
		//////////////////////////////////////////////////////////////////////
		// NOTE: The user should choose where the FLDVIEW files are exported
		////////////////////// ///////////////////////////////////////////////
		
		File fileName;
		
		String strLastOpenPath = tempPath;			// OK for starters

		JFileChooser fileChooser = new JFileChooser(strLastOpenPath);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.setDialogTitle("Select File Name For FLDVIEW .xy and .fcs Output");

		fileChooser.setFileFilter(new DamInputFileFilter(DamInputFileFilter.FLDVIEW_OUTPUT));
		int result = fileChooser.showSaveDialog(this);
		FileWriter output;
		
		// user clicked Cancel button on dialog
		if (result != JFileChooser.APPROVE_OPTION)
			return;
	
		fileName = fileChooser.getSelectedFile();
		if (fileName == null || fileName.getName().equals(""))
		{
			JOptionPane.showMessageDialog(this, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
			return;
		}

		// set the 'last file opened' path		// (currently not implemented)
		// strLastOpenPath = fileName.getPath();	

		String strPathName = fileName.getAbsolutePath();

		String s = strPathName;
		
		String strChosenRootName = strPathName;
		
    	int ix = s.lastIndexOf('.');
    	if (ix > 0 &&  ix < s.length() - 1) 
    	{	
			if(lowerCaseName.indexOf("windows") > -1)
	    		strChosenRootName = s.substring(0, ix).toLowerCase();
	    	else if(lowerCaseName.indexOf("linux") > -1)
	    		strChosenRootName = s.substring(0, ix);
    	}

		///////////////////////////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////

		
		String strExtendedFileName = strChosenRootName + ".xy";
		try {
			output = new FileWriter(strExtendedFileName);
			output.write("    1\n");
	
			int riverSectionCount = 1;
			boolean incremented = true;
			
			for (int i=0; i<nSections; i++) {
				DownstreamPoint dp = (DownstreamPoint)outputData.inputDownstream.get(i);
				int j;
				for (j=0; j<selections.length; j++)
					if (selections[j] == i)
						break;
				if (j == selections.length && incremented)
				{
					output.write(intToString5(0) + "," + floatToString15(dp.distanceToSection) + "," +
						// reversed per Albert Momo's request 03/15/04
						// floatToString15(dp.latitude) + "," + floatToString15(dp.longitude) +
						floatToString15(dp.longitude) + "," + floatToString15(dp.latitude) +
						"," + floatToString15(dp.getBestSection().getElevationData(0,0)) + "\n");
				}
				else if (j == selections.length && !incremented)
				{
					incremented = true;
					riverSectionCount++;
					output.write(intToString5(0) + "," + floatToString15(dp.distanceToSection) + "," +
						floatToString15(dp.latitude) + "," + floatToString15(dp.longitude) +
						"," + floatToString15(dp.getBestSection().getElevationData(0,0)) + "\n");
				}
				else
				{
					output.write(intToString5(riverSectionCount) + "," + floatToString15(dp.distanceToSection) + "," +
						floatToString15(dp.latitude) + "," + floatToString15(dp.longitude) +
						"," + floatToString15(dp.getBestSection().getElevationData(0,0)) + "\n");
					incremented = false;
				}
			}
			output.flush();
			output.close();
		} catch (Throwable e) {
			JOptionPane.showMessageDialog(this, "Exception thrown: " + e.getMessage());
			e.printStackTrace();
			return;
		}
		
		// generate .fcs file
		
		strExtendedFileName = strChosenRootName + ".fcs";
		
		try {
			output = new FileWriter(strExtendedFileName);
			output.write(" DBRK\n");
	
			for (int i=0; i<nSections; i++) {
				DownstreamPoint dp = (DownstreamPoint)outputData.inputDownstream.get(i);
				output.write(floatToString15(dp.distanceToSection) + "," +
					floatToString15(outputData.maxElevation[i]) + "," + floatToString15(XSWidth[i]) +
					"," + floatToString15(-999.0f) + "\n");
			}
			
			output.flush();
			output.close();
		} catch (Throwable e) {
			JOptionPane.showMessageDialog(this, "Exception thrown: " + e.getMessage());
			e.printStackTrace();
			return;
		}
		
		JOptionPane.showMessageDialog(this,"Successfully created:\n\n"+(strChosenRootName +".xy")+"\n"+(strChosenRootName +".fcs"),"Success",JOptionPane.INFORMATION_MESSAGE);
	}
	private String floatToString15(float value) {
		
		// *** this should be moved to a method in gov.dambreak.util package
		
		java.text.DecimalFormat df;
		df = new java.text.DecimalFormat("#########0.0000");
	
		StringBuffer result = new StringBuffer(df.format(value));
		
		while (result.length() < 15)
			result.insert(0, ' ');
			
		return result.toString();
	}
	/**
	 * A custom renderer is used in order to center the cross section labels
	 * in the JList.
	 **/
	public Component getListCellRendererComponent(
		JList list,
		Object value,
		int index,
		boolean isSelected,
		boolean cellHasFocus)
	
	{
     	JLabel item = new JLabel((String)value, JLabel.CENTER);
     	if (isSelected) {
     		item.setOpaque(true);
     		item.setBackground(xsChooser.getSelectionBackground());
     		item.setForeground(xsChooser.getSelectionForeground());
     		if (cellHasFocus)
				item.setBorder(BorderFactory.createLineBorder(Color.yellow, 1));
     	} else {
     		item.setOpaque(true);
     		item.setBackground(Color.white);
     	}
		return item;
	}
	private String intToString5(int value) {

		// *** this should be moved to a method in gov.dambreak.util package
		
		StringBuffer result = new StringBuffer(Integer.toString(value));
		
		for (int i=result.length(); i<5; i++)
			result.insert(0, ' ');

		return result.toString();
	}
}
