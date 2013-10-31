package gov.dambreak.smpdbk;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import gov.dambreak.util.*;

/**
 * This class represents a panel which contains a visualization of
 * SMPDBK output data.
 */
public class DisplayGraphicsPanel extends javax.swing.JPanel implements ActionListener, MouseListener 
{

	private ModelOutput outputFile;
	private JLabel longitudinalTitle;
	private JComboBox xsChooser;
	private int selectedXS;
	private Rectangle[] sectionBoxes;
	
	public DisplayGraphicsPanel(ModelOutput _outputFile) {
		
		super();
		initialize();
		outputFile = _outputFile;
		longitudinalTitle = new JLabel("");
		longitudinalTitle.setHorizontalAlignment(JLabel.CENTER);
		longitudinalTitle.setVerticalAlignment(JLabel.TOP);
		longitudinalTitle.setFont(new Font("SansSerif", Font.BOLD, 16));
		longitudinalTitle.setBounds(0,0,800,25);
		add(longitudinalTitle);
		
		// create drop-down box

		String[] items = new String[outputFile.inputDownstream.size()];

		// System.out.println("DisplayGraphicsPanel- size = " + outputFile.inputDownstream.size());
		
		for (int i=0; i<outputFile.inputDownstream.size(); i++)
			if (outputFile.getPointOfInterestXS() == i)
			{
				items[i] = outputFile.pointOfInterestName + " - Distance: " + NumFormat.format(((DownstreamPoint)outputFile.inputDownstream.get(i)).distanceToSection,4) + "(*)";
			}
			else
			{
				float dist = ((DownstreamPoint)outputFile.inputDownstream.get(i)).distanceToSection;
				
				if (dist == 0.0)
				{
					items[i] = "AT DAM - Distance: " + NumFormat.format(((DownstreamPoint)outputFile.inputDownstream.get(i)).distanceToSection,4);
				}
				else
				{
					items[i] = ((DownstreamPoint)outputFile.inputDownstream.get(i)).name + " - Distance: " + NumFormat.format(((DownstreamPoint)outputFile.inputDownstream.get(i)).distanceToSection,4);
				}
			}
			
		xsChooser = new JComboBox(items);
		xsChooser.addActionListener(this);
		xsChooser.setBounds(260,407,290,25);
		
		xsChooser.setSelectedIndex(outputFile.getPointOfInterestXS());
		selectedXS = outputFile.getPointOfInterestXS();
		add(xsChooser);
	}
	/**
	 * Process a cross section combo-box selection change
	 */
	public void actionPerformed(ActionEvent e) {
		try {
			repaint();
		} catch (Throwable ex) {
			System.out.println("caught in DisplayGraphicsPanel.actionPerformed");
			ex.printStackTrace();
		}
	}
	private void drawCrossSection(Graphics g, int[][] hs, int[][] bs, int x1, int y1, int x2, int y2) {

		// System.out.println("draw CrossSection");
		try {
			int minHS = Integer.MAX_VALUE, minBS = Integer.MAX_VALUE;
			int maxHS = Integer.MIN_VALUE, maxBS = Integer.MIN_VALUE;
		
			int nSections = outputFile.inputDownstream.size();
			int nElevations = AnalysisData.getNumPairs(outputFile.inputDownstream);
		
			int selectedXS = xsChooser.getSelectedIndex();

			java.text.DecimalFormat df;
			df = new java.text.DecimalFormat("#########0.0000");
		
			// find maximum and minimum HS and BS
			for (int j=0; j<nElevations; j++) {
				maxHS = Math.max(hs[selectedXS][j], maxHS);
				maxBS = Math.max(bs[selectedXS][j], maxBS);
				minHS = Math.min(hs[selectedXS][j], minHS);
				minBS = Math.min(bs[selectedXS][j], minBS);
			}

			// check to see if the max flood elevation flows over the bank - if so, this should be the maxHS
			maxHS = Math.max((int)outputFile.maxElevation[selectedXS], maxHS);
		
			double widthHS = maxHS - minHS;							// distance high to low HS
			double widthBS = maxBS - minBS;							// distance high to low BS
		
			int Xwidth = (int)((x2-x1)*0.6), Xheight = y2-y1-60;	// cross section dimensions
			int Xx1 = x1+((x2-x1-Xwidth)/2), Xy1 = y1+20;			// cross section starting coordinates
		
			// create flood water rectangle

			/////////////
			// NOTE: The following line should really be different depending on if the flood depth is measured
			// from the bottom of the channel or from MSL.  The uncommented line below assumes the depth
			// is from MSL as this is the type commonly found in the DAMCAT database.  The commented line
			// assumes a flood depth measurement from the channel bottom.
			/////////////		
			// int floodDepthLine = hs[selectedXS][0] + (int)(((DownstreamPoint)outputFile.inputDownstream.get(selectedXS)).floodDepth);		
			// int floodDepthLine = (int)(((DownstreamPoint)outputFile.inputDownstream.get(selectedXS)).floodDepth);

		
			DownstreamPoint dp = (DownstreamPoint)outputFile.inputDownstream.get(selectedXS);
			// *** add back in elevation 
			int floodDepthLine = (int) (dp.floodDepth + dp.elevation);
			int yFloodDepthPos = (int)(Xheight - ((floodDepthLine - minHS) / widthHS) * Xheight);
		
			/*System.out.println("floodDepthLine - " + floodDepthLine);
			System.out.println("Xheight - " + Xheight);
			System.out.println("minHS - " + minHS);
			System.out.println("widthHS - " + widthHS);
			System.out.println("yFloodDepthPos - " + yFloodDepthPos);*/
		
		
			int floodLine = (int)outputFile.maxElevation[selectedXS];
			int yFloodPos = (int)(Xheight - ((floodLine - minHS) / widthHS) * Xheight);
			g.setColor(new Color(190,255,255));
			g.fillRect(Xx1,Xy1+yFloodPos,Xwidth,Xheight-yFloodPos);
		
			// transform max elevation
			int yMaxFloodPos = (int)(((outputFile.maxElevation[selectedXS] - minHS) / widthHS) * Xheight);
		
			// create soil polygon
			int[]	soilPolyX = new int[2*nElevations+5],
				soilPolyY = new int[2*nElevations+5];
			int count=0;
			soilPolyX[count] = Xx1+Xwidth;	// top-right
			soilPolyY[count++] = Xy1;
			soilPolyX[count] = Xx1+Xwidth;	// bottom-right
			soilPolyY[count++] = Xy1+Xheight;
			soilPolyX[count] = Xx1;			// bottom-left
			soilPolyY[count++] = Xy1+Xheight;
			soilPolyX[count] = Xx1;			// top-left
			soilPolyY[count++] = Xy1;
			for (int i = nElevations-1; i >= 0; i--)
			{
				soilPolyX[count] = (int)(Xx1+((Xwidth/2) - (((bs[selectedXS][i] - minBS) / widthBS) * Xwidth) / 2));
				soilPolyY[count++] = (int)(Xy1+Xheight-(((hs[selectedXS][i] - minHS) / widthHS) * Xheight));
			}
			soilPolyX[count] = Xx1 + (Xwidth/2);
			soilPolyY[count++] = Xy1+Xheight;
			for (int i = 0; i < nElevations; i++)
			{
				soilPolyX[count] = (int)(Xx1+((Xwidth/2) + (((bs[selectedXS][i] - minBS) / widthBS) * Xwidth) / 2));
				soilPolyY[count++] = (int)(Xy1+Xheight-(((hs[selectedXS][i] - minHS) / widthHS) * Xheight));
			}
			g.setColor(new Color(150,83,82));
			g.fillPolygon(soilPolyX,soilPolyY,2*nElevations+5);
		
			// draw max elevation text
			g.setColor(Color.black);
			Font f4 = new Font("Serif",Font.PLAIN, 13);
			g.setFont(f4);
			FontMetrics fm = g.getFontMetrics();
			int junk1y = Xy1+(Xheight-yMaxFloodPos);
			String strMaxFloodElev = "Max Flood Elevation: " + df.format(outputFile.maxElevation[selectedXS]) + " ft";
			g.drawString(strMaxFloodElev, Xx1+(Xwidth/2)-(fm.stringWidth(strMaxFloodElev)/2), (Xy1-2)+(Xheight-yMaxFloodPos)-2);
		
			// draw max depth
			g.drawString("Max Flood Depth:", Xx1+Xwidth+21, Xy1+(Xheight-(yMaxFloodPos/2)));
			g.drawString(df.format(outputFile.maxDepth[selectedXS]) + " ft", Xx1+Xwidth+21, Xy1+(Xheight-(yMaxFloodPos/2))+13);
			g.drawLine(Xx1+Xwidth+5,Xy1+(Xheight-yMaxFloodPos),Xx1+Xwidth+21,Xy1+(Xheight-yMaxFloodPos));
			g.drawLine(Xx1+Xwidth,Xy1+Xheight,Xx1+Xwidth+21,Xy1+Xheight);
			g.drawLine(Xx1+Xwidth+19,Xy1+Xheight,Xx1+Xwidth+19,Xy1+(Xheight-yMaxFloodPos));
			g.drawLine(Xx1+Xwidth+21,Xy1+Xheight+2,Xx1+Xwidth+17,Xy1+Xheight-2); // bottom corner cross
			g.drawLine(Xx1+Xwidth+21,Xy1+(Xheight-yMaxFloodPos)+2,Xx1+Xwidth+17,Xy1+(Xheight-yMaxFloodPos)-2); // top corner cross
		
			// draw flood depth elevation
			double floodElevation = outputFile.maxElevation[selectedXS]+((DownstreamPoint)outputFile.inputDownstream.get(selectedXS)).floodDepth-outputFile.maxDepth[selectedXS];
			String strFloodDepthElevation = df.format(floodElevation) + " ft";
			if (Math.abs((Xy1+(Xheight-yMaxFloodPos)-2) - (Xy1+yFloodDepthPos-2)) >= fm.getAscent())
				g.drawString(strFloodDepthElevation, Xx1+(Xwidth/2)-(fm.stringWidth(strFloodDepthElevation)/2), Xy1+yFloodDepthPos-2);
			g.setColor(new Color(150,83,82));
			g.drawLine(Xx1,Xy1+yFloodDepthPos,Xx1+Xwidth,Xy1+yFloodDepthPos);
			g.setColor(Color.black);
		
			// draw flood depth
			/*
			System.out.println("Xx1- " + Xx1);
			System.out.println("Xy1- " + Xy1);
			System.out.println("yFloodDepthPos- " + yFloodDepthPos);
			System.out.println("Xheight- " + Xheight);
			*/
		
			g.drawString("Flood Stage:",Xx1-90,Xy1+yFloodDepthPos+(Xheight-yFloodDepthPos)/2);
			g.drawString(df.format(((DownstreamPoint)outputFile.inputDownstream.get(selectedXS)).floodDepth) + " ft", Xx1-90, Xy1+yFloodDepthPos+(Xheight-yFloodDepthPos)/2+13);
			g.drawLine(Xx1-18,Xy1+Xheight,Xx1,Xy1+Xheight);
			g.drawLine(Xx1-18,Xy1+Xheight+2,Xx1-14,Xy1+Xheight-2); // bottom corner cross
			g.drawLine(Xx1-18,Xy1+yFloodDepthPos,Xx1,Xy1+yFloodDepthPos);
			g.drawLine(Xx1-17,Xy1+Xheight,Xx1-17,Xy1+yFloodDepthPos);
			g.drawLine(Xx1-19,Xy1+yFloodDepthPos+2,Xx1-15,Xy1+yFloodDepthPos-2); // top corner cross
		} catch (Throwable e) {
			System.out.println ("Exception indrawCrossSection()");
			e.printStackTrace();
		}
	}
	private void drawRiverView(Graphics g, int x1, int y1, int x2, int y2) {

		// System.out.println("draw RiverView");
		try {
			double minHS = Integer.MAX_VALUE;
			double maxHS = Integer.MIN_VALUE;
			double minRM = Integer.MAX_VALUE;
			double maxRM = Integer.MIN_VALUE;
			int Xwidth = (int)((x2-x1)*0.9);						// cross section dimensions
			int Xheight = y2-y1-60;
			int Xx1 = x1+((x2-x1-Xwidth)/2);						// cross section starting coordinates
			int Xy1 = y1+20;
			int nSections = outputFile.inputDownstream.size(), nElevations = AnalysisData.getNumPairs(outputFile.inputDownstream);
			int selectedXS = xsChooser.getSelectedIndex();

			// find maximum and minimum river mile and land elevation
			for (int j=0; j<nSections; j++) {
				double height = outputFile.maxElevation[j]-outputFile.maxDepth[j];
				maxHS = Math.max(height, maxHS);
				minHS = Math.min(height, minHS);
				maxRM = Math.max(((DownstreamPoint)outputFile.inputDownstream.get(j)).distanceToSection, maxRM);
				minRM = Math.min(((DownstreamPoint)outputFile.inputDownstream.get(j)).distanceToSection, minRM);
			}
			double widthHS = maxHS - minHS;
			double widthRM = maxRM - minRM;
		
			double maxWHS = outputFile.inputScenario.HDE;
			double minWHS = minHS;
			double widthWHS = maxWHS-minWHS;
			if (widthWHS < 0.001d)
			{
				System.out.println("Can't draw - zero widthWHS");
				return;
			}
		
			////////////////////////////////////
			// create breach water polygon
			////////////////////////////////////
			int[]	waterPolyX = new int[nSections+3],
				waterPolyY = new int[nSections+3];
			int count=0;
		
			waterPolyX[count] = Xx1;
			waterPolyY[count++] = (int)(Xy1+Xheight-((outputFile.maxElevation[0]-minWHS)/widthWHS)*((Xheight*0.7)));
			waterPolyX[count] = Xx1;
			waterPolyY[count++] = Xy1+Xheight;
			waterPolyX[count] = Xx1+Xwidth;
			waterPolyY[count++] = Xy1+Xheight;
		
			for (int i=nSections-1; i>=0; i--) {
				waterPolyX[count] = (int)(((((DownstreamPoint)outputFile.inputDownstream.get(i)).distanceToSection-minRM)/widthRM)*(Xwidth-(Xwidth/5))+Xx1+(Xwidth/5));
				double height = outputFile.maxElevation[i];//-outputFile.maxDepth[i];
				waterPolyY[count++] = (int)(Xy1+Xheight-((height-minWHS)/widthWHS)*(Xheight*0.7));//(Xheight*0.4));
			}
			g.setColor(new Color(190,255,255));
			g.fillPolygon(waterPolyX,waterPolyY,nSections+3);
		
			////////////////////////////////////
			// create soil polygon
			////////////////////////////////////
			int[]	soilPolyX = new int[nSections+2],
					soilPolyY = new int[nSections+2];
			count=0;
		
			double damLandHeight = outputFile.maxElevation[0]-outputFile.maxDepth[0];
			soilPolyX[count] = Xx1+(Xwidth/5);
			soilPolyY[count++] = (int)(Xy1+Xheight-((damLandHeight-minWHS)/widthWHS)*((Xheight*0.7)));//(int)(Xy1+(Xheight*0.3));
			soilPolyX[count] = Xx1;
			soilPolyY[count++] = (int)(Xy1+Xheight-((damLandHeight-minWHS)/widthWHS)*((Xheight*0.7)));//(int)(Xy1+(Xheight*0.3));
			soilPolyX[count] = Xx1;
			soilPolyY[count++] = Xy1+Xheight;
			soilPolyX[count] = Xx1+Xwidth;
			soilPolyY[count++] = Xy1+Xheight;


		
			for (int i=nSections-2; i>=1; i--) {
				
				soilPolyX[count] = (int)(((((DownstreamPoint)outputFile.inputDownstream.get(i)).distanceToSection-minRM)/widthRM)*(Xwidth-(Xwidth/5))+Xx1+(Xwidth/5));
				double height = outputFile.maxElevation[i]-outputFile.maxDepth[i];
				soilPolyY[count++] = (int)(Xy1+Xheight-((height-minWHS)/widthWHS)*((Xheight*0.7)));//(Xheight*0.4));

			}
			g.setColor(new Color(150,83,82));
			
			g.fillPolygon(soilPolyX,soilPolyY,nSections+2);
		
			/////////////////////////////////////////////		
			// draw xs markers and set selected to yellow

			g.setColor(new Color(0,0,0));
			sectionBoxes = new Rectangle[nSections];
			for (int i=0; i<nSections; i++) {
				double height = outputFile.maxElevation[i]-outputFile.maxDepth[i];
				sectionBoxes[i] = new Rectangle((int)(((((DownstreamPoint)outputFile.inputDownstream.get(i)).distanceToSection-minRM)/widthRM)*(Xwidth-(Xwidth/5))+Xx1+(Xwidth/5)),
					(int)(Xy1+Xheight-((height-minWHS)/widthWHS)*((Xheight*0.7)))-1,
					6,6);
				g.fillRect(sectionBoxes[i].x, sectionBoxes[i].y, sectionBoxes[i].width, sectionBoxes[i].height);
			}
			g.setColor(Color.yellow);
			double selectedXSHeight = outputFile.maxElevation[selectedXS] - outputFile.maxDepth[selectedXS];
			g.fillRect(1+(int)(((((DownstreamPoint)outputFile.inputDownstream.get(selectedXS)).distanceToSection-minRM)/widthRM)*(Xwidth-(Xwidth/5))+Xx1+(Xwidth/5)),
				(int)(Xy1+Xheight-((selectedXSHeight-minWHS)/widthWHS)*((Xheight*0.7))),
				4,4);
		
			// draw dam and dam water
			int xDamStart = (int)(Xx1+(Xwidth/5)-(Xwidth/5)*0.3);
			int waterHeight = (int)(((outputFile.inputScenario.HDE-damLandHeight)/widthWHS)*(Xheight*0.7));
			int waterBase = (int)(Xy1+Xheight-((damLandHeight-minWHS)/widthWHS)*((Xheight*0.7)));
			g.setColor(new Color(190,255,255));
			g.fillRect(Xx1,(int)(waterBase-waterHeight),xDamStart-Xx1,waterHeight);	// water
		
			g.setColor(new Color(100,85,85));
			int[] xp1 = {xDamStart,xDamStart,(int)(Xx1+(Xwidth/5)),(int)(Xx1+(Xwidth/5)-((Xx1+(Xwidth/5)-xDamStart)*0.7))};
			int[] yp1 = {(int)(waterBase-waterHeight)-8,soilPolyY[0],soilPolyY[0],(int)(waterBase-waterHeight)-8};
			g.fillPolygon(xp1,yp1,4);	// dam
		
			//draw town at point of interest xs
			g.setColor(Color.black);
		
			/*
			double pointOfInterestHeight = outputFile.maxElevation[outputFile.getPointOfInterestXS()]-outputFile.maxDepth[outputFile.getPointOfInterestXS()];
			int cX = (int)(((((DownstreamPoint)outputFile.inputDownstream.get(outputFile.getPointOfInterestXS())).distanceToSection-minRM)/widthRM)*(Xwidth-(Xwidth/5))+Xx1+(Xwidth/5));
			int cY = (int)(Xy1+Xheight-((pointOfInterestHeight-minWHS)/widthWHS)*((Xheight*0.7)));
			*/
		
			double pointOfInterestHeight = outputFile.maxElevation[selectedXS]-outputFile.maxDepth[selectedXS];
			int cX = (int)(((((DownstreamPoint)outputFile.inputDownstream.get(selectedXS)).distanceToSection-minRM)/widthRM)*(Xwidth-(Xwidth/5))+Xx1+(Xwidth/5));
			int cY = (int)(Xy1+Xheight-((pointOfInterestHeight-minWHS)/widthWHS)*((Xheight*0.7)));
			g.setColor(Color.black);
			g.drawLine(cX,cY,cX+43,cY);
			int town_begin = cX;
			int town_end = cX + 43;
			int[] xp5={5,5,7,7,7,9,9,9,11,11,13,13,17,17,19,19,22,22,24,24,27,27};
			for (int i=0; i<xp5.length; i++)
				xp5[i] += cX;
			int[] yp5={cY,cY-3,cY-3,cY,cY-8,cY-8,cY,cY-4,cY-4,cY,cY,cY-9,cY-15,cY,cY,cY-17,cY-17,cY,cY,cY-6,cY-6,cY};
			g.drawPolyline(xp5,yp5,22);
			int[] xp6={29,29,31,34,34,31,31,38,38,37,37,40,40,41,41,43,43};
			for (int i=0; i<xp6.length; i++)
				xp6[i] += cX;
			int[] yp6={cY,cY-8,cY-12,cY-8,cY,cY,cY-4,cY-4,cY,cY,cY-7,cY-7,cY,cY,cY-3,cY-3,cY};
			
			g.drawPolyline(xp6,yp6,17);

			// draw mileage from dam to town
			Font f2 = new Font("Serif",Font.PLAIN, 13);
			g.setFont(f2);
			FontMetrics fm = g.getFontMetrics();
			// String mileageString = ((DownstreamPoint)outputFile.inputDownstream.get(outputFile.getPointOfInterestXS())).distanceToSection + " miles from dam";
			String mileageString = ((DownstreamPoint)outputFile.inputDownstream.get(selectedXS)).distanceToSection + " miles from dam";
			int distanceStart = Xx1 + (Xwidth/5), distanceEnd = cX;
			g.drawString(mileageString, distanceStart+(distanceEnd-distanceStart)/2-(fm.stringWidth(mileageString)/2), Xy1+Xheight+fm.getAscent()+1);
			g.drawLine(distanceStart,Xy1+Xheight+fm.getAscent()+3,distanceEnd,Xy1+Xheight+fm.getAscent()+3);
			g.drawLine(distanceStart,soilPolyY[0]+10,distanceStart,Xy1+Xheight+fm.getAscent()+3);
			g.drawLine(cX,cY+8,cX,Xy1+Xheight+fm.getAscent()+3);
			g.drawLine(distanceStart-2,Xy1+Xheight+fm.getAscent()+4,distanceStart+2,Xy1+Xheight+fm.getAscent());
			g.drawLine(cX-2,Xy1+Xheight+fm.getAscent()+4,cX+2,Xy1+Xheight+fm.getAscent());
		
			// draw dam water height
			g.drawString(outputFile.inputScenario.HDE + " ft",Xx1+3, (int)(waterBase-waterHeight)-3);
			java.text.DecimalFormat df = new java.text.DecimalFormat("#######.0000");
			g.drawString(df.format(outputFile.inputScenario.HDE-outputFile.inputScenario.BME)+" ft", Xx1+3, (int)(waterBase-waterHeight+(waterHeight/2)+fm.getAscent()/2));
			g.drawLine(Xx1-15,(int)(waterBase-waterHeight),Xx1-15,(int)(waterBase));
			g.drawLine(Xx1-19,(int)(waterBase-waterHeight),Xx1-3,(int)(waterBase-waterHeight));
			g.drawLine(Xx1-19,(int)(waterBase),Xx1-3,(int)(waterBase));
			g.drawLine(Xx1-15,(int)(waterBase-waterHeight+(waterHeight/2)),Xx1,(int)(waterBase-waterHeight+(waterHeight/2)));
		
			// draw dam breach water height
			g.drawLine((int)(Xx1+(Xwidth/5)),(int)(waterBase),(int)(Xx1+(Xwidth/5)+25),(int)(waterBase));
			g.drawLine((int)(Xx1+(Xwidth/5)),waterPolyY[0],(int)(Xx1+(Xwidth/5)+25),waterPolyY[0]);
			g.drawLine((int)(Xx1+(Xwidth/5)+21),(int)(waterBase),(int)(Xx1+(Xwidth/5)+21),waterPolyY[0]);
			if(selectedXS == 0)
				g.drawString(outputFile.maxElevation[0] + " ft",(int)(Xx1+(Xwidth/5)),waterPolyY[0]-16);
			else
				g.drawString(outputFile.maxElevation[0] + " ft",(int)(Xx1+(Xwidth/5)),waterPolyY[0]-3);
			g.drawLine((int)(Xx1+(Xwidth/5)+21),(int)((waterBase-waterPolyY[0])/2+waterPolyY[0]),(int)(Xx1+(Xwidth/5)+35),(int)((waterBase-waterPolyY[0])/2+waterPolyY[0]));
			if(selectedXS == 0)
				g.drawString(df.format(outputFile.maxElevation[0]-outputFile.inputScenario.BME)+" ft", (int)(Xx1+(Xwidth/5)+47), (int)((waterBase-waterPolyY[0])/2+waterPolyY[0]+(fm.getAscent()/2)));
			else
			{
				int bme_start = (int)(Xx1+(Xwidth/5)+37);
				if((town_begin < bme_start) && (bme_start < town_end))
				{
					g.drawString(df.format(outputFile.maxElevation[0]-outputFile.inputScenario.BME)+" ft", (int)(Xx1+(Xwidth/5)+37+(town_end-bme_start+2)), (int)((waterBase-waterPolyY[0])/2+waterPolyY[0]+(fm.getAscent()/2)));
				}
				else
				{
					g.drawString(df.format(outputFile.maxElevation[0]-outputFile.inputScenario.BME)+" ft", (int)(Xx1+(Xwidth/5)+37), (int)((waterBase-waterPolyY[0])/2+waterPolyY[0]+(fm.getAscent()/2)));
				}
			}
			/*
			// draw town data
			String townPeakFlow = "Peak Flow at " + outputFile.pointOfInterestName + ": " + outputFile.maxFlow[outputFile.getPointOfInterestXS()] + "  cfs",
				townMaxElevation = "Max Elevation at " + outputFile.pointOfInterestName + ": " + outputFile.maxElevation[outputFile.getPointOfInterestXS()] + "  ft",
				townMaxDepth = "Max Depth at " + outputFile.pointOfInterestName + ": " + outputFile.maxDepth[outputFile.getPointOfInterestXS()] + "  ft";
			*/
			// draw town data
			String townPeakFlow = "Peak Flow " + ": " + outputFile.maxFlow[selectedXS] + "  cfs",
				townMaxElevation = "Max Elevation " + ": " + outputFile.maxElevation[selectedXS] + "  ft",
				townMaxDepth = "Max Depth " + ": " + outputFile.maxDepth[selectedXS] + "  ft";
			
			int left,bottom;
		
			/*

			if (cX+fm.stringWidth(townMaxElevation) > Xx1+Xwidth) {	// right justify
				left = Xx1+Xwidth-fm.stringWidth(townMaxElevation);
				int which = 0;
				if (nSections > 3) {
					while ((which < nSections-1) && sectionBoxes[which].x < left)
						which++;
				}
				bottom = waterPolyY[nSections+2-which];
			}
			else {

				left = cX;
				bottom = cY;
			}
			*/
		
			//  **** always right justify ! 
			left = Xx1+Xwidth-fm.stringWidth(townMaxElevation);
			bottom = waterPolyY[nSections+2];
		
			g.drawString(townPeakFlow, left, bottom-3*fm.getAscent()-5);
			g.drawString(townMaxElevation, left, bottom-2*fm.getAscent()-5);
			g.drawString(townMaxDepth, left, bottom-1*fm.getAscent()-5);
		} catch (Throwable e) {
			System.out.println("Exception in drawRiverView()");
			e.printStackTrace();
		}
	}
	/**
	 * Initialize the class.
	 */
	private void initialize() {
		try {
			setName("DisplayGraphicsPanel");
			setLayout(null);
			setSize(657, 450);
			addMouseListener(this);
		} catch (java.lang.Throwable ivjExc) {
			System.out.println("Throwable caught In DisplayGraphicsPanel.initialize");
			ivjExc.printStackTrace();
		}
	}
	public void mouseClicked(MouseEvent event) {}
	public void mouseEntered(MouseEvent event) {}
	public void mouseExited(MouseEvent event) {}
	public void mousePressed(MouseEvent event)
	{
		int i;
		try {
			for (i=0; i<outputFile.inputDownstream.size(); i++)
				if (sectionBoxes[i].contains(event.getPoint()))
					break;
				
			if (i == outputFile.inputDownstream.size())
				return;
		
			xsChooser.setSelectedIndex(i);
		
			// Graphics myG = getGraphics();
			// drawRiverView(myG, 5,25,getWidth()-5,(int)(getHeight()*0.6)-5);
		
			repaint();
		} catch (Throwable e) {
			System.out.println("caught in mousePressed");
			e.printStackTrace();
		}
	}
	// unused MouseListener methods
	public void mouseReleased(MouseEvent event) {}
	/**
	 * Draw the SMPDBK model output graphic.  Assume the output has been validated.
	 */
	public void paint(Graphics g) {

		try {
			super.paint(g);

			int nElevations = AnalysisData.getNumPairs(outputFile.inputDownstream);
			if (nElevations <= 1) {
				// *** what can be done to prevent this error ?
				g.drawString("Inconsistent number of pairs.",50,50);
				return;
			}
		
			// convert all floats to int
			int[][] hs  = new int[outputFile.inputDownstream.size()][nElevations];
			int[][] bs  = new int[outputFile.inputDownstream.size()][nElevations];
			int[][] bss  = new int[outputFile.inputDownstream.size()][nElevations];

			for (int i=0; i<outputFile.inputDownstream.size(); i++)
			{
				DownstreamPoint down = (DownstreamPoint)outputFile.inputDownstream.get(i);
				for (int j=0; j<nElevations; j++)
				{
					hs[i][j] = (int)down.getBestSection().getElevationData(j,0);
					bs[i][j] = (int)down.getBestSection().getElevationData(j,1);
					bss[i][j] = (int)down.getBestSection().getElevationData(j,2);

					// include any 'bss' in the 'bs'
					bs[i][j] += bss[i][j];
				}
			}
		
			// set graphic title
			if (outputFile.pointOfInterestName.compareTo("") == 0)
				outputFile.pointOfInterestName = "Point of Interest";

			int index = outputFile.riverName.indexOf("(");
			if (index != -1)
			{
				String riverNameSubStr = outputFile.riverName.substring(0, index);
				longitudinalTitle.setText(outputFile.damName.trim() + " Dam to " + outputFile.pointOfInterestName.trim() + " on "+ 
					riverNameSubStr);
			}
			else
			{
			longitudinalTitle.setText(outputFile.damName.trim() + " Dam to " + outputFile.pointOfInterestName.trim() + " on "+ 
				outputFile.riverName.trim());
			}
			longitudinalTitle.setBounds(0,0,getWidth(),25);
		
			drawRiverView(g, 5, 25, getWidth()-5, (int)(getHeight()*0.6)-5);		
			drawCrossSection(g, hs, bs, 5,(int)(getHeight()*0.6)+5, getWidth()-5, getHeight()-5);
		
			selectedXS = xsChooser.getSelectedIndex();
			xsChooser.setBounds(getWidth()/2-145, getHeight()-37, 290, 25);
		} catch (Throwable e) {
			System.out.println("caught in paint");
			e.printStackTrace();
		}
	}
}
