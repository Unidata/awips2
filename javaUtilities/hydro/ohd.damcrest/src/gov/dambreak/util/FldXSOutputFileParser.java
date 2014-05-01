package gov.dambreak.util;

import java.io.*;
import java.util.*;
import java.awt.geom.*;
import gov.noaa.FloodWav.Component.*;
import javax.swing.*;

/**
 * This class allows for the reading of FLDXS output tables.
 * NOTE: This class should become the importFLDXS(String) method
 * of AnalysisData;
 */
public class FldXSOutputFileParser {
	
	// private fields
	
	private File		file;
	private float[][]	distancesForXS, elevationsForXS;
	private float[]		riverMiles;
	private Point2D.Float[] latlons;
	
	// old fields
	public String		filename;
	public int			numberXSections;
	// public methods
	public FldXSOutputFileParser(File _file) {
		file = _file;
		filename = file.getPath();
	}
	public boolean exists() {
		return file.exists();
	}
	public ArrayList parse() {
		
		boolean bResult = readFLDXSArrays();
		
		if (!bResult)
		{
			System.out.println("In FldXSOutputFileParser.parse ---");	
			System.out.println("False return from readFLDXSArrays");
			return null;
		}

		ArrayList downstream = new ArrayList();	// DownstreamPoints

		ElevationStationManager em = null;
		try {
			em = new ElevationStationManager();
		} catch (Throwable e) {
			System.out.println("Could not find ElevationStationManager class: FLDAT is not in the classpath.");
			e.printStackTrace();
			return null;
		}

		// convert XY defined cross sections to B vs H
		Vector v = new Vector(2);
		v.addElement(new String("0.0"));
		v.addElement(new String("0.0"));
		Table t;
		Vector[] elevations = new Vector[numberXSections];
		Vector[] distances1 = new Vector[numberXSections];
		Vector[] distances2 = new Vector[numberXSections];
		for (int i=0; i<numberXSections; i++) {
			
			Vector data = new Vector(2);
			Vector edata = new Vector(elevationsForXS[i].length);
			Vector ddata = new Vector(elevationsForXS[i].length);
			for (int j=0; j<elevationsForXS[i].length; j++) {
				edata.add(new Float(elevationsForXS[i][j]).toString());
				ddata.add(new Float(distancesForXS[i][j]).toString());
			}
			try {
				data.add(ddata);
				data.add(edata);
				t = new Table(data, v);
			} catch (Exception e) {
				System.out.println("Exception thrown while creating Table: " + e.getMessage());
				e.printStackTrace();
				return null;
			}
			Vector d = em.convertTopwidths(t);
			elevations[i] = (Vector)d.get(0);
			distances1[i] = (Vector)d.get(1);
			distances2[i] = (Vector)d.get(2);			// was 3
		}

		// move the data from the arrays into the final data structure
		for (int i=0; i<numberXSections; i++) {
			DownstreamPoint dp = new DownstreamPoint();
			
			dp.distanceToSection = riverMiles[i];
			dp.latitude = latlons[i].x;
			dp.longitude = latlons[i].y;

			SectionGeometry sg = new SectionGeometry();
			sg.setXSType("G2");
			for (int j=0; j<8; j++) {
				sg.setElevationData(j,0,((Float)elevations[i].get(j)).floatValue());
				sg.setElevationData(j,1,(((Float)distances2[i].get(j)).floatValue()-((Float)distances1[i].get(j)).floatValue()));
				sg.setElevationData(j,2, 0.0f);
				sg.setElevationData(j,3, 0.0f);
			}
			
			sg.setLastRowUsed(7);			// 8 are always computed by ElevationStationManager.convertTopWidths() ?
			
			if (i == 0)						// *** assume first is best !
			{
				dp.bestXS = 0;
				dp.xsecBestType = sg.getXSType();
			}
			dp.xsections.add(sg);
			downstream.add(dp);
		}
		
		return downstream;
	}
	// private methods
	private boolean readFLDXSArrays() {
		BufferedReader input = null;
		String line = "";
/*		
		String lineSave = "";
		int isave = 0;
		int ksave = 0;
		String distSave = "";
		String elevSave = "";
*/		
		try {
			input = new BufferedReader(new FileReader(file));

			// scan past column titles but check to make sure they are in first line
			boolean metric = false;
			line = input.readLine();
			if (line.startsWith("\"RiverMile (km)\",\"Distance\",\"Elevation\""))
				metric = true;
			else if (line.startsWith("\"RiverMile (mi)\",\"Distance\",\"Elevation\""))
				metric = false;
			else {
				input.close();
				return false;
			}

			line = input.readLine();
			StringTokenizer st;
			st = new StringTokenizer(line, ",\n\r");

			// *** second line contains number of Cross Sections contained in file
			
			numberXSections = (int)Float.parseFloat(st.nextToken());
			
			// *** allocate array storage for each Cross Section
			distancesForXS = new float[numberXSections][];
			elevationsForXS = new float[numberXSections][];
			riverMiles = new float[numberXSections];
			latlons = new Point2D.Float[numberXSections];
			
			String riverMile, distance, elevation, lastRiverMile;

			int numPts=0;
			int numPtsOld = 0;
				
			for (int i=0; i<numberXSections; i++) {

				// *** first line entry in repeating group contains Number of points for this Cross Section				
	  			line = input.readLine();
				st = new StringTokenizer(line, ",\n\r");
				numPtsOld = numPts;
				numPts = (int)Float.parseFloat(st.nextToken());
				
				// *** this makes a bad assumption that all Cross Sections will have the same number of XY pairs !
				/*
				if (i>0 && numPts != numPtsOld) {
		 			JOptionPane.showMessageDialog(null,"Error -- Elev-topwidth messed up? "
			 		+ "different number of points detected on line:\n" + line
			 		+ "     numPtsOld= " + numPtsOld, "Error", JOptionPane.ERROR_MESSAGE);
					return false;				
				}
				*/
				
				distancesForXS[i] = new float[numPts];
				elevationsForXS[i] = new float[numPts];
			
				// *** next line contains longitude of Cross Section
				line = input.readLine(); 
				st = new StringTokenizer(line, ",\n\r");
				float lon = Float.parseFloat(st.nextToken());
				
				// *** next line contains latitude of Cross Section				
				line = input.readLine(); 
				st = new StringTokenizer(line, ",\n\r");
				float lat = Float.parseFloat(st.nextToken());
				
				latlons[i] = new Point2D.Float(lat,lon);

				float rmOld = 0.0f;

				for (int k=0; k < numPts; k++) {
					line = input.readLine();				
					st = new StringTokenizer(line, ",\n\r");
						if (metric) {
							riverMiles[i] = Float.parseFloat(st.nextToken()) * 0.62f;
							distancesForXS[i][k] = Float.parseFloat(st.nextToken()) * 3.28f;
							elevationsForXS[i][k] = Float.parseFloat(st.nextToken()) * 3.28f;
						} else {
							riverMiles[i] = Float.parseFloat(st.nextToken());
							distancesForXS[i][k] = Float.parseFloat(st.nextToken());
							elevationsForXS[i][k] = Float.parseFloat(st.nextToken());
						}

					if (k>0 && riverMiles[i] != rmOld) {
		 				JOptionPane.showMessageDialog(null,"Error -- Elev-topwidth messed up? "
			 			+ "wrong number of points detected on line:\n" + line
			 			+ "     Old rivermile= " + rmOld, "Error", JOptionPane.ERROR_MESSAGE);
						return false;				
					}
					rmOld = riverMiles[i];

				
							
				}

				if (line == null)
					return true;
			}
			} catch (NumberFormatException nfe) {
				nfe.printStackTrace();
				JOptionPane.showMessageDialog(null,"Error -- bad data in fldxs TXT file on line:\n"
					+ line, "Error", JOptionPane.ERROR_MESSAGE);
				return false;
			} catch (Exception e) {
				e.printStackTrace();
				JOptionPane.showMessageDialog(null,"Error -- bad data/file in fldxs TXT file on line:\n",
					"Error", JOptionPane.ERROR_MESSAGE);
				System.out.println("Exception thrown FLDXS txt file: " + e.getMessage());
			return false;	
		}
		return true;
	}
}
