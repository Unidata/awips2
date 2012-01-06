/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.stations.sql;

import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBWriter;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Represents an entry in the Spatial Obs HashMap
 * @author dhladky
 *
 */
public class SpatialLine {
	
	private String gid = AWIPSCommonObsSQLGenerator.NULL;
	private String icao = AWIPSCommonObsSQLGenerator.NULL;
	private String wmo = AWIPSCommonObsSQLGenerator.NULL;
	private String country = AWIPSCommonObsSQLGenerator.NULL;
	private String elevation = AWIPSCommonObsSQLGenerator.NULL;
	private String geom = AWIPSCommonObsSQLGenerator.NULL;
	private String name = AWIPSCommonObsSQLGenerator.NULL;
	private String state = AWIPSCommonObsSQLGenerator.NULL;
	private String type = AWIPSCommonObsSQLGenerator.NULL;
	private static String MAROBS_PRE = "100";
	private static String ZERO = "0000000000";
	
	public SpatialLine (String[] line) {
		
		if (line.length == 9) {
			makeSynopticSpatial(line);
		}
		else {
			makeSpatial(line);
		}
	}
	
	/**
	 * Does the Synoptics
	 * @param line
	 */
	private void makeSynopticSpatial(String[] line) {
		
		if (!line[0].trim().equals("")) { 
			System.out.println(line[0]);
			if (line[0].trim().equals(AWIPSCommonObsSQLGenerator.SYNOPTIC_BLANK_POS)) {
				icao = AWIPSCommonObsSQLGenerator.NULL;
			}
			else {
				System.out.println("ICAO: "+line[0].trim());
				icao = line[0].trim();
			}
		}
		
		if (!line[1].equals("")) {
			System.out.println(line[1]);
			wmo = line[1].trim().substring(0, 5);
			System.out.println("WMO: "+wmo);
		}
		
		if (wmo.equals(AWIPSCommonObsSQLGenerator.NULL) && !icao.equals(AWIPSCommonObsSQLGenerator.NULL)) {
			setGid("null-"+icao);
		}
		else if (!wmo.equals(AWIPSCommonObsSQLGenerator.NULL) && icao.equals(AWIPSCommonObsSQLGenerator.NULL)) {
			setGid(wmo+"-null");
		}
		else {
			setGid(wmo+"-"+icao);
		}
		
		if (!line[2].equals("")) {
			System.out.println(line[2].trim());
			String line2 = line[2].trim();
			line2 = line2.replaceAll("'", "\\\\'");
			line2 = line2.replaceAll(";", ",");
			name = line2;
		}
		
		if (!line[3].trim().equals("")) {
			System.out.println(line[3].trim());
			state = line[3].trim();
		}
		
		if (!line[4].trim().equals("")) {
			System.out.println(line[4].trim());
			country = line[4].trim();
		}
		
		if (!line[5].equals("") && !line[6].equals("")) {
			// create a geometry
			System.out.println(line[5].trim()+" "+line[6].trim());
			WKTReader wktReader = new WKTReader();
			WKBWriter wkbWriter = new WKBWriter();
			Point geometry = null;
			double lat = (new Double(line[5].trim()).doubleValue()/100);
			double lon = (new Double(line[6].trim()).doubleValue()/100);
			System.out.println(lon+" "+lat);
			try {
				// reads X Y; or LON LAT
				geometry = (Point) wktReader.read("POINT ("+AWIPSCommonObsSQLGenerator.FORMAT.format(lon)+" "+AWIPSCommonObsSQLGenerator.FORMAT.format(lat)+")");
			} catch (ParseException pe) {
				pe.printStackTrace();
			}
		
			geom = WKBWriter.bytesToHex(wkbWriter.write(geometry));
		}
		
		if (!line[7].trim().equals("")) {
			System.out.println(line[7].trim());
			if (line[7].equals(AWIPSCommonObsSQLGenerator.SYNOPTIC_BLANK_NEG)) {
				elevation = AWIPSCommonObsSQLGenerator.NULL;
			}
			elevation = ""+new Double(line[7].trim()).intValue();
		}
		
		// final actual data column is ignored
		type = AWIPSCommonObsSQLGenerator.SYNOPTIC;
	}
	
	/**
	 * Read types other than synoptic
	 * @param line
	 */
	private void makeSpatial(String[] line) {
		
		if (!line[0].equals("")) { 
			System.out.println(line[0]);
			if (line[0].equals(ZERO)) {
				wmo = AWIPSCommonObsSQLGenerator.NULL;
			}
			else {
				System.out.println("WMO: "+line[0]);
				wmo = line[0].substring(5);
			}
		}
		
		if (!line[1].equals("")) {
			System.out.println(line[1]);
			if (line[1].matches("\\d{5}")) {
				if (line[1].equals(wmo)) {
					icao = AWIPSCommonObsSQLGenerator.NULL;
				}
				else {
					wmo = MAROBS_PRE+line[1].trim();
				}
			}
			else {
				icao = line[1].trim();
			}
		}
		
		if (wmo.equals(AWIPSCommonObsSQLGenerator.NULL) && !icao.equals(AWIPSCommonObsSQLGenerator.NULL)) {
			setGid("null-"+icao);
		}
		else if (!wmo.equals(AWIPSCommonObsSQLGenerator.NULL) && icao.equals(AWIPSCommonObsSQLGenerator.NULL)) {
			setGid(wmo+"-null");
		}
		else {
			setGid(wmo+"-"+icao);
		}
		
		if (!line[3].equals("") && !line[2].equals("")) {
			// create a geometry
			double lat = new Double(line[2].trim()).doubleValue();
			double lon = new Double(line[3].trim()).doubleValue();
			System.out.println(lon+" "+lat);
			WKTReader wktReader = new WKTReader();
			WKBWriter wkbWriter = new WKBWriter();
			Point geometry = null;
			try {
				// reads X Y; or LON LAT, backwards to the file format
				geometry = (Point) wktReader.read("POINT ("+lon+" "+lat+")");
			} catch (ParseException pe) {
				pe.printStackTrace();
			}
		
			geom = WKBWriter.bytesToHex(wkbWriter.write(geometry));
		}
		
		if (!line[4].equals("")) {
			System.out.println(line[4].trim());
			if (line[4].equals("-0")) {
				elevation = AWIPSCommonObsSQLGenerator.NULL;
			}
			elevation = line[4].trim();
		}
		
		if (!line[5].equals("")) {
			System.out.println(line[5].trim());
			String line5 = line[5].trim();
			line5 = line5.replaceAll("'", "\\\\'");
			line5 = line5.replaceAll(";", ",");
			name = line5;
		}
		
		if (!line[6].equals("")) {
			System.out.println(line[6].trim());
			country = line[6].trim();
		}
		if (!line[7].equals("")) {
			System.out.println(line[7].trim());
			type = line[7].trim();
		}
	}
	
	public String getICAO() {
		return icao;
	}
	public void setICAO(String icao) {
		this.icao = icao;
	}
	public String getCountry() {
		return country;
	}
	public void setCountry(String country) {
		this.country = country;
	}
	public String getElevation() {
		return elevation;
	}
	public void setElevation(String elevation) {
		this.elevation = elevation;
	}
	public String getWmo() {
		return wmo;
	}
	public void setWmo(String wmo) {
		this.wmo = wmo;
	}
	public String getGid() {
		return gid;
	}
	public void setGid(String gid) {
		this.gid = gid;
	}
	public String getGeom() {
		return geom;
	}
	public void setGeom(String geom) {
		this.geom = geom;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getState() {
		return state;
	}
	public void setState(String state) {
		this.state = state;
	}
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
}
