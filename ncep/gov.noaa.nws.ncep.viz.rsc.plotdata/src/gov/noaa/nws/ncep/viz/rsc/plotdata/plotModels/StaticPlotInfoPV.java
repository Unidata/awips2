package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.vividsolutions.jts.geom.Coordinate;
/**
 * copy from com.raytheon.viz.pointdata.StaticPlotInfoPV and changed to look up NC file
 * TODO : merge back with ratheons baseline
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/03/10                 ghull      incorporate TO11dr11 changes (bnList)
 * 
 * </pre>
 * 
 * @author BRock97
 * @version 1.0
 */

public class StaticPlotInfoPV {

	private static final String REGEX = "^\\s*(\\d+)\\s*(\\S+)\\s*(-?\\d+\\.\\d+)\\s*(-?\\d+\\.\\d+)\\s*(-?\\d+)\\s*(-?\\d+\\.\\d+)\\s*(\\S*)$";

	private final HashMap<String, SPIEntry> spiList;
	
	private final HashMap<Integer, SPIEntry> bnList;

	private final String filename;

	public class SPIEntry {
		public int blockNumber;

		public int elevation;

		public Coordinate latlon = new Coordinate();

		public double[] pixel = new double[2];

		public double distance;

		public String accessId;
	}

	protected StaticPlotInfoPV(String filename) {
		this.filename = filename;
		this.spiList = new HashMap<String, SPIEntry>();
		this.bnList = new HashMap<Integer, SPIEntry>();
	}

	public void setSPIEntry(String icao, int bn, double lat, double lon,
			int en, double dt, String ad) {
		SPIEntry entry = new SPIEntry();
		entry.blockNumber = bn;
		entry.distance = dt;
		entry.latlon.y = lat;
		entry.latlon.x = lon;
		entry.accessId = ad;
		entry.elevation = en;
		spiList.put(icao, entry);
		bnList.put(bn, entry);
	}

	public SPIEntry getSPIEntry(String icao) {
		SPIEntry spiEntry = null;
		if(this.spiList.containsKey(icao)){
	        spiEntry = this.spiList.get(icao);
	    } else {
	        try{
    	        Integer bnKey = Integer.parseInt(icao + "0");
    	        if (this.bnList.containsKey(bnKey)){
    	            spiEntry = this.bnList.get(bnKey);
    	        }
	        } catch(NumberFormatException e){
	            //icao is not an integer
	        }
	    }
	    return spiEntry;
	}

	public String getSPIFileName() {
		return this.filename;
	}

	public static StaticPlotInfoPV readStaticPlotInfoPV(String filename) {
		return readStaticPlotInfoPV(filename, false);
	}

	public static StaticPlotInfoPV readStaticPlotInfoPV(String filename,
			boolean isSPI) {

		File spiFile = null;
		if (isSPI) {
			spiFile = new File(filename);
		} else {
			/*
			 * comment out by M. Gao
			 */
//			spiFileName = new File( LocalizationManager.getInstance().getFilename("plotModelsDir") + File.separator + filename );
			spiFile = LocalizationManager.getInstance().getLocalizationFileDirectly(
					LocalizationResourcePathConstants.PLOTMODELS_DIR,
					filename);
		}
		BufferedReader input = null;
		StaticPlotInfoPV catalog = new StaticPlotInfoPV(filename);
		try {
			input = new BufferedReader(new FileReader(spiFile));
			String line = null;
			Pattern p = Pattern.compile(REGEX);
			int lineNum = 0;
			while ((line = input.readLine()) != null) {
				lineNum++;
				try {
					Matcher match = p.matcher(line);
					if (!match.find()) {
						System.out.println("Error parsing line "
										+ lineNum + " of file "
										+ spiFile.getAbsolutePath() + "\n"
										+ line);
						continue;
					}
					String icao = match.group(2);
					int bn = Integer.parseInt(match.group(1));
					double lat = Double.parseDouble(match.group(3));
					double lon = Double.parseDouble(match.group(4));
					int en = Integer.parseInt(match.group(5));
					double distance = Double.parseDouble(match.group(6));
					String accessId = match.group(7).length() > 0 ? match.group(7)
							: icao;
					catalog.setSPIEntry(icao, bn, lat, lon, en, distance, accessId);
				} catch (Exception e) {
					System.out.println("Error parsing line "
									+ lineNum + " of file "
									+ spiFile.getAbsolutePath() + "\n"
									+ line);
				}
			}
			input.close();
		} catch (FileNotFoundException ex) {
			ex.printStackTrace();
			return null;
		} catch (IOException ex) {
			ex.printStackTrace();
			return null;
		}
		return catalog;
	}

	public HashMap<String, SPIEntry> getSpiList() {
		return spiList;
	}
}
