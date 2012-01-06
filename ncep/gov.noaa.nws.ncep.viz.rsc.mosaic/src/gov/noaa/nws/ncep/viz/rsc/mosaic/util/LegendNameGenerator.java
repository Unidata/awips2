/*
 * gov.noaa.nws.ncep.viz.rsc.mosaic.util.LegendNameGenerator
 * 
 * 03-14-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.mosaic.util;

import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;
import gov.noaa.nws.ncep.viz.rsc.mosaic.rsc.RadarRadialResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.io.*;
import java.util.*;

import com.raytheon.uf.viz.core.drawables.IDescriptor;

/**
 * This class generates the local radar legend  text.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03-04-2011              G. Zhang     Initial creation
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public class LegendNameGenerator {
	
	/**
	 * read the text table of local radar info into a list.
	 * @param path:		directory/file path of the table.
	 * @return:			a list containing local radar info.
	 */
	public static List<String> parseTable(String path){
		
		java.util.logging.Logger LOG = java.util.logging.Logger.getAnonymousLogger();
		LOG.setLevel(java.util.logging.Level.INFO);
		
		List<String> list = new ArrayList<String>();
		
		if(path == null || path.isEmpty())
			return list;
		
		try{
			BufferedReader br = new BufferedReader(new FileReader(path));
			String line = null;
			while((line = br.readLine()) != null){
				list.add(line);
			}
		}catch(Exception e){
			LOG.info("___Error parsing file: "+e.getMessage());
		}
		
		return list;
	}
	
	public static String generateLocalName(String pcode){
		String  path = LocalizationManager.getInstance().getLocalizationFileNameDirectly(LocalizationResourcePathConstants.RADAR_RESOURCES_DIR, "localRadarLegendNames.txt");
		List<String> list = parseTable(path);//"res/legendNames.txt");
		
		StringBuilder sb = new StringBuilder();
		
		if(list == null || list.isEmpty())
			return sb.toString();
		
		for(String s : list){
			if(s.contains(pcode)){
				String[] ss = s.split(" 0 ");//only " 0 " NOT .50 etc
				sb.append(ss[0]);//first half
			}
		}
		
		return sb.toString();
	}
	
	public static String generateName(RadarRadialResource rrr, String pcode){
		
		StringBuilder sb = new StringBuilder();
		
		NCMapDescriptor map = (NCMapDescriptor)NmapUiUtils.getActiveNatlCntrsEditor().getActiveDisplayPane().getRenderableDisplay().getDescriptor();
		
		String time = map.getValidTime(map.getCurrentFrame());
		
		String[] stime = time.split(" ");
		if(stime.length > 1)
			time = stime[1];		
		String s = rrr.getTrueElevText();
		String sevenS = "            ";
		return sb.append(time).append(" ").append(generateLocalName(pcode)).append(" ").append(s.isEmpty() ? sevenS : s).toString();
		
	}
	
	public static String getLegendDeg(String ele){
		String sb = "";
		
		Map<String, String> map = new HashMap<String, String>();

//---------------------------------Temporary ONLY: 2011-03-14		
		map.put("0", "0.00 DEG");
		map.put("1", "0.50 DEG");
		map.put("2", "1.50 DEG");
		map.put("3", "2.50 DEG");
		map.put("4", "3.00 DEG");
		map.put("5", "3.50 DEG");
//---------------------------------End TODO
		
		if(ele==null || ( ! map.keySet().contains(ele) ))
			return sb;
		
		return map.get(ele);
	}

}
