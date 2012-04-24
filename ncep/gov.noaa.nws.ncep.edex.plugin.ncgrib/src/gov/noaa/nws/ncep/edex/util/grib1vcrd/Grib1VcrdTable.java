package gov.noaa.nws.ncep.edex.util.grib1vcrd;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 */

public class Grib1VcrdTable {
	
	private static final String file = "ncgrid" + File.separator + "grib1vcrd.xml";
	
	private static Grib1VcrdList g1VcrdList;
	
	private static Map<Integer, Grib1Vcrd> vcordMap = null;
	
	private static void readGrib1VcrdTable( String xmlFilename ) throws SerializationException{

		g1VcrdList = (Grib1VcrdList)SerializationUtil.jaxbUnmarshalFromXmlFile(xmlFilename);
		
    }   
	
	/**
	 *  Search a g2Vcrd given a field, and search key value.
	 *  
	 * @param sf
	 * @param key
	 * @return Grib2Vcrd
	 */
	public static Grib1Vcrd getGrib1VcrdById (int vcrdid ) {
		
		
		synchronized (Grib1VcrdTable.class) {
			if (g1VcrdList == null) {
				System.out.println("INITIALIZINGVCRD!!!!!!");
				try {
					String filename = findVcrdTable();
					readGrib1VcrdTable(filename);
					createMapOfTable();
				} catch (Exception e) {
					e.printStackTrace();
					g1VcrdList = null;
					return null;
				}
			}
		}
		
		if ( vcordMap.containsKey(vcrdid) ) 
			return vcordMap.get(vcrdid);
		else
			return null;
	}

	private static void createMapOfTable() {

		vcordMap = new HashMap<Integer, Grib1Vcrd>();
		
		for ( Grib1Vcrd vcord : g1VcrdList.getGrib1Vcrd() ) {
			vcordMap.put(vcord.getVcrdid(), vcord);
		}
		
	}

	private static String findVcrdTable() throws IOException{
		String filename = null;
		
		IPathManager pathMgr = PathManagerFactory.getPathManager();
		LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
		
		filename = pathMgr.getFile( commonStaticBase, Grib1VcrdTable.file).getCanonicalPath();
    	
		return filename;
	}
	
}