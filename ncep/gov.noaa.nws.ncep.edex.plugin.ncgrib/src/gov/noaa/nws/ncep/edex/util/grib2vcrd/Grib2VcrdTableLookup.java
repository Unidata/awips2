/**
 *  Grib2vcrdTableLookup - A Java class to define some known 
 *  both g2vcrdncep1.tbl and g2vcrdwmo2.tbl.
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/2010  	276		   L. Lin      Initial creation
 * 06/2011                 X. Guo      Added getGnamByVcrdId(()
 *                                     getScaleByVcrdId()  
 * 07/2011                 X. Guo      Added getGrib2VcrdByGrib1VcrdId()   
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.util.grib2vcrd;

import java.io.File;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

public class Grib2VcrdTableLookup {
	
	private static Grib2VcrdTable grib2vcrdTbl=null;
	private static String name;
	private static String units;
	private static String scale;
	private static String gnam;

	public static synchronized String readG2vcrdTable () throws GribException{

		Grib2VcrdTable myTbl = null;

		/*
         * Gets all predefined found in the utility directory
         */
		IPathManager pathMgr = PathManagerFactory.getPathManager();
		LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext commonStaticSite = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.SITE);

        String path = "";
        String sitePath = "";

        try {
        	path = pathMgr.getFile(
        			commonStaticBase,
        			"ncgrid" + File.separator + "grib2vcrd.xml")
        			.getCanonicalPath();

        	//System.out.println (" grib2 vcrd table lookup path=" + path);

        	sitePath = pathMgr.getFile(
        			commonStaticSite,
        			PropertiesFactory.getInstance().getEnvProperties()
        			.getEnvValue("SITENAME")
        			+ File.separator
        			+ "ncgrid"
        			+ File.separator
        			+ "g2vcrd.xml").getCanonicalPath();
        	//System.out.println (" grib2 vcrd table lookup sitepath=" + sitePath);
        } catch (Exception e) {
        	throw new GribException("Unable to unmarshal ncep grib2vcrd.xml file");
        }

        File modelFile = new File(path);
        try {
        	if (modelFile.exists()) {
        		myTbl = new Grib2VcrdTable(path);
        		setG2vcrdTable(myTbl);
        		//System.out.println("Grib2varsTableLookup- read grib2vars.xml file=" + path);
        	}
        } catch (Exception e) {
        	throw new GribException("Unable to read ncep grib2vcrd file");
        }
		return path;
	} 
    
    /**
     * Given discipline, category, pid, and pdt, find g2vcrd entry in "grib2vcrd.xml" 
     * and set g2vcrdid, name, units, scale, gnam, and missing fields.
     * .
     * 
     * @param discipline, category, pid and pdt are integers.
     * @return 
     * @throws GribException 
     *
     */
    public static int getG2vcrdId(int vcrdid1, int vcrdid2) throws GribException {
    	Grib2Vcrd g2vcrd=null;
    	int g2vcrdid = -1;
    	//  Wrap decoding in a try block, in case of exception on xml.
    	try {
    		

    		// Read in the stationNumber table XML if not exists
    		if (grib2vcrdTbl == null) {
    			readG2vcrdTable();
    		}
    		// Search station ID and return whole station record
    		if (grib2vcrdTbl != null) {
    			g2vcrd = grib2vcrdTbl.getGrib2Vcrd(vcrdid1, vcrdid2);
    		}
    		
			if ( g2vcrd != null) {
				g2vcrdid = g2vcrd.g2Vcrdid;
				name = g2vcrd.name;
				units = g2vcrd.units;
				gnam = g2vcrd.gnam;
				scale = g2vcrd.scale;
			} 
    	}
    	catch (Exception e) {
    		//TODO:  Use central error logging if this code is kept
    		throw new GribException("Error occurs while finding g2vcrd entry from g2vcrd.xml table");
    	}
    	
    	return g2vcrdid;
	}
	
    /**
     * @return the grib2vcrdTbl
     */
	public static Grib2VcrdTable getG2vcrdTable() {
		return grib2vcrdTbl;
	}
	
	/**
     * @param Grib2VcrdTable
     *            the grib2vcrdTbl to set
     */
	public static void setG2vcrdTable(Grib2VcrdTable grib2vcrdTbl) {
		Grib2VcrdTableLookup.grib2vcrdTbl = grib2vcrdTbl;
	}

	public static String getName() {
		return name;
	}

	public static void setName(String name) {
		Grib2VcrdTableLookup.name = name;
	}

	public static String getGnamByVcrdId ( int vcrdid1, int vcrdid2) throws GribException {
    	Grib2Vcrd g2vcrd=null;
    	String gvrdName = "NONE";
    	//  Wrap decoding in a try block, in case of exception on xml.
    	try {
    		

    		// Read in the stationNumber table XML if not exists
    		if (grib2vcrdTbl == null) {
    			readG2vcrdTable();
    		}
    		// Search station ID and return whole station record
    		if (grib2vcrdTbl != null) {
    			g2vcrd = grib2vcrdTbl.getGrib2Vcrd(vcrdid1, vcrdid2);
    		}
    		
			if ( g2vcrd != null) {
				gvrdName = g2vcrd.gnam;
				scale = g2vcrd.scale;
			} 
    	}
    	catch (Exception e) {
    		//TODO:  Use central error logging if this code is kept
    		throw new GribException("Error occurs while finding g2vcrd entry from g2vcrd.xml table");
    	}
    	
		return gvrdName;
	}

	public static Grib2Vcrd getGrib2VcrdByGrib1VcrdId ( int vcrdid ) throws GribException {
    	Grib2Vcrd g2vcrd=null;
    	//  Wrap decoding in a try block, in case of exception on xml.
    	try {
    		

    		// Read in the stationNumber table XML if not exists
    		if (grib2vcrdTbl == null) {
    			readG2vcrdTable();
    		}
    		// Search station ID and return whole station record
    		if (grib2vcrdTbl != null) {
    			g2vcrd = grib2vcrdTbl.getGrib2VcrdByVcrd1(vcrdid);
    		}
    	}
    	catch (Exception e) {
    		//TODO:  Use central error logging if this code is kept
    		throw new GribException("Error occurs while finding g2vcrd entry from g2vcrd.xml table");
    	}
    	
		return g2vcrd;
	}
	
	public static String getScaleByVcrdId ( int vcrdid1, int vcrdid2) throws GribException {
    	Grib2Vcrd g2vcrd=null;
    	//  Wrap decoding in a try block, in case of exception on xml.
    	try {
    		

    		// Read in the stationNumber table XML if not exists
    		if (grib2vcrdTbl == null) {
    			readG2vcrdTable();
    		}
    		// Search station ID and return whole station record
    		if (grib2vcrdTbl != null) {
    			g2vcrd = grib2vcrdTbl.getGrib2Vcrd(vcrdid1, vcrdid2);
    		}
    		
			if ( g2vcrd != null) {
				scale = g2vcrd.scale;
			} 
    	}
    	catch (Exception e) {
    		//TODO:  Use central error logging if this code is kept
    		throw new GribException("Error occurs while finding g2vcrd entry from g2vcrd.xml table");
    	}
    	
		return scale;
	}
	
	public static String getUnits() {
		return units;
	}

	public static void setUnits(String units) {
		Grib2VcrdTableLookup.units = units;
	}

	public static String getScale() {
		return scale;
	}

	public static void setScale(String scale) {
		Grib2VcrdTableLookup.scale = scale;
	}

	public static String getGnam() {
		return gnam;
	}

	public static void setGnam(String gnam) {
		Grib2VcrdTableLookup.gnam = gnam;
	}

}
