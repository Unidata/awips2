/**
 *  Grib2varsTableLookup - A Java class to define some known 
 *  both g2varsncep1.tbl and g2varswmo2.tbl.
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/2010  	276		   L. Lin      Initial creation
 * 06/2011                 X. Guo      Added getVarGnam()
 * 07/2011                 X. Guo      Added getVarGnam4Grib1()
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.util.grib2vars;

import java.io.File;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;

public class Grib2VarsTableLookup {

	private static Grib2VarsTable grib2varsTbl = null;
	private static String name;
	private static String units;
	private static Integer scale;
	private static String gnam;
	private static float missing;

	public Grib2VarsTableLookup() {
	}

	public static synchronized String readG2varsTable() throws GribException {

		Grib2VarsTable myTbl = null;

		/*
		 * Gets all predefined found in the utility directory
		 */
		IPathManager pathMgr = PathManagerFactory.getPathManager();
		LocalizationContext commonStaticBase = pathMgr.getContext(
				LocalizationContext.LocalizationType.COMMON_STATIC,
				LocalizationContext.LocalizationLevel.BASE);

		String path = "";

		try {
			path = pathMgr.getFile(commonStaticBase,
					"ncgrid" + File.separator + "grib2vars.xml")
					.getCanonicalPath();

			// System.out.println (" grib2 vars table lookup path=" + path);
		} catch (Exception e) {
			throw new GribException(
					"Unable to unmarshal ncep grib2vars.xml file");
		}

		File varsFile = new File(path);
		try {
			if (varsFile.exists()) {
				myTbl = new Grib2VarsTable(path);
				setG2varsTable(myTbl);
				// System.out.println("Grib2varsTableLookup- read grib2vars.xml file="
				// + path);
			}
		} catch (Exception e) {
			throw new GribException("Unable to read ncep grib2vars file");
		}
		return path;
	}

	/**
	 * Given discipline, category, pid, and pdt, find g2vars entry in
	 * "grib2vars.xml" and set g2varsid, name, units, scale, gnam, and missing
	 * fields. .
	 * 
	 * @param discipline
	 *            , category, pid and pdt are integers.
	 * @return
	 * @throws GribException
	 * 
	 */
	public static int getG2varsId(int discipline, int category, int pid, int pdt)
			throws GribException {
		Grib2Vars g2vars = null;
		int g2varsid = -1;
		// Wrap decoding in a try block, in case of exception on xml.
		try {
			// Read in the stationNumber table XML if not exists
			if (grib2varsTbl == null) {
				readG2varsTable();
			}

			// Search station ID and return whole station record
			if (grib2varsTbl != null) {
				g2vars = grib2varsTbl.getGrib2Vars(discipline, category, pid,
						pdt);
			}

			if (g2vars != null) {
				g2varsid = g2vars.g2Varsid;
				name = g2vars.name;
				units = g2vars.units;
				gnam = g2vars.gnam;
				scale = g2vars.scale;
				missing = g2vars.missing;
			}
		} catch (Exception e) {
			// TODO: Use central error logging if this code is kept
			throw new GribException(
					"Error occurs while finding g2vars entry from g2vars.xml table");
		}

		return g2varsid;
	}

	/**
	 * Given discipline, category, pid, and pdt, find g2vars entry in
	 * "grib2vars.xml" and set g2varsid, name, units, g2varsscale, gnam, and
	 * missing fields. .
	 * 
	 * @param discipline
	 *            , category, pid and pdt are integers.
	 * @return
	 * @throws GribException
	 * 
	 */
	public static int getG2varsScale(int discipline, int category, int pid,
			int pdt) throws GribException {
		Grib2Vars g2vars = null;
		int g2scale = 0;
		// Wrap decoding in a try block, in case of exception on xml.
		try {
			// Read in the stationNumber table XML if not exists
			if (grib2varsTbl == null) {
				readG2varsTable();
			}

			// Search station ID and return whole station record
			if (grib2varsTbl != null) {
				g2vars = grib2varsTbl.getGrib2Vars(discipline, category, pid,
						pdt);
			}

			if (g2vars != null) {
				// g2Varsid = g2vars.g2Varsid;
				name = g2vars.name;
				units = g2vars.units;
				gnam = g2vars.gnam;
				g2scale = g2vars.scale;
				missing = g2vars.missing;
			}
		} catch (Exception e) {
			// TODO: Use central error logging if this code is kept
			throw new GribException(
					"Error occurs while finding g2vars entry from g2vars.xml table");
		}

		return g2scale;
	}

	/**
	 * @return the grib2varsTbl
	 */
	public static Grib2VarsTable getG2varsTable() {
		return grib2varsTbl;
	}

	/**
	 * @param Grib2VarsTable
	 *            the grib2varsTbl to set
	 */
	public static void setG2varsTable(Grib2VarsTable grib2varsTbl) {
		Grib2VarsTableLookup.grib2varsTbl = grib2varsTbl;
	}

	public static String getName() {
		return name;
	}

	public static void setName(String name) {
		Grib2VarsTableLookup.name = name;
	}

	public static String getVarGnam( int discipline, int category, int pid, int pdt) throws GribException {
		Grib2Vars g2vars = null;
		String varName="NONE";
		// Wrap decoding in a try block, in case of exception on xml.
		try {
			// Read in the stationNumber table XML if not exists
			if (grib2varsTbl == null) {
				readG2varsTable();
			}

			// Search station ID and return whole station record
			if (grib2varsTbl != null) {
				g2vars = grib2varsTbl.getGrib2Vars(discipline, category, pid,
						pdt);
			}

			if (g2vars != null) {
				// g2Varsid = g2vars.g2Varsid;
				varName = g2vars.gnam;
			}
		} catch (Exception e) {
			// TODO: Use central error logging if this code is kept
			throw new GribException(
					"Error occurs while finding g2vars entry from g2vars.xml table");
		}

		return varName;
	}
	
	public static String getVarGnam4Grib1( int discipline, int category, int pid) throws GribException {
		Grib2Vars g2vars = null;
		String varName="NONE";
		// Wrap decoding in a try block, in case of exception on xml.
		try {
			// Read in the stationNumber table XML if not exists
			if (grib2varsTbl == null) {
				readG2varsTable();
			}

			// Search station ID and return whole station record
			if (grib2varsTbl != null) {
				g2vars = grib2varsTbl.getGrib2Vars4Grib1(discipline, category, pid );
			}

			if (g2vars != null) {
				// g2Varsid = g2vars.g2Varsid;
				varName = g2vars.gnam;
			}
		} catch (Exception e) {
			// TODO: Use central error logging if this code is kept
			throw new GribException(
					"Error occurs while finding g2vars entry from g2vars.xml table");
		}

		return varName;
	}
	
	public static String getUnits() {
		return units;
	}

	public static void setUnits(String units) {
		Grib2VarsTableLookup.units = units;
	}

	public static Integer getScale() {
		return scale;
	}

	public static void setScale(Integer scale) {
		Grib2VarsTableLookup.scale = scale;
	}

	public static String getGnam() {
		return gnam;
	}

	public static void setGnam(String gnam) {
		Grib2VarsTableLookup.gnam = gnam;
	}

	public static float getMissing() {
		return missing;
	}

	public static void setMissing(float missing) {
		Grib2VarsTableLookup.missing = missing;
	}

}
