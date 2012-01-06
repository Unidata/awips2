package gov.noaa.nws.ncep.viz.gempak.util;

import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

import com.raytheon.uf.viz.core.exception.VizException;

public class DatatypeTable {
	
	private static String[] COLUMN_NAMES;
	static {
		COLUMN_NAMES = new String[] { 
				"PATH", "TEMPLATE", "CATEGORY", "SUBCAT", "NUMBER OF FRAMES",
				"RANGE", "INTERVAL", "BIN HOURS", "TIME MATCH"
				};
	}
	
	public static final Map<String, Map<String, String>> DATATYPE_MAP = createDatatypeMap();
	
	public final static String datatypeTableFilename = "datatypeTable";
		
	/**
	 * @return
	 */
	public static String getDatatypeTableFile() {
		return LocalizationManager.getInstance().getFilename(datatypeTableFilename); 
    }
	
	/**
	 * Reads into memory GEMPAK datatype.tbl
	 * @return
	 */
	private static Map<String, Map<String,String>> createDatatypeMap() {
        Map<String, Map<String,String>> theMap = new HashMap<String, Map<String, String>>();
        File file = new File(getDatatypeTableFile());
		try {
			Scanner scanner = new Scanner(file);
			while (scanner.hasNextLine()) {
				String line = scanner.nextLine();
				String firstChar = line.substring(0,1);
				if ( firstChar.equalsIgnoreCase("!")) {
					continue;
				}
				else {
					Map<String,String> rowMap= new HashMap<String,String>();
					rowMap.put(COLUMN_NAMES[0], line.substring(13,  37 ).replaceAll("\\s", "")); // path
					rowMap.put(COLUMN_NAMES[1], line.substring(39,  86 ).replaceAll("\\s", "")); // template
					rowMap.put(COLUMN_NAMES[2], line.substring(88,  95 ).replaceAll("\\s", "")); // category
					rowMap.put(COLUMN_NAMES[3], line.substring(97,  105).replaceAll("\\s", "")); // subcat
					rowMap.put(COLUMN_NAMES[4], line.substring(106, 110).replaceAll("\\s", "")); // def #frm
					rowMap.put(COLUMN_NAMES[5], line.substring(111, 117).replaceAll("\\s", "")); // def range
					rowMap.put(COLUMN_NAMES[6], line.substring(118, 124).replaceAll("\\s", "")); // def intrvl
					rowMap.put(COLUMN_NAMES[7], line.substring(125, 146).replaceAll("\\s", "")); // bin hrs
					rowMap.put(COLUMN_NAMES[8], line.substring(147, 153).replaceAll("\\s", "")); // time match
					theMap.put(line.substring(0, 11).replaceAll("\\s", ""), rowMap);
				}
			}
			scanner.close();
		} catch (FileNotFoundException e) {
			return null;
		}
        Map<String, Map<String, String>> unmodifiableMap = Collections.unmodifiableMap(theMap);
		return unmodifiableMap;
    }
	
	/**
	 * returns the value from DATATYPE_TBL for a given column of an alias
	 * @param anAlias
	 *        eg "GFS"
	 * @param aColumnName
	 *        eg "PATH"
	 * @return
	 * @throws VizException
	 */
	public static String getDatatypeTblColumnValue ( String anAlias, String aColumnName ) 
	throws VizException {
		if ( DATATYPE_MAP == null ) {
			throw new VizException ("DATATYPE_MAP has not been initialized");
		}
		else {
			if ( DATATYPE_MAP.containsKey(anAlias)) {
				if ( DATATYPE_MAP.get(anAlias).containsKey(aColumnName)) {
					return DATATYPE_MAP.get(anAlias).get(aColumnName);
				}
				else {
					throw new VizException ("Column " + aColumnName + 
							" not found in DATATYPE_MAP for " + anAlias);
				}
			}
			else {
				throw new VizException ("Alias " + anAlias + 
						" not found in DATATYPE_MAP");
			}
		}		
	}

}
