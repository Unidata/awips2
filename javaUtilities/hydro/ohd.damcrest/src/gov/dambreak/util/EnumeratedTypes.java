package gov.dambreak.util;

/**
 * Insert the type's description here.
 * Creation date: (7/28/2003 10:15:58 AM)
 * @author:
 */
public class EnumeratedTypes {
	////////////////////////////////////////////////
	// NOTE: Please enter all code's in UPPER CASE
	////////////////////////////////////////////////
	private static String[] scenario = new String[] { "HF", "HN", "HS",
													  "MF", "MN", "MS",
													  "LF", "LN", "LS",
													  "NS" };	
	private static String[] scenario_english = new String[] { "High Fast (HF)",   "High Normal (HN)",  "High Slow (HS)",
															"Medium Fast (MF)", "Medium Normal (MN)", "Medium Slow (MS)",
															   "Low Fast (LF)",    "Low Normal (LN)",   "Low Slow (LS)",
															   "Not Specified"};
	
	private static String[] damType = new String[] { "0", "1", "2" };
	private static String[] damType_english = new String[] { "Earth", "Concrete Gravity", "Concrete Arch" };
	
	private static String[] xsType = new String[] {	"G1","G2","G3","G4",
													"M1","M2",
													"S1","S2",
													"P1",
													"R1" };
	private static String[] xsType_english = new String[] {	"G1 - GIS: 90 meter data","G2 - GIS: 30 meter data","G3 - GIS: 10 meter data","G4 - GIS: 1 meter data",
															"M1 - Manual: 7.5 degree minute","M2 - Manual: 30 x 60 minute",
															"S1 - Field: Direct Leveling","S2 - Field: GPS",
															"P1 - Parabolic",
															"R1 - Prismoidal" };

	// field type enumeration
	public static int FIELD_SCENARIO = 0;
	public static int FIELD_DAMTYPE = 1;
	public static int FIELD_XSTYPE = 2;
	public static int FIELD_DBUG = 3;
	private static java.lang.String[] dbug = new String[] { "0", "1", "2" };
	private static java.lang.String[] dbug_english = new String[] { "Off", "On", "System Level" };
/**
 * Given English Description, Return Code
 * Creation date: (7/28/2003 10:27:57 AM)
 * @return java.lang.String
 * @param english java.lang.String
 */
public static String getCode(int field, String english) {
	
	int found;
	if (field == FIELD_SCENARIO) {
		for (int i=0; i<scenario_english.length; i++)
			if (scenario_english[i].equals(english))
				return scenario[i];
	}
	else if (field == FIELD_DAMTYPE) {
		for (int i=0; i<damType_english.length; i++)
			if (damType_english[i].equals(english))
				return damType[i];
	}
	else if (field == FIELD_XSTYPE) {
		for (int i=0; i<xsType_english.length; i++)
			if (xsType_english[i].equals(english))
				return xsType[i];
	}
	else if (field == FIELD_DBUG) {
		for (int i=0; i<dbug_english.length; i++)
			if (dbug_english[i].equals(english))
				return dbug[i];
	}
	
	return "ERR: Mapping Not Found";
}
/**
 * Given Code, Return English Description
 * Creation date: (7/28/2003 10:19:30 AM)
 * @return java.lang.String
 * @param field int
 * @param code java.lang.String
 */
public static String getEnglish(int field, String code) 
{
	int found;
	if (field == FIELD_SCENARIO) 
    {
		for (int i=0; i<scenario.length; i++)
        {
			if (scenario[i].equals(code.toUpperCase()))
            {
                return scenario_english[i];
            }
        }
	}
	else if (field == FIELD_DAMTYPE) {
		for (int i=0; i<damType.length; i++)
			if (damType[i].equals(code.toUpperCase()))
				return damType_english[i];
	}
	else if (field == FIELD_XSTYPE) {
		for (int i=0; i<xsType.length; i++)
			if (xsType[i].equals(code.toUpperCase()))
				return xsType_english[i];
	}
	else if (field == FIELD_DBUG) {
		for (int i=0; i<dbug.length; i++)
			if (dbug[i].equals(code.toUpperCase()))
				return dbug_english[i];
	}
	
	return "ERR: Mapping Not Found for \"" + code + "\"";
}
/**
 * Return Array of English Descriptions
 * Creation date: (7/28/2003 10:29:27 AM)
 * @return java.lang.String[]
 * @param field int
 */
public static String[] getEnglishChoices(int field) {
	if (field == FIELD_SCENARIO)
		return scenario_english;
	else if (field == FIELD_DAMTYPE)
		return damType_english;
	else if (field == FIELD_XSTYPE)
		return xsType_english;
	else if (field == FIELD_DBUG)
		return dbug_english;
	
	return new String[] {"ERR: Mapping Not Found"};
}
}
