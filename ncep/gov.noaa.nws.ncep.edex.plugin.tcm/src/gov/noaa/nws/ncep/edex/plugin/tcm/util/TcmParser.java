/*
 * 
 * TcmParser
 * 
 * This java class parses TCM (Tropical Cyclone Message) data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ----			------- 	----------- --------------------------
 * 06/2009		128			T. Lee		Creation
 * 07/2009		128			T. Lee		Migrated to TO11
 * 06/2010		128			T. Lee		Migrated to TO11DR11
 * May 14, 2014 2536        bclement    moved WMO Header to common
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.tcm.util;

import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmPositionWinds;
import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmRecord;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;


public class TcmParser {
	private static Scanner sc = null;
    private final Log logger = LogFactory.getLog(getClass());
	int basin;
	static MatchResult result;
    boolean forecast;
    Calendar save_time = null;
	String TPC = "MIAMI FL\\s+(AL|EP)(\\d{2})(\\d{4})";
	String CPHC = "HONOLULU HI\\s+(EP|CP)(\\d{2})(\\d{4})";
	
    /**
     * Default constructor
     */    
    public TcmParser() {
    }

    /**
     * Decode TCM report
     * 
     * @param bull the bulletin message
     * @param record
     */
    public  void processTcm (String bull, TcmRecord record) {   	
    	int  basin;
    	forecast  = false;
    	String bull_partA, bull_partB;

    	/*
    	 * JTWC headers.
    	 */
    	String JTWC = "NAVMARFCSTCEN PEARL HARBOR HI/JTWC";
    	String jtwc_header = "[A-Z]{4}\\d{0,2} PGTW (\\d{6})\r\r\n";
    	Pattern pt_jtwc = Pattern.compile(jtwc_header);
    	Matcher mt_jtwc = pt_jtwc.matcher(bull);
    	
    	/*
    	 * TPC header.
    	 */
    	Pattern pt_tpc  = Pattern.compile (TPC);
    	Matcher mt_tpc  = pt_tpc.matcher(bull);
    	
    	/*
    	 * Central Pacific header.
    	 */
    	Pattern pt_cphc = Pattern.compile (CPHC);
    	Matcher mt_cphc = pt_cphc.matcher(bull);

    	try {     
    		if (bull.contains(JTWC) || mt_jtwc.find()) { 			
    			bull_partA = bull.substring(0,bull.indexOf("FORECASTS:"));
    			bull_partB = bull.substring(bull.indexOf("FORECASTS:"),bull.length());
    			sc = new Scanner(bull_partA);
    			basin = 2; 
        	} else {
        		try {
        			bull_partA = bull.substring(0,bull.indexOf("FORECAST VALID"));
        			bull_partB = bull.substring(bull.indexOf("FORECAST VALID"),bull.length());
        			sc = new Scanner(bull_partA);
        		} catch (Exception e){
        			bull_partA = bull;
        			bull_partB = null;
        			sc = new Scanner(bull);
        		}
        		if (mt_tpc.find()) {
        			basin = 1;
        		} else if (mt_cphc.find()) {
        			basin = 1;
        		} else {
        			throw new Exception (" Could not find forecast center!!");
        		}
        	}

    		/*
    		 * Find and decode storm ID, name and advisory number.
    		 */
    		processStormAttr(basin,record);

    		/*
    		 * Find and set correction.
    		 */
    	    processCorr(basin, record, bull);

    		/*
    		 * New TcmPositionWind record to hold current values
    		 */
    		TcmPositionWinds current = new TcmPositionWinds();
    		Calendar cal=null;
    		if (basin == 2) {
    			cal = (Calendar)record.getIssueTime().clone();
    			processLocationTime(basin,current,record,cal);
    		}
 
    		/*
    		 * Find and decode position accuracy.
    		 */
    		if (basin == 1) {
    			processPositionAccuracy(record);
    		}

       		/*
    		 * Find and decode storm motion.
    		 */
    		processStormMotion(basin,forecast,current);

    		/*
    		 * Find and decode position accuracy.
    		 */
    		if (basin==2) {
    			processPositionAccuracy(record);
    		}

    		/*
    		 * Find and decode minimum central pressure and eye size.
    		 */
    		if (basin == 1) {
    			processCentralPressure(record);
    			processEyeSize(record);
    		}

    		/*
    		 * Find and decode current wind speed and gusts.
    		 */
    		processMaxWind(basin,forecast,current);

    		/*
    		 * Find and decode current 64 knot wind radii.
    		 */
    		processRadii64(basin,forecast,current);

    		/*
    		 * Find and decode current 50 knot wind radii.
    		 */
    		processRadii50(basin,forecast,current);

    		/*
    		 * Find and decode current 34 knot wind radii.
    		 */
    		processRadii34(basin,forecast,current);

    		/*
    		 * Find and decode 12-ft seas.
    		 */
    		if (basin == 1) {
    			processWaveHeights(record);
    		}

    		/*
    		 * Find and decode storm location
    		 */
    		if (basin == 1) {
    			cal = (Calendar)record.getIssueTime().clone();
    			processLocationTime(basin,current,record,cal);
    		}
    		record.addPosWinds(current);

    		/*
    		 * Set past center location if exists.
    		 */
    		if (basin == 1) {
    			TcmPositionWinds past = new TcmPositionWinds();
    			cal = (Calendar)record.getIssueTime().clone();
    			processLocationTime(past,record,cal);
    			record.addPosWinds(past);
    		}

    		/*
    		 * Decode forecast message.
    		 */
    		processTcmForecasts(basin,bull_partB,record);
    	}  		
    	catch (Exception e) {
    		logger.info("Error in parsing TCM !!");
    	}
    }

    /**
     * Process and set WMO header
     * 
     * @param wmohd
     * @param record
     * @param mndTime
     */
    public void processWMO(byte[] wmohd, TcmRecord record, Calendar mndTime) {
    	WMOHeader hd = new WMOHeader (wmohd);
    	Calendar issueTime = UtilN.findDataTime(hd.getYYGGgg(),mndTime);
    	DataTime dt = new DataTime (issueTime);
    	if (wmohd != null) {
    		record.setIssueTime(issueTime);
    		record.setDataTime(dt);
    	} 
    }
    
    /**
     * Process storm motion
     * 
     * @param ocean basin
     * @param TcmPositionWinds
     */
    public void processStormAttr(int basin, TcmRecord record) {
    	
   		/*
		 * Storm attributes expression.
		 */
		String HU = "HURRICANE (\\w+) FORECAST/ADVISORY NUMBER\\s+(\\w+)";
   		String SUPER_TY = "SUPER TYPHOON (\\w+) \\((\\w+)\\) WARNING NR (\\d+)";
		String TY = "TYPHOON (\\w+) \\((\\w+)\\) WARNING NR (\\d+)";
		String TS = "TROPICAL STORM (\\w+) FORECAST/ADVISORY NUMBER\\s+(\\w+)";
		String TS_WP = "TROPICAL STORM (\\w+) \\((\\w+)\\) WARNING NR (\\d+)";
		String TC = "TROPICAL CYCLONE (\\w+) FORECAST/ADVISORY NUMBER\\s+(\\w+)";
		String TC_WP = "TROPICAL CYCLONE (\\w+) \\((\\w+)\\) WARNING NR (\\d+)";
		String TD = "TROPICAL DEPRESSION (\\S+) FORECAST/ADVISORY NUMBER\\s+(\\w+)";
		String TD_WP = "TROPICAL DEPRESSION (\\w+) \\((\\w+)\\) WARNING NR (\\d+)"; 
		String TD_SP = "TROPICAL DEPRESSION (\\S+) SPECIAL FORECAST/ADVISORY NUMBER\\s+(\\w+)";

		/*
		 * Find and set storm type, name and advisory from message
		 * 
		 * Atlantic/Central Pacific scenarios
		 */ 
		try {
			if (sc.findWithinHorizon(HU,0) != null) {
				record.setStormType ("HURRICANE");
			}
			else if (sc.findWithinHorizon(TS,0) != null) {
				record.setStormType ("TROPICAL STORM");
			}
			else if (sc.findWithinHorizon(TC,0) != null) {
				record.setStormType ("TROPICAL CYCLONE");
			}
			else if (sc.findWithinHorizon(TD,0) != null) {
				record.setStormType ("TROPICAL DEPRESSION");
			}
			else if (sc.findWithinHorizon(TD_SP,0) != null) {
				record.setStormType ("TROPICAL DEPRESSION SPECIAL");

			/* 
			 * NW Pacific scenarios
			 */
			} else if (sc.findWithinHorizon(SUPER_TY,0) != null) {
				record.setStormType ("SUPER TYPHOON");
			} else if (sc.findWithinHorizon(TY,0) != null) {
				record.setStormType ("TYPHOON");
			} else if (sc.findWithinHorizon(TC_WP,0) != null) {
				record.setStormType ("TROPICAL CYCLONE");
			} else if (sc.findWithinHorizon(TS_WP,0) != null) {
				record.setStormType ("TROPICAL STORM");
			} else if (sc.findWithinHorizon(TD_WP,0) != null) {
				record.setStormType ("TROPICAL DEPRESSION");	
			} else {
				throw new Exception("Could not find storm attributes!");
			}
		} catch (Exception e) {
			logger.info("Cannot find storm attributes");
		}

		result = sc.match();
		if (basin == 1) {
			record.setStormName (result.group(1));
			record.setAdvisoryNumber (result.group(2));   
			
			/*
			 * Find and decode storm name and basin.
			 */ 
			if (sc.findWithinHorizon(TPC,0) != null || 
					sc.findWithinHorizon(CPHC,0) != null) {      	     		
				result = sc.match();
				record.setBasin (result.group(1));
				record.setStormNumber (result.group(2));
			}

		} else if (basin == 2) {
			record.setStormNumber (result.group(1));
			record.setStormName (result.group(2));
			record.setAdvisoryNumber (result.group(3));
			record.setBasin ("WP");
		}
    }
    
    /**
     * Process correction
     * 
     * @param basin
     * @param TcmRecord
     * @param bulletin
     */
    public void processCorr(int basin, TcmRecord record, String bulletin) {
    	if ( bulletin == null ) {
    		return;
    	}
    	if (basin == 1) {
    		if (bulletin.contains("...CORRECTED")) {
    			record.setCorr(true);
    		} else {
    			record.setCorr(false);     			
    		}
    	} else if ( basin == 2 ) {
    		if (bulletin.contains("CORRECTED//")) {
    			record.setCorr(true);
    		} else {
    			record.setCorr(false);     			
    		}
    	}
    }

    /**
     * Process storm motion
     * 
     * @param ocean basin
     * @param forecast flag
     * @param TcmPositionWinds
     */
    public void processStormMotion(int basin, boolean forecast, TcmPositionWinds current) {
    	String DDSS = null;
    	if (basin == 1) {
    		DDSS = "PRESENT MOVEMENT TOWARD THE (\\w+) OR (\\d+) DEGREES AT\\s+(\\d+)\\s+KT";
    	} else if (basin == 2) {
    		if (forecast) {
    			DDSS = "VECTOR TO (\\d+) HR POSIT: (\\d+) DEG/\\s+(\\d+)\\s+KT";
    		} else {
    			DDSS = "MOVEMENT PAST (\\w+) HOURS - (\\d+)\\s+DEGREES AT\\s+(\\d+)\\s+KT";
    		}
    	}
    	if (sc.findWithinHorizon(DDSS, 0) != null) {
    		result = sc.match();
    		current.setStormDrct(Integer.parseInt(result.group(2)));
    		current.setStormSped(Integer.parseInt(result.group(3)));
    	}
    }

    /**
     * Process position accuracy
     * 
     * @param TcmRecord
     */
    public void processPositionAccuracy(TcmRecord record) {
   		String POSITION_ACCURACY = "POSITION ACCURATE TO WITHIN\\s+(\\d+)";
		if (sc.findWithinHorizon(POSITION_ACCURACY, 0) != null) {
			result = sc.match();
			record.setPositionAccuracy(Integer.parseInt(result.group(1)));
		} else {
			 POSITION_ACCURACY = "POSITION ACCURATE WITHIN\\s+(\\d+)";
             if (sc.findWithinHorizon(POSITION_ACCURACY, 0) != null) {
                     result = sc.match();
                     record.setPositionAccuracy(Integer.parseInt(result.group(1)));
             }
		}
    }
    
    /**
     * Process minimum central pressure
     * 
     * @param TcmRecord
     */
    public void processCentralPressure(TcmRecord record) {
    	String PMIN = "ESTIMATED MINIMUM CENTRAL PRESSURE\\s+(\\d+)\\sMB";
    	if (sc.findWithinHorizon(PMIN, 0) != null) {
    		result = sc.match();
    		record.setCentralPressure(Integer.parseInt(result.group(1)));
    	}
    }
    
    /**
     * Process eye size
     * 
     * @param TcmRecord
     */
    public void processEyeSize(TcmRecord record) {
    	String EYE = "EYE DIAMETER\\s+(\\d+)\\s+";
    	if (sc.findWithinHorizon(EYE, 0) != null) {
    		result = sc.match();
    		record.setEyeSize(Integer.parseInt(result.group(1)));	
    	}
    }

    /**
     * Process current sustained winds and gusts
     * 
     * @param ocean basin
     * @param TcmPositionWinds
     */
    public void processMaxWind(int basin, boolean forecast, TcmPositionWinds current) {
    	String wmax = null;
    	if (basin == 1) {
    		if (forecast) {
    			wmax = "MAX WIND\\s+(\\d+) KT...GUSTS\\s+(\\d+) KT";
    		} else {
    			wmax = "SUSTAINED WINDS\\s+(\\d+) KT WITH GUSTS TO\\s+(\\d+) KT";
    		}
    	} else if (basin == 2) {
    		wmax = "MAX SUSTAINED WINDS - (\\d+) KT, GUSTS\\s+(\\d+) KT";
    	}
    	if (sc.findWithinHorizon(wmax,0) != null) {
    		result = sc.match();
    		current.setWindMax(Integer.parseInt(result.group(1)));
    		current.setGust(Integer.valueOf(result.group(2)));
    	}      
    }
    
    /**
     * Process wind radii for 64 kts.
     * 
     * @param ocean basin
     * @param forecast flag
     * @param TcmPositionWinds
     */
    public void processRadii64(int basin, boolean fcst, TcmPositionWinds current) {
    	String KT64 = null;
    	if (basin == 1) {
    		if (fcst) {
    			KT64 = "64 KT...\\s*(\\d+)NE\\s+(\\d+)SE\\s+(\\d+)SW\\s+(\\d+)NW";
    		} else {
    			KT64 = "64 KT.......\\s*(\\d+)NE\\s+(\\d+)SE\\s+(\\d+)SW\\s+(\\d+)NW";
    		}
    	} else if (basin == 2) {
    		KT64 = "RADIUS OF 064 KT WINDS - " +
    			"(\\d+) NM NORTHEAST QUADRANT\\s+" +
    			"(\\d+) NM SOUTHEAST QUADRANT\\s+" +
    			"(\\d+) NM SOUTHWEST QUADRANT\\s+" +
    			"(\\d+) NM NORTHWEST QUADRANT";
    	}
   		if (sc.findWithinHorizon(KT64,0) != null) {
			result = sc.match();
			current.setNe64k(result.group(1));
			current.setSe64k(result.group(2));
			current.setSw64k(result.group(3));
			current.setNw64k(result.group(4));
		}
    }    
    
    /**
     * Process wind radii for 50 kts.
     * 
     * @param ocean basin
     * @param forecast flag
     * @param TcmPositionWinds
     */
    public void processRadii50(int basin, boolean fcst, TcmPositionWinds current) {
    	String KT50 = null;
    	if (basin == 1) {
    		if (fcst) {
    			KT50 = "50 KT...\\s*(\\d+)NE\\s+(\\d+)SE\\s+(\\d+)SW\\s+(\\d+)NW";
    		} else {
    			KT50 = "50 KT.......\\s*(\\d+)NE\\s+(\\d+)SE\\s+(\\d+)SW\\s+(\\d+)NW";
    		}
    	} else if (basin == 2) {
    		KT50 = "RADIUS OF 050 KT WINDS - " +
    			"(\\d+) NM NORTHEAST QUADRANT\\s+" +
    			"(\\d+) NM SOUTHEAST QUADRANT\\s+" +
    			"(\\d+) NM SOUTHWEST QUADRANT\\s+" +
    			"(\\d+) NM NORTHWEST QUADRANT";
    	}
   		if (sc.findWithinHorizon(KT50,0) != null) {
			result = sc.match();
			current.setNe50k(result.group(1));
			current.setSe50k(result.group(2));
			current.setSw50k(result.group(3));
			current.setNw50k(result.group(4));
		}
    }
    
    /**
     * Process wind radii for 34 kts.
     * 
     * @param ocean basin
     * @param forecast flag
     * @param TcmPositionWinds
     */
    public void processRadii34(int basin, boolean fcst, TcmPositionWinds current) {
    	String KT34 = null;
    	if (basin == 1) {
    		if (fcst) {
    			KT34 = "34 KT...\\s*(\\d+)NE\\s+(\\d+)SE\\s+(\\d+)SW\\s+(\\d+)NW";
    		} else {
    			KT34 = "34 KT.......\\s*(\\d+)NE\\s+(\\d+)SE\\s+(\\d+)SW\\s+(\\d+)NW";
    		}
    	} else if (basin == 2) {
    		KT34 = "RADIUS OF 034 KT WINDS - " +
    			"(\\d+) NM NORTHEAST QUADRANT\\s+" +
    			"(\\d+) NM SOUTHEAST QUADRANT\\s+" +
    			"(\\d+) NM SOUTHWEST QUADRANT\\s+" +
    			"(\\d+) NM NORTHWEST QUADRANT";
    	}
   		if (sc.findWithinHorizon(KT34,0) != null) {
			result = sc.match();
			current.setNe34k(result.group(1));
			current.setSe34k(result.group(2));
			current.setSw34k(result.group(3));
			current.setNw34k(result.group(4));
		}
    }
    
    /**
     * Process significant wave heights.
     * 
     * @param TcmRecord
     */
    public void processWaveHeights(TcmRecord record) {
    	String SEAS = "12 FT SEAS..\\s+(\\d+)NE\\s+(\\d+)SE\\s+(\\d+)SW\\s+(\\d+)NW";
    	String VARY = "SEAS VARY GREATLY IN EACH QUADRANT";
    	if (sc.findWithinHorizon(SEAS, 0) != null) {
    		result = sc.match();
    		record.setNe12ft(result.group(1));
    		record.setSe12ft(result.group(2));	
    		record.setSw12ft(result.group(3));	
    		record.setNw12ft(result.group(4));	
    	} else if (sc.findWithinHorizon(VARY, 0) != null) {
    		record.setNe12ft("vary");
    		record.setSe12ft("vary");	
    		record.setSw12ft("vary");	
    		record.setNw12ft("vary");       
    	}
    }
  
    /**
     * Process current storm location and valid time
     * 
     * @param ocean basin
     * @param TcmRecord
     * @param TcmPositionWinds
     */
    public void processLocationTime(int basin, TcmPositionWinds current,
    		TcmRecord record, Calendar cal) {
    	String SLOC = null;
    	if (basin == 1) {
    		SLOC = "CENTER LOCATED NEAR (\\d{0,2}.\\d{1})(N|S)\\s+(\\d{0,3}.\\d{1})" +
    			"(E|W) AT (\\d{1,2})/(\\d{2})(\\d{2})Z";
    		if (sc.findWithinHorizon(SLOC,0) != null) {
    			result = sc.match();
    			float lat = Float.parseFloat(result.group(1));
    			if (result.group(2).equals("S")) lat *= -1.0;
    			current.setClat(lat);
    			float lon = Float.parseFloat(result.group(3));
    			if (result.group(4).equals("W")) lon *= -1.0;
    			current.setClon(lon);

    			/*
    			 * Set valid time
    			 */
    			cal.set(Calendar.DAY_OF_MONTH, Integer.parseInt(result.group(5)));
    			cal.set(Calendar.HOUR_OF_DAY, Integer.parseInt(result.group(6)));
    			cal.set(Calendar.MINUTE, Integer.parseInt(result.group(7)));
 
    		}
    	} else if (basin == 2) {
    		SLOC = "(\\d{2})(\\d{2})(\\d{2})Z --- NEAR (\\d{0,2}.\\d{1})(N|S)\\s+" +
    			"(\\d{0,3}.\\d{1})(E|W)";
    		if (sc.findWithinHorizon(SLOC,0) != null) {
    			result = sc.match();
    			float lat = Float.parseFloat(result.group(4));
    			if (result.group(5).equals("S")) lat *= -1.0;
    			current.setClat(lat);
    			float lon = Float.parseFloat(result.group(6));
    			if (result.group(7).equals("W")) lon *= -1.0;
    			current.setClon(lon);

    			/*
    			 * set valid time
    			 */
    			cal.set(Calendar.DAY_OF_MONTH, Integer.parseInt(result.group(1)));
    			cal.set(Calendar.HOUR_OF_DAY, Integer.parseInt(result.group(2)));
    			cal.set(Calendar.MINUTE, Integer.parseInt(result.group(3)));
    		}
    	}   	
    	processRollOver(cal, record);
    	
    	/* 
    	 * Set the current time to main and position/wind table 
    	 */
        if (basin == 1) {
            current.setFcstHour("OBS");
            record.setObsTime(cal);
        } else if (basin == 2 ) {
            current.setFcstHour("F00");
            record.setObsTime(cal);
            save_time = (Calendar)cal.clone();
        }
        current.setValidTime(cal);
    }

    /**
     * Process past storm location and valid time
     * 
     * @param TcmPositionWinds
     * @param TcmRecord
     * @param Calendar
     */
    public void processLocationTime(TcmPositionWinds past,TcmRecord record,
    		Calendar cal) {
    	String PLOC = "AT (\\d{1,2})/(\\d{2})(\\d{2})Z CENTER WAS LOCATED NEAR " +
    		"(\\d{0,2}.\\d{1})(N|S)\\s+(\\d{0,3}.\\d{1})(E|W)";
    	if (sc.findWithinHorizon(PLOC,0) != null) {
    		result = sc.match();
    		float lat = Float.parseFloat(result.group(4));
    		if (result.group(5).equals("S")) lat *= -1.0;
    		past.setClat(lat);
    		float lon = Float.parseFloat(result.group(6));
    		if (result.group(7).equals("W")) lon *= -1.0;
    		past.setClon(lon);
    	}
    	cal.set(Calendar.DAY_OF_MONTH, Integer.parseInt(result.group(1)));
    	cal.set(Calendar.HOUR_OF_DAY, Integer.parseInt(result.group(2)));
    	cal.set(Calendar.MINUTE, Integer.parseInt(result.group(3)));
    	processRollOver (cal, record);
        past.setFcstHour("F00");
        save_time = (Calendar)cal.clone();
        past.setValidTime(cal);
    } 
    
    /**
     * Process roll over month and year.
     */
    public Calendar processRollOver(Calendar cal, TcmRecord record) {
    	if ( cal == null ) {
    		return null;
    	}
 
    	if (cal.get(Calendar.DATE) < record.getIssueTime().get(Calendar.DATE)) {
    		if (cal.get(Calendar.MONTH) != 11) {
    			cal.roll(Calendar.MONTH, +1);
    		}
    		else {
    			cal.set(Calendar.MONTH, 0);
    			cal.roll(Calendar.YEAR, +1);
    		}
    	} 
    	return cal;
    }
    
    /**
     * Decode TCM forecast section
     * 
     * @param bull The bulletin message
     * @param record
     */
    public void processTcmForecasts (int basin, String bull, TcmRecord record) {    	
    	TcmPositionWinds fcst;
    	Calendar cal;
    	MatchResult result;
    	forecast = true;
    	String DM = null;
    	if (bull == null) {
    		return;
    	}
    	if (basin == 1) {
    		DM = "\\r\\r\\n \\r\\r\\n";
    	} else if (basin == 2) {
    		DM = "( ---\\r\\n)|( ---\\n)";
    	}
    	Scanner cc = new Scanner(bull).useDelimiter(DM);
    	if (record == null) return;

    	/*
    	 * Loop through forecast section...
    	 */
    	while (cc.hasNext()) {
    		String next =  cc.next();
    		sc = new Scanner (next);
			fcst = new TcmPositionWinds();

    		/*
    		 * Find and decode valid time and storm position.
    		 */
    		String FV = null;
    		if (basin == 1) {
    			FV = "(FORECAST|OUTLOOK) VALID (\\d{2})/(\\d{2})(\\d{2})Z " +
    				"(\\d{0,2}.\\d{1})(N|S)\\s+(\\d{0,3}.\\d{1})(E|W)";
    		} else if (basin == 2) {
    			FV = "(\\w+) HRS, VALID AT:\\s+(\\d{2})(\\d{2})(\\d{2})Z --- " +
    				"(\\d{0,2}.\\d{1})(N|S)\\s+(\\d{0,3}.\\d{1})(E|W)";
    		}

    		if (sc.findWithinHorizon(FV,0) != null) {
    			result = sc.match();
    			fcst.setFcstHour("F"+result.group(1));

    			/*
    			 * Set valid time
    			 */
    			String sdate = null;
    			String shour = null;
    			String smin = null;
    			cal = (Calendar) record.getIssueTime().clone();
    			sdate = result.group(2);
    			shour = result.group(3);
    			smin = result.group(4);
    			cal.set(Calendar.DATE, Integer.parseInt(sdate));
    			cal.set(Calendar.HOUR_OF_DAY, Integer.parseInt(shour));
    			cal.set(Calendar.MINUTE, Integer.parseInt(smin));
    			if (cal.get(Calendar.DATE) < record.getIssueTime().get(Calendar.DATE)) {
    				if (cal.get(Calendar.MONTH) != 11) {
    					cal.roll(Calendar.MONTH, +1);
    				}
    				else {
    					cal.set(Calendar.MONTH, 0);
    					cal.roll(Calendar.YEAR, +1);
    				}
    			}   			
    			fcst.setValidTime(cal);
    			
    			/* 
    			 * Compute forecast hour for TPC
    			 */
    			if (basin == 1) {
    				long diff = ( cal.getTime().getTime() - 
    						save_time.getTime().getTime()) / (1000*60*60);
    				fcst.setFcstHour("F"+Long.toString(diff));  				
    			}

    			/*
    			 * Set forecast location
    			 */
    			String slat = null;
    			String slon = null;
    			String south = null;
    			String west = null;
    			slat = result.group(5);
    			south = result.group(6);
    			slon = result.group(7);
    			west = result.group(8);

    			float lat = Float.parseFloat(slat);
    			if (south.equals("S")) lat *= -1.0;
    			fcst.setClat(lat);
    			float lon = Float.parseFloat(slon);
    			if (west.equals("W")) lon *= -1.0;
    			fcst.setClon(lon);

    			/*
    			 * Find and decode forecast wind speed and gusts 
    			 */
    			processMaxWind(basin,forecast,fcst);

    			/*
    			 * Find and decode forecast 64 knot wind radii.
    			 */    
    			processRadii64(basin,forecast,fcst);

    			/*
    			 * Find and decode forecast 50 knot wind radii
    			 */
    			processRadii50(basin,forecast,fcst);

    			/*
    			 * Find and decode forecast 34 knot wind radii
    			 */
    			processRadii34(basin,forecast,fcst);

    	   		/*
        		 * Find and decode storm motion.
        		 */
    			if (basin == 2) {
    				processStormMotion(basin,forecast,fcst);
    			}

    			/*
    			 * Add forecast storm position and winds to set.
    			 */         
    			record.addPosWinds(fcst);
    		}
    	}
    }   	 
}