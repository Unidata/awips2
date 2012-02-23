package gov.noaa.nws.ncep.common.tools;

import java.util.Calendar;

import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * This software was a utility class developed by NCEP.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    	Engineer   	 Description
 * ------------ ---------- 	----------- 	--------------------------
 * Sep 29, 2009            	jkorman    	Initial creation
 * May 17, 2010			   	LLin		Modify Integer and float missing
 * 									   	identical to Raytheon's.
 * 4/2011					T. Lee		Added UAIR_INTEGER_MISSING
 * 7/2011					T. Lee		Added STORM_BULLSEPARATOR
 * 9/2011					B. Hebbard	Added CALENDAR_MISSING
 * 11/2011					S. Gurung	Added NEGATIVE_FLOAT_MISSING and NEGATIVE_INTEGER_MISSING
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface IDecoderConstantsN extends IDecoderConstants {

    /** Missing values */
    public static final Integer INTEGER_MISSING = IDecoderConstants.VAL_MISSING;
    public static final Float FLOAT_MISSING = 999999.f;
    public static final Float UAIR_FLOAT_MISSING = -9999.f;
    public static final Float NEGATIVE_FLOAT_MISSING = -9999.f;
    public static final Integer UAIR_INTEGER_MISSING = -9999;
    public static final Integer NEGATIVE_INTEGER_MISSING = -9999;
    public static final float GRID_MISSING = -999999.f;
    public static final Double DOUBLE_MISSING = -9999.0;
    public static final Calendar CALENDAR_MISSING = TimeTools.newCalendar(INTEGER_MISSING);
   
    /** FFG separator */
    public static final String FFG_BULLSEPARATOR = "\\d{3} \\r\\r\\n"+
            IDecoderConstants.WMO_HEADER + "(FFG[A-Z]{2}).\\r\\r\\n";
    
    /** Regular expression for FFG report */
    public static final String FFG_REPORT  = "([A-Z]{3}\\d{3})\\s{2}(.*)(\\r\\r\\n)";
    
    /** SCD separator */
    public static final String SCD_BULLSEPARATOR = "\\d{3} \\r\\r\\n"+
            IDecoderConstants.WMO_HEADER + "(SCD[A-Z]{2}).\\r\\r\\n";
    
    /** Regular expression for SCD report */
    public static final String SCD_REPORT  = "([A-Z]{4}) (SCD) ([A-Z]{3} )*(\\d{4})(.*)(\\r\\r\\n)" +
    		"(([0-9]{1}|/)(.*)(\\r\\r\\n))*";
    
    /** Regular expression for STORM_TRACK report */
    public static final String STORM_BULLSEPARATOR  = 
    		"(WP|IO|SH|CP|EP|AL|ML), +(\\d{1,2}|\\w{1,4}), +\\d{10}, +\\d{1,2}, +\\w{1,4}, +"
    		+"(-|\\d)\\d{0,2}, +\\d{1,3}(N|S| ), +\\d{1,4}(E|W|\\W), +.*\\x0a";
}
