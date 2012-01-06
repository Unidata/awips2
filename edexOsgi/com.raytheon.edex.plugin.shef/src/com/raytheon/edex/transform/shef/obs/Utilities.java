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
package com.raytheon.edex.transform.shef.obs;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2011            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public abstract class Utilities {

    public static final String OPT_TRUE = "T";
    
    public static final String OPT_FALSE = "F";
    
    public static final String IS_ASOS = "IS_ASOS";
    
    
    public static final float INDETERMINATE_PRECIP = -99.0f;

    // "A01", "A01A", "A02", "A02A", "AOA",
    // "A0A", "AO1", "AO1A", "AO2", "AO2A"
    public static final Pattern METAR_ASOS = Pattern.compile("\\s+A[0O][12](A?)(\\$|=)*\\s+");
    
    /** Regular expression for the 3 hour precipitation */
    public static final Pattern PRECIP_3HR_EXP = Pattern
            .compile("\\s6(\\d{3,4}|////)[^/]*");

    /** Regular expression for the 3 hour precipitation */
    public static final Pattern PRECIP_24HR_EXP = Pattern
            .compile("\\s7(\\d{3,4}|////)\\s");

    /**
     * 
     * @param remarks
     * @return
     */
    public static final boolean isAutoASOS(String remarks) {
        boolean isASOS = false;
        if((remarks != null)&&(remarks.length() > 3)) {
            Matcher m = METAR_ASOS.matcher(remarks);
            isASOS = m.find();
        }
        return isASOS;
    }
    
    /**
     * 
     * @param remarks
     * @return
     */
    public static final boolean isIndeterminate3_6HourPrecip(String remarks) {
        boolean indeterminate = false;
        if((remarks != null)&&(remarks.length() > 5)) {
            Matcher m = PRECIP_3HR_EXP.matcher(remarks);
            if(m.find()) {
                indeterminate = "////".equals(m.group(1));
            }
        }
        return indeterminate;
    }
    
    /**
     * 
     * @param remarks
     * @return
     */
    public static final boolean isIndeterminate24HourPrecip(String remarks) {
        boolean indeterminate = false;
        
        Matcher m = PRECIP_24HR_EXP.matcher(remarks);
        if(m.find()) {
            indeterminate = "////".equals(m.group(1));
        }
        return indeterminate;
    }
    
    public static final boolean isIn3HourWindow(Calendar c) {
        boolean inWindow = false;
        
        if(c != null) {
            int hrmnTime = c.get(Calendar.HOUR_OF_DAY) * 100;
            hrmnTime += c.get(Calendar.MINUTE);

            inWindow = (hrmnTime  >  231 && hrmnTime <  530 ) ||
            (hrmnTime  > 831 && hrmnTime < 1130 ) ||
            (hrmnTime  > 1431 && hrmnTime < 1730 ) ||
            (hrmnTime  > 2031 && hrmnTime < 2330 );
        }
        return inWindow;
    }
    
    /**
     * Determine if a given date time calendar is within a METAR
     * "6 hourly" time window.
     * @param c Date time calendar to check.
     * @return 
     */
    public static final boolean isIn6HourWindow_1(Calendar c) {
        boolean inWindow = false;
        
        if(c != null) {
            int hrmnTime = c.get(Calendar.HOUR_OF_DAY) * 100;
            hrmnTime += c.get(Calendar.MINUTE);

            inWindow = (hrmnTime  >  530 && hrmnTime <  610 ) ||
            (hrmnTime  > 1130 && hrmnTime < 1210 ) ||
            (hrmnTime  > 1730 && hrmnTime < 1810 ) ||
            (hrmnTime  > 2330 && hrmnTime < 2400 ) ||
            (hrmnTime  > 0000 && hrmnTime < 0010 );
        }
        return inWindow;
    }
    
    /**
     * Determine if a given date time calendar is within a METAR
     * "6 hourly" time window.
     * @param c Date time calendar to check.
     * @return 
     */
    public static final boolean isIn6HourWindow_2(Calendar c) {
        boolean inWindow = false;
        
        if(c != null) {
            int hrmnTime = c.get(Calendar.HOUR_OF_DAY) * 100;
            hrmnTime += c.get(Calendar.MINUTE);

            inWindow = (hrmnTime  >  531 && hrmnTime <  830 ) ||
            (hrmnTime  > 1131 && hrmnTime < 1430 ) ||
            (hrmnTime  > 1731 && hrmnTime < 2030 ) ||
            (hrmnTime  > 2331 && hrmnTime < 2400 ) ||
            (hrmnTime  >    0 && hrmnTime <  230 );
        }
        return inWindow;
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

        String trailingData =  " NAO2 RAB37"
        + "\r\r\n     CIG 007V011 SLP200 P0001 6//// 7//// T00390033 10044 20033"
        + "\r\r\n     53003 AO2A$=\r\r\b\003";
        
        // Gets the percipitation over the last 24 hours
        int precip = -9999;
        if(isIndeterminate24HourPrecip(trailingData)) {
            precip = -99;
        } else {
            Matcher matcher = PRECIP_24HR_EXP.matcher(trailingData);
            if (matcher.find()) {
                String s = matcher.group(1);
//                String s = trailingData.substring(matcher.start() + 1,
//                        matcher.end());
                try {
                    precip = Integer.parseInt(s);
                } catch (NumberFormatException nfe) {
                    if(isIndeterminate24HourPrecip(trailingData)) {
                        precip = -99;
                    }
                    // Nothing
                }
            }
        }
        System.out.println(precip);

//        matcher = METAR_ASOS.matcher(trailingData);
//        if(matcher.find()) {
//            System.out.println("Found ASOS");
//        }
//        
//        
//        matcher = PRECIP_3HR_EXP.matcher(" 6//// =");
//        if(matcher.find()) {
//            System.out.println(matcher.group(1));
//        }
        
        
//        Pattern p = Pattern.compile("\\s+A[0O][12](A?)(\\$|=)*\\s+");
//        Matcher m = p.matcher(" AO2A$=\r\r\n  ");
//        System.out.println("Found = " + m.find());
    }

}
