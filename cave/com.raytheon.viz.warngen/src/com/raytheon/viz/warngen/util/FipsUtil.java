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
package com.raytheon.viz.warngen.util;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.warning.UGCZone;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.warngen.gis.AffectedAreas;
import com.raytheon.viz.warngen.gis.AffectedAreasComparator;
import com.raytheon.viz.warnings.DateUtil;

/**
 * FIPS / UGC Header Utility. This is currently used by the Cave plugins Warngen
 * and Warnings.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 6, 2008				bwoodle	    Initial creation
 * Dec 28 2012  DR15599     mgamazaychikov  Updated method getListCounties to fix the problem
 * 											with generated list of counties.
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class FipsUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FipsUtil.class);

    private static HashMap<String, String> fipsToState;

    private static String[][] abbrlist = new String[][] { { "02", "AK" },
            { "01", "AL" }, { "05", "AR" }, { "60", "AS" }, { "04", "AZ" },
            { "06", "CA" }, { "08", "CO" }, { "09", "CT" }, { "11", "DC" },
            { "10", "DE" }, { "12", "FL" }, { "13", "GA" }, { "69", "GU" },
            { "66", "GU" }, { "15", "HI" }, { "19", "IA" }, { "16", "ID" },
            { "17", "IL" }, { "18", "IN" }, { "20", "KS" }, { "21", "KY" },
            { "22", "LA" }, { "25", "MA" }, { "24", "MD" }, { "23", "ME" },
            { "26", "MI" }, { "27", "MN" }, { "29", "MO" }, { "28", "MS" },
            { "30", "MT" }, { "37", "NC" }, { "38", "ND" }, { "31", "NE" },
            { "33", "NH" }, { "34", "NJ" }, { "35", "NM" }, { "32", "NV" },
            { "36", "NY" }, { "39", "OH" }, { "40", "OK" }, { "41", "OR" },
            { "42", "PA" }, { "72", "PR" }, { "44", "RI" }, { "45", "SC" },
            { "46", "SD" }, { "47", "TN" }, { "48", "TX" }, { "49", "UT" },
            { "51", "VA" }, { "78", "VI" }, { "50", "VT" }, { "53", "WA" },
            { "55", "WI" }, { "54", "WV" }, { "56", "WY" } };

    /** Catch the Date portion of the UGC Header */
    private static final String DATEPATTERN = "\\-([0-9]{6}\\-)";

    /** Another RE to Catch the Date portion of the UGC Header */
    private static final String DATESTR = "[0-9]{6}\\-";

    private static final NumberFormat FIPS_FORMAT = new DecimalFormat("#000");

    /**
     * This method creates a correctly formatted UGC Header based on the passed
     * AffectedAreas. The 6 digit datestamp at the end of the header is
     * determined based on the passed endtime.
     * 
     * @param areas
     * @param endtime
     * @return
     */
    public static String getUgcLine(AffectedAreas[] areas, Date endtime,
            int interval) {
        // TODO: If changed, change parseCountyHeader as well to reverse
        StringBuffer rval = new StringBuffer();
        ArrayList<String> countiesOrZones = new ArrayList<String>();
        DateUtil du = new DateUtil();

        fipsToState = new HashMap<String, String>();
        for (String[] abbr : abbrlist) {
            fipsToState.put(abbr[0], abbr[1]);
        }

        ArrayList<AffectedAreas> sortedAreas = new ArrayList<AffectedAreas>();
        Collections.addAll(sortedAreas, areas);
        ArrayList<String> fields = new ArrayList<String>();
        if (Character.isDigit(areas[0].getFips().charAt(0))) {
            fields.add("parent");
            fields.add("fips");
        } else {
            fields.add("fips");
        }

        AffectedAreasComparator comparator = new AffectedAreasComparator(fields);
        Collections.sort(sortedAreas, comparator);

        for (AffectedAreas area : sortedAreas) {
            String ugc = null;
            if (Character.isDigit(area.getFips().charAt(0))) {
                ugc = fipsToState.get(area.getFips().substring(0, 2)) + "C"
                        + area.getFips().substring(2, 5);
            } else {
                ugc = area.getFips().substring(0, 2) + "Z"
                        + area.getFips().substring(area.getFips().length() - 3);
            }

            if (ugc != null && countiesOrZones.contains(ugc) == false) {
                countiesOrZones.add(ugc);
            }
        }

        rval.append(simplifyHeader(getUgc(countiesOrZones)));
        rval.append(du
                .format(endtime, new SimpleDateFormat("ddHHmm"), interval)
                + "-");
        return rval.toString();
    }

    /**
     * This method creates a correctly formatted UGC Header based on the passed
     * AffectedAreas. The 6 digit datestamp at the end of the header is
     * determined based on the passed endtime.
     * 
     * @param areas
     * @param endtime
     * @return
     */
    public static String getUgcLine(Set<UGCZone> ugcs, Date endtime,
            int interval) {
        StringBuffer rval = new StringBuffer();
        ArrayList<String> countiesOrZones = new ArrayList<String>();
        DateUtil du = new DateUtil();

        for (UGCZone ugc : ugcs) {
            if (countiesOrZones.contains(ugc.toString()) == false) {
                countiesOrZones.add(ugc.toString());
            }
        }

        rval.append(simplifyHeader(getUgc(countiesOrZones)));
        rval.append(du
                .format(endtime, new SimpleDateFormat("ddHHmm"), interval)
                + "-");
        return rval.toString();
    }

    public static Map<String, String[]> parseCountyHeader(String countyHeader) {
        Map<String, String[]> stateToIdMap = new HashMap<String, String[]>();
        // Remove new lines:
        String[] lines = countyHeader.split("[\n]");
        countyHeader = "";
        for (String line : lines) {
            countyHeader += line;
        }

        String[] ranges = countyHeader.split("[-]");
        List<String> curList = null;
        String curState = null;
        for (String range : ranges) {
            if (Character.isLetter(range.charAt(0))) {
                // we start with a letter, we have a new state!
                if (curState != null) {
                    // add previous state and list to map
                    stateToIdMap.put(curState,
                            curList.toArray(new String[curList.size()]));
                }
                curState = range.substring(0, 2);
                curList = new ArrayList<String>();
                range = range.substring(3);
            }
            if (curList != null) {
                if (range.contains(">")) {
                    // we have a range
                    String left = range.substring(0, 3);
                    String right = range.substring(4);
                    int start = Integer.valueOf(left);
                    int end = Integer.valueOf(right);
                    for (int val = start; val <= end; ++val) {
                        curList.add(FIPS_FORMAT.format(val));
                    }
                } else {
                    curList.add(range);
                }
            }
        }
        if (curState != null) {
            stateToIdMap.put(curState,
                    curList.toArray(new String[curList.size()]));
        }
        return stateToIdMap;
    }

    private static String simplifyHeader(String countyHeader) {
        String simplifiedCountyHeader = "";
        String[] lines = countyHeader.split("[\n]");
        countyHeader = "";
        for (String line : lines) {
            countyHeader += line;
        }
        String[] ugcList = countyHeader.split("[-]");
        int reference = -1;
        ArrayList<String> temp = new ArrayList<String>();
        for (String ugc : ugcList) {
            int fips = Integer.parseInt(ugc.substring(ugc.length() - 3));
            if (Character.isLetter(ugc.charAt(0))) {
                simplifiedCountyHeader = appendUgc(simplifiedCountyHeader, temp);
                temp.clear();
                temp.add(ugc);
            } else if (reference + 1 == fips) {
                temp.add(ugc);
            } else {
                simplifiedCountyHeader = appendUgc(simplifiedCountyHeader, temp);
                temp.clear();
                temp.add(ugc);
            }
            reference = fips;
        }

        return appendUgc(simplifiedCountyHeader, temp) + "-";
    }

    private static String appendUgc(String countyHeader,
            ArrayList<String> ugcList) {
        if (ugcList.isEmpty() == false) {
            if (ugcList.size() < 3) {

                for (String t : ugcList) {
                    countyHeader += (countyHeader.length() > 0 ? "-" : "") + t;
                }
            } else {
                countyHeader += (countyHeader.length() > 0 ? "-" : "")
                        + ugcList.get(0) + ">"
                        + ugcList.get(ugcList.size() - 1);
            }
        }
        return countyHeader;
    }

    /**
     * Private method to determine the UGC Header given an ArrayList of the
     * affected zones. This does not add the end time.
     * 
     * @param counties
     * @return
     */
    private static String getUgc(ArrayList<String> countiesorZones) {
        ArrayList<String> states = new ArrayList<String>();
        StringBuffer rval = new StringBuffer();

        int nlCounter = 0;
        for (String countyOrZone : countiesorZones) {
            if (!states.contains(countyOrZone.substring(0, 3))) {
                states.add(countyOrZone.substring(0, 3));
            }
        }

        for (String state : states) {
            rval.append(state);
            nlCounter += state.length();
            for (String countyOrZone : countiesorZones) {
                if (countyOrZone.substring(0, 3).equals(state)) {
                    rval.append(countyOrZone.substring(3) + "-");
                    nlCounter += 4;
                    if (nlCounter >= 60) {
                        nlCounter = 0;
                        rval.append("\n");
                    }
                }
            }
        }

        return rval.toString();
    }

    /**
     * This returns a FIPS header consisting of the FIPS zones which are in
     * "oldFips" but not "newFips"
     * 
     * @return Difference between zones in 2 FIPS headers
     */
    public static String getDifference(String oldFips, String newFips) {
        String rval = "";
        Pattern pattern = Pattern.compile(DATEPATTERN);
        Matcher matcher = pattern.matcher(newFips);
        String dateStr = "";
        if (matcher.find()) {
            dateStr = matcher.group(1);
        }
        ArrayList<String> oldCounties = getListCounties(oldFips);
        ArrayList<String> newCounties = getListCounties(newFips);
        ArrayList<String> difference = new ArrayList<String>();

        for (String county : oldCounties) {
            if (!newCounties.contains(county)) {
                difference.add(county);
            }
        }

        /*
         * DR15599 - use simplifyHeader to get the correct rval
         */
        if (difference.size() > 0) {
            rval = simplifyHeader(getUgc(difference));
        }
        rval = rval + dateStr;
        return rval;
    }

    /**
     * Private utility method which will return an ArrayList of each zone
     * formatted from a FIPS Header.
     * 
     * @param fips
     * @return
     */
    private static ArrayList<String> getListCounties(String fips) {
        ArrayList<String> rval = new ArrayList<String>();
        String matchStr = "";

        Pattern pattern = Pattern.compile(DATESTR);
        Matcher matcher = pattern.matcher(fips);
        if (matcher.find()) {
            matchStr = fips.substring(0, fips.length() - 7);
        } else {
            matchStr = fips;
        }
        if (!matchStr.endsWith("-")) {
            matchStr += "-";
        }

        /*
         * DR15599 - completely re-did how rval is calculated. 
         */
        String[] lines = matchStr.split("[\n]");
        matchStr = "";
        for (String line : lines) {
        	matchStr += line;
        }

        String[] ranges = matchStr.split("[-]");
        List<String> curList = null;
        String curState = null;
        for (String range : ranges) {
            if (Character.isLetter(range.charAt(0))) {
                /*
                 * range starts with a character - get the new 
                 * state or marine zone name
                 */
                if (curState != null) {
                    for (String zone: curList) {
                    	rval.add(curState+zone);
                    }
                }
                curState = range.substring(0, 3);
                curList = new ArrayList<String>();
                range = range.substring(3);
            }
            if (curList != null) {
                if (range.contains(">")) {
                    /*
                     * get the full range of zones/counties
                     */
                    String left = range.substring(0, 3);
                    String right = range.substring(4);
                    int start = Integer.valueOf(left);
                    int end = Integer.valueOf(right);
                    for (int val = start; val <= end; ++val) {
                        curList.add(FIPS_FORMAT.format(val));
                    }
                } else {
                    curList.add(range);
                }
            }
        }
        if (curState != null) {
            for (String zone: curList) {
            	rval.add(curState+zone);
            }
        }
        return rval;
    }

    /**
     * Returns a state abbreviation given the FIPS code
     * 
     * @return two letter state abbreviation
     */
    public static String getStateNameFromFips(String fips) {
        fipsToState = new HashMap<String, String>();
        for (String[] abbr : abbrlist) {
            fipsToState.put(abbr[0], abbr[1]);
        }
        String statefips = fips.substring(0, 2);
        return fipsToState.get(statefips);
    }

    public static boolean containsSameCountiesOrZones(Set<UGCZone> a,
            Set<UGCZone> b) {
        boolean rval = true;

        // Check one way...
        for (UGCZone z1 : a) {
            boolean containsThisItem = false;
            for (UGCZone z2 : b) {
                if (z1.toString().equals(z2.toString())) {
                    containsThisItem = true;
                }
            }
            if (!containsThisItem) {
                rval = false;
            }
        }

        // Check the other way...
        for (UGCZone z2 : a) {
            boolean containsThisItem = false;
            for (UGCZone z1 : b) {
                if (z1.toString().equals(z2.toString())) {
                    containsThisItem = true;
                }
            }
            if (!containsThisItem) {
                rval = false;
            }
        }

        return rval;
    }
}
