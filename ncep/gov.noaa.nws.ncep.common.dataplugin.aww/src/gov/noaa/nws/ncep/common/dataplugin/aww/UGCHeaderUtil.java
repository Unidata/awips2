package gov.noaa.nws.ncep.common.dataplugin.aww;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

// *** Copied from raytheon's class in com.raytheon.edex.warning. 
// TODO : get this moved to a common plugin.

public class UGCHeaderUtil {

    /** Another RE to Catch the Date portion of the UGC Header */
    private static final String DATESTR = "[0-9]{6}\\-";

    /** Parses a single county from a UGC Header, can be repeated */
    private static final String SINGLECOUNTY = "(?:([A-Z]{3}[0-9]{3})([\\-\\>]))|(?:([0-9]{3})[\\-\\>])";

    /**
     * Private utility method which will return an ArrayList of each zone
     * formatted from a UGC Header.
     * 
     * @param fips
     * @return
     */
    public static ArrayList<String> getUGCZones(String fips) {
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

        String currentState = "";
        String startOfSeries = "";
        pattern = Pattern.compile(SINGLECOUNTY);
        matcher = pattern.matcher(matchStr);
        while (matcher.find()) {
            if (matcher.group(1) != null && matcher.group(1).length() == 6) {
                currentState = matcher.group(1).substring(0, 3);
                rval.add(matcher.group(1));
                if (matcher.group(2).equals(">")) {
                    startOfSeries = matcher.group(1).substring(3, 6);
                }
            } else {
                rval.add(currentState + matcher.group(3));
                if (!startOfSeries.equals("")) {
                    int start = Integer.parseInt(startOfSeries);
                    int end = Integer.parseInt(matcher.group(3));
                    for (int i = start + 1; i < end; i++) {
                        String str = String.valueOf(i);
                        while (str.length() < 3) {
                            str = "0" + str;
                        }
                        rval.add(currentState + str);
                    }
                    startOfSeries = "";
                }
                if (matcher.group(2) != null && matcher.group(2).equals(">")) {
                    startOfSeries = matcher.group(1).substring(3, 6);
                }
            }
        }
        return rval;
    }

}

