/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

/**
 * Utility methods for parsing coordinates from GML strings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class CoordinateUtil {

    private static final Pattern COORD_PATTERN = Pattern.compile(
            "[-+]?[0-9]*\\.?[0-9]+(\\s*,\\s*[-+]?[0-9]*\\.?[0-9]+)*",
            Pattern.MULTILINE);

    /**
     * Parses a formatted string that has space separated coordinates where the
     * coordinate values are comma separated. e.g (1,2 3,4)
     * 
     * @param str
     * @return
     * @throws ParseException
     */
    public static List<Double[]> parseCoordinates(String str)
            throws ParseException {
        List<Double[]> rval = new ArrayList<Double[]>();
        Matcher m = COORD_PATTERN.matcher(str);
        if (!m.find()) {
            throw new ParseException("Invalid coordinates string: " + str, 0);
        }
        String[] values = StringUtils.split(m.group(0), ", \t\n");
        int dims = values.length;
        do {
            values = StringUtils.split(m.group(0), ", \t\n");
            if (values.length != dims) {
                throw new ParseException("Inconsistent dimension count: "
                        + m.group(0), m.start(0));
            }
            Double[] nums = new Double[dims];
            for (int i = 0; i < dims; ++i) {
                try {
                    nums[i] = Double.parseDouble(values[i]);
                } catch (Exception e) {
                    throw new ParseException(
                            "Unable to parse coordinate value: " + values[i],
                            m.start(0));
                }
            }
            rval.add(nums);
        } while (m.find());
        return rval;
    }

}
