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
package com.raytheon.viz.gfe.rsc;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GFELinePatterns {
    private static Map<String, Integer> patternMap;
    static {
        patternMap = new HashMap<String, Integer>();

        patternMap.put("SOLID", SWT.LINE_SOLID);

        patternMap.put("DASHED", SWT.LINE_DASH);

        patternMap.put("DOTTED", SWT.LINE_DOT);

        patternMap.put("DASHED_DOTTED", SWT.LINE_DASHDOT);
    }

    private static int getPattern(String name) {
        if (!patternMap.containsKey(name)) {
            throw new IllegalArgumentException("\"" + name
                    + "\" is not a valid GFEFillPattern.");
        }
        int pattern = patternMap.get(name);
        return pattern;
    }

    // TODO determine how GL Line Patterns work
    // public static int[] getGLPattern(String name) {
    // return getPattern(name);
    // }

    public static int getSWTPattern(String name) {
        return getPattern(name);
    }
}
