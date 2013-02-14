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
package com.raytheon.edex.plugin.gfe.util;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;

/**
 * Singleton that assists with grid data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 5, 2008              njensen     Initial creation
 * Aug 22, 2008 1502        dglazesk    Changed to JAXB unmarshalling
 * Dec 06, 2012 1394        rjpeter     Attend static compiled patterns.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GridTranslator {

    private static final List<String> NEEDS_ZERO = Arrays.asList("BLD", "WBZ");

    private static final Pattern ANY_NUMBER = Pattern.compile("[0-9]");

    private static final Pattern NOT_NUMBER = Pattern.compile("[^0-9]");

    private GridTranslator() {
    }

    public static String getLevelName(String shorthand) {
        return ANY_NUMBER.matcher(shorthand).replaceAll("");
    }

    public static double[] getLevelValue(String shorthand) {
        String levelString = NOT_NUMBER.matcher(shorthand).replaceAll("");
        int length = levelString.length();
        double[] retVal = new double[2];
        retVal[0] = Level.getInvalidLevelValue();
        retVal[1] = Level.getInvalidLevelValue();
        switch (length) {
        case 0:
            retVal[0] = 0.0;
            break;
        case 1:
        case 2:
            retVal[0] = Double.parseDouble(levelString);
            break;
        case 3:
            if (levelString.charAt(0) != '0') {
                retVal[0] = Double.parseDouble(levelString);
            } else {
                retVal[1] = Double.parseDouble(levelString);
                if (levelString.charAt(0) == '0') {
                    retVal[0] = 0.0;
                }
            }
            break;
        case 4:
            if (levelString.charAt(0) == '0') {
                retVal[0] = 0.0;
                retVal[1] = Double.parseDouble(levelString);
            } else if (levelString.charAt(0) == '1') {
                retVal[0] = Double.parseDouble(levelString);
            } else {
                retVal[0] = Double.parseDouble(levelString.substring(0, 2));
                retVal[1] = Double.parseDouble(levelString.substring(2, 4));
            }
            break;
        case 5:
            retVal[0] = Double.parseDouble(levelString.substring(0, 2));
            retVal[1] = Double.parseDouble(levelString.substring(2, 5));
            break;
        case 6:
            retVal[0] = Double.parseDouble(levelString.substring(0, 3));
            retVal[1] = Double.parseDouble(levelString.substring(3, 6));
            break;
        case 7:
            retVal[0] = Double.parseDouble(levelString.substring(0, 4));
            retVal[1] = Double.parseDouble(levelString.substring(4, 7));
            break;
        case 8:
            retVal[0] = Double.parseDouble(levelString.substring(0, 4));
            retVal[1] = Double.parseDouble(levelString.substring(4, 8));
            break;
        default:
            break;
        }
        return retVal;
    }

    /**
     * Formats short level name in GFE format
     * 
     * @param name
     * @param levelOne
     * @param levelTwo
     * @return short level name
     */
    public static String getShortLevelName(String name, double levelOne,
            double levelTwo) {
        StringBuilder tmp = new StringBuilder();

        if (name == null) {
            tmp.append(LevelFactory.UNKNOWN_LEVEL);
        } else {
            tmp.append(name);

            if ((levelOne != Level.getInvalidLevelValue())
                    && ((levelOne != 0)
                            || (levelTwo != Level.getInvalidLevelValue()) || NEEDS_ZERO
                            .contains(name))) {
                tmp.append(String.valueOf(Math.round(levelOne)));
            }
            if (levelTwo != Level.getInvalidLevelValue()) {
                tmp.append(String.valueOf(Math.round(levelTwo)));
            }
        }
        return tmp.toString();
    }

    public static void main(String[] args) {
        double[] testLevelsBL = { 0, 30, 60, 90, 120, 150, 180, -999999 };
        double[] testLevelsMB = { 1100, 1050, 1000, 950, 900, 850, 800, 750,
                700, 650, 600, 550, 500, 450, 400, 350, 300, 250, 200, 150,
                100, -999999 };

        String levelName;
        String level;
        double[] levels;
        double l1, l2;

        levelName = "BL";
        for (int i = 0; i < testLevelsBL.length; i++) {
            l1 = testLevelsBL[i];
            for (int j = i + 1; j < testLevelsBL.length; j++) {
                l2 = testLevelsBL[j];
                level = GridTranslator.getShortLevelName(levelName, l1, l2);
                levels = GridTranslator.getLevelValue(level);
                System.out.println(levelName
                        + " "
                        + l1
                        + " "
                        + l2
                        + ": "
                        + level
                        + " "
                        + Arrays.toString(levels)
                        + ((l1 == levels[0]) && (l2 == levels[1]) ? " passed"
                                : " failed"));
            }
        }

        levelName = "MB";
        for (int i = 0; i < testLevelsMB.length; i++) {
            l1 = testLevelsMB[i];
            for (int j = i + 1; j < testLevelsMB.length; j++) {
                l2 = testLevelsMB[j];
                level = GridTranslator.getShortLevelName(levelName, l1, l2);
                levels = GridTranslator.getLevelValue(level);
                System.out.println(levelName
                        + " "
                        + l1
                        + " "
                        + l2
                        + ": "
                        + level
                        + " "
                        + Arrays.toString(levels)
                        + ((l1 == levels[0]) && (l2 == levels[1]) ? " passed"
                                : " failed"));
            }
        }

        levelName = "SFC";
        l1 = 0;
        l2 = -999999;
        level = GridTranslator.getShortLevelName(levelName, l1, l2);
        levels = GridTranslator.getLevelValue(level);
        System.out.println(levelName
                + " "
                + l1
                + " "
                + l2
                + ": "
                + level
                + " "
                + Arrays.toString(levels)
                + ((l1 == levels[0]) && (l2 == levels[1]) ? " passed"
                        : " failed"));

        levelName = "BLD";
        l1 = 0;
        l2 = -999999;
        level = GridTranslator.getShortLevelName(levelName, l1, l2);
        levels = GridTranslator.getLevelValue(level);
        System.out.println(levelName
                + " "
                + l1
                + " "
                + l2
                + ": "
                + level
                + " "
                + Arrays.toString(levels)
                + ((l1 == levels[0]) && (l2 == levels[1]) ? " passed"
                        : " failed"));

        levelName = "WBZ";
        l1 = 0;
        l2 = -999999;
        level = GridTranslator.getShortLevelName(levelName, l1, l2);
        levels = GridTranslator.getLevelValue(level);
        System.out.println(levelName
                + " "
                + l1
                + " "
                + l2
                + ": "
                + level
                + " "
                + Arrays.toString(levels)
                + ((l1 == levels[0]) && (l2 == levels[1]) ? " passed"
                        : " failed"));
    }
}
