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
package gov.noaa.nws.ncep.edex.util.ncgrib;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Singleton that assists with grid data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 5, 2008              njensen     Initial creation
 * Aug 22, 2008 1502        dglazesk    Changed to JAXB unmarshalling
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class NcgridTranslator {

    private static NcgridTranslator instance;

    private HashMap<String, String> levelsMap = new HashMap<String, String>();

    private HashMap<String, String> reverseLevelsMap = new HashMap<String, String>();

    private static Log logger = LogFactory.getLog(NcgridTranslator.class);

    private NcgridTranslator() {
        getNcLevels();
    }

    public static synchronized NcgridTranslator getInstance() {
        if (instance == null) {
            instance = new NcgridTranslator();
        }

        return instance;
    }

    public String getLevelName(String shorthand) {
        return shorthand.replaceAll("[0-9]*", "");
    }

    // public String getLevelOneValue(String shorthand) {
    // return getLevelValue(shorthand, 1);
    // }
    //
    // public String getLevelTwoValue(String shorthand) {
    // return getLevelValue(shorthand, 2);
    // }

    public double[] getLevelValue(String shorthand) {
        String levelString = shorthand.replaceAll("[^0-9]", "");
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
            retVal[0] = Double.parseDouble(levelString);
            if (levelString.charAt(0) == '0') {
                retVal[1] = 0.0;
            }
            break;
        case 4:
            if (levelString.charAt(0) == '0') {
                retVal[0] = Double.parseDouble(levelString);
                retVal[1] = 0.0;
            } else if (levelString.charAt(0) == '1') {
                retVal[0] = Double.parseDouble(levelString);
            } else {
                retVal[0] = Double.parseDouble(levelString.substring(2, 4));
                retVal[1] = Double.parseDouble(levelString.substring(0, 2));
            }
            break;
        case 5:
            retVal[0] = Double.parseDouble(levelString.substring(2, 5));
            retVal[1] = Double.parseDouble(levelString.substring(0, 2));
            break;
        case 6:
            retVal[0] = Double.parseDouble(levelString.substring(3, 6));
            retVal[1] = Double.parseDouble(levelString.substring(0, 3));
            break;
        case 7:
            retVal[0] = Double.parseDouble(levelString.substring(4, 7));
            retVal[1] = Double.parseDouble(levelString.substring(0, 4));
            break;
        default:
            break;
        }
        return retVal;
    }

    public String getShortLevelName(String name, double levelOne,
            double levelTwo) {
        StringBuilder tmp = new StringBuilder();

        if (name == null) {
            tmp.append(LevelFactory.UNKNOWN_LEVEL);
        } else {
            tmp.append(name);

            if (levelTwo != Level.getInvalidLevelValue()) {
                tmp.append(String.valueOf(Math.round(levelTwo)));
            }
            if (levelOne != Level.getInvalidLevelValue() && levelOne != 0) {
                tmp.append(String.valueOf(Math.round(levelOne)));
            }
        }
        return tmp.toString();
    }

    private void getNcLevels() {

        String path = "";

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        try {
            path = pathMgr.getFile(edexStaticBase,
                    "ncgrid" + File.separator + "ncgribLevelsMap.xml")
                    .getCanonicalPath();
        } catch (IOException e) {
            logger.error(e);
        }

        // System.out.println ("ncep path=" + path);
        NcgridLevels nclevels = new NcgridLevels();
        try {
            nclevels = (NcgridLevels) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(path);
        } catch (SerializationException e) {
            logger.error("Error unmarshalling NcgridLevels from " + path, e);
        }
        levelsMap = nclevels.getMap();

        String key = null;
        String value = null;
        for (Iterator<String> iterator = levelsMap.keySet().iterator(); iterator
                .hasNext();) {
            key = iterator.next();
            value = levelsMap.get(key);
            reverseLevelsMap.put(value.toLowerCase(), key);
        }
    }
}
