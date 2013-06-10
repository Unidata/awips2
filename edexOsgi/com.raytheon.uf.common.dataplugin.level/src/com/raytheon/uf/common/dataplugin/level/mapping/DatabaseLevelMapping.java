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
package com.raytheon.uf.common.dataplugin.level.mapping;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Class defines a database level
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/16/2008   #3576      rjpeter     Initial version
 * 04/17/2013   #1913      randerso    Moved to common
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class DatabaseLevelMapping {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatabaseLevelMapping.class);

    @XmlAttribute
    private String unit = null;

    @XmlAttribute
    private String levelTwoValue = null;

    @XmlAttribute
    private String levelOneValue = null;

    @XmlAttribute
    private String levelName = null;

    public String getLevelName() {
        return levelName;
    }

    public void setLevelName(String levelName) {
        this.levelName = levelName;
    }

    public String getLevelOneValue() {
        return levelOneValue;
    }

    public void setLevelOneValue(String levelOneValue) {
        this.levelOneValue = levelOneValue;
    }

    public String getLevelTwoValue() {
        return levelTwoValue;
    }

    public void setLevelTwoValue(String levelTwoValue) {
        this.levelTwoValue = levelTwoValue;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    public List<Level> getLevels() throws CommunicationException {
        String[] levelOneValues = new String[0];
        String[] levelTwoValues = new String[0];

        if (levelOneValue != null) {
            levelOneValues = levelOneValue.split(",");
        }

        if (levelTwoValue != null) {
            levelTwoValues = levelTwoValue.split(",");
        }

        List<Level> rval = new ArrayList<Level>(levelOneValues.length);

        for (int i = 0; i < levelOneValues.length; i++) {
            double lvl1 = Level.getInvalidLevelValue();
            double lvl2 = Level.getInvalidLevelValue();

            if (levelOneValues.length > i && levelOneValues[i] != null
                    && levelOneValues[i].trim().length() > 0) {
                try {
                    lvl1 = Double.parseDouble(levelOneValues[i]);
                } catch (NumberFormatException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error parsing level information for database level ["
                                    + levelName + "]", e);
                    continue;
                }
            }

            if (levelTwoValues.length > i && levelTwoValues[i] != null
                    && levelTwoValues[i].trim().length() > 0) {
                try {
                    lvl2 = Double.parseDouble(levelTwoValues[i]);
                } catch (NumberFormatException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error parsing level information for database level ["
                                    + levelName + "]", e);
                    continue;
                }
            }

            // handle any aliasing etc
            Level level = LevelFactory.getInstance().getLevel(levelName, lvl1,
                    lvl2, unit);
            if (level != null) {
                rval.add(level);
            }
        }

        return rval;
    }
}
