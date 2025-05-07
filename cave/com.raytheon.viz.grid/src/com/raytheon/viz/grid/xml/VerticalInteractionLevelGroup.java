/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.grid.xml;

import java.util.StringJoiner;

import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Group of levels that may be loaded on the fly for grid resources with the
 * specified master level, via the Vertical Interaction capability.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 06, 2024 2036517    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class VerticalInteractionLevelGroup {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VerticalInteractionLevelGroup.class);

    @XmlAttribute(required = true)
    private String masterLevel = "";

    @XmlAttribute(required = true)
    private String levels = "";

    @XmlAttribute(required = true)
    private String label = "";

    @XmlAttribute
    private String unit;

    public String getMasterLevel() {
        return masterLevel;
    }

    public void setMasterLevel(String masterLevel) {
        this.masterLevel = masterLevel;
    }

    public String getLevels() {
        return levels;
    }

    public void setLevels(String levels) {
        this.levels = levels;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    void afterUnmarshal(Unmarshaller unmarshaller, Object parent) {
        /*
         * JAXB automatically calls this after unmarshalling (see Unmarshaller
         * javadoc).
         */
        levels = normalizeLevelStr(levels, masterLevel, unit);
        unit = null;
    }

    /**
     * @param otherLevels
     *            comma-separated string of other levels that are for the same
     *            master level
     * @return true if this group's levels match the given levels, false
     *         otherwise
     */
    public boolean matchesLevels(String otherLevels) {
        otherLevels = normalizeLevelStr(otherLevels, masterLevel, null);
        return levels.equals(otherLevels);
    }

    /**
     * Get a normalized version of the given level string. This processes each
     * level value through LevelFactory so that each value is converted to the
     * master level's unit and rounded in a consistent way.
     *
     * @param levelStr
     *            comma-separated level values string to normalize
     * @param masterLevel
     *            master level of given levels
     * @param unit
     *            unit that given levels are in
     *
     * @return converted/normalized version of levelStr
     */
    private static String normalizeLevelStr(String levelStr, String masterLevel,
            String unit) {
        if (levelStr == null || levelStr.isBlank()) {
            return "";
        }
        StringJoiner normalizedLevelStr = new StringJoiner(",");
        for (String levelValue : levelStr.split(",")) {
            String msgOnError = "Error processing Vertical Interaction level: value='"
                    + levelValue + "', masterLevel='" + masterLevel
                    + "', unit='" + unit + "'";
            try {
                Level level = LevelFactory.getInstance().getLevel(masterLevel,
                        Double.parseDouble(levelValue), unit);
                if (level != null) {
                    normalizedLevelStr.add(level.getLevelOneValueAsString());
                } else {
                    statusHandler.error(msgOnError);
                }

            } catch (Exception e) {
                statusHandler.error(msgOnError, e);
            }
        }
        return normalizedLevelStr.toString();
    }
}