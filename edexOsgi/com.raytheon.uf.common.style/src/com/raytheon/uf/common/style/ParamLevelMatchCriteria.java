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

package com.raytheon.uf.common.style;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.style.level.Level;
import com.raytheon.uf.common.style.level.RangeLevel;
import com.raytheon.uf.common.style.level.SingleLevel;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2007            chammack    Initial Creation.	
 * Sep 21, 2007            njensen     Renamed to ImageMatchCriteria.
 * Mar 26, 2009      2086  jsanchez    Added creatingEntityNames.
 *                                      Updated the matches method.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "paramLevelMatches")
public class ParamLevelMatchCriteria extends MatchCriteria {

    @XmlElement(name = "parameter")
    private List<String> parameterNames = new ArrayList<String>();

    @XmlElementRef
    private List<Level> levels = new ArrayList<Level>();

    @XmlElement
    private boolean isLogarithmic;

    @XmlElement(name = "creatingEntity")
    private List<String> creatingEntityNames = new ArrayList<String>();

    /**
     * @return the parameterNames
     */
    public List<String> getParameterNames() {
        return parameterNames;
    }

    /**
     * @return the creatingEntityNames
     */
    public List<String> getCreatingEntityNames() {
        return creatingEntityNames;
    }

    /**
     * @param parameterNames
     *            the parameterNames to set
     */
    public void setParameterName(List<String> parameterNames) {
        this.parameterNames = parameterNames;
    }

    /**
     * @param creatingEntityNames
     *            the creatingEntityNames to set
     */
    public void setCreatingEntityNames(List<String> creatingEntityNames) {
        this.creatingEntityNames = creatingEntityNames;
    }

    /**
     * @return the levels
     */
    public List<Level> getLevels() {
        return levels;
    }

    /**
     * @param levels
     *            the levels to set
     */
    public void setLevels(List<Level> levels) {
        this.levels = levels;
    }

    /**
     * @param level
     *            the level to set
     */
    public void setLevel(Level level) {
        this.levels.clear();
        this.levels.add(level);
    }

    /**
     * @return the isLogarithmic
     */
    public boolean isLogarithmic() {
        return isLogarithmic;
    }

    /**
     * @param isLogarithmic
     *            the isLogarithmic to set
     */
    public void setLogarithmic(boolean isLogarithmic) {
        this.isLogarithmic = isLogarithmic;
    }

    @Override
    public int matches(MatchCriteria criteria) throws StyleException {
        int returnValue = -1;
        if (criteria instanceof ParamLevelMatchCriteria) {
            ParamLevelMatchCriteria imgCriteria = (ParamLevelMatchCriteria) criteria;
            if (this.parameterNames.size() != 1) {
                throw new StyleException(
                        "ParamLevelMatchCriteria requires single parameter to search for match against.");
            }
            String paramName = this.parameterNames.get(0);

            Level level = null;
            if (this.levels.size() == 1) {
                level = levels.get(0);
            } else if (this.levels.size() > 1) {
                throw new StyleException(
                        "ParamLevelMatchCriteria does not support matching against multiple levels.");
            }

            boolean paramMatches = imgCriteria.getParameterNames().contains(
                    paramName);
            boolean entityMatches = true;
            if (!this.creatingEntityNames.isEmpty()
                    && !imgCriteria.creatingEntityNames.isEmpty()) {
                // If we both specifiy an entity and it doesn't match then this
                // rule doesn't match
                List<String> matchingEntities = new ArrayList<String>(
                        imgCriteria.creatingEntityNames);
                matchingEntities.retainAll(this.creatingEntityNames);
                entityMatches = !matchingEntities.isEmpty();
            }
            if (entityMatches && paramMatches) {
                if (imgCriteria.getLevels().size() == 0) {
                    // if criteria has no level information, then it is
                    // default for that parameter if no stronger matches are
                    // found
                    returnValue = 1;

                    if (this.creatingEntityNames != null
                            && this.creatingEntityNames.size() > 0
                            && imgCriteria.getCreatingEntityNames().contains(
                                    this.creatingEntityNames.get(0))) {
                        returnValue++;
                    }
                } else if (level instanceof SingleLevel) {
                    for (Level imgLevel : imgCriteria.getLevels()) {
                        if (imgLevel instanceof RangeLevel) {
                            // if criteria is a range and the level falls within
                            // it
                            if (level.getType().equals(imgLevel.getType())) {
                                RangeLevel levelRange = (RangeLevel) imgLevel;
                                double levelValue = ((SingleLevel) level)
                                        .getValue();
                                if (levelValue >= levelRange.getLowerValue()
                                        && levelValue <= levelRange
                                                .getUpperValue()) {
                                    returnValue = 2;
                                    break;
                                }
                            }
                        } else if (imgLevel instanceof SingleLevel) {
                            if (level.getType().equals(imgLevel.getType())) {
                                SingleLevel argLevel = (SingleLevel) imgLevel;
                                SingleLevel thisLevel = (SingleLevel) level;
                                if (argLevel.getMeasure() == null) {
                                    // if criteria match on level types and the
                                    // rule
                                    // does not specify a distinct level value
                                    returnValue = 3;
                                    break;
                                } else if (thisLevel.getValue() == argLevel
                                        .getValue()) {
                                    // if criteria match exactly on levels
                                    returnValue = 4;
                                    break;
                                }
                            }
                        }

                    }
                }
            }
        }

        return returnValue;
    }
}
