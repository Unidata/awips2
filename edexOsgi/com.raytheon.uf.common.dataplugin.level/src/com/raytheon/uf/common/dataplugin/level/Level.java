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
package com.raytheon.uf.common.dataplugin.level;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Level
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 03, 2009            rjpeter     Initial creation.
 * Dec 20, 2012           njensen   Added Level(String)
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
@Entity
@Table(name = "level", uniqueConstraints = @UniqueConstraint(columnNames = {
        "masterLevel_name", "levelonevalue", "leveltwovalue" }))
@SequenceGenerator(name = "LEVEL_GENERATOR", sequenceName = "level_seq", allocationSize = 1)
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
@XmlType(namespace = "dataplugin-level")
public class Level extends PersistableDataObject implements ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Level.class);

    public static final double INVALID_VALUE = -999999;

    public static final String INVALID_VALUE_AS_STRING = "" + INVALID_VALUE;

    private static final long serialVersionUID = 1L;

    private static final Pattern PATTERN = Pattern
            .compile("([0-9]*)((_([0-9]*))??([a-zA-Z]+))");

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "LEVEL_GENERATOR")
    @XmlAttribute
    @DynamicSerializeElement
    private long id;

    @ManyToOne(optional = false)
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    @DataURI(position = 0, embedded = true)
    private MasterLevel masterLevel;

    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    @DataURI(position = 1)
    private double levelonevalue = INVALID_VALUE;

    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    @DataURI(position = 2)
    private double leveltwovalue = INVALID_VALUE;

    private transient String levelInfo;

    private transient int hashCode;

    private transient boolean dirtyFlag = true;

    /**
     * Constructor
     */
    public Level() {

    }

    /**
     * Constructor
     * 
     * @param level
     */
    public Level(String level) {
        Matcher m = PATTERN.matcher(level);
        if (m.matches()) {
            String levelOne = m.group(1);
            String levelTwo = m.group(4);
            String name = m.group(5);

            levelonevalue = Double.parseDouble(levelOne);
            if (levelTwo != null) {
                leveltwovalue = Double.parseDouble(levelTwo);
            }
            masterLevel = new MasterLevel(name);
        }
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public boolean isLevelOneValid() {
        return INVALID_VALUE != levelonevalue;
    }

    public double getLevelonevalue() {
        return levelonevalue;
    }

    public String getLevelOneValueAsString() {
        return Double.toString(levelonevalue);
    }

    public void setLevelonevalue(double levelonevalue) {
        this.levelonevalue = levelonevalue;
        dirtyFlag = true;
    }

    public boolean isLevelTwoValid() {
        return INVALID_VALUE != leveltwovalue;
    }

    public double getLeveltwovalue() {
        return leveltwovalue;
    }

    public String getLevelTwoValueAsString() {
        return Double.toString(leveltwovalue);
    }

    public void setLeveltwovalue(double leveltwovalue) {
        this.leveltwovalue = leveltwovalue;
        dirtyFlag = true;
    }

    public MasterLevel getMasterLevel() {
        return masterLevel;
    }

    public void setMasterLevel(MasterLevel masterLevel) {
        this.masterLevel = masterLevel;
        dirtyFlag = true;
    }

    public String getLevelInfo() {
        if (dirtyFlag) {
            generateDependentFields();
        }
        return levelInfo;
    }

    public void setLevelInfo(String levelInfo) {
        if (levelInfo != null && levelInfo.trim().length() > 0) {
            String tokens[] = levelInfo.trim().split("_", 2);
            try {
                levelonevalue = new Double(tokens[0]);

                if (tokens.length > 1) {
                    leveltwovalue = new Double(tokens[1]);
                }
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Received invalid levelInfo [" + levelInfo + "]", e);
                levelonevalue = INVALID_VALUE;
                leveltwovalue = INVALID_VALUE;
            }
        } else {
            levelonevalue = INVALID_VALUE;
            leveltwovalue = INVALID_VALUE;
        }
        dirtyFlag = true;
    }

    private void generateDependentFields() {
        final int prime = 31;
        hashCode = 1;
        hashCode = prime * hashCode
                + ((masterLevel == null) ? 0 : masterLevel.hashCode());
        hashCode = prime * hashCode + new Double(levelonevalue).hashCode();
        hashCode = prime * hashCode + new Double(leveltwovalue).hashCode();

        if (INVALID_VALUE != levelonevalue || INVALID_VALUE != leveltwovalue) {
            StringBuilder tmp = new StringBuilder();
            if (INVALID_VALUE != levelonevalue) {
                tmp.append(levelonevalue);
            }
            if (INVALID_VALUE != leveltwovalue) {
                tmp.append("_");
                tmp.append(leveltwovalue);
            }

            levelInfo = tmp.toString();
        } else {
            levelInfo = null;
        }

        dirtyFlag = false;
    }

    public Level getUpperLevel() throws CommunicationException {
        Level rval = null;

        if (isRangeLevel()) {
            Progression prog = masterLevel.getProgression();
            if (prog != null) {
                String levelName = null;
                if (prog.toString().equals(masterLevel.getType())) {
                    levelName = masterLevel.getName();
                } else {
                    // has sub level definition
                    levelName = masterLevel.getType();
                }

                Level l1 = LevelFactory.getInstance().getLevel(levelName,
                        levelonevalue);
                Level l2 = LevelFactory.getInstance().getLevel(levelName,
                        leveltwovalue);

                switch (prog) {
                case INC:
                    rval = levelonevalue < leveltwovalue ? l2 : l1;
                    break;
                case DEC:
                    rval = levelonevalue < leveltwovalue ? l1 : l2;
                    break;
                }
            } else {
                // no progression data, return leveltwo
                rval = LevelFactory.getInstance().getLevel(
                        masterLevel.getName(), leveltwovalue);
            }
        }

        return rval;
    }

    /**
     * Determine a closest level from a list of levels for which data is
     * available
     * 
     * @param levels
     *            a list of other levels for which data is available
     * @param relationship
     *            to get upper use ABOVE, lower use BELOW
     * @return the closest level from the list
     */
    public Level getClosestLevel(List<Level> levels, CompareType relationship) {
        Level bestLevel = null;
        Double bestDistance = Double.MAX_VALUE;
        for (Level level : levels) {
            if (level.compare(this) == relationship) {
                Double distance = Math.abs(levelonevalue - level.levelonevalue);
                if (distance < bestDistance) {
                    bestLevel = level;
                    bestDistance = distance;
                }
            }
        }
        return bestLevel;
    }

    public Level getLowerLevel() throws CommunicationException {
        Level rval = null;

        if (isRangeLevel()) {
            Progression prog = masterLevel.getProgression();
            if (prog != null) {
                String levelName = null;
                if (prog.toString().equals(masterLevel.getType())) {
                    levelName = masterLevel.getName();
                } else {
                    // has sub level definition
                    levelName = masterLevel.getType();
                }

                Level l1 = LevelFactory.getInstance().getLevel(levelName,
                        levelonevalue);
                Level l2 = LevelFactory.getInstance().getLevel(levelName,
                        leveltwovalue);

                switch (prog) {
                case INC:
                    rval = levelonevalue < leveltwovalue ? l1 : l2;
                    break;
                case DEC:
                    rval = levelonevalue < leveltwovalue ? l2 : l1;
                    break;
                }
            } else {
                // no progression data, return levelone
                rval = LevelFactory.getInstance().getLevel(
                        masterLevel.getName(), levelonevalue);
            }
        }

        return rval;
    }

    public boolean isRangeLevel() {
        return isLevelOneValid() && isLevelTwoValid();
    }

    /**
     * Compares two levels. Nulls are checked and allowed.
     * 
     * @param that
     * @return
     */
    public CompareType compare(Level that) {
        CompareType rval = CompareType.INCOMPATIBLE;

        if (that != null) {
            if (this.masterLevel.isCompatible(that.masterLevel)) {
                Progression progression = this.masterLevel.getProgression();

                if (progression != null) {
                    // has a progression/value to check
                    double thisVal1 = this.levelonevalue;
                    double thatVal1 = that.levelonevalue;
                    double thisVal2 = this.leveltwovalue;
                    double thatVal2 = that.leveltwovalue;

                    // handle extraneous case of val1 not being defined and val2
                    // being defined, why would there be a top and no bottom?
                    if (!this.isLevelOneValid() && this.isLevelTwoValid()) {
                        thisVal1 = thisVal2;
                        thisVal2 = INVALID_VALUE;
                    }
                    if (!that.isLevelOneValid() && that.isLevelTwoValid()) {
                        thatVal1 = thatVal2;
                        thatVal2 = INVALID_VALUE;
                    }

                    if (thisVal1 != INVALID_VALUE && thatVal1 != INVALID_VALUE) {
                        CompareType v1to1Comp = compareValue(progression,
                                thisVal1, thatVal1);

                        switch (v1to1Comp) {
                        case BELOW: {
                            if (thisVal2 != INVALID_VALUE
                                    && thatVal2 != INVALID_VALUE) {
                                // this and that are layers
                                CompareType v2to2Comp = compareValue(
                                        progression, thisVal2, thatVal2);
                                switch (v2to2Comp) {
                                case BELOW: {
                                    rval = CompareType.BELOW;
                                    break;
                                }
                                case ABOVE: // fall through
                                case EQUAL: {
                                    rval = CompareType.CONTAINS;
                                    break;
                                }
                                }
                            } else if (thisVal2 != INVALID_VALUE) {
                                // this is a layer, that is a level
                                CompareType v2to1Comp = compareValue(
                                        progression, thisVal2, thatVal1);
                                switch (v2to1Comp) {
                                case BELOW: {
                                    rval = CompareType.BELOW;
                                    break;
                                }
                                case ABOVE: // fall through
                                case EQUAL: {
                                    rval = CompareType.CONTAINS;
                                    break;
                                }
                                }
                            } else {
                                // this is a level, that is a layer or
                                // this and that are levels
                                rval = CompareType.BELOW;
                            }
                            break;
                        }
                        case ABOVE: {
                            if (thisVal2 != INVALID_VALUE
                                    && thatVal2 != INVALID_VALUE) {
                                // this and that are layers
                                CompareType v2to2Comp = compareValue(
                                        progression, thisVal2, thatVal2);
                                switch (v2to2Comp) {
                                case ABOVE: {
                                    rval = CompareType.ABOVE;
                                    break;
                                }
                                case BELOW: // fall through
                                case EQUAL: {
                                    rval = CompareType.CONTAINEDBY;
                                    break;
                                }
                                }
                            } else if (thatVal2 != INVALID_VALUE) {
                                // this is a level, that is a layer
                                CompareType v1to2Comp = compareValue(
                                        progression, thisVal1, thatVal2);
                                switch (v1to2Comp) {
                                case ABOVE: {
                                    rval = CompareType.ABOVE;
                                    break;
                                }
                                case BELOW: // fall through
                                case EQUAL: {
                                    rval = CompareType.CONTAINEDBY;
                                    break;
                                }
                                }
                            } else {
                                // this is a layer, that is a level or
                                // this and that are levels
                                rval = CompareType.ABOVE;
                            }
                            break;
                        }
                        case EQUAL: {
                            if (thisVal2 != INVALID_VALUE
                                    && thatVal2 != INVALID_VALUE) {
                                // this and that are layers
                                CompareType v2to2Comp = compareValue(
                                        progression, thisVal2, thatVal2);
                                switch (v2to2Comp) {
                                case BELOW: {
                                    rval = CompareType.CONTAINEDBY;
                                    break;
                                }
                                case ABOVE: {
                                    rval = CompareType.CONTAINS;
                                    break;
                                }
                                case EQUAL: {
                                    rval = CompareType.EQUAL;
                                    break;
                                }
                                }
                            } else if (thisVal2 != INVALID_VALUE) {
                                // this is a layer, that is a level
                                rval = CompareType.CONTAINS;
                            } else if (thatVal2 != INVALID_VALUE) {
                                // this is a level, that is a layer
                                rval = CompareType.CONTAINEDBY;
                            } else {
                                // this and that are levels
                                rval = CompareType.EQUAL;
                            }
                            break;
                        }
                        }
                    } else if (thisVal1 != INVALID_VALUE) {
                        // this has no bottom/top, but that does, consider this
                        // all inclusive
                        rval = CompareType.CONTAINS;
                    } else if (thatVal1 != INVALID_VALUE) {
                        // that has no bottom/top, but this does, consider that
                        // all inclusive
                        rval = CompareType.CONTAINEDBY;
                    } else {
                        // both are empty, equal
                        rval = CompareType.EQUAL;
                    }
                } else {
                    // no progression
                    rval = CompareType.EQUAL;
                }
            }
        }

        return rval;
    }

    private static CompareType compareValue(Progression prog, double val1,
            double val2) {
        CompareType rval = CompareType.EQUAL;

        if (val1 < val2) {
            rval = (Progression.INC.equals(prog) ? CompareType.BELOW
                    : CompareType.ABOVE);
        } else if (val2 < val1) {
            rval = (Progression.INC.equals(prog) ? CompareType.ABOVE
                    : CompareType.BELOW);
        } else {
            rval = CompareType.EQUAL;
        }

        return rval;
    }

    @Override
    public int hashCode() {
        if (dirtyFlag) {
            generateDependentFields();
        }
        return hashCode;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Level other = (Level) obj;
        if (masterLevel == null) {
            if (other.masterLevel != null)
                return false;
        } else if (!masterLevel.equals(other.masterLevel))
            return false;
        if (levelonevalue != other.levelonevalue)
            return false;
        if (leveltwovalue != other.leveltwovalue)
            return false;
        return true;
    }

    @Override
    public String toString() {
        return getLevelInfo() + masterLevel.getName();
    }

    public static final String getInvalidLevelValueAsString() {
        return INVALID_VALUE_AS_STRING;
    }

    public static final double getInvalidLevelValue() {
        return INVALID_VALUE;
    }
}
