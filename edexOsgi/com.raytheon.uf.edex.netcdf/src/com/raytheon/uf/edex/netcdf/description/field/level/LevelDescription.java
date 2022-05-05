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
package com.raytheon.uf.edex.netcdf.description.field.level;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.DimensionDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;

/**
 *
 * Contains the information necessary to extract a {@link Level} from the nodes
 * of a {@link NetcdfFile}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Aug 25, 2015  4699     nabowle   Extracted from Pointset netcdf plugin and
 *                                  refactored.
 * Jan 25, 2016  5208     bsteffen  Add validation.
 * Mar 21, 2016  5450     nabowle   Extracted getNumLevels() to here.
 *                                  Addded other XmlElements.
 * Apr 28, 2016  5442     skorolev  Added levelValues element.
 * May 19, 2016  5584     nabowle   Updates for consolidation.
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class LevelDescription {
    public static final String LEVEL_KEY = "level";

    @XmlElement(required = true)
    private DelegateFieldDescription masterLevel;

    @XmlElement(required = true)
    private DelegateFieldDescription levelOneValue;

    @XmlElement
    private DelegateFieldDescription levelTwoValue;

    @XmlElement
    private DimensionDescription dimension;

    public DelegateFieldDescription getMasterLevel() {
        return masterLevel;
    }

    public void setMasterLevel(DelegateFieldDescription masterLevel) {
        this.masterLevel = masterLevel;
    }

    public DelegateFieldDescription getLevelOneValue() {
        return levelOneValue;
    }

    public void setLevelOneValue(DelegateFieldDescription levelOneValue) {
        this.levelOneValue = levelOneValue;
    }

    public DelegateFieldDescription getLevelTwoValue() {
        return levelTwoValue;
    }

    public void setLevelTwoValue(DelegateFieldDescription levelTwoValue) {
        this.levelTwoValue = levelTwoValue;
    }

    /**
     * @return the dimension
     */
    public DimensionDescription getDimension() {
        return dimension;
    }

    /**
     * @param dimension
     *            the dimension to set
     */
    public void setDimension(DimensionDescription dimension) {
        this.dimension = dimension;
    }

    public Level getLevel(NetcdfFile file, LevelFactory factory)
            throws InvalidDescriptionException {
        return getLevel(file, factory, 0);
    }

    /**
     *
     *
     * @param file
     * @param factory
     * @param levelIndex
     *            The index for each level component (master level,level one,
     *            level two), it is expected that each component at this index
     *            is associated, or the component is scalar and valid for all
     *            indices of every other non-scalar component.
     * @return
     * @throws InvalidDescriptionException
     */
    public Level getLevel(NetcdfFile file, LevelFactory factory, int levelIndex)
            throws InvalidDescriptionException {
        if (levelIndex < 0) {
            throw new InvalidDescriptionException(
                    new ArrayIndexOutOfBoundsException(levelIndex));
        }

        String masterLevel = this.masterLevel.getString(file, levelIndex);
        if (masterLevel == null) {
            return null;
        }

        Number levelOneValue = this.levelOneValue.getNumber(file, levelIndex);
        if (levelOneValue == null) {
            return null;
        }

        if (this.levelTwoValue == null) {
            return factory.getLevel(masterLevel, levelOneValue.doubleValue());
        }

        Number levelTwoValue = this.levelTwoValue.getNumber(file, levelIndex);
        if (levelTwoValue == null) {
            return null;
        }
        return factory.getLevel(masterLevel, levelOneValue.doubleValue(),
                levelTwoValue.doubleValue());
    }

    public void validate() throws InvalidDescriptionException {
        if (masterLevel == null) {
            throw new InvalidDescriptionException(
                    "A master level element is not present.");
        }
        if (levelOneValue == null) {
            throw new InvalidDescriptionException(
                    "A level one value element is not present.");
        }
        try {
            masterLevel.validate();
        } catch (InvalidDescriptionException e) {
            throw new InvalidDescriptionException("Invalid master level: "
                    + e.getMessage(), e);
        }
        try {
            levelOneValue.validate();
        } catch (InvalidDescriptionException e) {
            throw new InvalidDescriptionException("Invalid level one value: "
                    + e.getMessage(), e);
        }

        if (levelTwoValue != null) {
            try {
                levelTwoValue.validate();
            } catch (InvalidDescriptionException e) {
                throw new InvalidDescriptionException(
                        "Invalid level two value: " + e.getMessage(), e);
            }
        }
    }

    /**
     * Get the number of levels.
     *
     * @param file
     *            The netcdf file.
     * @return The number of levels.
     * @throws InvalidDescriptionException
     *             If the individual parts of the level have incompatible
     *             lengths (multiple lengths > 1 that aren't equal).
     */
    public long getNumLevels(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.dimension != null) {
            Number num = this.dimension.getNumber(file);
            if (num != null) {
                return num.longValue();
            }
        }

        long numLevels = 0;
        if (this.getMasterLevel() != null) {
            numLevels = this.getMasterLevel().getLength(file);
        }

        long newLen;
        if (this.levelOneValue != null) {
            newLen = this.levelOneValue.getLength(file);
            if (newLen != numLevels && numLevels > 1 && newLen > 1) {
                throw new InvalidDescriptionException(
                        "Unable to provide a meaningful number of levels. "
                                + "The parts of the level don't have compatible lengths.");
            }
            numLevels = Math.max(numLevels, newLen);
        }

        if (this.levelTwoValue != null) {
            newLen = this.levelTwoValue.getLength(file);
            if (newLen != numLevels && numLevels > 1 && newLen > 1) {
                throw new InvalidDescriptionException(
                        "Unable to provide a meaningful number of levels. "
                                + "The parts of the level don't have compatible lengths.");
            }
            numLevels = Math.max(numLevels, newLen);
        }
        return numLevels;
    }

}
