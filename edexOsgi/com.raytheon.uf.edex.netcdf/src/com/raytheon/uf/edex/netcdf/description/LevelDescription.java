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
package com.raytheon.uf.edex.netcdf.description;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * 
 * Contains the information necessary to extract a {@link Level} from the global
 * attributes of a {@link NetcdfFile}.
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
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class LevelDescription {

    @XmlElements({
            @XmlElement(name = "masterLevelValue", type = ValueDescription.class),
            @XmlElement(name = "masterLevelVariable", type = VariableDescription.class),
            @XmlElement(name = "masterLevelAttribute", type = AttributeDescription.class) })
    private AbstractFieldDescription masterLevel;

    @XmlElements({
            @XmlElement(name = "levelOneValue", type = ValueDescription.class),
            @XmlElement(name = "levelOneValueVariable", type = VariableDescription.class),
            @XmlElement(name = "levelOneValueAttribute", type = AttributeDescription.class) })
    private AbstractFieldDescription levelOneValue;

    @XmlElements({
            @XmlElement(name = "levelTwoValue", type = ValueDescription.class),
            @XmlElement(name = "levelTwoValueVariable", type = VariableDescription.class),
            @XmlElement(name = "levelTwoValueAttribute", type = AttributeDescription.class) })
    private AbstractFieldDescription levelTwoValue;

    public AbstractFieldDescription getMasterLevel() {
        return masterLevel;
    }

    public void setMasterLevel(AbstractFieldDescription masterLevel) {
        this.masterLevel = masterLevel;
    }

    public AbstractFieldDescription getLevelOneValue() {
        return levelOneValue;
    }

    public void setLevelOneValue(AbstractFieldDescription levelOneValue) {
        this.levelOneValue = levelOneValue;
    }

    public AbstractFieldDescription getLevelTwoValue() {
        return levelTwoValue;
    }

    public void setLevelTwoValue(AbstractFieldDescription levelTwoValue) {
        this.levelTwoValue = levelTwoValue;
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
                    "A maseter level element is not present.");
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
    }
}
