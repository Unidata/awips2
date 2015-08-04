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
package com.raytheon.uf.edex.plugin.pointset.netcdf.description;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;

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
 * ------------- -------- --------- --------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class LevelDescription {

    @XmlElement
    private AttributeValue masterLevel;

    @XmlElement
    private AttributeValue levelOneValue;

    @XmlElement
    private AttributeValue levelTwoValue;

    public AttributeValue getMasterLevel() {
        return masterLevel;
    }

    public void setMasterLevel(AttributeValue masterLevel) {
        this.masterLevel = masterLevel;
    }

    public AttributeValue getLevelOneValue() {
        return levelOneValue;
    }

    public void setLevelOneValue(AttributeValue levelOneValue) {
        this.levelOneValue = levelOneValue;
    }

    public AttributeValue getLevelTwoValue() {
        return levelTwoValue;
    }

    public void setLevelTwoValue(AttributeValue levelTwoValue) {
        this.levelTwoValue = levelTwoValue;
    }

    public Level getLevel(NetcdfFile file, LevelFactory factory) {
        String masterLevel = this.masterLevel.getStringValue(file);
        if (masterLevel == null) {
            return null;
        }
        Double levelOneValue = this.levelOneValue.getDoubleValue(file);
        if (levelOneValue == null) {
            return null;
        }
        if (this.levelTwoValue == null) {
            return factory.getLevel(masterLevel, levelOneValue);
        }
        Double levelTwoValue = this.levelTwoValue.getDoubleValue(file);
        if (levelTwoValue == null) {
            return null;
        }
        return factory.getLevel(masterLevel, levelOneValue, levelTwoValue);
    }
}
