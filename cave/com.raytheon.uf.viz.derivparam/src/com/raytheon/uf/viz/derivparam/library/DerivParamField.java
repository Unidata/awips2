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
package com.raytheon.uf.viz.derivparam.library;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;

/**
 * Metadata about a derived parameter field.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 21, 2009  3576     rjpeter     Initial version
 * Jan 14, 2014  2661     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class DerivParamField implements IDerivParamField {

    @XmlAttribute(name = "abbreviation", required = true)
    private String param;

    @XmlTransient
    private LevelMapping levelMapping;

    @XmlTransient
    private LevelType levelType;

    @XmlAttribute
    private Integer timeShift;

    @XmlAttribute(name = "model")
    private String validSource;

    @XmlAttribute
    private String level;

    @XmlAttribute
    @XmlJavaTypeAdapter(value = UnitAdapter.class)
    private Unit<?> unit = Unit.ONE;

    public String getParam() {
        return param;
    }

    public void setParam(String param) {
        this.param = param;
    }

    public LevelMapping getLevelMapping() throws VizCommunicationException {
        if (levelType == null && level != null) {
            setLevel(level);
        }
        return levelMapping;
    }

    public LevelType getLevelType() throws VizCommunicationException {
        if (levelType == null && level != null) {
            setLevel(level);
        }
        return levelType;
    }

    public Integer getTimeShift() {
        return timeShift;
    }

    public void setTimeShift(Integer timeShift) {
        this.timeShift = timeShift;
    }

    public String getValidSource() {
        return validSource;
    }

    public void setValidSource(String validSource) {
        this.validSource = validSource;
    }

    /**
     * @return the level
     */
    public String getLevel() {
        return level;
    }

    /**
     * Set the levelType and levelMapping from the given token
     * 
     * @param level
     * @return true upon success, false if the token is not recognized, in which
     *         case levelType and levelMapping will both be reset to null
     * @throws VizCommunicationException
     */
    public boolean setLevel(String level) throws VizCommunicationException {
        levelType = LevelType.parseLevel(level);
        if (levelType == LevelType.LevelMapping) {
            levelMapping = LevelMappingFactory.getInstance(
                    LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                    .getLevelMappingForKey(level);
            if (levelMapping == null) {
                levelType = null;
                return false;
            }
        }
        this.level = level;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder tmp = new StringBuilder();
        if (param != null) {
            tmp.append(param);
        } else {
            tmp.append("param: null");
        }
        if (level != null) {
            tmp.append(", ");
            tmp.append(level);
        }
        if (validSource != null) {
            tmp.append(", ");
            tmp.append(validSource);
        }
        if (timeShift != null) {
            tmp.append(", ");
            tmp.append(timeShift);
        }
        return tmp.toString();
    }

    public void setUnit(Unit<?> unit) {
        this.unit = unit;
    }

    public Unit<?> getUnit() {
        return unit;
    }
}
