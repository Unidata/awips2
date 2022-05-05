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
package com.raytheon.uf.viz.objectiveanalysis.rsc;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.apache.commons.lang3.Validate;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.style.level.Level.LevelType;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Resource Data for Objective Analysis
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------
 * Nov 05, 2009           randerso  Initial creation
 * Jan 08, 2010  4205     jelkins   add equals checking for OA resources
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * May 17, 2018  7294     njensen   Overrode update(...)
 * Nov 01, 2018  7314     bsteffen  Handle Cloud Ceiling Level.
 * 
 * </pre>
 * 
 * @author randerso
 */

@XmlAccessorType(XmlAccessType.NONE)
public class OAResourceData extends AbstractRequestableResourceData {

    public static final String ALL_TILTS = "ALLTILTS";

    @XmlAttribute
    private String parameter;

    @XmlAttribute
    private String parameterName;

    @XmlAttribute
    private String levelKey;

    @XmlAttribute
    private String source;

    public OAResourceData() {
        super();
        setAlertParser(new OAAlertParser());
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((levelKey == null) ? 0 : levelKey.hashCode());
        result = prime * result
                + ((parameter == null) ? 0 : parameter.hashCode());
        result = prime * result + ((source == null) ? 0 : source.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        OAResourceData other = (OAResourceData) obj;
        if (levelKey == null) {
            if (other.levelKey != null) {
                return false;
            }
        } else if (!levelKey.equals(other.levelKey)) {
            return false;
        }
        if (parameter == null) {
            if (other.parameter != null) {
                return false;
            }
        } else if (!parameter.equals(other.parameter)) {
            return false;
        }
        if (source == null) {
            if (other.source != null) {
                return false;
            }
        } else if (!source.equals(other.source)) {
            return false;
        }
        return true;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the parameterName
     */
    public String getParameterName() {
        return parameterName;
    }

    /**
     * @param parameterName
     *            the parameterName to set
     */
    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    /**
     * @return the levelKey
     */
    public String getLevelKey() {
        return levelKey;
    }

    public SingleLevel getLevel() {
        SingleLevel level = null;
        if (ALL_TILTS.equals(levelKey)) {
            level = new SingleLevel(LevelType.TILT);
            level.setValue(-1);
            return level;
        } else if (levelKey.endsWith("MB")) {
            level = new SingleLevel(LevelType.PRESSURE);
        } else if (levelKey.endsWith("deg")) {
            level = new SingleLevel(LevelType.TILT);
        } else if ("Surface".equals(levelKey)) {
            level = new SingleLevel(LevelType.SURFACE);
            level.setValue(-1);
            return level;
        } else if ("CloudCeiling".equals(levelKey)) {
            level = new SingleLevel(LevelType.CLG);
            level.setValue(-1);
            return level;
        } else {
            throw new UnsupportedOperationException(
                    "Unsupported level for OA " + levelKey);
        }
        level.setValue(Double
                .parseDouble(levelKey.substring(0, levelKey.length() - 2)));
        return level;
    }

    /**
     * @param levelKey
     *            the levelKey to set
     */
    public void setLevelKey(String levelKey) {
        this.levelKey = levelKey;
    }

    /**
     * @return the source
     */
    public String getSource() {
        return source;
    }

    /**
     * @param source
     *            the source to set
     */
    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] times = super.getAvailableTimes();
        if (this.levelKey.equals(ALL_TILTS)) {
            LevelFactory factory = LevelFactory.getInstance();
            List<DataTime> timesWithLevels = new ArrayList<>();
            MasterLevel ml = factory.getMasterLevel("TILT");
            Set<Level> allLevels = LevelMappingFactory
                    .getInstance(
                            LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                    .getAllLevels();
            List<Level> levels = new ArrayList<>();
            for (Level l : allLevels) {
                if (l.getMasterLevel().equals(ml)) {
                    levels.add(l);
                }
            }

            for (int i = 0; i < times.length; ++i) {
                for (int j = 0; j < levels.size(); ++j) {
                    DataTime time = times[i].clone();
                    time.setLevelValue(levels.get(j).getLevelonevalue());
                    if (time.isSpatial()) {
                        timesWithLevels.add(time);
                    }
                }
            }

            return timesWithLevels
                    .toArray(new DataTime[timesWithLevels.size()]);
        } else {
            return times;
        }
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        return new OAResource(this, loadProperties);
    }

    @Override
    public void update(Object updateData) {
        Validate.isTrue(updateData instanceof Object[],
                "Update expected Object[]");
        this.fireChangeListeners(ChangeType.DATA_UPDATE, updateData);
    }

}
