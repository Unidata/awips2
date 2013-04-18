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

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.style.level.Level.LevelType;
import com.raytheon.uf.viz.core.style.level.SingleLevel;

/**
 * Resource Data for Objective Analysis
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2009            randerso     Initial creation
 * Jan 8, 2010  4205      jelkins      add equals checking for OA resources
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#equals(java
     * .lang.Object)
     */
    @Override
    public boolean equals(Object obj) {

        // check for standard object and instance equality
        if (!super.equals(obj) || obj instanceof OAResourceData == false) {
            return false;
        }

        OAResourceData other = (OAResourceData) obj;

        // check for object field equality
        if (!isObjectsEqual(parameter, other.parameter)
                || !isObjectsEqual(levelKey, other.levelKey)
                || !isObjectsEqual(source, other.source)) {
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
        if (levelKey.equals(ALL_TILTS)) {
            level = new SingleLevel(LevelType.TILT);
            level.setValue(-1);
            return level;
        } else if (levelKey.endsWith("MB")) {
            level = new SingleLevel(LevelType.PRESSURE);
        } else if (levelKey.endsWith("deg")) {
            level = new SingleLevel(LevelType.TILT);
        } else if (levelKey.equals("Surface")) {
            level = new SingleLevel(LevelType.SURFACE);
            level.setValue(-1);
            return level;
        } else {
            throw new UnsupportedOperationException("Unsupported level for OA "
                    + levelKey);
        }
        level.setValue(Double.parseDouble(levelKey.substring(0,
                levelKey.length() - 2)));
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
            List<DataTime> timesWithLevels = new ArrayList<DataTime>();
            try {
                MasterLevel ml = factory.getMasterLevel("TILT");
                Set<Level> allLevels = LevelMappingFactory.getInstance(
                        LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                        .getAllLevels();
                List<Level> levels = new ArrayList<Level>();
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
            } catch (CommunicationException e) {
                throw new VizCommunicationException(e);
            }
            return timesWithLevels
                    .toArray(new DataTime[timesWithLevels.size()]);
        } else {
            return times;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        return new OAResource(this, loadProperties);
    }

}
