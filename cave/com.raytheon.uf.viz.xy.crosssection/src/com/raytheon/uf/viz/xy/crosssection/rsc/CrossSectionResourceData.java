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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlTransient;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.alerts.DataCubeAlertMessageParser;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.IUpdateHandlingResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.adapter.CrossSectionAdapterUtil;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;

/**
 * Resource data for cross sections
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009            njensen     Initial creation
 * May 08, 2014 2060       njensen     Constructor sets alert parser
 * Mar 28, 2018 6874       dgilling    Return null in constructResource if
 *                                     there's no data.
 * Oct 29, 2022 8959       mapeters    Update how data time levels are set
 * Apr 02, 2024 2037091    mapeters    Implement IUpdateHandlingResourceData, remove blacklisted
 *                                     times to support auto-updating, add getAffectedLineTimes()
 * May 22, 2024 2037092    mapeters    Extract adapter determination to CrossSectionAdapterUtil,
 *                                     add setLineInfoFromDescriptor
 *
 * </pre>
 *
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CrossSectionResourceData extends AbstractRequestableResourceData
        implements IUpdateHandlingResourceData {

    @XmlAttribute
    protected String parameter;

    @XmlAttribute
    protected String parameterName;

    @XmlAttribute
    protected List<String> stationIDs;

    @XmlAttribute
    private String source;

    @XmlTransient
    protected int numLines;

    @XmlTransient
    protected String levelType;

    @XmlTransient
    private boolean sendUpdateNotifications = false;

    public CrossSectionResourceData() {
        this.setAlertParser(new DataCubeAlertMessageParser());
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (descriptor instanceof CrossSectionDescriptor) {
            CrossSectionDescriptor csDesc = (CrossSectionDescriptor) descriptor;
            setLineInfoFromDescriptor(csDesc);
        }
        return super.construct(loadProperties, descriptor);
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        if (objects.length == 0) {
            return null;
        }

        AbstractCrossSectionAdapter<?> adapter = CrossSectionAdapterUtil
                .getAdapter(objects[0]);
        adapter.setResourceData(this);
        AbstractCrossSectionResource resource = null;
        DisplayType displayType = loadProperties.getCapabilities()
                .getCapability(this, DisplayTypeCapability.class)
                .getDisplayType();
        if (displayType.equals(DisplayType.IMAGE)) {
            resource = new CrossSectionIntermediateImageResource(this,
                    loadProperties, adapter);
            sendUpdateNotifications = true;
        } else if (displayType.equals(DisplayType.ARROW)
                || displayType.equals(DisplayType.BARB)) {
            resource = new CrossSectionVectorResource(this, loadProperties,
                    adapter);
        }
        if (resource == null) {
            resource = new CrossSectionContourResource(this, loadProperties,
                    adapter);
        }
        adapter.setResource(resource);
        for (PluginDataObject pdo : objects) {
            resource.addRecord(pdo);
        }
        return resource;
    }

    public String getParameter() {
        return parameter;
    }

    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getParameterName() {
        return parameterName;
    }

    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        if (levelType == null || numLines <= 0) {
            // We can't determine actual frame times until these are set
            return new DataTime[0];
        }
        DataTime[] times = super.getAvailableTimes();
        List<DataTime> newTimes = new ArrayList<>();
        for (DataTime time : times) {
            newTimes.addAll(getAffectedFrameTimes(time));
        }
        return newTimes.toArray(new DataTime[0]);

    }

    @Override
    public PluginDataObject[] getLatestPluginDataObjects(DataTime[] desired,
            DataTime[] current) throws VizException {
        if (desired == null) {
            return new PluginDataObject[0];
        }
        DataTime[] stripped = new DataTime[desired.length];
        for (int i = 0; i < desired.length; ++i) {
            if (desired[i] != null) {
                stripped[i] = desired[i].clone();
                stripped[i].clearLevel();
            }
        }

        DataTime[] sc = new DataTime[current.length];
        for (int i = 0; i < current.length; ++i) {
            if (current[i] != null) {
                sc[i] = current[i].clone();
                sc[i].clearLevel();
            }
        }

        return super.getLatestPluginDataObjects(stripped, sc);
    }

    /**
     * @return the stationIDs
     */
    public List<String> getStationIDs() {
        return stationIDs;
    }

    /**
     * @param stationIDs
     *            the stationIDs to set
     */
    public void setStationIDs(List<String> stationIDs) {
        this.stationIDs = stationIDs;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
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
        CrossSectionResourceData other = (CrossSectionResourceData) obj;
        if (!Objects.equals(parameter, other.parameter)) {
            return false;
        }
        if (!Objects.equals(source, other.source)) {
            return false;
        }
        return true;
    }

    @Override
    public void handleUpdate(AlertMessage message) {
        DataTime time = (DataTime) message.decodedAlert
                .get(PluginDataObject.DATATIME_ID);
        if (sendUpdateNotifications) {
            fireChangeListeners(ChangeType.DATA_UPDATE, time);
        } else {
            fireChangeListeners(ChangeType.DATA_REMOVE, time);
        }
    }

    /**
     * Get the frame times that are affected by an update to the given time.
     *
     * The frame times each have their level type set to a description of the
     * type of lines that the resource is for (a baseline or longitude/latitude
     * lines), and the level value is the line's index in
     * CrossSectionDescriptor.lines.
     *
     * @param time
     * @return the affected frame times
     */
    public Collection<DataTime> getAffectedFrameTimes(DataTime time) {
        if (levelType == null || numLines <= 0) {
            // We can't determine actual frame times until these are set
            return List.of();
        }

        /*
         * If the time's level type matches, then it is already for a single
         * frame/line, and just that frame is affected.
         */
        if (levelType.equals(time.getLevelType())) {
            return List.of(time);
        }

        /*
         * Otherwise, assume that the time is for the underlying data that each
         * frame is derived from, and return times for all lines.
         */
        List<DataTime> frameTimes = new ArrayList<>();
        for (int i = 0; i < numLines; ++i) {
            DataTime frameTime = time.clone();
            frameTime.setLevel((double) i, levelType);
            frameTimes.add(frameTime);
        }
        return frameTimes;
    }

    /**
     * Update line info (number and type of lines) to match what's in the given
     * descriptor.
     *
     * @param csDesc
     *            descriptor to get line info from
     */
    protected void setLineInfoFromDescriptor(CrossSectionDescriptor csDesc) {
        numLines = csDesc.getLines().size();
        levelType = csDesc.getLevelType();
    }
}
