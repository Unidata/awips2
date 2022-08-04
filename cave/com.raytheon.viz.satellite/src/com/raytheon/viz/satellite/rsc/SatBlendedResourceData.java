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
package com.raytheon.viz.satellite.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.datacube.DataCubeContainer;

/**
 * Resource which displays multiple Satellite resources simultaneously. It is
 * designed for displaying multiple, potentially overlapping regions such as the
 * GOES East/West CONUS displays. This resource takes advantage of the mosaicing
 * capability to render all the satellite products into a single composite so
 * that alpha changes are consistent for overlapping areas.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 19, 2009           jsanchez    Initial creation
 * Mar 10, 2009  800      jsanchez    Returned a single display name at a time.
 * Mar 20, 2009           jsanchez    Constructed resource with highest
 *                                    frequency first.
 * Apr 29, 2009  2295     jsanchez    Removed the size parameter in
 *                                    getFrequencyIndex().
 * Jun 17, 2009  2493     jsanchez    Displayed both times in CONUS scale.
 * Apr 18, 2014  2947     bsteffen    Allow resources being blended to omit
 *                                    load properties.
 * Mar 3, 2015  DCS 14960 jgerth      Slight modification to getName
 * Mar 09, 2018  6731     bsteffen    Stop construction if super returns null.
 * Oct 19, 2018  7499     bsteffen    Ensure the time matcher is notified when
 *                                    internal resources are updated.
 * 
 * 
 * </pre>
 * 
 * @author jsanchez
 */
@XmlRootElement(name = "satBlendedResource")
@XmlAccessorType(XmlAccessType.NONE)
public class SatBlendedResourceData extends AbstractRequestableResourceData
        implements IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatBlendedResourceData.class);

    public static enum BlendedTimeMatchMode {
        EVERY_IMAGE, ALL_IMAGES, FIRST_IMAGE;

    }

    private Comparator<DataTime> comp = new Comparator<DataTime>() {
        @Override
        public int compare(DataTime o1, DataTime o2) {
            long diff = o1.getMatchRef() - o2.getMatchRef();
            if (Math.abs(diff) < millisecondsBlendAllowance) {
                return 0;
            } else {
                return (int) diff;
            }
        }
    };

    @XmlElement(name = "resource")
    protected ResourceList resourceList;

    @XmlAttribute
    protected BlendedTimeMatchMode timeMatchMode = BlendedTimeMatchMode.ALL_IMAGES;

    @XmlAttribute
    private long millisecondsBlendAllowance = 180_000L;

    public SatBlendedResourceData() {
        this.retrieveData = false;
        this.resourceList = new ResourceList();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                MapDescriptor descriptor = (MapDescriptor) resource
                        .getDescriptor();
                StringBuilder s = new StringBuilder();
                if (descriptor != null) {
                    /*
                     * Uses the first available name.
                     */
                    for (ResourcePair rp : resourceList) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        if (rsc != null) {
                            if (s.length() == 0) {
                                s.append(rsc.getName());
                            } else if (s.indexOf(rsc.getName()) < 0) {
                                s.append(" + ").append(rsc.getName());
                            }
                        }
                    }
                }

                return s.toString();
            }

        };
    }

    private void constructResources(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        for (ResourcePair rp : resourceList) {
            if (rp.getResource() != null) {
                rp.setResourceData(rp.getResource().getResourceData());
            }
            LoadProperties props = rp.getLoadProperties();
            if (props == null) {
                props = new LoadProperties();
                rp.setLoadProperties(props);
            }
            props.overrideCapabilities(loadProperties.getCapabilities());
            boolean success = true;
            try {
                success = rp.instantiateResource(descriptor, false);
            } catch (NoDataAvailableException e) {
                success = false;
            }
            if (success) {
                rp.getResource().getResourceData().addChangeListener(this);
            } else {
                resourceList.remove(rp);
            }
        }

        if (resourceList.size() == 0) {
            throw new NoDataAvailableException(getClass());
        }
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        mergeMetadataMap();

        DataTime[] availableTimes = getAvailableTimes();

        if (availableTimes.length == 0) {
            throw new NoDataAvailableException(SatBlendedResource.class);
        }

        AbstractVizResource<?, ?> rsc = super.construct(loadProperties,
                descriptor);
        if (rsc != null) {
            constructResources(loadProperties, descriptor);
        }

        return rsc;
    }

    private void mergeMetadataMap() {
        if (this.metadataMap == null || this.metadataMap.isEmpty()) {
            this.metadataMap = new HashMap<>();

            for (ResourcePair rp : resourceList) {
                Object obj = rp.getResourceData();

                if (obj instanceof AbstractRequestableResourceData) {
                    AbstractRequestableResourceData aRecData = ((AbstractRequestableResourceData) obj);
                    Map<String, RequestConstraint> constraintMap = aRecData
                            .getMetadataMap();

                    for (Entry<String, RequestConstraint> entry : constraintMap
                            .entrySet()) {
                        String key = entry.getKey();
                        RequestConstraint existing = this.metadataMap.get(key);
                        RequestConstraint secondaryConstraint = entry
                                .getValue();

                        if (existing == null) {
                            // new constraint
                            RequestConstraint newConstraint = new RequestConstraint(
                                    secondaryConstraint.getConstraintValue(),
                                    secondaryConstraint.getConstraintType());
                            this.metadataMap.put(key, newConstraint);
                        } else {
                            // only catching this case right now: if key in
                            // map and ConstraintType is "equals"
                            // then change ConstraintType to equals and add
                            // value to key's values
                            if (existing
                                    .getConstraintType() == ConstraintType.EQUALS) {
                                if (!existing.getConstraintValue()
                                        .equals(secondaryConstraint
                                                .getConstraintValue())) {
                                    RequestConstraint newConstraint = new RequestConstraint(
                                            existing.getConstraintValue(),
                                            ConstraintType.IN);
                                    newConstraint.addToConstraintValueList(
                                            secondaryConstraint
                                                    .getConstraintValue());
                                    this.metadataMap.put(key, newConstraint);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        return getAvailableTimesLimit();
    }

    @Override
    public void update(Object updateData) {
        if (updateData instanceof PluginDataObject[]) {
            Map<ResourcePair, List<PluginDataObject>> updates = new HashMap<>();
            for (PluginDataObject pdo : (PluginDataObject[]) updateData) {
                try {
                    Map<String, Object> pdoMap = RecordFactory.getInstance()
                            .loadMapFromUri(pdo.getDataURI());
                    for (ResourcePair rp : resourceList) {
                        AbstractResourceData rscData = rp.getResourceData();

                        if (rscData instanceof AbstractRequestableResourceData) {
                            AbstractRequestableResourceData arData = (AbstractRequestableResourceData) rscData;
                            for (Map<String, RequestConstraint> metadataMap : DataCubeContainer
                                    .getBaseUpdateConstraints(
                                            arData.getMetadataMap())) {
                                boolean match = true;

                                for (Entry<String, RequestConstraint> entry : metadataMap
                                        .entrySet()) {
                                    Object pdoItem = pdoMap.get(entry.getKey());

                                    if (pdoItem == null || !entry.getValue()
                                            .evaluate(pdoItem)) {
                                        match = false;
                                        break;
                                    }
                                }
                                if (match) {
                                    List<PluginDataObject> pdos = updates
                                            .get(rp);
                                    if (pdos == null) {
                                        pdos = new ArrayList<>();
                                        updates.put(rp, pdos);
                                    }
                                    pdos.add(pdo);
                                }
                            }
                        }
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error processing mosaic update", e);
                }
            }
            for (Entry<ResourcePair, List<PluginDataObject>> entry : updates
                    .entrySet()) {
                ResourcePair rp = entry.getKey();
                rp.getResourceData().update(
                        entry.getValue().toArray(new PluginDataObject[0]));
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null) {
                    IDescriptor desc = rsc.getDescriptor();
                    if (desc != null) {
                        AbstractTimeMatcher tm = desc.getTimeMatcher();
                        if (tm != null) {
                            tm.redoTimeMatching(rsc);
                        }
                    }
                }
            }
        } else {
            super.update(updateData);
        }
    }

    public ResourceList getResourceList() {
        return resourceList;
    }

    // Use getTimeMatchingMode, this method only exists for old serialization
    @Deprecated
    @XmlAttribute(name = "everyImage")
    public void setEveryImage(boolean everyImage) {
        if (everyImage) {
            timeMatchMode = BlendedTimeMatchMode.EVERY_IMAGE;
        } else {
            timeMatchMode = BlendedTimeMatchMode.ALL_IMAGES;
        }
    }

    // Use getTimeMatchingMode, this method only exists for old serialization
    @Deprecated
    public boolean isEveryImage() {
        return timeMatchMode == BlendedTimeMatchMode.EVERY_IMAGE;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        /*
         * under some circumstances obj might consider itself equal to this, so
         * just let it decide.
         */
        if (obj instanceof SatBestResResourceData) {
            return obj.equals(this);
        }
        if (obj == null || !obj.getClass().equals(this.getClass())) {
            return false;
        }
        SatBlendedResourceData other = (SatBlendedResourceData) obj;

        if (this.resourceList != null && other.resourceList == null) {
            return false;
        } else if (this.resourceList == null && other.resourceList != null) {
            return false;
        } else if (this.resourceList != null
                && !this.resourceList.equals(other.resourceList)) {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((resourceList == null) ? 0 : resourceList.hashCode());
        return result;
    }

    @Override
    public void resourceChanged(ChangeType type, Object data) {
        for (ResourcePair rp : resourceList) {
            if (rp.getResource() != null
                    && rp.getResource().getResourceData() != null) {
                fireChangeListeners(type, data);
            }
        }
    }

    /**
     * Returns DataTimes where abs(T1 - T2) < millisecondsBlendAllowance.
     */
    private DataTime[] getAvailableTimesLimit() throws VizException {
        List<DataTime> availableTimes = new ArrayList<>();
        ListIterator<ResourcePair> rpIter = resourceList
                .listIterator(resourceList.size());
        mergeMetadataMap();
        if (rpIter.hasPrevious()) {
            ResourcePair rp = rpIter.previous();

            if (rp.getResourceData() instanceof AbstractRequestableResourceData) {
                AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rp
                        .getResourceData();
                availableTimes.addAll(Arrays.asList(arrd.getAvailableTimes()));
            }
        }

        for (; rpIter.hasPrevious();) {
            ResourcePair rp = rpIter.previous();

            if (rp.getResourceData() instanceof AbstractRequestableResourceData) {
                AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rp
                        .getResourceData();
                List<DataTime> times = new ArrayList<>(
                        Arrays.asList(arrd.getAvailableTimes()));
                Collections.sort(times);

                switch (timeMatchMode) {
                case EVERY_IMAGE:
                    for (DataTime time : times) {
                        int diff = Collections.binarySearch(availableTimes,
                                time, comp);
                        if (diff < 0) {
                            availableTimes.add(time);
                        }
                    }
                    break;
                case ALL_IMAGES:
                    List<DataTime> toRemove = new ArrayList<>();
                    for (DataTime dt : availableTimes) {
                        int diff = Collections.binarySearch(times, dt, comp);

                        if (diff < 0) {
                            toRemove.add(dt);
                        }
                    }
                    availableTimes.removeAll(toRemove);
                    break;
                case FIRST_IMAGE:
                    // Only use times from the first resource, not later ones.
                    break;
                }
            }

            if (availableTimes.isEmpty()) {
                break;
            }
        }

        Collections.sort(availableTimes);
        return availableTimes.toArray(new DataTime[availableTimes.size()]);
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        return new SatBlendedResource(this, loadProperties);
    }

    @Override
    public HashMap<String, RequestConstraint> getMetadataMap() {
        mergeMetadataMap();
        return super.getMetadataMap();
    }

    public BlendedTimeMatchMode getTimeMatchMode() {
        return timeMatchMode;
    }

    public void setTimeMatchMode(BlendedTimeMatchMode timeMatchMode) {
        this.timeMatchMode = timeMatchMode;
    }

}
