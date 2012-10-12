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
package com.raytheon.viz.core.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Group ResourceData abstract parent class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2011            mpduff      Initial creation.
 * Sep 11, 2012  1162      mpduff      Made mergeMetaDataMap method public.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "vizGroupResource")
@XmlAccessorType(XmlAccessType.NONE)
public class VizGroupResourceData extends AbstractRequestableResourceData
        implements IResourceDataChanged {

    @XmlElement(name = "resource")
    protected ResourceList resourceList;

    @XmlAttribute
    protected String name;

    private List<AbstractVizResource<?, ?>> rscs = new ArrayList<AbstractVizResource<?, ?>>();

    public VizGroupResourceData() {
        super();
        nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return name;
            }
        };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        mergeMetadataMap();
        AbstractVizResource<?, ?> resource = super.construct(loadProperties, descriptor);

        for (int i = 0; i < resourceList.size(); i++) {
            try {
                AbstractVizResource rsc = resourceList.get(i).getResourceData()
                        .construct(resourceList.get(i).getLoadProperties(),
                                descriptor);
                rsc.setDescriptor(descriptor);
                resourceList.get(i).setResource(rsc);
                rscs.add(rsc);
                rsc.getResourceData().addChangeListener(this);
            } catch (NoDataAvailableException e) {
                // Do nothing
            }
        }
        if (rscs.size() == 0) {
            throw new NoDataAvailableException();
        }

        return resource;
    }

    public void mergeMetadataMap() {
        if ((this.metadataMap == null) || this.metadataMap.isEmpty()) {
            this.metadataMap = new HashMap<String, RequestConstraint>();

            for (ResourcePair rp : resourceList) {
                Object obj = rp.getResourceData();

                if (obj instanceof AbstractRequestableResourceData) {
                    AbstractRequestableResourceData aRecData = ((AbstractRequestableResourceData) obj);
                    HashMap<String, RequestConstraint> constraintMap = aRecData
                            .getMetadataMap();

                    for (String key : constraintMap.keySet()) {
                        RequestConstraint existing = this.metadataMap.get(key);
                        RequestConstraint secondaryConstraint = constraintMap
                                .get(key);

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
                            if (existing.getConstraintType() == ConstraintType.EQUALS) {
                                if (!existing.getConstraintValue().equals(
                                        secondaryConstraint
                                                .getConstraintValue())) {
                                    RequestConstraint newConstraint = new RequestConstraint(
                                            existing.getConstraintValue(),
                                            ConstraintType.IN);
                                    newConstraint
                                            .addToConstraintValueList(secondaryConstraint
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
        mergeMetadataMap();
        return super.getAvailableTimes();
//        List<DataTime> baseTimes = new ArrayList<DataTime>();
//        Iterator<ResourcePair> rpIter = resourceList.iterator();
//        mergeMetadataMap();
//
//        if (rpIter.hasNext()) {
//            ResourcePair rp = rpIter.next();
//
//            if (rp.getResourceData() instanceof AbstractRequestableResourceData) {
//                AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rp
//                        .getResourceData();
//                Collection<DataTime> times = Arrays.asList(arrd
//                        .getAvailableTimes());
//
//                baseTimes.addAll(times);
//            }
//        }
//
//        List<DataTime> availableTimes = new ArrayList<DataTime>();
//
//        Collections.sort(baseTimes);
//
//        while (rpIter.hasNext()) {
//            ResourcePair rp = rpIter.next();
//
//            if (rp.getResourceData() instanceof AbstractRequestableResourceData) {
//                AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rp
//                        .getResourceData();
//                Collection<DataTime> times = Arrays.asList(arrd
//                        .getAvailableTimes());
//
//                for (DataTime dt : times) {
//                    int diff = Collections.binarySearch(baseTimes, dt);
//
//                    if (diff >= 0) {
//                        availableTimes.add(dt);
//                    }
//                }
//            }
//        }
//
//        Collections.sort(availableTimes);
//
//        return availableTimes.toArray(new DataTime[availableTimes.size()]);
    }

//    @Override
//    public void update(Object updateData) {
//        Map<AbstractResourceData, List<PluginDataObject>> updates = new HashMap<AbstractResourceData, List<PluginDataObject>>();
//        for (PluginDataObject pdo : (PluginDataObject[]) updateData) {
//            try {
//                Map<String, Object> pdoMap = RecordFactory.getInstance()
//                        .loadMapFromUri(pdo.getDataURI());
//                for (ResourcePair rp : resourceList) {
//                    AbstractResourceData rscData = rp.getResourceData();
//
//                    if (rscData instanceof AbstractRequestableResourceData) {
//                        AbstractRequestableResourceData arData = (AbstractRequestableResourceData) rscData;
//                        HashMap<String, RequestConstraint> metadataMap = arData
//                                .getMetadataMap();
//                        boolean match = true;
//
//                        for (Entry<String, RequestConstraint> entry : metadataMap
//                                .entrySet()) {
//                            Object pdoItem = pdoMap.get(entry.getKey());
//
//                            if ((pdoItem == null)
//                                    || !entry.getValue().evaluate(pdoItem)) {
//                                match = false;
//                                break;
//                            }
//                        }
//                        if (match) {
//                            List<PluginDataObject> pdos = updates.get(arData);
//                            if (pdos == null) {
//                                pdos = new ArrayList<PluginDataObject>();
//                                updates.put(arData, pdos);
//                            }
//                            pdos.add(pdo);
//                        }
//                    }
//                }
//            } catch (VizException e) {
//                // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
//                // StatusConstants.CATEGORY_WORKSTATION, null,
//                // "Error processing mosaic update", e);
//                e.printStackTrace();
//            }
//        }
//
//        for (Entry<AbstractResourceData, List<PluginDataObject>> entry : updates
//                .entrySet()) {
//            entry.getKey().update(
//                    entry.getValue().toArray(new PluginDataObject[0]));
//        }
//    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        for (ResourcePair rp : resourceList) {
            if ((rp.getResource() != null)
                    && (rp.getResource().getResourceData() != null)) {
                fireChangeListeners(type, object);
            }
        }
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        return new VizGroupResource(this, loadProperties);
    }

    /**
     * @return the resourceList
     */
    public ResourceList getResourceList() {
        return resourceList;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @param resourceList
     *            the resourceList to set
     */
    public void setResourceList(ResourceList resourceList) {
        this.resourceList = resourceList;
    }

    @Override
    public HashMap<String, RequestConstraint> getMetadataMap() {
        mergeMetadataMap();
        return super.getMetadataMap();
    }

    /**
     * @return the rscs
     */
    public List<AbstractVizResource<?, ?>> getRscs() {
        return rscs;
    }

    /**
     * @param rscs the rscs to set
     */
    public void setRscs(List<AbstractVizResource<?, ?>> rscs) {
        this.rscs = rscs;
    }
}
