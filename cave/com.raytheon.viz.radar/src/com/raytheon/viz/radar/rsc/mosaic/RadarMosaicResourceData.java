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
package com.raytheon.viz.radar.rsc.mosaic;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.radar.rsc.mosaic.RadarMosaicRendererFactory.MosaicType;

/**
 * Provides the metadata and constructor for Radar
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class RadarMosaicResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarMosaicResourceData.class);

    @XmlAttribute
    private String productName = "NO DATA";

    private ResourceList validProductList;

    @XmlElement(name = "resource")
    protected ResourceList resourceList;

    @XmlElement
    protected AbstractMosaicResourceFactory resourceFactory;

    @XmlAttribute
    private MosaicType mosaicType;

    @XmlAttribute
    private Boolean mergeUpperText = false;

    public RadarMosaicResourceData() {
        this.retrieveData = true;
        this.resourceList = new ResourceList();
    }

    private class ResourceConstructJob implements Runnable {

        private final ResourcePair rp;

        private final IDescriptor descriptor;

        public VizException exception;

        private Object lock = new Object();

        private boolean done = false;

        public ResourceConstructJob(ResourcePair rp, IDescriptor descriptor) {
            super();
            this.rp = rp;
            this.descriptor = descriptor;
        }

        @SuppressWarnings("rawtypes")
        @Override
        public void run() {
            try {
                AbstractVizResource subRsc = rp.getResourceData().construct(
                        rp.getLoadProperties(), descriptor);
                // Manually set the descriptor since add/remove listeners do not
                // exist on validProductList
                subRsc.setDescriptor(descriptor);
                rp.setResource(subRsc);
                validProductList.add(rp);
            } catch (NoDataAvailableException e) {
                ;// Ignore
            } catch (VizException e) {
                exception = e;
            }
            done = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }

        protected void join() throws InterruptedException {
            synchronized (lock) {
                while (!done) {
                    lock.wait();
                }
            }
        }

    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if ((resourceList == null || resourceList.isEmpty())
                && resourceFactory != null) {
            resourceList = resourceFactory.getResourceList();
        }
        AbstractVizResource<?, ?> rrd = super.construct(loadProperties,
                descriptor);
        validProductList = new ResourceList();

        JobPool constructionJobPool = null;
        if (resourceList.size() > 3) {
            constructionJobPool = new JobPool("Constructing Mosaic Resources",
                    resourceList.size() / 4, null, Job.INTERACTIVE);
        }
        List<ResourceConstructJob> jobList = new ArrayList<ResourceConstructJob>();
        for (ResourcePair rp : resourceList) {
            rp.getLoadProperties().overrideCapabilities(
                    loadProperties.getCapabilities());
            ResourceConstructJob job = new ResourceConstructJob(rp, descriptor);
            if (constructionJobPool != null) {
                constructionJobPool.schedule(job);
            } else {
                job.run();
            }
            jobList.add(job);
        }
        for (ResourceConstructJob job : jobList) {
            try {
                job.join();
            } catch (InterruptedException e) {
                ;// ignore
            }
            if (job.exception != null) {
                throw new VizException(job.exception);
            }
        }
        if (validProductList.isEmpty()) {
            // Should have at least one resource in the list
            throw new NoDataAvailableException();
        }
        this.retrieveData = true;
        return rrd;
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        return new RadarMosaicResource(this, loadProperties);
    }

    /**
     * @return the productName
     */
    public String getProductName() {
        return productName;
    }

    /**
     * @param productName
     *            the productName to set
     */
    public void setProductName(String productName) {
        this.productName = productName;
    }

    /**
     * @return the mergeUpperText
     */
    public Boolean getMergeUpperText() {
        return mergeUpperText;
    }

    /**
     * @param mergeUpperText
     *            the mergeUpperText to set
     */
    public void setMergeUpperText(Boolean mergeUpperText) {
        this.mergeUpperText = mergeUpperText;
    }

    /**
     * @return the mosaicType
     */
    public MosaicType getMosaicType() {
        return mosaicType != null ? mosaicType : MosaicType.MaxValue;
    }

    /**
     * @param mosaicType
     *            the mosaicType to set
     */
    public void setMosaicType(MosaicType mosaicType) {
        this.mosaicType = mosaicType;
    }

    @Override
    public void update(Object updateData) {
        if (updateData instanceof PluginDataObject[]) {
            Map<AbstractResourceData, List<PluginDataObject>> updates = new HashMap<AbstractResourceData, List<PluginDataObject>>();
            for (PluginDataObject pdo : (PluginDataObject[]) updateData) {
                try {
                    Map<String, Object> pdoMap = RecordFactory.getInstance()
                            .loadMapFromUri(pdo.getDataURI());
                    for (ResourcePair pair : validProductList) {
                        if (pair.getResourceData() instanceof AbstractRequestableResourceData) {
                            boolean match = true;
                            HashMap<String, RequestConstraint> constraintMap = ((AbstractRequestableResourceData) pair
                                    .getResourceData()).getMetadataMap();
                            for (Entry<String, RequestConstraint> entry : constraintMap
                                    .entrySet()) {
                                Object pdoItem = pdoMap.get(entry.getKey());
                                if (pdoItem == null) {
                                    match = false;
                                    break;
                                }
                                if (!entry.getValue().evaluate(pdoItem)) {
                                    match = false;
                                    break;
                                }
                            }
                            if (match) {
                                List<PluginDataObject> pdos = updates.get(pair
                                        .getResourceData());
                                if (pdos == null) {
                                    pdos = new ArrayList<PluginDataObject>();
                                    updates.put(pair.getResourceData(), pdos);
                                }
                                pdos.add(pdo);
                            }
                        }
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error processing mosaic update", e);

                }
            }
            for (Entry<AbstractResourceData, List<PluginDataObject>> entry : updates
                    .entrySet()) {
                entry.getKey().update(
                        entry.getValue().toArray(new PluginDataObject[0]));
            }
        } else {
            super.update(updateData);
        }
    }

    public ResourceList getResourceList() {
        if (validProductList != null) {
            return validProductList;
        }
        if ((resourceList == null || resourceList.isEmpty())
                && resourceFactory != null) {
            resourceList = resourceFactory.getResourceList();
        }
        return resourceList;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((mergeUpperText == null) ? 0 : mergeUpperText.hashCode());
        result = prime * result
                + ((mosaicType == null) ? 0 : mosaicType.hashCode());
        result = prime * result
                + ((productName == null) ? 0 : productName.hashCode());
        result = prime * result
                + ((resourceList == null) ? 0 : resourceList.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RadarMosaicResourceData other = (RadarMosaicResourceData) obj;
        if (mergeUpperText == null) {
            if (other.mergeUpperText != null)
                return false;
        } else if (!mergeUpperText.equals(other.mergeUpperText))
            return false;
        if (mosaicType != other.mosaicType)
            return false;
        if (productName == null) {
            if (other.productName != null)
                return false;
        } else if (!productName.equals(other.productName))
            return false;
        if (resourceList == null) {
            if (other.resourceList != null)
                return false;
        } else if (!resourceList.equals(other.resourceList))
            return false;
        return true;
    }

}
