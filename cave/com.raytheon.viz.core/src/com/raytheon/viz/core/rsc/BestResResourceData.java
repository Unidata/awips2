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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Represents resource data for products that use the best resolution
 * functionality
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@XmlRootElement(name = "bestResResource")
@XmlAccessorType(XmlAccessType.NONE)
public class BestResResourceData extends AbstractRequestableResourceData
        implements IResourceDataChanged {

    @XmlElement(name = "resource")
    protected ResourceList resourceList;

    @XmlAttribute
    private String productIdentifierKey;

    @XmlElement
    private AbstractSpatialEnabler enabler = null;

    private HashMap<DataTime, AbstractVizResource<?, ?>> bestResTimes = null;

    private List<AbstractVizResource<?, ?>> rscs = new ArrayList<AbstractVizResource<?, ?>>();

    protected IDescriptor desc = null;

    public BestResResourceData() {
        this.retrieveData = true;
        bestResTimes = new HashMap<DataTime, AbstractVizResource<?, ?>>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        this.desc = descriptor;
        AbstractVizResource<?, ?> rsc = super.construct(loadProperties,
                descriptor);
        constructInternal(loadProperties);
        return rsc;
    }

    /**
     * Initial function to take all the present times and put the correct
     * resource in the bestResTimes hashmap
     * 
     * @param time
     * @param data
     * @throws VizException
     */
    private void populateDataTimes() throws VizException {
        DataTime[] dataTimes = null;
        for (int i = rscs.size() - 1; i >= 0; i--) {
            AbstractVizResource<?, ?> curr = rscs.get(i);
            if (curr != null) {
                dataTimes = curr.getDataTimes();
            } else {
                continue;
            }
            for (int j = 0; j < dataTimes.length; j++) {
                AbstractVizResource<?, ?> rsc = bestResTimes.get(dataTimes[j]);
                if (rsc != null && rsc != curr) {
                    rsc.remove(dataTimes[j]);
                }
                bestResTimes.put(dataTimes[j], curr);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        if (updateData instanceof PluginDataObject[]) {
            PluginDataObject[] updatePDO = (PluginDataObject[]) updateData;
            for (int i = 0; i < updatePDO.length; i++) {
                Map<String, Object> recordMap = null;
                try {
                    recordMap = RecordFactory.getInstance().loadMapFromUri(
                            updatePDO[i].toString());
                } catch (VizException e) {
                    e.printStackTrace();
                }
                AbstractVizResource<?, ?> rscToUse = null;
                for (AbstractVizResource<?, ?> rsc : rscs) {
                    if (rsc != null) {
                        AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rsc
                                .getResourceData();
                        String rscValue = arrd.getMetadataMap()
                                .get(productIdentifierKey).getConstraintValue();
                        String updateValue = recordMap
                                .get(productIdentifierKey).toString();
                        if (rscValue != null && rscValue.equals(updateValue)) {
                            rscToUse = rsc;
                            break;
                        }
                    }
                }

                // no resource found for update
                if (rscToUse == null) {
                    continue;
                }

                // We know the resource it is for
                if (enabler != null) {
                    enabler.enable(updatePDO[i], this);
                }
                DataTime updateTime = updatePDO[i].getDataTime();
                AbstractVizResource<?, ?> curRes = bestResTimes.get(updateTime);

                // we already have this time?
                if (rscToUse == curRes) {
                    continue;
                }

                // new time, rscToUse is best res
                if (curRes == null) {
                    rscToUse.getResourceData().update(
                            new PluginDataObject[] { updatePDO[i] });
                    bestResTimes.put(updateTime, rscToUse);
                } else {
                    // Check to see if rscToUse is higher res than curRes
                    int curIdx = rscs.indexOf(curRes);
                    int rscToUseIdx = rscs.indexOf(rscToUse);

                    if (curIdx == -1 /* shouldn't happen */
                            || rscToUseIdx < curIdx) {
                        // rscToUse is higher res than curRes
                        curRes.remove(updateTime);
                        rscToUse.getResourceData().update(
                                new PluginDataObject[] { updatePDO[i] });
                        bestResTimes.put(updateTime, rscToUse);
                    }
                }
            }
        }
        super.update(updateData);
    }

    /**
     * @return the resourceList
     */
    public ResourceList getResourceList() {
        return resourceList;
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] dts = super.getAvailableTimes();
        Set<DataTime> times = new HashSet<DataTime>(Arrays.asList(dts));
        dts = times.toArray(new DataTime[] {});
        Arrays.sort(dts);
        return dts;
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
        this.retrieveData = true;
        return new BestResResource(this, loadProperties);
    }

    @SuppressWarnings("unchecked")
    protected void constructInternal(LoadProperties loadProperties)
            throws VizException {
        for (int i = 0; i < resourceList.size(); i++) {
            try {
                resourceList
                        .get(i)
                        .getLoadProperties()
                        .setPerspectiveProperty(
                                loadProperties.getPerspectiveProperty());
                AbstractVizResource rsc = resourceList
                        .get(i)
                        .getResourceData()
                        .construct(resourceList.get(i).getLoadProperties(),
                                desc);
                if (rsc != null) {
                    rsc.setDescriptor(desc);
                    resourceList.get(i).setResource(rsc);
                    rscs.add(rsc);
                    rsc.getResourceData().addChangeListener(this);
                }
            } catch (NoDataAvailableException e) {
                // Do nothing
            }
        }
        if (rscs.size() == 0) {
            throw new NoDataAvailableException();
        }
        populateDataTimes();
    }

    protected HashMap<DataTime, AbstractVizResource<?, ?>> getMap() {
        return bestResTimes;
    }

    protected List<AbstractVizResource<?, ?>> getRscs() {
        return rscs;
    }

    /**
     * @return the desc
     */
    public IDescriptor getDesc() {
        return desc;
    }

    /**
     * @return the productIdentifierKey
     */
    public String getProductIdentifierKey() {
        return productIdentifierKey;
    }

    /**
     * @param productIdentifierKey
     *            the productIdentifierKey to set
     */
    public void setProductIdentifierKey(String productIdentifierKey) {
        this.productIdentifierKey = productIdentifierKey;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object data) {
        fireChangeListeners(type, data);
    }

}
