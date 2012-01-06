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

package com.raytheon.uf.viz.core.rsc;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.updater.DataUpdateTree;

/**
 * Manages a catalog of resource utilization
 * 
 * <P>
 * This class helps facilitate resource sharing and explicit memory cleanup of
 * unused resources. It should be automatically utilized by using the
 * com.raytheon.viz.core.map.MapDescriptor interface. End users should generally
 * not use this class directly.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class ResourceCatalog implements IDisposeListener {

    /** The utilization map */
    private Map<AbstractVizResource<?, ?>, Set<String>> theMapping;

    /** singleton instance */
    private static ResourceCatalog instance;

    /**
     * Get the singleton instance of the resource catalog
     * 
     * @return the system-wise catalog instance
     */
    public static ResourceCatalog getInstance() {
        if (instance == null) {
            instance = new ResourceCatalog();
        }

        return instance;
    }

    /**
     * Private constructor. Use getInstance().
     * 
     */
    private ResourceCatalog() {
        theMapping = new HashMap<AbstractVizResource<?, ?>, Set<String>>();
    }

    /**
     * Notify the catalog that a resource is part of a certain display
     * 
     * @param resource
     *            the resource
     * @param displayID
     *            the id of the display
     */
    public synchronized void addResource(AbstractVizResource<?, ?> resource,
            String displayID) {
        if (resource != null) {
            Set<String> list = theMapping.get(resource);
            if (list == null) {
                resource.registerListener(this);
                list = new HashSet<String>();
                theMapping.put(resource, list);
                // Add the request metadata to the tree

                AbstractResourceData resourceData = resource.getResourceData();

                if (resourceData instanceof AbstractRequestableResourceData) {
                    AbstractRequestableResourceData reqResourceData = (AbstractRequestableResourceData) resourceData;
                    Map<String, RequestConstraint> metadata = reqResourceData
                            .getMetadataMap();
                    Map<String, RequestConstraint> metadataCopy = new HashMap<String, RequestConstraint>(
                            metadata);
                    for (Map<String, RequestConstraint> updateMap : DataCubeContainer
                            .getBaseUpdateConstraints(metadataCopy)) {
                        DataUpdateTree.getInstance().insertCriteria(updateMap,
                                resource);
                    }
                }
            }
            list.add(displayID);
        }
    }

    /**
     * Notify the catalog that a resource is no longer part of a certain display
     * 
     * @param resource
     *            the resource
     * @param displayID
     *            the id of the display
     */
    public synchronized void removeResource(
            final AbstractVizResource<?, ?> resource, String displayID) {
        Set<String> list = theMapping.get(resource);
        if (list != null) {
            list.remove(displayID);
            if (list.size() == 0) {
                // Only dispose of resources if they have been initialized
                if (resource.getStatus() == ResourceStatus.INITIALIZED) {
                    // this needs to be async, otherwise other components like
                    // gfe can deadlock when this blocks but the UI thread is
                    // blocked waiting on a different lock
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            resource.dispose();
                        }
                    });
                } else {
                    disposed(resource);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IDisposeListener#diposed(com.raytheon.uf
     * .viz.core.rsc.AbstractVizResource)
     */
    @Override
    public void disposed(AbstractVizResource<?, ?> resource) {
        // Nothing points to this resource
        // remove it from the map, and invoke dispose.
        theMapping.remove(resource);
        if (resource.getResourceData() instanceof AbstractRequestableResourceData) {
            DataUpdateTree.getInstance().remove(resource);
        }
        if (resource instanceof IResourceGroup) {
            ((IResourceGroup) resource).getResourceList().clear();
        }
        if (resource.getResourceData() instanceof IResourceGroup) {
            ((IResourceGroup) resource.getResourceData()).getResourceList()
                    .clear();
        }
    }

}
