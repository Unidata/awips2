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

package com.raytheon.viz.ui.jobs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.rsc.BlendedResource;
import com.raytheon.viz.core.rsc.BlendedResourceData;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Request products from EDEX via JMS (Receives file pointer, not base64
 * encoding of product)
 * 
 * <pre>
 * 
 *        SOFTWARE HISTORY
 *       
 *        Date       	Ticket#		Engineer	Description
 *        ------------	----------	-----------	--------------------------
 *        7/1/06                    chammack    Initial Creation.
 *        8/8/07					chammack    Refactored: moved loading logic to core.comm.Loader
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class RequestJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RequestJob.class);

    /**
     * Resource that is being added via a request job
     */
    private class Resource {
        public AbstractVizResource<?, ?> vizResource;

        public IDescriptor descriptor;
    }

    public static class Request {
        public AbstractRequestableResourceData resourceData;

        public LoadProperties loadProperties;
    }

    private final List<Request> requests;

    private boolean blendable;

    private boolean difference;

    private IDisplayPane[] displayPanes;

    /**
     * Constructor
     * 
     * @param property
     *            the layer property to build data from
     * @param timeOut
     *            time out in seconds
     * @param requests2
     * @param displayPanes
     * @param loadProperties
     *            the load properties (null will construct a default load
     *            properties)
     */
    public RequestJob(int timeOut, AbstractEditor editor, Request... requests) {
        super("Requesting EDEX Product(s)");
        this.requests = new ArrayList<Request>();
        for (Request r : requests) {
            this.requests.add(r);
        }
        this.displayPanes = editor.getDisplayPanes();

        IDisplayPane selected = getSelectedDisplayPane(editor);
        if (selected != null) {
            this.displayPanes = new IDisplayPane[] { selected };
        }

        this.blendable = false;
        this.setDifference(false);
    }

    /**
     * @param editor
     * @return the selectedDisplayPane, null if nothing is selected
     */
    private IDisplayPane getSelectedDisplayPane(AbstractEditor editor) {

        IDisplayPane selected = null;
        if (editor instanceof IMultiPaneEditor) {
            selected = ((IMultiPaneEditor) editor)
                    .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
        }
        return selected;
    }

    /**
     * 
     * @param flag
     *            the requests to be loaded as blendable resources. by default
     *            resource blending is disabled.
     */
    public void setBlendable(boolean flag) {
        this.blendable = flag;
    }

    /**
     * 
     * @return true if there are exactly two resources and the resources to be
     *         loaded by this RequestJob are set to be blendable.
     */
    public boolean isBlendable() {
        return (requests.size() == 2) && blendable;
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {

        List<Resource> resources = null;

        try {
            resources = new ArrayList<Resource>();

            for (Request request : this.requests) {
                for (IDisplayPane displayPane : displayPanes) {

                    Resource resource = new Resource();
                    try {
                        resource.vizResource = request.resourceData.construct(
                                request.loadProperties,
                                displayPane.getDescriptor());
                        if (resource.vizResource != null) {
                            resource.descriptor = displayPane.getDescriptor();
                            resources.add(resource);
                        }
                    } catch (NoDataAvailableException e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "No Data Available for "
                                                + getMetaDataValueString(request.resourceData
                                                        .getMetadataMap()), e);
                    }
                }
            }
            requests.clear();
        } catch (VizException e1) {
            final Status s = new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
                    IStatus.ERROR, "Error: " + e1.getMessage(), e1);

            return s;
        }

        if (resources.size() == 2) {

            if (isBlendable()) {
                BlendedResourceData resourceData = new BlendedResourceData();
                BlendedResource resource = new BlendedResource(resourceData,
                        new LoadProperties());

                resource.addResource(resources.get(0).vizResource);
                resource.addResource(resources.get(1).vizResource);

                Resource blendedResource = new Resource();
                blendedResource.vizResource = resource;
                blendedResource.descriptor = resources.get(0).descriptor;

                resources.clear();
                resources.add(blendedResource);

            }

        }

        try {

            for (Resource resource : resources) {
                if (resource.vizResource
                        .hasCapability(ColorableCapability.class) == false) {
                    resource.vizResource.getCapability(
                            ColorableCapability.class).setColor(
                            ColorUtil.getNewColor(displayPanes));
                }
                resource.descriptor.getResourceList().add(resource.vizResource);
            }

            HistoryList.getInstance().refreshLatestBundle();

        } catch (VizException e) {

            final Status s = new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
                    IStatus.ERROR, "Error: " + e.getMessage(), e);

            return s;
        }

        return Status.OK_STATUS;
    }

    /**
     * @param metadataMap
     * @return
     */
    private String getMetaDataValueString(
            HashMap<String, RequestConstraint> metadataMap) {
        StringBuilder valueString = new StringBuilder();
        for (RequestConstraint constraint : metadataMap.values()) {
            if (valueString.length() != 0) {
                valueString.append("::");
            }
            valueString.append(constraint.getConstraintValue());
        }
        return valueString.toString();
    }

    public void setDifference(boolean difference) {
        this.difference = difference;
    }

    public boolean isDifference() {
        return difference;
    }
}
