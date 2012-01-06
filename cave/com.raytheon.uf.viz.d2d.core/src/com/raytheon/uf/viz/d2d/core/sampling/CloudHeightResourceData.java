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
package com.raytheon.uf.viz.d2d.core.sampling;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IMenuManager;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * D2D Cloud height resource data, uses extension point to look up algorithm for
 * computing cloud height
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CloudHeightResourceData extends AbstractResourceData {

    protected static final IUFStatusHandler handler = UFStatus
            .getHandler(CloudHeightResourceData.class);

    private static final String CLOUD_HEIGHT_EXTENSION_ID = "com.raytheon.uf.viz.d2d.core.cloudHeightAlgorithm";

    public static interface ICloudHeightAlgorithm {

        /**
         * Add any context menu items used to control the algorithm, only called
         * if isEnabled(...) returned true
         * 
         * @param descriptor
         * @param manager
         * @param x
         * @param y
         */
        public void addContextMenuItems(IDescriptor descriptor,
                IMenuManager manager, int x, int y);

        /**
         * Get the cloud height algorithm string for the time and point on the
         * descriptor. Only called if isEnabled(...) returned true
         * 
         * @param descriptor
         * @param time
         * @param latLon
         * @return
         */
        public String inspect(AbstractVizResource<?, ?> reqRsc,
                Coordinate latLon);

        public boolean isEnabled(IDescriptor descriptor);

        public DataTime[] getDataTimes();

        public void dispose();

    }

    private static ICloudHeightAlgorithm DEFAULT_ALGORITHM = new ICloudHeightAlgorithm() {

        @Override
        public void addContextMenuItems(IDescriptor descriptor,
                IMenuManager manager, int x, int y) {

        }

        @Override
        public String inspect(AbstractVizResource<?, ?> reqRsc,
                Coordinate latLon) {
            return null;
        }

        @Override
        public boolean isEnabled(IDescriptor descriptor) {
            return false;
        }

        @Override
        public DataTime[] getDataTimes() {
            return new DataTime[0];
        }

        @Override
        public void dispose() {

        }

    };

    private static IConfigurationElement elementToUse = null;

    /**
     * 
     */
    public CloudHeightResourceData() {

    }

    public ICloudHeightAlgorithm getAlgorithm() {
        IConfigurationElement elementToUse = getElementToUse();
        ICloudHeightAlgorithm algorithm = null;
        try {
            algorithm = (ICloudHeightAlgorithm) (elementToUse != null ? elementToUse
                    .createExecutableExtension("class") : DEFAULT_ALGORITHM);
        } catch (CoreException e) {
            handler.handle(Priority.PROBLEM,
                    "Error creating ICloudHeightAlgorithm object", e);
        }
        if (algorithm == null) {
            algorithm = DEFAULT_ALGORITHM;
        }
        return algorithm;
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
        return new CloudHeightResource(this, loadProperties);
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

    }

    private static IConfigurationElement getElementToUse() {
        if (elementToUse == null) {
            try {
                IExtensionRegistry registry = Platform.getExtensionRegistry();
                IExtensionPoint point = registry
                        .getExtensionPoint(CLOUD_HEIGHT_EXTENSION_ID);
                if (point == null) {
                    throw new VizException(
                            "Could not find cloud height algorithm extension point");
                }

                for (IConfigurationElement element : point
                        .getConfigurationElements()) {
                    try {
                        ICloudHeightAlgorithm algorithm = (ICloudHeightAlgorithm) element
                                .createExecutableExtension("class");
                        if (algorithm != null) {
                            elementToUse = element;
                            break;
                        }
                    } catch (CoreException e) {
                        handler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            } catch (Throwable t) {
                handler.handle(Priority.PROBLEM, t.getLocalizedMessage(), t);
            }
        }
        return elementToUse;
    }

    @Override
    public int hashCode() {
        return 173;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        return true;
    }

}
