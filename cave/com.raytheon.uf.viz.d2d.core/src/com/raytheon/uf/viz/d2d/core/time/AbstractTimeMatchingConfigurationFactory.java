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
package com.raytheon.uf.viz.d2d.core.time;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Interface for managing a time matching configuration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public abstract class AbstractTimeMatchingConfigurationFactory {
    /*
     * The well known extension point for implementing a time matching
     * configuration factory.
     */
    private static final String extensionPointId = "com.raytheon.uf.viz.d2d.core.timeMatchingConfigurationFactory";

    /**
     * Construct a TimeMatchingConfiguration for the given inputs.
     * 
     * @param loadProps
     * @param matcher
     * @param rscData
     * @param descriptor
     * @return
     * @throws VizException
     */
    public abstract TimeMatchingConfiguration getConfiguration(
            LoadProperties loadProps, D2DTimeMatcher matcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException;

    /**
     * Construct a TimeMatchingConfiguration overlay (with an existing time
     * match basis) for the given inputs.
     * 
     * @param loadProps
     * @param matcher
     * @param rscData
     * @param descriptor
     * @return
     * @throws VizException
     */
    public abstract TimeMatchingConfiguration getOverlayConfiguration(
            LoadProperties loadProps, D2DTimeMatcher matcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException;

    public abstract void resetMultiload();

    /**
     * Create the configuration factory from a well known extension point.
     * 
     * @return the first time matching configuration factory found in the
     *         extension registry.
     * @throws VizException
     */
    public static AbstractTimeMatchingConfigurationFactory constructConfigurationFactory()
            throws VizException {
        AbstractTimeMatchingConfigurationFactory manager = null;
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IConfigurationElement[] configArr = registry
                .getConfigurationElementsFor(extensionPointId);

        if (configArr != null && configArr.length > 0) {
            IConfigurationElement configElem = configArr[0];
            try {
                manager = (AbstractTimeMatchingConfigurationFactory) configElem
                        .createExecutableExtension("class");
            } catch (CoreException e) {
                throw new VizException(e);
            }
        }
        return manager;
    }
}
