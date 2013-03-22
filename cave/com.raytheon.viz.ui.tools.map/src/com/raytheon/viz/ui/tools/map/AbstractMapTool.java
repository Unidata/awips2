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

package com.raytheon.viz.ui.tools.map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Adds convenience methods for editor to avoid casting
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------	----------- --------------------------
 * Oct 10, 2006              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public abstract class AbstractMapTool extends AbstractTool {

    /**
     * Constructor
     * 
     */
    public AbstractMapTool() {
        super();
    }

    /**
     * Gets the given resource class, loads the resource class if it hasn't been
     * loaded yet
     * 
     * @param resourceClass
     *            the resource to get
     * @param resourceAction
     *            to use to load the resource in the case it hasn't already been
     *            loaded
     * @return the resource matching the given class
     * @throws ExecutionException
     */
    protected AbstractVizResource<?, ?> getResource(
            Class<? extends AbstractVizResource<?, ?>> resourceClass,
            Class<? extends AbstractTool> resourceAction)
            throws ExecutionException {

        AbstractVizResource<?, ?> resourceLayer = containsResource(resourceClass);

        // attempt to load the resource through it's action
        if (resourceLayer == null) {
            try {
                resourceAction.newInstance().execute(new ExecutionEvent());
            } catch (Exception e) {
                throw new ExecutionException("Unable to load "
                        + resourceAction.getCanonicalName());
            }
        }

        resourceLayer = containsResource(resourceClass);

        if (resourceLayer == null) {
            throw new ExecutionException("Unable to load "
                    + resourceClass.getCanonicalName());
        }

        return resourceLayer;
    }

}
