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

package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.osgi.framework.Bundle;
import org.osgi.framework.wiring.BundleWiring;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.d2d.ui.actions.BlinkToggleAction;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.EnableDisableAction;
import com.raytheon.viz.ui.cmenu.ImagingAction;
import com.raytheon.viz.ui.cmenu.MoveDownAction;
import com.raytheon.viz.ui.cmenu.MoveUpAction;
import com.raytheon.viz.ui.cmenu.RemoveResourceAction;
import com.raytheon.viz.ui.cmenu.RightClickSeparator;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;

/**
 * Provide resource-based right click support. This solution was lifted from the
 * Raytheon original of the same name.
 * 
 * This menu manager works on the same basis, yet removes certain actions based
 * on the resource and the Ensemble Tool mode of legend browser vs matrix
 * navigator. For example, the action to Unload a resource is currently
 * (v.16.2.2) not vaid in matrix mode; Display Product toggle and Move Up/Down
 * actions are currently (v.16.2.2) not valid in either mode.
 * 
 * It also puts in a separator to distinguish between menu item types.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 17, 2016  13211     polster    Bastardized from the Raytheon original
 * 
 * </pre>
 * 
 * @author chammack
 * @author polster
 * @version 1
 */
public class ContextMenuManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContextMenuManager.class);

    private static Map<Class<?>, Set<AbstractRightClickAction>> contextualItems;

    static {
        contextualItems = new HashMap<>();
        // Construct the resource mapping from Eclipse plugins
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry
                .getExtensionPoint("com.raytheon.viz.ui.contextualMenu");
        if (point != null) {
            IExtension[] extensions = point.getExtensions();
            for (int i = 0; i < extensions.length; i++) {
                IConfigurationElement[] config = extensions[i]
                        .getConfigurationElements();

                for (int j = 0; j < config.length; j++) {
                    String capInterface = config[j]
                            .getAttribute("capabilityInterface");
                    if (capInterface == null) {
                        capInterface = config[j]
                                .getAttribute("capabilityClass");
                    }
                    String actionClass = config[j].getAttribute("actionClass");

                    String sortID = config[j].getAttribute("sortID");
                    if (actionClass == null) {
                        continue; // Not constructable
                    }

                    Object o;
                    try {

                        o = config[j].createExecutableExtension("actionClass");
                        Class<?> c = null;
                        if (capInterface != null) {
                            String bundleName = extensions[i].getContributor()
                                    .getName();
                            Bundle bundle = Platform.getBundle(bundleName);
                            BundleWiring wiring = bundle
                                    .adapt(BundleWiring.class);
                            c = wiring.getClassLoader().loadClass(capInterface);
                        } else {
                            // if there's no interface but there is an action,
                            // the action applies to all resources
                            c = AbstractVizResource.class;
                        }

                        Set<AbstractRightClickAction> actions = contextualItems
                                .get(c);
                        if (actions == null) {
                            actions = new TreeSet<AbstractRightClickAction>(
                                    new MyComparator());

                        }
                        AbstractRightClickAction rca = (AbstractRightClickAction) o;
                        rca.setOrder(Integer.parseInt(sortID));

                        actions.add(rca);

                        contextualItems.put(c, actions);

                    } catch (CoreException e) {
                        statusHandler.error(
                                "Error loading action class: " + actionClass,
                                e);
                    } catch (Throwable e) {
                        statusHandler.error("Error loading interface class: "
                                + capInterface, e);
                    }
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    public static void fillContextMenu(IMenuManager manager, ResourcePair vr,
            IDisplayPaneContainer container) {
        if (vr == null) {
            return;
        }

        TreeSet<AbstractRightClickAction> actionSet = new TreeSet<>(
                new MyComparator());

        Iterator<Class<?>> classIterator = contextualItems.keySet().iterator();
        while (classIterator.hasNext()) {
            Class<?> c = classIterator.next();
            boolean include = false;

            if (c.isInstance(vr.getResource())) {
                include = true;

            } else if (AbstractCapability.class.isAssignableFrom(c)
                    && vr.getResource().getCapabilities().hasCapability(
                            (Class<? extends AbstractCapability>) c)) {
                if (!vr.getResource()
                        .getCapability((Class<? extends AbstractCapability>) c)
                        .isSuppressingMenuItems()) {
                    include = true;
                }
            }

            if (include) {
                Set<AbstractRightClickAction> actions = contextualItems.get(c);
                for (AbstractRightClickAction action : actions) {
                    action.setContainer(container);
                    action.setSelectedRsc(vr);
                    if (action.isHidden()) {
                        continue;
                    }
                    /**
                     * Ignore the 'Display Product' and 'Move' actions for both
                     * Legends and Matrix mode.
                     */
                    if (action instanceof EnableDisableAction
                            || action instanceof MoveUpAction
                            || action instanceof MoveDownAction
                            || action instanceof RemoveResourceAction) {
                        continue;
                    }

                    /**
                     * TODO: (For the 16.2.2 release) Ignore the
                     * "Unload Display" and "Imaging..." actions for Matrix
                     * mode.
                     */
                    if (EnsembleTool.getInstance()
                            .getToolMode() == EnsembleTool.EnsembleToolMode.MATRIX) {
                        if (action instanceof RemoveResourceAction
                                || action instanceof ImagingAction) {
                            continue;
                        }

                    }
                    actionSet.add(action);
                }
            }
        }

        Iterator<AbstractRightClickAction> iterator = actionSet.iterator();
        while (iterator.hasNext()) {
            AbstractRightClickAction action = iterator.next();
            if (action instanceof RightClickSeparator) {
            } else {
                if (action instanceof BlinkToggleAction) {
                    manager.add(new Separator());
                }
                manager.add(action);
            }

        }
    }

    private static class MyComparator
            implements Comparator<AbstractRightClickAction> {

        public int compare(AbstractRightClickAction o1,
                AbstractRightClickAction o2) {
            if (o1.getOrder() > o2.getOrder()) {
                return 1;
            }
            return -1;
        }

    }
}
