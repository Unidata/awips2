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
package com.raytheon.uf.viz.ui.menus.xml;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonCommandContribution;
import com.raytheon.uf.common.menus.xml.CommonDynamicMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonPlaceholderMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSeparatorMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleContribution;
import com.raytheon.uf.common.menus.xml.CommonToolbarSubmenuContribution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Jun 28, 2010           mnash       Initial creation
 * Jul 31, 2012  875      rferrel     Added DynamicMenuContribution.
 * Dec 11, 2013  2602     bsteffen    Load providers from extension point.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class MenuXMLMap {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DynamicMenuContribution.class);

    private static final String EXTENSION_ID = "com.raytheon.uf.viz.ui.menus.contribItemProvider";

    private static final Map<Class<? extends CommonAbstractMenuContribution>, IContribItemProvider> xmlMapping = init();

    private static Map<Class<? extends CommonAbstractMenuContribution>, IContribItemProvider> init() {
        Map<Class<? extends CommonAbstractMenuContribution>, IContribItemProvider> xmlMapping = new HashMap<Class<? extends CommonAbstractMenuContribution>, IContribItemProvider>();
        xmlMapping.put(CommonBundleMenuContribution.class,
                new BundleMenuContribution());
        xmlMapping.put(CommonCommandContribution.class,
                new CommandContribution());
        xmlMapping.put(CommonIncludeMenuContribution.class,
                new IncludeMenuContribution());
        xmlMapping.put(CommonPlaceholderMenuContribution.class,
                new PlaceholderMenuContribution());
        xmlMapping.put(CommonSeparatorMenuContribution.class,
                new SeparatorMenuContribution());
        xmlMapping.put(CommonSubmenuContribution.class,
                new SubmenuContribution());
        xmlMapping.put(CommonTitleContribution.class, new TitleContribution());
        xmlMapping.put(CommonToolbarSubmenuContribution.class,
                new ToolbarSubmenuContribution());
        xmlMapping.put(CommonDynamicMenuContribution.class,
                new DynamicMenuContribution());
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(EXTENSION_ID);
        if (point != null) {
            IExtension[] extensions = point.getExtensions();
            for (IExtension extension : extensions) {
                for (IConfigurationElement element : extension
                        .getConfigurationElements()) {
                    try {
                        CommonAbstractMenuContribution contrib = (CommonAbstractMenuContribution) element
                                .createExecutableExtension("contribution");
                        IContribItemProvider provider = (IContribItemProvider) element
                                .createExecutableExtension("itemProvider");
                        xmlMapping.put(contrib.getClass(), provider);
                    } catch (CoreException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error preparing menu contributions.", e);
                    }
                }
            }
        }
        return xmlMapping;
    }

    public static IContribItemProvider getProvider(
            Class<? extends CommonAbstractMenuContribution> contribClass) {
        return xmlMapping.get(contribClass);
    }
}
