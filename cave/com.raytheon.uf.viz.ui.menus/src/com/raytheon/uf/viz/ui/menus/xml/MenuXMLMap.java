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

import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonCommandContribution;
import com.raytheon.uf.common.menus.xml.CommonDynamicMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonPlaceholderMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSeparatorMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleContribution;
import com.raytheon.uf.common.menus.xml.CommonToolbarSubmenuContribution;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2010            mnash     Initial creation
 * Jul 31, 2012 875        rferrel     Added DynamicMenuContribution.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class MenuXMLMap {
    public static final Map<Class<?>, AbstractMenuContributionItem<?>> xmlMapping = new HashMap<Class<?>, AbstractMenuContributionItem<?>>();

    static {
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
    }

    public static void registerMapping(Class<?> clazz,
            AbstractMenuContributionItem<?> item) {
        xmlMapping.put(clazz, item);
    }
}
