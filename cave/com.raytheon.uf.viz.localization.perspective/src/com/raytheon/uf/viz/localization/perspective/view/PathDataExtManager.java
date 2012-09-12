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
package com.raytheon.uf.viz.localization.perspective.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.localization.adapter.LocalizationPerspectiveAdapter;
import com.raytheon.uf.viz.localization.filetreeview.PathData;

/**
 * Manager class for creating {@link PathData} objects for the localization path
 * extension point entries
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PathDataExtManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PathDataExtManager.class);

    /** Localization path extension point id */
    private static final String PATH_DEFINITION_ID = "com.raytheon.uf.viz.localization.localizationpath";

    private static final String APPLICATION_ATTR = "application";

    private static final String NAME_ATTR = "name";

    private static final String PATH_ATTR = "value";

    private static final String TYPE_ATTR = "localizationType";

    private static final String FILTER_ATTR = "extensionFilter";

    private static final String ADAPTER_ATTR = "localizationAdapter";

    private static final String RECURSIVE_ATTR = "recursive";

    /** Default application name */
    private static final String DEFAULT_APPLICATION = "Uncategorized";

    /** Default localization perspective adapter */
    private static final LocalizationPerspectiveAdapter DEFAULT_ADAPTER = new LocalizationPerspectiveAdapter();

    public static Collection<PathData> getPathData() {
        List<PathData> pathData = new ArrayList<PathData>();
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(PATH_DEFINITION_ID);
        if (point != null) {
            for (IExtension ext : point.getExtensions()) {
                for (IConfigurationElement element : ext
                        .getConfigurationElements()) {
                    PathData pd = new PathData();
                    pd.setApplication(element.getAttribute(APPLICATION_ATTR));
                    if (pd.getApplication() == null
                            || pd.getApplication().trim().isEmpty()) {
                        pd.setApplication(DEFAULT_APPLICATION);
                    }
                    pd.setName(element.getAttribute(NAME_ATTR));
                    if (pd.getName() == null || pd.getName().trim().isEmpty()) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Skipping path extension entry with no name set");
                        continue;
                    }
                    pd.setPath(element.getAttribute(PATH_ATTR));
                    if (pd.getPath() == null) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Skipping path extension entry with no path set");
                    }
                    pd.setFilter(element.getAttribute(FILTER_ATTR));
                    String recurse = element.getAttribute(RECURSIVE_ATTR);
                    pd.setRecursive(Boolean.parseBoolean(recurse));

                    pd.setType(LocalizationType.valueOf(element
                            .getAttribute(TYPE_ATTR)));
                    if (pd.getType() == null) {
                        // Skip if bad localization type specified
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Skipping path extension entry with name: "
                                        + pd.getName() + " and path: "
                                        + pd.getPath()
                                        + " with invalid localiation type: "
                                        + element.getAttribute(TYPE_ATTR));
                        continue;
                    }
                    LocalizationPerspectiveAdapter adapter = DEFAULT_ADAPTER;
                    try {
                        if (element.getAttribute(ADAPTER_ATTR) != null) {
                            adapter = (LocalizationPerspectiveAdapter) element
                                    .createExecutableExtension(ADAPTER_ATTR);
                        }
                    } catch (Throwable t) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Skipping path with name: "
                                                + pd.getName()
                                                + " and path: "
                                                + pd.getPath()
                                                + " due to error constructing adapter: "
                                                + t.getLocalizedMessage(), t);
                    }
                    pd.setAdapter(adapter);
                    pathData.add(pd);
                }
            }
        } else {
            throw new RuntimeException("Could not find extension point ("
                    + PATH_DEFINITION_ID
                    + ") from the extension point registry");
        }
        return pathData;
    }
}
