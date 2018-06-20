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
package com.raytheon.viz.grid;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.GridMapKey;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.CubeLevel;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Loads and manages {@link GridExtension} implementations. This class provides
 * static methods mirroring GridExtension to provide a centralized point of
 * access for the extension functionality.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 15, 2017  6332     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class GridExtensionManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridExtensionManager.class);

    private static final String EXTENSION_ID = "com.raytheon.viz.grid.ext";

    private static List<GridExtension> extensions;

    private static synchronized List<GridExtension> getExtensions() {
        if (extensions == null) {
            extensions = new ArrayList<>();
            IConfigurationElement[] config = Platform.getExtensionRegistry()
                    .getConfigurationElementsFor(EXTENSION_ID);
            for (IConfigurationElement e : config) {
                try {
                    Object o = e.createExecutableExtension("class");
                    if (o instanceof GridExtension) {
                        extensions.add((GridExtension) o);
                    }
                } catch (CoreException ex) {
                    statusHandler.error(
                            "Error loading GridExtension " + e.toString(), ex);
                }
            }
        }
        return extensions;
    }

    public static IDataRecord[] loadCustomData(GridRecord record,
            IDescriptor descriptor) throws VizException {
        for (GridExtension extension : getExtensions()) {
            IDataRecord[] result = extension.loadCustomData(record, descriptor);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    public static void addToBaseTree(DataTree dataTree,
            Map<String, DerivParamDesc> derParLibrary) {
        for (GridExtension extension : getExtensions()) {
            extension.addToBaseTree(dataTree, derParLibrary);
        }
    }

    public static Set<DataTime> timeInvariantQuery(
            Map<String, RequestConstraint> query) throws VizException {
        Set<DataTime> result = null;
        for (GridExtension extension : getExtensions()) {
            Set<DataTime> extResult = extension.timeInvariantQuery(query);
            if (extResult != null) {
                if (result == null) {
                    result = new HashSet<>(extResult);
                } else {
                    result.addAll(extResult);
                }
            }
        }
        return result;
    }

    public static String get3DMasterLevel(String model) {
        for (GridExtension extension : getExtensions()) {
            String result = extension.get3DMasterLevel(model);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    public static LevelNode getCubeNode(String modelName,
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> cubeLevels) {
        for (GridExtension extension : getExtensions()) {
            LevelNode result = extension.getCubeNode(modelName, cubeLevels);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    public static Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level) {
        for (GridExtension extension : getExtensions()) {
            Object result = extension.resolvePluginStaticData(sNode, field,
                    level);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    public static GridMapKey getUpdateKey(AbstractBaseDataNode node) {
        for (GridExtension extension : getExtensions()) {
            GridMapKey result = extension.getUpdateKey(node);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

}
