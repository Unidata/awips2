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
package com.raytheon.viz.pointdata.def;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * PlotParameterDefinitions object
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer       Description
 * ------------ ---------- -----------    --------------------------
 * 10/10/2019   71272      Mark Peters    Initial Creation
 * 12/10/2019   72280      K Sunil        Added MARKER type in the list of excluded
 * 01/13/2020   73084      K Sunil        PlotParameterDefinition class changed an attribute name.
 * </pre>
 *
 * @author mpeters
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class PlotParameterDefinitions {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotParameterDefinitions.class);

    private final Object paramDefsMapLock = new Object();

    @XmlElement
    private String plugin;

    @XmlElement(name = "plotParameterDefinition")
    private PlotParameterDefinition[] paramDefs;

    private Map<String, PlotParameterDefinition> paramDefsMap;

    private Map<String, PlotParameterDefinition> paramDefsforColorMapMap;

    private PlotParameterDefinitions() {
    }

    /**
     * @return the plugin
     */
    public String getPlugin() {
        return plugin;
    }

    public PlotParameterDefinition getParamDef(String paramDisplayName) {
        return getParamDefsMap().get(paramDisplayName);
    }

    public Collection<String> getParamDisplayNames() {
        // sorted because of TreeMap
        return Collections.unmodifiableSet(getParamDefsMap().keySet());
    }

    public Collection<String> getParamDisplayNamesForColorMap() {
        // sorted because of TreeMap
        return Collections
                .unmodifiableSet(getParamDefsMapForColorMap().keySet());
    }

    private Map<String, PlotParameterDefinition> getParamDefsMap() {
        synchronized (paramDefsMapLock) {
            if (paramDefsMap == null) {
                paramDefsMap = new TreeMap<>();
                for (PlotParameterDefinition def : paramDefs) {
                    String displayName = def.getDisplayName();
                    if (!paramDefsMap.containsKey(displayName)) {
                        paramDefsMap.put(def.getDisplayName(), def);
                    } else {
                        statusHandler
                                .warn("Multiple plot parameter definitions with display name '"
                                        + displayName + "' for plugin '"
                                        + plugin
                                        + "', the first one will be used.");
                    }
                }
            }
        }

        return paramDefsMap;
    }

    private Map<String, PlotParameterDefinition> getParamDefsMapForColorMap() {
        synchronized (paramDefsMapLock) {
            if (paramDefsforColorMapMap == null) {
                paramDefsforColorMapMap = new TreeMap<>();
                for (PlotParameterDefinition def : paramDefs) {
                    if (def.getDisplayType() != com.raytheon.viz.pointdata.PlotModelFactory.DisplayType.ARROW
                            && def.getDisplayType() != com.raytheon.viz.pointdata.PlotModelFactory.DisplayType.TABLE
                            && def.getDisplayType() != com.raytheon.viz.pointdata.PlotModelFactory.DisplayType.BARB
                            && def.getDisplayType() != com.raytheon.viz.pointdata.PlotModelFactory.DisplayType.MARKER) {
                        String displayName = def.getDisplayName();
                        if (!paramDefsforColorMapMap.containsKey(displayName)) {
                            paramDefsforColorMapMap.put(def.getDisplayName(),
                                    def);
                        } else {
                            statusHandler
                                    .warn("Multiple plot parameter definitions with display name '"
                                            + displayName + "' for plugin '"
                                            + plugin
                                            + "', the first one will be used.");
                        }
                    }
                }
            }
        }

        return paramDefsforColorMapMap;
    }
}
