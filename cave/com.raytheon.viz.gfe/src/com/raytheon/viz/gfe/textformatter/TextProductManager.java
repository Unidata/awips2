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
package com.raytheon.viz.gfe.textformatter;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.ProductDefinition;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Manages the available text products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 1, 2009             njensen     Initial creation
 * Jan 15, 2010  3395      ryu         Fix &quot;issued by&quot; functionality
 * Apr 24, 2013  1936      dgilling    Remove unused imports.
 * Feb 12, 2014  2591      randerso    Removed reloadModule method
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TextProductManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextProductManager.class);

    private String issuedBy = "";

    private FormatterScript script;

    private Map<String, String> scriptToModuleMap = new HashMap<String, String>();

    private Map<String, ProductDefinition> scriptToDefinitionMap = new HashMap<String, ProductDefinition>();

    private LocalizationFile lfConfigDir;

    private LocalizationFile lfSiteDir;

    private LocalizationFile lfUserDir;

    private TextProductListener listener;

    public TextProductManager() {
        try {
            init();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception initializing TextProductManager", e);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception initializing TextProductManager", e);
        }
    }

    private void init() throws VizException, JepException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext configContext = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.CONFIGURED);
        LocalizationContext siteContext = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationContext userContext = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

        lfConfigDir = pm.getLocalizationFile(configContext,
                GfePyIncludeUtil.TEXT_PRODUCTS);
        lfSiteDir = pm.getLocalizationFile(siteContext,
                GfePyIncludeUtil.TEXT_PRODUCTS);
        lfUserDir = pm.getLocalizationFile(userContext,
                GfePyIncludeUtil.TEXT_PRODUCTS);
        listener = new TextProductListener();
        lfConfigDir.addFileUpdatedObserver(listener);
        lfSiteDir.addFileUpdatedObserver(listener);
        lfUserDir.addFileUpdatedObserver(listener);

        script = FormatterScriptFactory.buildFormatterScript();

        buildTextProductMaps();
    }

    private void buildTextProductMaps() throws VizException, JepException {
        File configDir = lfConfigDir.getFile();
        File siteDir = lfSiteDir.getFile();
        File userDir = lfUserDir.getFile();

        scriptToModuleMap.clear();
        scriptToDefinitionMap.clear();

        HashMap<String, Object> map = new HashMap<String, Object>();
        String paths = configDir.getPath();
        if (siteDir != null && siteDir.exists()) {
            paths = siteDir + ":" + paths;
        }
        if (userDir != null && userDir.exists()) {
            paths = userDir + ":" + paths;
        }
        map.put("paths", paths);
        map.put("nameMap", scriptToModuleMap);
        map.put("definitionMap", scriptToDefinitionMap);

        script.execute("getScripts", map);
    }

    public String[] getProductNames() {
        Set<String> keySet = scriptToModuleMap.keySet();
        String[] names = keySet.toArray(new String[keySet.size()]);
        Arrays.sort(names, String.CASE_INSENSITIVE_ORDER);
        return names;
    }

    public String[] getModuleNames() {
        Collection<String> vals = scriptToModuleMap.values();
        String[] names = vals.toArray(new String[vals.size()]);
        Arrays.sort(names);
        return names;
    }

    public String getModuleName(String productName) {
        return scriptToModuleMap.get(productName);
    }

    public String getCombinationsFileName(String productName) {
        String filename = null;
        Object obj = getDefinitionValue(productName, "defaultEditAreas");
        if (obj != null && obj instanceof String) {
            filename = (String) obj;
        } else {
            filename = "NONE";
        }
        return filename;
    }

    public boolean isSingleZoneSelect(String productName) {
        boolean isSingle = false;
        Object obj = getDefinitionValue(productName, "singleComboOnly");
        if (obj != null && obj instanceof Integer) {
            if ((Integer) obj == 1) {
                isSingle = true;
            }
        }
        return isSingle;
    }

    public boolean mapRequired(String productName) {
        boolean mapRequired = false;
        Object obj = getDefinitionValue(productName, "showZoneCombiner");
        if (obj == null) {
            mapRequired = false;
        } else if (obj instanceof Integer) {
            mapRequired = ((Integer) obj).intValue() != 0;
        } else if (obj instanceof Boolean) {
            mapRequired = ((Boolean) obj).booleanValue();
        }
        return mapRequired;
    }

    public Object getDefinitionValue(String productName, String key) {
        Object obj;
        try {
            obj = scriptToDefinitionMap.get(productName).get(key);
        } catch (NullPointerException e) {
            obj = "Combinations_Default_" + productName;
        }
        return obj;
    }

    public ProductDefinition getProductDefinition(String productName) {
        return scriptToDefinitionMap.get(productName);
    }

    public String getVarDict(String productName, DataManager dataManager,
            String dbId) {
        String varDict = null;
        HashMap<String, Object> map = new HashMap<String, Object>(1);
        map.put("dspName", productName);
        map.put("dataMgr", dataManager);
        map.put("issuedBy", issuedBy);
        DatabaseID dataSource = new DatabaseID(dbId);
        map.put("dataSource", dataSource.getModelName());
        try {
            varDict = (String) script.execute("getVarDict", map);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM, "Exception getting VarDict",
                    e);
        }

        return varDict;
    }

    public String getVtecMessageType(String productCategory) {
        String vtec = null;
        HashMap<String, Object> map = new HashMap<String, Object>(1);
        map.put("productCategory", productCategory);
        try {
            vtec = (String) script.execute("getVTECMessageType", map);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception getting VTECMessageType", e);
        }
        return vtec;
    }

    public void dispose() {
        script.dispose();

        lfConfigDir.removeFileUpdatedObserver(listener);
        lfSiteDir.removeFileUpdatedObserver(listener);
        lfUserDir.removeFileUpdatedObserver(listener);
    }

    /**
     * Get the value of the "mapNameForCombinations" entry of the definition for
     * productName. In most cases, this is a simple String, but some definitions
     * supply a List of Strings, to specify both public and marine zones, for
     * example.
     * 
     * @param productName
     *            The name of the product whose map name is to be obtained
     * @return The map name from the Python script. This may be a simple String,
     *         or a List of Strings.
     */
    public Object getMapNameForCombinations(String productName) {
        Object mapName = null;
        mapName = getDefinitionValue(productName, "mapNameForCombinations");
        return mapName;
    }

    public void setIssuedBy(String issuedBy) {
        if (LocalizationManager.getInstance().getCurrentSite().equals(issuedBy)) {
            this.issuedBy = "";
        } else {
            this.issuedBy = issuedBy;
        }
    }

    public String getIssuedBy() {
        return issuedBy;
    }

    private class TextProductListener implements ILocalizationFileObserver {
        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    try {
                        buildTextProductMaps();
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Exception updating TextProductManager", e);
                    } catch (JepException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Exception updating TextProductManager", e);
                    }
                }
            });
        }
    }
}
