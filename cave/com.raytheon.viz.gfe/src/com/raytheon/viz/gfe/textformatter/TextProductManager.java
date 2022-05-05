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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.ProductDefinition;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IAsyncStartupObjectListener;

/**
 * Manages the available text products.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 01, 2009           njensen   Initial creation
 * Jan 15, 2010  3395     ryu       Fix &quot;issued by&quot; functionality
 * Apr 24, 2013  1936     dgilling  Remove unused imports.
 * Feb 12, 2014  2591     randerso  Removed reloadModule method
 * Dec 15, 2014  14946    ryu       Add getTimeZones() method.
 * Apr 20, 2015  4027     randerso  Made fileObservers conditional as they are
 *                                  not needed in a non-GUI environment like GFE
 *                                  formatter auto-tests
 * Jul 30, 2015  4263     dgilling  Major refactor so this object can be
 *                                  initialized off UI thread.
 * Dec 14, 2015  4816     dgilling  Support refactored PythonJobCoordinator API.
 * Apr 14, 2016  5578     dgilling  Add getVarDict.
 * Nov 02, 2016  5979     njensen   Cast to Number where applicable
 * Mar 01, 2018  6947     dgilling  Get rid of cached PythonScript objects.
 * Nov 30, 2020  8284     randerso  Added a call to SiteMap.getInstance() in the
 *                                  constructor so SiteMap is initialized before
 *                                  any formatters are run since it fails to
 *                                  initialized on a Jep thread.
 *
 * </pre>
 *
 * @author njensen
 */

public class TextProductManager implements ILocalizationPathObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private String issuedBy;

    /**
     * Text product inventory. Maps product display name to product metadata.
     */
    private final Map<String, TextProductMetadata> metadata;

    /**
     * Manages the default VTEC mode for Hazard products. Maps PIL to VTEC mode
     * code.
     */
    private final Map<String, String> productDefaultVtecCoding;

    private final Object accessLock;

    public TextProductManager() {
        this.issuedBy = StringUtils.EMPTY;
        this.productDefaultVtecCoding = new HashMap<>();
        this.metadata = new HashMap<>();
        this.accessLock = new Object();

        /*
         * This is necessary since SiteMap will not initialize properly when
         * first called from a FormatterScript
         */
        SiteMap.getInstance();
    }

    public void init(boolean startListener,
            final IAsyncStartupObjectListener initListener) {
        CompletableFuture.runAsync(() -> {
            try (FormatterScript script = new FormatterScriptFactory()
                    .createPythonScript()) {
                Map<String, Object> map = new HashMap<>(2, 1f);
                map.put("paths", GfePyIncludeUtil.getTextProductsIncludePath());
                map.put("getVtecCodes", true);
                TextProductConfigData data = (TextProductConfigData) script
                        .execute("getScripts", map);
                buildTextProductMaps(data, true);
            } catch (Exception e) {
                statusHandler.error("Error building text product inventory.",
                        e);
            }

            initListener.objectInitialized();
        });

        if (startListener) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            pathMgr.addLocalizationPathObserver(GfePyIncludeUtil.TEXT_UTILITIES,
                    this);
        }
    }

    private void buildTextProductMaps(TextProductConfigData configData,
            boolean updateVtecMap) {
        synchronized (accessLock) {
            if (updateVtecMap) {
                productDefaultVtecCoding.clear();
                productDefaultVtecCoding
                        .putAll(configData.getDefaultVtecModes());
            }

            metadata.clear();
            for (TextProductMetadata textProduct : configData
                    .getProductInventory()) {
                metadata.put(textProduct.getDisplayName(), textProduct);
            }
        }
    }

    public List<String> getProductNames() {
        List<String> names = Collections.emptyList();
        synchronized (accessLock) {
            names = new ArrayList<>(metadata.keySet());
        }
        Collections.sort(names, String.CASE_INSENSITIVE_ORDER);
        return names;
    }

    public List<String> getModuleNames() {
        List<String> names = new ArrayList<>();
        synchronized (accessLock) {
            for (TextProductMetadata textProduct : metadata.values()) {
                names.add(textProduct.getModuleName());
            }
        }
        Collections.sort(names);
        return names;
    }

    public String getModuleName(String productName) {
        TextProductMetadata productMetadata = null;
        synchronized (accessLock) {
            productMetadata = metadata.get(productName);
        }

        String moduleName = (productMetadata != null)
                ? productMetadata.getModuleName()
                : null;
        return moduleName;
    }

    public String getDisplayName(String moduleName) {
        String displayName = null;
        synchronized (accessLock) {
            for (Entry<String, TextProductMetadata> entry : metadata
                    .entrySet()) {
                if (moduleName.equals(entry.getValue().getModuleName())) {
                    displayName = entry.getKey();
                    break;
                }
            }
        }
        return displayName;
    }

    public String getCombinationsFileName(String productName) {
        String filename = null;
        Object obj = getDefinitionValue(productName, "defaultEditAreas");
        if ((obj != null) && (obj instanceof String)) {
            filename = (String) obj;
        } else {
            filename = "NONE";
        }
        return filename;
    }

    public boolean isSingleZoneSelect(String productName) {
        boolean isSingle = false;
        Object obj = getDefinitionValue(productName, "singleComboOnly");
        if ((obj != null) && (obj instanceof Number)) {
            if (((Number) obj).intValue() == 1) {
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
        } else if (obj instanceof Number) {
            mapRequired = ((Number) obj).intValue() != 0;
        } else if (obj instanceof Boolean) {
            mapRequired = ((Boolean) obj).booleanValue();
        }
        return mapRequired;
    }

    public Object getDefinitionValue(String productName, String key) {
        TextProductMetadata productMetadata = null;
        synchronized (accessLock) {
            productMetadata = metadata.get(productName);
        }

        Object obj = (productMetadata != null)
                ? productMetadata.getProductDefinition().get(key)
                : "Combinations_Default_" + productName;
        return obj;
    }

    public ProductDefinition getProductDefinition(String productName) {
        TextProductMetadata productMetadata = null;
        synchronized (accessLock) {
            productMetadata = metadata.get(productName);
        }

        ProductDefinition productDef = (productMetadata != null)
                ? productMetadata.getProductDefinition()
                : null;
        return productDef;
    }

    /**
     * Returns the varDict for the given text formatter. In GFE, the varDict is
     * a Map (or dict) containing the user's selections from an optional popup
     * dialog that can appear before executing a formatter.
     * <p>
     * To retrieve the varDict this will require a call into Jep (specifically
     * FormatterRunner.py's getVarDict method) to retrieve the varDict.
     * Depending on whether or not the formatter script defines the global
     * variable variableList, this may cause an instance of ValuesDialog to
     * display.
     * <p>
     * Do NOT call this function from the UI thread of CAVE or it will deadlock
     * the application.
     *
     * @param productName
     * @param dataManager
     * @param dbId
     * @return
     */
    public String obtainVarDictSelections(String productName,
            DataManager dataManager, String dbId) {
        CompletableFuture<String> varDictJob = CompletableFuture
                .supplyAsync(() -> {
                    try (FormatterScript script = new FormatterScriptFactory()
                            .createPythonScript()) {
                        Map<String, Object> map = new HashMap<>(5, 1f);
                        map.put("paths",
                                GfePyIncludeUtil.getTextProductsIncludePath());
                        map.put("dspName", getDisplayName(productName));
                        map.put("dataMgr", dataManager);
                        map.put("ifpClient",
                                dataManager.getClient().getPythonClient());
                        map.put("issuedBy", issuedBy);
                        map.put("dataSource",
                                new DatabaseID(dbId).getModelName());
                        String varDict = (String) script.execute("getVarDict",
                                map);

                        return varDict;
                    } catch (Exception e) {
                        statusHandler.error(String.format(
                                "Error retrieving varDict for product %s",
                                productName), e);
                    }

                    return null;
                });

        try {
            return varDictJob.get();
        } catch (InterruptedException | ExecutionException e) {
            statusHandler.error(String.format(
                    "Error retrieving varDict for product %s", productName), e);
        }

        return null;
    }

    public String getVtecMessageType(String productCategory) {
        String vtec = productDefaultVtecCoding.get(productCategory);
        if (vtec == null) {
            vtec = "";
        }
        return vtec;
    }

    public Collection<String> getTimeZones(Collection<String> zones,
            String officeTimeZone) {
        CompletableFuture<Collection<String>> timeZoneJob = CompletableFuture
                .supplyAsync(() -> {
                    try (FormatterScript script = new FormatterScriptFactory()
                            .createPythonScript()) {
                        Map<String, Object> map = new HashMap<>(2, 1f);
                        map.put("zones", zones);
                        map.put("officeTZ", officeTimeZone);
                        Collection<String> timeZones = (Collection<String>) script
                                .execute("getTimeZones", map);
                        return timeZones;
                    } catch (Exception e) {
                        statusHandler.error("Exception getting time zones.", e);
                    }

                    return Collections.emptyList();
                });

        try {
            return timeZoneJob.get();
        } catch (InterruptedException | ExecutionException e) {
            statusHandler.error("Exception getting time zones.", e);
        }

        return Collections.emptyList();
    }

    public void dispose() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        pathMgr.removeLocalizationPathObserver(this);
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
        if (LocalizationManager.getInstance().getCurrentSite()
                .equals(issuedBy)) {
            this.issuedBy = "";
        } else {
            this.issuedBy = issuedBy;
        }
    }

    public String getIssuedBy() {
        return issuedBy;
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        CompletableFuture.runAsync(() -> {
            try (FormatterScript script = new FormatterScriptFactory()
                    .createPythonScript()) {
                Map<String, Object> map = new HashMap<>(2, 1f);
                map.put("paths", GfePyIncludeUtil.getTextProductsIncludePath());
                map.put("getVtecCodes", false);
                TextProductConfigData data = (TextProductConfigData) script
                        .execute("getScripts", map);
                buildTextProductMaps(data, false);
            } catch (Exception e) {
                statusHandler.error("Error building text product inventory.",
                        e);
            }

        });
    }
}
