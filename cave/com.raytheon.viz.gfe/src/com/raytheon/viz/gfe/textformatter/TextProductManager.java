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
import com.raytheon.uf.common.python.concurrent.IPythonJobListener;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.gfe.core.IAsyncStartupObjectListener;

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
 * Dec 15, 2014  #14946    ryu         Add getTimeZones() method.
 * Apr 20, 2015  4027      randerso    Made fileObservers conditional as they are not needed
 *                                     in a non-GUI environment like GFE formatter auto-tests
 * Jul 30, 2015  4263      dgilling    Major refactor so this object can be initialized off
 *                                     UI thread.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TextProductManager implements ILocalizationFileObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private String issuedBy;

    private final PythonJobCoordinator<FormatterScript> jobCoordinator;

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

    private final LocalizationFile textProductsDir;

    public TextProductManager() {
        this.issuedBy = "";

        FormatterScriptFactory factory = new FormatterScriptFactory();
        this.jobCoordinator = PythonJobCoordinator.newInstance(factory);

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext baseContext = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);
        this.textProductsDir = pm.getLocalizationFile(baseContext,
                GfePyIncludeUtil.TEXT_PRODUCTS);

        this.productDefaultVtecCoding = new HashMap<>();
        this.metadata = new HashMap<>();
        this.accessLock = new Object();
    }

    public void init(boolean startListener,
            final IAsyncStartupObjectListener initListener) {
        TextProductConfigDataExecutor executor = new TextProductConfigDataExecutor(
                true);
        IPythonJobListener<TextProductConfigData> listener = new IPythonJobListener<TextProductConfigData>() {

            @Override
            public void jobFinished(TextProductConfigData result) {
                buildTextProductMaps(result, true);
                initListener.objectInitialized();
            }

            @Override
            public void jobFailed(Throwable t) {
                statusHandler
                        .error("Error building text product inventory.", t);
                initListener.objectInitialized();
            }
        };
        try {
            jobCoordinator.submitAsyncJob(executor, listener);
        } catch (Exception e) {
            statusHandler.error("Error building text product inventory.", e);
        }

        if (startListener) {
            textProductsDir.addFileUpdatedObserver(this);
        }
    }

    private void buildTextProductMaps(TextProductConfigData configData,
            boolean updateVtecMap) {
        synchronized (accessLock) {
            if (updateVtecMap) {
                productDefaultVtecCoding.clear();
                productDefaultVtecCoding.putAll(configData
                        .getDefaultVtecModes());
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

        String moduleName = (productMetadata != null) ? productMetadata
                .getModuleName() : null;
        return moduleName;
    }

    public String getDisplayName(String moduleName) {
        String displayName = null;
        synchronized (accessLock) {
            for (Entry<String, TextProductMetadata> entry : metadata.entrySet()) {
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
        if ((obj != null) && (obj instanceof Integer)) {
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
        TextProductMetadata productMetadata = null;
        synchronized (accessLock) {
            productMetadata = metadata.get(productName);
        }

        Object obj = (productMetadata != null) ? productMetadata
                .getProductDefinition().get(key) : "Combinations_Default_"
                + productName;
        return obj;
    }

    public ProductDefinition getProductDefinition(String productName) {
        TextProductMetadata productMetadata = null;
        synchronized (accessLock) {
            productMetadata = metadata.get(productName);
        }

        ProductDefinition productDef = (productMetadata != null) ? productMetadata
                .getProductDefinition() : null;
        return productDef;
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
        Collection<String> timeZones = Collections.emptyList();
        try {
            timeZones = jobCoordinator
                    .submitSyncJob(new TextProductTimeZonesExecutor(zones,
                            officeTimeZone));
        } catch (Exception e) {
            statusHandler.error("Exception getting time zones.", e);
        }
        return timeZones;
    }

    public void dispose() {
        textProductsDir.removeFileUpdatedObserver(this);
        jobCoordinator.shutdown();
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated
     * (com.raytheon.uf.common.localization.FileUpdatedMessage)
     */
    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        TextProductConfigDataExecutor executor = new TextProductConfigDataExecutor(
                false);
        IPythonJobListener<TextProductConfigData> listener = new IPythonJobListener<TextProductConfigData>() {

            @Override
            public void jobFinished(TextProductConfigData result) {
                buildTextProductMaps(result, false);
            }

            @Override
            public void jobFailed(Throwable t) {
                statusHandler
                        .error("Error updating text product inventory.", t);
            }
        };
        try {
            jobCoordinator.submitAsyncJob(executor, listener);
        } catch (Exception e) {
            statusHandler.error("Error updating text product inventory.", e);
        }
    }
}
