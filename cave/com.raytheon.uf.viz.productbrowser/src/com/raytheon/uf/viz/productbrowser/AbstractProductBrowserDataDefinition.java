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
package com.raytheon.uf.viz.productbrowser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference.PreferenceType;

/**
 * The most basic class of a product for the product browser, every product
 * browser plugin should extend this class or one that extends this
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public abstract class AbstractProductBrowserDataDefinition<T extends AbstractResourceData> {

    // display name of product for the tree
    public String displayName;

    // the resource data to construct the resource
    protected T resourceData;

    // the load properties to tell the resource how to load
    protected LoadProperties loadProperties;

    // most will have default values of true, only a select few (beta) will have
    // a default value of false
    protected boolean defaultEnabled = true;

    // the preference to enable or disable the plugin
    protected static final String ENABLE_PLUGIN = "Enable";

    // the preference to enable or disable formatting of data
    protected static final String FORMAT_DATA = "Format Data";

    protected List<ProductBrowserPreference> preferences = null;

    /**
     * Populates the first level of the tree
     * 
     * @return
     */
    public String populateInitial() {
        // if the preference says show, or if it doesn't have a preference page
        if (isEnabled()) {
            return displayName;
        }
        return null;
    }

    /**
     * Function for taking data and renaming it for the product browser tree
     * 
     * Should implement this for building the history list as well as for
     * building the tree correctly
     * 
     * NOTE : If overriden, it is the responsibility of the new data definition
     * to sort the data
     * 
     * @param param
     * @param parameters
     * @return
     */
    public List<ProductBrowserLabel> formatData(String param,
            String[] parameters) {
        List<ProductBrowserLabel> temp = new ArrayList<ProductBrowserLabel>();
        for (int i = 0; i < parameters.length; i++) {
            temp.add(new ProductBrowserLabel(parameters[i], null));
        }
        Collections.sort(temp);
        return temp;
    }

    /**
     * Populates the second and on levels of the tree
     * 
     * @param selection
     * @return
     */
    public abstract List<ProductBrowserLabel> populateData(String[] selection);

    public abstract List<String> buildProductList(List<String> historyList);

    /**
     * Builds the resource as it should be loaded based on the selection
     * 
     * @param selection
     * @param type
     */
    public abstract void constructResource(String[] selection, ResourceType type);

    /**
     * Returns the resource data that is built for each item within the product
     * browser tree
     */
    public abstract T getResourceData();

    /**
     * Used for any other things that need to be added to certain product types.
     * 
     * @return map of resourceType -> list of displayTypes
     */
    public Map<ResourceType, List<DisplayType>> getDisplayTypes() {
        return null;
    }

    /**
     * Used for adding awesome things to the product browser per plugin. This
     * level adds a boolean as to whether it is enabled or not (the default one)
     */
    protected List<ProductBrowserPreference> configurePreferences() {
        List<ProductBrowserPreference> widgets = new ArrayList<ProductBrowserPreference>();
        ProductBrowserPreference preference = new ProductBrowserPreference();
        preference.setLabel(ENABLE_PLUGIN);
        preference.setPreferenceType(PreferenceType.BOOLEAN);
        preference
                .setTooltip("Select this to enable the product in the Product Browser");
        preference.setValue(true);
        widgets.add(preference);
        return widgets;
    }

    public List<ProductBrowserPreference> getPreferences() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        if (preferences == null) {
            preferences = configurePreferences();
            for (ProductBrowserPreference pref : configurePreferences()) {
                switch (pref.getPreferenceType()) {
                case BOOLEAN:
                    store.setDefault(pref.getLabel() + displayName,
                            (Boolean) pref.getValue());
                    pref.setValue(store.getBoolean(pref.getLabel()
                            + displayName));
                    break;
                case EDITABLE_STRING:
                    store.setDefault(pref.getLabel() + displayName,
                            (String) pref.getValue());
                    pref.setValue(store.getString(pref.getLabel() + displayName));
                    break;
                case STRING_ARRAY:
                    String[] items = (String[]) pref.getValue();
                    String temp = "";
                    for (int i = 0; i < items.length; i++) {
                        if (temp.isEmpty()) {
                            temp += items[i];
                        } else {
                            temp += "," + items[i];
                        }
                    }
                    store.setDefault(pref.getLabel() + displayName, temp);
                    pref.setValue(store
                            .getString(pref.getLabel() + displayName)
                            .split(","));
                    break;
                }
            }
        }
        return preferences;
    }

    protected ProductBrowserPreference getPreference(String label) {
        for (ProductBrowserPreference pref : getPreferences()) {
            if (pref.getLabel().equals(label)) {
                IPreferenceStore store = Activator.getDefault()
                        .getPreferenceStore();
                switch (pref.getPreferenceType()) {
                case BOOLEAN:
                    pref.setValue(store.getBoolean(pref.getLabel()
                            + displayName));
                }
                return pref;
            }
        }
        return null;
    }

    protected boolean isEnabled() {
        return (Boolean) getPreference(ENABLE_PLUGIN).getValue();
    }
}