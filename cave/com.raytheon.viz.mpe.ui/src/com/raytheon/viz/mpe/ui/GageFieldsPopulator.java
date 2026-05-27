package com.raytheon.viz.mpe.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;

import com.raytheon.uf.common.ohd.AppsDefaults;

public class GageFieldsPopulator extends FieldsPopulator {

    private DisplayFieldData[] menuItems = new DisplayFieldData[] {
            DisplayFieldData.qc_precipitation,
            DisplayFieldData.qc_temperatures,
            DisplayFieldData.qc_freezinglevel, DisplayFieldData.savelevel2 };

    private static MenuManager menuMgr = new MenuManager("Gages",
            "com.raytheon.viz.mpe.Gages");

    private static Map<DisplayFieldData, MenuData> textMap = new HashMap<DisplayFieldData, MenuData>();
    static {

        textMap.put(DisplayFieldData.qc_precipitation, new MenuData(
                "QC Precipitation...", "P", Action.AS_PUSH_BUTTON,
                "com.raytheon.viz.mpe.ui.actions.qcPrecip", new HashMap()));

        textMap.put(DisplayFieldData.qc_temperatures, new MenuData(
                "QC Temperatures...", "T", Action.AS_PUSH_BUTTON,
                "com.raytheon.viz.mpe.ui.actions.qcTemp", new HashMap()));

        textMap.put(DisplayFieldData.qc_freezinglevel, new MenuData(
                "QC Freezing Level...", "F", Action.AS_PUSH_BUTTON,
                "com.raytheon.viz.mpe.ui.actions.qcFreeze", new HashMap()));

        textMap.put(DisplayFieldData.savelevel2, new MenuData(
                "Save Level 2 Data", "S", Action.AS_PUSH_BUTTON,
                "com.raytheon.viz.mpe.ui.actions.savelevel2", new HashMap()));

    }

    @Override
    protected Map<DisplayFieldData, MenuData> getTexMap() {
        return GageFieldsPopulator.textMap;
    }

    @Override
    protected DisplayFieldData[] getMenuItems() {
        if (MPEDisplayManager.isMpeQcOptionEnabled()) {
            return menuItems;
        }
        return new DisplayFieldData[] {};
    }

    protected IContributionItem[] getContributionItems() {
        AppsDefaults defaults = AppsDefaults.getInstance();
        String list = defaults.getToken("mpe_generate_list");
        List<String> fields = new ArrayList<String>();
        if (list != null) {
            fields = new ArrayList<String>(Arrays.asList(list.split("[,]")));
            Collections.sort(fields);
        }
        for (DisplayFieldData data : getMenuItems()) {

            final int found = Collections.binarySearch(fields, data.name(),
                    new Comparator<String>() {
                        @Override
                        public int compare(String o1, String o2) {
                            return o1.compareToIgnoreCase(o2);
                        }
                    });
            boolean enabled = found >= 0;

            if (!enabled && MPEDisplayManager.isMpeQcOptionEnabled()) {
                enabled = true;
            }

            ActionContributionItem itemFound = (ActionContributionItem) getMenuManger()
                    .find(getTexMap().get(data).handler);
            if (itemFound == null) {
                itemFound = (ActionContributionItem) getMenuManger().find(
                        getTexMap().get(data).text);
            }
            if (itemFound == null) {
                getMenuManger().add(
                        new ActionContributionItem(new DisplayFieldAction(data,
                                enabled)));
            } else {
                itemFound.getAction().setEnabled(enabled);
            }
        }

        return getMenuManger().getItems();
    }

    @Override
    protected MenuManager getMenuManger() {
        return GageFieldsPopulator.menuMgr;
    }

}
