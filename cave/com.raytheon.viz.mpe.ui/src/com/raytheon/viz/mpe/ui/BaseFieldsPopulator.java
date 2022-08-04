package com.raytheon.viz.mpe.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;

/**
 * Dynamic Field populator for the menu item BaseFields.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2011            rgeorge     Initial creation
 * Mar 14, 2013   1457     mpduff      Changed to use the ToggleGageTriangleDisplay handler.
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
public class BaseFieldsPopulator extends FieldsPopulator {

    private static DisplayFieldData[] menuItems = new DisplayFieldData[] { DisplayFieldData.p3lMosaic };

    private static MenuManager menuMgr = new MenuManager("BaseFields",
            "com.raytheon.viz.mpe.BaseFields");

    private static Map<String, String> gageMap = new HashMap<String, String>();
    static {
        gageMap.put("Gage", "Triangles");
    }

    private static Map<DisplayFieldData, MenuData> textMap = new HashMap<DisplayFieldData, MenuData>();
    static {
        textMap.put(DisplayFieldData.p3lMosaic, new MenuData("Gage Triangles",
                "G", Action.AS_CHECK_BOX,
                "com.raytheon.viz.mpe.ui.actions.ToggleGageTriangleDisplay",
                gageMap));
    }

    @Override
    protected Map<DisplayFieldData, MenuData> getTexMap() {
        return BaseFieldsPopulator.textMap;
    }

    @Override
    protected DisplayFieldData[] getMenuItems() {
        return BaseFieldsPopulator.menuItems;
    }

    @Override
    protected MenuManager getMenuManger() {
        return BaseFieldsPopulator.menuMgr;
    }
}