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
package com.raytheon.viz.gfe.actions;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxDefinition;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * Used by the right click menu on a WEATHER grid type, to set a discrete
 * weather element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * May 28, 2009 #2159      Richard Peter Initial Creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public abstract class AbstractSetDiscretePickupAction extends
        BaseSetDiscretePickupAction<String> {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(AbstractSetDiscretePickupAction.class);

    protected static final String DELIMITER = "|";

    protected Map<String, Menu> commonMenus = new HashMap<String, Menu>();

    public AbstractSetDiscretePickupAction(String title, Parm parm,
            String[] menuItemValues) {
        super(title, parm, menuItemValues);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getMenuCreator()
     */
    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        if (topMenu != null) {
            topMenu.dispose();
        }

        for (Menu menu : commonMenus.values()) {
            menu.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Control)
     */
    @Override
    public org.eclipse.swt.widgets.Menu getMenu(Control parent) {
        return getTopMenu(parent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        return getTopMenu(parent);
    }

    private void createCommonMenuItems() throws GFEServerException {
        DataManager dataManager = this.parm.getDataManager();

        ParmID parmId = this.parm.getParmID();
        GridType gridType = this.parm.getGridInfo().getGridType();
        String siteId = parmId.getDbId().getSiteId();
        boolean showDescription = dataManager.getSpatialDisplayManager()
                .getShowDescription();

        for (String menuItemValue : menuItemValues) {
            Menu menu = getParentMenu(menuItemValue);
            final MenuItem item = new MenuItem(menu, SWT.CASCADE);
            String uglyString = getMenuText(menuItemValue);
            WxValue wxKey;
            if (gridType.equals(GridType.WEATHER)) {
                wxKey = new WeatherWxValue(new WeatherKey(siteId, uglyString),
                        this.parm);
            } else {
                wxKey = new DiscreteWxValue(new DiscreteKey(siteId, uglyString,
                        parmId), this.parm);
            }
            if (wxKey.isValid()) {
                String pretty = wxKey.toString();
                String desc = description(showDescription, parm, wxKey);

                item.setText(pretty + desc);
                item.setData(wxKey);
                item.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        WxValue wxKey = (WxValue) ((MenuItem) e.getSource())
                                .getData();
                        updatePickupValue(wxKey);
                    }
                });
            }
        }
    }

    private Menu getTopMenu(Object parent) {
        if (topMenu == null) {
            if (parent instanceof Control) {
                topMenu = new Menu((Control) parent);
            } else {
                topMenu = new Menu((Menu) parent);
            }

            try {
                createCommonMenuItems();
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to create menu", e);
            }
        }

        return topMenu;
    }

    private Menu getParentMenu(String menuItemValue) {
        Menu retVal = topMenu;

        int lastIndex = menuItemValue.indexOf(DELIMITER);
        while (lastIndex >= 0) {
            String menuString = menuItemValue.substring(0, lastIndex);

            if (commonMenus.containsKey(menuString)) {
                retVal = commonMenus.get(menuString);
            } else {
                String menuText = menuString;
                int tempLastIndex = menuText.lastIndexOf(DELIMITER);

                if (tempLastIndex >= 0) {
                    menuText = menuString.substring(tempLastIndex + 1);
                }

                MenuItem menuItem = new MenuItem(retVal, SWT.CASCADE);
                menuItem.setText(menuText);
                retVal = new Menu(retVal);
                menuItem.setMenu(retVal);
                commonMenus.put(menuString, retVal);
            }

            lastIndex = menuItemValue.indexOf(DELIMITER, lastIndex + 1);
        }

        return retVal;
    }

    private String getMenuText(String menuItemValue) {
        String retVal = menuItemValue;

        int lastIndex = menuItemValue.lastIndexOf(DELIMITER);
        if (lastIndex >= 0) {
            retVal = menuItemValue.substring(lastIndex + 1);
        }

        return retVal;
    }

    /**
     * Set the parm state's pickup value.
     * 
     * @param wxValue
     *            the pickup value
     */
    private void updatePickupValue(WxValue wxValue) {
        this.parm.getParmState().setPickUpValue(wxValue);
        process();
    }

    private String description(boolean showIt, Parm parm, WxValue value) {
        if (!showIt) {
            return "";
        }

        StringBuilder v = new StringBuilder(" (");

        // if Discrete, use the keydef
        if (value instanceof DiscreteWxValue) {
            DiscreteKey dk = ((DiscreteWxValue) value).getDiscreteKey();
            DiscreteDefinition dd = dk.discreteDefinition();
            String compositeName = parm.getParmID().getCompositeName();
            List<String> subKeys = dk.getSubKeys();
            int i = 0;
            for (String subKey : subKeys) {
                v.append(dd.keyDesc(compositeName, subKey));
                if (i != subKeys.size() - 1) {
                    v.append(',');
                }
                i++;
            }
            v.append(')');
            return v.toString();
        }

        // if Weather
        else if (value instanceof WeatherWxValue) {
            WeatherKey wk = ((WeatherWxValue) value).getWeatherKey();
            List<WeatherSubKey> sk = wk.getSubKeys();
            String siteId = parm.getParmID().getDbId().getSiteId();
            WxDefinition wdef = WeatherSubKey.wxDef(siteId);
            int i = 0;
            for (WeatherSubKey subKey : sk) {
                if (!subKey.getCoverage().equals("<NoCov>")) {
                    String covDesc = wdef.coverageDesc(subKey.getType(),
                            subKey.getCoverage());
                    v.append(' ').append(covDesc);
                }
                if (!subKey.getIntensity().equals("<NoInten>")) {
                    String intenDesc = wdef.intensityDesc(subKey.getType(),
                            subKey.getIntensity());
                    v.append(' ').append(intenDesc);
                }
                v.append(' ').append(wdef.typeDesc(subKey.getType()));
                List<String> attributes = subKey.getAttributes();
                if (attributes.size() != 0) {
                    v.append(" [");
                    int j = 0;
                    for (String attr : attributes) {
                        v.append(wdef.attributeDesc(subKey.getType(), attr));
                        if (j != attributes.size() - 1) {
                            v.append(',');
                        }
                    }
                    v.append(']');
                }
                if (i != sk.size() - 1) {
                    v.append(',');
                }
                i++;
            }
            v.append(')');
            return v.toString();
        } else {
            return "";
        }
    }
}
