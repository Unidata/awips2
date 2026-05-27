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
package com.raytheon.viz.mpe.ui;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.MenuManager;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Defines the RFC Precip field menu items.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2011  ?          rgeorge     Initial creation
 * Feb 28, 2017 6152       bkowal      Overrode {@link #menuItemEnabled(List, DisplayFieldData)}.
 *
 * </pre>
 *
 * @author rgeorge
 */

public class RFCPrecipFieldsPopulator extends FieldsPopulator {

    private static final String APPS_DEFAULTS_MPE_GENERATE_AREA_QPE = "mpe_generate_areal_qpe";

    private static MenuManager menuMgr = new MenuManager("PrecipFields",
            "com.raytheon.viz.mpe.PrecipFields");

    private static DisplayFieldData[] menuItems = new DisplayFieldData[] {
            DisplayFieldData.rfcbMosaic, DisplayFieldData.rfcmMosaic,
            DisplayFieldData.rfcMosaic };

    private static Map<DisplayFieldData, MenuData> textMap = new HashMap<>();

    static {
        textMap.put(DisplayFieldData.rfcbMosaic,
                new MenuData("RFC Field Bias Mosaic", "B"));
        textMap.put(DisplayFieldData.rfcmMosaic,
                new MenuData("RFC Multisensor Mosaic", "i"));
        textMap.put(DisplayFieldData.rfcMosaic,
                new MenuData("RFC QPE Mosaic", "C"));
    }

    @Override
    protected Map<DisplayFieldData, MenuData> getTexMap() {
        return RFCPrecipFieldsPopulator.textMap;
    }

    @Override
    protected DisplayFieldData[] getMenuItems() {
        return RFCPrecipFieldsPopulator.menuItems;
    }

    @Override
    protected MenuManager getMenuManger() {
        return RFCPrecipFieldsPopulator.menuMgr;
    }

    @Override
    protected boolean menuItemEnabled(final List<String> fields,
            final DisplayFieldData data) {
        if (data == DisplayFieldData.rfcMosaic) {
            /*
             * Custom rule for RFC Mosaic.
             */
            final String generateAreaQpe = AppsDefaults.getInstance()
                    .getToken(APPS_DEFAULTS_MPE_GENERATE_AREA_QPE);
            return (generateAreaQpe != null && !generateAreaQpe.trim().isEmpty()
                    && AppsDefaults.getInstance()
                            .consideredTrue(generateAreaQpe));
        }
        return super.menuItemEnabled(fields, data);
    }
}