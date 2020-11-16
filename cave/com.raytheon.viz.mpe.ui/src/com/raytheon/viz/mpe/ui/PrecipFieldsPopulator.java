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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.MenuManager;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.mpe.constants.AppsDefaultsContants;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Class used to dynamically add menu precip menu items
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2011 ?          mschenke    Initial creation
 * Sep 21, 2017 6407       bkowal      Added optional menu item for GOESR SATPRE data.
 * Oct 06, 2017 6407       bkowal      Cleanup. Updates to support GOES-R SATPRE.
 * Aug 14, 2018 7434       mapeters    Listen to Apps_defaults changes
 *
 * </pre>
 *
 * @author mschenke
 */
public class PrecipFieldsPopulator extends FieldsPopulator {

    private static final Object LOCK = new Object();

    private static final MenuManager menuMgr = new MenuManager("PrecipFields",
            "com.raytheon.viz.mpe.PrecipFields");

    private static final ILocalizationPathObserver appsDefaultsObserver = new ILocalizationPathObserver() {

        @Override
        public void fileChanged(ILocalizationFile file) {
            synchronized (LOCK) {
                initialize();

                /*
                 * Clear the manager's contribution items so that they are
                 * reloaded the next time they're needed
                 */
                menuMgr.removeAll();
            }
        }
    };

    private static DisplayFieldData[] menuItems;

    private static Map<DisplayFieldData, MenuData> textMap;

    static {
        initialize();

        AppsDefaults.getInstance().addObserver(appsDefaultsObserver);
    }

    private static void initialize() {
        synchronized (LOCK) {
            boolean includeGoesRPrecip = isIncludeGoesRPrecip();

            // Create menuItems
            List<DisplayFieldData> menuDisplayFields = new LinkedList<>();
            menuDisplayFields.add(DisplayFieldData.rMosaic);
            menuDisplayFields.add(DisplayFieldData.avgrMosaic);
            menuDisplayFields.add(DisplayFieldData.maxrMosaic);
            menuDisplayFields.add(DisplayFieldData.bMosaic);
            menuDisplayFields.add(DisplayFieldData.lMosaic);
            menuDisplayFields.add(DisplayFieldData.gageOnly);
            menuDisplayFields.add(DisplayFieldData.mMosaic);
            menuDisplayFields.add(DisplayFieldData.mlMosaic);
            menuDisplayFields.add(DisplayFieldData.satPre);
            if (includeGoesRPrecip) {
                menuDisplayFields.add(DisplayFieldData.goesRSatPre);
            }
            menuDisplayFields.add(DisplayFieldData.lsatPre);
            menuDisplayFields.add(DisplayFieldData.srMosaic);
            menuDisplayFields.add(DisplayFieldData.sgMosaic);
            menuDisplayFields.add(DisplayFieldData.srgMosaic);
            menuDisplayFields.add(DisplayFieldData.p3lMosaic);
            menuDisplayFields.add(DisplayFieldData.qmosaic);
            menuDisplayFields.add(DisplayFieldData.lqmosaic);
            menuDisplayFields.add(DisplayFieldData.mlqmosaic);
            menuDisplayFields.add(DisplayFieldData.rdMosaic);
            menuDisplayFields.add(DisplayFieldData.avgrdMosaic);
            menuDisplayFields.add(DisplayFieldData.maxrdMosaic);
            menuDisplayFields.add(DisplayFieldData.bdMosaic);
            menuDisplayFields.add(DisplayFieldData.ldMosaic);
            menuDisplayFields.add(DisplayFieldData.mdMosaic);
            menuDisplayFields.add(DisplayFieldData.mldMosaic);
            menuDisplayFields.add(DisplayFieldData.srdMosaic);
            menuDisplayFields.add(DisplayFieldData.srdgMosaic);
            menuDisplayFields.add(DisplayFieldData.localField1);
            menuDisplayFields.add(DisplayFieldData.localField2);
            menuDisplayFields.add(DisplayFieldData.localField3);
            menuItems = menuDisplayFields.toArray(new DisplayFieldData[0]);

            // Create textMap
            Map<DisplayFieldData, MenuData> textMapTemp = new HashMap<>();
            textMapTemp.put(DisplayFieldData.rMosaic,
                    new MenuData("Radar Mosaic", "R"));
            textMapTemp.put(DisplayFieldData.avgrMosaic,
                    new MenuData("Average Radar Mosaic", "A"));
            textMapTemp.put(DisplayFieldData.maxrMosaic,
                    new MenuData("Max Radar Mosaic", "x"));
            textMapTemp.put(DisplayFieldData.bMosaic,
                    new MenuData("Field Bias Radar Mosaic", "F"));
            textMapTemp.put(DisplayFieldData.lMosaic,
                    new MenuData("Local Bias Radar Mosaic", "L"));
            textMapTemp.put(DisplayFieldData.gageOnly,
                    new MenuData("Gage Only Analysis", "G"));
            textMapTemp.put(DisplayFieldData.mMosaic,
                    new MenuData("Multisensor Mosaic", "M"));
            textMapTemp.put(DisplayFieldData.mlMosaic,
                    new MenuData("Local Bias Multisensor Mosaic", "a"));
            textMapTemp.put(DisplayFieldData.satPre,
                    new MenuData("Satellite Precip", "S"));
            if (includeGoesRPrecip) {
                textMapTemp.put(DisplayFieldData.goesRSatPre,
                        new MenuData("Satellite Precip (GOES-R)", "P"));
            }
            textMapTemp.put(DisplayFieldData.lsatPre,
                    new MenuData("Local Bias Satellite Precip", "o"));
            textMapTemp.put(DisplayFieldData.srMosaic,
                    new MenuData("Satellite Radar Mosaic", "e"));
            textMapTemp.put(DisplayFieldData.sgMosaic,
                    new MenuData("Satellite Gage Mosaic", "t"));
            textMapTemp.put(DisplayFieldData.srgMosaic,
                    new MenuData("Satellite Radar Gage Mosaic", "l"));
            textMapTemp.put(DisplayFieldData.p3lMosaic,
                    new MenuData("Triangulated Local Bias Mosaic", "T"));
            textMapTemp.put(DisplayFieldData.qmosaic,
                    new MenuData("Q2 Mosaic", "2"));
            textMapTemp.put(DisplayFieldData.lqmosaic,
                    new MenuData("Q2 Local Bias Mosaic", "B"));
            textMapTemp.put(DisplayFieldData.mlqmosaic,
                    new MenuData("Q2 Multi-sensor Mosaic", "i"));

            textMapTemp.put(DisplayFieldData.rdMosaic,
                    new MenuData("DP Radar Mosaic", "3"));
            textMapTemp.put(DisplayFieldData.avgrdMosaic,
                    new MenuData("DP Avg Radar Mosaic", "4"));
            textMapTemp.put(DisplayFieldData.maxrdMosaic,
                    new MenuData("DP Max Radar Mosaic", "5"));
            textMapTemp.put(DisplayFieldData.bdMosaic,
                    new MenuData("DP Field Bias Radar Mosaic", "6"));
            textMapTemp.put(DisplayFieldData.ldMosaic,
                    new MenuData("DP Local Bias Radar Mosaic", "7"));
            textMapTemp.put(DisplayFieldData.mdMosaic, new MenuData(
                    "DP Field Bias Multisensor Radar Mosaic", "8"));
            textMapTemp.put(DisplayFieldData.mldMosaic, new MenuData(
                    "DP Local Bias Multisensor Radar Mosaic", "9"));
            textMapTemp.put(DisplayFieldData.srdMosaic,
                    new MenuData("DP Satellite Radar Mosaic", "0"));
            textMapTemp.put(DisplayFieldData.srdgMosaic,
                    new MenuData("DP Satellite Radar Gage Mosaic", "1"));

            textMapTemp.put(DisplayFieldData.localField1,
                    new MenuData("Local Field #1", "d"));
            textMapTemp.put(DisplayFieldData.localField2,
                    new MenuData("Local Field #2", "f"));
            textMapTemp.put(DisplayFieldData.localField3,
                    new MenuData("Local Field #3", "O"));
            textMap = textMapTemp;
        }
    }

    private static boolean isIncludeGoesRPrecip() {
        return Boolean.TRUE
                .equals(AppsDefaultsConversionWrapper.getPropertyAsBoolean(
                        AppsDefaultsContants.APPS_DEFAULTS_USE_GOESR_PRECIP));
    }

    @Override
    protected Map<DisplayFieldData, MenuData> getTexMap() {
        synchronized (LOCK) {
            return textMap;
        }
    }

    @Override
    protected DisplayFieldData[] getMenuItems() {
        synchronized (LOCK) {
            return menuItems;
        }
    }

    @Override
    protected MenuManager getMenuManger() {
        synchronized (LOCK) {
            return menuMgr;
        }
    }

}
