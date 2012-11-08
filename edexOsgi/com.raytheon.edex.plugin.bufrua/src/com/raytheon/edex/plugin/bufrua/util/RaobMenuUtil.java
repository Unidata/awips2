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
package com.raytheon.edex.plugin.bufrua.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonPlaceholderMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.menus.AbstractMenuUtil;

/**
 * Builds the menus for raobs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RaobMenuUtil extends AbstractMenuUtil {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RaobMenuUtil.class);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.menus.AbstractMenuUtil#createMenus()
     */
    @Override
    public void createMenus() {
        statusHandler.info("Creating raob menus...");
        List<CommonAbstractMenuContribution> contributions = new ArrayList<CommonAbstractMenuContribution>();
        RaobSitesInUseUtil.setParsed(false);
        List<UpperAirSite> localSites = RaobSitesInUseUtil.getSite(getSite(),
                RaobSitesInUseUtil.UPPER_AIR_CONSTANT);
        List<UpperAirSite> otherSites = RaobSitesInUseUtil.getSite(getSite(),
                RaobSitesInUseUtil.LOCAL_UPPER_AIR_CONSTANT);
        MenuTemplateFile menuTemplate = new MenuTemplateFile();
        menuTemplate.contributions = new CommonAbstractMenuContribution[(localSites
                .size()) + 1];
        contributions
                .addAll(Arrays.asList(processMenuContribution(localSites)));
        if (otherSites.size() > 0) {
            CommonSubmenuContribution submenuCont = new CommonSubmenuContribution();
            submenuCont.id = "submenuCont_raob";
            submenuCont.menuText = "Local";
            submenuCont.contributions = new CommonSubmenuContribution[otherSites
                    .size()];
            submenuCont.contributions = processMenuContribution(otherSites);
            contributions.add(submenuCont);
        }
        menuTemplate.contributions = contributions
                .toArray(new CommonAbstractMenuContribution[contributions
                        .size()]);
        if (localSites.size() == 0 && otherSites.size() == 0) {
            menuTemplate.contributions = new CommonAbstractMenuContribution[1];
            menuTemplate.contributions[0] = new CommonPlaceholderMenuContribution();
            menuTemplate.contributions[0].id = "upperAirPlaceholder";
            ((CommonPlaceholderMenuContribution) menuTemplate.contributions[0]).menuText = "Not Configured";
        }
        toXml(menuTemplate, "menus" + File.separator + "upperair"
                + File.separator + "baseRAOB.xml");
        statusHandler.info("Finished processing raob menus.");
    }

    /**
     * Takes the size of the radar list and loops through to make menu items
     * 
     * @param menuTemplate
     * @param sites
     * @return
     */
    private CommonAbstractMenuContribution[] processMenuContribution(
            List<UpperAirSite> sites) {
        CommonAbstractMenuContribution[] menuConts = new CommonAbstractMenuContribution[(sites
                .size())];
        CommonBundleMenuContribution bundleCont;
        for (int i = 0; i < sites.size(); i++) {
            VariableSubstitution vars = new VariableSubstitution();
            vars.key = "stationId";
            vars.value = sites.get(i).getSiteId();
            bundleCont = new CommonBundleMenuContribution();
            bundleCont.text = sites.get(i).getCity();
            bundleCont.editorType = "gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor";
            bundleCont.id = "raob_local_" + sites.get(i).getIcao();
            bundleCont.bundleFile = "bundles" + File.separator
                    + "UpperAirRaob.xml";
            bundleCont.substitutions = new VariableSubstitution[1];
            bundleCont.substitutions[0] = vars;
            menuConts[i] = bundleCont;
        }
        return menuConts;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.menus.AbstractMenuUtil#checkCreated()
     */
    @Override
    public boolean checkCreated() {
        return super.checkCreated("raobSitesInUse.txt", "upperair");
    }
}
