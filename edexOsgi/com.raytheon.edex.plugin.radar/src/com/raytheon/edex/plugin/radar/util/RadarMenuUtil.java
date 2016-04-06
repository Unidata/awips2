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
package com.raytheon.edex.plugin.radar.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;
import com.raytheon.uf.common.dataplugin.radar.util.SsssRadarUtil;
import com.raytheon.uf.common.dataplugin.radar.util.TerminalRadarUtils;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuItem;
import com.raytheon.uf.common.menus.xml.CommonMenuContributionFile;
import com.raytheon.uf.common.menus.xml.CommonSeparatorMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.menus.AbstractMenuUtil;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Builds menus using JAXB
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2010            mnash       Initial creation
 * Feb 25, 2013 DR14418    zwang       Change radar menu to dual pol style
 * 03/07/2013   DR15495    zwang       Handle additional elevation for ssss radars
 * Mar 06, 2014   2876      mpduff     New NDM plugin.
 * Sep 08, 2015 ASM #17944 D. Friedman Handle other elevation list files.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarMenuUtil extends AbstractMenuUtil implements
        INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarMenuUtil.class);

    private final int NUM_POSSIBLE_RADARS = 25;

    /**
     * 
     */
    public RadarMenuUtil() {
        setSkipParse(false);
    }

    @Override
    public void createMenus() {
        statusHandler.info("Creating radar menus...");

        RadarsInUseUtil.setParsed(false);
        VariableSubstitution[] vars = null;
        // loop through all the radars
        Map<String, List<Double>> map = TerminalRadarUtils
                .parseTerminalRadarFile();
        String path = "menus" + File.separator + "radar" + File.separator;
        CommonMenuContributionFile menuContributionFile = new CommonMenuContributionFile();
        CommonIncludeMenuItem includeMenuItem = null;

        // Build radar menu for all stations
        List<String> radars = RadarsInUseUtil.getSite(getSite(),
                RadarsInUseUtil.MOSAIC_CONSTANT);

        MenuTemplateFile menuTemplateFile = new MenuTemplateFile();
        menuTemplateFile.contributions = new CommonIncludeMenuContribution[radars
                .size()];

        CommonIncludeMenuContribution includeMenuContribution = null;
        for (int i = radars.size() - 1; i >= 0; i--) {
            includeMenuContribution = new CommonIncludeMenuContribution();
            includeMenuContribution.substitutions = vars;

            boolean terminal = TerminalRadarUtils.isTerminalRadar(radars.get(i)
                    .toLowerCase());
            if (terminal) {
                List<Double> elevations = map.get(radars.get(i));
                includeMenuContribution.fileName = new File(path + "dualPol"
                        + File.separator + File.separator
                        + "baseTerminalLocalRadarMenu.xml");
                vars = new VariableSubstitution[(elevations.size() + 1)
                        + NUM_POSSIBLE_RADARS + 1];
                vars[0] = new VariableSubstitution();
                vars[0].key = "icao";
                vars[0].value = radars.get(i);
                for (int j = 1; j <= elevations.size(); j++) {
                    vars[j] = new VariableSubstitution();
                    vars[j].key = "elev" + (j - 1);
                    vars[j].value = String.valueOf(elevations.get(j - 1));
                }
                for (int j = 1; j <= elevations.size(); j++) {
                    vars[j + elevations.size()] = new VariableSubstitution();
                    vars[j + elevations.size()].key = "suppressErrors"
                            + (j - 1);
                    vars[j + elevations.size()].value = "false";
                }
                for (int j = elevations.size() + 1; j <= NUM_POSSIBLE_RADARS; j++) {
                    vars[j + elevations.size()] = new VariableSubstitution();
                    vars[j + elevations.size()].key = "suppressErrors"
                            + (j - 1);
                    vars[j + elevations.size()].value = "true";
                }
                includeMenuContribution.substitutions = vars;
                terminal = true;
            } else {
                if (SsssRadarUtil.isSsssRadar(radars.get(i).toLowerCase())) {
                    String ssssRadar = radars.get(i).toLowerCase();
                    includeMenuContribution.fileName = new File(path
                            + ssssRadar + File.separator
                            + "baseLocalRadarMenu.xml");
                } else {
                    includeMenuContribution.fileName = new File(path
                            + "dualPol" + File.separator
                            + "baseLocalRadarMenu.xml");
                }
                vars = new VariableSubstitution[1];
                vars[0] = new VariableSubstitution();
                vars[0].key = "icao";
                vars[0].value = radars.get(i);
                includeMenuContribution.substitutions = vars;
            }
            menuTemplateFile.contributions[radars.size() - 1 - i] = includeMenuContribution;
        }

        Arrays.sort(menuTemplateFile.contributions);
        // only want 18 radars in the dial radar menu, otherwise put it in
        // submenus
        if (menuTemplateFile.contributions.length > 18) {
            double numMenus = Math
                    .ceil(((double) menuTemplateFile.contributions.length) / 18);
            int perMenu = (int) (menuTemplateFile.contributions.length
                    / numMenus + 1);
            statusHandler.info("For " + menuTemplateFile.contributions.length
                    + " dial radars, menus have increased to " + (int) numMenus
                    + " with an average of " + perMenu + " per menu");
            List<CommonAbstractMenuContribution> list = Arrays
                    .asList(menuTemplateFile.contributions);
            menuTemplateFile.contributions = new CommonSubmenuContribution[(int) numMenus];

            int count = 0;
            for (int i = 0; i < numMenus; i++) {
                menuTemplateFile.contributions[i] = new CommonSubmenuContribution();
                int numCount = 0;
                if (list.size() - count < perMenu) {
                    numCount = list.size() - count;
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).contributions = new CommonIncludeMenuContribution[list
                            .size() - count];
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).menuText = ((CommonIncludeMenuContribution) list
                            .get(count)).substitutions[0].value
                            + "-"
                            + ((CommonIncludeMenuContribution) list.get(perMenu
                                    * i + list.size() - count - 1)).substitutions[0].value;
                } else {
                    numCount = perMenu;
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).contributions = new CommonIncludeMenuContribution[perMenu];
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).menuText = ((CommonIncludeMenuContribution) list
                            .get(count)).substitutions[0].value
                            + "-"
                            + ((CommonIncludeMenuContribution) list.get(perMenu
                                    * (i + 1) - 1)).substitutions[0].value;
                }
                for (int j = 0; j < numCount; j++) {
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).contributions[j] = list
                            .get(count);
                    count++;
                }
            }
        }

        toXml(menuTemplateFile, "menus" + File.separator + "radar"
                + File.separator + "dialRadars.xml");
        
        // now on to TDWR radars
        radars = RadarsInUseUtil.getSite(getSite(),
                RadarsInUseUtil.TDWR_CONSTANT);

     // create MenuTemplateFile for the dialRadars.xml
        menuTemplateFile = new MenuTemplateFile();
        menuTemplateFile.contributions = new CommonIncludeMenuContribution[radars
                .size()];

        includeMenuContribution = null;
        for (int i = radars.size() - 1; i >= 0; i--) {
            includeMenuContribution = new CommonIncludeMenuContribution();
            includeMenuContribution.substitutions = vars;

            boolean terminal = TerminalRadarUtils.isTerminalRadar(radars.get(i)
                    .toLowerCase());
            if (terminal) {
                List<Double> elevations = map.get(radars.get(i));
                includeMenuContribution.fileName = new File(path + "dualPol"
                        + File.separator + File.separator
                        + "baseTerminalLocalRadarMenu.xml");
                vars = new VariableSubstitution[(elevations.size() + 1)
                        + NUM_POSSIBLE_RADARS + 1];
                vars[0] = new VariableSubstitution();
                vars[0].key = "icao";
                vars[0].value = radars.get(i);
                for (int j = 1; j <= elevations.size(); j++) {
                    vars[j] = new VariableSubstitution();
                    vars[j].key = "elev" + (j - 1);
                    vars[j].value = String.valueOf(elevations.get(j - 1));
                }
                for (int j = 1; j <= elevations.size(); j++) {
                    vars[j + elevations.size()] = new VariableSubstitution();
                    vars[j + elevations.size()].key = "suppressErrors"
                            + (j - 1);
                    vars[j + elevations.size()].value = "false";
                }
                for (int j = elevations.size() + 1; j <= NUM_POSSIBLE_RADARS; j++) {
                    vars[j + elevations.size()] = new VariableSubstitution();
                    vars[j + elevations.size()].key = "suppressErrors"
                            + (j - 1);
                    vars[j + elevations.size()].value = "true";
                }
                includeMenuContribution.substitutions = vars;
                terminal = true;
            } else {
                if (SsssRadarUtil.isSsssRadar(radars.get(i).toLowerCase())) {
                    String ssssRadar = radars.get(i).toLowerCase();
                    includeMenuContribution.fileName = new File(path
                            + ssssRadar + File.separator
                            + "baseLocalRadarMenu.xml");
                } else {
                    includeMenuContribution.fileName = new File(path
                            + "dualPol" + File.separator
                            + "baseLocalRadarMenu.xml");
                }
                vars = new VariableSubstitution[1];
                vars[0] = new VariableSubstitution();
                vars[0].key = "icao";
                vars[0].value = radars.get(i);
                includeMenuContribution.substitutions = vars;
            }
            menuTemplateFile.contributions[radars.size() - 1 - i] = includeMenuContribution;
        }

        Arrays.sort(menuTemplateFile.contributions);
        
        // only want 12 radars in each menu, otherwise put it in submenus
        if (menuTemplateFile.contributions.length > 12) {
            double numMenus = Math
                    .ceil(((double) menuTemplateFile.contributions.length) / 12);
            int perMenu = (int) (menuTemplateFile.contributions.length
                    / numMenus + 1);
            statusHandler.info("For " + menuTemplateFile.contributions.length
                    + " dial radars, menus have increased to " + (int) numMenus
                    + " with an average of " + perMenu + " per menu");
            List<CommonAbstractMenuContribution> list = Arrays
                    .asList(menuTemplateFile.contributions);
            menuTemplateFile.contributions = new CommonSubmenuContribution[(int) numMenus];

            int count = 0;
            for (int i = 0; i < numMenus; i++) {
                menuTemplateFile.contributions[i] = new CommonSubmenuContribution();
                int numCount = 0;
                if (list.size() - count < perMenu) {
                    numCount = list.size() - count;
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).contributions = new CommonIncludeMenuContribution[list
                            .size() - count];
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).menuText = ((CommonIncludeMenuContribution) list
                            .get(count)).substitutions[0].value
                            + "-"
                            + ((CommonIncludeMenuContribution) list.get(perMenu
                                    * i + list.size() - count - 1)).substitutions[0].value;
                } else {
                    numCount = perMenu;
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).contributions = new CommonIncludeMenuContribution[perMenu];
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).menuText = ((CommonIncludeMenuContribution) list
                            .get(count)).substitutions[0].value
                            + "-"
                            + ((CommonIncludeMenuContribution) list.get(perMenu
                                    * (i + 1) - 1)).substitutions[0].value;
                }
                for (int j = 0; j < numCount; j++) {
                    ((CommonSubmenuContribution) menuTemplateFile.contributions[i]).contributions[j] = list
                            .get(count);
                    count++;
                }
            }
        }

        toXml(menuTemplateFile, "menus" + File.separator + "radar"
                + File.separator + "tdwrRadars.xml");

        menuContributionFile = new CommonMenuContributionFile();

        menuContributionFile.contribution = new CommonIncludeMenuItem[1];
        includeMenuItem = menuContributionFile.contribution[0] = new CommonIncludeMenuItem();
        includeMenuItem.installationLocation = "menu:radar?after=RADAR_MENU_START";
        includeMenuItem.fileName = new File(path + "baseRadarMenu.xml");
        vars = includeMenuItem.substitutions = new VariableSubstitution[1];
        vars[0] = new VariableSubstitution();
        vars[0].key = "mosaicIcaoList";
        vars[0].value = "";
        for (String icao : RadarsInUseUtil.getSite(getSite(),
                RadarsInUseUtil.MOSAIC_CONSTANT)) {
            if (!vars[0].value.isEmpty()) {
                vars[0].value += ",";
            }
            vars[0].value += icao;
        }
        toXml(menuContributionFile, "menus" + File.separator + "radar"
                + File.separator + "radarindex.xml");

        statusHandler.info("Finished processing radar menus.");
    }

    public void setSkipParse(boolean rebuild) {
        RadarsInUseUtil.setParsed(rebuild);
    }

    @Override
    public void notify(String fileName, File file) {
        if ("tdwrElevations.txt".equals(fileName)) {
            saveFile(file, TerminalRadarUtils.getElevationsFile());
            setSkipParse(false);
            createMenus();
        } else if ("elevationLists.txt".equals(fileName) ||
                "ssssElevationLists.txt".equals(fileName)) {
            saveFile(file, getRadarElevationLocalizationFile(fileName));
        }
        statusHandler.handle(Priority.INFO,
                "Successfully processed " + file.getAbsolutePath());
    }

    private LocalizationFile getRadarElevationLocalizationFile(String fileName) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        return pathMgr.getLocalizationFile(context,
                "radar" + File.separator + fileName);
    }

    private void saveFile(File file, LocalizationFile outFile) {
        if ((file != null) && file.exists()) {
            InputStream fis = null;
            OutputStream fos = null;
            try {
                fis = new FileInputStream(file);
                fos = outFile.openOutputStream();
                try {
                    FileUtil.copy(fis, fos);
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not read file: " + file.getName(), e);

                }
            } catch (LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to open localization file for output: "
                        + outFile, e);
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to find file: "
                        + file.getName(), e);
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
                if (fos != null) {
                    try {
                        fos.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.menus.AbstractMenuUtil#checkCreated()
     */
    @Override
    public boolean checkCreated() {
        return super.checkCreated("radarsInUse.txt", "radar");
    }
}
