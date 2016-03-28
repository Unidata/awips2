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
package com.raytheon.uf.edex.plugin.redbook.ingest;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.redbook.ingest.xml.MenuEntry;
import com.raytheon.uf.edex.plugin.redbook.ingest.xml.MenuEntryType;
import com.raytheon.uf.edex.plugin.redbook.ingest.xml.RedbookMenusXML;
import com.raytheon.uf.edex.plugin.redbook.menu.RedbookCpcMenuUtil;
import com.raytheon.uf.edex.plugin.redbook.menu.RedbookHazardsMenuUtil;
import com.raytheon.uf.edex.plugin.redbook.menu.RedbookHpcMenuUtil;
import com.raytheon.uf.edex.plugin.redbook.menu.RedbookMenuUtil;
import com.raytheon.uf.edex.plugin.redbook.menu.RedbookMpcMenuUtil;
import com.raytheon.uf.edex.plugin.redbook.menu.RedbookNcoMenuUtil;
import com.raytheon.uf.edex.plugin.redbook.menu.RedbookUaMenuUtil;

/**
 * This class updates the Redbook menus when Redbook AWIPS1 NDM files are
 * dropped into EDEX ingest.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2015   4030     mpduff      Initial creation
 * Mar 19, 2015   4310     mpduff      Some values must be trimmed.
 * Jun 26, 2015   4512     mapeters    Renamed from NdmMenuConverter, automatically
 *                                     runs and updates menus when A1 files dropped in.
 * Jul 14, 2015   4512     mapeters    Don't set product buttons' files to null.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RedbookNdmMenuSubscriber extends AbstractRedbookNdmSubscriber {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookNdmMenuSubscriber.class);

    private static final String WMO = "wmo";

    private static final String HPC_FILE = "redbookHPCMenus.txt";

    private static final String CPC_FILE = "redbookCPCMenus.txt";

    private static final String NCO_FILE = "redbookNCOMenus.txt";

    private static final String HAZARDS_FILE = "redbookHazardMenus.txt";

    private static final String MARINE_FILE = "redbookMarineMenus.txt";

    private static final String UPPER_AIR_FILE = "redbookUpperAirMenus.txt";

    /**
     * Build a {@link RedbookMenusXML} object from the NDM file with the given
     * file name and call for the menu to be updated from the XML object.
     * 
     * @param menuFileName
     *            the name of the A1 NDM menu file
     */
    public static void buildMenuXml(String menuFileName) {
        statusHandler.info("Processing " + menuFileName);
        RedbookMenusXML menuXml = new RedbookMenusXML();
        MenuEntry titleMenuEntry;
        int sepCounter = 0;
        List<MenuEntry> subMenuList = new ArrayList<>();

        List<String> menuFileLines = getNdmFileLines(menuFileName);
        List<String> dataKeysLines = getNdmFileLines(DATA_KEYS_FILE_NAME);
        List<String> depictKeysLines = getNdmFileLines(DEPICT_KEYS_FILE_NAME);
        List<String> productButtonsLines = getNdmFileLines(PRODUCT_BUTTONS_FILE_NAME);
        Map<String, String> menuTextMap = getMenuTextMap(productButtonsLines);
        Map<String, String> dataKeyMap = getSubstitutionMap(dataKeysLines);

        MenuEntry subMenuEntry = null;

        int subMenuCount = -1;
        for (String line : menuFileLines) {
            line = line.trim();
            if (line.startsWith("submenu")) {
                subMenuCount++;
                if (line.contains("&")) {
                    line = line.replace("&", "&&");
                }

                String[] parts = line.split(":");
                String text = parts[1].replace("\"", "");

                subMenuEntry = new MenuEntry();
                subMenuEntry.setFile(null);
                subMenuEntry.setType(MenuEntryType.Submenu);
                subMenuEntry.setText(text.trim());

                subMenuList.add(subMenuEntry);
            } else if (line.startsWith("title")) {
                String[] parts = line.split(":");
                String text = parts[1].trim().replace("\"", "");
                text = "------ " + text + " ------";
                titleMenuEntry = new MenuEntry();
                titleMenuEntry.setFile(null);
                titleMenuEntry.setType(MenuEntryType.Title);
                titleMenuEntry.setText(text);
                titleMenuEntry.setId(text);
                subMenuList.get(subMenuCount).addMenuEntry(titleMenuEntry);
            } else if (line.startsWith("productButton")) {
                String[] parts = line.split(":");
                MenuEntry me = new MenuEntry();
                /*
                 * Intentionally don't set MenuEntry's file, as it correctly
                 * defaults to bundles/Redbook.xml.
                 */
                me.setType(MenuEntryType.ProductButton);
                /*
                 * There are certain productButtons in the NCO menu with data
                 * keys in the (25000 range) and certain productButtons in the
                 * HPC menu with data keys in the (3313800000 range). These
                 * don't map to anything, which results in those menu items not
                 * being created. The site will need to fix this after
                 * generating the new menus.
                 */
                String dataKey = parts[1].trim().substring(0, 4);
                StringBuilder subValue = new StringBuilder();
                String menuText = menuTextMap.get(dataKey);
                if (menuText == null) {
                    /*
                     * Ignore entries that don't have corresponding entries in
                     * the product buttons file.
                     */
                    continue;
                }
                // Find the matching value
                for (String depictKeyLine : depictKeysLines) {
                    if (depictKeyLine.trim().startsWith(dataKey)) {
                        String[] depictKeyParts = depictKeyLine.split("\\|");
                        me.setText(menuText);
                        me.setId(depictKeyParts[6].trim());
                        subMenuList.get(subMenuCount).addMenuEntry(me);

                        String[] subParts = depictKeyParts[2].split(",");
                        MenuEntry subEntry = new MenuEntry();
                        subEntry.setFile(null);
                        subEntry.setType(MenuEntryType.Substitute);
                        subEntry.setKey(WMO);

                        for (String subPart : subParts) {
                            subPart = subPart.trim();
                            for (String key : dataKeyMap.keySet()) {
                                key = key.trim();
                                if (key.startsWith(subPart)) {
                                    subValue.append(dataKeyMap.get(key))
                                            .append(",");
                                    break;
                                }
                            }
                        }

                        String subValueStr = subValue.toString();
                        subValueStr = StringUtils.removeEnd(subValueStr, ",");
                        subEntry.setValue(subValueStr);
                        me.addMenuEntry(subEntry);
                        break;
                    }
                }
            } else if (line.startsWith("endSubmenu")) {
                // subMenuList.add(subMenuEntry);
                MenuEntry closedSubMenu = subMenuList.remove(subMenuCount);
                subMenuCount--;
                if (subMenuCount == -1) {
                    menuXml.addMenuEntry(closedSubMenu);
                } else {
                    subMenuList.get(subMenuCount).addMenuEntry(closedSubMenu);
                }
            } else if (line.startsWith("separator")) {
                MenuEntry sep = new MenuEntry();
                sep.setFile(null);
                sep.setType(MenuEntryType.Separator);
                sep.setId("Separator" + sepCounter++);
                subMenuList.get(subMenuCount).addMenuEntry(sep);
            }
        }

        updateMenu(menuFileName, menuXml);
    }

    /**
     * Get a map of menu keys to menu text.
     * 
     * @param productButtonKeys
     *            List of strings from redbookProductButtons.txt
     * 
     * @return Map for key -> menu text
     */
    private static Map<String, String> getMenuTextMap(
            List<String> productButtonKeys) {
        Map<String, String> menuTextMap = new HashMap<String, String>();
        for (String line : productButtonKeys) {
            line = line.trim();
            // Skip comment/empty lines
            if (line.startsWith("#") || line.trim().length() == 0) {
                continue;
            }

            String[] parts = line.split("\\|");
            menuTextMap.put(parts[0].trim(), parts[2].trim());
        }

        return menuTextMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notify(final String menuFileName, final File menuFile) {
        lock.lock();
        try {
            storeFile(menuFileName, menuFile);
            buildMenuXml(menuFileName);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Use the given menu XML object to update the menu that corresponds to the
     * given A1 menu file name
     * 
     * @param menuFileName
     *            the A1 menu file name
     * @param xml
     *            the menu XML object created from the A1 file
     */
    private static void updateMenu(String menuFileName, RedbookMenusXML xml) {
        RedbookMenuUtil menuUtil = null;
        if (HAZARDS_FILE.equals(menuFileName)) {
            menuUtil = new RedbookHazardsMenuUtil();
        } else if (HPC_FILE.equals(menuFileName)) {
            menuUtil = new RedbookHpcMenuUtil();
        } else if (CPC_FILE.equals(menuFileName)) {
            menuUtil = new RedbookCpcMenuUtil();
        } else if (MARINE_FILE.equals(menuFileName)) {
            menuUtil = new RedbookMpcMenuUtil();
        } else if (NCO_FILE.equals(menuFileName)) {
            menuUtil = new RedbookNcoMenuUtil();
        } else if (UPPER_AIR_FILE.equals(menuFileName)) {
            menuUtil = new RedbookUaMenuUtil();
        }

        if (menuUtil != null) {
            menuUtil.createMenusFromFile(xml);
        }
    }

    /**
     * Updates all the Redbook menus. Called from
     * {@link RedbookNdmMappingSubscriber } when new A1 files that are used by
     * all of the menus are dropped in.
     */
    public static void notifyAllMenus() {
        RedbookNdmMenuSubscriber menuSubscriber = new RedbookNdmMenuSubscriber();
        IPathManager pm = PathManagerFactory.getPathManager();
        for (String menuFileName : new String[] { CPC_FILE, HPC_FILE, NCO_FILE,
                HAZARDS_FILE, MARINE_FILE, UPPER_AIR_FILE }) {
            File menuFile = pm.getStaticLocalizationFile(
                    LocalizationType.COMMON_STATIC, NDM_LOC_DIR + menuFileName)
                    .getFile();
            menuSubscriber.notify(menuFileName, menuFile);
        }
    }
}
