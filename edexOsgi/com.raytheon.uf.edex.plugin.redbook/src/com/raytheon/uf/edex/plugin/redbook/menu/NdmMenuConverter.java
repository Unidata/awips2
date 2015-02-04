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
package com.raytheon.uf.edex.plugin.redbook.menu;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.edex.plugin.redbook.ingest.RedbookMenuSubscriber;
import com.raytheon.uf.edex.plugin.redbook.ingest.xml.MenuEntry;
import com.raytheon.uf.edex.plugin.redbook.ingest.xml.MenuEntryType;
import com.raytheon.uf.edex.plugin.redbook.ingest.xml.RedbookMenusXML;

/**
 * This class is called from /awips2/edex/bin/ndmMenuIngester.sh.
 * 
 * It reads in the NDM menu files and outputs an A2 version into the NDM
 * endpoint on edex.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2015   4030     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class NdmMenuConverter {
    private static final String WMO = "wmo";

    private static final String OUTPUT_PATH = File.separator + "awips2"
            + File.separator + "edex" + File.separator + "data"
            + File.separator + "ndm" + File.separator;

    private static final String HPC_FILE = "redbookHPCMenus.txt";

    private static final String CPC_FILE = "redbookCPCMenus.txt";

    private static final String NCO_FILE = "redbookNCOMenus.txt";

    private static final String HAZARDS_FILE = "redbookHazardMenus.txt";

    private static final String MARINE_FILE = "redbookMarineMenus.txt";

    private static final String UPPER_AIR_FILE = "redbookUpperAirMenus.txt";

    private String dataKeysPath;

    private String depictKeysPath;

    private String menuFilePath;

    private String productButtonPath;

    private JAXBContext jax;

    private Marshaller marshaller;

    private File depictFile;

    public NdmMenuConverter() {
        createContext();
    }

    public void convert() {
        RedbookMenusXML menuXml = new RedbookMenusXML();
        MenuEntry titleMenuEntry;
        int sepCounter = 0;
        List<MenuEntry> subMenuList = new ArrayList<MenuEntry>();

        try {
            File dataFile = new File(this.dataKeysPath);
            File menuFile = new File(this.menuFilePath);
            depictFile = new File(this.depictKeysPath);
            File productButtonFile = new File(this.productButtonPath);

            List<String> dataKeys = Files.readAllLines(dataFile.toPath(),
                    Charset.defaultCharset());
            List<String> depictKeys = Files.readAllLines(depictFile.toPath(),
                    Charset.defaultCharset());
            List<String> lines = Files.readAllLines(menuFile.toPath(),
                    Charset.defaultCharset());
            List<String> productButtonKeys = Files.readAllLines(
                    productButtonFile.toPath(), Charset.defaultCharset());
            Map<String, String> menuTextMap = getMenuTextMap(productButtonKeys);
            Map<String, String> dataKeyMap = getSubstitutionMap(dataKeys);

            MenuEntry subMenuEntry = null;

            int subMenuCount = -1;
            for (String line : lines) {
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
                    String text = parts[1].replace("\"", "");
                    titleMenuEntry = new MenuEntry();
                    titleMenuEntry.setFile(null);
                    titleMenuEntry.setType(MenuEntryType.Title);
                    titleMenuEntry.setText(text);
                    titleMenuEntry.setId(text);
                    subMenuList.get(subMenuCount).addMenuEntry(titleMenuEntry);
                } else if (line.startsWith("productButton")) {
                    String[] parts = line.split(":");
                    MenuEntry me = new MenuEntry();
                    me.setFile(null);
                    me.setType(MenuEntryType.ProductButton);
                    /*
                     * There are certain productButtons in the NCO menu data
                     * keys in the (25000 range) that have data keys that don't
                     * map to anything. This results in those menu items not
                     * being created. The site will need to fix this after
                     * generating the new menus.
                     */
                    String dataKey = parts[1].trim().substring(0, 4);
                    StringBuilder subValue = new StringBuilder();
                    // Find the matching value
                    for (String depictKeyLine : depictKeys) {
                        if (depictKeyLine.trim().startsWith(dataKey)) {
                            String[] depictKeyParts = depictKeyLine
                                    .split("\\|");
                            me.setText(menuTextMap.get(dataKey));
                            me.setId(depictKeyParts[6].trim());
                            subMenuList.get(subMenuCount).addMenuEntry(me);

                            String[] subParts = depictKeyParts[2].split(",");
                            MenuEntry subEntry = new MenuEntry();
                            subEntry.setFile(null);
                            subEntry.setType(MenuEntryType.Substitute);
                            subEntry.setKey(WMO);

                            for (String subPart : subParts) {
                                for (String key : dataKeyMap.keySet()) {
                                    if (key.startsWith(subPart)) {
                                        subValue.append(dataKeyMap.get(key))
                                                .append(",");
                                        break;
                                    }
                                }
                            }

                            String subValueStr = subValue.toString();
                            subValueStr = StringUtils.removeEnd(subValueStr,
                                    ",");
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
                        subMenuList.get(subMenuCount).addMenuEntry(
                                closedSubMenu);
                    }
                } else if (line.startsWith("separator")) {
                    MenuEntry sep = new MenuEntry();
                    sep.setFile(null);
                    sep.setType(MenuEntryType.Separator);
                    sep.setId("Separator" + sepCounter++);
                    subMenuList.get(subMenuCount).addMenuEntry(sep);
                }
            }

            // Set output file name
            String inputFileName = menuFile.getName();
            String outputFileName = null;
            if (inputFileName.equals(CPC_FILE)) {
                outputFileName = RedbookMenuSubscriber.CPC_MENU_FILE;
            } else if (inputFileName.equals(HPC_FILE)) {
                outputFileName = RedbookMenuSubscriber.HPC_MENU_FILE;
            } else if (inputFileName.equals(NCO_FILE)) {
                outputFileName = RedbookMenuSubscriber.NCO_MENU_FILE;
            } else if (inputFileName.equals(HAZARDS_FILE)) {
                outputFileName = RedbookMenuSubscriber.HAZARD_MENU_FILE;
            } else if (inputFileName.equals(MARINE_FILE)) {
                outputFileName = RedbookMenuSubscriber.MPC_MENU_FILE;
            } else if (inputFileName.equals(UPPER_AIR_FILE)) {
                outputFileName = RedbookMenuSubscriber.UA_MENU_FILE;
            } else {
                throw new IOException("Error processing file");
            }

            marshaller.marshal(menuXml, new File(OUTPUT_PATH + outputFileName));
        } catch (Exception e) {
            System.err.println("Error occurred processing file: "
                    + menuFilePath);
            e.printStackTrace();
        }
    }

    /**
     * Get a map of menu keys to menu text.
     * 
     * @param productButtonKeys
     *            List of strings from redbookProductButtons.txt
     * 
     * @return Map for key -> menu text
     */
    private Map<String, String> getMenuTextMap(List<String> productButtonKeys) {
        Map<String, String> menuTextMap = new HashMap<String, String>();
        for (String line : productButtonKeys) {
            line = line.trim();
            // Skip comment lines
            if (line.startsWith("#") || line.trim().length() == 0) {
                continue;
            }

            String[] parts = line.split("\\|");
            menuTextMap.put(parts[0].trim(), parts[2].trim());
        }

        return menuTextMap;
    }

    /**
     * Get a map of key to substitution values.
     * 
     * @param dataKeys
     *            List of strings from the redbookDataKeys.txt file
     * @returnMap for key -> substitution string
     */
    private Map<String, String> getSubstitutionMap(List<String> dataKeys) {
        Map<String, String> dataKeyMap = new HashMap<String, String>();
        for (String line : dataKeys) {
            line = line.trim();
            // Skip comment lines
            if (line.startsWith("#") || line.trim().length() == 0) {
                continue;
            }
            String[] parts = line.split("\\|");
            dataKeyMap.put(parts[0].trim(), parts[10].substring(0, 6));
        }

        return dataKeyMap;
    }

    public String getDataKeysPath() {
        return dataKeysPath;
    }

    public void setDataKeysPath(String dataKeysPath) {
        this.dataKeysPath = dataKeysPath;
    }

    public String getDepictKeysPath() {
        return depictKeysPath;
    }

    public void setDepictKeysPath(String depictKeysPath) {
        this.depictKeysPath = depictKeysPath;
    }

    public String getMenuFilePath() {
        return menuFilePath;
    }

    public void setMenuFilePath(String menuFilePath) {
        this.menuFilePath = menuFilePath;
    }

    public void setProductButtonPath(String productButtonPath) {
        this.productButtonPath = productButtonPath;
    }

    public String getProductButtonPath() {
        return this.productButtonPath;
    }

    private void createContext() {
        Class[] classes = new Class[] { MenuEntry.class, MenuEntryType.class,
                RedbookMenusXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.marshaller = jax.createMarshaller();
            this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {

        if (args.length != 2) {
            System.err.println("Unexpected Arguments");
            System.err
                    .println("Expecting local NDM directory and NDM File Menu Name");
            return;
        }

        String dirPath = args[0];
        if (!dirPath.endsWith(File.separator)) {
            dirPath = dirPath.concat(File.separator);
        }
        String menuFile = dirPath + args[1];
        String dataKeysFile = dirPath + "redbookDataKeys.txt";
        String depictKeysFile = dirPath + "redbookDepictKeys.txt";
        String productButtonFile = dirPath + "redbookProductButtons.txt";

        NdmMenuConverter converter = new NdmMenuConverter();
        converter.setDataKeysPath(dataKeysFile);
        converter.setMenuFilePath(menuFile);
        converter.setDepictKeysPath(depictKeysFile);
        converter.setProductButtonPath(productButtonFile);
        converter.convert();

    }
}
