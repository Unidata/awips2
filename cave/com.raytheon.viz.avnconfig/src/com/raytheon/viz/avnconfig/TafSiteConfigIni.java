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
package com.raytheon.viz.avnconfig;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.HierarchicalINIConfiguration;
import org.apache.commons.configuration.XMLConfiguration;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.avnconfig.AvnConfigConstants.triggerType;

/**
 * Contains the configuration for all of the AVNFPS data related to TAFs using
 * configuration files that are backward compatible with AWIPS I.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2010            rferrel     Initial creation
 * Dec  7, 2010 7621       rferrel     Added getTemplateFile and fixed
 *                                     saveTafTemplate to save localized file.
 * Feb 10. 2010 7926       rferrel     getProductSites now ignores whitespace
 *                                     when splitting the sites.idents
 * Feb 16, 2011 7878       rferrel     Modifications to use ids.cfg file.
 * Apr 08, 2011 8856       rferrel     Can now make a new station's templates
 * May 24, 2011 9060       rferrel     Limit downloading of localization files.
 * Aug 09, 2013 2033       mschenke    Switched File.separator to IPathManager.SEPARATOR
 * May 19, 2015 17417      yteng       Get all sites from product
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class TafSiteConfigIni implements ITafSiteConfig {
    /**
     * The singleton instance of this class.
     */
    private static TafSiteConfigIni instance = null;

    /**
     * Relative path to the tafs directory with file separator at the end.
     */
    private static final String TAFS_DIR = "aviation" + IPathManager.SEPARATOR
            + "config" + IPathManager.SEPARATOR + "tafs";

    /**
     * The file name for site configuration with the file separator prepended to
     * it.
     */
    private static final String INFO_FILE = IPathManager.SEPARATOR + "info.cfg";

    private static final String IDS_FILE = "aviation" + IPathManager.SEPARATOR
            + "config" + IPathManager.SEPARATOR + "ids.cfg";

    /**
     * Relative location for the file that contains the default product name.
     */
    private static final String DEFAULT_PRODUCT_FILE = TAFS_DIR
            + IPathManager.SEPARATOR + "DEFAULT";

    /**
     * The suffix for a site's start hour template file name.
     */
    private static final String TMPL_EXT = "template";

    /**
     * The template extension with the "." prefix.
     */
    private static final String TMPL_DOT_EXT = "." + TMPL_EXT;

    /**
     * Only needed until the no longer needed triggers getter/setter methods are
     * removed.
     */
    @Deprecated
    private XMLConfiguration config;

    /**
     * The current default product name.
     */
    private String defaultProduct;

    /**
     * Keys are the site names the value is a map where the keys are start hour
     * and the value is the template for the hour.
     */
    private Map<String, Map<String, String>> siteMap = new HashMap<String, Map<String, String>>();

    /**
     * Keys are the product names and the value is the parsed configuration
     * information for the product.
     */
    private Map<String, HierarchicalINIConfiguration> configMaps;

    /**
     * The extension on product configuration files.
     */
    private static final String PROD_EXT = "cfg";

    /**
     * the product extension the the "." prefix.
     */
    private static final String PROD_DOT_EXT = "." + PROD_EXT;

    /**
     * The fixed length of the product configuration file extension.
     */
    private static final int PROD_DOT_EXT_LEN = PROD_DOT_EXT.length();

    /**
     * A static method to obtain the singleton instance of this class.
     * 
     * @return instance
     * @throws ConfigurationException
     * @throws FileNotFoundException
     */
    public static synchronized TafSiteConfigIni getInstance()
            throws ConfigurationException, FileNotFoundException {
        if (instance == null) {
            instance = new TafSiteConfigIni();
        }
        return instance;
    }

    /**
     * Clears the instance in order to force reading all configuration files the
     * next time getInstance is invoked.
     */
    public static synchronized void clearInstance() {
        instance = null;
    }

    /**
     * Constructor is private to ensure singleton instance; it reads the needed
     * configuration files to set values.
     * 
     * @throws ConfigurationException
     * @throws FileNotFoundException
     */
    private TafSiteConfigIni() throws ConfigurationException,
            FileNotFoundException {
        defaultProduct = readFile(getFile(DEFAULT_PRODUCT_FILE));
        configMaps = new HashMap<String, HierarchicalINIConfiguration>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.avnconfig.ITafSiteConfig#setTriggers(java.util.Map,
     * java.util.Map, java.util.Map)
     */
    @Override
    public void setTriggers(Map<String, String> tafMap,
            Map<String, String> mtrMap, Map<String, String> cfpMap)
            throws ConfigurationException {
        config.clearTree("triggers");

        Set<String> keys = tafMap.keySet();

        for (String site : keys) {
            config.addProperty("triggers.taf." + site, tafMap.get(site));
            config.addProperty("triggers.mtr." + site, mtrMap.get(site));
        }

        keys = cfpMap.keySet();

        for (String key : keys) {
            config.addProperty("triggers.cfp." + key, cfpMap.get(key));
        }

        // config.save();
        throw new ConfigurationException("Unable to save triggers.");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#getTriggers(com.raytheon.viz
     * .avnconfig.AvnConfigConstants.triggerType)
     */
    @Override
    public HashMap<String, String> getTriggers(triggerType type) {
        HashMap<String, String> triggerMap = new HashMap<String, String>();
        String key;

        if (config == null) {
            return triggerMap;
        }

        switch (type) {
        case TAF:
            key = "triggers.taf.";
            break;
        case METAR:
            key = "triggers.mtr.";
            break;
        case CCFP:
            key = "triggers.cfp.";
            int idx = 1;
            String trigger;
            do {
                String i = Integer.toString(idx++);

                if (i.length() == 1) {
                    i = "0" + i;
                }

                trigger = config.getString(key + i);

                if (trigger != null && !trigger.equals("")) {
                    triggerMap.put(i, trigger);
                }
            } while (trigger != null && !trigger.equals(""));
        default:
            return triggerMap;
        }

        ArrayList<String> sites = getSiteList();

        for (String site : sites) {
            triggerMap.put(site, config.getString(key + site));
        }

        return triggerMap;
    }

    /**
     * Get configuration mapping for the product.
     * 
     * @param product
     * @return config - The configuration mapping for the product or null if
     *         problem getting the mapping.
     */
    private HierarchicalINIConfiguration getProductConfig(String product) {
        HierarchicalINIConfiguration config = configMaps.get(product);
        if (config == null) {
            try {
                config = readProductMap(product);
            } catch (ConfigurationException e) {
                config = null;
            } catch (FileNotFoundException e) {
                config = null;
            }
        }
        return config;
    }

    /**
     * Get the product's list of sites/idents maintaining the order the sites
     * are in the configuration file.
     * 
     * @param product
     * @return siteList
     */
    private List<String> getProductSites(String product) {
        List<String> siteList = new ArrayList<String>();
        HierarchicalINIConfiguration config = getProductConfig(product);
        if (config != null) {
            config.setDelimiterParsingDisabled(true);
            String[] sites = config.getStringArray("sites.idents");
            if (sites != null && sites.length > 0) {
                for (String site : sites) {
                    siteList.add(site.trim());
                }
            }
        }

        return siteList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#setDefault(java.lang.String)
     */
    @Override
    public void setDefault(String defaultProduct) throws ConfigurationException {
        this.defaultProduct = defaultProduct;

        try {
            writeFile(DEFAULT_PRODUCT_FILE, defaultProduct);
        } catch (IOException ex) {
            throw new ConfigurationException(ex.getMessage());
        } catch (LocalizationException ex) {
            throw new ConfigurationException(ex.getMessage());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.avnconfig.ITafSiteConfig#getDefaultProduct()
     */
    @Override
    public String getDefaultProduct() {
        return defaultProduct;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#getProductWorkPil(java.lang
     * .String)
     */
    @Override
    public String getProductWorkPil(String product) {
        HierarchicalConfiguration config = getProductConfig(product);
        if (config != null) {
            String value = config.getString("sites.workpil");
            if (value != null && value.length() > 0) {
                return value;
            }
        }

        return "XXXWRKTAF";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#getProductCollectivePil(java
     * .lang.String)
     */
    @Override
    public String getProductCollectivePil(String product) {
        HierarchicalConfiguration config = getProductConfig(product);
        if (config != null) {
            String value = config.getString("sites.collective");
            if (value != null && value.length() > 0) {
                return value;
            }
        }
        return "";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.avnconfig.ITafSiteConfig#getAllProducts()
     */
    @Override
    public Map<String, List<String>> getAllProducts() {
        Map<String, List<String>> products = new HashMap<String, List<String>>();

        for (LocalizationFile lFile : getLfiles(TAFS_DIR, PROD_EXT)) {
            String name = lFile.getName();
            String product = name.substring(name.lastIndexOf("/") + 1,
                    name.length() - PROD_DOT_EXT_LEN);
            products.put(product, getProductSites(product));
        }

        return products;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#saveProduct(java.lang.String,
     * java.util.ArrayList, java.lang.String, java.lang.String)
     */
    @Override
    public void saveProduct(String newProduct, List<String> siteList,
            String workPil, String collectivePil) throws ConfigurationException {
        LocalizationFile lFile = null;

        try {
            lFile = getFile(TAFS_DIR + File.separator + newProduct
                    + PROD_DOT_EXT);
        } catch (FileNotFoundException ex) {
            throw new ConfigurationException("Unable to save product: \""
                    + newProduct + "\"");
        }

        try {
            HierarchicalINIConfiguration config = configMaps.get(newProduct);
            if (config == null) {
                config = new HierarchicalINIConfiguration();
                config.setFile(lFile.getFile(false));
            }

            config.setDelimiterParsingDisabled(true);
            String prepend = "";
            StringBuilder idents = new StringBuilder();
            for (String site : siteList) {
                idents.append(prepend).append(site);
                prepend = ", ";
            }

            config.setProperty("sites.idents", idents.toString());
            config.setProperty("sites.workpil", workPil);
            config.setProperty("sites.collective", collectivePil);
            config.save();
            lFile.save();
            configMaps.put(newProduct, config);
        } catch (LocalizationException ex) {
            throw new ConfigurationException(ex);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#deleteProduct(java.lang.String)
     */
    @Override
    public void deleteProduct(String product) throws ConfigurationException,
            LocalizationException {

        HierarchicalINIConfiguration config = configMaps.get(product);

        if (config != null) {
            config.getFile().delete();
            configMaps.remove(product);
        }

        LocalizationFile lFile = null;
        try {
            lFile = getFile(TAFS_DIR + File.separator + product + PROD_DOT_EXT);
            lFile.delete();
        } catch (FileNotFoundException ex) {
            // Do nothing
        }
    }

    /**
     * This method reads a localized file and converts IOExceptions other then
     * file not found to configuration Exception to mimic behavior that complies
     * with the interface.
     * 
     * @param filename
     *            - Name of file to read
     * @return value - contents of file trimmed of leading/trailing whitespace.
     * @throws FileNotFoundException
     * @throws ConfigurationException
     */
    private String readFile(LocalizationFile lFile)
            throws FileNotFoundException, ConfigurationException {
        File file = lFile.getFile();
        if (file != null && file.exists()) {
            char[] c = new char[(int) file.length()];
            try (FileReader reader = new FileReader(file)) {
                reader.read(c);
            } catch (IOException ex) {
                throw new ConfigurationException(ex.getMessage());
            }
            return String.valueOf(c).trim();
        } else {
            return null;
        }
    }

    /**
     * Writes a string to a file updating the localization information.
     * 
     * @param filename
     *            - file to update or create
     * @param value
     *            - string to place in the file
     * @throws IOException
     * @throws LocalizationException
     */
    private void writeFile(String filename, String value) throws IOException,
            LocalizationException {
        LocalizationFile lFile = getFile(filename);
        File file = lFile.getFile(false);
        file.getParentFile().mkdirs();
        FileWriter writer = new FileWriter(file);
        writer.write(value);
        writer.close();
        lFile.save();
    }

    @Override
    public LocalizationFile getTemplateFile(String siteId, String startHour)
            throws FileNotFoundException {
        return getFile(TAFS_DIR + File.separator + siteId + File.separator
                + startHour + TMPL_DOT_EXT);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#saveTafTemplate(java.lang.String
     * , java.lang.String, java.lang.String)
     */
    @Override
    public void saveTafTemplate(String siteId, String startHour, String template)
            throws ConfigurationException {

        if (siteMap.get(siteId) == null) {
            siteMap.put(siteId, new HashMap<String, String>());
        }
        siteMap.get(siteId).put(startHour, template);
        try {
            writeFile(TAFS_DIR + File.separator + siteId + File.separator
                    + startHour + TMPL_DOT_EXT, template);
        } catch (IOException ex) {
            throw new ConfigurationException(ex.getMessage());
        } catch (LocalizationException ex) {
            throw new ConfigurationException(ex.getMessage());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#getTafTemplate(java.lang.String
     * , java.lang.String)
     */
    @Override
    public String getTafTemplate(String siteId, String startHour) {
        return siteMap.get(siteId).get(startHour);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.avnconfig.ITafSiteConfig#getSite(java.lang.String)
     */
    @Override
    public TafSiteData getSite(String siteId) throws IOException,
            ConfigurationException {
        TafSiteData site = null;
        String fileName = TAFS_DIR + IPathManager.SEPARATOR + siteId
                + INFO_FILE;

        try {
            File file = AvnConfigFileUtil.getStaticSiteFile(fileName, siteId);
            site = new TafSiteData();
            HierarchicalINIConfiguration config = new HierarchicalINIConfiguration(
                    file);
            site.wmo = config.getString("headers.wmo");
            site.afos = config.getString("headers.afos");
            site.longitude = config.getString("geography.lon");
            site.latitude = config.getString("geography.lat");
            site.elevation = config.getString("geography.elev");
            site.hours = config.getString("thresholds.tafduration");
            site.visibility = config.getStringArray("thresholds.vsby");
            site.ceiling = config.getStringArray("thresholds.cig");
            site.radarCutoff = config.getStringArray("thresholds.radar_cutoff");
            site.profilerCutoff = config
                    .getStringArray("thresholds.profiler_cutoff");

            // Sometimes the runways list contains a trailing ','
            String[] temp = config.getStringArray("geography.runway");
            String last = temp[temp.length - 1];
            if (last == null || last.trim().equals("")) {
                String[] temp2 = new String[temp.length - 1];
                for (int i = 0; i < temp2.length; i++) {
                    temp2[i] = temp[i];
                }
                temp = temp2;
            }
            site.runway = temp;

            site.acars = config.getString("sites.acars");
            site.metar = config.getStringArray("sites.metar");
            site.nam = config.getString("sites.nam");
            site.nammos = config.getString("sites.nammos");
            site.gfsmos = config.getString("sites.gfsmos");
            site.gfslamp = config.getString("sites.gfslamp");
            site.radars = config.getStringArray("sites.radars");
            site.profilers = config.getStringArray("sites.profilers");

            if (config.getString("qc.impact").equals("1")) {
                site.impactQc = true;
            } else {
                site.impactQc = false;
            }

            if (config.getString("qc.climate").equals("1")) {
                site.climateQc = true;
            } else {
                site.climateQc = false;
            }

            if (config.getString("qc.currentwx").equals("1")) {
                site.currentWxQc = true;
            } else {
                site.currentWxQc = false;
            }
        } catch (NullPointerException e) {
            throw new IOException(
                    "Error: file info.cfg file does not exist for site "
                            + siteId + ".");
        }

        return site;
    }

    /**
     * This clear productMap and generates new entries based on the product
     * configuration files that are found.
     * 
     * @throws ConfigurationException
     * @throws FileNotFoundException
     */
    private HierarchicalINIConfiguration readProductMap(String product)
            throws ConfigurationException, FileNotFoundException {
        String name = TAFS_DIR + File.separator + product + PROD_DOT_EXT;
        LocalizationFile lFile = getFile(name);
        HierarchicalINIConfiguration config = null;
        if (lFile.exists()) {
            File file = lFile.getFile();
            config = new HierarchicalINIConfiguration(file);
            configMaps.put(product, config);
        }
        return config;
    }

    /**
     * Get an array of localized files from a directory.
     * 
     * @param dir
     *            The directory to check
     * @param ext
     *            The extension to look file
     * @return lFileArray The array of localized files in the directory with the
     *         desired extension
     */
    private LocalizationFile[] getLfiles(String dir, String ext) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        return pm.listFiles(context, dir, new String[] { ext }, false, true);
    }

    /**
     * Obtains a localization file for desired file name.
     * 
     * @param filename
     * @return lFile
     * @throws FileNotFoundException
     */
    private LocalizationFile getFile(String filename)
            throws FileNotFoundException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile lFile = pm.getLocalizationFile(context, filename);
        if (lFile == null) {
            String site = LocalizationManager.getInstance().getCurrentSite();
            throw new FileNotFoundException("Unable to find \"" + filename
                    + "\" under the directory for site " + site + ".");

        }
        return lFile;
    }

    /**
     * Takes an array of strings and converts it into a single comma separated
     * string.
     * 
     * @param array
     * @return str
     */
    private String arrayToString(String[] array) {
        StringBuilder sb = new StringBuilder();
        String prefix = "";

        for (String s : array) {
            sb.append(prefix).append(s);
            prefix = ",";
        }

        return sb.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.avnconfig.ITafSiteConfig#setSite(java.lang.String,
     * com.raytheon.viz.avnconfig.TafSiteData)
     */
    @Override
    public void setSite(String siteId, TafSiteData site) throws IOException,
            ConfigurationException {
        String filepath = TAFS_DIR + File.separator + siteId + INFO_FILE;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile lFile = pm.getLocalizationFile(context, filepath);
        try {
            File file = lFile.getFile(false);

            file.getParentFile().mkdirs();
            HierarchicalINIConfiguration config = new HierarchicalINIConfiguration();
            config.setDelimiterParsingDisabled(true);
            config.setProperty("headers.wmo", site.wmo);
            config.setProperty("headers.afos", site.afos);
            config.setProperty("thresholds.radar_cutoff",
                    arrayToString(site.radarCutoff));
            config.setProperty("thresholds.cig", arrayToString(site.ceiling));
            config.setProperty("thresholds.profiler_cutoff",
                    arrayToString(site.profilerCutoff));
            config.setProperty("thresholds.vsby",
                    arrayToString(site.visibility));
            config.setProperty("thresholds.tafduration", site.hours);
            config.setProperty("sites.acars", site.acars);
            // TODO why is this a String[] instead of just a String like the
            // other
            // Alternate Ids?
            config.setProperty("sites.radars", arrayToString(site.radars));
            config.setProperty("sites.gfsmos", site.gfsmos);
            config.setProperty("sites.gfslamp", site.gfslamp);
            // TODO why is this a String[] instead of just a String like the
            // other
            // Alternate Ids?
            config.setProperty("sites.metar", arrayToString(site.metar));
            config.setProperty("sites.nam", site.nam);
            config.setProperty("sites.profilers", arrayToString(site.profilers));
            config.setProperty("sites.nammos", site.nammos);
            config.setProperty("geography.lat", site.latitude);
            config.setProperty("geography.runway", arrayToString(site.runway));
            config.setProperty("geography.lon", site.longitude);
            config.setProperty("geography.elev", site.elevation);

            if (site.impactQc) {
                config.setProperty("qc.impact", "1");
            } else {
                config.setProperty("qc.impact", "0");
            }

            if (site.climateQc) {
                config.setProperty("qc.climate", "1");
            } else {
                config.setProperty("qc.climate", "0");
            }

            if (site.currentWxQc) {
                config.setProperty("qc.currentwx", "1");
            } else {
                config.setProperty("qc.currentwx", "0");
            }

            FileWriter writer = new FileWriter(file);
            config.save(writer);
            writer.close();
            lFile.save();
        } catch (LocalizationException ex) {
            throw new ConfigurationException(ex.getMessage());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.avnconfig.ITafSiteConfig#getSiteList()
     */
    @Override
    public ArrayList<String> getSiteList() {
        ArrayList<String> siteList = new ArrayList<String>();
        for (String product : getProductList()) {
            for (String site : getProductSites(product)) {
                if (siteList.contains(site) == false) {
                    siteList.add(site);
                }
            }
        }
        Collections.sort(siteList);
        return siteList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.avnconfig.ITafSiteConfig#getProductList()
     */
    @Override
    public ArrayList<String> getProductList() {
        ArrayList<String> productList = new ArrayList<String>();
        String defaultProduct = getDefaultProduct();

        for (LocalizationFile lFile : getLfiles(TAFS_DIR, PROD_EXT)) {
            String name = lFile.getName();
            String product = name.substring(name.lastIndexOf("/") + 1,
                    name.length() - PROD_DOT_EXT_LEN);
            if (product.equals(defaultProduct) == false) {
                productList.add(product);
            }
        }

        Collections.sort(productList);

        if (defaultProduct != null && defaultProduct.length() > 0) {
            productList.add(0, defaultProduct);
        }

        return productList;
    }

    private HierarchicalINIConfiguration idsConfig;

    /**
     * Get the ids configuration file information.
     * 
     * @return
     * @throws ConfigurationException
     */
    private synchronized HierarchicalINIConfiguration getIdsConfig()
            throws ConfigurationException {
        if (idsConfig == null) {
            String filepath = IDS_FILE;
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
            LocalizationFile lFile = pm.getLocalizationFile(context, filepath);
            HierarchicalINIConfiguration config = new HierarchicalINIConfiguration();
            config.setDelimiterParsingDisabled(true);
            config.load(lFile.getFile());
            this.idsConfig = config;
        }
        return this.idsConfig;
    }

    /**
     * Common code to get the ids configuration file.
     * 
     * @return lFile
     */
    private LocalizationFile getIdsLfile() {
        String filepath = IDS_FILE;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile lFile = pm.getLocalizationFile(context, filepath);
        return lFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.avnconfig.ITafSiteConfig#getIdsSiteList()
     */
    @Override
    public ArrayList<String> getIdsSiteList() throws IOException,
            ConfigurationException, LocalizationOpFailedException {
        HierarchicalINIConfiguration config = getIdsConfig();
        ArrayList<String> siteList = new ArrayList<String>();
        for (Object site : config.getSections()) {
            siteList.add(site.toString());
        }
        Collections.sort(siteList);
        return siteList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#getIdsPil(java.lang.String)
     */
    @Override
    public String getIdsPil(String site) throws IOException,
            ConfigurationException, LocalizationOpFailedException {
        HierarchicalINIConfiguration config = getIdsConfig();
        String pil = config.getProperty(site + ".pil").toString();
        return pil;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#setIdsSite(java.lang.String,
     * java.lang.String)
     */
    @Override
    public void setIdsSite(String site, String pil) throws IOException,
            ConfigurationException {
        HierarchicalINIConfiguration config = getIdsConfig();
        LocalizationFile lFile = getIdsLfile();
        try {
            File file = lFile.getFile(false);

            config.setProperty(site + ".pil", pil);
            config.save(file);
            lFile.save();
        } catch (LocalizationException e) {
            throw new IOException("Unable to save file: " + lFile.getName());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.avnconfig.ITafSiteConfig#removeIdsSite(java.lang.String)
     */
    @Override
    public synchronized void removeIdsSite(String site)
            throws ConfigurationException {
        LocalizationFile lFile = getIdsLfile();
        try {
            File file = lFile.getFile(false);

            HierarchicalINIConfiguration config = getIdsConfig();
            HierarchicalINIConfiguration newConfig = new HierarchicalINIConfiguration();

            // Do not include the site in the new config.
            for (Object newSite : config.getSections()) {
                if (!newSite.equals(site)) {
                    newConfig.setProperty(newSite + ".pil",
                            config.getProperty(newSite + ".pil"));
                }
            }
            newConfig.save(file);
            lFile.save();
            this.idsConfig = newConfig;
        } catch (LocalizationException ex) {
            throw new ConfigurationException(ex.getMessage());
        }
    }
}
