package com.raytheon.viz.avnconfig;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.apache.commons.configuration.ConfigurationException;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.viz.avnconfig.AvnConfigConstants.triggerType;

/**
 * This interface contains the methods needed by a class that parses
 * configuration file(s).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2010            rferrel     Initial creation
 * Dec  7, 2010 7621       rferrel     Added getTemplateFile.
 * Feb 16, 2011 7878       rferrel     Modifications to handle ids.cfg.
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public interface ITafSiteConfig {

    /**
     * Only used by TriggerEditorDlg which is going away.
     * 
     * @param tafMap
     * @param mtrMap
     * @param cfpMap
     * @throws ConfigurationException
     */
    @Deprecated
    public void setTriggers(Map<String, String> tafMap,
            Map<String, String> mtrMap, Map<String, String> cfpMap)
            throws ConfigurationException;

    /**
     * Only used by TriggerEditorDlg which is going away.
     * 
     * @param type
     * @return
     */
    @Deprecated
    public Map<String, String> getTriggers(triggerType type);

    /**
     * Change the default product and save the change.
     * 
     * @param defaultProduct
     *            - New default product
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public void setDefault(String defaultProduct)
            throws ConfigurationException, LocalizationException;

    /**
     * @return defaultProduct
     */
    public String getDefaultProduct();

    /**
     * Obtain the product's workpil.
     * 
     * @param product
     * @return workPil
     */
    public String getProductWorkPil(String product);

    /**
     * Obtain the product's collectivePil.
     * 
     * @param product
     * @return collectivePil
     */
    public String getProductCollectivePil(String product);

    /**
     * Obtain a mapping with keys of product names and values that is a list of
     * the sites for the product.
     * 
     * @return productMap
     */
    public Map<String, List<String>> getAllProducts();

    /**
     * Create a new or change an existing product and save.
     * 
     * @param newProduct
     *            - product name
     * @param siteList
     *            - product's sites
     * @param workPil
     *            - product's work Pil value
     * @param collectivePil
     *            - product's collective Pil value
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public void saveProduct(String newProduct, List<String> siteList,
            String workPil, String collectivePil)
            throws ConfigurationException, LocalizationOpFailedException;

    /**
     * Removes a product and save the change.
     * 
     * @param product
     *            - product name to delete
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public void deleteProduct(String product) throws ConfigurationException,
            LocalizationException;

    /**
     * Update a site's start Hour template and save the change.
     * 
     * @param siteId
     *            - Site to modify
     * @param startHour
     *            - Start hour to modify
     * @param template
     *            - new template
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public void saveTafTemplate(String siteId, String startHour, String template)
            throws ConfigurationException, LocalizationOpFailedException;

    /**
     * Obtain the template for the given site and start hour
     * 
     * @param siteId
     * @param startHour
     * @return template
     */
    public String getTafTemplate(String siteId, String startHour);

    /**
     * Obtain the Taf site data for desired site name.
     * 
     * @param siteId
     *            - site name
     * @return site
     * @throws IOException
     * @throws ConfigurationException
     */
    public TafSiteData getSite(String siteId) throws IOException,
            ConfigurationException;

    /**
     * Change a site's Taf Site Data and save.
     * 
     * @param siteId
     *            - Name of site to change
     * @param site
     *            - new taf site data
     * @throws IOException
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public void setSite(String siteId, TafSiteData site) throws IOException,
            ConfigurationException, LocalizationOpFailedException;

    /**
     * Get a sorted list of sites names.
     * 
     * @return siteList
     */
    public List<String> getSiteList();

    /**
     * Get a list of products where the default product is at the top of the
     * list followed the remaining products in a sorted list.
     * 
     * @return productList
     */
    public List<String> getProductList();

    /**
     * Get the configuration file for given site an hour.
     * 
     * @param siteId
     * @param startHour
     * @return lFile
     * @throws FileNotFoundException
     */
    public LocalizationFile getTemplateFile(String siteId, String startHour)
            throws FileNotFoundException;

    /**
     * Get sorted site/ident list used by climate.
     * 
     * @return identList
     * @throws IOException
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public List<String> getIdsSiteList() throws IOException,
            ConfigurationException, LocalizationOpFailedException;

    /**
     * Get pil value for the desired site.
     * 
     * @param site
     * @return pil
     * @throws IOException
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public String getIdsPil(String site) throws IOException,
            ConfigurationException, LocalizationOpFailedException;

    /**
     * Generate a ids site/ident entry.
     * 
     * @param site
     *            - ident name
     * @param pil
     *            - site's pil/METAR AFOS ID
     * @throws IOException
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public void setIdsSite(String site, String pil) throws IOException,
            ConfigurationException, LocalizationOpFailedException;

    /**
     * Delete the ids site/ident entry.
     * 
     * @param site
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     */
    public void removeIdsSite(String site) throws ConfigurationException,
            LocalizationOpFailedException;
}
