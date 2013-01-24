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
package com.raytheon.uf.edex.plugin.ffmp;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.regex.Pattern;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.edex.plugin.radar.dao.RadarStationDao;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataContainer;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates.MODE;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinList;
import com.raytheon.uf.common.dataplugin.ffmp.dao.FFMPDao;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.DATA_TYPE;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.config.FFMPTemplateConfigurationManager;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.FFTISourceXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceIngestConfigXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.dat.utils.DatMenuUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.plugin.ffmp.common.FFMPConfig;
import com.raytheon.uf.edex.plugin.ffmp.common.FFMPProcessor;
import com.raytheon.uf.edex.plugin.ffmp.common.FFTI;
import com.raytheon.uf.edex.plugin.ffmp.common.FFTIData;
import com.raytheon.uf.edex.plugin.ffmp.common.FFTIProcessor;

/**
 * 
 * Generates FFMP Data records
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/21/2009   2521       dhladky     Initial Creation.
 * 02/03/2011   6500       cjeanbap    Fixed NullPointerException.
 * 07/31/2011   578        dhladky     FFTI modifications
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPGenerator extends CompositeProductGenerator implements
        MonitorConfigListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPGenerator.class);

    /**
     * Public constructor for FFMPGenerator
     * 
     * @param name
     * @param compositeProductType
     */
    public FFMPGenerator(Executor executor, Executor processexecutor) {

        super(genName, productType, executor);
        this.processexecutor = processexecutor;
    }

    private static final String genName = "FFMP";

    private static final String templateTaskName = "FFMP Template";

    private static final String productType = "ffmp";

    /** ArrayList of domains to filter for */
    private ArrayList<DomainXML> domains = null;

    /** template loader bool **/
    public boolean loaded = false;

    /** check ffg first time you run **/
    public boolean ffgCheck = false;

    /** ffti finished processing **/
    public boolean fftiDone = true;

    /** products **/
    private ConcurrentHashMap<String, FFMPRecord[]> products = null;

    /** Processes map <dataKey, SourceXML> **/
    private ConcurrentHashMap<String, SourceXML> processes = null;

    /** array list of sources to evaluate **/
    public ArrayList<FFTISourceXML> fftiSources = new ArrayList<FFTISourceXML>();

    /** run configuration manager **/
    public FFMPRunConfigurationManager frcm = null;

    /** source configuration manager **/
    public FFMPSourceConfigurationManager fscm = null;

    /** temp cache **/
    public ConcurrentHashMap<String, FFMPDataContainer> ffmpData = new ConcurrentHashMap<String, FFMPDataContainer>();

    /** FFTI accum/ratio/diff cache **/
    public ConcurrentHashMap<String, FFTIData> fftiData = new ConcurrentHashMap<String, FFTIData>();

    /** checks for initial load **/
    public ArrayList<String> loadedData = new ArrayList<String>();

    /** thread the productkeys **/
    public ConcurrentHashMap<String, ArrayList<String>> productKeys = new ConcurrentHashMap<String, ArrayList<String>>();

    /** template config manager **/
    public FFMPTemplateConfigurationManager tempConfig = null;

    /** FFMPConfig object **/
    public FFMPConfig config = null;

    /** template **/
    public FFMPTemplates template = null;

    private IPathManager pathManager;

    public static String sharePath = AppsDefaults.getInstance().getToken(
            "apps_dir")
            + File.separator + "ffmp" + File.separator;

    /** source bins used for finding basin to data correlations **/
    private HashMap<String, SourceBinList> sourceBins = new HashMap<String, SourceBinList>();

    /** thread executor **/
    public Executor processexecutor = null;

    @Override
    protected void configureFilters() {

        this.pathManager = PathManagerFactory.getPathManager();

        statusHandler.handle(Priority.DEBUG, getGeneratorName()
                + " process Filter Config...");
        domains = new ArrayList<DomainXML>();
        boolean configValid = getRunConfig().isPopulated();

        if (configValid) {
            for (FFMPRunXML run : getRunConfig().getFFMPRunners()) {
                domains = run.getDomains();
            }
        } else {

            /**
             * Don't have one, so create an EDEX generated default
             */
            LocalizationContext commonStaticSite = pathManager.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

            List<String> sites = RadarsInUseUtil.getSite(null,
                    RadarsInUseUtil.LOCAL_CONSTANT);

            FFMPRunXML runner = new FFMPRunXML();
            ArrayList<ProductRunXML> products = new ArrayList<ProductRunXML>();
            // these two are always there in default setups
            ProductRunXML hpeProduct = new ProductRunXML();
            hpeProduct.setProductName("DHRMOSAIC");
            hpeProduct.setProductKey("hpe");
            products.add(hpeProduct);

            ProductRunXML biasHpeProduct = new ProductRunXML();
            biasHpeProduct.setProductName("BDHRMOSAIC");
            biasHpeProduct.setProductKey("bhpe");
            products.add(biasHpeProduct);

            ArrayList<String> rfc = new ArrayList<String>();

            if (sites.isEmpty()) {
                RadarStationDao dao = new RadarStationDao();
                List<RadarStation> stations = null;
                try {
                    stations = dao.queryByWfo(PropertiesFactory.getInstance()
                            .getEnvProperties().getEnvValue("SITENAME"));
                } catch (DataAccessLayerException e) {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "Unable to access data object for radar station table");
                }

                for (RadarStation station : stations) {
                    // this is just for a default
                    ProductRunXML dhrProduct = new ProductRunXML();
                    dhrProduct.setProductName("DHR");
                    dhrProduct.setProductKey(station.getRdaId().toLowerCase());
                    products.add(dhrProduct);

                    String newRfc = FFMPUtils
                            .getRFC(dhrProduct.getProductKey());
                    if (!rfc.contains(newRfc)) {
                        rfc.add(newRfc);
                    }

                    sites.add(station.getRdaId().toLowerCase());
                }

            } else {
                for (String site : sites) {
                    // this is just for a default
                    ProductRunXML dhrProduct = new ProductRunXML();
                    dhrProduct.setProductName("DHR");
                    dhrProduct.setProductKey(site);
                    products.add(dhrProduct);

                    String newRfc = FFMPUtils
                            .getRFC(dhrProduct.getProductKey());
                    if (!rfc.contains(newRfc)) {
                        rfc.add(newRfc);
                    }
                }
            }

            runner.setProducts(products);

            // Apply site list to all QPE types
            for (String source : getSourceConfig().getQPESources()) {
                SourceXML qpeSource = getSourceConfig().getSource(source);
                // Radar Derived sources use the primary source site keys for
                // mosiac datakey
                // Auto Config for any Radar derived sources
                if (qpeSource.getDataType().equals(
                        DATA_TYPE.RADAR.getDataType())) {
                    SourceIngestConfigXML sicm = new SourceIngestConfigXML();
                    sicm.setSourceName(qpeSource.getSourceName());
                    sicm.setUriSubLocation(3);

                    for (String siteid : sites) {
                        sicm.addDataKey(siteid);
                    }

                    runner.addSourceIngest(sicm);
                }
            }

            // We have a list of available RFC's, now find mosaic
            // Apply this to all RFCFFG sources
            for (String source : getSourceConfig().getGuidances()) {
                SourceXML guidSource = getSourceConfig().getSource(source);

                // Auto config for RFC sources
                if (guidSource.isRfc()) {
                    // add a source mosaic config to the Run Config
                    SourceIngestConfigXML sicm = new SourceIngestConfigXML();
                    sicm.setSourceName(guidSource.getSourceName());
                    sicm.setUriSubLocation(3);

                    for (String dataKey : rfc) {
                        sicm.addDataKey(dataKey);
                    }

                    runner.addSourceIngest(sicm);
                }
            }

            // Apply site list to all SCANQPF default
            for (String source : getSourceConfig().getQPFSources()) {
                SourceXML qpfSource = getSourceConfig().getSource(source);
                // Radar Derived sources use the primary source site keys for
                // mosiac datakey
                // Auto Config for any Radar derived sources (QPFSCAN) for
                // example
                if (qpfSource.getSourceName().equals("QPFSCAN")) {
                    SourceIngestConfigXML sicm = new SourceIngestConfigXML();
                    sicm.setSourceName(qpfSource.getSourceName());
                    sicm.setUriSubLocation(3);

                    for (String siteid : sites) {
                        sicm.addDataKey(siteid);
                    }

                    runner.addSourceIngest(sicm);
                }
            }

            DomainXML domain = new DomainXML();
            domain.setPrimary(true);
            domain.setCwa(commonStaticSite.getContextName());
            runner.addDomain(domain);

            getRunConfig().addFFMPRunner(runner);
            getRunConfig().saveConfigXml();
            getRunConfig().setPopulated(true);

            domains.add(domain);
        }

        // kick off template generation
        this.getExecutor().execute(new TemplateLoader(domains));
    }

    @Override
    protected void createFilters() {
        // do more here if you wish

        ArrayList<FFMPRunXML> runners = getRunConfig().getFFMPRunners();
        ArrayList<FFMPURIFilter> tmp = new ArrayList<FFMPURIFilter>(
                runners.size());

        for (FFMPRunXML runner : runners) {
            DomainXML domain = runner.getPrimaryDomain();
            try {
                tmp.add(new FFMPURIFilter(getSiteString(runner) + ":"
                        + getRFCString(runner) + ":" + domain.getCwa()));

                statusHandler.handle(Priority.INFO, "Created FFMP Filter.."
                        + " primary Domain: " + domain.getCwa());
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Couldn't create FFMP Filter.." + " primary Domain: "
                                + domain.getCwa()
                                + " this RUNNER is not a viable FFMP config.");
                e.printStackTrace();
            }
        }

        filters = tmp.toArray(new URIFilter[tmp.size()]);

    }

    /**
     * Slight difference in the way ffmp operates as opposed to the URIFilter in
     * general.
     */
    @Override
    public void matchURIs(DataURINotificationMessage messages) {

        if (messages instanceof DataURINotificationMessage) {
            URIFilter[] filters = getFilters();
            if (filters != null) {
                for (int i = 0; i < filters.length; i++) {
                    if (filters[i] != null) {
                        FFMPURIFilter filter = (FFMPURIFilter) filters[i];

                        if (loaded) {

                            synchronized (filter) {

                                if (filter.isMatched(messages)) {

                                    if (!ffgCheck) {

                                        filter = getFFG(filter);
                                        filter.setValidTime(filter
                                                .getCurrentTime());
                                        ffgCheck = true;
                                    }

                                    dispatch(filter);
                                }
                            }
                        } else {
                            statusHandler
                                    .debug(getGeneratorName()
                                            + ": templates not loaded yet. Skipping product");
                        }
                    }
                }
            }
        }
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {
        if (loaded) {
            try {
                long time = System.currentTimeMillis();
                this.config = new FFMPConfig(
                        (FFMPURIGenerateMessage) genMessage, this);
                products = new ConcurrentHashMap<String, FFMPRecord[]>();
                processes = new ConcurrentHashMap<String, SourceXML>();
                // read config updates, make sure we don't miss something
                getRunConfig().readConfigXml();
                getSourceConfig().readConfigXml();

                if (config.getSources() != null) {
                    for (String source : config.getSources().keySet()) {
                        processes.put(source,
                                getSourceConfig().getSource(source));
                    }
                }

                // start threads

                for (String source : processes.keySet()) {
                    this.getExecutor().execute(
                            new ProcessProduct(processes.get(source), this));
                }

                // count down latch
                while (processes.size() > 0) {
                    // wait for all threads to finish before returning
                    try {
                        Thread.sleep(100);
                        statusHandler.handle(Priority.DEBUG,
                                "Checking status ..." + processes.size());
                        for (String source : processes.keySet()) {
                            statusHandler.handle(Priority.DEBUG,
                                    "Still processing ..." + source);
                        }
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }

                if (products.size() > 0) {

                    // Do the FFTI evaluation, if we have FFTI sources
                    if (fftiSources.size() > 0) {
                        this.getExecutor().execute(new FFTI(this));
                    }

                    ArrayList<FFMPRecord> records = new ArrayList<FFMPRecord>(
                            products.size());
                    for (String source : products.keySet()) {
                        for (FFMPRecord rec : products.get(source)) {
                            records.add(rec);
                        }
                    }

                    FFMPRecord[] recs = new FFMPRecord[records.size()];
                    for (int i = 0; i < records.size(); i++) {
                        recs[i] = records.get(i);
                    }

                    this.setPluginDataObjects(recs);
                    this.setPluginDao(new FFMPDao(getCompositeProductType(),
                            template, fscm, config.getCWA()));

                    while (fftiSources.size() > 0) {
                        try {
                            Thread.sleep(100);
                            statusHandler.handle(Priority.DEBUG,
                                    "Checking status ..." + fftiDone);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }

                    statusHandler.handle(
                            Priority.INFO,
                            config.getCWA() + " finished, duration: "
                                    + (System.currentTimeMillis() - time)
                                    + " ms, wrote " + records.size() + " ");

                } else {
                    statusHandler.handle(Priority.WARN, config.getCWA()
                            + " no new products to produce.");
                }
                // dump data we don't need anymore
                ffmpData.clear();
                // suggest garbage collection
                System.gc();

            } catch (Throwable e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to process FFMP Records.");
                e.printStackTrace();
            }
        }
    }

    /**
     * Get the list of domains
     * 
     * @return
     */
    public ArrayList<DomainXML> getDomains() {
        return domains;
    }

    /**
     * Add a filtering CWA
     * 
     * @param domain
     */
    public void addDomain(DomainXML domain) {
        domains.add(domain);
    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getFFMPState();
    }

    /**
     * Set list of CWA's
     * 
     * @param cwas
     */
    public void setDomains(ArrayList<DomainXML> domains) {
        this.domains = domains;
    }

    /**
     * Inner class to thread the ffmp processing
     * 
     * @author dhladky
     * 
     */
    private class ProcessProduct implements Runnable {

        SourceXML ffmpProduct;

        FFMPGenerator generator;

        @Override
        public void run() {
            try {
                logger.debug("ProcessProduct: Starting thread "
                        + ffmpProduct.getSourceName());
                process();
                logger.debug("ProcessProduct: Finishing thread "
                        + ffmpProduct.getSourceName());
            } catch (Exception e) {
                processes.remove(ffmpProduct.getSourceName());
                logger.error("ProcessProduct: removed "
                        + ffmpProduct.getSourceName());
                e.printStackTrace();
            }
        }

        public ProcessProduct(SourceXML ffmpProduct, FFMPGenerator generator) {
            this.ffmpProduct = ffmpProduct;
            this.generator = generator;
        }

        /**
         * The actual work gets done here
         */
        public void process() throws Exception {

            HashMap<String, Object> dataHash = config.getSourceData(ffmpProduct
                    .getSourceName());
            ArrayList<FFMPRecord> ffmpRecords = new ArrayList<FFMPRecord>(
                    dataHash.size());

            FFMPRunXML runner = getRunConfig().getRunner(config.getCWA());

            // process all of the dataKeys for this source
            for (String dataKey : dataHash.keySet()) {

                ArrayList<String> sites = new ArrayList<String>();

                // is it a mosaic?
                if (ffmpProduct.isMosaic()) {

                    // Take care of defaults, all in this case
                    for (ProductRunXML product : runner.getProducts()) {
                        // no duplicate keys!
                        if (!sites.contains(product.getProductKey())) {
                            sites.add(product.getProductKey());
                        }
                    }

                    // do filtering
                    for (ProductRunXML product : runner.getProducts()) {
                        // includes
                        if (product.hasIncludes()) {
                            for (String includeSourceName : product
                                    .getIncludes()) {
                                if (ffmpProduct.getSourceName().equals(
                                        includeSourceName)) {
                                    // no duplicate keys!
                                    if (!sites
                                            .contains(product.getProductKey())) {
                                        sites.add(product.getProductKey());
                                    }
                                }
                            }
                        }
                        // excludes
                        if (product.hasExcludes()) {
                            for (String excludeSourceName : product
                                    .getExcludes()) {
                                if (ffmpProduct.getSourceName().equals(
                                        excludeSourceName)) {
                                    sites.remove(product.getProductKey());
                                }
                            }
                        }
                    }
                } else {
                    // No mosaic, just individual site run
                    String siteKey = dataKey;

                    // special case for XMRG's
                    if (ffmpProduct.getDataType().equals(
                            FFMPSourceConfigurationManager.DATA_TYPE.XMRG
                                    .getDataType())) {

                        siteKey = null;
                        String primarySource = null;

                        for (ProductXML product : getSourceConfig()
                                .getProducts()) {
                            if (product.containsSource(ffmpProduct
                                    .getSourceName())) {
                                primarySource = product.getPrimarySource();
                                break;
                            }
                        }

                        for (ProductRunXML productRun : runner.getProducts()) {
                            if (productRun.getProductName().equals(
                                    primarySource)) {
                                siteKey = productRun.getProductKey();
                                break;
                            }
                        }
                    }

                    sites.add(siteKey);
                }

                int i = 0;
                if (sites != null) {
                    // set the latch keys
                    ArrayList<String> lsites = new ArrayList<String>();
                    for (String site : sites) {
                        lsites.add(site);
                    }

                    productKeys.put(ffmpProduct.getSourceName(), lsites);
                }

                for (String productKey : sites) {

                    FFMPRecord ffmpRec = new FFMPRecord();
                    ffmpRec.setSourceName(ffmpProduct.getSourceName());
                    ffmpRec.setDataKey(dataKey);
                    ffmpRec.setSiteKey(productKey);
                    ffmpRec.setPluginName(getCompositeProductType());
                    ffmpRec.setWfo(config.getCWA());
                    FFMPProcessor ffmp = new FFMPProcessor(config, generator,
                            ffmpRec, template);
                    ffmpRec = ffmp.processFFMP(ffmpProduct);
                    ffmpRec.constructDataURI();

                    if (ffmpRec != null) {

                        if (ffmp.isFFTI()) {
                            fftiDone = false;
                            if (!fftiSources.contains(ffmp.getFFTISource())) {
                                FFTIProcessor ffti = new FFTIProcessor(
                                        generator, ffmpRec,
                                        ffmp.getFFTISource());
                                fftiSources.add(ffmp.getFFTISource());
                                // System.out.println("Adding source to FFTISources!!!!!!!!!!!!"+ffmpRec.getSourceName());
                                ffti.processFFTI();
                            }
                        }
                        // this is a threaded process!!!!!!!!!!!
                        // Added this to speed the processing of mosaiced
                        // sources.
                        // Before all processing was in line to the source
                        // thread.
                        // This caused slowness in the overall processing.
                        // By allowing the mosaic components to be concurrently
                        // processed it has drastically sped up overall FFMP
                        // performance.
                        processDataContainer(ffmpRec, productKey);
                        ffmpRecords.add(ffmpRec);
                    }
                    i++;
                }

                while (productKeys.size() > 0) {
                    // wait for all threads to finish before returning
                    try {
                        Thread.sleep(50);
                        statusHandler.handle(Priority.DEBUG,
                                "Checking status ..." + productKeys.size());
                        for (String source : productKeys.keySet()) {
                            statusHandler.handle(Priority.DEBUG,
                                    "Still processing ..." + source);
                        }
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }

            FFMPRecord[] recs = new FFMPRecord[ffmpRecords.size()];
            for (int i = 0; i < ffmpRecords.size(); i++) {
                recs[i] = ffmpRecords.get(i);
            }
            products.put(ffmpProduct.getSourceName(), recs);
            processes.remove(ffmpProduct.getSourceName());
        }
    }

    /**
     * Inner class to background template creation
     * 
     * @author dhladky
     * 
     */
    private class TemplateLoader implements Runnable {

        ArrayList<DomainXML> templateDomains;

        DomainXML primaryDomain;

        @Override
        public void run() {
            statusHandler.handle(Priority.DEBUG, getGeneratorName()
                    + " Start loader ");

            for (DomainXML domain : templateDomains) {
                if (domain.isPrimary()) {
                    primaryDomain = domain;
                }
            }

            // generate templates and unify geometries
            loaded = load();
            statusHandler.handle(Priority.DEBUG, getGeneratorName()
                    + " Finishing loader ");
        }

        public TemplateLoader(ArrayList<DomainXML> templateDomains) {
            this.templateDomains = templateDomains;
        }

        /**
         * 
         * @param domain
         * @return
         */
        public void createUnifiedGeometries(DomainXML domain) {
            ArrayList<String> hucsToGen = new ArrayList<String>();
            hucsToGen.add("ALL");
            hucsToGen.add("COUNTY");

            for (int i = template.getTotalHucLevels() - 1; i >= 0; i--) {
                hucsToGen.add("HUC" + i);
            }

            for (String huc : hucsToGen) {
                template.verifyUnifiedGeometries(huc, domain.getCwa());
            }
        }

        public boolean load() {
            // load / create primary domain
            ClusterTask task = null;
            String lockDetails = getGeneratorName() + ":"
                    + primaryDomain.getCwa() + ":" + primaryDomain.getCwa();
            try {
                do {
                    task = ClusterLockUtils.lock(templateTaskName, lockDetails,
                            600 * 1000, true);
                } while (task.getLockState() != LockState.SUCCESSFUL);

                template = FFMPTemplates.getInstance(primaryDomain, MODE.EDEX);
                // setup the config
                getTemplateConfig();
                createUnifiedGeometries(primaryDomain);
            } finally {
                if ((task != null)
                        && (task.getLockState() == LockState.SUCCESSFUL)) {
                    ClusterLockUtils.unlock(task, false);
                }
            }

            // load the secondary domains
            List<DomainXML> domainsToGen = new ArrayList<DomainXML>(
                    templateDomains);
            while (domainsToGen.size() > 0) {
                Iterator<DomainXML> iter = domainsToGen.iterator();
                boolean processedDomain = false;
                while (iter.hasNext()) {
                    DomainXML domain = iter.next();
                    lockDetails = getGeneratorName() + ":"
                            + primaryDomain.getCwa() + ":" + domain.getCwa();
                    try {
                        task = ClusterLockUtils.lock(templateTaskName,
                                lockDetails, 300 * 1000, false);

                        if (task.getLockState() == LockState.SUCCESSFUL) {
                            template.addDomain(domain);
                            createUnifiedGeometries(domain);
                            iter.remove();
                            processedDomain = true;
                        }
                    } finally {
                        if ((task != null)
                                && (task.getLockState() == LockState.SUCCESSFUL)) {
                            ClusterLockUtils.unlock(task, false);
                        }
                    }
                }

                if (!processedDomain) {
                    // Didn't process a domain, locked by another cluster
                    // member, sleep and try again
                    try {
                        Thread.sleep(100);
                    } catch (InterruptedException e) {

                    }
                }
            }

            return template.done;
        }
    }

    /**
     * Gets the string buffer for the RFC's
     * 
     * @param run
     * @return
     */
    private String getRFCString(FFMPRunXML run) {
        StringBuffer buf = new StringBuffer();

        for (SourceIngestConfigXML ingest : run.getSourceIngests()) {
            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(ingest.getSourceName());
            if (source.isRfc()) {
                int i = 0;
                for (String dataKey : ingest.getDataKey()) {
                    if (i < ingest.getDataKey().size() - 1) {
                        buf.append(dataKey + ",");
                    } else {
                        buf.append(dataKey);
                    }
                    i++;
                }
                break;
            }
        }
        return buf.toString();
    }

    /**
     * Gets the string buffer for the sites, specific to RADAR type data
     * 
     * @param run
     * @return
     */
    private String getSiteString(FFMPRunXML run) {
        String sites = null;
        StringBuffer buf = new StringBuffer();
        for (ProductRunXML product : run.getProducts()) {
            SourceXML source = getSourceConfig().getSource(
                    product.getProductName());
            if (source.getDataType().equals(DATA_TYPE.RADAR.getDataType())) {
                buf.append(product.getProductKey() + ",");
            }
        }
        sites = buf.toString();
        if (sites.endsWith(",")) {
            sites = sites.substring(0, sites.length() - 1);
        }
        return sites;
    }

    /**
     * Write your new SourceBins
     * 
     * @param sourceList
     */
    public void writeSourceBins(SourceBinList sourceList) {

        try {
            LocalizationContext lc = pathManager.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

            LocalizationFile lflist = pathManager.getLocalizationFile(lc,
                    getAbsoluteSourceFileName(sourceList.getSourceId()));

            FileUtil.bytes2File(
                    SerializationUtil.transformToThrift(sourceList),
                    lflist.getFile(), true);

            lflist.save();

            statusHandler.handle(Priority.INFO, "Wrote FFMP source Bin File: "
                    + sourceList.getSourceId());

        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (FileNotFoundException fnfe) {
            fnfe.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } catch (LocalizationOpFailedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Read out your SourceBins
     * 
     * @param sourceId
     * @return
     */
    public SourceBinList readSourceBins(String sourceId) {

        SourceBinList sbl = null;
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteSourceFileName(sourceId));

        try {
            sbl = (SourceBinList) SerializationUtil
                    .transformFromThrift(FileUtil.file2bytes(f.getFile(), true));
        } catch (FileNotFoundException fnfe) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to locate file " + f.getName());
        } catch (SerializationException se) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to read file " + f.getName());
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }

        return sbl;
    }

    /**
     * Gets the completed filename
     * 
     * @return
     */
    public String getAbsoluteSourceFileName(String sourceId) {
        return "ffmp" + File.separator + "sources" + File.separator + sourceId
                + ".bin";
    }

    /**
     * See if you have one
     * 
     * @param sourceId
     * @return
     */
    public boolean isExistingSourceBin(String sourceId) {
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteSourceFileName(sourceId));
        return f.exists();
    }

    /**
     * Gets the list of bins for that source
     * 
     * @param sourceId
     * @return
     */
    public SourceBinList getSourceBinList(String sourceId) {
        SourceBinList sbl = null;
        if (!sourceBins.containsKey(sourceId)) {
            sbl = readSourceBins(sourceId);
            sourceBins.put(sourceId, sbl);
        } else {
            sbl = sourceBins.get(sourceId);
        }

        return sbl;
    }

    /**
     * Sets the source bins, first time
     * 
     * @param sbl
     */
    public void setSourceBinList(SourceBinList sbl) {
        sourceBins.put(sbl.getSourceId(), sbl);
        writeSourceBins(sbl);
    }

    /**
     * Do pull strategy on FFG data
     * 
     * @param filter
     * @return
     */
    private FFMPURIFilter getFFG(FFMPURIFilter filter) {

        ArrayList<String> uris = new ArrayList<String>();

        for (String rfc : filter.getRFC()) {
            // get a hash of the sources and their grib ids
            Set<String> sources = FFMPUtils.getFFGParameters(rfc);
            if (sources != null) {
                if (sources.size() > 0) {
                    for (String source : sources) {

                        SourceXML sourceXml = getSourceConfig().getSource(
                                source);

                        if (sourceXml != null) {

                            String plugin = getSourceConfig().getSource(source)
                                    .getPlugin();
                            uris.add(FFMPUtils.getFFGDataURI(rfc, source,
                                    plugin));
                        }
                    }
                }
            }
        }
        // treat it like a regular uri in the filter.
        if (uris.size() > 0) {
            for (String dataUri : uris) {
                // add your pattern checks to the key
                for (Pattern pattern : filter.getMatchURIs().keySet()) {
                    statusHandler.handle(Priority.DEBUG,
                            "Pattern: " + pattern.toString() + " Key: "
                                    + dataUri);
                    try {
                        if (pattern.matcher(dataUri).find()) {
                            // matches one of them, which one?
                            String matchKey = filter.getPatternName(pattern);
                            // put the sourceName:dataPath key into the sources
                            // array list
                            filter.getSources().put(matchKey, dataUri);
                        }
                    } catch (Exception e) {
                        statusHandler.handle(
                                Priority.ERROR,
                                "Unable to locate new FFG file. "
                                        + pattern.toString());
                    }
                }
            }
        }

        return filter;
    }

    /**
     * get the FFMP data container for this source
     * 
     * @param sourceName
     * 
     * @return
     */

    public FFMPDataContainer getFFMPDataContainer(String siteSourceKey,
            ArrayList<String> hucs, Date backDate) {

        FFMPDataContainer container = ffmpData.get(siteSourceKey);

        if (container == null) {

            String siteKey = null;

            String[] parts = siteSourceKey.split("-");

            if (parts.length > 1) {
                siteKey = parts[0];
            }

            container = loadFFMPDataContainer(siteSourceKey,

            hucs, siteKey,

            config.getCWA(), backDate);

            if (container != null) {
                ffmpData.put(siteSourceKey, container);
            }
        }

        return container;
    }

    /*
     * Gets the FFTI sources to be run
     */
    public ArrayList<FFTISourceXML> getFFTISources() {
        return fftiSources;
    }

    /**
     * source config manager
     * 
     * @return
     */
    public FFMPSourceConfigurationManager getSourceConfig() {
        if (fscm == null) {
            fscm = FFMPSourceConfigurationManager.getInstance();
            fscm.addListener(this);
        }
        return fscm;
    }

    /**
     * run config manager
     * 
     * @return
     */
    public FFMPRunConfigurationManager getRunConfig() {
        if (frcm == null) {
            frcm = FFMPRunConfigurationManager.getInstance();
            frcm.addListener(this);
        }
        return frcm;
    }

    /**
     * Template config manager
     * 
     * @return
     */
    public FFMPTemplateConfigurationManager getTemplateConfig() {
        if (tempConfig == null) {
            tempConfig = FFMPTemplateConfigurationManager.getInstance();
            tempConfig.addListener(this);
        }
        return tempConfig;
    }

    /**
     * dispatch a filter for processing
     * 
     * @param filter
     */
    private void dispatch(FFMPURIFilter filter) {

        try {
            EDEXUtil.getMessageProducer().sendAsync(
                    routeId,
                    SerializationUtil.transformToThrift(filter
                            .createGenerateMessage()));
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, getGeneratorName()
                    + ": filter: " + filter.getName()
                    + ": failed to route filter to generator", e);
            e.printStackTrace();
        }

        filter.setValidTime(new Date(System.currentTimeMillis()));
        filter.reset();
    }

    /**
     * Process this data container
     * 
     * @param sourceSiteDataKey
     * @param huc
     * @param wfo
     * @return
     */
    public void processDataContainer(FFMPRecord ffmpRec, String productKey) {

        this.getProcessExecutor().execute(
                new ProcessDataContainer(ffmpRec, productKey));
    }

    /**
     * Inner class to thread writing of BuddyFiles
     * 
     * @author dhladky
     * 
     */
    private class ProcessDataContainer implements Runnable {

        private FFMPRecord ffmpRec;

        private String productKey;

        public void run() {
            try {
                processDataContainer(ffmpRec, productKey);
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "ProcessDataContainer: removed " + e.getMessage());
            }
        }

        public ProcessDataContainer(FFMPRecord ffmpRec, String productKey) {
            this.ffmpRec = ffmpRec;
            this.productKey = productKey;
        }

        /**
         * Process this data container
         * 
         * @param ffmpRec
         * @param write
         */
        private void processDataContainer(FFMPRecord ffmpRec, String productKey) {

            String sourceName = null;
            Date backDate = null;
            String sourceSiteDataKey = null;
            FFMPDataContainer fdc = null;
            boolean write = true;

            try {
                // write out the fast loader buddy file

                long ptime = System.currentTimeMillis();
                SourceXML source = getSourceConfig().getSource(
                        ffmpRec.getSourceName());
                String dataKey = ffmpRec.getDataKey();

                if (source.getSourceType().equals(
                        SOURCE_TYPE.GUIDANCE.getSourceType())) {
                    sourceName = source.getDisplayName();
                    sourceSiteDataKey = sourceName;
                    // FFG is so infrequent go back a day
                    backDate = new Date(config.getDate().getTime()
                            - (3600 * 1000 * 24));
                } else {
                    sourceName = ffmpRec.getSourceName();
                    sourceSiteDataKey = sourceName + "-" + ffmpRec.getSiteKey()
                            + "-" + dataKey;
                    backDate = new Date(ffmpRec.getDataTime().getRefTime()
                            .getTime()
                            - (3600 * 1000 * 6));
                }

                // deal with setting of needed HUCS
                ArrayList<String> hucs = template.getTemplateMgr()
                        .getHucLevels();

                if (source.getSourceType().equals(
                        SOURCE_TYPE.GAGE.getSourceType())
                        || source.getSourceType().equals(
                                SOURCE_TYPE.GUIDANCE.getSourceType())) {
                    hucs.clear();
                    hucs.add("ALL");
                } else {
                    hucs.remove("VIRTUAL");
                }

                // pull from disk if there
                fdc = getFFMPDataContainer(sourceSiteDataKey, hucs, backDate);

                // brand new or initial load up
                if (fdc == null || !loadedData.contains(sourceSiteDataKey)) {

                    long time = System.currentTimeMillis();
                    fdc = new FFMPDataContainer(sourceSiteDataKey, hucs);
                    fdc = FFTIProcessor.populateDataContainer(fdc, template,
                            hucs, backDate, ffmpRec.getDataTime().getRefTime(),
                            ffmpRec.getWfo(), source, ffmpRec.getSiteKey());

                    if (!loadedData.contains(sourceSiteDataKey)) {
                        loadedData.add(sourceSiteDataKey);
                    }

                    long time2 = System.currentTimeMillis();
                    statusHandler.handle(Priority.DEBUG,
                            "Populated new source: in " + (time2 - time)
                                    + " ms: source: " + sourceSiteDataKey);

                } else {

                    long time = System.currentTimeMillis();
                    // guidance sources are treated as a mosaic and are handled
                    // differently. They are force read at startup.
                    // This is the main line sequence a source will take when
                    // updated.
                    if (!source.getSourceType().equals(
                            SOURCE_TYPE.GUIDANCE.getSourceType())) {

                        Date newDate = fdc.getNewest();
                        Date oldDate = fdc.getOldest();

                        if (newDate != null && oldDate != null) {
                            if ((ffmpRec.getDataTime().getRefTime().getTime() - newDate
                                    .getTime()) >= (source
                                    .getExpirationMinutes(ffmpRec.getSiteKey()) * 60 * 1000)) {
                                // force a re-query back to the newest time in
                                // existing source container, this will fill in
                                // gaps
                                // if
                                // they exist.
                                fdc = FFTIProcessor.populateDataContainer(fdc,
                                        template, null, newDate, ffmpRec
                                                .getDataTime().getRefTime(),
                                        ffmpRec.getWfo(), source, ffmpRec
                                                .getSiteKey());

                            } else if (oldDate.after(new Date(backDate
                                    .getTime()
                                    - (source.getExpirationMinutes(ffmpRec
                                            .getSiteKey()) * 60 * 1000)))) {
                                // force a re-query back to barrierTime for
                                // existing source container, this happens if
                                // the
                                // ingest was turned off for some period of
                                // time.
                                fdc = FFTIProcessor.populateDataContainer(fdc,
                                        template, null, backDate, oldDate,
                                        ffmpRec.getWfo(), source,
                                        ffmpRec.getSiteKey());
                            }
                        }

                        long time2 = System.currentTimeMillis();
                        statusHandler.handle(Priority.DEBUG,
                                "Checked Source files: in " + (time2 - time)
                                        + " ms: source: " + sourceSiteDataKey);
                    }
                }

                // add current record data
                for (String huc : hucs) {
                    fdc.addFFMPEntry(ffmpRec.getDataTime().getRefTime(),
                            source, ffmpRec.getBasinData(huc), huc,
                            ffmpRec.getSiteKey());
                }
                // set the name
                fdc.setFilePath("" + sharePath + ffmpRec.getWfo() + "/"
                        + sourceSiteDataKey);
                // cache it temporarily for FFTI use
                if (source.getSourceType().equals(
                        SOURCE_TYPE.GUIDANCE.getSourceType())) {
                    // only write last one
                    write = false;

                    if (!ffmpData.containsKey(sourceSiteDataKey)) {
                        ffmpData.put(sourceSiteDataKey, fdc);
                    } else {
                        ffmpData.replace(sourceSiteDataKey, fdc);
                    }
                }

                statusHandler.handle(
                        Priority.INFO,
                        "Processed FFMPDataContainer: in "
                                + (System.currentTimeMillis() - ptime)
                                + " ms: source: " + sourceSiteDataKey);
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Failed Processing FFMPDataContainer" + e.getMessage());

            } finally {
                // moved writing here to remain safe from possible race
                // condition between processing threads
                if (productKeys != null) {
                    if (productKeys.containsKey(ffmpRec.getSourceName())) {
                        productKeys.get(ffmpRec.getSourceName()).remove(
                                productKey);
                        // System.out.println("Removed productKey: "+productKey);
                        if (productKeys.get(ffmpRec.getSourceName()).size() == 0) {
                            // System.out.println("Removed source: "+ffmpRec.getSourceName()+" now writing");
                            productKeys.remove(ffmpRec.getSourceName());
                            // last one, allow write
                            write = true;
                        }
                    }
                }
                // purge it up
                if (fdc != null) {
                    // this is defensive for if errors get thrown
                    if (backDate == null) {
                        backDate = new Date((System.currentTimeMillis())
                                - (3600 * 1000 * 6));
                    }

                    fdc.purge(backDate);

                    if (write) {
                        // write it out
                        writeLoaderBuddyFiles(fdc);
                    }
                }
            }
        }
    }

    /**
     * load existing container
     * 
     * @param sourceSiteDataKey
     * @param hucs
     * @param siteKey
     * @param wfo
     * @return
     */
    public FFMPDataContainer loadFFMPDataContainer(String sourceSiteDataKey,
            ArrayList<String> hucs, String siteKey, String wfo, Date backDate) {

        long time = System.currentTimeMillis();
        FFMPDataContainer fdc = null;

        synchronized (hucs) {
            for (String huc : hucs) {

                FFMPBasinData basinData = null;

                if (checkBuddyFile(sourceSiteDataKey, huc, wfo, backDate)) {
                    try {
                        basinData = readLoaderBuddyFile(sourceSiteDataKey, huc,
                                wfo, backDate);
                    } catch (Exception e) {
                        statusHandler.handle(
                                Priority.ERROR,
                                "General Error Reading buddy file: "
                                        + e.getMessage());
                    }

                    if (fdc == null) {
                        fdc = new FFMPDataContainer(sourceSiteDataKey, hucs);
                    }
                }

                if (basinData != null) {
                    fdc.setBasinBuddyData(basinData, huc);
                }
            }
        }

        if (fdc != null) {
            long time2 = System.currentTimeMillis();
            statusHandler.handle(Priority.DEBUG, "Loaded Source files: in "
                    + (time2 - time) + " ms: source: " + sourceSiteDataKey);
        }

        return fdc;
    }

    /**
     * Load existing buddy file
     * 
     * @param sourceSiteDataKey
     * @param huc
     * @param wfo
     * @return
     * @throws IOException
     */
    private FFMPBasinData readLoaderBuddyFile(String sourceSiteDataKey,
            String huc, String wfo, Date backDate) throws IOException {

        File file = new File(sharePath + wfo + "/" + sourceSiteDataKey + "-"
                + huc + ".bin");
        FFMPBasinData basinData = null;
        BufferedInputStream is = null;

        try {
            is = new BufferedInputStream(new FileInputStream(file));
            DynamicSerializationManager dsm = DynamicSerializationManager
                    .getManager(SerializationType.Thrift);
            basinData = (FFMPBasinData) dsm.deserialize(is);
        } catch (SerializationException e) {
            statusHandler
                    .handle(Priority.ERROR,
                            "Serialization Error Reading buddy file: "
                                    + e.getMessage());
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR,
                    "IO Error Reading buddy file: " + e.getMessage());
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "General Error Reading buddy file: " + e.getMessage());
        } catch (Throwable t) {
            statusHandler.handle(Priority.ERROR,
                    "Bogus Thrift Error Reading buddy file: " + t.getMessage());
        } finally {
            if (is != null) {
                is.close();
            }
        }

        return basinData;

    }

    /**
     * Write buddy file
     * 
     * @param sourceSiteDataKey
     * @param huc
     * @param wfo
     * @return
     */
    public void writeLoaderBuddyFiles(FFMPDataContainer fdc) {

        // Write all huc levels in separate files
        File fileDir = new File("" + sharePath + config.getCWA());
        if (!fileDir.exists()) {
            fileDir.mkdir();
        }

        WriteFiles writer = new WriteFiles(fdc);
        writer.run();
    }

    /**
     * Inner class to thread writing of BuddyFiles
     * 
     * @author dhladky
     * 
     */
    private class WriteFiles implements Runnable {

        private FFMPDataContainer fdc;

        public void run() {
            try {
                long time = System.currentTimeMillis();
                write();
                long time2 = System.currentTimeMillis();
                statusHandler.handle(Priority.DEBUG, "Wrote loader files: in "
                        + (time2 - time) + " ms  :" + fdc.getFilePath());
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "WriteFile: removed " + e.getMessage());
            }
        }

        public WriteFiles(FFMPDataContainer fdc) {
            this.fdc = fdc;
            statusHandler.handle(Priority.DEBUG, "Created FileWriter");
        }

        /**
         * The actual work gets done here
         */
        public void write() throws Exception {

            try {

                File sharePathFile = new File(sharePath + config.getCWA());
                if (!sharePathFile.exists()) {
                    sharePathFile.mkdirs();
                }

                String fileName = fdc.getFilePath();
                // lock for atomic write and read
                HashMap<String, String> fileNames = new HashMap<String, String>();
                File lockfile = new File(fileName + ".lock");
                lockfile.createNewFile();

                try {

                    if (lockfile.canWrite()) {
                        // write the lock if we can even write to anything
                        synchronized (fdc.getKeys()) {
                            for (String huc : fdc.getKeys()) {

                                FFMPBasinData fbd = fdc.getBasinData(huc);

                                if (fbd.getBasins().size() > 0) {

                                    String tmpFilePath = fileName + "-" + huc
                                            + ".tmp";
                                    BufferedOutputStream os = null;

                                    try {
                                        File file = new File(tmpFilePath);
                                        file.createNewFile();

                                        if (file.canWrite()) {
                                            os = new BufferedOutputStream(
                                                    new FileOutputStream(file));
                                            DynamicSerializationManager dsm = DynamicSerializationManager
                                                    .getManager(SerializationType.Thrift);
                                            dsm.serialize(fbd, os);
                                            fileNames.put(tmpFilePath, fileName
                                                    + "-" + huc + ".bin");
                                        } else {
                                            statusHandler
                                                    .handle(Priority.WARN,
                                                            "Can not write buddy file: "
                                                                    + file.getAbsolutePath());
                                        }
                                    } catch (SerializationException e) {
                                        statusHandler.handle(Priority.ERROR,
                                                "Serialization Error Writing buddy file: "
                                                        + e.getMessage());
                                    } catch (IOException e) {
                                        statusHandler.handle(Priority.ERROR,
                                                "IO Error Writing buddy file: "
                                                        + e.getMessage());
                                    } catch (Exception e) {
                                        statusHandler.handle(Priority.ERROR,
                                                "General Error Writing buddy file: "
                                                        + e.getMessage());
                                    } finally {
                                        if (os != null) {
                                            os.close();
                                        }
                                    }
                                }
                            }
                        }
                    }
                } catch (Exception e) {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "Error writing Buddy File group: "
                                            + e.getMessage());
                } finally {
                    // rename the files to real path
                    try {
                        for (String tmpName : fileNames.keySet()) {
                            File file = new File(tmpName);
                            if (file.renameTo(new File(fileNames.get(tmpName)))) {
                                statusHandler.handle(
                                        Priority.DEBUG,
                                        "Successful rename: : "
                                                + fileNames.get(tmpName));
                            } else {
                                statusHandler.handle(
                                        Priority.ERROR,
                                        "UN-Successful rename: : "
                                                + fileNames.get(tmpName));
                            }
                        }

                        if (lockfile.exists()) {
                            lockfile.delete();
                        }

                    } catch (Exception e) {
                        statusHandler.handle(
                                Priority.ERROR,
                                "IO Error Renaming buddy file: "
                                        + e.getMessage());
                    }
                }

            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "IO Error writing buddy files: " + e.getMessage());
            }
        }
    }

    /**
     * Load existing buddy file
     * 
     * @param sourceSiteDataKey
     * @param huc
     * @param wfo
     * @return
     */
    public boolean checkBuddyFile(String sourceSiteDataKey, String huc,
            String wfo, Date backDate) {

        File file = new File(sharePath + wfo + "/" + sourceSiteDataKey + "-"
                + huc + ".bin");

        String sourceName = sourceSiteDataKey.split("-")[0];
        SourceXML source = getSourceConfig().getSourceByDisplayName(sourceName);
        if (source != null) {

            if (source.getSourceType().equals(
                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                if (file.exists() && file.canRead() && file.canWrite()) {
                    return true;
                }
            } else {
                if (file.exists() && file.canRead() && file.canWrite()
                        && (file.lastModified() > backDate.getTime())) {
                    // System.out.println("File update and exists..."+sourceSiteDataKey);
                    return true;
                }
            }
        }

        return false;

    }

    @Override
    public synchronized void configChanged(MonitorConfigEvent fce) {

        boolean reload = false;

        if (fce.getSource() instanceof FFMPTemplateConfigurationManager) {
            statusHandler
                    .handle(Priority.INFO,
                            "Re-configuring FFMP & URI filters...Template Config change");
            reload = true;
            FFMPTemplateConfigurationManager ftcm = (FFMPTemplateConfigurationManager) fce
                    .getSource();
            if (ftcm.isRegenerate()) {
                template.dumpTemplates();
            }

            tempConfig = null;

        } else if (fce.getSource() instanceof FFMPRunConfigurationManager) {
            statusHandler.handle(Priority.INFO,
                    "Re-configuring FFMP & URI filters...Run Config change");
            reload = true;
            frcm = null;
        }

        else if (fce.getSource() instanceof FFMPSourceConfigurationManager) {
            statusHandler.handle(Priority.INFO,
                    "Re-configuring FFMP & URI filters...Source Config change");
            reload = true;
            fscm = null;
        }

        if (reload) {

            ffgCheck = false;
            resetFilters();

            if (sourceBins != null) {
                sourceBins.clear();
            }

            loadedData.clear();

            if (ffmpData != null) {
                ffmpData.clear();
            }
            if (fftiData != null) {
                fftiData.clear();
            }

            DatMenuUtil dmu = new DatMenuUtil();
            dmu.setDatSite(PropertiesFactory.getInstance().getEnvProperties()
                    .getEnvValue("SITENAME"));
            dmu.setOverride(true);
            dmu.createMenus();
        }
    }

    /**
     * FFTI data cache
     * 
     * @param ffti
     */
    public void writeFFTIData(String fftiName, FFTIData ffti) {
        if (fftiData.containsKey(fftiName)) {
            fftiData.replace(fftiName, ffti);
        } else {
            fftiData.put(fftiName, ffti);
        }

        writeFFTIFile(ffti, fftiName);
    }

    /**
     * Get FFTI data cache
     * 
     * @param fftiName
     * @return
     */
    public FFTIData getFFTIData(String fftiName) {
        // preserve the state of the reset value
        boolean reset = true;
        if (fftiData.containsKey(fftiName)) {
            reset = fftiData.get(fftiName).isReset();
        }

        FFTIData ffti = readFFTIData(fftiName);

        if (fftiData != null) {
            ffti.setReset(reset);
            fftiData.put(fftiName, ffti);
        }
        return ffti;
    }

    /**
     * Write your FFTI Data files
     * 
     * @param sourceList
     */
    public void writeFFTIFile(FFTIData ffti, String fftiName) {

        try {
            LocalizationContext lc = pathManager.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

            LocalizationFile lflist = pathManager.getLocalizationFile(lc,
                    getAbsoluteFFTIFileName(fftiName));

            FileUtil.bytes2File(SerializationUtil.transformToThrift(ffti),
                    lflist.getFile(), true);

            lflist.save();

            statusHandler.handle(Priority.DEBUG, "Wrote FFMP FFTI file: "
                    + fftiName);

        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (FileNotFoundException fnfe) {
            fnfe.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } catch (LocalizationOpFailedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Read out your FFTI Files
     * 
     * @param sourceId
     * @return
     */
    public FFTIData readFFTIData(String fftiName) {

        FFTIData ffti = null;
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFFTIFileName(fftiName));

        try {
            ffti = (FFTIData) SerializationUtil.transformFromThrift(FileUtil
                    .file2bytes(f.getFile(), true));
        } catch (FileNotFoundException fnfe) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to locate file " + f.getName(), fnfe);
        } catch (SerializationException se) {
            statusHandler.handle(Priority.ERROR, "Unable to serialize file "
                    + f.getName(), se);
        } catch (IOException ioe) {
            statusHandler.handle(Priority.ERROR,
                    "IO problem reading file " + f.getName(), ioe);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "General Exception reading file " + f.getName(), e);
        }

        return ffti;
    }

    /**
     * Gets the completed filename
     * 
     * @return
     */
    public String getAbsoluteFFTIFileName(String fftiName) {
        return "ffmp" + File.separator + "ffti" + File.separator + fftiName
                + ".bin";
    }

    /**
     * See if you have one
     * 
     * @param fftiName
     * @return
     */
    public boolean isFFTI(String fftiName) {
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFFTIFileName(fftiName));
        boolean exists = false;
        if (f != null) {
            exists = f.exists();
        }

        return exists;
    }

    /**
     * get the whole container
     * 
     * @return
     */
    public ConcurrentHashMap<String, FFTIData> getFFTIDataContainer() {
        return fftiData;
    }

    /**
     * the executor runner
     * 
     * @return
     */
    public Executor getProcessExecutor() {
        return processexecutor;
    }

    public void setProcessExecutor(Executor processexecutor) {
        this.processexecutor = processexecutor;
    }

}