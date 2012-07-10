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

import java.io.File;
import java.io.FileNotFoundException;
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
import com.raytheon.uf.common.cache.CacheException;
import com.raytheon.uf.common.cache.CacheFactory;
import com.raytheon.uf.common.cache.DiskCache;
import com.raytheon.uf.common.cache.ICache;
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
    public FFMPGenerator(Executor executor) {
        super(genName, productType, executor);
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

    /** Stored data for FFMP/FFTI comparisons */
    private HashMap<String, FFMPDataContainer> ffmpData = new HashMap<String, FFMPDataContainer>();

    /** run configuration manager **/
    public FFMPRunConfigurationManager frcm = null;

    /** source configuration manager **/
    public FFMPSourceConfigurationManager fscm = null;

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
                        Thread.sleep(200);
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
                            Thread.sleep(200);
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
                    statusHandler.handle(Priority.INFO, config.getCWA()
                            + " no new products to produce.");
                }

            } catch (Throwable e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to process FFMP Records.");
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
                                ffti.processFFTI();
                            }
                        }

                        // Don't write loader buddy's on long duration
                        // sources. Not necessary 6*60 = 360
                        if (getSourceConfig()
                                .getSource(ffmpRec.getSourceName())
                                .getExpirationMinutes(productKey) < 360) {

                            processDataContainer(ffmpRec);
                        }

                        ffmpRecords.add(ffmpRec);
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
                    getAbsoluteFileName(sourceList.getSourceId()));

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
                getAbsoluteFileName(sourceId));

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
    public String getAbsoluteFileName(String sourceId) {
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
                getAbsoluteFileName(sourceId));
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

        sbl = getSourceBin(sourceId);
        
        if (sbl == null) {
            sbl = readSourceBins(sourceId);
            setSourceBin(sbl, sourceId);
        } 

        return sbl;
    }

    /**
     * Sets the source bins, first time
     * 
     * @param sbl
     */
    public void setSourceBinList(SourceBinList sbl) {
    	
        setSourceBin(sbl, sbl.getSourceId());
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
            HashMap<String, Integer> sources = FFMPUtils.getFFGModelInfo(rfc);
            if (sources != null) {
                if (sources.size() > 0) {
                    for (String source : sources.keySet()) {

                        SourceXML sourceXml = getSourceConfig().getSource(
                                source);

                        if (sourceXml != null) {

                            String plugin = getSourceConfig().getSource(source)
                                    .getPlugin();
                            uris.add(FFMPUtils.getFFGDataURI(
                                    sources.get(source), plugin));
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
     * get the hash
     * 
     * @return
     */
    public HashMap<String, FFMPDataContainer> getFFMPData() {
        return ffmpData;
    }

    /**
     * get the FFMP data container for this source
     * 
     * @param sourceName
     * 
     * @return
     */
    public FFMPDataContainer getFFMPDataContainer(String sourceName) {

        FFMPDataContainer container = ffmpData.get(sourceName);

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
     * FFMP buddy loader
     * 
     * @param ffmpRec
     * @param siteKey
     */
    private void processDataContainer(FFMPRecord ffmpRec) {

        // write out the fast loader buddy file
        long time = System.currentTimeMillis();
        SourceXML source = getSourceConfig().getSource(ffmpRec.getSourceName());
        String sourceName = null;
        String dataKey = null;

        if (source.getSourceType().equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
            sourceName = source.getDisplayName();
            dataKey = SOURCE_TYPE.GUIDANCE.getSourceType();
        } else {
            sourceName = ffmpRec.getSourceName();
            dataKey = ffmpRec.getDataKey();
        }

        String sourceSiteDataKey = sourceName + "-" + ffmpRec.getSiteKey()
                + "-" + dataKey;
        FFMPDataContainer fdc = ffmpData.get(sourceSiteDataKey);

        // Make a 6 hour "Buddy" loader file
        Date backDate = new Date(ffmpRec.getDataTime().getRefTime().getTime()
                - (3600 * 1000 * 6));

        // cache management
        if (fdc == null) {
            fdc = loadFFMPDataContainer(sourceSiteDataKey, ffmpRec
                    .getBasinsMap().keySet(), ffmpRec.getSiteKey(),
                    ffmpRec.getWfo(), backDate);
            // brand new
            if (fdc == null) {
                fdc = new FFMPDataContainer(sourceSiteDataKey, ffmpRec
                        .getBasinsMap().keySet());
            }

            fdc = FFTIProcessor.populateDataContainer(fdc, template, ffmpRec
                    .getBasinsMap().keySet(), backDate, ffmpRec.getDataTime()
                    .getRefTime(), ffmpRec.getWfo(), source, ffmpRec
                    .getSiteKey());
        }

		// add current record data
		for (String huc : ffmpRec.getBasinsMap().keySet()) {
			fdc.addFFMPEntry(ffmpRec.getDataTime().getRefTime(), source,
					ffmpRec.getBasinData(huc), huc, ffmpRec.getSiteKey());
		}

		fdc.purge(backDate);
        fdc.writeDataContainer(sourceSiteDataKey, sharePath, ffmpRec.getWfo());

        // write it back
        ffmpData.put(sourceSiteDataKey, fdc);
        long time2 = System.currentTimeMillis();

        statusHandler.handle(Priority.INFO, "Wrote loader buddy files: "
                + sourceSiteDataKey + " in " + (time2 - time) + " ms");

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
    private FFMPDataContainer loadFFMPDataContainer(String sourceSiteDataKey,
            Set<String> hucs, String siteKey, String wfo, Date backDate) {

        FFMPDataContainer fdc = new FFMPDataContainer(sourceSiteDataKey, hucs);

        for (String huc : hucs) {
			FFMPBasinData basinData = null;
			if (checkBuddyFile(sourceSiteDataKey, huc, wfo, backDate)) {
				basinData = readLoaderBuddyFile(sourceSiteDataKey, huc, wfo,
						backDate);
			}
			if (basinData != null) {
				fdc.setBasinBuddyData(basinData, huc);
			} else {
				// rebuild it
				return null;
			}
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
     */
    private FFMPBasinData readLoaderBuddyFile(String sourceSiteDataKey,
			String huc, String wfo, Date backDate) {

		File file = new File(sharePath + wfo + "/" + sourceSiteDataKey + "-"
				+ huc + ".bin");
		FFMPBasinData basinData = null;

		try {
			basinData = (FFMPBasinData) SerializationUtil
					.transformFromThrift(FileUtil.file2bytes(file));
		} catch (SerializationException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		return basinData;

	}
    
    /**
     * Load existing buddy file
     * 
     * @param sourceSiteDataKey
     * @param huc
     * @param wfo
     * @return
     */
    public boolean checkBuddyFile(String sourceSiteDataKey,
            String huc, String wfo, Date backDate) {

        File file = new File(sharePath + wfo + "/" + sourceSiteDataKey + "-"
                + huc + ".bin");

        if (file.exists() && (file.lastModified() > backDate.getTime())) {
            return true;
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
            getSourceBinCache().clearCache();
            getSourceBinCache().closeCache();
            if (ffmpData != null) {
                ffmpData.clear();
            }
            DatMenuUtil dmu = new DatMenuUtil();
            dmu.setDatSite(PropertiesFactory.getInstance().getEnvProperties()
                    .getEnvValue("SITENAME"));
            dmu.setOverride(true);
            dmu.createMenus();
        }
    }
    
    /**
     * Get SourceBins from cache
     * @param siteKey
     * @param sourceName
     * @return
     */
    public SourceBinList getSourceBin(String sourceName) {
    	
    	SourceBinList bins = null;
    	
        if (sourceName != null) {
            try {
            	
            	DiskCache<SourceBinList> diskCache = getSourceBinCache();
            	bins = (SourceBinList) diskCache.getFromCache(sourceName);
                
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return bins;
    }
       
    /**
     * Set Source Bins to cache
     * @param sourceName
     * @param record
     */
     
    public void setSourceBin(SourceBinList list, String sourceName) {
    	if (sourceName != null) {
            try {
            	
            	DiskCache<SourceBinList> diskCache = getSourceBinCache();
                
                try {
					diskCache.addToCache(sourceName, list);
				} catch (IOException e) {
					e.printStackTrace();
				}
            } catch (Exception e) {
               e.printStackTrace();
            }
        }
    }

	@SuppressWarnings({ "unchecked" })
	private DiskCache<SourceBinList> getSourceBinCache() {
    	
    	DiskCache<SourceBinList> diskCache = null;
    	
    	try {
			diskCache = (DiskCache<SourceBinList>)CacheFactory.getInstance()
			  .getCache("FFMP-SourceBinList-cache");
			
		} catch (CacheException e) {
			DiskCache<SourceBinList> dc = createSourceCache("FFMP-SourceBinList-cache", 4);
			CacheFactory.getInstance().addCache("FFMP-SourceBinList-cache", dc);
			return dc;
		}
		
		return diskCache;
    }
	    
    /**
     * Create cache objects if needed
     * @param siteKey
     * @return
     */
    private DiskCache<SourceBinList> createSourceCache(String name, int size) {
    	ICache<SourceBinList> cache = new DiskCache<SourceBinList>();
    	DiskCache<SourceBinList> dc = (DiskCache<SourceBinList>) cache;
    	dc.setName(name);
    	dc.setSizeMemCacheMap(size);
    	dc.activateEdexCache();
    	
    	return dc;
    }
    

}
