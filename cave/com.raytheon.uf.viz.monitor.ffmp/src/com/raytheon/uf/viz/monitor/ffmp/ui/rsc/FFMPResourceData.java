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
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.NavigableMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.config.FFMPTemplateConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.thread.BackgroundLoadJob;
import com.raytheon.uf.viz.monitor.ffmp.ui.thread.InitHucLevelGeomsJob;
import com.raytheon.uf.viz.monitor.ffmp.ui.thread.InitialLoadJob;
import com.raytheon.uf.viz.monitor.ffmp.xml.FFMPConfigBasinXML;

/**
 * Resource data for an FFMPResource. Schedules all of the loading jobs except
 * for jobs triggered by updates.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2009 2521       dhladky     Initial creation
 * Feb 01, 2013 1569       D. Hladky   Added constants
 * Feb 10, 2013 1584       mpduff      Add performance logging.
 * Feb 28, 2013 1729       dhladky     Got rid of thread sleeps
 * Mar 06, 2013 1769       dhladky     Changed threading to use count down latch.
 * Apr 26, 2013 1954       bsteffen    Minor code cleanup throughout FFMP.
 * Jun 06, 2013 2075       njensen     Use new load jobs
 * Jul 15, 2013 2184        dhladky     Remove all HUC's for storage except ALL
 * Jul 17, 2013 2197       njensen     Broke background loading into chunks
 * Dec 04, 2013 2607       dhladky     Nullpointer in job when starting with 24 hour pull.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "ffmpResourceData")
public class FFMPResourceData extends AbstractRequestableResourceData {

    /** Performance log entry prefix */
    private static final String prefix = "FFMP ResourceData:";

    /** Number of hours back from the most recent time to load data for **/
    private static final int HOURS_BACK = 24;

    /** Number of hours a background data job should request data for **/
    private static final int LOAD_INCREMENT = 1;

    /** Performance logger */
    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler(prefix);

    public boolean tableLoad = false;

    @XmlAttribute
    public String sourceName;

    @XmlAttribute
    public String huc;

    @XmlAttribute
    public String dataKey;

    @XmlAttribute
    public String siteKey;

    public String wfo;

    /** The human readable name */
    @XmlElement(required = true)
    private String mapName = null;

    private static String[] tables = new String[] { "mapdata.ffmp_basins" };

    private String[] columns = new String[] {};

    @XmlElement
    private String geomField = "the_geom";

    @XmlElement
    public String polyText = null;

    @XmlElement
    private String shadingField;

    /** The FFMP data cache and monitor **/
    protected FFMPMonitor monitor = null;

    /** County Warning Area (Domain) for which to display **/
    protected List<DomainXML> domains = null;

    /** Product Displayed **/
    private ProductXML product = null;

    /** Field default **/
    protected FIELDS field = FIELDS.QPE;

    public FFMPResourceData() {

        super();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return mapName;
            }

        };

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        this.wfo = LocalizationManager.getContextName(LocalizationLevel.SITE);
        FFMPMonitor monitor = getMonitor();
        monitor.setWfo(wfo);
        this.field = monitor.getField(sourceName);

        perfLog.log("Loading FFMP: source: " + sourceName + ", site: "
                + siteKey + ", data: " + dataKey + ", WFO: " + wfo + ", HUC: "
                + huc);

        DataTime[] availableTimes = this.getAvailableTimes();
        // no data available;
        FFMPTemplates templates = null;
        if (availableTimes.length != 0) {
            product = monitor.getProductXML(sourceName);
            if (product != null) {
                monitor.launchSplash(siteKey);
                templates = monitor.getTemplates(siteKey);
                tableLoad = true;
            }

            if (tableLoad) {

                // initialize the cache objects
                FFMPConfig ffmpConfig = monitor.getConfig();
                FFMPConfigBasinXML cfgBasinXML = ffmpConfig.getFFMPConfigData();
                String[] sdomains = cfgBasinXML.getIncludedCWAs().split(",");
                List<DomainXML> defaults = new ArrayList<DomainXML>(
                        sdomains.length);

                for (String domain : sdomains) {

                    if (domain.isEmpty()) {
                        defaults.add(monitor.getRunConfig().getDomain(wfo));
                        break;
                    }

                    DomainXML domainXML = monitor.getRunConfig().getDomain(
                            domain);
                    if (domainXML != null) {
                        if (!defaults.contains(domainXML)) {
                            defaults.add(domainXML);
                        }
                    }
                }
                this.domains = defaults;

                final Date mostRecentTime = availableTimes[availableTimes.length - 1]
                        .getRefTime();
                final double configTimeFrame = cfgBasinXML.getTimeFrame();
                final Date timeBack = new Date(
                        (long) (mostRecentTime.getTime() - (configTimeFrame * TimeUtil.MILLIS_PER_HOUR)));

                final List<String> onlyAllHuc = new ArrayList<String>();
                onlyAllHuc.add(FFMPRecord.ALL);
                InitialLoadJob initialJob = new InitialLoadJob(this, timeBack,
                        mostRecentTime, onlyAllHuc);

                // schedule the background load to start as soon as the initial
                // completes, it will load the other hours back to 24
                initialJob.addJobChangeListener(new JobChangeAdapter() {
                    @Override
                    public void done(IJobChangeEvent event) {
                        // step back in time in increments and
                        // load the data in chunks all the way
                        // back to 24 hours
                        // before the current time
                        Date farthestBack = new Date(mostRecentTime.getTime()
                                - (HOURS_BACK * TimeUtil.MILLIS_PER_HOUR));
                        int hourBack = (int) configTimeFrame;
                        Date loadedUpTo = new Date(timeBack.getTime());
                        BackgroundLoadJob firstJob = null;
                        Job previousJob = null;
                        while (loadedUpTo.after(farthestBack)) {
                            Date startTime = new Date(
                                    loadedUpTo.getTime()
                                            - (LOAD_INCREMENT * TimeUtil.MILLIS_PER_HOUR));
                            hourBack += LOAD_INCREMENT;
                            if (startTime.before(farthestBack)) {
                                startTime = farthestBack;
                                hourBack = HOURS_BACK;
                            }

                            String jobName = "FFMP loading to hr " + hourBack;
                            if (previousJob == null) {
                                firstJob = new BackgroundLoadJob(jobName,
                                        FFMPResourceData.this, startTime,
                                        loadedUpTo, onlyAllHuc);
                                firstJob.setPreloadAvailableUris(true);
                                previousJob = firstJob;
                            } else {
                                final BackgroundLoadJob nextJob = new BackgroundLoadJob(
                                        jobName, FFMPResourceData.this,
                                        startTime, loadedUpTo, onlyAllHuc);
                                nextJob.setPreloadAvailableUris(true);
                                previousJob
                                        .addJobChangeListener(new JobChangeAdapter() {
                                            @Override
                                            public void done(
                                                    IJobChangeEvent event) {
                                                nextJob.schedule();
                                            }
                                        });
                                previousJob = nextJob;
                            }

                            loadedUpTo = startTime;
                        }
                        if (firstJob != null) {
                            firstJob.schedule();
                        }
                    }
                });
                initialJob.schedule();

                // schedule this huc geometries job to run in the
                // background so the first paints of the resource
                // will be faster
                List<String> earlyLoadHucs = new ArrayList<String>();
                earlyLoadHucs.addAll(onlyAllHuc);

                for (String otherHuc : FFMPTemplateConfigurationManager
                        .getInstance().getHucLevels()) {
                    if (!earlyLoadHucs.contains(otherHuc)) {
                        earlyLoadHucs.add(otherHuc);
                    }
                }
                earlyLoadHucs.remove(FFMPRecord.VIRTUAL);
                InitHucLevelGeomsJob hucGeomsJob = new InitHucLevelGeomsJob(
                        this.siteKey, templates, earlyLoadHucs);
                hucGeomsJob.schedule();
            } else {
                /*
                 * This appears completely un-orthodox for anything in D2D. But
                 * alas FFMP always does things differently. According to Vada
                 * Driesbach, FFMP in stand alone mode functions similarly to
                 * the way it does in table mode. Meaning you have to reach back
                 * and find time windows for the source displayed +- the
                 * expirationTime. None of the sources are displayed for exact
                 * times like everything else in D2D. This forces us to use the
                 * same Data Population methods the table uses. The only
                 * difference here is they are done for single sources.
                 */

                this.domains = monitor.getRunConfig().getDomains();
                SourceXML source = monitor.getSourceConfig().getSource(
                        sourceName);
                Date standAloneTime = null;

                if (source != null) {
                    // Special Loading for guidance sources, as mentioned in the
                    // comment
                    if (source.getDataType().equals(
                            SOURCE_TYPE.GUIDANCE.getSourceType())) {
                        long oldestTime = availableTimes[0].getRefTime()
                                .getTime();
                        long expirationTime = source
                                .getExpirationMinutes(siteKey)
                                * TimeUtil.MILLIS_PER_MINUTE;
                        standAloneTime = new Date(oldestTime - expirationTime);
                    } else {
                        // Only load current frames time
                        standAloneTime = availableTimes[availableTimes.length - 1]
                                .getRefTime();
                    }

                    NavigableMap<Date, List<String>> sourceURIs = getMonitor()
                            .getAvailableUris(siteKey, dataKey, sourceName,
                                    standAloneTime);
                    monitor.processUris(sourceURIs, siteKey, sourceName,
                            standAloneTime, SubMonitor.convert(null));

                }
            }
        }

        return new FFMPResource(this, loadProperties);
    }

    /**
     * @return the mapName
     */
    public String getMapName() {
        return mapName;
    }

    /**
     * @param mapName
     *            the mapName to set
     */
    public void setMapName(String mapName) {
        this.mapName = mapName;
    }

    /**
     * @return the shadingField
     */
    public String getShadingField() {
        return shadingField;
    }

    /**
     * @param shadingField
     *            the shadingField to set
     */
    public void setShadingField(String shadingField) {
        this.shadingField = shadingField;
    }

    /**
     * @return the tables
     */
    public String[] getTables() {
        return tables;
    }

    /**
     * @return the columns
     */
    public String[] getColumns() {
        return columns;
    }

    /**
     * @param columns
     *            the columns to set
     */
    public void setColumns(String[] columns) {
        this.columns = columns;
    }

    /**
     * @return the geomField
     */
    public String getGeomField() {
        return geomField;
    }

    /**
     * @param geomField
     *            the geomField to set
     */
    public void setGeomField(String geomField) {
        this.geomField = geomField;
    }

    /**
     * polyText
     * 
     * @return
     */
    public String getPolyText() {
        return polyText;
    }

    /**
     * Sets the polygon text
     * 
     * @param polyText
     */
    public void setPolyText(String polyText) {
        this.polyText = polyText;
    }

    /**
     * populate FFMP Record
     * 
     * @param record
     */
    public void populateRecord(FFMPRecord precord) throws VizException {
        try {
            getMonitor().populateFFMPRecord(siteKey, precord,
                    precord.getSourceName());
        } catch (Exception e) {
            throw new VizException("Failed to populate ffmp record "
                    + precord.getDataURI());
        }
    }

    /**
     * get the monitor instance
     * 
     * @return
     */
    public FFMPMonitor getMonitor() {
        if (monitor == null) {
            monitor = FFMPMonitor.getInstance();
        }
        return monitor;

    }

    /**
     * gets the product if available
     * 
     * @param sourceName
     * @return
     */
    public ProductXML getProduct() {
        return product;
    }

    /**
     * Set the product
     * 
     * @param product
     */
    public void setProduct(ProductXML product) {
        this.product = product;
    }

    public void setDomains(ArrayList<DomainXML> domains) {
        this.domains = domains;
    }

    public List<DomainXML> getDomains() {
        return domains;
    }

    /**
     * Set the FIELD
     * 
     * @param sfield
     */
    public void setField(String sourceName) {
        String sfield = monitor.getSourceConfig().getSourceType(sourceName)
                .getSourceType();
        if (sfield.equals(SOURCE_TYPE.QPE.getSourceType())) {
            field = FFMPRecord.FIELDS.QPE;
        } else if (sfield.equals(SOURCE_TYPE.RATE.getSourceType())) {
            field = FFMPRecord.FIELDS.RATE;
        } else if (sfield.equals(SOURCE_TYPE.QPF.getSourceType())) {
            field = FFMPRecord.FIELDS.QPF;
        } else if (sfield.equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
            field = FFMPRecord.FIELDS.GUIDANCE;
        }
    }

    /**
     * Get the field
     * 
     * @return
     */
    public FIELDS getField() {
        return field;
    }

    /**
     * Set the field
     * 
     * @param field
     */
    public void setField(FIELDS field) {
        this.field = field;
    }

    /**
     * Gets the prmiary sourceXML
     * 
     * @return
     */
    public SourceXML getPrimarySourceXML() {
        return getMonitor().getSourceConfig().getSource(sourceName);
    }

    public String getPrimarySource() {
        return sourceName;
    }

    /**
     * Gets the source from the product for display
     * 
     * @param pfield
     * @return
     */
    public List<String> getSourceName(FIELDS pfield) {
        ArrayList<String> sourceNames = null;
        ProductXML product = getProduct();
        ProductRunXML productRun = FFMPRunConfigurationManager.getInstance()
                .getProduct(siteKey);
        FfmpTableConfig ffmpTableConfig = FfmpTableConfig.getInstance();

        if (product != null) {
            sourceNames = new ArrayList<String>();
            switch (pfield) {
            case QPE:
                sourceNames.add(product.getQpe());
                break;
            case RATE:
                sourceNames.add(product.getRate());
                break;
            case QPF:
                String qpfType = ffmpTableConfig.getTableConfigData(siteKey)
                        .getQpfType();
                sourceNames.add(productRun.getQpfSources(product, qpfType)
                        .get(0).getSourceName());
                break;
            case VIRTUAL:
                sourceNames.add(product.getVirtual());
                break;
            case GUIDANCE:
                String guidSrc = FFMPConfig.getInstance().getFFMPConfigData()
                        .getIncludedGuids();
                for (SourceXML guidance : productRun.getGuidanceSources(
                        product, guidSrc)) {
                    sourceNames.add(guidance.getSourceName());
                }
                break;
            }
        }

        return sourceNames;
    }

}
