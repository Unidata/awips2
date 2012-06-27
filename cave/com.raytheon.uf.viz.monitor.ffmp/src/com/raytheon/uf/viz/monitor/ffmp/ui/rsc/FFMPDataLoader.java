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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NavigableMap;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPLoadListener;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPLoaderEvent;

/**
 * Place holder more or less for a ResourceData Object This dosen't do anything
 * currently.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 Feb, 2011   7587    dhladky     Initial creation
 * 25 Jan, 2012   DR13839 gzhang	  Handle Uris and Huc processing
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class FFMPDataLoader extends Thread {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDataLoader.class);

    private String sharePath = null;

    private ProductXML product = null;

    private FFMPRunXML runner = null;

    private Date timeBack = null;

    private Date mostRecentTime = null;

    public boolean isDone = false;

    public LOADER_TYPE loadType = null;

    private String siteKey = null;

    private String dataKey = null;

    private ArrayList<String> hucsToLoad = null;

    private String wfo = null;

    private FFMPResourceData resourceData = null;

    private FFMPConfig config = null;

    private ArrayList<FFMPLoadListener> loadListeners = new ArrayList<FFMPLoadListener>();

    public FFMPDataLoader(FFMPResourceData resourceData, Date timeBack,
            Date mostRecentTime, LOADER_TYPE loadType,
            ArrayList<String> hucsToLoad) {

        sharePath = AppsDefaults.getInstance().getToken("apps_dir")
                + File.separator + "ffmp" + File.separator;
        //sharePath = "/awips2/edex/data/share/hydroapps/ffmp/";
        
        this.product = resourceData.getProduct();
        this.siteKey = resourceData.siteKey;
        this.dataKey = resourceData.dataKey;
        this.timeBack = timeBack;
        this.mostRecentTime = mostRecentTime;
        this.loadType = loadType;
        this.hucsToLoad = hucsToLoad;
        this.wfo = resourceData.wfo;
        this.resourceData = resourceData;
        this.runner = FFMPRunConfigurationManager.getInstance().getRunner(wfo);
        this.config = FFMPConfig.getInstance();

        if ((loadType == LOADER_TYPE.INITIAL)
                || (loadType == LOADER_TYPE.GENERAL)) {
            this.setPriority(MAX_PRIORITY);
        } else {
            this.setPriority(MIN_PRIORITY);
        }
    }

    /**
     * Add listener
     * 
     * @param fl
     */
    public synchronized void addListener(FFMPLoadListener fl) {
        loadListeners.add(fl);
    }

    /**
     * Remove listener
     * 
     * @param fl
     */
    public synchronized void removeListener(FFMPLoadListener fl) {
        loadListeners.remove(fl);
    }

    // kills the loader
    public void kill() {
        isDone = true;
    }

    @Override
    public void run() {
        long time = System.currentTimeMillis();

        try {
        	resourceData.setLoader(loadType);
        	System.out.println("Starting Loader: "+loadType.getLoaderType());
        	
            ProductRunXML productRun = runner.getProduct(siteKey);
            ArrayList<String> qpfSources = new ArrayList<String>();

            boolean isProductLoad = true;
            String rateURI = null;

            if ((loadType == LOADER_TYPE.INITIAL)
                    || (loadType == LOADER_TYPE.GENERAL)) {
                rateURI = getMonitor().getAvailableUri(siteKey, dataKey,
                        product.getRate(), mostRecentTime);
            } 
        
            NavigableMap<Date, List<String>> qpeURIs = getMonitor()
                    .getAvailableUris(siteKey, dataKey, product.getQpe(),
                            timeBack);

            ArrayList<NavigableMap<Date, List<String>>> qpfs = new ArrayList<NavigableMap<Date, List<String>>>();

            for (String qpfType : productRun.getQpfTypes(product)) {
                for (SourceXML qpfSource : productRun.getQpfSources(product,
                        qpfType)) {

                    NavigableMap<Date, List<String>> qpfURIs = null;
                    Date qpfTime = timeBack;

                    if (loadType == LOADER_TYPE.GENERAL) {
                        qpfTime = getMonitor().getPreviousQueryTime(siteKey,
                                qpfSource.getSourceName());
                    }

                    qpfURIs = getMonitor().getAvailableUris(siteKey, dataKey,
                            qpfSource.getSourceName(), qpfTime);

                    if (qpfURIs != null && !qpfURIs.isEmpty()) {
                        qpfs.add(qpfURIs);
                        qpfSources.add(qpfSource.getSourceName());
                    }
                }
            }

            NavigableMap<Date, List<String>> virtualURIs = getMonitor()
                    .getAvailableUris(siteKey, dataKey, product.getVirtual(),
                            timeBack);

            HashMap<String, NavigableMap<Date, List<String>>> guids = new HashMap<String, NavigableMap<Date, List<String>>>();

            for (String type : productRun.getGuidanceTypes(product)) {
                for (SourceXML guidSource : productRun.getGuidanceSources(
                        product, type)) {

                    NavigableMap<Date, List<String>> iguidURIs = null;
                    Date guidTime = timeBack;

                    if (loadType == LOADER_TYPE.GENERAL) {
                        guidTime = getMonitor().getPreviousQueryTime(siteKey,
                                guidSource.getSourceName());
                    }

                    iguidURIs = getMonitor().getAvailableUris(siteKey, dataKey,
                            guidSource.getSourceName(), guidTime);

                    if (iguidURIs != null && !iguidURIs.isEmpty()) {
                        guids.put(guidSource.getSourceName(), iguidURIs);
                    }
                }
            }
            // We only load all for long range data, all + layer for medium range
            if (loadType == LOADER_TYPE.TERTIARY) {
            	hucsToLoad.clear();
            	hucsToLoad.add("ALL");
            } 

            for (String phuc : hucsToLoad) {

                if (isDone) {
                    return;
                }

                if (phuc.equals("VIRTUAL")) {
                    // we don't do the virtuals like this
                    continue;
                } else {
                    // rate
                    if (rateURI != null) {
                    	fireLoaderEvent(loadType, "Processing "+product.getRate() + "/" + phuc,
                                isDone);
                    	
                        getMonitor().processUri(isProductLoad, rateURI,
                                siteKey, product.getRate(), timeBack, phuc);
                        fireLoaderEvent(loadType, product.getRate() + "/"
                                + phuc, isDone);
                    }

                    // qpes
                    fireLoaderEvent(loadType, "Processing "+product.getQpe() + "/" + phuc,
                            isDone);
                    FFMPBasinData qpeData = null;

                    if ((loadType == LOADER_TYPE.INITIAL)
                            || (loadType == LOADER_TYPE.SECONDARY)) {

                        SourceXML source = getMonitor().getSourceConfig()
                                .getSource(product.getQpe());

                        qpeData = readLoaderBuddyFile(source, phuc, dataKey,
                                wfo);

                        if (qpeData != null) {

                            getMonitor().insertFFMPData(qpeData, siteKey,
                                    product.getQpe(), phuc);
                        }
                    } 

                    if (!qpeURIs.isEmpty() && qpeData == null) {
                        if (phuc.equals(config.getFFMPConfigData().getLayer()) || phuc.equals("ALL")) {
                            getMonitor().processUris(qpeURIs, isProductLoad,
                                    siteKey, product.getQpe(), timeBack, phuc,
                                    loadType);
                        }
                    }

                    fireLoaderEvent(loadType, product.getQpe() + "/" + phuc,
                            isDone);

                    int i = 0;
                    for (NavigableMap<Date, List<String>> qpfURIs : qpfs) {
                        // qpf
                        fireLoaderEvent(loadType, "Processing "+product.getQpf(i) + "/" + phuc,
                                isDone);
                        FFMPBasinData qpfData = null;
                        if ((loadType == LOADER_TYPE.INITIAL)
                                || (loadType == LOADER_TYPE.SECONDARY)) {

                            SourceXML source = getMonitor().getSourceConfig()
                                    .getSource(qpfSources.get(i));

                            String pdataKey = findQPFHomeDataKey(source);
                            qpfData = readLoaderBuddyFile(source, phuc,
                                    pdataKey, wfo);

                            if (qpfData != null) {

                                if ((phuc.equals(config.getFFMPConfigData()
                                        .getLayer()) || phuc.equals("ALL"))
                                        && loadType == LOADER_TYPE.INITIAL
                                        && source.getSourceName().equals(
                                                config.getFFMPConfigData()
                                                        .getIncludedQPF())) {
                                    if (!qpfURIs.isEmpty()) {

                                        getMonitor().processUris(qpfURIs,
                                                isProductLoad, siteKey,
                                                source.getSourceName(),
                                                timeBack, phuc, loadType);
                                    }
                                }

                                getMonitor().insertFFMPData(qpfData, siteKey,
                                        source.getSourceName(), phuc);
                            }
                        }
                        //if (isUrisProcessNeeded(qpfData,qpfURIs)) {/*DR13839*/
                        if ((qpfData == null) && !qpfURIs.isEmpty()) {
                            if (phuc.equals(config.getFFMPConfigData()
                                    .getLayer()) || phuc.equals("ALL")) { //old code: keep for reference*/
                        	//if (isHucProcessNeeded(phuc)) {/*DR13839*/
                                getMonitor().processUris(qpfURIs,
                                        isProductLoad, siteKey,
                                        product.getQpf(i), timeBack, phuc,
                                        loadType);
                            }
                        }

                        fireLoaderEvent(loadType, product.getQpf(i) + "/"
                                + phuc, isDone);

                        i++;
                    }

                }
                // virtuals only have data for ALL
                if (phuc.equals("ALL")) {
                    fireLoaderEvent(loadType, "Processing "+product.getVirtual() + "/" + phuc,
                            isDone);
                    FFMPBasinData vgbData = null;

                    if ((loadType == LOADER_TYPE.INITIAL)
                            || (loadType == LOADER_TYPE.SECONDARY)) {

                        SourceXML source = getMonitor().getSourceConfig()
                                .getSource(product.getVirtual());

                        vgbData = readLoaderBuddyFile(source, phuc, dataKey,
                                wfo);

                        if (vgbData != null) {

                            getMonitor().insertFFMPData(vgbData, siteKey,
                                    product.getVirtual(), phuc);
                        }
                    }

                    if ((vgbData == null) && !virtualURIs.isEmpty()) {
                        getMonitor().processUris(virtualURIs, isProductLoad,
                                siteKey, product.getVirtual(), timeBack, phuc,
                                loadType);
                    }

                    fireLoaderEvent(loadType,
                            product.getVirtual() + "/" + phuc, isDone);
                }

                // process guidance all at once
                for (String type : productRun.getGuidanceTypes(product)) {
                
                    ArrayList<SourceXML> guidSources = productRun
                            .getGuidanceSources(product, type);
                    for (SourceXML guidSource : guidSources) {
                    	
                        NavigableMap<Date, List<String>> iguidURIs = guids
                                .get(guidSource.getSourceName());
                        
                        fireLoaderEvent(loadType, "Processing "+guidSource.getSourceName() + "/" + phuc,
                                isDone);

                        getMonitor().processUris(iguidURIs, isProductLoad,
                                siteKey, guidSource.getSourceName(), timeBack,
                                phuc, loadType);

                        fireLoaderEvent(loadType, guidSource.getSourceName()
                                + "/" + phuc, isDone);

                    }
                }

                fireLoaderEvent(loadType, phuc + " Load complete", isDone);
            }
        } catch (Exception e) {
            isDone = true;
            System.err.println("FFMP Data Loader terminated...."
                    + e.getMessage());
        } finally {
            isDone = true;
        }

        String message = null;
        if (loadType == LOADER_TYPE.INITIAL) {
            message = "Finished Initial Load";
        } else {
            message = "Finished General Data Load";
        }

        long endTime = (System.currentTimeMillis()) - time;
        System.out.println("Loader took: " + endTime / 1000 + " seconds");

        fireLoaderEvent(loadType, message, isDone);
    }

    /**
     * Fire loader updates to the front end displays
     * 
     * @param FFMPLoaderStatus
     **/
    public void fireLoaderEvent(LOADER_TYPE ltype, String lmessage,
            boolean lstatus) {

        final FFMPLoaderStatus sstatus = new FFMPLoaderStatus(ltype, lmessage,
                lstatus);

        VizApp.runAsync(new Runnable() {
            public void run() {
                FFMPLoaderEvent fle = new FFMPLoaderEvent(sstatus);
                Iterator<FFMPLoadListener> iter = loadListeners.iterator();

                while (iter.hasNext()) {
                    FFMPLoadListener listener = iter.next();
                    listener.loadStatus(fle);
                }
            }
        });
    }

    private FFMPMonitor getMonitor() {
        if (FFMPMonitor.isRunning()) {
            // System.out.println("Monitor is running...");
            return FFMPMonitor.getInstance();
        } else {
            // System.out.println("Monitor is dead...");
            isDone = true;
            return null;
        }
    }

    public enum LOADER_TYPE {

        INITIAL("Initial"), GENERAL("General"), SECONDARY("Secondary"), TERTIARY(
                "Tertiary");

        private final String loaderType;

        private LOADER_TYPE(String name) {
            loaderType = name;
        }

        public String getLoaderType() {
            return loaderType;
        }
    };

    /**
     * Loads the loader buddy files
     * 
     * @param sourceName
     * @param huc
     * @param wfo
     * @return
     */
    private FFMPBasinData readLoaderBuddyFile(SourceXML source, String huc,
            String pdataKey, String wfo) {

        String sourceName = source.getSourceName();
        File file = new File(sharePath + wfo + File.separator + sourceName
                + "-" + siteKey + "-" + pdataKey + "-" + huc + ".bin");
        System.out.println("Buddy File path: " + file.getAbsolutePath());

		FFMPBasinData basinData = null;

		if (file.exists()) {
			
			System.out.println("Last mod: " + new Date(file.lastModified()));
			//System.out.println("6 hour mod: " + new Date((System.currentTimeMillis() - (6 * 1000 * 3600))));
			//System.out.println("DIFF: "+(file.lastModified() - (System.currentTimeMillis() - (6 * 1000 * 3600))));

			if (file.lastModified() > (System.currentTimeMillis() - (6 * 1000 * 3600))) {
				
				System.out.println("Buddy File path: " + file.getName());
				
				try {
					basinData = (FFMPBasinData) SerializationUtil
							.transformFromThrift(FileUtil.file2bytes(file,
									false));
				} catch (SerializationException e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}

		return basinData;

	}

    /**
     * Finds the home datakey identifier for QPF sources
     * 
     * @param source
     * @return
     */
    private String findQPFHomeDataKey(SourceXML source) {

        FFMPRunConfigurationManager runManager = FFMPRunConfigurationManager
                .getInstance();

        for (ProductRunXML product : runManager.getProducts()) {
            File file = new File(sharePath + wfo + File.separator
                    + source.getSourceName() + "-" + siteKey + "-"
                    + product.getProductKey() + "-ALL.bin");

            if (file.exists()) {
                return product.getProductKey();
            }
        }

        return siteKey;

    }
    
}