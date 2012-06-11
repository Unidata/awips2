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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataContainer;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.dao.FFMPDao;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.FFTISourceXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;

/**
 * FFTIProcessor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 01, 2011            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFTIProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFTIProcessor.class);

    private FFTISourceXML fftiSource = null;

    private FFMPRecord ffmpRec = null;

    private FFMPGenerator ffmpgen = null;

    //private SourceXML source = null;

    private String wfo = null;

    private Date barrierTime = null;

    private FFMPDataContainer sourceContainer = null;

    /** Pattern for dates in radar */
    public static String datePattern = "yyyy-MM-dd HH:mm:ss";

    /**
     * useful constructor
     * 
     * @param fdm
     * @param ffmpgen
     * @param ffmpRec
     * @param source
     */
    public FFTIProcessor(FFMPGenerator ffmpgen, FFMPRecord ffmpRec,
            FFTISourceXML fftiSource) {

        this.ffmpgen = ffmpgen;
        this.ffmpRec = ffmpRec;
        this.fftiSource = fftiSource;
        this.wfo = ffmpRec.getWfo();
        long curr = ffmpRec.getDataTime().getRefTime().getTime();
        long fftiBarrier = (long) (fftiSource.getDurationHour() * 60.0 * 60.0 * 1000);
        this.barrierTime = new Date(curr - fftiBarrier);
    }

    /**
     * Process FFTI for this source
     */
    public void processFFTI() {
		ArrayList<String> dispNameList = fftiSource.getDisplayNameList();
		for (String sourceNameString : dispNameList) {
		
			String iSiteKey = ffmpRec.getSiteKey();
			String sourceString = sourceNameString;
			
			String[] parts = sourceNameString.split("-");
			SourceXML source = null;

			if (parts.length > 1) {
				iSiteKey = parts[0];
				String sourceName = parts[1];
				source = FFMPSourceConfigurationManager.getInstance()
						.getSource(sourceName);
			} else {
				// mosaic source
				source = FFMPSourceConfigurationManager.getInstance()
						.getSourceByDisplayName(sourceString);
			}

			if (!source.getSourceType().equals(
					FFMPSourceConfigurationManager.SOURCE_TYPE.GUIDANCE
							.getSourceType())
					&& !source.isMosaic()) {
				sourceString = source.getDisplayName() + "-" + iSiteKey + "-"
						+ iSiteKey;
			} else {
				sourceString = source.getDisplayName();
			}

			sourceContainer = ffmpgen.getFFMPDataContainer(sourceString);

			// we attempt to reload sourecs
			// this is done to keep all of the clustered
			// FFMP's in sync. otherwise one JVM would never
			// be updated with what the other had processed.
			if (sourceContainer == null) {
				// first time being read
				// check back this far for an existing file
				boolean reload = false;

				if (source.getSourceName().equals(ffmpRec.getSourceName())
						&& iSiteKey.equals(ffmpRec.getSiteKey())
						|| source
								.getSourceType()
								.equals(FFMPSourceConfigurationManager.SOURCE_TYPE.GUIDANCE
										.getSourceType())) {
					reload = true;
				} else {
					if (ffmpgen.checkBuddyFile(source.getSourceName() + "-"
							+ iSiteKey + "-" + iSiteKey, "ALl", sourceString,
							barrierTime)) {
						reload = true;
					}
				}

				if (reload) {
					sourceContainer = new FFMPDataContainer(sourceString);
					ffmpgen.getFFMPData().put(sourceString, sourceContainer);
				}
			}

			if (sourceContainer != null) {
				if (source.getSourceType().equals(
						SOURCE_TYPE.GUIDANCE.getSourceType())) {

					String primarySource = ffmpgen.fscm
							.getPrimarySource(source);
					ProductXML product = ffmpgen.fscm.getProduct(primarySource);
					Date ffgBackDate = new Date(ffmpRec.getDataTime()
							.getRefTime().getTime()
							- (3600 * 1000 * 12));

					// try to load any missing one's, other than the new one
					for (SourceXML guidSource : product
							.getGuidanceSourcesByType(source.getDisplayName())) {
						if (!sourceContainer.containsKey(guidSource
								.getSourceName())
								&& !source.getSourceName().equals(
										guidSource.getSourceName())) {
							sourceContainer = populateDataContainer(
									sourceContainer, ffmpgen.template, null,
									ffgBackDate, ffmpRec.getDataTime()
											.getRefTime(), wfo, source,
									iSiteKey);
						}
					}

				} else {

					Date newDate = sourceContainer.getNewest();
					Date oldDate = sourceContainer.getOldest();

					if (newDate != null && oldDate != null) {
						if ((ffmpRec.getDataTime().getRefTime().getTime() - newDate
								.getTime()) >= (source
								.getExpirationMinutes(iSiteKey) * 60 * 1000)) {
							// force a re-query back to the newest time in
							// existing source container
							sourceContainer = populateDataContainer(
									sourceContainer, ffmpgen.template, null,
									newDate,
									ffmpRec.getDataTime().getRefTime(), wfo,
									source, iSiteKey);
						} else if (oldDate
								.after(new Date(
										barrierTime.getTime()
												- (source
														.getExpirationMinutes(iSiteKey) * 60 * 1000)))) {
							// force a re-query back to barrierTime for
							// existing source container
							sourceContainer = populateDataContainer(
									sourceContainer, ffmpgen.template, null,
									barrierTime, oldDate, wfo, source, iSiteKey);
						}
					} else {
						// COMPLETELY EMPTY SOURCE CONTAINER
						// force a re-query back to barrierTime from current
						// refTime
						sourceContainer = populateDataContainer(
								sourceContainer, ffmpgen.template, null,
								barrierTime,
								ffmpRec.getDataTime().getRefTime(), wfo,
								source, iSiteKey);
					}
				}

				purge(barrierTime, sourceContainer);
			}
		}

	}

	/**
	 * Populates the FFTI Data back to the determined date
	 * 
	 * @param sourceContainer
	 * @param template
	 * @param startDate
	 * @param endDate
	 * @param wfo
	 * @param source
	 * @return
	 */
	public static FFMPDataContainer populateDataContainer(
			FFMPDataContainer sourceContainer, FFMPTemplates template,
			Set<String> hucs, Date startDate, Date endDate, String wfo,
			SourceXML source, String siteKey) {

		ArrayList<String> uris = getUris(startDate, endDate, wfo, source,
				siteKey);
		// System.out.println("Number of Records querried: " + siteKey + " : "
		// + uris.size());

		for (String uri : uris) {

			FFMPRecord rec = new FFMPRecord(uri);

			boolean contains = false;

			if (source.getSourceType().equals(
					SOURCE_TYPE.GUIDANCE.getSourceType())) {
				contains = sourceContainer.containsKey(source.getSourceName());
				// System.out.println("Processing FFG source!!!!!"
				// + source.getSourceName());
			} else {
				contains = sourceContainer.containsKey(rec.getDataTime()
						.getRefTime());
			}

			if (!contains) {
				try {
					if (hucs == null) {
						HashMap<String, String> myHucs = new HashMap<String, String>();
						myHucs.put("ALL", "ALL");
						hucs = myHucs.keySet();
					}

					for (String huc : hucs) {

						rec = populateRecord(rec, huc, template);
						FFMPBasinData newData = rec.getBasinData(huc);
						sourceContainer.addFFMPEntry(rec.getDataTime()
								.getRefTime(), source, newData, huc, siteKey);

					}

					// System.out.println("Adding Time: "
					// + rec.getDataTime().getRefTime());

				} catch (PluginException e) {
					e.printStackTrace();
					statusHandler.handle(Priority.ERROR,
							"Source: " + source.getDisplayName() + "  domain: "
									+ wfo
									+ " : failed to retrieve FFMP/FFTI Data ");
				}
			}
		}

		return sourceContainer;
	}

	/**
	 * Get the uris for this FFTI source
	 * 
	 * @param startDate
	 * @param endDate
	 * @param wfo
	 * @param source
	 * @return
	 */
	public static ArrayList<String> getUris(Date startDate, Date endDate,
			String wfo, SourceXML source, String siteKey) {

		SimpleDateFormat datef = new SimpleDateFormat(datePattern);
		datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
		StringBuilder query = new StringBuilder(200);

		query.append("select datauri from ffmp where wfo = '");
		query.append(wfo);
		query.append("' and sourcename = '");
		query.append(source.getSourceName());
		query.append("' and sitekey = '");
		query.append(siteKey);
		if (!source.isMosaic()) {
			query.append("' and datakey = '");
			query.append(siteKey);
		}
		query.append("' and reftime >= '");
		query.append(datef.format(startDate));
		query.append("' and reftime < '");
		query.append(datef.format(endDate));

		query.append("' order by reftime desc");
		// System.out.println("URI query: " + query.toString());

		ArrayList<String> uris = new ArrayList<String>();

		try {
			CoreDao dao = new CoreDao(DaoConfig.forDatabase(FFMPUtils.META_DB));
			Object[] results = dao.executeSQLQuery(query.toString());

			if (results.length > 0) {
				for (int i = 0; i < results.length; i++) {
					Object result = results[i];
					if (result != null) {
						/*
						 * System.out.println("Adding URI to FFTI list: " +
						 * (String) result);
						 */
						uris.add((String) result);
					}
				}
			}

		} catch (Exception e) {
			statusHandler.handle(Priority.ERROR,
					"Source: " + source.getSourceName() + " domain: " + wfo
							+ " : failed to query");
		}

        return uris;
    }

    /**
     * Get "All" basin container populated FFMPRecord
     * 
     * @param rec
     * @param template
     * @return
     * @throws PluginException
     */
    public static FFMPRecord populateRecord(FFMPRecord rec, String huc,
            FFMPTemplates template) throws PluginException {

        try {
            // file not populated, skip it
            if (rec.getPluginName() == null) {
                return rec;
            }

            FFMPDao dao = (FFMPDao) PluginFactory.getInstance().getPluginDao(
                    rec.getPluginName());
            rec = (FFMPRecord) dao.getMetadata(rec.getDataURI());
            IDataStore dataStore = dao.getDataStore(rec);

            rec.retrieveMapFromDataStore(dataStore, rec.getDataURI(), template,
                    huc, rec.getDataTime().getRefTime(), rec.getSourceName());

            // System.out.println("Size of huc: "
            // + rec.getBasinData(huc).getBasins().size());
        } catch (Exception se) {
            statusHandler.handle(Priority.ERROR,
                    "Source: " + rec.getSourceName() + " sitekey: "
                            + " domain: " + rec.getWfo()
                            + " : failed to populate records in FFMP/FFTI");
        }

        return rec;
    }

    /**
     * Rid us of old entries
     * 
     * @param barrierTime
     * @param sourceContainer
     */
    private void purge(Date barrierTime, FFMPDataContainer sourceContainer) {
        for (String huc : sourceContainer.getKeys()) {
            sourceContainer.getBasinData(huc).purgeData(barrierTime);
        }
    }
}
