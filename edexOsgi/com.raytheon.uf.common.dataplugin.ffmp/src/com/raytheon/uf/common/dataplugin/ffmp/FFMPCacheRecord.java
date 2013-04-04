package com.raytheon.uf.common.dataplugin.ffmp;
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

import java.awt.Point;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import com.raytheon.uf.common.cache.CacheException;
import com.raytheon.uf.common.cache.CacheFactory;
import com.raytheon.uf.common.cache.ICache;
import com.raytheon.uf.common.cache.disk.DiskCache;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Cache coherent record
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/24/12    632     D. Hladky   Initial release
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPCacheRecord extends FFMPRecord {

	private final String sourceCacheName;

	private final String cacheName;
	
	private final String cacheDir;

	private ArrayList<String> hucs = new ArrayList<String>();

	//private static final boolean useCache = !Boolean
	//		.getBoolean("com.raytheon.uf.common.ffmp.disableCache");
	private static final boolean useCache = false;
	
	private static final transient IUFStatusHandler statusHandler = UFStatus
     .getHandler(FFMPCacheRecord.class);


	/**
	 * Public constructor
	 * @param rec
	 * @param sourceCacheName
	 * @param cacheDir
	 */
	public FFMPCacheRecord(FFMPRecord rec, String sourceCacheName, String cacheDir) {

		this.setSiteKey(rec.getSiteKey());
		this.setWfo(rec.getWfo());
		this.setDataKey(rec.getDataKey());
		this.setSourceName(rec.getSourceName());
		this.setPluginName(rec.getPluginName());
		this.sourceCacheName = sourceCacheName;
		this.cacheName = "FFMP-" + getWfo() + "-" + getSiteKey() + "-" +getDataKey()+ "-"
				+ getSourceCacheName();
		// set a default value
		if (cacheDir == null) {
			cacheDir = "/tmp";
		}
		this.cacheDir = cacheDir;
	}

	public String getSourceCacheName() {
		return sourceCacheName;
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@SuppressWarnings({ "unchecked" })
	private DiskCache<FFMPBasinData> getCache() {

		DiskCache<FFMPBasinData> diskCache = null;
		CacheFactory cf = CacheFactory.getInstance();
		try {
			diskCache = (DiskCache<FFMPBasinData>) cf.getCache(this.cacheName);
		} catch (CacheException e) {
			synchronized (this) {
				// make sure not done on another thread
				try {
					diskCache = (DiskCache<FFMPBasinData>) cf
							.getCache(this.cacheName);
				} catch (CacheException e1) {
					diskCache = createCache(this.cacheName);
					CacheFactory.getInstance().addCache(this.cacheName,
							diskCache);
				}
			}
		}

		return diskCache;
	}
	/**
	 * Get BasinData Map from cache
	 * 
	 * @param siteKey
	 * @param sourceName
	 * @return
	 */
	@Override
	public FFMPBasinData getBasinData(String hucName) {
		return getBasinData(hucName, false);
	}
		
	/**
	 * Get BasinData Map from cache
	 * 
	 * @param siteKey
	 * @param sourceName
	 * @return
	 */
	public FFMPBasinData getBasinData(String hucName, boolean lock) {
		FFMPBasinData basins = null;

		if (hucName != null) {
			if (useCache) {
				try {

					DiskCache<FFMPBasinData> diskCache = getCache();
					basins = (FFMPBasinData) diskCache.getFromCache(hucName, lock);

					if (basins == null) {
						basins = new FFMPBasinData(hucName);
						if (!hucs.contains(hucName)) {
							hucs.add(hucName);
						}
					}

				} catch (Exception e) {
					e.printStackTrace();
				}
			
			} else {
				basins = super.getBasinData(hucName);
			}
		}

		return basins;
	}


	/**
	 * Create cache objects if needed
	 * 
	 * @param siteKey
	 * @return
	 */
	private DiskCache<FFMPBasinData> createCache(String name) {
		ICache<FFMPBasinData> cache = new DiskCache<FFMPBasinData>();
		DiskCache<FFMPBasinData> dc = (DiskCache<FFMPBasinData>) cache;
		dc.setName(name);
		dc.setBaseCacheDir(getCacheDir());
		dc.setSizeMemCacheMap(2); // For FFMP hold two generally COUNTY and ALL
		dc.activateCache();

		return dc;
	}

	/**
	 * Set source record to cache
	 * 
	 * @param siteKey
	 * @param sourceName
	 * @param record
	 */
	@Override
	public void setBasinData(FFMPBasinData basins, String hucName) {
		if (hucName != null) {
			if (useCache) {
				try {
					synchronized (basins) {
						DiskCache<FFMPBasinData> diskCache = getCache();

						try {
							diskCache.addToCache(hucName, basins);
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			} else {
				super.setBasinData(basins, hucName);
			}
		}
	}

	/**
	 * Cache File reader
	 * 
	 * @param basins
	 * @param hucName
	 */
	public void setCacheData(FFMPBasinData basins, String hucName) {
		if (getBasinData(hucName) != null) {
			
			basins = getBasinData(hucName, true);
			//System.out.println("Adding Cache Data: "+hucName+" "+getSourceName());
			
			synchronized (basins) {
				for (Entry<Long, FFMPBasin> entry : basins.getBasins()
						.entrySet()) {
					FFMPBasin basin = basins.get(entry.getKey());
					if (basin != null) {
						if (basin instanceof FFMPGuidanceBasin) {
							FFMPGuidanceBasin gbasin = (FFMPGuidanceBasin) basin;
							gbasin.getGuidValues().putAll(
									((FFMPGuidanceBasin) entry.getValue())
											.getGuidValues());
						} else {
							basin.getValues().putAll(
									entry.getValue().getValues());
						}
					} else {
						basins.put(entry.getKey(), entry.getValue());
					}
				}
			}

			setBasinData(basins, hucName);
			
		} else {
			setBasinData(basins, hucName);
			//System.out.println("Adding Whole Object Buddy Data: "+hucName+" "+getSourceName());
		}
	}
	
	/**
     * Gets the Hash out of the datastore by HUC
     * 
     * @param dataStore
     * @param huc
     */
    public void retrieveMapFromDataStore(IDataStore dataStore, String uri,
            FFMPTemplates template, String huc, Date date, String sourceName)
            throws Exception {
    	
        FFMPBasinData fbd = null;
       
        boolean aggregate = true;

        if (huc.equals("ALL")) {
            aggregate = false;
        }

        fbd = getBasinData(huc, true);
        String key = getSiteKey();

        synchronized (template) {

            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(sourceName);

            for (DomainXML domain : template.getDomains()) {
                LinkedHashMap<Long, ?> map = template.getMap(key,
                        domain.getCwa(), huc);

                if (map != null && map.keySet().size() > 0) {

                    IDataRecord rec = null;

                    try {
                        rec = dataStore.retrieve(uri + "/" + domain.getCwa(),
                                huc, Request.ALL);
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "FFMPRecord: no data record for: " + uri + "/"
                                        + domain.getCwa());
                    }

                    if (rec != null) {
                        float[] values = ((FloatDataRecord) rec).getFloatData();

                        int j = 0;
                        if (values != null) {
                            // System.err.println(sourceName);
                            if (source.getSourceType().equals(
                                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                                for (Long pfaf : map.keySet()) {
                                    try {
                                        FFMPGuidanceBasin basin = (FFMPGuidanceBasin) fbd
                                                .get(pfaf);

                                        if (basin == null) {
                                            basin = new FFMPGuidanceBasin(pfaf,
                                                    aggregate);
                                            fbd.put(pfaf, basin);
                                        }

                                        if (basin.containsKey(date, sourceName)) {
                                            if (basin
                                                    .getValue(date, sourceName) == FFMPUtils.MISSING
                                                    || basin.getValue(date,
                                                            sourceName).isNaN()) {

                                                float curval = basin.getValue(
                                                        date, sourceName);

                                                if (curval >= 0.0f
                                                        && values[j] >= 0.0f) {
                                                    basin.setValue(sourceName,
                                                            date, (curval + values[j])/ 2);
                                                } else {
                                                    basin.setValue(sourceName,
                                                            date, values[j]);
                                                }

                                            }
                                        } else {
                                            basin.setValue(sourceName, date,
                                                    values[j]);
                                        }

                                        j++;
                                    } catch (Exception e) {
                                        break;
                                    }

                                }
                            } else {
                                for (Long pfaf : map.keySet()) {
                                    try {
                                        FFMPBasin basin = fbd.get(pfaf);
                                        if (basin == null) {
                                            basin = new FFMPBasin(pfaf,
                                                    aggregate);
                                            fbd.put(pfaf, basin);
                                        }

                                        if (basin.contains(date)) {
                                            float curval = basin.getValue(date);
                                            if (curval >= 0.0f
                                                    && values[j] >= 0.0f) {
                                                basin.setValue(date, (curval + values[j]) / 2);
                                            } else {
                                                basin.setValue(date, values[j]);
                                            }
                                        } else {
                                            basin.setValue(date, values[j]);
                                        }
                                        j++;
                                    } catch (Exception e) {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        setBasinData(fbd, huc);
    }
    
    /**
     * Gets a single basin out of the dataStore
     * 
     * @param dataStore
     * @param huc
     */
    public void retrieveBasinFromDataStore(IDataStore dataStore, String uri,
            FFMPTemplates template, String huc, Date date, String sourceName,
            FFMPBasin basin) {
    	
    	FFMPBasinData fbd = null;
        
    	try {

            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(sourceName);
            Long pfaf = basin.getPfaf();
            fbd = getBasinData("ALL", true);

            synchronized (template) {

                for (DomainXML domain : template.getDomains()) {

                    LinkedHashMap<Long, ?> map = template.getMap(getSiteKey(),
                            domain.getCwa(), huc);

                    if (map != null && map.get(pfaf) != null) {

                        int index = 0;
                        for (Long pfafToCheck : map.keySet()) {
                            if (pfafToCheck.equals(pfaf)) {
                                break;
                            }
                            index++;
                        }

                        try {
                            IDataRecord rec = dataStore.retrieve(uri + "/"
                                    + domain.getCwa(), huc, Request
                                    .buildPointRequest(new Point(index, 0)));

                            if (rec != null) {
                                float[] values = ((FloatDataRecord) rec)
                                        .getFloatData();

                                boolean isFFG = false;

                                if (source.getSourceType().equals(
                                        SOURCE_TYPE.GUIDANCE.getSourceType())) {
                                    isFFG = true;
                                }

                                if (values != null) {
                                    // System.err.println(sourceName);
                                    if (isFFG) {
                                        ((FFMPGuidanceBasin) basin).setValue(
                                                sourceName, date, values[0]);
                                    } else {
                                        basin.setValue(date, values[0]);
                                    }
                                }
                            }
                        } catch (Throwable e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "ERROR Retrieving Map for URI: " + uri
                                            + "..." + huc, e);
                        }
                    }
                }
            }
            
            setBasinData(fbd, "ALL");

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "ERROR Retrieving HUC..."
                    + huc);
        }
    }

    
    /**
     * Gets the Virtual Hash out of the datastore by HUC
     * 
     * @param dataStore
     * @param huc
     */
    public void retrieveVirtualBasinFromDataStore(IDataStore dataStore,
            String uri, FFMPTemplates template, Date date, FFMPBasin basin) {
        FFMPBasinData fbd = null;
        try {
            boolean aggregate = false;
            fbd = getBasinData("ALL", true);
            String key = getDataKey();

            for (DomainXML domain : template.getDomains()) {

                LinkedHashMap<String, FFMPVirtualGageBasinMetaData> lids = template
                        .getVirtualGageBasins(key, domain.getCwa());
                int size = lids.size();

                if (size > 0) {
                    try {
                        IDataRecord rec = dataStore
                                .retrieve(uri + "/" + domain.getCwa(), "ALL",
                                        Request.ALL);

                        if (rec != null) {
                            float[] values = ((FloatDataRecord) rec)
                                    .getFloatData();
                            if (values != null) {
                                int j = 0;

                                for (Entry<String, FFMPVirtualGageBasinMetaData> entry : lids
                                        .entrySet()) {
                                    FFMPVirtualGageBasinMetaData fvgbmd = entry
                                            .getValue();
                                    FFMPVirtualGageBasin vgbasin = (FFMPVirtualGageBasin) fbd
                                            .get(fvgbmd.getLookupId());
                                    if (vgbasin == null) {
                                        vgbasin = new FFMPVirtualGageBasin(
                                                fvgbmd.getLid(),
                                                fvgbmd.getLookupId(), aggregate);
                                        fbd.put(fvgbmd.getLookupId(), vgbasin);
                                    }
                                    vgbasin.setValue(date, values[j]);
                                    j++;
                                }
                            }
                        }
                    }

                    catch (Throwable e) {
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "ERROR Retrieving Virtual ..."
                                        + domain.getCwa() + " : " + "ALL");
                    }
                }
            }
            
            setBasinData(fbd, "ALL");
            
        } catch (Throwable e) {
            statusHandler.handle(Priority.ERROR, "ERROR Retrieving Virtual..."
                    + "ALL");
        }
    }


	/**
	 * Purges out old data
	 * 
	 * @param date
	 */
	public void purgeData(Date date) {
		for (String ihuc : hucs) {
			FFMPBasinData basinData = getBasinData(ihuc, true);
			basinData.purgeData(date);
			setBasinData(basinData, ihuc);
		}
	}

	/**
	 * Dump cache
	 */
	public void closeCache() {
		getCache().closeCache();
	}

	public String getCacheDir() {
		return cacheDir;
	}

}
