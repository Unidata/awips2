package com.raytheon.uf.common.dataplugin.ffmp;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Map.Entry;

import com.raytheon.uf.common.cache.CacheException;
import com.raytheon.uf.common.cache.CacheFactory;
import com.raytheon.uf.common.cache.DiskCache;
import com.raytheon.uf.common.cache.ICache;

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

	private String sourceCacheName = null;
	
	private ArrayList<String> hucs = new ArrayList<String>();
		
	public FFMPCacheRecord(FFMPRecord rec, String sourceCacheName) {
		
		this.setSiteKey(rec.getSiteKey());
		this.setWfo(rec.getWfo());
		this.setDataKey(rec.getDataKey());
		this.setSourceName(rec.getSourceName());
		this.setPluginName(rec.getPluginName());
		this.setSourceCacheName(sourceCacheName);
	
	}
	
	/**
	 * Data path setter
	 * @param dataPath
	 */
	private void setSourceCacheName(String sourceCacheName) {
		this.sourceCacheName = sourceCacheName;
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
    	
    	try {
			diskCache = (DiskCache<FFMPBasinData>)CacheFactory.getInstance()
			  .getCache("FFMP-"+ getWfo()+ "-"+getSiteKey()+ "-" +getSourceCacheName());
			
		} catch (CacheException e) {
			DiskCache<FFMPBasinData> dc = createCache("FFMP-"+ getWfo()+ "-"+getSiteKey()+ "-" +getSourceCacheName());
			CacheFactory.getInstance().addCache("FFMP-"+ getWfo()+ "-"+getSiteKey()+ "-" +getSourceCacheName(), dc);
			return dc;
		}
		
		return diskCache;
    }
    
    
    /**
     * Get BasinData Map from cache
     * @param siteKey
     * @param sourceName
     * @return
     */
    public FFMPBasinData getBasinData(String hucName) {
    	
    	FFMPBasinData basins = null;
    	
        if (hucName != null) {
            try {
            	
            	DiskCache<FFMPBasinData> diskCache = getCache();
            	basins = (FFMPBasinData) diskCache.getFromCache(hucName);
            	
            	if (basins == null) {
            		basins = new FFMPBasinData(hucName);
            		if (!hucs.contains(hucName)) {
            			hucs.add(hucName);
            		}
            	}
                
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return basins;
    }
    
    /**
     * Create cache objects if needed
     * @param siteKey
     * @return
     */
    private DiskCache<FFMPBasinData> createCache(String name) {
    	ICache<FFMPBasinData> cache = new DiskCache<FFMPBasinData>();
    	DiskCache<FFMPBasinData> dc = (DiskCache<FFMPBasinData>) cache;
    	dc.setName(name);
    	dc.setSizeMemCacheMap(2); // For FFMP hold two generally COUNTY and ALL
    	dc.activateCache();
    	
    	return dc;
    }
    
    /**
     * Set source record to cache
     * @param siteKey
     * @param sourceName
     * @param record
     */
     
	public void setBasinData(FFMPBasinData basins, String hucName) {
		if (hucName != null) {
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
		}
	}
    
    /**
     * maybe this will work
     * 
     * @param basins
     * @param hucName
     */
    public void setBasinBuddyData(FFMPBasinData basins, String hucName) {

		if (getBasinData(hucName) != null) {

			for (Entry<Long, FFMPBasin> entry : basins.getBasins().entrySet()) {
				FFMPBasin basin = getBasinData(hucName).get(entry.getKey());
				if (basin != null) {
					if (basin instanceof FFMPGuidanceBasin) {
						FFMPGuidanceBasin gbasin = (FFMPGuidanceBasin) basin;
						gbasin.getGuidValues().putAll(
								((FFMPGuidanceBasin) entry.getValue())
										.getGuidValues());
					} else {
						basin.getValues().putAll(entry.getValue().getValues());
					}
				} else {
					getBasinData(hucName).put(entry.getKey(), entry.getValue());
				}
			}
		} else {
			setBasinData(basins, hucName);
		}
    }
        
    /**
     * Purges out old data
     * 
     * @param date
     */
    public void purgeData(Date date) {

        for (String ihuc : hucs) {
            FFMPBasinData basinData = getBasinData(ihuc);
            basinData.purgeData(date);
        }
    }
  
}
