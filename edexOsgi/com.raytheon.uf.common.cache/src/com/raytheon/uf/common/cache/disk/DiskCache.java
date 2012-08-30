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
package com.raytheon.uf.common.cache.disk;

import java.io.File;
import java.io.IOException;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.cache.CacheFactory;
import com.raytheon.uf.common.cache.ICache;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.SystemUtil;

/**
 * A object cache that writes all objects to disk. Each object is also kept in a
 * map of soft references. This will allow for the cache to grow as needed and
 * will objects in memory until a garbage collection is requested. Items can be
 * removed from the cache if they are no longer needed.
 * 
 * TODO Features to add:
 * 
 * 1) Configure cache to allow hard references based on configuration (last 20
 * objects for example)
 * 
 * 2) Specifcy a name/configuration for DiskCache's to allow for disk caches
 * with different configurations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 5, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DiskCache<K> implements ICache<K> {
	private static final transient IUFStatusHandler statusHandler = UFStatus
			.getHandler(DiskCache.class.getPackage().getName(), "CAVE",
					"WORKSTATION");

	/**
	 * Should this be static or one writer thread per cache? Only have so much
	 * through put to disk.
	 */
	protected static final DiskCacheWriter cacheWriter;

	static {
		cacheWriter = new DiskCacheWriter();
		cacheWriter.start();
	}

	protected String name;

	protected String baseCacheDir;

	/**
	 * Number of items allowed in the mem cache map. Defaults to 100 items.
	 */
	private int sizeMemCacheMap = 100;

	// unique per jvm, configured DiskCache instance, not clusterable
	protected File cacheDir;

	protected static final int MAX_PENDING_WRITES_PER_THREAD = 2;

	/**
	 * Contains objects that are in edit or have been evicted from in memory
	 * cache.
	 */
	private Map<String, MetaData<K>> metaDataMap = new HashMap<String, MetaData<K>>(
			128, 0.75f);

	/**
	 * Cached objects
	 */
	private LinkedHashMap<String, MetaData<K>> cacheMap = new RefMap<String, MetaData<K>>(
			128, 0.75f, true);

	private Object mapSyncLock = new Object();

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.cache.ICache#getFromCache(java.lang.String)
	 */
	@Override
	public K getFromCache(String id) {
		return getFromCache(id, false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.cache.ICache#getFromCache(java.lang.String,
	 * boolean)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public K getFromCache(String id, boolean lockForEdit) {
		MetaData<K> md = null;

		// get the meta data object
		synchronized (mapSyncLock) {
			md = cacheMap.get(id);
			if (md == null) {
				md = metaDataMap.get(id);
				if (md != null && md.numLockRequests.get() == 0 && !lockForEdit) {
					// move to cacheMap if not locked for edit and not going to
					// lock for edit
					cacheMap.put(id, md);
					metaDataMap.remove(id);
				}
			}

			if (md != null && lockForEdit
					&& md.numLockRequests.getAndIncrement() == 0) {
				// wasn't previously locked, and now needs to be locked
				metaDataMap.put(id, md);
				cacheMap.remove(id);
			}
		}

		if (md == null) {
			// object not cached
			return null;
		}

		K obj = md.ref;

		if (obj == null) {
			// check the soft reference
			SoftReference<K> ref = md.softRef;

			if (ref != null) {
				obj = ref.get();

				if (obj != null) {
					md.ref = obj;

					// cancel pending write for data if pending
					md.modified = cacheWriter.cancelWrite(md);
				}

				// clear the soft reference
				md.softRef = null;
			}

			if (obj == null) {
				// object no longer in memory, read from disk

				synchronized (md.syncObj) {
					// verify data wasn't already retrieved
					if (md.ref == null) {
						int tries = 0;
						boolean retry = true;
						while (retry) {
							File f = new File(md.cacheFilePath);

							try {
								// read from disk
								if (f.exists()) {
									int timeWaited = 0;
									while (f.length() == 0 && timeWaited < 1000) {
										// file should never be zero size, wait
										// for I/O operation to complete
										try {
											Thread.sleep(50);
										} catch (InterruptedException e) {
											// ignore
										}
										timeWaited += 50;
									}

									byte[] data = FileUtil.file2bytes(f);

									obj = (K) SerializationUtil
											.transformFromThrift(data);
									md.ref = obj;
								}

								retry = false;
							} catch (Exception e) {
								if (tries++ < 2) {
									statusHandler.handle(Priority.INFO,
											"Problem occurred retrieving cached data from disk: ["
													+ md.cacheFilePath
													+ "], size[" + f.length()
													+ "], retrying", e);

								} else {
									retry = false;
									statusHandler.handle(Priority.ERROR,
											"Failed to retrieve cached data from disk "
													+ tries + " times: ["
													+ md.cacheFilePath
													+ "], size[" + f.length()
													+ "]", e);
								}
							}
						}
					} else {
						obj = md.ref;
					}
				}
			}
		}

		return obj;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.cache.ICache#removeFromCache(java.lang.String)
	 */
	@Override
	public void removeFromCache(String id) {
		MetaData<K> md = null;
		synchronized (mapSyncLock) {
			md = cacheMap.remove(id);
			if (md == null) {
				md = metaDataMap.remove(id);
			}
		}

		if (md != null && md.cacheFilePath != null) {
			cacheWriter.cancelWrite(md);
			File f = new File(md.cacheFilePath);
			if (f.exists()) {
				f.delete();
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.cache.ICache#addToCache(java.lang.String, K)
	 */
	@Override
	public void addToCache(String id, K obj) throws IOException {
		MetaData<K> md = null;

		// check map for refs
		synchronized (mapSyncLock) {
			md = cacheMap.get(id);
			if (md == null) {
				md = metaDataMap.get(id);

				if (md != null && md.numLockRequests.get() > 0) {
					if (md.numLockRequests.decrementAndGet() == 0) {
						cacheMap.put(id, md);
						metaDataMap.remove(id);
					}
				}
			}
		}

		// no previous cache'd entry, make new one
		if (md == null) {
			md = new MetaData<K>(id, File.createTempFile("cache", ".bin",
					cacheDir).getAbsolutePath(), obj);

			synchronized (mapSyncLock) {
				cacheMap.put(id, md);
			}
		}

		md.ref = obj;
		md.softRef = null;
		md.modified = true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.cache.ICache#addToCache(K)
	 */
	@Override
	public String addToCache(K obj) throws IOException {
		MetaData<K> md = new MetaData<K>(File.createTempFile("cache", ".bin",
				cacheDir).getAbsolutePath(), obj);

		md.softRef = null;
		md.modified = true;

		synchronized (mapSyncLock) {
			cacheMap.put(md.cacheFilePath, md);
		}

		// unique id will be the unique temp file created
		return md.id;
	}

	public void closeCache() {
		// cacheWriter.run = false;
		// TODO: set flag that cache is closed that throws errors on access
		clearCache();
	}

	public void clearCache() {
		synchronized (mapSyncLock) {
			// cancel the writes
			for (MetaData<K> md : cacheMap.values()) {
				cacheWriter.cancelWrite(md);
			}
			for (MetaData<K> md : metaDataMap.values()) {
				cacheWriter.cancelWrite(md);
			}
			// delete the files
			for (MetaData<K> md : cacheMap.values()) {
				File f = new File(md.cacheFilePath);
				if (f.exists()) {
					f.delete();
				}
			}
			for (MetaData<K> md : metaDataMap.values()) {
				File f = new File(md.cacheFilePath);
				if (f.exists()) {
					f.delete();
				}
			}

			cacheMap.clear();
			metaDataMap.clear();
		}
	}

	public int getSizeMemCacheMap() {
		return sizeMemCacheMap;
	}

	public void setSizeMemCacheMap(int sizeMemCacheMap) {
		this.sizeMemCacheMap = sizeMemCacheMap;

		// need to push extra entries to disk?
		synchronized (mapSyncLock) {
			if (sizeMemCacheMap > cacheMap.size()) {
				RefMap<String, MetaData<K>> tmp = new RefMap<String, MetaData<K>>(
						(int) (sizeMemCacheMap * 1.25) + 1, 0.75f, true);
				tmp.putAll(cacheMap);
				cacheMap = tmp;
			}
		}

		this.sizeMemCacheMap = sizeMemCacheMap;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getBaseCacheDir() {
		return baseCacheDir;
	}

	public void setBaseCacheDir(String baseCacheDir) {
		this.baseCacheDir = baseCacheDir;
	}

	public void activateCache() {
		int pid = SystemUtil.getPid();

		if (baseCacheDir == null) {
			IPathManager pathMgr = PathManagerFactory.getPathManager();
			LocalizationContext userContext = pathMgr
					.getContext(LocalizationType.CAVE_STATIC,
							LocalizationLevel.WORKSTATION);
			String path = "diskCache" + File.separator + name + File.separator
					+ File.separator + "pid_" + pid;
			this.cacheDir = PathManagerFactory.getPathManager().getFile(
					userContext, path);
		} else {
			this.cacheDir = new File(baseCacheDir + File.separator
					+ "diskCache" + File.separator + name + File.separator
					+ File.separator + "pid_" + pid);
		}

		if (!cacheDir.exists()) {
			cacheDir.mkdirs();
		}

		CacheFactory factory = CacheFactory.getInstance();
		factory.addCache(name, this);

		// TODO: Throw exception if not properly configured
	}

	public void activateEdexCache() {
		int pid = SystemUtil.getPid();

		if (baseCacheDir == null) {
			IPathManager pathMgr = PathManagerFactory.getPathManager();
			LocalizationContext context = pathMgr.getContext(
					LocalizationType.EDEX_STATIC, LocalizationLevel.SITE);
			String path = "diskCache" + File.separator + name + File.separator
					+ File.separator + "pid_" + pid;

			try {
				LocalizationFile dir = PathManagerFactory.getPathManager()
						.getLocalizationFile(context, path);
				this.cacheDir = dir.getFile();
			} catch (Exception e) {
				// no localization file exists
				this.cacheDir = new File(path);
			}
		} else {
			this.cacheDir = new File(baseCacheDir + File.separator
					+ "diskCache" + File.separator + name + File.separator
					+ File.separator + "pid_" + pid);
		}

		if (!cacheDir.exists()) {
			cacheDir.mkdirs();
		}

		CacheFactory factory = CacheFactory.getInstance();
		factory.addCache(name, this);

		// TODO: Throw exception if not properly configured
	}

	protected class RefMap<X extends String, V extends MetaData> extends
			LinkedHashMap<X, V> {
		/**
		 * @param initialCapacity
		 * @param loadFactor
		 * @param accessOrder
		 */
		public RefMap(int initialCapacity, float loadFactor, boolean accessOrder) {
			super(initialCapacity, loadFactor, accessOrder);
		}

		@Override
		protected boolean removeEldestEntry(Entry<X, V> eldest) {
			boolean rval = size() > sizeMemCacheMap;

			if (rval) {
				@SuppressWarnings("unchecked")
				MetaData<K> md = eldest.getValue();

				if (md.modified) {
					md.modified = false;
					cacheWriter.asyncWrite(DiskCache.this, md);
				}

				md.softRef = new SoftReference<K>(md.ref);
				md.ref = null;

				synchronized (mapSyncLock) {
					metaDataMap.put(eldest.getKey(), md);
				}
			}

			return rval;
		}
	}
}
