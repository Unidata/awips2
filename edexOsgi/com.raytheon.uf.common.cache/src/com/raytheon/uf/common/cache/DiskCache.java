package com.raytheon.uf.common.cache;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.SystemUtil;

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

/**
 * TODO Remove old cache/hung cache. Cache should be removed on workspace exit.
 * 
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

	protected String name;

	protected String baseCacheDir;

	/**
	 * Number of items allowed in the mem cache map. Defaults to 100 items.
	 */
	private int sizeMemCacheMap = 100;

	// unique per jvm, configured DiskCache instance, not clusterable
	protected File cacheDir;

	/**
	 * Should this be static or one writer thread per cache? Only have so much
	 * through put to disk.
	 */
	protected DiskCacheWriter cacheWriter = null;

	protected static final int MAX_PENDING_WRITES_PER_THREAD = 2;

	/**
	 * Contains objects that are in edit or have been evicted from in memory
	 * cache.
	 */
	private Map<String, MetaData> metaDataMap = new HashMap<String, MetaData>(
			128, 0.75f);

	/**
	 * Cached objects
	 */
	private LinkedHashMap<String, MetaData> cacheMap = new RefMap<String, MetaData>(
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
		MetaData md = null;

		// get the meta data object
		synchronized (mapSyncLock) {
			md = cacheMap.get(id);
			if (md == null) {
				md = metaDataMap.get(id);
				if (md != null && !md.lockedForEdit && !lockForEdit) {
					// move to cacheMap if not locked for edit and not going to
					// lock for edit
					cacheMap.put(id, md);
					metaDataMap.remove(id);
				}
			}

			if (md != null && lockForEdit && !md.lockedForEdit) {
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
					cacheWriter.cancelWrite(md);
				}

				// clear the soft reference
				md.softRef = null;
			}

			if (obj == null) {
				// object no longer in memory, read from disk

				try {
					synchronized (md.syncObj) {
						// verify data wasn't already retrieved
						if (md.ref == null) {
							// read from disk
							File f = new File(md.cacheFilePath);
							if (f.exists()) {
								byte[] data = FileUtil.file2bytes(f);

								obj = (K) SerializationUtil
										.transformFromThrift(data);
								md.ref = obj;
							}
						} else {
							obj = md.ref;
						}
					}
				} catch (Exception e) {
					statusHandler.handle(Priority.ERROR,
							"Error occurred retrieving cached data from disk",
							e);
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
		MetaData md = null;
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
		MetaData md = null;

		// check map for refs
		synchronized (mapSyncLock) {
			md = cacheMap.get(id);
			if (md == null) {
				md = metaDataMap.get(id);

				if (md != null && md.lockedForEdit) {
					md.lockedForEdit = false;
					cacheMap.put(id, md);
					metaDataMap.remove(id);
				}
			}
		}

		// no previous cache'd entry, make new one
		if (md == null) {
			md = new MetaData(File.createTempFile("cache", ".bin", cacheDir)
					.getAbsolutePath(), obj);

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
		MetaData md = new MetaData(File.createTempFile("cache", ".bin",
				cacheDir).getAbsolutePath(), obj);

		md.softRef = null;
		md.modified = true;

		synchronized (mapSyncLock) {
			cacheMap.put(md.cacheFilePath, md);
		}

		// unique id will be the unique temp file created
		return md.cacheFilePath;
	}

	public void closeCache() {
		cacheWriter.run = false;
		clearCache();
	}

	public void clearCache() {
		synchronized (mapSyncLock) {
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
				RefMap<String, MetaData> tmp = new RefMap<String, MetaData>(
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
		IPathManager pathMgr = PathManagerFactory.getPathManager();
		LocalizationContext userContext = pathMgr.getContext(
				LocalizationType.CAVE_STATIC, LocalizationLevel.WORKSTATION);

		if (baseCacheDir == null) {
			baseCacheDir = "diskCache";
		}

		String path = baseCacheDir + File.separator + name + File.separator
				+ File.separator + "pid_" + pid;
		this.cacheDir = PathManagerFactory.getPathManager().getFile(
				userContext, path);

		if (!cacheDir.exists()) {
			cacheDir.mkdirs();
		}

		if (cacheWriter == null) {
			cacheWriter = new DiskCacheWriter(name);
			cacheWriter.start();
		}

		CacheFactory factory = CacheFactory.getInstance();
		factory.addCache(name, this);

		// TODO: Throw exception if not properly configured
	}

	public void activateEdexCache() {
		int pid = SystemUtil.getPid();
		IPathManager pathMgr = PathManagerFactory.getPathManager();
		LocalizationContext context = pathMgr.getContext(
				LocalizationType.EDEX_STATIC, LocalizationLevel.SITE);

		if (baseCacheDir == null) {
			baseCacheDir = "diskCache";
		}

		String path = baseCacheDir + File.separator + name + File.separator
				+ File.separator + "pid_" + pid;
		try {
			LocalizationFile dir = PathManagerFactory.getPathManager()
					.getLocalizationFile(context, path);
			this.cacheDir = dir.getFile();
		} catch (Exception e) {
			// no localization file exists
			this.cacheDir = new File(path);
		}

		if (!cacheDir.exists()) {
			cacheDir.mkdirs();
		}

		if (cacheWriter == null) {
			cacheWriter = new DiskCacheWriter(name);
			cacheWriter.start();
		}

		CacheFactory factory = CacheFactory.getInstance();
		factory.addCache(name, this);

		// TODO: Throw exception if not properly configured
	}

	protected class MetaData {
		private final Object syncObj;

		private final String cacheFilePath;

		private SoftReference<K> softRef = null;

		private K ref = null;

		private boolean modified = true;

		private boolean lockedForEdit = false;

		protected MetaData(String cacheFilePath, K ref) {
			this.cacheFilePath = cacheFilePath;
			this.syncObj = new Object();
			this.ref = ref;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((cacheFilePath == null) ? 0 : cacheFilePath.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			@SuppressWarnings("unchecked")
			MetaData other = (MetaData) obj;
			if (cacheFilePath == null) {
				if (other.cacheFilePath != null)
					return false;
			} else if (!cacheFilePath.equals(other.cacheFilePath))
				return false;
			return true;
		}

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
				MetaData md = eldest.getValue();
				if (md.modified) {
					cacheWriter.asyncWrite(md);
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

	protected class DiskCacheWriter extends Thread {
		protected boolean run = true;

		protected Map<MetaData, K> pendingWrites = new LinkedHashMap<MetaData, K>();

		public DiskCacheWriter(String name) {
			super(name);
		}

		public void asyncWrite(MetaData md) {
			if (md.modified) {
				synchronized (pendingWrites) {
					// if we have too many writes pending, wait for a write to
					// finish
					while (pendingWrites.size() >= MAX_PENDING_WRITES_PER_THREAD) {
						try {
							pendingWrites.wait();
						} catch (InterruptedException e) {
						}
					}

					pendingWrites.put(md, md.ref);
					pendingWrites.notify();
				}
			}
		}

		public void cancelWrite(MetaData md) {
			synchronized (pendingWrites) {
				pendingWrites.remove(md);
			}

			synchronized (md.syncObj) {
				// wait for any pending writes to finish
			}
		}

		@Override
		public void run() {
			while (run) {
				try {
					Map.Entry<MetaData, K> entry = null;
					synchronized (pendingWrites) {
						if (pendingWrites.size() == 0) {
							try {
								pendingWrites.wait(60000);
							} catch (InterruptedException e) {
								// ignore
							}
						}

						// did we get notified or did enough time pass?
						if (pendingWrites.size() > 0) {
							Iterator<Entry<MetaData, K>> iter = pendingWrites
									.entrySet().iterator();
							if (iter.hasNext()) {
								entry = iter.next();
								iter.remove();
							}
						}
					}

					if (entry != null) {
						MetaData md = entry.getKey();

						synchronized (md.syncObj) {
							// verify write wasn't canceled
							if (md.ref == null) {
								K dataObject = entry.getValue();
								OutputStream os = null;
								boolean success = false;

								try {
									File f = new File(md.cacheFilePath);

									if (dataObject != null) {
										// serialize object and write data
										// to disk
										os = new BufferedOutputStream(
												new FileOutputStream(f));
										DynamicSerializationManager dsm = DynamicSerializationManager
												.getManager(SerializationType.Thrift);
										dsm.serialize(dataObject, os);
										f.deleteOnExit();
									} else if (f.exists()) {
										// data is null, delete file
										f.delete();
									}
									success = true;
								} finally {
									if (os != null) {
										try {
											os.close();
										} catch (IOException e) {
											statusHandler.handle(
													Priority.ERROR,
													"Failed to close stream to cache file: "
															+ md.cacheFilePath,
													e);
										}
									}

									if (success) {
										md.modified = false;
									} else {
										// failed to save, don't evict from
										// memory
										md.ref = dataObject;
										md.softRef = null;
										synchronized (mapSyncLock) {
											cacheMap.put(md.cacheFilePath, md);
											metaDataMap
													.remove(md.cacheFilePath);
										}
									}

									synchronized (pendingWrites) {
										// notify threads that may have been
										// waiting for write to finish
										pendingWrites.notifyAll();
									}
								}
							}
						}
					}
				} catch (Throwable e) {
					statusHandler.handle(Priority.ERROR,
							"Error occurred writing data to disk cache", e);
				}
			}
		}
	}
}
