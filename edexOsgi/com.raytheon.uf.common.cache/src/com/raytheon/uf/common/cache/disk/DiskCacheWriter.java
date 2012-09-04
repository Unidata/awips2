package com.raytheon.uf.common.cache.disk;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This software was developed and / or modified by Raytheon Company, pursuant
 * to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA This software product contains
 * export-restricted data whose export/transfer/disclosure is restricted by U.S.
 * law. Dissemination to non-U.S. persons whether in the United States or abroad
 * requires an export license or other authorization.
 * 
 * Contractor Name: Raytheon Company Contractor Address: 6825 Pine Street, Suite
 * 340 Mail Stop B8 Omaha, NE 68106 402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for further
 * licensing information.
 **/

public class DiskCacheWriter extends Thread {
	private static final transient IUFStatusHandler statusHandler = UFStatus
			.getHandler(DiskCacheWriter.class.getPackage().getName(),
					"WORKSTATION", "CAVE");

	protected static final int MAX_PENDING_WRITES = 100;

	protected boolean run = true;

	protected Map<MetaData, Container> pendingWrites = new LinkedHashMap<MetaData, Container>();

	private class Container {
		DiskCache cache;
		Object obj;
	}

	public DiskCacheWriter() {
		super("DiskCacheWriter");
	}

	public void asyncWrite(DiskCache cache, MetaData md) {
		synchronized (pendingWrites) {
			// if we have too many writes pending, wait for a write to
			// finish
			while (pendingWrites.size() >= MAX_PENDING_WRITES && run) {
				try {
					pendingWrites.wait(1000);
				} catch (InterruptedException e) {
				}
			}

			Container c = new Container();
			c.cache = cache;
			c.obj = md.ref;
			pendingWrites.put(md, c);
			pendingWrites.notify();
		}
	}

	/**
	 * Cancels a pending write. If a write was pending, returns true.
	 * 
	 * @param md
	 * @return
	 */
	public boolean cancelWrite(MetaData md) {
		boolean rval = false;
		synchronized (pendingWrites) {
			rval = (pendingWrites.remove(md) != null);
		}

		synchronized (md.syncObj) {
			// wait for any pending writes to finish
		}

		return rval;
	}

	@Override
	public void run() {
		while (run) {
			try {
				Map.Entry<MetaData, Container> entry = null;
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
						Iterator<Entry<MetaData, Container>> iter = pendingWrites
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
							Container container = entry.getValue();
							Object dataObject = container.obj;
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
										statusHandler.handle(Priority.ERROR,
												"Failed to close stream to cache file: "
														+ md.cacheFilePath, e);
									}
								}

								if (!success) {
									// failed to save, don't evict from memory
									container.cache.addToCache(md.id,
											dataObject);
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
