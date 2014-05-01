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
package com.raytheon.uf.common.cache;

import java.io.IOException;

import com.raytheon.uf.common.serialization.SerializationException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 * @param <K>
 */

public interface ICache<K> {

	/**
	 * Returns the cache'd object. If you need to edit the object call
	 * getFromCache(String, true). Do not hold on to object or keep a reference
	 * to it for extended period. This will making caching not work as well. An
	 * internal memory cache is kept of the object that is managed for memory
	 * efficiency. Do no manually manage memory of object.
	 * 
	 * @param id
	 * @param lockForEdit
	 * @return The object from the cache or null if the object was not in the
	 *         cache.
	 */
	public abstract K getFromCache(String id);

	/**
	 * Returns the cache'd object. If the object is going to be edited you must
	 * pass true for lockForEdit. This will guarantee the object will not be
	 * cached out avoiding concurrent mod exception and also will ensure the
	 * object gets written back to disk. Note: any planned editing must still be
	 * externally synchronized if done from multiple threads. When the object is
	 * done being edited, addToCache must be called to release the object from
	 * editing. Do not hold on to object or keep a reference to it for extended
	 * period. This will making caching not work as well. An internal memory
	 * cache is kept of the object that is managed for memory efficiency. Do no
	 * manually manage memory of object.
	 * 
	 * @param id
	 * @param lockForEdit
	 * @return The object from the cache or null if the object was not in the
	 *         cache.
	 */
	public abstract K getFromCache(String id, boolean lockForEdit);

	/**
	 * Removes the object and any related meta data from the cache.
	 * 
	 * @param id
	 */
	public abstract void removeFromCache(String id);

	/**
	 * Object must implement dynamic serialize to be cached. If object is
	 * changed after addToCache is called, change is not persisted to disk until
	 * addToCache is called again. Change may be available in the pure memory
	 * first level cache if the object has not been evicted.
	 * 
	 * @param id
	 * @param obj
	 */
	public abstract void addToCache(String id, K obj)
			throws SerializationException, IOException;

	/**
	 * Object must implement dynamic serialize to be cached. If object is
	 * changed after addToCache is called, change is not persisted to disk until
	 * addToCache is called again. Change may be available in the pure memory
	 * first level cache if the object has not been evicted.
	 * 
	 * @param obj
	 * @return The id of the object for retrievals from cache.
	 * @throws SerializationException
	 * @throws IOException
	 */
	public abstract String addToCache(K obj) throws SerializationException,
			IOException;
}