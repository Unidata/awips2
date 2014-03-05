/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.db;

import java.util.Date;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;

/**
 * Interface for extending layer collectors to perform additional tasks
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface ICollectorAddon<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PersistableDataObject> {

	/**
	 * Called after record is added to layer. Both should be treated as
	 * read-only.
	 * 
	 * @param layer
	 * @param record
	 */
	public void onCollect(final L layer, final R record);

	/**
	 * Called after group of records have been added to layers
	 */
	public void onFinish();

	/**
	 * Called after collector has purged all data
	 */
	public void onPurgeAll();

	/**
	 * Called after collector has purged expired data
	 * 
	 * @param timesToKeep
	 */
	public void onPurgeExpired(Set<Date> timesToKeep);
}
