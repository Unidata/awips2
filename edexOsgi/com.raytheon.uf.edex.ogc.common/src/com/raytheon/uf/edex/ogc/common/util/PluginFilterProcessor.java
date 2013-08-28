/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.util;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.ReadLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.WriteLock;

import org.apache.camel.Exchange;
import org.apache.camel.Message;
import org.apache.camel.Processor;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Allows for dynamic filtering of plugin data objects in a route
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PluginFilterProcessor implements Processor {

	private final List<PluginIngestFilter> filters = new ArrayList<PluginIngestFilter>();

	private final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

	private final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

	/* (non-Javadoc)
	 * @see org.apache.camel.Processor#process(org.apache.camel.Exchange)
	 */
	@Override
	public void process(Exchange exchange) throws Exception {
		Message in = exchange.getIn();
		PluginDataObject[] body = null;
		try {
			body = in.getBody(PluginDataObject[].class);
		} catch (Exception e) {
			log.error("Unable to get data objects as array", e);
		}
		if (body == null) {
			return;
		}
		ReadLock read = lock.readLock();
		read.lock();
		try {
			for (PluginIngestFilter filter : filters) {
				body = filter.filter(body);
			}
		} finally {
			read.unlock();
		}
		in.setBody(body);
	}

	/**
	 * Add all filters to filter list, can be called multiple times
	 * 
	 * @param filters
	 */
	public void setFilters(List<PluginIngestFilter> filters) {
		WriteLock write = lock.writeLock();
		write.lock();
		try {
			this.filters.addAll(filters);
		} finally {
			write.unlock();
		}
	}

	/**
	 * Add single filter to filter list, can be called multiple times
	 * 
	 * @param filter
	 */
	public void setFilter(PluginIngestFilter filter) {
		WriteLock write = lock.writeLock();
		write.lock();
		try {
			this.filters.add(filter);
		} finally {
			write.unlock();
		}
	}

}
