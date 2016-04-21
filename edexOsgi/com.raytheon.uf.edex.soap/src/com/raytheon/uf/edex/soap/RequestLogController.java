/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.soap;

import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Singleton to control how incoming request information is logged. Implemented
 * as a singleton to allow runtime control from a Web Client
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * September, 2013         behemmi     Initial creation
 * May 23, 2014 3199       bclement    moved to edex.soap from edex.log
 * 
 * </pre>
 * 
 * @author behemmi
 * @version 1.0
 */
public class RequestLogController {

	private static RequestLogController instance = null;
	
	private Priority requestLogLevel;
	private boolean shouldLogRequestsInfo;

	private RequestLogController(){
		requestLogLevel = Priority.DEBUG;
		shouldLogRequestsInfo = true;
	}
	
	public static RequestLogController getInstance() {
		if(instance == null) {
			instance = new RequestLogController();
		}
		return instance;
	}
	
	public Priority getRequestLogLevel() {
		return requestLogLevel;
	}

	public void setRequestLogLevel(Priority requestLogLevel) {
		this.requestLogLevel = requestLogLevel;
	}

	public boolean shouldLogRequestsInfo() {
		return getShouldLogRequestsInfo();
	}

	/**
	 * Traditional getter for Jackson serialization.
	 *
	 * @return shouldLogRequestsInfo - if this logger is enabled
	 */
	public boolean getShouldLogRequestsInfo() {
		return shouldLogRequestsInfo;
	}

	public void setShouldLogRequestsInfo(boolean shouldLogRequestsInfo) {
		this.shouldLogRequestsInfo = shouldLogRequestsInfo;
	}
}
