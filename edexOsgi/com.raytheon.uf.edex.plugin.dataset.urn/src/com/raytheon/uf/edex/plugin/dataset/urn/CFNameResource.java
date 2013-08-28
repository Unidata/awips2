/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */

package com.raytheon.uf.edex.plugin.dataset.urn;

import org.springframework.core.io.Resource;

public class CFNameResource {

	/**
	 * Spring resource for properties file, this is preferred over a filepath
	 * string
	 */
	protected Resource configFileResource = null;
	
	public Resource getConfigFileResource() {
		return configFileResource;
	}

	public void setConfigFileResource(Resource configFileResource) {
		this.configFileResource = configFileResource;
	}
	
}
