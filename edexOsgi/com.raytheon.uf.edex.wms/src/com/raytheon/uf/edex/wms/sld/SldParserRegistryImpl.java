/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.sld;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * TODO Add Description
 * 
 * @author bclement
 * @version 1.0
 */
public class SldParserRegistryImpl implements SldParserRegistry {

	protected Map<String, SldParser> parsers = new ConcurrentHashMap<String, SldParser>();

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.sld.SldParserRegistry#register(com.raytheon.
	 * uf.edex.wms.sld.SldParser)
	 */
	@Override
	public SldParserRegistry register(SldParser parser) {
		// allow for overriding of previously registered parsers
		parsers.put(parser.getVersion(), parser);
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.sld.SldParserRegistry#unregister(com.raytheon
	 * .uf.edex.wms.sld.SldParser)
	 */
	@Override
	public SldParserRegistry unregister(SldParser parser) {
		parsers.remove(parser.getVersion());
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.sld.SldParserRegistry#getParser(java.lang.String
	 * )
	 */
	@Override
	public SldParser getParser(String version) {
		if (version == null) {
			return null;
		}
		return parsers.get(version);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.sld.SldParserRegistry#register(com.raytheon.
	 * uf.edex.wms.sld.SldParser[])
	 */
	@Override
	public SldParserRegistry register(SldParser[] parsers) {
		for (SldParser p : parsers) {
			register(p);
		}
		return this;
	}

}
