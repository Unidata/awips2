/**********************************************************************
 *
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
 **********************************************************************/
package com.raytheon.uf.edex.wms.reg;

import java.util.HashMap;
import java.util.Map;

import org.springframework.context.ApplicationContext;

import com.raytheon.uf.edex.core.EDEXUtil;

public class WmsSourceAccessor {

	public synchronized Map<String, WmsSource> getSources() {
		Map<String, WmsSource> sources = new HashMap<String, WmsSource>();
		ApplicationContext ctx = EDEXUtil.getSpringContext();
		String[] beans = ctx.getBeanNamesForType(WmsSource.class);
		for (String bean : beans) {
			WmsSource s = (WmsSource) ctx.getBean(bean);
			sources.put(s.getKey(), s);
		}
		return sources;
	}

}
