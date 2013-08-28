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
 * May 5, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.reg;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.context.ApplicationContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.format.IWcsDataFormatter;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class WcsSourceAccessor {

    private static Map<String, IWcsSource<?, ?>> cache;

    private static Map<String, IWcsDataFormatter> formatMap;

    private static IUFStatusHandler log = UFStatus
            .getHandler(WcsSourceAccessor.class);

    private WcsSourceAccessor() {

    }

    static {
        new Thread(new Runnable() {
            @Override
            public void run() {
                while (!EDEXUtil.isRunning()) {
                    try {
                        Thread.sleep(5000);
                    } catch (InterruptedException e) {
                    }
                }
                try {
                    ApplicationContext ctx = EDEXUtil.getSpringContext();
                    String[] beans = ctx.getBeanNamesForType(IWcsSource.class);
                    cache = new HashMap<String, IWcsSource<?, ?>>(beans.length);
                    for (String bean : beans) {
                        IWcsSource<?, ?> s = (IWcsSource<?, ?>) ctx
                                .getBean(bean);
                        cache.put(s.getKey(), s);
                    }
                } catch (Throwable e) {
                    log.error("Unable to init wcs sources", e);
                }
                try {
                    ApplicationContext ctx = EDEXUtil.getSpringContext();
                    String[] beans = ctx
                            .getBeanNamesForType(IWcsDataFormatter.class);
                    HashMap<String, IWcsDataFormatter> map = new HashMap<String, IWcsDataFormatter>(
                            beans.length);
                    for (String bean : beans) {
                        IWcsDataFormatter df = (IWcsDataFormatter) ctx
                                .getBean(bean);
                        map.put(df.getIdentifier(), df);
                    }
                    formatMap = Collections.unmodifiableMap(map);
                } catch (Throwable e) {
                    log.error("Unable to init wcs formats", e);
                }
            }
        }).run();
    }

    /**
     * @param summary
     * @return empty list if accessor isn't initialized
     */
    public static List<CoverageDescription> getCoverages(boolean summary) {
        if (cache == null) {
            return new ArrayList<CoverageDescription>(0);
        }
        List<CoverageDescription> cd = new ArrayList<CoverageDescription>();
        for (String key : cache.keySet()) {
            IWcsSource<?, ?> s = cache.get(key);
            cd.addAll(s.listCoverages(summary));
        }
        return cd;
    }

    /**
     * @param id
     * @return null if source is not found or accessor isn't initialized
     * @throws WcsException
     */
    public static IWcsSource<?, ?> getSource(String id) throws WcsException {
        if (cache == null) {
            return null;
        }
        for (String key : cache.keySet()) {
            IWcsSource<?, ?> s = cache.get(key);
            if (s.hasCoverage(id)) {
                return s;
            }
        }
        return null;
    }

    /**
     * @return empty map if accessor isn't initialized
     */
    public static Map<String, IWcsDataFormatter> getFormatMap() {
        if (formatMap == null) {
            return new HashMap<String, IWcsDataFormatter>(0);
        }
        return formatMap;
    }

    /**
     * @return empty list if accessor isn't initialized
     */
    public static List<String> getFormats() {
        if (formatMap == null) {
            return new ArrayList<String>(0);
        }
        return new ArrayList<String>(formatMap.keySet());
    }
}
