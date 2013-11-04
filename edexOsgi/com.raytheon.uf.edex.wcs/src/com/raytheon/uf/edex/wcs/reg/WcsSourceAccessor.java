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
