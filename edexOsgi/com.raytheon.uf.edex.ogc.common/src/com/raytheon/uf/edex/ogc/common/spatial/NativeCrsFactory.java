/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.collections.map.LRUMap;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.springframework.context.ApplicationContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.ogc.common.OgcException;

/**
 * Factory for retrieving geotools coordinate reference system objects from
 * native CRS URNs. Uses NativeCrsAuthority beans registered in spring to create
 * new CRS objects. Uses an internal LRU cache.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class NativeCrsFactory {

    @SuppressWarnings("unchecked")
    private static final Map<String, CoordinateReferenceSystem> cache = Collections
            .synchronizedMap(new LRUMap(5));

    private static Map<String, NativeCrsAuthority> auths = null;

	private static final IUFStatusHandler log = UFStatus
			.getHandler(NativeCrsAuthority.class);

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
                ApplicationContext ctx = EDEXUtil.getSpringContext();
                String[] beans = ctx
                        .getBeanNamesForType(NativeCrsAuthority.class);
                auths = new HashMap<String, NativeCrsAuthority>(beans.length);
                for (String bean : beans) {
                    NativeCrsAuthority nca = (NativeCrsAuthority) ctx
                            .getBean(bean);
                    auths.put(nca.getId(), nca);
                }
            }
        }).run();
    }

    /**
     * Lookup native CRS using URN
     * 
     * @param urn
     * @return null if not found
     * @throws OgcException
     */
    public static CoordinateReferenceSystem lookup(String urn)
            throws OgcException {
        CoordinateReferenceSystem rval = cache.get(urn);
        if (rval != null) {
            return rval;
        }
        if (auths == null) {
            log.error("Attempted to lookup CRS before initialized",
                    new Exception());
            return null;
        }
        
        for (Entry<String, NativeCrsAuthority> e : auths.entrySet()) {
            NativeCrsAuthority auth = e.getValue();
            rval = auth.lookup(urn);
            if (rval != null) {
                cache.put(urn, rval);
                return rval;
            }
        }
        return null;
    }
}
