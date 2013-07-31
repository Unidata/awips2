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

import org.springframework.context.ApplicationContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Service to get convert utility
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ConvertService {

    private static volatile Converter cache = null;

    private static final Object cacheMutex = new Object();

    private static final IUFStatusHandler status = UFStatus
            .getHandler(ConvertService.class);

    /**
     * @return registered converter
     */
    public static Converter get() {
        if (cache == null) {
            synchronized (cacheMutex) {
                if (cache == null) {
                    try {
                        ApplicationContext ctx = EDEXUtil.getSpringContext();
                        String[] beans = ctx
                                .getBeanNamesForType(Converter.class);
                        if (beans == null || beans.length < 1) {
                            return new DefaultConverter();
                        }
                        cache = (Converter) ctx.getBean(beans[0]);
                    } catch (Exception e) {
                        status.error("problem getting service from spring", e);
                        return new DefaultConverter();
                    }
                }
            }
        }
        return cache;
    }

}
