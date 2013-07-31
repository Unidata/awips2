/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4.cf;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class CfGridMapper {

    private static final Properties ATTRIB_MAP = new Properties();

    private static final IUFStatusHandler log = UFStatus
            .getHandler(CfGridMapper.class);

    static {
        ClassLoader loader = CfGridMapper.class.getClassLoader();
        InputStream res = loader
                .getResourceAsStream("META-INF/nc4-CF-grid-mapping.properties");
        if (res == null) {
            log.warn("Unable to find grid attribute map");
        } else {
            try {
                ATTRIB_MAP.load(res);
            } catch (IOException e) {
                log.error("Unable to load mapping properties", e);
            }
        }
    }

    public static String getMappingAttributeName(String name) {
        return ATTRIB_MAP.getProperty(name, name);
    }

}
