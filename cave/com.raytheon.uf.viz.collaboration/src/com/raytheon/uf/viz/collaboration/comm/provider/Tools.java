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
package com.raytheon.uf.viz.collaboration.comm.provider;

import org.eclipse.ecf.core.IContainer;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public abstract class Tools {

    public static final String VENUE_SUBJECT_PROP = "subject";
    
    public static final String NAME_DELIM = "@";
    
    public static final String PORT_DELIM = ":";
    
    public static final String RESOURCE_DELIM = "/";
    
    
    /**
     * 
     * @param <T>
     * @param container
     * @param c
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T getPresenceContainerAdapter(IContainer container, Class<T> c) {
        return (T) container.getAdapter(c);
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parseName(String id) {
        String name = null;
        if(id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if(delimPos >= 0) {
                name = id.substring(0,delimPos);
            }
        }
        return name;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parseHost(String id) {
        String host = null;
        if(id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if(delimPos >= 0) {
                // Ok we have the start of the host name
                int start = delimPos + 1;
                int stop = start;
                delimPos = id.indexOf(PORT_DELIM);
                if(delimPos > stop) {
                    // ok we have a port so grab anything in between
                    stop = delimPos;
                } else {
                    // no port delimiter, so check for the resource
                    delimPos = id.indexOf(RESOURCE_DELIM);
                    if(delimPos > stop) {
                        // we have the resource delimiter
                        stop = delimPos;
                    } else {
                        stop = id.length();
                    }
                }
                host = id.substring(start,stop);
            }
        }
        return host;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parsePort(String id) {
        String host = null;
        if(id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if(delimPos >= 0) {
                // Ok we have the start of the host name
                int start = delimPos + 1;
                int stop = start;
                delimPos = id.indexOf(PORT_DELIM);
                if(delimPos > stop) {
                    // ok we have a port so grab anything in between
                    start = delimPos + 1;
                    delimPos = id.indexOf(RESOURCE_DELIM);
                    if(delimPos > start) {
                        stop = delimPos;
                    } else {
                        stop = id.length();
                    }
                    host = id.substring(start,stop);
                }
            }
        }
        return host;
    }
    
    /**
     * 
     * @param id
     * @return
     */
    public static String parseResource(String id) {
        String resource = null;
        if(id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            // Ensure that the name delimiter is there first.
            if(delimPos >= 0) {
                int start = delimPos+1;
                delimPos = id.indexOf(RESOURCE_DELIM);
                if(delimPos > start) {
                    delimPos++;
                    if(delimPos < id.length()) {
                        resource = id.substring(delimPos);
                    }
                }
            }
        }
        return resource;
    }
    
    
    public static final void main(String [] args) {
        
        String [] ids = { "ghi@awipscm.omaha.us.ray.com:2562/resource",
                "ghi@awipscm.omaha.us.ray.com:2562/",
                "ghi@awipscm.omaha.us.ray.com/",
                "ghi@awipscm.omaha.us.ray.com",
        };
        
        for(String id : ids) {
            System.out.println(parseName(id));
            System.out.println(parseHost(id));
            System.out.println(parsePort(id));
            System.out.println(parseResource(id));
            System.out.println("-------------------------------------");
        }
    }
    
    
    
    
    
    
}
