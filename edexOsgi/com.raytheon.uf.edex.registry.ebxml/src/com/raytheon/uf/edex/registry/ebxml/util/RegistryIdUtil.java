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
package com.raytheon.uf.edex.registry.ebxml.util;

import java.util.List;

import com.raytheon.uf.common.util.ClusterIdUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;

/**
 * Common Utility class specifically for Registry that currently just
 * returns the Cluster ID.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2014 2771       bgonzale     Initial creation
 * Apr 23, 2014 2992       dhladky      General way to get all registries.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class RegistryIdUtil {

    // connection to the registry DAO object
    private RegistryObjectDao rdo;

    // public constructor
    public RegistryIdUtil() {
        
    }
    
    // public spring constructor
    public RegistryIdUtil(RegistryObjectDao rdo) {
        this.rdo = rdo;
    }
    
    /**
     * Return the Registry ID for this running instance of Registry.
     * 
     * @return Registry ID.
     */
    public static String getId() {
        return ClusterIdUtil.getId();
    }
    
    /**
     * Gets the list of unique registry nodes that have data in this node.
     * @return
     */
    public List<String> getUniqueRegistries(String objectType) {
       return rdo.getUniqueRegistries(objectType);
    }
  
}
