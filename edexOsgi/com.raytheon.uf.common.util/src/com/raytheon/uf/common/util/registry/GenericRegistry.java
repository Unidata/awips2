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
package com.raytheon.uf.common.util.registry;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Base registry class, uses map to register objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2009            mschenke     Initial creation
 * May 31, 2013 2038      djohnson     Allow sub-classes to define the backing map.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GenericRegistry<T, S> {

    protected Map<T, S> registry;

    protected GenericRegistry() {
        this(new HashMap<T, S>());
    }

    /**
     * Constructor allowing sub-classes to define the map to use.
     * 
     * @param registry
     */
    protected GenericRegistry(Map<T, S> registry) {
        this.registry = registry;
    }

    public Object register(T t, S s) throws RegistryException {
        registry.put(t, s);
        return this;
    }

    public S getRegisteredObject(T t) {
        return registry.get(t);
    }

    public Set<T> getRegisteredObjects() {
        return registry.keySet();
    }
}
