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
package com.raytheon.uf.common.util.mapping;

import java.util.Set;

/**
 * Exception thrown when trying to map an alias or baseName to exactly one name
 * and multiple definitions are defined.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class MultipleMappingException extends Exception {

    private static final long serialVersionUID = -376467583718598176L;

    private final boolean base;

    private final String name;

    private final String namespace;

    private final Set<String> mappings;

    public MultipleMappingException(boolean base, String name,
            String namespace, Set<String> mappings) {
        super();
        this.base = base;
        this.name = name;
        this.namespace = namespace;
        this.mappings = mappings;
    }

    public boolean isBase() {
        return base;
    }

    public String getName() {
        return name;
    }

    public String getNamespace() {
        return namespace;
    }

    /**
     * Get all the defined mappings.
     * 
     * @return
     */
    public Set<String> getMappings() {
        return mappings;
    }

    /**
     * Get one of the mappings, no guarantee as to which one.
     * 
     * @return
     */
    public String getArbitraryMapping() {
        return mappings.iterator().next();
    }

    @Override
    public String getMessage() {
        StringBuilder message = new StringBuilder(100);
        if (base) {
            message.append("Base name(");
        } else {
            message.append("Alias name(");
        }
        message.append(name);
        message.append(") maps to multiple names(");
        boolean first = true;
        for (String name : mappings) {
            if (first) {
                first = false;
            } else {
                message.append(", ");
            }
            message.append(name);
        }
        message.append(") in ");
        message.append(namespace);
        message.append(" when only one mapping is expected.");
        return message.toString();
    }

}
