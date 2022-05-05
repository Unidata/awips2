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
package com.raytheon.edex.plugin.gfe.config;

import java.util.Collection;

/**
 * Routing configuration data objection for supplemental ISC databases. Allows
 * the list of defined weather elements to be additional mosaicked into the
 * specified database name using the specified edit area type.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 09, 2015  #4383     dgilling    Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class ISCRoutingConfig {

    private final Collection<String> parmNames;

    private final String dbName;

    private final String editAreaPrefix;

    public ISCRoutingConfig(Collection<String> parmNames, String dbName,
            String editAreaPrefix) {
        this.parmNames = parmNames;
        this.dbName = dbName;
        this.editAreaPrefix = editAreaPrefix;
    }

    public Collection<String> getParmNames() {
        return parmNames;
    }

    public String getDbName() {
        return dbName;
    }

    public String getEditAreaPrefix() {
        return editAreaPrefix;
    }
}
