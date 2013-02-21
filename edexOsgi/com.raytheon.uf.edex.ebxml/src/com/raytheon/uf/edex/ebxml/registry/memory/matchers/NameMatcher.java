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
package com.raytheon.uf.edex.ebxml.registry.memory.matchers;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.edex.ebxml.query.matcher.IMatcher;
import com.raytheon.uf.edex.ebxml.util.EbxmlUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class NameMatcher implements IMatcher {

    private final String value;

    private final String lang;

    /**
     * @param value
     *            non-null string value.
     */
    public NameMatcher(String value) {
        this(value, "en-US");
    }

    public NameMatcher(String value, String lang) {
        this.value = value;
        this.lang = lang;
    }

    /** {@inheritDoc} */
    @Override
    public boolean matches(RegistryObjectType obj) {
        String name = EbxmlUtil.getLocalizedString(obj.getName(), lang);
        if (name != null && name.equals(value)) {
            return true;
        }
        return false;
    }

}
