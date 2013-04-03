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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * 
 * Represents a bidirectional map for going between base names and aliases.
 * Allows for caseInsensitive aliases since some naming conventions are ambigous
 * on case. The base names cannot be treated case insensitive because this would
 * cause ambiguity and require case insensitive handling of base names in all
 * namespaces.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AliasNamespace {

    protected final boolean caseSensitive;

    /**
     * Map an alias name to base names
     */
    protected Map<String, Set<String>> alias2base = new HashMap<String, Set<String>>();

    /**
     * maps base names to alias names.
     */
    protected Map<String, Set<String>> base2alias = new HashMap<String, Set<String>>();

    public AliasNamespace(AliasList aliasList) {
        this.caseSensitive = aliasList.isCaseSensitive();
        int mapSize = (int) (aliasList.getAliasList().size() / 0.75) + 1;
        alias2base = new HashMap<String, Set<String>>(mapSize, 0.75f);
        base2alias = new HashMap<String, Set<String>>(mapSize, 0.75f);
        for (Alias def : aliasList.getAliasList()) {
            String alias = def.getAlias();
            if (!caseSensitive) {
                alias = alias.toLowerCase();
            }
            String base = def.getBase();
            Set<String> baseSet = alias2base.get(alias);
            if (baseSet == null) {
                baseSet = new HashSet<String>();
                alias2base.put(alias, baseSet);
            }
            baseSet.add(base);
            Set<String> aliasSet = base2alias.get(base);
            if (aliasSet == null) {
                aliasSet = new HashSet<String>();
                base2alias.put(base, aliasSet);
            }
            aliasSet.add(alias);
        }
    }

    public Set<String> lookupBaseNames(String alias) {
        if (!caseSensitive) {
            alias = alias.toLowerCase();
        }
        Set<String> base = alias2base.get(alias);
        if (base == null) {
            return base;
        }
        return Collections.unmodifiableSet(base);
    }

    public Set<String> lookupAliases(String base) {
        Set<String> alias = base2alias.get(base);
        if (alias == null) {
            return alias;
        }
        return Collections.unmodifiableSet(alias);
    }

}