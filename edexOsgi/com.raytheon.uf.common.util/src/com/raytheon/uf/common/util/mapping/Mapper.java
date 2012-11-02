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

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 * This class can be used to map names between different naming conventions.
 * 
 * Names are grouped into namespaces which allows mapping from any namespace
 * into the "base" namespace. The "base" namespace does not actually exist but
 * it is a theoretical namespace that other things map into, often defined by
 * the contents of a file or database table depending on the implementation.
 * 
 * The "deprecated" namespace is handled specially. This namespace will contain
 * any names that were previously base names but have since changed. When any
 * mapping is performed, if the base name is deprecated it will map correctly to
 * the new base. Over time it is expected that all mappings and code will be
 * updated and any deprecated entries can be removed.
 * 
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
public abstract class Mapper {

    /**
     * The name of the special deprecated namespace
     */
    public static final String DEPRECATED = "deprecated";

    private static Unmarshaller unmarshaller;

    private Map<String, AliasNamespace> namespaceMap = new HashMap<String, AliasNamespace>();

    protected void addAliasList(AliasList list) {
        namespaceMap.put(list.getNamespace(), new AliasNamespace(list));
    }

    protected void addAliasList(File file) throws JAXBException {
        if (file == null || !file.exists() || file.length() < 0) {
            return;
        }
        Unmarshaller unmarshaller = Mapper.unmarshaller;
        if (unmarshaller == null) {
            // This will be safe if multiple threads get in before the sync
            // block.
            JAXBContext context = JAXBContext.newInstance(AliasList.class,
                    Alias.class);
            unmarshaller = context.createUnmarshaller();
        }
        synchronized (unmarshaller) {
            Object obj = unmarshaller.unmarshal(file);
            if (obj instanceof AliasList) {
                addAliasList((AliasList) obj);
            }
        }
        Mapper.unmarshaller = unmarshaller;
    }

    protected Set<String> lookupBaseNames(String alias, String namespace,
            boolean defaultUseAlias) {
        AliasNamespace list = namespaceMap.get(namespace);
        Set<String> baseNames = null;
        if (list != null) {
            baseNames = list.lookupBaseNames(alias);
        }
        if (baseNames == null) {
            if (defaultUseAlias) {
                baseNames = new HashSet<String>(Arrays.asList(alias));
            } else {
                return null;
            }
        }
        AliasNamespace deprecated = namespaceMap.get(DEPRECATED);
        if (deprecated != null) {
            Set<String> newBaseNames = new HashSet<String>(
                    (int) (baseNames.size() / 0.75) + 1, 0.75f);
            for (String name : baseNames) {
                Set<String> undepNames = deprecated.lookupBaseNames(name);
                if (undepNames == null) {
                    newBaseNames.add(name);
                } else {
                    newBaseNames.addAll(baseNames);
                }
            }
            // We don't really need to make this unmodifiable, but if we don't
            // and people start modifying the result then everything breaks if
            // there are no deprecated names.
            baseNames = Collections.unmodifiableSet(newBaseNames);
        }
        return baseNames;
    }

    /**
     * Lookup all the baseNames associated with the given alias in a namespace.
     * If no baseNames are defined the alias is returned.
     * 
     * @param namespace
     *            - the defined alias namespace to look for the name
     * @param alias
     *            - the name of an alias defined in the namespace
     * @return the base names or the alias if the namespace or alias is
     *         undefined
     */
    public Set<String> lookupBaseNames(String alias, String namespace) {
        return lookupBaseNames(alias, namespace, true);
    }

    /**
     * Lookup all the baseNames associated with the given alias in a namespace.
     * If no baseNames are defined null is returned.
     * 
     * @param namespace
     *            - the defined alias namespace to look for the name
     * @param alias
     *            - the name of an alias defined in the namespace
     * @return the base names or null if the namespace or alias is undefined
     */
    public Set<String> lookupBaseNamesOrNull(String alias, String namespace) {
        return lookupBaseNames(alias, namespace, false);
    }

    protected Set<String> lookupAliases(String base, String namespace,
            boolean defaultUseBase) {
        AliasNamespace ns = namespaceMap.get(namespace);
        Set<String> aliases = null;
        if (ns != null) {
            aliases = ns.lookupAliases(base);
        }
        if (aliases == null) {
            AliasNamespace deprecated = namespaceMap.get(DEPRECATED);
            if (deprecated != null) {
                Set<String> depNames = deprecated.lookupAliases(base);
                if (depNames != null) {
                    Set<String> newAliases = new HashSet<String>();
                    for (String depName : depNames) {
                        Set<String> depAliases = ns.lookupBaseNames(depName);
                        if (depAliases != null) {
                            newAliases.addAll(depAliases);
                        }
                    }
                    if (!newAliases.isEmpty()) {
                        aliases = newAliases;
                    } else if (defaultUseBase) {
                        aliases = depNames;
                    }
                }
            }
            if (aliases == null && defaultUseBase) {
                aliases = new HashSet<String>(Arrays.asList(base));
            }
        }
        return aliases;
    }

    /**
     * Lookup an alias name within a given namespace for a base name. If no
     * alias is defined then the baseName is returned
     * 
     * @param parameter
     *            - The base name to find an alias for
     * @param namespace
     *            - The namespace in which to look for an alias.
     * @return an alias abbreviation or the base name if none is found.
     */
    public Set<String> lookupAliases(String base, String namespace) {
        return lookupAliases(base, namespace, true);
    }

    /**
     * Lookup an alias name within a given namespace for a base name. If no
     * alias is defined then null is returned
     * 
     * @param parameter
     *            - The base name to find an alias for
     * @param namespace
     *            - The namespace in which to look for an alias.
     * @return an alias abbreviation or null if none is found.
     */
    public Set<String> lookupAliasesOrNull(String base, String namespace) {
        return lookupAliases(base, namespace, false);
    }

    /**
     * Provides same functionality as lookupBaseNames but is more convenient
     * when only alias is expected.
     * 
     * @param alias
     * @param namespace
     * @return
     * @throws MultipleMappingException
     */
    public String lookupBaseName(String alias, String namespace)
            throws MultipleMappingException {
        Set<String> baseNames = lookupBaseNames(alias, namespace);
        if (baseNames == null || baseNames.isEmpty()) {
            return null;
        } else if (baseNames.size() == 1) {
            return baseNames.iterator().next();
        } else {
            throw new MultipleMappingException(false, alias, namespace,
                    baseNames);
        }
    }

    /**
     * Provides same functionality as lookupAliases but is more convenient when
     * only one base name is expected.
     * 
     * @param base
     * @param namespace
     * @return
     * @throws MultipleMappingException
     */
    public String lookupAlias(String base, String namespace)
            throws MultipleMappingException {
        Set<String> aliases = lookupAliases(base, namespace);
        if (aliases == null) {
            return null;
        } else if (aliases.size() == 1) {
            return aliases.iterator().next();
        } else {
            throw new MultipleMappingException(true, base, namespace, aliases);
        }
    }

}
