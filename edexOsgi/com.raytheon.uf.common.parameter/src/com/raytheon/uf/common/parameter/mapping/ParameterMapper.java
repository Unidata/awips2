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
package com.raytheon.uf.common.parameter.mapping;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.ParameterDefinitions;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * This class can be used to map parameters between different parameter naming
 * conventions. There is a system wide base naming convention defined in the
 * parameter definition file. All other naming conventions can be defined as a
 * set of abbreviations that alias these base parameters. The aliases for a
 * single naming convention are grouped within a namespace.
 * 
 * For any namespace the base parameter that matches an abbreviation can be
 * found by looking up the parameter in the namespace. If the namespace is
 * unknown it is also possible to do a full search of all namespaces to find any
 * parameter that matches an abbreviation.
 * 
 * Given a base parameter, it is also possible to lookup an alias within a given
 * namespace.
 * 
 * There are two namespaces that recieve special treatment. The "base" namespace
 * will contain all parameter abbreviations from the parameter definition files.
 * The "deprecated" namespace should contain any parameters that were at one
 * time in base but have been replaced. If any class is doing lookups based off
 * standard parameters, they may want to check "deprecated" to determine if the
 * base set has changed.
 * 
 * Here are some examples of how this class might be used:
 * 
 * 1) To look up a base parameter that is defined by the TMP abbreviation in the
 * grib namespace:<br>
 * ParameterMapper.getInstance().lookupParameter("grib","TMP");
 * 
 * 2) To look up a base parameter that is defined by the "TMP" abbreviation in
 * any namespace:<br>
 * ParameterMapper.getInstance().lookupParameter("TMP");
 * 
 * 3) To find the parameter abbreviation in the "cf" namespace that matches the
 * "TMP" abbreviation from the grib namespace<br>
 * Parameter p = ParameterMapper.getInstance().lookupParameter("grib","TMP");<br>
 * String cfAbbrev = ParameterMapper.getInstance().lookupAlias(p,"cf");<br>
 * 
 * 4) To check if the "T" is a base parameter or a deprecated parameter:<br>
 * 
 * <pre>
 * Parameter p = ParameterMapper.getInstance().lookupParameter("base","T");
 * if (p != null){
 *      System.out.println("T is a base parameter");
 * }else{
 *      p = ParameterMapper.getInstance().lookupParameter("deprecated","T");
 *      if(p != null){
 *          System.out.println("T is deprecated, it has been replaced with " + p.getAbbreviation());
 *      }
 * }<br>
 * </pre>
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

public class ParameterMapper {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterMapper.class);

    private Map<String, Parameter> map = new HashMap<String, Parameter>();

    private Map<String, ParameterAliasList> namespaceMap = new HashMap<String, ParameterAliasList>();

    private ParameterMapper() {
        Unmarshaller unmarshaller = null;
        try {
            JAXBContext context = JAXBContext
                    .newInstance(ParameterAliasList.class);
            unmarshaller = context.createUnmarshaller();
        } catch (JAXBException e) {
            statusHandler
                    .error("Error creating Context for parameter mapper, no parameter aliases will be used.",
                            e);
        }
        if (unmarshaller != null) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext commonStaticBase = pathMgr.getContext(
                    LocalizationContext.LocalizationType.COMMON_STATIC,
                    LocalizationContext.LocalizationLevel.BASE);

            LocalizationContext commonStaticSite = pathMgr.getContext(
                    LocalizationContext.LocalizationType.COMMON_STATIC,
                    LocalizationContext.LocalizationLevel.SITE);

            // First fill the map with well defined parameters.
            ParameterAliasList baseList = new ParameterAliasList("base");
            for (Parameter p : ParameterDefinitions.getParameters()) {
                baseList.addAlias(new ParameterAlias(p.getAbbreviation(), p
                        .getAbbreviation()));
                map.put(p.getAbbreviation(), p);
            }
            // read in the namespace map
            LocalizationFile[] files = pathMgr.listFiles(
                    new LocalizationContext[] { commonStaticSite,
                            commonStaticBase }, "parameter"
                            + IPathManager.SEPARATOR + "alias",
                    new String[] { ".xml" }, true, true);
            List<ParameterAliasList> aliasLists = new ArrayList<ParameterAliasList>(
                    files.length);
            for (LocalizationFile file : files) {
                if (file == null || !file.exists()
                        || file.getFile().length() < 0) {
                    continue;
                }
                Object obj = null;
                try {
                    obj = unmarshaller.unmarshal(file.getFile());
                } catch (JAXBException e) {
                    statusHandler.error("Error reading parameter aliases: "
                            + file.getName() + " has been ignored.", e);
                }
                if (obj instanceof ParameterAliasList) {
                    ParameterAliasList list = (ParameterAliasList) obj;
                    aliasLists.add(list);
                    if (list.getNamespace() != null) {
                        ParameterAliasList realList = namespaceMap.get(list
                                .getNamespace());
                        if (realList == null) {
                            namespaceMap.put(list.getNamespace(), list);
                        } else {
                            realList.merge(list);
                            aliasLists.remove(list);
                        }
                    }
                } else if (obj != null) {
                    statusHandler.error("Error reading parameter aliases: "
                            + file.getName() + " was a "
                            + obj.getClass().getSimpleName());
                }
            }
            namespaceMap.put(baseList.getNamespace(), baseList);
            aliasLists.add(baseList);
            // Fill in lower case entries for a case insensitve namespace map
            Map<String, ParameterAliasList> lowerCaseNSMap = new HashMap<String, ParameterAliasList>();
            for (Entry<String, ParameterAliasList> entry : namespaceMap
                    .entrySet()) {
                String lowerCase = entry.getKey().toLowerCase();
                if (!namespaceMap.containsKey(lowerCase)
                        && !lowerCaseNSMap.containsKey(lowerCase)) {
                    lowerCaseNSMap.put(lowerCase, entry.getValue());
                }
            }
            namespaceMap.putAll(lowerCaseNSMap);
            // If there is a deprecated namespace it should take priority over
            // all other.
            ParameterAliasList depList = namespaceMap.get("deprecated");
            if (depList != null) {
                resolve(depList);
                aliasLists.remove(depList);
            }
            // Add all other namespaces to the map.
            for (ParameterAliasList list : aliasLists) {
                resolve(list);
            }
            // Finally fill in lower case entries for a case insensitve search
            Map<String, Parameter> lowerCaseMap = new HashMap<String, Parameter>();
            for (Entry<String, Parameter> entry : map.entrySet()) {
                String lowerCase = entry.getKey().toLowerCase();
                if (!map.containsKey(lowerCase)
                        && !lowerCaseMap.containsKey(lowerCase)) {
                    lowerCaseMap.put(lowerCase, entry.getValue());
                }
            }
            map.putAll(lowerCaseMap);
        }
    }

    private void resolve(ParameterAliasList list) {
        if (list.getParentNamespace() != null) {
            for (String parent : list.getParentNamespace()) {
                ParameterAliasList pl = namespaceMap.get(parent);
                if (pl != null) {
                    list.addParent(pl);
                }
            }
        }
        list.resolve(map);
    }

    /**
     * Find the parameter with an alias defined in the provided namespace.
     * 
     * @param namespace
     *            - the defined alias namespace to look for the parameter
     * @param alias
     *            - the name of an alias defined in the namespace
     * @return the defined parameter or null if the namespace or alias is
     *         undefined
     */
    public Parameter lookupParameter(String namespace, String alias) {
        if (alias == null) {
            return null;
        }
        ParameterAliasList list = namespaceMap.get(namespace);
        if (list == null) {
            list = namespaceMap.get(namespace.toLowerCase());
            if (list == null) {
                return null;
            }
        }
        Parameter p = list.lookupParameter(alias);
        if (p == null) {
            p = list.lookupParameterCaseInsensitive(alias);
        }
        return p;
    }

    /**
     * Look up a parameter from an abbreviation. First the base namespace is
     * searched, then the deprecated namespace, and then all other namespaces in
     * an unspecified order. If the abbreviation is not defined in any namespace
     * a case insensitive search is performed.
     * 
     * @param alias
     *            - an abbreviation for a parameter
     * @return The parameter that matches or is aliased to the provided alias or
     *         null if it is not found.
     */
    public Parameter lookupParameter(String alias) {
        Parameter p = map.get(alias);
        if (p == null) {
            p = map.get(alias.toLowerCase());
        }
        return p;
    }

    /**
     * Lookup an alias abbreviation for a parameter within a given namespace. If
     * multiple aliases for that parameter are in the given namespace, only one
     * will be returned.
     * 
     * @param parameter
     *            - The base parameter to find an alias for
     * @param namespace
     *            - The namespace in which to look for an alias.
     * @return an alias abbreviation or null if none is found.
     */
    public String lookupAlias(Parameter parameter, String namespace) {
        ParameterAliasList ns = namespaceMap.get(namespace);
        if (ns == null) {
            ns = namespaceMap.get(namespace.toLowerCase());
            if (ns == null) {
                return null;
            }
        }
        return ns.lookupAlias(parameter);
    }

    private static ParameterMapper instance;

    public static synchronized ParameterMapper getInstance() {
        if (instance == null) {
            instance = new ParameterMapper();
        }
        return instance;
    }

}
