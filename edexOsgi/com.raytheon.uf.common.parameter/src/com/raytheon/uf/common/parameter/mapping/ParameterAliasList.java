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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.parameter.Parameter;

/**
 * 
 * A ParameterAliasList represents a set of alternative abbreviations for a
 * parameter. This list defines the alternatives within a namespace so that it
 * is possible to match aliases for only a specific group. Parent namespaces can
 * be used to bring all aliases from another list into this namespace.
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
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class ParameterAliasList {

    /**
     * The parent namespace is used if a parameter lookup within this namespace
     * fails.
     */
    @XmlAttribute
    private List<String> parentNamespace;

    /**
     * A name for this group of parameter aliases
     */
    @XmlAttribute
    private String namespace;

    /**
     * The actual alias definitions for the list
     */
    @XmlElement(name = "alias")
    private List<ParameterAlias> aliasList = new ArrayList<ParameterAlias>();

    /**
     * Map an alias name to a parameter
     */
    private Map<String, Parameter> map = new HashMap<String, Parameter>();

    /**
     * maps a lower case alias name to a parameter for case insensitive searches
     */
    private Map<String, Parameter> lowerCaseMap = new HashMap<String, Parameter>();

    /**
     * maps parameters to thier alias name
     */
    private Map<Parameter, String> reverseMap = new HashMap<Parameter, String>();

    /**
     * a list of the parent list corresponding to the parent namespaces.
     */
    private List<ParameterAliasList> parentList = new ArrayList<ParameterAliasList>();

    public ParameterAliasList() {
    }

    public ParameterAliasList(String name) {
        this.namespace = name;
    }

    public List<String> getParentNamespace() {
        return parentNamespace;
    }

    public void setParentNamespace(List<String> parentNamespace) {
        this.parentNamespace = parentNamespace;
    }

    public String getNamespace() {
        return namespace;
    }

    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    public List<ParameterAlias> getAliasList() {
        return aliasList;
    }

    public void setAliasList(List<ParameterAlias> aliasList) {
        this.aliasList = aliasList;
    }

    public void addAlias(ParameterAlias alias) {
        this.aliasList.add(alias);
    }

    public Parameter lookupParameter(String alias) {
        Parameter p = map.get(alias);
        if (p == null) {
            // If I don't have it then check my parents.
            for (ParameterAliasList parent : parentList) {
                p = parent.lookupParameter(alias);
                if (p != null) {
                    break;
                }
            }
        }
        return p;
    }

    public Parameter lookupParameterCaseInsensitive(String alias) {
        Parameter p = lowerCaseMap.get(alias.toLowerCase());
        if (p == null) {
            // If I don't have it then check my parents.
            for (ParameterAliasList parent : parentList) {
                p = parent.lookupParameterCaseInsensitive(alias);
                if (p != null) {
                    break;
                }
            }
        }
        return p;
    }

    public String lookupAlias(Parameter parameter) {
        String alias = reverseMap.get(parameter);
        if (alias == null) {
            for (ParameterAliasList parent : parentList) {
                alias = parent.lookupAlias(parameter);
                if (alias != null) {
                    break;
                }
            }
        }
        return alias;
    }

    /**
     * @param pl
     */
    public void addParent(ParameterAliasList parent) {
        parentList.add(parent);
    }

    public void resolve(Map<String, Parameter> globalMap) {
        for (ParameterAlias alias : aliasList) {
            Parameter parameter = globalMap.get(alias.getBase());
            if (parameter == null) {
                continue;
            }
            map.put(alias.getAlias(), parameter);
            lowerCaseMap.put(alias.getAlias().toLowerCase(), parameter);
            reverseMap.put(parameter, alias.getAlias());
            if (!globalMap.containsKey(alias.getAlias())) {
                globalMap.put(alias.getAlias(), parameter);
            }
        }
    }

    public void merge(ParameterAliasList list) {
        if (list.getParentNamespace() != null) {
            for (String parent : list.getParentNamespace()) {
                if (!parentNamespace.contains(parent)) {
                    parentNamespace.add(parent);
                }
            }
        }
        if (list.getAliasList() != null) {
            for (ParameterAlias alias : list.getAliasList()) {
                aliasList.add(alias);
            }
        }
    }

}