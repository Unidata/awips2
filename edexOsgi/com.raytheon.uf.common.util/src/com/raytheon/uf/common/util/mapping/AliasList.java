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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * A AliasList represents a set of alternative names for a base name. This list
 * defines the alternatives within a namespace so that it is possible to match
 * aliases for only a specific group. Parent namespaces can be used to bring all
 * aliases from another list into this namespace.
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
public class AliasList {

    /**
     * A name for this group of parameter aliases
     */
    @XmlAttribute
    private String namespace;

    /**
     * A name for this group of parameter aliases
     */
    @XmlAttribute
    private boolean caseSensitive = true;

    /**
     * The actual alias definitions for the list
     */
    @XmlElement(name = "alias")
    private List<Alias> aliasList = new ArrayList<Alias>();

    public AliasList() {
    }

    public AliasList(String name) {
        this.namespace = name;
    }

    public String getNamespace() {
        return namespace;
    }

    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    public List<Alias> getAliasList() {
        return aliasList;
    }

    public void setAliasList(List<Alias> aliasList) {
        this.aliasList = aliasList;
    }

    public void addAlias(Alias alias) {
        this.aliasList.add(alias);
    }

    public boolean isCaseSensitive() {
        return caseSensitive;
    }

    public void setCaseSensitive(boolean caseSensitive) {
        this.caseSensitive = caseSensitive;
    }

}