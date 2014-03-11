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
package com.raytheon.edex.plugin.redbook.ingest.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * Button type xml object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 07, 2014    2858    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ButtonXML {

    /**
     * Button type
     */
    @XmlAttribute(name = "type")
    protected String type;

    /**
     * Awips 1 id
     */
    @XmlAttribute(name = "A1Id")
    protected String a1Id;

    /**
     * The menu text
     */
    @XmlAttribute(name = "menuText")
    protected String menuText;

    /**
     * A unique id for the menu
     */
    @XmlAttribute(name = "id")
    protected String id;

    /**
     * List of substitution items
     */
    @XmlElements({ @XmlElement(name = "substitute", type = SubstituteXML.class) })
    protected List<SubstituteXML> substitutionList = new ArrayList<SubstituteXML>();

    /**
     * Default constructor
     */
    public ButtonXML() {

    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the a1Id
     */
    public String getA1Id() {
        return a1Id;
    }

    /**
     * @param a1Id
     *            the a1Id to set
     */
    public void setA1Id(String a1Id) {
        this.a1Id = a1Id;
    }

    /**
     * @return the menuText
     */
    public String getMenuText() {
        return menuText;
    }

    /**
     * @param menuText
     *            the menuText to set
     */
    public void setMenuText(String menuText) {
        this.menuText = menuText;
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the substitutionList
     */
    public List<SubstituteXML> getSubstitutionList() {
        return substitutionList;
    }

    /**
     * @param substitutionList
     *            the substitutionList to set
     */
    public void setSubstitutionList(List<SubstituteXML> substitutionList) {
        this.substitutionList = substitutionList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final String nl = System.getProperty("line.separator");
        StringBuilder sb = new StringBuilder();
        sb.append("  A1Id: ").append(this.getA1Id()).append(nl);
        sb.append("  ID:   ").append(this.getId()).append(nl);
        sb.append("  Text: ").append(this.getMenuText()).append(nl);
        sb.append("  Type: ").append(this.getType()).append(nl);
        for (SubstituteXML xml : substitutionList) {
            sb.append(xml.toString());
        }

        return sb.toString();
    }
}
