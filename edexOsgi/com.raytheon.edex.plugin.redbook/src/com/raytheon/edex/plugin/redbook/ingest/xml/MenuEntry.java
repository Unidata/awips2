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
 * MenuEntry JaxB object for the Redbook user created NDM XML files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2014    2855    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MenuEntry {

    /**
     * Entry type.
     */
    @XmlAttribute(name = "type")
    protected MenuEntryType type;

    /**
     * Text value.
     */
    @XmlAttribute(name = "text")
    protected String text;

    /**
     * Unique id.
     */
    @XmlAttribute(name = "id")
    protected String id;

    /**
     * Key if entry is of type substitute.
     */
    @XmlAttribute(name = "key")
    protected String key;

    /**
     * Value if entry is of type substitute.
     */
    @XmlAttribute(name = "value")
    protected String value;

    /**
     * Bundle file if type is productButton
     */
    @XmlAttribute(name = "file")
    protected String file = "bundles/Redbook.xml";

    /**
     * List of MenuEntry items
     */
    @XmlElements({ @XmlElement(name = "menuEntry", type = MenuEntry.class) })
    protected List<MenuEntry> menuEntryList = new ArrayList<MenuEntry>();

    /**
     * Default constructor.
     */
    public MenuEntry() {

    }

    public MenuEntry(MenuEntryType type) {
        this.type = type;
    }

    /**
     * @return the type
     */
    public MenuEntryType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(MenuEntryType type) {
        this.type = type;
    }

    /**
     * @return the text
     */
    public String getText() {
        return text;
    }

    /**
     * @param text
     *            the text to set
     */
    public void setText(String text) {
        this.text = text;
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
     * @return the key
     */
    public String getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * @return the value
     */
    public String getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * @return the file
     */
    public String getFile() {
        return file;
    }

    /**
     * @param file
     *            the file to set
     */
    public void setFile(String file) {
        this.file = file;
    }

    /**
     * @return the menuEntryList
     */
    public List<MenuEntry> getMenuEntryList() {
        return menuEntryList;
    }

    /**
     * @param menuEntryList
     *            the menuEntryList to set
     */
    public void setMenuEntryyList(List<MenuEntry> menuEntryList) {
        this.menuEntryList = menuEntryList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final String nl = System.getProperty("line.separator");
        StringBuilder sb = new StringBuilder("Menu Entry:  ");
        if (type != null) {
            sb.append("Type:  ").append(this.type).append(nl);
        } else {
            sb.append(nl);
        }
        if (text != null) {
            sb.append("  Text:  ").append(this.text).append(nl);
        }
        if (id != null) {
            sb.append("  ID:    ").append(this.id).append(nl);
        }
        if (key != null) {
            sb.append("  Key:   ").append(this.key).append(nl);
        }
        if (value != null) {
            sb.append("  Value: ").append(this.value).append(nl);
        }

        sb.append("  File:  ").append(this.file).append(nl);

        for (MenuEntry sub : menuEntryList) {
            sb.append(sub.toString());
        }

        return sb.toString();
    }
}
