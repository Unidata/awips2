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
package com.raytheon.uf.common.menus.vb;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 *
 * Description of a single source for the volume browser.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Jan 06, 2011           bsteffen    Initial creation
 * Dec 11, 2013  2602     bsteffen    Remove ISerializableObject.
 * Aug 14, 2014  3506     mapeters    Added remove field and equals
 *                                    and hashCode functions.
 * Jul 10, 2015  4641     mapeters    Added toString().
 * Feb 08, 2018  6355     nabowle     Move to a common plugin.
 *
 * </pre>
 *
 * @author bsteffen
 * @see VbSourceList
 */
@XmlAccessorType(XmlAccessType.NONE)
public class VbSource {

    @XmlAttribute(required = true)
    private String key;

    @XmlAttribute(required = false)
    private String name;

    @XmlAttribute(required = true)
    private String category;

    @XmlAttribute(required = false)
    private List<ViewMenu> views;

    @XmlAttribute(required = false)
    private String subCategory;

    @XmlAttribute(required = false)
    private boolean remove;

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
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the category
     */
    public String getCategory() {
        return category;
    }

    /**
     * @param category
     *            the category to set
     */
    public void setCategory(String category) {
        this.category = category;
    }

    /**
     * @return the views
     */
    public List<ViewMenu> getViews() {
        return views;
    }

    /**
     * @param views
     *            the views to set
     */
    public void setViews(List<ViewMenu> views) {
        this.views = views;
    }

    /**
     * @return the subCategory
     */
    public String getSubCategory() {
        return subCategory;
    }

    /**
     * @param subCategory
     *            the subCategory to set
     */
    public void setSubCategory(String subCategory) {
        this.subCategory = subCategory;
    }

    /**
     * @return whether or not this source is to be removed
     */
    public boolean getRemove() {
        return remove;
    }

    /**
     * @param remove
     *            the remove status to set
     */
    public void setRemove(boolean remove) {
        this.remove = remove;
    }

    @Override
    public boolean equals(Object that) {
        if (that instanceof VbSource) {
            if ((this.key.equals(((VbSource) that).getKey())
                    && (this.getCategory().compareTo(
                            ((VbSource) that).getCategory()) == 0))) {
                return true;
            }
        }
        return false;
    }

    @Override
    public int hashCode() {
        String newKey = key.concat(category);
        return newKey.hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sourceString = new StringBuilder("VbSource[");
        sourceString.append("key=").append(key);
        sourceString.append(", ");
        sourceString.append("name=").append(name);
        sourceString.append(", ");
        sourceString.append("category=").append(category);
        sourceString.append(", ");
        sourceString.append("subCategory=").append(subCategory);
        sourceString.append("]");

        return sourceString.toString();
    }
}
