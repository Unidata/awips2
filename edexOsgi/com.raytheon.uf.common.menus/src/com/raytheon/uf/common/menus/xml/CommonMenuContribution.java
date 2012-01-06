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
package com.raytheon.uf.common.menus.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@XmlType(name = "menuItem")
@XmlAccessorType(XmlAccessType.NONE)
public class CommonMenuContribution extends CommonAbstractMenuContribution {
    /** The unique key for the menu item */
    @XmlAttribute(name = "key", required = true)
    public String key;

    /** The text displayed on the menu item */
    @XmlAttribute(name = "menuText", required = false)
    public String menuText = "Missing Text";

    /** The mapping for the text */
    @XmlAttribute(name = "textLookup", required = false)
    public String textLookup = "None";

    /** Flag indicating the menuText should be indented */
    @XmlAttribute(name = "indentText", required = false)
    public Boolean indentText = false;

    /** The scale at which the menu is to be displayed */
    @XmlAttribute(name = "mapScale", required = false)
    public String mapScale = "";
}
