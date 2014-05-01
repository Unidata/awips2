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

import java.io.File;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Utilized in the index file, provides an include capability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2009            chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "include")
public class CommonIncludeMenuItem implements ISerializableObject {

    /**
     * The file to include (relative to localization)
     */
    @XmlAttribute(name = "fileName", required = true)
    public File fileName;

    /**
     * The variable substitutions
     */
    @XmlElement(name = "substitute", required = false)
    public VariableSubstitution[] substitutions;

    /**
     * The items (identified by id) to remove from the include
     */
    @XmlElement(name = "remove")
    public String[] removals;

    /**
     * The eclipse menu URI to install the contributions to
     */
    @XmlAttribute(name = "installTo")
    public String installationLocation;

    /**
     * Determines which action sets to activate visibility
     */
    @XmlElement(name = "visibleOnActionSet")
    public String[] visibleOnActionSet;

    /**
     * Indicates to install the contributions as a subMenu of this name, instead
     * of inline (optional)
     */
    @XmlAttribute(name = "subMenu")
    public String subMenuName;
}
