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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Describes a bundle contribution
 * 
 * <HR>
 * The item should consist of, at minimum:
 * <UL>
 * <LI>The menu text
 * <LI>The ID - which allows itmes to be systematically removed in the index
 * file
 * <LI>The bundle file to load (should be relative to localization)
 * <LI>One or more dataURIs (if you want menu times to update)
 * </UL>
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
@XmlType(name = "bundleItem")
@XmlAccessorType(XmlAccessType.NONE)
public class CommonBundleMenuContribution extends
        CommonAbstractMenuContribution {

    /**
     * Indicates whether the bundle should be extracted for metadata and time
     * queried to show available times
     */
    @XmlAttribute(name = "timeQuery")
    public Boolean timeQuery = true;

    /** The text displayed on the menu item */
    @XmlAttribute(name = "menuText")
    public String text;

    /** The bundle file to load */
    @XmlAttribute(name = "file")
    public String bundleFile;

    /** The type of editor to use (otherwise uses default) */
    @XmlAttribute(name = "editorType", required = false)
    public String editorType;

    /** A set of one or more dataURIs to utilize for the time in the menu */
    @XmlElement(name = "dataURI")
    public String[] dataURIs;

    /** A set of variable substitutions to apply to the bundle (optional) */
    @XmlElement(name = "substitute", required = false)
    public VariableSubstitution[] substitutions;

    /** The interval to round the dataTimes to, in seconds (optional) */
    @XmlAttribute(name = "productInterval", required = false)
    public Integer productInterval;

    /**
     * Indicates whether to do a full bundle load, rather than an incremental
     * load (default)
     */
    @XmlAttribute(name = "fullBundleLoad", required = false)
    public Boolean fullBundleLoad;

    /**
     * Indicates whether reference time should be used, instead of valid time
     * (optional, defaults to true)
     */
    @XmlAttribute(name = "useReferenceTime", required = false)
    public boolean useReferenceTime = true;

    /**
     * The product offset in seconds (optional)
     */
    @XmlAttribute(name = "productOffset", required = false)
    public Integer productOffset;

    /**
     * Indicates a command to execute after loading the bundle
     */
    @XmlAttribute(name = "commandId", required = false)
    public String command;
}
