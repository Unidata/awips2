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
package com.raytheon.viz.aviation.xml;

import java.util.ArrayList;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElements;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * 
 * Class containing code for the Array of ViewerTabConfig data read in from XML.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2009             lvenable    Initial creation
 * Sep 15, 2015 4880       njensen     Removed ISerializableObject
 *
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@XmlRootElement(name = "TafViewerEditorConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class TafViewerEditorConfig {
    /**
     * Array of ViewerTabConfig data.
     */
    @XmlElements({ @XmlElement(name = "ViewerTabConfig", type = ViewerTabConfig.class) })
    private ArrayList<ViewerTabConfig> viewerTabs;

    /**
     * Constructor.
     */
    public TafViewerEditorConfig() {
    }

    /**
     * Get the viewer tab configuration data.
     * 
     * @return Return an array of ViewerTabConfig data.
     */
    public ArrayList<ViewerTabConfig> getViewerTabs() {
        return viewerTabs;
    }

    /**
     * Set the viewer tab configuration data array.
     * 
     * @param viewerTabs
     *            An array of ViewerTabConfig data.
     */
    public void setViewerTabs(ArrayList<ViewerTabConfig> viewerTabs) {
        this.viewerTabs = viewerTabs;
    }
}
