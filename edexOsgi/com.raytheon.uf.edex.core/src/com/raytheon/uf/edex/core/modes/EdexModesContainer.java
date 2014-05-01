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
package com.raytheon.uf.edex.core.modes;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Holds the modes deserialized from XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2010            njensen     Initial creation
 * May 29, 2013 1989       njensen     Removed high mem mode
 * Dec 05, 2013 2566       bgonzale    Migrated to edex.core.modes package.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlRootElement(name = "edexModes")
@XmlAccessorType(XmlAccessType.NONE)
public class EdexModesContainer {

    @XmlElements({ @XmlElement(name = "mode") })
    private ArrayList<EdexMode> modes;

    public EdexModesContainer() {
        modes = new ArrayList<EdexMode>();
    }

    public ArrayList<EdexMode> getModes() {
        return modes;
    }

    public void setModes(ArrayList<EdexMode> modes) {
        this.modes = modes;
    }

    public EdexMode getMode(String name) {
        EdexMode ret = null;

        for (EdexMode m : modes) {
            if (m.getName().equalsIgnoreCase(name)) {
                ret = m;
                if (!ret.isInited()) {
                    ret.init();
                }
                break;
            }
        }
        return ret;
    }

}
