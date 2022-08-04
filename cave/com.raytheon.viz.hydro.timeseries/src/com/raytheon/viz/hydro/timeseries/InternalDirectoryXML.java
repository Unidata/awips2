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
package com.raytheon.viz.hydro.timeseries;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * XML config object holding internal directories so the shef issue process from
 * Time Series can send the shef file internally.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2011            mpduff      Initial creation
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 *
 * </pre>
 *
 * @author mpduff
 */
@XmlAccessorType(XmlAccessType.NONE)
public class InternalDirectoryXML {
    @XmlElements({ @XmlElement(name = "directory", type = String.class) })
    private List<String> directories = new ArrayList<>();

    public InternalDirectoryXML() {

    }

    public void setDirectories(List<String> directories) {
        this.directories = directories;
    }

    public List<String> getDirectories() {
        return directories;
    }
}
