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
package com.raytheon.uf.common.grib;

import java.util.ArrayList;
import java.util.Collection;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * A JAXBable set of {@link GridRegion}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2016  5182       tjensen     Initial creation
 * Apr 19, 2016 5572       bsteffen    Move to common
 * 
 * </pre>
 * 
 * @author tjensen
 */
@XmlRootElement(name = "gribRegionSet")
@XmlAccessorType(XmlAccessType.NONE)
public class GridRegionSet {

    /**
     * List of regions for/from the XML.
     */
    @XmlElements({ @XmlElement(name = "region", type = GridRegion.class) })
    private ArrayList<GridRegion> regions;

    public ArrayList<GridRegion> getRegions() {
        return regions;
    }

    public void setRegions(ArrayList<GridRegion> regions) {
        this.regions = regions;
    }

    /**
     * Adds regions to this set
     * 
     * @param regions
     *            The regions to add
     */
    public void addRegions(Collection<GridRegion> regions) {
        if (this.regions == null) {
            this.regions = new ArrayList<GridRegion>();
        }
        this.regions.addAll(regions);
    }
}
