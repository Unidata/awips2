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
package com.raytheon.viz.ghg.monitor.data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.viz.ghg.utilities.GhgUtilities;

/**
 * Contains the information for a single named filter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 17Jun2008    1157       MW Fegan    Initial creation.
 * 19Jun2008    1157       MW Fegan    Made cloneable.
 * 09Apr2009               wdougher    Added JAXB annotations.
 *                                     Initialized string arrays to empty
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
@DynamicSerialize
public class GhgDataFilter implements Cloneable, ISerializableObject {
    
    /* current hazards filter */
    @XmlElement
    public boolean currentHazards = false;

    /* filter name */
    @XmlElement
    public String name;

    /* selection criteria */
    @XmlElement
    public String[] actions;

    @XmlElement
    public String[] phenSigs;

    @XmlElement
    public String[] pils;

    @XmlElement
    public String[] wfos;

    @XmlElement
    public String[] geoids;

    @XmlElement
    public String[] etns;

    @XmlElement
    public String[] segs;

    /* record consolidations flags */
    @XmlElement
    public boolean combineGeoId;

    @XmlElement
    public boolean combineSegments;

    @XmlElement
    public boolean combinePurgeTimes;

    @XmlElement
    public boolean combineActions;

    /* Filter override flags */
    @XmlElement
    public boolean includeAlerts;

    @XmlElement
    public boolean includeMapSelections;

    @XmlElement
    public boolean includePastEvents;

    @XmlElement
    public boolean includeOrgPilEvents;
    
    /**
     * Constructor.
     */
    public GhgDataFilter() {
        name = "";
        String[] empty = new String[0];
        actions = empty;
        phenSigs = empty;
        pils = empty;
        wfos = empty;
        geoids = empty;
        etns = empty;
        segs = empty;
        combineGeoId = true;
        combineSegments = true;
        combinePurgeTimes = true;
        combineActions = true;
        includeAlerts = true;
        includeMapSelections = true;
        includePastEvents = false;
        includeOrgPilEvents = false;
    }

    /**
     * Copy constructor.
     * 
     * @param rhs
     *            the object to copy
     */
    public GhgDataFilter(GhgDataFilter rhs) {
        /* copy the filter name */
        name = new String(rhs.name);

        /* copy the current hazards filter flag */
        currentHazards = rhs.currentHazards;

        /* copy the filters */
        actions = GhgUtilities.arrayClone(rhs.actions);
        phenSigs = GhgUtilities.arrayClone(rhs.phenSigs);
        pils = GhgUtilities.arrayClone(rhs.pils);
        wfos = GhgUtilities.arrayClone(rhs.wfos);
        geoids = GhgUtilities.arrayClone(rhs.geoids);
        etns = GhgUtilities.arrayClone(rhs.etns);
        segs = GhgUtilities.arrayClone(rhs.segs);

        /* copy the record consolidation flags */
        combineGeoId = rhs.combineGeoId;
        combineSegments = rhs.combineSegments;
        combinePurgeTimes = rhs.combinePurgeTimes;
        combineActions = rhs.combineActions;
        
        /* copy the filter override flags */
        includeAlerts = rhs.includeAlerts;
        includeMapSelections = rhs.includeMapSelections;
        includePastEvents = rhs.includePastEvents;
        includeOrgPilEvents = rhs.includeOrgPilEvents;
    }

    /**
     * Convenience method for returning the values from a specific filter.
     * 
     * @param type
     *            the filter to return
     */
    public String[] getFilterByType(GhgConfigData.AlertsFilterEnum type) {
        switch (type) {
        case Action:
            return actions;
        case PhenSig:
            return phenSigs;
        case Pil:
            return pils;
        case WFO:
            return wfos;
        case GeoId:
            return geoids;
        case ETN:
            return etns;
        case Seg:
            return segs;
        }
        return null;
    }

    /**
     * Convenience method for setting a filter based on the type.
     * 
     * @param type
     *            the filter type
     * @param filter
     *            the filter
     */
    public void setFilterByType(GhgConfigData.AlertsFilterEnum type,
            String[] filter) {
        switch (type) {
        case Action:
            actions = filter;
            break;
        case PhenSig:
            phenSigs = filter;
            break;
        case Pil:
            pils = filter;
            break;
        case WFO:
            wfos = filter;
            break;
        case GeoId:
            geoids = filter;
            break;
        case ETN:
            etns = filter;
            break;
        case Seg:
            segs = filter;
            break;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public GhgDataFilter clone() {
        return new GhgDataFilter(this);
    }
}