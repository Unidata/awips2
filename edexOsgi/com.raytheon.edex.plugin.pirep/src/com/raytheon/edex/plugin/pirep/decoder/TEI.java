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
package com.raytheon.edex.plugin.pirep.decoder;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Enum that identifies the PIREP Text Element Indicators (TEIs).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * AWIPS2 DR Work
 * Aug 7, 2012        1011 jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public enum TEI implements Iterable<TEI> {
    // This TEI is used to identify the start of the PIREP, i.e. SSS [UA|UUA]
    PIREP("PIREP"), OV("/OV "), TM("/TM "), FL("/FL"), TP("/TP"), SK("/SK "), WX(
            "/WX"), TA("/TA"), WV("/WV"), TB("/TB"), IC("/IC"), RM("/RM"),

    NF("NF"); // These two don't go into the id map!

    // Map of valid TEIs.
    private static Map<String, TEI> ID_MAP = new HashMap<String, TEI>();
    static {
        ID_MAP.put(OV.id, OV);
        ID_MAP.put(TM.id, TM);
        ID_MAP.put(FL.id, FL);
        ID_MAP.put(TP.id, TP);
        ID_MAP.put(SK.id, SK);
        ID_MAP.put(WX.id, WX);
        ID_MAP.put(TA.id, TA);
        ID_MAP.put(WV.id, WV);
        ID_MAP.put(TB.id, TB);
        ID_MAP.put(IC.id, IC);
        ID_MAP.put(RM.id, RM);
    }

    private String id;

    /**
     * Construct a TEI with a specified identifier.
     * 
     * @param id
     *            The identifier to assign.
     */
    private TEI(String id) {
        this.id = id;
    }

    /**
     * Get the identifier for this TEI.
     * 
     * @return The TEI identifier.
     */
    public String getId() {
        return id;
    }

    /**
     * Get the string representation of this TEI. This returns the same as
     * getId().
     * 
     * @return The TEI string representation.
     */
    public String toString() {
        return id;
    }

    /**
     * Get an iterator to the valid TEIs. This iterable iterator the TEI NF (not
     * found).
     * 
     * @return An iterator to the valid TEIs.
     */
    @Override
    public Iterator<TEI> iterator() {
        return ID_MAP.values().iterator();
    }

    /**
     * Get a TEI based on its string identifier. Returns the TEI "NF" if the TEI
     * identifier could not be found.
     * 
     * @param id
     *            A candidate TEI identifier.
     * @return The TEI found, or NF if not found.
     */
    public static TEI getTEI(String id) {
        TEI tei = NF;
        if (ID_MAP.containsKey(id)) {
            tei = ID_MAP.get(id);
        }
        return tei;
    }
}