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
package com.raytheon.viz.gfe.core.msgs;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import org.locationtech.jts.geom.Envelope;

/**
 * Listener interface for watching when the active reference set changes
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 08, 2008  1053     randerso  Initial creation
 * Jan 05, 2018  7178     randerso  Code cleanup
 *
 * </pre>
 *
 * @author randerso
 */

public interface IReferenceSetChangedListener {

    /**
     * Notification that the active reference set has changed
     *
     * @param refSet
     * @param domains
     *
     */
    public void referenceSetChanged(ReferenceData refSet,
            List<Envelope> domains);
}
