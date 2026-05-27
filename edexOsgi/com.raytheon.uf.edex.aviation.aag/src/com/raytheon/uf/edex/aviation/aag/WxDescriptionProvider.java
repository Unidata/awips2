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
package com.raytheon.uf.edex.aviation.aag;

/**
 * Turns short weather phenomenon codes found in METARs / TAF (e.g. "+SN") into
 * plain language descriptions (e.g. "heavy snow")
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2017 6110       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public interface WxDescriptionProvider {

    /**
     * @param wxShortCode
     *            The short code for a weather phenomenon (e.g. "+SN"
     * @return The plain language description (e.g. "heavy snow"), or "unknown
     *         weather" if the provided code has no known description
     */
    public String getWxDescription(String wxShortCode);
}
