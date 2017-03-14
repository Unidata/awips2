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
package com.raytheon.uf.edex.plugin.svrwx.decoder;

/**
 * Internal Line Type.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------
 * Jan 05, 2010  4112     jsanchez  Initial creation
 * Apr 10, 2014  2971     skorolev  Cleaned code.
 * Jun 25, 2014  3008     nabowle   Removed unused values.
 * Apr 12, 2016  4913     bsteffen  Add NO_REPORT
 * 
 * </pre>
 * 
 * @author jsanchez
 */
public enum InternalType {
    TIME_RANGE, REPORT_TYPE, EXTRA, EVENT_REPORT, NO_REPORT;
}
