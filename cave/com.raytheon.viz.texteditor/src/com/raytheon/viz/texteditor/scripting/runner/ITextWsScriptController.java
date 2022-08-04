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
package com.raytheon.viz.texteditor.scripting.runner;

/**
 * Interface to control a running TextWS script
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 3, 2018  6804       tgurney     Initial creation
 * Nov 5, 2018  6804       tgurney     Add waitFor
 * Mar 4, 2019  7601       tgurney     Remove waitFor (no longer needed), add
 *                                     doContinue
 *
 * </pre>
 *
 * @author tgurney
 */

public interface ITextWsScriptController {
    /** Request the script to cancel. */
    void cancel();

    /** Request the script to exit the current wait and continue running. */
    void doContinue();
}
