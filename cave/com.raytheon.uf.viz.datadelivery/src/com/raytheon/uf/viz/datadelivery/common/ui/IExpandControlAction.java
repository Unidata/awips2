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
package com.raytheon.uf.viz.datadelivery.common.ui;

/**
 * Interface to provide callback methods when the expand bar controls are used.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public interface IExpandControlAction {
    /**
     * Action to be performed when the collapse all button is clicked.
     */
    void collapseAction();

    /**
     * Action to be performed when the expand all button is clicked.
     */
    void expandAction();

    /**
     * Action to be performed when the expand selected button is clicked.
     */
    void expandSelectedAction();

    /**
     * Action to be performed when the disable button is clicked.
     */
    void disableAction();

    /**
     * Action to be performed when the clear all button is clicked.
     */
    void clearAllAction();

    /**
     * Action to be performed when the preview button is clicked.
     */
    void previewAction();
}
