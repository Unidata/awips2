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

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.ui.ContourServerAlgorithmMenu.ContourServerAlgorithm;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ContourServerMsg extends Message {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ContourServerMsg.class);
    private ContourServerAlgorithm algorithm;

    public ContourServerMsg() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();

        algorithm = ContourServerAlgorithm.CONTOUR_ANALYZER;
        String algorithmString;
        if (!(algorithmString = prefs.getString("ContourServer")).isEmpty()) {
            try {
                algorithm = ContourServerAlgorithm.valueFrom(algorithmString);
            } catch (Throwable e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reading default ContourServer algorithm", e);
            }
        }
    }

    /**
     * 
     */
    public ContourServerMsg(ContourServerAlgorithm algorithm) {
        this.algorithm = algorithm;
    }

    /**
     * @return the algorithm
     */
    public ContourServerAlgorithm getAlgorithm() {
        return algorithm;
    }

}
