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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.ui.ContourServerAlgorithmMenu.ContourServerAlgorithm;

/**
 * Contour Server Message
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 27, 2009           randerso  Initial creation
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class ContourServerMsg extends Message {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContourServerMsg.class);

    private ContourServerAlgorithm algorithm;

    /**
     * Default constructor for use in {@link Message#inquireLastMessage(Class)}
     *
     * Uses algorithm from GFEPreference
     */
    public ContourServerMsg() {
        String algorithmString = GFEPreference.getString("ContourServer",
                "Contour Analyzer");
        try {
            algorithm = ContourServerAlgorithm.valueFrom(algorithmString);
        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading default ContourServer algorithm", e);
            algorithm = ContourServerAlgorithm.CONTOUR_ANALYZER;
        }
    }

    /**
     * @param algorithm
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
