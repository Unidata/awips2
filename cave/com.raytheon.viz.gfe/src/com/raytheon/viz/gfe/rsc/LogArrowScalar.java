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
package com.raytheon.viz.gfe.rsc;

import com.raytheon.viz.core.contours.util.VectorGraphicsConfig;
import com.raytheon.viz.core.contours.util.VectorGraphicsConfig.IArrowScaler;

/**
 * 
 * Provides logarithmic scaling of arrows.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 23, 2013  2363     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @see VectorGraphicsConfig
 */
public class LogArrowScalar implements IArrowScaler {

    protected final double baseSize;

    protected final double logFactor;

    protected final double minLog;

    protected final double maxLog;

    protected final double maxLimit;

    public LogArrowScalar(double baseSize, double logFactor, double maxLimit) {
        this.baseSize = baseSize;
        this.logFactor = logFactor;
        this.maxLimit = maxLimit;
        minLog = Math.log(logFactor);
        maxLog = Math.log(logFactor + 1.0);
    }

    @Override
    public double scale(double magnitude) {
        double pcentRange = magnitude / maxLimit;
        double lg = Math.log(logFactor + pcentRange);
        double pcentLog = (lg - minLog) / (maxLog - minLog);
        return pcentLog * baseSize;
    }

}
