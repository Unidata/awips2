/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.d2d.core.legend;

import com.raytheon.uf.common.time.DataTime;

/**
 * Interface for resources to contribute extra text to their product legend.
 *
 * This interface only adds the text to the legend, as opposed to text added to
 * a resource's getName() method, which will add it to the legend but also
 * include it elsewhere such as in saved procedure/bundle names.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2024 2037565    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public interface IExtraLegendTextGeneratingResource {

    /**
     * Get the extra text to display in this resource's legend for the given
     * frame time.
     *
     * @param time
     *            the resource's frame time
     * @return the extra text string
     */
    String getExtraLegendText(DataTime time);
}
