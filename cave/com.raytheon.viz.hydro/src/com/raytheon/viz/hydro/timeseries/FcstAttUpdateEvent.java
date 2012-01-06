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
package com.raytheon.viz.hydro.timeseries;

import java.util.EventObject;

import com.raytheon.viz.hydro.timeseries.table.ForecastDataAttribute;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2009            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class FcstAttUpdateEvent extends EventObject {

    
    private static final long serialVersionUID = -6202373414366803280L;
    
    /** The ForecastDataAttribute object */
    private ForecastDataAttribute dataAtt = null;

    /**
     * Constructor.
     * 
     * @param source
     *      The event source
     * @param dataAtt
     *      The Forecast Data Attributes
     */
    public FcstAttUpdateEvent(Object source, ForecastDataAttribute dataAtt) {
        super(source);
        this.dataAtt = dataAtt;
    }

    /**
     * Returns the ForecastDataAttribute object
     * 
     * @return the ForecastDataAttribute
     */
    public ForecastDataAttribute getFcstAttributes() {
        return dataAtt;
    }
}
