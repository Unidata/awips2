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

package com.raytheon.viz.core.graphing;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;

/**
 * Utility class for graphs
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date       	Ticket#		Engineer	Description
 *  ----------- ----------	-----------	--------------------------
 *  24Oct2006	56			Phillippe	Initial Creation	
 *  30Oct2006	56			Phillippe	Added date formats
 *  Oct 2007                njensen     Major refactor
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class GraphUtil {

    public static Rectangle getBounds(IExtent extent) {
        Rectangle retVal = new Rectangle((int) extent.getMinX(), (int) extent
                .getMinY(), (int) (extent.getMaxX() - extent.getMinX()),
                (int) (extent.getMaxY() - extent.getMinY()));
        return retVal;
    }

    public static Rectangle getDrawingArea(IExtent extent) {
        Rectangle retVal = new Rectangle((int) (extent.getMinX() + extent
                .getWidth() * .06), (int) (extent.getMinY() + extent
                .getHeight() * .05), (int) (extent.getWidth() * .97),
                (int) (extent.getHeight() * .8));
        return retVal;
    }

    public static double getNumberRepresentation(Object anObject) {
        double result = Double.NaN;
        if (anObject instanceof Number) {
            result = ((Number) anObject).doubleValue();
        } else if (anObject instanceof Calendar) {
            result = ((GregorianCalendar) anObject).getTimeInMillis();
        } else if (anObject instanceof DataTime) {
            result = ((DataTime) anObject).getValidTime().getTimeInMillis();
        } else if (anObject instanceof Date) {
            result = ((Date) anObject).getTime();
        }

        return result;
    }

}
