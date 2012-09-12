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
package com.raytheon.uf.viz.points;

import com.raytheon.uf.viz.points.data.GroupNode;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;

/**
 * This class is used to queue actions to perform on a Point's localization
 * file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PointRequest {
    public static enum RequestType {
        ADD, UPDATE, DELETE
    };

    private final RequestType type;

    private final IPointNode point;

    /**
     * The constructor.
     * 
     * @param type
     *            - Kind of request to perform
     * @param point
     *            - The point to pefrom the request on.
     */
    public PointRequest(RequestType type, IPointNode point) {
        this.type = type;
        // Duplicate the point to capture information as it exists at the time
        // of the request.
        if (point.isGroup()) {
            this.point = new GroupNode((Point) point);
        } else {
            this.point = new Point((Point) point);
        }
    }

    public IPointNode getPoint() {
        return point;
    }

    public RequestType getType() {
        return type;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder("PointUpdatedMessage - ");
        sb.append("type: ").append(type).append(", point: ");
        if (point == null) {
            sb.append("null\n");
        } else {
            sb.append("\"").append(point.getName()).append("\", \"")
                    .append(point.getGroup()).append("\"\n");
        }
        return sb.toString();
    }
}
