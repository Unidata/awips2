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
package com.raytheon.uf.viz.points.data;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.dnd.ByteArrayTransfer;
import org.eclipse.swt.dnd.TransferData;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This singleton class converts an instance of the Point class to a byte array
 * or an byte array to an instance of Point. This is used to support drag and
 * drop.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2012 875        rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PointTransfer extends ByteArrayTransfer {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointTransfer.class);

    private static final String TYPE_NAME = "point_type";

    private static final int TYPE_ID = registerType(TYPE_NAME);

    private static PointTransfer instance = new PointTransfer();

    public static PointTransfer getInstance() {
        return instance;
    }

    private PointTransfer() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.dnd.ByteArrayTransfer#javaToNative(java.lang.Object,
     * org.eclipse.swt.dnd.TransferData)
     */
    @Override
    protected void javaToNative(Object object, TransferData transferData) {
        if (object == null || !(object instanceof Point[])) {
            return;
        }

        if (isSupportedType(transferData)) {
            Point[] points = (Point[]) object;

            ArrayList<PointTransferObject> transfers = new ArrayList<PointTransferObject>();
            for (Point point : points) {
                PointTransferObject transfer = new PointTransferObject();
                if (point.isGroup()) {
                    transfer.setPoint(new Point(point));
                } else {
                    transfer.setPoint(point);
                }
                transfer.setGroupNode(point.isGroup());
                transfer.setGroupName(point.getGroup());
                transfers.add(transfer);
            }

            try {
                byte[] buffer = SerializationUtil.transformToThrift(transfers);
                super.javaToNative(buffer, transferData);
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.dnd.ByteArrayTransfer#nativeToJava(org.eclipse.swt.dnd
     * .TransferData)
     */
    @SuppressWarnings("unchecked")
    @Override
    protected Object nativeToJava(TransferData transferData) {
        Point[] points = null;
        if (isSupportedType(transferData)) {
            byte[] buffer = (byte[]) super.nativeToJava(transferData);
            if (buffer != null) {
                try {
                    List<PointTransferObject> transfers = (List<PointTransferObject>) SerializationUtil
                            .transformFromThrift(buffer);
                    points = new Point[transfers.size()];
                    int index = 0;
                    for (PointTransferObject transfer : transfers) {
                        Point point = transfer.getPoint();
                        if (transfer.isGroupNode()) {
                            point = new GroupNode(point);
                        }
                        point.setGroup(transfer.getGroupName());
                        points[index] = point;
                        ++index;
                    }
                } catch (SerializationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
        return points;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.dnd.Transfer#getTypeNames()
     */
    @Override
    protected String[] getTypeNames() {
        return new String[] { TYPE_NAME };
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.dnd.Transfer#getTypeIds()
     */
    @Override
    protected int[] getTypeIds() {
        return new int[] { TYPE_ID };
    }
}
