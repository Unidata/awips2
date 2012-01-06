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
package com.raytheon.edex.plugin.gfe.config;

import java.awt.Point;

/**
 * StorageInfo is a base class for AreaStorageInfo and ParmStorageInfo. This
 * contains the storage type and grid size. Storage type may be byte, short, or
 * float.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/18/08     #1030      randerso    Initial port
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class StorageInfo {
    // TODO: if we don't end up porting AreaStorageInfo we should combine this
    // with ParmStorageInfo
    private String _storageType;

    private Point _gridSize;

    private String _storageAreaName;

    protected StorageInfo(final String storageType, final Point gridSize) {
        this._storageType = storageType;
        this._gridSize = gridSize;
        this._storageAreaName = makeStorageAreaName(storageType, gridSize);
    }

    public final String storageType() {
        return _storageType;
    }

    public final Point gridSize() {
        return _gridSize;
    }

    public final String storageAreaName() {
        return _storageAreaName;
    }

    public boolean equals(Object obj) {
        if (obj == null) {

        }
        if (!(obj instanceof StorageInfo)) {
            return false;
        }
        StorageInfo rhs = (StorageInfo) obj;
        if (!_storageType.equals(rhs.storageType())
                || !_gridSize.equals(rhs.gridSize())) {
            return false;
        }

        return true;
    }

    /**
     * Generates and returns a TextString that uniquely identifies the storage
     * info. This string is a concatenation of the storageType and gridSize.
     * 
     * @param storageType
     * @param gridSize
     * @return
     */
    static String makeStorageAreaName(final String storageType,
            final Point gridSize) {
        return storageType + gridSize.x + 'x' + gridSize.y;
    }
}
