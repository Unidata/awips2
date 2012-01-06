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
 * Derived class that consolidates storage info for a Parm.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/14/08     #1030      randerso    Initial port
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ParmStorageInfo extends StorageInfo {
    private String _parmName;

    private String _level;

    private float _dataOffset;

    private float _dataMultiplier;

    private String _dataType;

    public ParmStorageInfo(final String dataType, final Point gridSize,
            final String parmName, final String level, float dataOffset,
            float dataMultiplier, final String storageType) {
        super(storageType, gridSize);
        _parmName = parmName;
        _level = level;
        _dataOffset = dataOffset;
        _dataMultiplier = dataMultiplier;
        _dataType = dataType;
    }

    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof ParmStorageInfo)) {
            return false;
        }
        ParmStorageInfo rhs = (ParmStorageInfo) obj;

        if (!_parmName.equals(rhs.parmName()) || !_level.equals(rhs.level())
                || _dataOffset != rhs.dataOffset()
                || _dataMultiplier != rhs.dataMultiplier()
                || !_dataType.equals(rhs.dataType())) {
            return false;
        }
        return true;
    }

    public String parmName() {
        return _parmName;
    }

    public String level() {
        return _level;
    }

    public float dataOffset() {
        return _dataOffset;
    }

    public float dataMultiplier() {
        return _dataMultiplier;
    }

    public String dataType() {
        return _dataType;
    }

    @Override
    public String toString() {
        return "ParmStorageInfo for parm: " + parmName() + " Level: " + level()
                + " DataType: " + dataType() + " GridSize: " + gridSize()
                + " DataOffset: " + dataOffset() + " DataMultiplier: "
                + dataMultiplier() + " StorageAreaName: " + storageAreaName()
                + " StorageType: " + storageType();
    }
}
