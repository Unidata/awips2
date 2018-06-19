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
package com.raytheon.viz.gfe.core.parm;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.griddata.IGridData;

/**
 * An UndoBuffer contains a time range and a set of grids for purposes of
 * performing an undo operation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/22/2008              chammack    Port from AWIPS I
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class UndoBuffer {

    private TimeRange undoTimeRange;

    private List<IGridData> undoGrids;

    /**
     * Constructor for UndoBuffer taking a time range and a set of grid
     * pointers.
     * 
     * Makes copies of the grids and stores the copies in private data.
     * 
     * @param undoTimeRange
     * @param undoGrids
     */
    public UndoBuffer(final TimeRange undoTimeRange, IGridData[] undoGrids) {
        this.undoTimeRange = undoTimeRange;
        this.undoGrids = new ArrayList<IGridData>();

        for (int i = 0; i < undoGrids.length; i++) {
            IGridData grid;
            try {
                grid = undoGrids[i].clone();
            } catch (CloneNotSupportedException e) {
                grid = null;
            }
            if (grid != null)
                this.undoGrids.add(grid);
        }
    }

    /**
     * Copy constructor
     * 
     * @param rhs
     */
    public UndoBuffer(final UndoBuffer rhs) {
        this.undoTimeRange = rhs.undoTimeRange.clone();
        this.undoGrids = new ArrayList<IGridData>();
        for (int i = 0; i < rhs.undoGrids.size(); i++) {
            IGridData newGrid;
            try {
                newGrid = rhs.undoGrids.get(i).clone();
            } catch (CloneNotSupportedException e) {
                newGrid = null;
            }
            if (newGrid != null)
                this.undoGrids.add(newGrid);
        }
    }

    /**
     * @return the undoTimeRange
     */
    public TimeRange getUndoTimeRange() {
        return undoTimeRange;
    }

    /**
     * @return the undoGrids
     */
    public List<IGridData> getUndoGrids() {
        return undoGrids;
    }

}
