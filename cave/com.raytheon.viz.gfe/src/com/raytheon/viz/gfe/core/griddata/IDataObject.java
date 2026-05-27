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
package com.raytheon.viz.gfe.core.griddata;

/**
 * Interface for a DataObject which contains the actual gridded data for an
 * IGridData instance separate from the metadata.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2017            randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */
public interface IDataObject {
    /*
     * RODO DR #7178 design concept
     *
     * IGridSlice is a construct carried over from A1 that is used on both
     * client and server side. It contains both metaData and optionally data
     * (i.e. the actual grids/keys).
     *
     * IGridData was originally a wrapper around the IGridSlice on the client
     * side that kept track of changed points and last access.
     *
     * As part of this change I separated the metaData and data (metaData moved
     * into IGridData, data into IDataObject) so we can use soft references for
     * the data part when it's unmodified so the GC can throw it out if we need
     * the memory rather than evicting the data solely based on time.
     *
     * The old parm evictor code would replace the IGridSlice in IGridData with
     * one with the data part unpopulated. I couldn't use soft references to the
     * entire IGridSlice or we'd lose the metadata. Since IGridSlice is a server
     * side object I couldn't use the SoftReference for the data inside it.
     *
     * If I were designing GFE from scratch today I would have reworked the
     * server interface to separate the data and the metadata better.
     */

    /**
     * Assigns a dataObject
     *
     * @param dataObject
     *            the data grid containing values to assign
     */
    public void assign(IDataObject dataObject);

    /**
     * @return a copy of this object
     */
    public IDataObject copy();

    /**
     * Collapses the weather/discrete key/grid to not contain extra key
     * definitions. This is a no-op on non-weather/non-discrete data.
     */
    public void collapse();

    /**
     * Gets a representation of the underlying data grid as an NDArray.
     *
     * FIXME The returned NDArray object will have the x and y dimensions
     * reversed. That's what AWIPS 1 did and that makes the pre-existing python
     * code compatible. Java ordering is x,y while python is ordering is y,x.
     * It's confusing and questionable at best so someday someone should correct
     * all that. Good luck.
     *
     * @return an NDArray or NDArray[]
     */
    public Object getNDArray();

}
