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
package com.raytheon.uf.common.geospatial.interpolation.data;


/**
 * DataSource which can retrieve tiles of data on demand.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 12, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractTiledDataSource implements DataSource {

    private final int tileSize;

    private final int nx;

    private final int ny;

    public AbstractTiledDataSource(int tileSize, int nx, int ny) {
        super();
        this.tileSize = tileSize;
        this.nx = nx;
        this.ny = ny;
    }

    @Override
    public double getDataValue(int x, int y) {
        if (y < 0 || y > ny - 1) {
            // outside y range
            return Double.NaN;
        } else if (x < 0 || x > nx - 1) {
            // outside x range
            return Double.NaN;
        }
        int tileX = x / tileSize;
        int tileY = y / tileSize;
        int tileWidth = Math.min(tileSize, nx - tileX * tileSize);
        int tileHeight = Math.min(tileSize, ny - tileY * tileSize);
        DataSource tile = getTile(tileX * tileSize, tileY * tileSize,
                tileWidth, tileHeight);
        int newX = x - tileX * tileSize;
        int newY = y - tileY * tileSize;
        return tile.getDataValue(newX, newY);
    }

    protected abstract DataSource getTile(int startX, int startY, int width,
            int height);

}
