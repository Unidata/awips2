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
package com.raytheon.viz.radar;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;

/**
 * Radar Time Record
 * 
 * Data structure that organizes radar records by tilt and then by type.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 23, 2007             chammack    Initial Creation.
 * Sep 07, 2010             bsteffen    Merged RadarTimeRecord with RadarTiltRecord
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class RadarTimeRecord {

    public ColorMapParameters params;

    /**
     * The radar cache object is a bit unique in that it's metadata is also the
     * same data object to be requested and cached because the hdf5 data is
     * stored in the RadarRecord which is not the case for most PDOs
     */
    public CacheObject<? extends IRadarRecordMetadata, RadarRecord> radarCacheObject;

    public IImage image;

    public ImageTile tile;
}
