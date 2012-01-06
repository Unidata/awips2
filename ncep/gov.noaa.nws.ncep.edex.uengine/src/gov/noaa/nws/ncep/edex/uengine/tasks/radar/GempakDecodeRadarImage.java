package gov.noaa.nws.ncep.edex.uengine.tasks.radar;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarTiler;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;

/**
 * Derived from original uEngine DecodeRadarImage task.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 12, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class GempakDecodeRadarImage extends ScriptTask {

    private RadarRecord radarRecord;

    private RadarTiler radarTiler;

    private GridGeometry2D geometry;

    public GempakDecodeRadarImage(PluginDataObject aDataRecord, IDataRecord[] records) {
        radarRecord = (RadarRecord) aDataRecord;
        for (IDataRecord dataRecord : records) {
            if ("Data".equals(dataRecord.getName())) {
                radarRecord.setRawData(((ByteDataRecord) dataRecord)
                        .getByteData());
            } else if ("Angles".equals(dataRecord.getName())) {
                radarRecord.setAngleData(((FloatDataRecord) dataRecord)
                        .getFloatData());
            } else if ("Thresholds".equals(dataRecord.getName())) {
                radarRecord.setThresholds(((ShortDataRecord) dataRecord)
                        .getShortData());
            }
        }
        int tileSize = 2 * radarRecord.getNumBins();
        radarTiler = new RadarTiler(radarRecord, 1, tileSize);    
        geometry = radarTiler.constructGridGeometry();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        byte[] bi = null;

        // get the radar file
        bi = generateProduct();
        return bi;
    }

    private byte[] generateProduct() {
        byte[] image = radarTiler.createFullImage();
        return image;
    }

    public GridGeometry2D getGridGeometry() {
        return geometry;
    }

    public CoordinateReferenceSystem getCrs() {
        return geometry.getEnvelope2D().getCoordinateReferenceSystem();
    }
}
