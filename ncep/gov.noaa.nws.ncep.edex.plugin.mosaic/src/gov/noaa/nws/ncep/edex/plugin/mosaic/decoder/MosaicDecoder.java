package gov.noaa.nws.ncep.edex.plugin.mosaic.decoder;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;
import gov.noaa.nws.ncep.edex.plugin.mosaic.common.MosaicRecord;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.MosaicInfo;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.MosaicInfoDict;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.Layer;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.Level3Parser;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.RasterPacket;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.SymbologyBlock;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.SymbologyPacket;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Decoder implementation for mosaic plugin
 * 
 * Date Ticket# Engineer Description ------------ ---------- -----------
 * -------------------------- 09/2009 143 L. Lin Initial coding 11/2009 143 L.
 * Lin Migration to to11 d6. 1/2011 143 T. Lee Used prod name from
 * mosaicInfo.txt </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * 01/19/13   #      Greg hull   Use getStaticLocalizationFile to get mosaicInfo file
 * 
 * @author L. Lin
 * @version 1.0
 */

public class MosaicDecoder extends AbstractDecoder {

    private final Log theLogger = LogFactory.getLog(getClass());

    private String traceId = "";

    public static final String MOSAIC_INFO_FILE = "ncep"+File.separator+"dictionary"+
    													 File.separator+"mosaicInfo.txt";

    private MosaicInfoDict infoDict;

    private byte[] headerBlock = new byte[120];

    public MosaicDecoder() throws DecoderException {

        IPathManager manager = PathManagerFactory.getPathManager();

        LocalizationFile lf = manager.getStaticLocalizationFile( MOSAIC_INFO_FILE );

        if( lf == null ) {
        	theLogger.error("Error finding "+ MOSAIC_INFO_FILE );
        }
        else {
        	infoDict = MosaicInfoDict.getInstance( lf.getFile() );
        }
    }

    public PluginDataObject[] decode(byte[] messageData, Headers headers)
            throws DecoderException {
        String prodName  = null;
        
        if ( headers != null) {
            /*
             * traceId equals to the file name
             */
            traceId = (String) headers.get("traceId");
        }

        ArrayList<MosaicRecord> recordList = new ArrayList<MosaicRecord>();

        try {

            String arch = new String(messageData, 0, 4);

            // Only process level 3 data
            if (!"ARCH".equals(arch) && !"AR2V".equals(arch)) {
                // get headerBlock
                System.arraycopy(messageData, 0, headerBlock, 0, 120);
                // process level 3 data
                Level3Parser l3Mosaic = new Level3Parser(messageData);
                MosaicRecord record = new MosaicRecord();

                int prodCode = l3Mosaic.getProductCode();
                record.setProductCode(prodCode);
                record.setLatitude((float) l3Mosaic.getLatitude());
                record.setLongitude((float) l3Mosaic.getLongitude());
                record.setElevation((float) l3Mosaic.getHeight());
                record.setSourceId(l3Mosaic.getTheSourceId());
                record.setTrueElevationAngle((float) Level3Parser
                        .getProductDependentValue(2));

                if (prodName == null) {
                    prodName = "unknown";
                }
                
                MosaicInfo info = infoDict.getInfo(prodCode);
                if (info == null) {
                    theLogger.error(traceId + "-Unknown mosaic product code: "
                            + prodCode);
                    return new PluginDataObject[0];
                }

                record.setProdName(info.getProdName());
                record.setVolumeCoveragePattern(l3Mosaic
                        .getVolumeCoveragePattern());
                record.setDataTime(new DataTime(l3Mosaic.getVolumeScanTime()));
                record.setIssueTime(l3Mosaic.getMessageTimestamp());
                record.setScanTime(l3Mosaic.getVolumeScanTime());
                record.setGenerationTime(l3Mosaic.getProductGenerationTime());

                record.setNumLevels(info.getNumLevels());
                record.setResolution(info.getResolution());
                record.setFormat(info.getFormat());
                record.setUnit(info.getUnit());

                for (int i = 0; i < 16; ++i) {
                    record.setThreshold(i, l3Mosaic.getDataLevelThreshold(i));
                }

                record.setProductDependentValues(l3Mosaic
                        .getProductDependentValue());

                processSymbologyBlock(record, l3Mosaic.getSymbologyBlock());

                try {
                    finalizeRecord(record);
                } catch (PluginException e) {
                    logger.error(e);
                    return new PluginDataObject[0];
                }
                recordList.add(record);

            }
        } catch (IOException e) {
            theLogger
                    .error(traceId
                            + "-Couldn't properly handle your mosaic file; take a look at this error: ",
                            e);
        }

        return recordList.toArray(new PluginDataObject[recordList.size()]);
    }

    private void finalizeRecord(MosaicRecord record) throws PluginException {
        record.setTraceId(traceId);
        record.setPluginName("mosaic");
        record.constructDataURI();
        record.setInsertTime(TimeTools.getSystemCalendar());
    }

    /**
     * @param record
     * @param symbologyBlock
     */
    private void processSymbologyBlock(MosaicRecord record,
            SymbologyBlock symbologyBlock) {

        int errorCount = 0;

        if (symbologyBlock == null) {
            return;
        }
        List<Layer> packetsInLyrs = new ArrayList<Layer>();
        for (int layer = 0; layer < symbologyBlock.getNumLayers(); ++layer) {
            Layer lyr = new Layer();
            lyr.setLayerId(layer);

            List<SymbologyPacket> packets = new ArrayList<SymbologyPacket>();
            SymbologyPacket[] inPackets = symbologyBlock.getPackets(layer);

            if (inPackets != null) {
                for (SymbologyPacket packet : inPackets) {
                    if (packet instanceof RasterPacket) {
                        RasterPacket rasterPacket = (RasterPacket) packet;
                        processRasterPacket(record, rasterPacket);
                    } else {
                        packets.add(packet);
                    }
                }
            }
            lyr.setPackets(packets.toArray(new SymbologyPacket[packets.size()]));
            packetsInLyrs.add(lyr);

        }

        // remove the radial and raster from the symb block
        symbologyBlock.setLayers(packetsInLyrs.toArray(new Layer[packetsInLyrs
                .size()]));
        record.setSymbologyBlock(symbologyBlock);

        if (errorCount > 0) {
            logger.error("Mosaic file contains " + errorCount
                    + " unrecognized symbology packet types.");
        }
    }

    /**
     * @param l3Mosaic
     * @param rasterPacket
     * @return
     */
    private void processRasterPacket(MosaicRecord record,
            RasterPacket rasterPacket) {
        record.setNx(rasterPacket.getNumRows());
        record.setNy(rasterPacket.getNumCols());
        record.setRawData(rasterPacket.getRasterData());
        record.setHeaderBlock(headerBlock);
    }

}
