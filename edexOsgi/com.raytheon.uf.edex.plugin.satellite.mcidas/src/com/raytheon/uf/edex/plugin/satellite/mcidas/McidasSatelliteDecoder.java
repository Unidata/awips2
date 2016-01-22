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
package com.raytheon.uf.edex.plugin.satellite.mcidas;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import org.apache.commons.codec.binary.Base64;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.util.satellite.SatSpatialFactory;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.satellite.mcidas.util.McidasSatelliteLookups;
import com.raytheon.uf.edex.plugin.satellite.mcidas.util.McidasSatelliteLookups.PhysicalElementValue;

/**
 * McIDAS AREA Decoder
 * 
 * <p>
 * Implemented:
 * <ul>
 * <li>Mercator projection</li>
 * <li>Multiple bands</li>
 * </ul>
 * 
 * Not Implemented:
 * <ul>
 * <li>Interpretation of line prefix</li>
 * <li>Other projections</li>
 * <li>Calibration block</li>
 * <li>Non-byte data types</li>
 * </ul>
 * 
 * <pre>
 * 
 * OFTWARE HISTORY
 *                   
 * Date         Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * No previous history
 * - AWIPS2 Baseline Repository --------
 * 07/12/2012    798        jkorman     Changed projection "magic" numbers
 * 09/24/2012   1210        jkorman     Modified the decode method to create the
 *                                      IDataRecord required by the SatelliteDao
 * 12/03/2013   DR 16841    D. Friedman Allow record overwrites
 * 09/18/2014   3627        mapeters    Updated deprecated method calls.
 * 05/11/2015   ----        mjames@ucar PS (south and north) stereogrpahic support added.
 * 05/19/2015	----        mjames@ucar Added decoding of GVAR native projection products
 * 07/12/2015	----        mjames@ucar Account for GOES E and W UNIWISC AREA file numbers
 * 01/21/2016   ----        mjames@ucar Cleanup
 * </pre>
 * 
 * @author
 * @version
 */
public class McidasSatelliteDecoder {

    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(McidasSatelliteDecoder.class);

    private static final String UNEXPECTED_HEADER_VALUE = "Unexpected value in format";

    private static final int EXPECTED_IMAGE_TYPE_LE = 4;

    private static final int EXPECTED_IMAGE_TYPE_BE = 0x04000000;
    
    private static final int RADIUS = 6371200;

    final int SIZE_OF_AREA = 256;
    
    private static final double HALFPI = Math.PI / 2.;

    private static final double RTD = 180. / Math.PI;

    private static final double DTR = Math.PI / 180.;

    private String traceId;

    public McidasSatelliteDecoder() {
    }

    /**
     * 
     * @param data
     *            The file byte array data to be decoded.
     * @return The decoded data record(s).
     * @return
     * @throws Exception
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws Exception {
        traceId = (String) headers.get("traceId");
        try {
            return decodeMcidasArea(data);
        } catch (DecoderException e) {
            // Any DecoderExceptions throw by this decoder do not need a stack
            // trace
            theHandler.error(e.getMessage(), e);
            return new PluginDataObject[0];
        }
    }

    /**
     * References:<br/>
     * <ul>
     * <li>http://www.ssec.wisc.edu/mcidas/doc/prog_man/2006/formats-1.html</li>
     * <li>http://www.ssec.wisc.edu/mcidas/doc/misc_doc/area2.html</li>
     * </ul>
     * 
     * @param data
     * @return
     * @throws Exception
     */
    private PluginDataObject[] decodeMcidasArea(byte[] data) throws Exception {
       
        
        byte[] area = null;
        byte[] nonAreaBlock = new byte[data.length - SIZE_OF_AREA];
        area = new byte[SIZE_OF_AREA];
        System.arraycopy(data, 0, area, 0, SIZE_OF_AREA);
        System.arraycopy(data, SIZE_OF_AREA, nonAreaBlock, 0,
                nonAreaBlock.length);

        ByteBuffer buf = ByteBuffer.wrap(data);
        buf.order(ByteOrder.LITTLE_ENDIAN);
        // Decode the directory block
        if (buf.getInt() != 0) {
            formatError(UNEXPECTED_HEADER_VALUE);
        }
        if (buf.getInt() != EXPECTED_IMAGE_TYPE_LE) {
            if (buf.getInt(4) == EXPECTED_IMAGE_TYPE_BE) {
                buf.order(ByteOrder.BIG_ENDIAN);
            } else {
                formatError(UNEXPECTED_HEADER_VALUE);
            }
        }
        
        int sensorSourceNumber = buf.getInt(); 		// W3
        int yyyddd = buf.getInt(); 					// W4
        int hhmmss = buf.getInt(); 					// W5
        int ulImageLine = buf.getInt();				// W6
        int ulImageElement = buf.getInt();			// W7
        buf.getInt(); // reserved					// W8
        int nLines = buf.getInt();					// W9
        int nElementsPerLine = buf.getInt();		// W10
        int nBytesPerElement = buf.getInt();		// W11
        int lineResolution = buf.getInt();			// W12
        int elementResolution = buf.getInt();		// W13
        int nBands = buf.getInt();					// W14
        int linePrefixLength = buf.getInt();		// W15
        /* int projectNumber = */buf.getInt();		// W16
        /* int creationYyyddd = */buf.getInt();		// W17
        /* int creationHhmmss = */buf.getInt();		// W18
        
        /*
         * W19 
			32-bit filter band map for multichannel 
			images; if a bit is set, data exists for the band; 
			band 1 is the least significant byte (rightmost)
         */
        int bandMap1to32 = buf.getInt();			// W19
        /*
         * W20-24 
			satellite specific information  
         */
        int bandMap33to64 = buf.getInt();			// W20
        
        buf.position(buf.position() + (4 * 4)); // sensor specific
        buf.position(buf.position() + (4 * 8)); // memo
        int areaNumber = buf.getInt();
        int dataBlockOffset = buf.getInt();
        int navBlockOffset = buf.getInt();
        /* int validityCode = */buf.getInt();
        buf.position(buf.position() + (8 * 4)); // PDL
        buf.getInt(); // GOES AA band 8
        /* int imageYyyddd = */buf.getInt();
        /* int imageHhmmssOrMillis = */buf.getInt();
        /* int imageStartScan = */buf.getInt();
        /* int prefixDocLength = */buf.getInt();
        int prefixCalibrationLength = buf.getInt(); // W50
        /* int prefixBandListLength = */buf.getInt(); // W51
        buf.getInt(); // source type	// W52
        String calType = get4cc(buf); // cal type		// W53
        
        buf.position(buf.position() + (3 * 4)); // reserved
        /* int originalSourceType = */buf.getInt(); // actually a 4cc?
        /* int units = */buf.getInt(); // also 4cc?
        /* int scaling = */buf.getInt();
        /* int supplementalBlockOffset = */buf.getInt();
        buf.getInt(); // reserved
        int calibrationOffset = buf.getInt();
        buf.getInt(); // comment cards
        
        
        int navsize;
        if (calibrationOffset == 0){
        	navsize = dataBlockOffset - navBlockOffset;
        } else {
        	navsize = calibrationOffset - navBlockOffset;
        }
        byte[] navigation = new byte[navsize];
        System.arraycopy(nonAreaBlock, 0, navigation, 0, navsize);
        
        /* mjames@ucar
         * bandMap1to32 is a 32-bit filter band map for multichannel images.
         * if a bit is set, data exists for the band; band 1 is the least 
         * significant byte (rightmost)
         * 
         * Example: for GVAR GEWCOMP UNIWISC image,
         * 		bandMap1to32 = 4
         * 		bandMap33to64 = -1, so 
         * nBands = 1
         * bandBitsCount = 33
         * 
         * this is a problem...
         */
        long bandBits = ((long) bandMap33to64 << 32) | bandMap1to32;
        long bandBitsCount =  Long.bitCount(bandBits);
        if (nBands != bandBitsCount && nBands > 1) {
            formatError("Specified number of bands does not match number of bits in band map");
        }

        // Decode the navigation block
        buf.position(navBlockOffset);
        SatMapCoverage coverage = decodeNavigation(elementResolution,
                lineResolution, ulImageElement, ulImageLine, nElementsPerLine,
                nLines, buf, navigation);

        // Decode the data block, creating a SatelliteRecord for each band.
        PluginDataObject[] result = new PluginDataObject[nBands];
        int bitIndex = 0;
        RECORD: for (int ri = 0; ri < nBands; ++ri) {
            while ((bandBits & (1L << bitIndex)) == 0) {
                if (++bitIndex >= 64) {
                    break RECORD; // shouldn't happen
                }
            }
            SatelliteRecord rec = new SatelliteRecord();

            rec.setDataTime(new DataTime(unpackTime(yyyddd, hhmmss)));
            rec.setSource("McIDAS");
            rec.setCreatingEntity(getCreatingEntity(sensorSourceNumber));
            PhysicalElementValue pev = getPhysicalElement(sensorSourceNumber,
                    bitIndex + 1);
            rec.setPhysicalElement(pev.name);
            rec.setUnits(pev.units);
            rec.setSectorID(getAreaName(areaNumber));
            rec.setCoverage(coverage);

            // TODO: Line pad if not a multiple of four bytes
            if ((linePrefixLength == 0) && (nBytesPerElement == 1)
                    && (nBands == 1)) {
                byte[] imageBytes = new byte[nLines * nElementsPerLine];
                buf.position(dataBlockOffset);
                buf.get(imageBytes);

                rec.setMessageData(imageBytes);

            } else if (nBytesPerElement == 1) {
                byte[] imageBytes = new byte[nLines * nElementsPerLine];
                int si = dataBlockOffset + (ri * nBytesPerElement);
                int di = 0;
                int eincr = nBands * nBytesPerElement;
                for (int y = 0; y < nLines; ++y) {
                    si += linePrefixLength;
                    for (int x = 0; x < nElementsPerLine; ++x) {
                        imageBytes[di++] = buf.get(si);
                        si += eincr;
                    }
                }
            } else {
                unimplemented("non-byte elements");
            }

            rec.setTraceId(traceId);
            rec.setPersistenceTime(TimeUtil.newGmtCalendar().getTime());
            rec.setOverwriteAllowed(true);

            // Set the data into the IDataRecord
            // Set the data into the IDataRecord
            IDataRecord dataRec = SatelliteRecord.getDataRecord(rec);
            if (dataRec != null) {
                rec.setMessageData(dataRec);
            } else {
                theHandler.error(
                        String.format("Could not create datarecord for %s"),
                        traceId);
                rec = null;
            }
            if (rec != null) {
                result[ri] = rec;
            } else {
                result = new PluginDataObject[0];
            }
        }

        return result;
    }

    /**
     * Reference:
     * http://www.ssec.wisc.edu/mcidas/doc/prog_man/2006/formats-13a.html
     * 
     */
    private SatMapCoverage decodeNavigation(int xImgRes, int yImgRes, int ulX,
            int ulY, int nx, int ny, ByteBuffer buf, byte[] navigation) 
            		throws Exception {
        SatMapCoverage result = new SatMapCoverage();
        String navType = get4cc(buf);
        int lineOfEquator = buf.getInt();
        int elementOfEquator = buf.getInt();
        int stdLatDDMMSS = buf.getInt();
        int spacingAtStdLatInMeters = buf.getInt();
        int nrmlLonDDMMSS = buf.getInt();
   
        
        // NOTE: We do not check the following for compatibility with WGS84.
        int radiusInMeters = buf.getInt();
        /* int eccentricity = */buf.getInt();
        /* boolean geodetic = */buf.getInt()/* >= 0 */;

        boolean westPositive = buf.getInt() >= 0;
        float la1, lo1, la2, lo2;
        double dy;

        /*
         * The following is based on
         * gov.noaa.nws.ncep.edex.plugin.mcidas/src
         * /gov/noaa/nws/ncep/edex/plugin/mcidas/decoder/McidasDecoder.java
         */

        double clon = flipLon(unpackDdmmss(nrmlLonDDMMSS), westPositive);
        double clat = unpackDdmmss(stdLatDDMMSS);
        double dx = spacingAtStdLatInMeters * xImgRes;

        double phi0r = clat * DTR;
        double sign = 1.;
        if (phi0r < 0.) {
            sign = -1.;
        }
        double rxp = (((double) (elementOfEquator - ulX) / xImgRes) + 1.);
        double ryp = (ny - ((double) (lineOfEquator - ulY) / yImgRes));
        
        if (navType.equals("MERC")) {

            double dxp = 1. - rxp;
            double dyp = 1. - ryp;
            double rm = dx * dyp;
            double rcos = radiusInMeters * Math.cos(phi0r);
            double arg = Math.exp(rm / rcos);
            la1 = (float) (((2. * Math.atan(arg)) - HALFPI) * RTD);
            lo1 = (float) prnlon((clon + (((dx * dxp) / rcos) * RTD)));
            dxp = nx - rxp;
            dyp = ny - ryp;
            rm = dx * dyp;
            arg = Math.exp(rm / rcos);
            la2 = (float) (((2. * Math.atan(arg)) - HALFPI) * RTD);
            lo2 = (float) prnlon((clon + (((dx * dxp) / rcos) * RTD)));
            lo2 = (float) prnlon(lo2);
            
            result = SatSpatialFactory.getInstance().getCoverageTwoCorners(
                    SatSpatialFactory.PROJ_MERCATOR, nx, ny, (float) clon,
                    (float) clat, la1, lo1, la2, lo2);
            
        } else if (navType.trim().equals("PS")) {
        	
        	dy = (float) spacingAtStdLatInMeters * yImgRes;
        	double dxp = (1. - rxp) * dx;
            double dyp = (1. - ryp) * dy;
            double alpha = 1. + Math.sin(Math.abs(phi0r));
            double rm = Math.sqrt(((dxp * dxp) + (dyp * dyp))) / alpha;
            la1 = (float) (sign * ((HALFPI - (2. * Math.atan(rm / RADIUS)))) * RTD);
            double thta;
            if (dyp != 0) {
                dyp = (-dyp) * sign;
                thta = (Math.atan2(dxp, dyp)) * RTD;
                lo1 = (float) prnlon((clon + thta));
            } else {
            	lo1 = (float) clon;
            }

            /*
             * Compute lat/lon of the upper-right corner
             */
            dxp = (nx - rxp) * dx;
            dyp = (ny - ryp) * dy;
            rm = Math.sqrt(((dxp * dxp) + (dyp * dyp))) / alpha;
            la2 = (float) (sign * ((HALFPI - (2. * Math.atan(rm / RADIUS)))) * RTD);

            if (dyp != 0) {
                dyp = (-dyp) * sign;
                thta = (Math.atan2(dxp, dyp)) * RTD;
                lo2 = (float) prnlon((clon + thta));
            } else {
            	lo2 = (float) clon;
            }
        	
            result = SatSpatialFactory.getInstance().getCoverageTwoCorners(
                    SatSpatialFactory.PROJ_POLAR, nx, ny, (float) clon,
                    (float) clat, la1, lo1, la2, lo2);
            
        } else {
        	/* 
        	 * Native projection
        	 */
            //unimplemented(String.format("navigation type \"%s\"", navType));
            clon = nrmlLonDDMMSS / 10000000.f;
            clon = (float) Math.toDegrees(clon);
            ProjectedCRS crs = MapUtil.constructNative(navType, encodeNavBlock(navigation));

            result = SatSpatialFactory.getInstance().getCoverageNative(
                    SatSpatialFactory.PROJ_GVAR, nx, ny, (float) clon,
                    ulX, ulY, xImgRes, yImgRes, crs);
            
        }

        return result;
    }

    private static double prnlon(double lon) {
        double dlon = lon - ((int) (lon / 360.f) * 360.f);
        if (lon < -180.) {
            dlon = lon + 360.f;
        } else if (lon > 180.) {
            dlon = lon - 360.;
        }
        return dlon;
    }

    private static Calendar unpackTime(int yyyddd, int hhmmss) {
        Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(0);

        cal.set(Calendar.YEAR, +1900 + (yyyddd / 1000));
        cal.set(Calendar.DAY_OF_YEAR, yyyddd % 1000);

        int hh = hhmmss / 10000;
        cal.set(Calendar.HOUR_OF_DAY, hh);
        cal.set(Calendar.MINUTE, (hhmmss - (hh * 10000)) / 100);
        cal.set(Calendar.SECOND, hhmmss % 100);

        return cal;
    }

    private static double unpackDdmmss(int ddmmss) {
        int dd = ddmmss / 10000;
        int mm = (ddmmss - (dd * 10000)) / 100;
        int ss = ddmmss % 100;
        return dd + (mm / 60.0) + (ss / 3600.0);
    }

    private static double flipLon(double lon, boolean flip) {
        return flip ? -lon : lon;
    }

    private static String get4cc(ByteBuffer buf) {
        byte[] bytes = new byte[4];
        buf.get(bytes);
        return new String(bytes, Charset.forName("ISO-8859-1"));
    }

    private String getCreatingEntity(int sensorSourceNumber) {
        String value = McidasSatelliteLookups.getInstance().getCreatingEntity(
                sensorSourceNumber);
        return value != null ? value : String.format("Unknown-%d",
                sensorSourceNumber);
    }

    private PhysicalElementValue getPhysicalElement(int ssn, int bandIndex) {
        PhysicalElementValue value = McidasSatelliteLookups.getInstance()
                .getPhysicalElement(ssn, bandIndex);
        return value != null ? value : new PhysicalElementValue(String.format(
                "Unknown-%d", bandIndex), null);
    }

    private String getAreaName(int areaNumber) {
    	// GOES-West UNIWISC McIDAS AREA files
    	if ( (1161 < areaNumber && areaNumber <= 1254) ||
    			(1801 <= areaNumber && areaNumber <= 1854) ){
    		areaNumber = 1161; 
    	// GOES-East UNIWISC McIDAS AREA files
    	} else if (1261 < areaNumber && areaNumber <= 1524) {
    		areaNumber = 1261; 
    	// HIMAWARI-8 McIDAS AREA files
    	} else if (5900 < areaNumber && areaNumber <= 5999) {
    		areaNumber = 5900; 
    	// METEOSAT-10 UNIWISC McIDAS AREA files
    	} else if (6000 < areaNumber && areaNumber <= 6099) {
    		areaNumber = 6000; 
    	}
        String value = McidasSatelliteLookups.getInstance().getAreaName(
                areaNumber);
        return value != null ? value : String.format("AREA%04d", areaNumber);
    }

    private void formatError(String message) throws DecoderException {
        throw new DecoderException(String.format("%s: %s", traceId, message));
    }

    protected void unimplemented(String feature) throws DecoderException {
        throw new DecoderException(String.format("%s: unimplemented: %s",
                traceId, feature));
    }
    private String encodeNavBlock(byte[] navigation) {

        Base64 b64 = new Base64();
        byte[] coded = b64.encode(navigation);

        return new String(coded);
    }
}
