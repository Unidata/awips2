package gov.noaa.nws.ncep.edex.plugin.mcidas.decoder;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasMapCoverage;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasSpatialFactory;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.dao.McidasDao;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * This java class decodes McIDAS satellite plug-in image and creates area names
 * from area file number in the header.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                   
 * Date         Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 9/2009       144         T. Lee      Creation
 * 11/2009      144         T. Lee      Added geographic area names
 * 12/2009      144         T. Lee      Set calType, satelliteId,
 *                                      and imageTypeNumber
 * 12/2009      144         T. Lee      Renamed proj type for resource 
 *                                      rendering
 * 05/2010      144         L. Lin      Migration to TO11DR11.
 * 11/2011                  T. Lee      Enhanced for ntbn
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * </pre>
 * 
 * @author tlee
 * @version 1
 */
public class McidasDecoder extends AbstractDecoder {
    final int RADIUS = 6371200;

    final int SIZE_OF_AREA = 256;

    final double PI = 3.14159265;

    final double HALFPI = PI / 2.;

    final double RTD = 180. / PI;

    final double DTR = PI / 180.;

    private final String traceId = "";

    private McidasDao dao;

    Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

    private final McidasRecord mr = new McidasRecord();

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws Exception {
        int endian = 0;
        byte[] area = null;
        byte[] nonAreaBlock = new byte[data.length - SIZE_OF_AREA];
        McidasRecord record = new McidasRecord();
        String areaName = null;

        /*
         * Separate area file and non-area block.
         */
        record.setSizeRecords(data.length);

        area = new byte[SIZE_OF_AREA];
        System.arraycopy(data, 0, area, 0, SIZE_OF_AREA);
        System.arraycopy(data, SIZE_OF_AREA, nonAreaBlock, 0,
                nonAreaBlock.length);

        /*
         * First word contains all zero for a valid record
         */
        if (byteArrayToInt(area, 0, 0) == 0) {

            /*
             * Check endians for 2nd word, if it is not 4, i.e., big endian,
             * swapping bytes.
             */
            if (byteArrayToInt(area, 4, 0) != 4) {
                endian = 1;
            }

            /*
             * Satellite identification number (SID)
             */
            int sid = byteArrayToInt(area, 8, endian);
            record.setSatelliteId(sid);

            /*
             * Get and set the satellite name from SID
             */
            String satelliteName = "";
            if (dao.getSatelliteId(sid) == null) {
                satelliteName = Integer.toString(sid);
            } else {
                satelliteName = dao.getSatelliteId(sid).getSatelliteName();
            }
            record.setSatelliteName(satelliteName);

            /*
             * Nominal year and Julian day
             */
            int yyddd = byteArrayToInt(area, 12, endian);

            /*
             * Nominal image time
             */
            int hhmmss = byteArrayToInt(area, 16, endian);

            /*
             * Set nominal time as data time and set seconds/millid
             */
            Calendar cal = convertJulianToCalendar(yyddd, hhmmss);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            record.setDataTime(new DataTime(cal));

            /*
             * Upper-left line in satellite coordinates
             */
            int ulline = byteArrayToInt(area, 20, endian);

            /*
             * Upper-left element in satellite coordinates
             */
            int ulelem = byteArrayToInt(area, 24, endian);

            /*
             * Number of lines in y-axis
             */
            Integer ny = byteArrayToInt(area, 32, endian);

            /*
             * Number of pixels in x-axis
             */
            Integer nx = byteArrayToInt(area, 36, endian);

            /*
             * Number of bytes each element (1, 2 or 4) int zdim =
             * byteArrayToInt (area, 40, endian);
             */

            /*
             * Line resolution
             */
            int yres = byteArrayToInt(area, 44, endian);

            /*
             * Element (pixel) resolution
             */
            int xres = byteArrayToInt(area, 48, endian);

            /*
             * Maximum number of bands per scan line int zres = byteArrayToInt
             * (area, 52, endian);
             */

            /*
             * Length of the data block line prefix
             */
            int prefix = byteArrayToInt(area, 56, endian);
            record.setPrefix(prefix);

            /*
             * User project number under which the area is created int task =
             * byteArrayToInt (area, 60, endian);
             */

            /*
             * Get and set the area creation time
             */
            yyddd = byteArrayToInt(area, 64, endian);
            hhmmss = byteArrayToInt(area, 68, endian);

            if (hhmmss != 0) {
                cal = convertJulianToCalendar(yyddd, hhmmss);
            }
            record.setCreationTime(cal);

            /*
             * Get and set image type, e.g., VIS, IR, IR2 from satellite name
             * and image type number
             */
            int imageTypeNumber = byteArrayToInt(area, 72, endian);
            record.setImageTypeNumber(imageTypeNumber);
            if (imageTypeNumber <= 0) {
                imageTypeNumber = -1;
            }
            String imageType = dao
                    .getImageType(Integer.toString(sid),
                            Integer.toString(imageTypeNumber)).get(0)
                    .getImageType();

            /*
             * String memo = byteArrayToString(area,96,endian) +
             * byteArrayToString(area,100,endian) +
             * byteArrayToString(area,104,endian) +
             * byteArrayToString(area,108,endian) +
             * byteArrayToString(area,112,endian) +
             * byteArrayToString(area,116,endian) +
             * byteArrayToString(area,120,endian) +
             * byteArrayToString(area,124,endian) +
             * byteArrayToString(area,128,endian);
             * 
             * Get area file number (AFN)
             */
            int areaId = byteArrayToInt(area, 128, endian);

            /*
             * Get and set the area name from AFN. If area name has a "|", parse
             * the file and the 1st part is the group name for the satellite.
             * The 2nd part is the area name.
             */
            if (dao.getAreaId(areaId) == null) {
                areaName = Integer.toString(areaId);
            } else {
                areaName = dao.getAreaId(areaId).getAreaName();
            }

            if (areaName.contains("|")) {
                String[] yyy = areaName.split("\\|", 2);

                /*
                 * Handle special cases for duplicate area file numbers (see
                 * Design document)
                 */
                if ((areaId == 281) || (areaId == 280)) {
                    if (satelliteName.equals("Global")) {
                        if (areaId == 281) {
                            areaName = "TPW_PCT";
                        } else {
                            areaName = "TPW_GPS";
                        }
                    } else {
                        satelliteName = yyy[0];
                        areaName = yyy[1];
                        record.setSatelliteName(satelliteName);
                    }
                } else {
                    satelliteName = yyy[0];
                    areaName = yyy[1];
                    record.setSatelliteName(satelliteName);
                }
            }
            record.setAreaName(areaName);
            String fileName = "";
            if (headers != null) {
                // fileName = (String) headers.get("traceId");
                File ingestFile = new File(
                        (String) headers.get(DecoderTools.INGEST_FILE_NAME));
                fileName = ingestFile.getName();
            }
            record.setInputFileName(fileName);

            /*
             * Acquire image type from input file name if needed.
             */
            if (imageType.equals("UNKNOWN") || satelliteName.equals("VAAC")) {
                if (fileName.contains("_20")) {
                    int index = fileName.indexOf("_20");
                    ;
                    imageType = fileName.substring(0, index);
                    if (imageType.contains("_")) {
                        index = imageType.lastIndexOf("_");
                        imageType = imageType.substring(index + 1,
                                imageType.length());
                    }
                }
            }
            record.setImageType(imageType);

            /*
             * Area file number int filno = byteArrayToInt (area, 128, endian);
             */

            /*
             * Data offset: byte offset to the start of the data block
             */
            int dataoff = byteArrayToInt(area, 132, endian);
            byte[] header = new byte[dataoff];
            record.setHeaderBlock(header);

            /*
             * Navigation offset: byte offset to the start of the navigation
             * block
             */
            int navoff = byteArrayToInt(area, 136, endian);

            /*
             * Validity code: if these bytes are non-zero, they must match the
             * first four bytes of each DATA block line prefix or the line's
             * data is ignored.
             */
            int validcode = byteArrayToInt(area, 140, endian);
            record.setValidCode(validcode);

            /*
             * Get and set actual image scan time
             */
            yyddd = byteArrayToInt(area, 180, endian);
            hhmmss = byteArrayToInt(area, 184, endian);

            if (hhmmss != 0) {
                cal = convertJulianToCalendar(yyddd, hhmmss);
            }
            record.setImageTime(cal);

            /*
             * Actual starting scan int ascan = byteArrayToInt (area, 188,
             * endian);
             * 
             * Line prefix documentation section length in bytes int predoc =
             * byteArrayToInt (area, 192, endian);
             * 
             * Line prefix calibration section length in bytes int precal =
             * byteArrayToInt (area, 196, endian);
             * 
             * Line prefix level map section length in bytes int prelvl =
             * byteArrayToInt (area, 200, endian);
             * 
             * Image source type: "VISR', "VAS', 'AAA', ERBE', "AVHR' String
             * srctyp = byteArrayToString (area, 204, endian);
             */

            /*
             * Calibration type: 'RAW', "TEMP', 'BRIT'
             */
            String caltyp = byteArrayToString(area, 208, endian);
            record.setCalType(caltyp);

            /*
             * Processing type int prctyp = byteArrayToInt (area, 212, endian);
             * 
             * POES signal type int sigtyp = byteArrayToInt (area, 216, endian);
             * 
             * POES ascending/descending int phase = byteArrayToInt (area, 220,
             * endian);
             * 
             * Original source srctyp String orgtyp = byteArrayToString (area,
             * 224, endian);
             */

            /*
             * Byte offset to the start of the calibration block
             */
            int offcal = byteArrayToInt(area, 248, endian);

            /*
             * Number of card image comments
             */
            int icomment = byteArrayToInt(area, 252, endian);

            /*
             * Create navigation block
             */
            int navsize;
            if (offcal == 0) {
                navsize = dataoff - navoff;
            } else {
                navsize = offcal - navoff;
            }
            byte[] navigation = new byte[navsize];
            System.arraycopy(nonAreaBlock, 0, navigation, 0, navsize);

            /*
             * Set data block.
             */
            byte[] imageData = new byte[data.length - dataoff - (80 * icomment)];
            System.arraycopy(data, dataoff, imageData, 0, imageData.length);
            record.setMessageData(imageData);

            /*
             * Projection type
             */
            String navtyp = byteArrayToString(navigation, 0, endian);

            /*
             * For map coverage compliance: 1: Mecator (MERC), 3: Lamber
             * Conformal (LAMB), 5: Polar Steoreographic (PS)
             */
            int resolution = 0;
            Integer iproj = 0;
            if (navtyp.trim().equals("PS") || navtyp.equals("MERC")
                    || navtyp.trim().equals("MET")) {
                resolution = byteArrayToInt(navigation, 16, endian) / 1000;

                if (navtyp.equals("MERC")) {
                    iproj = 1;
                } else if (navtyp.trim().equals("PS")) {
                    iproj = 5;
                }
            } else if (navtyp.equals("LAMB") || navtyp.equals("TANC")) {
                resolution = byteArrayToInt(navigation, 20, endian) / 1000;
                if (navtyp.equals("TANC")) {
                    resolution = resolution / 1000;
                }
                iproj = 3;
            } else {
                // native satellite projections ( not remapped )
                iproj = 7585;
            }
            record.setResolution(resolution);

            /*
             * Create map coverage. n4: standard latitude or spacing for TANC
             */
            int n4 = byteArrayToInt(navigation, 12, endian);
            int angdd = n4 / 10000;
            int angmm = (n4 / 100) - (angdd * 100);
            int angss = n4 - (angdd * 10000) - (angmm * 100);
            Float stdlat1 = (float) (angdd + (angmm / 60.) + (angss / 3600.));
            Float stdlat2 = stdlat1;
            double phi0r = stdlat1 * DTR;
            double sign = 1.;
            if (phi0r < 0.) {
                sign = -1.;
            }

            /*
             * Central longitude
             */
            int n6 = byteArrayToInt(navigation, 20, endian);
            angdd = n6 / 10000;
            angmm = (n6 / 100) - (angdd * 100);
            angss = n6 - (angdd * 10000) - (angmm * 100);
            Float clon = (float) (angdd + (angmm / 60.) + (angss / 3600.));
            if (byteArrayToInt(navigation, 36, endian) >= 0) {
                clon = -clon;
            }

            /*
             * Set pixel/grid spacing and earth radius and eccentricity. For
             * TANC, n5 is the standard latitude.
             */
            int spacing = byteArrayToInt(navigation, 16, endian);
            Float dx = (float) (spacing * xres);

            /*
             * Earth radius
             */
            int re = byteArrayToInt(navigation, 24, endian);

            /*
             * Eccentricity int n8 = byteArrayToInt (navigation, 28, endian);
             * double ecc = n8 / 1000000.;
             */

            /*
             * Image y-coordinate of north pole
             */
            int n2 = byteArrayToInt(navigation, 4, endian);

            /*
             * Image x-coordinate of north pole
             */
            int n3 = byteArrayToInt(navigation, 8, endian);

            /*
             * location of pole point (rxp, ryp); (1,1) at lower-left corner.
             */
            double rxp = (((double) (n3 - ulelem) / xres) + 1.);
            double ryp = (ny - ((double) (n2 - ulline) / yres));

            /*
             * Polar steoreographic projection (PS)
             */
            Float dy = 0.f;
            Float lllat = 0.f, lllon = 0.f, urlat = 0.f, urlon = 0.f;
            String proj = null;
            if (iproj == 5) {
                proj = "STR";
                dy = (float) spacing * yres;

                /*
                 * Compute lat/lon of the lower-left corner
                 */
                double dxp = (1. - rxp) * dx;
                double dyp = (1. - ryp) * dy;
                double alpha = 1. + Math.sin(Math.abs(phi0r));
                double rm = Math.sqrt(((dxp * dxp) + (dyp * dyp))) / alpha;
                lllat = (float) (sign * ((HALFPI - (2. * Math.atan(rm / re)))) * RTD);
                double thta;
                if (dyp != 0) {
                    dyp = (-dyp) * sign;
                    thta = (Math.atan2(dxp, dyp)) * RTD;
                    lllon = prnlon((float) (clon + thta));
                } else {
                    lllon = (float) clon;
                }

                /*
                 * Compute lat/lon of the upper-right corner
                 */
                dxp = (nx - rxp) * dx;
                dyp = (ny - ryp) * dy;
                rm = Math.sqrt(((dxp * dxp) + (dyp * dyp))) / alpha;
                urlat = (float) (sign * ((HALFPI - (2. * Math.atan(rm / re)))) * RTD);

                if (dyp != 0) {
                    dyp = (-dyp) * sign;
                    thta = (Math.atan2(dxp, dyp)) * RTD;
                    urlon = prnlon((float) (clon + thta));
                } else {
                    urlon = (float) clon;
                }
            }

            /*
             * Mercator projection
             */
            else if (iproj == 1) {
                proj = "MER";
                dy = (float) spacing * yres;

                /*
                 * compute lat/lon of the lower-left corner
                 */
                double dxp = 1. - rxp;
                double dyp = 1. - ryp;
                double rm = dx * dyp;
                double rcos = re * Math.cos(phi0r);
                double arg = Math.exp(rm / rcos);
                lllat = (float) (((2. * Math.atan(arg)) - HALFPI) * RTD);
                lllon = prnlon((float) (clon + (((dx * dxp) / rcos) * RTD)));

                /*
                 * compute lat/lon of the upper-right corner
                 */
                dxp = nx - rxp;
                dyp = ny - ryp;
                rm = dx * dyp;
                arg = Math.exp(rm / rcos);
                urlat = (float) (((2. * Math.atan(arg)) - HALFPI) * RTD);
                urlon = prnlon((float) (clon + (((dx * dxp) / rcos) * RTD)));
                urlon = prnlon(urlon);

                /*
                 * Lamber conformal conic projection (LAMB)
                 */
            } else if (iproj == 3) {
                if (stdlat1 > 0) {
                    proj = "LCC";
                } else {
                    proj = "SCC";
                }

                if (navtyp.equals("LAMB")) {
                    // earth radius
                    re = byteArrayToInt(navigation, 28, endian);

                    if (re <= 6200000.) {
                        re = RADIUS;
                    }

                    /*
                     * Standard latitude 2
                     */
                    int n5 = byteArrayToInt(navigation, 16, endian);
                    angdd = n5 / 10000;
                    angmm = (n5 / 100) - (angdd * 100);
                    angss = n5 - (angdd * 10000) - (angmm * 100);
                    stdlat2 = (float) (angdd + (angmm / 60.) + (angss / 3600.));

                    /*
                     * Central longitude. If west positive, make west negative.
                     */
                    int n7 = byteArrayToInt(navigation, 24, endian);
                    angdd = n7 / 10000;
                    angmm = (n7 / 100) - (angdd * 100);
                    angss = n7 - (angdd * 10000) - (angmm * 100);
                    clon = (float) (angdd + (angmm / 60.) + (angss / 3600.));

                    if (byteArrayToInt(navigation, 36, endian) >= 0) {
                        clon = -clon;
                    }

                    /*
                     * compute pixel/grid spacing and colatitudes
                     */
                    n6 = byteArrayToInt(navigation, 20, endian);
                    dx = (float) n6 * xres;
                    dy = (float) n6 * yres;
                } else if (navtyp.equals("TANC")) {

                    /*
                     * McIDAS uses Earth Radius 6371.1 (RADIUS). Navigation
                     * block km per pixel scaled by 10000., convert to meters
                     */
                    re = RADIUS;
                    dx = ((n4 / 10000.f) * xres) * 1000.f;
                    dy = ((n4 / 10000.f) * yres) * 1000.f;
                    rxp = ((((n3 / 10000.) - ulelem) / xres) + 1.);
                    ryp = (ny - (((n2 / 10000.) - ulline) / yres));

                    /*
                     * Standard angles are in decimal degree for TANC only
                     */
                    int n5 = byteArrayToInt(navigation, 16, endian);
                    stdlat1 = n5 / 10000.f;
                    stdlat2 = stdlat1;
                    phi0r = stdlat1 * DTR;
                    if (phi0r < 0.) {
                        sign = -1.;
                        proj = "SCC";
                    } else {
                        proj = "LCC";
                    }
                }

                /*
                 * compute pixel/grid spacing and colatitude.
                 */
                double psi1 = HALFPI - (Math.abs(stdlat1) * DTR);
                double psi2 = HALFPI - (Math.abs(stdlat2) * DTR);

                /*
                 * compute cone constant
                 */
                double ccone;
                if (psi1 == psi2) {
                    ccone = Math.cos(psi1);
                } else {
                    double tmp1 = Math.log(Math.sin(psi2) / Math.sin(psi1));
                    double tmp2 = Math.log(Math.tan(psi2 / 2.)
                            / Math.tan(psi1 / 2.));
                    ccone = tmp1 / tmp2;
                }

                /*
                 * Compute lat/lon of the lower-left corner. Sing = 1/-1 denotes
                 * NH/SH
                 */
                double dxp = 1. - rxp;
                double dyp = 1. - ryp;
                double rm = dx * Math.sqrt(((dxp * dxp) + (dyp * dyp)));
                double tmp = ccone / (re * Math.sin(psi1));
                double arg = Math.pow(rm * tmp, 1. / ccone)
                        * Math.tan(psi1 / 2.);
                lllat = (float) (sign * (HALFPI - (2. * Math.atan(arg))) * RTD);

                double thta;
                if (dyp != 0) {
                    dyp = -dyp;
                    thta = ((Math.atan2(dxp, dyp)) * RTD) / ccone;
                    lllon = prnlon((float) (clon + thta));
                } else {
                    lllon = (float) clon;
                }

                /*
                 * compute lat/lon of the upper-right corner
                 */
                dxp = nx - rxp;
                dyp = ny - ryp;
                rm = dx * Math.sqrt(((dxp * dxp) + (dyp * dyp)));
                arg = Math.pow(rm * tmp, 1. / ccone) * Math.tan(psi1 / 2.);
                urlat = (float) (sign * ((HALFPI - (2. * Math.atan(arg)))) * RTD);

                if (dyp != 0) {
                    dyp = -dyp;
                    thta = ((Math.atan2(dxp, dyp)) * RTD) / ccone;
                    urlon = (float) (clon + thta);
                    urlon = prnlon(urlon);
                } else {
                    urlon = (float) clon;
                }
            } else if (iproj == 7585) {
                // native satellite projections ( not remapped )
                proj = navtyp;
                int ilonrad = byteArrayToInt(navigation, 20, endian);
                clon = ilonrad / 10000000.f;
                clon = (float) Math.toDegrees(clon);

            }
            record.setProjection(proj);

            /*
             * Create map coverage.
             */
            McidasMapCoverage mapCoverage = null;
            try {
                if (iproj <= 5) {
                    mapCoverage = McidasSpatialFactory.getInstance()
                            .getMapCoverage(iproj, nx, ny, dx, dy, clon,
                                    stdlat1, stdlat2, lllat, lllon, urlat,
                                    urlon, re);
                } else {
                    // non-remapped Navigations
                    mapCoverage = McidasSpatialFactory.getInstance()
                            .getMapCoverage(iproj, nx, ny, clon, ulelem,
                                    ulline, xres, yres, navigation);
                }
            } catch (Exception e) {
                StringBuffer buf = new StringBuffer();
                if (iproj <= 5) {
                    buf.append(
                            "Error getting or constructing SatMapCoverage for values: ")
                            .append("\n\t");
                    buf.append("mapProjection=" + iproj).append("\n\t");
                    buf.append("nx=" + nx).append("\n\t");
                    buf.append("ny=" + ny).append("\n\t");
                    buf.append("dx=" + dx).append("\n\t");
                    buf.append("dy=" + dy).append("\n\t");
                    buf.append("clon=" + clon).append("\n\t");
                    buf.append("stdlat1=" + stdlat1).append("\n\t");
                    buf.append("stdlat2=" + stdlat2).append("\n\t");
                    buf.append("la1=" + lllat).append("\n\t");
                    buf.append("lo1=" + lllon).append("\n\t");
                    buf.append("la2=" + urlat).append("\n\t");
                    buf.append("lo2=" + urlon).append("\n");
                } else {
                    buf.append(
                            "Error getting or constructing SatMapCoverage for Navigation Type "
                                    + proj).append("\n");
                }
                throw new DecoderException(buf.toString(), e);
            }

            record.setReportType("mcidas");
            if (record != null) {
                record.setTraceId(traceId);
                record.setCoverage(mapCoverage);
                record.setPersistenceTime(TimeTools.getSystemCalendar());
                try {
                    record.constructDataURI();
                } catch (PluginException e) {
                    e.printStackTrace();
                }
            }
            return new PluginDataObject[] { record };
        } else {
            return new PluginDataObject[0];
        }
    }

    /**
     * Convert from a Julian date to a Gregorian date
     * 
     * @param julian
     *            The julian date
     * @return The Calendar
     */

    private Calendar convertJulianToCalendar(int julian, int hhmmss) {

        /*
         * The Julian day format is nYYDDD where n = 1, year > 2000 and n = 0,
         * year is prior to 2001. For example, 109244 -> 2009/244.
         */
        if (julian > 100000) {
            julian = julian + 1900000;
        } else {
            julian = julian + 1800000;
        }
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.set(Calendar.YEAR, julian / 1000);
        cal.set(Calendar.DAY_OF_YEAR, julian - ((julian / 1000) * 1000));

        int ihour = hhmmss / 10000;
        cal.set(Calendar.HOUR_OF_DAY, ihour);
        int minute = (hhmmss - (ihour * 10000)) / 100;
        cal.set(Calendar.MINUTE, minute);
        int second = hhmmss - (ihour * 10000) - (minute * 100);
        cal.set(Calendar.SECOND, second);
        cal.set(Calendar.MILLISECOND, 0);
        return cal;
    }

    /**
     * Convert the byte array to an int starting from the given offset.
     * 
     * @param b
     *            The byte array
     * @param offset
     *            The array offset
     * @param endian
     *            endian flag
     * @return The Integer
     */
    public Integer byteArrayToInt(byte[] b, int offset, int endian) {
        Integer value = 0;

        // little endian (MSB at the highest memory address)
        if (endian == 0) {
            for (int i = 0; i < 4; i++) {
                int shift = (3 - i) * 8;
                value += (b[i + offset] & 0x000000FF) << shift;
            }
        }

        // big endian (MSB at the lowest memory address)
        else if (endian == 1) {
            for (int i = 0; i < 4; i++) {
                int shift = i * 8;
                value += (b[i + offset] & 0x000000FF) << shift;
            }
        } else {
            System.out.println(" Illegal endian input ");
        }
        return value;
    }

    /**
     * Convert the byte array to a string starting from the given offset.
     * 
     * @param b
     *            The byte array
     * @param offset
     *            The array offset
     * @param endian
     *            little and big endian flag
     * @return The integer
     */
    public String byteArrayToString(byte[] b, int offset, int endian) {
        String str = null;

        /*
         * little endian (MSB at the highest memory address)
         */
        if (endian == 0) {
            byte[] byteArray = new byte[] { b[offset + 3], b[offset + 2],
                    b[offset + 1], b[offset + 0] };
            str = new String(byteArray);
        }

        /*
         * big endian (MSB at the lowest memory address)
         */
        else if (endian == 1) {
            byte[] byteArray = new byte[] { b[offset + 0], b[offset + 1],
                    b[offset + 2], b[offset + 3] };
            str = new String(byteArray);
        } else {
            System.out.println(" Illegal endian input ");
        }
        return str;
    }

    /**
     * Convert a longitude in degrees which fall within the range -180 to 180.
     * 
     * @param lon
     * @return
     */
    public float prnlon(float lon) {
        float dlon = lon - ((int) (lon / 360.f) * 360.f);
        if (lon < -180.) {
            dlon = lon + 360.f;
        } else if (lon > 180.) {
            dlon = (float) (lon - 360.);
        }
        return dlon;
    }

    public McidasDao getDao() {
        return dao;
    }

    public void setDao(McidasDao dao) {
        this.dao = dao;
    }

    /**
     * @param data
     * @return
     * @throws Exception
     */

    public PluginDataObject[] decodeFile(File inputFile) throws Exception {
        byte[] fileData = null;
        InputStream is = null;
        try {
            try {
                is = new FileInputStream(inputFile);

                fileData = new byte[(int) inputFile.length()];
                int bytesRead = is.read(fileData);
                // If we didn't or couldn't read all the data, signal the
                // fact by setting the data to null;
                if (bytesRead != fileData.length) {
                    fileData = null;
                }
                fileName = inputFile.getName();
                mr.setInputFileName(fileName);
            } catch (IOException ioe) {
                logger.error("Error reading input file " + inputFile.getName(),
                        ioe);
                fileData = null;
            }
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ioe) {
                    logger.error("Could not close input file "
                            + inputFile.getName());
                }
            }
        }
        return decode(fileData, null);
    }
}
