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
package com.raytheon.viz.redbookua;

import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.redbook.Activator;
import com.raytheon.viz.redbookua.rsc.RedbookUpperAirResource;

/**
 * Decoder for redbook upper air products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2010 1029       dfriedma    Initial creation
 * 
 * </pre>
 * 
 * @author dfriedma
 * @version 1.0
 * 
 *          Implementation notes: Redbook upper air products as currently
 *          received by AWIPS are malformed. This decoder behaves similar to
 *          AWIPS 1:
 * 
 *          1. Rather than decode the blocks, it just looks for the start of
 *          useful data with a string search.
 * 
 *          2. It does not attempt to handle entries that are split over a
 *          block.
 * 
 *          3. It does not attempt to detect the end of valid data.
 */
public class RedbookUpperAirDecoder {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookUpperAirDecoder.class);

    // Format: NMCPLTxxx001020002048153628501425polexpoleyzzddmmyyhhmm
    // Example: NMCPLT20A001020002048153628501425+0975+1688001803100116
    static final String INFO_LINE_PATTERN = "NMCPLT(\\w{3})00\\w{22}([\\+\\-\\d]{5})([\\+\\-\\d]{5})\\w{8}(\\w{4})";

    static final int INFO_LINE_LEVEL = 1;

    static final int INFO_LINE_POLE_X = 2;

    static final int INFO_LINE_POLE_Y = 3;

    static final int INFO_LINE_HHMM = 4;

    /*
     * **Item format: x,y,z0001A,,s,typ,ddfff,zzz,t,dpd,,,dz; Trailing fields
     * may be omitted; all fields beyond typ are optional. x,y screen
     * coordinates (1-4 char each) z zoom factor [A,B(?)=RAOB; D=aircraft;
     * F=satellite] s station/flight # typ type (1 character: 0=dry; 8=wet;
     * S=satellite; A=aircraft) ddfff wind direction/speed zzz height/pressure |
     * t temperature |all are characters dpd dew point depression |to plot
     * directly dz height change |
     */

    public static final String P_LATITUDE = "latitude";

    public static final String P_LONGITUDE = "longitude";

    public static final String P_STATION_TYPE = "stationType";

    public static final String P_TEMPERATURE = "temperature";

    public static final String P_DEWPOINT_DEPRESSION = "dewpointDepression";

    public static final String P_HEIGHT = "height";

    public static final String P_HEIGHT_CHANGE = "heightChange";

    public static final String P_WIND_SPEED = "windSpeed";

    public static final String P_WIND_DIRECTION = "windDir";

    public static final String P_ZOOM_LEVEL = "zoom";

    private static final int T_X = 0;

    private static final int T_Y = 1;

    private static final int T_ZOOM_ETC = 2;

    // always empty 3
    // station/flight id 4
    private static final int T_STATION_TYPE = 5;

    private static final int T_DIRECTION_SPEED = 6;

    private static final int T_HEIGHT = 7;

    private static final int T_TEMPERATURE = 8;

    private static final int T_DEWPOINT_DEPRESSION = 9;

    // blank 10
    // blank 11
    private static final int T_HEIGHT_CHANGE = 12;

    private static PointDataDescription pointDataDescription;

    private String dumpTime; // HHMM

    private PointDataContainer pointData;

    MathTransform getDomainToWorld() {
        /*
         * All upper air products have the same projection. TODO: Even though it
         * is always the same, this should not be hard-coded.
         */
        try {
            ProjectedCRS crs = MapUtil.constructNorthPolarStereo(
                    MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, 60,
                    -105);
            double[] ll = new double[2];
            double[] ur = new double[2];
            MathTransform toProj = MapUtil.getTransformFromLatLon(crs);
            toProj.transform(new double[] { -135.02, 10.43 }, 0, ll, 0, 1);
            toProj.transform(new double[] { -23.04, 40.31 }, 0, ur, 0, 1);
            GeneralEnvelope env = new GeneralEnvelope(2);
            env.setCoordinateReferenceSystem(crs);
            env.setRange(0, ll[0], ur[0]);
            env.setRange(1, ll[1], ur[1]);
            GridGeometry2D gg = new GridGeometry2D(new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { 2048, 1536 }, false), env);
            MathTransform mt2 = gg.getGridToCRS(PixelInCell.CELL_CENTER);

            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            return dmtf.createConcatenatedTransform(mt2, toProj.inverse());
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to create transform", e);
            return null;
        }
    }

    public void decode(byte[] data) {
        Pattern infoLinePattern = Pattern.compile(INFO_LINE_PATTERN);
        Pattern tokensPattern = Pattern.compile(",");
        for (int bi = 0; bi < data.length; ++bi)
            if (data[bi] == 0)
                data[bi] = 31;
        String text = new String(data, Charset.forName("ISO-8859-1"));
        String[] lines = text.split("\n");

        int li = 0;

        while (li < lines.length) {
            Matcher matcher = infoLinePattern.matcher(lines[li++]);
            /*
             * Did not start with a clear string so cannot assume the match will
             * starts at zero.
             */
            if (matcher.find()) {
                dumpTime = matcher.group(INFO_LINE_HHMM);
                break;
            }
        }

        PointDataDescription pdd = getPointDataDescription();
        if (pdd == null)
            return;

        PointDataContainer pdc = PointDataContainer.build(pdd, lines.length);

        MathTransform domainToWorld = getDomainToWorld();
        if (domainToWorld == null)
            return;
        DirectPosition2D domainCoord = new DirectPosition2D();
        DirectPosition2D worldCoord = new DirectPosition2D();

        while (li < lines.length) {
            String line = lines[li++];
            int ei = line.length() - 1;
            while (ei >= 0) {
                char c = line.charAt(ei);
                if (c >= ' ' && c != ';')
                    break;
                ei--;
            }
            line = line.substring(0, ei + 1);
            String[] tokens = tokensPattern.split(line);
            if (tokens.length < 6)
                continue;

            try {
                domainCoord.x = Integer.parseInt(tokens[T_X]);
                domainCoord.y = 1536 - Integer.parseInt(tokens[T_Y]);
                domainToWorld.transform(domainCoord, worldCoord);
                if (worldCoord.y >= 9999 || worldCoord.x >= 9999)
                    continue;
                PointDataView pdv = pdc.append();
                pdv.setFloat(P_LATITUDE, (float) worldCoord.y);
                pdv.setFloat(P_LONGITUDE, (float) worldCoord.x);
                pdv.setInt(P_ZOOM_LEVEL,
                        Integer.parseInt(tokens[T_ZOOM_ETC].substring(0, 1)));
                pdv.setString(P_STATION_TYPE,
                        tokens[T_STATION_TYPE].substring(0, 1));
                if (tokens.length > T_DIRECTION_SPEED) {
                    String t = tokens[T_DIRECTION_SPEED];
                    if (t.length() == 5) {
                        pdv.setFloat(P_WIND_DIRECTION,
                                Integer.parseInt(t.substring(0, 2)) * 10);
                        pdv.setFloat(P_WIND_SPEED,
                                Integer.parseInt(t.substring(2)));
                    }
                }
                if (tokens.length > T_TEMPERATURE) {
                    String t = tokens[T_TEMPERATURE];
                    t = t.trim();
                    if (t.length() > 0) {
                        // sometimes +nn, so make it text...
                        pdv.setString(P_TEMPERATURE, t);
                    }
                }
                if (tokens.length > T_HEIGHT) {
                    String t = tokens[T_HEIGHT];
                    t = t.trim();
                    if (t.length() > 0) {
                        pdv.setString(P_HEIGHT, t);
                    }
                }
                if (tokens.length > T_HEIGHT_CHANGE) {
                    String t = tokens[T_HEIGHT_CHANGE];
                    t = t.trim();
                    if (t.length() > 0) {
                        pdv.setString(P_HEIGHT_CHANGE, t);
                    }
                }
                if (tokens.length > T_DEWPOINT_DEPRESSION) {
                    String t = tokens[T_DEWPOINT_DEPRESSION];
                    t = t.trim();
                    if (t.length() > 0) {
                        pdv.setFloat(P_DEWPOINT_DEPRESSION, Integer.parseInt(t));
                    }
                }
            } catch (TransformException e) {
                // Ignore this entry
            } catch (RuntimeException e) {
                // Malformed entry; Do nothing.
            }
        }

        pointData = pdc;
    }

    private static synchronized PointDataDescription getPointDataDescription() {
        if (pointDataDescription == null) {
            InputStream is = RedbookUpperAirResource.class
                    .getResourceAsStream("/res/pointdata/redbookua.xml");
            if (is != null) {
                try {
                    try {
                        pointDataDescription = PointDataDescription
                                .fromStream(is);
                    } finally {
                        is.close();
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could load point data description", e);
                }
            }
        }
        return pointDataDescription;
    }

    public String getDumpTime() {
        return dumpTime;
    }

    public PointDataContainer getPointData() {
        return pointData;
    }
}
