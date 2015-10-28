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
package com.raytheon.uf.edex.ogc.common.gml3_1_1;

import java.text.ParseException;
import java.util.Arrays;
import java.util.List;

import net.opengis.gml.v_3_1_1.CoordType;
import net.opengis.gml.v_3_1_1.CoordinatesType;
import net.opengis.gml.v_3_1_1.DirectPositionType;
import net.opengis.gml.v_3_1_1.EnvelopeType;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.spatial.CoordinateUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Convert GML 3.1.1 envelopes to JTS envelopes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2011           bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class EnvelopeConverter {

    /**
     * Returns number of dimensions in envelope
     * 
     * @param env
     * @return
     * @throws Exception
     */
    public static int getDims(EnvelopeType env) throws OgcException {
        if (env.isSetSrsDimension() && env.getSrsDimension().intValue() > 1) {
            return env.getSrsDimension().intValue();
        }
        if (env.isSetLowerCorner()) {
            DirectPositionType lc = env.getLowerCorner();
            return lc.getValue().size();
        }
        if (env.isSetCoord()) {
            CoordType coord = env.getCoord().get(0);
            int size = 0;
            // this will be incorrect if dimensions aren't populated
            // monotonically
            size += coord.getX() != null ? 1 : 0;
            size += coord.getY() != null ? 1 : 0;
            size += coord.getZ() != null ? 1 : 0;
            return size;
        }
        if (env.isSetCoordinates()) {
            CoordinatesType coords = env.getCoordinates();
            return StringUtils.split(coords.getValue()).length;
        }
        if (env.isSetPos()) {
            DirectPositionType pos = env.getPos().get(0);
            return pos.getValue().size();
        }
        throw new OgcException(Code.InvalidParameterValue,
                "Unsupported envelope dimensions");
    }

    /**
     * Convert GML envelope to JTS envelope
     * 
     * @param env
     * @return
     * @throws Exception
     */
    public Envelope convert(EnvelopeType env) throws OgcException {
        Coordinate[] coords = getCoordinates(env);
        return new Envelope(coords[0], coords[1]);
    }

    /**
     * Convert GML envelope to JTS coordinates.
     * 
     * @param env
     * @param dims
     * @return
     * @throws Exception
     */
    public Coordinate[] getCoordinates(EnvelopeType env) throws OgcException {
        int dims = getDims(env);
        if (env.isSetLowerCorner() && env.isSetUpperCorner()) {
            return translate(env.getLowerCorner(), env.getUpperCorner(), dims);
        }
        if (env.isSetCoord()) {
            return handleCoordList(env.getCoord(), dims);
        }
        if (env.isSetCoordinates()) {
            return handleCoordinates(env.getCoordinates(), dims);
        }
        if (env.isSetPos()) {
            return handlePosList(env.getPos(), dims);
        }
        throw new OgcException(Code.InvalidFormat,
                "Unsupported envelope format");
    }

    /**
     * Convert direct positions to JTS bounding box. Output will only include
     * the number of dimensions specified in dims
     * 
     * @param lower
     * @param upper
     * @param dims
     * @return
     * @throws Exception
     */
    protected Coordinate[] translate(DirectPositionType lower,
            DirectPositionType upper, int dims) throws OgcException {
        List<Double> lowers = lower.getValue();
        List<Double> uppers = upper.getValue();
        if (lowers == null || uppers == null || lowers.size() < dims
                || uppers.size() < dims) {
            throw new OgcException(Code.InvalidFormat,
                    "Unsupported envelope format");
        }
        Coordinate l;
        Coordinate u;
        if (dims == 2) {
            l = new Coordinate(lowers.get(0), lowers.get(1));
            u = new Coordinate(uppers.get(0), uppers.get(1));
        } else if (dims == 3) {
            l = new Coordinate(lowers.get(0), lowers.get(1), lowers.get(2));
            u = new Coordinate(uppers.get(0), uppers.get(1), uppers.get(2));
        } else {
            throw new OgcException(Code.InvalidFormat,
                    "Unsupported envelope format");
        }
        return new Coordinate[] { l, u };
    }

    /**
     * Convert list of direct positions to JTS coordinates. Output will only
     * include the number of dimensions specified in dims
     * 
     * @param pos
     * @param dims
     * @return
     * @throws Exception
     */
    private Coordinate[] handlePosList(List<DirectPositionType> pos, int dims)
            throws OgcException {
        return translate(pos.get(0), pos.get(1), dims);
    }

    /**
     * Convert list of coords to JTS coordinates. Output will only include the
     * number of dimensions specified in dims
     * 
     * @param coords
     * @param dims
     * @return
     * @throws Exception
     */
    protected Coordinate[] handleCoordList(List<CoordType> coords, int dims)
            throws OgcException {
        CoordType c0 = coords.get(0);
        CoordType c1 = coords.get(1);
        return translate(createPos(c0), createPos(c1), dims);
    }

    /**
     * Convert GML coordinates to JTS coordinates. Output will only include the
     * number of dimensions specified in dims
     * 
     * @param coordinates
     * @param dims
     * @return
     * @throws Exception
     */
    protected Coordinate[] handleCoordinates(CoordinatesType coordinates,
            int dims) throws OgcException {
        try {
            List<Double[]> coords = CoordinateUtil.parseCoordinates(coordinates
                    .getValue());
            return translate(createPos(coords.get(0)),
                    createPos(coords.get(1)), dims);
        } catch (ParseException e) {
            throw new OgcException(Code.InvalidFormat,
                    "Invalid coordinate string");
        }
    }

    /**
     * Create direct position type from double values in strings
     * 
     * @param doubles
     * @return
     */
    protected DirectPositionType createPos(String[] doubles) {
        Double[] rval = new Double[doubles.length];
        for (int i = 0; i < doubles.length; ++i) {
            rval[i] = Double.parseDouble(doubles[i]);
        }
        return createPos(rval);
    }

    /**
     * Convert coordtype to direct position
     * 
     * @param coord
     * @return
     * @throws Exception
     */
    protected DirectPositionType createPos(CoordType coord) throws OgcException {
        Double[] rval;
        if (coord.isSetZ()) {
            rval = new Double[] { coord.getX().doubleValue(),
                    coord.getY().doubleValue(), coord.getZ().doubleValue() };
        } else if (coord.isSetY()) {
            rval = new Double[] { coord.getX().doubleValue(),
                    coord.getY().doubleValue() };
        } else {
            rval = new Double[] { coord.getX().doubleValue() };
        }
        return createPos(rval);
    }

    /**
     * Create direct position type from double values in strings
     * 
     * @param doubles
     * @return
     */
    protected DirectPositionType createPos(Double... coords) {
        DirectPositionType rval = new DirectPositionType();
        rval.setValue(Arrays.asList(coords));
        return rval;
    }

}
