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
package com.raytheon.uf.edex.wcs.reg;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.measure.unit.SI;

import org.apache.commons.lang.StringUtils;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.spatial.AltUtil;
import com.raytheon.uf.edex.ogc.common.spatial.Composite3DBoundingBox;
import com.raytheon.uf.edex.ogc.common.spatial.CrsLookup;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;
import com.raytheon.uf.edex.plugin.dataset.urn.URNLookup;
import com.raytheon.uf.edex.wcs.WcsConfig;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class CoverageTransform<D extends SimpleDimension, L extends SimpleLayer<D>> {

	protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

	protected String key;

	public CoverageTransform(String key) {
		this.key = key;
	}

    public List<CoverageDescription> transform(List<L> layers,
            boolean summaryOnly)
            throws WcsException {
		if (layers == null) {
			return null;
		}
		List<CoverageDescription> rval = new ArrayList<CoverageDescription>(
				layers.size());
        for (L layer : layers) {
			rval.add(transform(layer, summaryOnly));
		}
		return rval;
	}

    public CoverageDescription transform(L layer,
            boolean summaryOnly) throws WcsException {
		CoverageDescription rval = new CoverageDescription();
        rval.setIdentifier(URNLookup.localToUrn(layer.getName()));
		OgcGeoBoundingBox bbox = LayerTransformer.getGeoBoundingBox(layer);
        List<Composite3DBoundingBox> bboxes = getBboxes(layer);
        if (!bboxes.isEmpty() && WcsConfig.isProperty(WcsConfig.FORCE_KM_BBOX)) {
            List<Composite3DBoundingBox> newList = new ArrayList<Composite3DBoundingBox>(
                    bboxes.size());
            for (Composite3DBoundingBox box : bboxes) {
                if (box.hasVertical()) {
                    VerticalCoordinate vert = box.getVertical();
                    vert = AltUtil.convert(SI.KILOMETER, Reference.UNKNOWN,
                            vert);
                    newList.add(new Composite3DBoundingBox(box.getHorizontal(),
                            box.getNative2DCrsUrn(), vert));
                } else {
                    newList.add(box);
                }
            }
            bboxes = newList;
        }
		if (bbox != null) {
			rval.setCrs84Bbox(bbox);
            rval.setCrs(getSupportedCrsList(layer.getTargetCrsCode(), bboxes));
		} else {
			log.warn("Unable to get geographic information for layer: "
					+ layer.getName());
		}
		if (!summaryOnly) {
            describe(rval, layer, bboxes);
		}
		return rval;
	}

    /**
     * @param rval
     * @param layer
     * @throws WcsException
     */
    protected void describe(CoverageDescription rval, L layer,
            List<Composite3DBoundingBox> bboxes) throws WcsException {

		rval.setGridOffsets(Arrays.asList((double) layer.getNx(),
				(double) layer.getNy()));
		
		double minx = layer.getTargetMinx();
		double miny = layer.getTargetMiny();
		double maxx = layer.getTargetMaxx();
		double maxy = layer.getTargetMaxy();
		
		GeometryFactory geomFact = new GeometryFactory();
		Coordinate[] coords = new Coordinate[5];
		coords[0] = new Coordinate(minx, miny);
		coords[1] = new Coordinate(minx, maxy);
		coords[2] = new Coordinate(maxx, maxy);
		coords[3] = new Coordinate(maxx, miny);
		coords[4] = new Coordinate(minx, miny);
		
		LinearRing ring = geomFact.createLinearRing(coords);
        rval.setTimes(getTimes(layer.getTimes(), layer.isTimesAsRanges()));
		rval.setRangeFields(getRangeFields(layer));
		rval.setPolygon(geomFact.createPolygon(ring, new LinearRing[0]));
		rval.setGridType("urn:ogc:def:method:WCS:1.1:grid2dIn2dMethod");
		rval.setGridOrigin(Arrays.asList(minx, maxy));
		
		int nx = layer.getNx();
		int ny = layer.getNy();
		
		rval.setGridOffsets(Arrays.asList(getDn(nx, minx, maxx), 0.0, 0.0,
				-getDn(ny, miny, maxy)));
        rval.setGridCs("urn:ogc:def:cs:OGC:0.0:Grid2dSquareCS");

        rval.setBboxes(bboxes);

		String targetCrs = translateCrs(layer.getTargetCrsCode());
		rval.setGridBaseCrs(targetCrs);
		
		//add all the crs' from the bounding boxes
		List<String> crs = new ArrayList<String>(Arrays.asList(targetCrs));
		for(Composite3DBoundingBox bBox : bboxes) {
			String bBoxCrs = CrsLookup.createCrsURN(bBox);
			if(bBoxCrs != null && !crs.contains(bBoxCrs)) {
				crs.add(bBoxCrs);
			}
		}
		rval.setCrs(crs);

	}

    /**
     * Create a list composed of target crs and bounding box CRS URNs
     * 
     * @param targetCrs
     * @param bboxes
     * @return
     */
    private List<String> getSupportedCrsList(String targetCrs,
            List<Composite3DBoundingBox> bboxes) {
        List<String> rval = new ArrayList<String>(Arrays.asList(targetCrs));
        for (Composite3DBoundingBox bBox : bboxes) {
            String bBoxCrs = CrsLookup.createCrsURN(bBox);
            if (bBoxCrs != null && !rval.contains(bBoxCrs)) {
                rval.add(bBoxCrs);
            }
        }
        return rval;
    }

    protected List<Composite3DBoundingBox> getBboxes(L layer)
            throws WcsException {
        Composite3DBoundingBox bbox = new Composite3DBoundingBox(
                getHorizontal(layer), getVertical(layer));
        return Arrays.asList(bbox);
    }

    /**
     * @param layer
     * @return null if layer doesn't have vertical information
     * @throws WcsException
     */
    protected abstract VerticalCoordinate getVertical(L layer)
            throws WcsException;

    /**
     * @param layer
     * @return
     * @throws WcsException
     */
    protected ReferencedEnvelope getHorizontal(L layer)
            throws WcsException {
        CoordinateReferenceSystem crs2d;
        try {
            crs2d = CrsLookup.lookup(layer.getTargetCrsCode());
        } catch (Exception e) {
            log.error("Unable to parse target crs", e);
            throw new WcsException(Code.InternalServerError);
        }
        return new ReferencedEnvelope(layer.getTargetMinx(),
                layer.getTargetMaxx(), layer.getTargetMiny(),
                layer.getTargetMaxy(), crs2d);
    }

	protected double getDn(int nn, double n1, double n2) {
		return Math.abs(n1 - n2) / (double) nn;
	}

	public static String translateCrs(String crs) {
		if ( crs == null){
			return null;
		}
		if (crs.equalsIgnoreCase("crs:84")) {
            return "urn:ogc:def:crs:OGC:2:84";
		}
		String[] split = crs.split(":");
		List<String> parts = new ArrayList<String>(split.length + 1);
		parts.add("urn:ogc:def:crs");
		if (split.length == 2) {
			parts.add(split[0]);
			parts.add("");
			parts.add(split[1]);
		} else {
			parts.addAll(Arrays.asList(split));
		}
		return StringUtils.join(parts, ":");
	}

	/**
	 * @param layer
	 * @return
	 */
    protected abstract List<RangeField> getRangeFields(L layer);

    protected List<DataTime> getTimes(Set<Date> times, boolean asRanges) {
        if (times == null || times.isEmpty()) {
            return new ArrayList<DataTime>(0);
		}
        List<DataTime> rval;
        if (asRanges) {
            rval = new ArrayList<DataTime>((int) Math.ceil(times.size() / 2.0));
            Iterator<Date> iter = times.iterator();
            while (iter.hasNext()) {
                Date start = iter.next();
                Date end = (iter.hasNext() ? iter.next() : start);
                DataTime time = new DataTime(Calendar.getInstance(),
                        new TimeRange(start, end));
                time.setUtilityFlags(EnumSet.of(FLAG.PERIOD_USED));
                rval.add(time);
            }
        } else {
            rval = new ArrayList<DataTime>(times.size());
            for (Date d : times) {
                rval.add(new DataTime(d));
            }
        }

		return rval;
	}
}
