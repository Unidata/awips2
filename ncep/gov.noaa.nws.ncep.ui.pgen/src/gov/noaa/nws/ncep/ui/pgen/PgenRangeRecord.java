/*
 * gov.noaa.nws.ncep.ui.pgen
 * 
 * November 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class used to hold the range record for a PGEN element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/13                    J. Wu       Initial Creation.
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 0.1
 */
public class PgenRangeRecord {

    public static final double RANGE_OFFSET = 10;

    private List<Coordinate> points;

    private List<Coordinate> extent;

    private boolean closed;

    /**
     * @param extent
     * @param points
     */
    public PgenRangeRecord() {
        super();
        this.extent = new ArrayList<Coordinate>();
        this.points = new ArrayList<Coordinate>();
        this.closed = false;
    }

    /**
     * @param extent
     * @param points
     */
    public PgenRangeRecord(List<Coordinate> points, boolean closed) {
        super();
        this.points = new ArrayList<Coordinate>();
        this.extent = new ArrayList<Coordinate>();
        if (points != null && points.size() > 0) {
            for (Coordinate cc : points) {
                this.points.add(new Coordinate(cc.x, cc.y));
            }

            this.extent.addAll(buildRangeBox(this.points, RANGE_OFFSET));
        }

        this.closed = closed;
    }

    /**
     * @param extent
     * @param points
     */
    public PgenRangeRecord(Coordinate[] points, boolean closed) {
        super();
        this.points = new ArrayList<Coordinate>();
        this.extent = new ArrayList<Coordinate>();
        if (points != null && points.length > 0) {
            for (Coordinate cc : points) {
                this.points.add(new Coordinate(cc.x, cc.y));
            }

            this.extent.addAll(buildRangeBox(this.points, RANGE_OFFSET));
        }
        this.closed = closed;
    }

    /**
     * @param extent
     * @param points
     */
    public PgenRangeRecord(List<Coordinate> extent, List<Coordinate> points,
            boolean closed) {
        super();
        this.extent = extent;
        this.points = points;
        this.closed = closed;
    }

    /**
     * @return the extent
     */
    public List<Coordinate> getExtent() {
        return extent;
    }

    /**
     * @return the extent
     */
    public List<Coordinate> getExtentWithoutBuffer() {

        List<Coordinate> rngBox = new ArrayList<Coordinate>();
        rngBox.add(new Coordinate(this.extent.get(0).x + RANGE_OFFSET,
                this.extent.get(0).y - PgenRangeRecord.RANGE_OFFSET));
        rngBox.add(new Coordinate(this.extent.get(1).x - RANGE_OFFSET,
                this.extent.get(1).y - RANGE_OFFSET));
        rngBox.add(new Coordinate(this.extent.get(2).x - RANGE_OFFSET,
                this.extent.get(2).y + RANGE_OFFSET));
        rngBox.add(new Coordinate(this.extent.get(3).x + RANGE_OFFSET,
                this.extent.get(3).y + RANGE_OFFSET));

        return rngBox;

    }

    /**
     * @param extent
     *            the extent to set
     */
    public void setExtent(List<Coordinate> extent) {
        this.extent = extent;
    }

    /**
     * @param extent
     * @param points
     */
    public void setRange(List<Coordinate> extent, List<Coordinate> points,
            boolean closed) {
        this.extent = extent;
        this.points = points;
        this.closed = closed;
    }

    /**
     * @return the points
     */
    public List<Coordinate> getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(List<Coordinate> points) {
        this.points = points;
    }

    /**
     * @return the closed flag
     */
    public boolean isClosed() {
        return closed;
    }

    /**
     * @param closed
     *            the closed to set
     */
    public void setClosed(boolean closed) {
        this.closed = closed;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPointsOnly(List<Coordinate> points, boolean closed) {
        if (points != null) {
            if (points != null && points.size() > 0) {
                for (Coordinate cc : points) {
                    this.points.add(new Coordinate(cc.x, cc.y));
                }

                this.extent.addAll(buildRangeBox(this.points, RANGE_OFFSET));
            }
        }

        this.closed = closed;
    }

    /**
     * @param extent
     * @param points
     */
    public static List<Coordinate> buildRangeBox(List<Coordinate> points,
            double buffer) {

        List<Coordinate> rangeBox = new ArrayList<Coordinate>();

        // Find the extent in x, y direction.
        double max_x = points.get(0).x;
        double max_y = points.get(0).y;
        double min_x = points.get(0).x;
        double min_y = points.get(0).y;
        for (Coordinate pp : points) {
            max_x = Math.max(max_x, pp.x);
            min_x = Math.min(min_x, pp.x);
            max_y = Math.max(max_y, pp.y);
            min_y = Math.min(min_y, pp.y);
        }

        // Add buffer
        max_x += buffer;
        min_x -= buffer;
        max_y += buffer;
        min_y -= buffer;

        // Build a rectangle (start from lower_left, go counter clockwise.
        rangeBox.add(new Coordinate(min_x, max_y));
        rangeBox.add(new Coordinate(max_x, max_y));
        rangeBox.add(new Coordinate(max_x, min_y));
        rangeBox.add(new Coordinate(min_x, min_y));

        return rangeBox;
    }

    /**
     * @return the max in x direction
     */
    public double getMaxx() {
        if (extent != null && extent.size() > 2) {
            return Math.max(extent.get(0).x, extent.get(2).x);
        } else {
            return Double.NaN;
        }
    }

    /**
     * @return the min in x direction
     */
    public double getMinx() {
        if (extent != null && extent.size() > 2) {
            return Math.min(extent.get(0).x, extent.get(2).x);
        } else {
            return Double.NaN;
        }
    }

    /**
     * @return the max in y direction
     */
    public double getMaxy() {
        if (extent != null && extent.size() > 2) {
            return Math.max(extent.get(0).y, extent.get(2).y);
        } else {
            return Double.NaN;
        }
    }

    /**
     * @return the min in y direction
     */
    public double getMiny() {
        if (extent != null && extent.size() > 2) {
            return Math.min(extent.get(0).y, extent.get(2).y);
        } else {
            return Double.NaN;
        }
    }

    /**
     * Check if this record is within another record.
     */
    public boolean within(PgenRangeRecord rr) {

        return (this.getMaxx() < rr.getMaxx() && this.getMinx() > rr.getMinx()
                && this.getMaxy() < rr.getMaxy() && this.getMiny() > rr
                .getMiny());
    }

    /**
     * Get the farest distance this record can placed within another record.
     */
    public double maxExtention(PgenRangeRecord rr) {
        double maxd = Double.MIN_VALUE;
        for (int ii = 0; ii < extent.size(); ii++) {
            maxd = Math.max(
                    maxd,
                    (extent.get(ii).x - rr.getExtent().get(ii).x)
                            * (extent.get(ii).x - rr.getExtent().get(ii).x)
                            + (extent.get(ii).y - rr.getExtent().get(ii).y)
                            * (extent.get(ii).y - rr.getExtent().get(ii).y));
        }

        return Math.sqrt(maxd);
    }

    /**
     * Deep copy
     */
    public PgenRangeRecord copy() {
        PgenRangeRecord newprr = new PgenRangeRecord();
        if (this.points != null && this.points.size() > 0) {
            for (Coordinate cc : this.points) {
                newprr.getPoints().add(new Coordinate(cc.x, cc.y));
            }
        }

        if (this.extent != null && this.extent.size() > 0) {
            for (Coordinate cc : this.extent) {
                newprr.getExtent().add(new Coordinate(cc.x, cc.y));
            }
        }

        newprr.closed = closed;

        return newprr;

    }

}
