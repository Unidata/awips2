/*
 * gov.noaa.nws.ncep.viz.idft.rsc.IDFTResource
 * 
 * September 2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.idft.rsc;

import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import java.awt.Color;
import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Display IDFT data.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/11/2009   154        Gang Zhang  Initial creation.
 * 06/03/2010   migration  ghull       to11dr11
 * 09/30/2010   307        ghull       dont need to override queryRecords anymore
 * 10/25/2010   307        ghull       rm idftParam for the forecast day and use the
 *                                     forecast hour in the data.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */
public class IDFTResource extends
        AbstractNatlCntrsResource<IDFTResourceData, MapDescriptor> implements
        INatlCntrsResource {

    private IDFTResourceData idftRscData;

    private IFont font;

    private class IDFTRscDataObj implements IRscDataObject {
        Symbol pointSymbol;

        Vector vector;

        DataTime dataTime;

        // int pointNum; // not used
        double lat, lon;

        double direction;

        double distanceNm;

        String distanceInTenthString;

        @Override
        public DataTime getDataTime() {
            return dataTime;
        }
    }

    private class FrameData extends AbstractFrameData {

        ArrayList<IDFTRscDataObj> idftDataList;

        public FrameData(DataTime frameTime, int timeInt) {
            super(frameTime, timeInt);
            idftDataList = new ArrayList<IDFTRscDataObj>();
        }

        @Override
        public boolean updateFrameData(IRscDataObject rscDataObj) {
            if (!(rscDataObj instanceof IDFTRscDataObj)) {
                System.out
                        .println("IDFT:updateFrameData expecting IDFTRscDataObj instead of: "
                                + rscDataObj.getClass().getName());
                return false;
            } else {
                idftDataList.add((IDFTRscDataObj) rscDataObj);
                return true;
            }
        }

    }

    public IDFTResource(IDFTResourceData ncresourceData,
            LoadProperties loadProperties) {
        super(ncresourceData, loadProperties);
        this.idftRscData = (IDFTResourceData) resourceData;
    }

    private IDFTRscDataObj getIDFTRscDataObj(IdftRecord idftRec) {
        IDFTRscDataObj idftData = new IDFTRscDataObj();

        // the dataTime uses the issue time as the refTime and computes the
        // forecast hour from
        // the validTime in the record.
        // int fcstSecs = (int)(idftRec.getValidTime().getTime().getTime() /
        // 1000) -
        // (int)(idftRec.getIssueTime().getTime().getTime() / 1000);
        // idftData.dataTime = new DataTime( idftRec.getIssueTime().getTime(),
        // fcstSecs );
        idftData.dataTime = idftRec.getDataTime();
        // idftData.pointNum = idftRec.getPointNum(); // not used
        idftData.lat = idftRec.getLat();
        idftData.lon = idftRec.getLon();
        idftData.direction = idftRec.getDirection();
        idftData.distanceNm = idftRec.getDistanceNm();
        idftData.distanceInTenthString = Integer.toString((int) (Math
                .round(idftData.distanceNm * 10)));
        com.vividsolutions.jts.geom.Coordinate coor = new com.vividsolutions.jts.geom.Coordinate(
                idftData.lon, idftData.lat);
        idftData.vector = new Vector(
                null,
                new Color[] { Color.YELLOW },
                idftRscData.getArrowLineWidth().floatValue(),// lineWidth
                idftRscData.getArrowLength(),// sizeScale
                true, coor,
                gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType.ARROW,
                idftData.distanceNm, idftData.direction,
                idftRscData.getArrowLength(),// arrowHeadSize, using arrowLength
                                             // for a match
                true, "Vector", "Arrow"); // hard-code for clarity
        idftData.pointSymbol = new Symbol(null, new Color[] { Color.RED },
                idftRscData.getArrowLineWidth().floatValue(),// lineWidth, same
                                                             // as arrow's
                idftRscData.getPointSize(), true, coor, "Symbol", "DOT");

        return idftData;
    }

    @Override
    protected IRscDataObject[] processRecord(Object pdo) {
        if (!(pdo instanceof IdftRecord)) {
            System.out
                    .println("IDFT processRecord() : Expecting IdftRecord object instead of: "
                            + pdo.getClass().getName());
            return null;
        }

        IDFTRscDataObj idftRscDataObj = getIDFTRscDataObj((IdftRecord) pdo);

        if (idftRscData == null) {
            return new IDFTRscDataObj[0];
        } else {
            return new IDFTRscDataObj[] { idftRscDataObj };
        }
    }

    @Override
    public void paintFrame(AbstractFrameData frameData,
            IGraphicsTarget grphTarget, PaintProperties paintProps)
            throws VizException {
        FrameData currFrameData = (FrameData) frameData;
        RGB pointColorRGB = idftRscData.getPointColor();
        java.awt.Color pointColor = new java.awt.Color(pointColorRGB.red,
                pointColorRGB.green, pointColorRGB.blue);

        RGB arrowColorRGB = idftRscData.getArrowColor();
        java.awt.Color arrowColor = new java.awt.Color(arrowColorRGB.red,
                arrowColorRGB.green, arrowColorRGB.blue);

        DisplayElementFactory df = new DisplayElementFactory(grphTarget,
                this.descriptor);

        for (IDFTRscDataObj idftData : currFrameData.idftDataList) {
            IExtent extent = paintProps.getView().getExtent();
            double maxX = (extent.getMaxX() < 0 ? 0 : extent.getMaxX());
            double minX = (extent.getMinX() < 0 ? 0 : extent.getMinX());
            double maxY = (extent.getMaxY() < 0 ? 0 : extent.getMaxY());
            double minY = (extent.getMinY() < 0 ? 0 : extent.getMinY());
            maxX = (maxX > 19999 ? 19999 : maxX);
            minX = (minX > 19999 ? 19999 : minX);
            maxY = (maxY > 9999 ? 9999 : maxY);
            minY = (minY > 9999 ? 9999 : minY);

            PixelExtent correctedExtent = new PixelExtent(minX, maxX, minY,
                    maxY);

            idftData.pointSymbol.setColors(new java.awt.Color[] { pointColor });

            idftData.vector.setSizeScale(idftRscData.getArrowLength());
            idftData.vector.setLineWidth(idftRscData.getArrowLineWidth()
                    .floatValue());
            idftData.vector.setColors(new java.awt.Color[] { arrowColor });

            double[] zonePix = this.descriptor.worldToPixel(new double[] {
                    idftData.lon, idftData.lat });
            if (zonePix != null
                    && correctedExtent.contains(zonePix[0], zonePix[1])) {

                ArrayList<IDisplayable> displayElsArrow = df
                        .createDisplayElements(idftData.vector, paintProps);
                for (IDisplayable each : displayElsArrow) {
                    each.draw(grphTarget);
                    each.dispose();
                }

                ArrayList<IDisplayable> displayElsPoint = df
                        .createDisplayElements(idftData.pointSymbol, paintProps);
                for (IDisplayable each : displayElsPoint) {
                    each.draw(grphTarget);
                    each.dispose();
                }

                grphTarget.drawString(font, idftData.distanceInTenthString,
                        zonePix[0], zonePix[1], 0.0, TextStyle.NORMAL,
                        idftRscData.getDistanceColor(),
                        HorizontalAlignment.CENTER, 0.0);
            }
        }
    }

    @Override
    public void disposeInternal() {
        super.disposeInternal();
        if (font != null) {
            font.dispose();
        }
    }

    @Override
    public void resourceAttrsModified() {
        // don't need to do anything
    }

    @Override
    protected AbstractFrameData createNewFrame(DataTime frameTime, int timeInt) {
        return new FrameData(frameTime, timeInt);
    }

    @Override
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
        queryRecords();
    }
}
