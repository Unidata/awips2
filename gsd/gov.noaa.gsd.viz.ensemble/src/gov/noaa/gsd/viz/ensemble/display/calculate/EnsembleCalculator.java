package gov.noaa.gsd.viz.ensemble.display.calculate;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.grid.rsc.general.GeneralGridData;
import com.raytheon.viz.grid.rsc.general.GridMemoryManager;

/**
 * Ensemble calculator for loaded products, which is the base class to process
 * and prepare the data for calculating of different display types. The input
 * data is one frame data of all ensemble members(generic members, can be any
 * loaded same level and same unit product data). The output data is calculation
 * result, like mean, mode, max and so on.. The derived classes implements the
 * real calculations with interfaces calculatePoint(). Currently implemented
 * display types are the plan view and time series, others like time-high,
 * cross-section... will be implemented in next Version. Each type is with its
 * own data format. plan view is grid and time series is xy points. So the data
 * processing is different.
 * 
 * @author jing
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2014     5056      jing     Initial creation
 * 
 * </pre>
 */

public abstract class EnsembleCalculator {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleCalculator.class);

    /**
     * What's calculation, like MEAM, ERF, MAX... Is used by GUI
     */
    protected Calculation calculationType;

    /**
     * The display type id load mode, like plan view, time series... Is used by
     * ensemble displaying
     */
    protected DisplayType displayType;

    /**
     * Define the display type.
     */
    public enum DisplayType {
        PLANVIEW, TIMESERIES
    }

    public EnsembleCalculator(Calculation c, DisplayType displayType) {
        calculationType = c;
        this.displayType = displayType;
    }

    /**
     * @param c
     *            : What's calculation
     */
    public EnsembleCalculator(Calculation c) {
        calculationType = c;
    }

    public EnsembleCalculator() {

    }

    public DisplayType getDisplayType() {
        return displayType;
    }

    public void setDisplayType(DisplayType displayType) {
        this.displayType = displayType;
    }

    public String getName() {
        return calculationType.getTitle();
    }

    public Calculation getCalculation() {
        return calculationType;
    }

    /**
     * Calculate grid data for plan view display. The input data may with
     * different geometries, need do the geometry matching process before
     * calculating to make all member grids are in same domain. This makes
     * calculation easier and faster, but may cost more memory in some cases.
     * Calculating with different geometries is no extra memory cost, but is
     * very complicated and slow. The calculation result data is added into the
     * grid data monitor.
     * 
     * @param inputData
     *            - loaded grid data of all members in one frame
     * @return - Calculation result grid data
     */
    public List<GeneralGridData> calculate(List<List<GeneralGridData>> inputData) {

        if (inputData == null || inputData.size() == 0
                || inputData.get(0) == null || inputData.get(0).size() == 0)
            return null;

        GridGeometry2D geometry = matchGeometry(inputData);

        /**
         * Suppose the grid data is in same domain and unit, therefore the grid
         * size of the result is same as any input member.
         * 
         */
        GeneralGridData[] resaultGridData = new GeneralGridData[inputData
                .get(0).size()];

        for (int k = 0; k < inputData.get(0).size(); k++) {// List

            int imax = inputData.size();

            if (inputData.get(0).get(0).isVector()) {
                /**
                 * Vector grid data case Is not used in current implementation
                 */

                // Get U component data
                GeographicDataSource[] dataU = new GeographicDataSource[imax];
                for (int i = 0; i < imax; i++) {
                    dataU[i] = inputData.get(i).get(k).getUComponent();
                }

                // Get V component data
                GeographicDataSource[] dataV = new GeographicDataSource[imax];
                for (int i = 0; i < imax; i++) {
                    dataV[i] = inputData.get(i).get(k).getVComponent();
                }

                // Do Calculation
                GeneralGridData resultData = GeneralGridData
                        .createVectorDataUV(geometry,
                                doGridCalculation(dataU, imax, geometry),
                                doGridCalculation(dataV, imax, geometry),
                                inputData.get(0).get(0).getDataUnit());

                // To be monitored by GridMemoryManager
                resultData = GridMemoryManager.getInstance().manage(resultData);

                resaultGridData[k] = resultData;

            } else {

                /**
                 * Generic grid data case
                 */

                // Get the grid data
                GeographicDataSource[] data = new GeographicDataSource[imax];
                for (int i = 0; i < imax; i++) {// ArrayList
                    data[i] = inputData.get(i).get(k).getScalarData();
                }

                // Do Calculation
                GeneralGridData resultData = GeneralGridData.createScalarData(
                        geometry, doGridCalculation(data, imax, geometry),
                        inputData.get(0).get(0).getDataUnit());

                // To be monitored by GridMemoryManager
                resultData = GridMemoryManager.getInstance().manage(resultData);

                resaultGridData[k] = resultData;
            }
        }

        return Arrays.asList(resaultGridData);

    }

    /**
     * Do a grid calculation with members' data. loop through each xy point, get
     * all member data and calculate the data set.
     * 
     * @param data
     *            - member grids
     * @param imax
     *            - How many members
     * @param geometry
     *            - the geometry of the grids
     * @return - the grid buffer of the calculation result
     */
    protected FloatBufferWrapper doGridCalculation(GeographicDataSource[] data,
            int imax, GridGeometry2D geometry) {
        if (data == null || imax < 1)
            return null;

        GridEnvelope2D gridRange = geometry.getGridRange2D();
        int numGridPoints = gridRange.width * gridRange.height;
        int sizeInBytes = numGridPoints * 4;

        float[] result = new float[gridRange.width * gridRange.height];
        float[] workValue = new float[imax];

        // loop through each xy poit of the grids
        for (int y = 0; y < gridRange.height; y++) {
            for (int x = 0; x < gridRange.width; x++) {
                int countJ = 0;

                // Read out all available member data at same location
                for (int i = 0; i < imax; i++)
                    if (!Float.isNaN((float) (data[i].getDataValue(x, y))))
                        workValue[countJ++] = (float) (data[i].getDataValue(x,
                                y));

                // Do real calculation at this location
                result[y * gridRange.width + x] = calculatePoint(workValue,
                        countJ);

            }
        }

        ByteBuffer directBuffer = ByteBuffer.allocateDirect(sizeInBytes);
        directBuffer.order(ByteOrder.nativeOrder());
        FloatBuffer dataBuffer = directBuffer.asFloatBuffer();
        dataBuffer.put(result);
        dataBuffer.position(0);
        FloatBufferWrapper wrapper = new FloatBufferWrapper(dataBuffer,
                gridRange);
        return wrapper;
    }

    /**
     * Make all member grid data with same geometry. Currently, we use the
     * geometry of the input member with biggest domain.
     * 
     * @param inputData
     *            - loaded grid data of all members in one frame
     * @return-
     */
    private GridGeometry2D matchGeometry(List<List<GeneralGridData>> inputData) {

        /**
         * Search the biggest one as main geometry, if the members are with
         * different geometries. Go through each grid and do re-projection if
         * its geometry is different then the main geometry.
         */

        // Look for a geometry with most coverage
        GridGeometry2D geometry = inputData.get(0).get(0).getGridGeometry();
        int maxSize = 0;
        for (int k = 0; k < inputData.get(0).size(); k++) {
            int imax = inputData.size();
            int gridSize = 0;
            for (int i = 0; i < imax; i++) {
                gridSize = inputData.get(i).get(k).getScalarData()
                        .getGridGeometry().getGridRange2D().height
                        * inputData.get(i).get(k).getScalarData()
                                .getGridGeometry().getGridRange2D().width;
                if (gridSize <= 1)
                    continue;

                if (maxSize < gridSize) {
                    maxSize = gridSize;
                    geometry = inputData.get(i).get(k).getGridGeometry();
                }
            }
        }

        if (maxSize < 0 || geometry == null)
            return null;

        // Make sure all data with same geometry by re-projecting
        BilinearInterpolation bilinear = new BilinearInterpolation();
        bilinear.setMissingThreshold(1.0f);

        for (int k = 0; k < inputData.get(0).size(); k++) {
            int imax = inputData.size();

            for (int i = 0; i < imax; i++) {

                if (geometry.equals(inputData.get(i).get(k).getGridGeometry()) == true) {
                    // do nothing for same geometry
                } else {

                    // Do re-projection
                    try {
                        inputData.get(i).set(
                                k,
                                (inputData.get(i).get(k).reproject(geometry,
                                        bilinear)));
                    } catch (FactoryException | TransformException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }

                }
            }

        }

        return geometry;
    }

    /**
     * Calculation for time series display. Actually it prepares the data. The
     * data processing deals special cases like miss data and time match. Use
     * valid time when doing time match. Suppose all members are with same point
     * data XYDataList but y can be NaN. Now, We consider the same product
     * different size case that each member maybe with different number of
     * values on the X axis. For different products case, calculate at all valid
     * times with available data.
     * 
     * @param inputData
     *            - xy point data sets of time series resource members
     * @return- xy - the calculated point data set.
     */
    public XYDataList calculateTimeSeries(List<XYDataList> inputData) {
        XYDataList result = new XYDataList();
        if (inputData == null || inputData.isEmpty())
            return result;

        // Prepare data by using all X axis values, matching time.
        ArrayList<Object> xlist = new ArrayList<Object>();
        for (XYDataList xyList : inputData) {
            ArrayList<XYData> data = xyList.getData();

            for (int j = 0; j < xyList.getData().size(); j++) {

                if (data.get(j).getX() != null && data.get(j).getY() != null
                        && !xlist.contains(data.get(j).getX())) {
                    boolean matched = false;
                    for (int k = 0; k < xlist.size(); k++) {

                        // Remove different object with same valid time
                        if (((DataTime) xlist.get(k)).getValidTime()
                                .getTimeInMillis() == ((DataTime) (data.get(j)
                                .getX())).getValidTime().getTimeInMillis()) {
                            matched = true;
                            break;
                        }
                    }
                    if (!matched) {
                        xlist.add(data.get(j).getX());
                    }
                }
            }
        }
        if (xlist.isEmpty())
            return result;

        // Sort times

        Object[] xValues = xlist.toArray(new Object[xlist.size()]);
        // Arrays.sort(xValues); doesn't works
        // Temporary solution for sort it, to support Datatime only
        Object temp;
        for (int i = 0; i < xValues.length; i++) {
            for (int j = i + 1; j < xValues.length; j++) {

                if (((DataTime) xValues[i]).getValidTime().compareTo(
                        ((DataTime) xValues[j]).getValidTime()) > 0) {
                    temp = xValues[i];
                    xValues[i] = xValues[j];
                    xValues[j] = temp;
                }
            }
        }

        // Calculate all points.
        result.setData((ArrayList<XYData>) doTimeSeriesCalculation(inputData,
                xValues));

        return result;

    }

    /**
     * Loop through each time/x point, do calculating for time series display.
     * 
     * @param inputData
     *            - xy point data sets of time series resource members
     * @param xValues
     *            - time/x values for calculated resource.
     * @return- The calculated xy data set.
     */
    protected List<XYData> doTimeSeriesCalculation(List<XYDataList> inputData,
            Object[] xValues) {

        ArrayList<XYData> dataList = new ArrayList<XYData>();

        // Calculate for all point
        for (int i = 0; i < xValues.length; i++) {

            // Calculate for one x point. Look for y data of this x point first.
            ArrayList<Float> yValues = new ArrayList<Float>();
            for (XYDataList xyList : inputData) {
                ArrayList<XYData> data = xyList.getData();

                for (int j = 0; j < xyList.getData().size(); j++) {
                    if (data.get(j).getX() == null
                            && data.get(j).getY() == null)
                        continue;

                    if (xValues[i].equals(data.get(j).getX())) {
                        yValues.add(Float.parseFloat(data.get(j).getY()
                                .toString()));

                        break;
                    }

                }
            }

            if (yValues.isEmpty())
                continue;

            // Put the y values in the work buffer and do calculate the point.
            float[] workValue = new float[yValues.size()];
            for (int k = 0; k < yValues.size(); k++)
                workValue[k] = (float) yValues.get(k);
            XYData xy = new XYData(xValues[i], calculatePoint(workValue,
                    yValues.size()));

            dataList.add(xy);

        }

        return dataList;
    }

    /**
     * Do real calculation for one data set, This interface is implemented in
     * the derived classes.
     * 
     * @param workValue
     *            - inputed data set as a work buffer
     * @param length
     *            - how many data in the work buffer.
     * @return- calculated result.
     */
    abstract protected float calculatePoint(float[] workValue, int length);

}
