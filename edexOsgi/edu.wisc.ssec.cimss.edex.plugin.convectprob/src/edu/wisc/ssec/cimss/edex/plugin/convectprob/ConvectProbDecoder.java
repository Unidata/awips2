package edu.wisc.ssec.cimss.edex.plugin.convectprob;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;

import edu.wisc.ssec.cimss.common.dataplugin.convectprob.ConvectProbRecord;
import edu.wisc.ssec.cimss.common.dataplugin.convectprob.impl.ShapeObject;
import edu.wisc.ssec.cimss.edex.plugin.convectprob.impl.ConvectProbParser;

/**
 * NOAA/CIMSS Prob Severe Model Data Decoder
 *
 * Data decoder that reads shapefile records of NOAA/CIMSS Prob Severe Model
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 27, 2014 DCS 15298   lcronce     Initial Creation.
 *
 * </pre
 *
 * @author Lee Cronce
 * @version 1.0
 *
 */
public class ConvectProbDecoder extends AbstractDecoder {

    private final IUFStatusHandler statusHandler = UFStatus.getHandler(ConvectProbDecoder.class);

    private String traceId = null;

    /**
     * Default empty constructor
     */
    public ConvectProbDecoder() {
    }

    /**
     * Creates the data object that will be persisted to the database and
     * hdf5 repository
     *
     * @param File object passed by EDEX
     * @return PluginDataObject[] object of shape data
     * @throws Throwable
     */
    public PluginDataObject[] decode(File file) throws Throwable {
        ArrayList<ShapeObject> shapes = new ArrayList<ShapeObject>();
        ConvectProbParser parser = new ConvectProbParser(file);

        ShapeObject token;
        while (parser.hasNext()) {
            token = parser.next();
            try {
                if (token != null) {
                    shapes.add(token);
                }
            } catch (Exception e) {
                statusHandler.error("Problem creating new convectprob object from file" + file.getName(), e);
                return new PluginDataObject[0];
            }
        }
        ConvectProbRecord record = null;

        if (shapes.size() > 0) {
            record = new ConvectProbRecord(shapes.size());
            for (ShapeObject shape : shapes) {
                record.addShape(shape);
            }
        } else {
            return new PluginDataObject[0];
        }

        String validTime = parser.getValidTime();
        if (validTime != null) {
            Calendar c = TimeUtil.newCalendar();
            record.setInsertTime(c);
            try {
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd_HHmmss");
                record.setDataTime(new DataTime(dateFormat.parse(validTime)));
            } catch (Exception e) {
                statusHandler.error("Problem defining valid convectprob file time information using: " + validTime, e);
                return new PluginDataObject[0];
            }
        } else {
            return new PluginDataObject[0];
        }

        if (record != null) {
            record.setTraceId(traceId);
        }

        return new PluginDataObject[] { record };
    }

    /**
     * Set a trace identifier for the source data.
     *
     * @param traceId
     *            A unique identifier associated with the input data.
     */
    public void setTraceId(String traceId) {
        this.traceId = traceId;
    }

}
