package gov.nasa.msfc.sport.edex.glmdecoder.decoder;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;

import ucar.ma2.Array;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgMsgType;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * The Class GLMDecoder decodes the GLM Netcdf format files. The decoder
 * extracts flash, groups, and events and encodes them into BinLightningRecords.
 */
public class GLMDecoder {

    /** The Constant handler for logging. */
    private static final IUFStatusHandler handler = UFStatus
            .getHandler(GLMDecoder.class);

    /** The Constant Greenwich Mean Time zone. */
    private static final TimeZone gmt = TimeZone.getTimeZone("GMT");

    /**
     * The Enum Type to indentify various types of decoded types of detections.
     */
    private static enum Type {

        /** The flash lightning element. */
        FLASH("flash", "flash_time_offset_of_first_event"),
        /** The event lightning element. */
        EVENT("event", "event_time_offset"),
        /** The group lightning element. */
        GROUP("group", "group_time_offset");

        /** The name. */
        public final String name;

        /** The offset name. */
        public final String offsetName;

        /**
         * Instantiates a new type.
         * 
         * @param name
         *            the name of the lightning type
         * @param offsetName
         *            the offset name of the variable in the netcdf file that
         *            describes the offset of time for the particular record.
         */
        Type(String name, String offsetName) {
            this.name = name;
            this.offsetName = offsetName;
        }
    }

    /**
     * Decode the netcdf data from ingest and return the lightning flashes,
     * groups and events from the input netcdf GLM file.
     * 
     * @param data
     *            the data input array of the netcdf input file.
     * @return the plugin data object[] that contains all of the flashes,
     *         groups, and events in the file. They are of type
     *         BinLightningRecord.
     */
    public PluginDataObject[] decode(byte[] data) {
        WMOHeader wmoHdr = new WMOHeader(data);
        if (wmoHdr.isValid()) {
            data = removeWMOHeader(data, wmoHdr);
        }
        NetcdfFile netCdfFile = null;
        List<BinLightningRecord> records = new ArrayList<BinLightningRecord>();
        try {
            netCdfFile = NetcdfFile.openInMemory(null, data);

            Date productTime = decodeProductTime(netCdfFile);
            List<LightningStrikePoint> flashes = decode(Type.FLASH, netCdfFile,
                    productTime);
            List<LightningStrikePoint> groups = decode(Type.GROUP, netCdfFile,
                    productTime);
            List<LightningStrikePoint> events = decode(Type.EVENT, netCdfFile,
                    productTime);

            if (events != null) {
                BinLightningRecord eventsRecord = new BinLightningRecord(events);
                eventsRecord.setSource("GLMev");
                records.add(eventsRecord);
            }

            if (flashes != null) {
                BinLightningRecord flashrecord = new BinLightningRecord(flashes);
                flashrecord.setSource("GLMfl");
                records.add(flashrecord);
            }
            if (groups != null) {
                BinLightningRecord groupsRecord = new BinLightningRecord(groups);
                groupsRecord.setSource("GLMgr");
                records.add(groupsRecord);
            }

        } catch (IOException e) {
            handler.error(e.getMessage());
        } finally {
            if (netCdfFile != null) {
                try {
                    netCdfFile.close();
                } catch (IOException e) {

                }
            }
        }
        int size = records.size();
        PluginDataObject[] objs = new PluginDataObject[size];
        return records.toArray(objs);
    }

    /**
     * Removes the wmo header.
     * 
     * @param data
     *            the data input array.
     * @param wmoHdr
     *            the wmo hdr
     * @return the byte[] of the data without the WMOHeader
     */
    private byte[] removeWMOHeader(byte[] data, WMOHeader wmoHdr) {
        return Arrays.copyOfRange(data, wmoHdr.getMessageDataStart(),
                data.length);
    }

    /**
     * Decode product time which is the basis for all other time measurements in
     * the file. The times are based on January 1, 2000 at 12Z.
     * 
     * @param netCdfFile
     *            the net cdf file input
     * @return the date for the basis of the file
     * @throws IOException
     *             Signals that an I/O exception has occurred.
     */
    private Date decodeProductTime(NetcdfFile netCdfFile) throws IOException {
        Variable product_time_var = netCdfFile.findVariable("product_time");

        GregorianCalendar cal = new GregorianCalendar(gmt);
        // Dates are based on seconds since January 1, 2000 at 12Z
        cal.set(2000, 0, 1, 12, 0, 0);
        int secondstimesince2000 = (int) product_time_var.readScalarFloat();
        cal.add(Calendar.SECOND, secondstimesince2000);
        Date date = cal.getTime();
        return date;

    }

    /**
     * Decode actual elements in the file. The method decodes flashes, events,
     * and groups from the netcdf file.
     * 
     * @param type
     *            the type to decode, can be group, flash or event.
     * @param netCdfFile
     *            the net cdf file to read the data.
     * @param producttime
     *            the producttime is the basis time for the file.
     * @return the list of plugin objects that represent the lightning element
     *         type decoded.
     */
    private List<LightningStrikePoint> decode(Type type, NetcdfFile netCdfFile,
            Date producttime) {
        List<LightningStrikePoint> points = new ArrayList<LightningStrikePoint>();

        Variable lon = netCdfFile.findVariable(type.name + "_lon");
        Variable lat = netCdfFile.findVariable(type.name + "_lat");
        double event_lon_scale_factor = 1;
        double event_lon_add_offset = 0;
        double event_lat_scale_factor = 1;
        double event_lat_add_offset = 0;
        if (type == Type.EVENT) {
            event_lon_scale_factor = lon.findAttribute("scale_factor")
                    .getNumericValue().doubleValue();
            event_lon_add_offset = lon.findAttribute("add_offset")
                    .getNumericValue().doubleValue();
            event_lat_scale_factor = lat.findAttribute("scale_factor")
                    .getNumericValue().doubleValue();
            event_lat_add_offset = lat.findAttribute("add_offset")
                    .getNumericValue().doubleValue();
        }

        Variable offset = netCdfFile.findVariable(type.offsetName);
        try {
            Array lon_array = lon.read();
            Array lat_array = lat.read();
            Array offset_array = offset.read();

            while (lon_array.hasNext() && lat_array.hasNext()
                    && offset_array.hasNext()) {
                float lonValue;
                float latValue;

                if (type == Type.EVENT) {
                    int lon_short = ucar.ma2.DataType
                            .unsignedShortToInt(lon_array.nextShort());
                    lonValue = (float) (lon_short * event_lon_scale_factor + event_lon_add_offset);
                    latValue = (float) (ucar.ma2.DataType
                            .unsignedShortToInt(lat_array.nextShort())
                            * event_lat_scale_factor + event_lat_add_offset);
                } else {
                    lonValue = lon_array.nextFloat();
                    latValue = lat_array.nextFloat();
                }

                short offsetValue = offset_array.nextShort();
                GregorianCalendar cal = new GregorianCalendar(gmt);
                cal.setTimeInMillis(producttime.getTime() + offsetValue);
                LightningStrikePoint point = new LightningStrikePoint(latValue,
                        lonValue, cal, LtgMsgType.TOTAL_LIGHTNING);
                point.setType(LtgStrikeType.TOTAL_FLASH);
                points.add(point);
            }

        } catch (IOException e) {
            handler.error(e.getMessage());
        }
        return points;
    }

}
