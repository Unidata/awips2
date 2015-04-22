package gov.noaa.nws.ncep.edex.plugin.solarimage;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;

import java.io.ByteArrayInputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import nom.tam.fits.BasicHDU;
import nom.tam.fits.Fits;
import nom.tam.fits.FitsException;
import nom.tam.fits.ImageHDU;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;

/**
 * Data access object for retrieving/persisting solarimage data *
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer        Description
 * ------------ ---------- --------------- ------------------------
 * Dec 05, 2012 865        qzhou, sgurung  Initial creation.
 * Jan 07, 2013 865        qzhou           Change "_" to "-". Add wavelength
 *                                         default "NA". Add "Site" for Halpha.
 * Jan 28, 2013 865        qzhou           Changed float to double for intTime.
 * Aug 30, 2013 2298       rjpeter         Make getPluginName abstract
 * Mar 18, 2014            qzhou           Modified getObservationTime(BasicHDU).
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */
public class SolarImageDecoder extends AbstractDecoder {

    private final String pluginName;

    private static final String WAVELENGTH = "WAVELNTH";

    private static final String NAXIS = "NAXIS";

    private static final String DETECTOR = "DETECTOR";

    private static final String STEREO = "STEREO";

    private static final String OBSERVATORY = "OBSRVTRY";

    private static final String INT_TIME = "INT_TIME";

    private static final String SITE = "SITE";

    private static final String[] OBS_TIME_STRINGS = { "DATE_OBS", "DATE-OBS",
            "DATEOBS" };

    private static final String[] THEMATIC = { "themes", "covariances",
            "channels", "means" };

    private static final String DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    private static final String DATE_TIME_FORMAT_SHORT = "yyyy-MM-dd'T'HH:mm:ss";

    private static final String DATE_TIME_FORMAT_ALT = "yyyy/MM/dd'T'HH:mm:ss.SSS";

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            DATE_TIME_FORMAT);

    private static final SimpleDateFormat sdfShort = new SimpleDateFormat(
            DATE_TIME_FORMAT_SHORT);

    /**
     * 
     */
    public SolarImageDecoder(String name) {
        pluginName = name;
    }

    public PluginDataObject[] decode(byte[] data) throws Exception {

        Fits fits = null;
        BasicHDU hdu = null;
        SolarImageRecord record = new SolarImageRecord();
        int imageHDUNum = 0;

        try {
            fits = new Fits(new ByteArrayInputStream(data));
            // look for 2-dimensional image
            while ((hdu = fits.readHDU()) != null) {

                int naxis = hdu.getHeader().getIntValue(NAXIS);
                if ((hdu instanceof ImageHDU) && (naxis == 2)) {
                    break;
                }
                imageHDUNum++;
            }

        } catch (FitsException e) {
            logger.error(e);
            // return new PluginDataObject[0];
        }

        if (hdu == null) {
            logger.info("Could not find 2 dimensional image in fits file.");
            return new PluginDataObject[0];
        }

        record.setReportType("SOLARIMAGE");
        record.setImageHDUNum(imageHDUNum);

        // combine instrument-detector to instrument
        String instrument = hdu.getInstrument();
        String detector = hdu.getTrimmedString(DETECTOR);
        if (instrument == null) {
            instrument = "NA";
        } else {
            instrument = instrument.replaceAll("_", "-");
        }
        record.setInstrument(instrument
                + ((detector != null) ? "-" + detector.replaceAll("_", "-")
                        : ""));

        String telescope = hdu.getTelescope();
        if ((telescope != null) && telescope.startsWith("SDO/")) {
            telescope = telescope.substring(0, 3);
        }
        if ((telescope != null) && telescope.equals(STEREO)) { // find which one
                                                               // (A or B)
            telescope = hdu.getTrimmedString(OBSERVATORY); // STEREO_A, STEREO_B
        }
        record.setSatellite(telescope.replaceAll("_", "-"));

        if (telescope.equals("NSO-GONG")) {
            record.setSite(hdu.getTrimmedString(SITE));
        } else {
            record.setSite("NA");
        }

        String wavelen = hdu.getTrimmedString(WAVELENGTH);
        if (wavelen == null) {
            // wavelen may have been encoded as number (not String)
            if (hdu.getHeader().containsKey(WAVELENGTH)) {
                wavelen = hdu.getHeader().findCard(WAVELENGTH).getValue();
            }
        }

        if ((wavelen == null) || wavelen.isEmpty()
                || wavelen.trim().equals("0") || wavelen.trim().equals("0.0")) {
            wavelen = "NA";
        }

        record.setWavelength(wavelen.toUpperCase().replaceAll("_", "-"));

        // Temporary hack for Thematic data
        if ((hdu.getTrimmedString(WAVELENGTH) == null)
                && hdu.getTelescope().startsWith("SDO/AIA")) {
            BasicHDU tempHDU = fits.getHDU(1);
            if (tempHDU != null) {
                String value = tempHDU.getTrimmedString("TTYPE1");
                if (value != null) {
                    for (String str : THEMATIC) {
                        if (value.contains(str)) {
                            record.setWavelength("THEMATIC");
                            break;
                        }
                    }
                }
            }
        }

        record.setIntTime(hdu.getHeader().getDoubleValue(INT_TIME));

        Calendar obTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        obTime.setTime(getObservationTime(hdu));
        record.setDataTime(new DataTime(obTime));

        record.setRawData(data);
        record.constructDataURI();

        return new PluginDataObject[] { record };
    }

    private Date getObservationTime(BasicHDU hdu) throws ParseException {

        String dateObs = null;
        for (String date : OBS_TIME_STRINGS) {
            dateObs = hdu.getTrimmedString(date);
            if (dateObs != null) {
                break;
            }
        }

        // LASCO dateObs = Date + Time
        if ((dateObs != null) && (dateObs.length() <= 10)) {
            SimpleDateFormat sdfAlt = new SimpleDateFormat(DATE_TIME_FORMAT_ALT);

            String time = hdu.getTrimmedString("TIME-OBS"); // 12:00:00.000
            if (time.length() == 8) // 12:00:00
                time = time + ".000";

            dateObs = dateObs + "T" + time;
            return sdfAlt.parse(dateObs);

        }

        if ((dateObs != null) && (dateObs.length() == 19)) {
            return sdfShort.parse(dateObs);
        } else if (dateObs != null) {
            return sdf.parse(dateObs);
        } else {
            return null;
        }
    }

}
