package com.raytheon.viz.awipstools.ui.action;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.awipstools.ui.action.LapsToolsData.LapsDomain;
import com.vividsolutions.jts.geom.Envelope;

/**
 * This class no longer performs all the file system, server input/output for
 * laps. LAPS support scripts now conduct the localization process.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * May 2009     #          bsteffen      Initial creation
 * Nov 2013     #          mccaslin      New design approach, changed from OS calls to file io, read xml code, etc
 * Feb 12, 2016 5242       dgilling      Remove calls to deprecated Localization APIs.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LapsToolsIO {

    private static final String WHATGOTIN_FILE_FRMT = "%s/%s.wgi";

    private static File fxaData;

    private static File lapsLogs;

    private static List<String> whatgotinFiles;

    private static List<String> dataChoices;

    static {
        // TODO all this configuration should be customizable by the user.
        // --- For what got in log files ---
        if (System.getenv("FXA_DATA") == null) {
            fxaData = new File("/data/fxa");
        } else {
            fxaData = new File(System.getenv("FXA_DATA"));
        }
        lapsLogs = new File(fxaData + "/laps/log/wgi");
        whatgotinFiles = Arrays.asList("sfc", "wind", "lq3driver", "cloud",
                "temp");
        dataChoices = Arrays.asList("Surface Analysis", "Wind Analysis",
                "Humidity Analysis", "Cloud Analysis", "Temperature Analysis");
    }

    public static Collection<String> getDataChoices() {
        return dataChoices;
    }

    public static String getLogs(String type) throws IOException, VizException {
        String wgiFile = String.format(WHATGOTIN_FILE_FRMT, lapsLogs,
                whatgotinFiles.get(dataChoices.indexOf(type)));
        String resultIO = loadWhatGotInFile(wgiFile, type);
        return (resultIO);
    }

    private static String loadWhatGotInFile(String wfile, String type)
            throws IOException {
        BufferedReader reader;
        StringBuilder output = new StringBuilder();
        File file = new File(wfile);

        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            String arg = String
                    .format("*** Cannot find expected log file for %s."
                            + "\n*** File %s ....does not appear to exist.\n\n",
                            type, file.getAbsolutePath());
            output.append(arg);
            return output.toString();
        }
        String line;
        int lineNumber = 0;
        while ((line = reader.readLine()) != null) {
            String arg = String.format("%04d: %s%n", ++lineNumber, line);
            output.append(arg);
        }
        reader.close();
        return output.toString();
    }

    public static LapsToolsData loadData() throws IOException, VizException,
            SpatialException {
        LapsToolsData data = new LapsToolsData();
        if (LapsToolsIO.readXmlFile(data)) {
            LapsToolsIO.readCountyWarningArea(data);
        } else {
            data = null;
        }
        return data;
    }

    public static boolean readXmlFile(LapsToolsData data) throws IOException,
            VizException {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        ILocalizationFile xmlLocalizationFile = pm.getLocalizationFile(lc,
                "LAPS/domain" + ".xml");
        if (!xmlLocalizationFile.exists()) {
            return false;
        }

        LapsDomain domain = new LapsDomain();
        try (InputStream inStream = xmlLocalizationFile.openInputStream()) {
            domain = JAXB.unmarshal(inStream, LapsDomain.class);
        } catch (LocalizationException e) {
            throw new VizException("xml is unreadable: "
                    + e.getLocalizedMessage());
        }

        data.setNx(domain.getNx());
        data.setNy(domain.getNy());
        data.setNz(domain.getNz());
        data.setGridSpacing(domain.getGridSpacing());
        data.setGridCenterLon(domain.getGridCenLon());
        data.setGridCenterLat(domain.getGridCenLat());
        return true;
    }

    public static void defaultDomain(LapsToolsData data) throws IOException,
            VizException {
        double distance = 111; // 111 km per 1 degree
        double gridSpacingDefault = 3500.;
        double dim = 200;
        double N = dim * dim;

        Envelope shapeArea = data.getValidAreaOrig();
        double widthY = Math.abs(shapeArea.getHeight() * distance);
        double widthX = Math.abs((shapeArea.getWidth() * distance)
                * (Math.cos(shapeArea.centre().x)));
        double aspectRatio = widthY / widthX;
        double buffer = 0.5;
        int nX = (int) (Math.sqrt(N / aspectRatio) + buffer);
        int nY = (int) ((aspectRatio * nX) + buffer);

        System.out.print("LAPS Tools IO:\nheight = " + shapeArea.getHeight()
                + " width = " + shapeArea.getWidth());
        System.out.print("\naspect ratio = " + aspectRatio);
        System.out.print("\nnX = " + nX + ", nY = " + nY + "\n");

        LapsDomain domain = new LapsDomain();
        domain.setNx(nX);
        domain.setNy(nY);
        domain.setNz(43);
        domain.setGridSpacing(gridSpacingDefault);
        domain.setGridCenLon(data.getCwaCenter().x);
        domain.setGridCenLat(data.getCwaCenter().y);

        data.setNx(domain.getNx());
        data.setNy(domain.getNy());
        data.setNz(domain.getNz());
        data.setGridSpacing(domain.getGridSpacing());
        data.setGridCenterLon(data.getCwaCenter().x);
        data.setGridCenterLat(data.getCwaCenter().y);
    }

    private static void readCountyWarningArea(LapsToolsData data)
            throws SpatialException {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("cwa", new RequestConstraint(LocalizationManager.getInstance()
                .getCurrentSite()));
        SpatialQueryResult[] result = SpatialQueryFactory.create().query("cwa",
                null, null, map, null);
        if (result == null || result.length == 0) {
            return;
        }
        data.setCwaArea(result[0].geometry.getEnvelopeInternal());
    }

    public static String getWriteXmlQuestion() {
        String date = new SimpleDateFormat("HH:mm").format(SimulatedTime
                .getSystemTime().getTime());
        return String
                .format("Are you sure you want write domain.xml?"
                        + "\nNote: Its %s and LAPS runs at ~20 minutes after the hour.",
                        date);
    }

    public static void writeXmlFile(LapsToolsData data) throws IOException,
            InterruptedException, VizException {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);
        ILocalizationFile xmlLocalizationFile = pm.getLocalizationFile(lc,
                "LAPS" + IPathManager.SEPARATOR + "domain.xml");
        LapsDomain lapsdomain = new LapsDomain();
        lapsdomain.setNx(data.getNx());
        lapsdomain.setNy(data.getNy());
        lapsdomain.setNz(data.getNz());
        lapsdomain.setGridSpacing(data.getGridSpacing());
        lapsdomain.setGridCenLat(data.getGridCenter().y);
        lapsdomain.setGridCenLon(data.getGridCenter().x);
        // marshal java object to XML file
        try (SaveableOutputStream outStream = xmlLocalizationFile
                .openOutputStream()) {
            JAXB.marshal(lapsdomain, outStream);
            outStream.save();
        } catch (LocalizationException e) {
            throw new VizException("Unable to save LapsDomain to xml.");
        }
    }
}
