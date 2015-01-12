package gov.noaa.nws.ncep.viz.common.area;

import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.JAXBException;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.crs.DefaultEngineeringCRS;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Read and create PredefinedAreas from localization. Also
 * 
 * TODO : add methods to save, delete and edit PredefinedAreas at the USER
 * localization level.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/28/11       #450      Greg Hull    Created / broke out from NmapResourceUtils. Use NcPathManager
 * 07/10/12       #646      Greg Hull    createPredefinedArea
 * 11/14/12       #630      Greg Hull    implement ILocalizationFileObserver
 * 01/18/13       #972      Greg Hull    NcDisplayType
 * 04/16/13       #863      Greg Hull    read menus table. 
 * 04/18/13       #863      Greg Hull    change key to be the areaName.
 * 05/07/13       #862      Greg Hull 	 Area Source ; change name from PredefinedAreasMngr
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PredefinedAreaFactory implements INcAreaProviderFactory {

    private static Map<String, LocalizationFile> predefinedAreasMap = null;

    private static List<VizException> badAreas = new ArrayList<VizException>();

    public PredefinedAreaFactory() {
    }

    @Override
    public void initialize(String srcName, String dataLoc, String configData)
            throws VizException {
        // should have already been created.
        AreaSource.createAreaSource(srcName);

        try {
            readPredefinedAreas();
        } catch (VizException v) {
            badAreas.add(v); // should treat this as a real exception instead of
                             // just an
                             // invalid area file.
        }
    }

    // read the NMAP areas from localization.
    // currently, only NcMAP areas are read from localization allow this
    // could be changed in the future to allow for NonMap NTRANS and
    // for SWPC displays to have more than the 1 default area.
    //
    private void readPredefinedAreas() throws VizException {

        // only null the first time. Empty if read and error.
        if (predefinedAreasMap == null) {
            predefinedAreasMap = new HashMap<String, LocalizationFile>();

            Map<String, LocalizationFile> areaLocFileMap = NcPathManager
                    .getInstance().listFiles(
                            NcPathConstants.PREDEFINED_AREAS_DIR,
                            new String[] { ".xml" }, false, true);

            if (areaLocFileMap.isEmpty()) {
                throw new VizException("No Predefined Area Files found under: "
                        + NcPathConstants.PREDEFINED_AREAS_DIR);
            }

            boolean dfltFound = false;

            // to a validity check by unmarshalling each of the files.
            // Also replace the localizationName (with the PredefinedArea
            // directory prefix
            // with the name of the area.
            for (LocalizationFile locF : areaLocFileMap.values()) {
                try {
                    PredefinedArea area = getPredefinedArea(locF);

                    if (area.getAreaName().equals(
                            NcDisplayType.NMAP_DISPLAY.getDefaultMap())) {
                        dfltFound = true;
                    }

                    locF.addFileUpdatedObserver(areaFileObserver);

                    if (predefinedAreasMap.containsKey(area.getAreaName())) {
                        badAreas.add(new VizException(
                                "Duplicate Area Name found for :"
                                        + area.getAreaName()));
                    } else {
                        predefinedAreasMap.put(area.getAreaName(), locF);
                    }
                } catch (VizException ve) {
                    badAreas.add(ve);
                }
            }

            if (!dfltFound) {
                System.out
                        .println("Could not find valid Default Predefined Area "
                                + NcDisplayType.NMAP_DISPLAY.getDefaultMap());
            }
        }
    }

    @Override
    public List<AreaName> getAvailableAreaNames() {
        List<AreaName> areaNames = new ArrayList<AreaName>();

        for (String aname : getAllAvailAreasForDisplayType()) {
            areaNames.add(new AreaName(getAreaSource(), aname));
        }
        return areaNames;
    }

    // make sure that the default map is first and maps defined for the main
    // menu are next and then all
    // the rest.
    //
    public static String[] getAllAvailAreasForDisplayType( /*
                                                            * NcDisplayType
                                                            * dispType
                                                            */) {
        // if dispType is null then return all types
        //
        if (predefinedAreasMap == null) {
            System.out
                    .println("getAllAvailPredefinedAreas() called before Areas have been read in.");
            return new String[0];
        }

        List<String> areaNamesList = new ArrayList<String>(
                predefinedAreasMap.keySet());
        String areaNamesArray[] = areaNamesList.toArray(new String[0]);

        return areaNamesArray;
    }

    // // to sort the list of area names. The default goes first
    // public static class AreaNamesComparator implements Comparator<String> {
    // private NcDisplayType dispType=NcDisplayType.NMAP_DISPLAY;
    //
    // public AreaNamesComparator( NcDisplayType dt ) {
    // dispType = dt;
    // }
    //
    // @Override
    // public int compare(String a1, String a2) {
    // if( a1.equals(a2) ) {
    // return 0;
    // }
    // if( a1.equals( dispType.getDefaultMap() ) ) {
    // return -1;
    // }
    // if( a2.equals( dispType.getDefaultMap() ) ) {
    // return 1;
    // }
    //
    // int a1menuIndx = (areaMenuNames == null ? 999 :
    // (areaMenuNames.contains(a1) ? areaMenuNames.indexOf(a1) : 999) );
    // int a2menuIndx = (areaMenuNames == null ? 999 :
    // (areaMenuNames.contains(a2) ? areaMenuNames.indexOf(a2) : 999) );
    //
    // if( a1menuIndx == a2menuIndx ) { // ie both -1
    // return a1.compareTo( a2 );
    // }
    // return (a1menuIndx < a2menuIndx ? -1 : 1 );
    // }
    // }

    // // it might be nice create a class specifically to store the predefined
    // // area but for now this will just be the Display (we need the zoomLevel
    // and
    // // mapCenter as well as the gridGeometry)
    // //
    public static PredefinedArea getDefaultPredefinedAreaForDisplayType(
            NcDisplayType dt) throws VizException {
        switch (dt) {
        case NMAP_DISPLAY:
            return getPredefinedArea(dt.getDefaultMap());
        case NTRANS_DISPLAY:
            return createDefaultNonMapArea(dt);
        case SOLAR_DISPLAY:
            return createDefaultNonMapArea(dt);
        case GRAPH_DISPLAY:
            return createDefaultNonMapArea(dt);
        }
        return null;
    }

    public static PredefinedArea getPredefinedArea(String areaName)
            throws VizException {

        if (areaName.equals(NcDisplayType.NTRANS_DISPLAY.getDefaultMap())) {
            return createDefaultNonMapArea(NcDisplayType.NTRANS_DISPLAY);
        } else if (areaName.equals(NcDisplayType.SOLAR_DISPLAY.getDefaultMap())) {
            return createDefaultNonMapArea(NcDisplayType.SOLAR_DISPLAY);
        } else if (areaName.equals(NcDisplayType.GRAPH_DISPLAY.getDefaultMap())) {
            return createDefaultNonMapArea(NcDisplayType.GRAPH_DISPLAY);
        }

        // String key = NcPathConstants.PREDEFINED_AREAS_DIR + File.separator +
        // areaName +".xml";
        String key = areaName;

        if (!predefinedAreasMap.containsKey(key)) {
            throw new VizException("Predefined Area : " + areaName
                    + ", is not in the Areas Map.");
        }

        return getPredefinedArea(predefinedAreasMap.get(key));
    }

    public static PredefinedArea getPredefinedArea(LocalizationFile lFile)
            throws VizException {
        try {
            PredefinedArea pa = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    PredefinedArea.class, lFile.getFile());

            // Todo : could fill this in with the filename???
            if (pa.getAreaName().isEmpty()) {
                VizException ve = new VizException(
                        "Error unmarshaling PredefinedArea: missing AreaName");
                throw ve;
            }

            return pa;

        } catch (SerializationException e) {
            throw new VizException(e);
        }

    }

    public static PredefinedArea clonePredefinedArea(PredefinedArea pArea)
            throws VizException {

        try {
            File tempRbdFile = File.createTempFile("tempArea-", ".xml");
            boolean mapCntrIsNull = (pArea.getMapCenter() == null);
            SerializationUtil.jaxbMarshalToXmlFile(pArea,
                    tempRbdFile.getAbsolutePath());
            String s = null;
            FileReader fr = new FileReader(tempRbdFile);
            char[] b = new char[(int) tempRbdFile.length()];
            fr.read(b);
            fr.close();
            s = new String(b);

            pArea = SerializationUtil.unmarshalFromXml(PredefinedArea.class, s);
            tempRbdFile.delete();

            // if null this will unmarshall as 0,0
            if (mapCntrIsNull)
                pArea.setMapCenter(null);
            return pArea;

        } catch (SerializationException e) {
            throw new VizException(e);
        } catch (IOException e) { // from createTempFile
            throw new VizException(e);
        } catch (JAXBException e) {
            throw new VizException(e);
        }
    }

    public static PredefinedArea createDefaultNonMapArea(NcDisplayType dt) {
        PixelExtent extent = new PixelExtent(0, 1000, 0, 1000);
        return createDefaultNonMapArea(dt, extent);
    }

    public static PredefinedArea createPredefinedArea(IGridGeometryProvider geom) {
        if (geom == null) {
            return null;
        } else if (geom instanceof PredefinedArea) {
            try {
                return PredefinedAreaFactory
                        .clonePredefinedArea((PredefinedArea) geom);
            } catch (VizException e) {
                System.out.println("Error cloning PA:" + e.getMessage());
                return null;
            }
        }
        return createPredefinedArea(
                new AreaName(geom.getSource(), geom.getProviderName()),
                geom.getGridGeometry(), geom.getMapCenter(),
                geom.getZoomLevel());
    }

    public static PredefinedArea createPredefinedArea(AreaName aName,
            GeneralGridGeometry geom, double[] mapCntr, String zlvl) {
        PredefinedArea parea = new PredefinedArea(aName.getSource(),
                aName.getName(), geom, mapCntr, zlvl,
                NcDisplayType.NMAP_DISPLAY);
        return parea;
    }

    public static PredefinedArea createDefaultNonMapArea(NcDisplayType dt,
            PixelExtent extent) {

        GeneralEnvelope envelope = new GeneralEnvelope(2);
        envelope.setRange(0, extent.getMinX(), extent.getMaxX());
        envelope.setRange(1, extent.getMinY(), extent.getMaxY());
        envelope.setCoordinateReferenceSystem(DefaultEngineeringCRS.CARTESIAN_2D);
        GeneralGridGeometry geom = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { 0, 0 }, new int[] { (int) extent.getWidth(),
                        (int) extent.getHeight() }, false), envelope);

        return new PredefinedArea(AreaSource.PREDEFINED_AREA,
                dt.getDefaultMap(), geom, new double[] { 500, 500 }, "1.0", dt);
    }

    private static ILocalizationFileObserver areaFileObserver = new ILocalizationFileObserver() {
        @Override
        public void fileUpdated(FileUpdatedMessage fumsg) {
            if (predefinedAreasMap == null) {
                return;// should be impossible
            }

            // first need to remove the entry for the updated file from the
            // map. Do it this way to best handle the case when a user changes
            // the
            // area name in an existing file.
            //
            String keyToRemove = null;

            for (Entry<String, LocalizationFile> areaEntry : predefinedAreasMap
                    .entrySet()) {
                LocalizationFile lf = areaEntry.getValue();

                // ?? check the context too?
                if (fumsg.getFileName().equals(lf.getName())) {
                    lf.removeFileUpdatedObserver(areaFileObserver);
                    keyToRemove = areaEntry.getKey();
                    break;
                }
            }

            try {
                synchronized (predefinedAreasMap) {

                    if (keyToRemove != null) {
                        predefinedAreasMap.remove(keyToRemove);
                    } else { // / ????
                        System.out
                                .println("sanity check: Area fileUpated(): couldn't find locFile name in the Area Map???");
                    }

                    // if added or updated, update the loc file in the map.
                    // else if deleted then check for another version of the
                    // file and 'revert'
                    LocalizationFile locFile;
                    PredefinedArea area;

                    if (fumsg.getChangeType() == FileChangeType.ADDED
                            || fumsg.getChangeType() == FileChangeType.UPDATED) {

                        locFile = NcPathManager.getInstance()
                                .getLocalizationFile(fumsg.getContext(),
                                        fumsg.getFileName());
                        if (locFile != null) {
                            area = getPredefinedArea(locFile);

                            // since the existing entry should have been
                            // removed, this means
                            // they have changed the name to a name that already
                            // exists.
                            if (predefinedAreasMap.containsKey(area
                                    .getAreaName())) {
                                System.out
                                        .println("Area fileUpdated: area "
                                                + area.getAreaName()
                                                + " is already defined in another LocalizationFile: "
                                                + predefinedAreasMap.get(
                                                        area.getAreaName())
                                                        .getName());
                            } // else ?????
                            else {
                                locFile.addFileUpdatedObserver(areaFileObserver);

                                predefinedAreasMap.put(area.getAreaName(),
                                        locFile);
                            }
                        }
                    } else if (fumsg.getChangeType() == FileChangeType.DELETED) {
                        locFile = NcPathManager.getInstance()
                                .getStaticLocalizationFile(fumsg.getFileName());
                        if (locFile != null) {
                            area = getPredefinedArea(locFile);

                            // shouldn't happen.
                            if (locFile.getContext().getLocalizationLevel() == LocalizationLevel.USER) {
                                System.out
                                        .println("sanity check: loc file deleted but still found a user-level version???");
                            } else {
                                locFile.addFileUpdatedObserver(areaFileObserver);

                                predefinedAreasMap.put(area.getAreaName(),
                                        locFile);
                            }
                        }
                    }
                }
            } catch (VizException e) {
                System.out
                        .println("Error Updateing Areas Map from Localization Update???");
                return;
            }
        }
    };

    @Override
    public AreaSource getAreaSource() {
        return AreaSource.PREDEFINED_AREA;// areaSource;
    }

    @Override
    public IGridGeometryProvider createGeomProvider(String areaName)
            throws VizException {
        return getPredefinedArea(areaName);
    }

    @Override
    public List<VizException> getInitializationExceptions() {
        return badAreas;
    }

    public static PredefinedArea savePredefinedArea(
            IGridGeometryProvider geomProv, String areaname, boolean overwrite)
            throws VizException {

        LocalizationFile lfile = predefinedAreasMap.get(areaname);
        boolean areaExists = (lfile != null);
        if (!overwrite && areaExists) {
            throw new VizException("A predefined area, " + areaname
                    + ", already exists");
        }

        PredefinedArea parea = PredefinedAreaFactory
                .createPredefinedArea(geomProv);

        if (parea == null) {
            throw new VizException("Error creating PredefinedArea for: "
                    + geomProv.getSource() + "/" + geomProv.getProviderName());
        }
        AreaName pareaName = new AreaName(AreaSource.PREDEFINED_AREA, areaname);
        parea.setAreaName(areaname);
        parea.setAreaSource(AreaSource.PREDEFINED_AREA.toString());

        if (!areaExists) {
            LocalizationContext usrCntxt = NcPathManager.getInstance()
                    .getContext(LocalizationType.CAVE_STATIC,
                            LocalizationLevel.USER);
            String lname = NcPathConstants.PREDEFINED_AREAS_DIR
                    + File.separator + areaname + ".xml";

            lfile = NcPathManager.getInstance().getLocalizationFile(usrCntxt,
                    lname);

            if (lfile == null) {
                throw new VizException("Error saving Predefined Area, "
                        + areaname + ": error creatinge localization File.");
            }
        }

        try {
            SerializationUtil.jaxbMarshalToXmlFile(parea, lfile.getFile()
                    .getAbsolutePath());
            lfile.save();

            if (!areaExists) {
                lfile.addFileUpdatedObserver(areaFileObserver);
                predefinedAreasMap.put(areaname, lfile);
            }

            return parea;
        } catch (LocalizationOpFailedException e) {
            throw new VizException("Error saving Predefined Area, " + areaname
                    + ": " + e.getMessage());
        } catch (SerializationException e) {
            throw new VizException("Error saving Predefined Area, " + areaname
                    + ": " + e.getMessage());
        }
    }
    // @Override
    // public void setInitializationData(IConfigurationElement config,
    // String propertyName, Object data) throws CoreException {
    // System.out.println("setInitializationData called with propertyName "+
    // propertyName+ " = "+(data == null ? "null" : data.toString()) );
    // }
}
