package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.area.NcGridGeometryAdapter;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsPaneManager;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.IPaneLayoutable;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneLayout;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.adapters.GridGeometrySerialized;
import com.raytheon.uf.common.serialization.jaxb.JAXBClassLocator;
import com.raytheon.uf.common.serialization.jaxb.JaxbDummyObject;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VariableSubstitutionUtil;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.reflect.SubClassLocator;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Bundle for Natl Cntrs Resources
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    02/20/10       #226      ghull       added Pane layout info to Bundle class.
 *    09/02/10       #307      ghull       use one timeMatcher for all descriptors
 *    11/15/11                 ghull       add resolveLatestCycleTimes
 *    04/26/12       #585      sgurung     Added rbdSequence 
 *    06/13/12       #817      Greg Hull   add resolveDominantResource()  
 *    06/20/12       #647      Greg Hull   add clone()
 *    06/29/12       #568      Greg Hull   implement Comparable
 *    11/25/12       #630      Greg Hull   getDefaultRBD()
 *    02/22/10       #972      Greg Hull   created from old RbdBundle
 *    05/14/13       #862      Greg Hull   implement INatlCntrsPaneManager
 *    11/21/13       #1066     Greg Hull   save off Native gridGeometries during clone()
 *    10/29/13       #2491     bsteffen    Use custom JAXB context instead of SerializationUtil.
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractRBD<T extends AbstractRenderableDisplay>
        implements INatlCntrsPaneManager, Comparable<AbstractRBD<?>> {
    private static JAXBManager jaxb;

    @XmlElement
    protected NcDisplayType displayType = NcDisplayType.NMAP_DISPLAY;

    // Could be an INcPaneLayout but right now the NcPaneLayout is the only
    // one that exists/is supported and there is a problem marshalling this as
    // an interface.
    @XmlElement
    protected NcPaneLayout paneLayout = new NcPaneLayout(1, 1);

    // TODO : wanted this to be an INcPaneID but JAXB can't handle interfaces.
    // Since
    // there is actually only the NcPaneID now, just make this an NcPaneID.
    @XmlElement
    protected NcPaneID selectedPaneId = new NcPaneID(); // 0,0

    @XmlElement
    protected boolean geoSyncedPanes;

    @XmlElement
    protected boolean autoUpdate;

    @XmlElement
    protected int rbdSequence;

    @XmlElement
    private NCTimeMatcher timeMatcher;

    // only set if read from Localization
    private LocalizationFile lFile;

    // this is defined as AbstractRenderableDisplay for JaxB but all the
    // renderable displays in the RBD must implement INatlCntrsRenderableDisplay
    @XmlElement
    @XmlElementWrapper(name = "displayList")
    protected T[] displays;

    // protected INatlCntrsRenderableDisplay[] displays;

    // if created from a loaded display, this it the display id.
    //
    protected int displayId = -1;

    @XmlAttribute
    protected String rbdName;

    // true if edited from the LoadRbd dialog
    private Boolean isEdited = false;

    // true if this Rbd was created from the DefaultRbd
    @XmlElement
    protected Boolean isDefaultRbd = false;

    // @XmlElement
    // protected Date dateCreated = null;
    //
    // @XmlElement
    // protected String createdBy = null;
    // @XmlElement
    // protected String description = null;

    // private HashMap<String,AbstractRenderableDisplay> displayPaneMap =
    // new HashMap<String,AbstractRenderableDisplay>

    public boolean isAutoUpdate() {
        return autoUpdate;
    }

    public void setAutoUpdate(boolean autoUpdate) {
        this.autoUpdate = autoUpdate;
        for (T disp : getDisplays()) {
            if (disp != null) {
                ((INatlCntrsDescriptor) disp.getDescriptor())
                        .setAutoUpdate(autoUpdate);
            }
        }
    }

    public LocalizationFile getLocalizationFile() {
        return lFile;
    }

    public void setLocalizationFile(LocalizationFile lFile) {
        this.lFile = lFile;
    }

    @Override
    public NcDisplayType getDisplayType() {
        return displayType;
    }

    public void setDisplayType(NcDisplayType rbdType) {
        this.displayType = rbdType;
    }

    public boolean isGeoSyncedPanes() {
        return geoSyncedPanes;
    }

    public void setGeoSyncedPanes(boolean geoSyncedPanes) {
        this.geoSyncedPanes = geoSyncedPanes;
    }

    public NcPaneID getSelectedPaneId() {
        return selectedPaneId;
    }

    public void setSelectedPaneId(NcPaneID selectedPaneId) {
        this.selectedPaneId = selectedPaneId;
    }

    public void setPaneLayout(NcPaneLayout paneLayout) {
        this.paneLayout = paneLayout;
    }

    public NcPaneLayout getPaneLayout() {
        return paneLayout;
    }

    // No id since it has not been assigned an id yet
    @Override
    public NcDisplayName getDisplayName() {
        return new NcDisplayName(displayId, getRbdName());
    }

    @Override
    public IPaneLayoutable getPane(INcPaneID pid) {
        if (paneLayout.containsPaneId(pid)
                && paneLayout.getPaneIndex(pid) < displays.length) {

            T pane = displays[paneLayout.getPaneIndex(pid)];

            if (pane != null && pane instanceof IPaneLayoutable) {
                return (IPaneLayoutable) pane;
            }
        }
        return null;
    }

    public Boolean isEdited() {
        return isEdited;
    }

    public void setIsEdited(Boolean isEdited) {
        this.isEdited = isEdited;
    }

    public Boolean getIsDefaultRbd() {
        return isDefaultRbd;
    }

    public void setIsDefaultRbd(Boolean isDefaultRbd) {
        this.isDefaultRbd = isDefaultRbd;
    }

    public void setRbdSequence(int seq) {
        this.rbdSequence = seq;
    }

    public int getRbdSequence() {
        return rbdSequence;
    }

    /**
     * Default constructor
     */
    public AbstractRBD() {
        timeMatcher = null;
        // ncEditor = null;
    }

    // used when creating an RBD to be written out.
    public AbstractRBD(NcPaneLayout paneLayout) {
        timeMatcher = null;
        // ncEditor = null;
        setPaneLayout(paneLayout);
        try {
            displays = (T[]) NcDisplayMngr.createDisplaysForNcDisplayType(this,
                    paneLayout);
        } catch (VizException e) {
            System.out.println(e.getMessage());
        }
    }

    // protected abstract T[] createDisplays( int num );

    public static AbstractRBD<?> clone(AbstractRBD<?> rbdBndl)
            throws VizException {
        try {
            NCTimeMatcher tm = new NCTimeMatcher(rbdBndl.getTimeMatcher());

            File tempRbdFile = File.createTempFile("tempRBD-", ".xml");

            // HACK Alert ; for Mcidas GVAR projections, the NAV_BLOCK_BASE64
            // parameter is a String, but
            // since the wkt format is assuming all projection params are
            // doubles, the CRS string will throw an
            // error on unmarshalling.
            // This temporary hack will substitude a dummy projection and save
            // off the wkt for the GVARs
            // and then call the McidasSpatialFactory to handle the parsing.
            //
            // Map<String,String> crsWktMap = new HashMap<String,String>();
            Map<String, GridGeometrySerialized> ggsMap = new HashMap<String, GridGeometrySerialized>();
            Map<String, AbstractRenderableDisplay> dispMap = new HashMap<String, AbstractRenderableDisplay>();

            NcGridGeometryAdapter geomAdapter = new NcGridGeometryAdapter();

            for (AbstractRenderableDisplay disp : rbdBndl.getDisplays()) {
                GeneralGridGeometry geom = disp.getDescriptor()
                        .getGridGeometry();

                if (geom.getEnvelope().getCoordinateReferenceSystem().getName()
                        .toString().startsWith("MCIDAS")) {

                    GridGeometrySerialized ggs = geomAdapter.marshal(geom);
                    ggsMap.put(((INatlCntrsRenderableDisplay) disp).getPaneId()
                            .toString(), ggs);
                    dispMap.put(((INatlCntrsRenderableDisplay) disp)
                            .getPaneId().toString(), disp);
                    // DefaultProjectedCRS pCRS =
                    // (DefaultProjectedCRS)geom.getCoordinateReferenceSystem();
                    // String crsWkt = pCRS.toWKT();
                    // crsWktMap.put(
                    // ((INatlCntrsRenderableDisplay)disp).getPaneId().toString(),
                    // crsWkt );
                    // something valid as a placeholder....
                    disp.getDescriptor().setGridGeometry(
                            PredefinedAreaFactory
                                    .getDefaultPredefinedAreaForDisplayType(
                                            rbdBndl.getDisplayType())
                                    .getGridGeometry());
                }
            }

            getJaxbManager().marshalToXmlFile(rbdBndl,
                    tempRbdFile.getAbsolutePath());

            AbstractRBD<?> clonedRbd = getRbd(tempRbdFile);

            for (AbstractRenderableDisplay disp : clonedRbd.getDisplays()) {
                String ggsKey = ((INatlCntrsRenderableDisplay) disp)
                        .getPaneId().toString();
                if (ggsMap.containsKey(ggsKey)) {
                    GeneralGridGeometry geom = geomAdapter.unmarshal(ggsMap
                            .get(ggsKey));
                    disp.getDescriptor().setGridGeometry(geom);

                    // another copy (should we save the original and set it
                    // back?)
                    geom = geomAdapter.unmarshal(ggsMap.get(ggsKey));
                    dispMap.get(ggsKey).getDescriptor().setGridGeometry(geom);
                }
            }

            clonedRbd.displayId = rbdBndl.displayId; // not serialized

            if (clonedRbd.getDisplayType() == null) {
                clonedRbd.setDisplayType(NcDisplayType.NMAP_DISPLAY);
            }

            clonedRbd.setTimeMatcher(tm);

            clonedRbd.setLocalizationFile(rbdBndl.getLocalizationFile());

            tempRbdFile.delete();

            // set the RbdName inside the renderable display panes
            clonedRbd.setRbdName(clonedRbd.getRbdName());

            for (AbstractRenderableDisplay disp : clonedRbd.getDisplays()) {
                if (disp instanceof IPaneLayoutable) {
                    ((IPaneLayoutable) disp).setPaneManager(clonedRbd);
                }
            }

            return clonedRbd;

        } catch (SerializationException e) {
            throw new VizException(e);
        } catch (JAXBException e) {
            throw new VizException(e);
        } catch (VizException e) {
            throw new VizException("Error loading rbd " + rbdBndl.rbdName
                    + " :" + e.getMessage());
        } catch (IOException e) { // from createTempFile
            throw new VizException(e);
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    public static AbstractRBD<?> createEmptyRbdForDisplayType(
            NcDisplayType dispType, NcPaneLayout pLayout) throws VizException {
        AbstractRBD<?> rbd = null;

        switch (dispType) {
        case NMAP_DISPLAY:
            rbd = new NcMapRBD(pLayout);
            break;
        case NTRANS_DISPLAY:
            rbd = new NTransRBD(pLayout);
            rbd.setRbdName("NTRANS");
            break;
        case SOLAR_DISPLAY:
            rbd = new SolarRBD(pLayout);
            rbd.setRbdName("Solar");
            break;
        }

        rbd.setIsDefaultRbd(true);
        rbd.setDisplayType(dispType);
        rbd.createDisplays();

        return rbd;
    }

    private void createDisplays() throws VizException {
        displays = (T[]) NcDisplayMngr.createDisplaysForNcDisplayType(this,
                getPaneLayout());
    }

    public static AbstractRBD<?> createRbdFromEditor(AbstractEditor ncEditor)
            throws VizException {
        if (ncEditor == null) {
            return null;
        }

        AbstractRBD<?> rbd = createEmptyRbdForDisplayType(
                NcEditorUtil.getNcDisplayType(ncEditor),
                (NcPaneLayout) NcEditorUtil.getPaneLayout(ncEditor));

        rbd.initRbdFromEditor(ncEditor);

        return rbd;
    }

    public void initRbdFromEditor(AbstractEditor ncEditor) throws VizException {

        selectedPaneId = new NcPaneID(); // 0,0

        NcDisplayName rbdDispName = NcEditorUtil.getDisplayName(ncEditor);
        displayId = rbdDispName.getId();
        // NcDisplayMngr.getNcDisplayID( NcEditorUtil.getDisplayName(ncEditor)
        // );
        rbdName = rbdDispName.getName();
        // NcDisplayMngr.getNcDisplayNameWithoutID(
        // NcEditorUtil.getDisplayName(ncEditor) );

        if (NcEditorUtil.isDisplayAvailableToLoad(ncEditor)) {
            // System.out.println("initializing RBD from an Available Editor??");
        }

        // NCDisplayPane[] dispPanes = (NCDisplayPane[])
        // ncEditor.getDisplayPanes();

        geoSyncedPanes = NcEditorUtil.arePanesGeoSynced(ncEditor);
        autoUpdate = NcEditorUtil.getAutoUpdate(ncEditor);

        displays = (T[]) NcDisplayMngr.createDisplaysForNcDisplayType(this,
                NcEditorUtil.getPaneLayout(ncEditor));

        for (int paneIndx = 0; paneIndx < paneLayout.getNumberOfPanes(); paneIndx++) {
            IDisplayPane pane = NcEditorUtil.getDisplayPane(ncEditor,
                    paneLayout.createPaneId(paneIndx));// new NcPaneID(r, c));

            T rDispPane = (T) pane.getRenderableDisplay();

            if (rDispPane instanceof IPaneLayoutable) {
                ((IPaneLayoutable) rDispPane).setPaneManager(this);
            }
            displays[paneIndx] = rDispPane;
        }

        setTimeMatcher(new NCTimeMatcher((NCTimeMatcher) displays[0]
                .getDescriptor().getTimeMatcher()));
    }

    /**
     * @return the rbdName
     */
    public String getRbdName() {
        return rbdName;
    }

    /**
     * @param name
     *            the rbdName to set
     */
    public void setRbdName(String rbdName) {
        this.rbdName = rbdName;
    }

    public String toXML() throws VizException {
        try {
            return getJaxbManager().marshalToXml(this);
        } catch (JAXBException e) {
            throw new VizException(e);
        }
    }

    //
    public INatlCntrsRenderableDisplay getDisplayPane(INcPaneID pid) {
        if (!paneLayout.containsPaneId(pid)) {
            System.out.println("NcMapRBD.getDisplayPane: pane id "
                    + pid.toString() + " is out of range.");
            return null;
        }
        //
        return (INatlCntrsRenderableDisplay) displays[paneLayout
                .getPaneIndex(pid)];
    }

    public abstract boolean addDisplayPane(T dispPane, NcPaneID pid);

    // if( !paneLayout.containsPaneId(pid) ) {
    // System.out.println("NcMapRBD.getDisplayPane: pane id "
    // + pid.toString() + " is out of range.");
    // return false;
    // }
    //
    // displays[paneLayout.getPaneIndex(pid)] = dispPane;
    //
    // // sync the descriptor's auto update with the value of this RBD.
    // ((INatlCntrsDescriptor)
    // displays[paneLayout.getPaneIndex(pid)].getDescriptor())
    // .setAutoUpdate( isAutoUpdate() );
    //
    // return true;
    // }

    // public INatlCntrsRenderableDisplay[] getINatlCntrsRenderableDisplays() {
    // IRenderableDisplay[] ncdisps = new IRenderableDisplay[displays.length];
    // int i=0;
    //
    // for( AbstractRenderableDisplay d : displays ) {
    // if( !(d instanceof INatlCntrsRenderableDisplay) ) {
    // System.out.println("Sanity Check in NcMapRBD: setting displays with non-INatlCntrsRenderableDisplay");
    // }
    // ncdisps[i++] = (INatlCntrsRenderableDisplay) d;
    // }
    // return (IRenderableDisplay[]) displays;
    // }

    public T[] getDisplays() {
        return displays;
    }

    public void setDisplays(T[] displays) {
        // for( AbstractRenderableDisplay d : displays ) {
        // if( !(d instanceof INatlCntrsRenderableDisplay ) ) {
        // System.out.println("Sanity Check in NcMapRBD: setting displays with non-INatlCntrsRenderableDisplay");
        // }
        // }
        // if( !(displays instanceof INatlCntrsRenderableDisplay[] ) ) {
        // System.out.println("Sanity Check in NcMapRBD: setting displays with non-INatlCntrsRenderableDisplay");
        // }
        this.displays = displays;
    }

    // TODO : add the ability to define what the default display type is. Ie.
    // NMAP or SWPC.
    //
    public static AbstractRBD<?> getDefaultRBD() throws VizException {
        NcDisplayType dfltDisplayType = NcDisplayType.NMAP_DISPLAY;
        return getDefaultRBD(dfltDisplayType);
    }

    // TODO : change/move this to be able to get user-based default RBDs.
    //
    public static AbstractRBD<?> getDefaultRBD(NcDisplayType displayType)
            throws VizException {

        String dfltRbdName = "";

        switch (displayType) {
        case NMAP_DISPLAY:
            dfltRbdName = NcPathConstants.DFLT_RBD;
            break;

        // If/when we need to we can save an NTRANS or SWPC default rbd to a
        // file and save in localization.
        // Right now there is nothing in the RBD so we just create an 'empty'
        // one.
        case NTRANS_DISPLAY:
            dfltRbdName = null; // NcPathConstants.DFLT_NTRANS_RBD;
            return AbstractRBD.createEmptyRbdForDisplayType(displayType,
                    new NcPaneLayout(1, 1));
        case SOLAR_DISPLAY:
            dfltRbdName = null; // NcPathConstants.DFLT_SOLAR_RBD;
            return AbstractRBD.createEmptyRbdForDisplayType(displayType,
                    new NcPaneLayout(1, 1));
        default:
            throw new VizException("Unable to find the default RBD name for "
                    + displayType.toString());
        }

        File rbdFile = NcPathManager.getInstance().getStaticFile(dfltRbdName);

        if (rbdFile == null) {
            throw new VizException("Unable to find the default RBD file for "
                    + displayType.toString());
        }

        try {
            AbstractRBD<?> dfltRbd = AbstractRBD.unmarshalRBD(rbdFile, null);

            // shouldn't need this but just in case the user creates a default
            // with
            // real resources in it
            dfltRbd.resolveLatestCycleTimes();
            dfltRbd.setIsDefaultRbd(true);

            return clone(dfltRbd);
        } catch (Exception ve) {
            throw new VizException("Error getting default RBD: "
                    + ve.getMessage());
        }
    }

    public static AbstractRBD<?> getRbd(File rbdFile) throws VizException {

        AbstractRBD<?> rbd = unmarshalRBD(rbdFile, null);

        // check for any required data that may be null or not set.
        // This shouldn't happen except possibly from an out of date RBD. (ie
        // older version)
        //
        if (rbd.displays == null || rbd.displays.length == 0) {
            throw new VizException(
                    "Error unmarshalling RBD: the renderable display list is null");
        }
        // getInitialArea can't return null.
        // for( AbstractRenderableDisplay d : rbd.getDisplays() ) {
        // if( ((INatlCntrsRenderableDisplay) d).getInitialArea() == null ) {
        // PredefinedArea dfltArea =
        // PredefinedAreaFactory.getDefaultPredefinedAreaForDisplayType(
        // rbd.getDisplayType() );
        // ((INatlCntrsRenderableDisplay) d).setInitialArea( dfltArea );
        // }
        // }

        // set the RbdName inside the renderable display panes
        // no longer need to do this since panes can now reference back to the
        // RBD to get name
        // rbd.setRbdName( rbd.getRbdName() );

        return rbd;
    }

    /**
     * Unmarshal a bundle
     * 
     * @param fileName
     *            the bundle to load
     * 
     * @param variables
     *            Optional: A map containing key value pairs to be used to
     *            perform variable substitution.
     * 
     * @return bundle loaded
     * 
     * @throws VizException
     */
    private static AbstractRBD<?> unmarshalRBD(File fileName,
            Map<String, String> variables) throws VizException {
        String s = null;
        try {
            FileReader fr = new FileReader(fileName);
            char[] b = new char[(int) fileName.length()];
            fr.read(b);
            fr.close();
            s = new String(b);

        } catch (Exception e) {
            throw new VizException("Error opening RBD file " + fileName, e);
        }

        return unmarshalRBD(s, variables);

    }

    /**
     * Unmarshal a bundle
     * 
     * @param bundle
     *            the bundle to load as a string
     * 
     * @param variables
     *            Optional: A map containing key value pairs to be used to
     *            perform variable substitution.
     * 
     * @return bundle loaded
     * 
     * @throws VizException
     */
    private static AbstractRBD<?> unmarshalRBD(String bundleStr,
            Map<String, String> variables) throws VizException {

        try {
            String substStr = VariableSubstitutionUtil.processVariables(
                    bundleStr, variables);

            AbstractRBD<?> b = (AbstractRBD<?>) getJaxbManager()
                    .unmarshalFromXml(substStr);

            if (b == null) {
                System.out.println("Unmarshalled rbd file is not a valid RBD?");
                return null;
            }

            // Make sure that all descriptors use the same timeMatcher instance.
            // All the timeMatchers should be the same but we need to share
            // them.
            // for( AbstractRenderableDisplay pane : b.getDisplays() ) {
            // MapDescriptor descr = (MapDescriptor)pane.getDescriptor();
            //
            // if( b.getTimeMatcher() == null ) {
            // b.setTimeMatcher( (NCTimeMatcher)descr.getTimeMatcher() );
            // }
            //
            // descr.setTimeMatcher( b.getTimeMatcher() );
            // }

            if (b.getTimeMatcher() == null) {
                b.setTimeMatcher(new NCTimeMatcher());
            }

            // This will make sure that all descriptors use the same timeMatcher
            // instance.
            // All the timeMatchers should be the same but we need to share
            // them.
            b.setTimeMatcher(b.getTimeMatcher());

            // if the dominant resource is not set then find the name
            // of the dominant resource in the list of all the resources in all
            // of the panes
            // in the descriptor and set the resource for the timeMatcher.
            // The dominant resource will be null if there are no requestable
            // resources.
            // The dominant resource has to be set...
            // if( b.getTimeMatcher().getDominantResource() == null ) {
            // String domRscName = b.getTimeMatcher().getDominantResourceName();
            // if( domRscName != null ) {
            // for( AbstractRenderableDisplay pane : b.getDisplays() ) {
            // MapDescriptor descr = (MapDescriptor)pane.getDescriptor();
            // ResourceList rscList = descr.getResourceList();
            // for( ResourcePair rscPair : rscList ) {
            // if( rscPair.getResourceData() instanceof
            // AbstractNatlCntrsRequestableResourceData ) {
            // AbstractNatlCntrsRequestableResourceData rscData =
            // (AbstractNatlCntrsRequestableResourceData)
            // rscPair.getResourceData();
            //
            // // NOTE that it is very possible that there are other resources
            // in this RBD with
            // // the same name but this shouldn't matter since they should all
            // yield the same times.
            // //
            // if( domRscName.equals( rscData.getResourceName() ) ) {
            // b.getTimeMatcher().setDominantResource(rscData);
            // break;
            // }
            // }
            // }
            // }
            // }
            // }

            return b;
        } catch (Exception e) {
            throw new VizException("Error loading bundle", e);
        }
    }

    /**
     * Builds and returns a JAXB manager which has the ability to
     * marshal/unmarshall all AbstractRBD types.
     * 
     * @return a JAXBManager to use for marshalling/unmarshalling RBDs.
     * @throws JAXBException
     *             if there are illegal JAXB annotations.
     */
    public static synchronized JAXBManager getJaxbManager()
            throws JAXBException {
        if (jaxb == null) {
            SubClassLocator locator = new SubClassLocator();
            Collection<Class<?>> classes = JAXBClassLocator.getJAXBClasses(
                    locator, AbstractRBD.class);
            locator.save();

            Class<?>[] jaxbClasses = new Class<?>[classes.size() + 1];
            classes.toArray(jaxbClasses);
            /*
             * Add JaxbDummyObject at the begining so properties are loaded
             * correctly
             */
            jaxbClasses[jaxbClasses.length - 1] = jaxbClasses[0];
            jaxbClasses[0] = JaxbDummyObject.class;

            jaxb = new JAXBManager(jaxbClasses);
        }
        return jaxb;
    }

    public NCTimeMatcher getTimeMatcher() {
        if (timeMatcher == null) {
            timeMatcher = (NCTimeMatcher) displays[0].getDescriptor()
                    .getTimeMatcher();
        }
        return timeMatcher;
    }

    public void setTimeMatcher(NCTimeMatcher timeMatcher) {
        // if( this.timeMatcher != null ) {
        // this.timeMatcher.removeDescriptor(desc)
        // }
        this.timeMatcher = timeMatcher;

        for (T disp : getDisplays()) {
            if (disp != null) {
                disp.getDescriptor().setTimeMatcher(timeMatcher);

                timeMatcher.addDescriptor((INatlCntrsDescriptor) disp
                        .getDescriptor());
            }
        }
    }

    // After and Rbd is unmarshalled it is possible for forecast resources
    // to have a cycle time of LATEST. We don't always want to resolve the
    // Rbd after unmarshalling it so we do this as a separate step here.
    //
    public boolean resolveLatestCycleTimes() {
        // loop through all of the
        for (T disp : getDisplays()) {
            ResourceList rl = disp.getDescriptor().getResourceList();
            for (int r = 0; r < rl.size(); r++) {
                ResourcePair rp = rl.get(r);
                if (rp.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData) {
                    AbstractNatlCntrsRequestableResourceData rscData = (AbstractNatlCntrsRequestableResourceData) rp
                            .getResourceData();
                    ResourceName rscName = rscData.getResourceName();

                    if (rscName.isForecastResource()
                            && rscName.isLatestCycleTime()) {

                        rscData.getAvailableDataTimes();

                        // TODO : do we leave as Latest, or flag
                        // as NoDataAvailable? Either way the resource is going
                        // to have to be able to handle this case.
                        //
                        if (rscName.isLatestCycleTime()) {
                            System.out
                                    .println("Unable to Resolve Latest cycle time for :"
                                            + rscName);
                        }
                    }
                }
            }
        }

        return true;
    }

    // if the dominantResourceName is set for the timeMatcher but the
    // dominantResourceData isn't then
    // find the dominant resource in the list and set it.
    public void resolveDominantResource() {

        ResourceName domRscName = timeMatcher.getDominantResourceName();

        if (domRscName != null && domRscName.isValid()
                && timeMatcher.getDominantResource() == null) {

            // loop thru the displays looking for the dominant resource
            //
            for (T disp : getDisplays()) {
                ResourceList rl = disp.getDescriptor().getResourceList();
                for (int r = 0; r < rl.size(); r++) {
                    ResourcePair rp = rl.get(r);
                    if (rp.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData) {
                        AbstractNatlCntrsRequestableResourceData rdata = (AbstractNatlCntrsRequestableResourceData) rp
                                .getResourceData();

                        if (domRscName.toString().equals(
                                rdata.getResourceName().toString())) {

                            timeMatcher.setDominantResourceData(rdata);
                            return;
                        }
                    }
                }
            }
        }

    }

    public void resolveAreaProvider() {
        // if the initial area provider name is not the same area as is stored
        // in the
        // renderable displays then create the areaProvider by either getting
        // the PredefinedArea
        // or by looking up the area-capable resource in the list of resources.

        // for( NCMapRenderableDisplay disp : displays ) {
        // if( )
        // ResourceList rl = disp.getDescriptor().getResourceList();
        // for (int r = 0; r < rl.size(); r++) {
        // ResourcePair rp = rl.get(r);
        // if (rp.getResourceData() instanceof
        // AbstractNatlCntrsRequestableResourceData) {
        // AbstractNatlCntrsRequestableResourceData rdata =
        // (AbstractNatlCntrsRequestableResourceData) rp
        // .getResourceData();
        //
        // }
        // }
        // }

    }

    // if the timeline has not been created then
    // get the dominant resource and initialize the timeMatcher
    public boolean initTimeline() {

        resolveDominantResource();

        return timeMatcher.loadTimes(true);
    }

    @Override
    public int compareTo(AbstractRBD<?> rbd) {
        if (rbdSequence == rbd.rbdSequence) {
            return rbdName.compareTo(rbd.rbdName);
        } else {
            return rbdSequence - rbd.rbdSequence;
        }
    }

}
