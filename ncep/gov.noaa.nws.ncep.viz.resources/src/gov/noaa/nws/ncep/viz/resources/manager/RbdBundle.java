/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager.PaneLayout;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Date;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.VariableSubstitutionUtil;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList;

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
 *
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class RbdBundle implements ISerializableObject, Comparable<RbdBundle> {

    @XmlElement
    protected PaneLayout paneLayout = new PaneLayout(1, 1);

    @XmlElement
    protected PaneID selectedPaneId = new PaneID(0, 0);

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

	/** Contains the descriptors */
    @XmlElement
    @XmlElementWrapper(name = "displayList")
    protected NCMapRenderableDisplay[] displays;

    // if created from a loaded display, this it the display id. 
    // 
    protected int displayId=0;
    
    @XmlAttribute
    protected String rbdName;

    // true if edited from the LoadRbd dialog
    private Boolean isEdited = false;

//    @XmlAttribute
//    protected Date dateCreated = null;
//
//    @XmlAttribute
//    protected String createdBy = null;

    
    // private HashMap<String,AbstractRenderableDisplay> displayPaneMap =
    // new HashMap<String,AbstractRenderableDisplay>

    public boolean isAutoUpdate() {
        return autoUpdate;
    }

    public void setAutoUpdate(boolean autoUpdate) {
        this.autoUpdate = autoUpdate;
//        if (this.ncEditor != null) {
//            this.ncEditor.setAutoUpdate(autoUpdate);
//        }
        for (NCMapRenderableDisplay disp : displays) {
            if (disp != null) {
                ((NCMapDescriptor) disp.getDescriptor())
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

    public boolean isGeoSyncedPanes() {
        return geoSyncedPanes;
    }

    public void setGeoSyncedPanes(boolean geoSyncedPanes) {
        this.geoSyncedPanes = geoSyncedPanes;
    }

    public PaneID getSelectedPaneId() {
        return selectedPaneId;
    }

    public void setSelectedPaneId(PaneID selectedPaneId) {
        this.selectedPaneId = selectedPaneId;
    }

    public void setPaneLayout(PaneLayout paneLayout) {
        this.paneLayout = paneLayout;
    }

    public PaneLayout getPaneLayout() {
        return paneLayout;
    }

    public Boolean isEdited() {
        return isEdited;
    }

    public void setIsEdited(Boolean isEdited) {
        this.isEdited = isEdited;
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
    public RbdBundle() {
        timeMatcher = null;
//        ncEditor = null;
    }

    // used when creating an RBD to be written out.
    public RbdBundle(PaneLayout paneLayout) {
        timeMatcher = null;
//        ncEditor = null;
        setPaneLayout(paneLayout);
        displays = new NCMapRenderableDisplay[paneLayout.getNumberOfPanes()];// numPaneRows*numPaneColumns];
    }

    public static RbdBundle clone( RbdBundle rbdBndl ) throws VizException {
		try {
			NCTimeMatcher tm = new NCTimeMatcher( rbdBndl.getTimeMatcher() );

			File tempRbdFile = File.createTempFile("tempRBD-", ".xml");
	        
			SerializationUtil.jaxbMarshalToXmlFile( rbdBndl, 
									tempRbdFile.getAbsolutePath() );
			
			RbdBundle clonedRbd = RbdBundle.getRbd( tempRbdFile );
			
			clonedRbd.displayId = rbdBndl.displayId; // not serialized
			
			clonedRbd.setTimeMatcher( tm );
    		
			clonedRbd.setLocalizationFile( rbdBndl.getLocalizationFile() );
			
			tempRbdFile.delete();

			// set the RbdName inside the renderable display panes
			clonedRbd.setRbdName( clonedRbd.getRbdName() );
			
    		return clonedRbd;
    		
		} catch (SerializationException e) {
			throw  new VizException( e );
		} catch (VizException e) {
			throw new VizException("Error loading rbd "+rbdBndl.rbdName+" :"+e.getMessage()  );
		} catch (IOException e) { // from createTempFile
			throw  new VizException( e ); 
		}
    }
//    public NCMapEditor getNcEditor() {
//        return ncEditor;
//    }
//
//    public void setNcEditor(NCMapEditor ncEditor) {
//        this.ncEditor = ncEditor;
//        this.ncEditor.setAutoUpdate(isAutoUpdate());
//    }

    public void initFromEditor(NCMapEditor ncEditor) {
        if (ncEditor == null) {
            return;
        }

        paneLayout = ncEditor.getPaneLayout();
        selectedPaneId = new PaneID(0, 0);

        displayId = NmapUiUtils.getNcDisplayID( ncEditor.getDisplayName() );
        
        rbdName = new String( NmapUiUtils.
        		getNcDisplayNameWithoutID( ncEditor.getDisplayName() ));

        // NCDisplayPane[] dispPanes = (NCDisplayPane[])
        // ncEditor.getDisplayPanes();

        geoSyncedPanes = ncEditor.arePanesGeoSynced();
        autoUpdate = ncEditor.getAutoUpdate();

        int numDisps = ncEditor.getDisplayPanes().length;

        displays = new NCMapRenderableDisplay[numDisps];

        for (int r = 0; r < paneLayout.getRows(); r++) {
            for (int c = 0; c < paneLayout.getColumns(); c++) {
                int paneIndx = c + r * paneLayout.getColumns();
                NCDisplayPane pane = (NCDisplayPane) ncEditor
                        .getDisplayPane(new PaneID(r, c));
                displays[paneIndx] = (NCMapRenderableDisplay) pane
                        .getRenderableDisplay();
                
            }
        }

        setTimeMatcher( 
        		new NCTimeMatcher(  
        		     (NCTimeMatcher) displays[0].getDescriptor().getTimeMatcher() ) );
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
        
        if( displays == null ) {
        	return;
        }
        
        for( NCMapRenderableDisplay display : displays ) {
        	if( display == null ) 
        		continue; 
        	
        	String idStr = (displayId == 0 ? "" : Integer.toString( displayId )+"-" );
        	
        	if( paneLayout.getNumberOfPanes() > 1 ) {
        		display.setPaneName( idStr + getRbdName()+"("+display.getPaneId()+")" );
        	}
        	else {
        		display.setPaneName( idStr + getRbdName() );
        	}
        }
    }

    public String toXML() throws VizException {
        try {
            return SerializationUtil.marshalToXml(this);
        } catch (JAXBException e) {
            throw new VizException(e);
        }
    }

    //
    public NCMapRenderableDisplay getDisplayPane(PaneID pid) {
        if (pid.getRow() >= paneLayout.getRows()
                || pid.getColumn() >= paneLayout.getColumns()) {
            System.out.println("RbdBundle.getDisplayPane: pane id "
                    + pid.toString() + " is out of range.");
            return null;
        }

        int paneIndx = pid.getColumn() + pid.getRow() * paneLayout.getColumns();

        if (displays == null || paneIndx >= displays.length) {
            System.out.println("RbdBundle.getDisplayPane: pane id "
                    + pid.toString() + " is out of range.");
            return null;
        }

        return displays[paneIndx];
    }

    public boolean addDisplayPane(NCMapRenderableDisplay dispPane, PaneID pid) {
        if (pid.getRow() >= paneLayout.getRows()
                || pid.getColumn() >= paneLayout.getColumns()) {
            System.out.println("RbdBundle.getDisplayPane: pane id "
                    + pid.toString() + " is out of range.");
            return false;
        }
        int paneIndx = pid.getColumn() + pid.getRow() * paneLayout.getColumns();

        if (displays == null || paneIndx >= displays.length) {
            System.out.println("RbdBundle.addDisplayPane: pane id "
                    + pid.toString() + " is out of range.");
            return false;
        }

        displays[paneIndx] = dispPane;

        // sync the descriptor's auto update with the value of this RBD.
        ((NCMapDescriptor) displays[paneIndx].getDescriptor())
                .setAutoUpdate(isAutoUpdate());

        return true;
    }

    /**
     * @return the displays
     */
    public AbstractRenderableDisplay[] getDisplays() {
        return displays;
    }

    /**
     * @param displays
     *            the displays to set
     */
    public void setDisplays(NCMapRenderableDisplay[] displays) {
        this.displays = displays;
    }

    
    public static RbdBundle getDefaultRBD() throws VizException{
    	    	
    	File rbdFile = NcPathManager.getInstance().getStaticFile( 
		         NcPathConstants.DFLT_RBD );
    	
    	try {
    		RbdBundle dfltRbd = RbdBundle.unmarshalRBD( rbdFile, null );
            
            // shouldn't need this but just in case the user creates a default with
            // real resources in it
    		dfltRbd.resolveLatestCycleTimes();
    		
    		return clone( dfltRbd );
    	}
    	catch ( Exception ve ) {
    		throw new VizException( "Error getting default RBD: " + ve.getMessage());
    	}  
    }

    public static RbdBundle getRbd( File rbdFile ) throws VizException {
    	RbdBundle rbd = unmarshalRBD( rbdFile, null );
    	
    	// check for any required data that may be null or not set.
    	// This shouldn't happen except possibly from an out of date RBD. (ie older version)
    	//
    	for( NCMapRenderableDisplay d : rbd.displays ) {
    		if( d.getInitialArea() == null ) {
    			PredefinedArea dfltArea = PredefinedAreasMngr.getPredefinedArea( NmapCommon.getDefaultMap() );
    			d.setInitialArea( dfltArea );
    		}
    	}
    	
    	// set the RbdName inside the renderable display panes
    	rbd.setRbdName( rbd.getRbdName() );
    	
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
    private static RbdBundle unmarshalRBD(File fileName,
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
    private static RbdBundle unmarshalRBD(String bundleStr,
            Map<String, String> variables ) throws VizException {

        try {
            String substStr = VariableSubstitutionUtil.processVariables(
                    bundleStr, variables);

            Object xmlObj = SerializationUtil.unmarshalFromXml(substStr);
            if (!(xmlObj instanceof RbdBundle)) {
                System.out.println("Unmarshalled rbd file is not a valid RBD?");
                return null;
            }

            RbdBundle b = (RbdBundle) xmlObj;

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

        for (NCMapRenderableDisplay disp : displays) {
            if (disp != null) {
                disp.getDescriptor().setTimeMatcher(timeMatcher);

                //
                timeMatcher.addDescriptor(disp.getDescriptor());
            }
        }
    }
    
    // After and Rbd is unmarshalled it is possible for forecast resources
    // to have a cycle time of LATEST. We don't always want to resolve the 
    // Rbd after unmarshalling it so we do this as a separate step here.
    //
    public boolean resolveLatestCycleTimes() {
        // loop through all of the 
        for (NCMapRenderableDisplay disp : displays) {
            ResourceList rl = disp.getDescriptor().getResourceList();
            for (int r = 0; r < rl.size(); r++) {
                ResourcePair rp = rl.get(r);
                if (rp.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData) {
                    AbstractNatlCntrsRequestableResourceData rscData = 
                    	(AbstractNatlCntrsRequestableResourceData) rp.getResourceData();
                    ResourceName rscName = rscData.getResourceName();

                    if( rscName.isForecastResource() &&
                    	rscName.isLatestCycleTime() ) {
                    
                    	rscData.getAvailableDataTimes();
                    	
                    	// TODO : do we leave as Latest, or flag 
                    	// as NoDataAvailable? Either way the resource is going
                    	// to have to be able to handle this case.
                    	//
                    	if( rscName.isLatestCycleTime() ) {
                    		System.out.println("Unable to Resolve Latest cycle time for :"+rscName );
                    	}
                    }
                }
            }
        }

    	return true;
    }

    // if the dominantResourceName is set for the timeMatcher but the dominantResourceData isn't then
    // find the dominant resource in the list and set it.
    public void resolveDominantResource() {

        ResourceName domRscName = timeMatcher.getDominantResourceName();

        if( domRscName != null && domRscName.isValid() &&
    		timeMatcher.getDominantResource() == null) {

            // loop thru the displays looking for the dominant resource
            //
            for (NCMapRenderableDisplay disp : displays) {
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
    	// if the initial area provider name is not the same area as is stored in the 
    	// renderable displays then create the areaProvider by either getting the PredefinedArea
    	// or by looking up the area-capable resource in the list of resources.
    	
//    	for( NCMapRenderableDisplay disp : displays ) {
//    		if( )
//            ResourceList rl = disp.getDescriptor().getResourceList();
//            for (int r = 0; r < rl.size(); r++) {
//                ResourcePair rp = rl.get(r);
//                if (rp.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData) {
//                    AbstractNatlCntrsRequestableResourceData rdata = (AbstractNatlCntrsRequestableResourceData) rp
//                            .getResourceData();
//
//                }
//            }
//        }

    }

    // if the timeline has not been created then
    // get the dominant resource and initialize the timeMatcher
    public boolean initTimeline() {
    	
    	resolveDominantResource();
    	
    	return timeMatcher.loadTimes(true);
    }

	@Override
	public int compareTo(RbdBundle rbd) {
		if( rbdSequence == rbd.rbdSequence ) {
			return rbdName.compareTo( rbd.rbdName );
		}
		else {
			return rbdSequence - rbd.rbdSequence;
		}
	}
}
