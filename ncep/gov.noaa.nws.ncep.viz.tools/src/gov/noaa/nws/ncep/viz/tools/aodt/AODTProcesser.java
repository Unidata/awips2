package gov.noaa.nws.ncep.viz.tools.aodt;


import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.tools.aodt.ui.AODTDialog;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;


/**
 *  AODT processer
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/24/09		150			M. Li		initial creation
 * 10/05/09      169        Greg Hull   integrate with NCMapEditor,
 * 05/25/10                 Greg Hull   Use ICloudHeightCapable and use UnitConverter
 * 08/11/11                 Chin Chen   1.Fixed AODT can not retrieve IR temp issue
 * 	                                    2.Fixed AODT crash CAVE issue when invalid data retrieved from database
 * 										3.Changed to retrieve IR data only when user click Run AODT button
 * 02/11/13      972        G. Hull     AbstractEditor instead of NCMapEditor
 * 
 * </pre>
 * 
 * @version 1
 */
public class AODTProcesser {

	private AODTDialog aodtDlg = null;
	
	private AODTResource aodtRsc = null; 
	
	//private ICloudHeightCapable satRsc = null;
	
	//private UnitConverter tempUnitsConverter = null;
	
	private AbstractEditor mapEditor;
	
	//private final int NUMX = 105;
	
	//private float[] temps = new float[NUMX * NUMX];
	//private float[] lats  = new float[NUMX * NUMX];
	//private float[] lons  = new float[NUMX * NUMX];

	
	public AODTProcesser( AODTDialog dlg ) {
		aodtDlg = dlg;
		mapEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
        if( NcEditorUtil.getNcDisplayType( mapEditor ) == NcDisplayType.NMAP_DISPLAY ) {
        	getResources();
        }
	}
    
	public void processAODT( Coordinate latlon) { 
		if( aodtDlg != null && aodtDlg.isOpen() && latlon != null ) {
			
			//System.out.println("latlon.y="+latlon.y+"  latlon.x= "+ latlon.x);
			//if( satRsc == null ) {
			//	getResources();
			//}

			if( aodtRsc == null ) { 
				getResources();
			}

			// Update center location
			aodtDlg.setLatLon( latlon.y, latlon.x );
			
			//if (!(satRsc != null )) {
			//	aodtDlg.setIRImage(false);
			//	return;
			//}
			
			//aodtDlg.setIRImage(true);
			
			/*/ Calculate temperatures for a tileset of 105 by 105 pixels
			double[] p1 = mapEditor.getDescriptor().worldToPixel(new double[] { latlon.x, latlon.y });
			int rad = NUMX / 2;
			for (int i = -rad; i <= rad; i++ ) {
				for ( int j = -rad; j <= rad; j++) {
					double[] ll = mapEditor.getDescriptor().pixelToWorld
						(new double[] {p1[0]+i, p1[1]+j});
					int indx = (i+rad) * NUMX + (j+rad);
					lats[indx] = (float) ll[1];
					lons[indx] = (float) ll[0];//chin  * (-1.0F);
					//System.out.println(indx+" lat="+lats[indx]+", lon="+lons[indx]);
					Double tmpC = satRsc.getSatIRTemperature( 
							      new Coordinate( lons[indx], lats[indx]) );
					//chin, AODT native library has reverse sign on longitude with Sat resource used
					lons[indx] = lons[indx] * -1.0F ;
					
					if( tmpC == null || tmpC.isNaN() ) {
						aodtDlg.setIRImageRetrieved(false); 
						return;
						//Chin: do not input bad number to native AODT lib, otherwise will crash CAVE
						
					}
					else if( tempUnitsConverter != null ) {
						temps[indx] = (float)tempUnitsConverter.convert( tmpC );
					}
					else {
						temps[indx] = tmpC.floatValue();
					}
				}
			}*/
			//aodtDlg.setIRImageRetrieved(true);

			// Update current date
			//aodtDlg.setCurrentDate(
			//		  ((AbstractNatlCntrsResource<?, ?>) satRsc).getCurrentFrameTime() );
			
			// draw the marker
			aodtRsc.setSelectedLoc( latlon );

			
			//aodtDlg.setIRImageInfo(lats, lons, temps);
				
		}

		mapEditor.refresh();
	}

	// check for a satellite IR image resource 
    private void getResources() {
    	if( aodtRsc != null  ) {
    		return;
    	}
    	ResourceList rscs = NcEditorUtil.getDescriptor(mapEditor).getResourceList();
        
    	for( ResourcePair r : rscs ) {
            if( r.getResource() instanceof AODTResource ) {
            	aodtRsc = (AODTResource) r.getResource();
                //break;
            }
           
        }
    	
	         
        if( aodtRsc == null ) {
        	//aodtRsc = new CloudHeightResource("Cloud Height");
            try {
            	AODTResourceData srd = new AODTResourceData();
            	aodtRsc = srd.construct(new LoadProperties(), NcEditorUtil.getDescriptor(mapEditor));
            	NcEditorUtil.getDescriptor(mapEditor).getResourceList().add( aodtRsc );
                aodtRsc.init( mapEditor.getActiveDisplayPane().getTarget() );
            } catch (VizException e) {
                e.printStackTrace();
            }
            mapEditor.refresh();
        }
        // if satRsc is not set then we will check for it later and print msgs til 
        // it is loaded.
    }
    
    public void close( ) {
    	if( aodtRsc != null ) {
    		NcEditorUtil.getDescriptor(mapEditor).getResourceList().removeRsc( aodtRsc );
        	aodtRsc = null;
        	mapEditor.refresh();
        }
    }
}