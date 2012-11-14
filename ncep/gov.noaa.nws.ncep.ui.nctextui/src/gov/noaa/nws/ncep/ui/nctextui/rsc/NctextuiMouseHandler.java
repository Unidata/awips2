/*
 * gov.noaa.nws.ncep.ui.nctextui.rsc.NctextuiMouseHandler
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/18/2012   #751       S. Gurung   Removed handleMouseDownMove(...) since this functionality
 * 									   is handled by NcPanHandler.handleMouseDownMove(...).
 * 
 *                                     
 */
package gov.noaa.nws.ncep.ui.nctextui.rsc;

import java.util.List;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.nctextui.dbutil.NctextDbQuery;
import gov.noaa.nws.ncep.ui.nctextui.dbutil.NctextStationInfo;
import gov.noaa.nws.ncep.ui.nctextui.palette.NctextuiPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.tools.*;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

public class NctextuiMouseHandler extends InputHandlerDefaultImpl {
	 
	private static int ASCII_CR_VAL = 13;
	private static final double NctextuiPointMinDistance = 45000;
	private int prevMouseX, prevMouseY;
	static int textDispIndex =0;
	
	/**
	 * Index of the selected point.
	 */
 	protected int	ptIndex = 0;
	private  NctextDbQuery query;// = NctextDbQuery.getAccess();
 	/**
	 * For single point element, the original location is needed for undo.
	 */
	
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
     *      int, int)
     */
    @Override	   	
    public boolean handleMouseDown(int x, int y, int button) { 
    	//System.out.println("mouse down");
    	prevMouseX = x;
    	prevMouseY = y;
        return false;    	
    }

    public NctextuiMouseHandler() {
    	
    	query = NctextDbQuery.getAccess();
    	
	}

	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     *      int, int)
     *  handle left button, so user be able to shift map while it is down
     */
   /* @Override
    public boolean handleMouseDownMove(int x, int y, int button) {
    	if (button == 1 ){
    		NCMapEditor mapEditor = NctextuiResource.getMapEditor();
    		if(mapEditor!= null){
    			IDisplayPane[] panes = ( mapEditor.arePanesGeoSynced() ?
    					mapEditor.getDisplayPanes() : mapEditor.getSelectedPanes() );

    			for( IDisplayPane p : panes ) {
    				p.shiftExtent(new double[] { x, y }, new double[] {
    						prevMouseX, prevMouseY });
    			}

    			mapEditor.refresh();
    		}

    		prevMouseX = x;
    		prevMouseY = y;

    	}
        return false;
            
    }*/
    private String removeCR(String curStr){		
		int i = ASCII_CR_VAL;
		char asciiCr = (char)i;
		String newStr = curStr.replace(asciiCr, ' ');
		return newStr;
	}
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
     *      int)
     *      handle right button, so user be able to pick stn and print text report
     */
    @Override
    public boolean handleMouseUp(int x, int y, int button) {
    	//System.out.println("NctextuiMouseHandler mouse up");
    	
    	// button 1 is left mouse button
    	if (button == 1 ){
    		NCMapEditor mapEditor = NctextuiResource.getMapEditor();
    		if(mapEditor!= null){
    			//  Check if mouse is in geographic extent
    			Coordinate loc = mapEditor.translateClick(x, y);
    			if ( loc == null ) 
    				return false;
    			NctextuiPaletteWindow nctextuiPaletteWindow = NctextuiPaletteWindow.getAccess();		
    			if(nctextuiPaletteWindow!=null){
    				//get the stn (point) list
    				List<NctextStationInfo> points = nctextuiPaletteWindow.getPoints();
    				if(points.isEmpty() == false){

    					//get the stn close to loc "enough" and retrieve text report for it
    					NctextStationInfo StnPt = getPtWithinMinDist(points, loc);
    					
    					if(StnPt != null){
    						nctextuiPaletteWindow.displayProduct(StnPt);
    					/*
    						//add RED "X" marker(s) on picked stn
    						List<NctextStationInfo> rtnStateStnLst = new ArrayList<NctextStationInfo> ();
    						if(nctextuiPaletteWindow.isState() == true) {

    							List<NctextStationInfo> stateStnLst = 
    								query.getStateStationInfoList(nctextuiPaletteWindow.getCurrentProductName()+StnPt.getState());
    							//need to filter out those stns does not have reports in DB now, use points list for reference
    							for (NctextStationInfo stnInState : stateStnLst){
    								for (NctextStationInfo stnHasRpt : points){
    									if(stnInState.getStnid().equals(stnHasRpt.getStnid()) == true){
    										rtnStateStnLst.add(stnInState);
    										break;
    									}
    								}
    							}
    						}
    						else{
    							rtnStateStnLst.add(StnPt);
    						}   
    						Text text = nctextuiPaletteWindow.getText();
    						if(nctextuiPaletteWindow.isReplaceText() == false){
    							//APPEND mode
    							//List<NctextStationInfo> prevPickedStnLst = nctextuiPaletteWindow.getNctextuiResource().getPickedStnPt();
    							List<NctextStationInfo> prevPickedStnLst = NctextuiResource.getNctextuiResource().getPickedStnPt();
    							if(prevPickedStnLst.size() > 0){
    								if(rtnStateStnLst.addAll(prevPickedStnLst) == false) {
    									//System.out.println("handleMouseUp : add picked stn failed");
    									return false;
    								} 
    							}
    						}
    						else {
    							//REPLACE mode
    							text.setText("");
    						}
    						//nctextuiPaletteWindow.getNctextuiResource().setPickedStnPt(rtnStateStnLst);
    						NctextuiResource.getNctextuiResource().setPickedStnPt(rtnStateStnLst);
    						mapEditor.refresh();
    						NctextuiModalTool.setModal();
    						// QUERY DB now....Object[0] = Rawrecord text data, Object[1] = issuesite
    						List<List<Object[]>>  rptLstList = query.getProductDataListList(nctextuiPaletteWindow.getCurrentProductName(), StnPt, nctextuiPaletteWindow.getTimeCovered(), nctextuiPaletteWindow.isState(),null);
    						if(rptLstList.isEmpty()){
    							if(nctextuiPaletteWindow.isState())
    								text.append("--State " +StnPt.getState()+ "--" + nctextuiPaletteWindow.getCurrentProductName()+
    										" Report (Station picked "+StnPt.getStnid()+")\n");
    							else
    								text.append("--Text-- "  + ": "+nctextuiPaletteWindow.getCurrentProductName()+": Reporting Station: ("+StnPt.getStnid()+") "+
    										StnPt.getStnname()+"\n");
    							if(nctextuiPaletteWindow.getTimeCovered().getTimeRange() == 0)
    								text.append("Report unavailable in database.\n");
    							else			
    								text.append("Report unavailable within "+nctextuiPaletteWindow.getTimeCovered().getTimeRange()+ " hour(s) range.\n");

    						}
    						else {
    							String textToDisp;
    							String textRawStr;
    							StringBuilder textStr;
    							if(nctextuiPaletteWindow.isState()){
    								//SelectBy State mode
    								textStr  = new StringBuilder("--State " +StnPt.getState()+ "--" + nctextuiPaletteWindow.getCurrentProductName()+" Report\n");

    								for(List<Object[]> lstObj : rptLstList){
    									textStr.append("--Station " + (String)(lstObj.get(0))[1] + "-- : "+nctextuiPaletteWindow.getCurrentProductName()+"\n");
    									textRawStr = (String)(lstObj.get(0))[0];   						
    									// remove CR before displaying
    									textToDisp = removeCR(textRawStr);							
    									textStr.append(textToDisp+"\n");        							
    								}

    								//When put text string to Text display, use "setText" but not "append" method, so, the text will show from top    							
    								if(nctextuiPaletteWindow.isReplaceText() == false){
    									// get current text string from Text
    									StringBuilder textStr1 = new StringBuilder(text.getText());
    									textStr1.append(textStr.toString());
    									text.setText(textStr1.toString());
    								}
    								else
    									text.setText(textStr.toString());     						}
    							else {
    								//SelectBy Station mode
    								// "----" used as text header delimiter   
    								String textHeader = "--Text-- "  + ": "+nctextuiPaletteWindow.getCurrentProductName()+": Reporting Station: ("+StnPt.getStnid()+") "
    								+StnPt.getStnname()+ "----"+"\n";
    								nctextuiPaletteWindow.setCurrentTextReports(rptLstList.get(0));

    								int currentTextIndex = 0;
    								nctextuiPaletteWindow.setCurrentTextIndex(currentTextIndex);
    								textRawStr = (String)(rptLstList.get(0).get(currentTextIndex))[0];   						
    								// remove CR before displaying
    								textToDisp = removeCR(textRawStr);	
    								//When put text string to Text display, use "setText" but not "append" method, so, the text will show from top    							
    								if(nctextuiPaletteWindow.isReplaceText() == false){
    									//Append mode:  get current text string from Text
    									StringBuilder textStr1 = new StringBuilder(text.getText());
    									textStr1.append(textHeader+textToDisp);
    									text.setText(textStr1.toString());
    								}
    								else //Replace mode
    									text.setText(textHeader+textToDisp); 
    								if((rptLstList.get(0).size() > 1 ) && (nctextuiPaletteWindow.isReplaceText() == true)){
    									//System.out.println("list size "+ rptLstList.get(0).size());
    									nctextuiPaletteWindow.enablePrevBtn(true);
    								}
    								else {
    									nctextuiPaletteWindow.enablePrevBtn(false);
    								}
    							}
    						} */
    					}
    					else
    					{	//debug
    						//System.out.println("Mouse point too far from stn");
    					}
    				}
    				else
    				{	//debug
    					//System.out.println("points is null");
    				}
    			}
    		}
    	}
        return false;        
    }
    
     /**
     * Gets the nearest point of an selected element to the input point
     * @param el 	element
     * @param pt 	input point
     * @return
     */
    protected NctextStationInfo getPtWithinMinDist( List<NctextStationInfo> points , Coordinate pt ){
    	
    	NctextStationInfo thePoint = null;
    	double	minDistance = NctextuiPointMinDistance; 	
      	GeodeticCalculator gc;
      	NCMapEditor mapEditor = NctextuiResource.getMapEditor();
		if(mapEditor != null ){
			IMapDescriptor desc;
			desc = (IMapDescriptor) mapEditor.getActiveDisplayPane().getRenderableDisplay().getDescriptor();

			gc = new GeodeticCalculator(desc.getCRS());
			gc.setStartingGeographicPoint(pt.x, pt.y);
			//int textDispIndex = 1;//debug
			for ( NctextStationInfo point : points){

				gc.setDestinationGeographicPoint( point.getLongitude(),point.getLatitude());
				double dist;
				try{
					dist = gc.getOrthodromicDistance();
					//System.out.println("dist to point " + textDispIndex++ + " is " +  dist);
					if (  dist < minDistance ) {

						minDistance = dist;
						thePoint = point; 
					}
				}
				catch (Exception e) {

					//e.printStackTrace();
					//System.out.println("getOrthodromicDistance exception happened!");
				}


			}					
		}
		return thePoint;
		
    }
    
}

