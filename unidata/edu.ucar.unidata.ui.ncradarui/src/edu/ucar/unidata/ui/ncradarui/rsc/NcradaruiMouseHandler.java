/*
 * edu.ucar.unidata.ui.ncradarui.rsc.NcradaruiMouseHandler
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/18/2012   #751       S. Gurung   Removed handleMouseDownMove(...) since this functionality
 * 									   is handled by NcPanHandler.handleMouseDownMove(...).
 * 02/15/2012     #972      G. Hull     NatlCntrsEditor 
 * 
 *                                     
 */
package edu.ucar.unidata.ui.ncradarui.rsc;

import edu.ucar.unidata.ui.ncradarui.dbutil.NcradarDbQuery;
import edu.ucar.unidata.ui.ncradarui.dbutil.NcradarStationInfo;
import edu.ucar.unidata.ui.ncradarui.palette.NcradaruiPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.viz.ui.display.NatlCntrsEditor;

import java.util.List;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

public class NcradaruiMouseHandler extends InputHandlerDefaultImpl {
	 
	private static int ASCII_CR_VAL = 13;

	private static final double NcradaruiPointMinDistance = 45000;

	private int prevMouseX, prevMouseY;

    static int textDispIndex = 0;
	
	/**
	 * Index of the selected point.
	 */
 	protected int	ptIndex = 0;

	private  NcradarDbQuery query;// = NcradarDbQuery.getAccess();

 	/**
	 * For single point element, the original location is needed for undo.
	 */
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override	   	
    public boolean handleMouseDown(int x, int y, int button) { 
        // System.out.println("mouse down");
    	prevMouseX = x;
    	prevMouseY = y;
        return false;    	
    }

    public NcradaruiMouseHandler() {
    	
    	query = NcradarDbQuery.getAccess();
    	
	}

	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     * int, int) handle left button, so user be able to shift map while it is
     * down
     */
    /*
     * @Override public boolean handleMouseDownMove(int x, int y, int button) {
     * if (button == 1 ){ NCMapEditor mapEditor =
     * NcradaruiResource.getMapEditor(); if(mapEditor!= null){ IDisplayPane[]
     * panes = ( mapEditor.arePanesGeoSynced() ? mapEditor.getDisplayPanes() :
     * mapEditor.getSelectedPanes() );
     * 
     * for( IDisplayPane p : panes ) { p.shiftExtent(new double[] { x, y }, new
     * double[] { prevMouseX, prevMouseY }); }
     * 
     * mapEditor.refresh(); }
     * 
     * prevMouseX = x; prevMouseY = y;
     * 
     * } return false;
     * 
     * }
     */
    private String removeCR(String curStr) {
		int i = ASCII_CR_VAL;
        char asciiCr = (char) i;
		String newStr = curStr.replace(asciiCr, ' ');
		return newStr;
	}
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int, int)
     *      handle right button, so user be able to pick stn and print text report
     */
    @Override
    public boolean handleMouseUp(int x, int y, int button) {
        // System.out.println("NcradaruiMouseHandler mouse up");
        if (!NcradaruiResource.getNcradaruiResource().isEditable())
            return false;
    	
    	// button 1 is left mouse button
        if (button == 1) {
    		NatlCntrsEditor mapEditor = NcradaruiResource.getMapEditor();
            if (mapEditor != null) {
    			//  Check if mouse is in geographic extent
    			Coordinate loc = mapEditor.translateClick(x, y);
                if (loc == null)
    				return false;
                NcradaruiPaletteWindow ncradaruiPaletteWindow = NcradaruiPaletteWindow
                        .getAccess();
                if (ncradaruiPaletteWindow != null) {
                    // get the stn (point) list
                    List<NcradarStationInfo> points = ncradaruiPaletteWindow
                            .getPoints();
                    if (points.isEmpty() == false) {

                        // get the stn close to loc "enough" and retrieve text
                        // report for it
                        NcradarStationInfo StnPt = getPtWithinMinDist(points,
                                loc);

                        if (StnPt != null) {
    						ncradaruiPaletteWindow.displayProduct(StnPt);
    					/*
                             * //add RED "X" marker(s) on picked stn
                             * List<NcradarStationInfo> rtnStateStnLst = new
                             * ArrayList<NcradarStationInfo> ();
                             * if(ncradaruiPaletteWindow.isState() == true) {
                             * 
                             * List<NcradarStationInfo> stateStnLst =
                             * query.getStateStationInfoList
                             * (ncradaruiPaletteWindow
                             * .getCurrentProductName()+StnPt.getState());
                             * //need to filter out those stns does not have
                             * reports in DB now, use points list for reference
                             * for (NcradarStationInfo stnInState : stateStnLst){
                             * for (NcradarStationInfo stnHasRpt : points){
                             * if(stnInState
                             * .getStnid().equals(stnHasRpt.getStnid()) ==
                             * true){ rtnStateStnLst.add(stnInState); break; } }
                             * } } else{ rtnStateStnLst.add(StnPt); } Text text
                             * = ncradaruiPaletteWindow.getText();
                             * if(ncradaruiPaletteWindow.isReplaceText() ==
                             * false){ //APPEND mode //List<NcradarStationInfo>
                             * prevPickedStnLst =
                             * ncradaruiPaletteWindow.getncradaruiResource
                             * ().getPickedStnPt(); List<NcradarStationInfo>
                             * prevPickedStnLst =
                             * NcradaruiResource.getncradaruiResource
                             * ().getPickedStnPt(); if(prevPickedStnLst.size() >
                             * 0){ if(rtnStateStnLst.addAll(prevPickedStnLst) ==
                             * false) { //System.out.println(
                             * "handleMouseUp : add picked stn failed"); return
                             * false; } } } else { //REPLACE mode
                             * text.setText(""); }
                             * //ncradaruiPaletteWindow.getncradaruiResource
                             * ().setPickedStnPt(rtnStateStnLst);
                             * NcradaruiResource
                             * .getncradaruiResource().setPickedStnPt
                             * (rtnStateStnLst); mapEditor.refresh();
                             * ncradaruiModalTool.setModal(); // QUERY DB
                             * now....Object[0] = Rawrecord text data, Object[1]
                             * = issuesite List<List<Object[]>> rptLstList =
                             * query
                             * .getProductDataListList(ncradaruiPaletteWindow
                             * .getCurrentProductName(), StnPt,
                             * ncradaruiPaletteWindow.getTimeCovered(),
                             * ncradaruiPaletteWindow.isState(),null);
                             * if(rptLstList.isEmpty()){
                             * if(ncradaruiPaletteWindow.isState())
                             * text.append("--State " +StnPt.getState()+ "--" +
                             * ncradaruiPaletteWindow.getCurrentProductName()+
                             * " Report (Station picked "
                             * +StnPt.getStnid()+")\n"); else
                             * text.append("--Text-- " +
                             * ": "+ncradaruiPaletteWindow
                             * .getCurrentProductName()
                             * +": Reporting Station: ("+StnPt.getStnid()+") "+
                             * StnPt.getStnname()+"\n");
                             * if(ncradaruiPaletteWindow
                             * .getTimeCovered().getTimeRange() == 0)
                             * text.append("Report unavailable in database.\n");
                             * else text.append("Report unavailable within "+
                             * ncradaruiPaletteWindow
                             * .getTimeCovered().getTimeRange()+
                             * " hour(s) range.\n");
                             * 
                             * } else { String textToDisp; String textRawStr;
                             * StringBuilder textStr;
                             * if(ncradaruiPaletteWindow.isState()){ //SelectBy
                             * State mode textStr = new StringBuilder("--State "
                             * +StnPt.getState()+ "--" +
                             * ncradaruiPaletteWindow.getCurrentProductName
                             * ()+" Report\n");
                             * 
                             * for(List<Object[]> lstObj : rptLstList){
                             * textStr.append("--Station " +
                             * (String)(lstObj.get(0))[1] +
                             * "-- : "+ncradaruiPaletteWindow
                             * .getCurrentProductName()+"\n"); textRawStr =
                             * (String)(lstObj.get(0))[0]; // remove CR before
                             * displaying textToDisp = removeCR(textRawStr);
                             * textStr.append(textToDisp+"\n"); }
                             * 
                             * //When put text string to Text display, use
                             * "setText" but not "append" method, so, the text
                             * will show from top
                             * if(ncradaruiPaletteWindow.isReplaceText() ==
                             * false){ // get current text string from Text
                             * StringBuilder textStr1 = new
                             * StringBuilder(text.getText());
                             * textStr1.append(textStr.toString());
                             * text.setText(textStr1.toString()); } else
                             * text.setText(textStr.toString()); } else {
                             * //SelectBy Station mode // "----" used as text
                             * header delimiter String textHeader = "--Text-- "
                             * +
                             * ": "+ncradaruiPaletteWindow.getCurrentProductName
                             * ()+": Reporting Station: ("+StnPt.getStnid()+") "
                             * +StnPt.getStnname()+ "----"+"\n";
                             * ncradaruiPaletteWindow
                             * .setCurrentTextReports(rptLstList.get(0));
                             * 
                             * int currentTextIndex = 0;
                             * ncradaruiPaletteWindow.setCurrentTextIndex
                             * (currentTextIndex); textRawStr =
                             * (String)(rptLstList
                             * .get(0).get(currentTextIndex))[0]; // remove CR
                             * before displaying textToDisp =
                             * removeCR(textRawStr); //When put text string to
                             * Text display, use "setText" but not "append"
                             * method, so, the text will show from top
                             * if(ncradaruiPaletteWindow.isReplaceText() ==
                             * false){ //Append mode: get current text string
                             * from Text StringBuilder textStr1 = new
                             * StringBuilder(text.getText());
                             * textStr1.append(textHeader+textToDisp);
                             * text.setText(textStr1.toString()); } else
                             * //Replace mode
                             * text.setText(textHeader+textToDisp);
                             * if((rptLstList.get(0).size() > 1 ) &&
                             * (ncradaruiPaletteWindow.isReplaceText() == true)){
                             * //System.out.println("list size "+
                             * rptLstList.get(0).size());
                             * ncradaruiPaletteWindow.enablePrevBtn(true); } else
                             * { ncradaruiPaletteWindow.enablePrevBtn(false); } }
                             * }
                             */
                        } else { // debug
                                 // System.out.println("Mouse point too far from stn");
    						}
                    } else { // debug
                             // System.out.println("points is null");
    				}
    			}
    		}
    	}
        return false;        
    }
    
     /**
     * Gets the nearest point of an selected element to the input point
     * 
     * @param el
     *            element
     * @param pt
     *            input point
     * @return
     */
    protected NcradarStationInfo getPtWithinMinDist(
            List<NcradarStationInfo> points, Coordinate pt) {
    	
    	NcradarStationInfo thePoint = null;
    	double	minDistance = NcradaruiPointMinDistance; 	
      	GeodeticCalculator gc;
      	NatlCntrsEditor mapEditor = NcradaruiResource.getMapEditor();
        if (mapEditor != null) {
			IMapDescriptor desc;
            desc = (IMapDescriptor) mapEditor.getActiveDisplayPane()
                    .getRenderableDisplay().getDescriptor();

			gc = new GeodeticCalculator(desc.getCRS());
			gc.setStartingGeographicPoint(pt.x, pt.y);
            // int textDispIndex = 1;//debug
            for (NcradarStationInfo point : points) {

                gc.setDestinationGeographicPoint(point.getLongitude(),
                        point.getLatitude());
				double dist;
                try {
					dist = gc.getOrthodromicDistance();
                    // System.out.println("dist to point " + textDispIndex++ +
                    // " is " + dist);
                    if (dist < minDistance) {

						minDistance = dist;
						thePoint = point; 
					}
                } catch (Exception e) {

                    // e.printStackTrace();
                    // System.out.println("getOrthodromicDistance exception happened!");
				}

			}					
		}
		return thePoint;
		
    }
    
}
