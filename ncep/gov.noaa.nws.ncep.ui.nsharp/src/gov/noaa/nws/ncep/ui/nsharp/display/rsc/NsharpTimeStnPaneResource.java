package gov.noaa.nws.ncep.ui.nsharp.display.rsc;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpOperationElement;
import gov.noaa.nws.ncep.ui.nsharp.NsharpSoundingElementStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;

import java.awt.geom.Rectangle2D;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

public class NsharpTimeStnPaneResource extends NsharpAbstractPaneResource{
	private Rectangle timeLineRectangle;
	private Rectangle stnIdRectangle;
	private Rectangle sndRectangle;
	private Rectangle colorNoteRectangle;
	private List<NsharpOperationElement> stnElemList;
	private List<NsharpOperationElement> timeElemList;
	private List<NsharpOperationElement> sndElemList;
	private List<List<List<NsharpSoundingElementStateProperty>>> stnTimeSndTable;
	private int curTimeLineIndex=0;
    private int curStnIndex=0;
    private int curSndIndex=0;
    private int curTimeLinePage=1;
    private int curStnIdPage=1;
    private int curSndPage=1;
    private int totalTimeLinePage=1;
    private int totalStnIdPage=1;
    private int totalSndPage=1;
    private int paneWidth = NsharpConstants.TIMESTN_PANE_REC_WIDTH;
    private int paneHeight = NsharpConstants.TIMESTN_PANE_REC_HEIGHT;
    private int dtXOrig = NsharpConstants.DATA_TIMELINE_X_ORIG;
    private int dtYOrig = NsharpConstants.DATA_TIMELINE_Y_ORIG;
    private int dtXEnd = NsharpConstants.DATA_TIMELINE_X_END;
    private int dtWidth = NsharpConstants.DATA_TIMELINE_WIDTH;
    private int dtHeight = NsharpConstants.DATA_TIMELINE_HEIGHT;
    private int dtNextPageEnd = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END_;
    private int stnXOrig = NsharpConstants.STATION_ID_X_ORIG;
    private int stnYOrig = NsharpConstants.STATION_ID_Y_ORIG;
    private int stnXEnd = NsharpConstants.STATION_ID_X_END;
    private int stnWidth = NsharpConstants.STATION_ID_WIDTH;
    private int stnHeight = NsharpConstants.STATION_ID_HEIGHT;
    private int sndXOrig = NsharpConstants.SND_TYPE_X_ORIG;
    private int sndYOrig = NsharpConstants.SND_TYPE_Y_ORIG;
    private int sndXEnd = NsharpConstants.SND_TYPE_X_END;
    private int sndWidth = NsharpConstants.SND_TYPE_WIDTH;
    private int sndHeight = NsharpConstants.SND_TYPE_HEIGHT;
    private int cnXOrig = NsharpConstants.COLOR_NOTATION_X_ORIG;
    private int cnYOrig = NsharpConstants.COLOR_NOTATION_Y_ORIG;
    //private int cnXEnd = NsharpConstants.COLOR_NOTATION_X_END;
    private int cnWidth = NsharpConstants.COLOR_NOTATION_WIDTH;
    private int cnHeight = NsharpConstants.COLOR_NOTATION_HEIGHT;
    private float xRatio=1;
	private float yRatio=1;
	private static String sndTypeStr="NA";
	private static String timelineStr="NA";
	private static String stationStr="NA";
	private boolean compareStnIsOn;
	private boolean compareTmIsOn;
	private boolean compareSndIsOn;
	
	public NsharpTimeStnPaneResource(AbstractResourceData resourceData,
			LoadProperties loadProperties, NsharpAbstractPaneDescriptor desc) {
		super(resourceData, loadProperties, desc);
		
		timeLineRectangle = new Rectangle(dtXOrig,dtYOrig,
        		dtWidth,dtHeight);
		stnIdRectangle = new Rectangle(stnXOrig,stnYOrig,
        		stnWidth,stnHeight);
		sndRectangle = new Rectangle(sndXOrig,sndYOrig,
        		sndWidth,sndHeight);
		colorNoteRectangle = new Rectangle(cnXOrig,cnYOrig,
				cnWidth,cnHeight);
	}
	
	private void drawNsharpColorNotation(IGraphicsTarget target,  Rectangle rect) throws VizException {
		
		PixelExtent extent = new PixelExtent(rect);
		RGB color;
		target.setupClippingPlane(extent);
		target.drawRect(extent,NsharpConstants.backgroundColor, 1.0f, 1.0f);
		//plot notations:
		if(dtHeight >= paneHeight)
			return;
			//draw time line page title etc. only when pane box height is larger than timeline box height
		double x = cnXOrig+5*xRatio;
		double y = cnYOrig+1.5*charHeight;
		
		//double xGap = paneWidth/3*xRatio;
		color = NsharpConstants.color_white;
		DrawableString str =new DrawableString( "Line State:",color);
		str.font = font10;
		str.setCoordinates(x, y);
		double horizRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
		x = x + target.getStringsBounds(str).getWidth() * horizRatio ;
		
		color = NsharpConstants.color_green;
		DrawableString str1 =new DrawableString( "Current", color);
		str1.setCoordinates(x, y);
		str1.font = font10;
		x = x + target.getStringsBounds(str1).getWidth() * horizRatio *1.1;

		color = NsharpConstants.color_yellow;
		DrawableString str2 =new DrawableString( "Active", color);
		str2.setCoordinates(x, y);
		str2.font = font10;
		x = x + target.getStringsBounds(str2).getWidth() * horizRatio * 1.1;

		color = NsharpConstants.color_white;
		DrawableString str3 =new DrawableString( "InActive", color);
		str3.setCoordinates(x, y);
		str3.font = font10;
		
		x = cnXOrig+5*xRatio;
		y=y+charHeight*1.3;
		color = NsharpConstants.color_white;
		DrawableString str4 =new DrawableString( "Load Status:", color);
		str4.setCoordinates(x, y);
		str4.font = font10;
		x = x + target.getStringsBounds(str4).getWidth() * horizRatio * 1.1;
		color = NsharpConstants.color_red;
		DrawableString str5 =new DrawableString( "* :Loaded", color);
		str5.setCoordinates(x, y);
		str5.font = font10;
		x = x + target.getStringsBounds(str5).getWidth() * horizRatio * 1.1;
		color = NsharpConstants.color_purple;
		DrawableString str6 =new DrawableString( "* :UnLoaded", color);
		str6.setCoordinates(x, y);
		str6.font = font10;
		target.drawStrings(str,str1,str2,str3,str4,str5,str6);
		
		if(compareStnIsOn || compareSndIsOn || compareTmIsOn){
			x = cnXOrig+5*xRatio;
			y=y+charHeight*1.3;
			color = NsharpConstants.color_white;
			String baseStr="";
			String cursndType, stnId, tl; 
			if(curTimeLineIndex<0){
				tl=timelineStr;
			}
			else {
				timelineStr = tl = timeElemList.get(curTimeLineIndex).getElementDescription();
			}
			if(curSndIndex<0){
				cursndType = sndTypeStr;
			}
			else {
				sndTypeStr = cursndType = sndElemList.get(curSndIndex).getElementDescription() ;
			}
			if(curStnIndex <0){
				stnId=stationStr;
			}  
			else{
				stationStr = stnId =  stnElemList.get(curStnIndex).getElementDescription() ;
			}
			if(compareStnIsOn)
				baseStr = "Comp Stn Based on: "+ tl + "-" + cursndType;
			else if(compareSndIsOn)
				baseStr = "Comp Src Based on: "+ tl + "-" + stnId;
			else
				baseStr = "Comp Tm Based on: "+ cursndType + "-" +stnId;

			DrawableString baseDrawStr =new DrawableString(baseStr, color);
			baseDrawStr.setCoordinates(x, y);
			baseDrawStr.font = font10;
			target.drawStrings( baseDrawStr);
		}
		target.clearClippingPlane();

	}
    @SuppressWarnings("deprecation")
    private void drawNsharpTimelinBox(IGraphicsTarget target,  Rectangle rect) throws VizException {
    	PixelExtent extent = new PixelExtent(rect);
    	target.setupClippingPlane(extent);
    	target.drawRect(extent,NsharpConstants.backgroundColor, 1.0f, 1.0f);
    	double x;
		double y;
		String s;
    	if(dtHeight < paneHeight){
    		//draw time line page title etc. only when pane box height is larger than timeline box height
    		x = dtXOrig;
    		y = dtYOrig-1.5*charHeight*yRatio;
    		s = timeElemList.size() + " time lines, page " + curTimeLinePage+"/"+totalTimeLinePage;
    		target.drawString(font10, s, x,
    				y, 0.0,
    				IGraphicsTarget.TextStyle.NORMAL,
    				NsharpConstants.color_green,
    				HorizontalAlignment.LEFT,
    				VerticalAlignment.TOP, null);
    		y = dtYOrig;
    		target.drawLine(dtXOrig, y, 0.0,dtXEnd , y, 0.0,NsharpConstants.color_white,1, LineStyle.SOLID);
    		

    		x = dtXOrig +dtWidth/2;
    		// line divide nextPage and prevPage strings
    		target.drawLine(x, y, 0.0, 
    				x , y+1.2*charHeight*yRatio, 0.0, 
    				NsharpConstants.color_white,1, LineStyle.SOLID);

    		x = dtXOrig + 5;
    		y = y+1.2*charHeight*yRatio;
    		s = "nextPage";
    		target.drawString(font10, s, x,
    				y, 0.0,
    				IGraphicsTarget.TextStyle.NORMAL,
    				NsharpConstants.color_yellow,
    				HorizontalAlignment.LEFT,  
    				VerticalAlignment.BOTTOM, null);
    		x= dtXOrig + dtWidth/2 + 5;
    		s = "prevPage";
    		target.drawString(font10, s, x,
    				y, 0.0,
    				IGraphicsTarget.TextStyle.NORMAL,
    				NsharpConstants.color_yellow,
    				HorizontalAlignment.LEFT,  
    				VerticalAlignment.BOTTOM, null);

    		//line below nextPage string
    		target.drawLine(dtXOrig, y, 0.0, 
    				dtXEnd , y, 0.0, 
    				NsharpConstants.color_white,1, LineStyle.SOLID);

    	}
    	int numTimeLineToShowPerPage = (cnYOrig-dtNextPageEnd)/charHeight;
    	if(numTimeLineToShowPerPage <1){
    		numTimeLineToShowPerPage = dtHeight/charHeight;
    		if(numTimeLineToShowPerPage <1)
    			numTimeLineToShowPerPage=1;
    	}
    	int startIndex = (curTimeLinePage-1) * numTimeLineToShowPerPage;
    	if(startIndex<0)
    		startIndex =0;
    	if(timeElemList!= null){
    		int colorIndex;
    		double ly = dtNextPageEnd +  charHeight;
    		RGB color;
    		double hRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
    		Rectangle2D strBD = target.getStringBounds(font10, "*");
    		double xGap = 2*strBD.getWidth()*hRatio;
    		for (int j = startIndex; j< timeElemList.size(); j++)
    		{
    			x = dtXOrig + 5;
    			boolean avail=false;
    			NsharpOperationElement elm = timeElemList.get(j);
    			NsharpConstants.ActState sta  = elm.getActionState();

    			if(sta == NsharpConstants.ActState.ACTIVE && j == curTimeLineIndex)
    				sta = NsharpConstants.ActState.CURRENT;
    			if(curStnIndex>=0 && curSndIndex >=0) {
					s = "*";
    				if ( stnTimeSndTable.get(curStnIndex).get(j).get(curSndIndex)!=null){
    					avail = true;
    				}
    				if(avail){
    					color = NsharpConstants.color_red;
    					if(sta == NsharpConstants.ActState.CURRENT)
        					color = NsharpConstants.color_green;
     				}
    				else {
    					color = NsharpConstants.color_purple;
    				}
    				//System.out.println("selectedTimeList: "+ s);

    				target.drawString(font10, s, x,
    						ly, 0.0,
    						IGraphicsTarget.TextStyle.NORMAL,
    						color,
    						HorizontalAlignment.LEFT,  
    						VerticalAlignment.BOTTOM, null);
    			}
    			
        		x=x+xGap;
    			RGB tmLnColor = rscHandler.getElementColorMap().get(sta.name());
    			String tmDesStr = elm.getElementDescription();
    			double tmX=x;
    			
    			if(compareTmIsOn  && elm.getActionState() == NsharpConstants.ActState.ACTIVE 
    			    && avail){
    				colorIndex = stnTimeSndTable.get(curStnIndex).get(j).get(curSndIndex).getCompColorIndex();
    				strBD = target.getStringBounds(font10, tmDesStr);
    				String colorIndexStr =""+ (colorIndex % NsharpConstants.LINE_COMP1 + 1);//compIndex;
    				x=x+ strBD.getWidth()*hRatio+5;
    				target.drawString(font10,colorIndexStr, x,
    						ly, 0.0,
    						IGraphicsTarget.TextStyle.NORMAL,
    						linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor(),
    						HorizontalAlignment.LEFT,  
    						VerticalAlignment.BOTTOM, null);
    				tmLnColor = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
    			} 
    			else if((compareStnIsOn||compareSndIsOn) && elm.getActionState() == NsharpConstants.ActState.ACTIVE && timeElemList.indexOf(elm) == curTimeLineIndex
        			    && avail){
        				colorIndex = stnTimeSndTable.get(curStnIndex).get(curTimeLineIndex).get(curSndIndex).getCompColorIndex();
        				strBD = target.getStringBounds(font10, tmDesStr);
        				String colorIndexStr =""+ (colorIndex % NsharpConstants.LINE_COMP1 + 1);//compIndex;
        				x=x+ strBD.getWidth()*hRatio+5;
        				target.drawString(font10,colorIndexStr, x,
        						ly, 0.0,
        						IGraphicsTarget.TextStyle.NORMAL,
        						linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor(),
        						HorizontalAlignment.LEFT,  
        						VerticalAlignment.BOTTOM, null);
        				tmLnColor = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
        		}
    			target.drawString(font10, tmDesStr, tmX,
    					ly, 0.0,
    					IGraphicsTarget.TextStyle.NORMAL,
    					tmLnColor,
    					HorizontalAlignment.LEFT,  
    					VerticalAlignment.BOTTOM, null);
    			ly = ly + charHeight;
    			if (ly >= cnYOrig)//-charHeight)
    				//we dont show time line that extends below time line box
    				break;
    		}
    	}
    }
    @SuppressWarnings("deprecation")
	private void drawNsharpStationIdBox(IGraphicsTarget target, Rectangle rect) throws VizException {        
        PixelExtent extent = new PixelExtent(rect);
    	target.setupClippingPlane(extent);
    	target.drawRect(extent,NsharpConstants.backgroundColor, 1.0f, 1.0f);
    	double x;
		double y;
		String s;
		if(dtHeight < paneHeight){
			//draw time line page title etc. only when pane box height is larger than timeline box height
			x = stnXOrig;
			y = stnYOrig -1.5*charHeight*yRatio;
			s = stnElemList.size() + " stations, page " + curStnIdPage+"/"+totalStnIdPage;
			target.drawString(font10, s, x,
					y, 0.0,
					IGraphicsTarget.TextStyle.NORMAL,
					NsharpConstants.color_green,
					HorizontalAlignment.LEFT,
					VerticalAlignment.TOP, null);
			y = dtYOrig;
			target.drawLine(stnXOrig, y, 0.0,stnXEnd , y, 0.0,NsharpConstants.color_white,1, LineStyle.SOLID);
			//System.out.println("drawNsharpDataTimelines picked stn info: "+ pickedStnInfoStr);
			x = stnXOrig +stnWidth/2;
			// line divide nextPage and prevPage strings
			target.drawLine(x, y, 0.0, 
					x , y+1.2*charHeight*yRatio, 0.0, 
					NsharpConstants.color_white,1, LineStyle.SOLID);

			x = stnXOrig + 5;
			y = y+1.2*charHeight*yRatio;
			s = "nextPage";
			target.drawString(font10, s, x,
					y, 0.0,
					IGraphicsTarget.TextStyle.NORMAL,
					NsharpConstants.color_yellow,
					HorizontalAlignment.LEFT,  
					VerticalAlignment.BOTTOM, null);

			x= stnXOrig + stnWidth/2 + 5;
			s = "prevPage";
			target.drawString(font10, s, x,
					y, 0.0,
					IGraphicsTarget.TextStyle.NORMAL,
					NsharpConstants.color_yellow,
					HorizontalAlignment.LEFT,  
					VerticalAlignment.BOTTOM, null);

			//line below neextPage string
			target.drawLine(stnXOrig, y, 0.0, 
					stnXEnd , y, 0.0, 
					NsharpConstants.color_white,1, LineStyle.SOLID);
		}

        int numStnToShow = (cnYOrig-dtNextPageEnd)/charHeight;
    	if(numStnToShow <1){
    		numStnToShow = dtHeight/charHeight;
    		if(numStnToShow <1)
    			numStnToShow=1;
    	}
    	int startIndex = (rscHandler.getCurStnIdPage()-1) * numStnToShow;
    	if(startIndex<0)
    		startIndex =0;
        int colorIndex;
    	double ly = dtNextPageEnd +  charHeight;
		RGB color;
		Rectangle2D strBD = target.getStringBounds(font10, "*");
		double hRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
		double xGap = 2*strBD.getWidth()*hRatio;
		for (int j = startIndex; j< stnElemList.size(); j++)
        {
        	boolean avail=false;
        	x = stnXOrig + 5;
        	NsharpOperationElement elm = stnElemList.get(j);
    		NsharpConstants.ActState sta = elm.getActionState();
    		if(sta == NsharpConstants.ActState.ACTIVE && j == curStnIndex)
    			sta = NsharpConstants.ActState.CURRENT;
    		
    		
    		if (curTimeLineIndex>=0 && curSndIndex >=0){
    			if(stnTimeSndTable.get(j).get(curTimeLineIndex).get(curSndIndex)!=null ){
    				avail =true;
    			}
    			s = "*";
    			if(avail){
    				color = NsharpConstants.color_red;
    				if(sta == NsharpConstants.ActState.CURRENT)
    					color = NsharpConstants.color_green;
    			}
    			else {
    				color = NsharpConstants.color_purple;
    			}
     			target.drawString(font10, s, x,
    					ly, 0.0,
    					IGraphicsTarget.TextStyle.NORMAL,
    					color,
    					HorizontalAlignment.LEFT,  
    					VerticalAlignment.BOTTOM, null);
    		}
    		x=x+xGap;
        	String stnId = elm.elementDescription;        	
        	double stnIdX = x;
        	color = rscHandler.getElementColorMap().get(sta.name());
         	
         	if( elm.getActionState() == NsharpConstants.ActState.ACTIVE && avail){
         		if(compareStnIsOn){
         			strBD = target.getStringBounds(font10, stnId);
         			//colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
         			colorIndex = stnTimeSndTable.get(j).get(curTimeLineIndex).get(curSndIndex).getCompColorIndex();
         			color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
         			s =""+ (colorIndex % NsharpConstants.LINE_COMP1 + 1);
         			x=x+ strBD.getWidth()*hRatio+5;
         			target.drawString(font10,s, x,
         					ly, 0.0,
         					IGraphicsTarget.TextStyle.NORMAL,
         					color,
         					HorizontalAlignment.LEFT,  
         					VerticalAlignment.BOTTOM, null);
         		}
         		else if((compareTmIsOn||compareSndIsOn) && j == curStnIndex){
         			strBD = target.getStringBounds(font10, stnId);
         			//colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
         			colorIndex = stnTimeSndTable.get(curStnIndex).get(curTimeLineIndex).get(curSndIndex).getCompColorIndex();
         			color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
         			s =""+ (colorIndex % NsharpConstants.LINE_COMP1 + 1);
         			x=x+ strBD.getWidth()*hRatio+5;
         			target.drawString(font10,s, x,
         					ly, 0.0,
         					IGraphicsTarget.TextStyle.NORMAL,
         					color,
         					HorizontalAlignment.LEFT,  
         					VerticalAlignment.BOTTOM, null);
         		}
         	}
         	target.drawString(font10, stnId, stnIdX,
        			ly, 0.0,
        			IGraphicsTarget.TextStyle.NORMAL,
        			color,
        			HorizontalAlignment.LEFT,  
        			VerticalAlignment.BOTTOM, null);
         	ly = ly + charHeight;
        	if (ly >= cnYOrig)
        		//we dont show stn id that extends below  box
        		break;
        }
    }
    
    @SuppressWarnings("deprecation")
	private void drawNsharpSndTypeBox(IGraphicsTarget target, Rectangle rect) throws VizException {        
        PixelExtent extent = new PixelExtent(rect);
    	target.setupClippingPlane(extent);
    	target.drawRect(extent,NsharpConstants.backgroundColor, 1.0f, 1.0f);
    	double x;
		double y;
		String s;
		if(dtHeight < paneHeight){
			//draw time line page title etc. only when pane box height is larger than timeline box height
			x = sndXOrig;
			y = sndYOrig -1.5*charHeight*yRatio;
			s = sndElemList.size() + " srcs, page " + curSndPage+"/"+totalSndPage;
			target.drawString(font10, s, x,
					y, 0.0,
					IGraphicsTarget.TextStyle.NORMAL,
					NsharpConstants.color_green,
					HorizontalAlignment.LEFT,
					VerticalAlignment.TOP, null);
			y = sndYOrig;  
			target.drawLine(sndXOrig, y, 0.0,sndXEnd , y, 0.0,NsharpConstants.color_white,1, LineStyle.SOLID);
			//System.out.println("drawNsharpDataTimelines picked stn info: "+ pickedStnInfoStr);
			x = sndXOrig +sndWidth/2; 
			// line divide nextPage and prevPage strings
			target.drawLine(x, y, 0.0, 
					x , y+1.2*charHeight*yRatio, 0.0, 
					NsharpConstants.color_white,1, LineStyle.SOLID);

			x = sndXOrig + 5;
			y = y+1.2*charHeight*yRatio;
			s = "nextPage";
			target.drawString(font10, s, x,
					y, 0.0,
					IGraphicsTarget.TextStyle.NORMAL,
					NsharpConstants.color_yellow,
					HorizontalAlignment.LEFT,  
					VerticalAlignment.BOTTOM, null);

			x= sndXOrig + sndWidth/2 + 5;
			s = "prevPage";
			target.drawString(font10, s, x,
					y, 0.0,
					IGraphicsTarget.TextStyle.NORMAL,
					NsharpConstants.color_yellow,
					HorizontalAlignment.LEFT,  
					VerticalAlignment.BOTTOM, null);

			//line below neextPage string
			target.drawLine(sndXOrig, y, 0.0, 
					sndXEnd , y, 0.0, 
					NsharpConstants.color_white,1, LineStyle.SOLID);
		}

        int numStnToShow = (cnYOrig-dtNextPageEnd)/charHeight;
    	if(numStnToShow <1){
    		numStnToShow = dtHeight/charHeight;
    		if(numStnToShow <1)
    			numStnToShow=1;
    	}
    	int startIndex = (rscHandler.getCurStnIdPage()-1) * numStnToShow;
    	if(startIndex<0)
    		startIndex =0;
        int colorIndex;
    	double ly = dtNextPageEnd +  charHeight;
		RGB color;
		Rectangle2D strBD = target.getStringBounds(font10, "*");
		double hRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
		double xGap = 2*strBD.getWidth()*hRatio;
		for (int j = startIndex; j< sndElemList.size(); j++)
        {
        	boolean avail=false;
        	x = sndXOrig + 5;
        	NsharpOperationElement elm = sndElemList.get(j);
    		NsharpConstants.ActState sta = elm.getActionState();
    		if(sta == NsharpConstants.ActState.ACTIVE && j == curSndIndex)
    			sta = NsharpConstants.ActState.CURRENT;
    		
    		
    		if (curTimeLineIndex>=0 && curStnIndex >=0){
    			if(stnTimeSndTable.get(curStnIndex).get(curTimeLineIndex).get(j)!=null ){
    				avail =true;
    			}
    			s = "*";
    			if(avail){
    				color = NsharpConstants.color_red;
    				if(sta == NsharpConstants.ActState.CURRENT)
    					color = NsharpConstants.color_green;
    			}
    			else {
    				color = NsharpConstants.color_purple;
    			}
     			target.drawString(font10, s, x,
    					ly, 0.0,
    					IGraphicsTarget.TextStyle.NORMAL,
    					color,
    					HorizontalAlignment.LEFT,  
    					VerticalAlignment.BOTTOM, null);
    		}
    		x=x+xGap;
        	String sndType = elm.elementDescription;        	
        	double sndX = x;
        	color = rscHandler.getElementColorMap().get(sta.name());
         	
         	if( elm.getActionState() == NsharpConstants.ActState.ACTIVE && avail){
         		if(compareSndIsOn){
         			strBD = target.getStringBounds(font10, sndType);
         			//colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
         			colorIndex = stnTimeSndTable.get(curStnIndex).get(curTimeLineIndex).get(j).getCompColorIndex();
         			color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
         			s =""+ (colorIndex % NsharpConstants.LINE_COMP1 + 1);
         			x=x+ strBD.getWidth()*hRatio+5;
         			target.drawString(font10,s, x,
         					ly, 0.0,
         					IGraphicsTarget.TextStyle.NORMAL,
         					color,
         					HorizontalAlignment.LEFT,  
         					VerticalAlignment.BOTTOM, null);
         		}
         		else if((compareTmIsOn||compareStnIsOn) && j == curSndIndex){
         			strBD = target.getStringBounds(font10, sndType);
         			//colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
         			colorIndex = stnTimeSndTable.get(curStnIndex).get(curTimeLineIndex).get(curSndIndex).getCompColorIndex();
         			color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
         			s =""+ (colorIndex % NsharpConstants.LINE_COMP1 + 1);
         			x=x+ strBD.getWidth()*hRatio+5;
         			target.drawString(font10,s, x,
         					ly, 0.0,
         					IGraphicsTarget.TextStyle.NORMAL,
         					color,
         					HorizontalAlignment.LEFT,  
         					VerticalAlignment.BOTTOM, null);
         		}
         	}
         	target.drawString(font10, sndType, sndX,
        			ly, 0.0,
        			IGraphicsTarget.TextStyle.NORMAL,
        			color,
        			HorizontalAlignment.LEFT,  
        			VerticalAlignment.BOTTOM, null);
         	ly = ly + charHeight;
        	if (ly >= cnYOrig)
        		//we dont show stn id that extends below  box
        		break;
        }
    }
	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		super.paintInternal(target, paintProps);
		//defineCharHeight(font10);
		//System.out.println("timeStn paintInternal zoomL="+currentZoomLevel);
		if(rscHandler== null)
			return;
		stnElemList = rscHandler.getStnElementList();
		timeElemList = rscHandler.getTimeElementList();
		sndElemList = rscHandler.getSndElementList();
		curTimeLineIndex = rscHandler.getCurrentTimeElementListIndex();
		curStnIndex = rscHandler.getCurrentStnElementListIndex();
		curSndIndex = rscHandler.getCurrentSndElementListIndex();
		curTimeLinePage = rscHandler.getCurTimeLinePage();
		curStnIdPage = rscHandler.getCurStnIdPage();
		curSndPage = rscHandler.getCurSndPage();
		totalTimeLinePage = rscHandler.getTotalTimeLinePage();
		totalStnIdPage = rscHandler.getTotalStnIdPage();
		totalSndPage = rscHandler.getTotalSndPage();
		compareStnIsOn = rscHandler.isCompareStnIsOn();
		compareSndIsOn = rscHandler.isCompareSndIsOn();
    	compareTmIsOn = rscHandler.isCompareTmIsOn();
    	stnTimeSndTable = rscHandler.getStnTimeSndTable();
		this.font10.setSmoothing(false);
		this.font10.setScaleFont(false);
		this.font9.setSmoothing(false);
		this.font9.setScaleFont(false);
		this.font12.setSmoothing(false);
		this.font12.setScaleFont(false);

		//plot data time line
		drawNsharpTimelinBox(target, timeLineRectangle);

		//plot station id
		drawNsharpStationIdBox(target, stnIdRectangle);
		
		drawNsharpSndTypeBox(target, sndRectangle);
		//plot color notations
		drawNsharpColorNotation(target, colorNoteRectangle );

	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		super.initInternal(target);
		/*if(paneConfigurationNumber == NsharpConstants.PANE_CONFIGURATION_0){
			currentCanvasBoundWidth = (int)(NsharpConstants.DISPLAY_WIDTH * (1-NsharpConstants.PC0_LEFT_GP_WIDTH_RATIO)* NsharpConstants.PC0_TIMESTN_WIDTH_RATIO);
			currentCanvasBoundHeight = (int)(NsharpConstants.DISPLAY_HEIGHT* NsharpConstants.PC0_TIMESTN_HEIGHT_RATIO);
			myDefaultCanvasWidth = currentCanvasBoundWidth;
			myDefaultCanvasHeight = currentCanvasBoundHeight;		
		} else if(paneConfigurationNumber == NsharpConstants.PANE_CONFIGURATION_1){
			currentCanvasBoundWidth = (int)(NsharpConstants.DISPLAY_WIDTH * NsharpConstants.PC1_LEFT_GP_WIDTH_RATIO* NsharpConstants.PC1_TIMESTN_WIDTH_RATIO);
			currentCanvasBoundHeight = (int)(NsharpConstants.DISPLAY_HEIGHT* NsharpConstants.PC1_TIMESTN_HEIGHT_RATIO);
			myDefaultCanvasWidth = currentCanvasBoundWidth;
			myDefaultCanvasHeight = currentCanvasBoundHeight;	
		}*/
	}
	@Override
	protected void disposeInternal() {
		
		super.disposeInternal();
	}
	public Rectangle getTimeLineRectangle() {
		return timeLineRectangle;
	}
	public Rectangle getStnIdRectangle() {
		return stnIdRectangle;
	}
	
	public Rectangle getSndRectangle() {
		return sndRectangle;
	}

	public Rectangle getColorNoteRectangle() {
		return colorNoteRectangle;
	}
	@Override
	public void handleResize() {
		
		super.handleResize();
		IExtent ext = getDescriptor().getRenderableDisplay().getExtent();
		ext.reset();
		this.rectangle = new Rectangle((int)ext.getMinX(), (int) ext.getMinY(),
				(int) ext.getWidth(), (int) ext.getHeight());
		pe = new PixelExtent(this.rectangle);
		getDescriptor().setNewPe(pe);
	    defineCharHeight(font10);
	    paneHeight = (int) ext.getHeight();
		paneWidth = (int) (ext.getWidth());
		//xRatio = xRatio* paneWidth/prevWidth;
		//DEBUGGING yRatio = yRatio* paneHeight/prevHeight;
		xRatio = 1;
		yRatio = 1;
		//if pane height is less than 10 char height, then just plot time line. not plot "messages/title/notations" etc.. 
		if(paneHeight > (int)(10* charHeight*yRatio)){
			dtYOrig = (int) ext.getMinY()+(int)(2* charHeight*yRatio);
			cnHeight = (int)(4* charHeight*yRatio);
			dtHeight = paneHeight-(int)(6* charHeight*yRatio);
			dtNextPageEnd = dtYOrig+ (int) (2*charHeight*yRatio);
		}
		else {
			dtYOrig = (int) (ext.getMinY());
		    cnHeight = 0;
		    dtHeight = paneHeight;
		    dtNextPageEnd = dtYOrig;
		}
		dtXOrig = (int) (ext.getMinX());
	    dtWidth = paneWidth* 40/100;
	    dtXEnd = dtXOrig + dtWidth;
	    stnXOrig = dtXEnd;
	    stnYOrig = dtYOrig;
	    stnWidth = paneWidth * 30/100;
	    stnXEnd = stnXOrig+ stnWidth;
	    sndXOrig = stnXEnd;
	    sndYOrig = stnYOrig;
	    sndWidth = paneWidth * 30/100;
	    sndXEnd = sndXOrig+ sndWidth;
	    stnHeight = dtHeight;
	    cnXOrig = dtXOrig;
	    cnYOrig = dtYOrig+ dtHeight;	    
	    cnWidth = paneWidth;
	    //cnXEnd = cnXOrig+cnWidth;
		timeLineRectangle = new Rectangle(dtXOrig,(int) ext.getMinY(),
        		dtWidth,paneHeight-cnHeight);
		stnIdRectangle = new Rectangle(stnXOrig,(int) ext.getMinY(),
        		stnWidth,paneHeight-cnHeight);
		sndRectangle = new Rectangle(sndXOrig,(int) ext.getMinY(),
        		sndWidth,paneHeight-cnHeight);
		colorNoteRectangle = new Rectangle(cnXOrig,cnYOrig,
				cnWidth,cnHeight);
		rscHandler.setTimeStnBoxData( cnYOrig, dtNextPageEnd,  dtYOrig ,dtXOrig, dtWidth, charHeight);
	}
}
