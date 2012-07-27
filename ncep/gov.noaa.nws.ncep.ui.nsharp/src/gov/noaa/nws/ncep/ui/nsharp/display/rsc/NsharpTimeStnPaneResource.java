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
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpTimeLineStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;

import java.awt.geom.Rectangle2D;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

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
	private Rectangle colorNoteRectangle;
	private List<NsharpTimeLineStateProperty> timeLineStateList;
    private List<NsharpStationStateProperty> stnStateList;
    private int curTimeLinePage=1;
    private int curStnIdPage=1;
    private int totalTimeLinePage=1;
    private int totalStnIdPage=1;
    private int paneWidth = NsharpConstants.TIMESTN_PANE_REC_WIDTH;
    private int paneHeight = NsharpConstants.TIMESTN_PANE_REC_HEIGHT;
    private int dtXOrig = NsharpConstants.DATA_TIMELINE_X_ORIG;
    private int dtYOrig = NsharpConstants.DATA_TIMELINE_Y_ORIG;
    private int dtXEnd = NsharpConstants.DATA_TIMELINE_X_END;
    private int dtWidth = NsharpConstants.DATA_TIMELINE_WIDTH;
    private int dtHeight = NsharpConstants.DATA_TIMELINE_HEIGHT;
    private int dtNextPageEnd = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END_;
    private int charHeight = NsharpConstants.CHAR_HEIGHT_;
    private int stnXOrig = NsharpConstants.STATION_ID_X_ORIG;
    private int stnYOrig = NsharpConstants.STATION_ID_Y_ORIG;
    private int stnXEnd = NsharpConstants.STATION_ID_X_END;
    private int stnWidth = NsharpConstants.STATION_ID_WIDTH;
    private int stnHeight = NsharpConstants.STATION_ID_HEIGHT;
    private int cnXOrig = NsharpConstants.COLOR_NOTATION_X_ORIG;
    private int cnYOrig = NsharpConstants.COLOR_NOTATION_Y_ORIG;
    private int cnXEnd = NsharpConstants.COLOR_NOTATION_X_END;
    private int cnWidth = NsharpConstants.COLOR_NOTATION_WIDTH;
    private int cnHeight = NsharpConstants.COLOR_NOTATION_HEIGHT;
    private float xRatio=1;
	private float yRatio=1;
	public NsharpTimeStnPaneResource(AbstractResourceData resourceData,
			LoadProperties loadProperties, NsharpAbstractPaneDescriptor desc) {
		super(resourceData, loadProperties, desc);
		
		timeLineRectangle = new Rectangle(dtXOrig,dtYOrig,
        		dtWidth,dtHeight);
		stnIdRectangle = new Rectangle(stnXOrig,stnYOrig,
        		stnWidth,stnHeight);
		colorNoteRectangle = new Rectangle(cnXOrig,cnYOrig,
				cnWidth,cnHeight);
	}


	
	@SuppressWarnings("deprecation")
	private void drawNsharpTimelineTitle(IGraphicsTarget target) throws VizException {
		String s = timeLineStateList.size() + " time lines";
		double x = dtXOrig;
		double y = dtYOrig-25*yRatio;

		target.drawString(font10, s, x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				NsharpConstants.color_white,
				HorizontalAlignment.LEFT,
				VerticalAlignment.MIDDLE, null);
		y = y+charHeight;
		s = "page " + curTimeLinePage+"/"+totalTimeLinePage;
		target.drawString(font10, s, x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				NsharpConstants.color_green,
				HorizontalAlignment.LEFT,
				VerticalAlignment.MIDDLE, null);
	}
	@SuppressWarnings("deprecation")
	private void drawNsharpStationIdTitle(IGraphicsTarget target) throws VizException {
		//darw title first
		String s;
		s = stnStateList.size() + " stations";
		double x = stnXOrig;
		double y = stnYOrig - 25*yRatio;

		target.drawString(font10, s, x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				NsharpConstants.color_white,
				HorizontalAlignment.LEFT,
				VerticalAlignment.MIDDLE, null);
		y = y+charHeight;
		s = "page " + curStnIdPage+"/"+totalStnIdPage;
		target.drawString(font10, s, x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				NsharpConstants.color_green,
				HorizontalAlignment.LEFT,
				VerticalAlignment.MIDDLE, null);

	}
	
	@SuppressWarnings("deprecation")
	private void drawNsharpColorNotation(IGraphicsTarget target,  Rectangle rect) throws VizException {
		PixelExtent extent = new PixelExtent(rect);
		RGB color;
		target.setupClippingPlane(extent);
		target.drawRect(extent,NsharpConstants.backgroundColor, 1.0f, 1.0f);
		//plot notations:

		double x = cnXOrig+5*xRatio;
		double y = cnYOrig+charHeight;
		double xGap = paneWidth/3*xRatio;
		color = NsharpConstants.color_white;
		target.drawString(font10, "TimeLine/Station State Color Notations:", x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				color,
				HorizontalAlignment.LEFT,  
				VerticalAlignment.BOTTOM, null);

		y=y+charHeight;
		color = NsharpConstants.color_green;
		target.drawString(font10, "Current:green", x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				color,
				HorizontalAlignment.LEFT,  
				VerticalAlignment.BOTTOM, null);

		//x = x+xGap;
		y=y+charHeight;
		color = NsharpConstants.color_yellow;
		target.drawString(font10, "Active:yellow", x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				color,
				HorizontalAlignment.LEFT,  
				VerticalAlignment.BOTTOM, null);


		//x = x+xGap;
		y=y+charHeight;
		color = NsharpConstants.color_white;
		target.drawString(font10, "InActive:white", x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				color,
				HorizontalAlignment.LEFT,  
				VerticalAlignment.BOTTOM, null);

		//x = cnXOrig+5*xRatio;
		//y=y+25*yRatio;
		y=y+charHeight;
		color = NsharpConstants.color_white;
		target.drawString(font10, "TimeLine/Station Data Loading Status:", x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				color,
				HorizontalAlignment.LEFT,  
				VerticalAlignment.BOTTOM, null);

		//x = cnXOrig+5*xRatio;
		y=y+charHeight;
		color = NsharpConstants.color_red;
		target.drawString(font10, "* :Data Loaded", x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				color,
				HorizontalAlignment.LEFT,   
				VerticalAlignment.BOTTOM, null);

		y=y+charHeight;
		color = NsharpConstants.color_cyan;
		target.drawString(font10, "* :Data Not Loaded", x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				color,
				HorizontalAlignment.LEFT,  
				VerticalAlignment.BOTTOM, null);

		target.clearClippingPlane();

	}
    @SuppressWarnings("deprecation")
	private void drawNsharpTimelinBox(IGraphicsTarget target,  Rectangle rect) throws VizException {
        PixelExtent extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        target.drawRect(extent,NsharpConstants.backgroundColor, 1.0f, 1.0f);
        //System.out.println("drawNsharpDataTimelines picked stn info: "+ pickedStnInfoStr);
        double x = dtXOrig + 5;//, x1 ;
        double nextPageY = dtNextPageEnd;//DATA_TIMELINE_Y_ORIG + charHeight;
        RGB color = NsharpConstants.color_yellow;
        String s = "nextPage";
        target.drawString(font10, s, x,
        		nextPageY, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        target.drawLine(dtXOrig, nextPageY, 0.0, 
    			dtXEnd , nextPageY, 0.0, 
    			NsharpConstants.color_white,1, LineStyle.SOLID);
        
        
        int numTimeLineToShowPerPage = (cnYOrig-dtNextPageEnd)/charHeight;
    	int startIndex = (curTimeLinePage-1) * numTimeLineToShowPerPage;
        int i = 1;
       if(timeLineStateList!= null){
        	int compIndex= 1;
        	int colorIndex;
        	boolean compareTmIsOn = rscHandler.isCompareTmIsOn();
        	int currentStnStateListIndex = rscHandler.getCurrentStnStateListIndex();
    		int currentTimeLineStateListIndex = rscHandler.getCurrentTimeLineStateListIndex();
           	for (int j = startIndex; j< timeLineStateList.size(); j++)
        	{
        		boolean avail=false;
        		NsharpTimeLineStateProperty elm = timeLineStateList.get(j);
        		NsharpConstants.State sta  = elm.getTimeState();
        		double ly = dtNextPageEnd + charHeight * i;
        		if(sta == NsharpConstants.State.ACTIVE && j == currentTimeLineStateListIndex)
    				sta = NsharpConstants.State.CURRENT;
        		if(currentStnStateListIndex>=0){
        			

        			if ( rscHandler.getStnTimeTable().get(currentStnStateListIndex).get(j).elementState == NsharpConstants.State.AVAIL ){
        				avail = true;
        			}
        			if(avail){
        				color = NsharpConstants.color_red;
        				s = "*";
        			}
        			else {
        				color = NsharpConstants.color_cyan;
        				s = "*";
        			}
        			//System.out.println("selectedTimeList: "+ s);

        			target.drawString(font10, s, x,
        					ly, 0.0,
        					IGraphicsTarget.TextStyle.NORMAL,
        					color,
        					HorizontalAlignment.LEFT,  
        					VerticalAlignment.BOTTOM, null);
        		}
        		
        		color = rscHandler.getElementColorMap().get(sta);
        		s = elm.timeDescription;
        		target.drawString(font10, s, x+10,
        				ly, 0.0,
        				IGraphicsTarget.TextStyle.NORMAL,
        				color,
        				HorizontalAlignment.LEFT,  
        				VerticalAlignment.BOTTOM, null);
        		Rectangle2D bd = target.getStringBounds(font10, s);
        		i++;
        		if(compareTmIsOn && elm.timeState == NsharpConstants.State.ACTIVE && avail){
        			colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
        			s ="Cp "+ compIndex;
        			target.drawString(font10,s, x+bd.getWidth()+15,
    						ly, 0.0,
    						IGraphicsTarget.TextStyle.NORMAL,
    						linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor(),
    						HorizontalAlignment.LEFT,  
    						VerticalAlignment.BOTTOM, null);
        			compIndex++;
        		}
         		if (ly >= cnYOrig-charHeight)
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
        double x= stnXOrig + 5;
        double nextPageY = dtNextPageEnd;
        RGB color = NsharpConstants.color_yellow;
        String s = "nextPage";
        target.drawString(font10, s, x,
        		nextPageY, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        target.drawLine(stnXOrig, nextPageY, 0.0, 
    			stnXEnd , nextPageY, 0.0, 
    			NsharpConstants.color_white,1, LineStyle.SOLID);
        
        
        int numStnToShow = (cnYOrig-dtNextPageEnd)/charHeight;
    	int startIndex = (rscHandler.getCurStnIdPage()-1) * numStnToShow;
        int i = 1;
        int compIndex= 1;
    	int colorIndex;
    	boolean compareStnIsOn = rscHandler.isCompareStnIsOn();
    	int currentStnStateListIndex = rscHandler.getCurrentStnStateListIndex();
		int currentTimeLineStateListIndex = rscHandler.getCurrentTimeLineStateListIndex();
       	
        for (int j = startIndex; j< stnStateList.size(); j++)
        {
        	boolean avail=false;
         	NsharpStationStateProperty elm = stnStateList.get(j);
    		NsharpConstants.State sta ;
    		if(elm.stnState == NsharpConstants.State.ACTIVE && j == currentStnStateListIndex)
    			sta = NsharpConstants.State.CURRENT;
    		else 
    			sta = elm.stnState; // set its state based on stn state
    		
    		double ly = dtNextPageEnd + charHeight * i;
    		if (currentTimeLineStateListIndex>=0){
    			if(rscHandler.getStnTimeTable().get(j).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL ){
    				avail =true;
    			}
    			if(avail){
    				color = NsharpConstants.color_red;
    				s = "*";
    			}
    			else {
    				color = NsharpConstants.color_cyan;
    				s = "*";
    			}
    			//System.out.println("selectedTimeList: "+ s);

    			target.drawString(font10, s, x,
    					ly, 0.0,
    					IGraphicsTarget.TextStyle.NORMAL,
    					color,
    					HorizontalAlignment.LEFT,  
    					VerticalAlignment.BOTTOM, null);
    		}
    		
        	String stnId = elm.stnDescription;        	
        	
        	color = rscHandler.getElementColorMap().get(sta);
         	target.drawString(font10, stnId, x+10,
        			ly, 0.0,
        			IGraphicsTarget.TextStyle.NORMAL,
        			color,
        			HorizontalAlignment.LEFT,  
        			VerticalAlignment.BOTTOM, null);
         	if(compareStnIsOn && elm.stnState == NsharpConstants.State.ACTIVE && avail){
         		Rectangle2D bd = target.getStringBounds(font10, stnId);
         		colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
    			s ="Cp "+ compIndex;
    			target.drawString(font10,s, x+bd.getWidth()+25,
						ly, 0.0,
						IGraphicsTarget.TextStyle.NORMAL,
						linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor(),
						HorizontalAlignment.LEFT,  
						VerticalAlignment.BOTTOM, null);
    			compIndex++;
    			
         	}//else if(compareTmIsOn){
    			//anything to do? 
    		//}
         	
         	
         	i++;
        	if (ly >= cnYOrig-charHeight)
        		//we dont show stn id that extends below  box
        		break;
        }
    }
    
	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		super.paintInternal(target, paintProps);
		if(rscHandler== null)
			return;
		timeLineStateList = rscHandler.getTimeLineStateList();	//System.out.println("NsharpTimeStnPaneResource "+ descriptor.getPaneNumber());
		stnStateList = rscHandler.getStnStateList();
		curTimeLinePage = rscHandler.getCurTimeLinePage();
		curStnIdPage = rscHandler.getCurStnIdPage();
		totalTimeLinePage = rscHandler.getTotalTimeLinePage();
		totalStnIdPage = rscHandler.getTotalStnIdPage();
		this.font10.setSmoothing(false);
		this.font10.setScaleFont(false);
		this.font9.setSmoothing(false);
		this.font9.setScaleFont(false);
		this.font12.setSmoothing(false);
		this.font12.setScaleFont(false);

		//data time title
		drawNsharpTimelineTitle(target);
		drawNsharpStationIdTitle(target);


		//plot data time line
		drawNsharpTimelinBox(target, timeLineRectangle);

		//plot station id
		drawNsharpStationIdBox(target, stnIdRectangle);

		//plot color notations
		drawNsharpColorNotation(target, colorNoteRectangle );

	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		super.initInternal(target);
		currentCanvasBoundWidth = NsharpConstants.TIMESTN_PANE_REC_WIDTH;
		currentCanvasBoundHeight = NsharpConstants.TIMESTN_PANE_REC_HEIGHT;
		myDefaultCanvasWidth = NsharpConstants.TIMESTN_PANE_REC_WIDTH;
		myDefaultCanvasHeight = NsharpConstants.TIMESTN_PANE_REC_HEIGHT;	
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
	public Rectangle getColorNoteRectangle() {
		return colorNoteRectangle;
	}
	@Override
	public void handleResize() {
		
		super.handleResize();
		IExtent ext = getDescriptor().getRenderableDisplay().getExtent();
		ext.reset();
		float prevHeight = paneHeight;
		float prevWidth = paneWidth;
		paneHeight = (int) ext.getHeight();
		paneWidth = (int) (ext.getWidth());
		xRatio = xRatio* paneWidth/prevWidth;
		yRatio = yRatio* paneHeight/prevHeight;
		charHeight = (int)(NsharpConstants.CHAR_HEIGHT_*yRatio);
		dtXOrig = (int) (ext.getMinX());
	    dtYOrig = (int) ext.getMinY()+(int)( NsharpConstants.DATA_TIMELINE_Y_ORIG*yRatio);
	    dtWidth = paneWidth/2;
	    dtXEnd = dtXOrig + dtWidth;
	    cnHeight = (int)(NsharpConstants.COLOR_NOTATION_HEIGHT*yRatio);
	    dtHeight = paneHeight-dtYOrig-cnHeight;
	    dtNextPageEnd = dtYOrig+ charHeight;
	    stnXOrig = dtXEnd;
	    stnYOrig = dtYOrig;
	    stnWidth = dtWidth;
	    stnXEnd = stnXOrig+ stnWidth;
	    stnHeight = dtHeight;
	    cnXOrig = dtXOrig;
	    cnYOrig = dtYOrig+ dtHeight;	    
	    cnWidth = paneWidth;
	    cnXEnd = cnXOrig+cnWidth;
		timeLineRectangle = new Rectangle(dtXOrig,dtYOrig,
        		dtWidth,dtHeight);
		stnIdRectangle = new Rectangle(stnXOrig,stnYOrig,
        		stnWidth,stnHeight);
		colorNoteRectangle = new Rectangle(cnXOrig,cnYOrig,
				cnWidth,cnHeight);
		rscHandler.setDtNextPageEnd(dtNextPageEnd);
		rscHandler.setDtYOrig(dtYOrig);
		rscHandler.setCnYOrig(cnYOrig);
		rscHandler.setCharHeight(charHeight);
				
	}
}
