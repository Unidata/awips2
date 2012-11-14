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
    private int stnXOrig = NsharpConstants.STATION_ID_X_ORIG;
    private int stnYOrig = NsharpConstants.STATION_ID_Y_ORIG;
    private int stnXEnd = NsharpConstants.STATION_ID_X_END;
    private int stnWidth = NsharpConstants.STATION_ID_WIDTH;
    private int stnHeight = NsharpConstants.STATION_ID_HEIGHT;
    private int cnXOrig = NsharpConstants.COLOR_NOTATION_X_ORIG;
    private int cnYOrig = NsharpConstants.COLOR_NOTATION_Y_ORIG;
    //private int cnXEnd = NsharpConstants.COLOR_NOTATION_X_END;
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
	
	private void drawNsharpColorNotation(IGraphicsTarget target,  Rectangle rect) throws VizException {
		PixelExtent extent = new PixelExtent(rect);
		RGB color;
		target.setupClippingPlane(extent);
		target.drawRect(extent,NsharpConstants.backgroundColor, 1.0f, 1.0f);
		//plot notations:

		double x = cnXOrig+5*xRatio;
		double y = cnYOrig+1.5*charHeight;
		//double xGap = paneWidth/3*xRatio;
		color = NsharpConstants.color_white;
		DrawableString str =new DrawableString( "State:",color);
		str.font = font10;
		str.setCoordinates(x, y);
		double horizRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
		x = x + target.getStringsBounds(str).getWidth() * horizRatio ;
		
		color = NsharpConstants.color_green;
		DrawableString str1 =new DrawableString( "Current", color);
		str1.setCoordinates(x, y);
		x = x + target.getStringsBounds(str1).getWidth() * horizRatio *1.1;

		color = NsharpConstants.color_yellow;
		DrawableString str2 =new DrawableString( "Active", color);
		str2.setCoordinates(x, y);
		x = x + target.getStringsBounds(str2).getWidth() * horizRatio * 1.1;

		color = NsharpConstants.color_white;
		DrawableString str3 =new DrawableString( "InActive", color);
		str3.setCoordinates(x, y);
		
		x = cnXOrig+5*xRatio;
		y=y+charHeight;
		color = NsharpConstants.color_white;
		DrawableString str4 =new DrawableString( "Status:", color);
		str4.setCoordinates(x, y);
		x = x + target.getStringsBounds(str4).getWidth() * horizRatio * 1.1;
		color = NsharpConstants.color_red;
		DrawableString str5 =new DrawableString( "* :Loaded", color);
		str5.setCoordinates(x, y);
		x = x + target.getStringsBounds(str5).getWidth() * horizRatio * 1.1;
		color = NsharpConstants.color_cyan;
		DrawableString str6 =new DrawableString( "* :UnLoaded", color);
		str6.setCoordinates(x, y);
		target.drawStrings(str,str1,str2,str3,str4,str5,str6);
		target.clearClippingPlane();

	}
    @SuppressWarnings("deprecation")
    private void drawNsharpTimelinBox(IGraphicsTarget target,  Rectangle rect) throws VizException {
    	PixelExtent extent = new PixelExtent(rect);
    	target.setupClippingPlane(extent);
    	target.drawRect(extent,NsharpConstants.backgroundColor, 1.0f, 1.0f);
    	double x = dtXOrig;
    	double y = dtYOrig-1.5*charHeight*yRatio;
    	String s = timeLineStateList.size() + " time lines, page " + curTimeLinePage+"/"+totalTimeLinePage;
    	target.drawString(font10, s, x,
    			y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			NsharpConstants.color_green,
    			HorizontalAlignment.LEFT,
    			VerticalAlignment.TOP, null);
    	y = dtYOrig;
    	target.drawLine(dtXOrig, y, 0.0,dtXEnd , y, 0.0,NsharpConstants.color_white,1, LineStyle.SOLID);
    	//System.out.println("drawNsharpDataTimelines picked stn info: "+ pickedStnInfoStr);

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


    	int numTimeLineToShowPerPage = (cnYOrig-dtNextPageEnd)/charHeight;
    	int startIndex = (curTimeLinePage-1) * numTimeLineToShowPerPage;
    	
    	if(timeLineStateList!= null){
    		int compIndex= 1;
    		int colorIndex;
    		boolean compareTmIsOn = rscHandler.isCompareTmIsOn();
    		int currentStnStateListIndex = rscHandler.getCurrentStnStateListIndex();
    		int currentTimeLineStateListIndex = rscHandler.getCurrentTimeLineStateListIndex();
    		double ly = dtNextPageEnd +  charHeight;
    		RGB color;
    		double hRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
    		Rectangle2D strBD = target.getStringBounds(font10, "*");
    		double xGap = 2*strBD.getWidth()*hRatio;
    		for (int j = startIndex; j< timeLineStateList.size(); j++)
    		{
    			x = dtXOrig + 5;
    			boolean avail=false;
    			NsharpTimeLineStateProperty elm = timeLineStateList.get(j);
    			NsharpConstants.State sta  = elm.getTimeState();

    			if(sta == NsharpConstants.State.ACTIVE && j == currentTimeLineStateListIndex)
    				sta = NsharpConstants.State.CURRENT;
    			if(currentStnStateListIndex>=0){
					s = "*";
    				if ( rscHandler.getStnTimeTable().get(currentStnStateListIndex).get(j).elementState == NsharpConstants.State.AVAIL ){
    					avail = true;
    				}
    				if(avail){
    					color = NsharpConstants.color_red;
     				}
    				else {
    					color = NsharpConstants.color_cyan;
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
    			color = rscHandler.getElementColorMap().get(sta);
    			s = elm.timeDescription;
    			target.drawString(font10, s, x,
    					ly, 0.0,
    					IGraphicsTarget.TextStyle.NORMAL,
    					color,
    					HorizontalAlignment.LEFT,  
    					VerticalAlignment.BOTTOM, null);
    			
    			if(compareTmIsOn && elm.timeState == NsharpConstants.State.ACTIVE && avail){
    				colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
    				strBD = target.getStringBounds(font10, s);
    				s ="Cp "+ compIndex;
    				x=x+ strBD.getWidth()*hRatio+5;
    				target.drawString(font10,s, x,
    						ly, 0.0,
    						IGraphicsTarget.TextStyle.NORMAL,
    						linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor(),
    						HorizontalAlignment.LEFT,  
    						VerticalAlignment.BOTTOM, null);
    				compIndex++;
    			}
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
		double x = stnXOrig;
		double y = stnYOrig -1.5*charHeight*yRatio;
		String s = stnStateList.size() + " stations, page " + curStnIdPage+"/"+totalStnIdPage;
		target.drawString(font10, s, x,
				y, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				NsharpConstants.color_green,
				HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
    	y = dtYOrig;
    	target.drawLine(stnXOrig, y, 0.0,stnXEnd , y, 0.0,NsharpConstants.color_white,1, LineStyle.SOLID);
    	//System.out.println("drawNsharpDataTimelines picked stn info: "+ pickedStnInfoStr);
    	x = stnXOrig +dtWidth/2;
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
    	
    	x= stnXOrig + dtWidth/2 + 5;
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
        
        
        int numStnToShow = (cnYOrig-dtNextPageEnd)/charHeight;
    	int startIndex = (rscHandler.getCurStnIdPage()-1) * numStnToShow;
        int compIndex= 1;
    	int colorIndex;
    	boolean compareStnIsOn = rscHandler.isCompareStnIsOn();
    	int currentStnStateListIndex = rscHandler.getCurrentStnStateListIndex();
		int currentTimeLineStateListIndex = rscHandler.getCurrentTimeLineStateListIndex();
		double ly = dtNextPageEnd +  charHeight;
		RGB color;
		Rectangle2D strBD = target.getStringBounds(font10, "*");
		double hRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
		double xGap = 2*strBD.getWidth()*hRatio;
		for (int j = startIndex; j< stnStateList.size(); j++)
        {
        	boolean avail=false;
        	x = stnXOrig + 5;
         	NsharpStationStateProperty elm = stnStateList.get(j);
    		NsharpConstants.State sta ;
    		if(elm.stnState == NsharpConstants.State.ACTIVE && j == currentStnStateListIndex)
    			sta = NsharpConstants.State.CURRENT;
    		else 
    			sta = elm.stnState; // set its state based on stn state
    		
    		
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
     			target.drawString(font10, s, x,
    					ly, 0.0,
    					IGraphicsTarget.TextStyle.NORMAL,
    					color,
    					HorizontalAlignment.LEFT,  
    					VerticalAlignment.BOTTOM, null);
    		}
    		x=x+xGap;
        	String stnId = elm.stnDescription;        	
        	
        	color = rscHandler.getElementColorMap().get(sta);
         	target.drawString(font10, stnId, x,
        			ly, 0.0,
        			IGraphicsTarget.TextStyle.NORMAL,
        			color,
        			HorizontalAlignment.LEFT,  
        			VerticalAlignment.BOTTOM, null);
         	if(compareStnIsOn && elm.stnState == NsharpConstants.State.ACTIVE && avail){
         		strBD = target.getStringBounds(font10, stnId);
         		colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
    			s ="Cp "+ compIndex;
    			x=x+ strBD.getWidth()*hRatio+5;
    			target.drawString(font10,s, x,
						ly, 0.0,
						IGraphicsTarget.TextStyle.NORMAL,
						linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor(),
						HorizontalAlignment.LEFT,  
						VerticalAlignment.BOTTOM, null);
    			compIndex++;
    			
         	}//else if(compareTmIsOn){
    			//anything to do? 
    		//}
         	
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
	    //rscHandler.setCharHeight(charHeight);
		float prevHeight = paneHeight;
		float prevWidth = paneWidth;
		paneHeight = (int) ext.getHeight();
		paneWidth = (int) (ext.getWidth());
		xRatio = xRatio* paneWidth/prevWidth;
		yRatio = yRatio* paneHeight/prevHeight;
		//charHeight = (int)(NsharpConstants.CHAR_HEIGHT_*yRatio);
		dtXOrig = (int) (ext.getMinX());
	    dtYOrig = (int) ext.getMinY()+(int)(2* charHeight*yRatio);
	    dtWidth = paneWidth/2;
	    dtXEnd = dtXOrig + dtWidth;
	    cnHeight = (int)(3* charHeight*yRatio);
	    dtHeight = paneHeight-dtYOrig-cnHeight;
	    dtNextPageEnd = dtYOrig+ (int) (2*charHeight*yRatio);
	    stnXOrig = dtXEnd;
	    stnYOrig = dtYOrig;
	    stnWidth = dtWidth;
	    stnXEnd = stnXOrig+ stnWidth;
	    stnHeight = dtHeight;
	    cnXOrig = dtXOrig;
	    cnYOrig = dtYOrig+ dtHeight;	    
	    cnWidth = paneWidth;
	    //cnXEnd = cnXOrig+cnWidth;
		timeLineRectangle = new Rectangle(dtXOrig,(int) ext.getMinY(),
        		dtWidth,paneHeight-cnHeight);
		stnIdRectangle = new Rectangle(stnXOrig,(int) ext.getMinY(),
        		stnWidth,paneHeight-cnHeight);
		colorNoteRectangle = new Rectangle(cnXOrig,cnYOrig,
				cnWidth,cnHeight);
		rscHandler.setTimeStnBoxData( cnYOrig, dtNextPageEnd,  dtYOrig ,dtXOrig, dtWidth, charHeight);
		/*rscHandler.setDtNextPageEnd(dtNextPageEnd);
		rscHandler.setDtYOrig(dtYOrig);
		rscHandler.setCnYOrig(cnYOrig);
		rscHandler.setCharHeight(charHeight);*/
		
		//System.out.println("pane 4 height="+paneHeight);
	}
}
