package gov.noaa.nws.ncep.viz.tools.logos;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;
import gov.noaa.nws.ncep.viz.tools.logos.LogoInfo.LogoEntry;

import java.awt.Image;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class displays Logos in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 2009  	105        M. Li    	Initial creation. 
 * Nov  2009               G. Hull      migrate to to11d6
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */


public class LogosResource extends AbstractVizResource<LogosResourceData,MapDescriptor> {

    protected String name;

    protected RGB color = new RGB(255, 255, 255);

    private final double LOGO_OFFSET = 20.0;
    
    private LogoInfo logoInfo;
    
    private List<LogoEntry> logoList = null;
    
    private int[] positionArray = null;
    
    private int[] scaleArray = null;
    
    
    
    
    protected LogosResource(LogosResourceData resourceData,
    		LoadProperties loadProperties) {        
    	super(resourceData, loadProperties);
    	name = new String("Logos");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    public String getName() {
        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    public void initInternal(IGraphicsTarget target) throws VizException {
    	
    	readLogoTable();
    	if (positionArray == null) positionArray = new int[logoList.size()];
    	if (scaleArray == null) scaleArray = new int[logoList.size()];
    	
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#isApplicable(com.raytheon.viz.
     * core.PixelExtent)
     */
   // public boolean isApplicable(PixelExtent anExtent) {
   //     return true;
   // }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	
    	if (logoList != null) {
    		// Ratio to maintain same size when zoom in/out
    		double screenToWorldRatio = paintProps.getCanvasBounds().width
    									/ paintProps.getView().getExtent().getWidth();
    		double ratio =  0.5 / screenToWorldRatio ;
    		
    		IExtent extent = paintProps.getView().getExtent();
    		double llX = extent.getMinX();
    		double llY = extent.getMaxY();
            double urX = extent.getMaxX();
            double urY = extent.getMinY();
            double ulX = extent.getMinX();
            double ulY = extent.getMinY();
            double lrX = extent.getMaxX();
    		double lrY = extent.getMaxY();
    		for (int i = 0; i < logoList.size(); i++) {
    			
    			/*
    			 * comment out by M. Gao
    			 */
//    			String logofile = LocalizationManager.getInstance().getFilename("logosImageDir") + File.separator
//    				+ logoList.get(i).getImageFile();
    			String logofileName = LocalizationManager.getInstance().getLocalizationFileNameDirectly(LocalizationResourcePathConstants.LOGO_IMAGES_DIR,
    					logoList.get(i).getImageFile()); 
 
	    		IImage theImage = target.initializeRaster( 
	    				new IODataPreparer( logofileName, 1), null );
    			
    			Image image = Toolkit.getDefaultToolkit().getImage(logofileName);
    			double h = image.getHeight(null) * ratio * scaleArray[i] / 100;
    			double w = image.getWidth(null)  * ratio * scaleArray[i] / 100;
    			
    			// Lower Left
    			if (positionArray[i] == 1) {
    				
    	    		Coordinate ul = new Coordinate(llX, llY - h);
    	    		Coordinate ur = new Coordinate(llX + w, llY - h);
    	    		Coordinate lr = new Coordinate(llX + w, llY);
    	    		Coordinate ll = new Coordinate(llX, llY);
    	    		
    	    		
    	    		PixelCoverage cov = new PixelCoverage(ul, ur, lr, ll);
    	            target.drawRaster(theImage, cov, paintProps);
    	            
    	            llX += w + LOGO_OFFSET * ratio; 
    			}
    			
    			// Upper Left
    			if (positionArray[i] == 2) {
    	    		Coordinate ul = new Coordinate(ulX, ulY);
    	    		Coordinate ur = new Coordinate(ulX + w, ulY);
    	    		Coordinate lr = new Coordinate(ulX + w, ulY + h);
    	    		Coordinate ll = new Coordinate(ulX, ulY + h);
    	    		
    	    		PixelCoverage cov = new PixelCoverage(ul, ur, lr, ll);
    	            target.drawRaster(theImage, cov, paintProps);
    	            
    	            ulX = ulX + w + LOGO_OFFSET * ratio; 
    			}

    			// Upper Right
    			if (positionArray[i] == 3) {
    	    		Coordinate ul = new Coordinate(urX - w, urY);
    	    		Coordinate ur = new Coordinate(urX, urY);
    	    		Coordinate lr = new Coordinate(urX, urY + h);
    	    		Coordinate ll = new Coordinate(urX - w, urY + h);
    	    		
    	    		PixelCoverage cov = new PixelCoverage(ul, ur, lr, ll);
    	            target.drawRaster(theImage, cov, paintProps);
    	            
    	            urX = urX - w - LOGO_OFFSET * ratio; 
    			}
    			
    			// Lower Right
    			if (positionArray[i] == 4) {
    	    		Coordinate ul = new Coordinate(lrX - w, lrY - h);
    	    		Coordinate ur = new Coordinate(lrX, lrY - h);
    	    		Coordinate lr = new Coordinate(lrX, lrY);
    	    		Coordinate ll = new Coordinate(lrX - w, lrY);
    	    		
    	    		PixelCoverage cov = new PixelCoverage(ul, ur, lr, ll);
    	            target.drawRaster(theImage, cov, paintProps);
    	            
    	            lrX = lrX - w - LOGO_OFFSET * ratio; 
    			}
    			
    			
    		}
    		
    	}
      
    }   	

   

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    public void disposeInternal() {
    	//theImage.dispose();
    }

   

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.capabilities.IColorableResource#getColor()
     */
    public RGB getColor() {
        return color;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IColorableResource#setColor(org
     * .eclipse.swt.graphics.RGB)
     */
    public void setColor(RGB color) {
        this.color = color;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getShortName()
     */
    /*@Override
    public String getShortName() {
        return getName();
    }
    */
    
    private void readLogoTable() {
    	if (logoList == null) {
    		logoInfo = new LogoInfo(LocalizationManager.getInstance().getFilename("logoTable"));
    		try {
    			if (logoInfo.readTable()) {
    				logoList = new ArrayList<LogoEntry>();
    				logoList = logoInfo.getLogoList();
    				
    				for (LogoEntry logo : logoList) {
    					/*
    					 * comment out by M. Gao
    					 */
//    					String logofile = LocalizationManager.getInstance().getFilename("logosImageDir") + File.separator
//    										+ logo.getImageFile();
    					String logofileName = LocalizationManager.getInstance().getLocalizationFileNameDirectly(LocalizationResourcePathConstants.LOGO_IMAGES_DIR,
    							logo.getImageFile()); 
    					Image image = Toolkit.getDefaultToolkit().getImage(logofileName);
    					logo.setImageHeigth(image.getHeight(null));
    					logo.setImageWidth(image.getWidth(null));
    				}
    			}
    		} catch (FileNotFoundException e) {
    			e.printStackTrace();
    		} catch (IOException e) {
    			e.printStackTrace();
    		}
    	}
        
    }
    
    public void setLogoAttrs(int[] pos, int[] sc) {
    	positionArray = pos;
    	scaleArray = sc;
    }

	public int[] getPositionArray() {
		return positionArray;
	}

	public int[] getScaleArray() {
		return scaleArray;
	}

}
