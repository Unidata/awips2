/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.hydro.gagedisplay;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydrocommon.data.GageData;

public class HydroImageMaker {
      
    private static boolean isFcstPt = false;
    private static boolean isReservoir = false;
    private static boolean isRiver = false;
    private static boolean isNonRiver = false;
    private static boolean isUnknown = false;
    
    private static BufferedImage damImage = null;
    
    public static enum ImageSize {
        VERY_SMALL(11, 20),
        SMALL(13, 25),
        MEDIUM(15, 30),
        LARGE(17, 35);

        private final int width;
        private final int height;
        
        ImageSize(int width, int height) {
            this.width = width;
            this.height = height;
        }
        
        public int getWidth() {
            return width;
        }
        
        public int getHeight() {
            return height;
        }
    }
    
    private static ImageSize imageSize = null;
    
    /**
     * 
     * @param gage
     *      The GageData object
     * @param size
     *      The size of the image
     * @return
     */
    public static BufferedImage getImage(GageData gage, ImageSize size) {
        return getImage(gage, size, null);
    }
    
    /**
     * The basic class you call when you need a HydroImage
     * @param gage
     *      The GageData object
     * @param size
     *      The size of the image
     * @param rgb
     *      An override to the GageData color
     * @return
     */
    public static BufferedImage getImage(GageData gage, ImageSize size, RGB rgb) {  
        imageSize = size;
        BufferedImage img = null;

        if (gage.getDispClass() != null) {
            if (gage.getDispClass().contains(GageData.OFFICIAL_RIVER)) {  // F
                isFcstPt = true;
            } else {
                isFcstPt = false;
            }

            if (gage.getDispClass().contains(GageData.RESERVOIR)) { // D
                isReservoir = true;
            } else {
                isReservoir = false;
            }

            if (gage.getDispClass().contains(GageData.RIVER) ||  // R
                    isFcstPt || isReservoir ) {
                isRiver = true;
            } else {
                isRiver = false;
            }

            if (gage.getDispClass().contains(GageData.PRECIP) ||
                    gage.getDispClass().contains(GageData.SNOW) ||
                    gage.getDispClass().contains(GageData.TEMPERATURE) ||
                    gage.getDispClass().contains(GageData.OTHER)) {
                isNonRiver = true;
            } else {
                isNonRiver = false;
            }
            
            if (gage.getDispClass().contains("U")) {
                isUnknown = true;
            } else {
                isUnknown = false;
            }
        }

        img = new BufferedImage(imageSize.getWidth(), imageSize.getHeight(), BufferedImage.TYPE_INT_ARGB);  
        
        if ( isRiver ) {
            if (rgb == null) {
                img = createRiverDataPointImage(img, gage.getColor());
            } else {
                img = createRiverDataPointImage(img, rgb);
            }
        }

        if (isFcstPt) {
            if (rgb == null) {
                img = drawForecastCircle(img, convert(gage.getColor()));
            } else {
                img = drawForecastCircle(img, convert(rgb));
            }
        }

        if (isReservoir){
            if (rgb == null) {
                img = drawReservoirLine(img, convert(gage.getColor()));
            } else {
                img = drawReservoirLine(img, convert(rgb));
            }
        }

        if ( isNonRiver ) {
            img = createMeteorologicalStationPointImage(img);
        }
        
        if (isUnknown) {
            if (rgb == null) {
                img = createUnknown(img, convert(rgb));
            } else {
                img = createUnknown(img, convert(rgb));
            }
        }
        
        return img;
    }
    
    
    
    /**
     * creates the river basic data point
     * @param color
     * @return
     */
    private static BufferedImage createRiverDataPointImage(BufferedImage img, RGB color) {  
        Graphics2D g = (Graphics2D)img.getGraphics();  
        // this makes it transparent
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f));
        Rectangle2D.Double rect = new Rectangle2D.Double(0,0,img.getWidth(),img.getHeight()); 
        g.fill(rect);
        
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(convert(color)); 
        // make a triangle of chosen color
        Polygon poly = new Polygon();
        poly.addPoint(1, imageSize.getHeight()-imageSize.getHeight()/5);
        poly.addPoint(imageSize.getWidth()/2, imageSize.getHeight()/4);
        poly.addPoint(imageSize.getWidth()-1, imageSize.getHeight()-imageSize.getHeight()/5);
        poly.addPoint(1, imageSize.getHeight()-imageSize.getHeight()/5);
       
        g.fillPolygon(poly);
    
        return img;  
    } 
    
    /**
     * Creates the meteorological station point
     * @param color
     * @return
     */
    private static BufferedImage createMeteorologicalStationPointImage(BufferedImage img) {  
        // make circle in center
        Graphics2D g = (Graphics2D)img.getGraphics(); 
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        // always red
        g.setColor(new Color(255, 0, 0));  
        g.fillOval(imageSize.getWidth()/2-(imageSize.getHeight()/6/2), imageSize.getHeight()/2, imageSize.getHeight()/6, imageSize.getHeight()/6);

        return img;
    } 
    
    /**
     * convert color
     * @param color
     * @return
     */
    private static Color convert(RGB color) {
        int blue;
        int green;
        int red;
        Color returnColor;
        if (color != null) {
            blue = color.blue;
            green = color.green;
            red = color.red;
            returnColor = new Color(red, green, blue);
        } else {
            blue = PDCConstants.DEFAULT_COLOR.blue;
            green = PDCConstants.DEFAULT_COLOR.green;
            red = PDCConstants.DEFAULT_COLOR.red;
            returnColor = new Color(red, green, blue);
        }

        return returnColor;
    }
    
    /**
     * Draw the forecast hat
     * @param image
     * @return
     */
    private static BufferedImage drawForecastCircle(BufferedImage image, Color color) {
       // add the circle to the top
       Graphics2D g = (Graphics2D)image.getGraphics(); 
       g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
       g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
       g.setColor(color);
       g.setStroke(new BasicStroke(2));
       g.drawOval(imageSize.getWidth()/2-(imageSize.getHeight()/6/2), imageSize.getHeight()/7, imageSize.getHeight()/6, imageSize.getHeight()/6);
       
       return image;
    }
    
    /**
     * draw the line at the bottom of reservoir icons.
     * @param image
     * @return
     */
    private static BufferedImage drawReservoirLine(BufferedImage image, Color color) {
       Graphics2D g = (Graphics2D)image.getGraphics();  
       g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
       g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
       g.setColor(color);
       g.setStroke(new BasicStroke(2));
       g.drawLine(1, imageSize.getHeight()-imageSize.getHeight()/7, imageSize.getWidth(), imageSize.getHeight()-imageSize.getHeight()/7);
       
       return image;
    }
    
    private static BufferedImage createUnknown(BufferedImage image, Color color) {
        Graphics2D g = (Graphics2D)image.getGraphics();  
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(color);
        g.setStroke(new BasicStroke(2));
        g.fillOval(0, imageSize.getHeight()/4, imageSize.getHeight()/2, imageSize.getHeight()/2);
       
       return image;
    }
    
    /**
     * Create the Dam icons.
     * 
     * @return
     */
    public static BufferedImage getDamIcon() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String iconColor = appsDefaults.getToken("dam_icon_color");
        RGB rgb = RGBColors.getRGBColor(iconColor);
        Color damColor = new Color(rgb.red/255f, rgb.green/255f, rgb.blue/255f);
        damImage = new BufferedImage(15, 15, BufferedImage.TYPE_INT_ARGB);  
        Graphics2D g = (Graphics2D)damImage.getGraphics(); 
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(damColor);  
        g.fillArc(1, 1, 15, 8, 90, -180);
        
        return damImage;
    }
}