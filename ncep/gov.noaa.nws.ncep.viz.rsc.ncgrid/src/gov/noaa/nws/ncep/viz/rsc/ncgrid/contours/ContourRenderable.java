package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import gov.noaa.nws.ncep.gempak.parameters.line.LineDataStringParser;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.ContourSupport.ContourGroup;

import java.util.UUID;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;

/**
 * Generalized contour renderable
 * 
 * May be embedded in other renderable displays or form the basis of contour
 * resources
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 10, 2008	#1233	    chammack	Initial creation
 * Mar 01, 2010 #164		M. Li	    Applied to NC Perspective
 * Mar 08, 2011				M. Li		abstract class -> general class 
 * Apr 05, 2011				M. Li		increase threading retries from 10 to 100
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ContourRenderable implements IRenderable {

    private ContourGroup[] contourGroup;

    private final IMapDescriptor descriptor;

    private LineStyle lineStyle;

    private RGB color;

    private int outlineWidth;
    
    private final String uuid;

    private static final double METERS_AT_BASE_ZOOMLEVEL = 5000000.0;

    private static final double ZOOM_REACTION_FACTOR = .45;

    private static final int NUMBER_CONTOURING_LEVELS = 5;
    
    private int actual_contour_level = 1	;

//    public abstract IDataRecord getData();
//
//    public abstract MathTransform getTransform();
//
//    public abstract GeneralGridGeometry getGridGeometry();
//
//    public abstract ContourAttributes getContourAttributes();
    
    protected ContourAttributes contourAttributes;
    
    protected IDataRecord data;
    
    protected GeneralGridGeometry gridGeometry;

    private double lastMagnification = 1.0;

    private IFont font;
    
    private String name;


    /**
     * Constructor
     * 
     * @param styleRule
     * @param descriptor
     * @param callback
     * @param lineStyle
     */
    public ContourRenderable(IMapDescriptor descriptor) {
        this.descriptor = descriptor;
        uuid = UUID.randomUUID().toString();
    }

    public ContourRenderable(IDataRecord data, IMapDescriptor descriptor, GeneralGridGeometry gridGeometry, 
    		ContourAttributes contourAttributes, String fullName ) {
        this.descriptor = descriptor;
        uuid = UUID.randomUUID().toString();
        
        this.data = data;
        this.gridGeometry = gridGeometry;
        this.contourAttributes = contourAttributes;
        this.name = fullName;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	
        synchronized (this) {

        	LineDataStringParser lineAttr = new LineDataStringParser(contourAttributes.getLine());
        	this.color = GempakColor.convertToRGB(lineAttr.getInstanceOfLineBuilder().getLineColorsList().get(0));
        	this.lineStyle = lineAttr.getInstanceOfLineBuilder().getLineStyleList().get(0);
        	this.outlineWidth = lineAttr.getInstanceOfLineBuilder().getLineWidthList().get(0);
        	
            if (contourGroup == null) {
            	if (contourAttributes.getCint() != null ) {
                	String[] cintArray = contourAttributes.getCint().trim().split(">");
                	actual_contour_level = cintArray.length+1;
                }
            	
                if (actual_contour_level > NUMBER_CONTOURING_LEVELS ) 
                	actual_contour_level = NUMBER_CONTOURING_LEVELS;
                
                contourGroup = new ContourGroup[actual_contour_level];
            }
            double density = 1.0;//paintProps.getDensity();
            double magnification = 1.0; //paintProps.getMagnification();

            if (density > 4.0f) {
                density = 4.0f;
            }

            if (contourGroup != null) {
                if (this.lastMagnification != magnification || font == null) {
                    this.lastMagnification = magnification;
                    if (this.font != null) {
                        font.dispose();
                    }
                    font = target.getDefaultFont();

                    font = target.initializeFont(font.getFontName(),
                            (float) (font.getFontSize() / 1.4 * magnification),
                            null);
                }

                int i = contourGroup.length - 1;
                double mapWidth = ((MapDescriptor) this.descriptor)
                        .getMapWidth();
                double widthInMeters = mapWidth * paintProps.getZoomLevel();
                
                while (i >= 0) {
                    double curlvl = (METERS_AT_BASE_ZOOMLEVEL)
                            * (Math.pow(ZOOM_REACTION_FACTOR, i));

                    if (widthInMeters <= curlvl || i == 0) {
                        boolean contains = (contourGroup[i] == null || contourGroup[i].lastUsedPixelExtent == null) ? false
                                : (contourGroup[i].lastUsedPixelExtent
                                        .getEnvelope()
                                        .contains(((PixelExtent) paintProps
                                                .getView().getExtent())
                                                .getEnvelope()));
                        // Run contours if:
                        // 1. Contours was never ran before -or-
                        // 2. The area currently viewed is outside of the
                        // contoured window -or-
                        // 3. The density has changed
                        if (contourGroup[i] == null || !contains
                                || contourGroup[i].lastDensity != density) {

                            MathTransform mathTransformFromGrid = gridGeometry.getGridToCRS(PixelInCell.CELL_CENTER);

                            // If required data unavailable, quit now
                            if (mathTransformFromGrid == null
                                    || data == null
                                    || gridGeometry == null) {
                                return;
                            }

                            ContourGroup cg = null;
                            int retries = 0;
                            do {

                                // calculate the pixel density
                                float pixelDensity = (float) (paintProps
                                        .getCanvasBounds().width / paintProps
                                        .getView().getExtent().getWidth());

//                            	float zoomLevel = paintProps.getZoomLevel();
//                                
//                            	System.out.println(" pixelDensity="+pixelDensity+ " zoomLevel="+zoomLevel);
                            	
                                cg = ContourManagerJob.getInstance().request(
                                        uuid + "::" + this + "::" + i,
                                        data, i,
                                        paintProps.getView().getExtent(),
                                        density, mathTransformFromGrid,
                                        gridGeometry,
                                        descriptor.getGridGeometry(), target,
                                        descriptor, contourAttributes, name, pixelDensity);

                                try {
                                    if (cg == null
                                            && paintProps.getLoopProperties() != null
                                            && paintProps.getLoopProperties()
                                                    .isLooping()) {
                                    	
                                        Thread.sleep(50);
                                    }
                                } catch (InterruptedException e) {
                                    // ignore
                                }
                                retries++;
                            } while (cg == null
                                    && paintProps.getLoopProperties() != null
                                    && paintProps.getLoopProperties()
                                            .isLooping() && retries < 100);  // Allow up to 5 seconds for contouring

                            if (cg != null) {
                                // Dispose old wireframe shapes
                                if (contourGroup[i] != null
                                        && contourGroup[i].posValueShape != null) {
                                    contourGroup[i].posValueShape.dispose();
                                }

                                if (contourGroup[i] != null
                                        && contourGroup[i].negValueShape != null) {
                                    contourGroup[i].negValueShape.dispose();
                                }

                                if (contourGroup[i] != null
                                		&& contourGroup[i].fillShapes != null) {
                                	contourGroup[i].fillShapes.dispose();
                                }
                                
                                contourGroup[i] = cg;
                                contourGroup[i].posValueShape.compile();
                                contourGroup[i].negValueShape.compile();
                                contourGroup[i].fillShapes.compile();
                            } else {
                                target.setNeedsRefresh(true);
                            }

                        }

                        LineStyle posLineStyle = null;
                        LineStyle negLineStyle = null;
                        if (this.lineStyle == null) {
                            posLineStyle = LineStyle.SOLID;
                            negLineStyle = LineStyle.DASHED_LARGE;
                        } else {
                            posLineStyle = this.lineStyle;
                            negLineStyle = this.lineStyle;
                        }

                        if (contourGroup[i] != null
                                && paintProps.getView().getExtent().intersects(
                                        contourGroup[i].lastUsedPixelExtent)) {
                        	target.drawShadedShape(contourGroup[i].fillShapes , 0.5f);
                        	
                            target.drawWireframeShape(
                                    contourGroup[i].posValueShape, this.color,
                                    this.outlineWidth, posLineStyle, font);
                            target.drawWireframeShape(
                                    contourGroup[i].negValueShape, this.color,
                                    this.outlineWidth, negLineStyle, font);
                            

                        } else {
                            // see if we can display a higher level
                            if (i > 0) {
                                int j = i - 1;
                                if (contourGroup[j] != null) {
                                    if (contourGroup[j].posValueShape != null) {
                                        if (contourGroup[j].lastUsedPixelExtent
                                                .intersects(paintProps
                                                        .getView().getExtent())) {
                                        	
                                        	target.drawShadedShape( contourGroup[j].fillShapes, 0.5f);
                                        	
                                            target.drawWireframeShape(
                                                            contourGroup[j].posValueShape,
                                                            this.color,
                                                            this.outlineWidth,
                                                            posLineStyle, font);
                                            target.drawWireframeShape(
                                                            contourGroup[j].negValueShape,
                                                            this.color,
                                                            this.outlineWidth,
                                                            negLineStyle, font);
                                        }
                                    }
                                }
                            }

                            target.setNeedsRefresh(true);

                        }
                        break;

                    }
                    i--;
                }
            }
        }
    }

    /**
     * Dispose the renderable
     */
    public void dispose() {
        if (contourGroup != null) {
            for (ContourGroup c : contourGroup) {
                if (c == null) {
                    continue;
                }

                if (c.posValueShape != null) {
                    c.posValueShape.dispose();
                }
                if (c.negValueShape != null) {
                    c.negValueShape.dispose();
                }
                if (c.fillShapes != null) {
                	c.fillShapes.dispose();
                }
            }
        }
        if (font != null) {
            font.dispose();
        }

    }

    public ContourAttributes getContourAttributes() {
		return contourAttributes;
	}

	public void setContourAttributes(ContourAttributes contourAttributes) {
		this.contourAttributes = contourAttributes;
	}

	public IDataRecord getData() {
		return data;
	}

	public void setData(IDataRecord data) {
		this.data = data;
	}

	public GeneralGridGeometry getGridGeometry() {
		return gridGeometry;
	}

	public void setGridGeometry(GeneralGridGeometry gridGeometry) {
		this.gridGeometry = gridGeometry;
	}
}
