package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import gov.noaa.nws.ncep.gempak.parameters.hilo.HILOBuilder;
import gov.noaa.nws.ncep.gempak.parameters.intext.TextStringParser;
import gov.noaa.nws.ncep.gempak.parameters.marker.MARKER;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.viz.common.ui.HILORelativeMinAndMaxLocator;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Display grid relative minima and maxima values
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 24, 2010 363        X. Guo     	Initial creation
 * Aut 17, 2012 655        B. Hebbard   Added paintProps as parameter to IDisplayable draw (2)
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
public class GridRelativeHiLoDisplay implements IRenderable {

    // new fields
    private final IMapDescriptor descriptor;

    private final GeneralGridGeometry gridGeometryOfGrid;

    private HILOBuilder hiloBuild;

    private HILORelativeMinAndMaxLocator hiloLocator;

    private TextStringParser textMarkerString;

    private TextStringParser textValueString;

    private SymbolLocationSet gridPointHiSymbolSet = null;

    private SymbolLocationSet gridPointLoSymbolSet = null;

    // Hi/L symbol overtical/horizontal offset parameters
    private double symbolHiHight = 0.0;

    private double symbolLoHight = 0.0;

    private int charPix = 4;

    private double vertRatio;

    Rectangle2D charSize;

    public GridRelativeHiLoDisplay(HILOBuilder hiloBuild,
            HILORelativeMinAndMaxLocator hiloLocator,
            TextStringParser markerStr, TextStringParser valueStr,
            IMapDescriptor descriptor, ISpatialObject gridLocation) {
        this.descriptor = descriptor;
        this.gridGeometryOfGrid = MapUtil.getGridGeometry(gridLocation);
        this.hiloBuild = hiloBuild;
        this.hiloLocator = hiloLocator;
        this.textMarkerString = markerStr;
        this.textValueString = valueStr;
        initHiLoSymbolSet();
        // display ();
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

        if (paintProps.isZooming()) {
            return;
        }
        vertRatio = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;

        paintHighs(target, paintProps);
        paintLows(target, paintProps);
    }

    /**
     * Draw Highs
     */
    private void paintHighs(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        if (gridPointHiSymbolSet != null) {
            DisplayElementFactory df = new DisplayElementFactory(target,
                    this.descriptor);
            ArrayList<IDisplayable> elements = df.createDisplayElements(
                    gridPointHiSymbolSet, paintProps);
            for (IDisplayable each : elements) {
                each.draw(target, paintProps);
                each.dispose();
            }
        } else if (hiloBuild.getSymbolHiType() == 1) {
            drawHiCharacter(target, paintProps);
        }

        if (hiloBuild.getSymbolHiPlotValue()) {
            plotGridHiValue(target);
        }
    }

    /**
     * Draw Lows
     */
    private void paintLows(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        if (gridPointLoSymbolSet != null) {
            DisplayElementFactory df = new DisplayElementFactory(target,
                    this.descriptor);
            ArrayList<IDisplayable> elements = df.createDisplayElements(
                    gridPointLoSymbolSet, paintProps);
            for (IDisplayable each : elements) {
                each.draw(target, paintProps);
                each.dispose();
            }
        } else if (hiloBuild.getSymbolLoType() == 1) {
            drawLoCharacter(target, paintProps);
        }

        if (hiloBuild.getSymbolLoPlotValue()) {
            plotGridLoValue(target);
        }
    }

    /**
     * Draw High character
     */
    private void drawHiCharacter(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        float[] tmp1 = hiloLocator.getMaxColPositions();
        if (hiloBuild.getSymbolHi().length() > 0 && tmp1.length > 0) {
            String text = hiloBuild.getSymbolHi();
            float[] tmp2 = hiloLocator.getMaxRowPositions();

            IFont font = target.initializeFont(
                    getFontName(textMarkerString.getTextFont()),
                    textMarkerString.getTextSize(),
                    new IFont.Style[] { IFont.Style.BOLD });
            charSize = target.getStringBounds(font, "N");
            symbolHiHight = charSize.getHeight() * charPix;

            for (int i = 0; i < tmp1.length; i++) {

                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate((double) tmp1[i] - 1,
                                (double) tmp2[i] - 1), this.gridGeometryOfGrid,
                        Type.GRID_CENTER);
                try {
                    double[] d = this.descriptor.worldToPixel(new double[] {
                            (double) c.asLatLon().x, (double) c.asLatLon().y });

                    target.drawString(font, text, d[0], d[1], 0.0,
                            TextStyle.NORMAL, hiloBuild.getColorHi(),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, 0.0);
                } catch (TransformException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (FactoryException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Draw Low character
     */
    private void drawLoCharacter(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        float[] tmp1 = hiloLocator.getMinColPositions();
        if (hiloBuild.getSymbolLo().length() > 0 && tmp1.length > 0) {
            String text = hiloBuild.getSymbolLo();
            float[] tmp2 = hiloLocator.getMinRowPositions();

            IFont font = target.initializeFont(
                    getFontName(textMarkerString.getTextFont()),
                    textMarkerString.getTextSize(),
                    new IFont.Style[] { IFont.Style.BOLD });
            charSize = target.getStringBounds(font, "N");
            symbolLoHight = charSize.getHeight() * charPix;

            for (int i = 0; i < tmp1.length; i++) {

                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate((double) tmp1[i] - 1,
                                (double) tmp2[i] - 1), this.gridGeometryOfGrid,
                        Type.GRID_CENTER);
                try {
                    double[] d = this.descriptor.worldToPixel(new double[] {
                            (double) c.asLatLon().x, (double) c.asLatLon().y });

                    target.drawString(font, text, d[0], d[1], 0.0,
                            TextStyle.NORMAL, hiloBuild.getColorLo(),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, 0.0);
                } catch (TransformException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (FactoryException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Plot High grid values
     */
    private void plotGridHiValue(IGraphicsTarget target) throws VizException {
        float[] tmp1 = hiloLocator.getMaxColPositions();

        if (tmp1.length > 0) {
            float[] tmp2 = hiloLocator.getMaxRowPositions();
            float[] tmp3 = hiloLocator.getMaxValues();
            double offY = symbolHiHight;
            IFont font = target.initializeFont(
                    getFontName(textValueString.getTextFont()),
                    textValueString.getTextSize(),
                    new IFont.Style[] { IFont.Style.BOLD });
            if (offY > 0.0) {
                charSize = target.getStringBounds(font, "N");
                // offY += charSize.getHeight()*charPix;
                offY = charSize.getHeight() * vertRatio;
            }
            for (int i = 0; i < tmp1.length; i++) {

                String text = convertFloat2String(tmp3[i],
                        hiloBuild.getSymbolHiNumOfDecPls());
                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate((double) tmp1[i] - 1,
                                (double) tmp2[i] - 1), this.gridGeometryOfGrid,
                        Type.GRID_CENTER);
                try {
                    double[] d = this.descriptor.worldToPixel(new double[] {
                            (double) c.asLatLon().x, (double) c.asLatLon().y });

                    target.drawString(font, text, d[0], d[1] + offY, 0.0,
                            TextStyle.NORMAL, hiloBuild.getColorHi(),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, 0.0);
                } catch (TransformException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (FactoryException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Plot Low grid values
     */
    private void plotGridLoValue(IGraphicsTarget target) throws VizException {
        float[] tmp1 = hiloLocator.getMinColPositions();

        if (tmp1.length > 0) {
            float[] tmp2 = hiloLocator.getMinRowPositions();
            float[] tmp3 = hiloLocator.getMinValues();

            double offY = symbolLoHight;
            IFont font = target.initializeFont(
                    getFontName(textValueString.getTextFont()),
                    textValueString.getTextSize(),
                    new IFont.Style[] { IFont.Style.BOLD });
            if (offY > 0.0) {
                charSize = target.getStringBounds(font, "N");
                offY = charSize.getHeight() * vertRatio;
            }
            for (int i = 0; i < tmp1.length; i++) {

                String text = convertFloat2String(tmp3[i],
                        hiloBuild.getSymbolLoNumOfDecPls());
                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate((double) tmp1[i] - 1,
                                (double) tmp2[i] - 1), this.gridGeometryOfGrid,
                        Type.GRID_CENTER);
                try {
                    double[] d = this.descriptor.worldToPixel(new double[] {
                            (double) c.asLatLon().x, (double) c.asLatLon().y });

                    target.drawString(font, text, d[0], d[1] + offY, 0.0,
                            TextStyle.NORMAL, hiloBuild.getColorLo(),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, 0.0);
                } catch (TransformException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (FactoryException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Initialize Hi/Lo Symbol set
     */
    private void initHiLoSymbolSet() {

        int i;
        String tmp;
        String symbolName;
        MARKER marker;
        String catType;
        Color markerColor;
        float[] tmp2;
        float[] tmp1 = hiloLocator.getMaxColPositions();
        /*
         * Initialize High markers
         */
        if ((tmp1.length > 0)
                && ((hiloBuild.getSymbolHiType() == 2) || (hiloBuild
                        .getSymbolHiType() == 3))) {
            if (hiloBuild.getSymbolHiType() == 2) {
                // set marker
                tmp = "2/" + hiloBuild.getSymbolHiMarkerNumber() + "/"
                        + textMarkerString.getTextWidth();
                marker = new MARKER(tmp);
                symbolName = marker.getMarkerName();
                catType = "Markers";
            } else {
                symbolName = getSymbolName(hiloBuild.getSymbolHiMarkerNumber());
                catType = "Symbols";
            }

            markerColor = new Color(hiloBuild.getColorHi().red,
                    hiloBuild.getColorHi().green, hiloBuild.getColorHi().blue);
            Color[] colors = new Color[] { markerColor };
            Coordinate[] locations1 = new Coordinate[tmp1.length];
            tmp2 = hiloLocator.getMaxRowPositions();
            for (i = 0; i < tmp1.length; i++) {
                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate((double) tmp1[i] - 1,
                                (double) tmp2[i] - 1), this.gridGeometryOfGrid,
                        Type.GRID_CENTER);

                try {
                    locations1[i] = new Coordinate((double) c.asLatLon().x,
                            (double) c.asLatLon().y);
                } catch (TransformException e) {
                    e.printStackTrace();
                } catch (FactoryException e) {
                    e.printStackTrace();
                }
            }
            gridPointHiSymbolSet = new SymbolLocationSet(null, colors,
                    textMarkerString.getTextWidth(),
                    textMarkerString.getSymbolMarkerSize(), false, locations1,
                    catType, symbolName);
            symbolHiHight = gridPointHiSymbolSet.getSymbol().getSizeScale()
                    * charPix;
        }

        /*
         * Initialize Low markers
         */
        tmp1 = hiloLocator.getMinColPositions();
        if ((tmp1.length > 0)
                && ((hiloBuild.getSymbolLoType() == 2) || (hiloBuild
                        .getSymbolLoType() == 3))) {
            if (hiloBuild.getSymbolLoType() == 2) {
                // set marker
                tmp = "2/" + hiloBuild.getSymbolLoMarkerNumber() + "/"
                        + textMarkerString.getTextWidth();
                marker = new MARKER(tmp);
                symbolName = marker.getMarkerName();
                catType = "Markers";
            } else {
                symbolName = getSymbolName(hiloBuild.getSymbolLoMarkerNumber());
                catType = "Symbols";
            }
            markerColor = new Color(hiloBuild.getColorLo().red,
                    hiloBuild.getColorLo().green, hiloBuild.getColorLo().blue);
            Color[] colors1 = new Color[] { markerColor };
            Coordinate[] locations2 = new Coordinate[tmp1.length];
            tmp2 = hiloLocator.getMinRowPositions();
            for (i = 0; i < tmp1.length; i++) {
                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate((double) tmp1[i] - 1,
                                (double) tmp2[i] - 1), this.gridGeometryOfGrid,
                        Type.GRID_CENTER);

                try {
                    locations2[i] = new Coordinate((double) c.asLatLon().x,
                            (double) c.asLatLon().y);
                } catch (TransformException e) {
                    e.printStackTrace();
                } catch (FactoryException e) {
                    e.printStackTrace();
                }
            }
            gridPointLoSymbolSet = new SymbolLocationSet(null, colors1,
                    textMarkerString.getTextWidth(),
                    textMarkerString.getSymbolMarkerSize(), false, locations2,
                    catType, symbolName);
            symbolLoHight = gridPointLoSymbolSet.getSymbol().getSizeScale()
                    * charPix;
        }

    }

    /**
     * COnvert float value to string
     */
    private String convertFloat2String(float value, int nPls) {

        String text = String.format("%f", value);

        int ipre = (int) Math.pow(10, nPls);
        float tmp = (float) Math.round(value * ipre) / ipre;

        if (nPls == 0) {
            int ii = Math.round(value);
            text = Integer.toString(ii);
        } else if (nPls == 1) {
            text = String.format("%#.1f", tmp);
        } else if (nPls == 2) {
            text = String.format("%#.2f", tmp);
        } else if (nPls == 3) {
            text = String.format("%#.3f", tmp);
        } else if (nPls == 4) {
            text = String.format("%#.4f", tmp);
        } else if (nPls == 5) {
            text = String.format("%#.5f", tmp);
        } else if (nPls == 6) {
            text = String.format("%#.6f", tmp);
        } else if (nPls == 7) {
            text = String.format("%#.7f", tmp);
        } else if (nPls == 8) {
            text = String.format("%#.8f", tmp);
        } else if (nPls == 9) {
            text = String.format("%#.9f", tmp);
        }
        return text.trim();
    }

    /**
     * Get symbol name
     */
    private String getSymbolName(int symbolNum) {
        String symbol = "SQUARE";
        String[] name = { "SQUARE", "FILLED_SQUARE", "CIRCLE", "FILLED_CIRCLE",
                "TRIANGLE_SPCL", "FILLED_TRIANGLE_SPCL", "DIAMOND_SPCL",
                "FILLED_DIAMOND_SPCL", "STAR_SPCL", "FILLED_STAR_SPCL",
                "HIGH_PRESSURE_H", "LOW_PRESSURE_L", "FILLED_HIGH_PRESSURE_H",
                "FILLED_LOW_PRESSURE_L", "SINGLE_BRACKET",
                "BOTTOM_HALF_OF_BRACKET", "TOP_HALF_OF_BRACKET" };

        if ((symbolNum > 0) && (symbolNum <= name.length)) {
            symbol = name[symbolNum - 1];
        }
        return symbol;
    }

    /**
     * Get Font Name
     */
    private String getFontName(int fontNum) {
        String font = "Monospace";
        String[] name = { "Monospace", "Serif", "Serif" };

        if ((fontNum > 0) && (fontNum <= name.length)) {
            font = name[fontNum - 1];
        }
        return font;
    }

    /**
     * Display all variables
     */
    private void display() {
        int i;
        System.out.println("    HIGH -- Color\t\t["
                + hiloBuild.getColorHi().red + " "
                + hiloBuild.getColorHi().green + " "
                + hiloBuild.getColorHi().blue + "]");
        System.out.println("            symbol\t\t" + hiloBuild.getSymbolHi());
        System.out.println("            Marker Num\t\t"
                + hiloBuild.getSymbolHiMarkerNumber());
        System.out.println("            symbol type\t\t"
                + hiloBuild.getSymbolHiType());
        System.out.println("            value flg\t\t"
                + ((hiloBuild.getSymbolHiPlotValue()) ? "T" : "F"));
        System.out.println("            precision\t\t"
                + hiloBuild.getSymbolHiNumOfDecPls());
        float[] tmp = hiloLocator.getMaxColPositions();
        if (tmp.length > 0) {
            System.out.println(" Number of maxima found:" + tmp.length);
            System.out.println(" Column positions of max:");
            for (i = 0; i < tmp.length; i++) {
                System.out.print(tmp[i] + "  ");
                if ((i + 1) % 20 == 0)
                    System.out.print("\n");
            }
            System.out.print("\n");

            tmp = hiloLocator.getMaxRowPositions();
            System.out.println(" Row positions of max:");
            for (i = 0; i < tmp.length; i++) {
                System.out.print(tmp[i] + "  ");
                if ((i + 1) % 20 == 0)
                    System.out.print("\n");
            }
            System.out.print("\n");
            tmp = hiloLocator.getMaxValues();
            System.out.println(" Values of max:");
            for (i = 0; i < tmp.length; i++) {
                System.out.print(tmp[i] + "  ");
                if ((i + 1) % 20 == 0)
                    System.out.print("\n");
            }
            System.out.print("\n");
            System.out.println("-------------------------------------------");
        }

        System.out.println("    LOW -- Color\t\t[" + hiloBuild.getColorLo().red
                + " " + hiloBuild.getColorLo().green + " "
                + hiloBuild.getColorLo().blue + "]");
        System.out.println("            symbol\t\t" + hiloBuild.getSymbolLo());
        System.out.println("            Marker Num\t\t"
                + hiloBuild.getSymbolLoMarkerNumber());
        System.out.println("            symbol type\t\t"
                + hiloBuild.getSymbolLoType());
        System.out.println("            value flg\t\t"
                + ((hiloBuild.getSymbolLoPlotValue()) ? "T" : "F"));
        System.out.println("            precision\t\t"
                + hiloBuild.getSymbolLoNumOfDecPls());
        tmp = hiloLocator.getMinColPositions();
        if (tmp.length > 0) {
            System.out.println(" Number of minima found:" + tmp.length);
            System.out.println(" Column positions of min:");
            for (i = 0; i < tmp.length; i++) {
                System.out.print(tmp[i] + "  ");
                if ((i + 1) % 20 == 0)
                    System.out.print("\n");
            }
            System.out.print("\n");

            tmp = hiloLocator.getMinRowPositions();

            System.out.println(" Row positions of min:");
            for (i = 0; i < tmp.length; i++) {
                System.out.print(tmp[i] + "  ");
                if ((i + 1) % 20 == 0)
                    System.out.print("\n");
            }
            System.out.print("\n");
            tmp = hiloLocator.getMinValues();

            System.out.println(" Values of min:");
            for (i = 0; i < tmp.length; i++) {
                System.out.print(tmp[i] + "  ");
                if ((i + 1) % 20 == 0)
                    System.out.print("\n");
            }
            System.out.print("\n");
            System.out.println("-------------------------------------------");
        }
        System.out.println("            Text String: size["
                + textMarkerString.getTextSize() + ";"
                + textMarkerString.getSymbolMarkerSize() + "] font["
                + textMarkerString.getTextFont() + "] width["
                + textMarkerString.getTextWidth() + "] HW Flag ["
                + textMarkerString.getTextHWFlag() + "]");
        System.out.println("            Value String: size["
                + textValueString.getTextSize() + ";"
                + textValueString.getSymbolMarkerSize() + "] font["
                + textValueString.getTextFont() + "] width["
                + textValueString.getTextWidth() + "] HW Flag ["
                + textValueString.getTextHWFlag() + "]");
        System.out
                .println("==================================================");
    }
}
