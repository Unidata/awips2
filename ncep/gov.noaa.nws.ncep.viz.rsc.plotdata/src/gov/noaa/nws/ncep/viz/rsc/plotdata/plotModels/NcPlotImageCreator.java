package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;

import gov.noaa.nws.ncep.edex.common.metparameters.AbstractMetParameter;
import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.ui.pgen.display.IVector;
import gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefn;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefnsMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModelElement;
import gov.noaa.nws.ncep.viz.rsc.plotdata.queue.QueueEntry;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.NcPlotResource2.Station;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.TimeLogger;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.Tracer;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Semaphore;

import javax.measure.quantity.Angle;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableBasics;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#     Engineer       Description
 * ------------ ---------- ----------- --------------------------
 * 05/20/2013     988        Archana.S    Initial creation.
 */

public class NcPlotImageCreator {

    private TimeLogger timeLogger = null;

    private JobPool imageCreationJobPool = null;

    private ConcurrentLinkedQueue<QueueEntry> queueOfStations = null;

    private IPointInfoRenderingListener iPointInfoRenderingListener = null;

    private PlotModel plotModel = null;

    private Map<DataTime, Map<Coordinate, IVector>> mapOfWindBarbsPerFrame = null;

    private Map<Position, PlotModelElement> plotModelPositionMap = null;

    public boolean isThereAConditionalFilter = false;

    private Map<String, Symbol> symbolNameToSymbolMap = null;

    private Map<DataTime, Map<String, Station>> mapOfStnsToDataTime = null;

    private Map<DataTime, Map<Position, List<SymbolLocationSet>>> mapOfSymbolsPerPlotPosPerFrame = null;

    private static String[] skycSymbolNames = new String[] { "SKY_COVER_00",
            "SKY_COVER_01", "SKY_COVER_02", "SKY_COVER_03", "SKY_COVER_04",
            "SKY_COVER_05", "SKY_COVER_06", "SKY_COVER_07", "SKY_COVER_08",
            "SKY_COVER_09", "SKY_COVER_10" };

    private static String[] presWxSymbolNames = new String[] {
            "PRESENT_WX_080", "PRESENT_WX_081", "PRESENT_WX_082",
            "PRESENT_WX_085", "PRESENT_WX_086", "PRESENT_WX_095",
            "PRESENT_WX_097", "PRESENT_WX_066", "PRESENT_WX_010",
            "PRESENT_WX_048", "PRESENT_WX_041", "PRESENT_WX_044",
            "PRESENT_WX_045", "PRESENT_WX_019", "PRESENT_WX_008",
            "PRESENT_WX_036", "PRESENT_WX_037", "PRESENT_WX_039",
            "PRESENT_WX_004", "PRESENT_WX_005", "PRESENT_WX_006",
            "PRESENT_WX_007", "PRESENT_WX_009", "PRESENT_WX_056",
            "PRESENT_WX_067", "PRESENT_WX_057", "PRESENT_WX_088",
            "PRESENT_WX_078", "PRESENT_WX_079", "PRESENT_WX_087",
            "PRESENT_WX_058", "PRESENT_WX_069", "PRESENT_WX_061",
            "PRESENT_WX_065", "PRESENT_WX_063", "PRESENT_WX_051",
            "PRESENT_WX_055", "PRESENT_WX_053", "PRESENT_WX_071",
            "PRESENT_WX_075", "PRESENT_WX_073", "PRESENT_WX_203",
            "PRESENT_WX_079", "PRESENT_WX_077", "PRESENT_WX_201",
            "PRESENT_WX_044" };

    private static String[] icingSymbolNames = new String[] { "ICING_00",
            "ICING_01", "ICING_02", "ICING_03", "ICING_04", "ICING_05",
            "ICING_06", "ICING_08" };

    private static String[] turbSymbolNames = new String[] { "TURBULENCE_0",
            "TURBULENCE_1", "TURBULENCE_2", "TURBULENCE_3", "TURBULENCE_4",
            "TURBULENCE_5", "TURBULENCE_6", "TURBULENCE_7" };

    private static String[] presTendencySymbolNames = new String[] {
            "PRESSURE_TENDENCY_00", "PRESSURE_TENDENCY_01",
            "PRESSURE_TENDENCY_02", "PRESSURE_TENDENCY_03",
            "PRESSURE_TENDENCY_04", "PRESSURE_TENDENCY_05",
            "PRESSURE_TENDENCY_06", "PRESSURE_TENDENCY_07",
            "PRESSURE_TENDENCY_08" };

    private Map<Position, PlotSymbolType> posToSymbolTypeMap = null;

    private Map<Position, RGB> plotPosToColorMap = null;

    private Map<PlotSymbolType, Boolean> symbolExistsMap = null;

    private float initialFontSize = 14;

    private PlotParameterDefns plotParameterDefinitions = null;

    private IView lastView;

    private double lastZoomLevel = Double.MIN_VALUE;

    private static final File COURIER_NORMAL_FONT_FILE = NcPathManager
            .getInstance().getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR + "cour.pfa");

    private static final File SERIF_NORMAL_FONT_FILE = NcPathManager
            .getInstance()
            .getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR + "VeraSe.ttf");

    private static final File SERIF_BOLD_FONT_FILE = NcPathManager
            .getInstance().getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR
                            + "l049016t.pfa");

    private static final File SERIF_ITALIC_FONT_FILE = NcPathManager
            .getInstance().getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR
                            + "l049033t.pfa");

    private static final File SERIF_BOLD_ITALIC_FONT_FILE = NcPathManager
            .getInstance().getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR
                            + "l049036t.pfa");

    private static final File SANS_SERIF_NORMAL_FONT_FILE = NcPathManager
            .getInstance()
            .getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR + "luxisr.ttf");

    private static final File SANS_SERIF_ITALIC_FONT_FILE = NcPathManager
            .getInstance().getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR
                            + "luxisri.ttf");

    private static final File SANS_SERIF_BOLD_FONT_FILE = NcPathManager
            .getInstance()
            .getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR + "luxisb.ttf");

    private static final File SANS_SERIF_BOLD_ITALIC_FONT_FILE = NcPathManager
            .getInstance().getStaticFile(
                    NcPathManager.NcPathConstants.FONT_FILES_DIR
                            + "luxisbi.ttf");

    private static IFont COURIER_BOLD_FONT = null;

    private static IFont COURIER_NORMAL_FONT = null;

    private static IFont COURIER_ITALIC_FONT = null;

    private static IFont COURIER_BOLD_ITALIC_FONT = null;

    private static IFont SERIF_BOLD_ITALIC_FONT = null;

    private static IFont SANS_SERIF_BOLD_ITALIC_FONT = null;

    private static IFont SERIF_BOLD_FONT = null;

    private static IFont SANS_SERIF_BOLD_FONT = null;

    private static IFont SERIF_ITALIC_FONT = null;

    private static IFont SANS_SERIF_ITALIC_FONT = null;

    private static IFont SERIF_NORMAL_FONT = null;

    private static IFont SANS_SERIF_NORMAL_FONT = null;

    private RGB defaultColor;

    private static double TOLERANCE = 0.000000000000000000000001;

    private static double ZOOM_TOLERANCE = 0.0000000000000000000001;

    private static Amount WIND_SPD_3KNOTS = new Amount(3, NonSI.KNOT);

    private Map<PlotSymbolType, StringLookup> symbolLookupTable = null;

    private Map<DataTime, Map<Position, Map<String, DrawableString>>> dataTimeToText = null;

    private HashMap<Position, Double> plotPosToSymbolSizeMap = null;

    private HashMap<Position, IFont> plotPosToFontMap = null;

    private double plotDensity = Double.MIN_NORMAL;

    String prevFontStyle;

    IFont prevFont;

    public static enum Position {
        TC, UL, UC, UR, ML, MC, MR, LL, LC, LR, BC, SC, WD, INVALID

    }

    public static enum DisplayMode {
        TEXT, BARB, TABLE, NULL // ARROW, AVAIL, RANGE, SAMPLE
    }

    public NcPlotImageCreator(IPointInfoRenderingListener listener,
            PlotModel plotModel, double initialDensity) {
        Tracer.print("> Entry");
        defaultColor = new RGB(255, 255, 255);
        this.plotModel = plotModel;
        plotParameterDefinitions = PlotParameterDefnsMngr.getInstance()
                .getPlotParamDefns(plotModel.getPlugin());
        imageCreationJobPool = new JobPool("Creating station plots...", 8,
                false);
        queueOfStations = new ConcurrentLinkedQueue<QueueEntry>();
        timeLogger = TimeLogger.getInstance();
        iPointInfoRenderingListener = listener;
        mapOfStnsToDataTime = new HashMap<DataTime, Map<String, Station>>();
        dataTimeToText = new HashMap<DataTime, Map<Position, Map<String, DrawableString>>>();
        mapOfWindBarbsPerFrame = new HashMap<DataTime, Map<Coordinate, IVector>>();
        posToSymbolTypeMap = new HashMap<Position, PlotSymbolType>();
        symbolLookupTable = new HashMap<PlotSymbolType, StringLookup>();
        symbolNameToSymbolMap = new HashMap<String, Symbol>(0);
        plotModelPositionMap = new HashMap<Position, PlotModelElement>(11);
        plotPosToColorMap = new HashMap<Position, RGB>(11);
        plotPosToSymbolSizeMap = new HashMap<Position, Double>(11);
        plotPosToFontMap = new HashMap<Position, IFont>(11);
        mapOfSymbolsPerPlotPosPerFrame = new HashMap<DataTime, Map<Position, List<SymbolLocationSet>>>();
        symbolExistsMap = new HashMap<PlotSymbolType, Boolean>();

        setUpPlotPositionToPlotModelElementMapping(plotModel);
        setUpSymbolMappingTables();
        plotDensity = initialDensity;
        initializeFonts();
        Tracer.print("< Exit");
    }

    public double getPlotDensity() {
        return plotDensity;
    }

    public void setPlotDensity(double plotDensity) {
        Tracer.print("> Entry");
        this.plotDensity = plotDensity;
        Tracer.print("< Exit");
    }

    public synchronized void queueStationsToCreateImages(DataTime dt,
            Collection<Station> stations, double thePlotDensity) {
        Tracer.print("> Entry " + Tracer.shortTimeString(dt));
        QueueEntry qe = new QueueEntry(dt, stations);
        Tracer.print("About to queue " + stations.size()
                + " stations from frame " + Tracer.shortTimeString(dt)
                + " for image creation\n");
        queueOfStations.add(qe);
        runCreateImageTask(thePlotDensity);
        Tracer.print("< Exit  " + Tracer.shortTimeString(dt));
    }

    private void runCreateImageTask(double thePlotDensity) {
        Tracer.print("> Entry");
        if (queueOfStations.peek() == null)
            return;
        while (queueOfStations.peek() != null) {
            QueueEntry qe = queueOfStations.poll();
            Tracer.print("About to schedule image drawing task for "
                    + qe.getStations().size() + " stations from frame "
                    + qe.getDataTime().toString() + "\n");

            CreateDrawableStringsTask task = new CreateDrawableStringsTask(
                    qe.getDataTime(), qe.getStations(), thePlotDensity);
            imageCreationJobPool.schedule(task);
        }
        Tracer.print("< Exit");
    }

    private void initializeFonts() {
        Tracer.print("> Entry");
        IDisplayPane displayPane = NcDisplayMngr.getActiveNatlCntrsEditor()
                .getActiveDisplayPane();
        IGraphicsTarget target = displayPane.getTarget();
        COURIER_BOLD_FONT = target.initializeFont(COURIER_NORMAL_FONT_FILE,
                IFont.FontType.TYPE1, initialFontSize,
                new IFont.Style[] { IFont.Style.BOLD });
        COURIER_BOLD_ITALIC_FONT = target.initializeFont(
                COURIER_NORMAL_FONT_FILE, IFont.FontType.TYPE1,
                initialFontSize, new IFont.Style[] { IFont.Style.BOLD,
                        IFont.Style.ITALIC });
        COURIER_ITALIC_FONT = target.initializeFont(COURIER_NORMAL_FONT_FILE,
                IFont.FontType.TYPE1, initialFontSize,
                new IFont.Style[] { IFont.Style.ITALIC });
        COURIER_NORMAL_FONT = target.initializeFont(COURIER_NORMAL_FONT_FILE,
                IFont.FontType.TYPE1, initialFontSize, null);

        SERIF_BOLD_FONT = target.initializeFont(SERIF_BOLD_FONT_FILE,
                IFont.FontType.TYPE1, initialFontSize,
                new IFont.Style[] { IFont.Style.BOLD });
        SERIF_BOLD_ITALIC_FONT = target.initializeFont(
                SERIF_BOLD_ITALIC_FONT_FILE, IFont.FontType.TYPE1,
                initialFontSize, new IFont.Style[] { IFont.Style.BOLD,
                        IFont.Style.ITALIC });
        SERIF_ITALIC_FONT = target.initializeFont(SERIF_ITALIC_FONT_FILE,
                IFont.FontType.TYPE1, initialFontSize,
                new IFont.Style[] { IFont.Style.ITALIC });
        SERIF_NORMAL_FONT = target.initializeFont(SERIF_NORMAL_FONT_FILE,
                IFont.FontType.TRUETYPE, initialFontSize, null);

        SANS_SERIF_BOLD_FONT = target.initializeFont(SANS_SERIF_BOLD_FONT_FILE,
                IFont.FontType.TRUETYPE, initialFontSize,
                new IFont.Style[] { IFont.Style.BOLD });
        SANS_SERIF_BOLD_ITALIC_FONT = target.initializeFont(
                SANS_SERIF_BOLD_ITALIC_FONT_FILE, IFont.FontType.TRUETYPE,
                initialFontSize, new IFont.Style[] { IFont.Style.BOLD,
                        IFont.Style.ITALIC });
        SANS_SERIF_ITALIC_FONT = target.initializeFont(
                SANS_SERIF_ITALIC_FONT_FILE, IFont.FontType.TRUETYPE,
                initialFontSize, new IFont.Style[] { IFont.Style.ITALIC });
        SANS_SERIF_NORMAL_FONT = target.initializeFont(
                SANS_SERIF_NORMAL_FONT_FILE, IFont.FontType.TRUETYPE,
                initialFontSize, null);
        Tracer.print("< Exit");
    }

    public Position getPositionFromPlotModelElementPosition(
            String plotModelElementPosition) {
        Tracer.print("> Entry");
        Position position = Position.INVALID;
        if (plotModelElementPosition.compareTo("TC") == 0) {
            position = Position.TC;
        } else if (plotModelElementPosition.compareTo("BC") == 0) {
            position = Position.BC;
        } else if (plotModelElementPosition.compareTo("UL") == 0) {
            position = Position.UL;
        }

        else if (plotModelElementPosition.compareTo("UC") == 0) {
            position = Position.UC;
        } else if (plotModelElementPosition.compareTo("UR") == 0) {
            position = Position.UR;
        } else if (plotModelElementPosition.compareTo("ML") == 0) {
            position = Position.ML;
        }

        else if (plotModelElementPosition.compareTo("MC") == 0) {
            position = Position.MC;
        } else if (plotModelElementPosition.compareTo("MR") == 0) {
            position = Position.MR;
        } else if (plotModelElementPosition.compareTo("LL") == 0) {
            position = Position.LL;
        }

        else if (plotModelElementPosition.compareTo("LC") == 0) {
            position = Position.LC;
        } else if (plotModelElementPosition.compareTo("LR") == 0) {
            position = Position.LR;
        } else if (plotModelElementPosition.compareTo("WD") == 0) {
            position = Position.WD;
        } else if (plotModelElementPosition.compareTo("SC") == 0) {
            position = Position.SC;
        }
        Tracer.print("< Exit");
        return position;
    }

    private void setUpPlotModelElementToPlotColorMapping(Position p,
            PlotModelElement pme) {
        Tracer.print("> Entry");
        if (pme != null) {
            gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.Color pmeColor = pme
                    .getColor();
            RGB oldColor = plotPosToColorMap.get(p);
            if (oldColor == null
                    || ((oldColor.red != pmeColor.getRed())
                            || (oldColor.green != pmeColor.getGreen()) || (oldColor.blue != pmeColor
                            .getBlue()))) {
                RGB newColor = new RGB(pmeColor.getRed(), pmeColor.getGreen(),
                        pmeColor.getBlue());
                plotPosToColorMap.put(p, newColor);
            }
        }
        Tracer.print("< Exit");
    }

    private void setUpPlotModelToSymbolSizeMapping(Position p,
            PlotModelElement pme) {
        Tracer.print("> Entry");
        if (pme != null) {
            Double newSymbolSize = pme.getSymbolSize();
            Double oldSymbolSize = plotPosToSymbolSizeMap.get(p);
            if (oldSymbolSize == null
                    || (Math.abs(oldSymbolSize.doubleValue()
                            - newSymbolSize.doubleValue()) > 0.001))
                plotPosToSymbolSizeMap.put(p, newSymbolSize);
        }
        Tracer.print("< Exit");
    }

    private void setUpPlotModelToFontMapping(Position p, PlotModelElement pme) {
        Tracer.print("> Entry");
        if (pme != null) {

            /* Set the font information */
            int fontSize = Integer.parseInt(pme.getTextSize());
            String fontName = pme.getTextFont();
            String fontStyle = pme.getTextStyle();
            IFont font = null;
            if (prevFontStyle == null || prevFontStyle.isEmpty())
                prevFontStyle = new String(fontStyle);

            if (prevFont == null) {
                font = getFont(fontName, fontSize, fontStyle);
                prevFont = font;
            } else {
                /*
                 * Change the font only if the style/size/font name do not match
                 * the previous font
                 */
                String fontNameToCompare = prevFont.getFontName();
                int fontSizeToCompare = (int) prevFont.getFontSize();
                if ((fontSizeToCompare != fontSize)
                        || (fontNameToCompare.compareTo(fontName) != 0)
                        || (prevFontStyle.compareTo(fontStyle) != 0)) {
                    font = getFont(fontName, fontSize, fontStyle);
                    prevFont = font;
                }

                if (prevFontStyle.compareTo(fontStyle) != 0) {
                    prevFontStyle = fontStyle;
                }
            }
            if (plotPosToFontMap.get(p) == null
                    || !plotPosToFontMap.get(p).equals(prevFont))
                plotPosToFontMap.put(p, prevFont);
        }
        Tracer.print("< Exit");
    }

    public void setUpPlotPositionToPlotModelElementMapping(PlotModel pm) {
        Tracer.print("> Entry");
        List<PlotModelElement> plotModelElementsList = pm
                .getAllPlotModelElements();
        if (plotModelElementsList != null && !plotModelElementsList.isEmpty()) {
            synchronized (plotModelElementsList) {
                for (PlotModelElement pme : plotModelElementsList) {
                    Position position = getPositionFromPlotModelElementPosition(pme
                            .getPosition());
                    plotModelPositionMap.put(position, pme);
                    setUpPlotModelElementToPlotColorMapping(position, pme);
                    setUpPlotModelToFontMapping(position, pme);
                    setUpPlotModelToSymbolSizeMapping(position, pme);
                }
            }
        }
        Tracer.print("< Exit");
    }

    public void setPlotModel(PlotModel pm) {
        Tracer.print("> Entry");
        this.plotModel = pm;
        Tracer.print("< Exit");
    }

    public void removeObsoletePMEEntries(PlotModel pm) {
        Tracer.print("> Entry");
        List<PlotModelElement> plotModelElementsList = pm
                .getAllPlotModelElements();
        Set<Position> posToRemove = new HashSet<Position>(0);
        if (plotModelPositionMap != null && !plotModelPositionMap.isEmpty()) {
            Set<Position> posSet = plotModelPositionMap.keySet();
            synchronized (posSet) {
                for (Position pos : posSet) {
                    boolean matchFound = false;
                    synchronized (plotModelElementsList) {
                        for (PlotModelElement pme : plotModelElementsList) {
                            if (pos.toString().compareTo(pme.getPosition()) == 0) {
                                matchFound = true;
                                break;
                            }
                        }
                    }
                    if (!matchFound)
                        posToRemove.add(pos);
                }
            }
            synchronized (posToRemove) {
                for (Position p : posToRemove) {
                    plotModelPositionMap.remove(p);
                    plotPosToColorMap.remove(p);
                    plotPosToSymbolSizeMap.remove(p);
                    plotPosToFontMap.remove(p);
                    posToSymbolTypeMap.remove(p);
                    if (p == Position.WD) {
                        if (mapOfWindBarbsPerFrame != null
                                && !mapOfWindBarbsPerFrame.isEmpty()) {
                            synchronized (mapOfWindBarbsPerFrame) {
                                mapOfWindBarbsPerFrame.clear();
                                mapOfWindBarbsPerFrame = null;
                            }
                        }
                    } else {
                        Semaphore ss2 = new Semaphore(1);
                        ss2.acquireUninterruptibly();
                        if (dataTimeToText != null && !dataTimeToText.isEmpty()) {
                            Set<DataTime> frameTimeSet = dataTimeToText
                                    .keySet();
                            if (frameTimeSet != null && !frameTimeSet.isEmpty()) {
                                synchronized (frameTimeSet) {
                                    for (DataTime dt : frameTimeSet) {
                                        Map<Position, Map<String, DrawableString>> mapOfStrPosition = dataTimeToText
                                                .get(dt);
                                        if (mapOfStrPosition != null
                                                && !mapOfStrPosition.isEmpty()) {
                                            mapOfStrPosition.remove(p);
                                            dataTimeToText.put(dt,
                                                    mapOfStrPosition);
                                        }
                                    }
                                }
                            }
                        }
                        ss2.release();
                        ss2.acquireUninterruptibly();
                        if (mapOfSymbolsPerPlotPosPerFrame != null
                                && !mapOfSymbolsPerPlotPosPerFrame.isEmpty()) {
                            Set<DataTime> frameTimeSet = mapOfSymbolsPerPlotPosPerFrame
                                    .keySet();
                            if (frameTimeSet != null && !frameTimeSet.isEmpty()) {
                                synchronized (frameTimeSet) {
                                    for (DataTime dt : frameTimeSet) {
                                        Map<Position, List<SymbolLocationSet>> mapOfSymbolsToEachPosition = mapOfSymbolsPerPlotPosPerFrame
                                                .get(dt);
                                        if (mapOfSymbolsToEachPosition != null
                                                && !mapOfSymbolsToEachPosition
                                                        .isEmpty()) {
                                            mapOfSymbolsToEachPosition
                                                    .remove(p);
                                            mapOfSymbolsPerPlotPosPerFrame.put(
                                                    dt,
                                                    mapOfSymbolsToEachPosition);
                                        }
                                    }
                                }
                            }
                        }
                        ss2.release();
                    }

                }

            }

        }

        Tracer.print("< Exit");
    }

    private IFont getFont(String fontName, int fontSize, String fontStyle) {
        Tracer.print("> Entry");
        IFont font = null;

        if (COURIER_BOLD_FONT == null || COURIER_ITALIC_FONT == null
                || COURIER_NORMAL_FONT == null
                || COURIER_BOLD_ITALIC_FONT == null || SERIF_BOLD_FONT == null
                || SERIF_ITALIC_FONT == null || SERIF_BOLD_ITALIC_FONT == null
                || SERIF_NORMAL_FONT == null || SANS_SERIF_ITALIC_FONT == null
                || SANS_SERIF_BOLD_ITALIC_FONT == null
                || SANS_SERIF_NORMAL_FONT == null
                || SANS_SERIF_BOLD_FONT == null)
            initializeFonts();

        if (fontName.compareTo("Courier") == 0) {
            if (fontStyle.compareTo("Bold") == 0) {
                font = COURIER_BOLD_FONT;
            } else if (fontStyle.compareTo("Italic") == 0) {
                font = COURIER_ITALIC_FONT;
            } else if (fontStyle.compareTo("Bold-Italic") == 0) {
                font = COURIER_BOLD_ITALIC_FONT;
            } else
                font = COURIER_NORMAL_FONT;
        }

        else if (fontName.compareTo("Times") == 0) {
            if (fontStyle.compareTo("Bold") == 0) {
                font = SERIF_BOLD_FONT;
            } else if (fontStyle.compareTo("Italic") == 0) {
                font = SERIF_ITALIC_FONT;
            } else if (fontStyle.compareTo("Bold-Italic") == 0) {
                font = SERIF_BOLD_ITALIC_FONT;
            } else
                font = SERIF_NORMAL_FONT;
        } else {
            if (fontStyle.compareTo("Bold") == 0) {
                font = SANS_SERIF_BOLD_FONT;
            } else if (fontStyle.compareTo("Italic") == 0) {
                font = SANS_SERIF_ITALIC_FONT;
            } else if (fontStyle.compareTo("Bold-Italic") == 0) {
                font = SANS_SERIF_BOLD_ITALIC_FONT;
            } else
                font = SANS_SERIF_NORMAL_FONT;
        }

        if (font != null && fontSize != initialFontSize)
            font = font.deriveWithSize(fontSize);

        if (font != null) {
            font.setMagnification(1);
            font.setScaleFont(false);
        }
        Tracer.print("< Exit");

        return font;
    }

    /**
     * Creates symbols based on attributes provided in the PlotModelElement and
     * maps them to the symbol pattern name as specified in Pgen.
     */
    public void setUpSymbolMappingTables() {
        Tracer.print("> Entry");
        Set<Position> positionSet = plotModelPositionMap.keySet();
        synchronized (positionSet) {
            for (Position position : positionSet) {
                PlotModelElement pme = plotModelPositionMap.get(position);
                String plotParamName = pme.getParamName();
                PlotParameterDefn thisPlotParamDefn = plotParameterDefinitions
                        .getPlotParamDefn(plotParamName);
                if (thisPlotParamDefn == null) {
                    Tracer.print("Unable to find " + plotParamName
                            + " in the list of plot parameter definitions for "
                            + plotModel.getPlugin() + ":" + plotModel.getName());
                    continue;
                }

                if (thisPlotParamDefn.getPlotMode()
                        .compareToIgnoreCase("table") == 0) {
                    PlotSymbolType symbolType = getPlotSymbolType(pme
                            .getParamName());
                    if (symbolType != PlotSymbolType.INVALID) {
                        posToSymbolTypeMap.put(position, symbolType);
                        if (symbolExistsMap.get(symbolType) == null)
                            symbolExistsMap.put(symbolType, Boolean.FALSE);
                        StringLookup lookupTable = StringLookup
                                .readS2SFile(thisPlotParamDefn
                                        .getPlotLookupTable());
                        symbolLookupTable.put(symbolType, lookupTable);
                        String[] arrayToSearch = new String[0];
                        switch (symbolType) {
                        case WSYM:
                            arrayToSearch = Arrays.copyOf(presWxSymbolNames,
                                    presWxSymbolNames.length);
                            break;
                        case SKYC:
                            arrayToSearch = Arrays.copyOf(skycSymbolNames,
                                    skycSymbolNames.length);
                            break;
                        case ICSY:
                            arrayToSearch = Arrays.copyOf(icingSymbolNames,
                                    icingSymbolNames.length);
                            break;

                        case TBSY:
                            arrayToSearch = Arrays.copyOf(turbSymbolNames,
                                    turbSymbolNames.length);
                            break;

                        case PTSY:
                            arrayToSearch = Arrays.copyOf(
                                    presTendencySymbolNames,
                                    presTendencySymbolNames.length);
                            break;
                        default:
                            break;
                        }

                        if (arrayToSearch.length > 0) {
                            RGB rgb = plotPosToColorMap.get(position);
                            Color symbolColor = new Color(rgb.red, rgb.green,
                                    rgb.blue);
                            Color[] colorArray = new Color[] { symbolColor };
                            Coordinate dummyCoordinate = new Coordinate(0.0,
                                    0.0);
                            float symbolSize = plotPosToSymbolSizeMap.get(
                                    position).floatValue();
                            /*
                             * Create each symbol once to render it at different
                             * locations as needed
                             */
                            synchronized (arrayToSearch) {
                                for (String symbolName : arrayToSearch) {
                                    Symbol symbol = symbolNameToSymbolMap
                                            .get(symbolName);
                                    if (symbol == null) {
                                        symbol = new Symbol(null, colorArray,
                                                symbolSize, symbolSize, true,
                                                dummyCoordinate, "Symbol",
                                                symbolName);
                                    } else {
                                        if (!symbolColor.equals(symbol
                                                .getColors()[0]))
                                            symbol.setColors(colorArray);
                                        if (Math.abs(symbolSize
                                                - symbol.getSizeScale()) > 0.01) {
                                            symbol.setSizeScale(symbolSize);
                                            plotPosToSymbolSizeMap.put(
                                                    position, new Double(
                                                            symbolSize));
                                        }
                                    }
                                    symbolNameToSymbolMap.put(symbolName,
                                            symbol);
                                }
                            }

                        }

                    }
                }
            }
        }
        Tracer.print("< Exit");
    }

    public static enum PlotSymbolType {
        WSYM, SKYC, ICSY, TBSY, PTSY, INVALID
    }

    private PlotSymbolType getPlotSymbolType(String symbolGEMPAKName) {
        PlotSymbolType symbolType = PlotSymbolType.INVALID;
        if (symbolGEMPAKName.compareTo("WSYM") == 0) {
            symbolType = PlotSymbolType.WSYM;
        } else if (symbolGEMPAKName.compareTo("ICSY") == 0) {
            symbolType = PlotSymbolType.ICSY;
        }
        // else if(symbolGEMPAKName.compareTo("ITSY") == 0){
        // symbolType = PlotSymbolType.ITSY;
        // }
        //
        // else if(symbolGEMPAKName.compareTo("TFSY") == 0){
        // symbolType = PlotSymbolType.TFSY;
        // }
        // else if(symbolGEMPAKName.compareTo("TTSY") == 0){
        // symbolType = PlotSymbolType.TTSY;
        // }

        else if (symbolGEMPAKName.compareTo("TBSY") == 0) {
            symbolType = PlotSymbolType.TBSY;
        }

        else if (symbolGEMPAKName.compareTo("PTSY") == 0) {
            symbolType = PlotSymbolType.PTSY;
        } else if (symbolGEMPAKName.compareTo("SKYC") == 0) {
            symbolType = PlotSymbolType.SKYC;
        }

        return symbolType;
    }

    public static interface IPointInfoRenderingListener {
        public void renderingComplete(DataTime time,
                Collection<Station> disclosed,
                List<DrawableString> listOfStrinsToDraw,
                List<IVector> listOfWindVectors,
                List<SymbolLocationSet> listOfSymbolLocSet);
    }

    private final class CreateDrawableStringsTask implements Runnable {
        private DataTime dataTime = null;

        private List<Station> listOfStations = null;

        private IDescriptor mapDescriptor = null;

        private IGraphicsTarget aTarget = null;

        private IDisplayPane activePane = null;

        private IView view = null;

        private Rectangle canvasBounds = null;

        private IRenderableDisplay renderableDisplay = null;

        private boolean drawTextFirstTime = false;

        private boolean drawVectorsFirstTime = false;

        private boolean hasStationDensityChanged = false;

        private double density = Double.MIN_VALUE;

        @Override
        public void run() {
            Tracer.print("> Entry  START TASK "
                    + Tracer.shortTimeString(this.dataTime) + " "
                    + this.listOfStations.size() + " stations");
            createRenderableData();
            Tracer.print("< Exit   END TASK   "
                    + Tracer.shortTimeString(this.dataTime) + " "
                    + this.listOfStations.size() + " stations");
        }

        public CreateDrawableStringsTask(DataTime time,
                Collection<Station> listOfStationsToDrawImages,
                double thePlotDensity) {
            Tracer.print("> Entry " + Tracer.shortTimeString(time));
            Tracer.print("Creating a CreateDrawableStringsTask for the frame time: "
                    + Tracer.shortTimeString(time)
                    + " with "
                    + listOfStationsToDrawImages.size() + " stations");
            this.dataTime = new DataTime(time.getRefTime());
            this.listOfStations = new ArrayList<Station>(
                    listOfStationsToDrawImages);
            activePane = NcDisplayMngr.getActiveNatlCntrsEditor()
                    .getActiveDisplayPane();
            aTarget = activePane.getTarget();
            mapDescriptor = activePane.getDescriptor();
            renderableDisplay = mapDescriptor.getRenderableDisplay();
            view = renderableDisplay.getView();
            density = thePlotDensity;

            if (lastZoomLevel == Double.MIN_VALUE
                    || (Math.abs(renderableDisplay.getZoom() - lastZoomLevel) > ZOOM_TOLERANCE)) {
                lastZoomLevel = renderableDisplay.getZoom();
            }
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    Tracer.printX("> Entry  [runSync]");
                    if (activePane != null) {
                        canvasBounds = activePane.getBounds();

                    }
                    Tracer.printX("< Exit   [runSync]");
                }
            });
            Tracer.print("< Exit  " + Tracer.shortTimeString(time));

        }

        /**
         * Creates a key for the Station based on its lat/lon
         * 
         * @param lat
         * @param lon
         * @return
         */
        private String createStationMapKey(Double lon, Double lat) {
            return new String("" + Math.round(lon * 1000.0) + ","
                    + Math.round(lat * 1000.0));

        }

        private String createKeyFromTextCoordinates(DrawableBasics db) {
            if (db == null)
                return null;
            String key = new String("" + Math.round(db.x * 10000) + ","
                    + Math.round(db.y * 10000));
            // System.out.println("Created key = " + key);
            return key;
        }

        /**
         * Returns the corresponding windbarb character to the actual speed
         * 
         * @param windSpeed
         * @return The character that corresponds to the nearest 5 knot speed
         */
        private int windNormalizer(double dWindSpeed) {
            int windSpeed = (int) dWindSpeed;
            int major = windSpeed / 5;
            int minor = windSpeed % 5;
            if (minor >= 3) {
                major++;
            }
            return major * 5;
        }

        private Map<Coordinate, IVector> createWindVectors(
                Collection<Station> stnColl, String plotUnit,
                double symbolSize, Color[] windVectorColorArray,
                String metPrm1, String metPrm2, PlotModelElement pme) {
            Tracer.print("> Entry " + Tracer.shortTimeString(this.dataTime)
                    + " with " + stnColl.size() + " stations" + " metPrm1 "
                    + metPrm1 + " metPrm2 " + metPrm2);
            Map<Coordinate, IVector> localVectorPosToVectorMap = new HashMap<Coordinate, IVector>(
                    0);
            Semaphore ss = new Semaphore(1);
            ss.acquireUninterruptibly();
            mapDescriptor = NcDisplayMngr.getActiveNatlCntrsEditor()
                    .getActiveDisplayPane().getDescriptor();
            synchronized (stnColl) {
                Tracer.printX(Tracer.shortTimeString(this.dataTime)
                        + " Still have " + stnColl.size()
                        + " stations inside synchronized (stnColl)");
                for (Station currentStation : stnColl) {

                    double[] stationWorldLoc = { currentStation.info.longitude,
                            currentStation.info.latitude };

                    AbstractMetParameter vectorParam1 = null;
                    AbstractMetParameter vectorParam2 = null;
                    try {

                        IVector vector = null;
                        synchronized (currentStation.listOfParamsToPlot) {
                            for (AbstractMetParameter metPrm : currentStation.listOfParamsToPlot) {
                                Tracer.printX(Tracer
                                        .shortTimeString(this.dataTime)
                                        + " Loop at metPrm "
                                        + metPrm.getMetParamName()
                                        + " for "
                                        + currentStation.info.stationId);
                                if (metPrm.getMetParamName()
                                        .compareToIgnoreCase(metPrm1) == 0)
                                    vectorParam1 = metPrm;

                                if (metPrm.getMetParamName()
                                        .compareToIgnoreCase(metPrm2) == 0)
                                    vectorParam2 = metPrm;

                                if (vectorParam1 != null
                                        && vectorParam2 != null) {
                                    RGB rgb = null;
                                    if (pme.hasAdvancedSettings()) {
                                        rgb = getConditionalColor(
                                                currentStation, pme);
                                    }

                                    if (rgb != null) {
                                        windVectorColorArray = new Color[] { new Color(
                                                rgb.red, rgb.green, rgb.blue) };
                                    }

                                    vector = processBarbDirective(vectorParam1,
                                            vectorParam2, plotUnit, symbolSize,
                                            windVectorColorArray,
                                            stationWorldLoc);
                                    break;
                                } else {
                                    Tracer.printX(Tracer
                                            .shortTimeString(this.dataTime)
                                            + " For "
                                            + currentStation.info.stationId
                                            + ((vectorParam1 == null) ? " vectorParam1 is NULL"
                                                    : "")
                                            + ((vectorParam2 == null) ? " vectorParam2 is NULL"
                                                    : ""));
                                }

                            }

                            if (vector != null) {
                                Tracer.printX(Tracer
                                        .shortTimeString(this.dataTime)
                                        + " Adding a wind barb for "
                                        + currentStation.info.stationId);
                                localVectorPosToVectorMap.put(
                                        vector.getLocation(), vector);
                                Tracer.printX(Tracer
                                        .shortTimeString(this.dataTime)
                                        + " Created vector for "
                                        + currentStation.info.stationId);
                            } else {
                                Tracer.printX(Tracer
                                        .shortTimeString(this.dataTime)
                                        + " Adding a wind barb for "
                                        + currentStation.info.stationId
                                        + " NOT!  vector == null");
                            }

                        }

                    } catch (ArithmeticException ae) {
                        // System.out.println("ArithmeticException instead of azimuth for station : "
                        // + currentStation.info.stationId);
                        ss.release();
                    }

                    catch (IllegalStateException ise) {
                        // System.out.println("IllegalStateException instead of azimuth for station : "
                        // + currentStation.info.stationId);
                        ss.release();
                    } catch (NullPointerException npe) {
                        // System.out.println("NullPointerException for " +
                        // currentStation.info.stationId);
                        ss.release();
                    } catch (Exception e) {
                        ss.release();
                    }

                    // }
                }
            }
            ss.release();
            Tracer.print("< Exit  " + Tracer.shortTimeString(this.dataTime));

            return localVectorPosToVectorMap;
        }

        private IVector processBarbDirective(AbstractMetParameter metParam1,
                AbstractMetParameter metParam2, String plotUnit,
                double symbolSize, Color[] windVectorColorArray,
                double[] stationLoc) {

            Tracer.print("> Entry");
            AbstractMetParameter windSpeed = null, windDir = null;

            Vector vector = null;
            if (metParam1 instanceof Angle) {
                windDir = metParam1;
                windSpeed = metParam2;
            } else if (metParam2 instanceof Angle) {
                windDir = metParam2;
                windSpeed = metParam1;
            }
            try {
                // the units in the element are for the windSpeed and not the
                // direction.
                double dWindDir = windDir.getValueAs(NonSI.DEGREE_ANGLE)
                        .doubleValue();

                double cWindSpeed = windSpeed.getValueAs(plotUnit)
                        .doubleValue();
                double dWindSpeed = Double.MIN_VALUE;
                Unit<?> unit;

                unit = (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
                        plotUnit);
                double cWindSpeedThresh = WIND_SPD_3KNOTS.getValueAs(unit)
                        .doubleValue();

                if (cWindSpeed >= cWindSpeedThresh)
                    dWindSpeed = windNormalizer(cWindSpeed);

                vector = new Vector(null, windVectorColorArray, 1.0f,
                        symbolSize, false, new Coordinate(stationLoc[0],
                                stationLoc[1]), VectorType.WIND_BARB,
                        dWindSpeed, dWindDir, 2.0, true, "Vector", "Barb");

            } catch (ParseException e) {

                e.printStackTrace();
            }
            Tracer.print("< Exit" + "  returning "
                    + ((vector == null) ? "NULL" : "a vector"));

            return vector;

        }

        private final synchronized void createRenderableData() {
            Tracer.print("> Entry " + Tracer.shortTimeString(this.dataTime));
            if (listOfStations == null || listOfStations.isEmpty())
                return;
            Tracer.print(Tracer.shortTimeString(this.dataTime)
                    + " listOfStations has " + listOfStations.size()
                    + " stations after entry");
            Tracer.print("About to create renderable data for frame: "
                    + Tracer.shortTimeString(dataTime));
            Map<Position, List<SymbolLocationSet>> mapOfAllSymbolsAtEachPlotPosition = null;
            Map<Position, Map<String, DrawableString>> localDMap = null;
            List<DrawableString> listOfStringsToDraw = new ArrayList<DrawableString>();
            List<IVector> listOfWindVectors = new ArrayList<IVector>(0);
            List<SymbolLocationSet> symLocSetList = new ArrayList<SymbolLocationSet>(
                    0);
            Map<Coordinate, IVector> vectorPosToVectorMap = null;

            if (dataTimeToText.get(dataTime) != null)
                localDMap = new HashMap<Position, Map<String, DrawableString>>(
                        dataTimeToText.get(dataTime));
            else
                localDMap = new HashMap<Position, Map<String, DrawableString>>();

            Set<Position> positionSet = plotModelPositionMap.keySet();
            Map<String, Station> stationMap = new HashMap<String, Station>(
                    listOfStations.size());
            Tracer.printX(Tracer.shortTimeString(this.dataTime)
                    + " listOfStations has " + listOfStations.size()
                    + " stations right after stationMap creation");
            Semaphore sm = new Semaphore(1);

            sm.acquireUninterruptibly();
            Tracer.printX("From createRenderableData()  - list of parameters plotted per station for frame: "
                    + Tracer.shortTimeString(dataTime));
            /* Create a map of stations that have been currently disclosed */
            Tracer.printX(Tracer.shortTimeString(this.dataTime)
                    + " stationMap has " + stationMap.size()
                    + " stations before loop (should be 0)");
            for (Station station : listOfStations) {

                String key = createStationMapKey(station.info.longitude,
                        station.info.latitude)
                        + "-"
                        + station.info.dataTime.toString();
                try {
                    synchronized (station.stnPlotMap) {
                        Tracer.printX(Tracer.shortTimeString(this.dataTime)
                                + " stnplotMap for " + station.info.stationId
                                + " : " + station.stnPlotMap.toString());
                        // Set<Position> stnPosSet =
                        // station.stnPlotMap.keySet();
                        // synchronized (stnPosSet) {
                        // for (Position p : stnPosSet) {
                        // DrawableBasics dbs = station.stnPlotMap.get(p);
                        // // System.out.println(p.toString() + " - " +
                        // // dbs.x + "," + dbs.y);
                        // }
                        // }
                    }
                    synchronized (station.listOfParamsToPlot) {
                        Tracer.printX("For frame "
                                + Tracer.shortTimeString(dataTime) + " "
                                + station.info.stationId + " at time "
                                + station.info.dataTime.toString()
                                + " - List of parameters:  "
                                + station.listOfParamsToPlot.toString());
                    }
                } catch (Exception e) {

                }

                stationMap.put(key, station);

                // }
            }
            Tracer.print(Tracer.shortTimeString(this.dataTime)
                    + " stationMap has " + stationMap.size()
                    + " stations after loop");
            sm.release();

            Map<String, Station> prevStationMap = mapOfStnsToDataTime
                    .get(dataTime);
            if (prevStationMap == null) {
                sm.acquireUninterruptibly();
                prevStationMap = new HashMap<String, Station>(stationMap);
                sm.release();
            } else {
                Tracer.print(Tracer.shortTimeString(this.dataTime)
                        + " prevStationMap has " + prevStationMap.size()
                        + " stations");
            }
            if (lastView == null)
                lastView = view.clone();

            Rectangle2D r = aTarget.getStringsBounds(new DrawableString("M",
                    defaultColor));
            double width = r.getWidth();

            Rectangle2D r2 = aTarget.getStringsBounds(new DrawableString("'y",
                    defaultColor));
            double height = r2.getHeight();
            Rectangle textBounds = new Rectangle(0, 0, (int) width,
                    (int) height);
            Set<String> setOfKeysfOfStationsNotDisclosed = new HashSet<String>();

            PixelExtent lastViewExtent = (PixelExtent) lastView.getExtent();
            PixelExtent currViewExtent = (PixelExtent) view.getExtent();
            double currZoomLevel = mapDescriptor.getRenderableDisplay()
                    .getZoom();

            /*
             * If the extents or the zoom level have changed since the last
             * time...
             */
            if (Math.abs(currViewExtent.getEnvelope().getMinX()
                    - lastViewExtent.getEnvelope().getMinX()) > TOLERANCE
                    || Math.abs(currViewExtent.getEnvelope().getMaxX()
                            - lastViewExtent.getEnvelope().getMaxX()) > TOLERANCE
                    || Math.abs(currViewExtent.getEnvelope().getMinY()
                            - lastViewExtent.getEnvelope().getMinY()) > TOLERANCE
                    || Math.abs(currViewExtent.getEnvelope().getMaxY()
                            - lastViewExtent.getEnvelope().getMaxY()) > TOLERANCE
                    || Math.abs((lastZoomLevel - currZoomLevel)) > ZOOM_TOLERANCE
                    || Math.abs(density - plotDensity) > TOLERANCE
                    || isThereAConditionalFilter) {
                // System.out.println("station density changed");
                hasStationDensityChanged = true;
            }

            plotDensity = density;

            if (hasStationDensityChanged) {
                sm.acquireUninterruptibly();

                /* ...working from the map of stations previously processed... */
                if (prevStationMap != null && !prevStationMap.isEmpty()) {
                    synchronized (prevStationMap) {
                        Set<String> prevStationKeySet = prevStationMap.keySet();

                        for (String oldStnKey : prevStationKeySet) {
                            /*
                             * For each station in the map of stations
                             * previously processed,if it is not present in the
                             * current map of stations, add it to a separate
                             * list of stationsthat will eventually be discarded
                             * in the current run
                             */
                            Station oldStn = stationMap.get(oldStnKey);
                            if (oldStn == null) {
                                setOfKeysfOfStationsNotDisclosed.add(oldStnKey);
                            }
                        }
                    }
                }

                sm.release();
            }

            Tracer.print(Tracer.shortTimeString(this.dataTime)
                    + " positionSet has " + positionSet.size() + " elements");
            for (Position position : positionSet) {

                /*
                 * Get the formatting information from the PlotModelElement for
                 * the current plot position
                 */
                PlotModelElement pme = plotModelPositionMap.get(position);

                RGB pmeColor = plotPosToColorMap.get(position);

                /* Set the font information */
                IFont font = plotPosToFontMap.get(position);
                // if(font == null ){
                // System.out.println("time to debug fonts");
                // }

                /*
                 * Get the data retrieval information from the PlotParameterDefn
                 * corresponding to the current PlotModelElement
                 */
                PlotParameterDefn plotParamDefn = plotParameterDefinitions
                        .getPlotParamDefn(pme.getParamName());

                boolean drawVector = (position == Position.WD);
                boolean drawSymbol = (posToSymbolTypeMap.get(position) != null);
                boolean drawTextOnly = (plotParamDefn.getPlotMode().compareTo(
                        "table") != 0)
                        && (plotParamDefn.getPlotMode().compareTo("barb") != 0);

                if (drawTextOnly) {
                    Map<String, DrawableString> mapOfStrPositions = localDMap
                            .get(position);
                    drawTextFirstTime = (mapOfStrPositions == null);

                    if (drawTextFirstTime) {

                        mapOfStrPositions = new HashMap<String, DrawableString>();

                        sm.acquireUninterruptibly();
                        /*
                         * Loop thru all the stations to create the a list of
                         * DrawableString for each position
                         */

                        Set<String> stnKeySet = stationMap.keySet();
                        synchronized (stnKeySet) {
                            for (String key : stnKeySet) {
                                Station station = stationMap.get(key);

                                // Sanity check - shouldn't happen since this is
                                // already taken care of after querying the data
                                // from Postgres
                                if (station.info.longitude > 180
                                        || station.info.longitude < -180
                                        || station.info.latitude > 90
                                        || station.info.latitude < -90) {
                                    // System.out.println("Skipping the station: "
                                    // + station.info.stationId +
                                    // " since it has invalid or missing coordinates");
                                    continue;
                                }

                                double worldLoc[] = new double[] {
                                        station.info.longitude,
                                        station.info.latitude };
                                double[] tempPixLoc = mapDescriptor
                                        .worldToPixel(worldLoc);
                                station.pixelLocation = new Coordinate(
                                        tempPixLoc[0], tempPixLoc[1]);

                                /* Create the string to be rendered */
                                DrawableString drawableString = getDrawableStringForStation(
                                        station, plotParamDefn, font, pmeColor,
                                        position, aTarget, pme);

                                if (drawableString != null) {

                                    /*
                                     * For each station - Store the pixel
                                     * coordinates of the string to be rendered,
                                     * mapped to the position at which it needs
                                     * to be rendered
                                     */
                                    station.stnPlotMap.put(position,
                                            drawableString.basics);

                                    /*
                                     * Add the pixel coordinates and the string
                                     * to render to the Map<DrawableBasics,
                                     * DrawableString>
                                     */
                                    String dbKey = createKeyFromTextCoordinates(drawableString.basics);
                                    mapOfStrPositions
                                            .put(dbKey, drawableString);

                                    // System.out.println("Creating the string "
                                    // + drawableString.getText()[0]
                                    // + " for the first time "
                                    // + " for station: "
                                    // + station.info.stationId
                                    // + " in the frame: "
                                    // + dataTime.toString()
                                    // +" at the pix position "
                                    // +drawableString.basics.x + ","
                                    // +drawableString.basics.y
                                    // +"for the plot position - "
                                    // + position.toString());
                                }
                                // else{
                                // System.out.println("Drawing strings for the first time: couldn't create the formatted met param value for "
                                // + plotParamDefn.getPlotParamName()
                                // + " for station: "
                                // + station.info.stationId + " for frame: " +
                                // dataTime.toString() );
                                //
                                // //continue;
                                // }

                                /*
                                 * Update the stationMap with the current
                                 * station
                                 */
                                stationMap.put(key, station);

                            }
                        }
                        sm.release();

                    }

                    else {
                        // if(mapOfStrPositions == null )
                        // System.out.println("mapOfStrPositions is null");
                        // else{
                        // sm.acquireUninterruptibly();
                        // try{
                        // synchronized( mapOfStrPositions ){
                        // Set<String> dSet = mapOfStrPositions.keySet();
                        // System.out.println(
                        // dataTime.toString()
                        // +
                        // " - Before starting any processing for plot position "
                        // + position.toString());
                        // synchronized ( dSet ){
                        // for( String d : dSet ){
                        // DrawableString str = mapOfStrPositions.get(d);
                        // System.out.println(" String "
                        // + str.getText()[0]
                        // + " at "
                        // + d);
                        // }
                        // }
                        // }
                        // }catch(Exception e){
                        // sm.release();
                        // }
                        //
                        // sm.release();
                        // }
                        if (hasStationDensityChanged) {
                            sm.acquireUninterruptibly();
                            for (String stnid : setOfKeysfOfStationsNotDisclosed) {
                                Station obsoleteStation = prevStationMap
                                        .get(stnid);
                                /*
                                 * Remove the corresponding string that was
                                 * previously rendered at the current plot
                                 * positionfor this station
                                 */
                                if (obsoleteStation != null
                                        && obsoleteStation.stnPlotMap != null) {
                                    DrawableBasics dbStn = obsoleteStation.stnPlotMap
                                            .get(position);
                                    DrawableString str = mapOfStrPositions
                                            .get(createKeyFromTextCoordinates(dbStn));
                                    mapOfStrPositions.remove(dbStn);
                                    // if( str != null )
                                    // System.out.println( dataTime.toString() +
                                    // " " + position.toString() + " : "
                                    // + plotParamDefn.getPlotParamName() +
                                    // " - Removing "
                                    // + str.getText()[0] +
                                    // " for obsolete station: "
                                    // + obsoleteStation.info.stationId
                                    // + " from the pixel position: "
                                    // + dbStn.x
                                    // + dbStn.y);
                                    /*
                                     * Also remove the obsolete pixel position
                                     * from the station's plot position map
                                     */
                                    obsoleteStation.stnPlotMap.remove(position);
                                    prevStationMap.put(stnid, obsoleteStation);
                                }
                            }
                            sm.release();

                            /*
                             * Loop through the remaining strings at the current
                             * plot position and if they don't lie within the
                             * current extents, discard them - this takes care
                             * of removing strings for stations that are
                             * currently disclosed - whose previously rendered
                             * strings need to be repositioned per the current
                             * pixel position of the station
                             */

                            synchronized (mapOfStrPositions) {
                                List<String> pixPosToRemoveList = new ArrayList<String>();
                                sm.acquireUninterruptibly();
                                Set<String> dbSet = mapOfStrPositions.keySet();
                                synchronized (dbSet) {
                                    try {
                                        for (String db : dbSet) {
                                            String[] coords = db.split(",");
                                            double dbx = Double
                                                    .parseDouble(coords[0]) / 1000;
                                            double dby = Double
                                                    .parseDouble(coords[1]) / 1000;
                                            if (!view.getExtent().contains(
                                                    new double[] { dbx, dby })) {
                                                pixPosToRemoveList.add(db);
                                            }
                                        }

                                        synchronized (pixPosToRemoveList) {
                                            for (String db : pixPosToRemoveList) {

                                                DrawableString str = mapOfStrPositions
                                                        .get(db);
                                                // if( str != null )
                                                // System.out.println(
                                                // dataTime.toString()
                                                // + " "
                                                // + position.toString() + " : "
                                                // +
                                                // plotParamDefn.getPlotParamName()
                                                // +
                                                // " - Removing "+str.getText()[0]
                                                // + " from " + db
                                                // +
                                                // " since it lies outside the screen extents");
                                                mapOfStrPositions.remove(db);
                                            }

                                        }

                                    } catch (Exception e) {
                                        sm.release();
                                    }

                                    sm.release();
                                }
                            }
                        }

                        lastView = view.clone();

                        sm.acquireUninterruptibly();
                        Set<String> stnKeySet = stationMap.keySet();
                        synchronized (stnKeySet) {

                            for (String key : stnKeySet) {

                                Station station = stationMap.get(key);

                                // Sanity check - shouldn't happen since this is
                                // already taken care of after querying the data
                                // from Postgres
                                if (station.info.longitude > 180
                                        || station.info.longitude < -180
                                        || station.info.latitude > 90
                                        || station.info.latitude < -90) {
                                    // System.out.println("Skipping the station: "
                                    // + station.info.stationId +
                                    // " since it has invalid or missing coordinates");
                                    continue;
                                }

                                /*
                                 * For each station, update its pixel
                                 * coordinates per the current map descriptor
                                 */
                                double worldLoc[] = new double[] {
                                        station.info.longitude,
                                        station.info.latitude };
                                double[] tempPixLoc = mapDescriptor
                                        .worldToPixel(worldLoc);
                                station.pixelLocation.x = tempPixLoc[0];
                                station.pixelLocation.y = tempPixLoc[1];

                                /*
                                 * Working off of the pixel coordinates at each
                                 * position per station
                                 */
                                DrawableBasics dbStn = station.stnPlotMap
                                        .get(position);

                                if (dbStn != null) {

                                    /*
                                     * Fetch the corresponding string from the
                                     * Map<DrawableBasics, DrawableString>
                                     */
                                    String strPosKey = createKeyFromTextCoordinates(dbStn);
                                    DrawableString strToReposition = mapOfStrPositions
                                            .get(strPosKey);

                                    if (strToReposition != null) {
                                        // System.out.println(
                                        // dataTime.toString() + " Removing "
                                        // + strToReposition.getText()[0]
                                        // +" from its original pix pos: "
                                        // + dbStn.x + " , "
                                        // + dbStn.y + " from station "
                                        // + station.info.stationId);
                                        mapOfStrPositions.remove(strPosKey);
                                        station.stnPlotMap.remove(position);

                                        /*
                                         * If the string exists update its
                                         * coordinates
                                         */
                                        // System.out.println(
                                        // dataTime.toString()
                                        // + " " + position.toString() + " : "
                                        // + plotParamDefn.getPlotParamName()
                                        // +
                                        // " - updating the pixel position for the string "
                                        // + strToReposition.getText()[0]
                                        // + " for station: "
                                        // + station.info.stationId
                                        // + " at " + dataTime.toString());

                                        Rectangle2D rr = aTarget
                                                .getStringsBounds(new DrawableString(
                                                        "'"
                                                                + strToReposition
                                                                        .getText()[0]
                                                                + "y",
                                                        strToReposition
                                                                .getColors()[0]));
                                        textBounds = new Rectangle(0, 0,
                                                (int) rr.getWidth(),
                                                (int) rr.getHeight());
                                        double[] pixLoc = getUpdatedCoordinates(
                                                textBounds,
                                                station.pixelLocation.x,
                                                station.pixelLocation.y, view,
                                                canvasBounds, position);

                                        strToReposition.setCoordinates(
                                                pixLoc[0], pixLoc[1]);

                                        /*
                                         * Update the map of strings to be
                                         * rendered as well as the stationMap
                                         */

                                        RGB oldColor = strToReposition
                                                .getColors()[0];
                                        if ((!pme.hasAdvancedSettings())
                                                && (oldColor != null)
                                                && (oldColor.red != pmeColor.red
                                                        || oldColor.green != pmeColor.green || oldColor.blue != pmeColor.blue)) {
                                            strToReposition.setText(
                                                    strToReposition.getText(),
                                                    pmeColor);
                                        } else if (pme.hasAdvancedSettings()) {
                                            RGB rgb = getConditionalColor(
                                                    station, pme);
                                            strToReposition.setText(
                                                    strToReposition.getText(),
                                                    rgb);
                                        }

                                        if ((font.getFontName().compareTo(
                                                strToReposition.font
                                                        .getFontName()) != 0)
                                                || (Math.abs(font.getFontSize()
                                                        - strToReposition.font
                                                                .getFontSize()) > TOLERANCE)
                                                || (font.getStyle() != null
                                                        && strToReposition.font
                                                                .getStyle() != null
                                                        && font.getStyle().length > 0
                                                        && strToReposition.font
                                                                .getStyle().length > 0 && font
                                                        .getStyle()[0] != strToReposition.font
                                                        .getStyle()[0])
                                                || ((font.getStyle() == null || font
                                                        .getStyle().length == 0) && strToReposition.font
                                                        .getStyle() != null) /*
                                                                              * The
                                                                              * style
                                                                              * is
                                                                              * set
                                                                              * to
                                                                              * null
                                                                              * for
                                                                              * plain
                                                                              * style
                                                                              * fonts
                                                                              */
                                                || (font.getStyle() != null && (strToReposition.font
                                                        .getStyle() == null || strToReposition.font
                                                        .getStyle().length == 0)))
                                            strToReposition.font = font;

                                        synchronized (strToReposition.basics) {

                                            station.stnPlotMap.put(position,
                                                    strToReposition.basics);

                                            // System.out.println(
                                            // dataTime.toString()
                                            // + " For stn: "
                                            // + station.info.stationId
                                            // + " : "
                                            // + position.toString() + " - "
                                            // +
                                            // station.stnPlotMap.get(position).x
                                            // + " , "
                                            // +
                                            // station.stnPlotMap.get(position).y
                                            // );

                                            String localDbKey = createKeyFromTextCoordinates(strToReposition.basics);
                                            mapOfStrPositions.put(localDbKey,
                                                    strToReposition);
                                            // if(mapOfStrPositions.get(
                                            // localDbKey) != null ){
                                            // System.out.println(
                                            // dataTime.toString()
                                            // + " For string: "
                                            // + mapOfStrPositions.get(
                                            // localDbKey).getText()[0]
                                            // + " :: " );
                                            //
                                            // System.out.println(
                                            // " Updated pix pos: "
                                            // + strToReposition.basics.x
                                            // + " , "
                                            // + strToReposition.basics.y);
                                            // }
                                        }
                                    } else {
                                        /*
                                         * If none exists - create it - maybe
                                         * the station reports this parameter
                                         * only at a designated time
                                         */
                                        DrawableString drawableString = getDrawableStringForStation(
                                                station, plotParamDefn, font,
                                                pmeColor, position, aTarget,
                                                pme);
                                        if (drawableString != null) {

                                            /*
                                             * Update the map of strings to be
                                             * rendered as well as the
                                             * stationMap
                                             */
                                            // System.out.println(
                                            // dataTime.toString()
                                            // + " - " + position.toString()
                                            // + "::New parameter "
                                            // +
                                            // plotParamDefn.getPlotParamName()
                                            // + " = "
                                            // + drawableString.getText()[0] +
                                            // " reported by "
                                            // + station.info.stationId);

                                            station.stnPlotMap.put(position,
                                                    drawableString.basics);
                                            String keydb = createKeyFromTextCoordinates(drawableString.basics);
                                            mapOfStrPositions.put(keydb,
                                                    drawableString);
                                        }
                                        // else{
                                        // System.out.println(
                                        // dataTime.toString()
                                        // +
                                        // " Unable to create the met param value for "
                                        // + plotParamDefn.getPlotParamName()
                                        // + " for station: "
                                        // + station.info.stationId );
                                        // }
                                    }

                                    stationMap.put(key, station);

                                } else {
                                    /*
                                     * This could either be a new station or the
                                     * previous pixel position could be
                                     * obsolete. So create the string to be
                                     * rendered
                                     */
                                    // System.out.println( dataTime.toString() +
                                    // " Trying to create the met parameter value for a new station ?");
                                    DrawableString drawableString = getDrawableStringForStation(
                                            station, plotParamDefn, font,
                                            pmeColor, position, aTarget, pme);

                                    // if( drawableString == null ){
                                    //
                                    // System.out.println( dataTime.toString()
                                    // +
                                    // " Couldn't create the met param value for "
                                    // + plotParamDefn.getPlotParamName()
                                    // + " for station: "
                                    // + station.info.stationId );
                                    // //continue;
                                    //
                                    // }
                                    /*
                                     * And if it is not null - update the map of
                                     * strings to be rendered as well as the
                                     * stationMap
                                     */
                                    if (drawableString != null) {

                                        RGB oldColor = drawableString
                                                .getColors()[0];
                                        if ((!pme.hasAdvancedSettings())
                                                && (oldColor.red != pmeColor.red
                                                        || oldColor.green != pmeColor.green || oldColor.blue != pmeColor.blue)) {
                                            drawableString.setText(
                                                    drawableString.getText(),
                                                    pmeColor);
                                        } else if (pme.hasAdvancedSettings()) {
                                            RGB rgb = getConditionalColor(
                                                    station, pme);
                                            drawableString.setText(
                                                    drawableString.getText(),
                                                    rgb);
                                        }

                                        if ((font.getFontName().compareTo(
                                                drawableString.font
                                                        .getFontName()) != 0)
                                                || (Math.abs(font.getFontSize()
                                                        - drawableString.font
                                                                .getFontSize()) > TOLERANCE)
                                                || (font.getStyle() != null
                                                        && drawableString.font
                                                                .getStyle() != null
                                                        && font.getStyle().length > 0
                                                        && drawableString.font
                                                                .getStyle().length > 0 && font
                                                        .getStyle()[0] != drawableString.font
                                                        .getStyle()[0])
                                                || ((font.getStyle() == null || font
                                                        .getStyle().length == 0) && drawableString.font
                                                        .getStyle() != null) /*
                                                                              * The
                                                                              * style
                                                                              * is
                                                                              * set
                                                                              * to
                                                                              * null
                                                                              * for
                                                                              * plain
                                                                              * style
                                                                              * fonts
                                                                              */
                                                || (font.getStyle() != null && (drawableString.font
                                                        .getStyle() == null || drawableString.font
                                                        .getStyle().length == 0)))
                                            drawableString.font = font;

                                        synchronized (drawableString.basics) {

                                            // System.out.println(
                                            // plotParamDefn.getPlotParamName()
                                            // + dataTime.toString()
                                            // + " Station "+
                                            // station.info.stationId + " : "
                                            // + position.toString()
                                            // + " - Created "
                                            // + drawableString.getText()[0]
                                            // + " at pix pos: "
                                            // + drawableString.basics.x
                                            // + ","
                                            // + drawableString.basics.y );

                                            station.stnPlotMap.put(position,
                                                    drawableString.basics);
                                            String dbkey = createKeyFromTextCoordinates(drawableString.basics);
                                            mapOfStrPositions.put(dbkey,
                                                    drawableString);

                                            // System.out.println(
                                            // dataTime.toString()
                                            // + " "
                                            // + drawableString.getText()[0]
                                            // + " - "
                                            // + drawableString.basics.x
                                            // + ","
                                            // + drawableString.basics.y );
                                        }

                                        // System.out.println(
                                        // dataTime.toString() + " "
                                        // + position.toString() + ":"
                                        // + plotParamDefn.getPlotParamName()
                                        // drawableString.getText()[0]
                                        // + " for station " +
                                        // station.info.stationId
                                        // + " at time " + dataTime.toString()
                                        // );

                                    }

                                    stationMap.put(key, station);
                                }
                            }

                        }
                        sm.release();

                    }

                    /*
                     * After looping thru all stations, for each position,
                     * update the Map<Position, Map<DrawableBasics,
                     * DrawableString>>
                     */
                    localDMap.put(position, mapOfStrPositions);

                    // sm.acquireUninterruptibly();
                    // try{
                    // Map<String, DrawableString> dMap =
                    // localDMap.get(position);
                    // synchronized( dMap ){
                    // Set<String> dbSet = dMap.keySet();
                    // System.out.println( dataTime.toString() +
                    // " After completing all processing for position "+
                    // position.toString());
                    //
                    // for( String d : dbSet ){
                    // DrawableString str = dMap.get(d);
                    // System.out.println(" String " + str.getText()[0] + " at "
                    // + d);
                    // }
                    // }
                    //
                    // System.out.println( dataTime.toString() + " - " +
                    // position.toString() + ":" + dMap.size());
                    // }catch(Exception e){
                    // sm.release();
                    // }
                    //
                    //
                    //
                    // sm.release();

                }

                if (drawVector) {
                    if (mapOfWindBarbsPerFrame == null)
                        mapOfWindBarbsPerFrame = new HashMap<DataTime, Map<Coordinate, IVector>>();
                    vectorPosToVectorMap = mapOfWindBarbsPerFrame.get(dataTime);
                    drawVectorsFirstTime = (vectorPosToVectorMap == null);
                    String[] vectorPrmNames = plotParamDefn
                            .getMetParamNamesForVectorPlot();
                    String metPrm1 = vectorPrmNames[0];
                    String metPrm2 = vectorPrmNames[1];
                    Double d = plotPosToSymbolSizeMap.get(position);
                    double symbolSize = (d == null ? 1.0 : d.doubleValue());
                    Color[] windVectorColorArray = new Color[] { new Color(
                            pmeColor.red, pmeColor.green, pmeColor.blue) };
                    Collection<Station> stnColl = stationMap.values();
                    Tracer.printX(Tracer.shortTimeString(this.dataTime)
                            + " (1) stnColl has " + stnColl.size()
                            + " stations");
                    // for( Station s: stnColl ){
                    // System.out.print(s.info.stationId + ",");
                    // }
                    // System.out.println("stnColl = " + stnColl);
                    if (drawVectorsFirstTime) {

                        sm.acquireUninterruptibly();

                        vectorPosToVectorMap = new HashMap<Coordinate, IVector>(
                                createWindVectors(stnColl,
                                        plotParamDefn.getPlotUnit(),
                                        symbolSize, windVectorColorArray,
                                        metPrm1, metPrm2, pme));

                        sm.release();

                    } else {

                        if (hasStationDensityChanged) {
                            /*
                             * If the station density is different from the
                             * previous time this method was called, remove the
                             * obsolete wind vectors
                             */
                            sm.acquireUninterruptibly();
                            int counter = 0;
                            if (prevStationMap != null
                                    && !prevStationMap.isEmpty()) {
                                for (String stnKey : setOfKeysfOfStationsNotDisclosed) {

                                    Station obsoleteStation = prevStationMap
                                            .get(stnKey);
                                    if (obsoleteStation != null) {
                                        Coordinate coord = new Coordinate(
                                                obsoleteStation.info.longitude,
                                                obsoleteStation.info.latitude);
                                        IVector vectToRemove = vectorPosToVectorMap
                                                .get(coord);
                                        if (vectToRemove != null) {
                                            vectorPosToVectorMap.remove(coord);
                                            // System.out.println("Removing barb from obsolete station: "+
                                            // obsoleteStation.info.stationId);
                                            counter++;
                                        }

                                    }
                                }

                            }

                            /*
                             * Removing vectors that are no longer within the
                             * current screen extents
                             */

                            try {
                                Set<Coordinate> vectorsToRemove = new HashSet<Coordinate>();
                                Set<Coordinate> prevVectorCoordSet = vectorPosToVectorMap
                                        .keySet();
                                if (prevVectorCoordSet != null
                                        && !prevVectorCoordSet.isEmpty()) {
                                    synchronized (prevVectorCoordSet) {
                                        for (Coordinate c : prevVectorCoordSet) {
                                            double[] tempPixLoc = mapDescriptor
                                                    .worldToPixel(new double[] {
                                                            c.x, c.y });
                                            if (!view.getExtent().contains(
                                                    tempPixLoc)) {
                                                synchronized (vectorsToRemove) {
                                                    vectorsToRemove.add(c);
                                                }
                                            }
                                        }
                                    }

                                    if (!vectorsToRemove.isEmpty()) {
                                        synchronized (vectorsToRemove) {
                                            for (Coordinate c : vectorsToRemove) {
                                                synchronized (vectorPosToVectorMap) {
                                                    vectorPosToVectorMap
                                                            .remove(c);
                                                }
                                            }
                                        }
                                    }

                                }
                            } catch (Exception e) {

                            }

                            // System.out.println("Remaining number of barbs for frame: "+
                            // dataTime.toString() + " - " +
                            // vectorPosToVectorMap.size());
                            Tracer.printX(Tracer.shortTimeString(this.dataTime)
                                    + " (2) stnColl has " + stnColl.size()
                                    + " stations");
                            Map<Coordinate, IVector> vecMap = new HashMap<Coordinate, IVector>(
                                    createWindVectors(stnColl,
                                            plotParamDefn.getPlotUnit(),
                                            symbolSize, windVectorColorArray,
                                            metPrm1, metPrm2, pme));
                            vectorPosToVectorMap.putAll(vecMap);
                            // System.out.println("Added " + vecMap.size() +
                            // " barbs for frame "+ dataTime.toString());
                            sm.release();
                        } else {

                            Set<Coordinate> coordSet = vectorPosToVectorMap
                                    .keySet();
                            sm.acquireUninterruptibly();
                            synchronized (coordSet) {
                                for (Coordinate c : coordSet) {
                                    Vector vector = (Vector) vectorPosToVectorMap
                                            .get(c);
                                    if (Math.abs(vector.getSizeScale()
                                            - symbolSize) > 0.01)
                                        vector.setSizeScale(symbolSize);
                                    if (!vector.getColors()[0]
                                            .equals(windVectorColorArray[0]))
                                        vector.setColors(windVectorColorArray);

                                    vectorPosToVectorMap.put(c, vector);
                                }
                            }
                            sm.release();
                        }

                    }

                }

                if (drawSymbol) {
                    /*
                     * Since the Symbol associated with each SymbolLocationSet
                     * is passed in as an argument to the constructor there is
                     * no explicit method to set the symbol should its
                     * attributes change. Hence the symbols get recreated afresh
                     * each time.
                     */
                    mapOfAllSymbolsAtEachPlotPosition = mapOfSymbolsPerPlotPosPerFrame
                            .get(dataTime);
                    PlotSymbolType symbolType = posToSymbolTypeMap
                            .get(position);

                    String metParamName = plotParamDefn.getMetParamName();
                    if (symbolType != null) {

                        StringLookup lookupTable = symbolLookupTable
                                .get(symbolType);
                        symbolExistsMap.put(symbolType, Boolean.TRUE);

                        if (mapOfAllSymbolsAtEachPlotPosition == null)
                            mapOfAllSymbolsAtEachPlotPosition = new HashMap<Position, List<SymbolLocationSet>>();

                        List<Coordinate> listOfCoords = null;
                        Map<Symbol, List<Coordinate>> symbolToSetOfCoordsMap = new HashMap<Symbol, List<Coordinate>>();
                        sm.acquireUninterruptibly();
                        Collection<Station> stnColl = stationMap.values();
                        Semaphore ss = new Semaphore(1);
                        ss.acquireUninterruptibly();

                        synchronized (stnColl) {

                            for (Station station : stnColl) {
                                AbstractMetParameter tableParamToPlot = null;
                                String symbolPatternName;

                                synchronized (station.listOfParamsToPlot) {
                                    try {
                                        for (AbstractMetParameter metPrm : station.listOfParamsToPlot) {
                                            if (metParamName.compareTo(metPrm
                                                    .getMetParamName()) == 0) {
                                                tableParamToPlot = metPrm;
                                                break;
                                            }
                                        }
                                    } catch (Exception e) {

                                    }

                                }

                                if (tableParamToPlot != null) {
                                    String formattedString = null;
                                    try {
                                        formattedString = getFormattedValueToPlot(
                                                plotParamDefn, tableParamToPlot);

                                        if (formattedString == null
                                                || formattedString.isEmpty()) {
                                            continue;
                                        }
                                        if (lookupTable == null
                                                || lookupTable
                                                        .recursiveTranslation(formattedString) == null) {
                                            continue;
                                        }

                                        symbolPatternName = new String(
                                                lookupTable
                                                        .recursiveTranslation(formattedString));
                                        if (symbolPatternName == null
                                                || symbolPatternName.isEmpty()) {
                                            continue;
                                        }

                                        Symbol symbol = symbolNameToSymbolMap
                                                .get(symbolPatternName);
                                        RGB rgb = null;
                                        /*
                                         * Get the conditional color for the
                                         * symbol if applicable
                                         */
                                        if (pme.hasAdvancedSettings()) {
                                            rgb = getConditionalColor(station,
                                                    pme);
                                        }

                                        if (symbol == null)
                                            continue;

                                        double worldLoc[] = new double[] {
                                                station.info.longitude,
                                                station.info.latitude };
                                        double[] tempPixLoc = mapDescriptor
                                                .worldToPixel(worldLoc);
                                        tempPixLoc = getUpdatedCoordinates(
                                                textBounds, tempPixLoc[0],
                                                tempPixLoc[1], view,
                                                canvasBounds, position);
                                        worldLoc = mapDescriptor
                                                .pixelToWorld(tempPixLoc);
                                        listOfCoords = symbolToSetOfCoordsMap
                                                .get(symbol);
                                        if (listOfCoords == null) {
                                            listOfCoords = new ArrayList<Coordinate>();
                                        }

                                        listOfCoords.add(new Coordinate(
                                                worldLoc[0], worldLoc[1]));

                                        if (rgb != null) {
                                            symbol.setColors(new Color[] { new Color(
                                                    rgb.red, rgb.blue,
                                                    rgb.green) });

                                            // symbol = new Symbol( null, new
                                            // Color[]{ new Color( rgb.red,
                                            // rgb.blue, rgb.green ) },
                                            // symbol.getLineWidth(),
                                            // symbol.getSizeScale(), true, new
                                            // Coordinate(0,0,0),
                                            // "Symbol", symbol.getPatternName()
                                            // );
                                        } else {
                                            Color currSymbolColor = symbol
                                                    .getColors()[0];
                                            rgb = new RGB(
                                                    currSymbolColor.getRed(),
                                                    currSymbolColor.getGreen(),
                                                    currSymbolColor.getBlue());
                                            if (!rgb.equals(pmeColor)) {
                                                symbol.setColors(new Color[] { new Color(
                                                        pmeColor.red,
                                                        pmeColor.blue,
                                                        pmeColor.green) });
                                                // symbol = new Symbol( null,
                                                // new Color[]{ new Color(
                                                // pmeColor.red, pmeColor.blue,
                                                // pmeColor.green ) },
                                                // symbol.getLineWidth(),
                                                // symbol.getSizeScale(), true,
                                                // new Coordinate(0,0,0),
                                                // "Symbol",
                                                // symbol.getPatternName() );

                                            }
                                        }
                                        double prevSizeOfSymbol = symbol
                                                .getSizeScale();
                                        double expectedSymbolSize = plotPosToSymbolSizeMap
                                                .get(position).doubleValue();
                                        if (Math.abs(expectedSymbolSize
                                                - prevSizeOfSymbol) > TOLERANCE)
                                            symbol.setSizeScale(expectedSymbolSize);

                                        symbolToSetOfCoordsMap.put(symbol,
                                                listOfCoords);
                                    } catch (VizException e) {

                                        e.printStackTrace();
                                    }

                                }
                            }
                        }
                        ss.release();
                        Set<Symbol> symSet = symbolToSetOfCoordsMap.keySet();
                        List<SymbolLocationSet> listOfSymLocSet = new ArrayList<SymbolLocationSet>();

                        for (Symbol symbol : symSet) {
                            List<Coordinate> coordSet = symbolToSetOfCoordsMap
                                    .get(symbol);
                            if (symbol == null)
                                continue;
                            SymbolLocationSet sset = new SymbolLocationSet(
                                    symbol, coordSet.toArray(new Coordinate[0]));
                            listOfSymLocSet.add(sset);
                        }
                        mapOfAllSymbolsAtEachPlotPosition.put(position,
                                listOfSymLocSet);
                        mapOfSymbolsPerPlotPosPerFrame.put(dataTime,
                                mapOfAllSymbolsAtEachPlotPosition);
                        sm.release();

                    }

                }

            }

            sm.acquireUninterruptibly();

            /* Consolidate all the DrawableString into a single list */
            try {
                synchronized (positionSet) {
                    for (Position pos : positionSet) {
                        Map<String, DrawableString> mapOfStrPositions = localDMap
                                .get(pos);
                        if (mapOfStrPositions != null
                                && !mapOfStrPositions.isEmpty()) {
                            synchronized (mapOfStrPositions) {
                                synchronized (listOfStringsToDraw) {
                                    listOfStringsToDraw
                                            .addAll(mapOfStrPositions.values());
                                }
                            }
                        }
                    }
                }
            } catch (Exception e) {

            }

            /* Consolidate all wind vectors into a single list */
            if (vectorPosToVectorMap != null && !vectorPosToVectorMap.isEmpty()
                    && vectorPosToVectorMap.size() > 0) {
                synchronized (vectorPosToVectorMap) {
                    synchronized (listOfWindVectors) {
                        Collection<IVector> vColl = vectorPosToVectorMap
                                .values();
                        synchronized (vColl) {
                            try {
                                listOfWindVectors = new ArrayList<IVector>(
                                        vColl);
                            } catch (Exception e) {

                            }
                        }
                    }
                }
            }

            synchronized (symLocSetList) {
                for (Position pos : positionSet) {
                    if (mapOfAllSymbolsAtEachPlotPosition == null
                            || mapOfAllSymbolsAtEachPlotPosition.get(pos) == null)
                        continue;
                    synchronized (mapOfAllSymbolsAtEachPlotPosition) {
                        List<SymbolLocationSet> symbolLocSetList = new ArrayList<SymbolLocationSet>(
                                mapOfAllSymbolsAtEachPlotPosition.get(pos));
                        if (!symbolLocSetList.isEmpty())
                            symLocSetList.addAll(symbolLocSetList);
                    }
                }

            }

            if (prevStationMap != null) { /*
                                           * To update the map of stations last
                                           * processed
                                           */

                synchronized (prevStationMap) {
                    /* Remove the stations not currently disclosed */
                    if (setOfKeysfOfStationsNotDisclosed != null
                            && !setOfKeysfOfStationsNotDisclosed.isEmpty()) {
                        for (String str : setOfKeysfOfStationsNotDisclosed) {
                            prevStationMap.remove(str);
                        }
                    }

                    Set<String> setOfCurrentStnKeys = stationMap.keySet();

                    /* Add the rest of the disclosed stations */
                    for (String eachStnKey : setOfCurrentStnKeys) {
                        prevStationMap.put(eachStnKey,
                                stationMap.get(eachStnKey));
                    }
                }
            }

            /* Update the map of stations processed for the current frame */
            if (mapOfStnsToDataTime != null && prevStationMap != null) {

                // System.out.println("Ending with this list of stations in prevStationMap for the frame time: "
                // + dataTime.toString() );
                // Collection<Station> stnColl = prevStationMap.values();
                // for( Station stn : stnColl ){
                // System.out.print(stn.info.stationId + ",");
                // }

                mapOfStnsToDataTime.put(dataTime, prevStationMap);
            }

            /* Update the map of text entries for the current frame */
            if (dataTimeToText != null && localDMap != null)
                dataTimeToText.put(dataTime, localDMap);

            /*
             * Update the map of wind-barbs with the latest list of wind barbs
             * for this frame
             */
            if (mapOfWindBarbsPerFrame != null && vectorPosToVectorMap != null) {
                Tracer.print("Storing  " + vectorPosToVectorMap.size()
                        + " barbs for frame at: "
                        + Tracer.shortTimeString(dataTime));
                mapOfWindBarbsPerFrame.put(dataTime, vectorPosToVectorMap);
            }

            sm.release();
            // System.out.println("\nFinished creating renderable data for frame: "
            // + dataTime.toString());
            /* Inform the resource that the data is ready for rendering */
            iPointInfoRenderingListener.renderingComplete(dataTime,
                    stationMap.values(), listOfStringsToDraw,
                    listOfWindVectors, symLocSetList);
            Tracer.print("< Exit");
        }

        /**
         * Creates and formats the string to be plotted at the input plot
         * position for the input station
         * 
         * @param station
         * @param plotParamDefn
         * @param font
         * @param textColor
         * @param position
         * @param textBounds
         * @return
         */

        private DrawableString getDrawableStringForStation(Station station,
                PlotParameterDefn plotParamDefn, IFont font, RGB textColor,
                Position position, IGraphicsTarget aTarget, PlotModelElement pme) {
            Tracer.printX("> Entry");
            DrawableString drawableString = null;
            String metParamName = plotParamDefn.getMetParamName();
            Semaphore sm = new Semaphore(1);
            synchronized (station.listOfParamsToPlot) {
                sm.acquireUninterruptibly();
                try {

                    if (pme.hasAdvancedSettings())
                        textColor = getConditionalColor(station, pme);

                    for (AbstractMetParameter metParamToPlot : station.listOfParamsToPlot) {
                        if (metParamToPlot.getMetParamName().compareTo(
                                metParamName) == 0) {

                            try {
                                String formattedString = getFormattedValueToPlot(
                                        plotParamDefn, metParamToPlot);
                                if (formattedString == null) {
                                    return null;
                                }

                                drawableString = new DrawableString(
                                        formattedString, textColor);
                                drawableString.horizontalAlignment = HorizontalAlignment.CENTER;
                                drawableString.verticallAlignment = VerticalAlignment.MIDDLE;
                                drawableString.font = font;
                                drawableString.magnification = 1.0;
                                drawableString.textStyle = TextStyle.NORMAL;

                                Rectangle2D bounds = aTarget
                                        .getStringsBounds(new DrawableString(
                                                "'"
                                                        + drawableString
                                                                .getText()[0]
                                                        + "y", drawableString
                                                        .getColors()[0]));
                                Rectangle textBounds = new Rectangle(0, 0,
                                        (int) bounds.getWidth(),
                                        (int) bounds.getHeight());
                                double[] pixLoc = getUpdatedCoordinates(
                                        textBounds, station.pixelLocation.x,
                                        station.pixelLocation.y, lastView,
                                        canvasBounds, position);
                                if (pixLoc != null)
                                    drawableString.setCoordinates(pixLoc[0],
                                            pixLoc[1]);
                                else
                                    drawableString.setCoordinates(
                                            station.pixelLocation.x,
                                            station.pixelLocation.y);

                                return drawableString;

                            } catch (VizException e) {
                                e.printStackTrace();
                                break;
                            }
                        }
                    }
                } catch (Exception e) {
                    sm.release();
                    return null;
                }
                sm.release();
            }
            Tracer.printX("< Exit");

            return drawableString;
        }

    }

    private String getFormattedValueToPlot(PlotParameterDefn plotParamDefn,
            AbstractMetParameter metPrm) throws VizException {

        Tracer.printX("> Entry");
        String formattedStringToPlot = null;

        if (metPrm == null) {
            return null;
        }

        if (!metPrm.hasValidValue()) {
            return formattedStringToPlot;
        } else {
            String plotUnit = plotParamDefn.getPlotUnit();
            if (plotUnit == null) {
                if (metPrm.hasStringValue()) {
                    if (metPrm.getStringValue() != null
                            && !metPrm.getStringValue().isEmpty())
                        formattedStringToPlot = new String(
                                metPrm.getStringValue());
                }
            } else {

                try {
                    Unit<?> newUnit = new UnitAdapter().unmarshal(plotUnit);

                    if (newUnit.isCompatible(metPrm.getUnit())
                            || newUnit.equals(metPrm.getUnit())) {

                        metPrm.setValue(metPrm.getValueAs(newUnit), newUnit);

                        if (!metPrm.hasValidValue())
                            return null;

                        String formattedStr = new String(metPrm
                                .getFormattedString(
                                        plotParamDefn.getPlotFormat()).trim());
                        String plotTrim = plotParamDefn.getPlotTrim();
                        if (plotTrim != null && !plotTrim.isEmpty()) {
                            int plotTrimVal = Integer.parseInt(plotTrim);
                            formattedStringToPlot = new String(
                                    formattedStr.substring(plotTrimVal));
                        } else
                            formattedStringToPlot = new String(formattedStr);

                    }

                } catch (Exception e) {
                    e.printStackTrace();
                    return plotUnit;
                }

            }

        }
        Tracer.printX("< Exit");

        return formattedStringToPlot;
    }

    public synchronized double[] getUpdatedCoordinates(Rectangle textBounds,
            double xPos, double yPos, IView view, Rectangle canvasSize,
            Position position) {

        Tracer.printX("> Entry");
        double charWidth = textBounds.width;
        double charHeight = textBounds.height;

        double xScale = (view.getExtent().getWidth() / canvasSize.width);
        double yScale = (view.getExtent().getHeight() / canvasSize.height);
        double adjustedWidth = charWidth * 0.625 * xScale;
        double adjustedHeight = charHeight * 0.625 * yScale;

        double canvasX1 = 0.0;
        double canvasY1 = 0.0;

        switch (position) {

        case ML:
            canvasX1 = xPos - adjustedWidth;
            canvasY1 = yPos;
            break;

        case MC:
            canvasX1 = xPos;
            canvasY1 = yPos;
            break;

        case MR:
            canvasX1 = xPos + adjustedWidth;
            canvasY1 = yPos;
            break;

        case UL:
            canvasX1 = xPos - adjustedWidth;
            canvasY1 = yPos - adjustedHeight;
            break;

        case UC:
            canvasX1 = xPos;
            canvasY1 = yPos - adjustedHeight;
            break;

        case UR:
            canvasX1 = xPos + adjustedWidth;
            canvasY1 = yPos - adjustedHeight;
            break;

        case LL:
            canvasX1 = xPos - adjustedWidth;
            canvasY1 = yPos + adjustedHeight;
            break;

        case LC:
            canvasX1 = xPos;
            canvasY1 = yPos + adjustedHeight;
            break;

        case LR:
            canvasX1 = xPos + adjustedWidth;
            canvasY1 = yPos + adjustedHeight;
            break;

        case BC:

            canvasX1 = xPos;
            canvasY1 = yPos + 2 * adjustedHeight;
            break;

        case TC:
            canvasX1 = xPos;
            canvasY1 = yPos - 2 * adjustedHeight;
            break;

        case SC:
            canvasX1 = xPos;
            canvasY1 = yPos;
            break;

        case WD:
            canvasX1 = xPos;
            canvasY1 = yPos;
            break;

        default:
            break;
        }
        Tracer.printX("< Exit");

        return new double[] { canvasX1, canvasY1 };
    }

    private RGB getConditionalColor(Station currentStation, PlotModelElement pme) {
        PlotParameterDefn condPlotParamDefn = PlotParameterDefnsMngr
                .getInstance().getPlotParamDefns(plotModel.getPlugin())
                .getPlotParamDefn(pme.getConditionalParameter());
        Tracer.print("> Entry");
        Semaphore semaphore = new Semaphore(1);
        semaphore.acquireUninterruptibly();

        RGB rgb = null;
        if (!currentStation.setOfConditionalColorParams.isEmpty()) {
            synchronized (currentStation.setOfConditionalColorParams) {
                for (AbstractMetParameter condColorMetPrm : currentStation.setOfConditionalColorParams) {

                    if (condColorMetPrm.getMetParamName().compareTo(
                            condPlotParamDefn.getMetParamName()) == 0) {
                        String condParamValue = getConditionalParameterValue(
                                condPlotParamDefn, condColorMetPrm);
                        if (condParamValue != null
                                && !condColorMetPrm.hasStringValue()) {
                            float value = Float.parseFloat(condParamValue);
                            if (pme.getConditionalColorBar() != null) {
                                rgb = pme.getConditionalColorBar()
                                        .getRGBForInterval(value);
                                break;
                            }
                        }
                    }
                }
            }
        }

        semaphore.release();
        Tracer.print("< Exit");
        return rgb;
    }

    private String getConditionalParameterValue(
            PlotParameterDefn plotParamDefn, AbstractMetParameter metPrm) {
        try {
            return getFormattedValueToPlot(plotParamDefn, metPrm);
        } catch (VizException e) {
            e.printStackTrace();
            return null;
        }
    }

    public synchronized Map<Position, PlotModelElement> getPlotModelPositionMap() {
        return plotModelPositionMap;
    }

    public void updateMapOfBarbs(Map<DataTime, List<IVector>> mapOfBarbs) {
        Set<DataTime> dtSet = mapOfBarbs.keySet();
        for (DataTime dt : dtSet) {
            mapOfWindBarbsPerFrame.get(dt);
        }
    }

    public void removeObsoletePlotEntriesAtThisPositionForAllFrames(Position p) {
        Tracer.print("> Entry" + "  Position " + p);
        Semaphore ss1 = new Semaphore(1);
        ss1.acquireUninterruptibly();
        if (dataTimeToText != null && !dataTimeToText.isEmpty()) {
            Set<DataTime> dtSet = dataTimeToText.keySet();
            synchronized (dtSet) {
                for (DataTime dt : dtSet) {
                    Map<Position, Map<String, DrawableString>> mapOfStrPos = dataTimeToText
                            .get(dt);
                    if ((mapOfStrPos != null && !mapOfStrPos.isEmpty() && mapOfStrPos
                            .containsKey(p))
                            && (p != Position.SC)
                            && (p != Position.WD)) {
                        mapOfStrPos.remove(p);
                        dataTimeToText.put(dt, mapOfStrPos);
                    }
                }
            }
        }
        ss1.release();

        ss1.acquireUninterruptibly();
        if (mapOfWindBarbsPerFrame != null && !mapOfWindBarbsPerFrame.isEmpty()
                && (p == Position.WD)) {
            mapOfWindBarbsPerFrame.clear();
        }
        if ((mapOfSymbolsPerPlotPosPerFrame != null)
                && (!mapOfSymbolsPerPlotPosPerFrame.isEmpty())) {
            Set<DataTime> dtSet = mapOfSymbolsPerPlotPosPerFrame.keySet();
            synchronized (dtSet) {
                for (DataTime dt : dtSet) {
                    Map<Position, List<SymbolLocationSet>> symbolPosMap = mapOfSymbolsPerPlotPosPerFrame
                            .get(dt);
                    symbolPosMap.remove(p);
                    mapOfSymbolsPerPlotPosPerFrame.put(dt, symbolPosMap);
                }
            }
        }
        ss1.release();
        Tracer.print("< Exit");

    }

    public void dispose() {
        Tracer.print("> Entry");
        // timeLogger.append("\n Invoking NcPlotImageCreator.dispose()\n");

        if (imageCreationJobPool.isActive()) {
            imageCreationJobPool.cancel();
        }
        imageCreationJobPool = null;

        if (queueOfStations != null) {
            queueOfStations.clear();
            queueOfStations = null;
        }

        if (dataTimeToText != null && !dataTimeToText.isEmpty()) {
            dataTimeToText.clear();
            dataTimeToText = null;
        }

        if (mapOfStnsToDataTime != null && !mapOfStnsToDataTime.isEmpty()) {
            mapOfStnsToDataTime.clear();
            mapOfStnsToDataTime = null;
        }

        if (mapOfSymbolsPerPlotPosPerFrame != null
                && !mapOfSymbolsPerPlotPosPerFrame.isEmpty()) {
            mapOfSymbolsPerPlotPosPerFrame.clear();
            mapOfSymbolsPerPlotPosPerFrame = null;
        }

        if (mapOfWindBarbsPerFrame != null && !mapOfWindBarbsPerFrame.isEmpty()) {
            mapOfWindBarbsPerFrame.clear();
            mapOfWindBarbsPerFrame = null;
        }

        if (symbolNameToSymbolMap != null && !symbolNameToSymbolMap.isEmpty()) {
            symbolNameToSymbolMap.clear();
            symbolNameToSymbolMap = null;
        }

        if (symbolLookupTable != null && !symbolLookupTable.isEmpty()) {
            symbolLookupTable.clear();
            symbolLookupTable = null;
        }

        if (plotPosToSymbolSizeMap != null && !plotPosToSymbolSizeMap.isEmpty()) {
            plotPosToSymbolSizeMap.clear();
            plotPosToSymbolSizeMap = null;
        }

        if (posToSymbolTypeMap != null && !posToSymbolTypeMap.isEmpty()) {
            posToSymbolTypeMap.clear();
            posToSymbolTypeMap = null;
        }

        if (plotPosToFontMap != null && !plotPosToFontMap.isEmpty()) {
            plotPosToFontMap.clear();
            plotPosToFontMap = null;
        }

        if (plotModelPositionMap != null && !plotModelPositionMap.isEmpty()) {
            plotModelPositionMap.clear();
            plotModelPositionMap = null;
        }

        if (COURIER_BOLD_FONT != null) {
            COURIER_BOLD_FONT.dispose();
            COURIER_BOLD_FONT = null;
        }

        if (COURIER_BOLD_ITALIC_FONT != null) {
            COURIER_BOLD_ITALIC_FONT.dispose();
            COURIER_BOLD_ITALIC_FONT = null;
        }

        if (COURIER_ITALIC_FONT != null) {
            COURIER_ITALIC_FONT.dispose();
            COURIER_ITALIC_FONT = null;
        }

        if (COURIER_NORMAL_FONT != null) {
            COURIER_NORMAL_FONT.dispose();
            COURIER_NORMAL_FONT = null;
        }

        if (SERIF_BOLD_FONT != null) {
            SERIF_BOLD_FONT.dispose();
            SERIF_BOLD_FONT = null;
        }

        if (SERIF_BOLD_ITALIC_FONT != null) {
            SERIF_BOLD_ITALIC_FONT.dispose();
            SERIF_BOLD_ITALIC_FONT = null;
        }

        if (SERIF_ITALIC_FONT != null) {
            SERIF_ITALIC_FONT.dispose();
            SERIF_ITALIC_FONT = null;
        }

        if (SERIF_NORMAL_FONT != null) {
            SERIF_NORMAL_FONT.dispose();
            SERIF_NORMAL_FONT = null;
        }

        if (SANS_SERIF_BOLD_FONT != null) {
            SANS_SERIF_BOLD_FONT.dispose();
            SANS_SERIF_BOLD_FONT = null;
        }

        if (SANS_SERIF_BOLD_ITALIC_FONT != null) {
            SANS_SERIF_BOLD_ITALIC_FONT.dispose();
            SANS_SERIF_BOLD_ITALIC_FONT = null;
        }

        if (SANS_SERIF_ITALIC_FONT != null) {
            SANS_SERIF_ITALIC_FONT.dispose();
            SANS_SERIF_ITALIC_FONT = null;
        }

        if (SANS_SERIF_NORMAL_FONT != null) {
            SANS_SERIF_NORMAL_FONT.dispose();
            SANS_SERIF_NORMAL_FONT = null;
        }
        Tracer.print("< Exit");

    }

    public boolean areThereSymbolsToBeRenderedInTheCurrentFrame(DataTime dt) {
        return ((posToSymbolTypeMap != null && !posToSymbolTypeMap.isEmpty()) && (mapOfSymbolsPerPlotPosPerFrame != null
                && !mapOfSymbolsPerPlotPosPerFrame.isEmpty() && mapOfSymbolsPerPlotPosPerFrame
                    .get(dt) != null));
    }

    public boolean areThereTextElementsToBeRenderedInTheCurrentFrame(DataTime dt) {
        boolean anyTextElementsInPlotModel = false;
        if (plotModelPositionMap != null && !plotModelPositionMap.isEmpty()) {
            Set<Position> posSet = plotModelPositionMap.keySet();

            for (Position p : posSet) {
                PlotModelElement pme = plotModelPositionMap.get(p);
                if (plotParameterDefinitions != null) {
                    String plotMode = plotParameterDefinitions
                            .getPlotParamDefn(pme.getParamName()).getPlotMode();
                    if (plotMode != null && plotMode.compareTo("text") == 0) {
                        anyTextElementsInPlotModel = true;
                        break;
                    }
                }
            }
        }

        return (anyTextElementsInPlotModel && (dataTimeToText != null
                && !dataTimeToText.isEmpty() && dataTimeToText.get(dt) != null));
    }

    public boolean areThereVectorsToBeRenderedInTheCurrentFrame(DataTime dt) {
        return ((plotModelPositionMap != null && plotModelPositionMap
                .containsKey(Position.WD)) && (mapOfWindBarbsPerFrame != null
                && !mapOfWindBarbsPerFrame.isEmpty() && mapOfWindBarbsPerFrame
                    .get(dt) != null));
    }
}
