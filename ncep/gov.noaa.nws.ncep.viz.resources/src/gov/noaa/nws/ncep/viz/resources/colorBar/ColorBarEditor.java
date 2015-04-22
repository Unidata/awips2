package gov.noaa.nws.ncep.viz.resources.colorBar;

import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarAnchorLocation;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarOrientation;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.ui.display.IColorBar;

import java.util.ArrayList;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

/**
 * This widget is used to edit ColorBars for non-imagery resources.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/21/10      #259        Greg Hull    Initial Creation.
 * 09/29/11      #248        Greg Hull    dispose()
 * 07/17/12      #743        Archana      Refactored the packages for 
 *                                        ColorBarAnchorLocation and ColorBarOrientation
 * 07/26/12      #797        Archana      Removed the boolean updatingIntervals,
 *                                        added the methods addModifyListenersForSpinners()
 *                                        and removeModifyListenersForSpinners()
 *                                        Updated the modifyEvent() for both max/min interval spinners to
 *                                        exit if the spinners are set to the MAX/MIN Integer values respectively.
 *                                        
 * 07/01/14  TTR 1018        SRussell     Altered constructor to make editable spinners an option
 * 
 * 
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class ColorBarEditor extends Composite {
    private final int CBAR_XOFF = 15;

    //	private final int CBAR_YOFF = 50;
    private ArrayList<Rectangle> colorBarRects = null;

    //	private ArrayList<Float> intrvlsToLabel = null;
    private int seldIntrvl = 0;

    private int dragXvalue = 0;

    private final Canvas canvas;

    private Font font;

    private Color canvasColor;

    private Color seldColor;

    private Color labelColor;

    private IColorBar colorBar;

    private Point canvasSize = null;

    //	private ArrayList<Unit<?>> availUnits;

    private Float pixPerUnit = 1.0f;

    private ColorBarAnchorLocation[] availAnchorLocs = new ColorBarAnchorLocation[] { ColorBarAnchorLocation.UpperLeft,
            //		ColorBarAnchorLocation.UpperCenter,
            ColorBarAnchorLocation.UpperRight,
            //		ColorBarAnchorLocation.CenterLeft,
            //		ColorBarAnchorLocation.CenterCenter,
            //		ColorBarAnchorLocation.CenterRight,
            ColorBarAnchorLocation.LowerLeft,
            //		ColorBarAnchorLocation.LowerCenter,
            ColorBarAnchorLocation.LowerRight };

    final Combo orientationCombo;

    final Combo anchorCombo;

    final Combo unitsCombo;

    final Spinner lengthSpnr;

    final Spinner widthSpnr;

    final Button showLabelsBtn;

    final Button drawToScaleBtn;

    final Spinner intrvlMinSpnr;

    final Spinner intrvlMaxSpnr;

    final Text pixelNumTxt;

    final Button labelPixelBtn;

    private Button negInfIntrvlBtn;

    private Button posInfIntrvlBtn;

    private Text negInfIntrvlTxt;

    private Text posInfIntrvlTxt;

    private Composite colorComp;

    private Composite labelColorComp;

    final ColorButtonSelector intrvlColorSelector;

    final ColorButtonSelector labelColorSelector; // also used for the border color

    // widgets used for non-image colorbars
    //	
    private Cursor pointerCursor;

    private Cursor dragIntrvlCursor;

    // If the colorbar applies to a color mappable image.
    // In this case the 
    //	private Boolean applyToImage = false; // this

    private Display colorDevice;

    private double scaleMult = 1.0;

    //	private boolean updatingIntervals = false;

    private final Listener mouseLstnr = new Listener() {

        @Override
        public void handleEvent(Event e) {
            canvas.redraw();
            switch (e.type) {
            case SWT.MouseDoubleClick:
                System.out.println("Double Click"); // doesn't work?
                break;
            case SWT.MouseDown:
                if (e.button != 1) {
                    return;
                }

                for (Rectangle rect : colorBarRects) {
                    int intrvlIndx = colorBarRects.indexOf(rect);

                    // if non-image and if drawing to scale then we
                    // first check to see if clicking on the edge of an interval
                    // (give them a little room for error)
                    // (can't drag the last interval)
                    if (colorBar.getDrawToScale()) {

                        if (intrvlIndx != colorBarRects.size() - 1 && Math.abs(e.x - rect.x - rect.width) <= 1) {

                            dragXvalue = rect.x + rect.width;
                            seldIntrvl = colorBarRects.indexOf(rect);
                            //   		 					updateSelectedInterval();
                            break;
                        }
                    }

                    if (rect.contains(e.x, e.y)) {
                        seldIntrvl = colorBarRects.indexOf(rect);
                        updateSelectedInterval();
                    }
                }

                canvas.redraw();

                break;

            case SWT.MouseMove:
                // if not dragging, check if we on an interval and change the cursor
                // 
                if (dragXvalue == 0) {
                    boolean crossingInterval = false;

                    // it only makes sense for the user to be able to drag an interval if
                    // the colorbar is drawn to scale.
                    if (colorBar.getDrawToScale()) {
                        // check to see if crossing the edge of an interval (but not the last interval)
                        for (int i = 0; i < colorBarRects.size() - 1; i++) {
                            Rectangle rect = colorBarRects.get(i);
                            if (e.y >= rect.y - 2 && e.y < rect.y + rect.height + 2) {
                                if (Math.abs(e.x - rect.x - rect.width) <= 1) {
                                    crossingInterval = true;
                                    break;
                                }
                            }
                        }
                    }

                    canvas.setCursor((crossingInterval ? dragIntrvlCursor : pointerCursor));
                } else {
                    dragXvalue = e.x;

                    // if the user drags the max value past the min value or past  
                    // an max value for the next interval then we will remove it and
                    // stop dragging.		        		
                    if (e.x <= colorBarRects.get(seldIntrvl).x) {
                        colorBar.removeInterval(seldIntrvl);
                        dragXvalue = 0;
                    } else { // note that we can't drag the last interval 	
                        Rectangle nextRect = colorBarRects.get(seldIntrvl + 1);

                        if (e.x >= nextRect.x + nextRect.width) {
                            Float newMax = colorBar.getIntervalMax(seldIntrvl + 1);
                            colorBar.removeInterval(seldIntrvl + 1);
                            colorBar.setIntervalMax(seldIntrvl, newMax);
                            dragXvalue = 0;
                        }
                    }

                    computeColorBarRectangles();
                    updateSelectedInterval();
                }

                break;

            case SWT.MouseUp:
                if (e.button != 1) {
                    return;
                }
                // if dragging, then interpolate the new max and update the interval
                if (dragXvalue != 0) {
                    Float newMax = interpolateNewIntervalMax();
                    if (newMax != Float.NaN) {
                        colorBar.setIntervalMax(seldIntrvl, newMax);
                    }

                    computeColorBarRectangles();
                    updateSelectedInterval();

                    dragXvalue = 0;
                }

                canvas.redraw();
                break;
            }
        }
    };

    public ColorBarEditor(Composite parent, IColorBar cbar, boolean editableSpinner) {
        super(parent, SWT.NONE);

        colorBar = cbar;
        colorDevice = parent.getDisplay();

        initialize();

        Composite topForm = this;
        FormData fd = new FormData(600, 275);
        fd.top = new FormAttachment(0, 0);
        fd.left = new FormAttachment(0, 0);
        fd.right = new FormAttachment(100, 0);
        fd.bottom = new FormAttachment(100, 0);
        topForm.setLayoutData(fd);

        topForm.setLayout(new FormLayout());

        orientationCombo = new Combo(topForm, SWT.DROP_DOWN | SWT.READ_ONLY);
        fd = new FormData();
        fd.left = new FormAttachment(0, 90);
        fd.top = new FormAttachment(0, 15);
        orientationCombo.setLayoutData(fd);

        Label orientationLbl = new Label(topForm, SWT.NONE);
        orientationLbl.setText("Orientation");
        fd = new FormData();
        fd.right = new FormAttachment(orientationCombo, -3, SWT.LEFT);
        fd.top = new FormAttachment(orientationCombo, 3, SWT.TOP);
        orientationLbl.setLayoutData(fd);

        anchorCombo = new Combo(topForm, SWT.DROP_DOWN | SWT.READ_ONLY);
        fd = new FormData();
        fd.left = new FormAttachment(orientationCombo, 0, SWT.LEFT);
        fd.top = new FormAttachment(orientationCombo, 10, SWT.BOTTOM);
        anchorCombo.setLayoutData(fd);

        Label anchorLbl = new Label(topForm, SWT.NONE);
        anchorLbl.setText("Anchor");
        fd = new FormData();
        fd.right = new FormAttachment(anchorCombo, -3, SWT.LEFT);
        fd.top = new FormAttachment(anchorCombo, 3, SWT.TOP);
        anchorLbl.setLayoutData(fd);

        lengthSpnr = new Spinner(topForm, SWT.BORDER);
        lengthSpnr.setToolTipText("ColorBar Length as a percentage of the screen size");
        fd = new FormData();
        //        fd.left = new FormAttachment( anchorCombo, 35, SWT.RIGHT );
        fd.right = new FormAttachment(50, 0);
        fd.top = new FormAttachment(orientationCombo, 10, SWT.TOP);
        lengthSpnr.setLayoutData(fd);

        Label lenLbl = new Label(topForm, SWT.NONE);
        lenLbl.setText("Length");
        fd = new FormData();
        fd.left = new FormAttachment(lengthSpnr, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(lengthSpnr, -3, SWT.TOP);
        lenLbl.setLayoutData(fd);

        Label lenUnitsLbl = new Label(topForm, SWT.NONE);
        lenUnitsLbl.setText("%");
        fd = new FormData();
        fd.left = new FormAttachment(lengthSpnr, 2, SWT.RIGHT);
        fd.top = new FormAttachment(lengthSpnr, 2, SWT.TOP);
        lenUnitsLbl.setLayoutData(fd);

        widthSpnr = new Spinner(topForm, SWT.BORDER);
        //        widthSpnr.setToolTipText("ColorBar Width in pixels");
        fd = new FormData();
        fd.left = new FormAttachment(lenUnitsLbl, 25, SWT.RIGHT);
        fd.top = new FormAttachment(lengthSpnr, 0, SWT.TOP);
        widthSpnr.setLayoutData(fd);

        Label widLbl = new Label(topForm, SWT.NONE);
        widLbl.setText("Width");
        fd = new FormData();
        fd.left = new FormAttachment(widthSpnr, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(widthSpnr, -3, SWT.TOP);
        widLbl.setLayoutData(fd);

        Label widUnitsLbl = new Label(topForm, SWT.NONE);
        widUnitsLbl.setText("pixels");
        fd = new FormData();
        fd.left = new FormAttachment(widthSpnr, 2, SWT.RIGHT);
        fd.top = new FormAttachment(widthSpnr, 2, SWT.TOP);
        widUnitsLbl.setLayoutData(fd);

        drawToScaleBtn = new Button(topForm, SWT.CHECK);
        drawToScaleBtn.setText("Draw to Scale");
        fd = new FormData();
        fd.left = new FormAttachment(lengthSpnr, 0, SWT.LEFT);
        fd.top = new FormAttachment(lengthSpnr, 10, SWT.BOTTOM);
        drawToScaleBtn.setLayoutData(fd);

        showLabelsBtn = new Button(topForm, SWT.CHECK);
        showLabelsBtn.setText("Show Labels");
        fd = new FormData();
        fd.left = new FormAttachment(77, 0);
        //        fd.left = new FormAttachment( widUnitsLbl, 25, SWT.RIGHT );
        fd.top = new FormAttachment(widLbl, 5, SWT.TOP);
        //        fd.right = new FormAttachment( 100, -15 );
        showLabelsBtn.setLayoutData(fd);

        labelColorComp = new Composite(topForm, SWT.None);
        fd = new FormData();
        fd.top = new FormAttachment(showLabelsBtn, 25);
        //        fd.left = new FormAttachment( showLabelsBtn, 20, SWT.LEFT );
        fd.right = new FormAttachment(showLabelsBtn, 0, SWT.RIGHT);
        labelColorComp.setLayoutData(fd);

        GridLayout gl = new GridLayout();
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        labelColorComp.setLayout(gl);

        labelColorSelector = new ColorButtonSelector(labelColorComp, 50, 25);

        Label lblColLbl = new Label(topForm, SWT.NONE);
        lblColLbl.setText("Label Color");
        fd = new FormData();
        fd.right = new FormAttachment(labelColorComp, -3, SWT.LEFT);
        fd.top = new FormAttachment(labelColorComp, 3, SWT.TOP);
        //        fd.left = new FormAttachment( labelColorComp, 0, SWT.LEFT );
        //        fd.bottom = new FormAttachment( labelColorComp, -2, SWT.TOP );
        lblColLbl.setLayoutData(fd);

        unitsCombo = new Combo(topForm, SWT.DROP_DOWN | SWT.READ_ONLY);
        fd = new FormData();
        fd.left = new FormAttachment(anchorCombo, 25, SWT.RIGHT);
        fd.top = new FormAttachment(anchorCombo, 0, SWT.TOP);
        unitsCombo.setLayoutData(fd);

        Label unitsLbl = new Label(topForm, SWT.NONE);
        unitsLbl.setText("Units");
        fd = new FormData();
        fd.left = new FormAttachment(unitsCombo, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(unitsCombo, -3, SWT.TOP);
        unitsLbl.setLayoutData(fd);

        unitsLbl.setVisible(false);
        unitsCombo.setVisible(false);

        canvas = new Canvas(topForm, SWT.BORDER);

        fd = new FormData();
        fd.height = 100;
        fd.top = new FormAttachment(anchorCombo, 25, SWT.BOTTOM);
        fd.left = new FormAttachment(0, 20);
        fd.right = new FormAttachment(100, -20);

        canvas.setLayoutData(fd);

        colorComp = new Composite(topForm, SWT.None);
        fd = new FormData();
        fd.top = new FormAttachment(canvas, 25, SWT.BOTTOM);
        fd.left = new FormAttachment(50, -30);
        fd.bottom = new FormAttachment(100, -20);
        colorComp.setLayoutData(fd);
        colorComp.setLayout(gl);

        intrvlColorSelector = new ColorButtonSelector(colorComp, 50, 25);

        // create rest of the widgets based on whether this is for Images
        // (Some widgets need to be 'final' since they are access by listers
        // but java complains if they are not set in the constructor so I'm 
        // creating them here. Don't like it but don't know a prefered way around it.)
        //		if( applyToImage ) {
        //			pixelNumTxt = new Text( topForm, SWT.BORDER | SWT.READ_ONLY );
        //			labelPixelBtn = new Button( topForm, SWT.CHECK );
        //			createImageCbarWidgets( );
        //
        //			intrvlMinSpnr = null;
        //			intrvlMaxSpnr = null;
        //			negInfIntrvlBtn = null;
        //			posInfIntrvlBtn = null;
        //			negInfIntrvlTxt = null;
        //			posInfIntrvlTxt = null;
        //			
        //			intrvlColorSelector.getButton().setEnabled(false);
        //		}
        //		else {
        //labelColorComp.setVisible( false );
        labelPixelBtn = null;
        pixelNumTxt = null;

        // TTR 1018
        if (editableSpinner) {
            intrvlMinSpnr = new Spinner(topForm, SWT.BORDER);
            intrvlMaxSpnr = new Spinner(topForm, SWT.BORDER);
        } else {
            intrvlMinSpnr = new Spinner(topForm, SWT.BORDER | SWT.READ_ONLY);
            intrvlMaxSpnr = new Spinner(topForm, SWT.BORDER | SWT.READ_ONLY);
        }

        negInfIntrvlBtn = new Button(topForm, SWT.CHECK);
        posInfIntrvlBtn = new Button(topForm, SWT.CHECK);
        negInfIntrvlTxt = new Text(topForm, SWT.BORDER | SWT.READ_ONLY);
        posInfIntrvlTxt = new Text(topForm, SWT.BORDER | SWT.READ_ONLY);

        createNonImageCbarWidgets();
        //		}

        initWidgets();

        computeColorBarRectangles();

        updateSelectedInterval();
    }

    //	private void createImageCbarWidgets() {
    //        Composite topForm = this;
    //    	FormData fd;
    //    	Button prevIntrvl = new Button( topForm, SWT.PUSH );
    //    	fd = new FormData();
    //    	prevIntrvl.setText("   <   ");
    //		fd.top = new FormAttachment( colorComp, 0, SWT.TOP );
    //		fd.right = new FormAttachment( colorComp, -30, SWT.LEFT );
    //		prevIntrvl.setLayoutData( fd );
    //    	
    //		Button nextIntrvl = new Button( topForm, SWT.PUSH );
    //    	fd = new FormData();
    //    	nextIntrvl.setText("   >   ");
    //		fd.top = new FormAttachment( colorComp, 0, SWT.TOP );
    //		fd.left = new FormAttachment( colorComp, 30, SWT.RIGHT );
    //		nextIntrvl.setLayoutData( fd );
    //    		
    //    	fd = new FormData();
    //		fd.top = new FormAttachment( prevIntrvl, 0, SWT.TOP );
    //		fd.right = new FormAttachment( prevIntrvl, -30, SWT.LEFT );
    //		fd.width = 30;
    //		pixelNumTxt.setLayoutData( fd );
    //		pixelNumTxt.setBackground( getParent().getBackground() );
    //
    //		Label pixNumLbl = new Label( topForm, SWT.None );
    //		pixNumLbl.setText("Pixel");
    //    	fd = new FormData();
    //		fd.bottom = new FormAttachment( pixelNumTxt, 0, SWT.TOP );
    //		fd.left = new FormAttachment( pixelNumTxt, 0, SWT.LEFT );
    //		pixNumLbl.setLayoutData( fd );
    //
    //		labelPixelBtn.setText("Label Pixel");
    //    	fd = new FormData();
    //		fd.top = new FormAttachment( nextIntrvl, 2, SWT.TOP );
    //		fd.left = new FormAttachment( nextIntrvl, 30, SWT.RIGHT );
    //		labelPixelBtn.setLayoutData( fd );
    //
    //		prevIntrvl.addSelectionListener( new SelectionAdapter() {
    //			@Override
    //			public void widgetSelected(SelectionEvent e) {
    //				if( seldIntrvl > 0 ) {
    //					seldIntrvl--;
    //					computeColorBarRectangles();
    //					updateSelectedInterval();
    //				}
    //			}
    //		});
    //
    //		nextIntrvl.addSelectionListener( new SelectionAdapter() {
    //			@Override
    //			public void widgetSelected(SelectionEvent e) {
    //				if( seldIntrvl < colorBar.getNumIntervals()-1 ) {
    //					seldIntrvl++;
    //					computeColorBarRectangles();
    //					updateSelectedInterval();
    //				}
    //			}
    //		});
    //		
    //		labelPixelBtn.addSelectionListener( new SelectionAdapter() {
    //			@Override
    //			public void widgetSelected(SelectionEvent e) {
    //				if( labelPixelBtn.getSelection() ) {
    //					colorBar.labelInterval(seldIntrvl, Integer.toString(seldIntrvl));
    //				}
    //				else {
    //					colorBar.labelInterval(seldIntrvl, null);
    //				}
    //				canvas.redraw();
    //			}
    //		});
    //	}

    private void createNonImageCbarWidgets() {
        Composite topForm = this;
        FormData fd;

        fd = new FormData();
        fd.width = 40;
        fd.top = new FormAttachment(colorComp, 0, SWT.TOP);
        fd.right = new FormAttachment(colorComp, -25, SWT.LEFT);
        intrvlMinSpnr.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(intrvlMinSpnr, 0, SWT.TOP);
        fd.left = new FormAttachment(intrvlMinSpnr, 0, SWT.LEFT);
        fd.right = new FormAttachment(intrvlMinSpnr, 0, SWT.RIGHT);
        fd.bottom = new FormAttachment(intrvlMinSpnr, 0, SWT.BOTTOM);

        negInfIntrvlTxt.setLayoutData(fd);
        negInfIntrvlTxt.setVisible(false);
        negInfIntrvlTxt.setBackground(negInfIntrvlTxt.getParent().getBackground());
        negInfIntrvlTxt.setText("-Inf");

        Label intrvlMinLbl = new Label(topForm, SWT.NONE);
        intrvlMinLbl.setText("Minimum");
        fd = new FormData();
        fd.left = new FormAttachment(intrvlMinSpnr, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(intrvlMinSpnr, -2, SWT.TOP);
        intrvlMinLbl.setLayoutData(fd);

        fd = new FormData();
        fd.width = 40;
        fd.top = new FormAttachment(colorComp, 0, SWT.TOP);
        fd.left = new FormAttachment(colorComp, 20, SWT.RIGHT);
        intrvlMaxSpnr.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(intrvlMaxSpnr, 0, SWT.TOP);
        fd.left = new FormAttachment(intrvlMaxSpnr, 0, SWT.LEFT);
        fd.right = new FormAttachment(intrvlMaxSpnr, 0, SWT.RIGHT);
        fd.bottom = new FormAttachment(intrvlMaxSpnr, 0, SWT.BOTTOM);

        posInfIntrvlTxt.setLayoutData(fd);
        posInfIntrvlTxt.setVisible(false);
        posInfIntrvlTxt.setBackground(posInfIntrvlTxt.getParent().getBackground());
        posInfIntrvlTxt.setText("Inf");
        posInfIntrvlTxt.setBounds(intrvlMaxSpnr.getBounds());

        Label intrvlMaxLbl = new Label(topForm, SWT.NONE);
        intrvlMaxLbl.setText("Maximum");
        fd = new FormData();
        fd.left = new FormAttachment(intrvlMaxSpnr, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(intrvlMaxSpnr, -2, SWT.TOP);
        intrvlMaxLbl.setLayoutData(fd);

        Button addIntrvlBtn = new Button(topForm, SWT.PUSH);
        fd = new FormData();
        addIntrvlBtn.setText("  Insert  ");
        fd.top = new FormAttachment(colorComp, 0, SWT.TOP);
        fd.right = new FormAttachment(intrvlMinSpnr, -20, SWT.LEFT);
        addIntrvlBtn.setLayoutData(fd);

        Button removeIntrvlBtn = new Button(topForm, SWT.PUSH);
        fd = new FormData();
        removeIntrvlBtn.setText(" Remove ");
        fd.top = new FormAttachment(colorComp, 0, SWT.TOP);
        fd.left = new FormAttachment(intrvlMaxSpnr, 20, SWT.RIGHT);
        removeIntrvlBtn.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(addIntrvlBtn, 0, SWT.TOP);
        fd.right = new FormAttachment(addIntrvlBtn, -20, SWT.LEFT);
        negInfIntrvlBtn.setLayoutData(fd);

        negInfIntrvlBtn.setText("-Inf");
        negInfIntrvlBtn.setVisible(false);

        negInfIntrvlBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (negInfIntrvlBtn.getSelection()) {
                    if (seldIntrvl == 0) { // should be 0 if this is visible 						
                        colorBar.addColorBarInterval(Float.NEGATIVE_INFINITY, colorBar.getIntervalMin(0), new RGB(100, 100, 100));
                    }
                } else { // 
                    if (seldIntrvl == 0) {
                        // This will remove the first interval but will keep the Inf minimum so
                        // we will remove it an replace the min with the min of the next interval
                        Float saveMin = colorBar.getIntervalMin(1);
                        colorBar.removeInterval(0);
                        colorBar.setIntervalMin(0, saveMin);
                    }
                }

                computeColorBarRectangles();
                updateSelectedInterval();
            }
        });

        fd = new FormData();
        fd.top = new FormAttachment(removeIntrvlBtn, 0, SWT.TOP);
        fd.left = new FormAttachment(removeIntrvlBtn, 20, SWT.RIGHT);
        posInfIntrvlBtn.setLayoutData(fd);

        posInfIntrvlBtn.setText("Inf");
        posInfIntrvlBtn.setVisible(false);

        posInfIntrvlBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int lastInt = colorBar.getNumIntervals() - 1;

                if (posInfIntrvlBtn.getSelection()) {
                    if (seldIntrvl == lastInt) { // sanity check 
                        colorBar.addColorBarInterval(colorBar.getIntervalMax(lastInt), Float.POSITIVE_INFINITY, new RGB(100, 100, 100));
                        seldIntrvl++;
                    }
                } else { // no infinite interval
                    if (seldIntrvl == lastInt) {
                        // This will remove the first interval but will keep the Inf minimum so
                        // we will remove it an replace the min with the min of the next interval
                        Float saveMax = colorBar.getIntervalMax(seldIntrvl - 1);
                        colorBar.removeInterval(lastInt);
                        seldIntrvl--;
                        colorBar.setIntervalMax(seldIntrvl, saveMax);
                    }
                }

                computeColorBarRectangles();
                updateSelectedInterval();
            }
        });

        removeIntrvlBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (colorBar.getNumIntervals() == 1) {
                    // popup a msg dialog
                    System.out.println("Can't remove last interval");
                    return;
                }

                colorBar.removeInterval(seldIntrvl);

                if (seldIntrvl >= colorBar.getNumIntervals()) {
                    seldIntrvl = colorBar.getNumIntervals() - 1;
                }

                computeColorBarRectangles();
                updateSelectedInterval();
            }
        });

        addIntrvlBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                colorBar.createNewInterval(seldIntrvl);
                computeColorBarRectangles();
                updateSelectedInterval();
            }
        });

        intrvlMinSpnr.addModifyListener(minSpnrListener);

        intrvlMaxSpnr.addModifyListener(maxSpnrListener);

        removeModifyListenersForSpinners();
        intrvlMinSpnr.setDigits(colorBar.getNumDecimals());
        intrvlMinSpnr.setIncrement(1);

        intrvlMaxSpnr.setDigits(colorBar.getNumDecimals());
        intrvlMaxSpnr.setIncrement(1);

        addModifyListenersForSpinners();

        //		intrvlMinSpnr.addSelectionListener( new SelectionListener() {
        //			@Override
        //			public void widgetDefaultSelected(SelectionEvent e) {
        //				Float minVal=Float.NEGATIVE_INFINITY;
        //				String txtStr = intrvlMinSpnr.getText().trim();
        //				
        //				if( txtStr.toLowerCase().startsWith("-inf") ) {
        //					minVal = Float.NEGATIVE_INFINITY;
        //				}
        //				else {
        //					try {
        //						minVal = Float.parseFloat( intrvlMinSpnr.getText().trim() );
        //					}
        //					catch( NumberFormatException nfe ) {
        //						intrvlMinSpnr.setSelection((int)(minVal*scaleMult));
        //						return;
        //					}
        //				}
        //				
        //				setIntervalMin( minVal, true );				
        //			}
        //
        //			@Override
        //			public void widgetSelected(SelectionEvent e) {
        //				// not called for Text widgets
        //			}
        //        });
        //
        //		intrvlMaxSpnr.addSelectionListener( new SelectionListener() {
        //			@Override
        //			public void widgetDefaultSelected(SelectionEvent e) {
        //				String txtStr = intrvlMaxSpnr.getText().trim();
        //				
        //				try {
        //					Float maxVal = Float.parseFloat( intrvlMaxSpnr.getText().trim() );
        //					setIntervalMax( maxVal, true );
        //				}
        //				catch( NumberFormatException nfe ) {
        //					return;
        //				}
        //				
        //				computeColorBarRectangles();
        //			//	updateSelectedInterval();
        //			}
        //
        //			@Override
        //			public void widgetSelected(SelectionEvent e) {
        //			}
        //        });
    }

    private void initialize() {

        int numDecimals = colorBar.getNumDecimals();
        scaleMult = Math.pow(10, numDecimals);

        // if there is a colorMap then this is for images and this widget will
        // be taylored to image colorBars.
        //		applyToImage = (colorBar instanceof ColorBarFromColormap == true);

        colorBar.setColorDevice(colorDevice);

        pointerCursor = new Cursor(colorDevice, SWT.CURSOR_ARROW);
        dragIntrvlCursor = new Cursor(colorDevice, SWT.CURSOR_SIZEWE);

        labelColor = new Color(colorDevice, colorBar.getLabelColor());
        canvasColor = new Color(colorDevice, 0, 0, 0);
        seldColor = new Color(colorDevice, 255, 255, 255); // white

        font = new Font(colorDevice, "Times", 10, SWT.BOLD);
    }

    // create listeners and init values and selections
    //
    private void initWidgets() {
        seldIntrvl = 0;

        canvasSize = canvas.getSize();

        orientationCombo.add(ColorBarOrientation.Vertical.name());
        orientationCombo.add(ColorBarOrientation.Horizontal.name());

        orientationCombo.select(colorBar.getOrientation() == ColorBarOrientation.Vertical ? 0 : 1);

        orientationCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                colorBar.setOrientation((orientationCombo.getSelectionIndex() == 0 ? ColorBarOrientation.Vertical : ColorBarOrientation.Horizontal));
            }
        });

        for (ColorBarAnchorLocation anchorLoc : availAnchorLocs) {
            anchorCombo.add(anchorLoc.name());
        }

        for (int a = 0; a < availAnchorLocs.length; a++) {
            if (colorBar.getAnchorLoc() == availAnchorLocs[a]) {
                anchorCombo.select(a);
            }
        }

        anchorCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                colorBar.setAnchorLoc(availAnchorLocs[anchorCombo.getSelectionIndex()]);
            }
        });

        unitsCombo.add("N/A");

        unitsCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });

        unitsCombo.setVisible(false);

        lengthSpnr.setSelection((int) (colorBar.getLengthAsRatio() * 100));

        lengthSpnr.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                colorBar.setLengthAsRatio(((float) lengthSpnr.getSelection()) / 100f);
                computeColorBarRectangles();
            }
        });

        lengthSpnr.setMinimum(10);
        lengthSpnr.setMaximum(100);
        lengthSpnr.setIncrement(5);

        widthSpnr.setSelection(colorBar.getWidthInPixels());

        widthSpnr.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                colorBar.setWidthInPixels(widthSpnr.getSelection());
                computeColorBarRectangles();
            }
        });

        widthSpnr.setMinimum(2);
        widthSpnr.setMaximum(50);
        widthSpnr.setIncrement(1);

        showLabelsBtn.setSelection(colorBar.getShowLabels());

        showLabelsBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                colorBar.setShowLabels(showLabelsBtn.getSelection());
                computeColorBarRectangles();
            }
        });

        //        if( applyToImage ) {
        //        	drawToScaleBtn.setVisible( false );
        //        }
        //        else {
        drawToScaleBtn.setSelection(colorBar.getDrawToScale());

        drawToScaleBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                colorBar.setDrawToScale(drawToScaleBtn.getSelection());
                computeColorBarRectangles();
            }
        });
        //        }

        canvas.setFont(font);
        canvas.setBackground(canvasColor);

        canvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawColorBar((Canvas) e.getSource(), e.gc);
            }
        });

        canvas.addControlListener(new ControlListener() {
            @Override
            public void controlMoved(ControlEvent e) {
                canvasSize = canvas.getSize();
                computeColorBarRectangles();
            }

            @Override
            public void controlResized(ControlEvent e) {
                canvasSize = canvas.getSize();
                computeColorBarRectangles();
            }

        });

        canvas.addListener(SWT.MouseDown, mouseLstnr);
        canvas.addListener(SWT.MouseMove, mouseLstnr);
        canvas.addListener(SWT.MouseUp, mouseLstnr);

        labelColorSelector.setColorValue(colorBar.getLabelColor());

        intrvlColorSelector.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                colorBar.setRGB(seldIntrvl, intrvlColorSelector.getColorValue());
                canvas.redraw();
            }
        });

        labelColorSelector.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                colorBar.setLabelColor(labelColorSelector.getColorValue());
                if (labelColor != null) {
                    labelColor.dispose();
                }
                labelColor = new Color(colorDevice, labelColorSelector.getColorValue());

                canvas.redraw();
            }
        });

    }

    // update the values used to draw the colorBar 
    private void computeColorBarRectangles() {

        // if an interval has been removed or added then just start over with a new array
        if (colorBarRects == null || colorBarRects.size() != colorBar.getNumIntervals()) {
            colorBarRects = new ArrayList<Rectangle>();

            for (int c = 0; c < colorBar.getNumIntervals(); c++) {
                colorBarRects.add(new Rectangle(0, 0, 1, 1));
            }
        }

        int barIntX = CBAR_XOFF;
        int cbarPixWidth = canvasSize.x - 2 * CBAR_XOFF;

        // center in the middle of the canvase
        int barIntY = canvasSize.y / 2 - colorBar.getWidthInPixels() / 2 + 4; // 4 for the text height approx.

        // this will be constant if not drawing to scale
        int barIntWidth = 1;

        if (colorBarRects.size() > 0) {
            barIntWidth = (canvasSize.x - 2 * CBAR_XOFF) / colorBarRects.size();
        }

        if (colorBar.getDrawToScale()) {
            int intCnt = colorBar.getNumIntervals();

            // the size in pixels of an interval to +- infinity
            int infIntSize = cbarPixWidth / intCnt;

            // a scale value by dividing the width in pixels by the 
            // range of actual values (not including infinite intervals)						
            Float rangeMin = (colorBar.getIntervalMin(0) == Float.NEGATIVE_INFINITY ? colorBar.getIntervalMax(0) : colorBar.getIntervalMin(0));
            Float rangeMax = (colorBar.getIntervalMax(intCnt - 1) == Float.POSITIVE_INFINITY ? colorBar.getIntervalMin(intCnt - 1) : colorBar.getIntervalMax(intCnt - 1));

            if (colorBar.getIntervalMin(0) == Float.NEGATIVE_INFINITY) {
                cbarPixWidth -= infIntSize;
            }
            if (colorBar.getIntervalMax(intCnt - 1) == Float.POSITIVE_INFINITY) {
                cbarPixWidth -= infIntSize;
            }

            pixPerUnit = cbarPixWidth / (rangeMax - rangeMin);

            for (int b = 0; b < intCnt; b++) {
                Rectangle cbarRect = colorBarRects.get(b);

                cbarRect.x = barIntX;
                cbarRect.y = barIntY;
                cbarRect.height = colorBar.getWidthInPixels();

                // determine the width of the interval in pixels. if inf then use the ave size of an interval
                if ((b == 0 && colorBar.getIntervalMin(0) == Float.NEGATIVE_INFINITY) || (b == intCnt - 1 && colorBar.getIntervalMax(b) == Float.POSITIVE_INFINITY)) {
                    barIntWidth = infIntSize;
                } else {
                    barIntWidth = (int) (pixPerUnit * (colorBar.getIntervalMax(b) - colorBar.getIntervalMin(b)));
                }
                barIntX += barIntWidth;
                cbarRect.width = barIntWidth;
            }
        } else {
            for (Rectangle cbarRect : colorBarRects) {
                cbarRect.x = barIntX;
                cbarRect.y = barIntY;
                cbarRect.height = colorBar.getWidthInPixels();

                barIntX += barIntWidth;
                cbarRect.width = barIntWidth;
            }
        }
        canvas.redraw();
    }

    // 
    private void drawColorBar(Canvas canvas, GC gc) {

        int textHeight = gc.getFontMetrics().getHeight();
        int charWidth = gc.getFontMetrics().getAverageCharWidth();

        gc.setLineWidth(1);

        // draw the rectangles in the given color
        for (int b = 0; b < colorBar.getNumIntervals(); b++) {
            gc.setBackground(colorBar.getColor(b));
            gc.setForeground(colorBar.getColor(b));
            gc.drawRectangle(colorBarRects.get(b));
            gc.fillRectangle(colorBarRects.get(b));
        }

        // for image cmaps draw a border around the colorbar
        //		if( applyToImage ) {
        //			gc.setForeground( labelColor );
        //			Rectangle r1 = colorBarRects.get(0);
        //			Rectangle r2 = colorBarRects.get(colorBarRects.size()-1);
        //
        //			gc.drawRectangle(r1.x-1, r1.y-1, r2.x+r2.width+1-r1.x, r2.height+1 );
        //		}

        Rectangle seldRect = colorBarRects.get(seldIntrvl);

        gc.setLineWidth(1); //(applyToImage ? 2 : 1) );
        gc.setForeground(seldColor);

        // if dragging, draw the modified interval selection.		
        // otherwise highlight the selected interval/pixel
        if (dragXvalue != 0) {
            gc.drawLine(seldRect.x + seldRect.width, seldRect.y, seldRect.x + seldRect.width, seldRect.y + seldRect.height);
            gc.drawLine(dragXvalue, seldRect.y - textHeight, dragXvalue, seldRect.y + seldRect.height);
            gc.drawRectangle(seldRect.x, seldRect.y - 1, dragXvalue - seldRect.x, seldRect.height + 2);

            String dragMaxLabel = getLabelString(interpolateNewIntervalMax());

            gc.drawText(dragMaxLabel, dragXvalue - charWidth * dragMaxLabel.length() / 2, seldRect.y - textHeight * 2, true);
        } else {
            //			if( applyToImage ) {
            //				gc.drawRectangle(seldRect.x, seldRect.y-2, seldRect.width, seldRect.height+3 );
            //			}
            //			else {
            gc.drawRectangle(seldRect.x, seldRect.y - 1, seldRect.width, seldRect.height + 2);
            //			}			
        }

        // if showing the labels, draw them based on whether this for an image or not
        if (colorBar.getShowLabels()) {

            gc.setForeground(labelColor);

            //			if( applyToImage ) {
            //				for( int pix=0 ; pix<colorBar.getNumIntervals() ; pix++ ) {
            //					if( colorBar.isIntervalLabeled(pix) ) {
            //						String labelStr = Integer.toString(pix);
            //						int textWidth = labelStr.length()*charWidth;
            //						Rectangle pixRect = colorBarRects.get(pix);
            //						int textX = pixRect.x-textWidth/2;
            //						int textY = pixRect.y-textHeight;
            //						gc.drawText( labelStr, textX, textY, true );
            //						gc.drawLine( pixRect.x, pixRect.y, pixRect.x, pixRect.y+pixRect.height );
            //					}
            //				}
            //			}
            //			else {
            int numIntrvs = colorBar.getNumIntervals();
            int textWidth = 0;
            int prevLabelExtent = 0;
            boolean labelRaised = false;

            for (int b = 0; b < numIntrvs; b++) {
                String labelStr = colorBar.getLabelString(b);
                textWidth = labelStr.length() * charWidth;
                int textX = colorBarRects.get(b).x - textWidth / 2;
                int textY = colorBarRects.get(b).y - textHeight;

                // if this label will overwrite the previous one and it was not raised then raise it up
                labelRaised = (!labelRaised && textX < prevLabelExtent);

                if (labelRaised) {
                    textY -= textHeight / 2;
                    textY = colorBarRects.get(b).y + colorBarRects.get(b).height;
                }

                gc.drawText(labelStr, textX, textY, true);

                prevLabelExtent = textX + textWidth;
            }

            //				if( !applyToImage ) {
            String labelStr = colorBar.getLabelString(numIntrvs);
            gc.drawText(labelStr, colorBarRects.get(numIntrvs - 1).x + colorBarRects.get(numIntrvs - 1).width - textWidth / 2, colorBarRects.get(numIntrvs - 1).y - textHeight,
                    true);
            //				}
            //			}
        }
    }

    // TODO : Add support for control over num of decimal places
    private String getLabelString(Float val) {
        if (val == Float.NEGATIVE_INFINITY) {
            return "-Inf";
        } else if (val == Float.POSITIVE_INFINITY) {
            return "Inf";
        } else
            return Float.toString(val);
    }

    // this is called when the user selects an interval and when the usr modifies either 
    // by setting the min/max or by inserting or removing an interval
    // 
    private void updateSelectedInterval() {
        removeModifyListenersForSpinners();

        RGB seldRGB = colorBar.getRGB(seldIntrvl);
        if (seldRGB != null) {
            intrvlColorSelector.setColorValue(seldRGB);
        }

        //		if( applyToImage ) {
        //			pixelNumTxt.setText( Integer.toString( seldIntrvl) );
        //			labelPixelBtn.setSelection( colorBar.isIntervalLabeled( seldIntrvl ) );
        //		}
        //		else {		
        int lastIntrvl = colorBar.getNumIntervals() - 1;

        // wait to set these values in the Spinners since we first have to change the min/max  
        // ranges or the Spinner won's accept the new selections.
        Float minVal = colorBar.getIntervalMin(seldIntrvl) * (float) scaleMult;
        Float maxVal = colorBar.getIntervalMax(seldIntrvl) * (float) scaleMult;

        // if the min/max is +-Inf then this will not be visible since it will be covered up by the Inf Text

        // set the new min an max allowed values for the Spinners based on the min and max interval
        // values of the adjacent intervals. This will also prevent the user from editing the text 
        // to be a value out of range for the interval.
        //			
        int minMinVal, maxMinVal;
        int minMaxVal, maxMaxVal;

        // Set the min for the min to the min of the prev interval
        if (seldIntrvl == 0 || (seldIntrvl == 1 && colorBar.getIntervalMin(0) == Float.NEGATIVE_INFINITY)) {
            minMinVal = Integer.MIN_VALUE; //Math.round( minVal ) - 1;
        } else {
            Float tVal = colorBar.getIntervalMin(seldIntrvl - 1).floatValue() * (float) scaleMult;
            minMinVal = Math.round(tVal) + 1;
        }

        // set the max for the min to the max of this interval
        maxMinVal = Math.round(maxVal) - 1;

        // set the min for the max to the min for this interval
        minMaxVal = Math.round(minVal) + 1;

        // set the max for the max to the max of the next interval 
        if (seldIntrvl == lastIntrvl || (seldIntrvl == lastIntrvl - 1 && colorBar.getIntervalMax(lastIntrvl) == Float.POSITIVE_INFINITY)) {
            maxMaxVal = Integer.MAX_VALUE; // colorBar.getIntervalMax( seldIntrvl ).floatValue() * (float)scaleMult;		
        } else {
            Float tVal = colorBar.getIntervalMax(seldIntrvl + 1).floatValue() * (float) scaleMult;
            maxMaxVal = Math.round(tVal) - 1;
        }

        intrvlMinSpnr.setMinimum(minMinVal);
        intrvlMinSpnr.setMaximum(maxMinVal);

        intrvlMaxSpnr.setMinimum(minMaxVal);
        intrvlMaxSpnr.setMaximum(maxMaxVal);

        setIntervalMin(minVal);
        setIntervalMax(maxVal);

        //intrvlMinSpnr.setTextLimit(limit);
        //		}
        addModifyListenersForSpinners();
    }

    private Float interpolateNewIntervalMax() {
        Rectangle seldRect = colorBarRects.get(seldIntrvl);
        Float newMax = colorBar.getIntervalMin(seldIntrvl) + (float) (dragXvalue - seldRect.x) / (float) seldRect.width
                * (colorBar.getIntervalMax(seldIntrvl) - colorBar.getIntervalMin(seldIntrvl));
        //		DecimalFormat fmt = new DecimalFormat();
        //		fmt.setMaximumFractionDigits(1);
        //		fmt.setMinimumFractionDigits(1);
        //		try {
        //			Float tMax =  fmt.format( newMax );
        //			if( tMax != newMax ) {
        //				System.out.println("adfadf");
        //			}
        //		} catch (ParseException e) {
        //		}

        //int m = 10^numDecimals;
        newMax = (float) (Math.round(newMax.floatValue() * scaleMult) / scaleMult);
        //newMax = (Float) (Math.floor( (float)newMax*(float)mult) / mult);
        return newMax;
    }

    // currently this is only used when a new colormap has been selected ....
    //	public void reset( IColorBar newColorBar ){// ColorMap colorMap ) {//String cat, String cmapName ) {
    //		if( colorBar != newColorBar ) {
    //			// TODO : will need to complete this method by resetting everything 
    //			System.out.println("colorBar != newColorbar");
    //		}
    //		
    //		if( !applyToImage ) {
    //			return;
    //		}
    ////	colorBar.removeAllLabels();
    //	
    //		seldIntrvl = 0;
    //		computeColorBarRectangles();
    //		updateSelectedInterval();
    //	}

    // 
    private void setIntervalMin(Float newMin) {

        negInfIntrvlBtn.setVisible((seldIntrvl == 0 ? true : false));

        boolean isInf = (newMin == Float.NEGATIVE_INFINITY ? true : false);

        negInfIntrvlBtn.setSelection(isInf);
        negInfIntrvlTxt.setVisible(isInf);
        intrvlMinSpnr.setVisible(!isInf);

        intrvlMinSpnr.setSelection((isInf ? Integer.MIN_VALUE : Math.round(newMin)));
    }

    private void setIntervalMax(Float newMax) {

        posInfIntrvlBtn.setVisible((seldIntrvl == colorBar.getNumIntervals() - 1 ? true : false));

        boolean isInf = (newMax == Float.POSITIVE_INFINITY ? true : false);

        posInfIntrvlBtn.setSelection(isInf);
        posInfIntrvlTxt.setVisible(isInf);
        intrvlMaxSpnr.setVisible(!isInf);

        intrvlMaxSpnr.setSelection((isInf ? Integer.MAX_VALUE : Math.round(newMax)));
    }

    @Override
    public void dispose() {
        super.dispose();

        if (font != null)
            font.dispose();
        if (canvas != null)
            canvas.dispose();
        if (seldColor != null)
            seldColor.dispose();
        if (labelColor != null)
            labelColor.dispose();
        if (pointerCursor != null)
            pointerCursor.dispose();
        if (dragIntrvlCursor != null)
            dragIntrvlCursor.dispose();
    }

    public ModifyListener minSpnrListener = new ModifyListener() {
        @Override
        public void modifyText(ModifyEvent e) {

            float intrvlValue = (float) intrvlMinSpnr.getSelection();

            if (intrvlValue == Integer.MIN_VALUE)
                return;

            Float minVal = intrvlValue / (float) scaleMult;

            colorBar.setIntervalMin(seldIntrvl, minVal);
            computeColorBarRectangles();
        }
    };

    public ModifyListener maxSpnrListener = new ModifyListener() {
        @Override
        public void modifyText(ModifyEvent e) {

            float intrvlValue = (float) intrvlMaxSpnr.getSelection();

            if (intrvlValue == Integer.MAX_VALUE)
                return;

            Float maxVal = (float) intrvlMaxSpnr.getSelection() / (float) scaleMult;

            colorBar.setIntervalMax(seldIntrvl, maxVal);
            computeColorBarRectangles();
        }
    };

    private void addModifyListenersForSpinners() {
        intrvlMinSpnr.addModifyListener(minSpnrListener);
        intrvlMaxSpnr.addModifyListener(maxSpnrListener);
    }

    private void removeModifyListenersForSpinners() {
        intrvlMinSpnr.removeModifyListener(minSpnrListener);
        intrvlMaxSpnr.removeModifyListener(maxSpnrListener);
    }

}