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
package com.raytheon.uf.viz.radarapps.products.ui;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.SimpleTimeZone;

import javax.xml.bind.JAXBException;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Scale;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.mqsrvr.ReplyObj.ROStatus;
import com.raytheon.rcm.mqsrvr.ReplyObj.StatusMessagesReply;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRadarStatusMessages;
import com.raytheon.rcm.products.ElevationInfo;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.ProductInfo.Selector;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.rcm.products.Usage;
import com.raytheon.rcm.request.Request;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.uf.viz.radarapps.products.ui.PMenu.PMenuItem;
import com.raytheon.uf.viz.radarapps.products.ui.PMenu.PProductItem;
import com.raytheon.uf.viz.radarapps.products.ui.PMenu.PSeparator;
import com.raytheon.uf.viz.radarapps.products.ui.PMenu.PSubMenu;
import com.raytheon.viz.ui.widgets.MenuButton;
import com.raytheon.viz.ui.widgets.MinimumSizeComposite;

/**
 * This abstract class generates a consistent layout for various products.
 * Example of use is the One Time Request Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Jul 11, 2012 #875       rferrel     Refactored for changes in MenuButton and
 *                                      converted to abstract class making the
 *                                      needed methods abstract.
 * Jul 31, 2012 #875       rferrel     Points now group in menu items.
 * Oct 04, 2012 #1248      rferrel     Added Point change listener.
 * 
 * </pre>
 * 
 * @author ?
 * @version 1.0
 */

public abstract class BaseRadarProductUI {
    private Composite productRequestUI;

    private Composite productSelectorUI;

    private MenuButton productMenuButton;

    private Composite parameterArea;

    private Composite vsSelectionUI;

    private Control vsSelectionSeparator;

    private Label selectedVSTimeLabel;

    private Button[] vsSelectionButtons;

    private Combo priorityCombo;

    private Composite parameterUI;

    private Combo elevationTypeCombo;

    private Combo elevationAnglesCombo;

    private ScaleWithLabel requestIntervalScale;

    private ScaleWithLabel[] azRan1;

    private ScaleWithLabel[] azRan2;

    protected Composite geomCombo;

    protected String desiredGeom = "A";

    private boolean isPoints;

    private Label azRanLabel;

    private Button defaultSpeedDirButton;

    private ScaleWithLabel[] speedDir;

    private ScaleWithLabel altitudeScale;

    private ScaleWithLabel bottomAltitude;

    private ScaleWithLabel topAltitude;

    private Button[] timeSpanType;

    private ScaleWithLabel[] timeSpan;

    private ScaleWithLabel[] timeSpanMinutes;

    private Combo cfcSegment;

    private Combo variantsCombo;

    private Label validationLabel;

    private Label validationImageLabel; // TODO: Something like jface status

    // display?

    private boolean prioritySelectionEnabled;

    private boolean volumeScanSelectionEnabled;

    private boolean repeatCountEnabled;

    private Composite repeatCountParent;

    private Composite repeatCountUI;

    private Combo repeatCountCombo;

    /**
     * Tracks repeat count setting independently of the request which may have
     * its repeat count forced to one. This is used to duplicate the behavior of
     * the AWIPS 1 GUI.
     */
    private short desiredRepeatCount = 1;

    private boolean rpgSelectorEnabled;

    private Composite rpgSelectorParent;

    private ExtProductsUI rpgSelectorUI;

    private Request request;
    {
        request = new Request();
        request.count = 1;
        request.interval = 1;
        request.selectCurrent();
    }

    private Usage usage;

    private String radarID;

    private RadarType radarType;

    private static final int UNSPECIFIED_VCP = -1;

    int vcp = UNSPECIFIED_VCP; // TODO: vs. the "OTR" entry in

    // elevationLists.txt and ElevationInfo

    private Selector productGroupSelector;

    private HashMap<String, MenuItem> productMenuItems = new HashMap<String, MenuItem>();

    private int liveVCP = Integer.MIN_VALUE;

    private String liveRadarID;

    private int[] liveElevationAngles;

    // This should always be non-null
    List<RadarProduct> currentPossibleProducts = new ArrayList<RadarProduct>();

    public Control createProductRequestUI(Composite parent) {
        if (productRequestUI != null) {
            productRequestUI.dispose();
            productRequestUI = null;
            repeatCountParent = null;
            rpgSelectorParent = null;
            disposeParameterUI();
            disposeVolumeScanSelectionUI();
        }

        GridLayout gl;

        productRequestUI = new Composite(parent, SWT.NONE);
        productRequestUI.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                onDispose();
            }
        });
        gl = new GridLayout(1, false);
        gl.marginWidth = gl.marginHeight = 0;
        productRequestUI.setLayout(gl);

        /*
         * Create top row which may contain the repeat count and RPG selector
         * controls. These controls are only used in the OTR tool.
         */
        Composite r;
        r = new Composite(productRequestUI, SWT.NONE);
        r.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
        gl = new GridLayout(2, false);
        gl.marginWidth = gl.marginHeight = 0;
        r.setLayout(gl);
        repeatCountParent = new Composite(r, SWT.NONE);
        repeatCountParent.setLayout(new FillLayout(SWT.HORIZONTAL));
        repeatCountParent.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
                false, false));
        rpgSelectorParent = new Composite(r, SWT.NONE);
        rpgSelectorParent.setLayout(new FillLayout(SWT.HORIZONTAL));
        rpgSelectorParent.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                false, false));
        if (repeatCountEnabled)
            createRepeatCountUI();
        if (rpgSelectorEnabled)
            createRPGSelectorUI();

        Control c;

        c = createProductSelectorUI(productRequestUI);
        c.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));

        c = new Label(productRequestUI, SWT.SEPARATOR | SWT.HORIZONTAL);
        c.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));

        c = createParameterSelectionArea(productRequestUI);
        c.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        if (isVolumeScanSelectionEnabled())
            createVolumeScanSelectionUI();

        return productRequestUI;
    }

    protected void onDispose() {
        // nothing
    }

    private Control createProductSelectorUI(Composite parent) {
        if (productSelectorUI != null) {
            productSelectorUI.dispose();
            productSelectorUI = null;
        }

        Composite row = new Composite(parent, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.center = true;
        row.setLayout(rl);

        Label l = new Label(row, SWT.LEFT);
        l.setText("Product: ");
        l.setAlignment(SWT.CENTER);

        MenuButton mb = new MenuButton(row);
        mb.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onProductSelected();
            }
        });
        Menu m = new Menu(mb);
        mb.setMenu(m);

        productMenuButton = mb;
        productSelectorUI = row;

        refillProductsMenu();

        return row;
    }

    private void onProductSelected() {
        MenuItem mi = productMenuButton.getSelectedItem();
        if (mi == null || mi.isDisposed())
            return;
        Selector sel = (Selector) mi.getData();

        setProductGroup(sel, false);
        changeRequestProduct();
        createParameterUI();
        /*
         * Changing the product type means the azimuth/range values for points
         * and baseline must be set.
         */
        if (geomCombo != null)
            onGeomSelected(desiredGeom);
    }

    private void setProductGroup(Selector sel, boolean setMenuButton) {

        productGroupSelector = sel.duplicate();

        if (setMenuButton && productMenuButton != null) {
            // Currently, menu items can be identified by mnemonic alone...
            String mne = sel.mnemonic;
            if (mne != null)
                productMenuButton.setSelectedItem(productMenuItems.get(mne));
            else
                productMenuButton.setSelectedItem((MenuItem) null);
        }
        sel = sel.narrow(new Selector(radarType, null, null, usage));

        if (sel.mnemonic != null) {
            Collection<RadarProduct> products = ProductInfo.getInstance()
                    .select(sel);
            List<RadarProduct> pl;
            try {
                pl = (List<RadarProduct>) products;
            } catch (ClassCastException e) {
                pl = new ArrayList<RadarProduct>(products);
            }
            currentPossibleProducts = pl;
        } else {
            currentPossibleProducts.clear();
        }

        // TODO: should sort variants by goodness..
    }

    // ... to conform to the current list of possible products
    private void changeRequestProduct() {
        for (RadarProduct p : currentPossibleProducts)
            if (request.productCode == p.pid)
                return;

        /* AWIPS 1 does not do this. Should I bother? */
        if (currentPossibleProducts.size() > 0) {
            RadarProduct oldProd = getProductInfo().getPoductForCode(
                    request.productCode);
            RadarProduct first = currentPossibleProducts.get(0);
            Request old = request;
            request = (Request) old.clone();
            request.pdw20 = 0;
            request.pdw21 = 0;
            request.pdw22 = 0;
            request.pdw23 = 0;
            request.pdw24 = 0;
            request.pdw25 = 0;
            request.productCode = (short) first.pid;
            for (Param p : first.params) {
                if (oldProd != null && oldProd.params.contains(p)) {
                    copyParam(old, request, p);
                    if (p == Param.ELEVATION && request.pdw22 == 0)
                        setParamDefault(p);
                } else
                    setParamDefault(p);
            }
        } else
            request.productCode = 0;
        onRequestChanged();
    }

    private void copyParam(Request src, Request dst, Param p) {
        if (p == Param.ALTITUDE)
            dst.setAltitude(src.getAltitude());
        else if (p == Param.BASELINE) {
            dst.setAzimuth(src.getAzimuth());
            dst.setRange(src.getRange());
            dst.setAzimuth2(src.getAzimuth2());
            dst.setRange2(src.getRange2());
            // TODO: Should set from baseline A
        } else if (p == Param.CFC_BITMAP)
            dst.setCfcWord(src.getCfcWord());
        else if (p == Param.ELEVATION) {
            dst.pdw22 = src.pdw22;
        } else if (p == Param.LAYER) {
            dst.setBottomAltitude(src.getBottomAltitude());
            dst.setTopAltitude(src.getTopAltitude());
        } else if (p == Param.MINI_VOLUME) {
            dst.setMiniVolume(src.getMiniVolume());
        } else if (p == Param.STORM_SPEED_DIR) {
            dst.setStormSpeed(src.getStormSpeed());
            dst.setStormDirection(src.getStormDirection());
        } else if (p == Param.TIME_SPAN || p == Param.TIME_SPAN_MINUTES) {
            dst.setEndHour(src.getEndHour());
            src.setTimeSpan(src.getTimeSpan());
        } else if (p == Param.WINDOW_AZ_RAN) {
            dst.setAzimuth(src.getAzimuth());
            dst.setRange(src.getRange());
        }
    }

    /**
     * Sets a default value for the given request parameter.
     * <p>
     * Note: Caller must call onRequestChanged().
     */
    private void setParamDefault(Param p) {
        if (p == Param.ALTITUDE)
            request.setAltitude(0);
        else if (p == Param.BASELINE) {
            request.setAzimuth(0);
            request.setRange(0);
            request.setAzimuth2(0);
            request.setRange2(0);
            // TODO: Should set from baseline A
        } else if (p == Param.CFC_BITMAP) {
            request.setCfcWord(2); // Set the second bit.
        } else if (p == Param.ELEVATION) {
            int[] e = getElevationAngles();
            if (e != null)
                request.setElevationAngle(e[0]);
            else
                request.setElevationAngle(0); // TODO: ??
        } else if (p == Param.LAYER) {
            request.setBottomAltitude(0);
            request.setTopAltitude(1);
        } else if (p == Param.MINI_VOLUME) {
            // I think AWIPS 1 sets this even for non-TDWRs
            if (radarType == RadarType.TDWR)
                request.setMiniVolume(2);
            // 2=end volume RadarServer follows AWIPS 1
            // behavoir and will automatically copy to a
            // 1==start-volume from this...
        } else if (p == Param.STORM_SPEED_DIR) {
            request.setStormDirection(0);
            request.setStormSpeed(0);
        } else if (p == Param.TIME_SPAN) {
            request.setEndHour(0);
            request.setTimeSpan(1);
        } else if (p == Param.TIME_SPAN_MINUTES) {
            request.setEndHour(0);
            request.setTimeSpan(15);
        } else if (p == Param.WINDOW_AZ_RAN) {
            request.setAzimuth(0);
            request.setRange(0);
            // TODO: Should set from point A
        }
        /* else... ?? p.init(request) ? */
        // TODO:..
    }

    private Control createParameterSelectionArea(Composite parent) {
        if (parameterArea != null) {
            parameterArea.dispose();
            parameterArea = null;
        }

        parameterArea = new Composite(parent, SWT.NONE);

        GridLayout gl = new GridLayout(1, true);
        gl.marginWidth = gl.marginHeight = 0;
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        parameterArea.setLayout(gl);

        realizeUIFromRequest();

        return parameterArea;
    }

    /**
     * Regenerates UIs (product selector and parameter area) from the current
     * request.
     */
    private void realizeUIFromRequest() {
        setProductGroup(
                new Selector(null, getProductInfo().getMnemonicForCode(
                        request.productCode), null, null), true);
        createParameterUI();
    }

    protected void onRepeatCountSelected() {
        int repeatCount = repeatCountCombo.getSelectionIndex();
        if (repeatCount < 0)
            return;
        if (++repeatCount > 9)
            repeatCount = 9;
        desiredRepeatCount = request.count = (short) repeatCount;
        onRequestChanged();
    }

    /**
     * Called after the request value has been changed. Does not update
     * controls.
     */
    protected void onRequestChanged() {
        autoValidate();
    }

    private Image getErrorImage(Display display) {
        return display.getSystemImage(SWT.ICON_ERROR);
    }

    public boolean isRequestValid() {
        return validateRequest() == null;
    }

    private void autoValidate() {
        String msg = validateRequest();

        if (validationLabel != null && validationImageLabel != null) {
            if (msg != null) {
                Image img = getErrorImage(validationLabel.getDisplay());
                validationImageLabel.setImage(img);
                validationLabel.setText(msg);
            } else {
                validationImageLabel.setImage(null);
                validationLabel.setText("");
            }
            validationLabel.getParent().layout();
        }
    }

    private String validateRequest() {
        RadarProduct product = null;

        for (RadarProduct rp : currentPossibleProducts)
            if (request.productCode == rp.pid) {
                product = rp;
                break;
            }
        if (product == null)
            return "Invalid product type.  Please select a valid product / variant.";

        /*
         * TODO: On AWIPS-1 the selector for TDWR only allows Single and
         * All-matching, but not sure if this is a valid restriction...
         */
        /*
         * if (product.params.contains(Param.ELEVATION) &&
         * findRequestElevationSelectionIndex() < 0) return
         * "Invalid elevation type";
         */

        if (product.params.contains(Param.WINDOW_AZ_RAN)
                && (request.getRange() < 0 || request.getRange() > 1240))
            return "Range must be between 0 and 124.0 nmi";

        // TODO: need to check baseline length -- but is the maximum really
        // 124.0 nmi?
        if (product.params.contains(Param.BASELINE)) {
            if (request.getRange() < 0 || request.getRange() > 1240
                    || request.getRange2() < 0 || request.getRange2() > 1240)
                return "Range of points on the baseline must be between 0 and 124.0 nmi";
            double r = getBaselineLength();
            if (r < 0.54) // TODO: max...
                return "Baseline range must be at least 0.54 nmi";
        }

        if (product.params.contains(Param.CFC_BITMAP)) {
            if ((request.getCfcWord() & 0x3e) == 0)
                return "Select an elevation segment";
        }

        return null;
    }

    private double getBaselineLength() {
        double a1 = request.getAzimuth() / 10.0 * Math.PI / 180;
        double r1 = request.getRange() / 10.0;
        double a2 = request.getAzimuth2() / 10.0 * Math.PI / 180;
        double r2 = request.getRange2() / 10.0;

        return Math.sqrt(r1 * r1 + r2 * r2 - 2 * r1 * r2 * Math.cos(a1 - a2));
    }

    public void setRequest(Request request) {
        this.request = (Request) request.clone();
        desiredRepeatCount = request.count;
        desiredGeom = "";

        boolean needChange = true;

        for (RadarProduct p : currentPossibleProducts)
            if (this.request.productCode == p.pid) {
                needChange = false;
                break;
            }

        if (needChange)
            realizeUIFromRequest();

        setUIFromRequest();
        onRequestChanged();
    }

    public void setDefaultRequest() {
        setProductGroup(new Selector(null, "Z", null, null), true);
        changeRequestProduct();
        realizeUIFromRequest();
        setRequest(getRequest());
        desiredGeom = "A";
    }

    public Request getRequest() {
        return (Request) request.clone();
    }

    private int uiChangeLocked;

    private boolean uiChangeRequested;

    private void lockUIChange() {
        ++uiChangeLocked;
    }

    private void unlockUIChange() {
        if (uiChangeLocked > 0)
            --uiChangeLocked;
        if (uiChangeLocked < 1 && uiChangeRequested) {
            requestUIChange();
        }
    }

    private void requestUIChange() {
        uiChangeRequested = true;
        if (uiChangeLocked < 1) {
            uiChangeRequested = false;

            refillProductsMenu();

            if (productGroupSelector != null
                    && productMenuItems.get(productGroupSelector.mnemonic) != null
                    && getProductInfo().select(
                            (Selector) productMenuItems.get(
                                    productGroupSelector.mnemonic).getData())
                            .size() > 0) {
                /*
                 * productGroupSelector may have other criteria that are no
                 * longer valid. Just create a new selector using only the
                 * mnemonic.
                 */
                setProductGroup(new Selector(null,
                        productGroupSelector.mnemonic, null, null), true);
                boolean ok = false;
                for (RadarProduct rp : currentPossibleProducts) {
                    if (rp.pid == request.productCode) {
                        ok = true;
                        break;
                    }
                }
                if (!ok)
                    changeRequestProduct();
            } else {
                setDefaultRequest();
            }

            createParameterUI();
        }
    }

    /**
     * Tells the UI which radar, the radar type, and whether or not it is
     * dedicated.
     * <p>
     * Calls setRadarType. After setting the VCP, controls which elevation
     * angles are available.
     */
    public void setRadarConfig(RadarConfig rc) {
        lockUIChange();
        try {
            RadarType type = Util.getRadarType(rc);
            if (type != radarType) {
                setRadarType(type);
            } else {
                // TODO: but also isDedicated may require more like setRadarType
            }
            setRadarID(rc.getRadarID());
            requestUIChange();
        } finally {
            unlockUIChange();
        }
    }

    /**
     * Sets the type of radar (WSR-88D vs. TDWR) for which to select products.
     * <p>
     * Determines which products are available and how certain parameters are
     * selected.
     */
    public void setRadarType(RadarType type) {
        radarType = type;
        requestUIChange();
    }

    private void refillProductsMenu() {
        Selector sel = new Selector(radarType, null, null, usage);
        fillProductsMenu(sel);
    }

    private void fillProductsMenu(Selector sel) {
        if (productMenuButton != null) {
            Menu menu = productMenuButton.getMenu();
            if (menu != null)
                fillProductsMenu(productMenuButton.getMenu(), sel);
        }
    }

    private void fillProductsMenu(Menu menu, Selector sel) {
        PMenu pmenu = getProductMenu();
        for (MenuItem mi : menu.getItems())
            mi.dispose();
        productMenuItems.clear();
        fillMenu1(menu, pmenu, sel);
        /*
         * could have SMB call this just before it does a popup, but could be
         * slow and no way to tell if a widget is changed?
         */
        productMenuButton.updateMenu();

        String mne = sel.mnemonic;
        if (mne != null)
            productMenuButton.setSelectedItem(productMenuItems.get(mne));
    }

    private void fillMenu1(Menu menu, PMenu pmenu, Selector sel) {
        for (PMenuItem pi : pmenu.items) {
            if (sel.usage != null && pi.use != null && pi.use.length > 0) {
                boolean ok = false;
                for (Usage use : pi.use) {
                    if (use == sel.usage) {
                        ok = true;
                        break;
                    }
                }
                if (!ok)
                    continue;
            }
            if (pi instanceof PProductItem) {
                PProductItem ppi = (PProductItem) pi;
                MenuItem mi = new MenuItem(menu, SWT.PUSH);
                if (pi.name != null)
                    mi.setText(pi.name);
                else {
                    String s = getProductInfo().getFullNameForMnemonic(
                            ppi.mnemonicReference);
                    if (s == null)
                        s = ppi.mnemonicReference.toUpperCase();
                    mi.setText(s);
                }
                Selector selCopy = sel.duplicate();
                selCopy.mnemonic = ppi.mnemonicReference;
                mi.setData(selCopy);
                mi.setEnabled(getProductInfo().select(selCopy).size() > 0);
                productMenuItems.put(ppi.mnemonicReference, mi);
                // but do we want *all* the terms in the menu item?
            } else if (pi instanceof PSeparator) {
                new MenuItem(menu, SWT.SEPARATOR);
            } else if (pi instanceof PSubMenu) {
                MenuItem mi = new MenuItem(menu, SWT.CASCADE);
                if (pi.name != null)
                    mi.setText(pi.name);
                Menu m = new Menu(menu);
                mi.setMenu(m);
                fillMenu1(m, ((PSubMenu) pi).menu, sel);
            }
        }
    }

    private PMenu pmenu;

    private PMenu getProductMenu() {
        if (pmenu == null) {
            try {
                InputStream s = RadarProductUI.class
                        .getResourceAsStream("ProductMenu.xml");
                pmenu = (PMenu) PMenu.getUnmarshaller().unmarshal(s);
            } catch (JAXBException e) {
                // TODO:
            }
        }
        return pmenu;
    }

    private ProductInfo getProductInfo() {
        return ProductInfo.getInstance();
    }

    // private ElevationInfo elevationInfo;
    public ElevationInfo getElevationInfo() {
        return ElevationInfo.getInstance();
    }

    protected void createParameterUI() {
        if (parameterArea == null)
            return;

        try {
            createParameterUIParams();
        } catch (Exception e) {
            // TODO: this is for debugging
            // Although, still need error handler around the selector creation
            // as
            // getSelectedItem may be null, or the getData() may be
            // unexpected...
            e.printStackTrace(System.err);
            return;
        }

        Point puiSz = parameterUI.computeSize(SWT.DEFAULT, SWT.DEFAULT, true);
        Point paSz = parameterArea.getSize();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = Math.max(puiSz.x, paSz.x);
        gd.minimumHeight = Math.max(puiSz.y, paSz.y);
        parameterUI.setLayoutData(gd);
        // Need to check isVisible(). Otherwise, JFace dialog will not
        // auto-resize on initial display.
        if (parameterArea.getShell().isVisible())
            parameterArea.getShell().pack();
        parameterArea.layout(); // because the above can leave pUI packed..
        gd.minimumWidth = 0;
        gd.minimumHeight = 0;
        parameterUI.setLayoutData(gd);

        setUIFromRequest();
        autoValidate();
    }

    /*
     * Shouldn't need further filter information. I will probably put the list
     * of possible products in the menu item...
     */
    private Control createParameterUIParams() {

        List<RadarProduct> pl = currentPossibleProducts;
        disposeParameterUI();

        Composite rows = new Composite(parameterArea, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.marginWidth = gl.marginHeight = 0;
        rows.setLayout(gl);

        Composite r;

        parameterUI = rows;

        if (pl.size() >= 1) {

            RadarProduct first = pl.get(0);

            if (pl.size() > 1) {
                for (RadarProduct rp : pl) {
                    if (!first.params.equals(rp.params)) {
                        /*
                         * TODO: This does not happen with the current set of
                         * products we work with (as long as setType has been
                         * called with WSR or TDWR.) So if this happens, it is
                         * an internal errors.
                         * 
                         * If this ever happens legitimately, we would need to
                         * recreate the parameter UI whenever the variant
                         * changes.
                         */
                    }
                }
            }

            Label l;

            if (prioritySelectionEnabled) {
                r = new Composite(rows, SWT.NONE);
                // r.setLayout(new GridLayout(2, false));
                l = new Label(r, SWT.LEFT);
                l.setText("Priority:");
                priorityCombo = new Combo(r, SWT.READ_ONLY);
                priorityCombo.add("Normal");
                priorityCombo.add("High");
                setupRow(r, l, priorityCombo);

                priorityCombo.addSelectionListener(new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent e) {
                        request.highPriority = priorityCombo
                                .getSelectionIndex() == 1;
                        onRequestChanged();
                    }
                });
            }

            // always?
            r = new Composite(rows, SWT.NONE);
            l = new Label(r, SWT.LEFT);
            l.setText("Request interval:");

            requestIntervalScale = new ScaleWithLabel(r, SWT.HORIZONTAL);
            final Scale scale = requestIntervalScale.getScale();
            scale.setMinimum(1);
            scale.setMaximum(9);
            scale.setIncrement(1);
            scale.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent e) {
                    request.interval = (short) scale.getSelection();
                    onRequestChanged();
                }
            });
            requestIntervalScale.updateValue();

            setupRow(r, l, requestIntervalScale);

            if (first.params.contains(Param.ELEVATION))
                createElevationUI(rows);

            if (first.params.contains(Param.BASELINE)) {
                String[] baselines = getBaselineList();
                if (baselines != null) {
                    isPoints = false;
                    createGeom(rows, "Baseline:", baselines, "Load Baselines");
                } else {
                    r = new Composite(rows, SWT.NONE);
                    l = new Label(r, SWT.LEFT);
                    l.setText("Point 1");
                    azRan1 = createAzRan(l, r);

                    r = new Composite(rows, SWT.NONE);
                    l = new Label(r, SWT.LEFT);
                    l.setText("Point 2");
                    azRan2 = createAzRan(l, r);
                }
            }

            if (first.params.contains(Param.WINDOW_AZ_RAN)) {
                isPoints = true;
                createGeom(rows, "Point:", null, "Load Points");
            }

            if (first.params.contains(Param.STORM_SPEED_DIR)) {
                // TODO: radarInfo.txt claims this is differrent from 55 and 56
                // but ICD says the same...

                /* TODO: Put these messages in a config file? */

                String defaultText;
                if (first.pid == 55)
                    defaultText = "Use storm closest to window center";
                else if (first.pid == 56)
                    defaultText = "Use average speed and direction of currently "
                            + "identified storms";
                else
                    defaultText = "Use default speed/direction";

                createSpeedDir(rows, defaultText);
            }

            if (first.params.contains(Param.ALTITUDE)) {
                r = new Composite(rows, SWT.NONE);

                altitudeScale = createAltitude(r, "Altitude:", 0, 70);
                altitudeScale.getScale().addSelectionListener(
                        new SelectionAdapter() {
                            public void widgetSelected(SelectionEvent e) {
                                request.setAltitude(altitudeScale.getScale()
                                        .getSelection());
                                onRequestChanged();
                            }
                        });
            }

            if (first.params.contains(Param.LAYER)) {
                r = new Composite(rows, SWT.NONE);
                topAltitude = createAltitude(r, "Top\nof Layer:", 1, 70);
                // set the default value to the highest possible
                request.setTopAltitude(topAltitude.getScale().getMaximum());
                topAltitude.getScale().addSelectionListener(
                        new SelectionAdapter() {
                            public void widgetSelected(SelectionEvent e) {
                                request.setTopAltitude(topAltitude.getScale()
                                        .getSelection());
                                if (request.getTopAltitude() <= request
                                        .getBottomAltitude()) {
                                    request.setBottomAltitude(request
                                            .getTopAltitude() - 1);
                                    setUIFromRequest();
                                }
                                onRequestChanged();
                            }
                        });
                r = new Composite(rows, SWT.NONE);
                bottomAltitude = createAltitude(r, "Bottom\nof Layer:", 0, 69);
                bottomAltitude.getScale().addSelectionListener(
                        new SelectionAdapter() {
                            public void widgetSelected(SelectionEvent e) {
                                request.setBottomAltitude(bottomAltitude
                                        .getScale().getSelection());
                                if (request.getTopAltitude() <= request
                                        .getBottomAltitude()) {
                                    request.setTopAltitude(request
                                            .getBottomAltitude() + 1);
                                    setUIFromRequest();
                                }
                                onRequestChanged();
                            }
                        });
            }

            if (first.params.contains(Param.TIME_SPAN)) {
                // TODO: Should be in a config file...
                int maxSpan = 1;
                switch (first.pid) {
                case 31: // USP
                    maxSpan = 24;
                    break;
                case 150: // User selectable snow...
                case 151:
                    maxSpan = 30;
                    break;
                default:
                    // TODO: ...
                    maxSpan = 24; // a guess
                }
                createTimeSpan(rows, maxSpan);
            }

            if (first.params.contains(Param.TIME_SPAN_MINUTES)) {
                createTimeSpanMinutes(rows);
            }

            if (first.params.contains(Param.CFC_BITMAP)) {
                r = new Composite(rows, SWT.NONE);
                l = new Label(r, SWT.LEFT);
                l.setText("Elevation Segment:");
                cfcSegment = new Combo(r, SWT.READ_ONLY);
                for (int i = 1; i <= 5; ++i)
                    cfcSegment.add(Integer.toString(i));

                cfcSegment.addSelectionListener(new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent e) {
                        request.setCfcWord(1 << (cfcSegment.getSelectionIndex() + 1));
                        onRequestChanged();
                    }
                });
                setupRow(r, l, cfcSegment);
            }

            if (pl.size() > 1) {
                r = new Composite(rows, SWT.NONE);
                l = new Label(r, SWT.LEFT);
                l.setText("Variant");
                variantsCombo = new Combo(r, SWT.READ_ONLY);

                boolean doLevels = false;
                boolean doLayer = false;
                boolean doRes = false;
                boolean doRange = false;

                for (RadarProduct rp : pl) {
                    if (first == rp)
                        continue;
                    else {
                        if (!variationsEqual(first.levels, rp.levels))
                            doLevels = true;
                        if (!variationsEqual(first.layer, rp.layer))
                            doLayer = true;
                        if (!variationsEqual(first.resolution, rp.resolution))
                            doRes = true;
                        if (!variationsEqual(first.range, rp.range))
                            doRange = true;
                    }
                }

                for (RadarProduct rp : pl) {
                    // TODO: should sort these...
                    StringBuilder sb = new StringBuilder();
                    if (doLevels && rp.levels != null)
                        sb.append(String.format("%d levels  ", rp.levels));
                    if (doLayer && rp.layer != null)
                        sb.append(String.format("Layer %d  ", rp.layer));
                    if (doRes && rp.resolution != null)
                        sb.append(String.format("%.2f res  ", rp.resolution));
                    if (doRange && rp.range != null)
                        sb.append(String.format("%.1fkm", rp.range));
                    // sb.append(String.format("  (%d)", rp.pid));
                    variantsCombo.add(sb.toString());
                }

                variantsCombo.addSelectionListener(new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent e) {
                        request.productCode = (short) currentPossibleProducts
                                .get(variantsCombo.getSelectionIndex()).pid;
                        onRequestChanged();
                    }
                });

                setupRow(r, l, variantsCombo);
            }
        }

        r = new Composite(rows, SWT.NONE);
        GridLayout grigl = new GridLayout(2, false);
        r.setLayout(grigl);

        validationImageLabel = new Label(r, SWT.CENTER);
        Image img = getErrorImage(validationImageLabel.getDisplay());
        GridData gd = new GridData(img.getBounds().width,
                img.getBounds().height);
        validationImageLabel.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        validationLabel = new Label(r, SWT.LEFT | SWT.WRAP);
        validationLabel.setLayoutData(gd);
        setupRow(r);

        return parameterUI;
    }

    private void createGeom(Composite rows, String labelText, String[] list,
            String buttonText) {
        Composite r = new Composite(rows, SWT.NONE);

        Composite r2 = new Composite(r, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.marginWidth = gl.marginHeight = 0;
        GridData gd;
        r2.setLayout(gl);
        Label l = new Label(r2, SWT.LEFT);
        l.setText(labelText);

        if (!isPoints) {
            Combo c = new Combo(r2, SWT.READ_ONLY);
            for (String s : list)
                c.add(s);
            gd = new GridData();
            gd.horizontalIndent = 20;
            c.setLayoutData(gd);
            // TODO: points=true/false .. means baseline/point are exclusive
            // (well, they are exclusive...)
            c.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent e) {
                    Combo c = (Combo) e.widget;
                    onGeomSelected(c.getItem(c.getSelectionIndex()));
                }
            });
            geomCombo = c;
        } else {
            final MenuButton mb = new MenuButton(r2);
            mb.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent e) {
                    MenuItem item = (MenuItem) e.data;
                    onGeomSelected(item.getText());
                }
            });
            mb.setMinimumSize(SWT.DEFAULT, SWT.DEFAULT);

            Menu menu = new Menu(mb);
            MenuItem firstItem = createMenuItems(menu, null);
            mb.setMenu(menu);
            mb.setSelectedItem(firstItem);

            final IPointChangedListener pointChangeListener = new IPointChangedListener() {

                @Override
                public void pointChanged() {
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            String name = mb.getSelectedItem().getText();
                            Menu menu = new Menu(mb);
                            MenuItem firstItem = createMenuItems(menu, null);
                            mb.setMenu(menu);
                            mb.setSelectedItem(name);
                            if (mb.getSelectedItem() == null) {
                                mb.setSelectedItem(firstItem);
                                onGeomSelected(firstItem.getText());
                            }
                        }
                    });
                }
            };

            PointsDataManager.getInstance().addPointsChangedListener(
                    pointChangeListener);
            geomCombo = mb;
            mb.addDisposeListener(new DisposeListener() {

                @Override
                public void widgetDisposed(DisposeEvent e) {
                    PointsDataManager.getInstance()
                            .removePointsChangedListener(pointChangeListener);
                }
            });
        }

        azRanLabel = new Label(r2, SWT.LEFT);
        if (isPoints)
            azRanLabel.setText("Azimuth: a.bcd nmi\nRange: e.fgh x");
        else
            azRanLabel
                    .setText("Length: 999.99 nmi\nAz/Ran\nA: 999/999\nA': 999/999");
        Point sz = azRanLabel.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        azRanLabel.setText("");
        gd = new GridData();
        gd.widthHint = sz.x;
        gd.heightHint = sz.y;
        azRanLabel.setLayoutData(gd);

        Button b = new Button(r, SWT.PUSH);
        b.setText(buttonText);
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                if (isPoints)
                    onLoadPoints();
                else
                    onLoadBaselines();
            }
        });
        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);

        // TODO: left side will not grab extra space... Needed for azRanLabel
        setupRow(r, r2, b);
    }

    private MenuItem createMenuItems(Menu menu, IPointNode root) {
        MenuItem item = null;
        MenuItem firstItem = null;
        for (IPointNode node : PointsDataManager.getInstance()
                .getChildren(root)) {
            if (node.isGroup()) {
                if (PointsDataManager.getInstance().getChildren(node).size() == 0) {
                    continue;
                }
                Menu childMenu = new Menu(menu);
                item = new MenuItem(menu, SWT.CASCADE);
                item.setMenu(childMenu);
                MenuItem it = createMenuItems(childMenu, node);
                if (firstItem == null) {
                    firstItem = it;
                }
            } else {
                item = new MenuItem(menu, SWT.PUSH);
                if (firstItem == null) {
                    firstItem = item;
                }
            }
            item.setText(node.getName());
            item.setData(node);
        }
        return firstItem;
    }

    protected void onLoadBaselines() {
        // Default implementation does nothing
    }

    protected void onLoadPoints() {
        // Default implementation does nothing
    }

    protected void onGeomSelected(String which) {
        int[] geom;

        desiredGeom = which;
        if (isPoints) {
            geom = getPoint(which);
            if (geom == null)
                return;
            request.setAzimuth(geom[0]);
            request.setRange(geom[1]);
        } else {
            geom = getBaseline(which);
            if (geom == null)
                return;
            request.setAzimuth(geom[0]);
            request.setRange(geom[1]);
            request.setAzimuth2(geom[2]);
            request.setRange2(geom[3]);
        }
        refreshAzRanLabel();
        onRequestChanged();
    }

    private void refreshAzRanLabel() {
        if (azRanLabel != null) {
            String text;
            if (isPoints) {
                // TODO: deg symbol
                // TODO: decimal places like AWIPS 1
                text = String.format("Azimuth: %.1f\u00b0\nRange: %.1f nmi",
                        (double) request.getAzimuth() / 10.0,
                        (double) request.getRange() / 10.0);
            } else {
                text = String
                        .format("Length: %.1f nmi\nAz/Ran\n%s: %.0f/%.0f\n%s': %.0f/%.0f",
                                getBaselineLength(), desiredGeom,
                                (double) request.getAzimuth() / 10.0,
                                (double) request.getRange() / 10.0,
                                desiredGeom,
                                (double) request.getAzimuth2() / 10.0,
                                (double) request.getRange2() / 10.0);
            }
            azRanLabel.setText(text);
        }
    }

    private void createTimeSpan(Composite rows, int maxSpan) {
        Composite r, r2, r3;
        Label l;
        Scale s;

        r = new Composite(rows, SWT.NONE);
        l = new Label(r, SWT.LEFT);
        l.setText("End Hour:");

        r2 = new Composite(r, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.marginLeft = rl.marginRight = rl.marginTop = rl.marginBottom = 0;
        r2.setLayout(rl);

        r3 = new Composite(r2, SWT.NONE);
        r3.setLayout(new RowLayout(SWT.VERTICAL));
        timeSpanType = new Button[2];
        SelectionListener sl = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                if (timeSpanType == null)
                    return;
                if (timeSpanType[0].getSelection()) {
                    request.setEndHour(-1);
                    timeSpan[0].setEnabled(false);
                } else {
                    timeSpan[0].setEnabled(true);
                    request.setEndHour(timeSpan[0].getScale().getSelection());
                }
                onRequestChanged();
            }
        };
        timeSpanType[0] = new Button(r3, SWT.RADIO);
        timeSpanType[0].setText("Most Recent");
        timeSpanType[0].addSelectionListener(sl);
        timeSpanType[1] = new Button(r3, SWT.RADIO);
        timeSpanType[1].setText("Selected");
        timeSpanType[1].addSelectionListener(sl);

        timeSpan = new ScaleWithLabel[2];
        timeSpan[0] = new ScaleWithLabel(r2, SWT.HORIZONTAL);
        s = timeSpan[0].getScale();
        s.setMinimum(0);
        s.setMaximum(23);
        s.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                request.setEndHour(timeSpan[0].getScale().getSelection());
                onRequestChanged();
            }
        });
        setupRow(r, l, r2);

        r = new Composite(rows, SWT.NONE);
        l = new Label(r, SWT.LEFT);
        l.setText("Time Span (Hours):");
        timeSpan[1] = new ScaleWithLabel(r, SWT.HORIZONTAL);
        s = timeSpan[1].getScale();
        s.setMinimum(1);
        s.setMaximum(maxSpan);
        s.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                request.setTimeSpan(timeSpan[1].getScale().getSelection());
                onRequestChanged();
            }
        });
        setupRow(r, l, timeSpan[1]);
    }

    private void fixHoursAndMinutesScales(ScaleWithLabel hScale,
            ScaleWithLabel mScale, int min, int max) {
        // Assumes hour-of-min != hour-of-max
        int minMinutes = 0;
        int maxMinutes = 59;
        hScale.getScale().setMinimum(min / 60);
        hScale.getScale().setMaximum(max / 60);
        if (hScale.getScale().getSelection() == min / 60)
            minMinutes = min % 60;
        else if (hScale.getScale().getSelection() == max / 60)
            maxMinutes = max % 60;
        if (minMinutes != maxMinutes) {
            mScale.getScale().setMinimum(minMinutes);
            mScale.getScale().setMaximum(maxMinutes);
            mScale.setEnabled(true);
        } else {
            /*
             * SWT does not allow the min and max of scale to be the same... So
             * just disable the scale...
             */
            mScale.getScale().setMinimum(minMinutes);
            mScale.getScale().setMaximum(minMinutes + 1);
            mScale.getScale().setSelection(minMinutes);
            mScale.setEnabled(false);
        }
        hScale.updateValue();
        mScale.updateValue();
    }

    /*
     * There are the values for the DUA, the only product that uses a
     * end/duration time in minutes.
     */
    private static final int END_TIME_MINUTES_MIN = 0;

    private static final int END_TIME_MINUTES_MAX = 1439;

    private static final int TIME_SPAN_MINUTES_MIN = 15;

    private static final int TIME_SPAN_MINUTES_MAX = 1440;

    private void fixHoursAndMinutesScales() {
        if (timeSpanType[1].getSelection())
            fixHoursAndMinutesScales(timeSpan[0], timeSpanMinutes[0],
                    END_TIME_MINUTES_MIN, END_TIME_MINUTES_MAX);
        fixHoursAndMinutesScales(timeSpan[1], timeSpanMinutes[1],
                TIME_SPAN_MINUTES_MIN, TIME_SPAN_MINUTES_MAX);
    }

    private void createTimeSpanMinutes(Composite rows) {
        Composite r, r2, r3, rx;
        Label l;
        Scale s;
        RowLayout rl;
        GridLayout gl;
        GridData gd;

        r = new Composite(rows, SWT.NONE);
        l = new Label(r, SWT.LEFT);
        l.setText("End Time:");

        rx = new Composite(r, SWT.NONE);
        gl = new GridLayout(1, false);
        gl.marginWidth = gl.marginHeight = 0;
        gl.verticalSpacing = 4;
        rx.setLayout(gl);

        r3 = new Composite(rx, SWT.NONE);
        r3.setLayout(new RowLayout(SWT.VERTICAL));
        timeSpanType = new Button[2];
        SelectionListener sl = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                if (timeSpanType == null)
                    return;
                if (timeSpanType[0].getSelection()) {
                    timeSpan[0].setEnabled(false);
                    timeSpanMinutes[0].setEnabled(false);
                } else {
                    timeSpan[0].setEnabled(true);
                    timeSpanMinutes[0].setEnabled(true);
                }
                setEndTimeMinutesFromUI();
                onRequestChanged();
            }

        };
        timeSpanType[0] = new Button(r3, SWT.RADIO);
        timeSpanType[0].setText("Most Recent");
        timeSpanType[0].addSelectionListener(sl);
        timeSpanType[1] = new Button(r3, SWT.RADIO);
        timeSpanType[1].setText("Selected");
        timeSpanType[1].addSelectionListener(sl);

        r2 = new Composite(rx, SWT.NONE);
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.marginLeft = rl.marginRight = rl.marginTop = rl.marginBottom = 0;
        r2.setLayout(rl);
        gd = new GridData();
        gd.horizontalIndent = 16;
        r2.setLayoutData(gd);

        timeSpan = new ScaleWithLabel[2];
        timeSpanMinutes = new ScaleWithLabel[2];

        sl = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                setEndTimeMinutesFromUI();
                onRequestChanged();
            }
        };
        Label l2 = new Label(r2, SWT.LEFT);
        l2.setText("Hour");
        timeSpan[0] = new ScaleWithLabel(r2, SWT.HORIZONTAL);
        s = timeSpan[0].getScale();
        s.setMinimum(0);
        s.setMaximum(23);
        s.addSelectionListener(sl);
        l2 = new Label(r2, SWT.LEFT);
        l2.setText("Min");
        timeSpanMinutes[0] = new ScaleWithLabel(r2, SWT.HORIZONTAL);
        s = timeSpanMinutes[0].getScale();
        s.setMinimum(0);
        s.setMaximum(59);
        s.addSelectionListener(sl);
        setupRow(r, l, rx);

        r = new Composite(rows, SWT.NONE);
        l = new Label(r, SWT.LEFT);
        l.setText("Time Span:");

        r2 = new Composite(r, SWT.NONE);
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.marginLeft = rl.marginRight = rl.marginTop = rl.marginBottom = 0;
        r2.setLayout(rl);

        sl = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                setTimeSpanMinutesFromUI();
                onRequestChanged();
            }
        };
        l2 = new Label(r2, SWT.LEFT);
        l2.setText("Hour");
        timeSpan[1] = new ScaleWithLabel(r2, SWT.HORIZONTAL);
        s = timeSpan[1].getScale();
        s.setMinimum(0);
        s.setMaximum(24);
        s.addSelectionListener(sl);
        l2 = new Label(r2, SWT.LEFT);
        l2.setText("Min");
        timeSpanMinutes[1] = new ScaleWithLabel(r2, SWT.HORIZONTAL);
        s = timeSpanMinutes[1].getScale();
        s.setMinimum(0);
        s.setMaximum(59);
        s.addSelectionListener(sl);
        setupRow(r, l, r2);
    }

    private void setEndTimeMinutesFromUI() {
        if (timeSpanType != null && timeSpan != null && timeSpanMinutes != null) {
            fixHoursAndMinutesScales();

            if (timeSpanType[0].getSelection())
                request.setEndHour(-1);
            else
                request.setEndHour(timeSpan[0].getScale().getSelection() * 60
                        + timeSpanMinutes[0].getScale().getSelection());
        }
    }

    private void setTimeSpanMinutesFromUI() {
        if (timeSpan != null && timeSpanMinutes != null) {
            fixHoursAndMinutesScales();
            request.setTimeSpan(timeSpan[1].getScale().getSelection() * 60
                    + timeSpanMinutes[1].getScale().getSelection());
        }
    }

    private boolean variationsEqual(Object a, Object b) {
        if ((a == null) != (b == null))
            return false;
        if (a != null && b != null)
            return a.equals(b);
        else
            return true;
    }

    private ScaleWithLabel createAltitude(Composite row, String string,
            int min, int max) {
        Label l = new Label(row, SWT.LEFT);
        l.setText(string);
        ScaleWithLabel result = new ScaleWithLabel(row, SWT.HORIZONTAL);
        Scale s = result.getScale();
        s.setMinimum(min);
        s.setMaximum(max);
        result.setScaleLength(70 * 3);
        result.setLabelSuffix(" kft");

        setupRow(row, l, result);
        return result;
    }

    private ScaleWithLabel[] createAzRan(Label l, Composite row) {
        ScaleWithLabel[] result = new ScaleWithLabel[2];
        Composite tb = new Composite(row, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.VERTICAL);
        Scale s;
        rl.marginTop = rl.marginBottom = 0;
        rl.marginLeft = rl.marginRight = rl.marginTop = rl.marginBottom = 0;
        rl.center = true;
        tb.setLayout(rl);

        result[0] = new ScaleWithLabel(tb, SWT.HORIZONTAL);
        s = result[0].getScale();
        s.setMinimum(0);
        s.setMaximum(3599);
        result[0].setScaleLength(360);
        result[0].setLabelScale(0.1);
        result[0].setLabelSuffix(" deg");

        result[1] = new ScaleWithLabel(tb, SWT.HORIZONTAL);
        s = result[1].getScale();
        s.setMinimum(0);
        s.setMaximum(1240);
        result[1].setScaleLength(360);
        result[1].setLabelScale(0.1);
        result[1].setLabelSuffix(" Nmi");

        setupRow(row, l, tb);

        return result;
    }

    private void fillInGrid(Control c, int hAlign) {
        GridData gd = new GridData(hAlign, SWT.CENTER, true, false);
        c.setLayoutData(gd);
    }

    private void fillInGrid(Control c) {
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        c.setLayoutData(gd);
    }

    private void createSpeedDir(Composite rows, String defaultText) {
        speedDir = new ScaleWithLabel[2];
        defaultSpeedDirButton = new Button(rows, SWT.CHECK);
        defaultSpeedDirButton.setText(defaultText);
        defaultSpeedDirButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                // AWIPS-1 does not disable the controls
                if (defaultSpeedDirButton.getSelection()) {
                    speedDir[0].setEnabled(false);
                    speedDir[1].setEnabled(false);
                    request.setStormSpeed(-1);
                    request.setStormDirection(0);
                } else {
                    speedDir[0].setEnabled(true);
                    speedDir[1].setEnabled(true);
                    request.setStormSpeed(speedDir[0].getScale().getSelection());
                    request.setStormDirection(speedDir[1].getScale()
                            .getSelection());
                }
                onRequestChanged();
            }
        });
        fillInGrid(defaultSpeedDirButton, SWT.LEFT);

        Label l;
        Scale s;
        l = new Label(rows, SWT.LEFT);
        l.setText("Speed (kts) - Used for Dedicated RPGs Only");
        fillInGrid(l, SWT.LEFT);
        ((GridData) l.getLayoutData()).verticalIndent = 10; // TODO: cleaner

        speedDir[0] = new ScaleWithLabel(rows, SWT.HORIZONTAL);
        s = speedDir[0].getScale();
        s.setMinimum(0);
        s.setMaximum(990);
        s.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                request.setStormSpeed(speedDir[0].getScale().getSelection());
                onRequestChanged();
            }
        });
        speedDir[0].setLabelScale(0.1);
        fillInGrid(speedDir[0]);
        // speedDir[0].setLabelSuffix(" kts");

        l = new Label(rows, SWT.LEFT);
        l.setText("Direction (deg) - Used for Dedicated RPGs Only");
        fillInGrid(l, SWT.LEFT);

        speedDir[1] = new ScaleWithLabel(rows, SWT.HORIZONTAL);
        s = speedDir[1].getScale();
        s.setMinimum(0);
        s.setMaximum(3599);
        s.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                request.setStormDirection(speedDir[1].getScale().getSelection());
                onRequestChanged();
            }
        });
        speedDir[1].setLabelScale(0.1);
        fillInGrid(speedDir[1]);
        // speedDir[1].setLabelSuffix(" deg");
    }

    enum ElevationSelection {
        ALL("All"), LOWEST_N("Lowest N"), UP_TO("All at or below"), SINGLE(
                "Single"/* "Selected" */), ALL_MATCHING("All cuts");
        public final String label;

        private ElevationSelection(String label) {
            this.label = label;
        }
    }

    private static final ElevationSelection[] wsrElevationSelections = {
            ElevationSelection.ALL, ElevationSelection.UP_TO,
            ElevationSelection.LOWEST_N, ElevationSelection.SINGLE };

    private static final ElevationSelection[] tdwrElevationSelections = {
            ElevationSelection.SINGLE, ElevationSelection.ALL_MATCHING };

    private ElevationSelection[] elevationSelections;

    private void createElevationUI(Composite rows) {
        Composite c = new Composite(rows, SWT.NONE);
        // c.setBackground(rows.getDisplay().getSystemColor(SWT.COLOR_CYAN));
        setupRow(c);
        GridLayout gl = new GridLayout(1, false);
        GridData gd;

        c.setLayout(gl);
        Label l = new Label(c, SWT.CENTER);
        l.setText("Elevation(s):");
        gd = new GridData();
        gd.horizontalAlignment = SWT.LEFT;

        Composite bot = new Composite(c, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        bot.setLayout(rl);
        // bot.setBackground(rows.getDisplay().getSystemColor(SWT.COLOR_GREEN));
        Combo cmb = new Combo(bot, SWT.READ_ONLY);

        if (radarType == RadarType.TDWR)
            elevationSelections = tdwrElevationSelections;
        else
            elevationSelections = wsrElevationSelections;

        for (ElevationSelection es : elevationSelections)
            cmb.add(es.label);
        // don't select current, will come from request...

        // for TDWR, also add [x] All mini-volumes
        // cmb.select(3);
        cmb.setSize(cmb.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        cmb.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onElevationTypeSelected();
            }
        });
        Combo cmb2 = new Combo(bot, SWT.READ_ONLY);
        cmb2.add("999.9");
        cmb2.select(0);
        cmb2.setSize(cmb.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        elevationTypeCombo = cmb;
        elevationAnglesCombo = cmb2;
        cmb2.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onElevationAngleSelected();
            }
        });
        gd = new GridData();
        gd.horizontalAlignment = SWT.RIGHT;
        gd.grabExcessHorizontalSpace = true;
        bot.setLayoutData(gd);
    }

    protected void onElevationTypeSelected() {
        if (elevationAnglesCombo != null) {
            elevationAnglesCombo.removeAll();

            // elevationAnglesCombo.select(-1);
            elevationAnglesCombo.setEnabled(false);

            int[] angles = getElevationAngles();
            boolean selOK = false;
            boolean reset = false;

            int ei = elevationTypeCombo.getSelectionIndex();
            if (ei < 0)
                return;
            ElevationSelection es = elevationSelections[ei];

            if (es == ElevationSelection.ALL) {
                // nothing -- leave disabled;
            } else if (es == ElevationSelection.ALL_MATCHING
                    || es == ElevationSelection.SINGLE
                    || es == ElevationSelection.UP_TO) {
                if (angles != null) {
                    for (int i = 0; i < angles.length; ++i) {
                        float fAngle = (float) angles[i] / 10.0f;
                        elevationAnglesCombo.add(Float.toString(fAngle));
                    }
                }
                selOK = true;
                reset = request.getElevationSelection() == Request.N_ELEVATIONS
                        || (request.getElevationSelection() == Request.ALL_ELEVATIONS && request
                                .getElevationAngle() == 0);
            } else if (es == ElevationSelection.LOWEST_N) {
                // should be a slider?
                if (angles != null) {
                    for (int i = 1; i <= angles.length; ++i) {
                        elevationAnglesCombo.add(Integer.toString(i));
                    }
                }
                selOK = true;
                reset = request.getElevationSelection() != Request.N_ELEVATIONS;
            }

            elevationAnglesCombo.setEnabled(selOK);

            if (selOK && elevationAnglesCombo.getItemCount() > 0 && reset)
                elevationAnglesCombo.select(0);
            else
                setElevationAngleComboFromRequest();

            onElevationAngleSelected(); // updates the request object
            // TODO: Also need to document above that this line depends
            // on how the UI is created. On getParent() alone does not work...
            // elevationAnglesCombo.getParent().getParent().layout(true);
        }
    }

    protected void onElevationAngleSelected() {
        if (elevationAnglesCombo != null && elevationTypeCombo != null) {
            int ti = elevationTypeCombo.getSelectionIndex();
            int ai = elevationAnglesCombo.getSelectionIndex();
            int[] angles = getElevationAngles();
            ElevationSelection es;

            if (angles == null || angles.length == 0)
                return;

            if (ti >= 0)
                es = elevationSelections[ti];
            else
                return;

            if (es == ElevationSelection.ALL)
                request.selectAllElevations();
            else if (es == ElevationSelection.LOWEST_N)
                request.selectLowerCuts(ai + 1);
            else {
                int angle;
                try {
                    angle = angles[ai];
                } catch (ArrayIndexOutOfBoundsException e) {
                    return;
                }
                if (es == ElevationSelection.ALL_MATCHING)
                    request.selectAllElevations(angle);
                else if (es == ElevationSelection.SINGLE)
                    request.setElevationAngle(angle);
                else if (es == ElevationSelection.UP_TO)
                    request.selectLowerEelevations(angle);
            }
            onRequestChanged();
        }
    }

    public void setRadar(RadarConfig rc, GSM gsm) {
        int vcp = -1;

        if (gsm != null) {
            vcp = gsm.vcp;

            liveVCP = vcp;
            liveRadarID = rc.getRadarID();
            liveElevationAngles = gsm.cuts;
        }
        // else note radar as "down" (may not be down.. could be ini connect)

        lockUIChange();
        try {
            setRadarConfig(rc);
            setVcp(vcp);
        } finally {
            // OTOH, if there is an error we may make things worse by
            // changing the ui...
            unlockUIChange();
        }
    }

    public String getRadarID() {
        return radarID;
    }

    public void setRadarID(String radarID) {
        this.radarID = radarID;
        requestUIChange();

        /*
         * Changing the radar means the azimuth/range values for points and
         * baseline change.
         */
        if (geomCombo != null)
            onGeomSelected(desiredGeom);
    }

    public int getVcp() {
        return vcp;
    }

    public void setVcp(int vcp) {
        this.vcp = vcp;
        requestUIChange();
    }

    private int[] getElevationAngles() {
        if (radarID != null && radarID.equals(liveRadarID) && liveVCP == vcp)
            return liveElevationAngles;
        else
            return ElevationInfo.getInstance()
                    .getUniqueElevations(radarID, vcp);
    }

    private void setupRow(Control c) {
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        c.setLayoutData(gd);
    }

    private void setupRow(Composite row, Control left, Control right) {
        GridData gd;

        setupRow(row);
        row.setLayout(new GridLayout(2, false));
        if (left != null) {
            gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
            left.setLayoutData(gd);
        }
        if (right != null) {
            gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
            right.setLayoutData(gd);
        }
    }

    private void disposeParameterUI() {
        if (parameterUI != null) {
            parameterUI.dispose();
            parameterUI = null;
            priorityCombo = null;
            /*
             * requestIntervalSlider = null; requestIntervalLabel = null;
             */
            requestIntervalScale = null;
            elevationTypeCombo = null;
            elevationAnglesCombo = null;
            azRan1 = null;
            azRan2 = null;
            azRanLabel = null;
            geomCombo = null;
            defaultSpeedDirButton = null;
            speedDir = null;
            altitudeScale = null;
            bottomAltitude = null;
            topAltitude = null;
            timeSpanType = null;
            timeSpan = null;
            timeSpanMinutes = null;
            cfcSegment = null;
            variantsCombo = null;

            validationLabel = null;
            validationImageLabel = null;
        }

    }

    private int findElevationSelection(ElevationSelection es) {
        if (elevationSelections != null)
            for (int i = 0; i < elevationSelections.length; ++i)
                if (elevationSelections[i] == es)
                    return i;
        return -1;
    }

    private int findRequestElevationSelectionIndex() {
        int i = -1;
        switch (request.getElevationSelection()) {
        case Request.ALL_ELEVATIONS:
            i = findElevationSelection(ElevationSelection.ALL);
            if (i < 0)
                i = findElevationSelection(ElevationSelection.ALL_MATCHING);
            break;
        case Request.LOWER_ELEVATIONS:
            i = findElevationSelection(ElevationSelection.UP_TO);
            break;
        case Request.N_ELEVATIONS:
            i = findElevationSelection(ElevationSelection.LOWEST_N);
            break;
        case Request.SPECIFIC_ELEVATION:
            i = findElevationSelection(ElevationSelection.SINGLE);
            break;
        }
        return i;
    }

    private void setUIFromRequest() {
        // TODO: setting -1 etc. means we have validation errors

        if (priorityCombo != null)
            priorityCombo.select(request.highPriority ? 1 : 0);

        if (requestIntervalScale != null)
            requestIntervalScale.setSelection(request.interval);

        if (elevationTypeCombo != null) {
            int i = findRequestElevationSelectionIndex();
            if (i >= 0)
                elevationTypeCombo.select(i);
            else {
                // This effectively clears the Combo (for GTK, at least).
                elevationTypeCombo.add("(Invalid)");
                i = elevationTypeCombo.getItemCount() - 1;
                elevationTypeCombo.select(i);
                elevationTypeCombo.remove(i);
            }
            onElevationTypeSelected(); // Also enables/disables elevation angle
            // combo and sets its value
        }

        if (geomCombo != null) {
            if (geomCombo instanceof Combo) {
                Combo c = (Combo) geomCombo;
                int index = c.indexOf(desiredGeom);
                if (index != -1) {
                    c.select(index);
                }
            } else {
                MenuButton mb = (MenuButton) geomCombo;

                // Check to see if desired point still exists.
                MenuItem item = mb.getSelectedItem();
                mb.setSelectedItem(desiredGeom);
                if (mb.getSelectedItem() == null) {
                    mb.setSelectedItem(item);
                    onGeomSelected(item.getText());
                }
            }

        }
        refreshAzRanLabel();

        // TODO: Not used as long we use CAVE baselines/points
        if (azRan1 != null) {
            azRan1[0].setSelection(request.getAzimuth());
            azRan1[1].setSelection(request.getRange());
        }
        if (azRan2 != null) {
            azRan2[0].setSelection(request.getAzimuth2());
            azRan2[1].setSelection(request.getRange2());
        }

        if (defaultSpeedDirButton != null) {
            boolean sel = request.getStormSpeed() == -1;
            defaultSpeedDirButton.setSelection(sel);
            if (speedDir != null) {
                speedDir[0].setEnabled(!sel);
                speedDir[1].setEnabled(!sel);
            }
        }
        if (speedDir != null) {
            speedDir[0].setSelection(request.getStormSpeed());
            speedDir[1].setSelection(request.getStormDirection());
        }

        if (altitudeScale != null)
            altitudeScale.setSelection(request.getAltitude());

        if (topAltitude != null)
            topAltitude.setSelection(request.getTopAltitude());
        if (bottomAltitude != null)
            bottomAltitude.setSelection(request.getBottomAltitude());

        if (timeSpanType != null) {
            boolean auto = request.getEndHour() == -1;
            timeSpanType[0].setSelection(auto);
            timeSpanType[1].setSelection(!auto);
            if (timeSpan != null)
                timeSpan[0].getScale().setEnabled(!auto);
            if (timeSpanMinutes != null)
                timeSpanMinutes[0].getScale().setEnabled(!auto);
        }

        if (timeSpanMinutes != null) {
            int endTimeVal = request.getEndHour();
            int timeSpanVal = request.getTimeSpan();
            timeSpanMinutes[0].setSelection(endTimeVal % 60);
            timeSpanMinutes[1].setSelection(timeSpanVal % 60);
            if (timeSpan != null) {
                timeSpan[0].setSelection(endTimeVal / 60);
                timeSpan[1].setSelection(timeSpanVal / 60);
            }
            fixHoursAndMinutesScales();
        } else if (timeSpan != null) {
            timeSpan[0].setSelection(request.getEndHour());
            timeSpan[1].setSelection(request.getTimeSpan());
        }

        if (cfcSegment != null) {
            // TODO: Are multiple selections allowed or not?
            int i;
            for (i = 0; i < 5; ++i)
                if ((request.getCfcWord() & (1 << (i + 1))) != 0)
                    break;
            cfcSegment.select(i < 5 ? i : -1);
        }
        if (variantsCombo != null) {
            int i;
            for (i = 0; i < currentPossibleProducts.size(); ++i)
                if (currentPossibleProducts.get(i).pid == request.productCode)
                    break;
            variantsCombo.select(i < currentPossibleProducts.size() ? i : -1);
        }

        setVolumeScanSelectionUIFromRequest();
        refreshRepeatCountUI();
    }

    private void setElevationAngleComboFromRequest() {
        ElevationSelection es = getElevationSelectionFromUI();
        if (elevationAnglesCombo != null && es != null) {
            int angle = request.getElevationAngle(); // or number of angles

            if (es == ElevationSelection.LOWEST_N) {
                // TODO: error handling...
                elevationAnglesCombo.select(angle - 1);
            } else if (es != ElevationSelection.ALL) {
                int i;
                int[] elevs = getElevationAngles();
                if (elevs != null) {
                    i = Arrays.binarySearch(elevs, angle);
                    if (i < 0)
                        i = -1;
                } else
                    i = -1;
                elevationAnglesCombo.select(i);
            }
        }

    }

    private ElevationSelection getElevationSelectionFromUI() {
        if (elevationTypeCombo != null) {
            int i = elevationTypeCombo.getSelectionIndex();
            if (i >= 0)
                return elevationSelections[i];
        }
        return null;
    }

    public boolean isPrioritySelectionEnabled() {
        return prioritySelectionEnabled;
    }

    public void setPrioritySelectionEnabled(boolean prioritySelectionEnabled) {
        this.prioritySelectionEnabled = prioritySelectionEnabled;
        createParameterUI();
    }

    public boolean isVolumeScanSelectionEnabled() {
        return volumeScanSelectionEnabled;
    }

    public void setVolumeScanSelectionEnabled(boolean volumeScanSelectionEnabled) {
        this.volumeScanSelectionEnabled = volumeScanSelectionEnabled;

        if (volumeScanSelectionEnabled) {
            if (vsSelectionUI == null)
                createVolumeScanSelectionUI();

            setVolumeScanSelectionUIFromRequest();
        } else {
            disposeVolumeScanSelectionUI();
        }
    }

    private Calendar createCurrentTime() {
        return new GregorianCalendar(new SimpleTimeZone(0, "GMT"));
    }

    private void setVolumeScanSelectionUIFromRequest() {
        if (vsSelectionUI != null) {
            for (int i = 0; i < vsSelectionButtons.length; ++i) {
                vsSelectionButtons[i]
                        .setSelection((Integer) vsSelectionButtons[i].getData() == request
                                .getVolumeScanSelection());
            }

            String s;
            if (request.getVolumeScanSelection() == Request.SELECT_SPECIFIC)
                s = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tSZ",
                        request.getVolumeScanTime());
            else
                s = "";
            selectedVSTimeLabel.setText(s);
            selectedVSTimeLabel.getParent().layout();
        }
    }

    /**
     * Responds to selection of one of the volume scan selection radio buttons.
     * <p>
     * Because a repeat count greater than one only makes sense for the
     * "Current" setting, this also affects the repeat count.
     */
    private void onTimeTypeSelected(int volumeScanSelection) {
        switch (volumeScanSelection) {
        case Request.SELECT_CURRENT:
            request.count = desiredRepeatCount;
            request.selectCurrent();
            break;
        case Request.SELECT_LATEST:
            request.count = 1;
            request.selectLatest();
            break;
        case Request.SELECT_SPECIFIC:
            Calendar cal = request.getVolumeScanTime();
            if (cal == null)
                cal = createCurrentTime();
            request.count = 1;
            request.selectTime(cal);
            break;
        }
        onRequestChanged();
        setVolumeScanSelectionUIFromRequest();
        refreshRepeatCountUI();
    }

    private void onChangeVolumeScanTime() {
        VolumeScanTimeSelectionDialog dlg = new VolumeScanTimeSelectionDialog(
                productRequestUI.getShell());
        if (dlg.open()) {
            // NOTE: Forces repeat count to one.
            request.count = 1;
            request.selectTime(dlg.getTime());
            setVolumeScanSelectionUIFromRequest();
            refreshRepeatCountUI();
            onRequestChanged();
        }
    }

    private void createVolumeScanSelectionUI() {
        if (productRequestUI == null)
            return;

        vsSelectionSeparator = new Label(productRequestUI, SWT.SEPARATOR
                | SWT.HORIZONTAL);
        vsSelectionSeparator.setLayoutData(new GridData(SWT.FILL, SWT.FILL,
                true, false));
        Composite p, c;
        vsSelectionUI = p = new Composite(productRequestUI, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        RowLayout rl;

        p.setLayout(gl);
        Label l;

        l = new Label(p, SWT.LEFT);
        l.setText("Time:");

        c = new Composite(p, SWT.NONE);
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.spacing = 10;
        c.setLayout(rl);

        Button b;

        final String[] labels = { "Current", "Latest", "Selected" };
        final int[] values = { Request.SELECT_CURRENT, Request.SELECT_LATEST,
                Request.SELECT_SPECIFIC };
        vsSelectionButtons = new Button[labels.length];
        for (int i = 0; i < labels.length; ++i) {
            b = new Button(c, SWT.RADIO);
            b.setText(labels[i]);
            b.setData(values[i]);
            b.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent e) {
                    Button b = (Button) e.widget;
                    if (b.getSelection())
                        onTimeTypeSelected((Integer) b.getData());
                }

            });
            vsSelectionButtons[i] = b;
        }

        l = new Label(p, SWT.LEFT);
        l.setText("Selected Time:");

        c = new Composite(p, SWT.NONE);
        c.setLayout(new RowLayout(SWT.HORIZONTAL));
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.spacing = 32;
        rl.center = true;
        c.setLayout(rl);
        MinimumSizeComposite msc = new MinimumSizeComposite(c);
        selectedVSTimeLabel = new Label(msc, SWT.LEFT);
        selectedVSTimeLabel.setText("9999-99-99 99:99:99ZXX");
        msc.setMinimumSize(selectedVSTimeLabel.computeSize(SWT.DEFAULT,
                SWT.DEFAULT));

        b = new Button(c, SWT.PUSH);
        b.setText("Change...");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onChangeVolumeScanTime();
            }
        });
    }

    private void disposeVolumeScanSelectionUI() {
        if (vsSelectionUI != null) {
            vsSelectionUI.dispose();
            vsSelectionSeparator.dispose();
            vsSelectionUI = null;
            vsSelectionSeparator = null;
            vsSelectionButtons = null;
        }
    }

    public boolean isRepeatCountEnabled() {
        return repeatCountEnabled;
    }

    public void setRepeatCountEnabled(boolean repeatCountEnabled) {
        this.repeatCountEnabled = repeatCountEnabled;

        if (repeatCountEnabled) {
            if (repeatCountUI == null)
                createRepeatCountUI();
            refreshRepeatCountUI();
        } else {
            disposeRepeatCountUI();
        }
    }

    /**
     * <p>
     * This is just another request parameter, but the AWIPS 1 UI puts it at the
     * top left.
     */
    private void createRepeatCountUI() {
        disposeRepeatCountUI();

        if (repeatCountParent != null) {
            Composite row = new Composite(repeatCountParent, SWT.NONE);
            RowLayout rl = new RowLayout(SWT.HORIZONTAL);
            rl.center = true;
            row.setLayout(rl);

            Label l = new Label(row, SWT.LEFT);
            l.setText("Repeat count: ");
            l.setAlignment(SWT.CENTER);

            Combo cmb = new Combo(row, SWT.READ_ONLY);
            cmb.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent e) {
                    onRepeatCountSelected();
                }
            });
            for (int i = 1; i <= 9; ++i) {
                cmb.add(Integer.toString(i));
            }

            repeatCountCombo = cmb;
            repeatCountUI = row;

            refreshRepeatCountUI();
        }
    }

    private void disposeRepeatCountUI() {
        if (repeatCountUI != null) {
            repeatCountUI.dispose();
            repeatCountUI = null;
            repeatCountCombo = null;
        }
    }

    private void refreshRepeatCountUI() {
        if (repeatCountCombo != null) {
            repeatCountCombo
                    .setEnabled(request.getVolumeScanSelection() == Request.SELECT_CURRENT);
            repeatCountCombo.select(request.count - 1); // TODO: ...
        }
    }

    public boolean isRpgSelectorEnabled() {
        return rpgSelectorEnabled;
    }

    public void setRpgSelectorEnabled(boolean rpgSelectorEnabled) {
        this.rpgSelectorEnabled = rpgSelectorEnabled;

        if (rpgSelectorEnabled) {
            if (rpgSelectorUI == null)
                createRPGSelectorUI();
            refreshRPGSelectorUI();
        } else {
            disposeRPGSelectorUI();
        }
    }

    public String getSelectedRpg() {
        if (rpgSelectorUI != null)
            return rpgSelectorUI.getSelectedRPG();
        else
            return null;
    }

    public ExtProductsUI getRpgSelector() {
        return rpgSelectorUI;
    }

    private void disposeRPGSelectorUI() {
        if (rpgSelectorUI != null) {
            rpgSelectorUI.dispose();
            rpgSelectorUI = null;
        }
    }

    private void refreshRPGSelectorUI() {
        // TODO: What to do? Set from our current config?
    }

    private void createRPGSelectorUI() {
        if (rpgSelectorParent != null) {
            rpgSelectorUI = new ExtProductsUI(rpgSelectorParent);
            rpgSelectorUI.getSelectionProvider().addSelectionChangedListener(
                    new ISelectionChangedListener() {
                        @Override
                        public void selectionChanged(SelectionChangedEvent event) {
                            onRpgSelected(event);
                        }
                    });
        }
    }

    private void onRpgSelected(SelectionChangedEvent event) {
        RadarConfig rc;
        try {
            rc = (RadarConfig) ((IStructuredSelection) rpgSelectorUI
                    .getSelectionProvider().getSelection()).getFirstElement();
        } catch (Exception e) {
            return;
        }
        if (rc != null) {
            GSM gsm = null;
            try {
                GetRadarStatusMessages req = new GetRadarStatusMessages();
                req.radarID = rc.getRadarID();
                StatusMessagesReply reply = (StatusMessagesReply) RadarApps
                        .getRcmSystem().sendCheckedAndHandled(req,
                                rpgSelectorUI.getShell());
                if (reply != null && reply.status != null
                        && reply.status.size() > 0) {
                    ROStatus status = reply.status.iterator().next();
                    if (status.currentGSM != null)
                        gsm = GSM.decode(status.currentGSM);
                }
            } catch (com.raytheon.rcm.message.MessageFormatException e) {
                // nothing
            }
            setRadar(rc, gsm);
        }
    }

    public Usage getUsage() {
        return usage;
    }

    public void setUsage(Usage usage) {
        this.usage = usage;
        requestUIChange();
    }

    /**
     * Get an array of baseline names to process.
     * 
     * @return baselineNames
     */
    abstract protected String[] getBaselineList();

    /**
     * Get point's azimuth and range from the radar.
     * 
     * @param which
     *            - point name
     * @return result - [0] azimuth of point, [1] range of point; or null if
     *         unable to determine
     */
    abstract protected int[] getPoint(String which);

    /**
     * Get the baseline's azimuth and range from the radar for the line's start
     * and end point.
     * 
     * @param which
     *            - baseline name
     * @return result - [0] azimuth start point, [1] range start point, [2]
     *         azimuth end point, [3] range end point; or null if unable to
     *         determine
     */
    abstract protected int[] getBaseline(String which);

    /**
     * Get the radar's location.
     * 
     * @param radar
     * @return array - [0] latitude, [1] longitude, [3] elevation
     */
    abstract protected float[] getRadarLocation(String radar);

    /**
     * Get point location
     * 
     * @param which
     *            - point name
     * @return array - [0] latitude, [1] longitude
     */
    abstract protected float[] getPointLatLon(String which);

    /**
     * Get location of baseline's start and end points.
     * 
     * @param which
     *            - baseline name
     * @return array - [0] start latitude, [1] start longitude, [2] end
     *         latitude, [3] end longitude
     */
    abstract protected float[] getBaselineLatLon(String which);
}
