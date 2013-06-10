/*
 * gov.noaa.nws.ncep.ui.pgen.tools.GfaFormatAttrDlg
 * 
 * June 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.BOS;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.CHI;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.DFW;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.MIA;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.SFO;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.SLC;
import static org.eclipse.jface.dialogs.IDialogConstants.CANCEL_ID;
import static org.eclipse.jface.dialogs.IDialogConstants.CANCEL_LABEL;
import static org.eclipse.jface.dialogs.IDialogConstants.OK_ID;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenGfaFormatTool;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.JAXBException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Create a dialog for PGEN format action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/10		#223		M.Laryukhin	Initial creation
 * 07/11		?			B. Yin		Use fixed font for text message.
 * 04/29        #977        S. Gilbert  PGEN Database support
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 */
public class GfaFormatAttrDlg extends AttrDlg {

    // private final static Logger logger =
    // Logger.getLogger(GfaFormatAttrDlg.class);

    private static final String ZULU = "ZULU";

    private static final String TANGO = "TANGO";

    private static final String SIERRA = "SIERRA";

    private static final String EAST = "EAST";

    private static final String CENTRAL = "CENTRAL";

    private static final String WEST = "WEST";

    static GfaFormatAttrDlg instance;

    private Composite top;

    // radio buttons
    private Button nrmlBtn;

    private Button testBtn;

    private static final int BTN_WIDTH = 90;

    private static final int BTN_HEIGHT = 23;

    private static final int TEXT_WIDTH = 660;

    private static final int TEXT_HEIGHT = 300;

    static private Font txtFt = null;

    private static final String SAVE_LABEL = "Generate/Save";

    // checkboxes
    private Button westBtn, slcBtn, sfoBtn;

    private Button centralBtn, chiBtn, dfwBtn;

    private Button eastBtn, bosBtn, miaBtn;

    private Button sierraBtn, tangoBtn, zuluBtn;

    // last used
    private boolean lastNrml = true;

    private boolean lastWest, lastSlc, lastSfo, lastCentral, lastChi, lastDfw;

    private boolean lastEast, lastBos, lastMia, lastSierra, lastTango,
            lastZulu;

    // text area
    private Text text;

    private PgenGfaFormatTool pgenGfaFormatTool;

    /**
     * Private constructor
     * 
     * @param parShell
     * @throws VizException
     */
    private GfaFormatAttrDlg(Shell parShell) throws VizException {

        super(parShell);

    }

    /**
     * Creates an extrapolation dialog if the dialog does not exist and returns
     * the instance. If the dialog exists, return the instance.
     * 
     * @param parShell
     * @return
     */
    public static GfaFormatAttrDlg getInstance(Shell parShell) {
        if (instance == null) {
            try {
                instance = new GfaFormatAttrDlg(parShell);

            } catch (VizException e) {
                // logger.error(e);
                e.printStackTrace();
            }
        }

        return instance;
    }

    /*
     * (non-Javadoc) Create all of the widgets on the Dialog
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {

        top = (Composite) super.createDialogArea(parent);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        // Initialize all of the menus, controls, and layouts
        initializeComponents();

        return top;
    }

    /**
     * Creates buttons, menus, and other controls in the dialog area
     * 
     * @param listener
     */
    private void initializeComponents() {

        this.getShell().setText("AIRMET Format");

        createFirstRowBtns();

        createWCEBtns();

        createSierraTangoZuluBtns();

        createTextArea();

        addSelectionlisteners();
    }

    private void createFirstRowBtns() {
        Composite comp = createComposite();

        nrmlBtn = new Button(comp, SWT.RADIO);
        nrmlBtn.setSelection(lastNrml);
        nrmlBtn.setText("NRML");
        testBtn = new Button(comp, SWT.RADIO);
        testBtn.setSelection(!lastNrml);
        testBtn.setText("TEST");
    }

    private Composite createComposite() {
        // centered, bordered, filled to the borders
        Group group = new Group(top, SWT.NONE);
        GridData gridData = new GridData(GridData.BEGINNING);
        gridData.horizontalAlignment = GridData.FILL;
        group.setLayoutData(gridData);
        GridLayout layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        group.setLayout(layout);

        Composite comp = new Composite(group, SWT.NONE);
        layout = new GridLayout(3, false);
        layout.marginHeight = 3;
        layout.marginWidth = 3;
        comp.setLayout(layout);
        comp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        return comp;
    }

    private void createWCEBtns() {

        Composite comp = createComposite();

        westBtn = createCheckBtn(comp, WEST, lastWest);
        centralBtn = createCheckBtn(comp, CENTRAL, lastCentral);
        eastBtn = createCheckBtn(comp, EAST, lastEast);
        slcBtn = createCheckBtn(comp, SLC, lastSlc);
        chiBtn = createCheckBtn(comp, CHI, lastChi);
        bosBtn = createCheckBtn(comp, BOS, lastBos);
        sfoBtn = createCheckBtn(comp, SFO, lastSfo);
        dfwBtn = createCheckBtn(comp, DFW, lastDfw);
        miaBtn = createCheckBtn(comp, MIA, lastMia);
    }

    private Button createCheckBtn(Composite comp, String str, boolean lastUsed) {
        Button btn = new Button(comp, SWT.CHECK);
        btn.setText(str);
        GridData gd = new GridData(BTN_WIDTH, BTN_HEIGHT);
        btn.setLayoutData(gd);
        btn.setSelection(lastUsed);
        return btn;
    }

    private void createSierraTangoZuluBtns() {
        Composite comp = createComposite();

        sierraBtn = createCheckBtn(comp, SIERRA, lastSierra);
        tangoBtn = createCheckBtn(comp, TANGO, lastTango);
        zuluBtn = createCheckBtn(comp, ZULU, lastZulu);
    }

    private void createTextArea() {
        int style = SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.WRAP;
        text = new Text(top, style);
        GridData gd = new GridData(TEXT_WIDTH, TEXT_HEIGHT);
        gd.verticalAlignment = GridData.BEGINNING;
        text.setLayoutData(gd);
        text.setEditable(false);
        text.addTraverseListener(new GfaAttrDlg.TraverseListenerTab());

        if (txtFt == null) {
            txtFt = new Font(this.getShell().getDisplay(), "Courier New", 12,
                    SWT.NORMAL);
        }
        text.setFont(txtFt);
    }

    private void addSelectionlisteners() {
        new ChkBtnSelectionListener(westBtn, slcBtn, sfoBtn);
        new ChkBtnSelectionListener(centralBtn, chiBtn, dfwBtn);
        new ChkBtnSelectionListener(eastBtn, bosBtn, miaBtn);

        // updateLastUsedListener
        SelectionListener listener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateLastUsed();
            }
        };

        nrmlBtn.addSelectionListener(listener);
        testBtn.addSelectionListener(listener);

        sierraBtn.addSelectionListener(listener);
        tangoBtn.addSelectionListener(listener);
        zuluBtn.addSelectionListener(listener);
    }

    @Override
    public void createButtonsForButtonBar(Composite parent) {
        createButton(parent, OK_ID, SAVE_LABEL, true);
        createButton(parent, CANCEL_ID, CANCEL_LABEL, false);
    }

    @Override
    public void okPressed() {
        // do not close here, just generate

        String dataURI = null;
        ArrayList<String> areas = getChecked();

        ArrayList<String> categories = getSelectedCategories();

        ArrayList<AbstractDrawableComponent> all = new ArrayList<AbstractDrawableComponent>();
        Product prod = null;
        if (drawingLayer != null) {
            prod = drawingLayer.getActiveProduct();
            for (Layer layer : prod.getLayers()) {
                // formatting each layer separately
                all.addAll(layer.getDrawables());
            }
        }

        if (prod != null) {
            try {
                prod.setOutputFile(drawingLayer.buildActivityLabel(prod));
                dataURI = StorageUtils.storeProduct(prod);
            } catch (PgenStorageException e) {
                StorageUtils.showError(e);
                return;
            }
        }

        ArrayList<Gfa> allGfa = new ArrayList<Gfa>();
        for (AbstractDrawableComponent adc : all) {
            if ((adc instanceof Gfa) && !((Gfa) adc).isSnapshot()) {
                allGfa.add((Gfa) adc);
            }
        }

        // tool
        StringBuilder sb;
        try {
            sb = pgenGfaFormatTool.generate(drawingLayer, allGfa, areas,
                    categories, dataURI);
            text.setText(sb.toString());
        } catch (IOException e) {
            text.setText("I/O Error");
            // logger.error(e);
            e.printStackTrace();
        } catch (JAXBException e) {
            text.setText("Serialization Error");
            // logger.error(e);
            e.printStackTrace();
        }

        getButton(OK_ID).setEnabled(false);
    }

    private ArrayList<String> getChecked() {
        ArrayList<String> checked = new ArrayList<String>();
        if (slcBtn.getSelection())
            checked.add(SLC);
        if (sfoBtn.getSelection())
            checked.add(SFO);
        if (chiBtn.getSelection())
            checked.add(CHI);
        if (dfwBtn.getSelection())
            checked.add(DFW);
        if (bosBtn.getSelection())
            checked.add(BOS);
        if (miaBtn.getSelection())
            checked.add(MIA);
        return checked;
    }

    private ArrayList<String> getSelectedCategories() {
        ArrayList<String> cats = new ArrayList<String>();
        if (sierraBtn.getSelection())
            cats.add(SIERRA);
        if (tangoBtn.getSelection())
            cats.add(TANGO);
        if (zuluBtn.getSelection())
            cats.add(ZULU);
        return cats;
    }

    @Override
    public void cancelPressed() {
        super.cancelPressed();
        PgenUtil.setSelectingMode();
    }

    /**
     * Set the location of the dialog
     */
    public int open() {

        if (this.getShell() == null) {
            this.create();
        }
        if (shellLocation == null) {
            shellLocation = centerOfParent();
        }

        return super.open();
    }

    public Point centerOfParent() {
        Rectangle parentSize = getParentShell().getBounds();
        Rectangle mySize = getShell().getBounds();

        int locationX, locationY;
        locationX = (parentSize.width - mySize.width) / 2 + parentSize.x;
        locationY = (parentSize.height - mySize.height) / 2 + parentSize.y;

        return new Point(locationX, locationY);
    }

    /**
     * Gets values of all attributes of the dialog.
     */
    public HashMap<String, Object> getAttrFromDlg() {

        HashMap<String, Object> attr = new HashMap<String, Object>();

        return attr;
    }

    /**
     * Sets values of all attributes of the dialog.
     */
    public void setAttrForDlg(IAttribute attr) {
    }

    private void updateLastUsed() {
        lastNrml = nrmlBtn.getSelection();
        lastWest = westBtn.getSelection();
        lastSlc = slcBtn.getSelection();
        lastSfo = sfoBtn.getSelection();
        lastCentral = centralBtn.getSelection();
        lastChi = chiBtn.getSelection();
        lastDfw = dfwBtn.getSelection();
        lastEast = eastBtn.getSelection();
        lastBos = bosBtn.getSelection();
        lastMia = miaBtn.getSelection();
        lastSierra = sierraBtn.getSelection();
        lastTango = tangoBtn.getSelection();
        lastZulu = zuluBtn.getSelection();

        getButton(OK_ID).setEnabled(true);
    }

    /**
     * Selection listener class handles selections.
     * 
     * @author mlaryukhin
     * 
     */
    private class ChkBtnSelectionListener extends SelectionAdapter {

        private Button b1;

        private Button b2;

        private Button b3;

        public ChkBtnSelectionListener(Button b1, Button b2, Button b3) {
            this.b1 = b1;
            this.b2 = b2;
            this.b3 = b3;
            b1.addSelectionListener(this);
            b2.addSelectionListener(this);
            b3.addSelectionListener(this);
        }

        @Override
        public void widgetSelected(SelectionEvent e) {
            super.widgetSelected(e);

            Button source = (Button) e.getSource();

            if (source == b1) {
                boolean select = b1.getSelection();
                b2.setSelection(select);
                b3.setSelection(select);
            } else {
                boolean select = b2.getSelection() && b3.getSelection();
                b1.setSelection(select);
            }

            updateLastUsed();
        }
    }

    public void setGfaFormatTool(PgenGfaFormatTool pgenGfaFormatTool) {
        this.pgenGfaFormatTool = pgenGfaFormatTool;
    }

}
