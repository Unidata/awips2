package gov.noaa.nws.ncep.viz.rsc.timeseries.view;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;
import gov.noaa.nws.ncep.viz.rsc.timeseries.rsc.RetrieveUtils;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

/**
 * 
 * Display KTableView for GeoMagResource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * 07/07/2014    R4079       qzhou     Initial Creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class KTableView extends ViewPart implements SelectionListener,
        DisposeListener, IPartListener {

    public static final String kTableId = "gov.noaa.nws.ncep.viz.rsc.timeseries.view.KTableView";//

    public Composite composite = null;

    private Text text;

    private boolean isEditorVisible = true;

    private static KTableView kTableView = null;

    public static KTableView getAccess() {
        return kTableView;
    }

    public KTableView() {

        super();
        if (kTableView == null)
            kTableView = this;
        else {
            KTableView tmp = kTableView;
            kTableView = this;

        }
    }

    /**
     * Invoked by the workbench to initialize this View.
     */
    public void init(IViewSite site) {
        try {
            super.init(site);

        } catch (PartInitException pie) {

            pie.printStackTrace();
        }

        // GeoMagResource.registerMouseHandler();
        // page = site.getPage();
        // page.addPartListener(this);

    }

    /**
     * Disposes resource. invoked by the workbench
     */
    public void dispose() {

        if (!isEditorVisible) {
            // GeoMagResource.unregisterMouseHandler();
            return;
        } else {
            super.dispose();

            // NatlCntrsEditor editor = GeoMagResource.getMapEditor();
            //
            // if (editor != null) {
            // for (IRenderableDisplay display : UiUtil
            // .getDisplaysFromContainer(editor)) {
            // // System.out.println("display " + display.toString());
            // for (ResourcePair rp : display.getDescriptor()
            // .getResourceList()) {
            // if (rp.getResource() instanceof GeoMagResource) {
            // GeoMagResource rsc = (GeoMagResource) rp
            // .getResource();
            // rsc.unload();
            // }
            // }
            // }
            // }
            kTableView = null;

            /*
             * remove the workbench part listener
             */
            // page.removePartListener(this);
        }

    }

    private void close() {
        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView(kTableId);
        wpage.hideView(vpart);

        NcDisplayMngr.setPanningMode();
    }

    @Override
    public void createPartControl(Composite parent) {

        parent.setLayout(new GridLayout(1, false));

        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 4,
                1));
        composite.setLayout(new GridLayout(2, false));

        createTextArea(composite);

    }

    public void createTextArea(Composite parent) {
        // Text display area
        text = new Text(parent, SWT.V_SCROLL | SWT.H_SCROLL);

        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        text.setLayoutData(data);

        Font font = text.getFont();
        FontData[] fontData = font.getFontData();
        for (int i = 0; i < fontData.length; i++) {
            fontData[i].setStyle(1);
            fontData[i].setName("courier");
        }
        Font newFont = new Font(font.getDevice(), fontData);
        text.setFont(newFont);
        text.setEditable(false);

        // RGB color = RGBColors.getRGBColor("black");
        // Device device = parent.getDisplay();
        // text.setBackground(new Color(device, color));

    }

    public void paintKTable(List<GeoMagRecord> recordsList) {

        text.setText("");
        List<String[]> kList = getKValues(recordsList);

        // Add title to kList
        String t0 = "          ";
        String t1 = " K        ";
        String t2 = " Gamma    ";
        String t3 = " HK       ";
        String t4 = " H-Gamma  ";
        String t5 = " DK       ";
        String t6 = " D-Gamma  ";

        // StringBuffer drawStrings = new StringBuffer();

        for (int j = 0; j < 7; j++) {
            // add title for each j
            if (j == 0)
                text.append(t0);
            else if (j == 1)
                text.append(t1);
            else if (j == 2)
                text.append(t2);
            else if (j == 3)
                text.append(t3);
            else if (j == 4)
                text.append(t4);
            else if (j == 5)
                text.append(t5);
            else if (j == 6)
                text.append(t6);

            // add rest for each j.
            for (int i = 0; i < kList.size(); i++) {
                String[] drawStr = kList.get(i);
                text.append(String.format("%12s", drawStr[j]));

            }
            text.append("\n");

        }

        // text.setText(drawStrings.toString());
        text.setVisible(true);

    }

    public List<String[]> getKValues(List<GeoMagRecord> recordsList) {
        List<GeoMagK1min> k1minList = null;
        List<String[]> kList = new ArrayList<String[]>();

        Calendar synTime = null;

        for (int i = 1; i < recordsList.size(); i++) { // omit the first

            Calendar calInstant = recordsList.get(i).getDataTime()
                    .getValidTime();
            if (i == recordsList.size() - 1) {
                if (calInstant.get(Calendar.MINUTE) == 0)
                    synTime = recordsList.get(recordsList.size() - 2)
                            .getDataTime().getValidTime();
                else
                    synTime = recordsList.get(recordsList.size() - 1)
                            .getDataTime().getValidTime();

                try {
                    k1minList = RetrieveUtils.retrieveSingleK1minRecord(
                            recordsList.get(0).getStationCode(), new Date(
                                    synTime.getTimeInMillis())); // RetrieveUtils.getUtcTime(synTime));

                } catch (GeoMagException e) {
                    System.out
                            .println("GeoMag KTableView: Error retrieving k1min record.");
                }

                if (k1minList != null && k1minList.size() > 0) {
                    GeoMagK1min k1min = k1minList.get(0);
                    String[] temp = new String[7];

                    Date d = k1min.getRefTime();
                    Calendar cal = RetrieveUtils.getUtcCalendar(d);

                    // deal with recordsList.size()
                    if (cal.get(Calendar.MINUTE) != 0) {
                        if (cal.get(Calendar.HOUR_OF_DAY) % 3 == 1)
                            cal.add(Calendar.HOUR_OF_DAY, 2);
                        else if (cal.get(Calendar.HOUR_OF_DAY) % 3 == 2)
                            cal.add(Calendar.HOUR_OF_DAY, 1);

                        cal.set(Calendar.MINUTE, 0);
                    }
                    String end = RetrieveUtils.hourTo2DigetStr(cal);
                    // minus 3 hour becomes previous synaptic point
                    cal.add(Calendar.HOUR_OF_DAY, -3);
                    String start = RetrieveUtils.hourTo2DigetStr(cal);

                    temp[0] = start + " - " + end;
                    temp[1] = String.valueOf(k1min.getKestIndex());
                    temp[2] = String.valueOf((int) k1min.getKestGamma());
                    temp[3] = String.valueOf(k1min.getHkIndex());
                    temp[4] = String.valueOf((int) k1min.getHkGamma());
                    temp[5] = String.valueOf(k1min.getDkIndex());
                    temp[6] = String.valueOf((int) k1min.getDkGamma());
                    kList.add(temp);
                }

            } else if (!(calInstant.get(Calendar.HOUR_OF_DAY) % 3 == 0 && calInstant
                    .get(Calendar.MINUTE) == 0)) {
                synTime = calInstant;

            } else {

                try {
                    k1minList = RetrieveUtils.retrieveSingleK1minRecord(
                            recordsList.get(0).getStationCode(), new Date(
                                    synTime.getTimeInMillis())); // RetrieveUtils.getUtcTime(synTime));

                } catch (GeoMagException e) {
                    System.out
                            .println("GeoMag KTableView: Error retrieving k1min record.");
                }

                if (k1minList != null && k1minList.size() > 0) {
                    GeoMagK1min k1min = k1minList.get(0);
                    String[] temp = new String[7];

                    Date d = k1min.getRefTime();
                    Calendar cal = RetrieveUtils.getUtcCalendar(d);

                    // adding 1 minute becomes next synaptic point
                    cal.add(Calendar.MINUTE, 1);
                    String end = RetrieveUtils.hourTo2DigetStr(cal);
                    // minus 3 hour becomes previous synaptic point
                    cal.add(Calendar.HOUR_OF_DAY, -3);
                    String start = RetrieveUtils.hourTo2DigetStr(cal);

                    temp[0] = start + " - " + end;
                    temp[1] = String.valueOf(k1min.getKestIndex());
                    temp[2] = String.valueOf((int) k1min.getKestGamma());
                    temp[3] = String.valueOf(k1min.getHkIndex());
                    temp[4] = String.valueOf((int) k1min.getHkGamma());
                    temp[5] = String.valueOf(k1min.getDkIndex());
                    temp[6] = String.valueOf((int) k1min.getDkGamma());
                    kList.add(temp);
                }
            }
        }

        return kList;
    }

    public Text getText() {
        return text;
    }

    public void setText(Text text) {
        this.text = text;
    }

    @Override
    public void setFocus() {

    }

    @Override
    public void partActivated(IWorkbenchPart part) {

    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {

    }

    @Override
    public void partClosed(IWorkbenchPart part) {

    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {

    }

    @Override
    public void partOpened(IWorkbenchPart part) {

    }

    @Override
    public void widgetDisposed(DisposeEvent e) {

    }

    @Override
    public void widgetSelected(SelectionEvent e) {

    }

    @Override
    public void widgetDefaultSelected(SelectionEvent e) {

    }

    public void setEditorVisible(boolean isVisible) {
        this.isEditorVisible = isVisible;
    }

    public boolean getEditorVisible() {
        return this.isEditorVisible;
    }
}
